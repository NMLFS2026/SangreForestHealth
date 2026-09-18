# Sangre Veg Data Analysis 2026
# 9/16/2026
# Angie Taylor

# Github setup

usethis::create_github_token()

gitcreds::gitcreds_set()

usethis::use_git_ignore()


# VEGW Vegetation Monitoring Analysis

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(vegan)
library(purrr)
library(openxlsx)

# Config -- change these two to rerun the whole script for a different year
YEAR       <- 2026   # current field season to analyze
PRIOR_YEAR <- 2025   # comparison year for year-over-year sections

# Folder every plot gets saved into, in addition to showing in the Plots pane
FIGURES_DIR <- "Figures"
if (!dir.exists(FIGURES_DIR)) dir.create(FIGURES_DIR, recursive = TRUE)

# Load data
veg_raw <- read.xlsx("Data/VEG_features.xlsx")


# Site groupings and shared palette -- defined once and reused everywhere
treated_sites   <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

# Just show 12 top species and the rest are other
TOP_N_SPECIES <- 12

# Theme
theme_set(
  theme_minimal(base_size = 13) +
    theme(
      plot.title      = element_text(face = "bold", size = 15),
      axis.text.x     = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      legend.title    = element_text(face = "bold")
    )
)

# cleaning
clean_veg <- function(df,
                      year = NULL,
                      cover_type = NULL,
                      require_species = TRUE,
                      data_entry = DATA_ENTRY_FILTER) {
  out <- df %>%
    mutate(
      Site       = str_extract(PlotID, "^[A-Za-z]+\\d+"),
      Species    = trimws(Species),
      CoverType  = str_to_title(trimws(as.character(CoverType))),
      DataEntry  = trimws(DataEntry),
      Percent    = na_if(as.character(Percent), "T"),   # trace -> NA
      Percent    = suppressWarnings(as.numeric(Percent)),
      Year       = as.numeric(Year)
    ) %>%
    filter(Site %in% c(treated_sites, untreated_sites)) %>%
    mutate(Treatment = case_when(
      Site %in% treated_sites   ~ "Treated",
      Site %in% untreated_sites ~ "Untreated"
    ))
  
  if (!is.na(data_entry)) out <- filter(out, DataEntry == data_entry)
  if (!is.null(year))       out <- filter(out, Year == year)
  if (!is.null(cover_type)) out <- filter(out, CoverType == cover_type)
  if (require_species) {
    out <- filter(out, !is.na(Species), Species != "", Species != "N/A")
  }
  out
}

# Number of plots actually surveyed per site
plots_per_site <- function(df) {
  df %>% distinct(Site, PlotID) %>% count(Site, name = "n_plots")
}

# Species richness by site, plus a t-test comparing Treated vs Untreated.
compute_richness_ttest <- function(df, label = "") {
  richness_summary <- df %>%
    group_by(Site, Treatment) %>%
    summarise(richness = n_distinct(Species), .groups = "drop")
  
  cat("\n\U0001F4CA Species richness by site", label, ":\n")
  print(richness_summary)
  
  if (n_distinct(richness_summary$Treatment) == 2) {
    cat("\n\U0001F4CA T-test: richness Treated vs Untreated", label, ":\n")
    print(t.test(richness ~ Treatment, data = richness_summary))
  } else {
    warning("T-test not run: only one treatment group present.")
  }
  invisible(richness_summary)
}

# Percent-cover t-test, Treated vs Untreated, for whatever data is passed in.
compute_percent_ttest <- function(df, label = "") {
  cat("\n\U0001F4CA T-test: percent cover Treated vs Untreated", label, ":\n")
  print(t.test(Percent ~ Treatment, data = df))
}

# Average percent cover per species per site noramlized by plits per site
avg_percent_by_site_species <- function(df) {
  n_plots <- plots_per_site(df)
  df %>%
    group_by(Site, Species) %>%
    summarise(TotalPercent = sum(Percent, na.rm = TRUE), .groups = "drop") %>%
    left_join(n_plots, by = "Site") %>%
    mutate(AvgPercentCover = TotalPercent / n_plots)
}

# Picks the TOP_N_SPECIES species with the most total cover (weight_col defaults to "Percent"), broken deterministically by arrange()+slice_head() rather than slice_max() -- slice_max() includes every tied species by default, so a tie for 8th place could silently inflate "top 8" to 10+.
top_species_by_cover <- function(df, top_n = TOP_N_SPECIES, weight_col = "Percent") {
  df %>%
    group_by(Species) %>%
    summarise(total = sum(.data[[weight_col]], na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total), Species) %>%
    slice_head(n = top_n) %>%
    pull(Species)
}

# Collapses every species not in top_species into "Other". Kept as a plain
# character column (not a factor) here, so a later count()/summarise() call
# correctly merges every lumped species into one "Other" total rather than
# ggplot's default per-chart legend behavior interfering with the grouping.
lump_species <- function(df, top_species) {
  mutate(df, Species = if_else(Species %in% top_species, Species, "Other"))
}

# A small, distinguishable color set for a fixed species order (top species +
# "Other" in grey). Uses ggplot's own hue wheel (scales::hue_pal) rather than
# a fixed ColorBrewer palette -- brewer palettes like "Set2"/"Dark2" include a
# built-in grey once you get to ~8 colors, which visually collided with the
# "Other" grey and made a real species disappear into the "Other" block.
species_palette <- function(species_order) {
  n <- length(species_order) - 1  # excluding "Other"
  setNames(c(hue_pal()(n), "grey55"), species_order)
}

# Stacked bar chart of species counts or cover, styled for a report: capped
# color legend, bold title, clean gridlines, no clipped bars (y-axis
# auto-scales). species_order is the FULL top-species + "Other" list decided
# once upstream (see top_species_by_cover()), so a given species always maps
# to the same color everywhere -- but the legend itself only lists whatever
# actually has a bar in THIS chart (drop = TRUE, ggplot's default), so a
# species with zero rows here doesn't clutter the key.
plot_species_stack <- function(df, x_var, y_var, title, y_lab, species_order) {
  df <- mutate(df, Species = factor(Species, levels = species_order))
  ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]], fill = Species)) +
    geom_col(width = 0.7) +
    scale_fill_manual(values = species_palette(species_order), drop = TRUE) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(title = title, x = NULL, y = y_lab, fill = "Species")
}

# Displays a plot in the Plots pane AND saves it to Figures/, so nothing is
# lost once the session closes. filename should NOT include a folder or
# extension, e.g. "basal_counts_2026" -> Figures/basal_counts_2026.png
show_and_save <- function(plot, filename, width = 9, height = 6) {
  print(plot)
  ggsave(
    filename = file.path(FIGURES_DIR, paste0(filename, ".png")),
    plot = plot, width = width, height = height, dpi = 300
  )
}

# Species-by-site abundance matrix, for vegan diversity functions.
species_matrix <- function(df) {
  df %>%
    count(Site, Species, name = "Abundance") %>%
    pivot_wider(names_from = Species, values_from = Abundance, values_fill = 0) %>%
    as.data.frame() %>%
    tibble::column_to_rownames("Site")
}

# Shannon and Simpson diversity per site, plus richness, plus a t-test.
compute_diversity_ttest <- function(df, index = c("shannon", "simpson"), label = "") {
  index <- match.arg(index)
  mat <- species_matrix(df)
  diversity_values <- data.frame(
    Site             = rownames(mat),
    Diversity        = diversity(mat, index = index),
    SpeciesRichness  = specnumber(mat),
    TotalOccurrences = rowSums(mat)
  ) %>%
    mutate(Treatment = case_when(
      Site %in% treated_sites   ~ "Treated",
      Site %in% untreated_sites ~ "Untreated"
    ))
  
  cat("\n\U0001F4CA", str_to_title(index), "diversity per site", label, ":\n")
  print(diversity_values)
  
  cat("\n\U0001F50D T-test:", str_to_title(index), "diversity Treated vs Untreated", label, ":\n")
  print(t.test(Diversity ~ Treatment, data = diversity_values))
  invisible(diversity_values)
}

# Per-plot Simpson evenness and Shannon evenness (Pielou's J) at the Species
# level, plus a t-test. (Lifeform-level evenness removed -- this year's data
# has no Lifeform column.)
compute_evenness_ttest <- function(df, label = "") {
  per_plot <- df %>%
    group_by(PlotID, Site, Treatment, Species) %>%
    summarise(total_cover = sum(Percent, na.rm = TRUE), .groups = "drop") %>%
    group_by(PlotID, Site, Treatment) %>%
    mutate(p = total_cover / sum(total_cover)) %>%
    summarise(
      D          = sum(p^2),
      H          = -sum(p * log(p)),
      S          = n(),
      SimpsonEvenness = (1 / D) / S,
      ShannonEvenness = H / log(S),
      .groups = "drop"
    )
  
  cat("\n\U0001F4CA T-test: Simpson evenness Treated vs Untreated", label, ":\n")
  print(t.test(SimpsonEvenness ~ Treatment, data = per_plot))
  cat("\n\U0001F4CA T-test: Shannon evenness Treated vs Untreated", label, ":\n")
  print(t.test(ShannonEvenness ~ Treatment, data = per_plot))
  invisible(per_plot)
}

# Fisher's exact test for presence/absence by treatment, for a set of species
# (or all species if species_list is NULL). Denominators (plots per treatment)
# are computed dynamically instead of hardcoded (the old script used fixed
# values like 72/71 that would silently go stale in a new year).
run_fisher_presence <- function(df, species_list = NULL, p_threshold = NULL) {
  if (is.null(species_list)) species_list <- unique(df$Species)
  
  plots <- df %>% distinct(PlotID, Treatment)
  n_by_treatment <- count(plots, Treatment, name = "n_plots")
  
  presence <- df %>%
    filter(Species %in% species_list) %>%
    distinct(PlotID, Species) %>%
    mutate(Present = 1L)
  
  full_grid <- expand_grid(PlotID = plots$PlotID, Species = species_list) %>%
    left_join(plots, by = "PlotID") %>%
    left_join(presence, by = c("PlotID", "Species")) %>%
    mutate(Present = if_else(is.na(Present), 0L, Present))
  
  summary_table <- full_grid %>%
    group_by(Species, Treatment) %>%
    summarise(Present = sum(Present), .groups = "drop") %>%
    pivot_wider(names_from = Treatment, values_from = Present, names_glue = "{Treatment}_Present") %>%
    mutate(
      Treated_Absent   = n_by_treatment$n_plots[n_by_treatment$Treatment == "Treated"]   - Treated_Present,
      Untreated_Absent = n_by_treatment$n_plots[n_by_treatment$Treatment == "Untreated"] - Untreated_Present
    )
  
  results <- summary_table %>%
    mutate(test = pmap(
      list(Treated_Present, Treated_Absent, Untreated_Present, Untreated_Absent),
      ~ fisher.test(matrix(c(..1, ..2, ..3, ..4), nrow = 2, byrow = TRUE))
    )) %>%
    mutate(
      p_value    = map_dbl(test, "p.value"),
      odds_ratio = map_dbl(test, ~ .x$estimate),
      conf_low   = map_dbl(test, ~ .x$conf.int[1]),
      conf_high  = map_dbl(test, ~ .x$conf.int[2])
    ) %>%
    select(Species, p_value, odds_ratio, conf_low, conf_high)
  
  if (!is.null(p_threshold)) results <- filter(results, p_value < p_threshold)
  # p < 0.05: presence/absence differs between treated vs untreated
  # odds_ratio > 1: more likely in treated plots; < 1: more likely in untreated
  # wide CI: low statistical power (common with small counts)
  results
}

# =============================================================================
# Basal cover: richness and percent cover (current year)
# =============================================================================
basal_current <- clean_veg(veg_raw, year = YEAR, cover_type = "Basal")

compute_richness_ttest(basal_current, label = paste0("(", YEAR, ")"))
compute_percent_ttest(
  filter(basal_current, !is.na(Percent)),
  label = paste0("(", YEAR, " Basal)")
)


# =============================================================================
# Species observation counts (current year, Basal)
# Lumped to the top species by cover so the chart and legend are readable;
# see top_species_by_cover()/lump_species(). Underlying stats above still use
# the full species list -- lumping is for the charts only.
# =============================================================================
top_species_current   <- top_species_by_cover(basal_current)
species_order_current <- c(top_species_current, "Other")
basal_current_lumped  <- lump_species(basal_current, top_species_current)

counts_by_treatment <- basal_current_lumped %>% count(Treatment, Species)
p_counts_treatment <- plot_species_stack(counts_by_treatment, "Treatment", "n",
                                         paste("Basal Species Observation Counts,", YEAR),
                                         "Species Observation Count", species_order_current)
show_and_save(p_counts_treatment, paste0("basal_species_counts_by_treatment_", YEAR))

counts_by_treated_site <- basal_current_lumped %>%
  filter(Site %in% treated_sites) %>%
  count(Site, Species)
p_counts_treated_site <- plot_species_stack(counts_by_treated_site, "Site", "n",
                                            paste("Basal Species Observation Counts per Treated Site,", YEAR),
                                            "Species Observation Count", species_order_current)
show_and_save(p_counts_treated_site, paste0("basal_species_counts_by_treated_site_", YEAR))

# =============================================================================
# Average percent cover per species per site (current year, Basal)
# Normalized by plots actually surveyed at each site (see
# avg_percent_by_site_species()), rather than a hardcoded x4/x6/x24 adjustment.
# =============================================================================
avg_cover_treated <- basal_current_lumped %>%
  filter(Site %in% treated_sites, !is.na(Percent)) %>%
  avg_percent_by_site_species()

avg_cover_untreated <- basal_current_lumped %>%
  filter(Site %in% untreated_sites, !is.na(Percent)) %>%
  avg_percent_by_site_species()

p_treated   <- plot_species_stack(avg_cover_treated, "Site", "AvgPercentCover",
                                  paste("Avg % Basal Cover, Treated Plots,", YEAR), "Avg % Basal Cover",
                                  species_order_current)
p_untreated <- plot_species_stack(avg_cover_untreated, "Site", "AvgPercentCover",
                                  paste("Avg % Basal Cover, Untreated Plots,", YEAR), "Avg % Basal Cover",
                                  species_order_current)

show_and_save(p_treated, paste0("avg_basal_cover_treated_", YEAR))
show_and_save(p_untreated, paste0("avg_basal_cover_untreated_", YEAR))

# =============================================================================
# Diversity indices (current year, Basal)
# =============================================================================
compute_diversity_ttest(basal_current, index = "shannon", label = paste0("(", YEAR, " Basal)"))
compute_diversity_ttest(basal_current, index = "simpson", label = paste0("(", YEAR, " Basal)"))
compute_evenness_ttest(filter(basal_current, !is.na(Percent)), label = paste0("(", YEAR, " Basal)"))

# =============================================================================
# Year-over-year species counts (current vs prior year, Basal)
# Top species picked fresh over the combined two-year pool, so the legend is
# consistent between the two bars in this specific chart.
# =============================================================================
basal_prior      <- clean_veg(veg_raw, year = PRIOR_YEAR, cover_type = "Basal")
basal_both       <- bind_rows(basal_current, basal_prior)
top_species_both <- top_species_by_cover(basal_both)
species_order_both <- c(top_species_both, "Other")
basal_both_lumped <- lump_species(basal_both, top_species_both)

counts_treated_years <- basal_both_lumped %>%
  filter(Site %in% treated_sites) %>%
  count(Year = factor(Year), Species)
p_years_treated <- plot_species_stack(counts_treated_years, "Year", "n",
                                      "Basal Species Observation Counts, Treated Sites", "Species Observation Count",
                                      species_order_both)
show_and_save(p_years_treated, paste0("basal_species_counts_treated_", PRIOR_YEAR, "_vs_", YEAR))

counts_untreated_years <- basal_both_lumped %>%
  filter(Site %in% untreated_sites) %>%
  count(Year = factor(Year), Species)
p_years_untreated <- plot_species_stack(counts_untreated_years, "Year", "n",
                                        "Basal Species Observation Counts, Untreated Sites", "Species Observation Count",
                                        species_order_both)
show_and_save(p_years_untreated, paste0("basal_species_counts_untreated_", PRIOR_YEAR, "_vs_", YEAR))

# =============================================================================
# Year-over-year percent cover t-tests (Basal and Aerial)
# =============================================================================
for (yr in c(PRIOR_YEAR, YEAR)) {
  for (ct in c("Basal", "Aerial")) {
    df_yr <- clean_veg(veg_raw, year = yr, cover_type = ct, require_species = FALSE) %>%
      filter(!is.na(Percent))
    if (nrow(df_yr) > 0 && n_distinct(df_yr$Treatment) == 2) {
      compute_percent_ttest(df_yr, label = paste0("(", yr, " ", ct, ")"))
    }
  }
}

# =============================================================================
# Presence/absence (Fisher's exact test, current year)
# =============================================================================
species_of_interest <- c(
  "CARXXX", "ELYELY", "MUHMON", "BOUGRA", "BROCIL",
  "DANSPI", "KOEMAC", "MUHTRI", "POAFEN"
)

cat("\nFisher's exact tests, species of interest:\n")
print(run_fisher_presence(basal_current, species_list = species_of_interest))

cat("\nFisher's exact tests, all species, p < 0.05 only:\n")
print(run_fisher_presence(basal_current, p_threshold = 0.05))