# -- important: using the set up of SangreTrees2026_MC.rmd, slightly cleaner spot for data
# analysis and visualization run all chunks before {r summary table}

# -- author: Maura Collins 9/24/26

# -- notes:
# based on report last year lets make the following stats:
# - tree density per hectarte
# - mortality per hectare
# - veg basal cover percentage vs arieal veg cover
#           - seedlings? <- skip
# - live tree density
# - count by species
# - dbh by species
#           - og <- we're not going to do that bc we didn't talk about it // replace with mortality factors
# - talk about spatial distance of veg plot to tree and see if that effects the veg composiont : look at by species

## list for use later!
# version 3: codes to common name 
#abco - white fir
#psme - Douglass Fir
#PIST -  white pine
#PIPO - ponderosa pine 
#PIED - pinyon pine
#JUSC - rocky mountain juniper
#JUMO - One seed juniper
#QUUN - Wavy leaf Oak
#QUGA - Gambels Oak
#ACGL - Rocky Mountain Maple
#POTR - Quaking Aspen
#SASC - 
#PRVI - 


## (I) General population trends over time 
#View(trees_a)

tree_counts_over_time <- trees_a %>%
  filter(!is.na(Year), !is.na(Site), !is.na(TreeID)) %>%
  filter(
    !(toupper(trimws(as.character(Year))) %in% c("NA", "N/A", "")),
    !(toupper(trimws(as.character(Site))) %in% c("NA", "N/A", ""))
  ) %>%
  distinct(Year, Site, TreeID) %>%
  count(Year, Site, name = "Total_Trees")

#View(tree_counts_over_time)

trees_over_time_plot <- ggplot(
  tree_counts_over_time,
  aes(x = Year, y = Total_Trees, group = Site)
) +
  geom_line(aes(color = Site), linewidth = 0.8) +
  geom_point(aes(color = Site), size = 2) +
  labs(
    title = "Total Number of Trees by Site Over Time",
    x = "Year",
    y = "Number of Trees"
  ) +
  theme_classic() +
  theme(legend.position = "none")
  
  ggsave( "Figures/trees_over_time.png", trees_over_time_plot,
  width = 10, height = 7, dpi = 300 )

treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

#as table
tree_counts_timeseries <- tree_counts_over_time %>%
  mutate(Treatment = case_when(
    Site %in% treated_sites ~ "Treated",
    Site %in% untreated_sites ~ "Untreated"
  )) %>%
  group_by(Year, Treatment) %>%
  summarise(Total_Trees = sum(Total_Trees), .groups = 'drop')
View(tree_counts_timeseries)

# living trees over time 
living_tree_count_timeseries <- trees_a %>%
  filter(Tree_condition %in% c(1, 3, 7)) %>% #living only
  filter(!is.na(Year), !is.na(Site), !is.na(TreeID)) %>%
  distinct(Year, Site, TreeID) %>%
  count(Year, Site, name = "Total_Trees")

living_tree_counts_timeseries <- living_tree_count_timeseries %>%
  mutate(Treatment = case_when(
    Site %in% treated_sites ~ "Treated",
    Site %in% untreated_sites ~ "Untreated"
  )) %>%
  group_by(Year, Treatment) %>%
  summarise(Total_Trees = sum(Total_Trees), .groups = 'drop')
View(living_tree_counts_timeseries)




## (II) Report Analysis 

#set up 
treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

View(trees_2026)
# .25 ha plots: 8
# ha plots: 2 "BTN4" "SFF2" // "SFS4" "SFF8"
# 3 ha per treatment 

living_2026 <- trees_2026 %>%
  filter(Year == 2026, Tree_condition %in% c(1))
trees_2025 <- trees_a %>%
  filter(Year == 2025)

## (A) Tree density 

# (A0) living tree density per hectare (treated vs untreated)
summary_2026living <- living_2026 %>%
  filter(Site %in% c(treated_sites, untreated_sites)) %>%
  group_by(Site) %>%
  summarise(Total_Trees = n(), .groups = 'drop') %>%
  mutate(Treatment = case_when(
    Site %in% treated_sites ~ "Treated",
    Site %in% untreated_sites ~ "Untreated"
  )) %>%
  group_by(Treatment) %>%
  summarise(
    Sum_Trees = sum(Total_Trees),           # total trees for the treatment
    SE_Trees = sqrt(sum((Total_Trees - mean(Total_Trees))^2)) / sqrt(n()),  # standard error of the sum approximation
    Trees_Divided_By_3 = Sum_Trees / 3, #3 ha per treatment
    SE_Divided_By_3 = SE_Trees / 3,
    .groups = 'drop' )
View(summary_2026living)
# statistic: 355 living trees/ha (treated) vs 1,323.3 living trees/ha (untreated)

#(A1) Tree density/ha by site 
one_ha_sites <- c("BTN4", "SFS4", "SFF2", "SFF8")

tree_density_site <- trees_2026 %>%
  group_by(Site) %>%
  summarise(Total_Trees = n(), .groups = "drop") %>%
  mutate(
    Plot_Area_ha = if_else(Site %in% one_ha_sites, 1, 0.25),
    Density_ha = Total_Trees / Plot_Area_ha)
View(tree_density_site)

#(A2) Livee tree density by site aka the count of living trees
live_tree_density_site <- living_2026 %>%
  group_by(Site) %>%
  summarise(Total_Trees = n(), .groups = "drop") %>%
  mutate(
    Plot_Area_ha = if_else(Site %in% one_ha_sites, 1, 0.25),
    Density_ha = Total_Trees / Plot_Area_ha)
View(live_tree_density_site)

#(A3) Replicating the Tree density vs live tree density plots
# x is treated vs untreated, y is density per hectare

#version 3: not averaging the sites 
#   density_by_treatment <- bind_rows(
#   tree_density_site %>%
#     mutate(Tree_Status = "All Trees"),
#   live_tree_density_site %>%
#     mutate(Tree_Status = "Living Trees")
# ) %>%
#   mutate(Treatment = case_when(
#     Site %in% treated_sites ~ "Treated",
#     Site %in% untreated_sites ~ "Untreated"
#   )) %>%
#   #standard error bars prep
#   filter(!is.na(Treatment)) %>%
#   mutate(Site_Density_ha = Total_Trees / Plot_Area_ha) %>%
#   group_by(Treatment, Tree_Status) %>%
#   summarise(
#     n = n(),
#     Density_ha = mean(Site_Density_ha),
#     SE = sd(Site_Density_ha) / sqrt(n),
#     .groups = "drop"
#   ) %>%
#   mutate(Treatment = factor(Treatment, levels = c("Treated", "Untreated")))

#version 4: averaging the density by site
one_ha_sites    <- c("BTN4", "SFF2", "SFS4", "SFF8")

#site reference table
site_info <- tibble(Site = c(treated_sites, untreated_sites)) %>%
  mutate(
    Treatment = if_else(Site %in% treated_sites, "Treated", "Untreated"),
    Plot_Area_ha = if_else(Site %in% one_ha_sites, 1, 0.25) )

# Tree counts per site for each status
tree_counts <- bind_rows(
  trees_2026 %>%
    count(Site, name = "Total_Trees") %>%
    mutate(Tree_Status = "All Trees"), #All trees, name changed for plot title
  trees_2026 %>%
    filter(Tree_condition == "1") %>%   
    count(Site, name = "Total_Trees") %>%
    mutate(Tree_Status = "Living Trees")
)

# density per site
site_density <- expand_grid(site_info, Tree_Status = c("All Trees", "Living Trees")) %>%
  left_join(tree_counts, by = c("Site", "Tree_Status")) %>%
  mutate(
    Total_Trees = replace_na(Total_Trees, 0),
    Site_Density_ha = Total_Trees / Plot_Area_ha)

density_by_treatment <- site_density %>%
  group_by(Treatment, Tree_Status) %>%
  summarise(
    n = n(),
    Density_ha = mean(Site_Density_ha),
    SE = sd(Site_Density_ha) / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(Treatment = factor(Treatment, levels = c("Treated", "Untreated")))

#plot
density_by_treatment_plot <- ggplot(
  density_by_treatment,
  aes(x = Treatment, y = Density_ha, fill = Treatment)
) +
  geom_col(width = 0.5, color = "black", linewidth = 0.3) +
  geom_errorbar(
    aes(ymin = Density_ha - SE, ymax = Density_ha + SE),
    width = 0.15,
    linewidth = 0.6
  ) +
  facet_wrap(~ Tree_Status, nrow = 1) +
  scale_fill_manual(
    values = c("Treated" = "#CC5500", "Untreated" = "grey60")
  ) +
  labs(
    x = NULL,
    y = "Trees per hectare"
  ) +
  theme_classic(base_size = 16) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.position = "none")

ggsave( "Figures/density_by_treatment.png", #open this in the Figures folder
  density_by_treatment_plot,
  width = 10, height = 5, dpi = 300)

# density statistics
#means and sd 

View(density_by_treatment)

# need to look at the difference in Bens code vs my code to see why the 
# numbers look so different. 

# (A4) Density tree/ha of regen (trees  dbh >2.5 cm) by treatment and species 
# species are ABCO, PIPO, PIST, PSME
# standard error also 

# regen_density <- trees_2026 %>%
#   filter(DBH )



#(B) Mortality 

#(B0) % new mortality per hectare (treated vs untreated)
Count_treatment_2026 <- trees_2026 %>%
mutate(Treatment = case_when(
    Site %in% treated_sites ~ "Treated",
    Site %in% untreated_sites ~ "Untreated"
  )) %>%
  group_by(Treatment) %>%
  summarise(Total_trees = n(), .groups = 'drop')


new_mort_2026_treatment <- trees_2026 %>%
  mutate(Treatment = case_when(
    Site %in% treated_sites ~ "Treated",
    Site %in% untreated_sites ~ "Untreated"
  )) %>%
  filter(Site %in% c(treated_sites, untreated_sites)) %>%
  group_by(Treatment) %>%
  summarise(
    Total_trees = n(),
    cond6_trees = sum(Tree_condition == 6, na.rm = TRUE),
    percent_cond6 = 100 * cond6_trees / Total_trees,
    .groups = "drop")

View(new_mort_2026_treatment)

#checking against the 2025 data
new_mort_2025_treatment <- trees_2025 %>%
  mutate(Treatment = case_when(
    Site %in% treated_sites ~ "Treated",
    Site %in% untreated_sites ~ "Untreated"
  )) %>%
  filter(Site %in% c(treated_sites, untreated_sites)) %>%
  group_by(Treatment) %>%
  summarise(
    Total_trees = n(),
    cond6_trees = sum(Tree_condition == 6, na.rm = TRUE),
    percent_cond6 = 100 * cond6_trees / Total_trees,
    .groups = "drop" )

#View(new_mort_2025_treatment )


#(B1) % new mortality by site and total new mortality
new_mort_2026_site <- trees_2026 %>%  
  filter(Site %in% c(treated_sites, untreated_sites)) %>%
  group_by(Site) %>%
  summarise(
    Total_trees = n(),
    cond6_trees = sum(Tree_condition == 6, na.rm = TRUE),
    percent_cond6 = 100 * cond6_trees / Total_trees,
    .groups = "drop") 

new_mort_2026_site <- bind_rows(
  new_mort_2026_site,
  new_mort_2026_site %>%
    summarise(
      Site = "Total",
      Total_trees = sum(Total_trees),
      cond6_trees = sum(cond6_trees),
      percent_cond6 = 100 * cond6_trees / Total_trees))

  View(new_mort_2026_site)

#View(trees_a)

#(B2) Plot of DBH of all Tree_cond = 6, by treatment and species 

new_mort_dbh_species_by_treatment <- trees_2026 %>%
  filter(!is.na(Year), !is.na(Site), !is.na(TreeID)) %>%
  filter(
    !(toupper(trimws(as.character(Year))) %in% c("NA", "N/A", "")),
    !(toupper(trimws(as.character(Site))) %in% c("NA", "N/A", "")),
    !is.na(SpeciesID),
    !(toupper(trimws(as.character(SpeciesID))) %in% c("NA", "N/A", "")),
    !is.na(DBH)
  ) %>%
  filter(
    Site %in% c(treated_sites, untreated_sites),
    Tree_condition == 6
  ) %>%
  mutate(Treatment = case_when(
    Site %in% treated_sites ~ "Treated",
    Site %in% untreated_sites ~ "Untreated"
  )) %>%
  mutate(SpeciesID = factor(SpeciesID, levels = species_order))

#View(live_species_by_treatment)

#plot prep
species_order <- c(
  "ABCO", "PSME",           # firs
  "PIST", "PIPO", "PIED",   # pines
  "JUSC", "JUMO",           # junipers
  "QUUN", "QUGA",           # oaks
  "ACGL", "POTR", "SASC", "PRVI"  # all else
)

species_colors <- c(
  # firs: steel blue -> soft sky
  ABCO = "#2F5D8C", PSME = "#8DB8D9",
  # pines: forest -> sage -> pale sage
  PIST = "#2E6B4F", PIPO = "#78ca89", PIED = "#c8e9b7",
  # junipers: muted indigo -> lavender
  JUSC = "#7d67b1", JUMO = "#d4c6ee",
  # oaks: brick -> peach
  QUUN = "#A8402A", QUGA = "#ee8d60",
  # all else: mustard, brown, warm greys
  ACGL = "#E9C46A", POTR = "#8C6A4A", SASC = "#CFCBC3", PRVI = "#6B6B6B"
)

#version 1
new_mort_dbh_species_by_treatment_plot <- ggplot(
  new_mort_dbh_species_by_treatment,
  aes(x = DBH, fill = SpeciesID)
) +
  geom_histogram(binwidth = 5, position = "stack") +
  facet_wrap(~ Treatment, nrow = 1) +
  scale_fill_manual(
    values = c(species_colors, unknown = "#808080"),
    drop = TRUE
  ) +
  labs(
    x = "DBH",
    y = "Tree count",
    fill = "Species"
  ) +
  theme_classic(base_size = 16) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.background = element_blank(),
    strip.text = element_text(size = 20, face = "bold", hjust = 0.5)
  )
ggsave(
  "Figures/new_mort_dbh_species_by_treatment.png",
  new_mort_dbh_species_by_treatment_plot,
  width = 12, height = 6, dpi = 300
)


#(C) basic stats/counts

#(C1) count of trees surveyed
all <- trees_2026 %>%
summarise(total_trees = n(), .groups = 'drop')
print(all) #5983 trees surveyed

#(C2) count of living trees surveyed
all_alive <- trees_2026 %>%
filter(Tree_condition %in% c(1, 3, 7)) %>% 
summarise(total_trees = n(), .groups = 'drop')
print(all_alive) #5035 trees surveyed

#(C3) count of new dead trees surveyed and percentage of live trees
all_new_mort <- trees_2026 %>%
filter(Tree_condition %in% c(6)) %>% 
summarise(total_trees = n(), .groups = 'drop')
print(all_new_mort) #65 trees surveyed

percent_new_mort <- (all_new_mort$total_trees / all_alive$total_trees) * 100
print(percent_new_mort) #1.29% new mortality

#(C4) count of trees in treated and untreated
all_treatment <- trees_2026 %>%
filter(!is.na(Year), !is.na(Site), !is.na(TreeID)) %>%
    filter(
      !(toupper(trimws(as.character(Year))) %in% c("NA", "N/A", "")),
      !(toupper(trimws(as.character(Site))) %in% c("NA", "N/A", "")),
      !is.na(SpeciesID),
      !(toupper(trimws(as.character(SpeciesID))) %in% c("NA", "N/A", ""))
    ) %>%
  mutate(Treatment = case_when(
      Site %in% treated_sites ~ "Treated",
      Site %in% untreated_sites ~ "Untreated"
    )) %>%
  group_by(Treatment) %>%
  summarise(total_trees = n(), .groups = 'drop')
  print(all_treatment) # treated 1283, untreated 4699


#(D) species statistics

#(D1) what species are present and how many of each are in treated vs untreated sites
species_counts <- trees_2026 %>%
  filter(!is.na(Year), !is.na(Site), !is.na(TreeID)) %>%
    filter(
      !(toupper(trimws(as.character(Year))) %in% c("NA", "N/A", "")),
      !(toupper(trimws(as.character(Site))) %in% c("NA", "N/A", "")),
      !is.na(SpeciesID),
      !(toupper(trimws(as.character(SpeciesID))) %in% c("NA", "N/A", ""))
    ) %>%
  filter(Site %in% c(treated_sites, untreated_sites)) %>%
  group_by(Site, SpeciesID) %>%
  summarise(Total_Trees = n(), .groups = 'drop') %>%
  mutate(Treatment = case_when(
    Site %in% treated_sites ~ "Treated",
    Site %in% untreated_sites ~ "Untreated"
  )) %>%
  group_by(Treatment, SpeciesID) %>%
  summarise(Total_Trees = sum(Total_Trees), .groups = 'drop') %>%
  arrange(desc(Total_Trees))
View(species_counts)

#(D2) QUGA, PSME, ABCO percentage of total trees and live trees in treated vs untreated
all_species_percentages <- trees_2026 %>%
  filter(!is.na(Year), !is.na(Site), !is.na(TreeID)) %>%
    filter(
      !(toupper(trimws(as.character(Year))) %in% c("NA", "N/A", "")),
      !(toupper(trimws(as.character(Site))) %in% c("NA", "N/A", "")),
      !is.na(SpeciesID),
      !(toupper(trimws(as.character(SpeciesID))) %in% c("NA", "N/A", ""))
    ) %>%
  filter(Site %in% c(treated_sites, untreated_sites)) %>%
  group_by(Site, SpeciesID) %>%
  summarise(Total_Trees = n(), .groups = 'drop') %>%
  mutate(Treatment = case_when(
    Site %in% treated_sites ~ "Treated",
    Site %in% untreated_sites ~ "Untreated"
  )) %>%
  group_by(Treatment) %>%
  mutate(
    Total_Trees_Treatment = sum(Total_Trees),
    Percent_of_Total = (Total_Trees / Total_Trees_Treatment) * 100
  ) %>%
  ungroup() %>%
  filter(SpeciesID %in% c("QUGA", "PSME", "ABCO")) %>%
  group_by(Treatment) %>%
  summarise(
    Total_Trees = sum(Total_Trees),
    Percent_of_Total = sum(Percent_of_Total),
    .groups = "drop"
  )

live_species_percentages <- trees_2026 %>%
filter(!is.na(Year), !is.na(Site), !is.na(TreeID)) %>%
    filter(
      !(toupper(trimws(as.character(Year))) %in% c("NA", "N/A", "")),
      !(toupper(trimws(as.character(Site))) %in% c("NA", "N/A", "")),
      !is.na(SpeciesID),
      !(toupper(trimws(as.character(SpeciesID))) %in% c("NA", "N/A", ""))
    ) %>%
  filter(Site %in% c(treated_sites, untreated_sites), Tree_condition %in% c(1, 3, 7)) %>%
  group_by(Site, SpeciesID) %>%
  summarise(Total_Trees = n(), .groups = 'drop') %>%
  mutate(Treatment = case_when(
    Site %in% treated_sites ~ "Treated",
    Site %in% untreated_sites ~ "Untreated"
  )) %>%
  group_by(Treatment) %>%
  mutate(
    Total_Trees_Treatment = sum(Total_Trees),
    Percent_of_Total = (Total_Trees / Total_Trees_Treatment) * 100
  ) %>%
  ungroup() %>%
  filter(SpeciesID %in% c("QUGA", "PSME", "ABCO")) %>%
  group_by(Treatment) %>%
  summarise(
    Total_Trees = sum(Total_Trees),
    Percent_of_Total = sum(Percent_of_Total),
    .groups = "drop"
  )


species_percentages <- all_species_percentages %>%
  rename(Total_Trees_All = Total_Trees, Percent_of_Total_All = Percent_of_Total) %>%
  left_join(live_species_percentages %>%
              rename(Total_Trees_Live = Total_Trees, Percent_of_Total_Live = Percent_of_Total),
            by = "Treatment")

View(species_percentages)

#(D3) Plot of species_counts of all trees by treatment and species

#firs = blue (ABCO, PSME)
#pines = green (PIST, PIPO, PIED)
#junipers = purple (JUSC, JUMO)
#Oaks = red (QUUN, QUGA)
#all else = (ACGL, POTR, SASC, PRVI)

species_counts_a <- species_counts %>%
  filter(SpeciesID %in% c("unknown", "NA", "N/A", "NA ") == FALSE)

# species_counts_plot <- ggplot(
#   species_counts_a,
#   aes(x = Treatment, y = Total_Trees, fill = SpeciesID)
# ) +
#   geom_col(position = "stack", width = 0.5, color = "black", linewidth = 0.3) +
#   labs(
#     x = NULL,
#     y = "Number of Trees",
#     fill = "Species"
#   ) +
#   theme_classic(base_size = 16) +
#   theme(
#     axis.title = element_text(size = 18),
#     axis.text = element_text(size = 16),
#     legend.title = element_text(size = 16),
#     legend.text = element_text(size = 14)
#   )

#   ggsave( "Figures/species_counts.png", species_counts_plot,
#   width = 10, height = 7, dpi = 300 )

#version 2:codes

species_order <- c(
  "ABCO", "PSME",           # firs
  "PIST", "PIPO", "PIED",   # pines
  "JUSC", "JUMO",           # junipers
  "QUUN", "QUGA",           # oaks
  "ACGL", "POTR", "SASC", "PRVI"  # all else
)

species_colors <- c(
  # firs: steel blue -> soft sky
  ABCO = "#2F5D8C", PSME = "#8DB8D9",
  # pines: forest -> sage -> pale sage
  PIST = "#2E6B4F", PIPO = "#78ca89", PIED = "#c8e9b7",
  # junipers: muted indigo -> lavender
  JUSC = "#7d67b1", JUMO = "#d4c6ee",
  # oaks: brick -> peach
  QUUN = "#A8402A", QUGA = "#ee8d60",
  # all else: mustard, brown, warm greys
  ACGL = "#E9C46A", POTR = "#8C6A4A", SASC = "#CFCBC3", PRVI = "#6B6B6B"
)
species_counts_a <- species_counts %>%
  filter(
    !is.na(SpeciesID),
    !SpeciesID %in% c("unknown", "NA", "N/A", "NA ")
  ) %>%
  mutate(SpeciesID = factor(SpeciesID, levels = species_order))

species_counts_plot <- ggplot(
  species_counts_a,
  aes(x = Treatment, y = Total_Trees, fill = SpeciesID)
) +
  geom_col(position = "stack", width = 0.5, color = "black", linewidth = 0.3) +
  scale_fill_manual(values = species_colors, drop = FALSE) +
  labs(
    x = NULL,
    y = "Number of Trees",
    fill = "Species"
  ) +
  theme_classic(base_size = 16) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

ggsave("Figures/species_counts.png", species_counts_plot,
       width = 10, height = 7, dpi = 300)



# (D4) Plot of species_counts of DBH by species and treatment of all Live Trees

live_species_by_treatment <- trees_2026 %>%
  filter(!is.na(Year), !is.na(Site), !is.na(TreeID)) %>%
  filter(
    !(toupper(trimws(as.character(Year))) %in% c("NA", "N/A", "")),
    !(toupper(trimws(as.character(Site))) %in% c("NA", "N/A", "")),
    !is.na(SpeciesID),
    !(toupper(trimws(as.character(SpeciesID))) %in% c("NA", "N/A", "")),
    !is.na(DBH)
  ) %>%
  filter(
    Site %in% c(treated_sites, untreated_sites),
    Tree_condition %in% c(1, 3, 7)
  ) %>%
  mutate(Treatment = case_when(
    Site %in% treated_sites ~ "Treated",
    Site %in% untreated_sites ~ "Untreated"
  )) %>%
  mutate(SpeciesID = factor(SpeciesID, levels = species_order))

View(live_species_by_treatment)

# live_species_by_treatment_plot <- ggplot(
#   live_species_by_treatment,
#   aes(x = DBH, fill = SpeciesID)
# ) +
#   geom_histogram(binwidth = 5, position = "stack") +
#   facet_wrap(~ Treatment, nrow = 1) +
#   scale_fill_manual(
#     values = c(species_colors, unknown = "#808080"),
#     drop = TRUE
#   ) +
#   labs(
#     x = "DBH",
#     y = "Tree count",
#     fill = "Species"
#   ) +
#   theme_classic(base_size = 16) +
#   theme(
#     axis.title = element_text(size = 18),
#     axis.text = element_text(size = 16),
#     legend.title = element_text(size = 16),
#     legend.text = element_text(size = 14)
#   )

  #version2 - codes

  live_species_by_treatment_plot <- ggplot(
  live_species_by_treatment,
  aes(x = DBH, fill = SpeciesID)
) +
  geom_histogram(binwidth = 5, position = "stack") +
  facet_wrap(~ Treatment, nrow = 1) +
  scale_fill_manual(
    values = c(species_colors, unknown = "#808080"),
    drop = TRUE
  ) +
  labs(
    x = "DBH",
    y = "Tree count",
    fill = "Species"
  ) +
  theme_classic(base_size = 16) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.background = element_blank(),
    strip.text = element_text(size = 20, face = "bold", hjust = 0.5)
  )
ggsave(
  "Figures/live_species_by_treatment.png",
  live_species_by_treatment_plot,
  width = 12, height = 6, dpi = 300
)

# (E) Big Appendix tables
# Treatment has Total, Treated, and Untreated rows for each species.
appendix_species_treatment_counts_by_group <- trees_2026 %>%
  filter(!is.na(Year), !is.na(Site), !is.na(TreeID), !is.na(SpeciesID)) %>%
  filter(
    !(toupper(trimws(as.character(Year))) %in% c("NA", "N/A", "")),
    !(toupper(trimws(as.character(Site))) %in% c("NA", "N/A", "")),
    !(toupper(trimws(as.character(SpeciesID))) %in% c("NA", "N/A", "")),
    Site %in% c(treated_sites, untreated_sites)
  ) %>%
  mutate(Treatment = case_when(
    Site %in% treated_sites ~ "Treated",
    Site %in% untreated_sites ~ "Untreated"
  )) %>%
  group_by(SpeciesID, Treatment) %>%
  summarise(
    Total_Count = n(),
    Live_Count = sum(Tree_condition %in% c(1, 3, 7), na.rm = TRUE),
    New_Death_Count = sum(Tree_condition == 6, na.rm = TRUE),
    .groups = "drop"
  )

appendix_species_totals <- appendix_species_treatment_counts_by_group %>%
  group_by(SpeciesID) %>%
  summarise(
    Total_Count = sum(Total_Count),
    Live_Count = sum(Live_Count),
    New_Death_Count = sum(New_Death_Count),
    .groups = "drop"
  ) %>%
  mutate(Treatment = "Total")

appendix_species_treatment_counts <- bind_rows(
  appendix_species_treatment_counts_by_group,
  appendix_species_totals
) %>%
  mutate(
    New_Death_Percent = if_else(
      New_Death_Count + Live_Count > 0,
      100 * New_Death_Count / (New_Death_Count + Live_Count),
      NA_real_
    ),
    Treatment = factor(Treatment, levels = c("Total", "Treated", "Untreated"))
  ) %>%
  select(
    SpeciesID, Treatment, Total_Count, Live_Count,
    New_Death_Count, New_Death_Percent
  )

appendix_all_species_treatment_totals <- appendix_species_treatment_counts_by_group %>%
  group_by(Treatment) %>%
  summarise(
    Total_Count = sum(Total_Count),
    Live_Count = sum(Live_Count),
    New_Death_Count = sum(New_Death_Count),
    .groups = "drop"
  ) %>%
  mutate(SpeciesID = "All Species")

appendix_all_species_total <- appendix_all_species_treatment_totals %>%
  summarise(
    Total_Count = sum(Total_Count),
    Live_Count = sum(Live_Count),
    New_Death_Count = sum(New_Death_Count),
    .groups = "drop"
  ) %>%
  mutate(SpeciesID = "All Species", Treatment = "Total")

appendix_species_treatment_counts <- bind_rows(
  appendix_species_treatment_counts,
  appendix_all_species_treatment_totals,
  appendix_all_species_total
) %>%
  mutate(
    New_Death_Percent = if_else(
      New_Death_Count + Live_Count > 0,
      100 * New_Death_Count / (New_Death_Count + Live_Count),
      NA_real_
    ),
    Treatment = factor(Treatment, levels = c("Total", "Treated", "Untreated"))
  ) %>%
  arrange(SpeciesID == "All Species", SpeciesID, Treatment)

View(appendix_species_treatment_counts)

unk <- trees_2026 %>%
  filter(SpeciesID == 'unknown')
print(unk) # cond is 9, it's been lost for a few years 

#(E1) Site composition table 

species_percentages_by_treatment <- trees_2026 %>%
  filter(!is.na(Year), !is.na(Site), !is.na(TreeID), !is.na(SpeciesID)) %>%
  filter(
    !(toupper(trimws(as.character(Year))) %in% c("NA", "N/A", "")),
    !(toupper(trimws(as.character(Site))) %in% c("NA", "N/A", "")),
    !(toupper(trimws(as.character(SpeciesID))) %in% c("NA", "N/A", "")),
    Site %in% c(treated_sites, untreated_sites)
  ) %>%
  mutate(Treatment = case_when(
    Site %in% treated_sites ~ "Treated",
    Site %in% untreated_sites ~ "Untreated"
  )) %>%
  group_by(Treatment, SpeciesID) %>%
  summarise(
    Total_Count = n(),
    Live_Count = sum(Tree_condition %in% c(1, 3, 7), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::complete(
    Treatment = c("Treated", "Untreated"),
    SpeciesID,
    fill = list(Total_Count = 0L, Live_Count = 0L)
  ) %>%
  group_by(Treatment) %>%
  mutate(
    Treatment_Total = sum(Total_Count),
    Treatment_Live = sum(Live_Count),
    Total_Percent = 100 * Total_Count / Treatment_Total,
    Live_Percent = if_else(
      Treatment_Live > 0,
      100 * Live_Count / Treatment_Live,
      0
    )
  ) %>%
  ungroup() %>%
  select(Treatment, SpeciesID, Total_Percent, Live_Percent) %>%
  tidyr::pivot_wider(
    names_from = Treatment,
    values_from = c(Total_Percent, Live_Percent),
    names_glue = "{Treatment}_{.value}",
    values_fill = 0
  ) %>%
  select(
    SpeciesID, Treated_Total_Percent, Treated_Live_Percent,
    Untreated_Total_Percent, Untreated_Live_Percent
  ) %>%
  arrange(SpeciesID)

View(species_percentages_by_treatment)