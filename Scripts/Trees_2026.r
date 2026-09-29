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


## (I) General population trends over time 
#View(trees_a)

tree_counts_over_time <- trees_a %>%
  filter(!is.na(Year), !is.na(Site), !is.na(TreeID)) %>%
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

View(new_mort_2026_treatment )

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

