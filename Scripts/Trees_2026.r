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


## General population trends over time 
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




# Report Analysis 

#set up 
treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

View(trees_2026)
# .25 ha plots: 8
# ha plots: 2 "BTN4" "SFF2" // "SFS4" "SFF8"
# 3 ha per treatment 

living_2026 <- trees_2026 %>%
  filter(Year == 2026, Tree_condition %in% c(1))


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
    .groups = 'drop'
  )
View(summary_2026living)
# statistic: 355 living trees/ha (treated) vs 1,323.3 living trees/ha (untreated)

#(A1) Tree density/ha by site 
one_ha_sites <- c("BTN4", "SFS4", "SFF2", "SFF8")

tree_density_site <- trees_2026 %>%
  group_by(Site) %>%
  summarise(Total_Trees = n(), .groups = "drop") %>%
  mutate(
    Plot_Area_ha = if_else(Site %in% one_ha_sites, 1, 0.25),
    Density_ha = Total_Trees / Plot_Area_ha
  )
View(tree_density_site)

#(A2) Live tree density/ha by site
live_tree_density_site <- living_2026 %>%
  group_by(Site) %>%
  summarise(Total_Trees = n(), .groups = "drop") %>%
  mutate(
    Plot_Area_ha = if_else(Site %in% one_ha_sites, 1, 0.25),
    Density_ha = Total_Trees / Plot_Area_ha
  )
View(live_tree_density_site)


#(B) mortality per hectare (treated vs untreated)

