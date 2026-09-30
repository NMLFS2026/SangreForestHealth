# Angie Taylor
# Sep 30, 2026
# Mort analysis

# load libraries
library(readxl)
library(dplyr)
library(ggplot2)

feat <- read_excel("Data/Tree_features_9_18_26.xlsx")
trees <- read_excel("Data/Trees_9_18_26.xlsx")

# add species
feat <- left_join(feat, trees[, c("TreeID", "SpeciesID")], by = "TreeID")

# site is the first part of the tree id
feat$Site <- sub("-.*", "", feat$TreeID)

# treated vs untreated
trt <- c("SFS4", "SFF1", "SFF5", "SFF7", "SFF8", "SFF10")
feat$Treatment <- ifelse(feat$Site %in% trt, "Treated", "Untreated")

# died = 4 or 6
feat$dead <- ifelse(feat$Tree_condition %in% c(4, 6), 1, 0)
# alive at start of yr (live, dalb, or died that yr)
feat$alive <- ifelse(feat$Tree_condition %in% c(1, 3, 4, 6, 7), 1, 0)

# take out first yr for each site bc no new mortality yet
yr1 <- feat %>% group_by(Site) %>% summarise(yr1 = min(Year))
feat <- left_join(feat, yr1, by = "Site")
feat <- filter(feat, Year > yr1)

spp <- c("PIPO", "ABCO", "PSME", "PIST", "PIED", "QUGA", "QUUN")
feat <- filter(feat, SpeciesID %in% spp)

mort <- feat %>%
  group_by(SpeciesID, Treatment, Year) %>%
  summarise(dead = sum(dead), alive = sum(alive))
mort$rate <- mort$dead / mort$alive * 100

mort

p1 <- ggplot(mort, aes(Year, rate, color = Treatment)) +
  geom_point() +
  geom_line() +
  facet_wrap(~SpeciesID) +
  scale_color_manual(values = c("darkgreen", "darkorange")) +
  ylab("Annual mortality (%)") +
  theme_bw()

p1

ggsave("Figures/mortality_spp_trt.png", p1, width = 10, height = 6)