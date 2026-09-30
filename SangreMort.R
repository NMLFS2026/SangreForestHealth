# Angie Taylor
# Sep 30, 2026
# Mort analysis


# load libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(lme4)

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
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  facet_wrap(~SpeciesID) +
  scale_color_manual(values = c("#CC5500", "#999999")) +
  labs(x = "", y = "Annual mortality (%)", color = "") +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "bottom")

p1

ggsave("Figures/mortality_spp_trt.png", p1, width = 10, height = 6)


# all species together, 2026 only
mort26 <- feat %>%
  filter(Year == 2026) %>%
  group_by(Treatment) %>%
  summarise(dead = sum(dead), alive = sum(alive))
mort26$rate <- mort26$dead / mort26$alive * 100

# label for top of bars
mort26$lab <- paste0(mort26$dead, " of ", mort26$alive, " trees\n= ",
                     round(mort26$rate, 1), "%")

mort26

p2 <- ggplot(mort26, aes(Treatment, rate, fill = Treatment)) +
  geom_col(width = 0.5, color = "gray30") +
  geom_text(aes(label = lab), vjust = -0.3, size = 3.5) +
  scale_fill_manual(values = c("#CC5500", "#999999")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(x = "", y = "2026 mortality (%)") +
  theme_classic() +
  theme(axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "none")

p2

ggsave("Figures/mortality_trt_2026.png", p2, width = 5, height = 4)


# stats - binomial glmm w site as random effect
# using 2025 and 2026 bc 2024 only has one treated site
site <- feat %>%
  filter(Year %in% c(2025, 2026)) %>%
  group_by(Site, Treatment, Year) %>%
  summarise(dead = sum(dead), alive = sum(alive))

site

# model with treatment
m1 <- glmer(cbind(dead, alive - dead) ~ Treatment + factor(Year) + (1 | Site),
            family = binomial, data = site)

# model without treatment
m0 <- glmer(cbind(dead, alive - dead) ~ factor(Year) + (1 | Site),
            family = binomial, data = site)

# likelihood ratio test - does treatment matter?
anova(m0, m1)

# does the treatment effect change between years?
m2 <- glmer(cbind(dead, alive - dead) ~ Treatment * factor(Year) + (1 | Site),
            family = binomial, data = site)
anova(m1, m2)

summary(m1)


# year interaction was significant so test each yr separately

# 2025
m25 <- glmer(cbind(dead, alive - dead) ~ Treatment + (1 | Site),
             family = binomial, data = filter(site, Year == 2025))
m25_0 <- glmer(cbind(dead, alive - dead) ~ 1 + (1 | Site),
               family = binomial, data = filter(site, Year == 2025))
anova(m25_0, m25)
summary(m25)

# 2026
m26 <- glmer(cbind(dead, alive - dead) ~ Treatment + (1 | Site),
             family = binomial, data = filter(site, Year == 2026))
m26_0 <- glmer(cbind(dead, alive - dead) ~ 1 + (1 | Site),
               family = binomial, data = filter(site, Year == 2026))
anova(m26_0, m26)
summary(m26)