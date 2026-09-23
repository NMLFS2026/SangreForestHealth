# Sangre Veg Lifeforms 
#9/23/2026
#Angie Taylor

# load libraries
library(readxl)
library(dplyr)
library(ggplot2)

# bring in data
veg <- read_excel("Data/VEG_features.xlsx")
species <- read_excel("Data/Species.xlsx")

# 2026 only
veg2026 <- veg %>% filter(Year == 2026)

# treated sites
treated <- c("SFS4V", "SFF1V", "SFF5V", "SFF7V", "SFF8V", "SFF10V")

#veg26 <- veg26 %>%
mutate(Site = sub("-.*", "", PlotID),
       Treatment = ifelse(Site %in% treated, "Treated", "Untreated"))

