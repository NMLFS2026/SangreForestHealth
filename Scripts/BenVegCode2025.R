# Github setup

usethis::create_github_token()

gitcreds::gitcreds_set()

usethis::use_git_ignore()


---
  title: "vegw2025"
output: html_document
date: "2025-10-21"
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
```{r}
setwd("~/Desktop")
file.exists("/Users/benmuher/Desktop")
library(dplyr)
library(ggplot2)
library(openxlsx)
library(tidyr)
library(stringr)
install.packages("gridExtra")
library(gridExtra)
library(cowplot)
library(patchwork)
library(vegan)
library(data.table)
library(RColorBrewer)
library(purrr)
library(openxlsx)
install.packages("writexl")
library(writexl)
#read in tree data
Veg<- read.xlsx("VEG_features.xlsx")
```
```{r}
### FOUND IT WAS EASIER TO FILTER BY PERSON ENTERING DATA (ME) THAN BY YEAR FOR SOME REASON 

treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

filtered_veg <- Veg %>%
  mutate(
    Site = trimws(toupper(Site)),
    Species = trimws(Species),
    CoverType = tolower(trimws(CoverType)),
    DataEntry = trimws(DataEntry)
  ) %>%
  filter(
    DataEntry == "Ben Muher",
    CoverType == "basal",
    !is.na(Species), Species != "N/A", Species != "",
    !is.na(CoverType), CoverType != "",
    Site %in% c(treated_sites, untreated_sites)
  ) %>%
  mutate(
    Treatment = case_when(
      Site %in% treated_sites ~ "Treated",
      Site %in% untreated_sites ~ "Untreated"
    )
  )

richness_summary <- filtered_veg %>%
  group_by(Site, Treatment) %>%
  summarise(
    richness = n_distinct(Species),
    .groups = "drop"
  )

if (length(unique(richness_summary$Treatment)) == 2) {
  print 
  cat("\nT-test comparing Basal Veg Species Richness between Treated and Untreated sites (2025):\n")
  (t.test(richness ~ Treatment, data = richness_summary))
} else {
  warning("T-test not run: Only one treatment group present in data.")
}

``` 
```{r}
treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

percent_data <- Veg %>%
  mutate(
    Site = trimws(toupper(Site)),
    CoverType = tolower(trimws(CoverType)),
    DataEntry = trimws(DataEntry),
    Percent = as.numeric(Percent)
  ) %>%
  filter(
    DataEntry == "Ben Muher",
    CoverType == "basal",
    !is.na(Percent),
    Site %in% c(treated_sites, untreated_sites)
  ) %>%
  mutate(
    Treatment = case_when(
      Site %in% treated_sites ~ "Treated",
      Site %in% untreated_sites ~ "Untreated"
    )
  )

table(percent_data$Treatment)

cat("\nT-test comparing Basal Percent Cover between Treated and Untreated sites (2025):\n")
t.test(Percent ~ Treatment, data = percent_data)
```
```{r}

treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

species_counts <- Veg %>%
  mutate(
    Site = trimws(toupper(Site)),
    CoverType = tolower(trimws(CoverType)),
    DataEntry = trimws(DataEntry),
    Species = trimws(Species)
  ) %>%
  filter(
    DataEntry == "Ben Muher",
    CoverType == "basal",
    !is.na(Species), Species != "", Species != "N/A",
    Site %in% c(treated_sites, untreated_sites)
  ) %>%
  mutate(
    Treatment = case_when(
      Site %in% treated_sites ~ "Treated",
      Site %in% untreated_sites ~ "Untreated"
    )
  ) %>%
  count(Treatment, Species)  # Count number of records per species per treatment group


ggplot(species_counts, aes(x = Treatment, y = n, fill = Species)) +
  geom_bar(stat = "identity", position = "stack") +
  ylim(0,350)+
  scale_fill_manual(values = c(
    "PENBAR" = "#1F77B4",
    "N/A" = "#808080",
    "ELYELY" = "#FF7F0E",
    "BERREP" = "#2CA02C",
    "ERISPE" = "#D62728",
    "SENERE" = "#9467BD",
    "BOEXXX" = "#8C564B",
    "PACFEN" = "#E377C2",
    "ANDSEP" = "#7F7F7F",
    "BROCIL" = "#BCBD22",
    "HETVIL" = "#17BECF",
    "GERRIC" = "#AEC7E8",
    "CARXXX" = "#FFBB78",
    "AMADIS" = "#98DF8A",
    "QUGA" = "#FF9896",
    "ABCO" = "#C5B0D5",
    "MUHMON" = "#C49C94",
    "DIECAN" = "#F7B6D2",
    "PSEMAC" = "#DBDB8D",
    "ARTLUD" = "#9EDAE5",
    "LAESCH" = "#393B79",
    "POAFEN" = "#637939",
    "ACHMIL" = "#8C6D31",
    "CEAFEN" = "#843C39",
    "VERTHA" = "#7B4173",
    "DRAXXX" = "#3182BD",
    "PIED" = "#E6550D",
    "HIEFEN" = "#31A354",
    "ALLCER" = "#756BB1",
    "CAMROT" = "#636363",
    "KOEMAC" = "#D6616B",
    "PIPO" = "#FCAE6B",
    "PHAHET" = "#BCBD22",
    "CONCAN" = "#17BECF",
    "PSME" = "#9EDAE5",
    "SENSPA" = "#393B79",
    "GERCAE" = "#637939",
    "CYMLEM" = "#8C6D31",
    "BERFEN" = "#843C39",
    "ANTMAR" = "#7B4173",
    "ERIDIV" = "#3182BD",
    "CLECOL" = "#E6550D",
    "SOLWRI" = "#31A354",
    "ARELAN" = "#756BB1",
    "BRIGRA" = "#636363",
    "TETARG" = "#D6616B",
    "PIST" = "#FCAE6B",
    "PAXMYR" = "#BCBD22",
    "DYSGRA" = "#17BECF",
    "COLPAR" = "#9EDAE5",
    "NAMDIC" = "#393B79",
    "COMDIA" = "#637939",
    "FORB" = "#8C6D31",
    "GALBOR" = "#843C39",
    "MIRLIN" = "#7B4173",
    "FRAVES" = "#3182BD",
    "SAXBRO" = "#E6550D",
    "LATLAN" = "#31A354",
    "ANTPAR" = "#756BB1",
    "ARCUVA" = "#636363",
    "SYMROT" = "#D6616B",
    "QUUN" = "#FCAE6B",
    "HELMUL" = "#BCBD22",
    "CHEPRA" = "#17BECF",
    "BOUGRA" = "#9EDAE5",
    "ERIFOR" = "#393B79",
    "CERMON" = "#637939",
    "MUHTRI" = "#8C6D31",
    "ERIVRE" = "#843C39",
    "APOAND" = "#7B4173",
    "NOCFEN" = "#3182BD",
    "SYMSPA" = "#E6550D",
    "ARTFRA" = "#31A354",
    "GOOOBL" = "#756BB1",
    "BRIEUP" = "#636363",
    "HEDDRU" = "#D6616B",
    "OENCES" = "#FCAE6B",
    "ERIJAM" = "#BCBD22",
    "EUPREV" = "#17BECF",
    "ERICAN" = "#9EDAE5",
    "ERYCAP" = "#393B79",
    "ERIFLA" = "#637939",
    "PENJAM" = "#8C6D31",
    "HYMRIC" = "#843C39",
    "BRASSI" = "#7B4173",
    "SENWOO" = "#3182BD",
    "DANSPI" = "#E6550D"
  ))+
  labs(
    title = "Basal Species Observation Counts (2025)",
    x = "",
    y = "Species Observation Count",
    fill = "Species"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12),
        legend.position = "none")
print(species_counts)
```
```{r}
treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")

# Filter and prepare data: only treated sites
species_counts_treated <- Veg %>%
  mutate(
    Site = trimws(toupper(Site)),
    CoverType = tolower(trimws(CoverType)),
    DataEntry = trimws(DataEntry),
    Species = trimws(Species)
  ) %>%
  filter(
    DataEntry == "Ben Muher",
    CoverType == "basal",
    !is.na(Species), Species != "", Species != "N/A",
    Site %in% treated_sites
  ) %>%
  count(Site, Species) 


ggplot(species_counts_treated, aes(x = Site, y = n, fill = Species)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c(
    "PENBAR" = "#1F77B4",
    "N/A" = "#808080",
    "ELYELY" = "#FF7F0E",
    "BERREP" = "#2CA02C",
    "ERISPE" = "#D62728",
    "SENERE" = "#9467BD",
    "BOEXXX" = "#8C564B",
    "PACFEN" = "#E377C2",
    "ANDSEP" = "#7F7F7F",
    "BROCIL" = "#BCBD22",
    "HETVIL" = "#17BECF",
    "GERRIC" = "#AEC7E8",
    "CARXXX" = "#FFBB78",
    "AMADIS" = "#98DF8A",
    "QUGA" = "#FF9896",
    "ABCO" = "#C5B0D5",
    "MUHMON" = "#C49C94",
    "DIECAN" = "#F7B6D2",
    "PSEMAC" = "#DBDB8D",
    "ARTLUD" = "#9EDAE5",
    "LAESCH" = "#393B79",
    "POAFEN" = "#637939",
    "ACHMIL" = "#8C6D31",
    "CEAFEN" = "#843C39",
    "VERTHA" = "#7B4173",
    "DRAXXX" = "#3182BD",
    "PIED" = "#E6550D",
    "HIEFEN" = "#31A354",
    "ALLCER" = "#756BB1",
    "CAMROT" = "#636363",
    "KOEMAC" = "#D6616B",
    "PIPO" = "#FCAE6B",
    "PHAHET" = "#BCBD22",
    "CONCAN" = "#17BECF",
    "PSME" = "#9EDAE5",
    "SENSPA" = "#393B79",
    "GERCAE" = "#637939",
    "CYMLEM" = "#8C6D31",
    "BERFEN" = "#843C39",
    "ANTMAR" = "#7B4173",
    "ERIDIV" = "#3182BD",
    "CLECOL" = "#E6550D",
    "SOLWRI" = "#31A354",
    "ARELAN" = "#756BB1",
    "BRIGRA" = "#636363",
    "TETARG" = "#D6616B",
    "PIST" = "#FCAE6B",
    "PAXMYR" = "#BCBD22",
    "DYSGRA" = "#17BECF",
    "COLPAR" = "#9EDAE5",
    "NAMDIC" = "#393B79",
    "COMDIA" = "#637939",
    "FORB" = "#8C6D31",
    "GALBOR" = "#843C39",
    "MIRLIN" = "#7B4173",
    "FRAVES" = "#3182BD",
    "SAXBRO" = "#E6550D",
    "LATLAN" = "#31A354",
    "ANTPAR" = "#756BB1",
    "ARCUVA" = "#636363",
    "SYMROT" = "#D6616B",
    "QUUN" = "#FCAE6B",
    "HELMUL" = "#BCBD22",
    "CHEPRA" = "#17BECF",
    "BOUGRA" = "#9EDAE5",
    "ERIFOR" = "#393B79",
    "CERMON" = "#637939",
    "MUHTRI" = "#8C6D31",
    "ERIVRE" = "#843C39",
    "APOAND" = "#7B4173",
    "NOCFEN" = "#3182BD",
    "SYMSPA" = "#E6550D",
    "ARTFRA" = "#31A354",
    "GOOOBL" = "#756BB1",
    "BRIEUP" = "#636363",
    "HEDDRU" = "#D6616B",
    "OENCES" = "#FCAE6B",
    "ERIJAM" = "#BCBD22",
    "EUPREV" = "#17BECF",
    "ERICAN" = "#9EDAE5",
    "ERYCAP" = "#393B79",
    "ERIFLA" = "#637939",
    "PENJAM" = "#8C6D31",
    "HYMRIC" = "#843C39",
    "BRASSI" = "#7B4173",
    "SENWOO" = "#3182BD",
    "DANSPI" = "#E6550D"
  )) +
  labs(
    title = "Basal Species Observation Counts per Treated Site (2025)",
    x = "Treated Site",
    y = "Species Observation Count",
    fill = "Species"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  
```
```{r}
treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")

# Sites where counts should be multiplied by 4
multiply_sites <- c("SFF1", "SFF10", "SFF5", "SFF7")

species_counts_treated <- Veg %>%
  mutate(
    Site = trimws(toupper(Site)),
    CoverType = tolower(trimws(CoverType)),
    DataEntry = trimws(DataEntry),
    Species = trimws(Species)
  ) %>%
  filter(
    DataEntry == "Ben Muher",
    CoverType == "basal",
    !is.na(Species), Species != "", Species != "N/A",
    Site %in% treated_sites
  ) %>%
  count(Site, Species) %>%
  mutate(
    n = if_else(Site %in% multiply_sites, n * 4, n)  # Multiply selectively
  )

treatedadjustedcount<-ggplot(species_counts_treated, aes(x = Site, y = n, fill = Species)) +
  geom_bar(stat = "identity", position = "stack") +
  ylim(0, 170) +
  scale_fill_manual(values = c(
    "PENBAR" = "#1F77B4",
    "N/A" = "#808080",
    "ELYELY" = "#FF7F0E",
    "BERREP" = "#2CA02C",
    "ERISPE" = "#D62728",
    "SENERE" = "#9467BD",
    "BOEXXX" = "#8C564B",
    "PACFEN" = "#E377C2",
    "ANDSEP" = "#7F7F7F",
    "BROCIL" = "#BCBD22",
    "HETVIL" = "#17BECF",
    "GERRIC" = "#AEC7E8",
    "CARXXX" = "#FFBB78",
    "AMADIS" = "#98DF8A",
    "QUGA" = "#FF9896",
    "ABCO" = "#C5B0D5",
    "MUHMON" = "#C49C94",
    "DIECAN" = "#F7B6D2",
    "PSEMAC" = "#DBDB8D",
    "ARTLUD" = "#9EDAE5",
    "LAESCH" = "#393B79",
    "POAFEN" = "#637939",
    "ACHMIL" = "#8C6D31",
    "CEAFEN" = "#843C39",
    "VERTHA" = "#7B4173",
    "DRAXXX" = "#3182BD",
    "PIED" = "#E6550D",
    "HIEFEN" = "#31A354",
    "ALLCER" = "#756BB1",
    "CAMROT" = "#636363",
    "KOEMAC" = "#D6616B",
    "PIPO" = "#FCAE6B",
    "PHAHET" = "#BCBD22",
    "CONCAN" = "#17BECF",
    "PSME" = "#9EDAE5",
    "SENSPA" = "#393B79",
    "GERCAE" = "#637939",
    "CYMLEM" = "#8C6D31",
    "BERFEN" = "#843C39",
    "ANTMAR" = "#7B4173",
    "ERIDIV" = "#3182BD",
    "CLECOL" = "#E6550D",
    "SOLWRI" = "#31A354",
    "ARELAN" = "#756BB1",
    "BRIGRA" = "#636363",
    "TETARG" = "#D6616B",
    "PIST" = "#FCAE6B",
    "PAXMYR" = "#BCBD22",
    "DYSGRA" = "#17BECF",
    "COLPAR" = "#9EDAE5",
    "NAMDIC" = "#393B79",
    "COMDIA" = "#637939",
    "FORB" = "#8C6D31",
    "GALBOR" = "#843C39",
    "MIRLIN" = "#7B4173",
    "FRAVES" = "#3182BD",
    "SAXBRO" = "#E6550D",
    "LATLAN" = "#31A354",
    "ANTPAR" = "#756BB1",
    "ARCUVA" = "#636363",
    "SYMROT" = "#D6616B",
    "QUUN" = "#FCAE6B",
    "HELMUL" = "#BCBD22",
    "CHEPRA" = "#17BECF",
    "BOUGRA" = "#9EDAE5",
    "ERIFOR" = "#393B79",
    "CERMON" = "#637939",
    "MUHTRI" = "#8C6D31",
    "ERIVRE" = "#843C39",
    "APOAND" = "#7B4173",
    "NOCFEN" = "#3182BD",
    "SYMSPA" = "#E6550D",
    "ARTFRA" = "#31A354",
    "GOOOBL" = "#756BB1",
    "BRIEUP" = "#636363",
    "HEDDRU" = "#D6616B",
    "OENCES" = "#FCAE6B",
    "ERIJAM" = "#BCBD22",
    "EUPREV" = "#17BECF",
    "ERICAN" = "#9EDAE5",
    "ERYCAP" = "#393B79",
    "ERIFLA" = "#637939",
    "PENJAM" = "#8C6D31",
    "HYMRIC" = "#843C39",
    "BRASSI" = "#7B4173",
    "SENWOO" = "#3182BD",
    "DANSPI" = "#E6550D"
  )) +
  labs(
    title = "Treated Adjusted/ha",
    x = "",
    y = "Count",
    fill = "Species"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) 
treatedadjustedcount
``` 
```{r}
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

# Sites where counts should be multiplied by 4
multiply_sites <- c("SFF3", "SFF4", "SFF6", "SFF9")

species_counts_untreated <- Veg %>%
  mutate(
    Site = trimws(toupper(Site)),
    CoverType = tolower(trimws(CoverType)),
    DataEntry = trimws(DataEntry),
    Species = trimws(Species)
  ) %>%
  filter(
    DataEntry == "Ben Muher",
    CoverType == "basal",
    !is.na(Species), Species != "", Species != "N/A",
    Site %in% untreated_sites
  ) %>%
  count(Site, Species) %>%
  mutate(
    n = if_else(Site %in% multiply_sites, n * 4, n)  # Multiply selectively
  )

untreatedadjustcount<-ggplot(species_counts_untreated, aes(x = Site, y = n, fill = Species)) +
  geom_bar(stat = "identity", position = "stack") +
  ylim(0, 170) +
  scale_fill_manual(values = c(
    "PENBAR" = "#1F77B4",
    "N/A" = "#808080",
    "ELYELY" = "#FF7F0E",
    "BERREP" = "#2CA02C",
    "ERISPE" = "#D62728",
    "SENERE" = "#9467BD",
    "BOEXXX" = "#8C564B",
    "PACFEN" = "#E377C2",
    "ANDSEP" = "#7F7F7F",
    "BROCIL" = "#BCBD22",
    "HETVIL" = "#17BECF",
    "GERRIC" = "#AEC7E8",
    "CARXXX" = "#FFBB78",
    "AMADIS" = "#98DF8A",
    "QUGA" = "#FF9896",
    "ABCO" = "#C5B0D5",
    "MUHMON" = "#C49C94",
    "DIECAN" = "#F7B6D2",
    "PSEMAC" = "#DBDB8D",
    "ARTLUD" = "#9EDAE5",
    "LAESCH" = "#393B79",
    "POAFEN" = "#637939",
    "ACHMIL" = "#8C6D31",
    "CEAFEN" = "#843C39",
    "VERTHA" = "#7B4173",
    "DRAXXX" = "#3182BD",
    "PIED" = "#E6550D",
    "HIEFEN" = "#31A354",
    "ALLCER" = "#756BB1",
    "CAMROT" = "#636363",
    "KOEMAC" = "#D6616B",
    "PIPO" = "#FCAE6B",
    "PHAHET" = "#BCBD22",
    "CONCAN" = "#17BECF",
    "PSME" = "#9EDAE5",
    "SENSPA" = "#393B79",
    "GERCAE" = "#637939",
    "CYMLEM" = "#8C6D31",
    "BERFEN" = "#843C39",
    "ANTMAR" = "#7B4173",
    "ERIDIV" = "#3182BD",
    "CLECOL" = "#E6550D",
    "SOLWRI" = "#31A354",
    "ARELAN" = "#756BB1",
    "BRIGRA" = "#636363",
    "TETARG" = "#D6616B",
    "PIST" = "#FCAE6B",
    "PAXMYR" = "#BCBD22",
    "DYSGRA" = "#17BECF",
    "COLPAR" = "#9EDAE5",
    "NAMDIC" = "#393B79",
    "COMDIA" = "#637939",
    "FORB" = "#8C6D31",
    "GALBOR" = "#843C39",
    "MIRLIN" = "#7B4173",
    "FRAVES" = "#3182BD",
    "SAXBRO" = "#E6550D",
    "LATLAN" = "#31A354",
    "ANTPAR" = "#756BB1",
    "ARCUVA" = "#636363",
    "SYMROT" = "#D6616B",
    "QUUN" = "#FCAE6B",
    "HELMUL" = "#BCBD22",
    "CHEPRA" = "#17BECF",
    "BOUGRA" = "#9EDAE5",
    "ERIFOR" = "#393B79",
    "CERMON" = "#637939",
    "MUHTRI" = "#8C6D31",
    "ERIVRE" = "#843C39",
    "APOAND" = "#7B4173",
    "NOCFEN" = "#3182BD",
    "SYMSPA" = "#E6550D",
    "ARTFRA" = "#31A354",
    "GOOOBL" = "#756BB1",
    "BRIEUP" = "#636363",
    "HEDDRU" = "#D6616B",
    "OENCES" = "#FCAE6B",
    "ERIJAM" = "#BCBD22",
    "EUPREV" = "#17BECF",
    "ERICAN" = "#9EDAE5",
    "ERYCAP" = "#393B79",
    "ERIFLA" = "#637939",
    "PENJAM" = "#8C6D31",
    "HYMRIC" = "#843C39",
    "BRASSI" = "#7B4173",
    "SENWOO" = "#3182BD",
    "DANSPI" = "#E6550D"
  )) +
  labs(
    title = "Untreated Adjusted/ha",
    x = "",
    y = "Count",
    fill = "Species"
  ) +
  theme_classic() +
  theme(legend.position = "none") 
theme(axis.text.x = element_text(angle = 0, hjust = 1)) 

untreatedadjustcount
```
```{r}
custom_colors <-scale_fill_manual(values = c(
  "PENBAR" = "#1F77B4",
  "N/A" = "#808080",
  "ELYELY" = "#FF7F0E",
  "BERREP" = "#2CA02C",
  "ERISPE" = "#D62728",
  "SENERE" = "#9467BD",
  "BOEXXX" = "#8C564B",
  "PACFEN" = "#E377C2",
  "ANDSEP" = "#7F7F7F",
  "BROCIL" = "#BCBD22",
  "HETVIL" = "#17BECF",
  "GERRIC" = "#AEC7E8",
  "CARXXX" = "#FFBB78",
  "AMADIS" = "#98DF8A",
  "QUGA" = "#FF9896",
  "ABCO" = "#C5B0D5",
  "MUHMON" = "#C49C94",
  "DIECAN" = "#F7B6D2",
  "PSEMAC" = "#DBDB8D",
  "ARTLUD" = "#9EDAE5",
  "LAESCH" = "#393B79",
  "POAFEN" = "#637939",
  "ACHMIL" = "#8C6D31",
  "CEAFEN" = "#843C39",
  "VERTHA" = "#7B4173",
  "DRAXXX" = "#3182BD",
  "PIED" = "#E6550D",
  "HIEFEN" = "#31A354",
  "ALLCER" = "#756BB1",
  "CAMROT" = "#636363",
  "KOEMAC" = "#D6616B",
  "PIPO" = "#FCAE6B",
  "PHAHET" = "#BCBD22",
  "CONCAN" = "#17BECF",
  "PSME" = "#9EDAE5",
  "SENSPA" = "#393B79",
  "GERCAE" = "#637939",
  "CYMLEM" = "#8C6D31",
  "BERFEN" = "#843C39",
  "ANTMAR" = "#7B4173",
  "ERIDIV" = "#3182BD",
  "CLECOL" = "#E6550D",
  "SOLWRI" = "#31A354",
  "ARELAN" = "#756BB1",
  "BRIGRA" = "#636363",
  "TETARG" = "#D6616B",
  "PIST" = "#FCAE6B",
  "PAXMYR" = "#BCBD22",
  "DYSGRA" = "#17BECF",
  "COLPAR" = "#9EDAE5",
  "NAMDIC" = "#393B79",
  "COMDIA" = "#637939",
  "FORB" = "#8C6D31",
  "GALBOR" = "#843C39",
  "MIRLIN" = "#7B4173",
  "FRAVES" = "#3182BD",
  "SAXBRO" = "#E6550D",
  "LATLAN" = "#31A354",
  "ANTPAR" = "#756BB1",
  "ARCUVA" = "#636363",
  "SYMROT" = "#D6616B",
  "QUUN" = "#FCAE6B",
  "HELMUL" = "#BCBD22",
  "CHEPRA" = "#17BECF",
  "BOUGRA" = "#9EDAE5",
  "ERIFOR" = "#393B79",
  "CERMON" = "#637939",
  "MUHTRI" = "#8C6D31",
  "ERIVRE" = "#843C39",
  "APOAND" = "#7B4173",
  "NOCFEN" = "#3182BD",
  "SYMSPA" = "#E6550D",
  "ARTFRA" = "#31A354",
  "GOOOBL" = "#756BB1",
  "BRIEUP" = "#636363",
  "HEDDRU" = "#D6616B",
  "OENCES" = "#FCAE6B",
  "ERIJAM" = "#BCBD22",
  "EUPREV" = "#17BECF",
  "ERICAN" = "#9EDAE5",
  "ERYCAP" = "#393B79",
  "ERIFLA" = "#637939",
  "PENJAM" = "#8C6D31",
  "HYMRIC" = "#843C39",
  "BRASSI" = "#7B4173",
  "SENWOO" = "#3182BD",
  "DANSPI" = "#E6550D"
))
treatedadjustedcount <- treatedadjustedcount + custom_colors
untreatedadjustcount <- untreatedadjustcount + custom_colors


# Extract the legend
legend <- get_legend(treatedadjustedcount)
# Remove legends from the plots themselves
treatedadjustedcount <- treatedadjustedcount + theme(legend.position = "none")
untreatedadjustcount <- untreatedadjustcount + theme(legend.position = "none")

# Arrange plots + legend
grid.arrange(
  arrangeGrob(treatedadjustedcount, untreatedadjustcount, ncol = 2),
  legend,
  ncol = 2,
  widths = c(4, 1) # adjust width ratio for plots vs legend
)


grid.arrange(treatedadjustedcount, untreatedadjustcount, ncol=2)
```
```{r}
treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")

# Sites where percent cover should be multiplied by 4
multiply_sites <- c("SFF1", "SFF10", "SFF5", "SFF7")

# Clean and calculate average percent cover
species_percent_treated <- Veg %>%
  mutate(
    Site = trimws(toupper(Site)),
    CoverType = tolower(trimws(CoverType)),
    DataEntry = trimws(DataEntry),
    Species = trimws(Species),
    Percent = suppressWarnings(as.numeric(Percent))  # Convert numeric, turn "T" to NA
  ) %>%
  filter(
    DataEntry == "Ben Muher",
    CoverType == "basal",
    !is.na(Species), Species != "", Species != "N/A",
    !is.na(Percent),
    Site %in% treated_sites
  ) %>%
  group_by(Site, Species) %>%
  summarise(TotalPercent = sum(Percent, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    # Divide by 6 for multiplied sites, 24 otherwise
    AvgPercentCover = if_else(Site %in% multiply_sites,
                              TotalPercent / 6,
                              TotalPercent / 24)
  )

treatedpct<-ggplot(species_percent_treated, aes(x = Site, y = AvgPercentCover, fill = Species)) +
  geom_bar(stat = "identity", position = "stack") +
  ylim(0, 10) +
  scale_fill_manual(values = c(
    "PENBAR" = "#1F77B4",
    "N/A" = "#808080",
    "ELYELY" = "#FF7F0E",
    "BERREP" = "#2CA02C",
    "ERISPE" = "#D62728",
    "SENERE" = "#9467BD",
    "BOEXXX" = "#8C564B",
    "PACFEN" = "#E377C2",
    "ANDSEP" = "#7F7F7F",
    "BROCIL" = "#BCBD22",
    "HETVIL" = "#17BECF",
    "GERRIC" = "#AEC7E8",
    "CARXXX" = "#FFBB78",
    "AMADIS" = "#98DF8A",
    "QUGA" = "#FF9896",
    "ABCO" = "#C5B0D5",
    "MUHMON" = "#C49C94",
    "DIECAN" = "#F7B6D2",
    "PSEMAC" = "#DBDB8D",
    "ARTLUD" = "#9EDAE5",
    "LAESCH" = "#393B79",
    "POAFEN" = "#637939",
    "ACHMIL" = "#8C6D31",
    "CEAFEN" = "#843C39",
    "VERTHA" = "#7B4173",
    "DRAXXX" = "#3182BD",
    "PIED" = "#E6550D",
    "HIEFEN" = "#31A354",
    "ALLCER" = "#756BB1",
    "CAMROT" = "#636363",
    "KOEMAC" = "#D6616B",
    "PIPO" = "#FCAE6B",
    "PHAHET" = "#BCBD22",
    "CONCAN" = "#17BECF",
    "PSME" = "#9EDAE5",
    "SENSPA" = "#393B79",
    "GERCAE" = "#637939",
    "CYMLEM" = "#8C6D31",
    "BERFEN" = "#843C39",
    "ANTMAR" = "#7B4173",
    "ERIDIV" = "#3182BD",
    "CLECOL" = "#E6550D",
    "SOLWRI" = "#31A354",
    "ARELAN" = "#756BB1",
    "BRIGRA" = "#636363",
    "TETARG" = "#D6616B",
    "PIST" = "#FCAE6B",
    "PAXMYR" = "#BCBD22",
    "DYSGRA" = "#17BECF",
    "COLPAR" = "#9EDAE5",
    "NAMDIC" = "#393B79",
    "COMDIA" = "#637939",
    "FORB" = "#8C6D31",
    "GALBOR" = "#843C39",
    "MIRLIN" = "#7B4173",
    "FRAVES" = "#3182BD",
    "SAXBRO" = "#E6550D",
    "LATLAN" = "#31A354",
    "ANTPAR" = "#756BB1",
    "ARCUVA" = "#636363",
    "SYMROT" = "#D6616B",
    "QUUN" = "#FCAE6B",
    "HELMUL" = "#BCBD22",
    "CHEPRA" = "#17BECF",
    "BOUGRA" = "#9EDAE5",
    "ERIFOR" = "#393B79",
    "CERMON" = "#637939",
    "MUHTRI" = "#8C6D31",
    "ERIVRE" = "#843C39",
    "APOAND" = "#7B4173",
    "NOCFEN" = "#3182BD",
    "SYMSPA" = "#E6550D",
    "ARTFRA" = "#31A354",
    "GOOOBL" = "#756BB1",
    "BRIEUP" = "#636363",
    "HEDDRU" = "#D6616B",
    "OENCES" = "#FCAE6B",
    "ERIJAM" = "#BCBD22",
    "EUPREV" = "#17BECF",
    "ERICAN" = "#9EDAE5",
    "ERYCAP" = "#393B79",
    "ERIFLA" = "#637939",
    "PENJAM" = "#8C6D31",
    "HYMRIC" = "#843C39",
    "BRASSI" = "#7B4173",
    "SENWOO" = "#3182BD",
    "DANSPI" = "#E6550D"
  )) +
  labs(
    title = "Avg % Basal Cover Treated PLOTs, 2025",
    x = "",
    y = "Avg % Basal Cover",
    fill = "Species"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))
treatedpct


```
```{r}
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

# Sites where percent cover should be multiplied by 4
multiply_sites <- c("SFF3", "SFF4", "SFF6", "SFF9")

# Clean and calculate average percent cover
species_percent_untreated <- Veg %>%
  mutate(
    Site = trimws(toupper(Site)),
    CoverType = tolower(trimws(CoverType)),
    DataEntry = trimws(DataEntry),
    Species = trimws(Species),
    Percent = suppressWarnings(as.numeric(Percent))  # Convert to numeric, turn "T" into NA
  ) %>%
  filter(
    DataEntry == "Ben Muher",
    CoverType == "basal",
    !is.na(Species), Species != "", Species != "N/A",
    !is.na(Percent),
    Site %in% untreated_sites
  ) %>%
  group_by(Site, Species) %>%
  summarise(TotalPercent = sum(Percent, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    # Divide by 6 for multiplied sites, 24 otherwise
    AvgPercentCover = if_else(Site %in% multiply_sites,
                              TotalPercent / 6,
                              TotalPercent / 24)
  )

species_percent_untreated %>% filter(Site == "SFF3")



species_percent_untreated <- species_percent_untreated %>%
  complete(Site = untreated_sites, Species, fill = list(TotalPercent = 0, AvgPercentCover = 0))

# Plot: stacked bar plot by species per untreated site
untreatedpct<-ggplot(species_percent_untreated, aes(x = Site, y = AvgPercentCover, fill = Species)) +
  geom_bar(stat = "identity", position = "stack") +
  ylim(0, 10) +
  scale_fill_manual(values = c(
    "PENBAR" = "#1F77B4",
    "N/A" = "#808080",
    "ELYELY" = "#FF7F0E",
    "BERREP" = "#2CA02C",
    "ERISPE" = "#D62728",
    "SENERE" = "#9467BD",
    "BOEXXX" = "#8C564B",
    "PACFEN" = "#E377C2",
    "ANDSEP" = "#7F7F7F",
    "BROCIL" = "#BCBD22",
    "HETVIL" = "#17BECF",
    "GERRIC" = "#AEC7E8",
    "CARXXX" = "#FFBB78",
    "AMADIS" = "#98DF8A",
    "QUGA" = "#FF9896",
    "ABCO" = "#C5B0D5",
    "MUHMON" = "#C49C94",
    "DIECAN" = "#F7B6D2",
    "PSEMAC" = "#DBDB8D",
    "ARTLUD" = "#9EDAE5",
    "LAESCH" = "#393B79",
    "POAFEN" = "#637939",
    "ACHMIL" = "#8C6D31",
    "CEAFEN" = "#843C39",
    "VERTHA" = "#7B4173",
    "DRAXXX" = "#3182BD",
    "PIED" = "#E6550D",
    "HIEFEN" = "#31A354",
    "ALLCER" = "#756BB1",
    "CAMROT" = "#636363",
    "KOEMAC" = "#D6616B",
    "PIPO" = "#FCAE6B",
    "PHAHET" = "#BCBD22",
    "CONCAN" = "#17BECF",
    "PSME" = "#9EDAE5",
    "SENSPA" = "#393B79",
    "GERCAE" = "#637939",
    "CYMLEM" = "#8C6D31",
    "BERFEN" = "#843C39",
    "ANTMAR" = "#7B4173",
    "ERIDIV" = "#3182BD",
    "CLECOL" = "#E6550D",
    "SOLWRI" = "#31A354",
    "ARELAN" = "#756BB1",
    "BRIGRA" = "#636363",
    "TETARG" = "#D6616B",
    "PIST" = "#FCAE6B",
    "PAXMYR" = "#BCBD22",
    "DYSGRA" = "#17BECF",
    "COLPAR" = "#9EDAE5",
    "NAMDIC" = "#393B79",
    "COMDIA" = "#637939",
    "FORB" = "#8C6D31",
    "GALBOR" = "#843C39",
    "MIRLIN" = "#7B4173",
    "FRAVES" = "#3182BD",
    "SAXBRO" = "#E6550D",
    "LATLAN" = "#31A354",
    "ANTPAR" = "#756BB1",
    "ARCUVA" = "#636363",
    "SYMROT" = "#D6616B",
    "QUUN" = "#FCAE6B",
    "HELMUL" = "#BCBD22",
    "CHEPRA" = "#17BECF",
    "BOUGRA" = "#9EDAE5",
    "ERIFOR" = "#393B79",
    "CERMON" = "#637939",
    "MUHTRI" = "#8C6D31",
    "ERIVRE" = "#843C39",
    "APOAND" = "#7B4173",
    "NOCFEN" = "#3182BD",
    "SYMSPA" = "#E6550D",
    "ARTFRA" = "#31A354",
    "GOOOBL" = "#756BB1",
    "BRIEUP" = "#636363",
    "HEDDRU" = "#D6616B",
    "OENCES" = "#FCAE6B",
    "ERIJAM" = "#BCBD22",
    "EUPREV" = "#17BECF",
    "ERICAN" = "#9EDAE5",
    "ERYCAP" = "#393B79",
    "ERIFLA" = "#637939",
    "PENJAM" = "#8C6D31",
    "HYMRIC" = "#843C39",
    "BRASSI" = "#7B4173",
    "SENWOO" = "#3182BD",
    "DANSPI" = "#E6550D"
  )) +
  labs(
    title = "Avg % Basal Cover Unreated PLOTs, 2025",
    x = "",
    y = "Avg % Basal Cover",
    fill = "Species"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))
untreatedpct


```
```{r}
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

# Sites where percent cover should be multiplied by 4
multiply_sites <- c("SFF3", "SFF4", "SFF6", "SFF9")

# Clean and calculate summed percent cover
species_percent_untreated_raw <- Veg %>%
  mutate(
    Site = trimws(toupper(Site)),
    CoverType = tolower(trimws(CoverType)),
    DataEntry = trimws(DataEntry),
    Species = trimws(Species),
    Percent = suppressWarnings(as.numeric(Percent))  # Convert numeric, turn "T" into NA
  ) %>%
  filter(
    DataEntry == "Ben Muher",
    CoverType == "basal",
    !is.na(Species), Species != "", Species != "N/A",
    !is.na(Percent),
    Site %in% untreated_sites
  ) %>%
  group_by(Site, Species) %>%
  summarise(TotalPercent = sum(Percent, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    # Multiply by 4 for selected sites (if desired)
  )

untreatedcount<-ggplot(species_percent_untreated_raw, aes(x = Site, y = TotalPercent, fill = Species)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c(
    "PENBAR" = "#1F77B4",
    "N/A" = "#808080",
    "ELYELY" = "#FF7F0E",
    "BERREP" = "#2CA02C",
    "ERISPE" = "#D62728",
    "SENERE" = "#9467BD",
    "BOEXXX" = "#8C564B",
    "PACFEN" = "#E377C2",
    "ANDSEP" = "#7F7F7F",
    "BROCIL" = "#BCBD22",
    "HETVIL" = "#17BECF",
    "GERRIC" = "#AEC7E8",
    "CARXXX" = "#FFBB78",
    "AMADIS" = "#98DF8A",
    "QUGA" = "#FF9896",
    "ABCO" = "#C5B0D5",
    "MUHMON" = "#C49C94",
    "DIECAN" = "#F7B6D2",
    "PSEMAC" = "#DBDB8D",
    "ARTLUD" = "#9EDAE5",
    "LAESCH" = "#393B79",
    "POAFEN" = "#637939",
    "ACHMIL" = "#8C6D31",
    "CEAFEN" = "#843C39",
    "VERTHA" = "#7B4173",
    "DRAXXX" = "#3182BD",
    "PIED" = "#E6550D",
    "HIEFEN" = "#31A354",
    "ALLCER" = "#756BB1",
    "CAMROT" = "#636363",
    "KOEMAC" = "#D6616B",
    "PIPO" = "#FCAE6B",
    "PHAHET" = "#BCBD22",
    "CONCAN" = "#17BECF",
    "PSME" = "#9EDAE5",
    "SENSPA" = "#393B79",
    "GERCAE" = "#637939",
    "CYMLEM" = "#8C6D31",
    "BERFEN" = "#843C39",
    "ANTMAR" = "#7B4173",
    "ERIDIV" = "#3182BD",
    "CLECOL" = "#E6550D",
    "SOLWRI" = "#31A354",
    "ARELAN" = "#756BB1",
    "BRIGRA" = "#636363",
    "TETARG" = "#D6616B",
    "PIST" = "#FCAE6B",
    "PAXMYR" = "#BCBD22",
    "DYSGRA" = "#17BECF",
    "COLPAR" = "#9EDAE5",
    "NAMDIC" = "#393B79",
    "COMDIA" = "#637939",
    "FORB" = "#8C6D31",
    "GALBOR" = "#843C39",
    "MIRLIN" = "#7B4173",
    "FRAVES" = "#3182BD",
    "SAXBRO" = "#E6550D",
    "LATLAN" = "#31A354",
    "ANTPAR" = "#756BB1",
    "ARCUVA" = "#636363",
    "SYMROT" = "#D6616B",
    "QUUN" = "#FCAE6B",
    "HELMUL" = "#BCBD22",
    "CHEPRA" = "#17BECF",
    "BOUGRA" = "#9EDAE5",
    "ERIFOR" = "#393B79",
    "CERMON" = "#637939",
    "MUHTRI" = "#8C6D31",
    "ERIVRE" = "#843C39",
    "APOAND" = "#7B4173",
    "NOCFEN" = "#3182BD",
    "SYMSPA" = "#E6550D",
    "ARTFRA" = "#31A354",
    "GOOOBL" = "#756BB1",
    "BRIEUP" = "#636363",
    "HEDDRU" = "#D6616B",
    "OENCES" = "#FCAE6B",
    "ERIJAM" = "#BCBD22",
    "EUPREV" = "#17BECF",
    "ERICAN" = "#9EDAE5",
    "ERYCAP" = "#393B79",
    "ERIFLA" = "#637939",
    "PENJAM" = "#8C6D31",
    "HYMRIC" = "#843C39",
    "BRASSI" = "#7B4173",
    "SENWOO" = "#3182BD",
    "DANSPI" = "#E6550D"
  )) +
  labs(
    title = "Avg % Basal Cover Untreated PLOTs, 2025",
    x = "",
    y = "Avg % Basal Cover",
    fill = "Species"
  ) +
  theme_classic() +
  theme(legend.position = "none")
theme(axis.text.x = element_text(angle = 0, hjust = 1))

untreatedcount

```
```{r}
grid.arrange(treatedpct, untreatedpct, ncol=2)
```
```{r}
Lf25<- read.xlsx("/Users/benmuher/Desktop/25basebig.xlsx")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

# Clean and calculate summed percent cover
lifeform_percent_untreated_raw <- Lf25 %>%
  mutate(
    Site = trimws(toupper(Site)),
    CoverType = tolower(trimws(CoverType)),
    DataEntry = trimws(DataEntry),
    Lifeform = trimws(Lifeform),
    Percent = suppressWarnings(as.numeric(Percent))  # Convert numeric, turn "T" into NA
  ) %>%
  filter(
    DataEntry == "Ben Muher",
    CoverType == "basal",
    !is.na(Percent),
    Site %in% untreated_sites
  ) %>%
  group_by(Site, Lifeform) %>%
  summarise(TotalPercent = sum(Percent, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    TotalPercent = case_when(
      Site %in% c("BTN4", "SFF2") ~ TotalPercent / 24,
      Site %in% c("SFF3", "SFF4", "SFF6", "SFF9") ~ TotalPercent / 6,
      TRUE ~ TotalPercent  # fallback in case another site appears
    )
  ) %>%
  # Fill in missing sites/lifeforms with 0
  complete(Site = untreated_sites, Lifeform, fill = list(TotalPercent = 0))


lfun<-ggplot(lifeform_percent_untreated_raw, aes(x = Site, y = TotalPercent, fill = Lifeform)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = NULL,
    x = "",
    y = NULL,
    fill = "Lifeform"
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


```
```{r}
Lf25<- read.xlsx("/Users/benmuher/Desktop/25basebig.xlsx")
treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")

# Clean and calculate summed percent cover
lifeform_percent_treated_raw <- Lf25 %>%
  mutate(
    Site = trimws(toupper(Site)),
    CoverType = tolower(trimws(CoverType)),
    DataEntry = trimws(DataEntry),
    Lifeform = trimws(Lifeform),
    Percent = suppressWarnings(as.numeric(Percent))  # Convert numeric, turn "T" into NA
  ) %>%
  filter(
    DataEntry == "Ben Muher",
    CoverType == "basal",
    !is.na(Percent),
    Site %in% treated_sites
  ) %>%
  group_by(Site, Lifeform) %>%
  summarise(TotalPercent = sum(Percent, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    TotalPercent = case_when(
      Site %in% c("SFS4", "SFF8") ~ TotalPercent / 24,
      Site %in% c("SFF1", "SFF10", "SFF5", "SFF7") ~ TotalPercent / 6,
      TRUE ~ TotalPercent  # fallback in case another site appears
    )
  )

lftr<-ggplot(lifeform_percent_treated_raw, aes(x = Site, y = TotalPercent, fill = Lifeform)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = NULL,
    x = "",
    y = "Avg % Basal Cover",
    fill = "Lifeform"
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

lfun

lftr
```
```{r}
install.packages("patchwork")

lfboth<-(lftr + lfun) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "right")
lfboth

```
```{r}
Lfair25<- read.xlsx("/Users/benmuher/Desktop/Air25.xlsx")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

# Clean and calculate summed percent cover
lifeform_percent_untreated_rawair <- Lfair25 %>%
  mutate(
    Site = trimws(toupper(Site)),
    CoverType = tolower(trimws(CoverType)),
    DataEntry = trimws(DataEntry),
    Lifeform = trimws(Lifeform),
    Percent = suppressWarnings(as.numeric(Percent))  # Convert numeric, turn "T" into NA
  ) %>%
  filter(
    DataEntry == "Ben Muher",
    !is.na(Percent),
    Site %in% untreated_sites
  ) %>%
  group_by(Site, Lifeform) %>%
  summarise(TotalPercent = sum(Percent, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    TotalPercent = case_when(
      Site %in% c("BTN4", "SFF2") ~ TotalPercent / 24,
      Site %in% c("SFF3", "SFF4", "SFF6", "SFF9") ~ TotalPercent / 6,
      TRUE ~ TotalPercent  # fallback in case another site appears
    )
  ) %>%
  # Fill in missing sites/lifeforms with 0
  complete(Site = untreated_sites, Lifeform, fill = list(TotalPercent = 0))

lfairuntreated<-ggplot(lifeform_percent_untreated_rawair, aes(x = Site, y = TotalPercent, fill = Lifeform)) +
  ylim(0,50)+
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = NULL,
    x = "",
    y = NULL,
    fill = "Lifeform"
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

print(lfairuntreated)
```
```{r}
Lfair25<- read.xlsx("/Users/benmuher/Desktop/Air25.xlsx")
treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")

# Clean and calculate summed percent cover
lifeform_percent_treated_rawair <- Lfair25 %>%
  mutate(
    Site = trimws(toupper(Site)),
    CoverType = tolower(trimws(CoverType)),
    DataEntry = trimws(DataEntry),
    Lifeform = trimws(Lifeform),
    Percent = suppressWarnings(as.numeric(Percent))  # Convert numeric, turn "T" into NA
  ) %>%
  filter(
    DataEntry == "Ben Muher",
    !is.na(Percent),
    Site %in% treated_sites
  ) %>%
  group_by(Site, Lifeform) %>%
  summarise(TotalPercent = sum(Percent, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    TotalPercent = case_when(
      Site %in% c("SFS4", "SFF8") ~ TotalPercent / 24,
      Site %in% c("SFF1", "SFF10", "SFF5", "SFF7") ~ TotalPercent / 6,
      TRUE ~ TotalPercent  # fallback in case another site appears
    )
  )

lftreatedair<-ggplot(lifeform_percent_treated_rawair, aes(x = Site, y = TotalPercent, fill = Lifeform)) +
  ylim(0,50)+
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = NULL,
    x = "",
    y = "Avg % Basal Cover",
    fill = "Lifeform"
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
lftreatedair
```

```{r}
(lftreatedair + lfairuntreated) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

```
```{r fig.width=8, fig.height=6}
Lfair25<- read.xlsx("/Users/benmuher/Desktop/Air25.xlsx")

treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

data2 <-read.xlsx("/Users/benmuher/Desktop/Air25.xlsx") %>%
  mutate(
    Percent = as.numeric(Percent),
    Treatment = case_when(
      Site %in% treated_sites ~ "Treated",
      Site %in% untreated_sites ~ "Untreated",
      TRUE ~ NA_character_
    )
  )

summ <- data2 %>%
  group_by(Treatment, Lifeform) %>%
  summarise(TotalPercent = sum(Percent, na.rm = TRUE), .groups = "drop")

summd5 <- summ %>%
  mutate(
    ScaledPercent = case_when(
      Treatment == "Treated" ~ TotalPercent / 72,
      Treatment == "Untreated" ~ TotalPercent / 71,
      TRUE ~ NA_real_
    )
  )

lfgoodair<-ggplot(summd5, aes(x = Treatment, y = ScaledPercent, fill = Lifeform)) +
  geom_col(width = 0.8) +
  labs(
    x = NULL,
    y = "Mean % cover per plot",
    fill = "Lifeform",
    title = "Aerial"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15),  
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),  
    plot.title = element_text(size = 18)
  )


lfgoodair


```
```{r}
###With error bars
Lfair25<- read.xlsx("/Users/benmuher/Desktop/Air25.xlsx")

treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

data2 <-read.xlsx("/Users/benmuher/Desktop/Air25.xlsx") %>%
  mutate(
    Percent = as.numeric(Percent),
    Treatment = case_when(
      Site %in% treated_sites ~ "Treated",
      Site %in% untreated_sites ~ "Untreated",
      TRUE ~ NA_character_
    )
  )

summd6 <- data2 %>%
  group_by(Treatment, Lifeform) %>%
  summarise(
    TotalPercent = sum(Percent, na.rm = TRUE),
    mean = mean(Percent, na.rm = TRUE),
    sd = sd(Percent, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    ScaledPercent = case_when(
      Treatment == "Treated" ~ TotalPercent / 72,
      Treatment == "Untreated" ~ TotalPercent / 71,
      TRUE ~ NA_real_
    ),
    
    # SE of scaled values
    se_scaled = case_when(
      Treatment == "Treated" ~ se / 72,
      Treatment == "Untreated" ~ se / 71,
      TRUE ~ NA_real_
    )
  )

meanlfairSE<-ggplot(summd6, aes(x = Treatment, y = ScaledPercent, fill = Lifeform)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.8) +
  geom_errorbar(
    aes(ymin = ScaledPercent - se,
        ymax = ScaledPercent + se),
    width = 0.2,
    position = position_dodge(width = 0.8)
  ) +
  labs(
    x = NULL,
    y = "Mean % cover per plot",
    fill = "Lifeform",
    title = "Aerial"
  ) +
  theme_classic()
meanlfairSE

```
```{r}
##Stacked error bars, I was having issues so I just manually added from above 
summ_tot <- summd6 %>%
  group_by(Treatment) %>%
  summarise(
    ScaledPercent = sum(ScaledPercent),
    .groups = "drop"
  )

# Example manual errors
summ_tot <- data.frame(
  Treatment = c("Treated", "Untreated"),
  ScaledPercent = c(sum(summd6$ScaledPercent[summd6$Treatment=="Treated"]),
                    sum(summd6$ScaledPercent[summd6$Treatment=="Untreated"])),
  my_error = c(4.781935282, 6.126412861)   # <-- manually set error bars
)



StackedmeanlfairSE<-ggplot(summd6, aes(x = Treatment, y = ScaledPercent, fill = Lifeform)) +
  geom_col(position = "stack", width = 0.8) +
  
  geom_errorbar(
    data = summ_tot,
    inherit.aes = FALSE,  # important: don't try to map fill from main dataset
    aes(x = Treatment,
        ymin = ScaledPercent - my_error,
        ymax = ScaledPercent + my_error),
    width = 0.2
  ) +
  
  labs(
    x = NULL,
    y = "Mean % cover per plot",
    fill = "Lifeform",
    title = "Aerial"
  ) +
  theme_classic()

StackedmeanlfairSE
```

```{r fig.width=8, fig.height=6}
Lf25<- read.xlsx("/Users/benmuher/Desktop/25basebig.xlsx")
treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

data3 <-Lf25 %>%
  mutate(
    Percent = as.numeric(Percent),
    Treatment = case_when(
      Site %in% treated_sites ~ "Treated",
      Site %in% untreated_sites ~ "Untreated",
      TRUE ~ NA_character_
    )
  )

summd3 <- data3 %>%
  group_by(Treatment, Lifeform) %>%
  summarise(TotalPercent = sum(Percent, na.rm = TRUE), .groups = "drop")

summd4 <- summd3 %>%
  mutate(
    ScaledPercent = case_when(
      Treatment == "Treated" ~ TotalPercent / 72,
      Treatment == "Untreated" ~ TotalPercent / 71,
      TRUE ~ NA_real_
    )
  )

lfgoodbase<-ggplot(summd4, aes(x = Treatment, y = ScaledPercent, fill = Lifeform)) +
  geom_col(width = 0.8) +
  labs(
    x = NULL,
    y = "Mean % cover per plot",
    fill = "Lifeform",
    title = "Basal"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15),  
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),  
    plot.title = element_text(size = 18)
  )+ 
  theme(legend.position = "none")


print(lfgoodbase)


```
```{r}
###With error bars
Lf25<- read.xlsx("/Users/benmuher/Desktop/25basebig.xlsx")
treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

data3 <-Lf25 %>%
  mutate(
    Percent = as.numeric(Percent),
    Treatment = case_when(
      Site %in% treated_sites ~ "Treated",
      Site %in% untreated_sites ~ "Untreated",
      TRUE ~ NA_character_
    )
  )

summd7 <- data3 %>%
  group_by(Treatment, Lifeform) %>%
  summarise(
    TotalPercent = sum(Percent, na.rm = TRUE),
    mean = mean(Percent, na.rm = TRUE),
    sd = sd(Percent, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    ScaledPercent = case_when(
      Treatment == "Treated" ~ TotalPercent / 72,
      Treatment == "Untreated" ~ TotalPercent / 71,
      TRUE ~ NA_real_
    ),
    
    # SE of scaled values
    se_scaled = case_when(
      Treatment == "Treated" ~ se / 72,
      Treatment == "Untreated" ~ se / 71,
      TRUE ~ NA_real_
    )
  )

meanlfbaseSE<-ggplot(summd7, aes(x = Treatment, y = ScaledPercent, fill = Lifeform)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.8) +
  geom_errorbar(
    aes(ymin = ScaledPercent - se,
        ymax = ScaledPercent + se),
    width = 0.2,
    position = position_dodge(width = 0.8)
  ) +
  labs(
    x = NULL,
    y = "Mean % cover per plot",
    fill = "Lifeform",
    title = "Basal"
  ) +
  theme_classic()
meanlfbaseSE
```
```{r}
##Stacked error bars, I was having issues so I just manually added from above 
summ_totbase <- summd7 %>%
  group_by(Treatment) %>%
  summarise(
    ScaledPercent = sum(ScaledPercent),
    .groups = "drop"
  )

# Example manual errors
summ_totbase <- data.frame(
  Treatment = c("Treated", "Untreated"),
  ScaledPercent = c(sum(summd7$ScaledPercent[summd7$Treatment=="Treated"]),
                    sum(summd7$ScaledPercent[summd7$Treatment=="Untreated"])),
  my_error = c(0.478860291, 1.132214118)   # <-- manually set error bars
)

StackedmeanlfbaseSE<-ggplot(summd7, aes(x = Treatment, y = ScaledPercent, fill = Lifeform)) +
  geom_col(position = "stack", width = 0.8) +
  geom_errorbar(
    data = summ_totbase,
    inherit.aes = FALSE,  # important: don't try to map fill from main dataset
    aes(x = Treatment,
        ymin = ScaledPercent - my_error,
        ymax = ScaledPercent + my_error),
    width = 0.2
  ) +
  
  labs(
    x = NULL,
    y = "Mean % cover per plot",
    fill = "Lifeform",
    title = "Basal"
  ) +
  theme_classic()

StackedmeanlfbaseSE
```
```{r}
# Remove legend AND removes scale guides from dbhtr
lfgoodbase2 <- lfgoodbase + theme(legend.position = "none")
lfgoodair2 <- lfgoodair + theme(legend.position = "right")

(lfgoodbase2 + lfgoodair2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

# Combine
goodlftotal<-(lfgoodbase2 + lfgoodair2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

```
```{r}
###for stacked summed error bars 
# Remove legend AND removes scale guides from dbhtr
StackedmeanlfbaseSE2 <- StackedmeanlfbaseSE + theme(legend.position = "none")
StackedmeanlfairSE2 <- StackedmeanlfairSE + theme(legend.position = "right")

(StackedmeanlfbaseSE2 + StackedmeanlfairSE2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

# Combine
SEstackedlftotal<-(StackedmeanlfbaseSE2 + StackedmeanlfairSE2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

SEstackedlftotal

```
```{r}
custom_colors <-scale_fill_manual(values = c(
  "PENBAR" = "#1F77B4",
  "N/A" = "#808080",
  "ELYELY" = "#FF7F0E",
  "BERREP" = "#2CA02C",
  "ERISPE" = "#D62728",
  "SENERE" = "#9467BD",
  "BOEXXX" = "#8C564B",
  "PACFEN" = "#E377C2",
  "ANDSEP" = "#7F7F7F",
  "BROCIL" = "#BCBD22",
  "HETVIL" = "#17BECF",
  "GERRIC" = "#AEC7E8",
  "CARXXX" = "#FFBB78",
  "AMADIS" = "#98DF8A",
  "QUGA" = "#FF9896",
  "ABCO" = "#C5B0D5",
  "MUHMON" = "#C49C94",
  "DIECAN" = "#F7B6D2",
  "PSEMAC" = "#DBDB8D",
  "ARTLUD" = "#9EDAE5",
  "LAESCH" = "#393B79",
  "POAFEN" = "#637939",
  "ACHMIL" = "#8C6D31",
  "CEAFEN" = "#843C39",
  "VERTHA" = "#7B4173",
  "DRAXXX" = "#3182BD",
  "PIED" = "#E6550D",
  "HIEFEN" = "#31A354",
  "ALLCER" = "#756BB1",
  "CAMROT" = "#636363",
  "KOEMAC" = "#D6616B",
  "PIPO" = "#FCAE6B",
  "PHAHET" = "#BCBD22",
  "CONCAN" = "#17BECF",
  "PSME" = "#9EDAE5",
  "SENSPA" = "#393B79",
  "GERCAE" = "#637939",
  "CYMLEM" = "#8C6D31",
  "BERFEN" = "#843C39",
  "ANTMAR" = "#7B4173",
  "ERIDIV" = "#3182BD",
  "CLECOL" = "#E6550D",
  "SOLWRI" = "#31A354",
  "ARELAN" = "#756BB1",
  "BRIGRA" = "#636363",
  "TETARG" = "#D6616B",
  "PIST" = "#FCAE6B",
  "PAXMYR" = "#BCBD22",
  "DYSGRA" = "#17BECF",
  "COLPAR" = "#9EDAE5",
  "NAMDIC" = "#393B79",
  "COMDIA" = "#637939",
  "FORB" = "#8C6D31",
  "GALBOR" = "#843C39",
  "MIRLIN" = "#7B4173",
  "FRAVES" = "#3182BD",
  "SAXBRO" = "#E6550D",
  "LATLAN" = "#31A354",
  "ANTPAR" = "#756BB1",
  "ARCUVA" = "#636363",
  "SYMROT" = "#D6616B",
  "QUUN" = "#FCAE6B",
  "HELMUL" = "#BCBD22",
  "CHEPRA" = "#17BECF",
  "BOUGRA" = "#9EDAE5",
  "ERIFOR" = "#393B79",
  "CERMON" = "#637939",
  "MUHTRI" = "#8C6D31",
  "ERIVRE" = "#843C39",
  "APOAND" = "#7B4173",
  "NOCFEN" = "#3182BD",
  "SYMSPA" = "#E6550D",
  "ARTFRA" = "#31A354",
  "GOOOBL" = "#756BB1",
  "BRIEUP" = "#636363",
  "HEDDRU" = "#D6616B",
  "OENCES" = "#FCAE6B",
  "ERIJAM" = "#BCBD22",
  "EUPREV" = "#17BECF",
  "ERICAN" = "#9EDAE5",
  "ERYCAP" = "#393B79",
  "ERIFLA" = "#637939",
  "PENJAM" = "#8C6D31",
  "HYMRIC" = "#843C39",
  "BRASSI" = "#7B4173",
  "SENWOO" = "#3182BD",
  "DANSPI" = "#E6550D"
))
treatedpct <- ggplot(data = species_percent_treated, aes(x = Site, y = AvgPercentCover, fill = Species)) +
  geom_bar(stat = "identity")
species_percent_treated$Species <- factor(species_percent_treated$Species, levels = names(custom_colors$values))
untreatedpct <- ggplot(data = species_percent_untreated, aes(x = Site, y = AvgPercentCover, fill = Species)) +
  geom_bar(stat = "identity")
species_percent_untreated$Species <- factor(species_percent_untreated$Species, levels = names(custom_colors$values))

treatedpct <- treatedpct + custom_colors
untreatedpct <- untreatedpct + custom_colors
legend <- get_legend(treatedpct)

treatedpct <- treatedpct + theme(legend.position = "none")
untreatedpct <- untreatedpct + theme(legend.position = "none")

grid.arrange(
  arrangeGrob(treatedpct, untreatedpct, ncol = 2),
  legend,
  ncol = 2,
  widths = c(4, 1)
)
```
```{r}
treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

filtered_df <- Veg %>%
  filter(
    CoverType == "Basal",
    DataEntry == "Ben Muher",
    !is.na(Species),
    CoverType != ""
  )

# Count occurrences of each species per site
species_counts <- filtered_df %>%
  group_by(Site, Species) %>%
  summarise(Count = n(), .groups = "drop")

# Summarize total species richness and total encounters per site
site_summary <- species_counts %>%
  group_by(Site) %>%
  summarise(
    SpeciesRichness = n_distinct(Species),
    TotalEncounters = sum(Count),
    .groups = "drop"
  ) %>%
  mutate(
    Treatment = case_when(
      Site %in% treated_sites ~ "Treated",
      Site %in% untreated_sites ~ "Untreated",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Treatment))

cat("\n📊 Species richness and total encounters per site:\n")
print(site_summary)

# Summarize by treatment group
treatment_summary <- site_summary %>%
  group_by(Treatment) %>%
  summarise(
    Mean_SpeciesRichness = mean(SpeciesRichness),
    SD_SpeciesRichness = sd(SpeciesRichness),
    Total_Species = sum(SpeciesRichness),
    Mean_Encounters = mean(TotalEncounters),
    SD_Encounters = sd(TotalEncounters),
    Total_Encounters = sum(TotalEncounters),
    n_Sites = n(),
    .groups = "drop"
  )


cat("\n📈 Summary by treatment group:\n")
print(treatment_summary)
```
```{r}
treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

# Count how many times each species appears at each site
species_matrix <- filtered_df %>%
  group_by(Site, Species) %>%
  summarise(Abundance = n(), .groups = "drop") %>%
  pivot_wider(names_from = Species, values_from = Abundance, values_fill = 0) %>%
  as.data.frame()

# Make sure species columns are numeric
rownames(species_matrix) <- species_matrix$Site
species_data <- species_matrix[, -1]
species_data[] <- lapply(species_data, as.numeric)

diversity_values <- data.frame(
  Site = species_matrix$Site,
  Shannon = diversity(species_data, index = "shannon"),
  SpeciesRichness = specnumber(species_data),
  TotalOccurrences = rowSums(species_data)
)

# Add treatment classification
diversity_values <- diversity_values %>%
  mutate(
    Treatment = case_when(
      Site %in% treated_sites ~ "Treated",
      Site %in% untreated_sites ~ "Untreated",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Treatment))

t_test_result <- t.test(Shannon ~ Treatment, data = diversity_values)

treatment_summary <- diversity_values %>%
  group_by(Treatment) %>%
  summarise(
    Mean_Shannon = mean(Shannon, na.rm = TRUE),
    SD_Shannon = sd(Shannon, na.rm = TRUE),
    Mean_Richness = mean(SpeciesRichness, na.rm = TRUE),
    SD_Richness = sd(SpeciesRichness, na.rm = TRUE),
    Mean_Encounters = mean(TotalOccurrences, na.rm = TRUE),
    n_Sites = n(),
    .groups = "drop"
  )

cat("\n📊 Diversity per site:\n")
print(diversity_values)

cat("\n📈 Summary by treatment group:\n")
print(treatment_summary)

cat("\n🔍 T-test comparing Shannon diversity (Treated vs Untreated) Basal Veg 2025:\n")
print(t_test_result)



```
```{r}
treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

# Count species occurrences per site
species_matrix <- filtered_veg %>%
  group_by(Site, Species) %>%
  summarise(Abundance = n(), .groups = "drop") %>%
  pivot_wider(names_from = Species, values_from = Abundance, values_fill = 0) %>%
  as.data.frame()

# Ensure species data is numeric
rownames(species_matrix) <- species_matrix$Site
species_data <- species_matrix[, -1]
species_data[] <- lapply(species_data, as.numeric)

# Calculate Simpson's diversity index (1 - D)
diversity_values <- data.frame(
  Site = species_matrix$Site,
  Simpson = diversity(species_data, index = "simpson"),
  SpeciesRichness = specnumber(species_data),
  TotalOccurrences = rowSums(species_data)
)

# Add treatment classification
diversity_values <- diversity_values %>%
  mutate(
    Treatment = case_when(
      Site %in% treated_sites ~ "Treated",
      Site %in% untreated_sites ~ "Untreated",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Treatment))

# Run t-test comparing Simpson's diversity between treatments
t_test_simpson <- t.test(Simpson ~ Treatment, data = diversity_values)

# Summary by treatment group
treatment_summary <- diversity_values %>%
  group_by(Treatment) %>%
  summarise(
    Mean_Simpson = mean(Simpson, na.rm = TRUE),
    SD_Simpson = sd(Simpson, na.rm = TRUE),
    Mean_Richness = mean(SpeciesRichness, na.rm = TRUE),
    Mean_Encounters = mean(TotalOccurrences, na.rm = TRUE),
    n_Sites = n(),
    .groups = "drop"
  )

cat("\n📊 Simpson's diversity per site:\n")
print(diversity_values)

cat("\n📈 Summary by treatment group:\n")
print(treatment_summary)

cat("\n🔍 T-test comparing Simpson's diversity (Treated vs Untreated) Basal Veg 2025:\n")
print(t_test_simpson)

```
```{r}
basal2025<- Veg %>%
  filter(
    CoverType == "Basal",
    Year == "2025",
    !is.na(Species),
    CoverType != "",
    (Species != "N/A") 
  )
```
```{r}
basal2024<- Veg %>%
  filter(
    CoverType == "Basal",
    Year == "2024",
    !is.na(Species),
    CoverType != "", 
    (Species != "N/A")
  )
```
```{r}
treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")

# Combine datasets for easier counting
basal_combined <- bind_rows(
  basal2024 %>% mutate(Year = 2024),
  basal2025 %>% mutate(Year = 2025)
)

# Filter for treated sites and valid species, then count encounters
species_counts <- basal_combined %>%
  filter(Site %in% treated_sites, Species != "N/A") %>%
  group_by(Year, Species) %>%
  summarise(Encounters = n(), .groups = "drop") %>%
  arrange(Year, desc(Encounters))

species_counts
```
```{r}
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

# Combine datasets for easier counting
basal_UNcombined <- bind_rows(
  basal2024 %>% mutate(Year = 2024),
  basal2025 %>% mutate(Year = 2025)
)

# Filter for treated sites and valid species, then count encounters
UNspecies_counts <- basal_UNcombined %>%
  filter(Site %in% untreated_sites, Species != "N/A") %>%
  group_by(Year, Species) %>%
  summarise(Encounters = n(), .groups = "drop") %>%
  arrange(Year, desc(Encounters))

UNspecies_counts
```
```{r}
# 1. Convert from long to wide format (species as columns)
UNwide_data <- UNspecies_counts %>%
  pivot_wider(names_from = Species, values_from = Encounters, values_fill = 0)

# 2. Calculate Shannon diversity for each row
# Exclude the 'Year' column
UNshannon_index <- diversity(UNwide_data[, -which(names(UNwide_data) == "Year")], index = "shannon")

# 3. Add Shannon index to the dataset
UNwide_data$Shannon <- UNshannon_index

print(UNwide_data)
cat("Shannon diversity in 2024:", UNwide_data$Shannon[UNwide_data$Year == 2024], "\n")
cat("Shannon diversity in 2025:", UNwide_data$Shannon[UNwide_data$Year == 2025], "\n")



```
```{r}
# 1. Convert from long to wide format (species as columns)
Twide_data <- species_counts %>%
  pivot_wider(names_from = Species, values_from = Encounters, values_fill = 0)

# 2. Calculate Shannon diversity for each row
# Exclude the 'Year' column
Tshannon_index <- diversity(Twide_data[, -which(names(Twide_data) == "Year")], index = "shannon")

# 3. Add Shannon index to the dataset
Twide_data$Shannon <- Tshannon_index

print(Twide_data)
cat("Shannon diversity in 2024:", Twide_data$Shannon[Twide_data$Year == 2024], "\n")
cat("Shannon diversity in 2025:",Twide_data$Shannon[Twide_data$Year == 2025], "\n")
```
```{r}
# Convert to data.table
dt2024 <- as.data.table(basal2024)
dt2025 <- as.data.table(basal2025)

treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

# Function to summarize counts by site type
summarize_year <- function(dt, treated, untreated, year) {
  dt[, .(
    untreated_count = sum(Site %in% untreated),
    treated_count   = sum(Site %in% treated),
    total_count     = .N
  ), by = Species] -> tmp
  
  # Rename columns with year suffix
  setnames(tmp, c("untreated_count", "treated_count", "total_count"),
           c(paste0("untreated_", year), paste0("treated_", year), paste0("total_", year)))
  return(tmp)
}

summary2024 <- summarize_year(dt2024, treated_sites, untreated_sites, 2024)
summary2025 <- summarize_year(dt2025, treated_sites, untreated_sites, 2025)
final_summary <- merge(summary2024, summary2025, by = "Species", all = TRUE)
final_summary[is.na(final_summary)] <- 0

```
```{r}
# Reshape 2024 data to long format
species_counts_2024 <- final_summary %>%
  select(Species, untreated_2024, treated_2024) %>%
  pivot_longer(
    cols = c(untreated_2024, treated_2024),
    names_to = "Treatment",
    values_to = "Count"
  ) %>%
  mutate(
    Treatment = ifelse(Treatment == "treated_2024", "Treated", "Untreated")
  )

species_counts_2024$Species <- as.character(species_counts_2024$Species)

# Define the named color vector (no scale_fill_manual here)
species_colors <- c(
  "PENBAR" = "#1F77B4",
  "N/A" = "#808080",
  "ELYELY" = "#FF7F0E",
  "BERREP" = "#2CA02C",
  "ERISPE" = "#D62728",
  "SENERE" = "#9467BD",
  "BOEXXX" = "#8C564B",
  "PACFEN" = "#E377C2",
  "ANDSEP" = "#7F7F7F",
  "BROCIL" = "#BCBD22",
  "HETVIL" = "#17BECF",
  "GERRIC" = "#AEC7E8",
  "CARXXX" = "#FFBB78",
  "AMADIS" = "#98DF8A",
  "QUGA" = "#FF9896",
  "ABCO" = "#C5B0D5",
  "MUHMON" = "#C49C94",
  "DIECAN" = "#F7B6D2",
  "PSEMAC" = "#DBDB8D",
  "ARTLUD" = "#9EDAE5",
  "LAESCH" = "#393B79",
  "POAFEN" = "#637939",
  "ACHMIL" = "#8C6D31",
  "CEAFEN" = "#843C39",
  "VERTHA" = "#7B4173",
  "DRAXXX" = "#3182BD",
  "PIED" = "#E6550D",
  "HIEFEN" = "#31A354",
  "ALLCER" = "#756BB1",
  "CAMROT" = "#636363",
  "KOEMAC" = "#D6616B",
  "PIPO" = "#FCAE6B",
  "PHAHET" = "#BCBD22",
  "CONCAN" = "#17BECF",
  "PSME" = "#9EDAE5",
  "SENSPA" = "#393B79",
  "GERCAE" = "#637939",
  "CYMLEM" = "#8C6D31",
  "BERFEN" = "#843C39",
  "ANTMAR" = "#7B4173",
  "ERIDIV" = "#3182BD",
  "CLECOL" = "#E6550D",
  "SOLWRI" = "#31A354",
  "ARELAN" = "#756BB1",
  "BRIGRA" = "#636363",
  "TETARG" = "#D6616B",
  "PIST" = "#FCAE6B",
  "PAXMYR" = "#BCBD22",
  "DYSGRA" = "#17BECF",
  "COLPAR" = "#9EDAE5",
  "NAMDIC" = "#393B79",
  "FORB" = "#8C6D31",
  "GALBOR" = "#843C39",
  "MIRLIN" = "#7B4173",
  "FRAVES" = "#3182BD",
  "SAXBRO" = "#E6550D",
  "LATLAN" = "#31A354",
  "ANTPAR" = "#756BB1",
  "ARCUVA" = "#636363",
  "SYMROT" = "#D6616B",
  "QUUN" = "#FCAE6B",
  "HELMUL" = "#BCBD22",
  "CHEPRA" = "#17BECF",
  "BOUGRA" = "#9EDAE5",
  "ERIFOR" = "#393B79",
  "CERMON" = "#637939",
  "MUHTRI" = "#8C6D31",
  "ERIVRE" = "#843C39",
  "APOAND" = "#7B4173",
  "NOCFEN" = "#3182BD",
  "SYMSPA" = "#E6550D",
  "ARTFRA" = "#31A354",
  "GOOOBL" = "#756BB1",
  "BRIEUP" = "#636363",
  "HEDDRU" = "#D6616B",
  "OENCES" = "#FCAE6B",
  "ERIJAM" = "#BCBD22",
  "EUPREV" = "#17BECF",
  "ERICAN" = "#9EDAE5",
  "ERYCAP" = "#393B79",
  "ERIFLA" = "#637939",
  "PENJAM" = "#8C6D31",
  "HYMRIC" = "#843C39",
  "BRASSI" = "#7B4173",
  "SENWOO" = "#3182BD",
  "DANSPI" = "#E6550D"
)

# Plot
ggplot(species_counts_2024, aes(x = Treatment, y = Count, fill = Species)) +
  geom_bar(stat = "identity", position = "stack") +
  ylim(0,350)+
  scale_fill_manual(values = species_colors) +  # pass the named vector here
  labs(
    title = "Basal Species Observation Counts (2024)",
    x = "",
    y = "Species Observation Count",
    fill = "Species"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12),
        legend.position = "none")
```
```{r}
##grid.arrange(overall24, Overall25, ncol=2)
```
```{r}
# Select only treated counts for 2024 and 2025
treated_counts <- final_summary %>%
  select(Species, treated_2024, treated_2025) %>%
  pivot_longer(
    cols = c(treated_2024, treated_2025),
    names_to = "Year",
    values_to = "Count"
  ) %>%
  mutate(
    Year = ifelse(Year == "treated_2024", "2024", "2025")
  )

treated_counts$Species <- as.character(treated_counts$Species)

# Reuse your species_colors vector from before
# Make sure all species in treated_counts exist in species_colors
missing_colors <- setdiff(unique(treated_counts$Species), names(species_colors))
if(length(missing_colors) > 0){
  # Assign random colors from RColorBrewer if any species missing
  new_colors <- setNames(brewer.pal(n = length(missing_colors), name = "Set3"), missing_colors)
  species_colors <- c(species_colors, new_colors)
}

# Plot
ggplot(treated_counts, aes(x = Year, y = Count, fill = Species)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = species_colors) +
  labs(
    title = "Basal Species Observation Counts (Treated Sites)",
    x = "Year",
    y = "Species Observation Count",
    fill = "Species"
  ) +
  theme_minimal() +
  ylim(0,350)+
  theme(axis.text.x = element_text(size = 12),
        legend.position = "none")
```
```{r}
# Select only untreated counts for 2024 and 2025
untreated_counts <- final_summary %>%
  select(Species, untreated_2024, untreated_2025) %>%
  pivot_longer(
    cols = c(untreated_2024, untreated_2025),
    names_to = "Year",
    values_to = "Count"
  ) %>%
  mutate(
    Year = ifelse(Year == "untreated_2024", "2024", "2025")
  )

untreated_counts$Species <- as.character(untreated_counts$Species)

# Ensure all species have assigned colors
missing_colors <- setdiff(unique(untreated_counts$Species), names(species_colors))
if(length(missing_colors) > 0){
  # Assign new colors from RColorBrewer if any missing
  new_colors <- setNames(brewer.pal(n = length(missing_colors), name = "Set3"), missing_colors)
  species_colors <- c(species_colors, new_colors)
}

# Plot: Stacked bar for untreated sites
ggplot(untreated_counts, aes(x = Year, y = Count, fill = Species)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = species_colors) +
  labs(
    title = "Basal Species Observation Counts (Untreated Sites)",
    x = "Year",
    y = "Species Observation Count",
    fill = "Species"
  ) +
  theme_minimal() +
  ylim(0,350)+
  theme(axis.text.x = element_text(size = 12),
        legend.position = "none")
```
```{r}
#grid.arrange(untreatedbothyears, treatedbothyears, ncol=2)
```
```{r}
### a lot of that stuff above, especially all the species encounters stuff kinda sucks, hopefully this stuff is better
GOODbasal24<- read.xlsx("24basebig.xlsx")
GOODbasal25<- read.xlsx("25basebig.xlsx")
GOODair24<- read.xlsx("air24.xlsx")
GOODair25<- read.xlsx("air25.xlsx")
```
```{r}
###25' basal percent cover, t test, treated vs untreated 

treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

basal25_clean <- GOODbasal25 %>%
  mutate(
    Percent = as.numeric(Percent),          # convert Percent to numeric
    Species = trimws(Species),              # remove leading/trailing spaces
    Site = trimws(toupper(Site))            # standardize site names
  ) %>%
  filter(
    CoverType == "Basal",
    !is.na(Percent),                        # remove blanks
    Species != "N/A",
    Site %in% c(treated_sites, untreated_sites)
  ) %>%
  mutate(
    Treatment = case_when(
      Site %in% treated_sites ~ "Treated",
      Site %in% untreated_sites ~ "Untreated"
    )
  )

# Check summary
table(basal25_clean$Treatment)
# Run T-test: Percent ~ Treatment
cat("\n📊 Ttest 2025 basal treated vs untreated % cover:\n")
t_test_result <- t.test(Percent ~ Treatment, data = basal25_clean)

t_test_result
```
```{r}
###24' basal percent cover, t test, treated vs untreated 

treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

basal24_clean <- GOODbasal24 %>%
  mutate(
    Percent = as.numeric(Percent),          # convert Percent to numeric
    Species = trimws(Species),              # remove leading/trailing spaces
    Site = trimws(toupper(Site))            # standardize site names
  ) %>%
  filter(
    CoverType == "Basal",
    !is.na(Percent),                        # remove blanks
    Species != "N/A",
    Site %in% c(treated_sites, untreated_sites)
  ) %>%
  mutate(
    Treatment = case_when(
      Site %in% treated_sites ~ "Treated",
      Site %in% untreated_sites ~ "Untreated"
    )
  )

table(basal24_clean$Treatment)
# Run T-test: Percent ~ Treatment
cat("\n📊 Ttest 2024 basal treated vs untreated % cover:\n")
t_test_result <- t.test(Percent ~ Treatment, data = basal25_clean)

t_test_result
```
```{r}
###24' aerial percent cover, t test, treated vs untreated 

treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

Air24_clean <- GOODair24 %>%
  mutate(
    Percent = as.numeric(Percent),          # convert Percent to numeric
    Species = trimws(Species),              # remove leading/trailing spaces
    Site = trimws(toupper(Site))            # standardize site names
  ) %>%
  mutate(
    Treatment = case_when(
      Site %in% treated_sites ~ "Treated",
      Site %in% untreated_sites ~ "Untreated"
    )
  )

table(Air24_clean$Treatment)
# Run T-test: Percent ~ Treatment
cat("\n📊 Ttest 2024 Aerieal treated vs untreated % cover:\n")
t_test_result <- t.test(Percent ~ Treatment, data = Air24_clean)

t_test_result
```
```{r}
###25' aerial percent cover, t test, treated vs untreated 

treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

Air25_clean <- GOODair25 %>%
  mutate(
    Percent = as.numeric(Percent),          # convert Percent to numeric
    Species = trimws(Species),              # remove leading/trailing spaces
    Site = trimws(toupper(Site))            # standardize site names
  ) %>%
  mutate(
    Treatment = case_when(
      Site %in% treated_sites ~ "Treated",
      Site %in% untreated_sites ~ "Untreated"
    )
  )

table(Air25_clean$Treatment)
# Run T-test: Percent ~ Treatment
cat("\n📊 Ttest 2025 Aerieal treated vs untreated % cover:\n")
t_test_result <- t.test(Percent ~ Treatment, data = Air25_clean)

t_test_result
```
```{r}
### made a different data set that included trace for lifeform work
lf24<- read.xlsx("24basebig.xlsx")
lf25<- read.xlsx("25basebig.xlsx")
```
```{r}

treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

lf25_clean <- lf25 %>%
  mutate(
    Percent = na_if(Percent, "T"),   # convert "T" to NA
    Percent = as.numeric(Percent),   # make numeric
    Site = trimws(toupper(Site)),
    LifeForm = trimws(Lifeform)
  ) %>%
  filter(
    Site %in% c(treated_sites, untreated_sites),
    Lifeform %in% c("Tree", "Shrub", "Gram", "Forb"),
    !is.na(Percent)
  ) %>%
  mutate(
    Treatment = case_when(
      Site %in% treated_sites ~ "Treated",
      Site %in% untreated_sites ~ "Untreated"
    )
  )
lifeforms <- c("Tree", "Shrub", "Gram", "Forb")
lf25_clean %>%
  group_by(LifeForm, Treatment) %>%
  summarise(
    n = n(),
    mean_cover = mean(Percent, na.rm = TRUE),
    sd_cover = sd(Percent, na.rm = TRUE),
    min_cover = min(Percent, na.rm = TRUE),
    max_cover = max(Percent, na.rm = TRUE)
  )
```
```{r}

treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

lf24_clean <- lf24 %>%
  mutate(
    Percent = na_if(Percent, "T"),   # convert "T" to NA
    Percent = as.numeric(Percent),   # make numeric
    Site = trimws(toupper(Site)),
    LifeForm = trimws(Lifeform)
  ) %>%
  filter(
    Site %in% c(treated_sites, untreated_sites),
    Lifeform %in% c("Tree", "Shrub", "Gram", "Forb"),
    !is.na(Percent)
  ) %>%
  mutate(
    Treatment = case_when(
      Site %in% treated_sites ~ "Treated",
      Site %in% untreated_sites ~ "Untreated"
    )
  )
lifeforms <- c("Tree", "Shrub", "Gram", "Forb")
lf24_clean %>%
  group_by(LifeForm, Treatment) %>%
  summarise(
    n = n(),
    mean_cover = mean(Percent, na.rm = TRUE),
    sd_cover = sd(Percent, na.rm = TRUE),
    min_cover = min(Percent, na.rm = TRUE),
    max_cover = max(Percent, na.rm = TRUE)
  )
```
```{r}

treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

Air25good_clean <- GOODair25 %>%
  mutate(
    Percent = na_if(Percent, "T"),   # convert "T" to NA
    Percent = as.numeric(Percent),   # make numeric
    Site = trimws(toupper(Site)),
    LifeForm = trimws(Lifeform)
  ) %>%
  filter(
    Site %in% c(treated_sites, untreated_sites),
    Lifeform %in% c("Tree", "Shrub", "Gram", "Forb"),
    !is.na(Percent)
  ) %>%
  mutate(
    Treatment = case_when(
      Site %in% treated_sites ~ "Treated",
      Site %in% untreated_sites ~ "Untreated"
    )
  )
lifeforms <- c("Tree", "Shrub", "Gram", "Forb")
Air25good_clean %>%
  group_by(LifeForm, Treatment) %>%
  summarise(
    n = n(),
    mean_cover = mean(Percent, na.rm = TRUE),
    sd_cover = sd(Percent, na.rm = TRUE),
    min_cover = min(Percent, na.rm = TRUE),
    max_cover = max(Percent, na.rm = TRUE)
  )
```
```{r}
treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

# Prep data
lf_clean2 <- lf25_clean %>%
  filter(!is.na(Percent), Percent != "") %>%
  filter(Species != "N/A") %>%
  mutate(SiteType = case_when(
    Site %in% treated_sites ~ "Treated",
    Site %in% untreated_sites ~ "Untreated",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(SiteType))

# ---- 1. Calculate Simpson's Evenness for each plot ----
evenness_data <- lf_clean2 %>%
  group_by(PlotID, Site, SiteType, Lifeform) %>%
  summarise(total_cover = sum(Percent), .groups = "drop") %>%   # total cover per lifeform
  group_by(PlotID, Site, SiteType) %>%
  mutate(p = total_cover / sum(total_cover)) %>%
  summarise(
    D = sum(p^2),          # Simpson's D
    S = n(),               # richness (# lifeforms present)
    Evenness = (1 / D) / S,
    .groups = "drop"
  )
# ---- 2. T-test comparing treated vs untreated ----
cat("\n📊 Ttest 2025 basal lifeform percent cover, treated vs untreated:\n")
t.test(Evenness ~ SiteType, data = evenness_data)

```
```{r}
treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

# Prep data
lf24_clean2 <- lf24_clean %>%
  filter(!is.na(Percent), Percent != "") %>%
  filter(Species != "N/A") %>%
  mutate(SiteType = case_when(
    Site %in% treated_sites ~ "Treated",
    Site %in% untreated_sites ~ "Untreated",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(SiteType))

# ---- 1. Calculate Simpson's Evenness for each plot ----
evenness_data <- lf24_clean2 %>%
  group_by(PlotID, Site, SiteType, Lifeform) %>%
  summarise(total_cover = sum(Percent), .groups = "drop") %>%   # total cover per lifeform
  group_by(PlotID, Site, SiteType) %>%
  mutate(p = total_cover / sum(total_cover)) %>%
  summarise(
    D = sum(p^2),          # Simpson's D
    S = n(),               # richness (# lifeforms present)
    Evenness = (1 / D) / S,
    .groups = "drop"
  )
# ---- 2. T-test comparing treated vs untreated ----
cat("\n📊 Ttest eveness 2024 basal lifeform percent cover, treated vs untreated:\n")
t.test(Evenness ~ SiteType, data = evenness_data)
```
```{r}
treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

# Prep data
lf24_clean2 <- lf24_clean %>%
  filter(!is.na(Percent), Percent != "") %>%
  filter(Species != "N/A") %>%
  mutate(SiteType = case_when(
    Site %in% treated_sites ~ "Treated",
    Site %in% untreated_sites ~ "Untreated",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(SiteType))

# ---- 1. Calculate Simpson's Evenness for each plot ----
evenness_data <- lf24_clean2 %>%
  group_by(PlotID, Site, SiteType, Species) %>%
  summarise(total_cover = sum(Percent), .groups = "drop") %>%   # total cover per lifeform
  group_by(PlotID, Site, SiteType) %>%
  mutate(p = total_cover / sum(total_cover)) %>%
  summarise(
    D = sum(p^2),          # Simpson's D
    S = n(),               # richness (# lifeforms present)
    Evenness = (1 / D) / S,
    .groups = "drop"
  )
# ---- 2. T-test comparing treated vs untreated ----
cat("\n📊 Ttest eveness 2024 basal species percent cover, treated vs untreated:\n")
t.test(Evenness ~ SiteType, data = evenness_data)
```
```{r}
treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

# Prep data
lf_clean2 <- lf25_clean %>%
  filter(!is.na(Percent), Percent != "") %>%
  filter(Species != "N/A") %>%
  mutate(SiteType = case_when(
    Site %in% treated_sites ~ "Treated",
    Site %in% untreated_sites ~ "Untreated",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(SiteType))

# ---- 1. Calculate Simpson's Evenness for each plot ----
evenness_data <- lf_clean2 %>%
  group_by(PlotID, Site, SiteType, Species) %>%
  summarise(total_cover = sum(Percent), .groups = "drop") %>%   # total cover per lifeform
  group_by(PlotID, Site, SiteType) %>%
  mutate(p = total_cover / sum(total_cover)) %>%
  summarise(
    D = sum(p^2),          # Simpson's D
    S = n(),               # richness (# lifeforms present)
    Evenness = (1 / D) / S,
    .groups = "drop"
  )
# ---- 2. T-test comparing treated vs untreated ----
cat("\n📊 Ttest 2025 basal lifeform percent cover, treated vs untreated:\n")
t.test(Evenness ~ SiteType, data = evenness_data)
```
```{r}
treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

# Prep data
lf_clean2 <- lf25_clean %>%
  filter(!is.na(Percent), Percent != "") %>%      # keep nonblank percent
  filter(Species != "N/A") %>%                    # remove N/A species
  mutate(SiteType = case_when(
    Site %in% treated_sites ~ "Treated",
    Site %in% untreated_sites ~ "Untreated",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(SiteType))

# ---- 1. Shannon Diversity and Evenness per plot ----
shannon_data <- lf_clean2 %>%
  group_by(Site, SiteType, Species) %>%
  summarise(total_cover = sum(Percent), .groups = "drop") %>%   # total cover per lifeform
  group_by(Site, SiteType) %>%
  mutate(p = total_cover / sum(total_cover)) %>%
  summarise(
    H = -sum(p * log(p)),        # Shannon diversity H'
    S = n(),                     # richness (# lifeforms present)
    Evenness = H / log(S),       # Pielou's evenness J
    .groups = "drop"
  )
cat("\n📊 Ttest 2025 basal Species percent cover diversity and eveness group_by(Site, SiteType, Species), treated vs untreated:\n")
t.test(H ~ SiteType, data = shannon_data)
t.test(Evenness ~ SiteType, data = shannon_data)
```
```{r}
treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

# Prep data
lf24_clean2 <- lf24_clean %>%
  filter(!is.na(Percent), Percent != "") %>%      # keep nonblank percent
  filter(Species != "N/A") %>%                    # remove N/A species
  mutate(SiteType = case_when(
    Site %in% treated_sites ~ "Treated",
    Site %in% untreated_sites ~ "Untreated",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(SiteType))

# ---- 1. Shannon Diversity and Evenness per plot ----
shannon_data24 <- lf24_clean2 %>%
  group_by(PlotID, Site, SiteType, Species) %>%
  summarise(total_cover = sum(Percent), .groups = "drop") %>%   # total cover per species
  group_by(PlotID,Site, SiteType) %>%
  mutate(p = total_cover / sum(total_cover)) %>%
  summarise(
    H = -sum(p * log(p)),        # Shannon diversity H'
    S = n(),                     # richness (# lifeforms present)
    Evenness = H / log(S),       # Pielou's evenness J
    .groups = "drop"
  )
cat("\n📊 Ttest 2024 basal Species percent cover diversity and eveness, treated vs untreated group_by(Plot ID, Site, SiteType, Species) :\n")
t.test(H ~ SiteType, data = shannon_data24)
t.test(Evenness ~ SiteType, data = shannon_data24)
```
```{r}
treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

# Prep data
GOODair24_clean2 <- GOODair24 %>%
  filter(!is.na(Percent), Percent != "", Percent != "T") %>%  # remove blanks and T
  filter(Species != "N/A") %>% 
  mutate(
    Percent = as.numeric(Percent),   # convert to numeric
    SiteType = case_when(
      Site %in% treated_sites ~ "Treated",
      Site %in% untreated_sites ~ "Untreated",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(SiteType))

# ---- 1. Shannon Diversity and Evenness per plot ----
shannon_dataair24 <- GOODair24_clean2 %>%
  group_by(Site, SiteType, Species) %>%
  summarise(total_cover = sum(Percent), .groups = "drop") %>%   # total cover per species
  group_by(Site, SiteType) %>%
  mutate(p = total_cover / sum(total_cover)) %>%
  summarise(
    H = -sum(p * log(p)),        # Shannon diversity H'
    S = n(),                     # richness (# species present)
    Evenness = H / log(S),       # Pielou's evenness J
    .groups = "drop"
  )

# ---- 2. T-tests ----
cat("\n📊 T-test 2024 aerial Species percent cover diversity and evenness   group_by(Site, SiteType, Species), treated vs untreated:\n")
t.test(H ~ SiteType, data = shannon_dataair24)
t.test(Evenness ~ SiteType, data = shannon_dataair24)

```
```{r}
treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

# Prep data
GOODair25_clean2 <- GOODair25 %>%
  filter(!is.na(Percent), Percent != "", Percent != "T") %>%  # remove blanks and T
  filter(Species != "N/A") %>% 
  mutate(
    Percent = as.numeric(Percent),   # convert to numeric
    SiteType = case_when(
      Site %in% treated_sites ~ "Treated",
      Site %in% untreated_sites ~ "Untreated",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(SiteType))

# ---- 1. Shannon Diversity and Evenness per plot ----
shannon_dataair25 <- GOODair25_clean2 %>%
  group_by(PlotID, Site, SiteType, Lifeform) %>%
  summarise(total_cover = sum(Percent), .groups = "drop") %>%   # total cover per species
  group_by(PlotID, Site, SiteType) %>%
  mutate(p = total_cover / sum(total_cover)) %>%
  summarise(
    H = -sum(p * log(p)),        # Shannon diversity H'
    S = n(),                     # richness (# species present)
    Evenness = H / log(S),       # Pielou's evenness J
    .groups = "drop"
  )

# ---- 2. T-tests ----
cat("\n📊 T-test 2025 aerial Lifeform percent cover diversity and evenness   group_by(PlotID, Site, SiteType, Lifeform), treated vs untreated:\n")
t.test(H ~ SiteType, data = shannon_dataair25)
t.test(Evenness ~ SiteType, data = shannon_dataair25)

```
```{r}
treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

# Prep data
GOODair24_clean2 <- GOODair24 %>%
  filter(!is.na(Percent), Percent != "", Percent != "T") %>%  # remove blanks and T
  filter(Species != "N/A") %>% 
  mutate(
    Percent = as.numeric(Percent),   # convert to numeric
    SiteType = case_when(
      Site %in% treated_sites ~ "Treated",
      Site %in% untreated_sites ~ "Untreated",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(SiteType))

# ---- 1. Shannon Diversity and Evenness per plot ----
shannon_dataair24 <- GOODair24_clean2 %>%
  group_by(PlotID, Site, SiteType, Lifeform) %>%
  summarise(total_cover = sum(Percent), .groups = "drop") %>%   # total cover per species
  group_by(PlotID, Site, SiteType) %>%
  mutate(p = total_cover / sum(total_cover)) %>%
  summarise(
    H = -sum(p * log(p)),        # Shannon diversity H'
    S = n(),                     # richness (# species present)
    Evenness = H / log(S),       # Pielou's evenness J
    .groups = "drop"
  )

# ---- 2. T-tests ----
cat("\n📊 T-test 2024 aerial Lifeform percent cover diversity and evenness   group_by(PlotID, Site, SiteType, Lifeform), treated vs untreated:\n")
t.test(H ~ SiteType, data = shannon_dataair24)
t.test(Evenness ~ SiteType, data = shannon_dataair24)

```
```{r}
treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

# Prep data
lf24_clean2 <- lf24_clean %>%
  filter(!is.na(Percent), Percent != "") %>%      # keep nonblank percent
  filter(Species != "N/A") %>%                    # remove N/A species
  mutate(SiteType = case_when(
    Site %in% treated_sites ~ "Treated",
    Site %in% untreated_sites ~ "Untreated",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(SiteType))

# ---- 1. Shannon Diversity and Evenness per plot ----
shannon_data24 <- lf24_clean2 %>%
  group_by(PlotID, Site, SiteType, Lifeform) %>%
  summarise(total_cover = sum(Percent), .groups = "drop") %>%   # total cover per species
  group_by(PlotID,Site, SiteType) %>%
  mutate(p = total_cover / sum(total_cover)) %>%
  summarise(
    H = -sum(p * log(p)),        # Shannon diversity H'
    S = n(),                     # richness (# lifeforms present)
    Evenness = H / log(S),       # Pielou's evenness J
    .groups = "drop"
  )
cat("\n📊 Ttest 2024 basal Lifeform percent cover diversity and eveness, treated vs untreated group_by(Plot ID, Site, SiteType, Lifeform) :\n")
t.test(H ~ SiteType, data = shannon_data24)
t.test(Evenness ~ SiteType, data = shannon_data24)
```
```{r}
treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

# Prep data
lf_clean2 <- lf25_clean %>%
  filter(!is.na(Percent), Percent != "") %>%      # keep nonblank percent
  filter(Species != "N/A") %>%                    # remove N/A species
  mutate(SiteType = case_when(
    Site %in% treated_sites ~ "Treated",
    Site %in% untreated_sites ~ "Untreated",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(SiteType))

# ---- 1. Shannon Diversity and Evenness per plot ----
shannon_data <- lf_clean2 %>%
  group_by(PlotID, Site, SiteType, Lifeform) %>%
  summarise(total_cover = sum(Percent), .groups = "drop") %>%   # total cover per lifeform
  group_by(PlotID, Site, SiteType) %>%
  mutate(p = total_cover / sum(total_cover)) %>%
  summarise(
    H = -sum(p * log(p)),        # Shannon diversity H'
    S = n(),                     # richness (# lifeforms present)
    Evenness = H / log(S),       # Pielou's evenness J
    .groups = "drop"
  )
cat("\n📊 Ttest 2025 basal Lifeform percent cover diversity and eveness group_by(PlotId, Site, SiteType, Lifeform), treated vs untreated:\n")
t.test(H ~ SiteType, data = shannon_data)
t.test(Evenness ~ SiteType, data = shannon_data)
```
```{r}
treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")
lf25gram <- Lf25 %>%
  mutate(Treatment = case_when(
    Site %in% treated_sites ~ "Treated",
    Site %in% untreated_sites ~ "Untreated",
    TRUE ~ NA_character_
  ))

presence <- lf25gram %>%
  group_by(Site, Treatment) %>%
  summarise(
    GramPresent = as.integer(any(Lifeform == "Gramn")),
    .groups = "drop"
  )

tbl <- table(presence$Treatment, presence$GramPresent)
tbl
```
```{r}
treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")

species_of_interest <- c(
  "CARXXX","ELYELY","MUHMON","BOUGRA","BROCIL",
  "DANSPI","KOEMAC","MUHTRI","POAFEN"
)

lf25 <- Lf25 %>% 
  mutate(
    Treatment = case_when(
      Site %in% treated_sites ~ "Treated",
      Site %in% untreated_sites ~ "Untreated",
      TRUE ~ NA_character_
    )
  )

plots <- lf25 %>% 
  filter(!is.na(Treatment)) %>%
  distinct(PlotID, Treatment)

presence <- lf25 %>%
  filter(Species %in% species_of_interest) %>%
  distinct(PlotID, Species) %>%
  mutate(Present = 1)

all <- expand_grid(
  PlotID = plots$PlotID,
  Species = species_of_interest
) %>%
  left_join(plots, by = "PlotID") %>%
  left_join(presence, by = c("PlotID", "Species")) %>%
  mutate(Present = if_else(is.na(Present), 0L, Present))

summary_table <- all %>%
  group_by(Species, Treatment) %>%
  summarise(
    Present = sum(Present == 1),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Treatment,
    values_from = Present,
    names_glue = "{Treatment}_Present"
  ) %>%
  mutate(
    Treated_Absent   = 72 - Treated_Present,
    Untreated_Absent = 71 - Untreated_Present
  ) %>%
  dplyr::select(
    Species,
    Treated_Present, Treated_Absent,
    Untreated_Present, Untreated_Absent
  )


summary_table

```
```{r}
summary_pct <- summary_table %>%
  mutate(
    Treated_pct   = Treated_Present   / 72,
    Untreated_pct = Untreated_Present / 71
  )
summary_pct
# Paired t-test comparing treated vs untreated percent
t_test_result <- t.test(summary_pct$Treated_pct, 
                        summary_pct$Untreated_pct, 
                        paired = TRUE)

t_test_result


```
```{r}
fisher_results <- summary_table %>%
  mutate(
    result = pmap(
      list(Treated_Present, Treated_Absent,
           Untreated_Present, Untreated_Absent),
      ~ {
        tbl <- matrix(
          c(..1, ..2, ..3, ..4),
          nrow = 2,
          byrow = TRUE
        )
        fisher.test(tbl)
      }
    )
  ) %>%
  mutate(
    p_value = map_dbl(result, "p.value"),
    odds_ratio = map_dbl(result, ~ .$estimate),
    conf_low  = map_dbl(result, ~ .$conf.int[1]),
    conf_high = map_dbl(result, ~ .$conf.int[2])
  ) %>%
  select(Species, p_value, odds_ratio, conf_low, conf_high)
fisher_results
###p < 0.05: presence/absence differs between treated vs untreated
###odds_ratio > 1: more likely in treated plots
###odds_ratio < 1: more likely in untreated plots
###wide CI = low statistical power (likely with small counts)



```
```{r}
lf25<- read.xlsx("/Users/benmuher/Desktop/25basebig.xlsx")
treated_sites <- c("SFF1", "SFF5", "SFF7", "SFF8", "SFF10", "SFS4")
untreated_sites <- c("SFF2", "SFF3", "SFF4", "SFF6", "SFF9", "BTN4")


lf25 <- Lf25 %>% 
  mutate(
    Treatment = case_when(
      Site %in% treated_sites ~ "Treated",
      Site %in% untreated_sites ~ "Untreated",
      TRUE ~ NA_character_
    )
  )

all_species <- lf25 %>%
  filter(!is.na(Treatment)) %>%
  distinct(Species) %>%
  pull(Species)
presence <- lf25 %>%
  filter(!is.na(Treatment)) %>%
  distinct(PlotID, Species) %>%
  mutate(Present = 1)
all <- expand_grid(
  PlotID = plots$PlotID,
  Species = all_species
) %>%
  left_join(plots, by = "PlotID") %>%
  left_join(presence, by = c("PlotID", "Species")) %>%
  mutate(Present = if_else(is.na(Present), 0L, Present))
summary_tableall <- all %>%
  group_by(Species, Treatment) %>%
  summarise(
    Present = sum(Present == 1),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Treatment,
    values_from = Present,
    names_glue = "{Treatment}_Present"
  ) %>%
  mutate(
    Treated_Absent   = 72 - Treated_Present,
    Untreated_Absent = 71 - Untreated_Present
  ) %>%
  dplyr::select(
    Species,
    Treated_Present, Treated_Absent,
    Untreated_Present, Untreated_Absent
  )
summary_tableall


```

```{r}
fisher_results <- summary_tableall %>%
  mutate(
    result = pmap(
      list(Treated_Present, Treated_Absent,
           Untreated_Present, Untreated_Absent),
      ~ {
        tbl <- matrix(
          c(..1, ..2, ..3, ..4),
          nrow = 2,
          byrow = TRUE
        )
        fisher.test(tbl)
      }
    )
  ) %>%
  mutate(
    p_value = map_dbl(result, "p.value"),
    odds_ratio = map_dbl(result, ~ .$estimate),
    conf_low  = map_dbl(result, ~ .$conf.int[1]),
    conf_high = map_dbl(result, ~ .$conf.int[2])
  ) %>%
  select(Species, p_value, odds_ratio, conf_low, conf_high) %>%
  filter(p_value < 0.05)   #
fisher_results




###p < 0.05: presence/absence differs between treated vs untreated
###odds_ratio > 1: more likely in treated plots
###odds_ratio < 1: more likely in untreated plots
###wide CI = low statistical power (likely with small counts)
```
