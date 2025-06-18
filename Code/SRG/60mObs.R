#rad census to 60m

library(dplyr)
library(ggplot2)
library(tidyverse)
library(patchwork)

#===filter to 60m to align with Russ's survey===================================
census_dat <- read.csv("Z:/MooreSRER/FieldData/DataSpreadsheets/US-SRG_WoodyPlantCensus_28052025.csv")
avg_census_dat <- census_dat%>%
  group_by(ID)%>%
  summarize(Species = Species,
            Quadrant = Quadrant,
            Distance = Distance,
            BasalDiameter = mean(BasalDiameter, na.rm = T),
            CrownDiameter = mean(CrownDiameter, na.rm = T),
            Height = Height)%>%
  filter(Species != "",
         Distance < 60)%>%
  mutate(Area = pi*((CrownDiameter/2)^2))%>%
  arrange(Quadrant, Species)

#write.csv(avg_census_dat, "./60mObs.csv")
#===calc same stats as Russ=====================================================
stats <- avg_census_dat %>%
  group_by(Quadrant, Species) %>%
  summarise(
    MeanHeight = mean(Height, na.rm = TRUE),
    SdHeight = sd(Height, na.rm = TRUE),
    MeanCanDi = mean(CrownDiameter, na.rm = TRUE),
    SdCanDi = sd(CrownDiameter, na.rm = TRUE),
    MeanArea = mean(Area, na.rm = TRUE),
    SdArea = sd(Area, na.rm = TRUE),
    Plants = n(),
    PercentCover = sum(Area) / (((60^2) * pi) / 4)*100,
    Density = Plants * 10000 / (((60^2) * pi) / 4),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

#======Prep Russ's Data from obs values=========================================

russ_data <- read.csv("./Data/SRG/SRG_WoodyCover_Oct_2014.csv")
avg_russ_data <- russ_data%>%
  group_by(CW_from_bearing)%>%
  mutate(Ht..cm. = Ht..cm./100,
         Diam..cm./100)%>%
  rename(Species = Plant,
         Quadrant = CW_from_bearing,
         Distance = Dist..m.,
         Height = Ht..cm.,
         CrownDiameter = Diam..cm.,
         Area = Area..m2.)%>%
  filter(Distance < 60)%>%
  #mutate(Area = pi*((CrownDiameter/2)^2))%>%
  arrange(Quadrant, Species)


russ_stats <- avg_russ_data %>%
  group_by(Quadrant, Species) %>%
  summarise(
    MeanHeight = mean(Height, na.rm = TRUE),
    SdHeight = sd(Height, na.rm = TRUE),
    MeanCanDi = mean(CrownDiameter, na.rm = TRUE)/100,
    SdCanDi = sd(CrownDiameter, na.rm = TRUE)/100,
    MeanArea = mean(Area, na.rm = TRUE),
    SdArea = sd(Area, na.rm = TRUE),
    Plants = n(),
    PercentCover = sum(Area) / (((60^2) * pi) / 4)*100,
    Density = Plants * 10000 / (((60^2) * pi) / 4),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

russ_stats <- russ_stats %>%
  mutate(Quadrant = case_when(
    Quadrant == 0   ~ "NE",
    Quadrant == 90  ~ "SE",
    Quadrant == 180 ~ "SW",
    Quadrant == 270 ~ "NW",
    TRUE            ~ as.character(Quadrant)))

#===merge and compare dataframes================================================
russ_stats <- russ_stats %>%
  mutate(Source = "Russ")

stats <- stats %>%
  mutate(Source = "Lindsey")
combined_stats <- bind_rows(russ_stats, stats)%>%
  filter(Species == "MES")

#===plot========================================================================
ggplot(combined_stats, aes(x = Species, y = MeanHeight, fill = Source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = MeanHeight - SdHeight, ymax = MeanHeight + SdHeight),
                position = position_dodge(width = 0.9), width = 0.2) +
  facet_wrap(~ Quadrant) +
  labs(title = "Mesquite: Mean Height",
       y = "Mean Height (m)",
       x = "Species") +
  theme_minimal() +
  scale_fill_manual(values = c("Russ" = "steelblue", "Lindsey" = "orange")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#===plot measurements with mean and sd------------------------------------------
mean_vars <- c("MeanHeight", "MeanCanDi", "MeanArea")
sd_vars   <- c("SdHeight",   "SdCanDi",   "SdArea")

mean_df <- combined_stats %>%
  select(Quadrant, Source, all_of(mean_vars)) %>%
  pivot_longer(cols = all_of(mean_vars), names_to = "Variable", values_to = "Mean")

sd_df <- combined_stats %>%
  select(Quadrant, Source, all_of(sd_vars)) %>%
  pivot_longer(cols = all_of(sd_vars), names_to = "Variable", values_to = "SD") %>%
  mutate(Variable = str_replace(Variable, "Sd", "Mean"))

stats_long <- left_join(mean_df, sd_df, by = c("Quadrant", "Source", "Variable"))

p1 <- ggplot(stats_long, aes(x = Quadrant, y = Mean, fill = Source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.8), width = 0.2) +
  facet_wrap(~ Variable, scales = "free_y", nrow = 1) +
  scale_fill_manual(values = c("Russ" = "#91bfdb", "Lindsey" = "#fc8d59")) +
  labs(
    title = "Mesquite",
    x = "",
    y = "Meters",
    fill = "Survey"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#===Vars w/o mean and sd--------------------------------------------------------
extra_vars <- c("Plants", "PercentCover", "Density")

extra_long <- combined_stats %>%
  select(Quadrant, Source, all_of(extra_vars)) %>%
  pivot_longer(cols = all_of(extra_vars), names_to = "Variable", values_to = "Value")

p2 <- ggplot(extra_long, aes(x = Quadrant, y = Value, fill = Source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  facet_wrap(~ Variable, scales = "free_y", nrow = 1) +
  scale_fill_manual(values = c("Russ" = "#91bfdb", "Lindsey" = "#fc8d59")) +
  labs(
    title = "",
    x = "Quadrant",
    y = "Value",
    fill = "Source"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

#combine plots
p1 / p2
