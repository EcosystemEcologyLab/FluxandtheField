# At the US-SRG flux tower site, we conducted a woody plant census within a 100m
# radius of the tower to compare to a similar census from 2014. This script 
# compares average values by survey quadrant between the two collections. 
# 
# This script executes the following steps:
#   1. Formats 2025 data
#   2. Formats 2014 data
#   3. Combines datasets into a single data frame and calculate mean, st.dev
#   4. Visualizes and compares average measurement values by quadrant

#===============================================================================
#Load necessary packages--------------------------------------------------------
#===============================================================================
library(dplyr)
library(ggplot2)
library(tidyverse)
library(patchwork)

#===============================================================================
#Format 2025 data---------------------------------------------------------------
#===============================================================================
census_dat <- read.csv("./Data/GitData/US-SRG_WoodyPlantCensus_28052025.csv")
avg_census_dat <- census_dat%>%
  group_by(ID)%>%
  reframe(Species = Species,
            Quadrant = Quadrant,
            Distance = Distance,
            BasalDiameter = mean(BasalDiameter, na.rm = T),
            CrownDiameter = mean(CrownDiameter, na.rm = T),
            Height = Height)%>%
  filter(Species != "",
         Distance < 60)%>%
  mutate(Area = pi*((CrownDiameter/2)^2))%>%
  arrange(Quadrant, Species)

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

#===============================================================================
#Format 2014 data---------------------------------------------------------------
#===============================================================================
russ_data <- read.csv("./Data/GitData/SRG_WoodyCover_Oct_2014.csv")
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

#===============================================================================
#Combine dataframes and calc stats----------------------------------------------
#===============================================================================
russ_stats <- russ_stats %>%
  mutate(Survey = "2014")
stats <- stats %>%
  mutate(Survey = "2025")

combined_stats <- bind_rows(russ_stats, stats)%>%
  filter(Species == "MES")


#Variables with sd: 
mean_vars <- c("MeanHeight", "MeanCanDi", "MeanArea")
sd_vars   <- c("SdHeight",   "SdCanDi",   "SdArea")

mean_df <- combined_stats %>%
  select(Quadrant, Survey, all_of(mean_vars)) %>%
  pivot_longer(cols = all_of(mean_vars), names_to = "Variable", values_to = "Mean")

sd_df <- combined_stats %>%
  select(Quadrant, Survey, all_of(sd_vars)) %>%
  pivot_longer(cols = all_of(sd_vars), names_to = "Variable", values_to = "SD") %>%
  mutate(Variable = str_replace(Variable, "Sd", "Mean"))

stats_long <- left_join(mean_df, sd_df, by = c("Quadrant", "Survey", "Variable"))


#Variables without sd: 
extra_vars <- c("Plants", "PercentCover", "Density")

extra_long <- combined_stats %>%
  select(Quadrant, Survey, all_of(extra_vars)) %>%
  pivot_longer(cols = all_of(extra_vars), names_to = "Variable", values_to = "Value")

#===============================================================================
#Plot measurements with/without mean and sd-------------------------------------
#===============================================================================
#Vars w/ mean and sd
p1 <- ggplot(stats_long, aes(x = Quadrant, y = Mean, fill = Survey)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.8), width = 0.2) +
  facet_wrap(~ Variable, scales = "free_y", nrow = 1) +
  scale_fill_manual(values = c("2014" = "#91bfdb", "2025" = "#fc8d59")) +
  labs(
    title = "Comparison of Mesquite Observations",
    x = "",
    y = "Meters",
    fill = "Survey"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#Vars w/o mean and sd
p2 <- ggplot(extra_long, aes(x = Quadrant, y = Value, fill = Survey)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  facet_wrap(~ Variable, scales = "free_y", nrow = 1) +
  scale_fill_manual(values = c("2014" = "#91bfdb", "2025" = "#fc8d59")) +
  labs(
    title = "",
    x = "Quadrant",
    y = "Value",
    fill = "Survey"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

#combine plots
p1 / p2
