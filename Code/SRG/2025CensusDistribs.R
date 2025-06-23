# At the US-SRG flux tower site, we conducted a woody plant census within a 100m
# radius of the tower. This script visualizes 2025 mesquite census data as 
# histograms of height, canopy diameter, basal diameter, stem count, and
# distance from the tower.
# 
# This script executes the following steps:
#   1. Formats 2025 data
#   2. Visualizes distributions of mesquite census observations with histograms

#===============================================================================
#Load necessary packages--------------------------------------------------------
#===============================================================================
library(dplyr)
library(ggplot2)
library(patchwork)

#===============================================================================
#Format 2025 data---------------------------------------------------------------
#===============================================================================
census_dat <- read.csv("./Data/GitData/US-SRG_WoodyPlantCensus_28052025.csv")
avg_census_dat <- census_dat%>%
  group_by(ID)%>%
  reframe(Species = Species,
            Distance = Distance,
            BasalDiameter = mean(BasalDiameter, na.rm = T),
            CrownDiameter = mean(CrownDiameter, na.rm = T),
            Height = Height, 
            StemCount = StemCount)%>%
  filter(Species != "")

mes_dat <- avg_census_dat%>%
  filter(Species == "MES")

#===============================================================================
#Create histograms--------------------------------------------------------------
#===============================================================================
HeightHist <- ggplot(avg_census_dat, aes(x = Height))+
  geom_histogram(binwidth = 0.5, fill = "steelblue", color ="black")+
  labs(title = "Height- All Overstory",
       x = "Height (m)",
       y = "Count")+
  theme_minimal()

MesHeightHist <- ggplot(mes_dat, aes(x = Height))+
  geom_histogram(binwidth = 0.5, fill = "steelblue", color ="black")+
  labs(title = "Mesquite Height Histogram",
       x = "Height (m)",
       y = "Count")+
  theme_minimal()

MesCanDiHist <- ggplot(mes_dat, aes(x = CrownDiameter))+
  geom_histogram(binwidth = 0.5, fill = "steelblue", color ="black")+
  labs(title = "Mesquite Crown Diameter Histogram",
       x = "Average Canopy Diameter (m)",
       y = "Count")+
  theme_minimal()

MesBasDiHist <- ggplot(mes_dat, aes(x = BasalDiameter))+
  geom_histogram(binwidth = 1, fill = "steelblue", color ="black")+
  labs(title = "Mesquite Basal Diameter Histogram",
       x = "Average Basal Diameter (cm)",
       y = "Count")+
  theme_minimal()

MesStemHist <- ggplot(mes_dat, aes(x = StemCount))+
  geom_histogram(binwidth = 1, fill = "steelblue", color ="black")+
  labs(title = "Mesquite Stem Count Histogram",
       x = "Stem Count",
       y = "Count")+
  theme_minimal()

MesDistHist <- ggplot(mes_dat, aes(x = Distance))+
  geom_histogram(binwidth = 2, fill = "steelblue", color ="black")+
  labs(title = "Mesquite Distance Histogram",
       x = "Distance from Tower",
       y = "Count")+
  theme_minimal()

(MesHeightHist + MesCanDiHist)/ (MesBasDiHist + MesStemHist) / MesDistHist
