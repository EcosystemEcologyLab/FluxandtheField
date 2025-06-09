#histogram of height canopy di and basal di

library(dplyr)
library(ggplot2)

census_dat <- read.csv("X:/moore/FieldData/DataSpreadsheets/US-SRG_WoodyPlantCensus_28052025.csv")
avg_census_dat <- census_dat%>%
  group_by(ID)%>%
  summarize(Species = Species,
            Distance = Distance,
            BasalDiameter = mean(BasalDiameter, na.rm = T),
            CrownDiameter = mean(CrownDiameter, na.rm = T),
            Height = Height, 
            StemCount = StemCount)%>%
  filter(Species != "")

HeightHist <- ggplot(avg_census_dat, aes(x = Height))+
  geom_histogram(binwidth = 0.5, fill = "steelblue", color ="black")+
  labs(title = "Height Histogram for All Overstory",
       x = "Height (m)",
       y = "Count")+
  theme_minimal()


mes_dat <- avg_census_dat%>%
  filter(Species == "MES")
  
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
