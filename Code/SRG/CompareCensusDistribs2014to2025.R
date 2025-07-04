# At the US-SRG flux tower site, we conducted a woody plant census within a 100m
# radius of the tower to compare to a similar census from 2014. This script 
# compares the distributions of mesquite heights and crown diameters between the
# two surveys.
# 
# This script executes the following steps:
#   1. Formats and combines 2014 and 2025 data into a single data frame
#   2. Visualizes height and diameter distributions with histograms
#   3. Performs Kolmogorov-Smirnov and Wilcoxon statistical tests

#===============================================================================
#Load necessary packages--------------------------------------------------------
#===============================================================================
library(dplyr)
library(ggplot2)
library(patchwork)

#===============================================================================
#Format and combine data into a single data frame-------------------------------
#===============================================================================
lindsey_dat <- read.csv("./Data/GitData/US-SRG_WoodyPlantCensus_28052025.csv")
russ_dat <- read.csv("./Data/GitData/SRG_WoodyCover_Oct_2014.csv")

filt_lindsey_dat <- lindsey_dat%>%
  group_by(ID)%>%
  summarize(Species = Species,
            Distance = Distance,
            CrownDiameter = mean(CrownDiameter, na.rm = T),
            Height = Height)%>%
  filter(Species != "",
         Distance < 60)%>%
  ungroup()%>%
  select(-ID)%>%
  mutate(Survey = "2025")%>%
  filter(Species == "MES")

filt_russ_dat <- russ_dat%>%
  filter(Dist..m. < 60)%>%
  select(Plant, Dist..m., Ht..cm., Diam..cm.)%>%
  mutate(Ht..cm. = Ht..cm./100,
         Diam..cm. = Diam..cm./100)%>%
  rename(Species = Plant, 
         Distance = Dist..m.,
         Height = Ht..cm.,
         CrownDiameter = Diam..cm.)%>%
  mutate(Survey = "2014")%>%
  filter(Species == "MES")

combd_dat <- bind_rows(filt_lindsey_dat, filt_russ_dat)
combd_dat$Survey <- factor(combd_dat$Survey, levels = c("2025", "2014"))

#===============================================================================
#Plot height and canopy diameter histograms-------------------------------------
#===============================================================================
TrHgt <- ggplot(combd_dat, aes(x = Height, fill = Survey)) +
  geom_histogram(position = "identity", alpha = 0.85, bins = 30) +
  scale_fill_manual(values = c("2014" = "#91bfdb", "2025" = "#fc8d59")) +
  theme_minimal() +
  labs(title = "",
       x = "Height (m)",
       y = "Count",
       fill = "Survey")+
  theme(
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 18),
    axis.text = element_text(size = 13),
    legend.position = "none")

CanDi <- ggplot(combd_dat, aes(x = CrownDiameter, fill = Survey)) +
  geom_histogram(position = "identity", alpha = 0.85, bins = 30) +
  scale_fill_manual(values = c("2014" = "#91bfdb", "2025" = "#fc8d59")) +
  theme_minimal() +
  labs(title = "",
       x = "Crown Diameter (m)",
       y = "Count",
       fill = "Survey")+
  theme(
    axis.title.x = element_text(size = 15),
    plot.title = element_text(size = 18),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.title.y = element_blank(),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 15))

TrHgt + CanDi

#===============================================================================
#Statistically compare distributions--------------------------------------------
#===============================================================================
ks.test(filt_lindsey_dat$Height, filt_russ_dat$Height)
# ^ distribs are sig dif
ks.test(filt_lindsey_dat$CrownDiameter, filt_russ_dat$CrownDiameter)
# ^ distribs are sig dif

wilcox.test(Height ~ Survey, data = combd_dat)
# ^ means are sig dif
wilcox.test(CrownDiameter ~ Survey, data = combd_dat)
# ^ means are sig dif
