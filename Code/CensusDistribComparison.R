#compare distributions of Russ and Lindsey's data

library(dplyr)
library(ggplot2)

lindsey_dat <- read.csv("X:/moore/FieldData/DataSpreadsheets/US-SRG_WoodyPlantCensus_28052025.csv")
russ_dat <- read.csv("./Data/SRG_WoodyCover_Oct_2014.csv")

#---Prep dataframes=============================================================

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
  mutate(Survey = "Lindsey")%>%
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
  mutate(Survey = "Russ")%>%
  filter(Species == "MES")

combd_dat <- bind_rows(filt_lindsey_dat, filt_russ_dat)

#---Plot Histograms=============================================================
ggplot(combd_dat, aes(x = Height, fill = Survey)) +
  geom_histogram(position = "identity", alpha = 0.85, bins = 30) +
  scale_fill_manual(values = c("Russ" = "#91bfdb", "Lindsey" = "#fc8d59")) +
  theme_minimal() +
  labs(title = "Mesquite Height Distribution by Survey",
       x = "Height (m)",
       y = "Count",
       fill = "Survey")+
  theme(
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 18),
    axis.text = element_text(size = 13),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 15)
    
  )

ggplot(combd_dat, aes(x = CrownDiameter, fill = Survey)) +
  geom_histogram(position = "identity", alpha = 0.85, bins = 30) +
  scale_fill_manual(values = c("Russ" = "#91bfdb", "Lindsey" = "#fc8d59")) +
  theme_minimal() +
  labs(title = "Mesquite Crown Diameter Distribution by Survey",
       x = "Crown Diameter (m)",
       y = "Count",
       fill = "Survey")+
  theme(
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 18),
    axis.text = element_text(size = 13),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 15))

#---Stat tests==================================================================
ks.test(filt_lindsey_dat$Height, filt_russ_dat$Height)
# ^ distribs are sig dif
ks.test(filt_lindsey_dat$CrownDiameter, filt_russ_dat$CrownDiameter)
# ^ distribs are sig dif

wilcox.test(Height ~ Survey, data = combd_dat)
# ^ means are sig dif
wilcox.test(CrownDiameter ~ Survey, data = combd_dat)
# ^ means are sig dif
