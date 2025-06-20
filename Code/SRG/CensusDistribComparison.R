#compare distributions of Russ and Lindsey's data

library(dplyr)
library(ggplot2)
library(lubridate)
library(patchwork)

lindsey_dat <- read.csv("Z:/MooreSRER/FieldData/DataSpreadsheets/US-SRG_WoodyPlantCensus_28052025.csv")
russ_dat <- read.csv("./Data/SRG/SRG_WoodyCover_Oct_2014.csv")

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

#---Plot Histograms=============================================================
combd_dat$Survey <- factor(combd_dat$Survey, levels = c("2025", "2014"))

ggplot(combd_dat, aes(x = Height, fill = Survey)) +
  geom_histogram(position = "identity", alpha = 0.85, bins = 30) +
  scale_fill_manual(values = c("2014" = "#91bfdb", "2025" = "#fc8d59")) +
  theme_minimal() +
  labs(title = "Mesquite Height Distribution",
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
  scale_fill_manual(values = c("2014" = "#91bfdb", "2025" = "#fc8d59")) +
  theme_minimal() +
  labs(title = "Mesquite Crown Diameter Distribution",
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

#---NEE increment===============================================================
srg_flux <- read.csv("./Data/FluxData/AMF_US-SRG_FLUXNET_SUBSET_MM_2008-2023_4-6.csv") %>%
  select(TIMESTAMP, NEE_VUT_REF) %>%
  mutate(TIMESTAMP = ym(TIMESTAMP),
         Year = year(TIMESTAMP),
         Month = month(TIMESTAMP)) %>%
  filter(Year %in% 2014:2023) %>%
  mutate(CumulativeNEE = cumsum(NEE_VUT_REF),
         Site = "US-SRG")
p1 <- ggplot(srg_flux, aes(x = TIMESTAMP, y = CumulativeNEE))+
  geom_point()+
  theme_minimal()+
  labs(title = "US-SRG Monthly Cumulative NEE",
       x = "",
       y = "NEE (µmolCO2 m-2 s-1)")+
  theme(
    plot.title = element_text("US-SRM Monthly Cumulative NEE", size = 18),
    axis.title.y = element_text(size = 13),
    axis.text = element_text(size = 13)
  )

srm_flux <- read.csv("./Data/FluxData/AMF_US-SRM_FLUXNET_SUBSET_MM_2004-2023_3-6.csv") %>%
  select(TIMESTAMP, NEE_VUT_REF) %>%
  mutate(TIMESTAMP = ym(TIMESTAMP),
         Year = year(TIMESTAMP),
         Month = month(TIMESTAMP)) %>%
  filter(Year %in% 2014:2023) %>%
  mutate(CumulativeNEE = cumsum(NEE_VUT_REF),
         Site = "US-SRM")
p2 <- ggplot(srm_flux, aes(x = TIMESTAMP, y = CumulativeNEE))+
  geom_point()+
  theme_minimal()+
  labs(title = "US-SRM Monthly Cumulative NEE",
       x = "",
       y = "NEE (µmolCO2 m-2 s-1)")+
  theme(
    plot.title = element_text("US-SRM Monthly Cumulative NEE", size = 18),
    axis.title.y = element_text(size = 13),
    axis.text = element_text(size = 13)
  )

combined_flux <- bind_rows(srg_flux, srm_flux)%>%
  mutate(Site = factor(Site, level = c("US-SRM","US-SRG")))
nee_increment <- combined_flux %>%
  group_by(Site) %>%
  summarise(
    Start = first(CumulativeNEE),
    End = last(CumulativeNEE),
    Increment = End - Start
  )

# Build the base plot
p <- ggplot(combined_flux, aes(x = TIMESTAMP, y = CumulativeNEE, color = Site)) +
  geom_point(size = 1) +
  theme_minimal() +
  labs(
    title = "Monthly Cumulative NEE (2014–2023)",
    x = "",
    y = "Cumulative NEE (µmolCO2 m-2)",
    color = "Site"
  ) +
  theme(
    plot.title = element_text(size = 18),
    axis.title.y = element_text(size = 13),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12)
  )

# Add text annotations
p + 
  geom_text(
    data = nee_increment,
    aes(x = as.Date("2023-12-01"), 
        y = End, 
        label = paste0("ΔNEE: ", round(Increment, 1), " µmolCO2 m-2")),
    hjust = 1, vjust = -3, size = 4.5, color = "black", inherit.aes = FALSE
  )

