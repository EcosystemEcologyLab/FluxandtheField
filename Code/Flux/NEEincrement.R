# At the US-SRG and US-SRM flux tower sites, ecosystem gas exchange observations 
# have been collected over the 11 years between the 2014 and 2025 vegetation 
# surveys. This script calculates and compares the NEE increments between sites
# using available data (2014 to 2023). 
# 
# This script executes the following steps:
#   1. Formats US-SRG and US-SRM flux data
#   2. Visualizes monthly NEE from 2014 to 2023

#===============================================================================
#Load necessary packages--------------------------------------------------------
#===============================================================================
library(dplyr)
library(ggplot2)
library(lubridate)

#===============================================================================
#Format flux data (both sites)--------------------------------------------------
#===============================================================================
srg_flux <- read.csv("./Data/GitData/AMF_US-SRG_FLUXNET_SUBSET_MM_2008-2023_4-6.csv") %>%
  select(TIMESTAMP, NEE_VUT_REF) %>%
  mutate(TIMESTAMP = ym(TIMESTAMP),
         Year = year(TIMESTAMP),
         Month = month(TIMESTAMP)) %>%
  filter(Year %in% 2014:2023) %>%
  mutate(CumulativeNEE = cumsum(NEE_VUT_REF),
         Site = "US-SRG")

srm_flux <- read.csv("./Data/GitData/AMF_US-SRM_FLUXNET_SUBSET_MM_2004-2023_3-6.csv") %>%
  select(TIMESTAMP, NEE_VUT_REF) %>%
  mutate(TIMESTAMP = ym(TIMESTAMP),
         Year = year(TIMESTAMP),
         Month = month(TIMESTAMP)) %>%
  filter(Year %in% 2014:2023) %>%
  mutate(CumulativeNEE = cumsum(NEE_VUT_REF),
         Site = "US-SRM")

combined_flux <- bind_rows(srg_flux, srm_flux)%>%
  mutate(Site = factor(Site, level = c("US-SRM","US-SRG")))
nee_increment <- combined_flux %>%
  group_by(Site) %>%
  summarise(
    Start = first(CumulativeNEE),
    End = last(CumulativeNEE),
    Increment = End - Start
  )

#===============================================================================
#Visualize monthly NEE from 2014 to 2023----------------------------------------
#===============================================================================
srgNEE <- ggplot(srg_flux, aes(x = TIMESTAMP, y = CumulativeNEE))+
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

srmNEE <- ggplot(srm_flux, aes(x = TIMESTAMP, y = CumulativeNEE))+
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

compNEE <- ggplot(combined_flux, aes(x = TIMESTAMP, y = CumulativeNEE, color = Site)) +
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
compNEE + 
  geom_text(
    data = nee_increment,
    aes(x = as.Date("2023-12-01"), 
        y = End, 
        label = paste0("ΔNEE: ", round(Increment, 1), " µmolCO2 m-2")),
    hjust = 1, vjust = -3, size = 4.5, color = "black", inherit.aes = FALSE
  )

