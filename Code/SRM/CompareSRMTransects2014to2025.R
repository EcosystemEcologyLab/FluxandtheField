
#===============================================================================


####script buggy- math/syntax needs to be fixed!!!###


#===============================================================================

# At the US-SRM flux tower site, we conducted line-intercept and belt transects 
# in eight directions (cardinal and subcardinal) to compare cover and density 
# estimates to a similar survey from 2014. 
# 
# This script executes the following steps:
#   1. Import field data
#   2. Format and visualize differences in line-intercept data
#   3. Format and visualize differences in belt transect data

#===============================================================================
#Load necessary packages--------------------------------------------------------
#===============================================================================
library(dplyr)
library(tidyverse)
library(ggplot2)

#===============================================================================
#Import field data--------------------------------------------------------------
#===============================================================================
LIT_Ldat <- read.csv("./Data/GitData/US-SRM_Transects_27052025.csv")
LIT_Rdat <- read.csv("./Data/GitData/SRM_Transects(RussDataSRM).csv")

#===============================================================================
#Compare line-intercept transect observations-----------------------------------
#===============================================================================
LIT_Lcov <- LIT_Ldat %>%
  filter(!is.na(Start)) %>%
  mutate(Cover = Stop - Start,
         CoverGroup = if_else(CoverType == "GRA", "Understory", "Overstory")) %>%
  group_by(Bearing, CoverGroup) %>%
  summarise(CoverTotal = (sum(Cover, na.rm = TRUE)),
            CoverPercent = (sum(Cover, na.rm = TRUE) / 60) * 100, .groups = "drop") %>%
  mutate(Survey = "2025")

LIT_Rcov <- LIT_Rdat %>%
  filter(SurveyType == "L") %>%
  mutate(CoverGroup = if_else(Species == "GRA", "Understory", "Overstory")) %>%
  group_by(Bearing, CoverGroup) %>%
  summarise(CoverTotal = (sum(Cover, na.rm = TRUE)),
            CoverPercent = (sum(Cover, na.rm = TRUE) / 60) * 100, .groups = "drop") %>%
  mutate(Survey = "2014")

LIT_quadcov <- bind_rows(LIT_Lcov, LIT_Rcov)%>%
  mutate(Bearing = as.character(Bearing))
#----total cover
LIT_Ltot <- LIT_Ldat %>%
  filter(!is.na(Start)) %>%
  mutate(Cover = Stop - Start,
         CoverGroup = if_else(CoverType == "GRA", "Understory", "Overstory")) %>%
  group_by(CoverGroup) %>%
  summarise(CoverTotal = (sum(Cover, na.rm = TRUE))) %>%
  mutate(Survey = "2025")

LIT_Rtot <- LIT_Rdat %>%
  filter(SurveyType == "L") %>%
  mutate(CoverGroup = if_else(Species == "GRA", "Understory", "Overstory")) %>%
  group_by( CoverGroup) %>%
  summarise(CoverTotal = (sum(Cover, na.rm = TRUE))) %>%
  mutate(Survey = "2014")

LIT_total <- bind_rows(LIT_Ltot, LIT_Rtot)%>%
  group_by(Survey, CoverGroup) %>%
  mutate(CoverPercent = ((sum(CoverTotal, na.rm = TRUE))/(60*8))*100,
         Bearing = "Total")%>%
  mutate(Bearing = as.character(Bearing))

alldat <- bind_rows(LIT_quadcov, LIT_total)

bearing_levels <- alldat$Bearing[alldat$Bearing != "Total"] %>%
  unique() %>%
  as.numeric() %>%
  sort() %>%
  as.character()
bearing_levels <- c("Total", bearing_levels)

alldat <- alldat %>%
  mutate(Bearing = factor(Bearing, levels = bearing_levels))

diffdat <- alldat %>%
  select(Bearing, CoverGroup, Survey, CoverPercent) %>%
  pivot_wider(names_from = Survey, values_from = CoverPercent) %>%
  drop_na(`2025`, `2014`) %>%  # ensure no NA values in either year
  mutate(
    Change = `2025` - `2014`,
    Direction = case_when(
      Change > 0 ~ "Increase",
      Change < 0 ~ "Decrease",
      TRUE ~ "No Change"
    )
  )
#plot comparison---------------        --------------          ------------  ---
ggplot(alldat, aes(x = factor(Bearing), y = CoverPercent, fill = Survey)) +
  geom_col(position = "dodge") +
  facet_wrap(~ CoverGroup) +
  labs(
    x = "Bearing",
    y = "Percent Cover (%)",
    fill = "Survey"
  ) +
  scale_fill_manual(values = c("2025" = "#fc8d59", "2014" = "#91bfdb")) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 13)
  )

ggplot(alldat, aes(x = factor(Bearing), 
                   y = CoverPercent, 
                   fill = interaction(Survey, CoverGroup, sep = " "))) +
  geom_col(position = "dodge") +
  labs(
    x = "Bearing",
    y = "Percent Cover (%)",
    fill = "Survey and Cover Type"
  ) +
  scale_fill_manual(values = c(
    "2025 Understory" = "#fdbb84",
    "2025 Overstory"  = "#fc8d59",
    "2014 Understory" = "#c6dbef",
    "2014 Overstory"  = "#91bfdb"
  )) +
  theme_minimal(base_size = 14)



ggplot(diffdat, aes(x = factor(Bearing), y = Change, fill = Direction)) +
  geom_col() +
  facet_wrap(~ CoverGroup) +
  scale_fill_manual(values = c("Increase" = "steelblue", "Decrease" = "firebrick", "No Change" = "gray")) +
  labs(
    x = "Bearing",
    y = "Change in Percent Cover (2025 - 2014)",
    fill = "Direction of Change"
  ) +
  theme_minimal(base_size = 14)

#===============================================================================
#Compare belt transect observations---------------------------------------------
#===============================================================================
BT_Ldat <- LIT_Ldat%>%
  filter(CanopyDiameter != is.na(CanopyDiameter))%>%
  mutate(BasalArea = ((BasalDiameter/2)^2)*pi)%>%
  group_by(ID)%>%
  reframe(Bearing = Bearing,
          Species = CoverType,
          Height = Height, 
          BasalArea = sum(BasalArea, na.rm = T))%>%
  filter(!is.na(Height))%>%
  select(-ID)
sptot <- BT_Ldat%>%
  group_by(Bearing)%>%
  reframe(TotalHeight = sum(Height, na.rm = T),
          TotalBasalArea = sum(BasalArea, na.rm = T))%>%
  mutate(PercTotalBasalArea = (TotalBasalArea/(60*2*10000))*100)%>%
  mutate(Survey = "2025")

  
BT_Rdat <- LIT_Rdat%>%
  filter(SurveyType == "B")%>%
  mutate(BasalArea = ((Diam/2)^2)*pi,
         Height = Height/100)%>%
  group_by(Bearing)%>%
  summarise(TotalHeight = sum(Height, na.rm = T),
            TotalBasalArea = sum(BasalArea, na.rm = T))%>%
  mutate(PercTotalBasalArea = (TotalBasalArea/(60*2*10000))*100)%>%
  mutate(Survey = "2014")

tot_BTdat <- bind_rows(sptot, BT_Rdat)


BT_total <- tot_BTdat %>%
  group_by(Survey)%>%
  summarise(
    TotalHeight = sum(TotalHeight, na.rm = TRUE),
    TotalBasalArea = sum(TotalBasalArea, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    PercTotalBasalArea = (TotalBasalArea / (60 * 2 * 10000)) * 100,
    Bearing = "Total"
  ) %>%
  select(Bearing, TotalHeight, TotalBasalArea, PercTotalBasalArea, Survey)
tot_BTdat <- tot_BTdat %>%
  mutate(Bearing = as.character(Bearing)) %>%
  bind_rows(BT_total)


bearing_levels <- tot_BTdat$Bearing[tot_BTdat$Bearing != "Total"] %>%
  unique() %>%
  as.numeric() %>%
  sort() %>%
  as.character()
bearing_levels <- c("Total", bearing_levels)

tot_BTdat <- tot_BTdat %>%
  mutate(Bearing = factor(Bearing, levels = bearing_levels))

bt_diff <- tot_BTdat %>%
  select(Bearing, Survey, TotalBasalArea) %>%
  pivot_wider(names_from = Survey, values_from = TotalBasalArea) %>%
  filter(!is.na(`2025`) & !is.na(`2014`)) %>%
  mutate(
    Change = `2025` - `2014`,
    Direction = case_when(
      Change > 0 ~ "Increase",
      Change < 0 ~ "Decrease",
      TRUE ~ "No Change"
    )
  )

#-----plotting--------
ggplot(tot_BTdat, aes(x = Bearing, y = TotalHeight, fill = Survey)) +
  geom_col(position = "dodge") +
  labs(
    x = "Bearing",
    y = "Total Height (m)",
    fill = "Survey",
    title = ""
  ) +
  scale_fill_manual(values = c("2025" = "#fc8d59", "2014" = "#91bfdb")) +
  theme_minimal(base_size = 14)

ggplot(tot_BTdat, aes(x = Bearing, y = PercTotalBasalArea, fill = Survey)) +
  geom_col(position = "dodge") +
  labs(
    x = "Bearing",
    y = "Percent Basal Area",
    fill = "Survey",
    title = ""
  ) +
  scale_fill_manual(values = c("2025" = "#fc8d59", "2014" = "#91bfdb")) +
  theme_minimal(base_size = 14)


ggplot(bt_diff, aes(x = Bearing, y = Change, fill = Direction)) +
  geom_col() +
  scale_fill_manual(values = c("Increase" = "steelblue", "Decrease" = "firebrick", "No Change" = "gray")) +
  labs(
    x = "Bearing",
    y = "Change in Basal Area (cm2) 2025 - 2014",
    fill = "Change",
    title = ""
  ) +
  theme_minimal(base_size = 14)

