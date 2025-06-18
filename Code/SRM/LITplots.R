#SRM line intercept transects

library(dplyr)
library(ggplot2)

LIT_Ldat <- read.csv("Z:/MooreSRER/FieldData/DataSpreadsheets/US-SRM_Transects_27052025.csv")
LIT_Rdat <- read.csv("./Data/SRM/SRM_Transects(RussDataSRM).csv")

#overstory cover----------------------------------------------------------------
LIT_Lcov <- LIT_Ldat%>%
  filter(CoverType != "GRA",
         Start != is.na(Start))%>%
  mutate(Cover = Stop - Start)%>%
  group_by(Bearing)%>%
  summarise(TotalCover = (sum(Cover, na.rm = T)/60)*100)%>%
  mutate(Survey = "2025")

LIT_Rcov <- LIT_Rdat%>%
  filter(SurveyType == "L")%>%
  group_by(Bearing)%>%
  summarise(TotalCover = (sum(Cover, na.rm = T)/60)*100)%>%
  mutate(Survey = "2014")

LIT_allcov <- bind_rows(LIT_Lcov, LIT_Rcov)%>%
  mutate(Bearing = as.character(Bearing))
survey_totals <- LIT_allcov %>%
  group_by(Survey) %>%
  summarise(
    Bearing = "Total",
    TotalCover = ((sum(TotalCover, na.rm = TRUE))/(60*8))*100,
    .groups = "drop"
  )
LIT_allcov <- bind_rows(LIT_allcov, survey_totals)

bearing_levels <- LIT_allcov$Bearing[!LIT_allcov$Bearing %in% "Total"] %>%
  unique() %>%
  as.numeric() %>%
  sort() %>%
  as.character() %>%
  c("Total")

LIT_allcov <- LIT_allcov %>%
  mutate(Bearing = factor(Bearing, levels = bearing_levels))


#plot comparison---------------        --------------          ------------  ---
ggplot(LIT_allcov, aes(x = factor(Bearing), y = TotalCover, fill = Survey)) +
  geom_col(position = "dodge") +
  labs(
    x = "Bearing",
    y = "Total Cover (%)",
    fill = "Survey",
    title = "Percent Overstory Cover by Transect"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("2025" = "#fc8d59", "2014" = "#91bfdb"))+
  theme(
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 18),
    axis.text = element_text(size = 13),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 15)
    
  )

#===============================================================================
#Belt transect canopy cover comparison 

BT_Ldat <- LIT_Ldat%>%
  filter(CanopyDiameter != is.na(CanopyDiameter))%>%
  group_by(ID)%>%
  reframe(Bearing = Bearing,
          Species = CoverType,
            BasDi = mean(BasalDiameter, na.rm = T))%>%
  group_by(Bearing, Species)%>%
  summarize(TotalBasDi = sum(BasDi, na.rm = T))#%>%
  mutate(Area = pi*((TotalBasDi/2)^2))%>%
  mutate(PercArea = (Area/1200000)*100)

BT_Ldat <- LIT_Ldat%>%
    filter(CanopyDiameter != is.na(CanopyDiameter))%>%
    group_by(ID)%>%
    reframe(Bearing = Bearing,
            #Species = CoverType,
            CanDi = mean(CanopyDiameter, na.rm = T))%>%
    group_by(Bearing)%>%
    summarize(TotalCanDi = sum(CanDi, na.rm = T))#%>%
  mutate(Area = pi*((TotalBasDi/2)^2))%>%
    mutate(PercArea = (Area/1200000)*100)

  
BT_Rdat <- LIT_Rdat%>%
  filter(SurveyType == "B")%>%
  group_by(Bearing)%>%
  summarise(TotalCanDi = sum(Diam, na.rm = T)/100)%>%
  mutate(Area = pi*((TotalCanDi/2)^2))%>%
  mutate(PercArea = (Area/120)*100)

