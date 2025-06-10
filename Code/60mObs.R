#rad census to 60m

library(dplyr)

census_dat <- read.csv("X:/moore/FieldData/DataSpreadsheets/US-SRG_WoodyPlantCensus_28052025.csv")
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

write.csv(avg_census_dat, "./60mObs.csv")

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

