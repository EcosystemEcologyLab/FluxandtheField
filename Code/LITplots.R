#SRM line intercept transects

library(ggplot2)

LIT_dat <- read.csv("X:/moore/FieldData/DataSpreadsheets/US-SRM_Transects_27052025.csv")

MesHeightHist <- ggplot(LIT_dat, aes(x = Height))+
  geom_histogram(binwidth = 0.5, fill = "steelblue", color ="black")+
  labs(title = "",
       x = "Height (m)",
       y = "Count")+
  theme_minimal()
