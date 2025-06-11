#biomet grad and hypsometer error

library(dplyr)
library(ggplot2)
library(patchwork)

dat <-  read.csv("X:/moore/FieldData/DataSpreadsheets/US-SRG_BiometGrad_07062025.csv")

height_summary <- dat %>%
  group_by(ID) %>%
  summarize(
    mean_height = mean(CanopyHeight, na.rm = TRUE),
    se_height = sd(CanopyHeight, na.rm = TRUE) / sqrt(n()))

height_summary <- height_summary %>%
  mutate(ID = factor(ID, levels = ID[order(mean_height)]))

p1 <- ggplot(height_summary, aes(x = ID, y = mean_height)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = mean_height - se_height, ymax = mean_height + se_height),
                width = 0.2) +
  labs(x = "Tree ID #", y = "Average Canopy Height (m)",
       title = "") +
  theme_minimal()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 13))



CanDi_summary <- dat %>%
  group_by(ID) %>%
  summarize(
    mean_di = mean(CanopyDiameter, na.rm = TRUE),
    se_di = sd(CanopyDiameter, na.rm = TRUE) / sqrt(n()))

CanDi_summary <- CanDi_summary %>%
  mutate(ID = factor(ID, levels = ID[order(mean_di)]))

p2 <- ggplot(CanDi_summary, aes(x = ID, y = mean_di)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = mean_di - se_di, ymax = mean_di + se_di),
                width = 0.2) +
  labs(x = "Tree ID #", y = "Average Canopy Diameter (m)",
       title = "") +
  theme_minimal()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 13))


BasDi_summary <- dat %>%
  group_by(ID) %>%
  summarize(
    mean_di = mean(BasalDiameter, na.rm = TRUE),
    se_di = sd(BasalDiameter, na.rm = TRUE) / sqrt(n()))

BasDi_summary <- BasDi_summary %>%
  mutate(ID = factor(ID, levels = ID[order(mean_di)]))

p3 <- ggplot(BasDi_summary, aes(x = ID, y = mean_di)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = mean_di - se_di, ymax = mean_di + se_di),
                width = 0.2) +
  labs(x = "Tree ID #", y = "Average Basal Diameter (m)",
       title = "") +
  theme_minimal()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 13))


p1+p2+p3
