#observations: 
library(dplyr)
library(terra)
library(ggplot2)

veg_dat <- read.csv("./Data/GitData/US-SRG_WoodyPlantCensus_28052025.csv")%>%
  filter(Distance < 60)%>%
  group_by(ID)%>%
  reframe(Quadrant, 
          Species, 
          CanDi = mean(CrownDiameter, na.rm = T))%>%
  group_by(ID)%>%
  slice(1)

obsdf <- veg_dat%>%
  filter(Species %in% c("ACIA", "CHAC", "GTN", "HAC", "MES", "WAC", "WTA", "Salix"))%>%
  mutate(area = pi*((CanDi/2)^2))

#===========================
#model from biomet grad

SRGdat <-  read.csv("./Data/GitData/US-SRG_BiometGrad_07062025.csv")%>%
  mutate(Site = "US-SRG")%>%
  group_by(ID)%>%
  summarize(BasalDi = mean(BasalDiameter, na.rm = T),
            CanopyDi = mean(CanopyDiameter, na.rm = T))%>%
  mutate(biomass = exp((-2.9255) + 2.4109 * log(BasalDi)))%>% #in kg
  #select(-BasalDi)%>%
  mutate(area = pi*((CanopyDi/2)^2),
         method = "Allometry")

model <- nls(biomass ~ a * area^b,
             data = SRGdat,
             start = list(a = 0.1, b = 1)
)

# ggplot(SRGdat, aes(x = area, y = biomass)) +
#   geom_point() +
#   stat_function(fun = function(x) {
#     coef(model)["a"] * x^coef(model)["b"]
#   }, color = "blue", size = 1) +
#   labs(title = "")

#===============================================================================
#predict polygon biomass from model above

manu_output <- vect("./Data/GitData/canopypolys.shp")
auto_output <- vect("./Data/testJiamingcrowns2.shp")
survey_outline <- vect("./QGIS/100msurveybuffer.shp")

manushp <- crop(manu_output, survey_outline)
autoshp <- crop(auto_output, survey_outline)

manushp$area <- expanse(manushp, unit = "m")
autoshp$area <- expanse(autoshp, unit = "m")
manudf <- as.data.frame(manushp)
autodf <- as.data.frame(autoshp)
manudf$predicted_biomass <- predict(model, newdata = manudf)
autodf$predicted_biomass <- predict(model, newdata = autodf)
obsdf$predicted_biomass <- predict(model, newdata = obsdf)


manudf$method <- "Manual"
autodf$method <- "Automatic"
obsdf <- obsdf%>%
  mutate(method = "Observed")%>%
  rename(id = ID,
         canopy_area = area)

# Combine both into one data frame
biomass_df <- bind_rows(manudf, autodf, obsdf)

#-----biomass totals------
biomass_df <- biomass_df %>%
  mutate(biomass_Mg = predicted_biomass / 1000)  # 1 Mg = 1000 kg
total_biomass_by_method <- biomass_df %>%
  group_by(method) %>%
  summarize(total_biomass_Mg = sum(biomass_Mg, na.rm = TRUE))

#plotting-------------------------------
# Plot density distribution
ggplot(biomass_df, aes(x = predicted_biomass, fill = method)) +
  geom_density(alpha = 0.5) +
  scale_fill_viridis_d(option = "C") +
  labs(x = "Predicted Biomass (kg)", y = "Density",
       title = "Distribution of Predicted Biomass by Detection Method",
       fill = "Method") +
  theme_minimal()


ggplot(biomass_df, aes(x = predicted_biomass, fill = method)) +
  geom_histogram(alpha = 0.5, bins = 30, position = "identity") +
  scale_fill_viridis_d(option = "C") +
  labs(x = "Predicted Biomass (kg)", y = "Count",
       title = "Histogram of Biomass by Method",
       fill = "Method") +
  theme_minimal()



ggplot(biomass_df, aes(x = predicted_biomass, fill = method, color = method)) +
  geom_histogram(aes(y = ..density..), alpha = 0.2, position = "identity", bins = 30) +
  geom_density(alpha = 0.4, size = 1) +  # transparency here
  scale_x_log10() +  # Optional: apply if skewed
  scale_fill_viridis_d(option = "C") +
  scale_color_viridis_d(option = "C") +
  labs(x = "Predicted Biomass (kg, log scale)", y = "Density",
       title = "",
       fill = "Method", color = "Method") +
  theme_minimal()


