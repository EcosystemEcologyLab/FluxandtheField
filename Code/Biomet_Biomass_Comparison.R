#Biometric Gradient Comparison: 5 biomass estimation methods
library(dplyr)
library(terra)
library(tidyverse)

SRGdat <-  read.csv("./Data/GitData/US-SRG_BiometGrad_07062025.csv")%>%
  mutate(Site = "US-SRG")%>%
  group_by(ID)%>%
  summarize(BasalDi = mean(BasalDiameter, na.rm = T),
            CanopyDi = mean(CanopyDiameter, na.rm = T))%>%
  mutate(Basal_bio = exp((-2.9255) + 2.4109 * log(BasalDi)))%>% #in kg
  mutate(area = pi*((CanopyDi/2)^2))

model <- nls(Basal_bio ~ a * area^b,
             data = SRGdat,
             start = list(a = 0.1, b = 1))

SRGdat$Canopy_bio <- predict(model, newdata = SRGdat)

vox_est <- readRDS("./Data/biomet_vox_biomass.RDS")%>%
  rename(Vox_bio = Biomass_kg)%>%
  select(ID, Vox_bio)

biomet_dat <- merge(SRGdat, vox_est, by = "ID")


#-----canopy detection
corrected_coords <- vect("./QGIS/SRGCorrectedTreePoints.shp")
manu_polys <- vect("./Data/GitData/canopypolys.shp")
auto_polys <- vect("./Data/testJiamingcrowns2.shp")
survey_outline <- vect("./QGIS/100msurveybuffer.shp")

manu_polys <- crop(manu_polys, survey_outline)
auto_polys <- crop(auto_polys, survey_outline)

corrected_coords <- project(corrected_coords, crs(manu_polys))

if (!("polyID" %in% names(manu_polys)) || all(is.na(manu_polys$polyID))) {
  manu_polys$polyID <- 1:nrow(manu_polys)
}

if (!("polyID" %in% names(auto_polys)) || all(is.na(auto_polys$polyID))) {
  auto_polys$polyID <- 1:nrow(auto_polys)
}

manual_mapping <- data.frame(
  TreeID = 1:10,
  polyID = c(243, 248, 339, 335, 141, 142, 306, 26, 32, 3)
)

auto_mapping <- data.frame(
  TreeID = c(1, 2, 7, 5, 4, 10, 8),
  polyID = c(125, 221, 330, 334, 356, 243, 246)
)

match_points_to_polygons <- function(points, polygons, model, method_name, mapping = NULL) {
  matched_polys <- list()
  
  for (i in seq_len(nrow(points))) {
    matches <- which(relate(polygons, points[i], "intersects"))
    if (length(matches) > 0) {
      matched_polys[[i]] <- polygons[matches[1], ]
    }
  }
  
  if (length(matched_polys) == 0) return(data.frame())
  
  matched_vec <- do.call(rbind, matched_polys)
  matched_vec$area <- expanse(matched_vec, unit = "m")
  
  if (!is.null(mapping)) {
    df <- as.data.frame(matched_vec)
    df <- df %>%
      left_join(mapping, by = "polyID") %>%
      rename(ID = TreeID)
    matched_vec$ID <- df$ID
  }
  
  df <- as.data.frame(matched_vec)
  df$predicted_biomass <- predict(model, newdata = df)
  df$method <- method_name
  return(df)
}

manual_df <- match_points_to_polygons(
  points = corrected_coords,
  polygons = manu_polys,
  model = model,
  method_name = "Manual",
  mapping = manual_mapping
)

man_df <- manual_df%>%
  rename(manu_bio = predicted_biomass)%>%
  select(ID, manu_bio)

auto_df <- match_points_to_polygons(
  points = corrected_coords,
  polygons = auto_polys,
  model = model,
  method_name = "Automatic",
  mapping = auto_mapping
)


auto_mapping <- data.frame(
  BiometID = c(1, 2, 7, 5, 4, 10, 8),
  treeID = c(125, 221, 330, 334, 356, 243, 246)
)
auto_df <- merge(auto_df, auto_mapping, by = "treeID")
auto_df <- auto_df%>%
  rename(auto_bio = predicted_biomass)%>%
  select(BiometID, auto_bio)%>%
  rename(ID = BiometID)

candetection <- left_join(man_df, auto_df, by = "ID")


#---------complete dataframe---------

biomet_comp <- merge(biomet_dat, candetection, by = "ID")

#---------plot------------------------------------------------------------------
biomet_long <- biomet_comp %>%
  pivot_longer(
    cols = ends_with("_bio"),
    names_to = "method",
    values_to = "biomass"
  ) %>%
  mutate(method = recode(method,
                         Basal_bio = "Basal Area",
                         Canopy_bio = "Canopy Area",
                         manu_bio = "Manual Crown",
                         auto_bio = "Automated Crown",
                         Vox_bio = "Voxelization"),
         method = factor(method, levels = c("Basal Area", "Canopy Area", "Manual Crown", "Automated Crown", "Voxelization")))

ggplot(biomet_long, aes(x = factor(ID), y = biomass, fill = method)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_viridis_d(option = "D", end = 0.85, name = "Method") +
  labs(x = "Tree ID", y = "Biomass (kg)", title = "") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  )




