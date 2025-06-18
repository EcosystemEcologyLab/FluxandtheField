#SRG canopy heights

library(lidR)
library(sf)
library(terra)
library(ForestTools)
library(dplyr)
library(ggplot2)
library(patchwork)

LidarPath <- "Z:/Drone_Wim/SRERGrass5-28-25/SRERLidar5-28-25/lidars/terra_las/cloud_merged.las"

las <- readLAS(LidarPath)

if (is.null(las@data$Classification) || !any(las@data$Classification %in% c(2))) {
  las <- classify_ground(las, algorithm = csf())
}
dtm <- rasterize_terrain(las, res = 0.1, algorithm = knnidw(k = 10, p = 2))

dsm <- rasterize_canopy(las, res = 0.1, algorithm = p2r(subcircle = 0.15))

chm <- dsm - dtm
writeRaster(chm_zoom, "./Data/SRGchmzoomed.tif")
#saveRDS(chm, "./Data/SRGchm.RDS")
chm[chm < 0 | chm > 20] <- NA


#Use manually traced polygons to calc canopy stats

sfpath <- "./QGIS/canopypolys.shp"
canpol <- vect(sfpath)
plot(canpol)

chm_crop <- crop(chm, canpol)
chm_masked <- mask(chm_crop, canpol)

max_height <- extract(chm_masked, canpol, fun = max, na.rm = T)
names(max_height)[2] <- "Height"

areas <- expanse(canpol, unit = "m")
diameters <- 2 * sqrt((areas / pi))

TreeMeas <- data.frame(
  ID = canpol$id,
  Height = max_height$Height,
  Area = areas,
  Diameter = diameters,
  Survey = "Drone"
)%>%
  select(-ID)

#combine with ground data:
lindsey_dat <- read.csv("X:/moore/FieldData/DataSpreadsheets/US-SRG_WoodyPlantCensus_28052025.csv")
filt_lindsey_dat <- lindsey_dat%>%
  group_by(ID)%>%
  summarize(Species = Species,
            Diameter = mean(CrownDiameter, na.rm = T),
            Height = Height)%>%
  filter(Species != "",)%>%
  ungroup()%>%
  select(-ID)%>%
  mutate(Survey = "Ground",
         Area = pi*((Diameter/2)^2))%>%
  filter(Species %in% c("MES", "HAC", "CHAC", "GTN", "WTA", "SALIX", "ACIA", "WAC"))%>%
  select(-Species)

combd_dat <- bind_rows(filt_lindsey_dat, TreeMeas)

#------------------
#----Plot
#------------------
combd_dat$Survey <- factor(combd_dat$Survey, levels = c("Ground", "Drone"))
p1 <- ggplot(combd_dat, aes(x = Height, fill = Survey)) +
  geom_histogram(position = "identity", alpha = 0.85, bins = 30) +
  scale_fill_manual(values = c("Drone" = "#91bfdb", "Ground" = "#fc8d59")) +
  theme_minimal() +
  labs(title = "",
       x = "Height (m)",
       y = "Count",
       fill = "Survey")+
  theme(
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 18),
    axis.text = element_text(size = 13),
    legend.position = "none")

p2 <- ggplot(combd_dat, aes(x = Diameter, fill = Survey)) +
  geom_histogram(position = "identity", alpha = 0.85, bins = 30) +
  scale_fill_manual(values = c("Drone" = "#91bfdb", "Ground" = "#fc8d59")) +
  theme_minimal() +
  labs(title = "",
       x = "Canopy Diameter (m)",
       y = "Count",
       fill = "Survey")+
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 18),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    legend.position = "none")

p3 <- ggplot(combd_dat, aes(x = Area, fill = Survey)) +
  geom_histogram(position = "identity", alpha = 0.85, bins = 30) +
  scale_fill_manual(values = c("Drone" = "#91bfdb", "Ground" = "#fc8d59")) +
  theme_minimal() +
  labs(title = "",
       x = "Canopy Area (m-2)",
       y = "Count",
       fill = "Survey")+
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 18),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 15))

p1+p2+p3


#stat tests--------------------------------------------------------

ks.test(filt_lindsey_dat$Height, TreeMeas$Height)
# ^ distribs are sig dif
ks.test(filt_lindsey_dat$Diameter, TreeMeas$Diameter)
# ^ distribs are sig dif
ks.test(filt_lindsey_dat$Area, TreeMeas$Area)
# ^ distribs are sig dif

wilcox.test(Height ~ Survey, data = combd_dat)
# ^ medians are not sig dif
wilcox.test(Diameter ~ Survey, data = combd_dat)
# ^ medians are sig dif
wilcox.test(Area ~ Survey, data = combd_dat)
# ^ medians are sig dif

#===============================================================================
#====Exploring other options...=================================================
#===============================================================================


chm <- aggregate(chm, 5, "mean")
plot(chm)

kernel <- matrix(1, 3, 3)
chm_smoothed <- focal(chm, w = kernel, fun = median, na.rm = TRUE)

col <- height.colors(50)
plot(chm_smoothed, col = col, main = "Smoothed CHM")


ext <- ext(chm_smoothed)
center_x <- 516311.8
center_y <- 3517106

zoom_width <- 120
zoom_height <- 120
zoom_extent <- ext(
  center_x - zoom_width,
  center_x + zoom_width,
  center_y - zoom_height,
  center_y + zoom_height
)

chm_zoom <- crop(chm_smoothed, zoom_extent)
plot(chm_zoom, main = "", axes = F)


ttops <- locate_trees(las = chm_zoom, algorithm = lmf(ws = 3.5))
ttops
plot(chm_zoom, axes = F)
plot(ttops, col = "white", add = TRUE, cex = 0.5)

#dynamic window function (function of chm cell size)
lin <- function(x){x * 0.2 + 5}
ttops <- vwf(chm_zoom, winFun = lin, minHeight = 0.5)
plot(chm_zoom, axes = F)
plot(ttops, col = "white", add = TRUE, cex = 0.5)

crowns_ras <- mcws(treetops = ttops, CHM = chm_zoom, minHeight = 0.5)
plot(crowns_ras, col = sample(rainbow(50), nrow(unique(chm)), replace = TRUE), legend = FALSE, xlab = "", ylab = "", xaxt='n', yaxt = 'n')


