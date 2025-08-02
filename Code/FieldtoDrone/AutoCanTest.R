#create a relationship between biomass and canopy diameter using allometry and biomet grad

SRGdat <-  read.csv("./Data/GitData/US-SRG_BiometGrad_07062025.csv")%>%
  mutate(Site = "US-SRG")%>%
  group_by(ID)%>%
  summarize(BasalDi = mean(BasalDiameter, na.rm = T),
            CanopyDi = mean(CanopyDiameter, na.rm = T))%>%
  mutate(biomass = exp((-2.9255) + 2.4109 * log(BasalDi)))%>% #in kg
  #select(-BasalDi)%>%
  mutate(method = "Allometry")

model <- nls(biomass ~ a * CanopyDi^b,
             data = SRGdat,
             start = list(a = 0.1, b = 1)
             )

ggplot(SRGdat, aes(x = CanopyDi, y = biomass)) +
  geom_point() +
  stat_function(fun = function(x) {
    coef(model)["a"] * x^coef(model)["b"]
  }, color = "blue", size = 1) +
  labs(title = "")

new_df$predicted_biomass <- predict(model, newdata = new_df)



manu_output <- vect("./Data/GitData/canopypolys.shp")
auto_output <- vect("./Data/testJiamingcrowns2.shp")
survey_outline <- vect("./QGIS/100msurveybuffer.shp")

manushp <- crop(manu_output, survey_outline)
autoshp <- crop(auto_output, survey_outline)












#mesquite mask
mesmask <- rast("X:/moore/SRER/RF_allyr_SRER_norton.tif")


# Number of bands
nlyr(mesmask)

# Band names
names(mesmask)

# Print summary info for each band
summary(mesmask)

# Inspect first few values of each band
values <- values(mesmask, mat = FALSE)
head(values)

# Load packages
library(lidR)
library(sf)
library(terra)

# Read in LiDAR file and set some color palettes
col <- height.colors(50)
col1 <- pastel.colors(900)

chm <- rast("./Data/0.5mSRGchm.tif")
survey_outline <- vect("./QGIS/100msurveybuffer.shp")
chm_crop <- crop(chm, survey_outline)
plot(chm_crop, col = col)


# Generate kernel and smooth chm
schm <- chm
kernel <- matrix(1,3, 3)
schm <- terra::focal(x = chm_crop, w = kernel, fun = median, na.rm = TRUE)
plot(schm, col = col)

ttops <- locate_trees(las = schm, algorithm = lmf(ws = function(x) { 0.5 * x + 2 }))
plot(schm, col = col)
plot(ttops, col = "black", add = TRUE, cex = 0.5)




#-------------------
#using ForestTools
library(ForestTools)
winFun <- function(h) pmin( 0.1 + 2.5 * h, 6 )
# winFun <- function(h) pmin( 0.3 + 1.5 * h, 5 )
# winFun <- function(h) pmin( 0.3 + 1.7 * h, 5 )

ttops <- vwf(schm, winFun, minHeight = 0.5, warnings = T, minWinNeib = "queen", IDfield = "treeID", resolution_round = 5)

plot(schm, col = col)
plot(ttops, col = "black", add = TRUE, cex = 0.5)



segments <- mcws(ttops, chm)



crowns_li <- crown_metrics(MBA_treeseg_li, func = NULL,  geom = "concave")
windows()
plot(sf::st_geometry(crowns_li), reset = FALSE)

lidR::plot(MBA_treeseg_li, color ="treeID")