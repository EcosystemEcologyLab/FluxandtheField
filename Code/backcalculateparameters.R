library(lidR)
library(sf)
library(terra)
library(dplyr)
library(ggplot2)

# Load canopy polygons
canopy_polys <- st_read("./Data/GitData/canopypolys.shp")

# Load and crop CHM raster
chm_load <- rast("./Data/0.5mSRGchm.tif")
survey_outline <- vect("./QGIS/100msurveybuffer.shp")
chm_crop <- crop(chm_load, survey_outline)
chm <- chm_crop

# Match CRS
canopy_polys <- st_transform(canopy_polys, crs(chm))

# Extract mean height per polygon from CHM
canopy_polys$mean_ht <- terra::extract(chm, vect(canopy_polys), fun = mean, na.rm = TRUE)[,2]

# Compute crown diameter (from area of polygons)
canopy_polys$crown_diam <- 2 * sqrt(st_area(canopy_polys) / pi) |> as.numeric()

# Clean data
df <- canopy_polys |>
  st_drop_geometry() |>
  filter(!is.na(mean_ht), !is.na(crown_diam), mean_ht > 1, crown_diam > 0)

# Fit linear model: crown diameter ~ mean height
fit <- lm(crown_diam ~ mean_ht, data = df)
a <- coef(fit)[["mean_ht"]]
b <- coef(fit)[["(Intercept)"]]

# Automatically choose floor threshold and floor value
floor_thresh <- quantile(df$mean_ht, probs = 0.10) |> as.numeric()
floor_val <- round(median(df$crown_diam[df$mean_ht <= floor_thresh]))

cat("Fitted window parameters:\n")
cat("a =", round(a, 3), "\n")
cat("b =", round(b, 3), "\n")
cat("floor_thresh =", round(floor_thresh, 2), "\n")
cat("floor_val =", floor_val, "\n")

# Plot relationship
ggplot(df, aes(x = mean_ht, y = crown_diam)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", col = "red") +
  geom_vline(xintercept = floor_thresh, linetype = "dashed", color = "blue") +
  labs(x = "Mean Height (m)", y = "Crown Diameter (m)",
       title = "Fitted Crown Diameter vs Height")

# Generate window function
custom_ws <- function(x) {
  y <- ceiling(a * x + b)
  y[x < floor_thresh] <- floor_val
  return(y)
}

# Smooth CHM with 3x3 median filter
kernel <- matrix(1, 3, 3)
chm_smoothed <- terra::focal(chm, w = kernel, fun = median, na.rm = TRUE)

# Identify treetops
ttops <- locate_trees(las = chm_smoothed, algorithm = lmf(ws = custom_ws, hmin = 1))

# Plot results
plot(chm_smoothed, main = "Detected Tree Tops")
plot(ttops, add = TRUE, col = "white", cex = 0.5)
