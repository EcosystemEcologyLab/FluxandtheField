#download NEON data 
#DP3.30015.001 = canopy height model product
#DP1.30003.001 = discrete return lidar

library(neonUtilities)
library(sf)
#load function to convert lat/lon to UTM----------------------------------------
latlon_to_utm <- function(coord_string) {

  coords <- as.numeric(unlist(strsplit(coord_string, ",")))
  
  if (length(coords) != 2) {
    stop("Input must be a string in the format 'lat, lon'")
  }
  
  lat <- coords[1]
  lon <- coords[2]
  
  zone <- floor((lon + 180) / 6) + 1
  epsg_code <- if (lat >= 0) 32600 + zone else 32700 + zone
  
  point_sf <- st_as_sf(data.frame(lon = lon, lat = lat), coords = c("lon", "lat"), crs = 4326)
  
  point_utm <- st_transform(point_sf, crs = epsg_code)
  
  coords_out <- st_coordinates(point_utm)
  return(data.frame(Easting = coords_out[,1], Northing = coords_out[,2], UTM_Zone = zone))
}


#prep site info-----------------------------------------------------------------

SRG_UTM <- latlon_to_utm("31.7894, -110.8277")
SRM_UTM <- latlon_to_utm("31.8214, -110.8661")
xSR_UTM <- latlon_to_utm("31.9107, -110.8355")
xJR_UTM <- latlon_to_utm("32.5907, -106.8425")

locations <- list(
  list(site = "SRER", easting = SRG_UTM[1], northing = SRG_UTM[2], savepath = "./Data/NEON/US-SRG"),
  list(site = "SRER", easting = SRM_UTM[1], northing = SRM_UTM[2], savepath = "./Data/NEON/US-SRM"),
  list(site = "SRER", easting = xSR_UTM[1], northing = xSR_UTM[2], savepath = "./Data/NEON/US-xSR"),
  list(site = "JORN", easting = xJR_UTM[1], northing = xJR_UTM[2], savepath = "./Data/NEON/US-xJR")
)

years <- 2014:2023

#Download data------------------------------------------------------------------
for (i in locations) {
  for (year in years) {
    message("Downloading for site: ", i$site, ", year: ", year, 
            ", location: (", i$easting, ", ", i$northing, ")")
    
    try({
      byTileAOP(
        dpID = "DP1.30003.001",
        site = i$site,
        year = year,
        easting = i$easting,
        northing = i$northing,
        buffer = 200,
        include.provisional = T,
        check.size = T,
        savepath = i$savepath,
        token = "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJsaW5kc2V5YmVsbEBhcml6b25hLmVkdSIsInNjb3BlIjoicmF0ZTpwdWJsaWMiLCJpc3MiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnLyIsImV4cCI6MTkxMTE0NTU0MCwiaWF0IjoxNzUzNDY1NTQwLCJlbWFpbCI6ImxpbmRzZXliZWxsQGFyaXpvbmEuZWR1In0.f7uTfAgCUP4t3p9qdm9eaIh3bSfi0KlQ6EmFpnRxRMPeiogxCeWDNjlXUYG0kbTsKUloywaGLMJMeV0Pp4GtqA"
      )
    }, silent = TRUE)
  }
}


#---------------------------------------


byTileAOP(
  dpID = "DP3.30015.001",
  site = "SRER",
  year = 2024,
  easting = SRM_UTM[1],
  northing = SRM_UTM[2],
  buffer = 200,
  include.provisional = T,
  check.size = T,
  savepath = "./Data/NEON/US-SRM",
  options(timeout = 1000),
  token = "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJsaW5kc2V5YmVsbEBhcml6b25hLmVkdSIsInNjb3BlIjoicmF0ZTpwdWJsaWMiLCJpc3MiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnLyIsImV4cCI6MTkxMTE0NTU0MCwiaWF0IjoxNzUzNDY1NTQwLCJlbWFpbCI6ImxpbmRzZXliZWxsQGFyaXpvbmEuZWR1In0.f7uTfAgCUP4t3p9qdm9eaIh3bSfi0KlQ6EmFpnRxRMPeiogxCeWDNjlXUYG0kbTsKUloywaGLMJMeV0Pp4GtqA"
)
