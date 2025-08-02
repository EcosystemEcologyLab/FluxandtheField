#compare biomass products to lidar estimates
library(dplyr)
library(ggplot2)

biomassprods <- read.csv("./Data/AnnualBiomassProducts.csv")%>%
  select(-ffp_radius)
biomass_SW <- biomassprods%>%
  filter(SITE_ID %in% c("US-SRM", "US-SRG", "US-xSR", "US-xJR"))%>%
  filter(year %in% 2017:2023)
# %>%
#   filter(year %in% c(2017:2023, "2019-2023"))
biomass_SW$year <- as.factor(biomass_SW$year)



ggplot(biomass_SW, aes(x = year, y = agb_Mg_ha, fill = product)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ SITE_ID, ncol = 2) +
  labs(
    title = "",
    x = "Year",
    y = "AGB (Mg/ha)",
    fill = "Product"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#===============================================================================
library(dplyr)
library(ggplot2)
library(terra)

# Read and prepare external product data
biomassprods <- read.csv("./Data/AnnualBiomassProducts.csv") %>%
  select(-ffp_radius)

summary_bio <- biomassprods %>%
  filter(SITE_ID == "US-SRG") %>%
  group_by(product) %>%
  summarise(
    max_bio = max(agb_Mg_ha, na.rm = TRUE),
    mean_bio = mean(agb_Mg_ha, na.rm = TRUE),
    recent_year = max(year, na.rm = TRUE),
    recent_bio = agb_Mg_ha[year == recent_year][1],
    .groups = "drop"
  ) %>%
  filter(!is.na(recent_bio)) %>%
  mutate(
    is_zero = recent_bio == 0,
    plot_biomass = ifelse(is_zero, 0.1, recent_bio),
    source = "Product"
  )

# Survey area from shapefile
survey_outline <- vect("./QGIS/100msurveybuffer.shp")
area_m2 <- expanse(survey_outline, unit = "m")
area_ha <- area_m2 / 10000
total_area_ha <- sum(area_ha)

# SRG estimates
srg_bio <- data.frame(
  product = c("basal area", "canopy area", "manual crown", "automated crown", "voxelization"),
  agb_Mg = c(12.66, 11.7, 18.0, 3.8, 46.9),
  recent_year = 2025
) %>%
  mutate(
    recent_bio = agb_Mg / total_area_ha,  # Convert to Mg/ha
    is_zero = recent_bio == 0,
    plot_biomass = ifelse(is_zero, 0.1, recent_bio),
    source = "Report Estimate",
    recent_year = as.character(recent_year)
  )

# Combine
combined_bio <- bind_rows(
  summary_bio %>% select(product, recent_bio, recent_year, plot_biomass, source),
  srg_bio %>% select(product, recent_bio, recent_year, plot_biomass, source)
) %>%
  mutate(product_year = paste0(product, " (", recent_year, ")"))

# Plot
ggplot(combined_bio, aes(x = reorder(product_year, -plot_biomass), y = plot_biomass, fill = source)) +
  geom_col(alpha = 0.9, position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c("Product" = "skyblue", "Report Estimate" = "forestgreen")) +  # teal & pink
  labs(
    title = "",
    x = "Product (Most Recent Year)",
    y = "AGB (Mg/ha)",
    fill = "Data Source"
  ) +
  theme_minimal(base_size = 13) +
  coord_flip()