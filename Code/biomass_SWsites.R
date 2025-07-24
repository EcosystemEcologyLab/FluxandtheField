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

