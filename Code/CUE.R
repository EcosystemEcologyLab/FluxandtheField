#carbon use efficiency 

biomass <- read.csv("./Data/AnnualBiomassProducts.csv")%>%
  rename(site = SITE_ID)
library(dplyr)
library(dplyr)

base_dir <- "X:/moore/FluxNetData/"
GradSites <- c("US-SRM", "US-Jo1", "US-CMW", "US-SRG", "US-Whs", "US-Jo2", "US-Wkg",
               "US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-MtB", "US-Vcs", "US-Vcm")

all_folders <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
data_list <- list()

# Loop over each site
for (site in GradSites) {
  site_folder <- all_folders[grepl(site, all_folders)]
  
  if (length(site_folder) == 1) {
    annual_file <- list.files(site_folder, pattern = "YY_.*\\.csv$", full.names = TRUE)
    
    if (length(annual_file) == 1) {
      df <- read.csv(annual_file, na.strings = -9999)
      df$site <- site
      data_list[[site]] <- df
      message(paste("Loaded:", site))
    } else {
      warning(paste("Annual file not found or multiple found for", site))
    }
  } else if (length(site_folder) > 1) {
    warning(paste("Multiple folders found for", site))
  } else {
    warning(paste("No folder found for", site))
  }
}

annual_data_combined <- bind_rows(data_list)

annual_data_sub <- annual_data_combined%>%
  select(site, TIMESTAMP, GPP_DT_VUT_REF, RECO_DT_VUT_REF, NEE_VUT_REF)%>%
  rename(year = TIMESTAMP)

fluxbio_dat <- merge(annual_data_sub, biomass, by = c("site", "year"))
fluxbio_filt <- fluxbio_dat%>%
  mutate(CUE = agb_Mg_ha/GPP_DT_VUT_REF)
  
  
fluxbio_filt <- fluxbio_filt %>%
  filter(is.finite(CUE))

ggplot(fluxbio_filt, aes(x = year, y = CUE, color = product, group = interaction(site, product))) +
  geom_line() +
  geom_point() +
  facet_wrap(~ site, scales = "free_y") + 
  theme_minimal() +
  labs(
    title = "",
    x = "Year",
    y = "CUE (AGB / GPP)",
    color = "Product"
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


  
ggplot(fluxbio_filt, aes(x = year, y = agb_Mg_ha, color = product, group = interaction(site, product))) +
  geom_line() +
  geom_point() +
  facet_wrap(~ site, scales = "free_y") + 
  theme_minimal() +
  labs(
    title = "",
    x = "Year",
    y = "agb",
    color = "Product"
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )        

ggplot(fluxbio_filt, aes(x = year, y = GPP_DT_VUT_REF, color = product, group = interaction(site, product))) +
  geom_line() +
  geom_point() +
  facet_wrap(~ site, scales = "free_y") + 
  theme_minimal() +
  labs(
    title = "",
    x = "Year",
    y = "gpp",
    color = "Product"
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )   
















library(dplyr)

# Create a new column that defines the time period
fluxbio_period <- fluxbio_filt %>%
  mutate(period = case_when(
    year >= 2001 & year <= 2010 ~ "2001-2010",
    year >= 2011 & year <= 2021 ~ "2011-2021",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(period))  # remove years outside desired range

# Group by site, product, and period and summarize
fluxbio_summary <- fluxbio_period %>%
  group_by(site, product, period) %>%
  summarise(
    mean_CUE = mean(CUE, na.rm = TRUE),
    mean_agb = mean(agb_Mg_ha, na.rm = TRUE),
    mean_GPP = mean(GPP_DT_VUT_REF, na.rm = TRUE),
    n_years = n(),  # number of years averaged
    .groups = "drop"
  )
cue_change <- fluxbio_summary %>%
  select(site, product, period, mean_CUE) %>%
  pivot_wider(names_from = period, values_from = mean_CUE) %>%
  mutate(delta_CUE = `2011-2021` - `2001-2010`)

library(ggplot2)

# Add color column based on sign of delta_CUE
cue_change <- cue_change %>%
  mutate(
    color = ifelse(delta_CUE < 0, "red", "blue"),
    site_product = paste(site, product, sep = "_")  # optional label
  )

# Bar plot
ggplot(cue_change, aes(x = site, y = delta_CUE, fill = color)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  facet_wrap(~ product, scales = "free_x") +
  scale_fill_identity() +  # use colors directly from the column
  theme_minimal() +
  labs(
    title = "ΔCUE (2011–2021 minus 2001–2010)",
    x = "Site",
    y = expression(Delta~CUE)
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )
