GradSites <- c("US-SRM","US-SRG")

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
annual_data_sub <- annual_data_combined %>%
  select(site, TIMESTAMP, GPP_DT_VUT_REF, RECO_DT_VUT_REF, NEE_VUT_REF) %>%
  filter(!is.na(GPP_DT_VUT_REF)) %>%
  group_by(site) 


















all_folders <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
data_list <- list()

# Loop over each site
for (site in GradSites) {
  site_folder <- all_folders[grepl(site, all_folders)]
  
  if (length(site_folder) == 1) {
    annual_file <- list.files(site_folder, pattern = "DD_.*\\.csv$", full.names = TRUE)
    
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
annual_data_sub <- annual_data_combined %>%
  select(site, TIMESTAMP, GPP_DT_VUT_REF, RECO_DT_VUT_REF, NEE_VUT_REF) %>%
  filter(!is.na(GPP_DT_VUT_REF)) %>%
  mutate(TIMESTAMP = ymd(TIMESTAMP))%>%
  group_by(site)%>%
  mutate(doy = yday(TIMESTAMP))

common_dates <- tseries_dat %>%
  group_by(TIMESTAMP) %>%
  summarize(n_sites = n_distinct(site)) %>%
  filter(n_sites == 2) %>%
  pull(TIMESTAMP)

tseries_common <- tseries_dat %>%
  filter(TIMESTAMP %in% common_dates)
gpp_wide <- tseries_dat %>%
  select(site, TIMESTAMP, year, doy, GPP_DT_VUT_REF) %>%
  pivot_wider(names_from = site, values_from = GPP_DT_VUT_REF) %>%
  drop_na()
gpp_diff <- gpp_wide %>%
  mutate(GPP_diff = `US-SRG` - `US-SRM`)
gpp_diff <- gpp_diff %>%
  mutate(diff_sign = ifelse(GPP_diff >= 0, "positive", "negative"))
highlight_rects <- gpp_diff %>%
  filter(doy >= 60 & doy <= 150) %>%
  group_by(year) %>%
  summarise(start = min(TIMESTAMP), end = max(TIMESTAMP)) %>%
  ungroup()

#Plot
ggplot(gpp_diff, aes(x = TIMESTAMP, y = GPP_diff, fill = diff_sign)) +
  geom_rect(data = highlight_rects,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "gray80", alpha = 0.3) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  scale_fill_manual(values = c("positive" = "blue", "negative" = "red")) +
  labs(title = "Daily GPP Difference (US-SRM − US-SRG)",
       x = "Date",
       y = "GPP Difference") +
  theme_minimal()





# Create wide NEE data
nee_wide <- tseries_dat %>%
  select(site, TIMESTAMP, year, doy, NEE_VUT_REF) %>%
  pivot_wider(names_from = site, values_from = NEE_VUT_REF) %>%
  drop_na()

# Calculate difference between the two sites
nee_diff <- nee_wide %>%
  mutate(NEE_diff = `US-SRG` - `US-SRM`) %>%
  mutate(diff_sign = ifelse(NEE_diff >= 0, "positive", "negative"))

# Highlight rectangle for springtime (DOY 60–150)
highlight_rects_nee <- nee_diff %>%
  filter(doy >= 60 & doy <= 150) %>%
  group_by(year) %>%
  summarise(start = min(TIMESTAMP), end = max(TIMESTAMP)) %>%
  ungroup()

# Plot
ggplot(nee_diff, aes(x = TIMESTAMP, y = NEE_diff, fill = diff_sign)) +
  geom_rect(data = highlight_rects_nee,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "gray80", alpha = 0.3) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  scale_fill_manual(values = c("positive" = "blue", "negative" = "red")) +
  labs(title = "Daily NEE Difference (US-SRM − US-SRG)",
       x = "Date",
       y = "NEE Difference") +
  theme_minimal()














ggplot(tseries_common, aes(x = TIMESTAMP, y = GPP_DT_VUT_REF, color = site)) +
  geom_line(alpha = 0.8) +
  labs(title = "GPP Time Series (Common Dates Only)",
       x = "Date",
       y = "GPP",
       color = "Site") +
  theme_minimal()

climatologydat <- annual_data_sub%>%
  group_by(site, doy)%>%
  summarise(gpp = mean(GPP_DT_VUT_REF, na.rm = T),
            nee = mean(NEE_VUT_REF, na.rm = T))



ggplot(climatologydat, aes(x = doy, y = gpp, color = site)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(
    title = "",
    x = "doy",
    y = "gpp",
    color = "site"
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(climatologydat, aes(x = doy, y = nee, color = site)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(
    title = "",
    x = "doy",
    y = "nee",
    color = "site"
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )





ggplot(annual_data_sub, aes(x = TIMESTAMP_START, y = GPP_DT_VUT_REF, color = site)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(
    title = "",
    x = "year-week",
    y = "gpp",
    color = "site"
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )   






gpp_wide <- annual_data_sub %>%
  select(site, TIMESTAMP_START, GPP_DT_VUT_REF) %>%
  pivot_wider(names_from = site, values_from = GPP_DT_VUT_REF)%>%
  mutate(TIMESTAMP_START = ymd(TIMESTAMP_START))%>%
  mutate(doy = yday(TIMESTAMP_START),
         year = year(TIMESTAMP_START))
gpp_diff <- gpp_wide %>%
  mutate(diff = `US-SRG` - `US-SRM`)%>%
  mutate(date = row_number())%>%
  filter(!is.na(diff))

ggplot(gpp_diff, aes(x = doy, y = diff)) +
  #geom_line(color = "black") +  # Line is always black
  geom_point(aes(color = diff > 0), size = 2) +
  scale_color_manual(
    values = c("TRUE" = "blue", "FALSE" = "red"),
    labels = c("FALSE" = "Negative", "TRUE" = "Positive")
  ) +
  theme_minimal() +
  labs(
    title = "",
    x = "Year-Week",
    y = "GPP Difference",
    color = "Sign"
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )




ggplot(gpp_diff, aes(x = doy, y = diff)) +
  geom_line(color = "black", alpha = 0.4) +  # Line for each year panel
  geom_point(aes(color = diff > 0), size = 2) +  # Points colored by sign
  scale_color_manual(
    values = c("TRUE" = "blue", "FALSE" = "red"),
    labels = c("FALSE" = "Negative", "TRUE" = "Positive")
  ) +
  facet_wrap(~ year, ncol = 1) +  # One panel per year, stacked vertically
  theme_minimal() +
  labs(
    title = "",
    x = "Day of Year",
    y = "GPP Difference",
    color = "Sign"
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )







library(dplyr)

gpp_diff_summary <- gpp_diff %>%
  mutate(sign = ifelse(diff > 0, "positive", "negative")) %>%
  group_by(year, sign) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(year) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  filter(sign == "positive") 
library(ggplot2)

ggplot(gpp_diff_summary, aes(x = factor(year), y = percent)) +
  geom_col(fill = "lightblue") +
  theme_minimal() +
  labs(
    title = "",
    x = "Year",
    y = "% Positive GPP Differences"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )




