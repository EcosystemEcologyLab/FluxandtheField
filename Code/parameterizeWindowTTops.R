library(ggplot2)
library(dplyr)
library(sf)
library(terra)
library(lidR)
library(pracma)

compute_overlap <- function(x, y, bins = 30) {
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]
  
  if (length(x) < 2 || length(y) < 2 || var(x) == 0 || var(y) == 0) {
    message("Histogram overlap skipped: too few values or zero variance")
    return(NA_real_)
  }
  
  combined_range <- range(c(x, y))
  breaks <- seq(combined_range[1], combined_range[2], length.out = bins + 1)
  
  h_x <- hist(x, breaks = breaks, plot = FALSE)
  h_y <- hist(y, breaks = breaks, plot = FALSE)
  
  overlap_val <- sum(pmin(h_x$density, h_y$density)) * diff(breaks[1:2])
  return(overlap_val)
}


make_custom_ws <- function(a, b, floor_thresh, floor_val) {
  function(x) {
    y <- ceiling(a * x + b)
    y[x < floor_thresh] <- floor_val
    return(y)
  }
}


param_grid <- expand.grid(
  a = seq(0.2, 1.4, by = 0.2),
  b = seq(-0.3, 0.5, by = 0.2),
  floor_thresh = seq(1.7, 2.7, by = 0.1),
  floor_val = seq(1, 4, by = 1)
)

results <- list()

# ---- Main loop ----
for (i in 1:nrow(param_grid)) {
  p <- param_grid[i, ]
  ws_fun <- make_custom_ws(p$a, p$b, p$floor_thresh, p$floor_val)
  
  ttops <- tryCatch({
    locate_trees(las = chm_smoothed, algorithm = lmf(ws = ws_fun, hmin = 1))
  }, error = function(e) return(NULL))
  
  if (is.null(ttops) || nrow(ttops) == 0) {
    results[[i]] <- cbind(p, D = NA, p_value = NA, median_diff = NA,
                          overlap = NA, count_diff = NA)
    next
  }
  
  tree_segments <- segment_trees(
    las = las_filt,
    algorithm = dalponte2016(
      chm = chm_smoothed,
      treetops = ttops,
      th_tree = 2,
      th_seed = 0.5,
      th_cr = 0.5,
      max_cr = 20
    )
  )
  
  if (length(unique(tree_segments$treeID)) == 0) {
    results[[i]] <- cbind(p, D = NA, p_value = NA, median_diff = NA,
                          overlap = NA, count_diff = NA)
    next
  }
  
  crowns <- crown_metrics(tree_segments, func = .stdtreemetrics, geom = "concave")
  crowns_sf <- st_as_sf(crowns)
  
  centroids <- suppressWarnings(st_centroid(crowns_sf))
  inside <- st_within(centroids, survbound_sf, sparse = FALSE)[, 1]
  filt_crowns <- crowns_sf[inside, ]
  filt_crowns$area <- st_area(filt_crowns) |> as.numeric()
  
  if (nrow(filt_crowns) == 0) {
    results[[i]] <- cbind(p, D = NA, p_value = NA, median_diff = NA,
                          overlap = NA, count_diff = NA)
    next
  }
  
  auto_df <- data.frame(Area = filt_crowns$area, Method = "R")
  manual_df <- data.frame(Area = TreeMeas$Area, Method = "Manual")
  
  # Debug info
  cat(sprintf("Params [%d]: auto = %d, manual = %d, var(auto) = %.2f, var(manual) = %.2f\n",
              i, nrow(auto_df), nrow(manual_df),
              var(auto_df$Area), var(manual_df$Area)))
  
  if (nrow(auto_df) < 2 || nrow(manual_df) < 2) {
    results[[i]] <- cbind(p, D = NA, p_value = NA, median_diff = NA,
                          overlap = NA, count_diff = NA)
    next
  }
  
  kstest <- ks.test(auto_df$Area, manual_df$Area)
  overlap_val <- compute_overlap(auto_df$Area, manual_df$Area, bins = 30)
  count_diff <- abs(nrow(auto_df) - nrow(manual_df))
  
  results[[i]] <- cbind(
    p,
    D = kstest$statistic,
    p_value = kstest$p.value,
    median_diff = abs(median(auto_df$Area) - median(manual_df$Area)),
    overlap = overlap_val,
    count_diff = count_diff
  )
}

#results
results_df <- do.call(rbind, results)
results_clean <- results_df
results_sorted <- results_clean %>%
  mutate(across(c(D, p_value, median_diff, overlap), ~ round(.x, 2)))|>
  arrange(desc(overlap), count_diff, median_diff, desc(p_value), D)
#saveRDS(results_sorted, "./Data/parameterization.RDS")





# > head(results_sorted)
# a    b floor_thresh floor_val         D      p_value median_diff   overlap count_diff
# 1 0.2  0.5          2.4         4 0.1790235 2.901102e-07    2.912742 0.8484122        111
# 2 0.6  0.1          2.4         4 0.2037815 1.082340e-08    2.384747 0.8446994         34
# 3 0.6  0.1          2.5         4 0.2068230 7.022836e-09    2.169809 0.8418991         27
# 4 0.4  0.1          2.3         4 0.1779817 3.852255e-07    2.669802 0.8417286        103
# 5 0.4  0.1          2.4         4 0.1819887 2.238799e-07    2.314457 0.8415186         91
# 6 0.8 -0.3          2.5         4 0.2160356 1.869348e-09    1.939838 0.8388223          7 <- chose these params

#plot
ggplot(results_clean, aes(x = D, y = p_value)) +
  geom_point(aes(size = overlap), alpha = 0.7) +
  geom_text(
    data = head(results_sorted, 5),
    aes(label = paste0("a=", a, ", b=", b)),
    hjust = -0.1, size = 3
  ) +
  labs(
    title = "Crown Area Distribution Comparison",
    x = "KS D-statistic",
    y = "KS p-value",
    size = "Histogram Overlap"
  ) +
  theme_minimal()



library(tidyverse)
library(patchwork)

df <- results_df

output_vars <- c("overlap", "count_diff", "median_diff", "D")
yaxis_titles <- c("Overlap", "Crown Difference", "Median Difference", "KS Test")

param_labels <- c(
  a = "Slope",
  b = "Intercept",
  floor_thresh = "Height Threshold",
  floor_val = "Default Size"
)

plot_list <- list()

for (i in seq_along(output_vars)) {
  output_var <- output_vars[i]
  y_title <- yaxis_titles[i]
  
  param_long <- df %>%
    pivot_longer(cols = c(a, b, floor_thresh, floor_val),
                 names_to = "parameter", values_to = "param_value") %>%
    select(parameter, param_value, all_of(output_var))
  
  colnames(param_long)[3] <- "output_value"
  
  p <- ggplot(param_long, aes(x = as.factor(param_value), y = output_value)) +
    geom_boxplot(fill = "lightblue", alpha = 0.8) +
    facet_wrap(~parameter, scales = "free_x",
               labeller = as_labeller(param_labels)) +
    labs(x = "", y = y_title) +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  plot_list[[i]] <- p
}

combined_plot <- wrap_plots(plotlist = plot_list, ncol = 2)
combined_plot
