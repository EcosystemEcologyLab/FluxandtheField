

make_custom_ws <- function(a, b, floor_thresh, floor_val) {
  function(x) {
    y <- ceiling(a * x + b)
    y[x < floor_thresh] <- floor_val
    return(y)
  }
}
# custom_ws <- function(x) {
#   y <- ceiling(1.211 * x + 0.024)
#   y[x < 2] <- 5
#   return(y)
# }
param_grid <- expand.grid(
  a = seq(0.5, 2, by = 0.1),
  b = seq(0, 1, by = 0.1),
  floor_thresh = c(1, 3),
  floor_val = c(1, 6)
)



results <- list()

for (i in 1:nrow(param_grid)) {
  p <- param_grid[i, ]
  
  ws_fun <- make_custom_ws(p$a, p$b, p$floor_thresh, p$floor_val)
  
  ttops <- tryCatch({
    locate_trees(las = chm_smoothed, algorithm = lmf(ws = ws_fun, hmin = 1))
  }, error = function(e) return(NULL))
  
  if (is.null(ttops) || nrow(ttops) == 0) {
    results[[i]] <- cbind(p, D = NA, p_value = NA, median_diff = NA)
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
    results[[i]] <- cbind(p, D = NA, p_value = NA, median_diff = NA)
    next
  }
  
  crowns <- crown_metrics(tree_segments, func = .stdtreemetrics, geom = "concave")
  crowns_sf <- st_as_sf(crowns)
  
  centroids <- st_centroid(crowns_sf)
  inside <- st_within(centroids, survbound_sf, sparse = FALSE)[, 1]
  filt_crowns <- crowns_sf[inside, ]
  filt_crowns$area <- st_area(filt_crowns) |> as.numeric()
  
  if (nrow(filt_crowns) == 0) {
    results[[i]] <- cbind(p, D = NA, p_value = NA, median_diff = NA)
    next
  }
  
  auto_df <- data.frame(Area = filt_crowns$area, Method = "R")
  manual_df <- data.frame(Area = TreeMeas$Area, Method = "Manual")
  
  kstest <- ks.test(auto_df$Area, manual_df$Area)
  
  results[[i]] <- cbind(
    p,
    D = kstest$statistic,
    p_value = kstest$p.value,
    median_diff = abs(median(auto_df$Area) - median(manual_df$Area))
  )
}

results_df <- do.call(rbind, results)
results_clean <- na.omit(results_df)
results_sorted <- results_clean[order(-results_clean$p_value, results_clean$D), ]

head(results_sorted, 10)

ggplot(results_clean, aes(x = D, y = p_value)) +
  geom_point(alpha = 0.7) +
  geom_text(
    data = head(results_sorted, 5),
    aes(label = paste0("a=", a, ", b=", b)),
    hjust = -0.1, size = 3
  ) +
  labs(
    title = "Crown Area Distrib",
    x = "KS D-statistic (difference in distributions)",
    y = "p-value"
  ) +
  theme_minimal()
