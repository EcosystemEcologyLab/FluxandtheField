# At the US-SRG and US-SRM flux tower sites, 10 and 16 mesquite trees, 
# respectively, were selected to represent a range of canopy diameters, basal 
# diameters, and heights. We refer to these ranges as biometric gradients. 
# Each tree was tagged and measured for long-term monitoring. A summary of 
# methods and data can be found in /Reports/Biometric Gradient Notes.
# 
# This script performs the following steps for each site:
#   1. Imports the field measurement data
#   2. Creates a function to determine averages and errors for measurements
#   3. Visualizes the distribution of values for each measurement type

#===============================================================================
#Load necessary packages--------------------------------------------------------
#===============================================================================
library(dplyr)
library(ggplot2)
library(tidyverse)
library(patchwork)

#===============================================================================
#Import field observations------------------------------------------------------
#===============================================================================
SRGdat <-  read.csv("./Data/GitData/US-SRG_BiometGrad_07062025.csv")
SRMdat <-  read.csv("./Data/GitData/US-SRM_BiometGrad_12062025.csv")

#===============================================================================
#Define mean/se plotting function-----------------------------------------------
#===============================================================================
MeasStats <- function(data, variable, y_label, dendro = FALSE) {
  
  #summarize mean and se for a given variable
  summary_df <- data %>%
    group_by(ID) %>%
    summarize(
      avg = mean(.data[[variable]], na.rm = TRUE),
      se = sd(.data[[variable]], na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  #add annotation for trees with dendrometers (US-SRM)
  dendro_ids <- character(0)
  if (dendro) {
    dendro_ids <- data %>%
      group_by(ID) %>%
      summarize(has_dendro = any(!is.na(DendrometerID)), .groups = "drop") %>%
      filter(has_dendro) %>%
      pull(ID) %>% as.character()
  }
  summary_df <- summary_df %>%
    mutate(
      ID_char = as.character(ID),
      ID_label = ifelse(ID_char %in% dendro_ids,
                        paste0(ID_char, "\n*"),  # newline + asterisk below
                        ID_char)
    ) %>%
    mutate(ID_label = factor(ID_label, levels = ID_label[order(avg)]))
  
  
  #combine plot elements; adjust aesthetics
  p <- ggplot(summary_df, aes(x = ID_label, y = avg)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_errorbar(aes(ymin = avg - se, ymax = avg + se), width = 0.2) +
    labs(x = "Tree ID #", y = y_label, title = "") +
    theme_minimal() +
    theme(
      axis.title.y = element_text(size = 16),
      axis.title.x = element_text(size = 16, margin = margin(t = 10)),
      axis.text.x = element_text(size = 13, hjust = 0.5, lineheight = 0.9),
      axis.text.y = element_text(size = 13)
    )
  
  return(p)
}

#===============================================================================
#SRG plots----------------------------------------------------------------------
#===============================================================================

p1g <- MeasStats(SRGdat, "CanopyHeight", "Average Canopy Height (m)")
p2g <- MeasStats(SRGdat, "CanopyDiameter", "Average Canopy Diameter (m)")
p3g <- MeasStats(SRGdat, "BasalDiameter", "Average Basal Diameter (cm)")

# Combine plots
srg_plot <- p1g + p2g + p3g +
  plot_layout(ncol = 3) &
  theme(axis.title.x = element_blank())

# Add common x-axis label using patchwork's plot_annotation
srg_plot + plot_annotation(
  title = "US-SRG Biometric Gradient",
  caption = "Tree ID #",
  theme = theme(
    plot.caption = element_text(size = 16, hjust = 0.5, margin = margin(t = 2))
  )
)

#===============================================================================
#SRM plots----------------------------------------------------------------------
#===============================================================================
p1m <- MeasStats(SRMdat, "CanopyHeight", "Average Canopy Height (m)", dendro = T)
p2m <- MeasStats(SRMdat, "CanopyDiameter", "Average Canopy Diameter (m)", dendro = T)
p3m <- MeasStats(SRMdat, "BasalDiameter", "Average Basal Diameter (cm)", dendro = T)

srm_plot <- p1m + p2m + p3m +
  plot_layout(ncol = 3) &
  theme(axis.title.x = element_blank())

srm_plot + plot_annotation(
  title = "US-SRM Biometric Gradient",
  caption = "Tree ID #",
  theme = theme(
    plot.caption = element_text(size = 16, hjust = 0.5, margin = margin(t = 2))
  )
)

