# FluxandtheField

**FluxandtheField** contains code and figures used to analyze field-based vegetation data, drone imagery, and eddy covariance flux data collected at the **US-SRM** and **US-SRG** AmeriFlux sites, located at the **Santa Rita Experimental Range (SRER)** in southern Arizona.

Data span two campaigns:

-    **May-June 2025**: flux, field, and remote sensing measurements

-    **2014**: historical field and flux data from USDA-ARS campaign

This project contributes to a broader effort to understand how ecosystem structure influences the carbon cycle in semi-arid ecosystems. Specifically, this repository includes code to:

-   Evaluate the effectiveness of **remote sensing versus ground-based methods** for characterizing vegetation structure and biometric distributions at the flux tower sites by comparing observations across a biometric gradient at two flux sites.

-    Analyze **ecosystem change over a 10-year period** using flux data and biometric measurements collected in 2014 and 2025.

## Repository Structure

### /Code

This folder contains R scripts for processing and visualizing field, drone, and flux data. The folder is organized into three subdirectories:

**FieldtoDrone/**

Code specific to comparing drone and ground data at US-SRG and US-SRM flux sites.

-   HeightError.R: Visualize height, canopy diameter, and basal diameter of trees for the biometric gradients at US-SRM and US-SRG. Use the multiple observation collected to calculate and visualize the error associated with using the hypsometer in-field to collect height data.

-   SRGtreetops.R: Use canopy polygons and canopy height model to extract height and canopy diameter statistics from LiDAR. Compare extracted values with woody census field data from US-SRG.

**SRG/**

Code specific to comparing 2014 and 2025 field data collected at US-SRG.

-   60mObs.R: Filter 2014 and 2025 field data to 60-m, summarize observations, and visualize by quadrant.

-   CensusDistribComparison.R: Visualize mesquite height and canopy diameter distributions. Determine significant differences in distributions. Calculate NEE increment from flux data.

-   RadCensusPlots.R: Visualize distributions of 2025 woody census field data.

**SRM/**

Code specific to comparing 2014 and 2025 field data collected at US-SRM.

-   LITplots.R: process and visualize line intersect transects (LIT) and belt transects from 2014 and 2025.

-   SRM_CHM.R: create canopy height model (CHM) for US-SRM from 2025 LiDAR.

### /Plots

Plots made from the code in /Code are saved in /Plots.
