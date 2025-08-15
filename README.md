#Update:https://ecosystemecologylab.github.io/FluxandtheField/BiomassReport.html?nocache=20250814
# FluxandtheField

**FluxandtheField** contains code and figures used to analyze field-based vegetation data, drone imagery, and eddy covariance flux data collected at the **US-SRM** and **US-SRG** AmeriFlux sites, located at the **Santa Rita Experimental Range (SRER)** in southern Arizona.

Data span two campaigns:

-   **May-June 2025**: flux, field, and remote sensing measurements

-   **2014**: historical field and flux data from USDA-ARS campaign

This project contributes to a broader effort to understand how ecosystem structure influences the carbon cycle in semi-arid ecosystems. Specifically, this repository includes code to:

-   Evaluate the effectiveness of **remote sensing versus ground-based methods** for characterizing vegetation structure and biometric distributions at the flux tower sites by comparing observations across a biometric gradient at two flux sites.

-   Analyze **ecosystem change over a 10-year period** using flux data and biometric measurements collected in 2014 and 2025.

## Repository Structure

### /Code

This folder contains R scripts for processing and visualizing field, drone, and flux data. The folder is organized into three subdirectories:

**FieldtoDrone/**

Code specific to comparing drone and ground data at US-SRG and US-SRM flux sites.

-   VisualizeBiometGrad.R: Visualize the distributions of height, canopy diameter, and basal diameter of trees for the biometric gradients at US-SRM and US-SRG. Use the multiple observations collected to calculate and visualize measurement error.

-   CompareDroneAndCensusDistribs.R: Use canopy polygons and canopy height model to extract height and canopy diameter statistics from LiDAR. Compare extracted values with woody census field data from US-SRG.

**Flux/**

Code utilitzing flux data.

-   NEEincrement.R: Use monthly long-term flux data at both US-SRM and US-SRG to compare trends in NEE from 2014 to 2023.

**SRG/**

Code specific to comparing 2014 and 2025 field data collected at US-SRG.

-   CompareCensusAverages2014to2025.R: Filter 2014 and 2025 field data to 60-m, summarize observations, and visualize by quadrant.

-   CompareCensusDistribs2014to2025.R: Visualize mesquite height and canopy diameter distributions. Determine significant differences in distributions. Calculate NEE increment from flux data.

-   2025CensusDistribs.R: Visualize distributions of 2025 woody census field data (mainly mesquite observations).

**SRM/**

Code specific to comparing 2014 and 2025 field data collected at US-SRM.

-   CompareSRMTransects2014to2025.R: Process and visualize line intersect transects (LIT) and belt transects from 2014 and 2025.

-   SRM_CHM.R: Create canopy height model (CHM) for US-SRM from 2025 LiDAR.

### /Data/GitData

Data needed to run code in /Code.

### /Plots

Plots made from the code in /Code are saved in /Plots.

### /Reports

This folder contains summaries of work done collecting and analyzing data.

-   Biometric Gradient Notes.pdf: Summarizes methods used to establish a biometric gradient at both US-SRG and US-SRM. Includes a table of average measurements and coordinates of the mesquite individuals chosen and figures of the ranges of heights, canopy diameters, and basal diameters included in the gradients.
-   DroneToCensusComparison: Summarizes methods used to compare drone and ground collected data on woody individuals within 100-m of the US-SRG flux tower. Includes figures of height, canopy diameter, and cannopy area distributions, as well as a visual of canopy outlines.
