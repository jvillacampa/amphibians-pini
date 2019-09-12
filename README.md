# Analysis of amphibian communities changes with altitude
In this respository you can find the code used to analyze the data presented in: Villacampa, J., Whitworth, A., Allen, L., & Malo, J. E. (2019). [Altitudinal differences in alpha, beta and functional diversity of an amphibian community in a biodiversity hotspot](https://www.tandfonline.com/doi/full/10.1080/23766808.2019.1659022). Neotropical Biodiversity, 5(1), 60-68.

## Scripts R
- 1-creating_basefiles: This script needs to be run first. It creates the basefiles that will be used through the analysis.
- Functions and packages: Includes functions and packages used in the analysis. It is called in the rest of scripts.
- Abundance analysis: It creates abundance tables and look at species abundance changes with altitude.
- Diversity analysis: Calculates alpha and beta diversity for the community and the different groups. Figures 1-3 too.
- Models: Series of models between abundance and alpha diversity measure and altitudinal band.
- Multivariate analysis: Principal component analysis for the forest structure data.
- Whittaker plots: Looking at community structure using Whittaker plots. Not used in manuscript.
- session-info: list that includes the R version and the packages used.

## Datos/raw_data
This folder includes all the data collected during the study:
- amphibian_survey_data: dataset with all observation of herpetofauna recorded during the study.
- functional_groups_lookup: list of species and their functional groups.
- vegmap_transect_data: aggregated data on forest structure variables for each one of the transects.
- weather_data: weather data collected for each of the altitudinal bands.
