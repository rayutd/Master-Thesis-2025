# Master Thesis 2025

## Title: Teachers and Technology: A Longitudinal Investigation of Attitudes, Perceived Usefulness, and Ease of Use

### *Exploring Change Over Time Through Latent Growth Curve Models*

 Rayad Sakyar

 May 2025

Assessment, Measurement and Evaluation

Centre for Educational Measurement at the University of Oslo (CEMO)



This repository contains the R scripts for a longitudinal analysis of technology acceptance examining three key constructs:
- Attitudes toward technology (ATT)
- Perceived usefulness (PU)
- Perceived ease of use (PEOU)

The analysis uses longitudinal measurement invariance testing and latent growth curve modeling to investigate how these constructs change over time.

## Repository Structure

The repository is organized into five main R scripts, each handling a distinct part of the analysis:

1. **01_data_wrangling.R**
   - Data loading, cleaning, and preparation
   - Missing data analysis
   - Variable transformations

2. **02_measurement_invariance.R**
   - Configural, weak, and strong invariance testing for each construct
   - Tests whether the measurement of constructs remains stable over time

3. **03_growth_curve_models.R**
   - Individual growth models for each construct
   - Comparison between no-growth, linear, quadratic, and piecewise models
   - Covariate analyses with gender and age

4. **04_parallel_process_models.R**
   - Combined models examining relationships between constructs
   - Analysis of correlations between initial levels and change trajectories

5. **05_visualization.R**
   - Data visualization code
   - Missing data visualizations
   - Trajectory plots
   - Covariate effect visualizations
   - SEM path diagrams

## How to Use

1. Clone this repository
2. Ensure you have the required packages installed:
   ```r
   install.packages(c("lavaan", "dplyr", "tidyverse", "readxl", "naniar", 
                    "VIM", "psych", "ggplot2", "RColorBrewer", "semPlot"))
   ```
3. Run the scripts in order (01 → 02 → 03 → 04 → 05)
4. Each script saves necessary data and model objects for use in subsequent analyses


The original data file used is "TAM-LT-FullData.xlsx", which should be placed in the same directory as the scripts.

## References

This analysis follows the longitudinal structural equation modeling framework as described in:

- Technology Acceptance Model (TAM) as proposed by Davis (1989).
- Little, T. D. (2013). Longitudinal structural equation modeling. Guilford Press.
- Preacher, K. J., Wichman, A. L., MacCallum, R. C., & Briggs, N. E. (2008). Latent growth curve modeling (1st ed., Vol. 157). Sage Publ.



