# Master-Thesis-2025

# Title Master thesis: Teachers and Technology: A Longitudinal Investigation of Attitudes, Perceived Usefulness, and Ease of Use
#### Author:
#### Date
#### Master 

This repository contains the R scripts for a longitudinal analysis of technology acceptance examining three key constructs:
- Attitudes toward technology (ATT)
- Perceived usefulness (PU)
- Perceived ease of use (PEOU)

The analysis uses longitudinal measurement invariance testing and latent growth curve modeling to investigate how these constructs change over time.

## Repository Structure

The repository is organized into four main R scripts, each handling a distinct part of the analysis:

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

## Key Findings

- ATT and PU showed stable patterns over time (no-growth models)
- PEOU demonstrated a linear change trajectory over time
- Significant relationships were found between the initial levels of all three constructs
- Gender and age had varying effects on the development of these technology acceptance factors

## How to Use

1. Clone this repository
2. Ensure you have the required packages installed:
   ```r
   install.packages(c("lavaan", "dplyr", "tidyverse", "readxl", "naniar", 
                    "VIM", "psych", "ggplot2", "RColorBrewer"))
   ```
3. Run the scripts in order (01 → 02 → 03 → 04)
4. Each script saves necessary data and model objects for use in subsequent analyses

## Data Requirements

The analysis requires a dataset with the following structure:
- Multiple time points (T1, T2, T3) of measurement
- Items for each construct (ATT, PU, PEOU) at each time point
- Demographic variables (gender, age)

The original data file used is "TAM-LT-FullData.xlsx", which should be placed in the same directory as the scripts.

## References

This analysis follows the longitudinal structural equation modeling framework as described in:

- Little, T. D. (2013). Longitudinal structural equation modeling. Guilford Press.
- Newsom, J. T. (2015). Longitudinal structural equation modeling: A comprehensive introduction. Routledge.
- Technology Acceptance Model (TAM) as proposed by Davis (1989).

## License

[Insert your license information here]

## Contact

[Insert your contact information here]

References This analysis follows the longitudinal structural equation modeling framework as described in:

Little, T. D. (2013). Longitudinal structural equation modeling. Guilford Press. Technology Acceptance Model (TAM) as proposed by Davis (1989). Preacher, K. J., Wichman, A. L., MacCallum, R. C., & Briggs, N. E. (2008). Latent growth curve modeling (1st ed., Vol. 157). Sage Publ. https://doi.org/10.4135/9781412984737![image](https://github.com/user-attachments/assets/9cd24083-5d9d-4f9b-b409-8f96579f3c53)
