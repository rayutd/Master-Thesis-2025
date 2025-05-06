###############################################################################
#                          DATA VISUALIZATION CODE                             #
###############################################################################
# Author: Rayad Sakyar
# Date: May 2025
# Description: Visualization scripts for the technology acceptance longitudinal study

# Load required packages
library(ggplot2)
library(tidyverse)

#-------------------------------------------------------------------------------
# 1. Missing Data Visualization
#-------------------------------------------------------------------------------
# Create summary of missing data
missing_data_summary <- data.frame(
  variable = names(data_descript),
  missing_n = colSums(is.na(data_descript)),
  missing_percent = round(colMeans(is.na(data_descript)) * 100, 1)
)

# Basic missing data plot
missing_data_plot <- ggplot(missing_data_summary, aes(x = reorder(variable, -missing_percent), y = missing_percent)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Variable", y = "% Missing") +
  theme_minimal()

# Enhanced missing data plot with better formatting
missing_data_plot_enhanced <- ggplot(missing_data_summary, aes(x = reorder(variable, -missing_percent), y = missing_percent)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Variable", y = "% Missing") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),  # Increase variable names text size
    axis.title = element_text(size = 14),   # Increase axis titles size
    axis.text.x = element_text(size = 10)   # Adjust x-axis numbers size
  )

# Display the enhanced plot
print(missing_data_plot_enhanced)

#-------------------------------------------------------------------------------
# 2. Longitudinal Trajectory Visualization
#-------------------------------------------------------------------------------

# Create mean scores per construct and time point
New_data_long <- New_data %>%
  mutate(
    ATT_T1 = rowMeans(select(., ATT1_T1, ATT2_T1, ATT4_T1), na.rm = TRUE),
    ATT_T2 = rowMeans(select(., ATT1_T2, ATT2_T2, ATT4_T2), na.rm = TRUE),
    ATT_T3 = rowMeans(select(., ATT1_T3, ATT2_T3, ATT4_T3), na.rm = TRUE),
    PU_T1 = rowMeans(select(., PU1_T1, PU2_T1, PU3_T1, PU4_T1), na.rm = TRUE),
    PU_T2 = rowMeans(select(., PU1_T2, PU2_T2, PU3_T2, PU4_T2), na.rm = TRUE),
    PU_T3 = rowMeans(select(., PU1_T3, PU2_T3, PU3_T3, PU4_T3), na.rm = TRUE),
    PEOU_T1 = rowMeans(select(., PEOU1_T1, PEOU2_T1, PEOU3_T1, PEOU4_T1), na.rm = TRUE),
    PEOU_T2 = rowMeans(select(., PEOU1_T2, PEOU2_T2, PEOU3_T2, PEOU4_T2), na.rm = TRUE),
    PEOU_T3 = rowMeans(select(., PEOU1_T3, PEOU2_T3, PEOU3_T3, PEOU4_T3), na.rm = TRUE)
  ) %>%
  select(ID, starts_with("ATT_T"), starts_with("PU_T"), starts_with("PEOU_T"))

# Reshape to long format for plotting
plot_data <- New_data_long %>%
  pivot_longer(
    cols = -ID,
    names_to = c("Construct", "Time"),
    names_pattern = "(.*)_T(\\d)",
    values_to = "Score"
  ) %>%
  mutate(
    Time = as.numeric(Time),
    Construct = factor(Construct, levels = c("ATT", "PEOU", "PU"))
  )

# Color plot of individual trajectories
color_trajectory_plot <- ggplot(plot_data, aes(x = Time, y = Score, group = ID, color = Construct)) +
  geom_line(alpha = 0.2) +
  stat_summary(fun = mean, geom = "line", size = 1.2, aes(group = Construct)) +
  scale_color_manual(values = c("ATT" = "red", "PEOU" = "green", "PU" = "blue")) +
  facet_wrap(~Construct, scales = "free_y") +
  labs(
    title = "",
    x = "Time",
    y = "Score",
    color = "Construct"
  ) +
  theme_minimal(base_size = 14)

# Black and white plot of individual trajectories (publication-ready)
bw_trajectory_plot <- ggplot(plot_data, aes(x = Time, y = Score, group = ID)) +
  geom_line(alpha = 0.2, color = "black") +  # Individual trajectories in black
  stat_summary(fun = mean, geom = "line", size = 1.2, color = "black", aes(group = Construct)) +  # Mean line
  facet_wrap(~Construct, scales = "free_y") +
  labs(
    title = "",
    x = "Time",
    y = "Score"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 14, face = "bold")
  )

# Display the black and white plot
print(bw_trajectory_plot)

# Save the plot as a high-resolution PNG
ggsave(
  filename = "construct_trajectories_bw.png",
  plot = bw_trajectory_plot,
  width = 10,
  height = 6,
  dpi = 600
)

#-------------------------------------------------------------------------------
# 3. Covariate Effects Visualization for PEOU
#-------------------------------------------------------------------------------
# Time points (T1 = 0, T2 = 1, T3 = 2)
time_points <- 0:2
labels <- c("T1", "T2", "T3")

# Standardized coefficients from your model
# Intercept & slope estimates for each covariate
intercept_gender <- 0.222   # standardized beta (GENDER on intercept)
slope_gender     <- -0.148  # standardized beta (GENDER on slope)
intercept_age    <- -0.375  # standardized beta (AGE_centered on intercept)
slope_age        <- 0.837   # standardized beta (AGE_centered on slope)

# Function to simulate latent means by covariate
simulate_trajectory <- function(gender, age_centered, label) {
  intercept <- 0 + gender * intercept_gender + age_centered * intercept_age
  slope     <- 0 + gender * slope_gender + age_centered * slope_age
  
  # Compute expected latent means across time
  peou_scores <- intercept + slope * time_points
  
  data.frame(
    Time = time_points,
    PEOU = peou_scores,
    Group = label
  )
}

# Simulate for different covariate combinations
peou_data <- bind_rows(
  simulate_trajectory(gender = 0, age_centered = -10, label = "F, Age -1 SD"),
  simulate_trajectory(gender = 0, age_centered =  10, label = "F, Age +1 SD"),
  simulate_trajectory(gender = 1, age_centered = -10, label = "M, Age -1 SD"),
  simulate_trajectory(gender = 1, age_centered =  10, label = "M, Age +1 SD")
)

# Convert time points to T1–T3 labels
peou_data$Time <- factor(peou_data$Time, levels = 0:2, labels = labels)

# Create covariate effects plot
covariate_plot <- ggplot(peou_data, aes(x = Time, y = PEOU, color = Group, group = Group)) +
  geom_line(size = 1.1) +
  geom_point(size = 2.5) +
  labs(
    title = "",
    x = "Time",
    y = "Standardized PEOU Latent Mean"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = c(0.9, 0.5),  # legend inside the plot
    legend.background = element_rect(fill = "white", color = "gray80"),
    legend.title = element_blank(),
    legend.text = element_text(size = 11)
  )

# Display the covariate plot
print(covariate_plot)

#-------------------------------------------------------------------------------
# 4. Contexual Structural Equation Model Path Diagrams
#-------------------------------------------------------------------------------
# semPlot package need to loaded
library(semPlot)

# Plot the no-growth model for ATT
semPlot::semPaths(
  fit_no_growth_att, 
  "path", "stand", 
  edge.label.cex = 0.7, 
  nCharNodes = 1, 
  nDigits = 2, 
  rotation = 1,
  edge.color = "black"
)

# Plot the no-growth model with covariates for ATT
semPlot::semPaths(
  fit_linear_att, 
  "path", "stand", 
  edge.label.cex = 0.7, 
  nCharNodes = 1, 
  nDigits = 2, 
  rotation = 1,
  edge.color = "black"
)

# Plot the linear growth model with covariates for PEOU
semPlot::semPaths(
  fit_quadratic_att, 
  "path", "stand", 
  edge.label.cex = 0.7, 
  nCharNodes = 1, 
  nDigits = 2, 
  rotation = 1,
  edge.color = "black"
)

