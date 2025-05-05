###############################################################################
#                           DATA WRANGLING & PREPARATION                         #
###############################################################################
# Author: Rayad Sakyar
# Date: May 2025
# Description: This script handles data preparation and cleaning for the 
#              technology acceptance longitudinal study analysis, including
#              missing data assessment and sample description.

# Load required packages
library(readxl)      # For reading Excel files
library(dplyr)       # For data manipulation
library(tidyverse)   # For data transformation
library(naniar)      # For visualizing missing data
library(VIM)         # For missing data visualization
library(psych)       # For descriptive statistics
library(ggplot2)     # For creating plots
library(RColorBrewer) # For color palettes in plots

## Increase the length of possible results
options(max.print = 100000)

Data <- read_excel("TAM-LT-FullData.xlsx")
head(Data)

table(Data$COUNTRY)
sum(duplicated(Data$COUNTRY))

str(Data$COUNTRY_Move)
table(Data$COUNTRY_Move)
which(Data$COUNTRY=="United Kingdom")


# Filter data to include only UK respondents
data <- Data[Data$COUNTRY=="United Kingdom",]

### checking if there is duplicated ID #####
length(unique(data$ProlificID))
sum(duplicated(data$ProlificID))


#-------------------------------------------------------------------------------
# Extract key measurement variables
#-------------------------------------------------------------------------------
ATT_cols <- grep("^ATT", names(data), value = TRUE) # looking at the attitude columns 
ATT_data <- data[,ATT_cols]


Useful_cols <- grep("^PU", names(data), value = TRUE) # Perceived usefulness of technology for teaching
Useful_data <- data[,Useful_cols]


PEOU_cols <- grep("^PEOU", names(data),value = T) # Perceived ease of use
PEOU_data <- data[,PEOU_cols]


#-------------------------------------------------------------------------------
# Create dataset with only needed variables
#-------------------------------------------------------------------------------
selected_col <- c("ProlificID","GENDER","AGE_T1",
                  "AGE_T2","AGE_T3","AGE_T4",
                  ATT_cols,Useful_cols,PEOU_cols)

# Create new dataset for analysis
New_data <- data[,selected_col]

#-------------------------------------------------------------------------------
# Descriptive statistics and missingness analysis
#-------------------------------------------------------------------------------
# Custom function to add missing value info to descriptive statistics
data_descript <- data[selected_col]
describe_with_missing <- function(data_descript) {
  desc <- describe(data_descript)
  missing_count <- colSums(is.na(data_descript))
  missing_percent <- round((missing_count / nrow(data_descript)) * 100, 1)
  
  desc$missing_n <- missing_count
  desc$missing_percent <- missing_percent
  
  return(desc)
}
describe_with_missing(data_descript)

# Analyze row-wise missingness 
New_data <- New_data %>%
  mutate(
    missing_count = rowSums(is.na(select(., -ProlificID, -GENDER)))
  )

# Summarize missingness distribution
missing_summary <- New_data %>%
  count(missing_count) %>%
  mutate(percentage = n / nrow(New_data) * 100)

(missing_summary)


# Calculate missingness per variable
variable_missing <- New_data %>%
  select(-ProlificID, -GENDER) %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count") %>%
  arrange(desc(missing_count))

print(variable_missing, n=54 ) 


#-------------------------------------------------------------------------------
# Visualize missingness patterns
#-------------------------------------------------------------------------------
# Percentage of missingness
vis_miss(New_data)
colMeans(is.na(New_data)) * 100  # Percentage missing per variable

# Create missingness summary dataframe
missing_data_summary <- data.frame(
  variable = names(data_descript),
  missing_n = colSums(is.na(data_descript)),
  missing_percent = round(colMeans(is.na(data_descript)) * 100, 1)
)

(missing_data_summary)


# Create bar chart of missing values percentage
ggplot(missing_data_summary, aes(x = reorder(variable, -missing_percent), y = missing_percent)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Variable", y = "% Missing") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 16),    # Variable names text size
    axis.title.x = element_text(size = 18),   # "% Missing" label size
    axis.title.y = element_text(size = 22),   # "Variable" label size (this is what you want to increase)
    axis.text.x = element_text(size = 16)     # Percentage values text size
  )

#-------------------------------------------------------------------------------
# Check for Missing Completely At Random (MCAR) pattern
#-------------------------------------------------------------------------------
result <- TestMCARNormality(New_data[, -c(1:2)])
print(result)  # If p < 0.05, MCAR is rejected

#-------------------------------------------------------------------------------
# Data preparation
#-------------------------------------------------------------------------------
# Recode IDs and Gender
# changning IDs and Gender to Numeric where 0= Female and 1=Male and 3 is Other ----
New_data <- New_data %>%
  mutate(
    # Convert ProlificID to numeric ID (1, 2, 3...)
    ID = as.numeric(factor(ProlificID, levels = unique(ProlificID))),
    
    # Recode GENDER to numeric (Female=0, Male=1, Other=3, NA remains NA)
    GENDER = case_when(
      GENDER == "Female" ~ 0,
      GENDER == "Male" ~ 1,
      GENDER == "Other" ~ 3,
      TRUE ~ NA_real_  # Keep NAs as NA
    )
  )

# checking the changes
table(New_data$GENDER, useNA = "always")
New_data <- New_data %>%
  filter(GENDER != 3)                       # Remove participants with gender = 3 (Other)
str(New_data)

# Reorder the dataset ----
New_data <- New_data %>%
  select(ID, everything(), -ProlificID) %>% # Move ID to the first column & remove ProlificID
  select(-matches("_T4$")) %>%              # Remove all columns ending with "_T4"
  select(-missing_count)                    # Remove the missing_count column

# checking the column order
colnames(New_data)
summary(New_data)


# Identify rows where all values (except ID and GENDER) are missing
fully_missing_rows <- apply(New_data[, -c(1, 2)], 1, function(x) all(is.na(x)))
# Create a new dataset with only these rows
New_data_missing <- New_data[fully_missing_rows, ]

# Check the number of rows in the new dataset
nrow(New_data_missing)
New_data <- New_data[!fully_missing_rows, ]
New_data
str(New_data)


# Save the prepared dataset for further analysis
saveRDS(New_data, "New_data_clean.rds")

