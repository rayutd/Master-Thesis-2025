
###############################################################################
#                     MEASUREMENT INVARIANCE TESTING                            #
###############################################################################
# Author: Rayad Sakyar
# Date: May 2025
# Description: This script tests measurement invariance across time points
#              for the three key constructs: Attitudes (ATT), Perceived 
#              Usefulness (PU), and Perceived Ease of Use (PEOU).

# Load required packages
library(lavaan)      # For CFA and SEM analyses
library(dplyr)       # For data manipulation

# Load prepared data
New_data <- readRDS("New_data_clean.rds")

###############################################################################
#                   Factor loadings extract                           #
###############################################################################
att_modelxx <- '
ATT_T1 =~ ATT1_T1 + ATT2_T1 + ATT3_T1 + ATT4_T1
ATT_T2 =~ ATT1_T2 + ATT2_T2 + ATT3_T2 + ATT4_T2
ATT_T3 =~ ATT1_T3 + ATT2_T3 + ATT3_T3 + ATT4_T3
'
# Fit the model for ATT
fit_attxx <- cfa(att_modelxx, data = data, estimator = "MLR", missing = "fiml")

# Get the factor loadings
att_loadings <- parameterEstimates(fit_attxx, standardized = TRUE)
att_loadings <- att_loadings[att_loadings$op == "=~", c("lhs", "rhs", "est", "std.all")]
(att_loadings) # View the loadings



pu_modelxx <- '
PU_T1 =~ PU1_T1 + PU2_T1 + PU3_T1 + PU4_T1
PU_T2 =~ PU1_T2 + PU2_T2 + PU3_T2 + PU4_T2
PU_T3 =~ PU1_T3 + PU2_T3 + PU3_T3 + PU4_T3
'
fit_puxx <- cfa(pu_modelxx, data = data, estimator = "MLR", missing = "fiml")
# Extract standardized factor loadings
pu_loadings <- parameterEstimates(fit_puxx, standardized = TRUE)
pu_loadings <- pu_loadings[pu_loadings$op == "=~", c("lhs", "rhs", "est", "std.all")]
(pu_loadings) 



peou_modelxx <- '
PEOU_T1 =~ PEOU1_T1 + PEOU2_T1 + PEOU3_T1 + PEOU4_T1
PEOU_T2 =~ PEOU1_T2 + PEOU2_T2 + PEOU3_T2 + PEOU4_T2
PEOU_T3 =~ PEOU1_T3 + PEOU2_T3 + PEOU3_T3 + PEOU4_T3
'
fit_peouxx <- cfa(peou_modelxx, data = data, estimator = "MLR", missing = "fiml")
# Extract standardized factor loadings
peou_loadings <- parameterEstimates(fit_peouxx, standardized = TRUE)
peou_loadings <- pu_loadings[pu_loadings$op == "=~", c("lhs", "rhs", "est", "std.all")]
(pu_loadings) 


# Based on factor loading analysis, remove ATT3 items
New_data <- New_data[, !grepl("ATT3_", names(New_data))] 

###############################################################################
#                   ATTITUDES TOWARD TECHNOLOGY (ATT)                         #
###############################################################################
#-------------------------------------------------------------------------------
#                1. Configural Invariance (Baseline CFA)
#-------------------------------------------------------------------------------
# Test if same factor structure holds across time points
att_configural <- "
  ATT_T1 =~ ATT1_T1 + ATT2_T1 + ATT4_T1
  ATT_T2 =~ ATT1_T2 + ATT2_T2 + ATT4_T2
  ATT_T3 =~ ATT1_T3 + ATT2_T3 + ATT4_T3

  ## Correlate residuals
  ATT1_T1 ~~ ATT1_T2 + ATT1_T3
  ATT1_T2 ~~ ATT1_T3
  ATT2_T1 ~~ ATT2_T2 + ATT2_T3
  ATT2_T2 ~~ ATT2_T3
  ATT4_T1 ~~ ATT4_T2 + ATT4_T3
  ATT4_T2 ~~ ATT4_T3
"

fit_configural_att <- cfa(
  att_configural,
  data       = New_data,
  missing    = "fiml",  # dealing with missing data
  estimator  = "MLR"    
)
summary(fit_configural_att, fit.measures = TRUE, standardized = TRUE)


#-------------------------------------------------------------------------------
#                   2. Weak Invariance (Equal Loadings)
#-------------------------------------------------------------------------------
att_weak <- "
  ## weak invariance: same loadings across time
  ATT_T1 =~ l1*ATT1_T1 + l2*ATT2_T1 + l3*ATT4_T1
  ATT_T2 =~ l1*ATT1_T2 + l2*ATT2_T2 + l3*ATT4_T2
  ATT_T3 =~ l1*ATT1_T3 + l2*ATT2_T3 + l3*ATT4_T3
  
  # # Free latent variances at later time points
  # ATT_T2 ~~ NA*ATT_T2
  # ATT_T3 ~~ NA*ATT_T3

  ## correlations residual
  ATT1_T1 ~~ ATT1_T2 + ATT1_T3
  ATT1_T2 ~~ ATT1_T3
  ATT2_T1 ~~ ATT2_T2 + ATT2_T3
  ATT2_T2 ~~ ATT2_T3
  ATT4_T1 ~~ ATT4_T2 + ATT4_T3
  ATT4_T2 ~~ ATT4_T3
"

fit_weak_att <- cfa(
  att_weak,
  data      = New_data,
  missing   = "fiml",
  estimator = "MLR"
)
summary(fit_weak_att, fit.measures = TRUE, standardized = TRUE)

# Compare with configural
lavTestLRT(fit_configural_att, fit_weak_att)


#-------------------------------------------------------------------------------
#             3. Strong Invariance (Equal Loadings + Intercepts)
#-------------------------------------------------------------------------------
att_strong <- "
  ## Same loadings
  ATT_T1 =~ l1*ATT1_T1 + l2*ATT2_T1 + l3*ATT4_T1
  ATT_T2 =~ l1*ATT1_T2 + l2*ATT2_T2 + l3*ATT4_T2
  ATT_T3 =~ l1*ATT1_T3 + l2*ATT2_T3 + l3*ATT4_T3
  
  # # Free latent variances at later time points
  # ATT_T2 ~~ NA*ATT_T2
  # ATT_T3 ~~ NA*ATT_T3
  
  ## constrain Intercepts
  ATT1_T1 ~ t1*1
  ATT2_T1 ~ t2*1
  ATT4_T1 ~ t4*1
  ATT1_T2 ~ t1*1
  ATT2_T2 ~ t2*1
  ATT4_T2 ~ t4*1
  ATT1_T3 ~ t1*1
  ATT2_T3 ~ t2*1
  ATT4_T3 ~ t4*1


  ## residual correlations
  ATT1_T1 ~~ ATT1_T2 + ATT1_T3
  ATT1_T2 ~~ ATT1_T3
  ATT2_T1 ~~ ATT2_T2 + ATT2_T3
  ATT2_T2 ~~ ATT2_T3
  ATT4_T1 ~~ ATT4_T2 + ATT4_T3
  ATT4_T2 ~~ ATT4_T3
  
  # Free Latent Means at T2/T3
  ATT_T2 ~ 1  # Estimated mean relative to T1 (fixed at 0)
  ATT_T3 ~ 1
"

fit_strong_att <- cfa(
  att_strong,
  data      = New_data,
  missing   = "fiml",
  estimator = "MLR"
)
summary(fit_strong_att, fit.measures = TRUE, standardized=TRUE)

# Quick fit measure check
fitMeasures(fit_configural_att, c("cfi","tli","rmsea","srmr","aic","bic"))
fitMeasures(fit_weak_att,   c("cfi","tli","rmsea","srmr","aic","bic"))
fitMeasures(fit_strong_att, c("cfi","tli","rmsea","srmr","aic","bic"))

# Compare with metric
lavTestLRT(fit_configural_att, fit_weak_att, fit_strong_att)

###############################################################################
#                   ATTITUDES TOWARD TECHNOLOGY (ATT)                         #
###############################################################################
#-------------------------------------------------------------------------------
#                  1. PU 1: Configural Invariance (PU)
#-------------------------------------------------------------------------------
pu_configural <- "
  PU_T1 =~ PU1_T1 + PU2_T1 + PU3_T1 + PU4_T1
  PU_T2 =~ PU1_T2 + PU2_T2 + PU3_T2 + PU4_T2
  PU_T3 =~ PU1_T3 + PU2_T3 + PU3_T3 + PU4_T3

  # Residual correlations
  PU1_T1 ~~ PU1_T2 + PU1_T3
  PU1_T2 ~~ PU1_T3
  PU2_T1 ~~ PU2_T2 + PU2_T3
  PU2_T2 ~~ PU2_T3
  PU3_T1 ~~ PU3_T2 + PU3_T3
  PU3_T2 ~~ PU3_T3
  PU4_T1 ~~ PU4_T2 + PU4_T3
  PU4_T2 ~~ PU4_T3
"

fit_configural_pu <- cfa(
  pu_configural,
  data = New_data,
  missing = "fiml",
  estimator = "MLR"
)
summary(fit_configural_pu, fit.measures=TRUE, standardized=TRUE)

#-------------------------------------------------------------------------------
#                  2. Weak (Metric) Invariance (PU)
#-------------------------------------------------------------------------------
pu_weak <- "
  PU_T1 =~ l1*PU1_T1 + l2*PU2_T1 + l3*PU3_T1 + l4*PU4_T1
  PU_T2 =~ l1*PU1_T2 + l2*PU2_T2 + l3*PU3_T2 + l4*PU4_T2
  PU_T3 =~ l1*PU1_T3 + l2*PU2_T3 + l3*PU3_T3 + l4*PU4_T3
  
  # Free latent variances at later time points
  PU_T2 ~~ NA*PU_T2
  PU_T3 ~~ NA*PU_T3
  
  
  # Residual correlations (same as configural)
  PU1_T1 ~~ PU1_T2 + PU1_T3
  PU1_T2 ~~ PU1_T3
  PU2_T1 ~~ PU2_T2 + PU2_T3
  PU2_T2 ~~ PU2_T3
  PU3_T1 ~~ PU3_T2 + PU3_T3
  PU3_T2 ~~ PU3_T3
  PU4_T1 ~~ PU4_T2 + PU4_T3
  PU4_T2 ~~ PU4_T3
"

fit_weak_pu <- cfa(
  pu_weak,
  data = New_data,
  missing = "fiml",
  estimator = "MLR"
)
summary(fit_weak_pu, fit.measures=TRUE, standardized=TRUE)

#-------------------------------------------------------------------------------
#              3. scalar (Strong) Invariance (PU) 
#-------------------------------------------------------------------------------
pu_strong <- "
  PU_T1 =~ l1*PU1_T1 + l2*PU2_T1 + l3*PU3_T1 + l4*PU4_T1
  PU_T2 =~ l1*PU1_T2 + l2*PU2_T2 + l3*PU3_T2 + l4*PU4_T2
  PU_T3 =~ l1*PU1_T3 + l2*PU2_T3 + l3*PU3_T3 + l4*PU4_T3
   
  # Free latent variances at later time points
  PU_T2 ~~ NA*PU_T2
  PU_T3 ~~ NA*PU_T3
  
  
  # intercept constraints
  PU1_T1 ~ t1*1
  PU1_T2 ~ t1*1
  PU1_T3 ~ t1*1
  
  PU2_T1 ~ t2*1
  PU2_T2 ~ t2*1
  PU2_T3 ~ t2*1
  
  PU3_T1 ~ t3*1
  PU3_T2 ~ t3*1
  PU3_T3 ~ t3*1
  
  PU4_T1 ~ t4*1
  PU4_T2 ~ t4*1
  PU4_T3 ~ t4*1
  
  # Residual correlations
  PU1_T1 ~~ PU1_T2 + PU1_T3
  PU1_T2 ~~ PU1_T3
  PU2_T1 ~~ PU2_T2 + PU2_T3
  PU2_T2 ~~ PU2_T3
  PU3_T1 ~~ PU3_T2 + PU3_T3
  PU3_T2 ~~ PU3_T3
  PU4_T1 ~~ PU4_T2 + PU4_T3
  PU4_T2 ~~ PU4_T3
  
  # Free Latent Means at T2/T3
  PU_T2 ~ 1  # Estimated mean relative to T1 (fixed at 0)
  PU_T3 ~ 1
  
"
fit_strong_pu <- cfa(
  pu_strong,
  data = New_data,
  missing = "fiml",
  estimator = "MLR"
)
summary(fit_strong_pu, fit.measures=TRUE, standardized=TRUE)

# Extracting model fit indicies
fitmeasures(fit_configural_pu, c("cfi","tli","rmsea","srmr","aic","bic")) 
fitmeasures(fit_weak_pu, c("cfi","tli","rmsea","srmr","aic","bic")) 
fitmeasures(fit_strong_pu, c("cfi","tli","rmsea","srmr","aic","bic")) 

# Comparing models
anova(fit_configural_pu, fit_weak_pu)
anova(fit_weak_pu, fit_strong_pu)
anova(fit_configural_pu, fit_weak_pu,fit_strong_pu)


###############################################################################
#                    PERCEIVED EASE OF USE (PEOU)                             #
###############################################################################

#-------------------------------------------------------------------------------
#                   1. Configural Invariance (PEOU)
#-------------------------------------------------------------------------------

peou_configural <- "
  PEOU_T1 =~ PEOU1_T1 + PEOU2_T1 + PEOU3_T1 + PEOU4_T1
  PEOU_T2 =~ PEOU1_T2 + PEOU2_T2 + PEOU3_T2 + PEOU4_T2
  PEOU_T3 =~ PEOU1_T3 + PEOU2_T3 + PEOU3_T3 + PEOU4_T3

  # Residual correlations
  PEOU1_T1 ~~ PEOU1_T2 + PEOU1_T3
  PEOU1_T2 ~~ PEOU1_T3
  PEOU2_T1 ~~ PEOU2_T2 + PEOU2_T3
  PEOU2_T2 ~~ PEOU2_T3
  PEOU3_T1 ~~ PEOU3_T2 + PEOU3_T3
  PEOU3_T2 ~~ PEOU3_T3
  PEOU4_T1 ~~ PEOU4_T2 + PEOU4_T3
  PEOU4_T2 ~~ PEOU4_T3
"
fit_configural_peou <- cfa(peou_configural, data = New_data, missing = "fiml", estimator = "MLR")
summary(fit_configural_peou, fit.measures = TRUE, standardized = TRUE)

#-------------------------------------------------------------------------------
#                      2. Metric (weak)  (peou)
#-------------------------------------------------------------------------------
peou_weak <- "
  PEOU_T1 =~ l1*PEOU1_T1 + l2*PEOU2_T1 + l3*PEOU3_T1 + l4*PEOU4_T1
  PEOU_T2 =~ l1*PEOU1_T2 + l2*PEOU2_T2 + l3*PEOU3_T2 + l4*PEOU4_T2
  PEOU_T3 =~ l1*PEOU1_T3 + l2*PEOU2_T3 + l3*PEOU3_T3 + l4*PEOU4_T3
  
  # free latent variance 
  PEOU_T2 ~~ NA*PEOU_T2
  PEOU_T3 ~~ NA*PEOU_T3
  
  # Same residual correlations as configural
  PEOU1_T1 ~~ PEOU1_T2 + PEOU1_T3
  PEOU1_T2 ~~ PEOU1_T3
  PEOU2_T1 ~~ PEOU2_T2 + PEOU2_T3
  PEOU2_T2 ~~ PEOU2_T3
  PEOU3_T1 ~~ PEOU3_T2 + PEOU3_T3
  PEOU3_T2 ~~ PEOU3_T3
  PEOU4_T1 ~~ PEOU4_T2 + PEOU4_T3
  PEOU4_T2 ~~ PEOU4_T3
"
fit_weak_peou <- cfa(peou_weak, data = New_data, missing = "fiml", estimator = "MLR")
summary(fit_weak_peou, fit.measures = TRUE, standardized = TRUE)
lavTestLRT(fit_configural_peou, fit_weak_peou)

#-------------------------------------------------------------------------------
#                       3. scalar (strong) (PEOU))
#-------------------------------------------------------------------------------
peou_strong <- "
  PEOU_T1 =~ l1*PEOU1_T1 + l2*PEOU2_T1 + l3*PEOU3_T1 + l4*PEOU4_T1
  PEOU_T2 =~ l1*PEOU1_T2 + l2*PEOU2_T2 + l3*PEOU3_T2 + l4*PEOU4_T2
  PEOU_T3 =~ l1*PEOU1_T3 + l2*PEOU2_T3 + l3*PEOU3_T3 + l4*PEOU4_T3
  
  # free latent variance
  PEOU_T2 ~~ NA*PEOU_T2
  PEOU_T3 ~~ NA*PEOU_T3
  

  # Equal intercepts
  PEOU1_T1 ~ t1*1
  PEOU1_T2 ~ t1*1
  PEOU1_T3 ~ t1*1

  PEOU2_T1 ~ t2*1
  PEOU2_T2 ~ t2*1
  PEOU2_T3 ~ t2*1  

  PEOU3_T1 ~ t3*1
  PEOU3_T2 ~ t3*1
  PEOU3_T3 ~ t3*1

  PEOU4_T1 ~ t4*1
  PEOU4_T2 ~ t4*1
  PEOU4_T3 ~ t4*1 

  # residual correlations
  PEOU1_T1 ~~ PEOU1_T2 + PEOU1_T3
  PEOU1_T2 ~~ PEOU1_T3
  PEOU2_T1 ~~ PEOU2_T2 + PEOU2_T3
  PEOU2_T2 ~~ PEOU2_T3
  PEOU3_T1 ~~ PEOU3_T2 + PEOU3_T3
  PEOU3_T2 ~~ PEOU3_T3
  PEOU4_T1 ~~ PEOU4_T2 + PEOU4_T3
  PEOU4_T2 ~~ PEOU4_T3
  
  # Model latent means
  PEOU_T2 ~ 1
  PEOU_T3 ~ 1
"
fit_strong_peou <- cfa(peou_strong, data = New_data, missing = "fiml", estimator = "MLR")
summary(fit_strong_peou, fit.measures = TRUE, standardized = TRUE)

# Compare model fit measures
fitMeasures(fit_configural_peou, c("cfi","tli","rmsea","srmr","aic","bic"))
fitMeasures(fit_weak_peou, c("cfi","tli","rmsea","srmr","aic","bic"))
fitMeasures(fit_strong_peou, c("cfi","tli","rmsea","srmr","aic","bic"))

# Comparing nested models
lavTestLRT(fit_configural_peou,fit_weak_peou, fit_strong_peou)


# Save measurement models for later use
saveRDS(fit_strong_att, "fit_strong_att.rds")
saveRDS(fit_strong_pu, "fit_strong_pu.rds")
saveRDS(fit_strong_peou, "fit_strong_peou.rds")
