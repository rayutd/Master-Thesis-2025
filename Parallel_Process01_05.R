###############################################################################
#                        PARALLEL PROCESS MODELS                               #
###############################################################################
# Author: Rayad Sakyar
# Date: May 2025
# Description: This script fits parallel process growth models to examine 
#              relationships between the three key constructs: Attitudes (ATT),
#              Perceived Usefulness (PU), and Perceived Ease of Use (PEOU).
#              Based on previous analyses, ATT and PU show no-growth patterns,
#              while PEOU shows linear growth.

# Load required packages
library(lavaan)      # For SEM and growth curve modeling
library(dplyr)       # For data manipulation

# Load prepared data and previous models
New_data <- readRDS("New_data_clean.rds")


##==============================================================================
#         RQ3: Parallel Process Models for Pairs of Constructs 
# ==============================================================================
###############################################################################
#                    ATT-PU PARALLEL PROCESS MODEL                            #
###############################################################################
att_pu_combined <- "
  # ATT Measurement Model (Strong Invariance)
  ATT_T1 =~ l1_att*ATT1_T1 + l2_att*ATT2_T1 + l3_att*ATT4_T1
  ATT_T2 =~ l1_att*ATT1_T2 + l2_att*ATT2_T2 + l3_att*ATT4_T2
  ATT_T3 =~ l1_att*ATT1_T3 + l2_att*ATT2_T3 + l3_att*ATT4_T3
  
  # Free latent variances at later time points (ATT)
  ATT_T2 ~~ NA*ATT_T2
  ATT_T3 ~~ NA*ATT_T3
  
  # ATT Intercepts
  ATT1_T1 ~ t1_att*1
  ATT2_T1 ~ t2_att*1
  ATT4_T1 ~ t4_att*1
  ATT1_T2 ~ t1_att*1
  ATT2_T2 ~ t2_att*1
  ATT4_T2 ~ t4_att*1
  ATT1_T3 ~ t1_att*1
  ATT2_T3 ~ t2_att*1
  ATT4_T3 ~ t4_att*1
  
  # ATT Residual correlations
  ATT1_T1 ~~ ATT1_T2 + ATT1_T3
  ATT1_T2 ~~ ATT1_T3
  ATT2_T1 ~~ ATT2_T2 + ATT2_T3
  ATT2_T2 ~~ ATT2_T3
  ATT4_T1 ~~ ATT4_T2 + ATT4_T3
  ATT4_T2 ~~ ATT4_T3
  
  # PU Measurement Model (Strong Invariance)
  PU_T1 =~ l1_pu*PU1_T1 + l2_pu*PU2_T1 + l3_pu*PU3_T1 + l4_pu*PU4_T1
  PU_T2 =~ l1_pu*PU1_T2 + l2_pu*PU2_T2 + l3_pu*PU3_T2 + l4_pu*PU4_T2
  PU_T3 =~ l1_pu*PU1_T3 + l2_pu*PU2_T3 + l3_pu*PU3_T3 + l4_pu*PU4_T3
  
  # Free latent variances at later time points (PU)
  PU_T2 ~~ NA*PU_T2
  PU_T3 ~~ NA*PU_T3
  
  # PU Intercepts
  PU1_T1 ~ t1_pu*1
  PU2_T1 ~ t2_pu*1
  PU3_T1 ~ t3_pu*1
  PU4_T1 ~ t4_pu*1
  PU1_T2 ~ t1_pu*1
  PU2_T2 ~ t2_pu*1
  PU3_T2 ~ t3_pu*1
  PU4_T2 ~ t4_pu*1
  PU1_T3 ~ t1_pu*1
  PU2_T3 ~ t2_pu*1
  PU3_T3 ~ t3_pu*1
  PU4_T3 ~ t4_pu*1
  
  # PU Residual correlations
  PU1_T1 ~~ PU1_T2 + PU1_T3
  PU1_T2 ~~ PU1_T3
  PU2_T1 ~~ PU2_T2 + PU2_T3
  PU2_T2 ~~ PU2_T3
  PU3_T1 ~~ PU3_T2 + PU3_T3
  PU3_T2 ~~ PU3_T3
  PU4_T1 ~~ PU4_T2 + PU4_T3
  PU4_T2 ~~ PU4_T3
  
  # ATT Growth Model (No-Growth)
  i_att =~ 1*ATT_T1 + 1*ATT_T2 + 1*ATT_T3
  
  # PU Growth Model (No-Growth)
  i_pu =~ 1*PU_T1 + 1*PU_T2 + 1*PU_T3
  
  # Growth Factor Means
  i_att ~ 1
  i_pu ~ 1
  
  # Growth Factor Variances & Covariance
  i_att ~~ i_att
  i_pu ~~ i_pu
  i_att ~~ i_pu  # This is the key correlation we are interested in
"

fit_att_pu <- growth(
  att_pu_combined,
  data = New_data,
  missing = "fiml",
  estimator = "MLR"
)

# Extract and print the correlation between intercepts
summary(fit_att_pu, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit_att_pu, 
            c("chisq","df","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr")
)
lavInspect(fit_att_pu, "cov.lv") # covariance matrics
lavInspect(fit_att_pu, "cor.lv") # correlation matrics
parameterEstimates(fit_att_pu)   # parameter estimation


# ------------------------------------------------------------------------------
#       ATT-PEOU Combination (no-growth for ATT, linear growth for PEOU) 
# ------------------------------------------------------------------------------
att_peou_combined <-"
# ATT Measurement Model 
ATT_T1 =~ l1_att*ATT1_T1 + l2_att*ATT2_T1 + l3_att*ATT4_T1
ATT_T2 =~ l1_att*ATT1_T2 + l2_att*ATT2_T2 + l3_att*ATT4_T2
ATT_T3 =~ l1_att*ATT1_T3 + l2_att*ATT2_T3 + l3_att*ATT4_T3

# Free latent variances at later time points (ATT)
ATT_T2 ~~ NA*ATT_T2
ATT_T3 ~~ NA*ATT_T3

# ATT Intercepts
ATT1_T1 ~ t1_att*1
ATT2_T1 ~ t2_att*1
ATT4_T1 ~ t4_att*1
ATT1_T2 ~ t1_att*1
ATT2_T2 ~ t2_att*1
ATT4_T2 ~ t4_att*1
ATT1_T3 ~ t1_att*1
ATT2_T3 ~ t2_att*1
ATT4_T3 ~ t4_att*1

# ATT Residual correlations
ATT1_T1 ~~ ATT1_T2 + ATT1_T3
ATT1_T2 ~~ ATT1_T3
ATT2_T1 ~~ ATT2_T2 + ATT2_T3
ATT2_T2 ~~ ATT2_T3
ATT4_T1 ~~ ATT4_T2 + ATT4_T3
ATT4_T2 ~~ ATT4_T3

# PEOU Measurement Model 
PEOU_T1 =~ l1_peou*PEOU1_T1 + l2_peou*PEOU2_T1 + l3_peou*PEOU3_T1 + l4_peou*PEOU4_T1
PEOU_T2 =~ l1_peou*PEOU1_T2 + l2_peou*PEOU2_T2 + l3_peou*PEOU3_T2 + l4_peou*PEOU4_T2
PEOU_T3 =~ l1_peou*PEOU1_T3 + l2_peou*PEOU2_T3 + l3_peou*PEOU3_T3 + l4_peou*PEOU4_T3

# Free latent variances at later time points (PEOU)
PEOU_T2 ~~ NA*PEOU_T2
PEOU_T3 ~~ NA*PEOU_T3

# PEOU Intercepts 
PEOU1_T1 ~ t1_peou*1
PEOU2_T1 ~ t2_peou*1
PEOU3_T1 ~ t3_peou*1
PEOU4_T1 ~ t4_peou*1
PEOU1_T2 ~ t1_peou*1
PEOU2_T2 ~ t2_peou*1
PEOU3_T2 ~ t3_peou*1
PEOU4_T2 ~ t4_peou*1
PEOU1_T3 ~ t1_peou*1
PEOU2_T3 ~ t2_peou*1  
PEOU3_T3 ~ t3_peou*1
PEOU4_T3 ~ t4_peou*1  

# PEOU Residual correlations
PEOU1_T1 ~~ PEOU1_T2 + PEOU1_T3
PEOU1_T2 ~~ PEOU1_T3
PEOU2_T1 ~~ PEOU2_T2 + PEOU2_T3
PEOU2_T2 ~~ PEOU2_T3
PEOU3_T1 ~~ PEOU3_T2 + PEOU3_T3
PEOU3_T2 ~~ PEOU3_T3
PEOU4_T1 ~~ PEOU4_T2 + PEOU4_T3
PEOU4_T2 ~~ PEOU4_T3

# ATT Growth Model (No-Growth)
i_att =~ 1*ATT_T1 + 1*ATT_T2 + 1*ATT_T3

# PEOU Growth Model (Linear Growth)
i_peou =~ 1*PEOU_T1 + 1*PEOU_T2 + 1*PEOU_T3
s_peou =~ 0*PEOU_T1 + 1*PEOU_T2 + 2*PEOU_T3

# Growth Factor Means
i_att ~ 1
i_peou ~ 1
s_peou ~ 1

# Growth Factor Variances
i_att ~~ i_att
i_peou ~~ i_peou
s_peou ~~ 0.001*s_peou # since it was close to zero or negativ so it fixed to 0.001

# Growth Factor Covariances (key correlations we are interested in)
i_att ~~ i_peou  # correlation between intercepts
i_att ~~ s_peou  # correlation between ATT level and PEOU change
i_peou ~~ s_peou  # correlation between PEOU level and PEOU change
"

fit_att_peou <- growth(
  att_peou_combined,
  data = New_data,
  missing = "fiml",
  estimator = "MLR"
)

# Extract and print the correlations
summary(fit_att_peou,fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit_att_peou, 
            c("chisq","df","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr")
)
lavInspect(fit_att_peou, "cov.lv") # covariance matrics
lavInspect(fit_att_peou, "cor.lv") # Correlation matrics
parameterEstimates(fit_att_peou)

# ------------------------------------------------------------------------------
#         PU-PEOU Combination (no-growth for PU, linear growth for PEOU)
# ------------------------------------------------------------------------------
pu_peou_combined <- "
  # PU Measurement Model (Strong Invariance)
  PU_T1 =~ l1_pu*PU1_T1 + l2_pu*PU2_T1 + l3_pu*PU3_T1 + l4_pu*PU4_T1
  PU_T2 =~ l1_pu*PU1_T2 + l2_pu*PU2_T2 + l3_pu*PU3_T2 + l4_pu*PU4_T2
  PU_T3 =~ l1_pu*PU1_T3 + l2_pu*PU2_T3 + l3_pu*PU3_T3 + l4_pu*PU4_T3
  
  # Free latent variances at later time points (PU)
  PU_T2 ~~ NA*PU_T2
  PU_T3 ~~ NA*PU_T3
  
  # PU Intercepts
  PU1_T1 ~ t1_pu*1
  PU2_T1 ~ t2_pu*1
  PU3_T1 ~ t3_pu*1
  PU4_T1 ~ t4_pu*1
  PU1_T2 ~ t1_pu*1
  PU2_T2 ~ t2_pu*1
  PU3_T2 ~ t3_pu*1
  PU4_T2 ~ t4_pu*1
  PU1_T3 ~ t1_pu*1
  PU2_T3 ~ t2_pu*1
  PU3_T3 ~ t3_pu*1
  PU4_T3 ~ t4_pu*1
  
  # PU Residual correlations
  PU1_T1 ~~ PU1_T2 + PU1_T3
  PU1_T2 ~~ PU1_T3
  PU2_T1 ~~ PU2_T2 + PU2_T3
  PU2_T2 ~~ PU2_T3
  PU3_T1 ~~ PU3_T2 + PU3_T3
  PU3_T2 ~~ PU3_T3
  PU4_T1 ~~ PU4_T2 + PU4_T3
  PU4_T2 ~~ PU4_T3
  
  # PEOU Measurement Model 
  PEOU_T1 =~ l1_peou*PEOU1_T1 + l2_peou*PEOU2_T1 + l3_peou*PEOU3_T1 + l4_peou*PEOU4_T1
  PEOU_T2 =~ l1_peou*PEOU1_T2 + l2_peou*PEOU2_T2 + l3_peou*PEOU3_T2 + l4_peou*PEOU4_T2
  PEOU_T3 =~ l1_peou*PEOU1_T3 + l2_peou*PEOU2_T3 + l3_peou*PEOU3_T3 + l4_peou*PEOU4_T3
  
  # Free latent variances at later time points (PEOU)
  PEOU_T2 ~~ NA*PEOU_T2
  PEOU_T3 ~~ NA*PEOU_T3
  
  # PEOU Intercepts 
  PEOU1_T1 ~ t1_peou*1
  PEOU2_T1 ~ t2_peou*1
  PEOU3_T1 ~ t3_peou*1
  PEOU4_T1 ~ t4_peou*1
  PEOU1_T2 ~ t1_peou*1
  PEOU2_T2 ~ t2_peou*1
  PEOU3_T2 ~ t3_peou*1
  PEOU4_T2 ~ t4_peou*1
  PEOU1_T3 ~ t1_peou*1
  PEOU2_T3 ~ t2_peou*1  
  PEOU3_T3 ~ t3_peou*1
  PEOU4_T3 ~ t4_peou*1   
  
  # PEOU Residual correlations
  PEOU1_T1 ~~ PEOU1_T2 + PEOU1_T3
  PEOU1_T2 ~~ PEOU1_T3
  PEOU2_T1 ~~ PEOU2_T2 + PEOU2_T3
  PEOU2_T2 ~~ PEOU2_T3
  PEOU3_T1 ~~ PEOU3_T2 + PEOU3_T3
  PEOU3_T2 ~~ PEOU3_T3
  PEOU4_T1 ~~ PEOU4_T2 + PEOU4_T3
  PEOU4_T2 ~~ PEOU4_T3
  
  # PU Growth Model (No-Growth)
  i_pu =~ 1*PU_T1 + 1*PU_T2 + 1*PU_T3
  
  # PEOU Growth Model (Linear Growth)
  i_peou =~ 1*PEOU_T1 + 1*PEOU_T2 + 1*PEOU_T3
  s_peou =~ 0*PEOU_T1 + 1*PEOU_T2 + 2*PEOU_T3
  
  # Growth Factor Means
  i_pu ~ 1
  i_peou ~ 1
  s_peou ~ 1
  
  # Growth Factor Variances
  i_pu ~~ i_pu
  i_peou ~~ i_peou
  s_peou ~~ s_peou
  
  # Growth Factor Covariances 
i_pu ~~ i_peou  # correlation between intercepts
i_pu ~~ s_peou  # correlation between PU level and PEOU change
i_peou ~~ s_peou  # correlation between PEOU level and PEOU change
"

fit_pu_peou <- growth(
  pu_peou_combined,
  data = New_data,
  missing = "fiml",
  estimator = "MLR"
)

# Extract and print the correlations
summary(fit_pu_peou, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit_pu_peou, 
            c("chisq","df","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr")
)
lavInspect(fit_pu_peou, "cov.lv") # covariates matrics
lavInspect(fit_pu_peou, "cor.lv") # correlation matrics
parameterEstimates(fit_pu_peou) # parameter estimations
