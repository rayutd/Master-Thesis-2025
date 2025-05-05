###############################################################################
#                     LATENT GROWTH CURVE MODELS                              #
###############################################################################
# Author: Rayad Sakyar
# Date: May 2025
# Description: This script fits latent growth curve models to the three key
#              constructs - Attitudes (ATT), Perceived Usefulness (PU), and
#              Perceived Ease of Use (PEOU). For each construct, we compare
#              various growth trajectories: no-growth, linear, quadratic, and
#              piecewise growth. We also examine the effects of covariates
#              (gender, age) on the growth factors.

# Load required packages
library(lavaan)      # For SEM and growth curve modeling
library(dplyr)       # For data manipulation

# Load prepared data and models from previous scripts
New_data <- readRDS("New_data_clean.rds")
fit_strong_att <- readRDS("fit_strong_att.rds")
fit_strong_pu <- readRDS("fit_strong_pu.rds")
fit_strong_peou <- readRDS("fit_strong_peou.rds")


###############################################################################
#                  ATTITUDES TOWARD TECHNOLOGY (ATT)                          #
###############################################################################
#-------------------------------------------------------------------------------
#                 a. No-Growth (Intercept-Only) for ATT
#-------------------------------------------------------------------------------
att_no_growth_att <- "
  ## (A) strong invariance
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


  ATT1_T1 ~~ ATT1_T2 + ATT1_T3
  ATT1_T2 ~~ ATT1_T3
  ATT2_T1 ~~ ATT2_T2 + ATT2_T3
  ATT2_T2 ~~ ATT2_T3
  ATT4_T1 ~~ ATT4_T2 + ATT4_T3
  ATT4_T2 ~~ ATT4_T3



  ## (B) NO-GROWTH GCM
  i_att =~ 1*ATT_T1 + 1*ATT_T2 + 1*ATT_T3
  # slope factor is turned off
  s_att =~ 0*ATT_T1 + 0*ATT_T2 + 0*ATT_T3

  # Means:
  i_att ~ 1
  s_att ~ 0  # slope mean forced to 0

  # Variances:
  i_att ~~ i_att
  s_att ~~ 0*s_att
  i_att ~~ 0*s_att
  
"

fit_no_growth_att <- growth(
  att_no_growth_att,
  data      = New_data,
  missing   = "fiml",
  estimator = "MLR"
)
summary(fit_no_growth_att, fit.measures=TRUE, standardized=TRUE)

#-------------------------------------------------------------------------------
#                      b: Linear Growth for ATT
#-------------------------------------------------------------------------------
att_linear_att <- "
  ## (A) strong invariance
  ATT_T1 =~ l1*ATT1_T1 + l2*ATT2_T1 + l3*ATT4_T1
  ATT_T2 =~ l1*ATT1_T2 + l2*ATT2_T2 + l3*ATT4_T2
  ATT_T3 =~ l1*ATT1_T3 + l2*ATT2_T3 + l3*ATT4_T3
  
  # Free latent variances at later time points
  ATT_T2 ~~ NA*ATT_T2
  ATT_T3 ~~ NA*ATT_T3
  
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


  ATT1_T1 ~~ ATT1_T2 + ATT1_T3
  ATT1_T2 ~~ ATT1_T3
  ATT2_T1 ~~ ATT2_T2 + ATT2_T3
  ATT2_T2 ~~ ATT2_T3
  ATT4_T1 ~~ ATT4_T2 + ATT4_T3
  ATT4_T2 ~~ ATT4_T3


  ## (B) LINEAR GROWTH
  i_att =~ 1*ATT_T1 + 1*ATT_T2 + 1*ATT_T3
  s_att =~ 0*ATT_T1 + 1*ATT_T2 + 2*ATT_T3

  # Means
  i_att ~ 1
  s_att ~ 1

  # Variances/covariances
  i_att ~~ i_att
  s_att ~~ s_att
  i_att ~~ s_att
"

fit_linear_att <- growth(
  att_linear_att,
  data      = New_data,
  missing   = "fiml",
  estimator = "MLR"
)
summary(fit_linear_att, fit.measures=TRUE, standardized=TRUE)


#-------------------------------------------------------------------------------
#                     c: Quadratic Growth 
#-------------------------------------------------------------------------------
att_quadratic_att <- "
## (A) strong invariance
  ATT_T1 =~ l1*ATT1_T1 + l2*ATT2_T1 + l3*ATT4_T1
  ATT_T2 =~ l1*ATT1_T2 + l2*ATT2_T2 + l3*ATT4_T2
  ATT_T3 =~ l1*ATT1_T3 + l2*ATT2_T3 + l3*ATT4_T3
  
  # Free latent variances at later time points
  ATT_T2 ~~ NA*ATT_T2
  ATT_T3 ~~ NA*ATT_T3
  
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


  ATT1_T1 ~~ ATT1_T2 + ATT1_T3
  ATT1_T2 ~~ ATT1_T3
  ATT2_T1 ~~ ATT2_T2 + ATT2_T3
  ATT2_T2 ~~ ATT2_T3
  ATT4_T1 ~~ ATT4_T2 + ATT4_T3
  ATT4_T2 ~~ ATT4_T3


  # (B) QUADRATIC
  i_att =~ 1*ATT_T1 + 1*ATT_T2 + 1*ATT_T3
  s1_att =~ 0*ATT_T1 + 1*ATT_T2 + 2*ATT_T3
  s2_att =~ 0*ATT_T1 + 1*ATT_T2 + 4*ATT_T3   

  i_att ~ 1
  s1_att ~ 1
  s2_att ~ 1

  i_att ~~ i_att
  s1_att ~~ s1_att
  s2_att ~~ s2_att
  i_att ~~ s1_att + s2_att
  s1_att ~~ s2_att
"

fit_quadratic_att <- growth(
  att_quadratic_att,
  data = New_data,
  missing = "fiml",
  estimator = "MLR"
)
summary(fit_quadratic_att, fit.measures=TRUE, standardized=TRUE)

#-------------------------------------------------------------------------------
#                     d: Piecewise Growth  
#-------------------------------------------------------------------------------
att_piecewise <- "
  ## (A) Strong invariance lines
  ATT_T1 =~ l1*ATT1_T1 + l2*ATT2_T1 + l3*ATT4_T1
  ATT_T2 =~ l1*ATT1_T2 + l2*ATT2_T2 + l3*ATT4_T2
  ATT_T3 =~ l1*ATT1_T3 + l2*ATT2_T3 + l3*ATT4_T3
  
  # Free latent variances at later time points
  ATT_T2 ~~ NA*ATT_T2
  ATT_T3 ~~ NA*ATT_T3
  
  ## constrain Intercepts
  ATT1_T1 ~ t1*1
  ATT1_T2 ~ t1*1
  ATT1_T3 ~ t1*1
  
  ATT2_T1 ~ t2*1
  ATT2_T2 ~ t2*1
  ATT2_T3 ~ t2*1
  
  ATT4_T1 ~ t4*1
  ATT4_T2 ~ t4*1
  ATT4_T3 ~ t4*1


  ATT1_T1 ~~ ATT1_T2 + ATT1_T3
  ATT1_T2 ~~ ATT1_T3
  ATT2_T1 ~~ ATT2_T2 + ATT2_T3
  ATT2_T2 ~~ ATT2_T3
  ATT4_T1 ~~ ATT4_T2 + ATT4_T3
  ATT4_T2 ~~ ATT4_T3

  ## (B) PIECEWISE
  i_att =~ 1*ATT_T1 + 1*ATT_T2 + 1*ATT_T3
  s1_att =~ 0*ATT_T1 + 1*ATT_T2 + 1*ATT_T3  # slope for T1->T2, but T3 also loads =1
  s2_att =~ 0*ATT_T1 + 0*ATT_T2 + 1*ATT_T3  # slope from T2->T3 specifically

  i_att ~ 1
  s1_att ~ 1
  s2_att ~ 1

  i_att ~~ i_att
  s1_att ~~ s1_att
  s2_att ~~ s2_att
  i_att ~~ 0.001*s1_att + s2_att
  s1_att ~~ s2_att
"

fit_piecewise_att <- growth(
  att_piecewise,
  data = New_data,
  missing = "fiml",
  estimator = "MLR"
)
summary(fit_piecewise_att, fit.measures=TRUE, standardized=TRUE)

# Quick fit measure check
fitMeasures(fit_no_growth_att, c("cfi","tli","rmsea","srmr","aic","bic"))
fitMeasures(fit_linear_att,   c("cfi","tli","rmsea","srmr","aic","bic"))
fitMeasures(fit_quadratic_att, c("cfi","tli","rmsea","srmr","aic","bic"))
fitMeasures(fit_piecewise_att, c("cfi","tli","rmsea","srmr","aic","bic"))

# Comparing models
anova(fit_no_growth_att, fit_linear_att,
      fit_quadratic_att,fit_piecewise_att)
lavTestLRT(fit_no_growth_att, fit_linear_att)
lavTestLRT(fit_no_growth_att, fit_quadratic_att)
lavTestLRT(fit_no_growth_att, fit_piecewise_att)
parameterEstimates(fit_no_growth_att, standardized = TRUE)


###############################################################################
#                     PERCEIVED USEFULNESS (PU)                               #
###############################################################################
#-------------------------------------------------------------------------------
#               PU  a: No-Growth (Intercept-Only)
#-------------------------------------------------------------------------------
pu_no_growth <- "
  # Strong invariance part
  PU_T1 =~ l1*PU1_T1 + l2*PU2_T1 + l3*PU3_T1 + l4*PU4_T1
  PU_T2 =~ l1*PU1_T2 + l2*PU2_T2 + l3*PU3_T2 + l4*PU4_T2
  PU_T3 =~ l1*PU1_T3 + l2*PU2_T3 + l3*PU3_T3 + l4*PU4_T3
  
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

  # No-growth model
  i_pu =~ 1*PU_T1 + 1*PU_T2 + 1*PU_T3
  s_pu =~ 0*PU_T1 + 0*PU_T2 + 0*PU_T3
  
  # mean
  i_pu ~ 1 # intercept factor mean
  s_pu ~ 0 # slope mean forced to 0
  
  # variance
  i_pu ~~ i_pu # intercept variance
  s_pu ~~ 0*s_pu
  i_pu~~0*s_pu
  
"

fit_no_growth_pu <- growth(
  pu_no_growth,
  data = New_data,
  missing = "fiml",
  estimator = "MLR"
)
summary(fit_no_growth_pu, fit.measures=TRUE, standardized=TRUE)
# fitmeasures(fit_no_growth_pu, c("cfi", "rmsea", "srmr")) 

#-------------------------------------------------------------------------------
#                        PU  b: Linear Growth 
#-------------------------------------------------------------------------------
pu_linear <- "
  # Strong invariance part
  PU_T1 =~ L1*PU1_T1 + L2*PU2_T1 + L3*PU3_T1 + L4*PU4_T1
  PU_T2 =~ L1*PU1_T2 + L2*PU2_T2 + L3*PU3_T2 + L4*PU4_T2
  PU_T3 =~ L1*PU1_T3 + L2*PU2_T3 + L3*PU3_T3 + L4*PU4_T3
  
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

  # Linear growth
  i_pu =~ 1*PU_T1 + 1*PU_T2 + 1*PU_T3
  s_pu =~ 0*PU_T1 + 1*PU_T2 + 2*PU_T3
  
  # mean
  i_pu ~ 1
  s_pu ~ 1
  
  # variance/covariance
  i_pu ~~ i_pu
  s_pu ~~ s_pu  # Fixed to small positive value
  i_pu ~~ s_pu
"

fit_linear_pu <- growth(
  pu_linear,
  data = New_data,
  missing = "fiml",
  estimator = "MLR"
)
summary(fit_linear_pu, fit.measures=TRUE, standardized=TRUE)

#-------------------------------------------------------------------------------
#                          PU c: Quadratic Growth 
#-------------------------------------------------------------------------------
pu_quadratic <- "
  # Strong invariance part
  PU_T1 =~ L1*PU1_T1 + L2*PU2_T1 + L3*PU3_T1 + L4*PU4_T1
  PU_T2 =~ L1*PU1_T2 + L2*PU2_T2 + L3*PU3_T2 + L4*PU4_T2
  PU_T3 =~ L1*PU1_T3 + L2*PU2_T3 + L3*PU3_T3 + L4*PU4_T3
  
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

  # Quadratic growth
  i_pu =~ 1*PU_T1 + 1*PU_T2 + 1*PU_T3
  s_pu =~ 0*PU_T1 + 1*PU_T2 + 2*PU_T3
  q_pu =~ 0*PU_T1 + 1*PU_T2 + 4*PU_T3

  i_pu ~ 1
  s_pu ~ 1
  q_pu ~ 1
  i_pu ~~ i_pu + s_pu + q_pu
  s_pu ~~ s_pu + q_pu
  q_pu ~~ q_pu
"
fit_quadratic_pu <- growth(
  pu_quadratic,
  data = New_data,
  missing = "fiml",
  estimator = "MLR"
)
summary(fit_quadratic_pu, fit.measures=TRUE, standardized=TRUE)

#-------------------------------------------------------------------------------
#            PU d: Piecewise Growth (Split at T2) 
#-------------------------------------------------------------------------------
pu_piecewise <- "
  # Strong invariance part
  PU_T1 =~ L1*PU1_T1 + L2*PU2_T1 + L3*PU3_T1 + L4*PU4_T1
  PU_T2 =~ L1*PU1_T2 + L2*PU2_T2 + L3*PU3_T2 + L4*PU4_T2
  PU_T3 =~ L1*PU1_T3 + L2*PU2_T3 + L3*PU3_T3 + L4*PU4_T3
  
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

  # Piecewise growth
  i_pu =~ 1*PU_T1 + 1*PU_T2 + 1*PU_T3
  s_pu1 =~ 0*PU_T1 + 1*PU_T2 + 1*PU_T3
  s_pu2 =~ 0*PU_T1 + 0*PU_T2 + 1*PU_T3
  
  # mean
  i_pu ~ 1
  s_pu1 ~ 1
  s_pu2 ~ 1
  
  # co/variance
  i_pu ~~ i_pu
  s_pu1 ~~ 0.01*s_pu1
  s_pu2 ~~ 0.01*s_pu2
  
  i_pu ~~ s_pu1
  i_pu ~~ s_pu2
  s_pu1 ~~ 0*s_pu2  # Fixed to zero for identification
"
fit_piecewise_pu <- growth(
  pu_piecewise,
  data = New_data,
  missing = "fiml",
  estimator = "MLR"
)
summary(fit_piecewise_pu, fit.measures=TRUE, standardized=TRUE)

# Quick fit measure check
fitMeasures(fit_no_growth_pu, c("chisq","cfi","tli","rmsea","srmr","aic","bic"))
fitMeasures(fit_linear_pu,   c("chisq","cfi","tli","rmsea","srmr","aic","bic"))
fitMeasures(fit_quadratic_pu, c("chisq","cfi","tli","rmsea","srmr","aic","bic"))
fitMeasures(fit_piecewise_pu, c("chisq","cfi","tli","rmsea","srmr","aic","bic"))

anova(fit_no_growth_pu, fit_linear_pu,
      fit_quadratic_pu,fit_piecewise_pu)

lavTestLRT(fit_no_growth_pu, fit_linear_pu)
lavTestLRT(fit_no_growth_pu, fit_quadratic_pu)
lavTestLRT(fit_no_growth_pu, fit_piecewise_pu)
# parameterEstimates(fit_no_growth_pu, standardized = TRUE)


###############################################################################
#                    PERCEIVED EASE OF USE (PEOU)                             #
###############################################################################
#-------------------------------------------------------------------------------
#                    a: No-Growth (Intercept-Only) 
#-------------------------------------------------------------------------------
peou_no_growth <- "
  # P-Strong invariance constraints
  PEOU_T1 =~ l1*PEOU1_T1 + l2*PEOU2_T1 + l3*PEOU3_T1 + l4*PEOU4_T1
  PEOU_T2 =~ l1*PEOU1_T2 + l2*PEOU2_T2 + l3*PEOU3_T2 + l4*PEOU4_T2
  PEOU_T3 =~ l1*PEOU1_T3 + l2*PEOU2_T3 + l3*PEOU3_T3 + l4*PEOU4_T3
  
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
  PEOU1_T1 ~~ PEOU1_T2
  PEOU1_T1 ~~ PEOU1_T3
  PEOU1_T2 ~~ PEOU1_T3
  
  PEOU2_T1 ~~ PEOU2_T2
  PEOU2_T1 ~~ PEOU2_T3
  PEOU2_T2 ~~ PEOU2_T3
  
  PEOU3_T1 ~~ PEOU3_T2
  PEOU3_T1 ~~ PEOU3_T3
  PEOU3_T2 ~~ PEOU3_T3
  
  PEOU4_T1 ~~ PEOU4_T2
  PEOU4_T1 ~~ PEOU4_T3
  PEOU4_T2 ~~ PEOU4_T3
  
  # Model latent means
  PEOU_T2 ~ NA*1
  PEOU_T3 ~ NA*1
 
  # No-growth model 
  i_peou =~ 1*PEOU_T1 + 1*PEOU_T2 + 1*PEOU_T3
  
  i_peou ~ 1
  i_peou ~~ i_peou
  
  # Fix first-order factor residual variances to zero
  PEOU_T1 ~~ 0*PEOU_T1
  PEOU_T2 ~~ 0*PEOU_T2
  PEOU_T3 ~~ 0*PEOU_T3
"
fit_no_growth_peou <- growth(peou_no_growth, data = New_data, missing = "fiml", estimator = "MLR")
summary(fit_no_growth_peou, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit_no_growth_peou, c("cfi", "tli", "rmsea", "srmr"))

#-------------------------------------------------------------------------------
#                         b: Linear Growth
#-------------------------------------------------------------------------------
peou_linear <- "
  # P-Strong invariance constraints
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
  PEOU1_T1 ~~ PEOU1_T2
  PEOU1_T1 ~~ PEOU1_T3
  PEOU1_T2 ~~ PEOU1_T3
  
  PEOU2_T1 ~~ PEOU2_T2
  PEOU2_T1 ~~ PEOU2_T3
  PEOU2_T2 ~~ PEOU2_T3
  
  PEOU3_T1 ~~ PEOU3_T2
  PEOU3_T1 ~~ PEOU3_T3
  PEOU3_T2 ~~ PEOU3_T3
  
  PEOU4_T1 ~~ PEOU4_T2
  PEOU4_T1 ~~ PEOU4_T3
  PEOU4_T2 ~~ PEOU4_T3
  

  
  # Linear growth
  i_peou =~ 1*PEOU_T1 + 1*PEOU_T2 + 1*PEOU_T3
  s_peou =~ 0*PEOU_T1 + 1*PEOU_T2 + 2*PEOU_T3
  i_peou ~ 1
  s_peou ~ 1
  i_peou ~~ i_peou + s_peou
  s_peou ~~ s_peou 
  
"
fit_linear_peou <- growth(peou_linear, data = New_data, missing = "fiml", estimator = "MLR")
summary(fit_linear_peou, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit_linear_peou, c("cfi", "tli", "rmsea", "srmr"))

#-------------------------------------------------------------------------------
#                          c: quadratics peou
#-------------------------------------------------------------------------------
peou_quadratic <- "
 # P-Strong invariance constraints
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
  PEOU1_T1 ~~ PEOU1_T2
  PEOU1_T1 ~~ PEOU1_T3
  PEOU1_T2 ~~ PEOU1_T3
  
  PEOU2_T1 ~~ PEOU2_T2
  PEOU2_T1 ~~ PEOU2_T3
  PEOU2_T2 ~~ PEOU2_T3
  
  PEOU3_T1 ~~ PEOU3_T2
  PEOU3_T1 ~~ PEOU3_T3
  PEOU3_T2 ~~ PEOU3_T3
  
  PEOU4_T1 ~~ PEOU4_T2
  PEOU4_T1 ~~ PEOU4_T3
  PEOU4_T2 ~~ PEOU4_T3
  
  
  
  # Quadratic growth
  i_peou =~ 1*PEOU_T1 + 1*PEOU_T2 + 1*PEOU_T3
  s1_peou =~ 0*PEOU_T1 + 1*PEOU_T2 + 2*PEOU_T3
  s2_peou =~ 0*PEOU_T1 + 1*PEOU_T2 + 4*PEOU_T3
 
 
  i_peou ~ 1
  s1_peou ~ 1
  s2_peou ~ 1
  i_peou ~~ i_peou
  i_peou ~~ 0.001*s1_peou # high covariance
  i_peou ~~ s2_peou
  s1_peou ~~ s1_peou  
  s1_peou ~~ s2_peou 
  s2_peou ~~ s2_peou  

"

fit_quadratic_peou <- growth(peou_quadratic, data = New_data, missing = "fiml", estimator = "MLR")
summary(fit_quadratic_peou, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit_quadratic_peou, c("cfi", "tli", "rmsea", "srmr"))


#-------------------------------------------------------------------------------
#                             peou 4d: piecewise peou
#-------------------------------------------------------------------------------
peou_piecewise <- "
# P-Strong invariance constraints
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
  PEOU1_T1 ~~ PEOU1_T2
  PEOU1_T1 ~~ PEOU1_T3
  PEOU1_T2 ~~ PEOU1_T3
  
  PEOU2_T1 ~~ PEOU2_T2
  PEOU2_T1 ~~ PEOU2_T3
  PEOU2_T2 ~~ PEOU2_T3
  
  PEOU3_T1 ~~ PEOU3_T2
  PEOU3_T1 ~~ PEOU3_T3
  PEOU3_T2 ~~ PEOU3_T3
  
  PEOU4_T1 ~~ PEOU4_T2
  PEOU4_T1 ~~ PEOU4_T3
  PEOU4_T2 ~~ PEOU4_T3
  
  
  # Piecewise growth
  i_peou =~ 1*PEOU_T1 + 1*PEOU_T2 + 1*PEOU_T3
  s1_peou =~ 0*PEOU_T1 + 1*PEOU_T2 + 1*PEOU_T3  # T1-T2 slope
  s2_peou =~ 0*PEOU_T1 + 0*PEOU_T2 + 1*PEOU_T3  # T2-T3 slope
  
  i_peou ~ 1
  s1_peou ~ 1
  s2_peou ~ 1
  
  i_peou ~~ i_peou
  i_peou ~~ s1_peou
  i_peou ~~ s2_peou
  s1_peou ~~ 0.01*s1_peou  # Fix to small positive value
  s1_peou ~~ 0*s2_peou     # Considered forcing these to be uncorrelated
  s2_peou ~~ 0.01*s2_peou  # Fix to small positive value
"
fit_piecewise_peou <- growth(peou_piecewise, data = New_data, missing = "fiml", estimator = "MLR")
summary(fit_piecewise_peou, fit.measures = TRUE, standardized = TRUE)

# quick check of models indices 
fitMeasures(fit_no_growth_peou, c("chisq","cfi","tli","rmsea","srmr","aic","bic"))
fitMeasures(fit_linear_peou, c("chisq","cfi","tli","rmsea","srmr","aic","bic"))
fitMeasures(fit_quadratic_peou, c("chisq","cfi","tli","rmsea","srmr","aic","bic"))
fitMeasures(fit_piecewise_peou, c("chisq","cfi","tli","rmsea","srmr","aic","bic"))

# Likelihood ratio tests 
lavTestLRT(fit_no_growth_peou, fit_linear_peou)
lavTestLRT(fit_linear_peou, fit_quadratic_peou)
lavTestLRT(fit_linear_peou, fit_piecewise_peou)
# parameterEstimates(fit_linear_peou, standardized = TRUE)




###############################################################################
#                    LGCM with Covariates( Gender + Age)                            #
###############################################################################
#-------------------------------------------------------------------------------
#            No-Growth Model with Covariates (Gender + Age) for ATT
#-------------------------------------------------------------------------------
# Step 1: Creating centered age variables and the quadratic term
age_mean <- mean(New_data$AGE_T3, na.rm=TRUE)
New_data$AGE_centered <- New_data$AGE_T3 - age_mean   # choosing AGE_T3 since have less NAs
New_data$AGE_squared <- New_data$AGE_centered^2 

# Step 2: Checking the transformations
summary(New_data$AGE_centered)
summary(New_data$AGE_squared)
cor(New_data$AGE_centered, New_data$AGE_squared, use="complete.obs")


att_no_growth_cov <- "
  # Strong Measurement Invariance
  ATT_T1 =~ l1*ATT1_T1 + l2*ATT2_T1 + l3*ATT4_T1
  ATT_T2 =~ l1*ATT1_T2 + l2*ATT2_T2 + l3*ATT4_T2
  ATT_T3 =~ l1*ATT1_T3 + l2*ATT2_T3 + l3*ATT4_T3

  # Equal Intercepts
  ATT1_T1 ~ t1*1
  ATT2_T1 ~ t2*1
  ATT4_T1 ~ t4*1
  ATT1_T2 ~ t1*1
  ATT2_T2 ~ t2*1
  ATT4_T2 ~ t4*1
  ATT1_T3 ~ t1*1
  ATT2_T3 ~ t2*1
  ATT4_T3 ~ t4*1

  # Residual Correlations
  ATT1_T1 ~~ ATT1_T2 + ATT1_T3
  ATT1_T2 ~~ ATT1_T3
  ATT2_T1 ~~ ATT2_T2 + ATT2_T3
  ATT2_T2 ~~ ATT2_T3
  ATT4_T1 ~~ ATT4_T2 + ATT4_T3
  ATT4_T2 ~~ ATT4_T3

  # No-Growth Model with Covariates
  i_att =~ 1*ATT_T1 + 1*ATT_T2 + 1*ATT_T3
  s_att =~ 0*ATT_T1 + 0*ATT_T2 + 0*ATT_T3
  
  # Covariates predict intercept
  i_att ~ GENDER + AGE_centered + AGE_squared
  
  # Means of covariates
  AGE_centered ~ 1
  AGE_squared ~ 1
  GENDER ~ 1
  
  # Correlation between continuous covariates
  AGE_centered ~~ AGE_squared
  
  # Constraints
  s_att ~ 0
  i_att ~~ i_att
  s_att ~~ 0*s_att
  i_att ~~ 0*s_att
"

# Fit the model
fit_no_growth_att_cov <- growth(
  att_no_growth_cov,
  data = New_data,
  missing = "fiml",
  estimator = "MLR",
  fixed.x = FALSE
)

summary(fit_no_growth_att_cov, 
        fit.measures = TRUE, 
        standardized = TRUE)
fitMeasures(fit_no_growth_att_cov, c("cfi", "tli", "rmsea","srmr"))

modindices(fit_no_growth_att_cov)
fitMeasures(fit_no_growth_att_cov, c("cfi","tli", "rmsea", "srmr"))
lavTestLRT(fit_no_growth_att_cov, fit_no_growth_att)


#-------------------------------------------------------------------------------
#           No-Growth by adding covariates, gender + age for PU
#-------------------------------------------------------------------------------
pu_no_growth_cov <- "
  # Strong invariance 
  PU_T1 =~ l1*PU1_T1 + l2*PU2_T1 + l3*PU3_T1 + l4*PU4_T1
  PU_T2 =~ l1*PU1_T2 + l2*PU2_T2 + l3*PU3_T2 + l4*PU4_T2
  PU_T3 =~ l1*PU1_T3 + l2*PU2_T3 + l3*PU3_T3 + l4*PU4_T3
  
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

  # No-growth model
  i_pu =~ 1*PU_T1 + 1*PU_T2 + 1*PU_T3
  s_pu =~ 0*PU_T1 + 0*PU_T2 + 0*PU_T3
  
  # Covariates predict intercept
  i_pu ~ GENDER + AGE_centered + AGE_squared
  
  # Means of covariates
  AGE_centered ~ 1
  AGE_squared ~ 1
  GENDER ~ 1
  
  # Correlation between continuous covariates
  AGE_centered ~~ AGE_squared
  
  # constraints
  s_pu ~ 0
  i_pu ~~ i_pu
  s_pu ~~ 0*s_pu
  i_pu ~~ 0*s_pu
"

# Fit the model
fit_no_growth_pu_cov <- growth(
  pu_no_growth_cov,
  data = New_data,
  missing = "fiml",
  estimator = "MLR",
  fixed.x = FALSE
)


summary(fit_no_growth_pu_cov, fit.measures=TRUE, standardized=TRUE)
modindices(fit_no_growth_pu_cov)
fitMeasures(fit_no_growth_pu_cov, c("cfi","tli", "rmsea", "srmr"))
lavTestLRT(fit_no_growth_pu_cov, fit_no_growth_pu)


#-------------------------------------------------------------------------------
#             Linear-Growth by adding covariates, gender + age for peou 
#-------------------------------------------------------------------------------

peou_linear_gnder_age <- "
  # P-Strong invariance constraints
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
  PEOU1_T1 ~~ PEOU1_T2
  PEOU1_T1 ~~ PEOU1_T3
  PEOU1_T2 ~~ PEOU1_T3
  
  PEOU2_T1 ~~ PEOU2_T2
  PEOU2_T1 ~~ PEOU2_T3
  PEOU2_T2 ~~ PEOU2_T3
  
  PEOU3_T1 ~~ PEOU3_T2
  PEOU3_T1 ~~ PEOU3_T3
  PEOU3_T2 ~~ PEOU3_T3
  
  PEOU4_T1 ~~ PEOU4_T2
  PEOU4_T1 ~~ PEOU4_T3
  PEOU4_T2 ~~ PEOU4_T3
  

  # Linear Growth Model with Covariates
  i_peou =~ 1*PEOU_T1 + 1*PEOU_T2 + 1*PEOU_T3
  s_peou =~ 0*PEOU_T1 + 1*PEOU_T2 + 2*PEOU_T3
  
  # Latent means
  i_peou ~ 1
  s_peou ~ 1

  # Variance/Covariance Structure
  i_peou ~~ i_peou
  s_peou ~~ 0.001*s_peou
  i_peou ~~ s_peou
  
  
  # Covariates predict both intercept and slope
  i_peou ~ GENDER + AGE_centered + AGE_squared
  s_peou ~ GENDER + AGE_centered + AGE_squared
  
  # Means of covariates
  AGE_centered ~1
  AGE_squared ~ 1
  GENDER ~ 1
  
  # Correlation between continuous covariates
  AGE_centered  ~~  AGE_squared
 
 "

fit_linear_peou_cov <- growth(
  peou_linear_gnder_age,
  data = New_data,
  missing = "fiml",
  estimator = "MLR", 
  fixed.x=FALSE
)


summary(fit_linear_peou_cov, 
        fit.measures = TRUE, 
        standardized = TRUE)

fitMeasures(fit_linear_peou_cov,c("cfi","tli","rmsea","srmr"))
modindices(fit_linear_peou_cov)

# Compare with model without covariates
lavTestLRT(fit_linear_peou_cov, fit_linear_peou)













