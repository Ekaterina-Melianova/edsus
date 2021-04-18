# tch-quality_sch-envir1.3b.R

# Teaching Styles, Teaching Quality, School Environmemt, Students' Beliefs, and Learning Outcomes
# Mediation modelling

# WELLNESS

# Libraries
library(foreign)
library(dplyr)
library(tidyr)
library(tidyverse)
library(magrittr)
library(lavaan)
library(Amelia)
library(mice)
#install.packages("https://cran.r-project.org/bin/windows/contrib/4.1/OpenMx_2.18.1.zip", repos = NULL)
library(semPlot)
library(tidySEM)
library(stargazer)
library(GPArotation)
library(semTools)
library(hrbrthemes)
# t.tests weighted
library(weights)

Sys.setlocale("LC_CTYPE", "russian")


# -----------------------------------------------------------------------------------------------------------------#
# Modelling
# -----------------------------------------------------------------------------------------------------------------#

## CFA

# Model
cfa <- '

# Teacher_Quality

Math_Teacher_Understandable =~ BSBM17B + BSBM17C + BSBM17D + BSBM17E + BSBM17G;
Math_Teacher_Oderliness =~ BSBM18A + BSBM18B + BSBM18C + BSBM18D + BSBM18E + BSBM18F;

# Student_Attitudes

Math_Important =~ BSBM20A + BSBM20C + BSBM20D + BSBM20E + BSBM20F + BSBM20G + BSBM20I;
Math_Enjoy =~ BSBM16A + BSBM16D + BSBM16E + BSBM16F + BSBM16G + BSBM16H + BSBM16I
Math_Strong =~ BSBM19A + BSBM19C + BSBM19D + BSBM19F + BSBM19G

# School_Environment

Audibility =~ n15_0 + n15_1
Visibility =~ n16_0 + n16_1 + n16_2
Temperature =~ n11_2 + n12_2 
Comfort_furniture =~ n17_1 + n17_2
Safety =~ n24_0 + n24_1 + n24_2 + n24_4

# Outcomes

WELLNESS =~ BSBG13E + BSBG13B + BSBG13C + BSBG13D + BSBG13A
'

# Run Lavaan:
fit_cfa <- cfa(cfa, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT')
summary(fit_cfa, fit.measures = T, standardized = T)

# Reliability
reliability(fit_cfa)

# -----------------------------------------------------------------------------------------------------------------#
# Establishing measurement invariance
# -----------------------------------------------------------------------------------------------------------------#

#### Configural: the number of items per construct and signs of factor loadings are identical across groups
fit_cfa_configural <- cfa(cfa, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT',
                          group = 'traditional_style')
summary(fit_cfa_configural , fit.measures = T, standardized = T)

#### Metric: factor loadings are equal in magnitude across groups 
fit_cfa_metric <- cfa(cfa, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT',
                      group = 'traditional_style', group.equal = "loadings")
summary(fit_cfa_metric , fit.measures = T, standardized = T)
anova(fit_cfa_configural, fit_cfa_metric)

# Configural vs. metric model: CFI/TLI, RMSEA, and SRMR comparison
d.fitmeasures.comparison <- data.frame(cbind(fitMeasures(fit_cfa_configural, c("cfi", "rmsea", "srmr")),
                                             fitMeasures(fit_cfa_metric, c("cfi", "rmsea", "srmr"))))
colnames(d.fitmeasures.comparison) <- c("Configural", "Metric")
d.fitmeasures.comparison$diff = d.fitmeasures.comparison[,2] - d.fitmeasures.comparison[,1]
round(d.fitmeasures.comparison, digits = 5)

#### Scalar: factor loadings AND intercepts are equal across groups 
fit_cfa_scalar <- cfa(cfa, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT',
                      group = 'traditional_style', group.equal = c("loadings", 'intercepts'))
summary(fit_cfa_scalar , fit.measures = T, standardized = T)
anova(fit_cfa_scalar, fit_cfa_metric)

# Model comparison showed the significance of the chi-squared test -> the assumption of identical
# intercepts and thus strong measurement invariance must be rejected.
# However, according to AIC and BIC the strong measurement invariance model may be preferred.
# Let's investigate CFI/TLI, RMSEA, and SRMR: their difference between the models should be less than 
# 0.01, 0.015, 0.01 respectively.
# See Chen, F. F. (2007). Sensitivity of goodness of fit indexes to lack of measurement invariance.
#                         Structural equation modeling 14 (3), 464 504.

# Metric vs. scalar model: CFI/TLI, RMSEA, and SRMR comparison
d.fitmeasures.comparison_2 <- data.frame(cbind(fitMeasures(fit_cfa_metric, c("cfi", "rmsea", "srmr")),
                                               fitMeasures(fit_cfa_scalar, c("cfi","rmsea", "srmr"))))
colnames(d.fitmeasures.comparison_2) <- c("Metric", "Scalar")
d.fitmeasures.comparison_2$Diff <- d.fitmeasures.comparison_2[,2] - d.fitmeasures.comparison_2[,1]
round(d.fitmeasures.comparison_2, digits = 5)

# Total comparative fit statistics
fit.stat <- as.data.frame(cbind(round(d.fitmeasures.comparison, digits = 5),
                                round(d.fitmeasures.comparison_2, digits = 5)))
fit.stat
# The indices' differences do not exceed the cut-off values: strong scalar invariance is supported.

# -----------------------------------------------------------------------------------------------------------------#
# Measurement invariance of higher order factors
# -----------------------------------------------------------------------------------------------------------------#

# Model
cfa_higher <- paste0(cfa,'
                     Student_Attitudes =~ Math_Important + Math_Enjoy + Math_Strong
                     Conditions =~ Audibility + Visibility + Temperature
                     ')

#### Configural: the number of items per construct and signs of factor loadings are identical across groups
fit_cfa_configural_higher <- cfa(cfa_higher, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT',
                                 group = 'traditional_style')
summary(fit_cfa_configural_higher, fit.measures = T, standardized = T)

#### Metric: factor loadings are equal in magnitude across groups 
fit_cfa_metric_higher <- cfa(cfa_higher, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT',
                             group = 'traditional_style', group.equal = "loadings")
summary(fit_cfa_metric_higher, fit.measures = T, standardized = T)
anova(fit_cfa_configural_higher, fit_cfa_metric_higher)

# Configural vs. metric model: CFI/TLI, RMSEA, and SRMR comparison
d.fitmeasures.comparison_higher <- data.frame(cbind(fitMeasures(fit_cfa_configural_higher, c("cfi", "rmsea", "srmr")),
                                                    fitMeasures(fit_cfa_metric_higher, c("cfi", "rmsea", "srmr"))))
colnames(d.fitmeasures.comparison_higher) <- c("Configural", "Metric")
d.fitmeasures.comparison_higher$diff = d.fitmeasures.comparison_higher[,2] - d.fitmeasures.comparison_higher[,1]
round(d.fitmeasures.comparison_higher, digits = 5)

#### Scalar: factor loadings AND intercepts are equal across groups 
fit_cfa_scalar_higher <- cfa(cfa_higher, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT',
                             group = 'traditional_style', group.equal = c("loadings", 'intercepts'))
summary(fit_cfa_scalar_higher, fit.measures = T, standardized = T)
anova(fit_cfa_scalar_higher, fit_cfa_metric_higher)

# Metric vs. scalar model: CFI/TLI, RMSEA, and SRMR comparison
d.fitmeasures.comparison_2_higher <- data.frame(cbind(fitMeasures(fit_cfa_metric_higher, c("cfi", "rmsea", "srmr")),
                                                      fitMeasures(fit_cfa_scalar_higher, c("cfi","rmsea", "srmr"))))
colnames(d.fitmeasures.comparison_2_higher) <- c("Metric", "Scalar")
d.fitmeasures.comparison_2_higher$Diff <- d.fitmeasures.comparison_2_higher[,2] - d.fitmeasures.comparison_2_higher[,1]
round(d.fitmeasures.comparison_2_higher, digits = 5)

# Total comparative fit statistics
fit.stat_higher <- as.data.frame(cbind(round(d.fitmeasures.comparison_higher, digits = 5),
                                       round(d.fitmeasures.comparison_2_higher, digits = 5)))
# -----------------------------------------------------------------------------------------------------------------#
# SEM
# -----------------------------------------------------------------------------------------------------------------#
formative <- '

# Formative Factors
Teacher_Quality =~ 0
Teacher_Quality ~ 1*Math_Teacher_Understandable + Math_Teacher_Oderliness
'
sem <- paste0(cfa_higher, formative,
              '
              # Regressions:
              
              Student_Attitudes ~ a*Teacher_Quality + b*Comfort_furniture + c*Safety + d*Conditions + e*n_tech + BSBGHER
              WELLNESS ~ f*Student_Attitudes + BSBGHER 
              
              # Ind effects
              Ind1 := a*f
              Ind2 := b*f
              Ind3 := c*f
              Ind4 := d*f
              Ind5 := e*f
              ')

# Run Lavaan:
fit_sem3 <- sem(sem, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT')
summary(fit_sem3, fit.measures = T, standardized = T)

sem_multi <- paste0(cfa_higher, formative,
                    '
                    # Regressions:
                    
                    Student_Attitudes ~ c(a1, a2)*Teacher_Quality + c(b1, b2)*Comfort_furniture + c(c1, c2)*Safety + c(d1, d2)*Conditions + c(e1, e2)*n_tech + BSBGHER
                    WELLNESS ~ c(f1, f2)*Student_Attitudes + BSBGHER 
                    
                    # Ind effects
                    Ind1_1 := a1*f1
                    Ind2_1 := b1*f1
                    Ind3_1 := c1*f1
                    Ind4_1 := d1*f1
                    Ind5_1 := e1*f1
                    
                    Ind1_2 := a2*f2
                    Ind2_2 := b2*f2
                    Ind3_2 := c2*f2
                    Ind4_2 := d2*f2
                    Ind5_2 := e2*f2
                    ')

# By groups
fit_sem_multi3 <- sem(sem_multi, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT',
                      group = 'traditional_style', meanstructure = TRUE)
summary(fit_sem_multi3 , fit.measures = T, standardized = T)

# For plotting
sem_multi_plot <- paste0(cfa_higher, formative,
                         '
                         # Regressions:
                         
                         Student_Attitudes ~ Teacher_Quality + Comfort_furniture + Safety + Conditions + n_tech + BSBGHER
                         WELLNESS ~ Student_Attitudes + BSBGHER 
                         ')

# By groups
fit_sem_multi_plot3 <- sem(sem_multi_plot, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT',
                           group = 'traditional_style', meanstructure = TRUE)
summary(fit_sem_multi_plot3 , fit.measures = T, standardized = T)


# -----------------------------------------------------------------------------------------------------------------#
# Structural Invariance of Mediation Paths
# -----------------------------------------------------------------------------------------------------------------#

# Teacher_Quality -> Student_Attitudes -> WELLNESS

sem_1.3 <- paste0(cfa_higher, formative,'
                  # Regressions
                  
                  Student_Attitudes ~ c(a, a)*Teacher_Quality + Comfort_furniture + Safety + Conditions + n_tech + BSBGHER
                  WELLNESS ~ c(b, b)*Student_Attitudes + BSBGHER
                  ')

fit_sem_1.3 <- sem(sem_1.3, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT',
                   group = 'traditional_style')
anova_res1.3 <- anova(fit_sem_multi3, fit_sem_1.3)


# Comfort_furniture -> Student_Attitudes -> WELLNESS
sem_2.3 <- paste0(cfa_higher, formative,'
                  # Regressions
                  
                  Student_Attitudes ~ Teacher_Quality + c(a, a)*Comfort_furniture + Safety + Conditions + n_tech + BSBGHER 
                  WELLNESS ~ c(b, b)*Student_Attitudes + BSBGHER   
                  ')

fit_sem_2.3 <- sem(sem_2.3, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT',
                   group = 'traditional_style')
anova_res2.3 <- anova(fit_sem_multi3, fit_sem_2.3)


# Safety -> Student_Attitudes -> WELLNESS
sem_3.3 <- paste0(cfa_higher, formative,'
                  # Regressions
                  
                  Student_Attitudes ~ Teacher_Quality + Comfort_furniture + c(a, a)*Safety + Conditions + n_tech + BSBGHER 
                  WELLNESS ~ c(b, b)*Student_Attitudes + BSBGHER   
                  ')

fit_sem_3.3 <- sem(sem_3.3, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT',
                   group = 'traditional_style')
anova_res3.3 <- anova(fit_sem_multi3, fit_sem_3.3)


# Conditions -> Student_Attitudes -> WELLNESS
sem_4.3 <- paste0(cfa_higher, formative,'
                  # Regressions
                  
                  Student_Attitudes ~ Teacher_Quality + Comfort_furniture + Safety + c(a, a)*Conditions + n_tech + BSBGHER 
                  WELLNESS ~ c(b, b)*Student_Attitudes + BSBGHER   
                  ')

fit_sem_4.3 <- sem(sem_4.3, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT',
                   group = 'traditional_style')
anova_res4.3 <- anova(fit_sem_multi3, fit_sem_4.3)


# n_tech -> Student_Attitudes -> WELLNESS
sem_5.3 <- paste0(cfa_higher, formative,'
                  # Regressions
                  
                  Student_Attitudes ~ Teacher_Quality + Comfort_furniture + Safety + Conditions + c(a, a)*n_tech + BSBGHER 
                  WELLNESS ~ c(b, b)*Student_Attitudes + BSBGHER   
                  ')

fit_sem_5.3 <- sem(sem_5.3, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT',
                   group = 'traditional_style')
anova_res5.3 <- anova(fit_sem_multi3, fit_sem_5.3)

anova_res1.3
anova_res2.3
anova_res3.3
anova_res4.3
anova_res5.3










