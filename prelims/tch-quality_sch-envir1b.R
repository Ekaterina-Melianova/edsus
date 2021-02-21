# tch-quality_sch-envir1b.R

# Teaching Styles, Teaching Quality, School Environmemt, Students' Beliefs, and Learning Outcomes
# Mediation modelling

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

# Standardizing
vars_to_scale <- c('BSMMAT01', 'BSMMAT02', 'BSMMAT03', 'BSMMAT04', 'BSMMAT05',
                   'BSSSCI01', 'BSSSCI02', 'BSSSCI03', 'BSSSCI04', 'BSSSCI05')
stu_sus_timss[vars_to_scale] <- scale(stu_sus_timss[vars_to_scale])

# Model
cfa <- '

# Teacher_Quality

Math_Teacher_Understandable =~ BSBM17B + BSBM17C + BSBM17D + BSBM17E + BSBM17F + BSBM17G;
Math_Teacher_Oderliness =~ BSBM18A + BSBM18B + BSBM18C + BSBM18D + BSBM18E + BSBM18F;

# Student_Attitudes

Math_Important =~ BSBM20A + BSBM20B + BSBM20C + BSBM20D + BSBM20E + BSBM20F + BSBM20G + BSBM20I;
Math_Enjoy =~ BSBM16A + BSBM16B + BSBM16C + BSBM16D + BSBM16E + BSBM16F + BSBM16G + BSBM16H + BSBM16I
Math_Strong =~ BSBM19A + BSBM19C + BSBM19D + BSBM19F + BSBM19G + BSBM19H + BSBM19I

# School_Environment

Audibility =~ n15_0 + n15_1
Visibility =~ n16_0 + n16_1 + n16_2
Temperature =~ n11_2 + n12_2 
Comfort_furniture =~ n17_1 + n17_2
Safety =~ n24_0 + n24_1 + n24_2 + n24_4


# Outcomes

MATH =~ BSMMAT01 + BSMMAT02 + BSMMAT03 + BSMMAT04 + BSMMAT05
SCIENCE =~ BSSSCI02 + BSSSCI01 + BSSSCI03 + BSSSCI04 + BSSSCI05
WELLNESS =~ BSBG13E + BSBG13B + BSBG13C + BSBG13D + BSBG13A
'

# Run Lavaan:
fit_cfa <- cfa(cfa, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT', meanstructure = T)
summary(fit_cfa , fit.measures = T, standardized = T)
#semPaths(fit_cfa)

# Reliability
reliability(fit_cfa)

# By teaching styles separately
#fit_cfa_traditional <- cfa(cfa, data = stu_sus_timss[stu_sus_timss$traditional_style == 1,],
#                           std.lv = T, sampling.weights = 'TOTWGT', meanstructure = T)
#summary(fit_cfa_traditional, fit.measures = T, standardized = T)

#fit_cfa_modern <- cfa(cfa, data = stu_sus_timss[stu_sus_timss$traditional_style == 0,],
#                           std.lv = T, sampling.weights = 'TOTWGT', meanstructure = F)
#summary(fit_cfa_modern, fit.measures = T, standardized = T)

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
# The indices' differences do not exceed the cut-off values: strong scalar invariance is supported.

# -----------------------------------------------------------------------------------------------------------------#
# Measurement invariance of higher order factors
# -----------------------------------------------------------------------------------------------------------------#

# Model
cfa_higher <- '

# Teacher_Quality

Math_Teacher_Understandable =~ BSBM17B + BSBM17C + BSBM17D + BSBM17E + BSBM17F + BSBM17G;
Math_Teacher_Oderliness =~ BSBM18A + BSBM18B + BSBM18C + BSBM18D + BSBM18E + BSBM18F;

# Student_Attitudes

Math_Important =~ BSBM20A + BSBM20B + BSBM20C + BSBM20D + BSBM20E + BSBM20F + BSBM20G + BSBM20I;
Math_Enjoy =~ BSBM16A + BSBM16B + BSBM16C + BSBM16D + BSBM16E + BSBM16F + BSBM16G + BSBM16H + BSBM16I
Math_Strong =~ BSBM19A + BSBM19C + BSBM19D + BSBM19F + BSBM19G + BSBM19H + BSBM19I

Student_Attitudes =~ Math_Important + Math_Enjoy + Math_Strong

# School_Environment

Audibility =~ n15_0 + n15_1
Visibility =~ n16_0 + n16_1 + n16_2
Temperature =~ n11_2 + n12_2 
Comfort_furniture =~ n17_1 + n17_2
Safety =~ n24_0 + n24_1 + n24_2 + n24_4

Conditions =~ Audibility + Visibility + Temperature

# Pseudo factors

psf_n_tech =~ n_tech
psf_BSBGHER =~ BSBGHER

# Outcomes

MATH =~ BSMMAT01 + BSMMAT02 + BSMMAT03 + BSMMAT04 + BSMMAT05
SCIENCE =~ BSSSCI02 + BSSSCI01 + BSSSCI03 + BSSSCI04 + BSSSCI05
WELLNESS =~ BSBG13E + BSBG13B + BSBG13C + BSBG13D + BSBG13A
'

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

sem <- '

# Teacher_Quality

Math_Teacher_Understandable =~ BSBM17B + BSBM17C + BSBM17D + BSBM17E + BSBM17F + BSBM17G;
Math_Teacher_Oderliness =~ BSBM18A + BSBM18B + BSBM18C + BSBM18D + BSBM18E + BSBM18F;

# Student_Attitudes

Math_Important =~ BSBM20A + BSBM20B + BSBM20C + BSBM20D + BSBM20E + BSBM20F + BSBM20G + BSBM20I;
Math_Enjoy =~ BSBM16A + BSBM16B + BSBM16C + BSBM16D + BSBM16E + BSBM16F + BSBM16G + BSBM16H + BSBM16I
Math_Strong =~ BSBM19A + BSBM19C + BSBM19D + BSBM19F + BSBM19G + BSBM19H + BSBM19I
Student_Attitudes =~ Math_Important + Math_Enjoy + Math_Strong

# School_Environment
Audibility =~ n15_0 + n15_1
Visibility =~ n16_0 + n16_1 + n16_2
Temperature =~ n11_2 + n12_2 
Comfort_furniture =~ n17_1 + n17_2
Safety =~ n24_0 + n24_1 + n24_2 + n24_4

Conditions =~ Audibility + Visibility + Temperature 

# Formative Factors
Teacher_Quality =~ 0
Teacher_Quality ~ 1*Math_Teacher_Understandable + Math_Teacher_Oderliness

# Outcomes
MATH =~ BSMMAT01 + BSMMAT02 + BSMMAT03 + BSMMAT04 + BSMMAT05
SCIENCE =~ BSSSCI02 + BSSSCI01 + BSSSCI03 + BSSSCI04 + BSSSCI05
WELLNESS =~ BSBG13E + BSBG13B + BSBG13C + BSBG13D + BSBG13A

# Regressions:
Student_Attitudes ~ Teacher_Quality + Comfort_furniture + Safety + Conditions + n_tech
MATH ~ Student_Attitudes + Teacher_Quality + Comfort_furniture + Safety + Conditions + n_tech + BSBGHER + ITSEX 
SCIENCE ~ Student_Attitudes + Teacher_Quality + Comfort_furniture + Safety + Conditions + n_tech + BSBGHER + ITSEX
WELLNESS ~ Student_Attitudes + Teacher_Quality + Comfort_furniture + Safety + Conditions + n_tech + BSBGHER + ITSEX
'

# Run Lavaan:
fit_sem <- sem(sem, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT')
summary(fit_sem , fit.measures = T, standardized = T)

fit_sem_multi <- sem(sem, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT',
                          group = 'traditional_style')

# -----------------------------------------------------------------------------------------------------------------#
# Structural Invariance of Mediation Paths
# -----------------------------------------------------------------------------------------------------------------#

# Teacher_Quality -> Student_Attitudes -> MATH

sem_1.1 <- '

# Teacher_Quality

Math_Teacher_Understandable =~ BSBM17B + BSBM17C + BSBM17D + BSBM17E + BSBM17F + BSBM17G
Math_Teacher_Oderliness =~ BSBM18A + BSBM18B + BSBM18C + BSBM18D + BSBM18E + BSBM18F

# Student_Attitudes

Math_Important =~ BSBM20A + BSBM20B + BSBM20C + BSBM20D + BSBM20E + BSBM20F + BSBM20G + BSBM20I
Math_Enjoy =~ BSBM16A + BSBM16B + BSBM16C + BSBM16D + BSBM16E + BSBM16F + BSBM16G + BSBM16H + BSBM16I
Math_Strong =~ BSBM19A + BSBM19C + BSBM19D + BSBM19F + BSBM19G + BSBM19H + BSBM19I
Student_Attitudes =~ Math_Important + Math_Enjoy + Math_Strong

# School_Environment

Audibility =~ n15_0 + n15_1
Visibility =~ n16_0 + n16_1 + n16_2
Temperature =~ n11_2 + n12_2 
Comfort_furniture =~ n17_1 + n17_2
Safety =~ n24_0 + n24_1 + n24_2 + n24_4

Conditions =~ Audibility + Visibility + Temperature 

# Formative Factors

Teacher_Quality =~ 0
Teacher_Quality ~ 1*Math_Teacher_Understandable + Math_Teacher_Oderliness 

# Outcomes

MATH =~ BSMMAT01 + BSMMAT02 + BSMMAT03 + BSMMAT04 + BSMMAT05
SCIENCE =~ BSSSCI02 + BSSSCI01 + BSSSCI03 + BSSSCI04 + BSSSCI05
WELLNESS =~ BSBG13E + BSBG13B + BSBG13C + BSBG13D + BSBG13A

# Pseudo factors

psf_n_tech =~ n_tech
psf_BSBGHER =~ BSBGHER

# Regressions

Student_Attitudes ~ c(a, a)*Teacher_Quality + Comfort_furniture + Safety + Conditions + n_tech + BSBGHER
MATH ~ c(b, b)*Student_Attitudes + Teacher_Quality + Comfort_furniture + Safety + Conditions + n_tech + BSBGHER
SCIENCE ~ Student_Attitudes + Teacher_Quality + Comfort_furniture + Safety + Conditions + n_tech + BSBGHER
WELLNESS ~ Student_Attitudes + Teacher_Quality + Comfort_furniture + Safety + Conditions + n_tech + BSBGHER

'
fit_sem_1.1 <- sem(sem_1.1, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT',
                    group = 'traditional_style', group.equal = 'loadings')
anova(fit_cfa_metric_higher, fit_sem_1.1)


# Teacher_Quality -> Student_Attitudes -> SCIENCE

sem_1.2 <- '

# Teacher_Quality

Math_Teacher_Understandable =~ BSBM17B + BSBM17C + BSBM17D + BSBM17E + BSBM17F + BSBM17G
Math_Teacher_Oderliness =~ BSBM18A + BSBM18B + BSBM18C + BSBM18D + BSBM18E + BSBM18F

# Student_Attitudes

Math_Important =~ BSBM20A + BSBM20B + BSBM20C + BSBM20D + BSBM20E + BSBM20F + BSBM20G + BSBM20I
Math_Enjoy =~ BSBM16A + BSBM16B + BSBM16C + BSBM16D + BSBM16E + BSBM16F + BSBM16G + BSBM16H + BSBM16I
Math_Strong =~ BSBM19A + BSBM19C + BSBM19D + BSBM19F + BSBM19G + BSBM19H + BSBM19I
Student_Attitudes =~ Math_Important + Math_Enjoy + Math_Strong

# School_Environment

Audibility =~ n15_0 + n15_1
Visibility =~ n16_0 + n16_1 + n16_2
Temperature =~ n11_2 + n12_2 
Comfort_furniture =~ n17_1 + n17_2
Safety =~ n24_0 + n24_1 + n24_2 + n24_4

Conditions =~ Audibility + Visibility + Temperature 

# Formative Factors

Teacher_Quality =~ 0
Teacher_Quality ~ 1*Math_Teacher_Understandable + Math_Teacher_Oderliness 

# Outcomes

MATH =~ BSMMAT01 + BSMMAT02 + BSMMAT03 + BSMMAT04 + BSMMAT05
SCIENCE =~ BSSSCI02 + BSSSCI01 + BSSSCI03 + BSSSCI04 + BSSSCI05
WELLNESS =~ BSBG13E + BSBG13B + BSBG13C + BSBG13D + BSBG13A

# Pseudo factors

psf_n_tech =~ n_tech
psf_BSBGHER =~ BSBGHER

# Regressions

Student_Attitudes ~ c(a, a)*Teacher_Quality + Comfort_furniture + Safety + Conditions + n_tech + BSBGHER 
MATH ~ Student_Attitudes + Teacher_Quality + Comfort_furniture + Safety + Conditions + n_tech + BSBGHER   
SCIENCE ~ c(b, b)*Student_Attitudes + Teacher_Quality + Comfort_furniture + Safety + Conditions + n_tech + BSBGHER
WELLNESS ~ Student_Attitudes + Teacher_Quality + Comfort_furniture + Safety + Conditions + n_tech + BSBGHER
'
fit_sem_1.2 <- sem(sem_1.2, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT',
                    group = 'traditional_style', group.equal = "loadings")
anova(fit_cfa_metric_higher, fit_sem_1.2)


# Teacher_Quality -> Student_Attitudes -> WELLNESS

sem_1.3 <- '

# Teacher_Quality

Math_Teacher_Understandable =~ BSBM17B + BSBM17C + BSBM17D + BSBM17E + BSBM17F + BSBM17G
Math_Teacher_Oderliness =~ BSBM18A + BSBM18B + BSBM18C + BSBM18D + BSBM18E + BSBM18F

# Student_Attitudes

Math_Important =~ BSBM20A + BSBM20B + BSBM20C + BSBM20D + BSBM20E + BSBM20F + BSBM20G + BSBM20I
Math_Enjoy =~ BSBM16A + BSBM16B + BSBM16C + BSBM16D + BSBM16E + BSBM16F + BSBM16G + BSBM16H + BSBM16I
Math_Strong =~ BSBM19A + BSBM19C + BSBM19D + BSBM19F + BSBM19G + BSBM19H + BSBM19I
Student_Attitudes =~ Math_Important + Math_Enjoy + Math_Strong

# School_Environment

Audibility =~ n15_0 + n15_1
Visibility =~ n16_0 + n16_1 + n16_2
Temperature =~ n11_2 + n12_2 
Comfort_furniture =~ n17_1 + n17_2
Safety =~ n24_0 + n24_1 + n24_2 + n24_4

Conditions =~ Audibility + Visibility + Temperature 

# Formative Factors

Teacher_Quality =~ 0
Teacher_Quality ~ 1*Math_Teacher_Understandable + Math_Teacher_Oderliness 

# Outcomes

MATH =~ BSMMAT01 + BSMMAT02 + BSMMAT03 + BSMMAT04 + BSMMAT05
SCIENCE =~ BSSSCI02 + BSSSCI01 + BSSSCI03 + BSSSCI04 + BSSSCI05
WELLNESS =~ BSBG13E + BSBG13B + BSBG13C + BSBG13D + BSBG13A

# Pseudo factors

psf_n_tech =~ n_tech
psf_BSBGHER =~ BSBGHER

# Regressions

Student_Attitudes ~ c(a, a)*Teacher_Quality + Comfort_furniture + Safety + Conditions + n_tech + BSBGHER 
MATH ~ Student_Attitudes + Teacher_Quality + Comfort_furniture + Safety + Conditions + n_tech + BSBGHER   
SCIENCE ~ Student_Attitudes + Teacher_Quality + Comfort_furniture + Safety + Conditions + n_tech + BSBGHER
WELLNESS ~ c(b, b)*Student_Attitudes + Teacher_Quality + Comfort_furniture + Safety + Conditions + n_tech + BSBGHER
'
fit_sem_1.3 <- sem(sem_1.3, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT',
                 group = 'traditional_style', group.equal = "loadings")
anova(fit_cfa_metric_higher, fit_sem_1.3)


# Comfort_furniture -> Student_Attitudes -> MATH
sem_2.1 <- '

# Teacher_Quality

Math_Teacher_Understandable =~ BSBM17B + BSBM17C + BSBM17D + BSBM17E + BSBM17F + BSBM17G
Math_Teacher_Oderliness =~ BSBM18A + BSBM18B + BSBM18C + BSBM18D + BSBM18E + BSBM18F

# Student_Attitudes

Math_Important =~ BSBM20A + BSBM20B + BSBM20C + BSBM20D + BSBM20E + BSBM20F + BSBM20G + BSBM20I
Math_Enjoy =~ BSBM16A + BSBM16B + BSBM16C + BSBM16D + BSBM16E + BSBM16F + BSBM16G + BSBM16H + BSBM16I
Math_Strong =~ BSBM19A + BSBM19C + BSBM19D + BSBM19F + BSBM19G + BSBM19H + BSBM19I
Student_Attitudes =~ Math_Important + Math_Enjoy + Math_Strong

# School_Environment

Audibility =~ n15_0 + n15_1
Visibility =~ n16_0 + n16_1 + n16_2
Temperature =~ n11_2 + n12_2 
Comfort_furniture =~ n17_1 + n17_2
Safety =~ n24_0 + n24_1 + n24_2 + n24_4

Conditions =~ Audibility + Visibility + Temperature 

# Formative Factors

Teacher_Quality =~ 0
Teacher_Quality ~ 1*Math_Teacher_Understandable + Math_Teacher_Oderliness 

# Outcomes

MATH =~ BSMMAT01 + BSMMAT02 + BSMMAT03 + BSMMAT04 + BSMMAT05
SCIENCE =~ BSSSCI02 + BSSSCI01 + BSSSCI03 + BSSSCI04 + BSSSCI05
WELLNESS =~ BSBG13E + BSBG13B + BSBG13C + BSBG13D + BSBG13A

# Pseudo factors

psf_n_tech =~ n_tech
psf_BSBGHER =~ BSBGHER

# Regressions

Student_Attitudes ~ Teacher_Quality + c(a, a)*Comfort_furniture + Safety + Conditions + n_tech + BSBGHER 
MATH ~ c(b, b)*Student_Attitudes + Teacher_Quality + Comfort_furniture + Safety + Conditions + n_tech + BSBGHER   
SCIENCE ~ Student_Attitudes + Teacher_Quality + Comfort_furniture + Safety + Conditions + n_tech + BSBGHER
WELLNESS ~ Student_Attitudes + Teacher_Quality + Comfort_furniture + Safety + Conditions + n_tech + BSBGHER
'
fit_sem_2.1 <- sem(sem_2.1, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT',
                 group = 'traditional_style', group.equal = "loadings")
anova(fit_cfa_metric_higher, fit_sem_2.1)

# Comfort_furniture -> Student_Attitudes -> SCIENCE
sem_2.2 <- '

# Teacher_Quality

Math_Teacher_Understandable =~ BSBM17B + BSBM17C + BSBM17D + BSBM17E + BSBM17F + BSBM17G
Math_Teacher_Oderliness =~ BSBM18A + BSBM18B + BSBM18C + BSBM18D + BSBM18E + BSBM18F

# Student_Attitudes

Math_Important =~ BSBM20A + BSBM20B + BSBM20C + BSBM20D + BSBM20E + BSBM20F + BSBM20G + BSBM20I
Math_Enjoy =~ BSBM16A + BSBM16B + BSBM16C + BSBM16D + BSBM16E + BSBM16F + BSBM16G + BSBM16H + BSBM16I
Math_Strong =~ BSBM19A + BSBM19C + BSBM19D + BSBM19F + BSBM19G + BSBM19H + BSBM19I
Student_Attitudes =~ Math_Important + Math_Enjoy + Math_Strong

# School_Environment

Audibility =~ n15_0 + n15_1
Visibility =~ n16_0 + n16_1 + n16_2
Temperature =~ n11_2 + n12_2 
Comfort_furniture =~ n17_1 + n17_2
Safety =~ n24_0 + n24_1 + n24_2 + n24_4

Conditions =~ Audibility + Visibility + Temperature 

# Formative Factors

Teacher_Quality =~ 0
Teacher_Quality ~ 1*Math_Teacher_Understandable + Math_Teacher_Oderliness 

# Outcomes

MATH =~ BSMMAT01 + BSMMAT02 + BSMMAT03 + BSMMAT04 + BSMMAT05
SCIENCE =~ BSSSCI02 + BSSSCI01 + BSSSCI03 + BSSSCI04 + BSSSCI05
WELLNESS =~ BSBG13E + BSBG13B + BSBG13C + BSBG13D + BSBG13A

# Pseudo factors

psf_n_tech =~ n_tech
psf_BSBGHER =~ BSBGHER

# Regressions

Student_Attitudes ~ Teacher_Quality + c(a, a)*Comfort_furniture + Safety + Conditions + n_tech + BSBGHER 
MATH ~ Student_Attitudes + Teacher_Quality + Comfort_furniture + Safety + Conditions + n_tech + BSBGHER   
SCIENCE ~ c(b, b)*Student_Attitudes + Teacher_Quality + Comfort_furniture + Safety + Conditions + n_tech + BSBGHER
WELLNESS ~ Student_Attitudes + Teacher_Quality + Comfort_furniture + Safety + Conditions + n_tech + BSBGHER
'
fit_sem_2.2 <- sem(sem_2.2, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT',
                   group = 'traditional_style', group.equal = "loadings")
anova(fit_cfa_metric_higher, fit_sem_2.2)


# Comfort_furniture -> Student_Attitudes -> WELLNESS
sem_2.3 <- '

# Teacher_Quality

Math_Teacher_Understandable =~ BSBM17B + BSBM17C + BSBM17D + BSBM17E + BSBM17F + BSBM17G
Math_Teacher_Oderliness =~ BSBM18A + BSBM18B + BSBM18C + BSBM18D + BSBM18E + BSBM18F

# Student_Attitudes

Math_Important =~ BSBM20A + BSBM20B + BSBM20C + BSBM20D + BSBM20E + BSBM20F + BSBM20G + BSBM20I
Math_Enjoy =~ BSBM16A + BSBM16B + BSBM16C + BSBM16D + BSBM16E + BSBM16F + BSBM16G + BSBM16H + BSBM16I
Math_Strong =~ BSBM19A + BSBM19C + BSBM19D + BSBM19F + BSBM19G + BSBM19H + BSBM19I
Student_Attitudes =~ Math_Important + Math_Enjoy + Math_Strong

# School_Environment

Audibility =~ n15_0 + n15_1
Visibility =~ n16_0 + n16_1 + n16_2
Temperature =~ n11_2 + n12_2 
Comfort_furniture =~ n17_1 + n17_2
Safety =~ n24_0 + n24_1 + n24_2 + n24_4

Conditions =~ Audibility + Visibility + Temperature 

# Formative Factors

Teacher_Quality =~ 0
Teacher_Quality ~ 1*Math_Teacher_Understandable + Math_Teacher_Oderliness 

# Outcomes

MATH =~ BSMMAT01 + BSMMAT02 + BSMMAT03 + BSMMAT04 + BSMMAT05
SCIENCE =~ BSSSCI02 + BSSSCI01 + BSSSCI03 + BSSSCI04 + BSSSCI05
WELLNESS =~ BSBG13E + BSBG13B + BSBG13C + BSBG13D + BSBG13A

# Pseudo factors

psf_n_tech =~ n_tech
psf_BSBGHER =~ BSBGHER

# Regressions

Student_Attitudes ~ Teacher_Quality + c(a, a)*Comfort_furniture + Safety + Conditions + n_tech + BSBGHER 
MATH ~ Student_Attitudes + Teacher_Quality + Comfort_furniture + Safety + Conditions + n_tech + BSBGHER   
SCIENCE ~ Student_Attitudes + Teacher_Quality + Comfort_furniture + Safety + Conditions + n_tech + BSBGHER
WELLNESS ~ c(b, b)*Student_Attitudes + Teacher_Quality + Comfort_furniture + Safety + Conditions + n_tech + BSBGHER
'
fit_sem_2.3 <- sem(sem_2.3, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT',
                   group = 'traditional_style', group.equal = "loadings")
anova(fit_cfa_metric_higher, fit_sem_2.3)


# Safety -> Student_Attitudes -> MATH
# Safety -> Student_Attitudes -> SCIENCE
# Safety -> Student_Attitudes -> WELLNESS

# Conditions -> Student_Attitudes -> MATH
# Conditions -> Student_Attitudes -> SCIENCE
# Conditions -> Student_Attitudes -> WELLNESS

# n_tech -> Student_Attitudes -> MATH
# n_tech -> Student_Attitudes -> SCIENCE
# n_tech -> Student_Attitudes -> WELLNESS














