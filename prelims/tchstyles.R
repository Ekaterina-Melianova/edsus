# tchstyles_practices.R
# Spatial Arrangement, Teaching Practices, and Learning Outcomes: a Mediation Analysis

# Libraries
library(foreign)
library(dplyr)
library(tidyr)
library(tidyverse)
library(magrittr)
library(lavaan)
library(Amelia)
#install.packages("https://cran.r-project.org/bin/windows/contrib/4.1/OpenMx_2.18.1.zip", repos = NULL)
library(semPlot)
library(stargazer)
library(GPArotation)
library(semTools)
library(hrbrthemes)

################################################# Data pre-processing #################################################
Sys.setlocale("LC_CTYPE", "russian")

### SUS

setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/SUS_TIMSS/Data/SUS'))
tch_sus <- read.csv('export_teachers_flat.csv', sep = '\t') %>% 
  select(IDSCHOOL, IDTEACH, StratNameENG, n24_1, n24_3, CourseName, n3_1)

# Defining a subject: Math (1), Science (2) or Other (3)
tch_sus$subject_math <- ifelse(tch_sus$CourseName == '1 - "Математика"', 1, 0)
tch_sus$subject_science <- ifelse(tch_sus$CourseName == '2 - "Физика"', 1, 0)
table(tch_sus$subject_math)
table(tch_sus$subject_science)

# Fixing teaching experience
tch_sus$n3_1 <- as.character(tch_sus$n3_1)
tch_sus$n3_1 <- sub("[а-яА-Я]+", x = tch_sus$n3_1, replacement = '')
tch_sus$n3_1 <- sub(",", x = tch_sus$n3_1, replacement = '.')
tch_sus[tch_sus$IDTEACH == 306302, 'n3_1'] <- 7 # fixing manually 
tch_sus$n3_1 <- as.numeric(tch_sus$n3_1)

# Subsetting by subjects
tch_sus_math <- tch_sus %>% filter(subject_math == 1) %>% 
  select(IDSCHOOL, IDTEACH, StratNameENG, n24_1, n24_3, n3_1)
names(tch_sus_math) <- c('IDSCHOOL_ORI', 'IDTEACH_ORI', 'Region', 'Small_Groups', 'Team_Teaching', 'Teacher_Exper')

tch_sus_science <- tch_sus %>% filter(subject_science == 1) %>%
  select(IDSCHOOL, IDTEACH, StratNameENG, n24_1, n24_3, n3_1)
names(tch_sus_science) <- c('IDSCHOOL_ORI', 'IDTEACH_ORI', 'Region', 'Small_Groups', 'Team_Teaching', 'Teacher_Exper')


# Function: summarise numeric columns, return first value of non-numeric (regions in our case)
summarise_by_col_class <- function(x){
   if(is.numeric(x)){
     return(median(x, na.rm = T))
   }
   else{
     nth(x, 1)
   }
}

## Averaging by teachers if they appear within a subject area several times: I refused from that step for now
# table(duplicated(tch_sus_math$IDTEACH_ORI))
# table(duplicated(tch_sus_science$IDTEACH_ORI))
# 
# tch_sus_math %<>% group_by(IDSCHOOL_ORI, IDTEACH_ORI, Region) %>%
#   summarise_all(summarise_by_col_class) %>% ungroup()
# 
# tch_sus_science %<>% group_by(IDSCHOOL_ORI, IDTEACH_ORI, Region) %>%
#   summarise_all(summarise_by_col_class) %>% ungroup()

### TIMSS

setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/SUS_TIMSS/Data/TIMSS/SPSS'))

# Students' achievements
bstrusm7 <- read.spss('bstrusm7.sav', use.value.labels = F, to.data.frame = T)  %>%
  select(IDSCHOOL_ORI, IDCLASS_ORI, IDSTUD_ORI, IDTEACH_ORI,
         BSMMAT01, BSMMAT02, BSMMAT03, BSMMAT04, BSMMAT05,
         BSSSCI01, BSSSCI02, BSSSCI03, BSSSCI04, BSSSCI05,
         TOTWGT)

# Students' SES
bsgrusm7 <- read.spss('bsgrusm7.sav', use.value.labels = F, to.data.frame = T) %>%
  select(IDSCHOOL_ORI, IDCLASS_ORI, IDSTUD_ORI, ITSEX, BSDAGE, BSBGHER, BSDGHER)

# Merge SES and achievements
stu_timss <- bstrusm7 %>%
  left_join(bsgrusm7, by = c('IDSCHOOL_ORI', 'IDCLASS_ORI', 'IDSTUD_ORI'))

# Teachers practices: MATH
btmrusm7 <- read.spss('btmrusm7.sav', use.value.labels = F, to.data.frame = T) %>%
  select(IDSCHOOL_ORI, IDTEACH_ORI, IDSUBJ, BTBG02, BTBG03,
         BTBG12A, BTBG12B, BTBG12C, BTBG12D, BTBG12E,  BTBG12F, BTBG12G,
         BTBM15A, BTBM15B, BTBM15C, BTBM15D, BTBM15E, BTBM15F, BTBM15G, BTBM15H)#, BTBM15I)

# Teachers practices: SCIENCE
btsrusm7 <- read.spss('btsrusm7.sav', use.value.labels = F, to.data.frame = T) %>%
  select(IDSCHOOL_ORI, IDTEACH_ORI, IDSUBJ, BTBG02, BTBG03,
         BTBG12A, BTBG12B, BTBG12C, BTBG12D, BTBG12E, BTBG12F, BTBG12G,
         BTBS15A, BTBS15B, BTBS15C, BTBS15D, BTBS15E, BTBS15F, BTBS15G,
         BTBS15H, BTBS15I, BTBS15J, BTBS15K, BTBS15L, BTBS15M, BTBS15N)

# Teachers and Students TIMSS joint: MATH
math_timss <- stu_timss %>%
  inner_join(btmrusm7, by = c('IDSCHOOL_ORI', 'IDTEACH_ORI'))

# Teachers and Students TIMSS joint: SCIENCE
science_timss <- stu_timss %>%
  inner_join(btsrusm7, by = c('IDSCHOOL_ORI', 'IDTEACH_ORI')) 
  

### Merging TIMSS and SUS
# MATH
math_timss_sus <- math_timss %>%
  left_join(tch_sus_math, by = c('IDSCHOOL_ORI', 'IDTEACH_ORI'))
math_timss_sus$IDSUBJ <- NULL

# SCIENCE
science_timss_sus <- science_timss %>%
  left_join(tch_sus_science, by = c('IDSCHOOL_ORI', 'IDTEACH_ORI'))
science_timss_sus$IDSUBJ <- NULL

### Recode practices
# Math
practice_vars_math <- c('BTBG12B', 'BTBG12D', 'BTBG12E',  'BTBG12F', 'BTBG12G', 'BTBM15A', 'BTBM15B', 'BTBM15C', 'BTBM15D', 'BTBM15E', 'BTBM15F')
practice_vars_math_n <- which(colnames(math_timss_sus) %in% practice_vars_math)
math_timss_sus %<>% 
  mutate_at(practice_vars_math_n, recode, '1'='4', '2'='3', '3'='2', '4'='1')
math_timss_sus[,practice_vars_math_n] <- sapply(math_timss_sus[,practice_vars_math_n], as.numeric)

# Science
practice_vars_science <- c('BTBG12A', 'BTBG12B', 'BTBG12C', 'BTBG12D', 'BTBG12E', 'BTBG12F', 'BTBG12G',
                           'BTBS15A', 'BTBS15B', 'BTBS15C', 'BTBS15D', 'BTBS15E', 'BTBS15F', 'BTBS15G',
                           'BTBS15H', 'BTBS15I', 'BTBS15J', 'BTBS15K', 'BTBS15L', 'BTBS15M', 'BTBS15N')
practice_vars_science_n <- which(colnames(science_timss_sus) %in% practice_vars_science)
science_timss_sus %<>% 
  mutate_at(practice_vars_science_n, recode, '1'='4', '2'='3', '3'='2', '4'='1')
science_timss_sus[,practice_vars_science_n] <- sapply(science_timss_sus[,practice_vars_science_n], as.numeric)

# Checking classes of variables
#lapply(1:ncol(math_timss_sus), function(i){class(math_timss_sus[,i])})
#lapply(1:ncol(science_timss_sus), function(i){class(science_timss_sus[,i])})

################# Averaging by students within subject areas (since each student can have several teachers)

# Duplicated IDSTUD_ORI
table(duplicated(math_timss_sus$IDSTUD_ORI))
table(duplicated(science_timss_sus$IDSTUD_ORI))

math_timss_sus$IDTEACH_ORI <- NULL
math_timss_sus %<>% group_by(IDSCHOOL_ORI, IDCLASS_ORI, IDSTUD_ORI) %>%
  summarise_all(summarise_by_col_class) %>% ungroup()

science_timss_sus$IDTEACH_ORI <- NULL
science_timss_sus %<>% group_by(IDSCHOOL_ORI, IDCLASS_ORI, IDSTUD_ORI)  %>%
  summarise_all(summarise_by_col_class) %>% ungroup()

# Moving all ids to the beginning of the datasets
math_timss_sus <- math_timss_sus[,c(which(names(math_timss_sus) == 'Region'),
                                    1:(which(names(math_timss_sus) == 'Region') - 1),
                                    (which(names(math_timss_sus) == 'Region') + 1):ncol(math_timss_sus))]
science_timss_sus <- science_timss_sus[,c(which(names(science_timss_sus) == 'Region'),
                                    1:(which(names(science_timss_sus) == 'Region') - 1),
                                    (which(names(science_timss_sus) == 'Region') + 1):ncol(science_timss_sus))]


################################################# Some Distributions #################################################

# Listwise deletion of missing values
summary(math_timss_sus)
math_timss_sus <- as.data.frame(na.omit(math_timss_sus))

summary(science_timss_sus)
science_timss_sus <- as.data.frame(na.omit(science_timss_sus))

# Math Scores as a factor
cfa_math_scores <- '
MATH =~ BSMMAT01 + BSMMAT02 + BSMMAT03 + BSMMAT04 + BSMMAT05
'
fit_cfa_math_scores  <- cfa(cfa_math_scores, data = math_timss_sus, std.lv = T, sampling.weights = 'TOTWGT')
#summary(fit_cfa_math_scores, fit.measures = T, standardized = T)
math_timss_sus$math_scores <- as.numeric(lavPredict(fit_cfa_math_scores) * sd(math_timss_sus$BSMMAT01) + 
  mean(math_timss_sus$BSMMAT01))

# Math Scores as a factor
cfa_science_scores <- '
SCIENCE =~ BSSSCI02 + BSSSCI01 + BSSSCI03 + BSSSCI04 + BSSSCI05
'
fit_cfa_science_scores  <- cfa(cfa_science_scores, data = science_timss_sus, std.lv = T, sampling.weights = 'TOTWGT')
#summary(fit_cfa_science_scores, fit.measures = T, standardized = T)
science_timss_sus$science_scores <- as.numeric(lavPredict(fit_cfa_science_scores) * sd(science_timss_sus$BSSSCI02) +
  mean(science_timss_sus$BSSSCI02))

# Dist
table(math_timss_sus$Small_Groups)
table(math_timss_sus$Team_Teaching)
table(science_timss_sus$Small_Groups)
table(science_timss_sus$Team_Teaching)

# Binarizing small groups and team teaching variables: Never vs. At least once a month
# 1 = Never;
# 2 = Once a month;
# 3 = Once a week;
# 4 = 2-4 times a week;
# 5 = Every day;

math_timss_sus$Small_Groups_bin <- ifelse(math_timss_sus$Small_Groups == 1, 0, 1)
math_timss_sus$Team_Teaching_bin <- ifelse(math_timss_sus$Team_Teaching == 1, 0, 1)
science_timss_sus$Small_Groups_bin <- ifelse(science_timss_sus$Small_Groups == 1, 0, 1)
science_timss_sus$Team_Teaching_bin <- ifelse(science_timss_sus$Team_Teaching == 1, 0, 1)

# t.tests weighted
library(weights)

# MATH
wtd.t.test(math_timss_sus$math_scores[math_timss_sus$Small_Groups_bin == 1],
           math_timss_sus$math_scores[math_timss_sus$Small_Groups_bin == 0],
           math_timss_sus$TOTWGT[math_timss_sus$Small_Groups_bin == 1],
           math_timss_sus$TOTWGT[math_timss_sus$Small_Groups_bin == 0],
           samedata = F)
wtd.t.test(math_timss_sus$math_scores[math_timss_sus$Team_Teaching_bin == 1],
           math_timss_sus$math_scores[math_timss_sus$Team_Teaching_bin == 0],
           math_timss_sus$TOTWGT[math_timss_sus$Team_Teaching_bin == 1],
           math_timss_sus$TOTWGT[math_timss_sus$Team_Teaching_bin == 0],
           samedata = F)

# SCIENCE
wtd.t.test(science_timss_sus$science_scores[science_timss_sus$Small_Groups_bin == 1],
           science_timss_sus$science_scores[science_timss_sus$Small_Groups_bin == 0],
           science_timss_sus$TOTWGT[science_timss_sus$Small_Groups_bin == 1],
           science_timss_sus$TOTWGT[science_timss_sus$Small_Groups_bin == 0],
           samedata = F)
wtd.t.test(science_timss_sus$science_scores[science_timss_sus$Team_Teaching_bin == 1],
           science_timss_sus$science_scores[science_timss_sus$Team_Teaching_bin == 0],
           science_timss_sus$TOTWGT[science_timss_sus$Team_Teaching_bin == 1],
           science_timss_sus$TOTWGT[science_timss_sus$Team_Teaching_bin == 0],
           samedata = F)

######### Validating with graphs
# MATH 
# Small Groups
math_timss_sus %>%
  ggplot(aes(x = math_scores, fill = factor(Small_Groups_bin,
                                            labels = c("Almost never",
                                                       'Once per month or more often'))))  +
  geom_density(alpha = .3) + 
  theme_classic() +
  scale_fill_manual(values = c('deepskyblue2', 'darksalmon')) +
  labs(x = "Math Scores", y = "Density", fill = "") + 
  geom_vline(aes(xintercept = mean(math_timss_sus[Small_Groups_bin == 1, 'math_scores'])),  
             color = "darksalmon", linetype = "dashed", size = 3) + 
  geom_vline(aes(xintercept = mean(math_timss_sus[Small_Groups_bin == 0, 'math_scores'])),  
             color = "deepskyblue2", linetype = "dashed", size = 3) + 
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        legend.position = "bottom") + 
  ggplot2::annotate("text", label = paste0(as.character(round(mean(math_timss_sus[math_timss_sus$Small_Groups_bin == 1, 'math_scores']), 1)),
                    "\nMean Score\n(teachers use small groups layout)"), x = 400, y = 0.003, size = 6,
                    colour = "darksalmon") + 
  ggplot2::annotate("text", label = paste0(as.character(round(mean(math_timss_sus[math_timss_sus$Small_Groups_bin == 0, 'math_scores']), 1)),
                    "\nMean Score\n(teachers do not use small groups layout)"), x = 700, y = 0.003, size = 6,
                    colour = "deepskyblue2") +
  theme(legend.text = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) + 
  ggtitle("Math Scores for the Usage of the Small Groups Layout (Binarized)")


# Team Teaching
math_timss_sus %>%
  ggplot(aes(x = math_scores, fill = factor(Team_Teaching_bin,
                                            labels = c("Almost never",
                                                       'Once per month or more often'))))  +
  geom_density(alpha = .3) + 
  theme_classic() +
  scale_fill_manual(values = c('deepskyblue2', 'darksalmon')) +
  labs(x = "math Scores", y = "Density", fill = "") + 
  geom_vline(aes(xintercept = mean(math_timss_sus[Team_Teaching_bin == 1, 'math_scores'])),  
             color = "darksalmon", linetype = "dashed", size = 3) + 
  geom_vline(aes(xintercept = mean(math_timss_sus[Team_Teaching_bin == 0, 'math_scores'])),  
             color = "deepskyblue2", linetype = "dashed", size = 3) + 
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        legend.position = "bottom") + 
  ggplot2::annotate("text", label = paste0(as.character(round(mean(math_timss_sus[math_timss_sus$Team_Teaching_bin == 1, 'math_scores']), 1)),
                                           "\nMean Score\n(teachers use team teaching layout)"), x = 400, y = 0.003, size = 6,
                    colour = "darksalmon") + 
  ggplot2::annotate("text", label = paste0(as.character(round(mean(math_timss_sus[math_timss_sus$Team_Teaching_bin == 0, 'math_scores']), 1)),
                                           "\nMean Score\n(teachers do not use team teaching layout)"), x = 700, y = 0.003, size = 6,
                    colour = "deepskyblue2") +
  theme(legend.text = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) + 
  ggtitle("Math Scores for the Usage of the Team Teaching Layout (Binarized)")

# SCIENCE 
# Small Groups
science_timss_sus %>%
  ggplot(aes(x = science_scores, fill = factor(Small_Groups_bin,
                                            labels = c("Almost never",
                                                       'Once per month or more often'))))  +
  geom_density(alpha = .3) + 
  theme_classic() +
  scale_fill_manual(values = c('deepskyblue2', 'darksalmon')) +
  labs(x = "science Scores", y = "Density", fill = "") + 
  geom_vline(aes(xintercept = mean(science_timss_sus[Small_Groups_bin == 1, 'science_scores'])),  
             color = "darksalmon", linetype = "dashed", size = 3) + 
  geom_vline(aes(xintercept = mean(science_timss_sus[Small_Groups_bin == 0, 'science_scores'])),  
             color = "deepskyblue2", linetype = "dashed", size = 3) + 
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        legend.position = "bottom") + 
  ggplot2::annotate("text", label = paste0(as.character(round(mean(science_timss_sus[science_timss_sus$Small_Groups_bin == 1, 'science_scores']), 1)),
                                           "\nMean Score\n(teachers use small groups layout)"), x = 400, y = 0.003, size = 6,
                    colour = "darksalmon") + 
  ggplot2::annotate("text", label = paste0(as.character(round(mean(science_timss_sus[science_timss_sus$Small_Groups_bin == 0, 'science_scores']), 1)),
                                           "\nMean Score\n(teachers do not use small groups layout)"), x = 700, y = 0.003, size = 6,
                    colour = "deepskyblue2") +
  theme(legend.text = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) + 
  ggtitle("science Scores for the Usage of the Small Groups Layout (Binarized)")


# Team Teaching
science_timss_sus %>%
  ggplot(aes(x = science_scores, fill = factor(Team_Teaching_bin,
                                               labels = c("Almost never",
                                                          'Once per month or more often'))))  +
  geom_density(alpha = .3) + 
  theme_classic() +
  scale_fill_manual(values = c('deepskyblue2', 'darksalmon')) +
  labs(x = "Science Scores", y = "Density", fill = "") + 
  geom_vline(aes(xintercept = mean(science_timss_sus[Team_Teaching_bin == 1, 'science_scores'])),  
             color = "darksalmon", linetype = "dashed", size = 3) + 
  geom_vline(aes(xintercept = mean(science_timss_sus[Team_Teaching_bin == 0, 'science_scores'])),  
             color = "deepskyblue2", linetype = "dashed", size = 3) + 
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        legend.position = "bottom") + 
  ggplot2::annotate("text", label = paste0(as.character(round(mean(science_timss_sus[science_timss_sus$Team_Teaching_bin == 1, 'science_scores']), 1)),
                                           "\nMean Score\n(teachers use team teaching layout)"), x = 400, y = 0.003, size = 6,
                    colour = "darksalmon") + 
  ggplot2::annotate("text", label = paste0(as.character(round(mean(science_timss_sus[science_timss_sus$Team_Teaching_bin == 0, 'science_scores']), 1)),
                                           "\nMean Score\n(teachers do not use team teaching layout)"), x = 700, y = 0.003, size = 6,
                    colour = "deepskyblue2") +
  theme(legend.text = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) + 
  ggtitle("Science Scores for the Usage of the Team Teaching Layout (Binarized)")

################################################# Modelling: MATH ################################################# 

################# EFA
formulaEFA <- as.formula("~ BTBG12A + BTBG12B + BTBG12C + BTBG12D + BTBG12E +  BTBG12F + BTBG12G + BTBM15A + BTBM15B + BTBM15C + BTBM15D + BTBM15E + BTBM15F + BTBM15G + BTBM15H")
EFA3 <- factanal(formulaEFA,
                 factors = 3,
                 data = math_timss_sus,
                 rotation = "geominQ",
                 na.action = na.exclude)

print(loadings(EFA3), cutoff=0.2, sort=T) #Print all the loadings

# Exclude low loadings (12A, 12C) and 15G, 15H since they substantively differ from the rest of the items
# Try 4 Factors
formulaEFA <- as.formula("~ BTBG12B + BTBG12D + BTBG12E +  BTBG12F + BTBG12G + BTBM15A + BTBM15B + BTBM15C + BTBM15D + BTBM15E + BTBM15F")
EFA4 <- factanal(formulaEFA,
                 factors = 4,
                 data = math_timss_sus,
                 rotation = "geominQ",
                 na.action = na.exclude)

print(loadings(EFA4), cutoff=0.3, sort=T) #Print all the loadings

################# CFA
# Model:
cfa_math <- '
Discussions =~ BTBG12G + BTBG12F + BTBG12D;
Explain =~  BTBG12E + BTBG12B;
Follow_Teacher =~ BTBM15B + BTBM15A + BTBM15C + BTBM15F;
Practice =~ BTBM15D + BTBM15E;
MATH =~ BSMMAT01 + BSMMAT02 + BSMMAT03 + BSMMAT04 + BSMMAT05
'
vars_to_scale <- c('BSMMAT01', 'BSMMAT02', 'BSMMAT03', 'BSMMAT04', 'BSMMAT05')
math_timss_sus[vars_to_scale] <- scale(math_timss_sus[vars_to_scale])

# Run Lavaan:
fit_cfa_math  <- cfa(cfa_math, data = math_timss_sus, std.lv = T, sampling.weights = 'TOTWGT')
summary(fit_cfa_math , fit.measures = T, standardized = T)

# Create the path diagram:
#semPaths(fit_cfa_math, "std", title = F, intercepts = F,
#         residuals = F, thresholds = F, label.prop = 1.8, rotation = 4)
#title("Path Diagram for the Measurement Model", line = 2)

################# SEM 

structural_math <- '
# Measurement model

Discussions =~ BTBG12G + BTBG12F + BTBG12D;
Explain =~  BTBG12E + BTBG12B;
Follow_Teacher =~ BTBM15B + BTBM15A + BTBM15C + BTBM15F;
Practice =~ BTBM15D + BTBM15E;
MATH =~ BSMMAT01 + BSMMAT02 + BSMMAT03 + BSMMAT04 + BSMMAT05;

# Associations

MATH ~ m1*Discussions + m2*Explain + m3*Follow_Teacher + m4*Practice + m5*Small_Groups + m6*Team_Teaching;

Discussions ~ d1*Small_Groups + d2*Team_Teaching;
Explain ~ e1*Small_Groups + e2*Team_Teaching;
Follow_Teacher ~ f1*Small_Groups + f2*Team_Teaching;
Practice ~ p1*Small_Groups + p2*Team_Teaching;

# Covariances

Practice ~~ Discussions;
Discussions ~~ Explain;
Explain ~~ Practice;

### Indirect effects

# Small_Groups

SG__Disc_ind := d1*m1
SG__Expl_ind := e1*m2
SG__FllwTch_ind := f1*m3
SG__Prac_ind := p1*m4

# Team_Teaching

TT__Disc_ind := d2*m1
TT__Expl_ind := e2*m2
TT__FllwTch_ind := f2*m3
TT__Prac_ind := p2*m4

# Total total

SG__ind := d1*m1 + e1*m2 + f1*m3 + p1*m4
SG__tot := m5 + SG__ind

TT__ind := d2*m1 + e2*m2 + f2*m3 + p2*m4
TT__tot := m6 + TT__ind
'

fit_structural_math <- sem(structural_math, data = math_timss_sus, sampling.weights = 'TOTWGT')
#summary(fit_structural_math, fit.measures = T, standardized = T, modindices = F)
# modindices(fit_structural_math, minimum.value = 10, sort=TRUE)[1:20,]

# Manually fixing the layout
path_test <- semPaths(fit_structural_math, 'std', intercepts = F,
                      residuals = F, thresholds = F, colFactor = 0,
                      label.prop = 1.3, optimizeLatRes = T, mar = c(2,2,2,2),
                      rotation = 2, esize = 2, layout = 'tree2',
                      equalizeManifests = F, DoNotPlot = T)
layout <- path_test$layout
layout[1,] <- c(0.5, -1)
layout[2,] <- c(0.5, -0.8)
layout[3,] <- c(0.5, -0.6)
layout[10,] <- c(0.5, 0.8)
layout[11,] <- c(0.5, 1)
layout[12,] <- c(-0.5, -1)
layout[13,] <- c(-0.5, 1)
layout[23,] <- c(0.5, 0.2)
layout[19,] <- c(0, -1)
layout[20,] <- c(0, -0.25)
layout[21,] <- c(0, 0.15)
layout[22,] <- c(0, 1)

# Generating a vector with colors depending on a significance and sign
st_Sol <- standardizedSolution(fit_structural_math)
obj <- semPlotModel(fit_structural_math)
Pars <- as.data.frame(obj@Pars)
st_Sol$rhs_ <- ifelse(st_Sol$op == '~', st_Sol$lhs, st_Sol$rhs)
st_Sol$lhs_ <- ifelse(st_Sol$op == '~', st_Sol$rhs, st_Sol$lhs)
st_Sol$lhs <- NULL
st_Sol$rhs <- NULL

Pars_st_Sol <- Pars %>% select(lhs, edge, rhs) %>%
  left_join((st_Sol %>% select(lhs_, op, rhs_, pvalue, est.std) %>%
               rename(lhs = lhs_, rhs = rhs_)),
            by = c('lhs', 'rhs'))
Pars_st_Sol$pvalue <- round(as.numeric(Pars_st_Sol$pvalue), 3)
Pars_st_Sol$pvalue[is.na(Pars_st_Sol$pvalue)] <- 0
Pars_st_Sol$edge_col <- ifelse(Pars_st_Sol$est.std > 0 & Pars_st_Sol$pvalue < 0.1 & !Pars_st_Sol$op %in% c('=~', '~~'), 'darkgreen',
                               ifelse(Pars_st_Sol$pvalue > 0.1, 'lightgrey', 
                                      ifelse(Pars_st_Sol$op %in% c('=~', '~~'), 'white', 'red')))
Pars_st_Sol$edge_width <- ifelse(Pars_st_Sol$op != '=~', 3, 0.5)

# Plotting
path_fin <- semPaths(fit_structural_math, 'std', intercepts = F,
            residuals = F, thresholds = F, colFactor = 0,
            optimizeLatRes = F, mar = c(2,2,2,2),
            rotation = 2, layout = layout,
            nodeLabels = c('EXPRESS IDEAS', 'PROBLEM SOLVING', 'CLASSROOM DISCUSSIONS',
                           'LINK KNOWLEDGE', 'EXPLAIN ANSWERS',
                           'EXPLAIN HOW TO SOLVE', 'EXPLAIN NEW CONTENT', 'MEMORIZE RULES', 'WORK IN WHOLE CLASS',
                           'PRACTICE PROCEDURES', 'APPLY WHAT LEARNED',
                           'Small Groups', 'Team Teaching',
                           'PVMATH1', 'PVMATH2', 'PVMATH3', 'PVMATH4', 'PVMATH5',
                           'Discussion', 'Interpretation', 'Following Teacher', 'Practice',
                           'Math Scores'),
            sizeMan = 16, sizeLat = 16, sizeMan2 = 4, sizeLat2 = 4, edge.label.cex = 1,
            label.cex = 2.2, label.prop = 0, edge.color = Pars_st_Sol$edge_col, 
            DoNotPlot = T)

path_fin$graphAttributes$Edges$width <- Pars_st_Sol$edge_width
path_fin$graphAttributes$Nodes$color <- c(rep('#E6E600FF', 3), rep('#EF8A62', 2), rep('#FFBFFFFF', 4), 
                                          rep('#FDDBC7', 2), rep('#FFFFFFFF', 2),
                                          rep('#D1E5F0', 5), '#E6E600FF', '#EF8A62', '#FFBFFFFF',
                                          '#FDDBC7', '#D1E5F0')
plot(path_fin)

################################################# Modelling: SCIENCE ################################################# 
################# EFA 

formulaEFA <- as.formula("~ BTBG12A + BTBG12B + BTBG12C + BTBG12D + BTBG12E +  BTBG12F + BTBG12G + BTBS15A + BTBS15B + 
                         BTBS15C + BTBS15D + BTBS15E + BTBS15F + BTBS15G + BTBS15H + BTBS15I + BTBS15J + BTBS15K + BTBS15L")
EFA3 <- factanal(formulaEFA,
                 factors = 5,
                 data = science_timss_sus,
                 rotation = "geominQ",
                 na.action = na.exclude)

#print(loadings(EFA3), cutoff=0.35, sort=T) #Print all the loadings

# Exclude low loadings
# Try 4 Factors
formulaEFA <- as.formula("~ BTBG12D +  BTBG12F + BTBG12G  + BTBS15B + 
                          BTBS15D + BTBS15E + BTBS15F + BTBS15G + BTBS15H + BTBS15I + BTBS15J + BTBS15K + BTBS15L")
EFA4 <- factanal(formulaEFA,
                 factors = 4,
                 data = science_timss_sus,
                 rotation = "geominQ",
                 na.action = na.exclude)

print(loadings(EFA4), cutoff=0.3, sort=T) #Print all the loadings

################# CFA 
vars_to_scale <- c('BSSSCI01', 'BSSSCI02', 'BSSSCI03', 'BSSSCI04', 'BSSSCI05')
science_timss_sus[vars_to_scale] <- scale(science_timss_sus[vars_to_scale])

# Model:
cfa_science <- '
Discussions =~ BTBG12G + BTBG12F + BTBG12D;
Experiments =~ BTBS15F + BTBS15D + BTBS15E +  BTBS15G + BTBS15H;
Memorizing =~ BTBS15K + BTBS15I + BTBS15J;
Field_Work =~ BTBS15L + BTBS15B;
SCIENCE =~ BSSSCI02 + BSSSCI01 + BSSSCI03 + BSSSCI04 + BSSSCI05
'

# Run Lavaan:
fit_cfa_science  <- cfa(cfa_science, data = science_timss_sus, std.lv = T, sampling.weights = 'TOTWGT')
summary(fit_cfa_science , fit.measures = T, standardized = T)

# Create the path diagram:
#semPaths(fit_cfa_science, "std", title = F, curvePivot = T, intercepts = F,
#         residuals = F, thresholds = F, label.prop = 1.8)
#title("Path Diagram for the Measurement Model", line = 2)

################# SEM 

structural_science <- '
# Measurement model

Discussions =~ BTBG12G + BTBG12F + BTBG12D;
Experiments =~ BTBS15G + BTBS15F + BTBS15D + BTBS15E + BTBS15H;
Memorizing =~ BTBS15J + BTBS15K + BTBS15I;
Field_Work =~ BTBS15B + BTBS15L;
SCIENCE =~ BSSSCI05 + BSSSCI02 + BSSSCI03 + BSSSCI04 + BSSSCI01;

# Associations

SCIENCE ~ m1*Discussions + m3*Experiments + m4*Memorizing + m5*Field_Work +
m6*Small_Groups + m7*Team_Teaching;

Discussions ~ d1*Small_Groups + d2*Team_Teaching;
Experiments ~ ex1*Small_Groups + ex2*Team_Teaching;
Memorizing ~ me1*Small_Groups + me2*Team_Teaching;
Field_Work ~ fw1*Small_Groups + fw2*Team_Teaching;

# Covariances

Field_Work ~~ Discussions;
Field_Work ~~ Experiments;
Field_Work ~~ Memorizing;
Experiments ~~ Memorizing;
Discussions ~~ Memorizing;
Discussions ~~ Experiments;
BTBS15F ~~ BTBS15H;

### Indirect effects

# Small_Groups

SG__Disc_ind := d1*m1
SG__Exper_ind := ex1*m3
SG__Memor_ind := me1*m4
SG__FldWrk_ind := fw1*m5

# Team_Teaching

TT__Disc_ind := d2*m1
TT__Exper_ind := ex2*m3
TT__Memor_ind := me2*m4
TT__FldWrk_ind := fw2*m5

# Total total

SG__ind := d1*m1 + ex1*m3 + me1*m4 + fw1*m5
SG__tot := m6 + SG__ind

TT__ind := d2*m1 + ex2*m3 + me2*m4 + fw2*m5
TT__tot := m7 + TT__ind
'
fit_structural_science <- sem(structural_science, data = science_timss_sus, sampling.weights = 'TOTWGT')
#summary(fit_structural_science, fit.measures = T, standardized = T, modindices = F)
#modindices(fit_structural_science, minimum.value = 10, sort=TRUE)[1:20,]

# Manually changing the layout
path_test <- semPaths(fit_structural_science, 'std', intercepts = F,
                      residuals = F, thresholds = F, colFactor = 0,
                      label.prop = 1.3, optimizeLatRes = T, mar = c(2,2,2,2),
                      rotation = 2, esize = 2, layout = 'tree2',
                      equalizeManifests = F, DoNotPlot = T)
layout <- path_test$layout
layout[1,] <- c(0.5, -1)
layout[2,] <- c(0.5, -0.8)
layout[3,] <- c(0.5, -0.6)

layout[12,] <- c(0.5, 0.8)
layout[13,] <- c(0.5, 1)

layout[14,] <- c(-0.5, -1)
layout[15,] <- c(-0.5, 1)

layout[25,] <- c(0.5, 0.2)
layout[21,] <- c(0, -1)
layout[22,] <- c(0, -0.25)
layout[23,] <- c(0, 0.15)
layout[24,] <- c(0, 1)

# Generating a vector with colors depending on a significance and sign
st_Sol <- standardizedSolution(fit_structural_science)
obj <- semPlotModel(fit_structural_science)
Pars <- as.data.frame(obj@Pars)
st_Sol$rhs_ <- ifelse(st_Sol$op == '~', st_Sol$lhs, st_Sol$rhs)
st_Sol$lhs_ <- ifelse(st_Sol$op == '~', st_Sol$rhs, st_Sol$lhs)
st_Sol$lhs <- NULL
st_Sol$rhs <- NULL

Pars_st_Sol <- Pars %>% select(lhs, edge, rhs) %>%
  left_join((st_Sol %>% select(lhs_, op, rhs_, pvalue, est.std) %>%
               rename(lhs = lhs_, rhs = rhs_)),
            by = c('lhs', 'rhs'))
Pars_st_Sol$pvalue <- round(as.numeric(Pars_st_Sol$pvalue), 3)
Pars_st_Sol$pvalue[is.na(Pars_st_Sol$pvalue)] <- 0
Pars_st_Sol$edge_col <- ifelse(Pars_st_Sol$est.std > 0 & Pars_st_Sol$pvalue < 0.1 & !Pars_st_Sol$op %in% c('=~', '~~'), 'darkgreen',
                               ifelse(Pars_st_Sol$pvalue > 0.1, 'lightgrey', 
                                      ifelse(Pars_st_Sol$op %in% c('=~', '~~'), 'white', 'red')))
Pars_st_Sol$edge_width <- ifelse(Pars_st_Sol$op != '=~', 3, 0.5)

# Plotting
path_fin <- semPaths(fit_structural_science, 'std', intercepts = F,
            residuals = F, thresholds = F, colFactor = 0,
            optimizeLatRes = F, mar = c(2,2,2,2),
            rotation = 2, esize = 2, layout = layout,
            nodeLabels = c('EXPRESS IDEAS', 'PROBLEM SOLVING', 'CLASSROOM DISCUSSIONS',
                          'INTERPRET DATA', 'PRESENT DATA', 'PLAN EXPERIMENTS', 'CONDUCT EXPERIMENTS', 'USE EVIDENCE TO SUPPORT',
                          'MEMORIZE FACTS', 'USE FORMULAS', 'READ TEXTBOOKS',
                          'OBSERVE PHENOMENA', 'DO FIELD WORK',
                          'Small Groups', 'Team Teaching',
                          'PVSCIENCE1', 'PVSCIENCE2', 'PVSCIENCE3', 'PVSCIENCE4', 'PVSCIENCE5',
                          'Discussion', 'Experimenting', 'Memorizing', 'Field Work',
                          'Science Scores'),
            sizeMan = 16, sizeLat = 16, sizeMan2 = 4, sizeLat2 = 4, edge.label.cex = 1,
            label.cex = 2.2, label.prop = 0, edge.color = Pars_st_Sol$edge_col, 
            DoNotPlot = T)

path_fin$graphAttributes$Edges$width <- Pars_st_Sol$edge_width
path_fin$graphAttributes$Nodes$color <- c(rep('#E6E600FF', 3), rep('#EF8A62', 5), rep('#FFBFFFFF', 3), 
                                          rep('#FDDBC7', 2), rep('#FFFFFFFF', 2),
                                          rep('#D1E5F0', 5), '#E6E600FF', '#EF8A62', '#FFBFFFFF',
                                          '#FDDBC7', '#D1E5F0')
plot(path_fin)
