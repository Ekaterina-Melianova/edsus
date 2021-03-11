# tch-quality_sch-envir1a.R

# Teaching Styles, Teaching Quality, School Environmemt, Students' Beliefs, and Learning Outcomes
# Data pre-processing 

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
library(stargazer)
library(GPArotation)
library(semTools)
library(hrbrthemes)
# t.tests weighted
library(weights)

Sys.setlocale("LC_CTYPE", "russian")



# -----------------------------------------------------------------------------------------------------------------#
### SUS
# -----------------------------------------------------------------------------------------------------------------#



# 1. Teachers

setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/SUS_TIMSS/Data/SUS'))
tch_sus <- read.csv('export_teachers_flat.csv', sep = '\t') %>% 
  select(IDSCHOOL, IDTEACH, IDCLASS, StratNameENG, n24_0, n26_0, CourseName, exper = n3_1)

# Teaching Styles: 
# TRADITIONAL (Everyday uses layouts that support explicit instruction/ presentation AND
# never needs to rearrange tables, chairs or other aspects of the space (e.g. sliding partitions)
# prior to the start of a lesson because a previous user had them in a different position) 
# vs. MODERN 

tch_sus$traditional_style <- ifelse(tch_sus$n24_0 == 5 & tch_sus$n26_0 == 1, 1, 0)
table(tch_sus$traditional_style)/length(tch_sus$traditional_style)

# Defining a subject: Math (1), Science (0)
tch_sus$subject <- ifelse(tch_sus$CourseName == '1 - "Математика"', 'math', 'science')
table(tch_sus$subject)/length(tch_sus$subject)

# Fixing teaching experience
tch_sus$exper <- as.character(tch_sus$exper)
tch_sus$exper <- sub("[а-яА-Я]+", x = tch_sus$exper, replacement = '')
tch_sus$exper <- sub(",", x = tch_sus$exper, replacement = '.')
tch_sus[tch_sus$IDTEACH == 306302, 'exper'] <- 7 # fixing manually 
tch_sus$exper <- as.numeric(tch_sus$exper)

# Selecting the variables of focus
tch_sus <- tch_sus %>% select(IDSCHOOL, IDTEACH, IDCLASS, exper, traditional_style)

# Wide format for teachers by subject
#tch_sus$IDTEACH <- NULL
#tch_sus <- pivot_wider(tch_sus, id_cols = c(IDSCHOOL, IDCLASS),
#                       names_from = subject,
#                       values_from = c(traditional_style, n3_1),
#                       names_repair = 'minimal',
#                       values_fn = list(traditional_style = max,
#                                        n3_1 = mean))

# Merging with students' ids
stu_sus_id <- read.csv('export_students_flat.csv', sep = '\t') %>% 
  select(IDSCHOOL, IDCLASS, 'IDSTUD' = login)

tch_sus <- stu_sus_id %>%
  left_join(tch_sus, by = c('IDSCHOOL', 'IDCLASS'))


# SUS: Averaging by students 

# Function: summarise numeric columns, return first value of non-numeric if we have them in the dataset
summarise_by_col_class <- function(x){
  if(is.numeric(x)|is.integer(x)){
    return(round(median(x, na.rm = T), 0))
  }
  else{
    nth(x, 1)
  }
}
# lapply(1:ncol(tch_sus), function(i){class(tch_sus[,i])}) # checking classes of variables


tch_sus %<>% group_by(IDSCHOOL, IDCLASS, IDSTUD)  %>%
  summarise_all(summarise_by_col_class) %>% ungroup()

# Checking uniqueness of ids
table(duplicated(tch_sus$IDSTUD))

# 2. Students

# Usage of space, comfort, safety, and technology variables
stu_sus <- read.csv('export_students_flat.csv', sep = '\t') %>% 
  select(IDSCHOOL, IDCLASS, 'IDSTUD' = login,
         # Use of space:
         n3_0,
         n3_1,
         n3_2,
         n3_3,
         n3_4,
         n3_5,
         n3_6,
         n3_7,
         n3_8,
         n3_9,
         n3_10,
         n3_11,
         
         n7_0,
         n7_1,
         n7_2,
         n7_3,
         n7_4,
         
         # Comfort: 
         n11_2, n12_2, n14_2,
         
         n15_0,
         n15_1,
         n15_2,
         
         n16_0,
         n16_1,
         n16_2,
         
         n17_0,
         n17_1,
         n17_2,
         
         # Safety:
         n24_0,
         n24_1,
         n24_2,
         n24_3,
         n24_4,
         
         # Technology:
         n25_0,
         n25_1,
         n25_2,
         n25_3,
         n25_4,
         n25_5,
         n25_6,
         n25_7,
         n25_8,
         n25_9
   )

# Missing values 
stu_sus[stu_sus == '-'| stu_sus == ''] <- NA

# Reverse coding
reverse_var_names <- c('n11_2', 'n12_2', 'n14_2',
                       'n15_0', 'n15_1',
                       'n16_0', 'n16_1', 'n16_2',
                       'n17_0', 'n17_1', 'n17_2')
reverse_vars_n <- which(colnames(stu_sus) %in% reverse_var_names)
stu_sus %<>% 
  mutate_at(reverse_vars_n, recode, '1'='4', '2'='3', '3'='2', '4'='1', '5'='1')
stu_sus[,reverse_vars_n] <- sapply(stu_sus[,reverse_vars_n], as.numeric)

# Recoding Not applicable category in a question about safety: averaging based on the remaining categories
n24 <- c('n24_0', 'n24_1', 'n24_2', 'n24_3','n24_4')
for (i in 1:nrow(stu_sus)){
  for (j in n24){
    if (stu_sus[i,j] == 5 & is.na(stu_sus[i,j]) == F) {
      stu_sus[i,j] <- round(mean(stu_sus[i, n24[!n24 %in% j]][!(stu_sus[i, n24[!n24 %in% j]] == 5)], na.rm = T), 0)

    }
    else if (all(is.na(stu_sus[i, n24]))) {
      stu_sus[i,n24] <- NA
    }
  }
}

# Return NA for NaN
stu_sus[is.nan(stu_sus[,n24[1]]), n24[1]] <- NA
stu_sus[is.nan(stu_sus[,n24[2]]), n24[2]] <- NA
stu_sus[is.nan(stu_sus[,n24[3]]), n24[3]] <- NA
stu_sus[is.nan(stu_sus[,n24[4]]), n24[4]] <- NA
stu_sus[is.nan(stu_sus[,n24[5]]), n24[5]] <- NA

# Checking dist
#lapply(n24, function(i){table(stu_sus[,i])})
#lapply(n24, function(i){table(is.nan(stu_sus[,i]))})

####  Creating non-factor learning environment variables:
# - Number of spaces used during lesson time over the last week
# - Number of outside spaces used 1-3 times per month or more often
# - Number of learning tasks for which technology devices were used in a typical week

# Empty variables
stu_sus$n_spaces <- NA 
stu_sus$n_outside_spaces <- NA 
stu_sus$n_tech <- NA 

# Defining series of items
n3 = eval(parse(text = paste0('c(', paste0(" 'n3_", 0:11, "'", collapse = ', '), ')')))
n7 = eval(parse(text = paste0('c(', paste0(" 'n7_", 0:4, "'", collapse = ', '), ')')))
n25 = eval(parse(text = paste0('c(', paste0(" 'n25_", 0:9, "'", collapse = ', '), ')')))

# Filling the empty variables
for (i in 1:nrow(stu_sus)){
  # Summing the number of spaces used ones a week or more often, i.e. !=1
  stu_sus[i, 'n_spaces'] <- as.numeric(sum(!stu_sus[i, n3] == 1, na.rm = T))
  # Summing the number of outside spaces used 1-3 times per month or more often, i.e. !=1
  stu_sus[i, 'n_outside_spaces'] <- as.numeric(sum(!stu_sus[i, n7] == 1, na.rm = T))
  # Summing the number of learning task for which technology devices were used in a typical week
  stu_sus[i, 'n_tech'] <- as.numeric(sum(!stu_sus[i, n25] == 1, na.rm = T))
  
}

# Removing the initial items
stu_sus %<>% select(!c(n3, n7, n25))

# 3. SUS: Teachers + Students
sus <- stu_sus %>%
  left_join(tch_sus, by = c('IDSCHOOL', 'IDCLASS', 'IDSTUD'))

# Remove students without teachers
sus <- sus[!is.na(sus$IDTEACH),]
sus$IDTEACH <- NULL



# -----------------------------------------------------------------------------------------------------------------#
### TIMSS
# -----------------------------------------------------------------------------------------------------------------#



setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/SUS_TIMSS/Data/TIMSS/SPSS'))

# 1. Students' achievements
bstrusm7 <- read.spss('bstrusm7.sav', use.value.labels = F, to.data.frame = T)  %>%
  select(IDSCHOOL_ORI, IDCLASS_ORI, IDSTUD_ORI, IDTEACH_ORI,
         BSMMAT01, BSMMAT02, BSMMAT03, BSMMAT04, BSMMAT05,
         BSSSCI01, BSSSCI02, BSSSCI03, BSSSCI04, BSSSCI05,
         TOTWGT,
         # Math applying, knowing, reasoning
         BSMAPP01,
         BSMAPP02,
         BSMAPP03,
         BSMAPP04,
         BSMAPP05,
         
         BSMKNO01,
         BSMKNO02,
         BSMKNO03,
         BSMKNO04,
         BSMKNO05,
         
         BSMREA01,
         BSMREA02,
         BSMREA03,
         BSMREA04,
         BSMREA05,
         
         # Science applying, knowing, reasoning
         BSSAPP01,
         BSSAPP02,
         BSSAPP03,
         BSSAPP04,
         BSSAPP05,
         
         BSSKNO01,
         BSSKNO02,
         BSSKNO03,
         BSSKNO04,
         BSSKNO05,
         
         BSSREA01,
         BSSREA02,
         BSSREA03,
         BSSREA04,
         BSSREA05
  )

# Unique IDSTUD_ORI
# bstrusm7 <- bstrusm7[!duplicated(bstrusm7$IDSTUD_ORI),]

# 2. Students' SES, wellness, teacher quality, Student beliefs and attitudes
bsgrusm7 <- read.spss('bsgrusm7.sav', use.value.labels = F, to.data.frame = T) %>%
  select(IDSCHOOL_ORI, IDCLASS_ORI, IDSTUD_ORI, ITSEX, BSDAGE, BSBGHER, BSDGHER,
         BSBG13A, BSBG13B, BSBG13C, BSBG13D, BSBG13E,
         
         # MATH Teacher quality
         BSBM17A,
         BSBM17B,
         BSBM17C,
         BSBM17D,
         BSBM17E,
         BSBM17F,
         BSBM17G,
         
         # Oderliness in math
         BSBM18A,
         BSBM18B,
         BSBM18C,
         BSBM18D,
         BSBM18E,
         BSBM18F,
         
         # SCIENCE Teacher quality
         #BSBS23A,
         #BSBS23B,
         #BSBS23C,
         #BSBS23D,
         #BSBS23E,
         #BSBS23F,
         #BSBS23G,
         
         # MATH Student beliefs and attitudes
         BSBM16A,
         BSBM16B,
         BSBM16C,
         BSBM16D,
         BSBM16E,
         BSBM16F,
         BSBM16G,
         BSBM16H,
         BSBM16I,
         
         # MATH Student beliefs and attitudes
         BSBM19A,
         BSBM19B,
         BSBM19C,
         BSBM19D,
         BSBM19E,
         BSBM19F,
         BSBM19G,
         BSBM19H,
         BSBM19I,
         
         # MATH Student beliefs and attitudes
         BSBM20A,
         BSBM20B,
         BSBM20C,
         BSBM20D,
         BSBM20E,
         BSBM20F,
         BSBM20G,
         BSBM20H,
         BSBM20I,
         
         # SCIENCE Student beliefs and attitudes
         #BSBS22A,
         #BSBS22B,
         #BSBS22C,
         #BSBS22D,
         #BSBS22E,
         #BSBS22F,
         #BSBS22G,
         #BSBS22H,
         #BSBS22I,
         
         # SCIENCE Student beliefs and attitudes
         #BSBS24A,
         #BSBS24B,
         #BSBS24C,
         #BSBS24D,
         #BSBS24E,
         #BSBS24F,
         #BSBS24G,
         #BSBS24H,
         
         # SCIENCE Student beliefs and attitudes
         BSBS25A,
         BSBS25B,
         BSBS25C,
         BSBS25D,
         BSBS25E,
         BSBS25F,
         BSBS25G,
         BSBS25H,
         BSBS25I
  
  )

# 3. Combine 1. and 2.
stu_timss <- bstrusm7 %>%
  left_join(bsgrusm7, by = c('IDSCHOOL_ORI', 'IDCLASS_ORI', 'IDSTUD_ORI'))

# 4. Teacher quality/practices

# MATH
btmrusm7_gen <- read.spss('btmrusm7.sav', use.value.labels = F, to.data.frame = T) %>%
  select(IDSCHOOL_ORI, IDTEACH_ORI, BTBG02, BTBG03,
         BTBG12A, BTBG12B, BTBG12C, BTBG12D, BTBG12E, BTBG12F, BTBG12G)

btmrusm7_math <- read.spss('btmrusm7.sav', use.value.labels = F, to.data.frame = T) %>%
  select(IDSCHOOL_ORI, IDTEACH_ORI,
         BTBM15A, BTBM15B, BTBM15C, BTBM15D, BTBM15E, BTBM15F, BTBM15G, BTBM15H)

# SCIENCE
btsrusm7_gen <- read.spss('btsrusm7.sav', use.value.labels = F, to.data.frame = T) %>%
  select(IDSCHOOL_ORI, IDTEACH_ORI, BTBG02, BTBG03,
         BTBG12A, BTBG12B, BTBG12C, BTBG12D, BTBG12E, BTBG12F, BTBG12G)

btsrusm7_science <- read.spss('btsrusm7.sav', use.value.labels = F, to.data.frame = T) %>%
  select(IDSCHOOL_ORI, IDTEACH_ORI,
         BTBS15A, BTBS15B, BTBS15C, BTBS15D, BTBS15E, BTBS15F, BTBS15G,
         BTBS15H, BTBS15I, BTBS15J, BTBS15K, BTBS15L, BTBS15M, BTBS15N)

# MATH + SCIENCE
btmrusm7_btsrusm7_gen <- plyr::rbind.fill(btmrusm7_gen, btsrusm7_gen)
# table(duplicated(btmrusm7_btsrusm7$IDTEACH_ORI))

# 5. TIMSS: Students + Teachers
table(duplicated(stu_timss$IDTEACH_ORI))
stu_tch_timss <- stu_timss %>%
  left_join(btmrusm7_btsrusm7_gen, by = c('IDSCHOOL_ORI', 'IDTEACH_ORI'))%>%
  left_join(btmrusm7_math, by = c('IDSCHOOL_ORI', 'IDTEACH_ORI'))%>%
  left_join(btsrusm7_science, by = c('IDSCHOOL_ORI', 'IDTEACH_ORI'))


# Reverse coding
reverse_var_names <- c('BSBG13A', 'BSBG13B', 'BSBG13C', 'BSBG13D', 'BSBG13E',
                       'BSBM17A', 'BSBM17B', 'BSBM17C', 'BSBM17D', 'BSBM17E',
                       'BSBM17F', 'BSBM17G', 'BSBM20A', 'BSBM20B', 'BSBM20C', 
                       'BSBM20D', 'BSBM20E', 'BSBM20F', 'BSBM20G', 'BSBM20H',
                       'BSBM20I', 'BSBS25A', 'BSBS25B', 'BSBS25C', 'BSBS25D',
                       'BSBS25E', 'BSBS25F', 'BSBS25G', 'BSBS25H', 'BSBS25I',
                       'BTBG12A', 'BTBG12B', 'BTBG12C', 'BTBG12D', 'BTBG12E', 'BTBG12F', 'BTBG12G',
                       'BTBM15A', 'BTBM15B', 'BTBM15C', 'BTBM15D', 'BTBM15E', 'BTBM15F', 'BTBM15G', 'BTBM15H',
                       'BTBS15A', 'BTBS15B', 'BTBS15C', 'BTBS15D', 'BTBS15E', 'BTBS15F', 'BTBS15G',
                       'BTBS15H', 'BTBS15I', 'BTBS15J', 'BTBS15K', 'BTBS15L', 'BTBS15M', 'BTBS15N',
                       'BSBM16A', 'BSBM16D', 'BSBM16E', 'BSBM16F', 'BSBM16G', 'BSBM16H','BSBM16I',
                       'BSBM19A', 'BSBM19D', 'BSBM19F', 'BSBM19G')
reverse_vars_n <- which(colnames(stu_tch_timss) %in% reverse_var_names)
stu_tch_timss %<>% 
  mutate_at(reverse_vars_n, recode, '1'='4', '2'='3', '3'='2', '4'='1')
stu_tch_timss[,reverse_vars_n] <- sapply(stu_tch_timss[,reverse_vars_n], as.numeric)

# TIMSS: Averaging by students

stu_tch_timss$IDTEACH_ORI <- NULL
stu_tch_timss <- stu_tch_timss %>% group_by(IDSCHOOL_ORI, IDCLASS_ORI, IDSTUD_ORI)  %>%
  summarise_all(summarise_by_col_class) %>% ungroup()

# Checking uniqueness of ids
table(duplicated(stu_tch_timss$IDSTUD_ORI))
summary(stu_tch_timss)

# -----------------------------------------------------------------------------------------------------------------#
# Merging all
### TIMSS + SUS: Students

stu_sus_timss <- sus %>%
  left_join(stu_tch_timss, by = c('IDSTUD' = 'IDSTUD_ORI')) %>%
  select(!c(IDSCHOOL_ORI, IDCLASS_ORI))

# Deletion of missing values in scores
stu_sus_timss <- stu_sus_timss[complete.cases(stu_sus_timss[,c('BSMMAT01', 'BSMMAT02', 'BSMMAT03', 
                                                               'BSMMAT04', 'BSMMAT05','BSSSCI01',
                                                               'BSSSCI02', 'BSSSCI03', 'BSSSCI04',
                                                               'BSSSCI05', 'BSBG13E', 'BSBG13B',
                                                               'BSBG13C', 'BSBG13D', 'BSBG13A')]),]
summary(stu_sus_timss)

# -----------------------------------------------------------------------------------------------------------------#
# Dealing with the remained missing values
# -----------------------------------------------------------------------------------------------------------------#

# Exploring the missing data patterns
md.pattern <- md.pattern(stu_sus_timss, plot = F)

# Let us remove observations with more than 10 missing values and impute the rest
rowid_remove <- which(as.numeric(lapply(1:nrow(stu_sus_timss), function(x){sum(is.na(stu_sus_timss[x,]))})) > 10)
stu_sus_timss <- stu_sus_timss[-rowid_remove,]

# Imputation

# Setting bounds for imputations
max <- as.matrix(as.numeric(as.character(apply(stu_sus_timss[,-c(1:3)], 2 , max, na.rm = T))))
min <- as.matrix(as.numeric(as.character(apply(stu_sus_timss[,-c(1:3)], 2 , min, na.rm = T))))

# Ids and Ordinal variables
idvars <- c("IDSCHOOL", "IDCLASS", "IDSTUD")
ords <- names(stu_sus_timss)[!names(stu_sus_timss) %in% 
                               c('exper', 'BSMMAT01', 'BSMMAT02', 'BSMMAT03', 'BSMMAT04',
                                 'BSMMAT05', 'BSSSCI01', 'BSSSCI02', 'BSSSCI03', 'BSSSCI04',
                                 'BSSSCI05', 'BSBG13E', 'BSBG13B',  'BSBG13C', 'BSBG13D', 
                                 'BSBG13A', 'TOTWGT', 'BSDAGE', 'BSBGHER',
                                 'BSMAPP01','BSMAPP02', 'BSMAPP03', 'BSMAPP04', 'BSMAPP05',
                                 'BSMKNO01', 'BSMKNO02',  'BSMKNO03', 'BSMKNO04', 'BSMKNO05',
                                 'BSMREA01',  'BSMREA02', 'BSMREA03', 'BSMREA04','BSMREA05',
                                 'BSSAPP01', 'BSSAPP02', 'BSSAPP03', 'BSSAPP04', 'BSSAPP05',
                                 'BSSKNO01', 'BSSKNO02', 'BSSKNO03','BSSKNO04','BSSKNO05',
                                 'BSSREA01','BSSREA02', 'BSSREA03', 'BSSREA04','BSSREA05',
                                 idvars)]

# Running imputation
set.seed(123)
amelia_res <- amelia(as.data.frame(stu_sus_timss), m = 10, idvars = idvars,
                     bounds = cbind(4:ncol(stu_sus_timss), min, max),
                     ords = ords)
imputed <- amelia_res$imputations$imp7

# Checking the distribution of ordinal variables
# lapply(ords, function(i){table(imputed[,i])}) # looks fine

# New imputed dataset
stu_sus_timss <- imputed

# -----------------------------------------------------------------------------------------------------------------#
### Outcome variables: Wellness index and achievements 
# -----------------------------------------------------------------------------------------------------------------#

# Math
cfa_math_scores <- '
MATH =~ BSMMAT01 + BSMMAT02 + BSMMAT03 + BSMMAT04 + BSMMAT05
'
fit_cfa_math_scores  <- cfa(cfa_math_scores, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT')
#summary(fit_cfa_math_scores, fit.measures = T, standardized = T)
stu_sus_timss$math_scores <- as.numeric(lavPredict(fit_cfa_math_scores) * sd(stu_sus_timss$BSMMAT01) + 
                                           mean(stu_sus_timss$BSMMAT01))
###########
# Science
cfa_science_scores <- '
SCIENCE =~ BSSSCI02 + BSSSCI01 + BSSSCI03 + BSSSCI04 + BSSSCI05
'
fit_cfa_science_scores  <- cfa(cfa_science_scores, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT')
#summary(fit_cfa_science_scores, fit.measures = T, standardized = T)
stu_sus_timss$science_scores <- as.numeric(lavPredict(fit_cfa_science_scores) * sd(stu_sus_timss$BSSSCI02) +
                                                 mean(stu_sus_timss$BSSSCI02))
###########
# Math Applying
cfa_math_appl_scores <- '
APPLYING_MATH =~ BSMAPP01 + BSMAPP02 + BSMAPP03 + BSMAPP04 + BSMAPP05 
'
fit_cfa_math_appl_scores  <- cfa(cfa_math_appl_scores, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT')
#summary(fit_cfa_math_appl_scores, fit.measures = T, standardized = T)
stu_sus_timss$math_appl_scores <- as.numeric(lavPredict(fit_cfa_math_appl_scores) * sd(stu_sus_timss$BSMAPP01) + 
                                          mean(stu_sus_timss$BSMAPP01))
###########
# Science Applying
cfa_science_appl_scores <- '
APPLYING_SCIENCE =~ BSSAPP01 + BSSAPP02 + BSSAPP03 + BSSAPP04 + BSSAPP05 
'
fit_cfa_science_appl_scores  <- cfa(cfa_science_appl_scores, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT')
#summary(fit_cfa_science_appl_scores, fit.measures = T, standardized = T)
stu_sus_timss$science_appl_scores <- as.numeric(lavPredict(fit_cfa_science_appl_scores) * sd(stu_sus_timss$BSSAPP01) + 
                                               mean(stu_sus_timss$BSSAPP01))
###########
# Math Reasoning
cfa_math_reason_scores <- '
REASONING_MATH =~ BSMREA01 + BSMREA02 + BSMREA03 + BSMREA04 + BSMREA05 
'
fit_cfa_math_reason_scores  <- cfa(cfa_math_reason_scores, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT')
#summary(fit_cfa_math_reason_scores, fit.measures = T, standardized = T)
stu_sus_timss$math_reason_scores <- as.numeric(lavPredict(fit_cfa_math_reason_scores) * sd(stu_sus_timss$BSMREA01) + 
                                               mean(stu_sus_timss$BSMREA01))
###########
# Science Reasoning
cfa_science_reason_scores <- '
REASONING_SCIENCE =~ BSSREA01 + BSSREA02 + BSSREA03 + BSSREA04 + BSSREA05 
'
fit_cfa_science_reason_scores  <- cfa(cfa_science_reason_scores, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT')
#summary(fit_cfa_science_reason_scores, fit.measures = T, standardized = T)
stu_sus_timss$science_reason_scores <- as.numeric(lavPredict(fit_cfa_science_reason_scores) * sd(stu_sus_timss$BSSREA01) + 
                                                  mean(stu_sus_timss$BSSREA01))

###########
# Math Knowing
cfa_math_know_scores <- '
KNOWING_MATH =~ BSMKNO01 + BSMKNO02 + BSMKNO03 + BSMKNO04 + BSMKNO05
'
fit_cfa_math_know_scores  <- cfa(cfa_math_know_scores, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT')
#summary(fit_cfa_science_know_scores, fit.measures = T, standardized = T)
stu_sus_timss$math_know_scores <- as.numeric(lavPredict(fit_cfa_math_know_scores) * sd(stu_sus_timss$BSMKNO01) + 
                                                  mean(stu_sus_timss$BSMKNO01))

###########
# Science Knowing
cfa_science_know_scores <- '
KNOWING_SCIENCE =~ BSSKNO01 + BSSKNO02 + BSSKNO03 + BSSKNO04 + BSSKNO05 
'
fit_cfa_science_know_scores  <- cfa(cfa_science_know_scores, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT')
#summary(fit_cfa_science_know_scores, fit.measures = T, standardized = T)
stu_sus_timss$science_know_scores <- as.numeric(lavPredict(fit_cfa_science_know_scores) * sd(stu_sus_timss$BSSKNO01) + 
                                                    mean(stu_sus_timss$BSSKNO01))

###########
# Wellness
cfa_wellness <- '
WELLNESS =~ BSBG13E + BSBG13B + BSBG13C + BSBG13D + BSBG13A
'
fit_cfa_wellness  <- cfa(cfa_wellness, data = stu_sus_timss, std.lv = T, sampling.weights = 'TOTWGT')
#summary(fit_cfa_wellness, fit.measures = T, standardized = T)
stu_sus_timss$wellness <- as.numeric(lavPredict(fit_cfa_wellness) * sd(stu_sus_timss$BSBG13E) +
                                             mean(stu_sus_timss$BSBG13E))


########### t.test comparisons in outcomes by teaching styles (modern vs. traditional) ########### 

table(stu_sus_timss$traditional_style)/length(stu_sus_timss$traditional_style)

# Math
wtd.t.test(stu_sus_timss$math_scores[stu_sus_timss$traditional_style == 1],
           stu_sus_timss$math_scores[stu_sus_timss$traditional_style == 0],
           stu_sus_timss$TOTWGT[stu_sus_timss$traditional_style == 1],
           stu_sus_timss$TOTWGT[stu_sus_timss$traditional_style == 0],
           samedata = F)
# Science
wtd.t.test(stu_sus_timss$science_scores[stu_sus_timss$traditional_style == 1],
           stu_sus_timss$science_scores[stu_sus_timss$traditional_style == 0],
           stu_sus_timss$TOTWGT[stu_sus_timss$traditional_style == 1],
           stu_sus_timss$TOTWGT[stu_sus_timss$traditional_style == 0],
           samedata = F)

# Wellness
wtd.t.test(stu_sus_timss$wellness[stu_sus_timss$traditional_style == 1],
           stu_sus_timss$wellness[stu_sus_timss$traditional_style == 0],
           stu_sus_timss$TOTWGT[stu_sus_timss$traditional_style == 1],
           stu_sus_timss$TOTWGT[stu_sus_timss$traditional_style == 0],
           samedata = F)

# Math Reasoning
wtd.t.test(stu_sus_timss$math_reason_scores[stu_sus_timss$traditional_style == 1],
           stu_sus_timss$math_reason_scores[stu_sus_timss$traditional_style == 0],
           stu_sus_timss$TOTWGT[stu_sus_timss$traditional_style == 1],
           stu_sus_timss$TOTWGT[stu_sus_timss$traditional_style == 0],
           samedata = F)

# Science Reasoning
wtd.t.test(stu_sus_timss$science_reason_scores[stu_sus_timss$traditional_style == 1],
           stu_sus_timss$science_reason_scores[stu_sus_timss$traditional_style == 0],
           stu_sus_timss$TOTWGT[stu_sus_timss$traditional_style == 1],
           stu_sus_timss$TOTWGT[stu_sus_timss$traditional_style == 0],
           samedata = F)

# Math Applying
wtd.t.test(stu_sus_timss$math_appl_scores[stu_sus_timss$traditional_style == 1],
           stu_sus_timss$math_appl_scores[stu_sus_timss$traditional_style == 0],
           stu_sus_timss$TOTWGT[stu_sus_timss$traditional_style == 1],
           stu_sus_timss$TOTWGT[stu_sus_timss$traditional_style == 0],
           samedata = F)

# Science Applying
wtd.t.test(stu_sus_timss$science_appl_scores[stu_sus_timss$traditional_style == 1],
           stu_sus_timss$science_appl_scores[stu_sus_timss$traditional_style == 0],
           stu_sus_timss$TOTWGT[stu_sus_timss$traditional_style == 1],
           stu_sus_timss$TOTWGT[stu_sus_timss$traditional_style == 0],
           samedata = F)

# Math Knowing
wtd.t.test(stu_sus_timss$math_know_scores[stu_sus_timss$traditional_style == 1],
           stu_sus_timss$math_know_scores[stu_sus_timss$traditional_style == 0],
           stu_sus_timss$TOTWGT[stu_sus_timss$traditional_style == 1],
           stu_sus_timss$TOTWGT[stu_sus_timss$traditional_style == 0],
           samedata = F)

# Science Knowing
wtd.t.test(stu_sus_timss$science_know_scores[stu_sus_timss$traditional_style == 1],
           stu_sus_timss$science_know_scores[stu_sus_timss$traditional_style == 0],
           stu_sus_timss$TOTWGT[stu_sus_timss$traditional_style == 1],
           stu_sus_timss$TOTWGT[stu_sus_timss$traditional_style == 0],
           samedata = F)



