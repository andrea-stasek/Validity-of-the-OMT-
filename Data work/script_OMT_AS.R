##########################################################################################
###                                                                                    ###
###                      Implicit Motivation of Online Gamers:                         ###
###                      Properties of the Operant Motive Test                         ###
###                                                                                    ###    
###                   Andrea Stasek, Lukas Blinka, Dita Sirinkova                      ###
###                                 IVDMR, Brno, 2022                                  ###
###                                                                                    ###
##########################################################################################

# code written by Andrea Stasek & Hynek Cigler


## Contents ----
# 1) Libraries
# 2) Data preparation
# 3) Descriptive statistics
# 4) Matrix preparation for analysis

##########################################################################################
## ---- 1) LIBRARIES ---------------------------------------------------------------------
##########################################################################################

library(readr) # loading the data
library(psych) # descriptive stats
library(epiDisplay) # freq tables
library(gmodels) # freq tables
library(dplyr) # sample extraction
library(rstatix) # summary stats tables
library(lavaan) # factor analysis
library(lavaanPlot) # path model
library(semPlot) # path model
library(xlsx) # export to Excel
library(mirt) # IRT

citation("")

##########################################################################################
## ---- 2) DATA PREPARATION --------------------------------------------------------------
##########################################################################################

setwd("C:/Users/428213/OneDrive - MUNI/DISK/Czech Gamers/studie/2022 OMT motivace/data")

set.seed(963)

### Loading the data ----

#### GAMING SAMPLE - our measurement 2022 ####
data_merged <- read_delim("survey_and_OMT.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
  # for sample descriptives

data_OMT <- read_delim("OMT.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
  # for analysis

#### STUDENT SAMPLE - Nicola Baumann's measurement 2017-21 ####
data_NB <- read_delim("cizí data/data_NB.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

### Renaming and recoding variables ----

#### GAMING SAMPLE ####
##### OMT ----

###### recoding motive variables----

data_OMT_rc <- data_OMT |> mutate(OMT_1_L = recode(OMT_1_L, '1' = 'Affiliation',
                                                            '2' = 'Achievement',
                                                            '3' = 'Power',
                                                            '0' = '0'),
                                  OMT_2_L = recode(OMT_2_L, '1' = 'Affiliation',
                                                   '2' = 'Achievement',
                                                   '3' = 'Power',
                                                   '0' = '0'),
                                  OMT_3_L = recode(OMT_3_L, '1' = 'Affiliation',
                                                   '2' = 'Achievement',
                                                   '3' = 'Power',
                                                   '0' = '0'),
                                  OMT_4_L = recode(OMT_4_L, '1' = 'Affiliation',
                                                   '2' = 'Achievement',
                                                   '3' = 'Power',
                                                   '0' = '0'),
                                  OMT_5_L = recode(OMT_5_L, '1' = 'Affiliation',
                                                   '2' = 'Achievement',
                                                   '3' = 'Power',
                                                   '0' = '0'),
                                  OMT_6_L = recode(OMT_6_L, '1' = 'Affiliation',
                                                   '2' = 'Achievement',
                                                   '3' = 'Power',
                                                   '0' = '0'),
                                  OMT_7_L = recode(OMT_7_L, '1' = 'Affiliation',
                                                   '2' = 'Achievement',
                                                   '3' = 'Power',
                                                   '0' = '0'),
                                  OMT_8_L = recode(OMT_8_L, '1' = 'Affiliation',
                                                   '2' = 'Achievement',
                                                   '3' = 'Power',
                                                   '0' = '0'),
                                  OMT_9_L = recode(OMT_9_L, '1' = 'Affiliation',
                                                   '2' = 'Achievement',
                                                   '3' = 'Power',
                                                   '0' = '0'),
                                  OMT_10_L = recode(OMT_10_L, '1' = 'Affiliation',
                                                   '2' = 'Achievement',
                                                   '3' = 'Power',
                                                   '0' = '0'),
                                  OMT_11_L = recode(OMT_11_L, '1' = 'Affiliation',
                                                   '2' = 'Achievement',
                                                   '3' = 'Power',
                                                   '0' = '0'),
                                  OMT_12_L = recode(OMT_12_L, '1' = 'Affiliation',
                                                   '2' = 'Achievement',
                                                   '3' = 'Power',
                                                   '0' = '0'),
                                  OMT_13_L = recode(OMT_13_L, '1' = 'Affiliation',
                                                   '2' = 'Achievement',
                                                   '3' = 'Power',
                                                   '0' = '0'),
                                  OMT_14_L = recode(OMT_14_L, '1' = 'Affiliation',
                                                   '2' = 'Achievement',
                                                   '3' = 'Power',
                                                   '0' = '0'),
                                  OMT_15_L = recode(OMT_15_L, '1' = 'Affiliation',
                                                   '2' = 'Achievement',
                                                   '3' = 'Power',
                                                   '0' = '0'))

###### frequency of motive per respondent ----

OMT_L_cols <- data_OMT_rc[,c(19,21,23,25,27,29,31,33,35,37,39,41,43,45,47)]

####### absolute frequencies ----
data_OMT_rc$f_0 <- rowSums(OMT_L_cols == "0", na.rm = T) 
data_OMT_rc$f_Aff <- rowSums(OMT_L_cols == "Affiliation", na.rm = T) 
data_OMT_rc$f_Ach <- rowSums(OMT_L_cols == "Achievement", na.rm = T) 
data_OMT_rc$f_Pow <- rowSums(OMT_L_cols == "Power", na.rm = T) 
data_OMT_rc$f_non0 <- rowSums(OMT_L_cols != "0", na.rm = T) 

###### deleting cases with > 5 zero codings ----

data_OMT_drop <- data_OMT_rc[data_OMT_rc$f_non0 >= 10, ]
  # n = 689 remaining

###### relative frequencies ----
data_OMT_drop$perc_0 <- (data_OMT_drop$f_0/data_OMT_drop$f_non0)*100
data_OMT_drop$perc_Aff <- (data_OMT_drop$f_Aff/data_OMT_drop$f_non0)*100
data_OMT_drop$perc_Ach <- (data_OMT_drop$f_Ach/data_OMT_drop$f_non0)*100
data_OMT_drop$perc_Pow <- (data_OMT_drop$f_Pow/data_OMT_drop$f_non0)*100




### STUDENT SAMPLE ####

# how much % of NAs per OMT item item?
perc_miss <- function(x) {sum(is.na(x))/length(x)*100}
apply(data_NB[4:23], 2, perc_miss) # original items

OMT_NB_cols <- data_NB[,4:23]

####### absolute frequencies ----
data_NB$f_0 <- rowSums(OMT_NB_cols == "0", na.rm = T) 
data_NB$f_Aff <- rowSums(OMT_NB_cols == "1", na.rm = T) 
data_NB$f_Ach <- rowSums(OMT_NB_cols == "2", na.rm = T) 
data_NB$f_Pow <- rowSums(OMT_NB_cols == "3", na.rm = T)
data_NB$f_Fre <- rowSums(OMT_NB_cols == "4", na.rm = T) 
data_NB$f_non0 <- rowSums(OMT_NB_cols != "0", na.rm = T) 

###### deleting cases with > 5 zero codings ----

data_OMT_NB_drop <- data_NB[data_NB$f_non0 >= 10, ]
# n = 1168 remaining


###### relative frequencies ----
data_OMT_NB_drop$perc_0 <- (data_OMT_NB_drop$f_0/data_OMT_NB_drop$f_non0)*100
data_OMT_NB_drop$perc_Aff <- (data_OMT_NB_drop$f_Aff/data_OMT_NB_drop$f_non0)*100
data_OMT_NB_drop$perc_Ach <- (data_OMT_NB_drop$f_Ach/data_OMT_NB_drop$f_non0)*100
data_OMT_NB_drop$perc_Pow <- (data_OMT_NB_drop$f_Pow/data_OMT_NB_drop$f_non0)*100
data_OMT_NB_drop$perc_Fre <- (data_OMT_NB_drop$f_Fre/data_OMT_NB_drop$f_non0)*100


##########################################################################################
## ---- 3) DESCRIPTIVE STATISTICS --------------------------------------------------------
##########################################################################################

### GAMING SAMPLE ####

#### Demographic variables ---- NOT WORKING _________________________________________________________


###### merged survey data preparation for demographic descriptives

OMT_L_merged_cols <- data_merged[,c(43,45,47,49,51,53,55,57,59,61,63,65,67,69,71)]

data_merged$f_0 <- rowSums(OMT_L_merged_cols == "0", na.rm = T) 
data_merged$f_Aff <- rowSums(OMT_L_merged_cols == "1", na.rm = T) 
data_merged$f_Ach <- rowSums(OMT_L_merged_cols == "2", na.rm = T) 
data_merged$f_Pow <- rowSums(OMT_L_merged_cols == "3", na.rm = T) 
data_merged$f_non0 <- rowSums(OMT_L_merged_cols != "0", na.rm = T) 

###### deleting cases with > 5 zero codings ----

data_merged_drop <- data_merged[data_merged$f_non0 >= 10, ]
  # n = 532 remaining

##### age ----

data_merged_drop |> 
  get_summary_stats(age, type = 'full')
hist(data_merged_drop$age)

##### gender ----

tab1(data_merged_drop$gender, graph = F) 
# 1 = man, 2 = woman, 3 = neither category suits me, 4 = I do not want to answer

##### socio-economical status ----

tab1(data_merged_drop$SES, graph = F)
# 1 = with much difficulties, 5 = very well

##### education ----

tab1(data_merged_drop$educ, graph = F)
# 1 = currently in high school, 2 = high school graduated, 3 = currently in college,
# 4 = college graduated, 5 = other

#####  partneship ----

tab1(data_merged_drop$partner, graph = F)
# 1 = single, 2 = dating, 3 = in a relationship, not living together, 
# 4 = in a relationship, living together

##### (non-)professional player ----

tab1(data_merged_drop$prof, graph = F)
# 1 = professional, 2 = not professional

#### STUDENT SAMPLE ####
describe(data_OMT_NB_drop$Age)

#### Main variables ----

##### GAMING SAMPLE ####

###### item descriptives ----
tab1(data_OMT_drop$OMT_1_L, graph = F)
tab1(data_OMT_drop$OMT_2_L, graph = F)
tab1(data_OMT_drop$OMT_3_L, graph = F)
tab1(data_OMT_drop$OMT_4_L, graph = F)
tab1(data_OMT_drop$OMT_5_L, graph = F)
tab1(data_OMT_drop$OMT_6_L, graph = F)
tab1(data_OMT_drop$OMT_7_L, graph = F)
tab1(data_OMT_drop$OMT_8_L, graph = F)
tab1(data_OMT_drop$OMT_9_L, graph = F)
tab1(data_OMT_drop$OMT_10_L, graph = F)
tab1(data_OMT_drop$OMT_11_L, graph = F)
tab1(data_OMT_drop$OMT_12_L, graph = F)
tab1(data_OMT_drop$OMT_13_L, graph = F)
tab1(data_OMT_drop$OMT_14_L, graph = F)
tab1(data_OMT_drop$OMT_15_L, graph = F)


  # Aff/Ach/Pow/zero shares in each item (picture)
  # we can see deviation from/congruence with expected motive measured in each item

###### motive descriptives ----

data_OMT_drop |> 
  get_summary_stats(perc_Aff, perc_Ach, perc_Pow, perc_0, 
                         type = 'full')
  # mean Aff, Ach, and Pow in GAMING SAMPLE __________________________________________________

##### STUDENT SAMPLE ####

###### item descriptives ----
tab1(data_OMT_NB_drop$OMT_NB_1, graph = F)
tab1(data_OMT_NB_drop$OMT_NB_2, graph = F)
tab1(data_OMT_NB_drop$OMT_NB_3, graph = F)
tab1(data_OMT_NB_drop$OMT_NB_4, graph = F)
tab1(data_OMT_NB_drop$OMT_NB_5, graph = F)
tab1(data_OMT_NB_drop$OMT_NB_6, graph = F)
tab1(data_OMT_NB_drop$OMT_NB_7, graph = F)
tab1(data_OMT_NB_drop$OMT_NB_8, graph = F)
tab1(data_OMT_NB_drop$OMT_NB_9, graph = F)
tab1(data_OMT_NB_drop$OMT_NB_10, graph = F)
tab1(data_OMT_NB_drop$OMT_NB_11, graph = F)
tab1(data_OMT_NB_drop$OMT_NB_12, graph = F)
tab1(data_OMT_NB_drop$OMT_NB_13, graph = F)
tab1(data_OMT_NB_drop$OMT_NB_14, graph = F)
tab1(data_OMT_NB_drop$OMT_NB_15, graph = F)
tab1(data_OMT_NB_drop$OMT_NB_16, graph = F)
tab1(data_OMT_NB_drop$OMT_NB_17, graph = F)
tab1(data_OMT_NB_drop$OMT_NB_18, graph = F)
tab1(data_OMT_NB_drop$OMT_NB_19, graph = F)
tab1(data_OMT_NB_drop$OMT_NB_20, graph = F)

###### motive descriptives ----

data_OMT_NB_drop |> 
  get_summary_stats(perc_Aff, perc_Ach, perc_Pow, perc_Fre, perc_0, 
                    type = 'full')
    # mean Aff, Ach, and Pow in STUDENT SAMPLE __________________________________________________

##########################################################################################
## ---- 4) MATRIX PREPARATION FOR ANALYSIS -----------------------------------------------
##########################################################################################

OMT_L_drop <- data_OMT_drop[,c(19,21,23,25,27,29,31,33,35,37,39,41,43,45,47)]

OMT_items_gamers <- na.omit(as.data.frame(OMT_L_drop[, 1:15]))
# sample droped to n = 676

OMT_items_gamers <- OMT_items_gamers |> mutate(OMT_1_L = recode(OMT_1_L, 'Affiliation' = '1',
                                                                'Achievement' = '2',
                                                                'Power' = '3',
                                                                '0' = '0'),
                                               OMT_2_L = recode(OMT_2_L, 'Affiliation' = '1',
                                                                'Achievement' = '2',
                                                                'Power' = '3',
                                                                '0' = '0'),
                                               OMT_3_L = recode(OMT_3_L, 'Affiliation' = '1',
                                                                'Achievement' = '2',
                                                                'Power' = '3',
                                                                '0' = '0'),
                                               OMT_4_L = recode(OMT_4_L, 'Affiliation' = '1',
                                                                'Achievement' = '2',
                                                                'Power' = '3',
                                                                '0' = '0'),
                                               OMT_5_L = recode(OMT_5_L, 'Affiliation' = '1',
                                                                'Achievement' = '2',
                                                                'Power' = '3',
                                                                '0' = '0'),
                                               OMT_6_L = recode(OMT_6_L, 'Affiliation' = '1',
                                                                'Achievement' = '2',
                                                                'Power' = '3',
                                                                '0' = '0'),
                                               OMT_7_L = recode(OMT_7_L, 'Affiliation' = '1',
                                                                'Achievement' = '2',
                                                                'Power' = '3',
                                                                '0' = '0'),
                                               OMT_8_L = recode(OMT_8_L, 'Affiliation' = '1',
                                                                'Achievement' = '2',
                                                                'Power' = '3',
                                                                '0' = '0'),
                                               OMT_9_L = recode(OMT_9_L, 'Affiliation' = '1',
                                                                'Achievement' = '2',
                                                                'Power' = '3',
                                                                '0' = '0'),
                                               OMT_10_L = recode(OMT_10_L, 'Affiliation' = '1',
                                                                 'Achievement' = '2',
                                                                 'Power' = '3',
                                                                 '0' = '0'),
                                               OMT_11_L = recode(OMT_11_L, 'Affiliation' = '1',
                                                                 'Achievement' = '2',
                                                                 'Power' = '3',
                                                                 '0' = '0'),
                                               OMT_12_L = recode(OMT_12_L, 'Affiliation' = '1',
                                                                 'Achievement' = '2',
                                                                 'Power' = '3',
                                                                 '0' = '0'),
                                               OMT_13_L = recode(OMT_13_L, 'Affiliation' = '1',
                                                                 'Achievement' = '2',
                                                                 'Power' = '3',
                                                                 '0' = '4'),
                                               OMT_14_L = recode(OMT_14_L, 'Affiliation' = '1',
                                                                 'Achievement' = '2',
                                                                 'Power' = '3',
                                                                 '0' = '4'),
                                               OMT_15_L = recode(OMT_15_L, 'Affiliation' = '1',
                                                                 'Achievement' = '2',
                                                                 'Power' = '3',
                                                                 '0' = '4'))

OMT_items_gamers[,c(1:15)] <- lapply(OMT_items_gamers[,c(1:15)], as.numeric)

OMT_items_students <- na.omit(as.data.frame(data_OMT_NB_drop[, 4:23]))

### Save output
save(OMT_items_gamers, OMT_items_students, file="data_for_MNRM.RData")


### visualizing a full-information factor model

colnames(OMT_items_gamers)
fa_paths <- '
AFF =~ NA*OMT_1_L + OMT_2_L + OMT_3_L + OMT_4_L + OMT_5_L + OMT_6_L + OMT_7_L + OMT_8_L + 
       OMT_9_L + OMT_10_L + OMT_11_L + OMT_12_L + OMT_13_L + OMT_14_L + OMT_15_L
ACH =~ NA*OMT_1_L + OMT_2_L + OMT_3_L + OMT_4_L + OMT_5_L + OMT_6_L + OMT_7_L + OMT_8_L + 
       OMT_9_L + OMT_10_L + OMT_11_L + OMT_12_L + OMT_13_L + OMT_14_L + OMT_15_L
POW =~ NA*OMT_1_L + OMT_2_L + OMT_3_L + OMT_4_L + OMT_5_L + OMT_6_L + OMT_7_L + OMT_8_L + 
       OMT_9_L + OMT_10_L + OMT_11_L + OMT_12_L + OMT_13_L + OMT_14_L + OMT_15_L
ZERO =~ NA*OMT_1_L + OMT_2_L + OMT_3_L + OMT_4_L + OMT_5_L + OMT_6_L + OMT_7_L + OMT_8_L + 
        OMT_9_L + OMT_10_L + OMT_11_L + OMT_12_L + OMT_13_L + OMT_14_L + OMT_15_L

AFF ~~ ACH
AFF ~~ POW
ACH ~~ POW
ZERO ~~ 0*AFF
ZERO ~~ 0*ACH
ZERO ~~ 0*POW
'

fa_model <- cfa(model = fa_paths, OMT_items_gamers[,c(1:15)], sample.nobs = 1000, )
fa_visual <- semPaths(fa_model, mar = c(20, 2, 20, 2),
                      edge.label.cex = 0.9, label.cex = 1, sizeMan = 7, sizeMan2 = 3, nCharNodes = 4,
                      curvePivot = F, thresholds = F, intercepts = F, residuals = F, exoVar = F,
                      layout = "tree3", curve = 1.5, fixedStyle = c("white","0"))
