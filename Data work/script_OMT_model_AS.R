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


# Contents ----
# 1) Objects and data
# 2) Nominal Response Models

##########################################################################################
# ---- 1) Objects, data, and libraries ---------------------------------------------------
##########################################################################################

setwd("C:/Users/428213/OneDrive - MUNI/DISK/Czech Gamers/studie/2022 OMT motivace/data")

set.seed(963)

load(file="data_for_MNRM.RData")

library(mirt) # IRT model


##########################################################################################
# ---- 2) NOMINAL RESPONSE MODELS --------------------------------------------------------
##########################################################################################

  # procedure from Falk & Cai, 2020


## Models 1G a 1S - ZERO factor not specified (HYNEK) ####################################

# without specifying ZERO factor, 
# the conditional probability of zero-coding of a response is equal for all respondents


### 1G ###################################################################################

#### specification ----
sf_1G <- list()
for(i in 1:15){
  sf_1G[[i]]<-matrix(
    c(1, 0, 0, 0, # sf(AFF)
      0, 1, 0, 0, # sf(ACH)
      0, 0, 1, 0 # sf(POW)
    ), 4, 3)
}

model_1G <- "
AFF = 1-15
ACH = 1-15
POW = 1-15

COV = AFF*ACH*POW
"

#### estimation ----
fit_1G <- mirt(OMT_items_gamers, model_1G, gpcm_mats = sf_1G, 
                      itemtype="gpcm", method = "MHRM", verbose = T, 
                      technical = list(NCYCLES = 10000), SE = T, calcNull = T)
# Log-likelihood = -8693.236, SE = 0.042
# Estimated parameters: 93 
# AIC = 17572.47
# BIC = 17992.48; SABIC = 17697.19
# G2 (1073741730) = 8604.3, p = 1
# RMSEA = 0, CFI = 0, TLI = 0

coef_1G <- coef(fit_1G)
load_corrs_1G <- summary(fit_1G)

itemplot(fit_1G, item = 1)
itemplot(fit_1G, item = 1, degrees = c(45, 45, 90))

fs_rel_1G <- fscores(fit_1G, returnER = T)
#       AFF       ACH       POW 
# 0.2182827 0.2433192 0.2350685

itemfit_1G <- itemfit(fit_1G)


fitM2_1G <- M2(fit_1G, QMC = T, type = "C2")
#             M2 df        p RMSEA RMSEA_5   RMSEA_95      SRMSR      TLI CFI
# stats 45.59464 57 0.861159     0       0 0.01330596 0.02973765 2.376516   1
# OVERFIT! who would have thought huh

############################################################################### HYNEK END 

### 1S ###################################################################################

#### specification ----
sf_1S <- list()
for(j in 1:20){
  sf_1S[[j]]<-matrix(
    c(1, 0, 0, 0, 0, # sf(AFF)
      0, 1, 0, 0, 0, # sf(ACH)
      0, 0, 1, 0, 0, # sf(POW)
      0, 0, 0, 1, 0, # sf(AUT)
    ), 5, 4)
}

model_1S <- "
AFF = 1-20
ACH = 1-20
POW = 1-20
AUT = 1-20

COV = AFF*ACH*POW*AUT
"

#### estimation ----
fit_1S <- mirt(OMT_items_students, model_1S, gpcm_mats = sf_1S, 
               itemtype="gpcm", method = "MHRM", verbose = T, 
               technical = list(NCYCLES = 10000), SE = T, calcNull = T)
coef_1S <- coef(fit_1S)
load_corrs_1S <- summary(fit_1S)

itemplot(fit_1S, item = 1)
itemplot(fit_1S, item = 1, degrees = c(45, 45, 90))

fs_rel_1S <- fscores(fit_1S, returnER = T)
itemfit_1S <- itemfit(fit_1S)
fs_1S <- fscores(fit_1S, method = "MAP", full.scores = T, 
                 QMC = T, full.scores.SE = F)

fitM2_1S <- M2(fit_1S, QMC = T, type = "C2")
#             M2  df          p      RMSEA     RMSEA_5   RMSEA_95      SRMSR       TLI       CFI
# stats 130.4437 104 0.04065742 0.01476078 0.003303725 0.02217194 0.03172809 0.8635522 0.9253128
# not bad!

## Model 2G and 2S - ZERO coding not included, ZEROes as NAs -----------------------------

### zero codes become NAs
OMT_items_gamers2G <- OMT_items_gamers
OMT_items_gamers2G[OMT_items_gamers == 4] <- NA
OMT_items_gamers2G[,c(1:15)] <- lapply(OMT_items_gamers2G[,c(1:15)], as.numeric)

OMT_items_students2S <- OMT_items_students
OMT_items_students2S[OMT_items_students == 0] <- NA
OMT_items_students2S[,c(1:20)] <- lapply(OMT_items_students2S[,c(1:20)], as.numeric)

### 2G ###################################################################################

#### specification ----

# Custom scoring functions for AFF, ACH, and POW
sf_2G <- list()
for(k in 1:15){
  sf_2G[[k]]<-matrix(
    c(1, 0, 0, # sf(AFF)
      0, 1, 0, # sf(ACH)
      0, 0, 1, # sf(POW)
    ), 3, 3)
}

model_2G <- "
AFF = 1-15
ACH = 1-15
POW = 1-15
COV = AFF*ACH*POW
"

#### estimation ----
fit_2G <- mirt(OMT_items_gamers2G, model_2G, gpcm_mats = sf_2G, 
                     itemtype="gpcm", method = "MHRM", verbose = T, 
                     technical = list(NCYCLES = 10000), SE = T, calcNull = T)
# imputation
fs_for_imp2G <- fscores(fit_2G, method = "MAP", full.scores = T, scores.only = T)
OMT_items_gamers_imp2G <- imputeMissing(fit_2G, fs_for_imp2G)

# imputed data model 
fit_2G_imp <- mirt(OMT_items_gamers_imp2G, model_2G, gpcm_mats = sf_2G, 
                         itemtype="gpcm", method = "MHRM", verbose = T, 
                         technical = list(NCYCLES = 10000), SE = T, calcNull = T)
# Log-likelihood = -7952.143, SE = 0.041
# Estimated parameters: 90 
# AIC = 16084.29
# BIC = 16490.74; SABIC = 16204.98
# G2 (452984741) = 7161.98, p = 1
# RMSEA = 0, CFI = 0, TLI = 0

#### test diagnostics ----

fitM2_2G <- M2(fit_2G_imp, QMC = T, type = "C2")
#            M2 df         p RMSEA RMSEA_5   RMSEA_95    SRMSR      TLI CFI
# stats 56.84914 57 0.4807135     0       0 0.02372697 0.029967 1.014651   1
# OVEFIT! again

coef_2G <- coef(fit_2G_imp)
# a1 = slope estimate for AFF
# d1 = intercept estimate for AFF

load_corrs_2G <- summary(fit_2G_imp)

fs_2G <- fscores(fit_2G_imp, method = "MAP", full.scores = T, 
                       QMC = T, full.scores.SE = F)

fs_rel_2G <- fscores(fit_2G_imp, returnER = T)
#       AFF       ACH       POW 
# 0.2230006 0.2561201 0.2376717

### 2S ###################################################################################

#### specification ----

# Custom scoring functions for AFF, ACH, and POW
sf_2S <- list()
for(l in 1:15){
  sf_2S[[l]]<-matrix(
    c(1, 0, 0, 0, # sf(AFF)
      0, 1, 0, 0, # sf(ACH)
      0, 0, 1, 0, # sf(POW)
      0, 0, 0, 1, # sf(AUT)
    ), 4, 4)
}

model_2S <- "
AFF = 1-20
ACH = 1-20
POW = 1-20
AUT = 1-20

COV = AFF*ACH*POW*AUT
"

#### estimation ----
fit_2S <- mirt(OMT_items_students2S, model_2S, gpcm_mats = sf_2S, 
               itemtype="gpcm", method = "MHRM", verbose = T, 
               technical = list(NCYCLES = 10000), SE = T, calcNull = T)
# imputation
fs_for_imp2S <- fscores(fit_2S, method = "MAP", full.scores = T, scores.only = T)
OMT_items_gamers_imp2S <- imputeMissing(fit_2S, fs_for_imp2S)

# imputed data model 
fit_2S_imp <- mirt(OMT_items_gamers_imp2S, model_2S, gpcm_mats = sf_2S, 
                   itemtype="gpcm", method = "MHRM", verbose = T, 
                   technical = list(NCYCLES = 10000), SE = T, calcNull = T)
# Log-likelihood = -19903.57, SE = 0.051
# Estimated parameters: 146 
# AIC = 40099.15
# BIC = 40838.35; SABIC = 40374.61
# G2 (1e+10) = 23313.41, p = 1
# RMSEA = 0, CFI = 0, TLI = 0


#### test diagnostics ----

fitM2_2S_imp <- M2(fit_2S_imp, QMC = T, type = "C2")
#            M2 df         p RMSEA RMSEA_5   RMSEA_95    SRMSR      TLI CFI
# stats 56.84914 57 0.4807135     0       0 0.02372697 0.029967 1.014651   1
# OVEFIT! again

coef_2S_imp <- coef(fit_2S_imp)
# a1 = slope estimate for AFF
# d1 = intercept estimate for AFF

load_corrs_2S_imp <- summary(fit_2S_imp)

fs_2S_imp <- fscores(fit_2S_imp, method = "MAP", full.scores = T, 
                 QMC = T, full.scores.SE = F)

fs_rel_2S_imp <- fscores(fit_2S_imp, returnER = T)
#      AFF       ACH       POW       AUT 
# 0.3455024 0.3444887 0.2903626 0.3233063 













##########################################################################################
# DO NOT RUN #
##########################################################################################


###### + absolute frequencies ------------------------------------------------------------

OMT_item_cols <- OMT_items[,1:15]


OMT_items$f_0 <- rowSums(OMT_item_cols == "0", na.rm = T) 
OMT_items$f_Aff <- rowSums(OMT_item_cols == "Affiliation", na.rm = T) 
OMT_items$f_Ach <- rowSums(OMT_item_cols == "Achievement", na.rm = T) 
OMT_items$f_Pow <- rowSums(OMT_item_cols == "Power", na.rm = T) 
OMT_items$f_non0 <- rowSums(OMT_item_cols != "0", na.rm = T) 

View(OMT_items)

##### + relative frequencies ----
OMT_items$perc_0 <- (OMT_items$f_0/OMT_items$f_non0)*100
OMT_items$perc_Aff <- (OMT_items$f_Aff/OMT_items$f_non0)*100
OMT_items$perc_Ach <- (OMT_items$f_Ach/OMT_items$f_non0)*100
OMT_items$perc_Pow <- (OMT_items$f_Pow/OMT_items$f_non0)*100

freq_scores_mat <- OMT_items[,21:24]

score_mat <- cbind(freq_scores_mat, eap_motives)

ggplot(score_mat, aes(x = ACH, y = perc_Ach)) +
  geom_point(size = 1.8) +
  labs(x ="Factor score", y = "Frequency score", title = "Achievemnt")

ggplot(score_mat, aes(x = AFF, y = perc_Aff)) +
  geom_point(size = 1.8) +
  labs(x ="Factor score", y = "Frequency score", title = "Affiliation")

ggplot(score_mat, aes(x = POW, y = perc_Pow)) +
  geom_point(size = 1.8) +
  labs(x ="Factor score", y = "Frequency score", title = "Power")

hist(eap_mat$ZERO)




### specification and modelling ----

# Scoring functions for AFF, ACH, POW, and ZERO
sf_motives1 <- list()
for(i in 1:15){
  sf_motives1[[i]]<-matrix(
    c(1, 0, 0, 0, # sf(AFF)
      0, 1, 0, 0, # sf(ACH)
      0, 0, 1, 0, # sf(POW)
      NA, NA, NA, NA  # sf(ZERO) freely
    ), 4, 4)
}

model_motives1 <- "
AFF = 1-15
ACH = 1-15
POW = 1-15
ZERO = 1-15

COV = AFF*ACH*POW
"
fit_motives_par <- mirt(OMT_items_MNRM, model_motives1, # gpcm_mats = sf_motives1, 
                        itemtype="nominal", method = "MHRM", verbose = T, 
                        technical = list(NCYCLES = 10000), SE = T, calcNull = T, pars = "values")

fit_motives1 <- mirt(OMT_items_MNRM, model_motives1, gpcm_mats = sf_motives1, 
                     itemtype="gpcm", method = "MHRM", verbose = T, 
                     technical = list(NCYCLES = 10000), SE = T, calcNull = T, pars = fit_motives_par)


fit_motives1
### test diagnostics ----

fitM2_motives1 <- M2(fit_motives1, QMC = T, type = "C2")

coef_motives1 <- coef(fit_motives1)
# a1 = slope estimate for AFF
# d1 = intercept estimate for AFF

summary(fit_motives1)


### item diagnostics ----
itemfit(fit_motives1, QMC = T, fit_stats = c("S_X2"), method = "MAP")
# High-dimensional models should use Quasi-Monte Carlo
# For Rasch specific models, the popular infit and outfit stats are computed 
# (values close to 1 are considered good, and come with an associated z value),
# Zh statistics printed for all models (Drasgow, Levine and Williams, 1985); 
# values greater than 0 indicate a better fit than expected, less than 0 indicate worse

# plots: observed vs. expected values for items
itemfit(fit_motives1, QMC = T, fit_stats = "S_X2", method = "MAP", S_X2.plot = 1)

# ?   itemplot(fit_motives1, 2, drop.zeros = T, degrees = c(0, 90, 90, 90))


fs_motives1 <- fscores(fit_motives1, method = "MAP", full.scores = T, 
                       QMC = T, full.scores.SE = F)
fs_motives1_SE <- as.data.frame(fscores(fit_motives1, method = "MAP", full.scores = T, 
                                        QMC = T, full.scores.SE = T))
fs_motives1_rel <- as.data.frame(fscores(fit_motives1, method = "MAP", full.scores = T, 
                                         QMC = T, returnER = T))


### visualization by Chalmers: itemplot(shiny = TRUE) 
# https://philchalmers.github.io/mirt/extra/mirt-Workshop-2015_Day-1.pdf

