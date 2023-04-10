library(dplyr)
library(hBayesDM)

projPath <- '~/Documents/GitHub/Article-Subjective-Feelings-in-ICD'
setwd(file.path(projPath,"Data", "2_FittedRatingModel"))

## Load stan models
load(file = "fit_happiness_noICD_OFF.RData")
load(file = "fit_happiness_noICD_ON.RData")
load(file = "fit_happiness_ICD_OFF.RData")
load(file = "fit_happiness_ICD_ON.RData")

## Group Models: Extract group-level parameters
parVals_noICD_OFF <- rstan::extract(fit_happiness_noICD_OFF, permuted = TRUE) 
parVals_noICD_ON <- rstan::extract(fit_happiness_noICD_ON, permuted = TRUE)
parVals_ICD_OFF <- rstan::extract(fit_happiness_ICD_OFF, permuted = TRUE)
parVals_ICD_ON <- rstan::extract(fit_happiness_ICD_ON, permuted = TRUE)

# Group parameters
w0 <- c(parVals_noICD_OFF$mu_w0, parVals_noICD_ON$mu_w0, parVals_ICD_OFF$mu_w0, parVals_ICD_ON$mu_w0)
w1 <- c(parVals_noICD_OFF$mu_w1, parVals_noICD_ON$mu_w1, parVals_ICD_OFF$mu_w1, parVals_ICD_ON$mu_w1)
w2 <- c(parVals_noICD_OFF$mu_w2, parVals_noICD_ON$mu_w2, parVals_ICD_OFF$mu_w2, parVals_ICD_ON$mu_w2)
w3 <- c(parVals_noICD_OFF$mu_w3, parVals_noICD_ON$mu_w3, parVals_ICD_OFF$mu_w3, parVals_ICD_ON$mu_w3)
gam <- c(parVals_noICD_OFF$mu_gam, parVals_noICD_ON$mu_gam, parVals_ICD_OFF$mu_gam, parVals_ICD_ON$mu_gam)
groups <- c(rep('noICD_Off',dim(parVals_noICD_OFF$mu_w1)), 
            rep('noICD_On', dim(parVals_noICD_ON$mu_w1)), 
            rep('ICD_Off', dim(parVals_ICD_OFF$mu_w1)), 
            rep('ICD_On', dim(parVals_ICD_ON$mu_w1)))
groupParams <- data.frame(w0, w1, w2, w3, gam, groups)
saveRDS(groupParams, file = file.path(projPath,"Data", "2_FittedRatingModel","groupParameters.RDS"))

# Extract individual-level means
w0_indiv <- c(apply(parVals_noICD_OFF$w0, 2,mean),
              apply(parVals_noICD_ON$w0, 2,mean),
              apply(parVals_ICD_OFF$w0, 2,mean),
              apply(parVals_ICD_ON$w0, 2,mean))
w1_indiv <- c(apply(parVals_noICD_OFF$w1, 2,mean),
              apply(parVals_noICD_ON$w1, 2,mean),
              apply(parVals_ICD_OFF$w1, 2,mean),
              apply(parVals_ICD_ON$w1, 2,mean))
w2_indiv <- c(apply(parVals_noICD_OFF$w2, 2,mean),
              apply(parVals_noICD_ON$w2, 2,mean),
              apply(parVals_ICD_OFF$w2, 2,mean),
              apply(parVals_ICD_ON$w2, 2,mean))
w3_indiv <- c(apply(parVals_noICD_OFF$w3, 2,mean),
              apply(parVals_noICD_ON$w3, 2,mean),
              apply(parVals_ICD_OFF$w3, 2,mean),
              apply(parVals_ICD_ON$w3, 2,mean))
gam_indiv <- c(apply(parVals_noICD_OFF$gam, 2,mean),
               apply(parVals_noICD_ON$gam, 2,mean),
               apply(parVals_ICD_OFF$gam, 2,mean),
               apply(parVals_ICD_ON$gam, 2,mean))
groups <- c(rep('noICD_Off',dim(parVals_noICD_OFF$w1)[2]), 
            rep('noICD_On', dim(parVals_noICD_ON$w1)[2]), 
            rep('ICD_Off', dim(parVals_ICD_OFF$w1)[2]), 
            rep('ICD_On', dim(parVals_ICD_ON$w1)[2]))
indiv_means<- data.frame(w0_indiv, w1_indiv, w2_indiv, w3_indiv, gam_indiv, groups)
saveRDS(indiv_means, file = file.path(projPath,"Data", "2_FittedRatingModel","individualParameters.RDS"))

## Calculate HDI
calcHDI <- function(group) {
  w0 <- groupParams %>% 
    filter(groups == group) %>% 
    select(w0) %>% t() %>% 
    HDIofMCMC()
  w1 <- groupParams %>% 
    filter(groups == group) %>% 
    select(w1) %>% t() %>% 
    HDIofMCMC()
  w2 <- groupParams %>% 
    filter(groups == group) %>% 
    select(w2) %>% t() %>% 
    HDIofMCMC()
  w3 <- groupParams %>% 
    filter(groups == group) %>% 
    select(w3) %>% t() %>% 
    HDIofMCMC()
  gam <- groupParams %>% 
    filter(groups == group) %>% 
    select(gam) %>% t() %>% 
    HDIofMCMC()
  return(data.frame(w0, w1, w2, w3, gam))
}

hdi <- list( noICD_Off = calcHDI("noICD_Off"), 
             icd_Off = calcHDI("ICD_Off"),
             noICD_On = calcHDI("noICD_On"),
             icd_On = calcHDI("ICD_On"))
saveRDS(hdi, file = file.path(projPath,"Data", "2_FittedRatingModel","groupHDI.RDS"))