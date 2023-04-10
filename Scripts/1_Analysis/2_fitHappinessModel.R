# Fits the happiness model to the data
## Required packages
library(rstan)
library(dplyr)
library(phonTools) # zeros function

## Stan model options
rstan_options(auto_write = TRUE)
set.seed(1331)

## Load dataset and create input data list for Stan models
################################################################
projPath <- '~/Documents/GitHub/Article-Subjective-Feelings-in-ICD'
setwd(file.path(projPath))

# Load data for model fitting
load(file.path(projPath, "Data", "1_ParsedData", "parsed_noICD_Off.RData"))
load(file.path(projPath, "Data", "1_ParsedData", "parsed_noICD_On.RData"))
load(file.path(projPath, "Data", "1_ParsedData", "parsed_ICD_Off.RData"))
load(file.path(projPath, "Data", "1_ParsedData", "parsed_ICD_On.RData"))

# Remove too late trials & Shift trial numbers to exclude late trials
shiftTrialNumbers <- function (happiness_input) {
  subj <- unique(happiness_input$.id)
  for (subjName in subj) {
    idx <- happiness_input$.id==subjName
    lateIndices <- is.na(happiness_input$resultOnsetTime) & idx
    lateTrials <- happiness_input$trial[lateIndices]
    lastTrial <-  which(idx) %>% last()
    lateIndices <- which(lateIndices)
    for (ltrial in lateIndices) {
      happiness_input$trial[ltrial:lastTrial] <- happiness_input$trial[ltrial:lastTrial] -1
    }
  }
  return(happiness_input)
}
# Shift trial numbers after too late trials
happiness_input_noICD_OFF <- shiftTrialNumbers(happiness_input_noICD_OFF)
happiness_input_noICD_ON <- shiftTrialNumbers(happiness_input_noICD_ON)
happiness_input_ICD_OFF <- shiftTrialNumbers(happiness_input_ICD_OFF)
happiness_input_ICD_ON <- shiftTrialNumbers(happiness_input_ICD_ON)

# remove late trials
happiness_input_noICD_OFF<-happiness_input_noICD_OFF[!is.na(happiness_input_noICD_OFF$resultOnsetTime),]  
happiness_input_noICD_ON<-happiness_input_noICD_ON[!is.na(happiness_input_noICD_ON$resultOnsetTime),]  
happiness_input_ICD_OFF<-happiness_input_ICD_OFF[!is.na(happiness_input_ICD_OFF$resultOnsetTime),]  
happiness_input_ICD_ON<-happiness_input_ICD_ON[!is.na(happiness_input_ICD_ON$resultOnsetTime),]  

################################################################
## Create input data for Stan models
################################################################
setupModelData <- function (happiness_data){
  # Find number of trials for each subject
  subjects <- unique(happiness_data$.id)
  totalTrials<- c()
  for (subject in subjects) {
    totalTrials<- c(totalTrials, sum(happiness_data$.id== subject))
  }
  maxTrials <- max(totalTrials)
  N <- length(subjects) 
  
  gamble_opt1<- zeros(N,maxTrials)
  gamble_opt2<- zeros(N,maxTrials)
  cert<- zeros(N,maxTrials)
  outcome<- zeros(N,maxTrials)
  type_cert<- zeros(N,maxTrials)
  type_gamb<- zeros(N,maxTrials)
  rpe <- zeros(N,maxTrials)
  group <- matrix(NA, nrow = N, ncol = maxTrials)
  id <- matrix(NA, nrow = N, ncol = maxTrials)
  
  subjIndex <- 1
  for (subject in subjects) { # Set extra columns at end as 0
    gamble_opt1[subjIndex,1:totalTrials[subjIndex]]<- happiness_data$gambleBIG[happiness_data$.id== subject]
    gamble_opt2[subjIndex,1:totalTrials[subjIndex]]<- happiness_data$gambleSMALL[happiness_data$.id== subject]
    cert[subjIndex,1:totalTrials[subjIndex]]<- happiness_data$surebet[happiness_data$.id== subject]
    outcome[subjIndex,1:totalTrials[subjIndex]]<- happiness_data$reward[happiness_data$.id== subject]
    type_gamb[subjIndex,1:totalTrials[subjIndex]]<- happiness_data$choice[happiness_data$.id== subject]
    type_cert[subjIndex,1:totalTrials[subjIndex]]<- as.numeric(happiness_data$choice[happiness_data$.id== subject]==0)
    rpe[subjIndex,1:totalTrials[subjIndex]]<- happiness_data$rpe[happiness_data$.id== subject]
    subjIndex<- subjIndex+1
  }
  
  # Get the number of rated trials
  Rated_trials <- c()
  for (subject in subjects) { 
    Rated_trials<- c(Rated_trials, sum(!is.na(happiness_data$ratingsZScored[happiness_data$.id== subject])))
  }
  maxRatedTrials <- max(Rated_trials)
  
  # Get the ratings for each subject and put into a matrix
  rating <- zeros(N,maxRatedTrials)
  rated_trials_idx<- zeros(N,maxRatedTrials)
  subjIndex <- 1
  for (subject in subjects) {
    ratings<- happiness_data$ratingsZScored[happiness_data$.id== subject]
    rating[subjIndex,1:Rated_trials[subjIndex]] <- ratings[!is.na(ratings)]# - 5#- mean(ratings[!is.na(ratings)]) # Subtract ratings by mean for the new scale
    rated_trials_idx[subjIndex,1:Rated_trials[subjIndex]]<- happiness_data$trial[ !is.na(happiness_data$ratingsZScored) & (happiness_data$.id == subject) ]
    subjIndex<- subjIndex+1
  }
  
  dat <- list(N = N,
              T = maxTrials,
              Tsubj = totalTrials,
              
              gamble_opt1 = gamble_opt1,
              gamble_opt2 = gamble_opt2,
              cert = cert,
              outcome = outcome,
              rpe = rpe,
              
              type_cert = type_cert,
              type_gamb = type_gamb,
              
              Max_Rated_trials =  maxRatedTrials,
              Rated_trials = Rated_trials,
              rating = rating,
              rated_trials_idx = rated_trials_idx,
              Trials_back = 8 # Includes the current trial, Goes 7 trials before the current trial
  )
  return(dat)
}

dat_noICD_OFF <- setupModelData(happiness_input_noICD_OFF)
dat_noICD_ON <- setupModelData(happiness_input_noICD_ON)
dat_ICD_OFF <- setupModelData(happiness_input_ICD_OFF)
dat_ICD_ON <- setupModelData(happiness_input_ICD_ON)

# Create folder for Fitted data if it does not already exist
savePath <- file.path(projPath, "Data", "2_FittedRatingModel")
dir.create(savePath, showWarnings = FALSE)

################################################################
## Form stan model objects for desired models (to be sampled from)
################################################################
model_arg_hap <- stan_model(file.path(projPath, "Scripts", "1_Analysis", "Happiness.stan"))

################################################################
## Run model fitting (sampling) for each cohort+treatment
################################################################
fit_happiness_noICD_OFF <- sampling(model_arg_hap, 
                                    data    = dat_noICD_OFF, 
                                    iter    = 4500,
                                    warmup  = 1500, 
                                    cores   = 8,
                                    chains  = 8,
                                    seed   = 7624)
save(file = file.path(savePath, "fit_happiness_noICD_OFF.RData"), fit_happiness_noICD_OFF)

fit_happiness_noICD_ON <- sampling(model_arg_hap, 
                                   data    = dat_noICD_ON, 
                                   iter    = 4500,
                                   warmup  = 1500,
                                   cores   = 8,
                                   chains  = 8,
                                   seed   = 7624)
save(file = file.path(savePath, "fit_happiness_noICD_ON.RData"), fit_happiness_noICD_ON)

fit_happiness_ICD_OFF <- sampling(model_arg_hap, 
                                  data    = dat_ICD_OFF, 
                                  iter    = 4500, 
                                  warmup  = 1500, 
                                  cores   = 8,
                                  chains  = 8,
                                  seed   = 7624)
save(file = file.path(savePath, "fit_happiness_ICD_OFF.RData"), fit_happiness_ICD_OFF)

fit_happiness_ICD_ON <- sampling(model_arg_hap, 
                                data    = dat_ICD_ON, 
                                iter    = 4500,
                                warmup  = 1500, 
                                cores   = 8,
                                chains  = 8,
                                seed   = 7624)
save(file = file.path(savePath, "fit_happiness_ICD_ON.RData"), fit_happiness_ICD_ON)

################################################################
## View summary of parameters
################################################################
summary_fit_noICD_OFF <- summary(fit_happiness_noICD_OFF)
View(summary_fit_noICD_OFF$summary)
summary_fit_noICD_ON <- summary(fit_happiness_noICD_ON)
View(summary_fit_noICD_ON$summary)
summary_fit_ICD_OFF <- summary(fit_happiness_ICD_OFF)
View(summary_fit_ICD_OFF$summary)
summary_fit_ICD_ON <- summary(fit_happiness_ICD_ON)
View(summary_fit_ICD_ON$summary)

################################################################
## Extract individual-level parameters
################################################################
reformatParams <- function (fit_obj, dat_obj, cohort, saveDir) {
  paramsFormatted<- extract(fit_obj, c('w0','w1','w2','w3','gam')) %>%
    lapply(.,function(x){apply(x,2,mean)}) %>%
    data.frame(c(cohort),.)
  colnames(paramsFormatted) <- c("Group",
                                 "w0 (baseline)",
                                 "w1 (weight of certain rewards)",
                                 "w2 (weight of expected values)",
                                 "w3 (weight of reward prediction errors)",
                                 "gam (forgetting factor)")
  save( file = file.path(saveDir, paste0("SubjectParams_", cohort, ".RData")), paramsFormatted)
  return(paramsFormatted)
}
noICD_Off <- reformatParams(fit_happiness_noICD_OFF, dat_noICD_OFF, "non-ICD Off", savePath)
noICD_On <- reformatParams(fit_happiness_noICD_ON, dat_noICD_ON, "non-ICD On", savePath)
icd_Off <- reformatParams(fit_happiness_ICD_OFF, dat_ICD_OFF, "ICD Off", savePath)
icd_On <- reformatParams(fit_happiness_ICD_ON, dat_ICD_ON, "ICD On", savePath)

################################################################
## Model Diagnostics
################################################################
# Create a folder for model diagnostic plots & outputs if it does not already exist
diagDir <- file.path(projPath, "RatingMdlDiagnostics")
dir.create(diagDir, showWarnings = FALSE)

## Make diagnostic plots
################################################################
diagPlots <- function (stanFitObj, cohortStatus, diagDir) {
  png(file=file.path(diagDir,paste0("pairsPlt_", cohortStatus, ".png")), width=2000, height=1500)
  pairs(stanFitObj, pars = c("mu_w0","mu_w1","mu_w2","mu_w3","mu_gam","lp__"), las = 1)
  dev.off()
  
  png(file=file.path(diagDir,paste0("stanplot_w0_", cohortStatus, ".png")), width=1500, height=1000)
  plot <- stan_plot(stanFitObj,pars="w0",show_density=TRUE)
  print(plot)
  dev.off()
  
  png(file=file.path(diagDir,paste0("stanplot_w1_", cohortStatus, ".png")), width=1500, height=1000)
  plot <- stan_plot(stanFitObj,pars="w1",show_density=TRUE)
  print(plot)
  dev.off()
  
  png(file=file.path(diagDir,paste0("stanplot_w2_", cohortStatus, ".png")), width=1500, height=1000)
  plot <- stan_plot(stanFitObj,pars="w2",show_density=TRUE)
  print(plot)
  dev.off()
  
  png(file=file.path(diagDir,paste0("stanplot_w3_", cohortStatus, ".png")), width=1500, height=1000)
  plot <- stan_plot(stanFitObj,pars="w3",show_density=TRUE)
  print(plot)
  dev.off()
  
  png(file=file.path(diagDir,paste0("stanplot_gam_", cohortStatus, ".png")), width=1500, height=1000)
  plot <- stan_plot(stanFitObj,pars="gam",show_density=TRUE)
  print(plot)
  dev.off()
  
  png(file=file.path(diagDir,paste0("traceplot_indiv_", cohortStatus, ".png")), width=3000, height=1900)
  plot <- traceplot(stanFitObj, pars = c("w0","w1","w2","w3","gam"), inc_warmup=TRUE) 
  print(plot)
  dev.off()
  
  png(file=file.path(diagDir,paste0("traceplot_group_", cohortStatus, ".png")), width=2200, height=1000)
  plot <- traceplot(stanFitObj, pars = c("mu_w0","mu_w1","mu_w2","mu_w3","mu_gam"), inc_warmup=TRUE) 
  print(plot)
  dev.off()
}

diagPlots(fit_happiness_noICD_OFF, "noICD_OFF", diagDir)
diagPlots(fit_happiness_noICD_ON, "noICD_ON", diagDir)
diagPlots(fit_happiness_ICD_OFF, "ICD_OFF", diagDir)
diagPlots(fit_happiness_ICD_ON, "ICD_ON", diagDir)

## Generate summary of posterior distributions over model parameters
################################################################
fit_summary_noICD_OFF <- summary(fit_happiness_noICD_OFF, pars = c("w0", "w1", "w2","w3","gam"))
posterior_noICD_OFF <- as.array(fit_summary_noICD_OFF$summary)
print(posterior_noICD_OFF)
capture.output(print(posterior_noICD_OFF),
               file = file.path(diagDir, "RHat_noICD_OFF.txt"))

fit_summary_ICD_OFF <- summary(fit_happiness_ICD_OFF, pars = c("w0", "w1", "w2","w3","gam"))
posterior_ICD_OFF <- as.array(fit_summary_ICD_OFF$summary)
print(posterior_ICD_OFF)
capture.output(print(posterior_noICD_OFF),
               file = file.path(diagDir, "RHat_ICD_OFF.txt"))

fit_summary_noICD_ON <- summary(fit_happiness_noICD_ON, pars = c("w0", "w1", "w2","w3","gam"))
posterior_noICD_ON <- as.array(fit_summary_noICD_ON$summary)
print(posterior_noICD_ON)
capture.output(print(posterior_noICD_OFF),
               file = file.path(diagDir, "RHat_noICD_ON.txt"))

fit_summary_ICD_ON <- summary(fit_happiness_ICD_ON, pars = c("w0", "w1", "w2","w3","gam"))
posterior_ICD_ON <- as.array(fit_summary_ICD_ON$summary)
print(posterior_ICD_ON)
capture.output(print(posterior_noICD_OFF),
               file = file.path(diagDir, "RHat_ICD_ON.txt"))
