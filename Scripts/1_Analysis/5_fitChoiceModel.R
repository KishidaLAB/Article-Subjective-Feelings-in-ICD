# Model 1: Gamble Choice Model
# Gamble Choice (t) =Gamble EV (t) + Sure bet EV (t) + (Imputed Subjective feeling on (t-1))
# Logistic regression
projPath <- '~/Documents/GitHub/Article-Subjective-Feelings-in-ICD'
setwd(file.path(projPath,"Data","3_PredictedRatings"))

load('predicted_happiness_noICD_OFF.RData')
load('predicted_happiness_noICD_ON.RData')
load('predicted_happiness_ICD_OFF.RData')
load('predicted_happiness_ICD_ON.RData')

# Convert choice made into a factor
gambleChoiceModelInput_noICD_OFF$choiceVector <- as.factor(gambleChoiceModelInput_noICD_OFF$choiceVector)
gambleChoiceModelInput_noICD_ON$choiceVector <- as.factor(gambleChoiceModelInput_noICD_ON$choiceVector)
gambleChoiceModelInput_ICD_OFF$choiceVector <- as.factor(gambleChoiceModelInput_ICD_OFF$choiceVector)
gambleChoiceModelInput_ICD_ON$choiceVector <- as.factor(gambleChoiceModelInput_ICD_ON$choiceVector)

# Get imputed subjective feeling for t-1
gambleChoiceModelInput_noICD_OFF$prevTrialHappiness <- c(NA, gambleChoiceModelInput_noICD_OFF$mixedActualImpHappiness[1:(dim(gambleChoiceModelInput_noICD_OFF)[1]-1)]) # 1st to end-1 value
gambleChoiceModelInput_noICD_ON$prevTrialHappiness <- c(NA, gambleChoiceModelInput_noICD_ON$mixedActualImpHappiness[1:(dim(gambleChoiceModelInput_noICD_ON)[1]-1)]) 
gambleChoiceModelInput_ICD_OFF$prevTrialHappiness <- c(NA, gambleChoiceModelInput_ICD_OFF$mixedActualImpHappiness[1:(dim(gambleChoiceModelInput_ICD_OFF)[1]-1)]) 
gambleChoiceModelInput_ICD_ON$prevTrialHappiness <- c(NA, gambleChoiceModelInput_ICD_ON$mixedActualImpHappiness[1:(dim(gambleChoiceModelInput_ICD_ON)[1]-1)]) 

# remove 1st trial since it has no previous trial imputed happiness
rmv1stTrial4EachSubj  <- function(gambleChoiceModelInput) {
  ptName <- unique(gambleChoiceModelInput$idVector)
  for (ptNumber in 1:length(ptName)){
    gambleChoiceModelInput<- gambleChoiceModelInput[-match(ptName[ptNumber], gambleChoiceModelInput$idVector),]
  }
  return(gambleChoiceModelInput)
}
gambleChoiceModelInput_noICD_OFF<- rmv1stTrial4EachSubj(gambleChoiceModelInput_noICD_OFF)
gambleChoiceModelInput_noICD_ON<- rmv1stTrial4EachSubj(gambleChoiceModelInput_noICD_ON)
gambleChoiceModelInput_ICD_OFF<- rmv1stTrial4EachSubj(gambleChoiceModelInput_ICD_OFF)
gambleChoiceModelInput_ICD_ON<- rmv1stTrial4EachSubj(gambleChoiceModelInput_ICD_ON)

# Fit logistic regression model on each person's "on" or "off" 
fitLogGambleModel <- function(exp_gambleChoiceModelInput) {
  ptName <- unique(exp_gambleChoiceModelInput$idVector)
  exp_gambleChoiceModelInput$choiceVector <- as.numeric(exp_gambleChoiceModelInput$choiceVector)-1
  pt_list <- list()
  cohort_predictions <- list()
  for (ptNumber in 1:length(ptName)){
    individualDF<- exp_gambleChoiceModelInput[exp_gambleChoiceModelInput$idVector == ptName[ptNumber],]
    individualModel <- glm(choiceVector ~ evVector + crVector + prevTrialHappiness, family = binomial, data = individualDF) 
    pt_list[ptNumber]<- individualModel
    cohort_predictions[[ptNumber]] <- predict.glm(individualModel, individualDF, type="response")
    pt_list[[ptNumber]]$id<- ptName[ptNumber]
  }
  
  gambleChoicePredDF <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(gambleChoicePredDF)<- c('subjectID','expTrialNumb', 'predictions','actual')
  for (ptIdx in 1:length(ptName)) {
    expTrialNumb <- c(1:length(cohort_predictions[[ptIdx]]))
    ptNameVector <- rep(ptName[ptIdx],length(cohort_predictions[[ptIdx]]))
    indivGambleChoicePredDF <- data.frame(ptNameVector, expTrialNumb, cohort_predictions[[ptIdx]], 
                                          exp_gambleChoiceModelInput$choiceVector[exp_gambleChoiceModelInput$idVector == ptName[ptIdx]])
    colnames(indivGambleChoicePredDF)<- c('subjectID','expTrialNumb', 'predictions','actual')
    gambleChoicePredDF <- rbind(gambleChoicePredDF, indivGambleChoicePredDF)
  }
  # Convert 0 and 1 to "Sure Bet" and "Gamble" for plot legends
  gambleChoicePredDF$actualLabel[gambleChoicePredDF$actual==0] <- "Sure Bet"
  gambleChoicePredDF$actualLabel[gambleChoicePredDF$actual==1] <- "Gamble"
  return(list(gambleChoicePredDF, pt_list))
}

gambleChoicePred_noICD_OFF <- fitLogGambleModel(gambleChoiceModelInput_noICD_OFF)[[1]]
gambleChoiceModel_noICD_OFF <- fitLogGambleModel(gambleChoiceModelInput_noICD_OFF)[[2]]
gambleChoicePred_noICD_ON <- fitLogGambleModel(gambleChoiceModelInput_noICD_ON)[[1]]
gambleChoiceModel_noICD_ON <- fitLogGambleModel(gambleChoiceModelInput_noICD_ON)[[2]]
gambleChoicePred_ICD_OFF <- fitLogGambleModel(gambleChoiceModelInput_ICD_OFF)[[1]]
gambleChoiceModel_ICD_OFF <- fitLogGambleModel(gambleChoiceModelInput_ICD_OFF)[[2]]
gambleChoicePred_ICD_ON <- fitLogGambleModel(gambleChoiceModelInput_ICD_ON)[[1]]
gambleChoiceModel_ICD_ON <- fitLogGambleModel(gambleChoiceModelInput_ICD_ON)[[2]]

# Save Model Coefficients
reshapeCoeffs <- function(gambleChoiceModel) {
  coeff <- data.frame(matrix(ncol = 4, nrow = length(gambleChoiceModel)))
  colnames(coeff) <- c('intercept','evBeta','crBeta','prevImputedHappinessBeta')
  for (ptNumber in 1:length(gambleChoiceModel)) {
    coeff$intercept[ptNumber]<- gambleChoiceModel[[ptNumber]]$`(Intercept)`
    coeff$evBeta[ptNumber]<- gambleChoiceModel[[ptNumber]]$evVector
    coeff$crBeta[ptNumber]<- gambleChoiceModel[[ptNumber]]$crVector
    coeff$prevImputedHappinessBeta[ptNumber]<- gambleChoiceModel[[ptNumber]]$prevTrialHappiness
  }
  return(coeff)
}
coeff_noICD_off <- reshapeCoeffs(gambleChoiceModel_noICD_OFF) 
coeff_noICD_on <- reshapeCoeffs(gambleChoiceModel_noICD_ON) 
coeff_ICD_off <- reshapeCoeffs(gambleChoiceModel_ICD_OFF) 
coeff_ICD_on <- reshapeCoeffs(gambleChoiceModel_ICD_ON) 

# Create a folder for model diagnostic plots & outputs if it does not already exist
savePath <- file.path(projPath, "Data", "4_GambleChoiceModel")
dir.create(savePath, showWarnings = FALSE)

save(file = file.path(savePath, "gambleChoiceModelCoeff.RData"), 
     coeff_noICD_off, coeff_noICD_on, coeff_ICD_off, coeff_ICD_on)


####################################################################
## Plot ROC Curve
####################################################################

library(ggplot2)
library(cowplot)
library(pROC)

plotROC <- function(actual, pred, cohort) {
  # convert factors to numeric
  actual <- as.numeric(actual)-1
  #define object to plot and calculate AUC
  rocobj <- roc(actual, pred)
  auc <- round(rocobj$auc,4)
  #create ROC plot
  rocPlot <- ggroc(rocobj, colour = 'black', size = 2) +
    ggtitle(paste0(cohort, ' ( AUC = ', auc, ')')) + 
    theme_classic(base_size=20)
  
  return(rocPlot)
}
a<- plotROC(gambleChoicePred_noICD_ON$actual, gambleChoicePred_noICD_ON$predictions, 'Non-ICD On')
b<- plotROC(gambleChoicePred_ICD_ON$actual, gambleChoicePred_ICD_ON$predictions, 'ICD On')
c<- plotROC(gambleChoicePred_noICD_OFF$actual, gambleChoicePred_noICD_OFF$predictions, 'Non-ICD Off')
d<- plotROC(gambleChoicePred_ICD_OFF$actual, gambleChoicePred_ICD_OFF$predictions, 'ICD Off')
ggsave(
  file.path(projPath,"Plots", paste0("gambleChoice-ROCCurve.png")),
  plot_grid(a, b,c,d, labels = c('A', 'B','C','D'), label_size = 35),
  width = 16,
  height = 10,
  dpi = 1200
)