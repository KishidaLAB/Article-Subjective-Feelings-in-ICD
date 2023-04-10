## Load Libraries
library(stringr)
library(ggplot2)
library(ggpubr)
library(xlsx)

## Set Working Directory to Project Path
projPath <- '~/Documents/GitHub/Article-Subjective-Feelings-in-ICD'
setwd(file.path(projPath,"Data"))

## Load subject data
load(file.path(projPath,"Data", "1_ParsedData", "parsed_noICD_Off.RData"))
load(file.path(projPath,"Data", "1_ParsedData", "parsed_noICD_On.RData"))
load(file.path(projPath,"Data", "1_ParsedData", "parsed_ICD_Off.RData"))
load(file.path(projPath,"Data", "1_ParsedData", "parsed_ICD_On.RData"))

## Load subject parameters
load(file.path(projPath,"Data", "2_FittedRatingModel", "SubjectParams_non-ICD Off.RData"))
indivParams_noICDOff <- paramsFormatted
load(file.path(projPath,"Data", "2_FittedRatingModel", "SubjectParams_non-ICD On.RData"))
indivParams_noICDOn <- paramsFormatted
load(file.path(projPath,"Data", "2_FittedRatingModel", "SubjectParams_ICD Off.RData"))
indivParams_ICDOff <- paramsFormatted
load(file.path(projPath,"Data", "2_FittedRatingModel", "SubjectParams_ICD On.RData"))
indivParams_ICDOn <- paramsFormatted
rm(paramsFormatted)

## Calculate imputed happiness
calculateImpHappiness <- function (happiness_input, cohortStatus, indivParams) {
  ptName <- unique(happiness_input$.id)
  tooLateRemovedDf <- happiness_input[!is.na(happiness_input$resultOnsetTime),]
  gambleChoiceModelInput <- data.frame()
  rmse_cohort<-c()
  for (ptNumber in 1:length(ptName))
  {
    gam_pt <- indivParams$`gam (forgetting factor)`[ptNumber]
    w0_pt <- indivParams$`w0 (baseline)`[ptNumber]
    w1_pt <- indivParams$`w1 (weight of certain rewards)`[ptNumber]
    w2_pt <- indivParams$`w2 (weight of expected values)`[ptNumber]
    w3_pt <- indivParams$`w3 (weight of reward prediction errors`[ptNumber]
    singlePt <- tooLateRemovedDf[!is.na(str_match(tooLateRemovedDf$.id,paste0(ptName[ptNumber],'$'))),]
    # Save these variables later for the gamble choice model
    evVector <- singlePt$gamblEV
    crVector <- singlePt$surebet
    
    singlePt$surebet[singlePt$choice==1] <- 0 # if gamble picked, zero out crTerm
    singlePt$gamblEV[singlePt$choice==0 ] <- 0 # if sure bet picked, zero out gamble terms
    
    imputedHappinessVector <- c()
    for (trial in 1:7) # calculate happiness for each trial
    {
      crTerm <- 0
      for (j in (trial-(trial-1)):trial ) # Assume j=t-7 in the happiness equation
      { crTerm <- crTerm + ((gam_pt^(trial-j))*singlePt$surebet[j]) }
      
      evTerm <- 0
      for (j in (trial-(trial-1)):trial )
      { evTerm <- evTerm + ((gam_pt^(trial-j))*singlePt$gamblEV[j]) }
      
      rpeTerm <- 0
      for (j in (trial-(trial-1)):trial )
      { rpeTerm <- rpeTerm + ((gam_pt^(trial-j))*singlePt$rpe[j]) }
      
      happiness<- w0_pt + w1_pt*crTerm + w2_pt*evTerm + w3_pt*rpeTerm
      imputedHappinessVector<- c(imputedHappinessVector,happiness)
      rpeVector <- singlePt$rpe
    }
    for (trial in 8:dim(singlePt)[1]) # calculate happiness for each trial
    {
      crTerm <- 0
      for (j in (trial-7):trial ) # Assume j=t-7 in the happiness equation
      { crTerm <- crTerm + ((gam_pt^(trial-j))*singlePt$surebet[j]) }
      
      evTerm <- 0
      for (j in (trial-7):trial )
      { evTerm <- evTerm + ((gam_pt^(trial-j))*singlePt$gamblEV[j]) }
      
      rpeTerm <- 0
      for (j in (trial-7):trial )
      { rpeTerm <- rpeTerm + ((gam_pt^(trial-j))*singlePt$rpe[j]) }
      
      happiness<- w0_pt + w1_pt*crTerm + w2_pt*evTerm + w3_pt*rpeTerm
      imputedHappinessVector<- c(imputedHappinessVector,happiness)
      rpeVector <- singlePt$rpe
      choiceVector <- singlePt$choice
      ratingVector <- singlePt$ratings
      idVector <- singlePt$.id
      trialVector <- singlePt$trial
    }

    imputedHappinessVectorMSE<- imputedHappinessVector[!is.na(ratingVector)]
    ratingVector<- (ratingVector - mean(ratingVector, na.rm = TRUE)) / sd(ratingVector, na.rm = TRUE) 
    ratingVectorMSE <- ratingVector[!is.na(ratingVector)]
    rmse_cohort[ptNumber] <- sqrt(sum((imputedHappinessVectorMSE-ratingVectorMSE)^2)/length(imputedHappinessVectorMSE))
    
    # Combine actual and predicted ratings
    mixedActualImpHappiness <- ratingVector
    mixedActualImpHappiness[is.na(mixedActualImpHappiness)] <- imputedHappinessVector[is.na(mixedActualImpHappiness)]
    
    gambleChoiceModelInput <- rbind(gambleChoiceModelInput, data.frame(idVector, imputedHappinessVector,  mixedActualImpHappiness, rpeVector, evVector, crVector, choiceVector, ratingVector, trialVector))
  }
  rmse_cohort_dataFrame <- data.frame(ptName, rmse_cohort)
  return(list(gambleChoiceModelInput, rmse_cohort_dataFrame))
}

dir.create(file.path(projPath, "Data","3_PredictedRatings"), showWarnings = FALSE)

gambleChoiceModelInput_noICD_OFF <- calculateImpHappiness(happiness_input_noICD_OFF, 'noICD_OFF', indivParams_noICDOff)[[1]]
rmse_dataFrame_noICD_OFF <- calculateImpHappiness(happiness_input_noICD_OFF, 'noICD_OFF', indivParams_noICDOff)[[2]]
save(gambleChoiceModelInput_noICD_OFF, file=file.path(projPath, "Data", "3_PredictedRatings", "predicted_happiness_noICD_OFF.RData"))

gambleChoiceModelInput_noICD_ON <- calculateImpHappiness(happiness_input_noICD_ON, 'noICD_ON', indivParams_noICDOn)[[1]]
rmse_dataFrame_noICD_ON <- calculateImpHappiness(happiness_input_noICD_ON, 'noICD_ON', indivParams_noICDOn)[[2]]
save(gambleChoiceModelInput_noICD_ON, file=file.path(projPath,"Data", "3_PredictedRatings", "predicted_happiness_noICD_ON.RData"))

gambleChoiceModelInput_ICD_OFF <- calculateImpHappiness(happiness_input_ICD_OFF, 'ICD_OFF', indivParams_ICDOff)[[1]]
rmse_dataFrame_ICD_OFF <- calculateImpHappiness(happiness_input_ICD_OFF, 'ICD_OFF', indivParams_ICDOff)[[2]]
save(gambleChoiceModelInput_ICD_OFF, file=file.path(projPath,"Data", "3_PredictedRatings", "predicted_happiness_ICD_OFF.RData"))

gambleChoiceModelInput_ICD_ON <- calculateImpHappiness(happiness_input_ICD_ON, 'ICD_ON', indivParams_ICDOn)[[1]]
rmse_dataFrame_ICD_ON <- calculateImpHappiness(happiness_input_ICD_ON, 'ICD_ON', indivParams_ICDOn)[[2]]
save(gambleChoiceModelInput_ICD_ON, file=file.path(projPath,"Data", "3_PredictedRatings", "predicted_happiness_ICD_ON.RData"))


#################################################################
## Calculate r-squared values for the model's predicted vs actual ratings
#################################################################
## Remove trials without ratings
gambleChoiceModelInput_ratingComparison_noICD_OFF <- na.omit(gambleChoiceModelInput_noICD_OFF) 
gambleChoiceModelInput_ratingComparison_noICD_ON <- na.omit(gambleChoiceModelInput_noICD_ON) 
gambleChoiceModelInput_ratingComparison_ICD_OFF <- na.omit(gambleChoiceModelInput_ICD_OFF) 
gambleChoiceModelInput_ratingComparison_ICD_ON <- na.omit(gambleChoiceModelInput_ICD_ON) 

## Get R-squared and p-value for predicted vs actual ratings
getR2_pVal <- function (modelInput) {
  pt <- unique(modelInput$idVector)
  r2 <- c()
  pVal <- c()
  for (ptIdx in pt) {
    data <- modelInput[modelInput$idVector == ptIdx,]
    model <- lm(ratingVector~imputedHappinessVector, data=data)
    r2 <- c(r2, summary(model)$r.squared)
    pVal <- c(pVal, summary(model)$coefficients[2,4])
    rm(model)
  }
  output <- data.frame(pt,r2,pVal)
  # Pool all subjects
  model<-lm(ratingVector~imputedHappinessVector, data=modelInput)
  output[nrow(output)+1,] <- c('All', summary(model)$r.squared, summary(model)$coefficients[2,4])
  return (output)
}

fitStats_noICD_OFF <- getR2_pVal(gambleChoiceModelInput_ratingComparison_noICD_OFF)
fitStats_noICD_ON <- getR2_pVal(gambleChoiceModelInput_ratingComparison_noICD_ON)
fitStats_ICD_OFF <- getR2_pVal(gambleChoiceModelInput_ratingComparison_ICD_OFF)
fitStats_ICD_ON <- getR2_pVal(gambleChoiceModelInput_ratingComparison_ICD_ON)

# Create folder for Tables if it does not already exist
dir.create(file.path(projPath, "Tables"), showWarnings = FALSE)
# Save
write.xlsx(fitStats_noICD_OFF, file.path(projPath, 'Tables', 'In Text-RatingModelFitStats.xlsx'), sheetName = "noICD Off", 
           col.names = TRUE, row.names = FALSE, append = FALSE)
write.xlsx(fitStats_noICD_ON, file.path(projPath, 'Tables', 'In Text-RatingModelFitStats.xlsx'), sheetName = "noICD On", 
           col.names = TRUE, row.names = FALSE, append = TRUE)
write.xlsx(fitStats_ICD_OFF, file.path(projPath, 'Tables', 'In Text-RatingModelFitStats.xlsx'), sheetName = "ICD Off", 
           col.names = TRUE, row.names = FALSE, append = TRUE)
write.xlsx(fitStats_ICD_ON, file.path(projPath, 'Tables', 'In Text-RatingModelFitStats.xlsx'), sheetName = "ICD On", 
           col.names = TRUE, row.names = FALSE, append = TRUE)
