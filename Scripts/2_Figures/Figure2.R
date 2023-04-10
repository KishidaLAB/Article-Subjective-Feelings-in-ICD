library(ggplot2)
library(ggpubr)
library(cowplot)

projPath <- '~/Documents/GitHub/Article-Subjective-Feelings-in-ICD'
saveDir <- file.path(projPath,"Plots")
dir.create(saveDir, showWarnings = FALSE)
## Load Data
load(file.path(projPath, "Data", "3_PredictedRatings", "predicted_happiness_noICD_OFF.RData"))
load(file.path(projPath, "Data", "3_PredictedRatings", "predicted_happiness_noICD_ON.RData"))
load(file.path(projPath, "Data", "3_PredictedRatings", "predicted_happiness_ICD_OFF.RData"))
load(file.path(projPath, "Data", "3_PredictedRatings", "predicted_happiness_ICD_ON.RData"))

## Remove trials without ratings
gambleChoiceModelInput_ratingComparison_noICD_OFF <- na.omit(gambleChoiceModelInput_noICD_OFF) 
gambleChoiceModelInput_ratingComparison_noICD_ON <- na.omit(gambleChoiceModelInput_noICD_ON) 
gambleChoiceModelInput_ratingComparison_ICD_OFF <- na.omit(gambleChoiceModelInput_ICD_OFF) 
gambleChoiceModelInput_ratingComparison_ICD_ON <- na.omit(gambleChoiceModelInput_ICD_ON) 

## Plot selected individuals' ratings
nonICD6Off<- gambleChoiceModelInput_ratingComparison_noICD_OFF[gambleChoiceModelInput_ratingComparison_noICD_OFF$idVector == "nonICD6",]
nonICD6OffPlt<- ggplot(nonICD6Off, aes(x=trialVector)) +
  geom_line(aes(y = ratingVector), color = "#e83333", size=1) + 
  geom_line(aes(y = imputedHappinessVector), color="steelblue", size=1) +
  theme_classic()+
  ggtitle('Non-ICD6 OFF') + ylab('Ratings') + 
  xlab('Rating Trial')

nonICD6On<- gambleChoiceModelInput_ratingComparison_noICD_ON[gambleChoiceModelInput_ratingComparison_noICD_ON$idVector == "nonICD6",]
nonICD6OnPlt<- ggplot(nonICD6On, aes(x=trialVector)) +
  geom_line(aes(y = ratingVector), color = "#e83333", size=1) + 
  geom_line(aes(y = imputedHappinessVector), color="steelblue", size=1) +
  theme_classic()+
  ggtitle('Non-ICD6 ON') + ylab('Ratings') + 
  xlab('Rating Trial')

nonICD6OffScatterPlt <- ggplot(nonICD6Off, aes(x=ratingVector, y=imputedHappinessVector)) + 
  geom_point() + ggtitle('Non-ICD6 OFF') + ylab('Predicted Ratings') + 
  xlab('Actual Ratings')+ 
  theme_classic()+theme(panel.grid = element_blank())+
  stat_cor(method = "pearson",  aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+#, label.x = -4.5, label.y.npc = "top" , color='black') +
  geom_smooth(method='lm', se=F, color='black')

nonICD6OnScatterPlt <- ggplot(nonICD6On, aes(x=ratingVector, y=imputedHappinessVector)) + 
  geom_point() + ggtitle('Non-ICD6 ON') + ylab('Predicted Ratings') + 
  xlab('Actual Ratings')+ 
  theme_classic()+theme(panel.grid = element_blank())+
  stat_cor(method = "pearson",  aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+#, label.x = -4.5, label.y.npc = "top" , color='black') +
  geom_smooth(method='lm', se=F, color='black')

icd4On<- gambleChoiceModelInput_ratingComparison_ICD_ON[gambleChoiceModelInput_ratingComparison_ICD_ON$idVector == "ICD4",]
icd4OnPlt<- ggplot(icd4On, aes(x=trialVector)) +
  geom_line(aes(y = ratingVector), color = "#e83333", size=1) + 
  geom_line(aes(y = imputedHappinessVector), color="steelblue", size=1) +
  theme_classic()+
  ggtitle('ICD4 ON') + ylab('Ratings') + 
  xlab('Rating Trial')

icd4Off<- gambleChoiceModelInput_ratingComparison_ICD_OFF[gambleChoiceModelInput_ratingComparison_ICD_OFF$idVector == "ICD4",]
icd4OffPlt<- ggplot(icd4Off, aes(x=trialVector)) +
  geom_line(aes(y = ratingVector), color = "#e83333", size=1) + 
  geom_line(aes(y = imputedHappinessVector), color="steelblue", size=1) +
  theme_classic()+
  ggtitle('ICD4 OFF') + ylab('Ratings') + 
  xlab('Rating Trial')

icd4OffScatterPlt <- ggplot(icd4Off, aes(x=ratingVector, y=imputedHappinessVector)) + 
  geom_point() + ggtitle('ICD4 OFF') + ylab('Predicted Ratings') + 
  xlab('Actual Ratings')+ 
  theme_classic()+theme(panel.grid = element_blank())+
  stat_cor(method = "pearson",  aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+#, label.x = -4.5, label.y.npc = "top" , color='black') +
  geom_smooth(method='lm', se=F, color='black')

icd4OnScatterPlt <- ggplot(icd4On, aes(x=ratingVector, y=imputedHappinessVector)) + 
  geom_point() + ggtitle('ICD4 ON') + ylab('Predicted Ratings') + 
  xlab('Actual Ratings')+ 
  theme_classic()+theme(panel.grid = element_blank())+
  stat_cor(method = "pearson",  aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+#, label.x = -4.5, label.y.npc = "top" , color='black') +
  geom_smooth(method='lm', se=F, color='black')


### Plot combined Ratings for each group
plotGroupRatings <- function (ratingDF, cohortStatus, projPath) {
  uniqueTrials <- unique(ratingDF$trialVector)
  meanActual <- c()
  stdActual <- c()
  meanPred <- c()
  stdPred <- c()
  for (trial in uniqueTrials) {
    trialIdx <- ratingDF$trialVector == trial
    meanActual<- c(meanActual, mean(ratingDF$ratingVector[trialIdx]))
    stdActual <- c(stdActual, sd(ratingDF$ratingVector[trialIdx]))
    meanPred<- c(meanPred, mean(ratingDF$imputedHappinessVector[trialIdx]))
    stdPred <- c(stdPred, sd(ratingDF$imputedHappinessVector[trialIdx]))
  }
  
  allRatings <- data.frame(uniqueTrials,meanActual,meanPred, stdActual, stdPred)
  
  rm('meanActual','stdActual','meanPred','stdPred')
  subTitleFontSz <- 12
  lineWidth <- 1
  legColor <- c("Rating" = "#e83333", "Model Fit" = "steelblue")
  
  ratingPlt <- ggplot(allRatings, aes(x=uniqueTrials)) +
    geom_line(aes(y = meanActual, colour = "Rating"), size=lineWidth) +
    geom_line(aes(y = meanPred, colour="Model Fit"), size=lineWidth) +
    scale_color_manual(name = "", values = legColor)+
    theme_classic(base_size=subTitleFontSz)+
    theme(strip.background = element_blank(), plot.title = element_text(hjust = 0.5), legend.position="none")+
    ggtitle(cohortStatus) + labs(x = "Trial", y = "Rating") + scale_x_continuous(limits = c(0,max(allRatings$uniqueTrials)), expand = c(0, 0)) 
  return(ratingPlt)
}
group_ratingPlt_noICDOff<- plotGroupRatings(gambleChoiceModelInput_ratingComparison_noICD_OFF, 'Non-ICD Off', projPath)
group_ratingPlt_noICDOn<- plotGroupRatings(gambleChoiceModelInput_ratingComparison_noICD_ON, 'Non-ICD On', projPath)
group_ratingPlt_ICDOff<- plotGroupRatings(gambleChoiceModelInput_ratingComparison_ICD_OFF, 'ICD Off', projPath)
group_ratingPlt_ICDOn<- plotGroupRatings(gambleChoiceModelInput_ratingComparison_ICD_ON, 'ICD On', projPath)

## Figure 2
fig2R1_v2 <- plot_grid(nonICD6OnPlt, nonICD6OnScatterPlt, icd4OnPlt, icd4OnScatterPlt, labels=c('A','B','C','D'),nrow = 1)
fig2R2_v2 <- plot_grid(nonICD6OffPlt, nonICD6OffScatterPlt, icd4OffPlt, icd4OffScatterPlt, labels=c('E','F', 'G','H'),nrow = 1)
fig2R3_v2 <- plot_grid(group_ratingPlt_noICDOff, group_ratingPlt_ICDOff, labels=c('I','J'),nrow = 1)
fig2R4_v2 <- plot_grid(group_ratingPlt_noICDOn, group_ratingPlt_ICDOn, labels=c('K','L'),nrow = 1)
fig2_v2 <- plot_grid(fig2R1_v2, fig2R2_v2, 
                     fig2R3_v2, fig2R4_v2,
                     ncol = 1)
## Save Figure
ggsave( file.path(saveDir, "Figure2.png"), 
        fig2_v2, width = 20, height = 12, dpi = 1200)

# Fig 2 legend: Re-plot nonICD6Off for the legend
lineWidth <- 1
legColor <- c("Rating" = "#e83333", "Model Fit" = "steelblue")
legPlt <- ggplot(nonICD6Off) +
  geom_line(aes(x = trialVector, y = ratingVector, colour = "Rating"), size=lineWidth) +
  geom_line(aes(x = trialVector, y = imputedHappinessVector, colour="Model Fit"), size=lineWidth)+
  scale_color_manual(name = "", values = legColor) + theme_classic()
ggsave( file.path(saveDir, "Figure2_legend.png"), 
        legPlt, width = 12, height = 10, dpi = 1200)

