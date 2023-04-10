library(Rmisc)
library(xlsx)

projPath <- '~/Documents/GitHub/Article-Subjective-Feelings-in-ICD'
setwd(file.path(projPath, "Data", "1_ParsedData"))

## Load data
load('parsed_noICD_Off.RData')
load('parsed_noICD_On.RData')
load('parsed_ICD_Off.RData')
load('parsed_ICD_On.RData')

createSubset <- function (happiness_input) {
  sbLargerTrials <- happiness_input$surebet > happiness_input$gamblEV
  happiness_input_subOptGamb <- happiness_input[sbLargerTrials,]
  return (happiness_input_subOptGamb)
}

subOpt_happiness_input_ICD_OFF <- createSubset(happiness_input_ICD_OFF)
subOpt_happiness_input_ICD_ON <- createSubset(happiness_input_ICD_ON)
subOpt_happiness_input_noICD_OFF <- createSubset(happiness_input_noICD_OFF)
subOpt_happiness_input_noICD_ON <- createSubset(happiness_input_noICD_ON)

# get gambling percentage for groups
percGamb_ICDOff<- sum(subOpt_happiness_input_ICD_OFF$choice==1, na.rm = TRUE) / sum(!is.na(subOpt_happiness_input_ICD_OFF$choice)) 
percGamb_noICDOff<- sum(subOpt_happiness_input_noICD_OFF$choice==1, na.rm = TRUE) / sum(!is.na(subOpt_happiness_input_noICD_OFF$choice)) 
percGamb_ICDOn<- sum(subOpt_happiness_input_ICD_ON$choice==1, na.rm = TRUE) / sum(!is.na(subOpt_happiness_input_ICD_ON$choice)) 
percGamb_noICDOn<- sum(subOpt_happiness_input_noICD_ON$choice==1, na.rm = TRUE) / sum(!is.na(subOpt_happiness_input_noICD_ON$choice)) 

# get gambling percentage for individuals
calcIndivGambPerc <- function (happiness_input) {
  # Get the percent 
  subjects <- unique (happiness_input$id)
  subjects <- na.omit(subjects)
  gambPerc <- c()
  for (subj in subjects) {
    subjDF <- happiness_input[happiness_input$id==subj,]
    gambPerc <- c(gambPerc, sum(subjDF$choice == 1, na.rm=TRUE) / sum(!is.na(subjDF$choice))*100)
  }
  return(data.frame(subjects, gambPerc))
}

percGambIndiv_ICDOff <- calcIndivGambPerc(subOpt_happiness_input_ICD_OFF)
percGambIndiv_ICDOn <- calcIndivGambPerc(subOpt_happiness_input_ICD_ON)
percGambIndiv_noICDOff <- calcIndivGambPerc(subOpt_happiness_input_noICD_OFF)
percGambIndiv_noICDOn <- calcIndivGambPerc(subOpt_happiness_input_noICD_ON)

tableS5 <- data.frame(matrix(nrow = 4, ncol = 5))
colnames(tableS5) <- c('Desc', 'Mean','CI_low','to','CI_high')

ci <- CI(percGambIndiv_noICDOff$gambPerc) %>% round(2)
tableS5[1,] <- c('noICD Off', ci[2], ci[3], 'to', ci[1])

ci <- CI(percGambIndiv_noICDOn$gambPerc) %>% round(2)
tableS5[2,] <- c('noICD On', ci[2], ci[3], 'to', ci[1])

ci <- CI(percGambIndiv_ICDOff$gambPerc) %>% as.numeric %>% round(2)
tableS5[3,] <- c('ICD Off', ci[2], ci[3], 'to', ci[1])

ci <- CI(na.omit(percGambIndiv_ICDOn$gambPerc)) %>% round(2)
tableS5[4,] <- c('ICD On', ci[2], ci[3], 'to', ci[1])

wilcDF_subOpt <- data.frame(matrix(nrow = 4, ncol=3))
colnames(wilcDF_subOpt) <- c('Comparison','p-value','WStatistic')
wilc <- wilcox.test(percGambIndiv_ICDOff$gambPerc, percGambIndiv_noICDOff$gambPerc, alternative = "two.sided")
wilcDF_subOpt[1,] <- c('ICD Off:non-ICD Off', wilc[3]$p.value %>% round(.,4), wilc[1])
wilc <- wilcox.test(percGambIndiv_ICDOn$gambPerc, percGambIndiv_noICDOn$gambPerc, alternative = "two.sided")
wilcDF_subOpt[2,] <- c('ICD On:non-ICD On', wilc[3]$p.value %>% round(4), wilc[1])
wilc <- wilcox.test(percGambIndiv_noICDOn$gambPerc, percGambIndiv_noICDOff$gambPerc, alternative = "two.sided")
wilcDF_subOpt[3,] <- c('non-ICD On:non-ICD Off', wilc[3]$p.value %>% round(4), wilc[1])
wilc <- wilcox.test(percGambIndiv_ICDOn$gambPerc, percGambIndiv_ICDOff$gambPerc, alternative = "two.sided")
wilcDF_subOpt[4,] <- c('ICD On:ICD Off', wilc[3]$p.value %>% round(4), wilc[1])


## For all trials
percGambAll_ICDOff <- calcIndivGambPerc(happiness_input_ICD_OFF)
percGambAll_noICDOff <- calcIndivGambPerc(happiness_input_noICD_OFF)
percGambAll_ICDOn <- calcIndivGambPerc(happiness_input_ICD_ON)
percGambAll_noICDOn <- calcIndivGambPerc(happiness_input_noICD_ON)

tableS2 <- data.frame(matrix(nrow = 4, ncol = 5))
colnames(tableS2) <- c('Desc', 'Mean','CI_low','to','CI_high')

ci <- CI(percGambAll_noICDOff$gambPerc) %>% round(2)
tableS2[1,] <- c('noICD Off', ci[2], ci[3], 'to', ci[1])

ci <- CI(percGambAll_noICDOn$gambPerc) %>% round(2)
tableS2[2,] <- c('noICD On', ci[2], ci[3], 'to', ci[1])

ci <- CI(percGambAll_ICDOff$gambPerc) %>% as.numeric %>% round(2)
tableS2[3,] <- c('ICD Off', ci[2], ci[3], 'to', ci[1])

ci <- CI(na.omit(percGambAll_ICDOn$gambPerc)) %>% round(2)
tableS2[4,] <- c('ICD On', ci[2], ci[3], 'to', ci[1])

wilcDF <- data.frame(matrix(nrow = 4, ncol=3))
colnames(wilcDF) <- c('Comparison','p-value','WStatistic')
wilc <- wilcox.test(percGambAll_ICDOff$gambPerc, percGambAll_noICDOff$gambPerc, alternative = "two.sided")
wilcDF[1,] <- c('ICD Off:non-ICD Off', wilc[3]$p.value %>% round(.,4), wilc[1])
wilc <- wilcox.test(percGambAll_ICDOn$gambPerc, percGambAll_noICDOn$gambPerc, alternative = "two.sided")
wilcDF[2,] <- c('ICD On:non-ICD On', wilc[3]$p.value %>% round(4), wilc[1])
wilc <- wilcox.test(percGambAll_noICDOn$gambPerc, percGambAll_noICDOff$gambPerc, alternative = "two.sided")
wilcDF[3,] <- c('non-ICD On:non-ICD Off', wilc[3]$p.value %>% round(4), wilc[1])
wilc <- wilcox.test(percGambAll_ICDOn$gambPerc, percGambAll_ICDOff$gambPerc, alternative = "two.sided")
wilcDF[4,] <- c('ICD On:ICD Off', wilc[3]$p.value %>% round(4), wilc[1])

write.xlsx(tableS2, 
           file = file.path(projPath, "Tables", "Supplemental Table 2.xlsx"), 
           sheetName = "Gambling Percentage", 
           row.names = FALSE, append = FALSE)
write.xlsx(wilcDF, 
           file = file.path(projPath, "Tables", "Supplemental Table 2.xlsx"), 
           sheetName = "Wilcoxon Rank Sum Test", 
           row.names = FALSE, append = TRUE)

write.xlsx(tableS5, 
           file = file.path(projPath, "Tables", "Supplemental Table 5.xlsx"),
           sheetName = "Gambling Percentage", 
           row.names = FALSE, append = FALSE)
write.xlsx(wilcDF_subOpt, 
           file = file.path(projPath, "Tables", "Supplemental Table 5.xlsx"), 
           sheetName = "Wilcoxon Rank Sum Test", 
           row.names = FALSE, append = TRUE)
