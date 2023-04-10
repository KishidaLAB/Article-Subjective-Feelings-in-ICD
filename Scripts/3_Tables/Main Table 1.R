library(dplyr)
library(Rmisc)
library(xlsx)

projPath <- '~/Documents/GitHub/Article-Subjective-Feelings-in-ICD'
load(file.path(projPath, "Data", "4_GambleChoiceModel", "gambleChoiceModelCoeff.RData"))

table1 <- data.frame(matrix(ncol = 8, nrow = 8))
colnames(table1) <- c('Non-ICD Parameter Estimate','Non-ICD 95% Confidence Interval', 'Non-ICD P-value', 'Non-ICD T-Statistic', 
                      'ICD Parameter Estimate','ICD 95% Confidence Interval', 'ICD P-value', 'ICD T-Statistic')
rownames(table1) <- c('Off Baseline', 'Off Gamble EV', 'Off Certain Reward EV','Off Subject Feeling on (t-1)',
                      'On Baseline', 'On Gamble EV', 'On Certain Reward EV','On Subject Feeling on (t-1)')

summarizeCoeffs <- function (coeffs) {
  returnDF <- data.frame(matrix(ncol = 4, nrow = 4))
  colnames(returnDF) <- c('Estimate','CI','PValue','TStat')

  for (columnIdx in c(1:ncol(coeffs))) {
    # Parameter Estimate & Confidence intervals
    estimate <- CI( coeffs[,columnIdx], ci=0.95) %>%
      sprintf(., fmt=paste0("%#.", c(3,4,3), "f"))
    # T-test
    ttest_out <- t.test(coeffs[,columnIdx], mu=0 )
    returnDF[columnIdx,] <- c(estimate[2], 
                      paste(toString(estimate[3]) ,'to', toString(estimate[1])),
                      ttest_out$p.value %>% {if(.>0.001) round(.,4) else signif(.,digits=3)},
                      ttest_out$statistic %>% round(4))
    rm(list= c('estimate', 'ttest_out'))
  }
  return(returnDF)
}

table1[1:4, 1:4] <- summarizeCoeffs(coeff_noICD_off)
table1[1:4, 5:8] <- summarizeCoeffs(coeff_ICD_off)
table1[5:8, 1:4] <- summarizeCoeffs(coeff_noICD_on)
table1[5:8, 5:8] <- summarizeCoeffs(coeff_ICD_on)

write.xlsx(table1, 
           file = file.path(projPath, "Tables", "Main Table 1.xlsx"),
           sheetName = "Gamble Choice Model Parameters", 
           row.names = TRUE, append=FALSE)
