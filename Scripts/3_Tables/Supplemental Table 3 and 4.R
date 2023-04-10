############################################################################
## Required packages
############################################################################
library(Rmisc)
library(dplyr)
library(xlsx)

############################################################################
## Set Project path and load variables
############################################################################
projPath <- '~/Documents/GitHub/Article-Subjective-Feelings-in-ICD'
setwd(file.path(projPath, "Data", "2_FittedRatingModel"))
indivParams <- readRDS("individualParameters.RDS")

############################################################################
## Parameter Estimates and Confidence Intervals for each Group
############################################################################

# Create Supplemental Table 3 data frame
tableS3 <- data.frame(matrix(ncol = 7, nrow = 26))
colnames(tableS3) <- c('group','desc','w0','w1', 'w2','w3','gamma')

# Function to format estimates and CIs
summarizeCoeffs <- function (indivParams, group) {
  return_df <- data.frame(matrix(ncol = 5, nrow = 4))
  for ( columnIdx in c(1:(ncol(indivParams)-1)) ) {
    estimate <- CI( indivParams %>% filter(groups == group) %>% .[[columnIdx]], ci=0.95) %>%
      sprintf(., fmt=paste0("%#.", c(3,4,3), "f")) # Round values to 4 places for mean, 3 places for CI
    return_df[,columnIdx] <- c(estimate[2], estimate[3],'to', estimate[1]) # Add 'to' for CI
  }
  return(return_df)
}

# Fill-in table
tableS3[1:4,1] <- c('ICD Off Medication')
tableS3[1:4,2] <- c('Parameter Estimate','95% Confidence Interval - Low','to','95% Confidence Interval - High')
tableS3[1:4,3:7] <- summarizeCoeffs(indivParams, 'ICD_Off')

tableS3[5:8,1] <- c('Non-ICD Off Medication')
tableS3[5:8,2] <- c('Parameter Estimate','95% Confidence Interval - Low','to','95% Confidence Interval - High')
tableS3[5:8,3:7] <- summarizeCoeffs(indivParams, 'noICD_Off')

tableS3[14:17,1] <- c('ICD On Medication')
tableS3[14:17,2] <- c('Parameter Estimate','95% Confidence Interval - Low','to','95% Confidence Interval - High')
tableS3[14:17,3:7] <- summarizeCoeffs(indivParams, 'ICD_On')

tableS3[18:21,1] <- c('Non-ICD On Medication')
tableS3[18:21,2] <- c('Parameter Estimate','95% Confidence Interval - Low','to','95% Confidence Interval - High')
tableS3[18:21,3:7] <- summarizeCoeffs(indivParams, 'noICD_On')

############################################################################
## Compare parameters between ICD and Non-ICD groups when On and Off medication

# Order groups for tukey
indivParams$groupsFactor <- indivParams$groups
indivParams$groupsFactor[indivParams$groupsFactor=="ICD_On"]<- "4_ICD_On"
indivParams$groupsFactor[indivParams$groupsFactor=="noICD_On"]<- "3_noICD_On"
indivParams$groupsFactor[indivParams$groupsFactor=="ICD_Off"]<- "2_ICD_Off"
indivParams$groupsFactor[indivParams$groupsFactor=="noICD_Off"]<- "1_noICD_Off"
indivParams$groupsFactor <- as.factor(indivParams$groupsFactor)

# Tukey
tukeyResults <- list (w0 = aov(w0_indiv~groupsFactor, data=indivParams) %>% TukeyHSD(.),
                      w1 = aov(w1_indiv~groupsFactor, data=indivParams) %>% TukeyHSD(.),
                      w2 = aov(w2_indiv~groupsFactor, data=indivParams) %>% TukeyHSD(.),
                      w3 = aov(w3_indiv~groupsFactor, data=indivParams) %>% TukeyHSD(.),
                     gam = aov(gam_indiv~groupsFactor, data=indivParams) %>% TukeyHSD(.))

# Function to format Tukey results to insert into the table
formatTukey <- function (tukeyResult) {
  # Create empty data frame to store string values
  tukeyFormatted <- data.frame(matrix(nrow=nrow(tukeyResult), ncol = ncol(tukeyResult)))
  colnames(tukeyFormatted) <- colnames(tukeyResult)
  rownames(tukeyFormatted) <- rownames(tukeyResult)
  # Round values
  tukeyFormatted[1,] <- tukeyResult[1,] %>% round(4) %>% format(., nsmall=4) # Round Mean Difference to 4 places
  tukeyFormatted[2:3,] <- tukeyResult[2:3,] %>% round(3) %>% format(., nsmall=3) # Round CI to 3 places
  tukeyFormatted[4,] <- tukeyResult[4,] %>% # Round p-val to 4 places or 3 significant digits if smaller than 0.001
    apply(., 2,function(x){if(x>=0.0001) format(round(x,4), nsmall=4) else signif(x,digits=3); }) 
  # Add "to" row for the table
  tukeyFormatted <- rbind(tukeyFormatted[1:2,], 'to' = rep('to',5), tukeyFormatted[3:4,]) 
  # Return
  return(tukeyFormatted)
}

# Comparisons
off.icd_noICD <- lapply(tukeyResults, function (x){x$groups[row.names(x$groups) == "2_ICD_Off-1_noICD_Off",];}) %>%
  data.frame() %>% formatTukey()
on.icd_noICD <- lapply(tukeyResults, function (x){x$groups[row.names(x$groups) == "4_ICD_On-3_noICD_On",];}) %>%
  data.frame() %>% formatTukey()

# Add to table
tableS3[9:13,1] <- c('ICD Off : non-ICD Off')
tableS3[9:13,2] <- c('Difference in Means (MD)','95% Confidence Interval - Low','to','95% Confidence Interval - High', 'P-value')
tableS3[9:13,3:7] <- off.icd_noICD

tableS3[22:26,1] <- c('ICD On : non-ICD On')
tableS3[22:26,2] <- c('Difference in Means (MD)','95% Confidence Interval - Low','to','95% Confidence Interval - High', 'P-value')
tableS3[22:26,3:7] <- on.icd_noICD

# Save Table
write.xlsx(tableS3, 
           file = file.path(projPath, "Tables", "Supplemental Table 3.xlsx"),
           sheetName = "Supplemental Table 3", 
           row.names = FALSE, append=FALSE)

############################################################################
## Supplemental Table 4
############################################################################

# Comparisons
noICD.on_off <- lapply(tukeyResults, function (x){x$groups[row.names(x$groups) == "3_noICD_On-1_noICD_Off",];}) %>%
  data.frame() %>% formatTukey() %>% t()
icd.on_off <- lapply(tukeyResults, function (x){x$groups[row.names(x$groups) == "4_ICD_On-2_ICD_Off",];}) %>%
  data.frame() %>% formatTukey() %>% t()

# Create Supplemental Table 4 data frame
tableS4 <- data.frame(matrix(ncol = 7, nrow = 10))
colnames(tableS4) <- c('Comparison','Parameter','MD','CILow', 'CIto','CIHigh','pVal')

# Fill-in table
tableS4$Comparison[1:5] <- c('non-ICD On : non-ICD Off') 
tableS4$Parameter[1:5] <- c('Baseline (w0)', 'Certain Reward (w1)', 'Gamble EV (w2)', 'RPE (w3)', 'Recent Experience Weight (gamma)')
tableS4[1:5,3:7]<- noICD.on_off
tableS4$Comparison[6:10] <- c('ICD On : ICD Off') 
tableS4$Parameter[6:10] <- c('Baseline (w0)', 'Certain Reward (w1)', 'Gamble EV (w2)', 'RPE (w3)', 'Recent Experience Weight (gamma)')
tableS4[6:10,3:7]<- icd.on_off

# Rename columns detailed names
colnames(tableS4) <- c('Comparison','Parameter','Difference in Means (MD)',
                       '95% Confidence Interval - Low', 'to','95% Confidence Interval - High','P-Value')
# Save Table
write.xlsx(tableS4, 
           file = file.path(projPath, "Tables", "Supplemental Table 4.xlsx"),
           sheetName = "Supplemental Table 4", 
           row.names = FALSE, append=FALSE)