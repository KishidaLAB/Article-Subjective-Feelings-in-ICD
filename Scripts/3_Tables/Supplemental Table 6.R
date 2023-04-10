# Required packages
library(stringr)
library(dplyr)
library(xlsx)

# Set Project path and load variables
projPath <- '~/Documents/GitHub/Article-Subjective-Feelings-in-ICD'
setwd(file.path(projPath, "Data", "0_Raw"))
dataFiles <- list.files(pattern='*.csv')

############################################################################
# Supplemental Table 6
############################################################################
# Set-up variable for table
subjects<- dataFiles %>% sub('_.*.csv','',.) %>% unique()
tableS6 <- data.frame(matrix(ncol = 7, nrow = length(subjects)))
colnames(tableS6) <- c('Patient ID','ICD Status','Number','Off Total Trials','Off Late Trials','On Total Trials','On Late Trials')
tableS6$`Patient ID` <- subjects

# Get total number of trials and too late trials
for (dataFile in dataFiles) {
  subject <- dataFile %>% sub('_.*.csv','',.)
  medStatus <- str_extract(dataFile, '_on|_off') %>% sub('_','',.)
  # Read data
  output <- read.csv(file = dataFile, header = TRUE)
  # Parse data
  totalRounds <- grep('ROUND', output$Description) %>% last() %>% 
    output$Description[.] %>% str_extract(., 'ROUND \\d\\d\\d|ROUND \\d\\d|ROUND\\d') %>%
    sub('ROUND ','',.) %>% as.numeric
  totalLateRounds <- grep('TOO LATE', output$Description) %>% length()
  # Insert in to table
  rowIdx <- tableS6$`Patient ID` == subject
  if (medStatus == 'on') {
    tableS6$`On Total Trials`[rowIdx] <- totalRounds
    tableS6$`On Late Trials`[rowIdx] <- totalLateRounds
  } else if (medStatus == 'off') {
    tableS6$`Off Total Trials`[rowIdx] <- totalRounds
    tableS6$`Off Late Trials`[rowIdx] <- totalLateRounds
  }
  tableS6$`ICD Status`[rowIdx] <- str_extract(dataFile, '^ICD|^nonICD')
  tableS6$`Number`[rowIdx] <- str_extract(dataFile, '\\d\\d|\\d') %>% as.numeric()
  # Clear variables in loop
  rm(list=c('subject', 'medStatus', 'output', 'rowIdx', 'totalRounds', 'totalLateRounds'))
}

# Order rows by patient IDs
tableS6$`Patient ID` <- gsub("D(\\d*)", "D \\1", tableS6$`Patient ID`) # Add space
tableS6 <- tableS6 %>% arrange(`Number`) %>% arrange(`ICD Status`)
tableS6 <- subset(tableS6, select = -c(`Number`,`ICD Status`) )

# Mean and Standard Deviation
tableS6[31,] <- c('Mean', apply(tableS6[,2:5],2,mean) %>% round(3))
tableS6[32,] <- c('Standard Deviation', apply(tableS6[,2:5],2,sd) %>% round(3))

tableS6[33,1] <- 'Total Trials Completed for all visits (Mean and Standard Deviation)'
tableS6[33,2] <- paste(mean(c(tableS6[1:length(subjects),2], tableS6[1:length(subjects),4]) %>% as.numeric),
                       sd(c(tableS6[1:length(subjects),2], tableS6[1:length(subjects),4]) %>% as.numeric) 
                       %>% round(3),
                       sep = ' ± ')
tableS6[33,3:5] <- c('')

# T-test
ttestTrials <- t.test(tableS6[1:length(subjects),2] %>% as.numeric(), 
               tableS6[1:length(subjects),4] %>% as.numeric(), 
               paired = TRUE, alternative = "less")

tableS6[34,1:5] <- c('Supplementary Table 6b.', 'Off : On Medication','','','')
tableS6[35,1:5] <- c('P-value', ttestTrials$p.value %>% round(5),'','','')
tableS6[36,1:5] <- c('t-statistic', ttestTrials$statistic %>% round(4),'','','')
tableS6[37,1:5] <- c('df', ttestTrials$parameter,'','','')

# Save Table
write.xlsx(tableS6, 
           file = file.path(projPath, "Tables", "Supplemental Table 6.xlsx"),
           sheetName = "Supplemental Table 6", 
           row.names = FALSE, append=FALSE)