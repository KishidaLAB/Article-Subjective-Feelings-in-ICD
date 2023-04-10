############################################################################
# Supplemental Table 1b: Group analysis
############################################################################
# Required packages
library(dplyr)
library(xlsx)

# Project path and load variables
projPath <- '~/Documents/GitHub/Article-Subjective-Feelings-in-ICD'
setwd(file.path(projPath, "Tables"))
s1 <- read.csv('Supplemental Table 1a.csv') # Load table S1a

# ICD Status column to sort rows into groups
s1$ICDStatus[grep('NonICD', s1$Patient.ID)] <- 'NonICD'
s1$ICDStatus[grep('^ICD', s1$Patient.ID)] <- 'ICD'

# Tests
ageTTest <- t.test(Age ~ ICDStatus, data = s1)
sexChiTest <- chisq.test(s1$Sex, s1$ICDStatus)
visitOrderTest <- chisq.test(s1$First.Visit.Medication.Status, s1$ICDStatus)

# Create empty matrix for Table S1b
tableS1b <- matrix(rep(NaN, times=27), ncol=9, byrow=TRUE)
#define column names and row names of matrix
colnames(tableS1b) <- c('Sample Size', 'Mean Age','Proportion Male', 
                        'Proportion first visit ON', 'Proportion Carbidopa Levodopa', 
                        'Proportion Pramipexole','Proportion Ropinirole',
                        'Proportion Amantadine','Proportion Other')
rownames(tableS1b) <- c('Non-ICD', 'ICD','P-Value')

# Fill-in table
tableS1b["ICD","Sample Size"] <- sum(s1$ICDStatus == "ICD")
tableS1b["Non-ICD","Sample Size"] <- sum(s1$ICDStatus == "NonICD")
tableS1b["P-Value","Sample Size"] <- ''

tableS1b["ICD","Mean Age"] <- mean(s1$Age[s1$ICDStatus == "ICD"]) %>% round(1)
tableS1b["Non-ICD","Mean Age"] <- mean(s1$Age[s1$ICDStatus == "NonICD"]) %>% round(1)
tableS1b["P-Value","Mean Age"] <- ageTTest$p.value %>% round(4)

tableS1b["ICD","Proportion Male"] <- (sum(s1$Sex[s1$ICDStatus == "ICD"]=="M")/ sum(s1$ICDStatus == "ICD")) %>% round(3)
tableS1b["Non-ICD","Proportion Male"] <- (sum(s1$Sex[s1$ICDStatus == "NonICD"]=="M")/ sum(s1$ICDStatus == "NonICD")) %>% round(3)
tableS1b["P-Value","Proportion Male"] <- sexChiTest$p.value

tableS1b["ICD","Proportion first visit ON"] <- (sum(s1$First.Visit.Medication.Status[s1$ICDStatus == "ICD"]=="ON")/ sum(s1$ICDStatus == "ICD")) %>% round(3)
tableS1b["Non-ICD","Proportion first visit ON"] <- (sum(s1$First.Visit.Medication.Status[s1$ICDStatus == "NonICD"]=="ON")/ sum(s1$ICDStatus == "NonICD")) %>% round(3)
tableS1b["P-Value","Proportion first visit ON"] <- visitOrderTest$p.value %>% round(2)

tableS1b["ICD","Proportion Carbidopa Levodopa"] <- (sum(s1$Carbidopa...Levodopa[s1$ICDStatus == "ICD"]==1)/ sum(s1$ICDStatus == "ICD")) %>% round(3)
tableS1b["Non-ICD","Proportion Carbidopa Levodopa"] <- (sum(s1$Carbidopa...Levodopa[s1$ICDStatus == "NonICD"]==1)/ sum(s1$ICDStatus == "NonICD")) %>% round(3)
tableS1b["P-Value","Proportion Carbidopa Levodopa"] <- '-'

tableS1b["ICD","Proportion Pramipexole"] <- (sum(s1$Pramipexole[s1$ICDStatus == "ICD"]==1)/ sum(s1$ICDStatus == "ICD")) %>% round(3)
tableS1b["Non-ICD","Proportion Pramipexole"] <- (sum(s1$Pramipexole[s1$ICDStatus == "NonICD"]==1)/ sum(s1$ICDStatus == "NonICD")) %>% round(3)
tableS1b["P-Value","Proportion Pramipexole"] <- chisq.test(s1$Pramipexole, s1$ICDStatus)$p.value %>% round(4)

tableS1b["ICD","Proportion Ropinirole"] <- (sum(s1$Ropinirole[s1$ICDStatus == "ICD"]==1)/ sum(s1$ICDStatus == "ICD")) %>% round(3)
tableS1b["Non-ICD","Proportion Ropinirole"] <- (sum(s1$Ropinirole[s1$ICDStatus == "NonICD"]==1)/ sum(s1$ICDStatus == "NonICD")) %>% round(3)
tableS1b["P-Value","Proportion Ropinirole"] <- chisq.test(s1$Ropinirole, s1$ICDStatus)$p.value %>% round(4)

tableS1b["ICD","Proportion Amantadine"] <- (sum(s1$Amantadine[s1$ICDStatus == "ICD"]==1)/ sum(s1$ICDStatus == "ICD")) %>% round(3)
tableS1b["Non-ICD","Proportion Amantadine"] <- (sum(s1$Amantadine[s1$ICDStatus == "NonICD"]==1)/ sum(s1$ICDStatus == "NonICD")) %>% round(3)
tableS1b["P-Value","Proportion Amantadine"] <- chisq.test(s1$Amantadine, s1$ICDStatus)$p.value %>% round(4)

tableS1b["ICD","Proportion Other"] <- (sum(s1$Other[s1$ICDStatus == "ICD"]==1)/ sum(s1$ICDStatus == "ICD")) %>% round(3)
tableS1b["Non-ICD","Proportion Other"] <- (sum(s1$Other[s1$ICDStatus == "NonICD"]==1)/ sum(s1$ICDStatus == "NonICD")) %>% round(3)
tableS1b["P-Value","Proportion Other"] <- chisq.test(s1$Other, s1$ICDStatus)$p.value %>% round(4)

# Save Table
write.xlsx(tableS1b, 
           file = file.path(projPath, "Tables", "Supplemental Table 1b.xlsx"),
           sheetName = "Supplemental Table 1b", 
           row.names = TRUE, append=FALSE)