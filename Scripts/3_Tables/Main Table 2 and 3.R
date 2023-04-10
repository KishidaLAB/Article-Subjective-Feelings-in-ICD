library(dplyr)
library(effsize) # Cohen's D
library(xlsx) # Save excel file
library(hBayesDM) # HDIofMCMC

projPath <- '~/Documents/GitHub/Article-Subjective-Feelings-in-ICD'
# Load group-level distributions
groupParams <- readRDS(file.path(projPath,"Data", "2_FittedRatingModel","groupParameters.RDS"))
# Load group-level HDIs
hdi <- readRDS(file.path(projPath,"Data", "2_FittedRatingModel","groupHDI.RDS"))

###########################################
## Cohen's D
cohensDTable <- matrix(nrow=5, ncol=6) # 5 parameters, 4 comparisons
rowIdx <- 1
cohensDTable[rowIdx,1] <- cohen.d(groupParams %>% filter(groups=='ICD_Off') %>% select(w0) %>% unlist() %>% as.numeric(),  
                                  groupParams %>% filter(groups=='noICD_Off') %>% select(w0)%>% unlist() %>% as.numeric())$estimate
cohensDTable[rowIdx,2] <- cohen.d(groupParams %>% filter(groups=='ICD_On') %>% select(w0) %>% unlist() %>% as.numeric(),  
                                  groupParams %>% filter(groups=='noICD_On') %>% select(w0)%>% unlist() %>% as.numeric())$estimate
cohensDTable[rowIdx,3] <- cohen.d(groupParams %>% filter(groups=='noICD_On') %>% select(w0) %>% unlist() %>% as.numeric(),  
                                  groupParams %>% filter(groups=='noICD_Off') %>% select(w0)%>% unlist() %>% as.numeric())$estimate
cohensDTable[rowIdx,4] <- cohen.d(groupParams %>% filter(groups=='ICD_On') %>% select(w0) %>% unlist() %>% as.numeric(),  
                                  groupParams %>% filter(groups=='ICD_Off') %>% select(w0)%>% unlist() %>% as.numeric())$estimate
cohensDTable[rowIdx,5] <- cohen.d(groupParams %>% filter(groups=='noICD_Off') %>% select(w0) %>% unlist() %>% as.numeric(),  
                                  groupParams %>% filter(groups=='ICD_On') %>% select(w0)%>% unlist() %>% as.numeric())$estimate
cohensDTable[rowIdx,6] <- cohen.d(groupParams %>% filter(groups=='noICD_On') %>% select(w0) %>% unlist() %>% as.numeric(),  
                                  groupParams %>% filter(groups=='ICD_Off') %>% select(w0)%>% unlist() %>% as.numeric())$estimate

rowIdx <- 2
cohensDTable[rowIdx,1] <- cohen.d(groupParams %>% filter(groups=='ICD_Off') %>% select(w1) %>% unlist() %>% as.numeric(),  
                                  groupParams %>% filter(groups=='noICD_Off') %>%   select(w1)%>% unlist() %>% as.numeric())$estimate
cohensDTable[rowIdx,2] <- cohen.d(groupParams %>% filter(groups=='ICD_On') %>%  select(w1) %>% unlist() %>% as.numeric(),  
                                  groupParams %>% filter(groups=='noICD_On') %>%    select(w1)%>% unlist() %>% as.numeric())$estimate
cohensDTable[rowIdx,3] <- cohen.d(groupParams %>% filter(groups=='noICD_On') %>% select(w1) %>% unlist() %>% as.numeric(),  
                                  groupParams %>% filter(groups=='noICD_Off') %>%  select(w1)%>% unlist() %>% as.numeric())$estimate
cohensDTable[rowIdx,4] <- cohen.d(groupParams %>% filter(groups=='ICD_On') %>%   select(w1) %>% unlist() %>% as.numeric(),  
                                  groupParams %>% filter(groups=='ICD_Off') %>%    select(w1)%>% unlist() %>% as.numeric())$estimate
cohensDTable[rowIdx,5] <- cohen.d(groupParams %>% filter(groups=='noICD_Off') %>% select(w1) %>% unlist() %>% as.numeric(),  
                                  groupParams %>% filter(groups=='ICD_On') %>%    select(w1)%>% unlist() %>% as.numeric())$estimate
cohensDTable[rowIdx,6] <- cohen.d(groupParams %>% filter(groups=='noICD_On') %>%  select(w1) %>% unlist() %>% as.numeric(),  
                                  groupParams %>% filter(groups=='ICD_Off') %>%   select(w1)%>% unlist() %>% as.numeric())$estimate
rowIdx <- 3
cohensDTable[rowIdx,1] <- cohen.d(groupParams %>% filter(groups=='ICD_Off') %>% select(w2) %>% unlist() %>% as.numeric(),  
                                  groupParams %>% filter(groups=='noICD_Off') %>%   select(w2)%>% unlist() %>% as.numeric())$estimate
cohensDTable[rowIdx,2] <- cohen.d(groupParams %>% filter(groups=='ICD_On') %>%  select(w2) %>% unlist() %>% as.numeric(),  
                                  groupParams %>% filter(groups=='noICD_On') %>%    select(w2)%>% unlist() %>% as.numeric())$estimate
cohensDTable[rowIdx,3] <- cohen.d(groupParams %>% filter(groups=='noICD_On') %>% select(w2) %>% unlist() %>% as.numeric(),  
                                  groupParams %>% filter(groups=='noICD_Off') %>%  select(w2)%>% unlist() %>% as.numeric())$estimate
cohensDTable[rowIdx,4] <- cohen.d(groupParams %>% filter(groups=='ICD_On') %>%   select(w2) %>% unlist() %>% as.numeric(),  
                                  groupParams %>% filter(groups=='ICD_Off') %>%    select(w2)%>% unlist() %>% as.numeric())$estimate
cohensDTable[rowIdx,5] <- cohen.d(groupParams %>% filter(groups=='noICD_Off') %>% select(w2) %>% unlist() %>% as.numeric(),  
                                  groupParams %>% filter(groups=='ICD_On') %>%    select(w2)%>% unlist() %>% as.numeric())$estimate
cohensDTable[rowIdx,6] <- cohen.d(groupParams %>% filter(groups=='noICD_On') %>%  select(w2) %>% unlist() %>% as.numeric(),  
                                  groupParams %>% filter(groups=='ICD_Off') %>%   select(w2)%>% unlist() %>% as.numeric())$estimate
rowIdx <- 4
cohensDTable[rowIdx,1] <- cohen.d(groupParams %>% filter(groups=='ICD_Off') %>% select(w3) %>% unlist() %>% as.numeric(),  
                                  groupParams %>% filter(groups=='noICD_Off') %>%   select(w3)%>% unlist() %>% as.numeric())$estimate
cohensDTable[rowIdx,2] <- cohen.d(groupParams %>% filter(groups=='ICD_On') %>%  select(w3) %>% unlist() %>% as.numeric(),  
                                  groupParams %>% filter(groups=='noICD_On') %>%    select(w3)%>% unlist() %>% as.numeric())$estimate
cohensDTable[rowIdx,3] <- cohen.d(groupParams %>% filter(groups=='noICD_On') %>% select(w3) %>% unlist() %>% as.numeric(),  
                                  groupParams %>% filter(groups=='noICD_Off') %>%  select(w3)%>% unlist() %>% as.numeric())$estimate
cohensDTable[rowIdx,4] <- cohen.d(groupParams %>% filter(groups=='ICD_On') %>%   select(w3) %>% unlist() %>% as.numeric(),  
                                  groupParams %>% filter(groups=='ICD_Off') %>%    select(w3)%>% unlist() %>% as.numeric())$estimate
cohensDTable[rowIdx,5] <- cohen.d(groupParams %>% filter(groups=='noICD_Off') %>% select(w3) %>% unlist() %>% as.numeric(),  
                                  groupParams %>% filter(groups=='ICD_On') %>%    select(w3)%>% unlist() %>% as.numeric())$estimate
cohensDTable[rowIdx,6] <- cohen.d(groupParams %>% filter(groups=='noICD_On') %>%  select(w3) %>% unlist() %>% as.numeric(),  
                                  groupParams %>% filter(groups=='ICD_Off') %>%   select(w3)%>% unlist() %>% as.numeric())$estimate
rowIdx <- 5
cohensDTable[rowIdx,1] <- cohen.d(groupParams %>% filter(groups=='ICD_Off') %>% select(gam) %>% unlist() %>% as.numeric(),  
                                  groupParams %>% filter(groups=='noICD_Off') %>%   select(gam)%>% unlist() %>% as.numeric())$estimate
cohensDTable[rowIdx,2] <- cohen.d(groupParams %>% filter(groups=='ICD_On') %>%  select(gam) %>% unlist() %>% as.numeric(),  
                                  groupParams %>% filter(groups=='noICD_On') %>%    select(gam)%>% unlist() %>% as.numeric())$estimate
cohensDTable[rowIdx,3] <- cohen.d(groupParams %>% filter(groups=='noICD_On') %>% select(gam) %>% unlist() %>% as.numeric(),  
                                  groupParams %>% filter(groups=='noICD_Off') %>%  select(gam)%>% unlist() %>% as.numeric())$estimate
cohensDTable[rowIdx,4] <- cohen.d(groupParams %>% filter(groups=='ICD_On') %>%   select(gam) %>% unlist() %>% as.numeric(),  
                                  groupParams %>% filter(groups=='ICD_Off') %>%    select(gam)%>% unlist() %>% as.numeric())$estimate
cohensDTable[rowIdx,5] <- cohen.d(groupParams %>% filter(groups=='noICD_Off') %>% select(gam) %>% unlist() %>% as.numeric(),  
                                  groupParams %>% filter(groups=='ICD_On') %>%    select(gam)%>% unlist() %>% as.numeric())$estimate
cohensDTable[rowIdx,6] <- cohen.d(groupParams %>% filter(groups=='noICD_On') %>%  select(gam) %>% unlist() %>% as.numeric(),  
                                  groupParams %>% filter(groups=='ICD_Off') %>%   select(gam)%>% unlist() %>% as.numeric())$estimate
rownames(cohensDTable)<- c('w0 intercept','w1 sure bet','w2 gamble EV','w3 RPE','gam discount')
colnames(cohensDTable)<- c('ICD vs noICD Off','ICD vs noICD On','noICD On vs Off','ICD On vs Off','noICD Off vs ICD On','noICD On vs ICD Off')


## Table 2
table2 <- data.frame(matrix(ncol = 7, nrow = 26))
colnames(table2) <- c('Group','Description','w0', 'w1', 'w2', 'w3','gam')

table2[1,] <- c('ICD Off Medication', 'Parameter Estimate', 
                groupParams$w0[groupParams$groups == 'ICD_Off'] %>% mean() %>% round(4), 
                groupParams$w1[groupParams$groups == 'ICD_Off'] %>% mean() %>% round(4), 
                groupParams$w2[groupParams$groups == 'ICD_Off'] %>% mean() %>% round(4), 
                groupParams$w3[groupParams$groups == 'ICD_Off'] %>% mean() %>% round(4), 
                groupParams$gam[groupParams$groups == 'ICD_Off'] %>% mean() %>% round(4))
table2[2,] <- c('ICD Off Medication', '95% HDI Low', 
                hdi$icd_Off$w0[1] %>% round(3)  %>% format(., nsmall = 3), 
                hdi$icd_Off$w1[1] %>% round(3), 
                hdi$icd_Off$w2[1] %>% round(3),
                hdi$icd_Off$w3[1] %>% round(3),
                hdi$icd_Off$gam[1] %>% round(3))
table2[3,] <- c('ICD Off Medication', '95% HDI', rep('to',5))
table2[4,] <- c('ICD Off Medication', '95% HDI High', 
                hdi$icd_Off$w0[2] %>% round(3), 
                hdi$icd_Off$w1[2] %>% round(3), 
                hdi$icd_Off$w2[2] %>% round(3),
                hdi$icd_Off$w3[2] %>% round(3),
                hdi$icd_Off$gam[2] %>% round(3))


table2[5,] <- c('Non-ICD Off Medication', 'Parameter Estimate',
                groupParams$w0[groupParams$groups == 'noICD_Off'] %>% mean() %>% round(4), 
                groupParams$w1[groupParams$groups == 'noICD_Off'] %>% mean() %>% round(4), 
                groupParams$w2[groupParams$groups == 'noICD_Off'] %>% mean() %>% round(4), 
                groupParams$w3[groupParams$groups == 'noICD_Off'] %>% mean() %>% round(4), 
                groupParams$gam[groupParams$groups == 'noICD_Off'] %>% mean() %>% round(4))
table2[6,] <- c('Non-ICD Off Medication', '95% HDI Low', 
                hdi$noICD_Off$w0[1] %>% round(3), 
                hdi$noICD_Off$w1[1] %>% round(3), 
                hdi$noICD_Off$w2[1] %>% round(3),
                hdi$noICD_Off$w3[1] %>% round(3),
                hdi$noICD_Off$gam[1] %>% round(3))
table2[7,] <- c('Non-ICD Off Medication', '95% HDI', rep('to',5))
table2[8,] <- c('Non-ICD Off Medication', '95% HDI High', 
                hdi$noICD_Off$w0[2] %>% round(3), 
                hdi$noICD_Off$w1[2] %>% round(3), 
                hdi$noICD_Off$w2[2] %>% round(3),
                hdi$noICD_Off$w3[2] %>% round(3),
                hdi$noICD_Off$gam[2] %>% round(3))
table2[9,] <- c("ICD Off: non-ICD Off", "Difference in Means (Cohen's d)", -cohensDTable[,1] %>% round(3)) 

table2[14,] <- c('ICD On Medication', 'Parameter Estimate',
                 groupParams$w0[groupParams$groups == 'ICD_On'] %>% mean() %>% round(4), 
                 groupParams$w1[groupParams$groups == 'ICD_On'] %>% mean() %>% round(4), 
                 groupParams$w2[groupParams$groups == 'ICD_On'] %>% mean() %>% round(4), 
                 groupParams$w3[groupParams$groups == 'ICD_On'] %>% mean() %>% round(4), 
                 groupParams$gam[groupParams$groups == 'ICD_On'] %>% mean() %>% round(4))
table2[15,] <- c('ICD On Medication', '95% HDI Low', 
                 hdi$icd_On$w0[1] %>% round(3), 
                 hdi$icd_On$w1[1] %>% round(3), 
                 hdi$icd_On$w2[1] %>% round(3),
                 hdi$icd_On$w3[1] %>% round(3),
                 hdi$icd_On$gam[1] %>% round(3))
table2[16,] <- c('ICD On Medication', '95% HDI', rep('to',5))
table2[17,] <- c('ICD On Medication', '95% HDI High', 
                 hdi$icd_On$w0[2] %>% round(3), 
                 hdi$icd_On$w1[2] %>% round(3), 
                 hdi$icd_On$w2[2] %>% round(3),
                 hdi$icd_On$w3[2] %>% round(3),
                 hdi$icd_On$gam[2] %>% round(3))

table2[18,] <- c('Non-ICD On Medication', 'Parameter Estimate',
                 groupParams$w0[groupParams$groups == 'noICD_On'] %>% mean() %>% round(4), 
                 groupParams$w1[groupParams$groups == 'noICD_On'] %>% mean() %>% round(4), 
                 groupParams$w2[groupParams$groups == 'noICD_On'] %>% mean() %>% round(4), 
                 groupParams$w3[groupParams$groups == 'noICD_On'] %>% mean() %>% round(4), 
                 groupParams$gam[groupParams$groups == 'noICD_On'] %>% mean() %>% round(4))
table2[19,] <- c('Non-ICD On Medication', '95% HDI Low', 
                 hdi$noICD_On$w0[1] %>% round(3), 
                 hdi$noICD_On$w1[1] %>% round(3), 
                 hdi$noICD_On$w2[1] %>% round(3),
                 hdi$noICD_On$w3[1] %>% round(3),
                 hdi$noICD_On$gam[1] %>% round(3))
table2[20,] <- c('Non-ICD On Medication', '95% HDI', rep('to',5))
table2[21,] <- c('Non-ICD On Medication', '95% HDI High', 
                 hdi$noICD_On$w0[2] %>% round(3), 
                 hdi$noICD_On$w1[2] %>% round(3), 
                 hdi$noICD_On$w2[2] %>% round(3),
                 hdi$noICD_On$w3[2] %>% round(3),
                 hdi$noICD_On$gam[2] %>% round(3))
table2[22,] <- c("ICD Off: non-ICD Off", "Difference in Means (Cohen's d)", -cohensDTable[,2] %>% round(3)) 

## Compare ICD and non-ICD groups when: 1. Off medication, 2. On medication
meanDiffStats <- function (groupParams, group1, group2, param) {
  diffMeans <- (groupParams %>% filter(groups==group1) %>% select({{param}}) %>% unlist() %>% as.numeric()) -
    (groupParams %>% filter(groups==group2) %>% select({{param}}) %>% unlist() %>% as.numeric())
  hdiMu <- HDIofMCMC(diffMeans) %>% round(., 4) %>% format(., nsmall = 4)
  cred_greater <- (sum(diffMeans>0)/length(diffMeans) * 100) %>% round(2) %>% format(., nsmall = 2)
  cred_lesser <- (sum(diffMeans<0)/length(diffMeans) * 100) %>% round(2) %>% format(., nsmall = 2)
  cred <- paste0(cred_lesser, '% < 0 < ', cred_greater,'%')
  return(c(hdiMu[1], 'to', hdiMu[2], cred))
}
# 1. Off Medication
table2$Group[10:13]<- c('ICD Off : non-ICD Off')
table2$Description[10:13] <- c('95% HDI Low','95% HDI to','95% HDI High',
                               '% Credible differences greater & less than 0')
table2$w0[10:13] <- meanDiffStats(groupParams, 'ICD_Off','noICD_Off', w0)
table2$w1[10:13] <- meanDiffStats(groupParams, 'ICD_Off','noICD_Off', w1)
table2$w2[10:13] <- meanDiffStats(groupParams, 'ICD_Off','noICD_Off', w2)
table2$w3[10:13] <- meanDiffStats(groupParams, 'ICD_Off','noICD_Off', w3)
table2$gam[10:13] <- meanDiffStats(groupParams, 'ICD_Off','noICD_Off', gam)

# 2. On Medication
table2$Group[23:26]<- c('ICD On : non-ICD On')
table2$Description[23:26] <- c('95% HDI Low','95% HDI to','95% HDI High',
                               '% Credible differences greater & less than 0')
table2$w0[23:26] <- meanDiffStats(groupParams, 'ICD_On','noICD_On', w0)
table2$w1[23:26] <- meanDiffStats(groupParams, 'ICD_On','noICD_On', w1)
table2$w2[23:26] <- meanDiffStats(groupParams, 'ICD_On','noICD_On', w2)
table2$w3[23:26] <- meanDiffStats(groupParams, 'ICD_On','noICD_On', w3)
table2$gam[23:26] <- meanDiffStats(groupParams, 'ICD_On','noICD_On', gam)

write.xlsx(table2, 
           file = file.path(projPath, "Tables", "Main Table 2.xlsx"), 
           sheetName = "Params", row.names = FALSE, append = FALSE)



## Table 3
table3 <- data.frame(matrix(ncol = 7, nrow = 10))
colnames(table3) <- c('Comparison','Parameter','CohensD','HDILow','HDIto', 'HDIHigh', 'CredDiff')
table3$Comparison[1:5] <- c('non-ICD On : non-ICD Off') 
table3$Parameter[1:5] <- c('Baseline (w0)', 'Certain Reward (w1)', 'Gamble EV (w2)', 'RPE (w3)', 'Recent Experience Weight (gamma)')
table3$CohensD[1:5] <- cohensDTable[,3]
table3[1,4:7] <- meanDiffStats(groupParams, 'noICD_On','noICD_Off', w0)
table3[2,4:7] <- meanDiffStats(groupParams, 'noICD_On','noICD_Off', w1)
table3[3,4:7] <- meanDiffStats(groupParams, 'noICD_On','noICD_Off', w2)
table3[4,4:7] <- meanDiffStats(groupParams, 'noICD_On','noICD_Off', w3)
table3[5,4:7] <- meanDiffStats(groupParams, 'noICD_On','noICD_Off', gam)

table3$Comparison[6:10] <- c('ICD On : ICD Off') 
table3$Parameter[6:10] <- c('Baseline (w0)', 'Certain Reward (w1)', 'Gamble EV (w2)', 'RPE (w3)', 'Recent Experience Weight (gamma)')
table3$CohensD[6:10] <- cohensDTable[,4]
table3[6,4:7] <- meanDiffStats(groupParams, 'ICD_On','ICD_Off', w0)
table3[7,4:7] <- meanDiffStats(groupParams, 'ICD_On','ICD_Off', w1)
table3[8,4:7] <- meanDiffStats(groupParams, 'ICD_On','ICD_Off', w2)
table3[9,4:7] <- meanDiffStats(groupParams, 'ICD_On','ICD_Off', w3)
table3[10,4:7] <- meanDiffStats(groupParams, 'ICD_On','ICD_Off', gam)

colnames(table3) <- c("Comparison","Parameter","Cohen's d","95% HDI Low", "95% HDI to","95% HDI High","% Credible Differences")

write.xlsx(table3, 
           file = file.path(projPath, "Tables", "Main Table 3.xlsx"), 
           sheetName = "Params", row.names = FALSE, append = FALSE)
