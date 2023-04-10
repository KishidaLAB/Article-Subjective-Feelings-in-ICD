############################################################################
## Required packages
############################################################################
library(dplyr)
library(ggplot2)
library(cowplot)
library(hBayesDM) # HDIofMCMC

############################################################################
## Set project directory and load group-level distributions
############################################################################
projPath <- '~/Documents/GitHub/Article-Subjective-Feelings-in-ICD'
# Load group-level distributions
groupParams <- readRDS(file.path(projPath,"Data", "2_FittedRatingModel","groupParameters.RDS"))

############################################################################
## Calculate 95% HDI and Credible Intervals for Group-level Difference between Means
############################################################################
# Compare: 1. Off medication: nonICD vs ICD, 2. On medication: nonICD vs ICD, 
#          3. Non-ICD: On vs Off medication, 4. ICD: On vs Off medication
meanDiffStats <- function (groupParams, group1, group2, param) {
  diffMeans <- (groupParams %>% filter(groups==group1) %>% select({{param}}) %>% unlist() %>% as.numeric()) -
    (groupParams %>% filter(groups==group2) %>% select({{param}}) %>% unlist() %>% as.numeric())
  hdiMu <- HDIofMCMC(diffMeans)
  cred_greater <- (sum(diffMeans>0)/length(diffMeans) * 100) %>% round(2) %>% format(., nsmall = 2)
  cred_lesser <- (sum(diffMeans<0)/length(diffMeans) * 100) %>% round(2) %>% format(., nsmall = 2)
  cred <- paste0(cred_lesser, '% < 0 < ', cred_greater,'%')
  return(list(hdiMu, cred))
}
# 1. Off medication: nonICD vs ICD
hdiMu_Off <- data.frame(matrix(nrow = 5, ncol = 4))
rownames(hdiMu_Off) <- c('w0','w1','w2','w3','gam')
colnames(hdiMu_Off) <- c('desc','hdiL','hdiH','cred')
hdiMu_Off[,'desc'] <- 'ICD off - non-ICD off'
for (param in rownames(hdiMu_Off)) {
  hdiMu_Off[param,2:3] <- meanDiffStats(groupParams, 'ICD_Off','noICD_Off', param)[[1]]
  hdiMu_Off[param,4] <- meanDiffStats(groupParams, 'ICD_Off','noICD_Off', param)[[2]]
}
# 2. On medication: nonICD vs ICD
hdiMu_On <- data.frame(matrix(nrow = 5, ncol = 4))
rownames(hdiMu_On) <- c('w0','w1','w2','w3','gam')
colnames(hdiMu_On) <- c('desc','hdiL','hdiH','cred')
hdiMu_On[,'desc'] <- 'ICD on - non-ICD on'
for (param in rownames(hdiMu_Off)) {
  hdiMu_On[param,2:3] <- meanDiffStats(groupParams, 'ICD_On','noICD_On', param)[[1]]
  hdiMu_On[param,4] <- meanDiffStats(groupParams, 'ICD_On','noICD_On', param)[[2]]
}
# 3. Non-ICD: On vs Off Medication
hdiMu_nonICD <- data.frame(matrix(nrow = 5, ncol = 4))
rownames(hdiMu_nonICD) <- c('w0','w1','w2','w3','gam')
colnames(hdiMu_nonICD) <- c('desc','hdiL','hdiH','cred')
hdiMu_nonICD[,'desc'] <- 'non-ICD on - non-ICD off'
for (param in rownames(hdiMu_nonICD)) {
  hdiMu_nonICD[param,2:3] <- meanDiffStats(groupParams, 'noICD_On','noICD_Off', param)[[1]]
  hdiMu_nonICD[param,4] <- meanDiffStats(groupParams, 'noICD_On','noICD_Off', param)[[2]]
}
# 4. ICD: On vs Off Medication
hdiMu_ICD <- data.frame(matrix(nrow = 5, ncol = 4))
rownames(hdiMu_ICD) <- c('w0','w1','w2','w3','gam')
colnames(hdiMu_ICD) <- c('desc','hdiL','hdiH','cred')
hdiMu_ICD[,'desc'] <- 'ICD on - ICD off'
for (param in rownames(hdiMu_ICD)) {
  hdiMu_ICD[param,2:3] <- meanDiffStats(groupParams, 'ICD_On','ICD_Off', param)[[1]]
  hdiMu_ICD[param,4] <- meanDiffStats(groupParams, 'ICD_On','ICD_Off', param)[[2]]
}

############################################################################
## Figure 4: Difference between posterior distributions
############################################################################

# Figure 4a : ICD vs non-ICD OFF Medication, Difference between Mean distributions
# w0
w0Diff_Off <- groupParams %>% filter(groups == "ICD_Off") %>% select(w0) - 
            groupParams %>% filter(groups == "noICD_Off") %>% select(w0)
offCompare_w0 <- ggplot(w0Diff_Off, aes(x=w0)) +
  geom_density(alpha = 0.3, aes(colour='Off medication: Non-ICD - ICD'), fill='grey') + 
  geom_vline(xintercept=0, linetype="dotted") + 
  scale_colour_manual(values=c("black")) +
  geom_segment(aes(x = hdiMu_Off['w0','hdiL'] %>% as.numeric, 
                   xend = hdiMu_Off['w0','hdiH'] %>% as.numeric,   
                   y = 0.01, yend = 0.01), 
               color = I("black"), size = 2, alpha = .7) +
  labs(y='Posterior Density', x=bquote('w'[0])) + 
  theme_classic() + theme(legend.position = "none") +
  annotate("text", x=0.01, y=max(density(w0Diff_Off$w0)$y)*1.05, label= hdiMu_Off['w0','cred'], hjust = 0) + 
  scale_y_continuous(limits = c(0,max(density(w0Diff_Off$w0)$y*1.1)), expand = c(0, 0)) 

# w1
w1Diff_Off <- groupParams %>% filter(groups == "ICD_Off") %>% select(w1) - 
            groupParams %>% filter(groups == "noICD_Off") %>% select(w1)
offCompare_w1 <- ggplot(w1Diff_Off, aes(x=w1))+
  geom_density(alpha = 0.3, aes(colour='Off medication: Non-ICD - ICD'), fill='grey') +  
  geom_vline(xintercept=0, linetype="dotted") + 
  scale_colour_manual(values=c("black")) +
  geom_segment(aes(x = hdiMu_Off['w1','hdiL'] %>% as.numeric,   
                   xend = hdiMu_Off['w1','hdiH'] %>% as.numeric,   
                   y = 0.08, yend = 0.08), 
               color = I("black"), size = 2, alpha = .7) +
  labs(y='Posterior Density', x=bquote('w'[1])) + 
  theme_classic() + theme(legend.position = "none") +
  annotate("text", x=-0.001, y=max(density(w1Diff_Off$w1)$y)*1.05, label= hdiMu_Off['w1','cred'], hjust = 1) + 
  scale_y_continuous(limits = c(0,max(density(w1Diff_Off$w1)$y*1.1)), expand = c(0, 0)) 

# w2
w2Diff_Off <- groupParams %>% filter(groups == "ICD_Off") %>% select(w2) - 
            groupParams %>% filter(groups == "noICD_Off") %>% select(w2)
offCompare_w2<- ggplot(w2Diff_Off, aes(x=w2)) +
  geom_density(alpha = 0.3, aes(colour='Off medication: Non-ICD - ICD'), fill='grey') +  
  geom_vline(xintercept=0, linetype="dotted") + scale_colour_manual(values=c("black")) +
  geom_segment(aes(x = hdiMu_Off['w2','hdiL'] %>% as.numeric,   
                   xend = hdiMu_Off['w2','hdiH'] %>% as.numeric,   
                   y = 0.05, yend = 0.05), 
               color = I("black"), size = 2, alpha = .7) +
  labs(y='Posterior Density', x=bquote('w'[2])) + 
  theme_classic() + theme(legend.position = "none") +
  annotate("text", x=-0.001, y=max(density(w2Diff_Off$w2)$y)*1.05, label= hdiMu_Off['w2','cred'], hjust = 1) + 
  scale_y_continuous(limits = c(0,max(density(w2Diff_Off$w2)$y*1.1)), expand = c(0, 0)) 

# w3
w3Diff_Off <- groupParams %>% filter(groups == "ICD_Off") %>% select(w3) - 
            groupParams %>% filter(groups == "noICD_Off") %>% select(w3)
offCompare_w3<- ggplot(w3Diff_Off, aes(x=w3))+
  geom_density(alpha = 0.3, aes(colour='Off medication: Non-ICD - ICD'), fill='grey') +  
  geom_vline(xintercept=0, linetype="dotted") + 
  scale_colour_manual(values=c("black")) +
  geom_segment(aes(x = hdiMu_Off['w3','hdiL'] %>% as.numeric,   
                   xend = hdiMu_Off['w3','hdiH'] %>% as.numeric,   
                   y = 0.02, yend = 0.02), 
               color = I("black"), size = 2, alpha = .7) +
  labs(y='Posterior Density', x=bquote('w'[3])) + 
  theme_classic() + theme(legend.position = "none") +
  annotate("text", x=-0.001, y=max(density(w3Diff_Off$w3)$y)*1.05, label= hdiMu_Off['w3','cred'], hjust = 1) + 
  scale_y_continuous(limits = c(0,max(density(w3Diff_Off$w3)$y*1.1)), expand = c(0, 0)) 

# Gamma
gamDiff_Off <- groupParams %>% filter(groups == "ICD_Off") %>% select(gam) - 
         groupParams %>% filter(groups == "noICD_Off") %>% select(gam)
offCompare_gam <- ggplot(gamDiff_Off, aes(x=gam)) +
  geom_density(alpha = 0.3, aes(colour='Off medication: Non-ICD - ICD'), fill='grey') +  
  geom_vline(xintercept=0, linetype="dotted") + 
  scale_colour_manual(values=c("black")) +
  geom_segment(aes(x = hdiMu_Off['gam','hdiL'] %>% as.numeric,   
                   xend = hdiMu_Off['gam','hdiH'] %>% as.numeric,   
                   y = 0.02, yend = 0.02), 
               color = I("black"), size = 2, alpha = .7) +
  labs(y='Posterior Density', x=bquote(paste('', gamma))) + 
  theme_classic() + theme(legend.position = "none") +
  annotate("text", x=0.002, y=max(density(gamDiff_Off$gam)$y)*1.05, label= hdiMu_Off['gam','cred'], hjust = 0) + 
  scale_y_continuous(limits = c(0,max(density(gamDiff_Off$gam)$y*1.1)), expand = c(0, 0)) 

############################################################################
# Figure 4b : ICD vs non-ICD ON Medication, Difference between Mean distributions
# w0
w0Diff_on <- groupParams %>% filter(groups == "ICD_On") %>% select(w0) - 
           groupParams %>% filter(groups == "noICD_On") %>% select(w0)
onCompare_w0 <- ggplot(w0Diff_on, aes(x=w0)) +
  geom_density(alpha = 0.3, aes(colour='On medication: ICD - nonICD'), fill='#2981d9') +  
  geom_vline(xintercept=0, linetype="dotted") + 
  scale_colour_manual(values=c("black")) +
  geom_segment(aes(x = hdiMu_On['w0','hdiL'] %>% as.numeric,   
                   xend = hdiMu_On['w0','hdiH'] %>% as.numeric,   
                   y = 0.027, yend = 0.027), 
               color = I("black"), size = 2, alpha = .7) +
  labs(y='Posterior Density', x=bquote(' w'[0])) + 
  theme_classic() + theme(legend.position = "none") +
  annotate("text", x=0.002, y=max(density(w0Diff_on$w0)$y)*1.05, label= hdiMu_On['w0','cred'], hjust = 0) + 
  scale_y_continuous(limits = c(0,max(density(w0Diff_on$w0)$y*1.15)), expand = c(0, 0)) 

# w1
w1Diff_on <- groupParams %>% filter(groups == "ICD_On") %>% select(w1) - 
           groupParams %>% filter(groups == "noICD_On") %>% select(w1)
onCompare_w1 <- ggplot(w1Diff_on, aes(x=w1)) +
  geom_density(alpha = 0.3, aes(colour='On medication: Non-ICD - ICD'), fill='#2981d9') +  
  geom_vline(xintercept=0, linetype="dotted") + scale_colour_manual(values=c("black")) +
  geom_segment(aes(x = hdiMu_On['w1','hdiL'] %>% as.numeric,   
                   xend = hdiMu_On['w1','hdiH'] %>% as.numeric,   
                   y = 0.1, yend = 0.1), 
               color = I("black"), size = 2, alpha = .7) +
  labs(y='Posterior Density', x=bquote('w'[1])) + 
  theme_classic() + theme(legend.position = "none") +
  annotate("text", x=-0.001, y=max(density(w1Diff_on$w1)$y)*1.05, label= hdiMu_On['w1','cred'], hjust = 1) + 
  scale_y_continuous(limits = c(0,max(density(w1Diff_on$w1)$y*1.1)), expand = c(0, 0)) 

# w2
w2Diff_on <- groupParams %>% filter(groups == "ICD_On")   %>% select(w2) - 
             groupParams %>% filter(groups == "noICD_On") %>% select(w2)
onCompare_w2<- ggplot(w2Diff_on, aes(x=w2)) +
  geom_density(alpha = 0.3, aes(colour='On medication: Non-ICD - ICD'), fill='#2981d9') +  
  geom_vline(xintercept=0, linetype="dotted") + scale_colour_manual(values=c("black")) +
  geom_segment(aes(x = hdiMu_On['w2','hdiL'] %>% as.numeric,   
                   xend = hdiMu_On['w2','hdiH'] %>% as.numeric,   
                   y = 0.1, yend = 0.1), 
               color = I("black"), size = 2, alpha = .7) +
  labs(y='Posterior Density', x=bquote('w'[2])) + 
  theme_classic() + theme(legend.position = "none") +
  annotate("text", x=-0.001, y=max(density(w2Diff_on$w2)$y)*1.05, label= hdiMu_On['w2','cred'], hjust = 1) + 
  scale_y_continuous(limits = c(0,max(density(w2Diff_on$w2)$y*1.1)), expand = c(0, 0)) 

# w3
w3Diff_on <- groupParams %>% filter(groups == "ICD_On") %>% select(w3) - 
           groupParams %>% filter(groups == "noICD_On") %>% select(w3)
onCompare_w3<- ggplot(w3Diff_on, aes(x=w3)) +
  geom_density(alpha = 0.3, aes(colour='On medication: Non-ICD - ICD'), fill='#2981d9') + 
  geom_vline(xintercept=0, linetype="dotted") + 
  scale_colour_manual(values=c("black")) +
  geom_segment(aes(x = hdiMu_On['w3','hdiL'] %>% as.numeric,   
                   xend = hdiMu_On['w3','hdiH'] %>% as.numeric,   
                   y = 0.05, yend = 0.05), 
               color = I("black"), size = 2, alpha = .7) +
  labs(y='Posterior Density', x=bquote('w'[3])) + 
  theme_classic() + theme(legend.position = "none") +
  annotate("text", x=-0.01, y=max(density(w3Diff_on$w3)$y)*1.05, label= hdiMu_On['w3','cred'], hjust = 1) + 
  scale_y_continuous(limits = c(0,max(density(w3Diff_on$w3)$y*1.1)), expand = c(0, 0)) 

# gamma
gamDiff_on <- groupParams %>% filter(groups == "ICD_On")   %>% select(gam) - 
              groupParams %>% filter(groups == "noICD_On") %>% select(gam)
onCompare_gam <- ggplot(gamDiff_on, aes(x=gam)) +
  geom_density(alpha = 0.3, aes(colour='On medication: Non-ICD - ICD'), fill='#2981d9') + 
  geom_vline(xintercept=0, linetype="dotted") + scale_colour_manual(values=c("black")) +
  geom_segment(aes(x = hdiMu_On['gam','hdiL'] %>% as.numeric,   
                   xend = hdiMu_On['gam','hdiH'] %>% as.numeric,   
                   y = 0.05, yend = 0.05), 
               color = I("black"), size = 2, alpha = .7) +
  labs(y='Posterior Density', x=bquote(paste(gamma))) + 
  theme_classic() + theme(legend.position = "none") +
  annotate("text", x=-0.005, y=max(density(gamDiff_on$gam)$y)*1.05, label= hdiMu_Off['gam','cred'], hjust = 1) + 
  scale_y_continuous(limits = c(0,max(density(gamDiff_on$gam)$y*1.1)), expand = c(0, 0)) 


############################################################################
# Figure 4c : non-ICD ON vs OFF Medication, Difference between Mean distributions
#w0
w0Diff_nonICD <- groupParams %>% filter(groups == "noICD_On") %>% select(w0) - 
                groupParams %>% filter(groups == "noICD_Off") %>% select(w0)
nonCompare_w0 <- ggplot(w0Diff_nonICD, aes(x=w0)) +
  geom_density(alpha = 0.3, aes(colour='Non-ICD: On - Off medication'), fill='#b3503e') + 
  geom_vline(xintercept=0, linetype="dotted") + scale_colour_manual(values=c("black"))+
  geom_segment(aes(x = hdiMu_nonICD['w0','hdiL'] %>% as.numeric,   
                   xend = hdiMu_nonICD['w0','hdiH'] %>% as.numeric,   
                   y = 0.027, yend = 0.027), 
               color = I("black"), size = 2, alpha = .7) +
  labs(y='Posterior Density', x=bquote('w'[0])) + 
  theme_classic() + theme(legend.position = "none") +
  annotate("text", x=0.002, y = max(density(w0Diff_nonICD$w0)$y)*1.05, label= hdiMu_nonICD['w0','cred'], hjust = 0) + 
  scale_y_continuous(limits=c(0,max(density(w0Diff_nonICD$w0)$y*1.15)), expand = c(0, 0)) 

# w1
w1Diff_nonICD <- groupParams %>% filter(groups == "noICD_On") %>% select(w1) - 
                groupParams %>% filter(groups == "noICD_Off") %>% select(w1)
nonCompare_w1 <- ggplot(w1Diff_nonICD, aes(x=w1)) +
  geom_density(alpha = 0.3, aes(colour='Non-ICD: On - Off medication'), fill='#b3503e') + 
  geom_vline(xintercept=0, linetype="dotted") + 
  scale_colour_manual(values=c("black"))+
  geom_segment(aes(x = hdiMu_nonICD['w1','hdiL'] %>% as.numeric,   
                   xend = hdiMu_nonICD['w1','hdiH'] %>% as.numeric,   
                   y = 0.1, yend = 0.1), 
               color = I("black"), size = 2, alpha = .7) +
  labs(y='Posterior Density', x=bquote('w'[1])) + 
  theme_classic() + theme(legend.position = "none") +
  annotate("text", x=-0.001, y=max(density(w1Diff_nonICD$w1)$y)*1.05, label= hdiMu_nonICD['w1','cred'], hjust = 1) + 
  scale_y_continuous(limits = c(0,max(density(w1Diff_nonICD$w1)$y*1.1)), expand = c(0, 0)) 

#w2
w2Diff_nonICD <- groupParams %>% filter(groups == "noICD_On") %>% select(w2) - 
              groupParams %>% filter(groups == "noICD_Off")   %>% select(w2)
nonCompare_w2<- ggplot(w2Diff_nonICD, aes(x=w2)) +
  geom_density(alpha = 0.3, aes(colour='Non-ICD: On - Off medication'), fill='#b3503e') +  
  geom_vline(xintercept=0, linetype="dotted") + 
  scale_colour_manual(values=c("black")) +
  geom_segment(aes(x = hdiMu_nonICD['w2','hdiL'] %>% as.numeric,   
                   xend = hdiMu_nonICD['w2','hdiH'] %>% as.numeric,   
                   y = 0.1, yend = 0.1), 
               color = I("black"), size = 2, alpha = .7) +
  labs(y='Posterior Density', x=bquote('w'[2])) + 
  theme_classic() + theme(legend.position = "none") +
  annotate("text", x=-0.001, y=max(density(w2Diff_nonICD$w2)$y)*1.05, label= hdiMu_nonICD['w2','cred'], hjust = 1) + 
  scale_y_continuous(limits = c(0,max(density(w2Diff_nonICD$w2)$y*1.1)), expand = c(0, 0)) 

#w3
w3Diff_nonICD <- groupParams %>% filter(groups == "noICD_On") %>% select(w3) - 
                groupParams %>% filter(groups == "noICD_Off") %>% select(w3)
nonCompare_w3<- ggplot(w3Diff_nonICD, aes(x=w3))+
  geom_density(alpha = 0.3, aes(colour='Non-ICD: On - Off medication'), fill='#b3503e') + 
  geom_vline(xintercept=0, linetype="dotted") + scale_colour_manual(values=c("black")) +
  geom_segment(aes(x = hdiMu_nonICD['w3','hdiL'] %>% as.numeric,   
                   xend = hdiMu_nonICD['w3','hdiH'] %>% as.numeric,   
                   y = 0.05, yend = 0.05), 
               color = I("black"), size = 2, alpha = .7) +
  labs(y='Posterior Density', x=bquote('w'[3])) +  
  theme_classic() + theme(legend.position = "none") +
  annotate("text", x=-0.01, y=max(density(w3Diff_nonICD$w3)$y)*1.05, label= hdiMu_nonICD['w3','cred'], hjust = 1)  + 
  scale_y_continuous(limits = c(0,max(density(w3Diff_nonICD$w3)$y*1.1)), expand = c(0, 0)) 

# gamma
gamDiff_nonICD <- groupParams %>% filter(groups == "noICD_On") %>% select(gam) -
               groupParams %>% filter(groups == "noICD_Off")   %>% select(gam)
nonCompare_gam <- ggplot(gamDiff_nonICD, aes(x=gam)) +
  geom_density(alpha = 0.3, aes(colour='Non-ICD: On - Off medication'), fill='#b3503e') + 
  geom_vline(xintercept=0, linetype="dotted") + scale_colour_manual(values=c("black")) +
  geom_segment(aes(x = hdiMu_nonICD['gam','hdiL'] %>% as.numeric,   
                   xend = hdiMu_nonICD['gam','hdiH'] %>% as.numeric,   
                   y = 0.05, yend = 0.05), 
               color = I("black"), size = 2, alpha = .7) +
  labs(y='Posterior Density', x=bquote(paste(gamma))) + 
  theme_classic() + theme(legend.position = "none") +
  annotate("text", x=-0.005, y=max(density(gamDiff_nonICD$gam)$y)*1.05, label= hdiMu_nonICD['gam','cred'], hjust = 1) + 
  scale_y_continuous(limits = c(0,max(density(gamDiff_nonICD$gam)$y*1.1)), expand = c(0, 0)) 


############################################################################
# Figure 4c : ICD ON vs OFF Medication, Difference between Mean distributions
# w0
w0_ICDOn <- groupParams %>% filter(groups == "ICD_On") %>% select(w0)
w0_ICDOff <- groupParams %>% filter(groups == "ICD_Off") %>% select(w0)
w0Diff_ICD <- w0_ICDOn-w0_ICDOff
icdCompare_w0 <- ggplot(w0Diff_ICD, aes(x=w0)) +
  geom_density(alpha = 0.3, aes(colour='ICD: On - Off medication'), fill='#7636a3') +  
  geom_vline(xintercept=0, linetype="dotted") + scale_colour_manual(values=c("black"))+
  geom_segment(aes(x = hdiMu_ICD['w0','hdiL'] %>% as.numeric,   
                   xend = hdiMu_ICD['w0','hdiH'] %>% as.numeric,   
                   y = 0.027, yend = 0.027), 
               color = I("black"), size = 2, alpha = .7) +
  labs(y='Posterior Density', x=bquote('w'[0])) + 
  theme_classic() + theme(legend.position = "none") +
  annotate("text", x=-0.002, y = max(density(w0Diff_ICD$w0)$y)*1.05, label= hdiMu_ICD['w0','cred'], hjust = 1) + 
  scale_y_continuous(limits=c(0,max(density(w0Diff_ICD$w0)$y*1.15)), expand = c(0, 0)) 

# w1
w1_ICDOn <- groupParams %>% filter(groups == "ICD_On") %>% select(w1)
w1_ICDOff <- groupParams %>% filter(groups == "ICD_Off") %>% select(w1)
w1Diff_ICD <- w1_ICDOn-w1_ICDOff
icdCompare_w1 <- ggplot(w1Diff_ICD, aes(x=w1))+
  geom_density(alpha = 0.3, aes(colour='ICD: On - Off medication'), fill='#7636a3') +  
  geom_vline(xintercept=0, linetype="dotted") + 
  scale_colour_manual(values=c("black"))+
  geom_segment(aes(x = hdiMu_ICD['w1','hdiL'] %>% as.numeric,   
                   xend = hdiMu_ICD['w1','hdiH'] %>% as.numeric,   
                   y = 0.1, yend = 0.1), 
               color = I("black"), size = 2, alpha = .7) +
  labs(y='Posterior Density', x=bquote('w'[1])) + 
  theme_classic() + theme(legend.position = "none") +
  annotate("text", x=0.001, y=max(density(w1Diff_ICD$w1)$y)*1.05, label= hdiMu_ICD['w1','cred'], hjust = 0) + 
  scale_y_continuous(limits = c(0,max(density(w1Diff_ICD$w1)$y*1.1)), expand = c(0, 0)) 

# w2
w2_ICDOn <- groupParams %>% filter(groups == "ICD_On") %>% select(w2)
w2_ICDOff   <- groupParams %>% filter(groups == "ICD_Off")   %>% select(w2)
w2Diff_ICD <- w2_ICDOn - w2_ICDOff
icdCompare_w2<- ggplot(w2Diff_ICD, aes(x=w2)) +
  geom_density(alpha = 0.3, aes(colour='ICD: On - Off medication'), fill='#7636a3') +  
  geom_vline(xintercept=0, linetype="dotted") + scale_colour_manual(values=c("black")) +
  geom_segment(aes(x = hdiMu_ICD['w2','hdiL'] %>% as.numeric,   
                   xend = hdiMu_ICD['w2','hdiH'] %>% as.numeric,   
                   y = 0.1, yend = 0.1), 
               color = I("black"), size = 2, alpha = .7) +
  labs(y='Posterior Density', x=bquote('w'[2])) + 
  theme_classic() + theme(legend.position = "none") +
  annotate("text", x=0.001, y=max(density(w2Diff_ICD$w2)$y)*1.05, label= hdiMu_ICD['w2','cred'], hjust = 0) + 
  scale_y_continuous(limits = c(0,max(density(w2Diff_ICD$w2)$y*1.1)), expand = c(0, 0)) 

# w3
w3_ICDOn <- groupParams %>% filter(groups == "ICD_On") %>% select(w3)
w3_ICDOff <- groupParams %>% filter(groups == "ICD_Off") %>% select(w3)
w3Diff_ICD <- w3_ICDOn - w3_ICDOff
icdCompare_w3<- ggplot(w3Diff_ICD, aes(x=w3)) +
  geom_density(alpha = 0.3, aes(colour='ICD: On - Off medication'), fill='#7636a3') + 
  geom_vline(xintercept=0, linetype="dotted") + scale_colour_manual(values=c("black")) +
  geom_segment(aes(x = hdiMu_ICD['w3','hdiL'] %>% as.numeric,   
                   xend = hdiMu_ICD['w3','hdiH'] %>% as.numeric,   
                   y = 0.05, yend = 0.05), 
               color = I("black"), size = 2, alpha = .7) +
  labs(y='Posterior Density', x=bquote('w'[3])) + 
  theme_classic() + theme(legend.position = "none") +
  annotate("text", x=0.01, y=max(density(w3Diff_ICD$w3)$y)*1.05, label= hdiMu_ICD['w3','cred'], hjust = 0) + 
  scale_y_continuous(limits = c(0,max(density(w3Diff_ICD$w3)$y*1.1)), expand = c(0, 0)) 

# gamma
gam_ICDOn <- groupParams %>% filter(groups == "ICD_On") %>% select(gam)
gam_ICDOff   <- groupParams %>% filter(groups == "ICD_Off") %>% select(gam)
gamDiff_ICD <- gam_ICDOn - gam_ICDOff
icdCompare_gam <- ggplot(gamDiff_ICD, aes(x=gam)) +
  geom_density(alpha = 0.3, aes(colour='ICD: On - Off medication'), fill='#7636a3') +  
  geom_vline(xintercept=0, linetype="dotted") + scale_colour_manual(values=c("black")) +
  geom_segment(aes(x = hdiMu_ICD['gam','hdiL'] %>% as.numeric,   xend = hdiMu_ICD['gam','hdiH'] %>% as.numeric,   
                   y = 0.05, yend = 0.05), color = I("black"), size = 2, alpha = .7) +
  labs(y='Posterior Density', x=bquote(paste(gamma))) + 
  theme_classic() + theme(legend.position = "none") +
  annotate("text", x=-0.005, y=max(density(gamDiff_ICD$gam)$y)*1.05, label= hdiMu_ICD['gam','cred'], hjust = 1) + 
  scale_y_continuous(limits = c(0,max(density(gamDiff_ICD$gam)$y*1.1)), expand = c(0, 0)) 

############################################################################
## Figure 4: Difference between posterior distributions
## Put together plots & save
############################################################################
fig4a <- plot_grid(offCompare_w0, offCompare_w1, offCompare_w2, offCompare_w3, offCompare_gam, nrow = 1)
fig4b <- plot_grid(onCompare_w0, onCompare_w1, onCompare_w2, onCompare_w3, onCompare_gam, nrow = 1)
fig4c <- plot_grid(nonCompare_w0, nonCompare_w1, nonCompare_w2, nonCompare_w3, nonCompare_gam, nrow = 1)
fig4d <- plot_grid(icdCompare_w0, icdCompare_w1, icdCompare_w2, icdCompare_w3, icdCompare_gam, nrow = 1)

ggsave(file.path(projPath,'Plots', 'Figure 4.png'), 
       plot_grid(fig4a, fig4b, fig4c, fig4d, ncol = 1, labels=c('A','B','C','D')),
       width = 19, height = 10, dpi = 600)

############################################################################
## Figure 4 Plot Legends
############################################################################
# Redo some plots for the legends
legendData<- data.frame(c(w3Diff_Off$w3, w3Diff_on$w3, w3Diff_nonICD$w3, w3Diff_ICD$w3), 
           c(rep('Off medication: ICD - Non-ICD', dim(w3Diff_Off)[1]),
             rep('On medication: ICD - Non-ICD', dim(w3Diff_on)[1]),
             rep('Non-ICD: On - Off medication',dim(w3Diff_nonICD)[1]),
             rep('ICD: On - Off medication', dim(w3Diff_ICD)[1])))
colnames(legendData) <- c('data','comparison')
legendData$comparison <- factor(legendData$comparison, levels=c('Off medication: ICD - Non-ICD', 
                                                                'On medication: ICD - Non-ICD', 
                                                                'Non-ICD: On - Off medication',
                                                                'ICD: On - Off medication'))

leg <- ggplot(legendData, aes(x=data, colour = comparison, fill = comparison)) +
  geom_density(alpha = 0.3) + 
  theme_classic() + 
  scale_color_manual(values=c("black","black","black","black"))+
  scale_fill_manual(values=c('grey','#2981d9','#b3503e','#7636a3'))+
  theme(legend.position="bottom")

ggsave(file.path(projPath,'Plots', 'Figure 4 Legend.png'), 
       leg, width = 10, height = 5, dpi = 600)
