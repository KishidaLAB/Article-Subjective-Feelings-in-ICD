# Figure 3
############################################################################
## Required packages
############################################################################
library(ggplot2)
library(cowplot)

############################################################################
## Set project directory and load data
############################################################################
projPath <- '~/Documents/GitHub/Article-Subjective-Feelings-in-ICD'
# Group-level distributions
groupParams <- readRDS(file.path(projPath,"Data", "2_FittedRatingModel","groupParameters.RDS"))
# Group-level HDIs
hdi <- readRDS(file.path(projPath,"Data", "2_FittedRatingModel","groupHDI.RDS"))
# Individual-level means
indiv_means <- readRDS(file.path(projPath,"Data", "2_FittedRatingModel","individualParameters.RDS"))

###########################################################################
# Posterior Density Plots with HDI and individual means on the HDIs
###########################################################################
# Plot Aesthetics
ptSz <- 3
alphVal <- 0.5
densAlpha <- 0.2
groupColors <- c("#d7191c", "#fdae61", "#2c7bb6", "#abd9e9")
groupLnColors <- c("#d7191c", "#fdae61", "#2c7bb6", "black")
fontSz <- 15

# Set y-axis offset in graph for each group's individual means
indiv_means$groupsY <- c(-1)
indiv_means$groupsY[indiv_means$groups=='ICD_Off'] <- -0
indiv_means$groupsY[indiv_means$groups=='ICD_On'] <- -1
indiv_means$groupsY[indiv_means$groups=='noICD_Off'] <- -2
indiv_means$groupsY[indiv_means$groups=='noICD_On'] <- -3

# Plot
hdiYVal_w0 <- seq(-0.1,-0.5,-0.1)
hdi2_w0 <- ggplot(groupParams, aes(x=w0, color=groups, fill=groups))+
  geom_density(alpha = 0.3) + theme_classic() + labs(y='Posterior Density', x=bquote(w[0])) +
  geom_segment(aes(x = hdi$icd_Off$w0[1],   xend = hdi$icd_Off$w0[2],   y = hdiYVal_w0[1], yend = hdiYVal_w0[1]), color = I(groupColors[1]), size = 1.5, alpha = .7) +
  geom_segment(aes(x = hdi$icd_On$w0[1],    xend = hdi$icd_On$w0[2],    y = hdiYVal_w0[2], yend = hdiYVal_w0[2]), color = I(groupColors[2]), size = 1.5, alpha = .7) +
  geom_segment(aes(x = hdi$noICD_Off$w0[1], xend = hdi$noICD_Off$w0[2], y = hdiYVal_w0[3], yend = hdiYVal_w0[3]), color = I(groupColors[3]), size = 1.5, alpha = .7) +
  geom_segment(aes(x = hdi$noICD_On$w0[1],  xend = hdi$noICD_On$w0[2],  y = hdiYVal_w0[4], yend = hdiYVal_w0[4]), color = I(groupColors[4]), size = 1.5, alpha = .7)+
  geom_point(indiv_means, mapping = aes(x=w0_indiv, y=groupsY*.1-0.1), color='black', stroke=0, alpha=0.5, size=ptSz)  + 
  scale_color_manual(values=groupLnColors)+
  scale_fill_manual(values=groupColors)+
  theme(legend.position = "none", text = element_text(size = fontSz)) 

hdiYVal_w1 <- seq(-0.4,-2.5,-0.7)
hdi2_w1 <- ggplot(groupParams, aes(x=w1, color=groups, fill=groups))+
  geom_density(alpha = 0.3) + theme_classic() + labs(y='Posterior Density', x=bquote(w[1])) +
  geom_segment(aes(x = hdi$icd_Off$w1[1],   xend = hdi$icd_Off$w1[2],   y = hdiYVal_w1[1], yend = hdiYVal_w1[1]), color = I(groupColors[1]), size = 1.5, alpha = .7) +
  geom_segment(aes(x = hdi$icd_On$w1[1],    xend = hdi$icd_On$w1[2],    y = hdiYVal_w1[2], yend = hdiYVal_w1[2]), color = I(groupColors[2]), size = 1.5, alpha = .7) +
  geom_segment(aes(x = hdi$noICD_Off$w1[1], xend = hdi$noICD_Off$w1[2], y = hdiYVal_w1[3], yend = hdiYVal_w1[3]), color = I(groupColors[3]), size = 1.5, alpha = .7) +
  geom_segment(aes(x = hdi$noICD_On$w1[1],  xend = hdi$noICD_On$w1[2],  y = hdiYVal_w1[4], yend = hdiYVal_w1[4]), color = I(groupColors[4]), size = 1.5, alpha = .7)+
  geom_point(indiv_means, mapping = aes(x=w1_indiv, y=groupsY*.7-0.4), color='black', stroke=0, alpha=0.5, size=ptSz)   + 
  scale_color_manual(values=groupLnColors)+
  scale_fill_manual(values=groupColors)+
  theme(legend.position = "none", text = element_text(size = fontSz)) 

hdiYVal_w2 <- seq(-0.3,-1.8,-0.5)
hdi2_w2 <- ggplot(groupParams, aes(x=w2, color=groups, fill=groups))+
  geom_density(alpha = 0.3) + theme_classic() + labs(y='Posterior Density', x=bquote(w[2])) +
  geom_segment(aes(x = hdi$icd_Off$w2[1],   xend = hdi$icd_Off$w2[2],   y = hdiYVal_w2[1], yend = hdiYVal_w2[1]), color = I(groupColors[1]), size = 1.5, alpha = .7) +
  geom_segment(aes(x = hdi$icd_On$w2[1],    xend = hdi$icd_On$w2[2],    y = hdiYVal_w2[2], yend = hdiYVal_w2[2]), color = I(groupColors[2]), size = 1.5, alpha = .7) +
  geom_segment(aes(x = hdi$noICD_Off$w2[1], xend = hdi$noICD_Off$w2[2], y = hdiYVal_w2[3], yend = hdiYVal_w2[3]), color = I(groupColors[3]), size = 1.5, alpha = .7) +
  geom_segment(aes(x = hdi$noICD_On$w2[1],  xend = hdi$noICD_On$w2[2],  y = hdiYVal_w2[4], yend = hdiYVal_w2[4]), color = I(groupColors[4]), size = 1.5, alpha = .7)+
  geom_point(indiv_means, mapping = aes(x=w2_indiv, y=groupsY*.5-0.3), color='black', stroke=0, alpha=0.5, size=ptSz)   + 
  scale_color_manual(values=groupLnColors)+
  scale_fill_manual(values=groupColors)+
  theme(legend.position = "none", text = element_text(size = fontSz)) 

hdiYVal_w3 <- seq(-0.3,-1.8,-0.5)
hdi2_w3 <- ggplot(groupParams, aes(x=w3, color=groups, fill=groups))+
  geom_density(alpha = 0.3) + theme_classic() + labs(y='Posterior Density', x=bquote(w[3])) +
  geom_segment(aes(x = hdi$icd_Off$w3[1],   xend = hdi$icd_Off$w3[2],   y = hdiYVal_w3[1], yend = hdiYVal_w3[1]), color = I(groupColors[1]), size = 1.5, alpha = .7) +
  geom_segment(aes(x = hdi$icd_On$w3[1],    xend = hdi$icd_On$w3[2],    y = hdiYVal_w3[2], yend = hdiYVal_w3[2]), color = I(groupColors[2]), size = 1.5, alpha = .7) +
  geom_segment(aes(x = hdi$noICD_Off$w3[1], xend = hdi$noICD_Off$w3[2], y = hdiYVal_w3[3], yend = hdiYVal_w3[3]), color = I(groupColors[3]), size = 1.5, alpha = .7) +
  geom_segment(aes(x = hdi$noICD_On$w3[1],  xend = hdi$noICD_On$w3[2],  y = hdiYVal_w3[4], yend = hdiYVal_w3[4]), color = I(groupColors[4]), size = 1.5, alpha = .7)+
  geom_point(indiv_means, mapping = aes(x=w3_indiv, y=groupsY*.5-0.3), color='black', stroke=0, alpha=0.5, size=ptSz)   + 
  scale_color_manual(values=groupLnColors)+
  scale_fill_manual(values=groupColors)+
  theme(legend.position = "none", text = element_text(size = fontSz)) 

hdiYVal_gam <- seq(-0.2,-1.1,-0.3)
hdi2_gam <- ggplot(groupParams, aes(x=gam, color=groups, fill=groups))+
  geom_density(alpha = 0.3) + theme_classic() + labs(y='Posterior Density', x=bquote(gamma)) +
  geom_segment(aes(x = hdi$icd_Off$gam[1],   xend = hdi$icd_Off$gam[2],   y = hdiYVal_gam[1], yend = hdiYVal_gam[1]), color = I(groupColors[1]), size = 1.5, alpha = .7) +
  geom_segment(aes(x = hdi$icd_On$gam[1],    xend = hdi$icd_On$gam[2],    y = hdiYVal_gam[2], yend = hdiYVal_gam[2]), color = I(groupColors[2]), size = 1.5, alpha = .7) +
  geom_segment(aes(x = hdi$noICD_Off$gam[1], xend = hdi$noICD_Off$gam[2], y = hdiYVal_gam[3], yend = hdiYVal_gam[3]), color = I(groupColors[3]), size = 1.5, alpha = .7) +
  geom_segment(aes(x = hdi$noICD_On$gam[1],  xend = hdi$noICD_On$gam[2],  y = hdiYVal_gam[4], yend = hdiYVal_gam[4]), color = I(groupColors[4]), size = 1.5, alpha = .7)+
  geom_point(indiv_means, mapping = aes(x=gam_indiv, y=groupsY*.3-0.2), colour='black', stroke=0, alpha=0.5, size=ptSz)  + 
  scale_color_manual(values=groupLnColors)+
  scale_fill_manual(values=groupColors)+
  theme(legend.position = "none", text = element_text(size = fontSz)) 

# Save
ggsave(file.path(projPath,'Plots', 'Figure 3.png'),
       plot_grid(hdi2_w0, hdi2_w1, hdi2_w2, hdi2_w3, hdi2_gam, nrow = 3), width = 12, height = 12, dpi = 600)


###########################################################################
# Replot for legends
###########################################################################
# Re-label groups with it's long name
groupParamsLeg <- groupParams
groupParamsLeg$groups[groupParamsLeg$groups== "noICD_Off"] <- "Non-ICD Off Medication"
groupParamsLeg$groups[groupParamsLeg$groups== "noICD_On"] <- "Non-ICD On Medication"
groupParamsLeg$groups[groupParamsLeg$groups== "ICD_Off"] <- "ICD Off Medication"
groupParamsLeg$groups[groupParamsLeg$groups== "ICD_On"] <- "ICD On Medication"

indiv_meansLeg <- indiv_means
indiv_meansLeg$groups[indiv_meansLeg$groups== "noICD_Off"] <- "Non-ICD Off Medication"
indiv_meansLeg$groups[indiv_meansLeg$groups== "noICD_On"] <- "Non-ICD On Medication"
indiv_meansLeg$groups[indiv_meansLeg$groups== "ICD_Off"] <- "ICD Off Medication"
indiv_meansLeg$groups[indiv_meansLeg$groups== "ICD_On"] <- "ICD On Medication"

# Plot with legend
hdi2_leg <- ggplot(groupParamsLeg, aes(x=gam, color=groups, fill=groups))+
  geom_density(alpha = 0.3) + theme_classic() + labs(y='Posterior Density', x=bquote(gamma)) +
  geom_segment(aes(x = hdi$icd_Off$gam[1],   xend = hdi$icd_Off$gam[2],   y = hdiYVal_gam[1], yend = hdiYVal_gam[1]), color = I(groupColors[1]), size = 1.5, alpha = .7) +
  geom_segment(aes(x = hdi$icd_On$gam[1],    xend = hdi$icd_On$gam[2],    y = hdiYVal_gam[2], yend = hdiYVal_gam[2]), color = I(groupColors[2]), size = 1.5, alpha = .7) +
  geom_segment(aes(x = hdi$noICD_Off$gam[1], xend = hdi$noICD_Off$gam[2], y = hdiYVal_gam[3], yend = hdiYVal_gam[3]), color = I(groupColors[3]), size = 1.5, alpha = .7) +
  geom_segment(aes(x = hdi$noICD_On$gam[1],  xend = hdi$noICD_On$gam[2],  y = hdiYVal_gam[4], yend = hdiYVal_gam[4]), color = I(groupColors[4]), size = 1.5, alpha = .7)+
  geom_point(indiv_meansLeg, mapping = aes(x=gam_indiv, y=groupsY*.3-0.2), colour='black', stroke=0, alpha=0.5, size=ptSz)  + 
  scale_color_manual(values=groupLnColors) + 
  scale_fill_manual(values=groupColors)

# Save
ggsave(file.path(projPath,'Plots', 'Figure 3 Legend.png'), 
       hdi2_leg, width = 4, height = 4, dpi = 600)
