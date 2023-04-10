## Parse events of interest and their timestamps from the .csv data files
# Load the required libraries
library(stringr)
library(dplyr)

projPath <- '~/Documents/GitHub/Article-Subjective-Feelings-in-ICD'

# Go to data folder and list files
setwd(file.path(projPath,'Data','0_Raw'))
filename = list.files(path = ".", pattern = '.csv', all.files = TRUE)

getRoundNumber <- function(text) {
  str_extract(text,'ROUND \\d\\d\\d\\d|ROUND \\d\\d\\d|ROUND \\d\\d|ROUND \\d') %>% 
    sub('ROUND ', '', .) %>% as.numeric(.)
}

parseData <- function(input)
{
  # Reads data csv files
  output <- read.csv(file = input, header = TRUE)
  # Shorten description variable name (other column output$Time_ms is the time stamp)
  outputText <- output$Description
  # Get start time of game
  startTime <- grep("Game Start by TTL", outputText) %>%
    output$Time_ms[.] %>% as.numeric
  
  optionLines <- grep('SHOW: OPTION SELECTION', outputText)
  optionTime<- (as.numeric(output$Time_ms[optionLines])-startTime) / 1000
  optionRounds<- outputText[optionLines] %>% getRoundNumber()
  surebet<- as.numeric(sub('SURE BET: ', '', str_extract(outputText[optionLines], 'SURE BET: \\d')))
  gambleL<- as.numeric(sub('GAMBLE LEFT BET: ', '', str_extract(outputText[optionLines], 'GAMBLE LEFT BET: \\d')))
  gambleR<- as.numeric(sub('GAMBLE RIGHT BET: ', '', str_extract(outputText[optionLines], 'GAMBLE RIGHT BET: \\d')))
  
  choiceDF<- data.frame(optionRounds, optionTime, surebet, gambleL, gambleR)
  choiceDF$gamblEV <- (choiceDF$gambleL+choiceDF$gambleR)/2
  
  tooLateLines <- grep('SHOW: LATE SCREEN', outputText)
  tooLateTimes <- (as.numeric(output$Time_ms[tooLateLines]) - startTime) / 1000
  #Check each previous line until round # is found
  tooLateRounds <- c()
  index <- 1
  while (index<=length(tooLateLines)) {
    index2<- tooLateLines[index]
    while (is.na(str_extract(outputText[index2], 'ROUND'))) {
      index2<- index2-1
    }
    roundNumb <- outputText[index2] %>% getRoundNumber()
    tooLateRounds <- c(tooLateRounds, roundNumb)
    index <- index+1
  }
  
  if (!is.null(tooLateRounds)) {# If there are too late rounds do this:
    # Create Data Frame for late rounds
    lateDF<-data.frame(tooLateRounds, tooLateTimes, choiceDF[tooLateRounds,])
    # save late rounds before removing from other choiceDFLate
    choiceDFLate<- choiceDF[(tooLateRounds),] 
    # remove too late rounds
    choiceDF <- choiceDF[-(tooLateRounds),] 
  }
  # Choice Made
  choiceMadeLines <- grep("KEYPRESS: 1, OPTION: 1|KEYPRESS: 2, OPTION: 2", outputText)
  choiceMadeTime <- (as.numeric(output$Time_ms[choiceMadeLines])-startTime) / 1000
  # Reaction time
  reactionTime <- choiceMadeTime - choiceDF$optionTime
  choiceDF$reactionTime <- reactionTime
  # Result
  resultLines <- grep("SHOW: Results", outputText)
  resultOnsetTime <- (as.numeric(output$Time_ms[resultLines])-startTime) / 1000
  reward <- as.numeric(sub('reward: ', '', str_extract(outputText[resultLines],'reward: \\d')))
  choiceDF$optionPlusResultDuration <- (resultOnsetTime+1)-choiceDF$optionTime
  
  choiceMade<- str_extract(outputText[resultLines], 'sure bet|gamble')
  choiceMade[choiceMade == 'sure bet'] <- 0 # Code sure bet = 0
  choiceMade[choiceMade == 'gamble'] <- 1 # Code gamble = 1
  choiceMade <- as.numeric(choiceMade)
  choiceFactor <- as.factor(choiceMade)
  rRound<- grep("fixed outcome duration", outputText) %>% outputText[.] %>% getRoundNumber()
  resultDF<- data.frame(rRound, resultOnsetTime,  reward, choiceMade, choiceFactor)
  
  choiceDF$resultOnsetTime <- resultOnsetTime
  choiceDF$reward <- reward
  choiceDF$choice <- choiceMade
  choiceDF$choiceFactor <- choiceFactor
  
  choiceDF$rpe<- rep(0,length(reward))
  choiceDF$rpe[choiceDF$choice == 1] <- choiceDF$reward[choiceDF$choice == 1]-choiceDF$gamblEV[choiceDF$choice == 1]
  
  # Rating
  ratingSubmitLines<- grep('SUBMIT rating value: ', outputText)
  ratings<- outputText[ratingSubmitLines] %>% 
    str_extract(., 'value: \\d|value: -\\d') %>% 
    sub('value: ','',.) %>% 
    as.numeric(.)
  trial <- outputText[ratingSubmitLines] %>% getRoundNumber()
  finalRatingLine <- grep('FINAL RATING SELECTION', outputText)
  if (length(finalRatingLine)==1 && ratingSubmitLines[length(ratingSubmitLines)] > finalRatingLine) { #remove  final rating from ratings
    ratings<- ratings[-length(trial)]
    trial<-  trial[-length(trial)]
  }
  
  # remove ratings for too late rounds
  if (!is.null(tooLateRounds)) {
    ratings <- ratings[!(trial %in% tooLateRounds)]
    trial <- trial[!(trial %in% tooLateRounds)]
  }
  
  ratingDF<- data.frame(trial,ratings, choiceDF[choiceDF$optionRounds  %in%  trial,])
  #z-score = (x-mean) / std
  rating_mean<- mean(ratings)
  rating_sd<- sd(ratings)
  ratingDF$ratingsZScored <- (ratings-rating_mean) / rating_sd
  
  ## Add back in too late rounds
  if (!is.null(tooLateRounds)) { # only run if there are too late rounds
    choiceDFLate$reactionTime <- NaN
    choiceDFLate$optionPlusResultDuration <- NaN
    choiceDFLate$resultOnsetTime <- NaN # Insert NaN for result, reward, choice, and rpe of all too late rounds
    choiceDFLate$reward <- NaN
    choiceDFLate$choice <- NaN
    choiceDFLate$choiceFactor <- NaN
    choiceDFLate$rpe <- NaN
    
    for (tooLateRoundIndex in 1:length(tooLateRounds)) {
      choiceDF <- rbind(choiceDF[1:tooLateRounds[tooLateRoundIndex]-1,], choiceDFLate[tooLateRoundIndex,], choiceDF[tooLateRounds[tooLateRoundIndex]:nrow(choiceDF),])
    }
  }
  
  # Define column names for returned data frame
  colnames(choiceDF)[1] <- 'trial'
  dataDF <- merge(choiceDF,ratingDF, by = c("trial","optionTime","surebet","gambleL","gambleR","reactionTime","optionPlusResultDuration","resultOnsetTime","reward","choice","choiceFactor","rpe","gamblEV"), all = TRUE) 
  # Sort gamble values into larger and smaller values
  dataDF$gambleBIG <- pmax(dataDF$gambleL, dataDF$gambleR) 
  dataDF$gambleSMALL <- pmin(dataDF$gambleL, dataDF$gambleR)
  
  return(dataDF)
}

#create final output by looping through all visits
all_data <- vector("list",length(filename))
key <- as.data.frame(matrix(nrow=length(filename),ncol=3))
colnames(key) <- c('subjID','cohort','status')
names_key <- c()
cohort_key <- c()
status_key <- c()
for (k in 1:length(filename))
{
  all_data[[k]] <- parseData(filename[k]) 
  key$subjID[k] <- str_extract(filename[k],'.*_') %>% sub('_','',.)
  key$cohort[k] <- sub('\\d\\d|\\d', '', key$subjID[k])
  key$status[k] <- str_extract(filename[k],'_.*') %>% sub('_','',.) %>% sub('.csv','',.)
  all_data[[k]]$id <- key$subjID[k]
  all_data[[k]]$group <- paste0(key$cohort[k],'_',key$status[k])
}

#set the names
names(all_data) <- key$subjID
nonICD_off_data <- all_data[ key$cohort=="nonICD" & key$status=="off" ]
nonICD_on_data <- all_data[ key$cohort=="nonICD" & key$status=="on" ]
icd_off_data <- all_data[ key$cohort=="ICD" & key$status=="off" ]
icd_on_data <- all_data[ key$cohort=="ICD" & key$status=="on" ]

#flatten
happiness_input_noICD_OFF <- plyr::ldply(nonICD_off_data, rbind)
happiness_input_noICD_ON <- plyr::ldply(nonICD_on_data , rbind)
happiness_input_ICD_OFF <- plyr::ldply(icd_off_data, rbind)
happiness_input_ICD_ON <- plyr::ldply(icd_on_data, rbind)

# Add type column
happiness_input_noICD_OFF$type <- dplyr::if_else(happiness_input_noICD_OFF$choice == 1 & happiness_input_noICD_OFF$reward == happiness_input_noICD_OFF$gambleSMALL, happiness_input_noICD_OFF$type <- "-1", happiness_input_noICD_OFF$type <- "1") %>% as.character() %>% as.numeric()
happiness_input_noICD_ON$type <- dplyr::if_else(happiness_input_noICD_ON$choice == 1 & happiness_input_noICD_ON$reward == happiness_input_noICD_ON$gambleSMALL, happiness_input_noICD_ON$type <- "-1", happiness_input_noICD_ON$type <- "1") %>% as.character() %>% as.numeric()
happiness_input_ICD_OFF$type <- dplyr::if_else(happiness_input_ICD_OFF$choice == 1 & happiness_input_ICD_OFF$reward == happiness_input_ICD_OFF$gambleSMALL, happiness_input_ICD_OFF$type <- "-1", happiness_input_ICD_OFF$type <- "1") %>% as.character() %>% as.numeric()
happiness_input_ICD_ON$type <- dplyr::if_else(happiness_input_ICD_ON$choice == 1 & happiness_input_ICD_ON$reward == happiness_input_ICD_ON$gambleSMALL, happiness_input_ICD_ON$type <- "-1", happiness_input_ICD_ON$type <- "1") %>% as.character() %>% as.numeric()

# Create folder for processed data if it does not already exist
dir.create(file.path(projPath, "Data", "1_ParsedData"), showWarnings = FALSE)

# Save parsed variables in the "ParsedData" folder
save(happiness_input_noICD_OFF, file = file.path(projPath, "Data", "1_ParsedData","parsed_noICD_Off.RData"))
save(happiness_input_noICD_ON, file = file.path(projPath, "Data", "1_ParsedData","parsed_noICD_On.RData"))
save(happiness_input_ICD_OFF, file = file.path(projPath, "Data", "1_ParsedData","parsed_ICD_Off.RData"))
save(happiness_input_ICD_ON, file = file.path(projPath, "Data", "1_ParsedData","parsed_ICD_On.RData"))

