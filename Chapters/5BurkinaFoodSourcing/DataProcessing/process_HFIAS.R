#################
#HFIAS indicators
#
#
library(dplyr)
library(tidyr)


rawHFIAS <- read.csv('5_3 Securite alimentaire.csv')

#which(colnames(rawHFIAS) == "Inqutepanoud") #start index for HFIAP
# rawHFIAS$HFIAP <- ifelse(rawHFIAS[,15] %in% c("A") | rawHFIAS[,16] %in% c("A") | rawHFIAS[,17] %in% c("A", "B", "C", "D", "F") | rawHFIAS[,18] %in% c("A", "B", "C", "D", "F") | rawHFIAS[,19] %in% c("A", "B", "C", "D", "F"),  "SeverelyFI", NA)
# rawHFIAS$HFIAP <- ifelse(is.na(rawHFIAS$HFIAP == T), ifelse(rawHFIAS[,13] %in% c("A", "B", "C") | rawHFIAS[,14] %in% c("A", "B", "C") | rawHFIAS[,15] %in% c("B", "C", "D", "F") | rawHFIAS[,16] %in% c("B", "C", "D", "F"),  "ModeratelyFI", NA), rawHFIAS$HFIAP)
# rawHFIAS$HFIAP <- ifelse(is.na(rawHFIAS$HFIAP == T), ifelse(rawHFIAS[,11] %in% c("A", "B", "C", "D") | rawHFIAS[,12] %in% c("A", "B", "C", "D", "F") | rawHFIAS[,13] %in% c("D", "F") | rawHFIAS[,14] %in% c("D", "F"),  "MildlyFI", NA), rawHFIAS$HFIAP)
# rawHFIAS$HFIAP <- ifelse(is.na(rawHFIAS$HFIAP == T) & rawHFIAS[,11] %in% c("E", "F"),  "FoodSecure", rawHFIAS$HFIAP)

#HFIASdataGood <- select(rawHFIAS, Household.ID, ends_with("n"), -Quatouann)
#HFIASdatabad <- select(rawHFIAS, Household.ID, ends_with("d"))

HFIAS_status_good <- rep(0, nrow(rawHFIAS))

for (index in 1:length(rawHFIAS$Household.ID)) {
  if  (rawHFIAS$Rienmangn[index]%in%c('F','D','C','B','A')|rawHFIAS$Dorsanmangn[index]%in%c('F','D','C','B','A')|rawHFIAS$Toujousanman[index]%in%c('F','D','C','B','A')) {
    HFIAS_status_good[index]<-'SeverelyFI'
  } else { 
    if (rawHFIAS$Mangqumoinn[index]=='A'|rawHFIAS$Mangmoisoun[index]=='A') {
      HFIAS_status_good[index]<-'SeverelyFI'
    }
    if (rawHFIAS$Mangqumoinn[index]%in%c('B','C')|rawHFIAS$Mangmoisoun[index]%in%c('B','C')) {
      HFIAS_status_good[index]<-'ModeratelyFI'
    }
  }
  if (HFIAS_status_good[index]=='0') {
    if (rawHFIAS$Pamangvarin[index]%in%c('D','F','C')|rawHFIAS$Mangnevoun[index]%in%c('D','F','C')) {
      HFIAS_status_good[index]<-'MildlyFI'
    }
    if (rawHFIAS$Pamangvarin[index]%in%c('B','A')|rawHFIAS$Mangnevoun[index]%in%c('B','A')) {
      HFIAS_status_good[index]<-'ModeratelyFI'
    }
  }
  if (HFIAS_status_good[index]=='0') {
    if (rawHFIAS$Pamangen[index]%in%c('F','D','C','B','A')) {
      HFIAS_status_good[index]<-'MildlyFI'
    }
  }
  if (HFIAS_status_good[index]=='0') {
    if (rawHFIAS$Inqutepanoun[index]%in%c('B','A')) {
      HFIAS_status_good[index]<-'MildlyFI'
    }
  }
  if (HFIAS_status_good[index]=='0') {
    HFIAS_status_good[index]<-'FoodSecure'
  }
}

rawHFIAS$HFIAS_status_good <- HFIAS_status_good


HFIAS_status <- rep(0, nrow(rawHFIAS))

for (index in 1:length(rawHFIAS$Household.ID)) {
  if  (rawHFIAS$Rienmangd[index]%in%c('F','D','C','B','A')|rawHFIAS$Dorsanmangd[index]%in%c('F','D','C','B','A')|rawHFIAS$Toujousanmad[index]%in%c('F','D','C','B','A')) {
    HFIAS_status[index]<-'SeverelyFI'
  } else { 
    if (rawHFIAS$Mangqumoind[index]=='A'|rawHFIAS$Mangmoisoud[index]=='A') {
      HFIAS_status[index]<-'SeverelyFI'
    }
    if (rawHFIAS$Mangqumoind[index]%in%c('B','C')|rawHFIAS$Mangmoisoud[index]%in%c('B','C')) {
      HFIAS_status[index]<-'ModeratelyFI'
    }
  }
  if (HFIAS_status[index]=='0') {
    if (rawHFIAS$Pamangvarid[index]%in%c('D','F','C')|rawHFIAS$Mangnevoud[index]%in%c('D','F','C')) {
      HFIAS_status[index]<-'MildlyFI'
    }
    if (rawHFIAS$Pamangvarid[index]%in%c('B','A')|rawHFIAS$Mangnevoud[index]%in%c('B','A')) {
      HFIAS_status[index]<-'ModeratelyFI'
    }
  }
  if (HFIAS_status[index]=='0') {
    if (rawHFIAS$Pamanged[index]%in%c('F','D','C','B','A')) {
      HFIAS_status[index]<-'MildlyFI'
    }
  }
  if (HFIAS_status[index]=='0') {
    if (rawHFIAS$Inqutepanoud[index]%in%c('B','A')) {
      HFIAS_status[index]<-'MildlyFI'
    }
  }
  if (HFIAS_status[index]=='0') {
    HFIAS_status[index]<-'FoodSecure'
  }
}

rawHFIAS$HFIAS_status_bad <- HFIAS_status
