#
#    __________________          To count food categories eaten by season and source
#_.-", ,' .'. ,  `. .  "-._      Tested r versions: 3.4.1
#.'. `    .    ,  `  .  ' '  `.  Authors: Simon Fraval and Mark van Wijk
#.`____________________________'.
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#  `""""""""""""""""""""""""""""""'
#`____________________________' mh
#ASCII credit: Hamburger, Maija
####################################3
#HDDS purchased
rawHDDS <- read.csv('5_4 Diversity nutritionnel.csv', stringsAsFactors = F)



rawHDDSsel <- data.frame(lapply(rawHDDS[,-c(1:6)], function(x) {
  x <- gsub("A", 1, x)
  x <- gsub("B", 1, x)
  x <- gsub("C", 1, x)
  x <- gsub("D", 0, x)
  x <- gsub("E", 0, x)
  x <- gsub("F", 0, x)
})) #stringsAsFactors = F

rawHDDSsel <- data.frame(Household.ID = rawHDDS$Household.ID, lapply(rawHDDSsel, function(x) {as.numeric(levels(x))[x]}))

rawHDDSConsumed <- (select(rawHDDSsel, Household.ID, contains("Con")))
rawHDDSConsumedBad <- (select(rawHDDSConsumed, Household.ID, ends_with("d")))
rawHDDSConsumedGood <- (select(rawHDDSConsumed, Household.ID, ends_with("f")))

rawHDDSConsumedBad$HDDS_bad <- rowSums(select(rawHDDSConsumedBad, -Household.ID), na.rm=T)
rawHDDSConsumedGood$HDDS_good <- rowSums(select(rawHDDSConsumedGood, -Household.ID), na.rm=T)

datHDDS <- left_join(select(rawHDDSConsumedBad, Household.ID, HDDS_bad), select(rawHDDSConsumedGood, Household.ID, HDDS_good))

rawHDDSsource <- (select(rawHDDSsel, contains("Source")))
rawHDDSsourceBad <- (select(rawHDDSsource, ends_with("d")))
rawHDDSsourceGood <- (select(rawHDDSsource, ends_with("f")))

#HDDS sourced from purchases
#If both purchased and produced then counted in both farm and purch as 1
rawHDDSsourceBadPurch <- rawHDDSsourceBad
rawHDDSsourceBadPurch[rawHDDSsourceBadPurch == 1] <- 0
rawHDDSsourceBadPurch[rawHDDSsourceBadPurch == 3 | rawHDDSsourceBadPurch == 2] <- 1
#rawHDDSsourceBadPurch[rawHDDSsourceBadPurch == 1 | rawHDDSsourceBadPurch == 3] <- 0 #exclude all cases where it is purchased and farm supplied
#rawHDDSsourceBadPurch[rawHDDSsourceBadPurch == 2] <- 1

rawHDDSsourceBadPurch <- rawHDDSConsumedBad[c(2:13)] * rawHDDSsourceBadPurch
rawHDDSsourceBadPurch$HDDS_bad_purch <- rowSums(rawHDDSsourceBadPurch, na.rm=T)

rawHDDSsourceGoodPurch <- rawHDDSsourceGood
rawHDDSsourceGoodPurch[rawHDDSsourceGoodPurch == 1] <- 0
rawHDDSsourceGoodPurch[rawHDDSsourceGoodPurch == 3 | rawHDDSsourceGoodPurch == 2] <- 1
#rawHDDSsourceGoodPurch[rawHDDSsourceGoodPurch == 1 | rawHDDSsourceGoodPurch == 3] <- 0
#rawHDDSsourceGoodPurch[rawHDDSsourceGoodPurch == 2] <- 1


rawHDDSsourceGoodPurch <- rawHDDSConsumedGood[c(2:13)] * rawHDDSsourceGoodPurch
rawHDDSsourceGoodPurch$HDDS_good_purch <- rowSums(rawHDDSsourceGoodPurch, na.rm=T)

rawHDDSsourceTotalPurch <- rawHDDSsourceBad
rawHDDSsourceTotalPurch1 <- rawHDDSsourceGood
rawHDDSsourceTotalPurch[rawHDDSsourceTotalPurch == 1] <- 0
rawHDDSsourceTotalPurch[rawHDDSsourceTotalPurch == 2 | rawHDDSsourceTotalPurch == 3] <- 1
#rawHDDSsourceTotalPurch[rawHDDSsourceTotalPurch == 3] <- 0
rawHDDSsourceTotalPurch1[rawHDDSsourceTotalPurch1 == 1] <- 0
rawHDDSsourceTotalPurch1[rawHDDSsourceTotalPurch1 == 2 | rawHDDSsourceTotalPurch1 == 3] <- 1
#rawHDDSsourceTotalPurch1[rawHDDSsourceTotalPurch1 == 3] <- 0
rawHDDSsourceTotalPurch <- rawHDDSConsumedBad[c(2:13)] * rawHDDSsourceTotalPurch
rawHDDSsourceTotalPurch1 <- rawHDDSConsumedGood[c(2:13)] * rawHDDSsourceTotalPurch1
rawHDDSsourceTotalPurch <- rawHDDSsourceTotalPurch + rawHDDSsourceTotalPurch1
rawHDDSsourceTotalPurch$HDDS_Purch <- rowSums(rawHDDSsourceTotalPurch>0, na.rm=T)


#HDDS from Farm
rawHDDSsourceBadFarm <- rawHDDSsourceBad
rawHDDSsourceBadFarm[rawHDDSsourceBadFarm == 2] <- 0
rawHDDSsourceBadFarm[rawHDDSsourceBadFarm == 3 | rawHDDSsourceBadFarm == 1] <- 1
#rawHDDSsourceBadFarm[rawHDDSsourceBadFarm == 2 | rawHDDSsourceBadFarm == 3] <- 0

rawHDDSsourceBadFarm <- rawHDDSConsumedBad[c(2:13)] * rawHDDSsourceBadFarm
rawHDDSsourceBadFarm$HDDS_bad_farm <- rowSums(rawHDDSsourceBadFarm, na.rm=T)

rawHDDSsourceGoodFarm <- rawHDDSsourceGood
rawHDDSsourceGoodFarm[rawHDDSsourceGoodFarm == 2] <- 0
rawHDDSsourceGoodFarm[rawHDDSsourceGoodFarm == 3 | rawHDDSsourceGoodFarm == 1] <- 1
#rawHDDSsourceGoodFarm[rawHDDSsourceGoodFarm == 2 | rawHDDSsourceGoodFarm == 3] <- 0


rawHDDSsourceGoodFarm <- rawHDDSConsumedGood[c(2:13)] * rawHDDSsourceGoodFarm
rawHDDSsourceGoodFarm$HDDS_good_farm <- rowSums(rawHDDSsourceGoodFarm, na.rm=T)

rawHDDSsourceTotalFarm <- rawHDDSsourceBad
rawHDDSsourceTotalFarm1 <- rawHDDSsourceGood
rawHDDSsourceTotalFarm[rawHDDSsourceTotalFarm == 2] <- 0
#rawHDDSsourceTotalFarm[rawHDDSsourceTotalFarm == 1 | rawHDDSsourceTotalFarm == 3] <- 1
rawHDDSsourceTotalFarm[rawHDDSsourceTotalFarm == 3] <- 0
rawHDDSsourceTotalFarm1[rawHDDSsourceTotalFarm1 == 2] <- 0
#rawHDDSsourceTotalFarm1[rawHDDSsourceTotalFarm1 == 1 | rawHDDSsourceTotalFarm1 == 3] <- 1
rawHDDSsourceTotalFarm1[rawHDDSsourceTotalFarm1 == 3] <- 0
rawHDDSsourceTotalFarm <- rawHDDSConsumedBad[c(2:13)] * rawHDDSsourceTotalFarm
rawHDDSsourceTotalFarm1 <- rawHDDSConsumedGood[c(2:13)] * rawHDDSsourceTotalFarm1
rawHDDSsourceTotalFarm <- rawHDDSsourceTotalFarm + rawHDDSsourceTotalFarm1
rawHDDSsourceTotalFarm$HDDS_farm <- rowSums(rawHDDSsourceTotalFarm>0, na.rm=T)

datHDDS <- cbind(datHDDS, HDDS_farm = rawHDDSsourceTotalFarm$HDDS_farm, HDDS_purch = rawHDDSsourceTotalPurch$HDDS_Purch, HDDS_good_farm = rawHDDSsourceGoodFarm$HDDS_good_farm, HDDS_good_purch = rawHDDSsourceGoodPurch$HDDS_good_purch, HDDS_bad_farm = rawHDDSsourceBadFarm$HDDS_bad_farm, HDDS_bad_purch = rawHDDSsourceBadPurch$HDDS_bad_purch)
