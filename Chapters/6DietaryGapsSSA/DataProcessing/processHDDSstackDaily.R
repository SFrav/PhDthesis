library(dplyr)
library(tidyr)

##HDDS
datHDDS <- read.csv('DataParams/RHoMISmother_rawdata_Simon.csv', stringsAsFactors = F)
datHDDS$HouseholdID <- paste0(datHDDS$HouseholdID, datHDDS$country, datHDDS$sublocation)

datHDDS <- datHDDS[!(datHDDS$projectname %in% c("INDER", "CCAFS_IWMI", "SIIL")),] #Remove HHs with HDDS rather than MDD-W

#Select HDDS good columns and convert frequrency of occurance to binary consume or not
HDDSgood <- data.frame(lapply(select(datHDDS, ends_with("good_season")), function(x) {
  x <- gsub("monthly", 0, x)
  x <- gsub("weekly", 0, x)
  x <- gsub("daily", 1, x)
  x <- gsub("never", 0, x)
}))

HDDSgood <- data.frame(HouseholdID = datHDDS$HouseholdID, lapply(HDDSgood, function(x) {as.numeric(levels(x))[x]}))
HDDSgood$HDDSgood <- rowSums(HDDSgood[,2:11], na.rm=T)

colnames(HDDSgood) <- c("HouseholdID", "iHDDSgrRT_good", "iHDDSleg_good", "iHDDSnutS_good", "iHDDSvegL_good", "iHDDSvegA_good", "iHDDSvegO_good", "iHDDSfru_good", "iHDDSmeat_good", "iHDDSeggs_good", "iHDDSmilk_good", "HDDSgood")


HDDSbad <- data.frame(lapply(select(datHDDS, ends_with("bad_season")), function(x) {
  x <- gsub("monthly", 0, x)
  x <- gsub("weekly", 0, x)
  x <- gsub("daily", 1, x)
  x <- gsub("never", 0, x)
}))
HDDSbad <- data.frame(HouseholdID = datHDDS$HouseholdID, lapply(HDDSbad, function(x) {as.numeric(levels(x))[x]}))
HDDSbad$HDDSbad <- rowSums(HDDSbad[,2:11], na.rm=T)

colnames(HDDSbad) <- c("HouseholdID", "iHDDSgrRT_bad", "iHDDSleg_bad", "iHDDSnutS_bad", "iHDDSvegL_bad", "iHDDSvegA_bad", "iHDDSvegO_bad", "iHDDSfru_bad", "iHDDSmeat_bad", "iHDDSeggs_bad", "iHDDSmilk_bad", "HDDSbad")

##HDDS recent month - Households that do not have a 'lean season'
HDDSrecent <- select(datHDDS, ends_with("last_month"))
HDDSrecent <- data.frame(lapply(select(HDDSrecent, -contains("source")), function(x) {
  x <- gsub("monthly", 0, x)
  x <- gsub("weekly", 0, x)
  x <- gsub("daily", 1, x)
  x <- gsub("never", 0, x)
}))
HDDSrecent <- data.frame(HouseholdID = datHDDS$HouseholdID, lapply(HDDSrecent, function(x) {as.numeric(levels(x))[x]}))
HDDSrecent$HDDSrecent <- rowSums(HDDSrecent[,2:11], na.rm=T)



HDDSgood[HDDSgood$HDDSgood == 0,] <- HDDSrecent[HDDSgood$HDDSgood == 0,]# dataframes have the same HH order so can match based on the replacement index
HDDSbad[HDDSbad$HDDSbad == 0,] <- HDDSrecent[HDDSbad$HDDSbad == 0,]

HDDSstackDaily <- left_join(datHDDS, HDDSgood)
HDDSstackDaily <- left_join(HDDSstackDaily, HDDSbad)

HDDSstackDaily$GrainsRootsTubers_source_good <- ifelse(is.na(HDDSstackDaily$GrainsRootsTubers_source_good), HDDSstackDaily$GrainsRootsTubers_source_last_month, HDDSstackDaily$GrainsRootsTubers_source_good)
HDDSstackDaily$VitA_Veg_Fruit_source_good <- ifelse(is.na(HDDSstackDaily$VitA_Veg_Fruit_source_good), HDDSstackDaily$VitA_Veg_Fruit_source_last_month, HDDSstackDaily$VitA_Veg_Fruit_source_good)
HDDSstackDaily$Veg_Leafy_source_good <- ifelse(is.na(HDDSstackDaily$Veg_Leafy_source_good), HDDSstackDaily$Veg_Leafy_source_last_month, HDDSstackDaily$Veg_Leafy_source_good)
HDDSstackDaily$Vegetables_source_good <- ifelse(is.na(HDDSstackDaily$Vegetables_source_good), HDDSstackDaily$Vegetables_source_last_month, HDDSstackDaily$Vegetables_source_good)
HDDSstackDaily$Nuts_Seeds_source_good <- ifelse(is.na(HDDSstackDaily$Nuts_Seeds_source_good), HDDSstackDaily$Nuts_Seeds_source_last_month, HDDSstackDaily$Nuts_Seeds_source_good)
HDDSstackDaily$Fruits_source_good <- ifelse(is.na(HDDSstackDaily$Fruits_source_good), HDDSstackDaily$Fruits_source_last_month, HDDSstackDaily$Fruits_source_good)
HDDSstackDaily$Legumes_source_good <- ifelse(is.na(HDDSstackDaily$Legumes_source_good), HDDSstackDaily$Legumes_source_last_month, HDDSstackDaily$Legumes_source_good)
HDDSstackDaily$Milk_Dairy_source_good <- ifelse(is.na(HDDSstackDaily$Milk_Dairy_source_good), HDDSstackDaily$Milk_Dairy_source_last_month, HDDSstackDaily$Milk_Dairy_source_good)
HDDSstackDaily$Meat_source_good <- ifelse(is.na(HDDSstackDaily$Meat_source_good), HDDSstackDaily$Meat_source_last_month, HDDSstackDaily$Meat_source_good)
HDDSstackDaily$Eggs_source_good <- ifelse(is.na(HDDSstackDaily$Eggs_source_good), HDDSstackDaily$Eggs_source_last_month, HDDSstackDaily$Eggs_source_good)

HDDSstackDaily$iHDDSsourcegrRT_good <- paste(HDDSstackDaily$iHDDSgrRT_good, HDDSstackDaily$GrainsRootsTubers_source_good, sep = "@")
HDDSstackDaily$iHDDSsourcevegA_good <- paste(HDDSstackDaily$iHDDSvegA_good, HDDSstackDaily$VitA_Veg_Fruit_source_good, sep = "@")
HDDSstackDaily$iHDDSsourcevegL_good <- paste(HDDSstackDaily$iHDDSvegL_good, HDDSstackDaily$Veg_Leafy_source_good, sep = "@")
HDDSstackDaily$iHDDSsourcevegO_good <- paste(HDDSstackDaily$iHDDSvegO_good, HDDSstackDaily$Vegetables_source_good, sep = "@")
HDDSstackDaily$iHDDSsourcenutS_good <- paste(HDDSstackDaily$iHDDSnutS_good, HDDSstackDaily$Nuts_Seeds_source_good, sep = "@")
HDDSstackDaily$iHDDSsourcefru_good <- paste(HDDSstackDaily$iHDDSfru_good, HDDSstackDaily$Fruits_source_good, sep = "@")
HDDSstackDaily$iHDDSsourceleg_good <- paste(HDDSstackDaily$iHDDSleg_good, HDDSstackDaily$Legumes_source_good, sep = "@")
HDDSstackDaily$iHDDSsourcemilk_good <- paste(HDDSstackDaily$iHDDSmilk_good, HDDSstackDaily$Milk_Dairy_source_good, sep = "@")
HDDSstackDaily$iHDDSsourcemeat_good <- paste(HDDSstackDaily$iHDDSmeat_good, HDDSstackDaily$Meat_source_good, sep = "@")
HDDSstackDaily$iHDDSsourceeggs_good <- paste(HDDSstackDaily$iHDDSeggs_good, HDDSstackDaily$Eggs_source_good, sep = "@")

HDDSstackDaily$GrainsRootsTubers_source_bad <- ifelse(is.na(HDDSstackDaily$GrainsRootsTubers_source_bad), HDDSstackDaily$GrainsRootsTubers_source_last_month, HDDSstackDaily$GrainsRootsTubers_source_bad)
HDDSstackDaily$VitA_Veg_Fruit_source_bad <- ifelse(is.na(HDDSstackDaily$VitA_Veg_Fruit_source_bad), HDDSstackDaily$VitA_Veg_Fruit_source_last_month, HDDSstackDaily$VitA_Veg_Fruit_source_bad)
HDDSstackDaily$Veg_Leafy_source_bad <- ifelse(is.na(HDDSstackDaily$Veg_Leafy_source_bad), HDDSstackDaily$Veg_Leafy_source_last_month, HDDSstackDaily$Veg_Leafy_source_bad)
HDDSstackDaily$Vegetables_source_bad <- ifelse(is.na(HDDSstackDaily$Vegetables_source_bad), HDDSstackDaily$Vegetables_source_last_month, HDDSstackDaily$Vegetables_source_bad)
HDDSstackDaily$Nuts_Seeds_source_bad <- ifelse(is.na(HDDSstackDaily$Nuts_Seeds_source_bad), HDDSstackDaily$Nuts_Seeds_source_last_month, HDDSstackDaily$Nuts_Seeds_source_bad)
HDDSstackDaily$Fruits_source_bad <- ifelse(is.na(HDDSstackDaily$Fruits_source_bad), HDDSstackDaily$Fruits_source_last_month, HDDSstackDaily$Fruits_source_bad)
HDDSstackDaily$Legumes_source_bad <- ifelse(is.na(HDDSstackDaily$Legumes_source_bad), HDDSstackDaily$Legumes_source_last_month, HDDSstackDaily$Legumes_source_bad)
HDDSstackDaily$Milk_Dairy_source_bad <- ifelse(is.na(HDDSstackDaily$Milk_Dairy_source_bad), HDDSstackDaily$Milk_Dairy_source_last_month, HDDSstackDaily$Milk_Dairy_source_bad)
HDDSstackDaily$Meat_source_bad <- ifelse(is.na(HDDSstackDaily$Meat_source_bad), HDDSstackDaily$Meat_source_last_month, HDDSstackDaily$Meat_source_bad)
HDDSstackDaily$Eggs_source_bad <- ifelse(is.na(HDDSstackDaily$Eggs_source_bad), HDDSstackDaily$Eggs_source_last_month, HDDSstackDaily$Eggs_source_bad)


HDDSstackDaily$iHDDSsourcegrRT_bad <- paste(HDDSstackDaily$iHDDSgrRT_bad, HDDSstackDaily$GrainsRootsTubers_source_bad, sep = "@")
HDDSstackDaily$iHDDSsourcevegA_bad <- paste(HDDSstackDaily$iHDDSvegA_bad, HDDSstackDaily$VitA_Veg_Fruit_source_bad, sep = "@")
HDDSstackDaily$iHDDSsourcevegL_bad <- paste(HDDSstackDaily$iHDDSvegL_bad, HDDSstackDaily$Veg_Leafy_source_bad, sep = "@")
HDDSstackDaily$iHDDSsourcevegO_bad <- paste(HDDSstackDaily$iHDDSvegO_bad, HDDSstackDaily$Vegetables_source_bad, sep = "@")
HDDSstackDaily$iHDDSsourcenutS_bad <- paste(HDDSstackDaily$iHDDSnutS_bad, HDDSstackDaily$Nuts_Seeds_source_bad, sep = "@")
HDDSstackDaily$iHDDSsourcefru_bad <- paste(HDDSstackDaily$iHDDSfru_bad, HDDSstackDaily$Fruits_source_bad, sep = "@")
HDDSstackDaily$iHDDSsourceleg_bad <- paste(HDDSstackDaily$iHDDSleg_bad, HDDSstackDaily$Legumes_source_bad, sep = "@")
HDDSstackDaily$iHDDSsourcemilk_bad <- paste(HDDSstackDaily$iHDDSmilk_bad, HDDSstackDaily$Milk_Dairy_source_bad, sep = "@")
HDDSstackDaily$iHDDSsourcemeat_bad <- paste(HDDSstackDaily$iHDDSmeat_bad, HDDSstackDaily$Meat_source_bad, sep = "@")
HDDSstackDaily$iHDDSsourceeggs_bad <- paste(HDDSstackDaily$iHDDSeggs_bad, HDDSstackDaily$Eggs_source_bad, sep = "@")

HDDSstackDaily <- select(HDDSstackDaily, contains("iHDDSsource"), HouseholdID, country, region, HDDSgood, HDDSbad)
HDDSstackDaily_long <- gather(HDDSstackDaily, key, val, contains("iHDDS"))
HDDSstackDaily_long <- separate(HDDSstackDaily_long, key, c("Category", "Season"), sep = "_")
HDDSstackDaily_long <- separate(HDDSstackDaily_long, val, c("val", "Source"), sep = "@")
HDDSstackDaily_long$Category <- gsub("iHDDSsource", "", HDDSstackDaily_long$Category)
HDDSstackDaily_long$Source <- gsub("both", "on-farm bought", HDDSstackDaily_long$Source)
HDDSstackDaily_long$Source <- gsub("onfarm", "on-farm", HDDSstackDaily_long$Source)
HDDSstackDaily_long$Source <- gsub("off-farm", "bought", HDDSstackDaily_long$Source)
HDDSstackDaily_long$onFarm <- grepl("on-farm", HDDSstackDaily_long$Source)
HDDSstackDaily_long$bought <- grepl("bought", HDDSstackDaily_long$Source)
HDDSstackDaily_long$Source <- gsub("gift_exchange", "free", HDDSstackDaily_long$Source)
#!Treat gathered and exchanged the same as 'free'
HDDSstackDaily_long$Source <- gsub("gathered", "free", HDDSstackDaily_long$Source)
HDDSstackDaily_long$free <- grepl("free", HDDSstackDaily_long$Source)

#@Commented out because HDDSonFarmCrops/Lvst added
# summaryHDDSstackDaily <- group_by(HDDSstackDaily_long, HouseholdID, Season)
# summaryHDDSstackDaily <- summaryHDDSstackDaily[summaryHDDSstackDaily$val == 1,]
# summaryHDDSstackDaily <- summarise(summaryHDDSstackDaily, HDDSonFarm =sum(onFarm, na.rm=T), HDDSpurchased = sum(bought, na.rm=T))
# summaryHDDSstackDaily <- unite(summaryHDDSstackDaily, "onFarm_purchased", c("HDDSonFarm", "HDDSpurchased"))
# summaryHDDSstackDaily <- spread(summaryHDDSstackDaily, Season, onFarm_purchased)
# summaryHDDSstackDaily <- separate(summaryHDDSstackDaily, bad, c("HDDSonFarmBad", "HDDSpurchasedBad"))
# summaryHDDSstackDaily <- separate(summaryHDDSstackDaily, good, c("HDDSonFarmGood", "HDDSpurchasedGood"))

summaryHDDSstackDaily <- group_by(HDDSstackDaily_long, HouseholdID, Season)
summaryHDDSstackDaily <- summaryHDDSstackDaily[summaryHDDSstackDaily$val == 1,]
summaryHDDSstackDaily <- summarise(summaryHDDSstackDaily, HDDSonFarm =sum(onFarm, na.rm=T), HDDSpurchased = sum(bought, na.rm=T), HDDSonFarmCrops = sum(onFarm[Category %in% c("fru", "grRT", "nutS", "vegA", "vegL", "vegO", "leg")], na.rm=T), HDDSonFarmLvst = sum(onFarm[Category %in% c("eggs", "meat", "milk")], na.rm=T))
summaryHDDSstackDaily <- unite(summaryHDDSstackDaily, "onFarm_purchased", c("HDDSonFarm", "HDDSpurchased", "HDDSonFarmCrops", "HDDSonFarmLvst"))
summaryHDDSstackDaily <- spread(summaryHDDSstackDaily, Season, onFarm_purchased)
summaryHDDSstackDaily <- separate(summaryHDDSstackDaily, bad, c("HDDSonFarmBad", "HDDSpurchasedBad", "HDDSonFarmCropsBadDaily", "HDDSonFarmLvstBadDaily"))
summaryHDDSstackDaily <- separate(summaryHDDSstackDaily, good, c("HDDSonFarmGood", "HDDSpurchasedGood", "HDDSonFarmCropsGoodDaily", "HDDSonFarmLvstGoodDaily"))


HDDSstackDaily_long <- left_join(HDDSstackDaily_long, select(summaryHDDSstackDaily, HouseholdID, HDDSonFarmGood, HDDSonFarmBad, HDDSpurchasedGood, HDDSpurchasedBad, HDDSonFarmCropsBadDaily, HDDSonFarmLvstBadDaily, HDDSonFarmCropsGoodDaily, HDDSonFarmLvstGoodDaily))



