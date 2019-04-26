##HDDS
datHDDS <- read.csv('DataParams/RHoMISmother_rawdata_Simon.csv', stringsAsFactors = F)
datHDDS$HouseholdID <- paste0(datHDDS$HouseholdID, datHDDS$country, datHDDS$sublocation) #Not really necessary anymore

#Select HDDS good columns and convert frequrency of occurance to binary consume or not
HDDSgood <- data.frame(lapply(select(datHDDS, ends_with("good_season")), function(x) {
  x <- gsub("monthly", 0, x)
  x <- gsub("weekly", 1, x)
  x <- gsub("daily", 1, x)
  x <- gsub("never", 0, x)
}))

HDDSgood <- data.frame(HouseholdID = datHDDS$HouseholdID, lapply(HDDSgood, function(x) {as.numeric(levels(x))[x]}))
HDDSgood$HDDSgood <- rowSums(HDDSgood[,2:11], na.rm=T)

colnames(HDDSgood) <- c("HouseholdID", "iHDDSgrRT_good", "iHDDSleg_good", "iHDDSnutS_good", "iHDDSvegL_good", "iHDDSvegA_good", "iHDDSvegO_good", "iHDDSfru_good", "iHDDSmeat_good", "iHDDSeggs_good", "iHDDSmilk_good", "HDDSgood")


HDDSbad <- data.frame(lapply(select(datHDDS, ends_with("bad_season")), function(x) {
  x <- gsub("monthly", 0, x)
  x <- gsub("weekly", 1, x)
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
  x <- gsub("weekly", 1, x)
  x <- gsub("daily", 1, x)
  x <- gsub("never", 0, x)
}))
HDDSrecent <- data.frame(HouseholdID = datHDDS$HouseholdID, lapply(HDDSrecent, function(x) {as.numeric(levels(x))[x]}))
HDDSrecent$HDDSrecent <- rowSums(HDDSrecent[,2:11], na.rm=T)



HDDSgood[HDDSgood$HDDSgood == 0,] <- HDDSrecent[HDDSgood$HDDSgood == 0,]# dataframes have the same HH order so can match based on the replacement index
HDDSbad[HDDSbad$HDDSbad == 0,] <- HDDSrecent[HDDSbad$HDDSbad == 0,]
