#
#           _ /
#    /\----_- -     Perpare all main dataframes associated with micronutrient-SSA study
#   /         \\    Tested R versions: 3.4.1
#  /           \__  Authors: Simon Fraval
#  \___-_        /
#        \      /
#         |     |
#         |     //|
#         (    ||/
#          \__/
#
#ASCII art by John Savard

exportCSVs <- T #Change to T for the first time the scripts are updated
exclude12HDDS <- T #Remove households that used 12 diet diversity categories
weekly_dailyDD <- "daily" #Variable to test the sensitivity of results to diet diversity based on a minimum frequency of 'weekly' or 'daily' consumption

library(dplyr)
library(tidyr)
library(stringr)

library(stringi)


##Read in data
#dat <- read.csv('DataParams/RHoMISSimonFraval_rawdata_all.csv', stringsAsFactors = F)
dat <- read.csv('DataParams/RHoMISmother_rawdata_Simon.csv', stringsAsFactors = F)

#nutritionTable <- read.csv('DataParams/nutritionCropLivestock.csv', stringsAsFactors = F)
exchangeR_orig <- select(read.csv('DataParams/exchangeRates.csv'), country, Exchange.rate)
dat <- left_join(dat, exchangeR_orig)
dat$Exchange.rate[dat$country == "drc" & dat$projectname == "Forets"] <- 925.9 # Exchange rate for Forets project in DRC was enumerated in local currency. The project was run in December 2016. At the time the exchange rate was 0.00108 CDF/USD https://www.xe.com/currencycharts/?from=CDF&to=USD&view=5Y 


if(exclude12HDDS ==T) {
#dat <- dat[!(dat$region %in% c("tanga", "seno", "yatenga")),] 
dat <- dat[!(dat$projectname %in% c("INDER", "CCAFS_IWMI", "SIIL")),]
}

#Create unique ID based on existing HHID, country and sublocation. NB. some studies do not list sublocation - that's okay for this purpose
dat$HouseholdID <- paste0(dat$HouseholdID, dat$country, dat$sublocation) #Not really necessary anymore

dat$country <- gsub("burkina$", "burkina faso", dat$country)
dat$country <- gsub("eth$", "ethiopia", dat$country)
dat$country <- gsub("tnz", "tanzania", dat$country)
dat$country <- tolower(dat$country)

dat$cropResFed <- grepl(pattern = "feed", x = dat$crop_residue_use_1)
dat$cropResFed[is.na(dat$cropResFed)] <- FALSE
#table(is.na(dat$manure_compost_crops))
dat$livedstockIncome_percFarm

#Revise land cultivated data
#!There are some households that do not have land.
dat$landcultivated[is.na(dat$landcultivated)] <- 0
dat$landcultivated <- ifelse(dat$unitland %in% c("acre", "acres"), dat$landcultivated * 0.404686,
                             ifelse(dat$unitland %in% c("0.25_ha", "timad", "other"), dat$landcultivated * 0.25,
                                    ifelse(dat$unitland %in% c("are_25x25m", "carre_25x25m"), dat$landcultivated * 0.0025,
                                           ifelse(dat$unitland == "igito_60x60m", dat$landcultivated * 0.006,
                                                  ifelse(dat$unitland == "mrapa", dat$landcultivated * 9.999993e-05,
                                                         ifelse(dat$unitland %in% c("kipanda", "fields", "lima"), dat$landcultivated * 0.1012,
                                                                ifelse(dat$unitland %in% c("ha", "hectare", "kg_maize_seed_80_equal_one_ha"), dat$landcultivated, dat$landcultivated)))))))



##TLU
#!There are bee hives, fish and 'other_lstk' - not included in TLU. 7 HHs with fish. 
dat <- mutate(rowwise(dat), tlu = sum((livestock_heads_cattle * 1), (livestock_heads_goats * 0.2), (livestock_heads_sheep * 0.2), (livestock_heads_donkeys * 0.8), (livestock_heads_pigs * 0.3), (livestock_heads_chicken * 0.04), (livestock_heads_otherpoultry *0.04), (livestock_heads_rabbits * 0.05), na.rm=T))

##Adult eq
#Replace coded NAs with 0
dat$malesover50 <- ifelse(dat$malesover50 >600 | dat$malesover50 < 0, 0, dat$malesover50)
dat$children_under_4 <- ifelse(dat$children_under_4 >600 | dat$children_under_4 < 0, 0, dat$children_under_4)
dat$children_4to10 <- ifelse(dat$children_4to10 >600 | dat$children_4to10 < 0, 0, dat$children_4to10)
dat$males11to24 <- ifelse(dat$males11to24 >600 | dat$males11to24 < 0, 0, dat$males11to24)
dat$males25to50 <- ifelse(dat$males25to50 >600 | dat$males25to50 < 0, 0, dat$males25to50)
dat$females11to24 <- ifelse(dat$females11to24 >600 | dat$females11to24 < 0, 0, dat$females11to24)
dat$females25to50 <- ifelse(dat$females25to50 >600 | dat$females25to50 < 0, 0, dat$females25to50)
dat$femalesover50 <- ifelse(dat$femalesover50 >600 | dat$femalesover50 < 0, 0, dat$femalesover50)
#dat <- mutate(rowwise(dat), adultEq = sum((children_under_4 * 0.5), (children_4to10 * 0.5), (males11to24* 0.8), 
#                                          (males25to50*1), (malesover50*1), (females11to24 * 0.75), (females25to50 * 0.8), 
#                                          (femalesover50 *0.8), na.rm=T))

#Adult equivalents following Claro et al. 2010 http://www.scielo.br/scielo.php?script=sci_arttext&pid=S0102-311X2010001100020
DRVlookup <- read.csv('DataParams/nutrientIntakeRequirementsFAO.csv')
DRVlookup$adultEq <- DRVlookup$requirement / mean(DRVlookup$requirement[DRVlookup$class %in% c("females25to50", "males25to50") & DRVlookup$nutrient == "ENERGY_KC"])
adultEqlookup <- select(subset(DRVlookup, nutrient == "ENERGY_KC"), class, adultEq)

adultEqLong <- gather(select(dat, HouseholdID, children_under_4:femalesover50), class, val, - HouseholdID)
adultEqLong <- left_join(adultEqLong, adultEqlookup)
adultEqLong$adultEqInter <- adultEqLong$val * adultEqLong$adultEq
adultEqLong <- group_by(adultEqLong, HouseholdID)
datAdultEq <- summarise(adultEqLong, adultEq = sum(adultEqInter, na.rm=T)) 
dat <- left_join(dat, datAdultEq)

dat$children_under_4[is.na(dat$children_under_4)] <- 0
dat$children_4to10[is.na(dat$children_4to10)] <- 0
dat$children_U10 <- ifelse(dat$children_under_4 >0 | dat$children_4to10 >0, 1, 0)

dat$householdComposition <- ifelse(dat$children_U10 == 1 & dat$household_type %in% c("man_single", "woman_single", "single"), "Single with children", 
                                   ifelse(dat$children_U10 == 0 & dat$household_type %in% c("man_single", "woman_single", "single"), "Single no children",
                                          ifelse(dat$children_U10 == 1 & !(dat$household_type %in% c("man_single", "woman_single", "single")), "Couple with children",
                                                 ifelse(dat$children_U10 == 0 & !(dat$household_type %in% c("man_single", "woman_single", "single")), "Couple no children", NA))))

##PPI
dat$PPI_1 <- as.numeric(gsub('[a-zA-Z]', "", dat$PPI_1))
dat$PPI_2 <- as.numeric(gsub('[a-zA-Z]', "", dat$PPI_2))
dat$PPI_3 <- as.numeric(gsub('[a-zA-Z]', "", dat$PPI_3))
dat$PPI_4 <- as.numeric(gsub('[a-zA-Z]', "", dat$PPI_4))
dat$PPI_5 <- as.numeric(gsub('[a-zA-Z]', "", dat$PPI_5))
dat$PPI_6 <- as.numeric(gsub('[a-zA-Z]', "", dat$PPI_6))
dat$PPI_10 <- as.numeric(gsub('[a-zA-Z]', "", dat$PPI_10))
dat$PPIscore <- rowSums(dat[, c("PPI_1", "PPI_2", "PPI_3", "PPI_4", "PPI_5", "PPI_6", "PPI_7", "PPI_8", "PPI_9", "PPI_10")], na.rm=T)

##HFIAS
#select HFIAS columns and convert frequency of occurance to their score equivalent
HFIAS <- data.frame(lapply(select(dat, HFIAS_1:HFIAS_9), function(x) {
  x <- gsub("monthly", 1, x)
  x <- gsub("weekly", 2, x)
  x <- gsub("daily", 3, x)
  x <- gsub("never", 0, x)
}))
HFIAS[is.na(HFIAS)] <- 0
#Change all variables to numeric and append to HHID
HFIAS <- data.frame(HouseholdID = dat$HouseholdID, lapply(HFIAS, function(x) {as.numeric(levels(x))[x]}))
HFIAS$HFIAS <- rowSums(HFIAS[,2:10], na.rm=T) #Calculate HFIAS, then next categorise HFIAP
HFIAS$HFIAP <- ifelse(HFIAS$HFIAS_1 %in% c(0,1) & rowSums(HFIAS[,3:10], na.rm=T) ==0, "Food secure", 
                      ifelse((HFIAS$HFIAS_1 %in% c(2,3) | HFIAS$HFIAS_2 %in% c(1,2,3) | HFIAS$HFIAS_3 %in% c(1) | HFIAS$HFIAS_4 %in% c(1)) & rowSums(HFIAS[,6:10], na.rm=T) ==0, "Mildly food insecure", 
                             ifelse((HFIAS$HFIAS_3 %in% c(2,3) | HFIAS$HFIAS_4 %in% c(2,3) | HFIAS$HFIAS_5 %in% c(1,2) | HFIAS$HFIAS_6 %in% c(1,2)) & rowSums(HFIAS[,8:10], na.rm=T) ==0, "Moderately food insecure",
                                    ifelse(HFIAS$HFIAS_5 ==3 | HFIAS$HFIAS_6 ==3 | HFIAS$HFIAS_7 %in% c(1,2,3) | HFIAS$HFIAS_8 %in% c(1,2,3) | HFIAS$HFIAS_9 %in% c(1,2,3), "Severely food insecure", NA))))

dat <- left_join(dat, select(HFIAS, HouseholdID, HFIAS, HFIAP))

#Count number of food shortage months
dat$numFoodShortMonth <- str_count(dat$foodshortagetime_months_which, "\\S+")
dat$numFoodShortMonth[is.na(dat$numFoodShortMonth)] <- 0 # NA in this instance is 0 months


source('processHDDS.R')
dat <- left_join(dat, HDDSgood)
dat <- left_join(dat, HDDSbad)

dat$HDDSgood_crop <- rowSums(dat[, c("iHDDSgrRT_good", "iHDDSleg_good", "iHDDSnutS_good", "iHDDSvegL_good", "iHDDSvegA_good", "iHDDSvegO_good", "iHDDSfru_good")], na.rm=T)
dat$HDDSbad_crop <- rowSums(dat[, c("iHDDSgrRT_bad", "iHDDSleg_bad", "iHDDSnutS_bad", "iHDDSvegL_bad", "iHDDSvegA_bad", "iHDDSvegO_bad", "iHDDSfru_bad")], na.rm=T)

dat$HDDSgood_lvst <- rowSums(dat[, c("iHDDSmeat_good", "iHDDSmilk_good", "iHDDSeggs_good")], na.rm=T)
dat$HDDSbad_lvst <- rowSums(dat[, c("iHDDSmeat_bad", "iHDDSmilk_bad", "iHDDSeggs_bad")], na.rm=T)


source('processCrops.R')
if(exportCSVs == T) {
write.csv(datCropLong, 'Outputs/DFs/datCropLong.csv') 
}

dat <- left_join(dat, summaryCrop)

##Uncomment if crop price per food category is needed
# cropPrice <- gather(select(dat, HouseholdID, Exchange.rate, fruPrice_kg_lc:vegVitAPrice_kg_lc), var, price_lc, c(-HouseholdID, -Exchange.rate)) 
# cropPrice$price_USD <- cropPrice$price_lc / cropPrice$Exchange.rate
# cropPrice <- group_by(cropPrice, var)
# cropPrice <- summarise(subset(cropPrice, price_USD >0 & price_USD < 50), price = mean(price_USD, na.rm=T))

source('processLivestock.R')
dat <- left_join(dat, summaryLivestock)
if(exportCSVs == T) {
write.csv(datLivestockLong, 'Outputs/DFs/datLivestockLong.csv') 
}

dat$meatFarmConsumedBin <- as.factor(ifelse(dat$meatFarmConsumedKG >0, 1, 0))
dat$milkFarmConsumedBin <- as.factor(ifelse(dat$milkFarmConsumedKG >0, 1, 0))
dat$eggsFarmConsumedBin <- as.factor(ifelse(dat$eggsFarmConsumedNum >0, 1, 0))

dat$lvstProdHDDS <- rowSums(dat[, c("totalMeatKG", "totalEggsNum", "totalMilkL")] > 0, na.rm = T)
dat$prodHDDS <- rowSums(dat[, c("cropProdHDDS", "lvstProdHDDS")], na.rm=T)


source('processFarmBasedNutrition.R')
if(exportCSVs == T) {
write.csv(datProducedNutrition, 'Outputs/DFs/datProducedNutrition.csv')
}
rm(datCheeseButter_DRV_water)

source('processIncome.R') # works directly on dat, need to import this CSV first


source('processHDDSstack.R') #requires 'dat' with HDDS

dat <- left_join(dat, summaryHDDSstack)

dat$HDDSpurchasedGood[is.na(dat$HDDSpurchasedGood)] <- 0
dat$HDDSpurchasedBad[is.na(dat$HDDSpurchasedBad)] <- 0
dat$farmSourceDependentGood <- ifelse(dat$HDDSpurchasedGood ==0 & dat$HDDSpurchasedBad == 0, 1, 0) #Households that do not purchase food category weekly or daily are categorised as subsistence
dat$farmSourceDependentGood_SensitivityWeekly <- ifelse(dat$HDDSpurchasedGood <2 & dat$HDDSpurchasedBad <2, 1, 0) #Households that only purchase 1 food category weekly or daily for sensitivity


##
#!Make all Inf in DF NA
dat[mapply(is.infinite, dat)] <- NA

source('processHDDSstackDaily.R')
if(exportCSVs == T) {
  write.csv(HDDSstackDaily_long, 'Outputs/DFs/HDDSstackDaily_long.csv')
  write.csv(HDDSstackDaily, 'Outputs/DFs/HDDSstackdaily.csv')
}

#Add MDD daily (only sourced daily) to the main DF
dat <- left_join(dat, select(HDDSstackDaily, HouseholdID, HDDSgoodDaily = HDDSgood, HDDSbadDaily = HDDSbad))
dat <- left_join(dat, select(summaryHDDSstackDaily, HouseholdID, HDDSonFarmGoodDaily = HDDSonFarmGood, HDDSonFarmBadDaily = HDDSonFarmBad, HDDSpurchasedGoodDaily = HDDSpurchasedGood, HDDSpurchasedBadDaily = HDDSpurchasedBad, HDDSonFarmCropsBadDaily, HDDSonFarmLvstBadDaily, HDDSonFarmCropsGoodDaily, HDDSonFarmLvstGoodDaily))


source('processFarmType.R') # works directly on dat, need to import this CSV first

HDDSstack_long <- left_join(HDDSstack_long, select(dat, HouseholdID, farmType))
if(exportCSVs == T) {
  write.csv(HDDSstack_long, 'Outputs/DFs/HDDSstack_long.csv')
}

farmTypeSummary <- group_by(dat, farmType)
farmTypeSummary <- summarise(farmTypeSummary, n = length(farmType), nSubsist = sum(farmSourceDependentGood, na.rm=T))
farmTypeSummary$percSubsist <- farmTypeSummary$nSubsist / farmTypeSummary$n

datProdConsNutrition <- left_join(datProdConsNutrition, select(dat, HouseholdID, country, farmSourceDependentGood, adultEq, farmType, children_U10, householdComposition))

datProdConsNutrition$region <-  ifelse(datProdConsNutrition$country %in% c("burkina faso", "drc", "ghana", "mali"), "westAfrica", "eastAfrica")

if(exportCSVs == T) {
write.csv(datProdConsNutrition, 'Outputs/DFs/datProdConsNutrition.csv')
write.csv(farmTypeSummary, 'Outputs/DFs/farmTypeSummary.csv')
}


if(exportCSVs == T) {
  write.csv(datProdConsNutrition, 'Outputs/DFs/datProdConsNutrition.csv')
}


#@processHDDSstackDaily was here

if(exportCSVs == T) {
  write.csv(dat, 'Outputs/DFs/dat.csv')
}

source('processHDDSnutrition.R')
#datProdConsNutrition <- left_join(datProdConsNutrition, datProducedNutrition)
if(exportCSVs == T) {
  write.csv(allnutrientNplot, 'Outputs/DFs/allnutrientsNplot.csv')
  write.csv(datProdConsNutrition, 'Outputs/DFs/datProdConsNutrition.csv') #@This wverwrites now that DF has been modified
}

#Add total produced for market orientation calculation
datTotalProducedNutrition <- summarise(subset(datProducedNutritionWater, proportion == "total"), totalProducedValNutrient = sum(totalValNutrient, na.rm = T))
colnames(datTotalProducedNutrition)[2] <- "nutrient"
datProdConsNutrition <- left_join(datProdConsNutrition, datTotalProducedNutrition)
datProdConsNutrition$mktOri <- 1 - (datProdConsNutrition$totalConsumedValNutrient / datProdConsNutrition$totalProducedValNutrient) # Some miconutrients are consumed more than produced because of water intake
marketOri <- group_by(subset(datProdConsNutrition, nutrient %in% c("ENERGY_KC", "PROCNT")), HouseholdID, nutrient)
marketOri <- spread(select(marketOri, HouseholdID, nutrient, mktOri), nutrient, -HouseholdID)
colnames(marketOri)[2:3] <- c("marketOrientationKcal", "marketOrientationProtein")

dat <- left_join(dat, marketOri)

source('processAEZ.R')
if(exportCSVs == T) {
  write.csv(dat, 'Outputs/DFs/datSpatial.csv')
}

rm(carcassWeight, countryDF, cropNutritionLkp, datCropNutritionLong, datCropNutritionWowSoLong, datLvstNutrition, datLvstNutritionLong, datLvstNutritionWowSoLong, datOtherLvstNutrition, datOtherLvstNutritionWowSoLong, exchangeR_orig, HDDSbad, HDDSgood, HDDSrecent, HDDSstack, HFIAS, lvstNutritionLkp, nutritionTable, PPPcurrent, summaryCrop, summaryLivestock, summaryHDDSstack, yieldUnits, paramEggLayingDays, paramLactLength, paramPropGoodSeason, DRVlookup, nutrientDRVs)


##Remove households with potential errors
outlierObs <- unique(c(as.character(dat$HouseholdID[dat$landcultivated > 100]),
                       as.character(dat$HouseholdID[dat$tlu > 250]),
                       as.character(dat$HouseholdID[dat$adultEq > 30]),
                       as.character(dat$HouseholdID[dat$quality_reliability %in% c(1)]),
                       as.character(dat$HouseholdID[dat$HDDSgood == 0]),
                       as.character(dat$HouseholdID[dat$HDDSbad == 0]),
                       as.character(dat$HouseholdID[dat$adultEq == 0]),
                       as.character(dat$HouseholdID[dat$projectname == "TreeAID" & dat$country == "ghana"],
                       as.character(dat$HouseholdID[is.na(dat$farmType)]))))

dat <- dat[!(dat$HouseholdID %in% outlierObs),]
datProdConsNutrition <- datProdConsNutrition[!(as.character(datProdConsNutrition$HouseholdID) %in% outlierObs),]
#HDDSstack_long <- HDDSstack_long[!(HDDSstack_long$HouseholdID %in% outlierObs),]
#HDDSstackDaily_long <- HDDSstackDaily_long[!(HDDSstackDaily_long$HouseholdID %in% outlierObs),]
HDDSstackDaily_long <- HDDSstackDaily_long[(HDDSstackDaily_long$HouseholdID %in% dat$HouseholdID),]

detach("package:raster", unload = T)
