#
#     (\
#      \ \
#  __    \/ ___,.-------..__        __
# //\\ _,-'\\               `'--._ //\\
# \\ ;'      \\                   `: //
#  `(          \\                   )'
#    :.          \\,----,         ,;
#     `.`--.___   (    /  ___.--','
#       `.     ``-----'-''     ,'
#          -.               ,-
#       gpyy  `-._______.-'
#############################


#H3C    CH3    CH3     CH3
#   `.,'       |       |   CH2OH
#   ,'`. ,'\\,'\\,'\\,'\\,'
#  |   ||
#   `..' `.
#          CH3        mbfh
#
#Vitamin A - retinol
#
#                                         H3C
#H3C    CH3    CH3     CH3                   `.,'`,
#   `.,'       |       |                      ||   |
#   ,'`. ,'\\,'\\,'\\,'\\,'\\,'\\,'\\,'\\,'\\,'`.,'       mbfh
#  |   ||                       |       |      ,'`,
#   `..' `.                     CH3     CH3 H3C    CH3
#          CH3
#
#Pro Vitamin A - Beta-carotene

library(dplyr)
library(tidyr)
##Nutrition
datCropLong  <- read.csv('Outputs/DFs/datCropLong.csv', stringsAsFactors = F)
datLivestockLong <- read.csv('Outputs/DFs/datLivestockLong.csv', stringsAsFactors = F)
nutritionTable <- read.csv('DataParams/nutritionCropLivestock.csv', stringsAsFactors = F)
cropNutritionLkp <- read.csv('DataParams/CropNamesNutri.csv') #A lookup table to match crop names to food item names in nutrition database
refuseProportion <- read.csv('DataParams/refusePortion.csv') #@This CSV provides a lookup to USDAcompositionTable. Can use to assess sensitivity to database
datCheeseButter_DRV_water <- read.csv('DataParams/RHoMISmother_rawdata_Simon.csv', stringsAsFactors = F) #Full HH dataset mainly for the cheese and butter variables
datCheeseButter_DRV_water$HouseholdID <- paste0(datCheeseButter_DRV_water$HouseholdID, datCheeseButter_DRV_water$country, datCheeseButter_DRV_water$sublocation)
datCheeseButter_DRV_water <- datCheeseButter_DRV_water[!(datCheeseButter_DRV_water$projectname %in% c("INDER", "CCAFS_IWMI", "SIIL")),] #Remove HHs with HDDS rather than MDD-W
paramLactLength <- 270 #Based on Millogo, 2010 https://pub.epsilon.slu.se/2208/1/millogo_v_100107.pdf
paramPropGoodSeason <- 0.8 #Assuming feed availability is also limited in the 'bad season'



##Crop nutrition
datCropNutritionLong <- group_by(datCropLong, HouseholdID, crop_name)
datCropNutritionLong <- summarise(datCropNutritionLong, total = sum(crop_yieldKG, na.rm=T), consumed = sum(cropFarmConsumedKG, na.rm=T))
datCropNutritionLong <- gather(datCropNutritionLong, proportion, mass, -HouseholdID, -crop_name)
datCropNutritionLong <- left_join(datCropNutritionLong, cropNutritionLkp)
nutritionTable <- select(nutritionTable, -X)
datCropNutritionLong <- left_join(datCropNutritionLong, nutritionTable)
datCropNutritionLong <- left_join(datCropNutritionLong, select(refuseProportion, foodItem, USDAlookup, refusePercent)) #Can pull in USDA selenium values here. But need to do the same for livestock products

#Gather nutrients in to one column and remove superfluous columns
datCropNutritionWowSoLong <- gather(select(datCropNutritionLong, -code, -lookup, -farmClassCat, -Notes, -index, -Source, -Selenium, -USDAlookup), var, val, -HouseholdID, -crop_name, -foodItem, -category, -HDDScat, -proportion, -mass, -refusePercent)
datCropNutritionWowSoLong <- datCropNutritionWowSoLong[datCropNutritionWowSoLong$mass >0,]
colnames(datCropNutritionWowSoLong)[2] <- "product"
datCropNutritionWowSoLong$mass <- datCropNutritionWowSoLong$mass * ((100 - datCropNutritionWowSoLong$refusePercent) / 100)

datCropNutritionWowSoLong$totalValNutrient <- datCropNutritionWowSoLong$mass * (as.numeric(datCropNutritionWowSoLong$val)*10)


##Livestock nutrition
lvstNutritionLkp <- read.csv('DataParams/LivestockNamesNutri.csv')
datLvstNutritionLong <- group_by(datLivestockLong, HouseholdID, livestock_name)
datLvstNutritionLong <- summarise(datLvstNutritionLong, total_Liver = sum(liverSlaughterKG, na.rm=T), consumed_Liver = sum(liverFarmConsumedKG, na.rm=T), total_Meat = sum(meatSlaughterKG, na.rm=T), consumed_Meat = sum(meatFarmConsumedKG, na.rm=T), total_Milk = sum(totalMilkL, na.rm = T), total_Honey = sum(totalHoneyKG, na.rm=T), consumed_Milk = sum(milkFarmConsumedKG, na.rm=T), total_Eggs = sum(totalEggsNum, na.rm=T), consumed_Eggs = sum(eggsFarmConsumedNum, na.rm=T), consumed_Honey = sum(honeyFarmConsumedKG, na.rm=T))
datLvstNutritionLong <- gather(datLvstNutritionLong, var, mass, c("total_Milk", "total_Eggs", "total_Meat", "total_Liver", "total_Honey", "consumed_Milk", "consumed_Eggs", "consumed_Meat", "consumed_Liver", "consumed_Honey"))

datLvstNutritionLong <- separate(datLvstNutritionLong, var, c("proportion", "product"), sep = "_")
datLvstNutritionLong <- datLvstNutritionLong[datLvstNutritionLong$mass >0,]
#Assume each egg is 60g
datLvstNutritionLong$val <- ifelse(datLvstNutritionLong$product == "Eggs", datLvstNutritionLong$mass *0.06, datLvstNutritionLong$mass)


datLvstNutritionLong <- left_join(datLvstNutritionLong, lvstNutritionLkp)
datLvstNutritionLong <- left_join(datLvstNutritionLong, nutritionTable)

datLvstNutritionWowSoLong <- gather(select(datLvstNutritionLong, HouseholdID, product, proportion, mass, foodItem, category, HDDScat, ENERGY_KC:MN, -code, -livestock_name), var, val, -HouseholdID, -product, -proportion, -mass, -foodItem, -category, -HDDScat) #!Why does val become a character?
#Nutritional values are currently per 100g. Multiply by 10 and then convert mass to nutrients
datLvstNutritionWowSoLong$totalValNutrient <- datLvstNutritionWowSoLong$mass * (as.numeric(datLvstNutritionWowSoLong$val)*10)

#Add cheese, butter and crops first
datLvstNutritionWowSoLong <- group_by(datLvstNutritionWowSoLong, HouseholdID, var)
datLvstNutrition <- summarise(datLvstNutritionWowSoLong, val = sum(totalValNutrient, na.rm=T))
datLvstNutrition <- spread(datLvstNutrition, var, val)


##Adding nutrition information for cheese and butter - total and consumed
datCheeseButter_DRV_water$totalCheeseKG <- ifelse(datCheeseButter_DRV_water$cheese_units %in% c("kg", "litres"), datCheeseButter_DRV_water$cheese_amount, 
                            ifelse(datCheeseButter_DRV_water$cheese_units == "0.5_kg", datCheeseButter_DRV_water$cheese_amount * 0.5,
                                   ifelse(datCheeseButter_DRV_water$cheese_units == "cups_0.3_litre", datCheeseButter_DRV_water$cheese_amount * 0.3,
                                          ifelse(datCheeseButter_DRV_water$cheese_units == "0.25_kg", datCheeseButter_DRV_water$cheese_amount * 0.25, NA))))

#shekla = pot, wancha = cow horn cup
datCheeseButter_DRV_water$totalCheeseKG <- ifelse(is.na(datCheeseButter_DRV_water$totalCheeseKG) & datCheeseButter_DRV_water$cheese_amount_units_other %in% c("clay pot", "shekla", "shella", "skekla"), 3,
                            ifelse(is.na(datCheeseButter_DRV_water$totalCheeseKG) & datCheeseButter_DRV_water$cheese_amount_units_other == "tin", 2,
                                   ifelse(is.na(datCheeseButter_DRV_water$totalCheeseKG) & datCheeseButter_DRV_water$cheese_amount_units_other == "wancha", 0.3, datCheeseButter_DRV_water$totalCheeseKG)))

datCheeseButter_DRV_water$totalCheeseKG <- ifelse(datCheeseButter_DRV_water$cheese_time_units == "day", datCheeseButter_DRV_water$totalCheeseKG * paramLactLength,
                            ifelse(datCheeseButter_DRV_water$cheese_time_units == "week", datCheeseButter_DRV_water$totalCheeseKG * (paramLactLength / 7),
                                   ifelse(datCheeseButter_DRV_water$cheese_time_units == "month", datCheeseButter_DRV_water$totalCheeseKG * (paramLactLength / 30),
                                          datCheeseButter_DRV_water$totalCheeseKG)))

datCheeseButter_DRV_water$cheeseFarmConsumedKG <- ifelse(datCheeseButter_DRV_water$cheese_consumed_amount == "half", datCheeseButter_DRV_water$totalCheeseKG * 0.5,
                                   ifelse(datCheeseButter_DRV_water$cheese_consumed_amount == "most", datCheeseButter_DRV_water$totalCheeseKG * 0.7,
                                          ifelse(datCheeseButter_DRV_water$cheese_consumed_amount == "underhalf", 0.25 * datCheeseButter_DRV_water$totalCheeseKG,
                                                 ifelse(datCheeseButter_DRV_water$cheese_consumed_amount == "little", datCheeseButter_DRV_water$totalCheeseKG * 0.05, datCheeseButter_DRV_water$totalCheeseKG)))) 


datCheeseButter_DRV_water$totalButterKG <- ifelse(datCheeseButter_DRV_water$butter_units %in% c("kg", "litres"), datCheeseButter_DRV_water$butter_amount, 
                            ifelse(datCheeseButter_DRV_water$butter_units == "ladle", datCheeseButter_DRV_water$butter_amount * 0.1,
                                   ifelse(datCheeseButter_DRV_water$butter_units == "cups_0.3_litre", datCheeseButter_DRV_water$butter_amount * 0.3,
                                          ifelse(datCheeseButter_DRV_water$butter_units == "0.25_kg", datCheeseButter_DRV_water$butter_amount * 0.25, NA))))

#! find weights of kitsae, cheote and qitsiot
datCheeseButter_DRV_water$totalButterKG <- ifelse(is.na(datCheeseButter_DRV_water$totalButterKG) & datCheeseButter_DRV_water$butter_amount_units_other %in% c("kitsae", "kiseo", "kisae", "kitso", "kesto", "ketso"), datCheeseButter_DRV_water$butter_amount *1.5, 
                            ifelse(is.na(datCheeseButter_DRV_water$totalButterKG) & datCheeseButter_DRV_water$butter_amount_units_other %in% c("cheo", "cheote", "chiot", "choe"), datCheeseButter_DRV_water$butter_amount * 0.5,
                                   ifelse(is.na(datCheeseButter_DRV_water$totalButterKG) & datCheeseButter_DRV_water$butter_amount_units_other %in% c("qitsiot", "tsiot"), datCheeseButter_DRV_water$butter_amount * 0.7, NA)))

datCheeseButter_DRV_water$totalButterKG <- ifelse(datCheeseButter_DRV_water$butter_time_units == "day", datCheeseButter_DRV_water$totalButterKG * paramLactLength,
                            ifelse(datCheeseButter_DRV_water$butter_time_units == "week", datCheeseButter_DRV_water$totalButterKG * (paramLactLength / 7),
                                   ifelse(datCheeseButter_DRV_water$butter_time_units == "month", datCheeseButter_DRV_water$totalButterKG * (paramLactLength / 30),
                                          ifelse(datCheeseButter_DRV_water$butter_time_units == "year", datCheeseButter_DRV_water$totalButterKG, NA))))

datCheeseButter_DRV_water$totalButterKG <- ifelse(datCheeseButter_DRV_water$butter_time_units == "other" & datCheeseButter_DRV_water$butter_amount_time_units_other == "twice per month especially rainy season", paramLactLength / 15, datCheeseButter_DRV_water$totalButterKG)

datCheeseButter_DRV_water$butterFarmConsumedKG <- ifelse(datCheeseButter_DRV_water$butter_consumed_amount == "half", datCheeseButter_DRV_water$totalButterKG * 0.5,
                                   ifelse(datCheeseButter_DRV_water$butter_consumed_amount == "most", datCheeseButter_DRV_water$totalButterKG * 0.7,
                                          ifelse(datCheeseButter_DRV_water$butter_consumed_amount == "underhalf", 0.25 * datCheeseButter_DRV_water$totalButterKG,
                                                 ifelse(datCheeseButter_DRV_water$butter_consumed_amount == "little", datCheeseButter_DRV_water$totalButterKG * 0.05, datCheeseButter_DRV_water$totalButterKG)))) 

datOtherLvstNutrition <- gather(select(datCheeseButter_DRV_water, HouseholdID, total_Cheese = totalCheeseKG, consumed_Cheese = cheeseFarmConsumedKG, total_Butter = totalButterKG, consumed_Butter = butterFarmConsumedKG), var, mass, -HouseholdID)

datOtherLvstNutrition <- separate(datOtherLvstNutrition, var, c("proportion", "product"), sep = "_")
datOtherLvstNutrition <- datOtherLvstNutrition[datOtherLvstNutrition$mass >0,]

datOtherLvstNutrition <- left_join(datOtherLvstNutrition, lvstNutritionLkp)
datOtherLvstNutrition <- left_join(datOtherLvstNutrition, nutritionTable)

datOtherLvstNutritionWowSoLong <- gather(select(datOtherLvstNutrition, HouseholdID, product, proportion, mass, foodItem, category, HDDScat, ENERGY_KC:MN, -code), var, val, -HouseholdID, -product, -proportion, -mass, -foodItem, -category, -HDDScat)
#Nutritional values are currently per 100g. Multiply by 10 and then convert mass to nutrients
datOtherLvstNutritionWowSoLong$totalValNutrient <- datOtherLvstNutritionWowSoLong$mass * (as.numeric(datOtherLvstNutritionWowSoLong$val)*10)

#datProducedNutrition <- data.frame(rbind(datCropNutritionWowSoLong, datLvstNutritionWowSoLong))
#datProducedNutrition <- data.frame(rbind(datProducedNutrition, datOtherLvstNutritionWowSoLong))

datProducedNutrition <- bind_rows(datCropNutritionWowSoLong, datLvstNutritionWowSoLong, datOtherLvstNutritionWowSoLong)
datProducedNutrition$val <- as.numeric(datProducedNutrition$val)

##Calculate approximate daily requirement values for all nutrients
DRVlookup <- read.csv('DataParams/nutrientIntakeRequirementsFAO.csv')
#Sulphur containing AAs grouped - Methionine (essental) and Cystine (non-essential)
#Some aromatic AAs grouped - Phenylalanine (essential) and Tyrosine (non-essential)


nutrientDRVs <- gather(select(datCheeseButter_DRV_water, HouseholdID, children_under_4:femalesover50), class, val, - HouseholdID)
nutrientDRVs <- inner_join(nutrientDRVs, DRVlookup)
nutrientDRVs$totalRequired <- nutrientDRVs$val * nutrientDRVs$requirement * 365
nutrientDRVs <- group_by(nutrientDRVs, HouseholdID, nutrient)
nutrientDRVs <- summarise(nutrientDRVs, HHrequirement = sum(totalRequired, na.rm=T))

#Individuals will generally get calcium and magnisium from the water supply
waterParams <- read.csv('DataParams/nutritionWaterIntake.csv')
waterIntake <- gather(select(datCheeseButter_DRV_water, HouseholdID, children_under_4:femalesover50), class, val, - HouseholdID)
waterIntake <- left_join(waterIntake, waterParams)
waterIntake$CA <- waterIntake$val * waterIntake$volWater * waterIntake$CA * 365
waterIntake$MG <- waterIntake$val * waterIntake$volWater * waterIntake$MG *365
waterIntake <- group_by(waterIntake, HouseholdID)
waterIntakeLong <- summarise(waterIntake, CA = sum(CA, na.rm=T), MG = sum(MG, na.rm=T))
waterIntakeLong <- gather(waterIntakeLong, var, totalValNutrient, -HouseholdID)
waterIntakeLong$proportion <- "consumed"

datProducedNutritionWater <- bind_rows(datProducedNutrition, waterIntakeLong)
#@ To remove water intake, simply remove the word 'Water' from the following two lines. Not a big influence on the final results.
datProducedNutritionWater <- group_by(datProducedNutritionWater, HouseholdID, var)
datProdConsNutrition <- summarise(subset(datProducedNutritionWater, proportion == "consumed"), totalConsumedValNutrient = sum(totalValNutrient, na.rm = T))

datProdConsNutrition$totalConsumedValNutrient[is.na(datProdConsNutrition$totalConsumedValNutrient)] <- 0
datProdConsNutrition <- left_join(nutrientDRVs, datProdConsNutrition, by = c("HouseholdID", "nutrient" = "var"))

datProdConsNutrition <- datProdConsNutrition[datProdConsNutrition$HHrequirement > 0,]
datProdConsNutrition$totalConsumedValNutrient[is.na(datProdConsNutrition$totalConsumedValNutrient)] <- 0

datProdConsNutrition$nutritionSufficPerc <- (datProdConsNutrition$totalConsumedValNutrient / datProdConsNutrition$HHrequirement) * 100
datProdConsNutrition$nutritionSufficPerc[datProdConsNutrition$nutritionSufficPerc > 100] <- 100

#!Add total produced ... Add later in 'prepareIndicatorsMain because I'm not sure if adding here will disrupt other processing.
#datProducedNutrition <- group_by(datProducedNutrition, HouseholdID, var, HDDScat)
#datProducedNutrition <- summarise(subset(datProducedNutritionWater, proportion == "total"), totalProducedValNutrient = sum(totalValNutrient, na.rm = T))
#colnames(datProducedNutrition)[2] <- "nutrient"
#datProdConsNutrition <- left_join(datProdConsNutrition, datProducedNutrition)
#datProdConsNutrition$mktOri <- 1 - (datProdConsNutrition$totalConsumedValNutrient / datProdConsNutrition$totalProducedValNutrient)
#ggplot(subset(datProdConsNutrition, farmSourceDependentGood == 1 & nutrient %in% c("TRP", "THR", "ILE", "LEU", "LYS", "SAA", "AAA", "VAL", "HIS") & country %in% c("drc", "kenya", "tanzania", "tnz", "zambia")), aes(y = HHrequirement - totalConsumedValNutrient, nutrient)) + geom_boxplot() + coord_cartesian(ylim = c(-5000000, 5000000)) + facet_wrap(~country)
#table(datProdConsNutrition$nutrient, datProdConsNutrition$HHrequirement > datProdConsNutrition$totalConsumedValNutrient)



#! Not using because the data will be better served by focusing on individual micronutrients. Also questions about energy sufficiency and the implied relationships in the equation
if(F) {
##Calculate Micronutrient Density Index

#!Sticking to beal et al. for now. Can expand to more - eg. K)
micronutrientMDI <- c("CA", "CU", "FE", "ZN", "MG", "P", "VITA", "VITC", "VIT.B6", "VIT.B12", "FOL", "NIA", "RIBF", "THIA")
#Add energy requirement and energy consumed as part of the MDI calculation
datProdConsNutriMDI <- left_join(subset(datProdConsNutrition, nutrient %in% micronutrientMDI), select(subset(datProdConsNutrition, nutrient == "ENERGY_KC"), HouseholdID, EER = HHrequirement, E = totalConsumedValNutrient))  

# Calculate MDI #!Insert LaTeX formula here.
n <- length(unique(datProdConsNutriMDI$nutrient))
#datProdConsNutriMDI$MDIi <- (datProdConsNutriMDI$totalConsumedValNutrient / datProdConsNutriMDI$HHrequirement) / (datProdConsNutriMDI$E/datProdConsNutriMDI$EER)
datProdConsNutriMDI$MDIi <- (datProdConsNutriMDI$totalConsumedValNutrient / datProdConsNutriMDI$E) / (datProdConsNutriMDI$HHrequirement/datProdConsNutriMDI$EER)
datProdConsNutriMDI$MDIi <- ifelse(datProdConsNutriMDI$MDIi >1, 1, datProdConsNutriMDI$MDIi)
datProdConsNutriMDI$MDIi[is.na(datProdConsNutriMDI$MDIi)] <- 0
datProdConsNutriMDI_HH <- group_by(datProdConsNutriMDI, HouseholdID)
datProdConsNutriMDI_HH <- summarise(datProdConsNutriMDI_HH, MDI = sum(MDIi, na.rm=T))
datProdConsNutriMDI_HH$MDI <- (1/n) * datProdConsNutriMDI_HH$MDI

# M <- seq(0.1, 0.5,by = 0.05 )
# E <- seq(1000, 2450, length.out = 9)
# RDA <- 0.5
# EER <- 2498
# 
# i <- (M/E)/(RDA/EER)
# j <- (M/RDA)/(E/EER)
}
