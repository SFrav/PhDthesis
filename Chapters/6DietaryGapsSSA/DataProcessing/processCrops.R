#
#               ,,
#    ,,       ,\\//,          Prepare crop yield and utilisation variables
#  ,\\//,    ,\\\///,   ,,    Tested R versions: 3.4.1
# ,\\\///,   \\\\//// ,\\//,  Authors: Simon Fraval
# \\\\////    \\\/// ,\\\///, Contact: simon.fraval@outlook.com
#  \\\///     ###### \\\\//// 
#  ######    ////\\\\ \\\///
#  ////\\\\  /////\\\\\######
# /////\\\\\//////\\\\////\\\\
#//////\\\\\\/,///\\\/////\\\\\
#//////_,_\\\\(_)    //////\\\\\\,
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 
#ASCII credit: 'Pumpkins in the cornfield', anonymous, 1997
###########################################################

library(dplyr)
library(tidyr)
library(reshape)


##Crop wrangling
datCropLong <- read.csv('DataParams/RHoMISmother_rawdata_Simon.csv', stringsAsFactors = F)
datCropLong$HouseholdID <- paste0(datCropLong$HouseholdID, datCropLong$country, datCropLong$sublocation)
datCropLong <- datCropLong[!(datCropLong$projectname %in% c("INDER", "CCAFS_IWMI", "SIIL")),] #Remove HHs with HDDS rather than MDD-W
yieldUnits <- read.csv('DataParams/CropUnitsAll.csv') #Crop yield unit conversion CSV
yieldUnits$crop_name <- trimws(yieldUnits$crop_name) #Remove duplicate matches with witespace at the end. 
yieldUnits <- yieldUnits[!duplicated(yieldUnits),]

cropIncomeUnits <- read.csv('DataParams/CropIncomeUnits.csv')

cropNutritionLkp <- read.csv('DataParams/CropNamesNutri.csv') #A lookup table to match crop names to food item names in nutrition database
cropNutritionLkp$crop_name <- trimws(cropNutritionLkp$crop_name) #Remove duplicate matches with witespace at the end. 
cropNutritionLkp <- cropNutritionLkp[!duplicated(cropNutritionLkp),]

#Revise land cultivated data
datCropLong$landcultivated[is.na(datCropLong$landcultivated)] <- 0
datCropLong$landcultivated <- ifelse(datCropLong$unitland %in% c("acre", "acres"), datCropLong$landcultivated * 0.404686,
                             ifelse(datCropLong$unitland %in% c("0.25_ha", "timad", "other"), datCropLong$landcultivated * 0.25,
                                    ifelse(datCropLong$unitland %in% c("are_25x25m", "carre_25x25m"), datCropLong$landcultivated * 0.0025,
                                           ifelse(datCropLong$unitland == "igito_60x60m", datCropLong$landcultivated * 0.006,
                                                  ifelse(datCropLong$unitland == "mrapa", datCropLong$landcultivated * 9.999993e-05,
                                                         ifelse(datCropLong$unitland %in% c("kipanda", "fields", "lima"), datCropLong$landcultivated * 0.1012,
                                                                ifelse(datCropLong$unitland %in% c("ha", "hectare", "kg_maize_seed_80_equal_one_ha"), datCropLong$landcultivated, datCropLong$landcultivated)))))))

#Convert data to long format for ease of manipulation. Include country, land cultivated and the names of 'other' crops
datCropLong <- select(datCropLong, HouseholdID, crop_name_1:crop_residue_use_8, country, landcultivated, crops_other1, crops_other2, crops_other3)
colnames(datCropLong)[2:145] <- gsub('(.*)_+', '\\1.', names(datCropLong)[2:145]) # replace the last underscore with a period for conversion to long format

datCropLong <- reshape(data.frame(datCropLong), direction="long", varying=names(datCropLong)[2:145], 
                       idvar=c("HouseholdID"), timevar="loop", sep = ".")
datCropLong <- datCropLong[!is.na(datCropLong$crop_name),]

#Replace 'other' with crop name
datCropLong$crop_name <- ifelse(datCropLong$crop_name == "other1", datCropLong$crops_other1, 
                                ifelse(datCropLong$crop_name == "other2", datCropLong$crops_other2, 
                                       ifelse(datCropLong$crop_name == "other3", datCropLong$crops_other3, datCropLong$crop_name)))

datCropLong$crop_name <- trimws(datCropLong$crop_name)

#Join the yield units DF to the long crop DF
yieldUnits$crop_yield_units_other <- tolower(yieldUnits$crop_yield_units_other)
datCropLong$crop_yield_units_other <- tolower(datCropLong$crop_yield_units_other)

datCropLong <- left_join(datCropLong, select(yieldUnits, -Notes))

#!There are 9 obs (8 in Kenya, 1 in DRC) with no yield units
datCropLong$crop_yield_units[is.na(datCropLong$crop_yield_units) & datCropLong$crop_yield > 0] <- "kg"
datCropLong$crop_yield[datCropLong$crop_yield < 0] <- 0

#garden, pique (area), !franc congolais

#!Other units are all multiples of kg values in 
datCropLong$crop_yieldKG <- as.numeric(datCropLong$crop_yield) * datCropLong$convKG

#take area and season harvests as total yields
#datCropLong$crop_yieldKG <- ifelse(is.na(datCropLong$crop_yieldKG) & datCropLong$crop_yield_units_other %in% c("garden", "partie", "partie ", "partie  (10 m ã- 10 m)", "partie (10 ã-10 m)", "partie (10 m ã- 20 m)", "partie (10 m x 10 m)",
#                                                                                                              "partie (10x15m)", "piquet", "piquet ", "piquet (10m ã-10m)", "piquet (surface de 10 m x 10m)",
#                                                                                                               "piquet(10m x 10m)", "poquet"), as.numeric(datCropLong$crop_yield), datCropLong$crop_yieldKG)

#Some households have no data on the proportion consumed or sold, but do list single uses.
datCropLong$crop_consumed_prop <- ifelse(is.na(datCropLong$crop_consumed_prop) & is.na(datCropLong$crop_sold_prop) & datCropLong$crop_use %in% c("eat", "use"), "all", datCropLong$crop_consumed_prop)
datCropLong$crop_consumed_prop <- ifelse(is.na(datCropLong$crop_consumed_prop) & is.na(datCropLong$crop_sold_prop) & datCropLong$crop_use == "use sell", "half", datCropLong$crop_consumed_prop)


#Convert crop consumption categories into proportions
datCropLong$cropFarmConsumedPerc <- ifelse(datCropLong$crop_consumed_prop == "all", 1,
                                           ifelse(datCropLong$crop_consumed_prop == "half", 0.5,
                                                  ifelse(datCropLong$crop_consumed_prop == "little", 0.05,
                                                         ifelse(datCropLong$crop_consumed_prop == "most", 0.7,
                                                                ifelse(datCropLong$crop_consumed_prop == "underhalf", 0.25,
                                                                       ifelse(datCropLong$crop_consumed_prop == "none", 0, NA))))))


#Approximate sold proportion
#Some households have no data on the proportion consumed or sold, but do list single uses.

datCropLong$crop_sold_prop <- ifelse(is.na(datCropLong$crop_sold_prop) & is.na(datCropLong$crop_sold_prop) & datCropLong$crop_use %in% c("sell"), "all", datCropLong$crop_sold_prop)
datCropLong$crop_sold_prop <- ifelse(is.na(datCropLong$crop_sold_prop) & is.na(datCropLong$crop_sold_prop) & datCropLong$crop_use == "use sell", "half", datCropLong$crop_sold_prop)

datCropLong$cropFarmSoldPerc <- ifelse(datCropLong$crop_sold_prop == "all", 1,
                                       ifelse(datCropLong$crop_sold_prop == "half", 0.5,
                                              ifelse(datCropLong$crop_sold_prop == "little", 0.05,
                                                     ifelse(datCropLong$crop_sold_prop == "most", 0.7,
                                                            ifelse(datCropLong$crop_sold_prop == "underhalf", 0.25,
                                                                   ifelse(datCropLong$crop_sold_prop == "none", 0, NA))))))

datCropLong$cropFarmLvstFeedPerc <- ifelse(datCropLong$crop_feed_lstk_prop == "all", 1,
                                           ifelse(datCropLong$crop_feed_lstk_prop == "half", 0.5,
                                                  ifelse(datCropLong$crop_feed_lstk_prop == "little", 0.05,
                                                         ifelse(datCropLong$crop_feed_lstk_prop == "most", 0.7,
                                                                ifelse(datCropLong$crop_feed_lstk_prop == "underhalf", 0.25,
                                                                       ifelse(datCropLong$crop_feed_lstk_prop == "none", 0, NA))))))

datCropLong$cropFarmConsumedLvstPerc <- rowSums(datCropLong[, c("cropFarmConsumedPerc", "cropFarmLvstFeedPerc")], na.rm=T)

#!The proportion sold is more likely accurate. Base consumed proportion on the recip of sold where possible
datCropLong$cropFarmSoldPerc <- ifelse(is.na(datCropLong$cropFarmSoldPerc) & (datCropLong$cropFarmLvstFeedPerc == 0 | is.na(datCropLong$cropFarmLvstFeedPerc)), 1 - datCropLong$cropFarmConsumedPerc, datCropLong$cropFarmSoldPerc)
datCropLong$cropFarmConsumedPerc <- ifelse(!is.na(datCropLong$cropFarmSoldPerc) & (datCropLong$cropFarmLvstFeedPerc == 0 | is.na(datCropLong$cropFarmLvstFeedPerc)), 1 - datCropLong$cropFarmSoldPerc, datCropLong$cropFarmConsumedPerc)


#Approximate consumed and sold KG
datCropLong$cropFarmConsumedKG <- datCropLong$crop_yieldKG * datCropLong$cropFarmConsumedPerc
datCropLong$cropFarmSoldKG <- datCropLong$crop_yieldKG * datCropLong$cropFarmSoldPerc


#Appoximate land area
datCropLong$crop_land_areaHa <- ifelse(datCropLong$crop_land_area == "all", 1 * datCropLong$landcultivated, 
                                       ifelse(datCropLong$crop_land_area == "most", 0.7 * datCropLong$landcultivated,
                                              ifelse(datCropLong$crop_land_area == "half", 0.5 * datCropLong$landcultivated,
                                                     ifelse(datCropLong$crop_land_area == "underhalf", 0.25 * datCropLong$landcultivated, 
                                                            ifelse(datCropLong$crop_land_area == "little", 0.05 * datCropLong$landcultivated,
                                                                   ifelse(datCropLong$crop_land_area == "none", 0,
                                                                          datCropLong$crop_land_area))))))

#! remove unknown value - 999
datCropLong$crop_land_areaHa <- as.numeric(gsub(999, 0 , datCropLong$crop_land_areaHa))
datCropLong$yield_ha <- datCropLong$crop_yieldKG / datCropLong$crop_land_areaHa

#Calculate income from crop 
datCropLong$crop_sold_income <- ifelse(datCropLong$crop_sold_income < 0, abs(datCropLong$crop_sold_income), datCropLong$crop_sold_income)
datCropLong$crop_sold_price_quantityunits[datCropLong$crop_sold_price_quantityunits %in% c("kg") & is.na(datCropLong$crop_sold_price_quantityunits) & datCropLong$crop_sold_income >0] <- "price_per_kg"
#@same for tons, sacks_50kg, sack_magunia & debe table(paste(datCropLong$crop_yield_units[!(datCropLong$crop_sold_price_quantityunits %in% c("price_per_kg", "total_income_per_year")) & datCropLong$crop_sold_income >0], datCropLong$crop_sold_price_quantityunits[!(datCropLong$crop_sold_price_quantityunits %in% c("price_per_kg", "total_income_per_year")) & datCropLong$crop_sold_income >0]))

#There are 3 types of crop income enumeration: total per year, value per unit and currency (in DRC where most is enumerated in USD but some enumerate CDF - convert to USD).
datCropLong <- left_join(datCropLong, select(cropIncomeUnits, - Notes), by = c("crop_name", "crop_sold_price_quantityunits", "crop_price_quantityunits_other"))

#First, calculate instances of total income and currency conversions
datCropLong$cropIncomeAnnual <- ifelse(datCropLong$crop_sold_price_quantityunits == "total_income_per_year", datCropLong$crop_sold_income, NA)
datCropLong$cropIncomeAnnual <- ifelse(is.na(datCropLong$cropIncomeAnnual) & datCropLong$crop_sold_price_quantityunits == "other" & datCropLong$crop_price_quantityunits_other == "franc congolais", datCropLong$crop_sold_income * datCropLong$conv_perkg, 
                                       ifelse(is.na(datCropLong$cropIncomeAnnual) & datCropLong$crop_sold_price_quantityunits == "other" & datCropLong$crop_price_quantityunits_other %in% c("k58000 for 5bags", "total income from the two bags", "they sold 10 bags for k118000", "season"), datCropLong$crop_sold_income, datCropLong$cropIncomeAnnual))
#Next, convert unit specific values
datCropLong$cropIncomeAnnual <- ifelse(is.na(datCropLong$cropIncomeAnnual), datCropLong$cropFarmSoldKG * (datCropLong$crop_sold_income / datCropLong$conv_perkg), datCropLong$cropIncomeAnnual)

#!remove all HHs with NA income values or infer from price and available info.


datCropLong$cropIncomeHa <- datCropLong$cropIncomeAnnual / datCropLong$crop_land_areaHa

datCropLong$female_controlRevenue <- ifelse(grepl(paste(c("\\bmale\\w*?\\b", "man"), collapse="|"), datCropLong$crop_who_control_revenue) == F  & !is.na(datCropLong$crop_who_control_revenue), datCropLong$cropIncomeAnnual, NA)
#datCropLong$female_controlConsumption <- ifelse(grepl(paste(c("\\bmale\\w*?\\b", "man"), collapse="|"), datCropLong$crop_consume_control) == F  & !is.na(datCropLong$crop_consume_control), datCropLong$cropKcal)

datCropLong <- left_join(datCropLong, cropNutritionLkp)

datCropLong$maizeHa <- ifelse(datCropLong$crop_name == "maize", datCropLong$crop_land_areaHa, NA)
datCropLong$cerealHa <- ifelse(datCropLong$farmClassCat == "Cereal.and.Cereal.products", datCropLong$crop_land_areaHa, NA)
datCropLong$rootTuberBananaHa <- ifelse(datCropLong$farmClassCat == "Roots..Tubers.and.Banana", datCropLong$crop_land_areaHa, NA)
datCropLong$hortHa <- ifelse(datCropLong$farmClassCat == "Horticulture", datCropLong$crop_land_areaHa, NA)
datCropLong$pulsesnsHa <- ifelse(datCropLong$farmClassCat == "Pulses..nuts.and.seeds", datCropLong$crop_land_areaHa, NA)
datCropLong$nutsSeedsHa <- ifelse(datCropLong$HDDScat == "nutS", datCropLong$crop_land_areaHa, NA)

datCropLong$price_kg_lc <- datCropLong$cropIncomeAnnual / datCropLong$crop_yieldKG
datCropLong$fruPrice_kg_lc <- ifelse(datCropLong$HDDScat == "fru", datCropLong$price_kg_lc, NA)
datCropLong$grRTPrice_kg_lc <- ifelse(datCropLong$HDDScat == "GrRT", datCropLong$price_kg_lc, NA)
datCropLong$legPrice_kg_lc <- ifelse(datCropLong$HDDScat == "leg", datCropLong$price_kg_lc, NA)
datCropLong$nutSPrice_kg_lc <- ifelse(datCropLong$HDDScat == "nutS", datCropLong$price_kg_lc, NA)
datCropLong$vegLeafyPrice_kg_lc <- ifelse(datCropLong$HDDScat == "vegLeafy", datCropLong$price_kg_lc, NA)
datCropLong$vegOtherPrice_kg_lc <- ifelse(datCropLong$HDDScat == "vegOther", datCropLong$price_kg_lc, NA)
datCropLong$vegVitAPrice_kg_lc <- ifelse(datCropLong$HDDScat == "vegVitA", datCropLong$price_kg_lc, NA)

datCropLong$cropHDDSgood <- ifelse(datCropLong$crop_harvest %in% c("normal_harvest", "good_harvest"), datCropLong$HDDScat, NA)
datCropLong$cropHDDSbad <- ifelse(datCropLong$crop_harvest == "bad_harvest", datCropLong$HDDScat, NA)

datCropLong$foodItem[datCropLong$foodItem ==""] <- NA
datCropLong$cropProd <- ifelse(datCropLong$crop_land_areaHa >0.005, datCropLong$foodItem, NA)

datCropLong <- group_by(datCropLong, HouseholdID)
summaryCrop <- summarise(datCropLong, cropProdDiv = sum(!is.na(unique(cropProd))), fruPrice_kg_lc = weighted.mean(fruPrice_kg_lc, cropFarmSoldKG, na.rm=T), grRTPrice_kg_lc = weighted.mean(grRTPrice_kg_lc, cropFarmSoldKG, na.rm=T), legPrice_kg_lc = weighted.mean(legPrice_kg_lc, na.rm=T), nutSPrice_kg_lc = weighted.mean(nutSPrice_kg_lc, cropFarmSoldKG, na.rm=T), vegLeafyPrice_kg_lc = weighted.mean(vegLeafyPrice_kg_lc, na.rm=T), vegOtherPrice_kg_lc = weighted.mean(vegOtherPrice_kg_lc, cropFarmSoldKG, na.rm=T), vegVitAPrice_kg_lc = weighted.mean(vegVitAPrice_kg_lc, cropFarmSoldKG, na.rm=T), pulsesnsHa = sum(pulsesnsHa, na.rm=T), nutsSeedsHa = sum(nutsSeedsHa, na.rm = T), cropProdHDDS = length(unique(HDDScat)), cropHDDSGood = length(unique(cropHDDSgood)), cropHDDSbad = length(unique(cropHDDSbad)), maizeHa = sum(maizeHa, na.rm = T), cerealHa = sum(cerealHa, na.rm=T), rootTuberBananaHa = sum(rootTuberBananaHa, na.rm=T), hortHa = sum(hortHa, na.rm=T), incomeCrops_lc = sum(cropIncomeAnnual, na.rm=T), cropIncomeHa_lc = mean(cropIncomeHa, na.rm=T), incomeCropFemaleControl_lc = sum(female_controlRevenue, na.rm=T))

##Average cassava yield in DRC - used to estimate area measurements
#mean(datCropLong$yield_ha[datCropLong$crop_name == "cassava" & datCropLong$country == "drc" & !is.infinite(datCropLong$yield_ha) & datCropLong$yield_ha < 10000], na.rm=T)

#datCropLong$wild <- ifelse(datCropLong$crop_yield_units_other %in% c("garden", "partie", "partie ", "partie  (10 m ã- 10 m)", "partie (10 ã-10 m)", "partie (10 m ã- 20 m)", "partie (10 m x 10 m)",
#                                                                                                               "partie (10x15m)", "piquet", "piquet ", "piquet (10m ã-10m)", "piquet (surface de 10 m x 10m)",
#                                                                                                               "piquet(10m x 10m)", "poquet"), "totally", "nope")


