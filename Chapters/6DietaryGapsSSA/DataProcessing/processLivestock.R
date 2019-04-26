#             / /
#          (\/_//`)
#           /   '/
#          0  0   \
#         /        \
#        /    __/   \
#       /,  _/ \     \_
#       `-./ )  |     ~^~^~^~^~^~^~^~\~.
#           (   /                     \_}
#              |               /      |
#              ;     |         \      /
#               \/ ,/           \    |
#               / /~~|~|~~~~~~|~|\   |
#              / /   | |      | | `\ \
#             / /    | |      | |   \ \
#            / (     | |      | |    \ \
#           /,_)    /__)     /__)   /,_/ 
#     '''''"""""'''""""""'''""""""''"""""'''''
#ASCII art by JGS
library(dplyr)
library(tidyr)
library(reshape)

datLivestockLong <- read.csv('DataParams/RHoMISmother_rawdata_Simon.csv', stringsAsFactors = F)
datLivestockLong$HouseholdID <- paste0(datLivestockLong$HouseholdID, datLivestockLong$country, datLivestockLong$sublocation)
datLivestockLong <- datLivestockLong[!(datLivestockLong$projectname %in% c("INDER", "CCAFS_IWMI", "SIIL")),] #Remove HHs with HDDS rather than MDD-W

#yieldUnits <- read.csv('DataParams/CropUnitsAll.csv')

##Livestock wrangling
datLivestockLong <- select(datLivestockLong, HouseholdID, livestock_name_1:bees_who_control_eating_5, country, livestock_heads_cattle, livestock_heads_chicken, livestock_other1, livestock_other2, livestock_other3)
colnames(datLivestockLong)[2:281] <- gsub('(.*)_+', '\\1.', names(datLivestockLong)[2:281]) # replace the last underscore with a period

datLivestockLong <- reshape(data.frame(datLivestockLong), direction="long", varying=names(datLivestockLong)[2:281], 
                            idvar=c("HouseholdID"), timevar="loop", sep = ".")
datLivestockLong <- datLivestockLong[!is.na(datLivestockLong),]

datLivestockLong$livestock_name <- ifelse(datLivestockLong$livestock_name == "other1", datLivestockLong$livestock_other1, 
                                          ifelse(datLivestockLong$livestock_name == "other2", datLivestockLong$livestock_other2, 
                                                 ifelse(datLivestockLong$livestock_name == "other3", datLivestockLong$livestock_other3, as.character(datLivestockLong$livestock_name))))

#@Dressed weight and assuming 20% bone and trim (therefore only 80% of dressed weight available for consumption/sale)
carcassWeight <- data.frame(livestock_name = c("cattle", "cows", "calves", "turkey", "chicken", "chicken_poultry", "guinea_fowl", "otherpoultry", "dogs", "fish", "goats", "guinea_pigs_cavy", "pigs", "rabbits", "sheep", "canard", "canards", "cobaille", "cobaye", "cobayes", "dacks", "duck", "ducks", "vache", "pintades", "small_mammals"), 
                            carcassWeight = c(150 * 0.8, 150 * 0.8, 50 * 0.8, 2 *0.8, 1.2 * 0.8, 1.2 * 0.8, 1 * 0.8, 1 * 0.8, 3, 1, 11.25, 0.5, 15, 0.5, 11.25, 1 * 0.8, 1 * 0.8, 0.5, 0.5, 0.5, 1 * 0.8, 1 * 0.8, 1 * 0.8, 150 * 0.8, 1 * 0.8, 3 * 0.8),
                            liverWeight = c(6, 6, 2, 0.03, 0.02, 0.02, 0.02, 0.02, 0, 0.01, 0.05, 0.01, 0.6, 0.1, 0.5, 0.02, 0.02, 0.01, 0.01, 0.01, 0.02, 0.02, 0.02, 6, 0.02, 0.5)) #not slaughtered: "camel",  "donkeys_horses" 

#No meat consumed from donkeys_horses table(datLivestockLong$meat_consumed_amount[datLivestockLong$livestock_name == "donkeys_horses"])

datLivestockLong$livestock_name[datLivestockLong$livestock_name %in% c("cow", "vaches")] <- "cows"
datLivestockLong$livestock_name[datLivestockLong$livestock_name %in% c("dack", "canard ", "ducks ", "bata")] <- "duck"
datLivestockLong$livestock_name[datLivestockLong$livestock_name == "cow"] <- "cattle"
datLivestockLong$livestock_name[datLivestockLong$livestock_name == "dindon"] <- "turkey"
datLivestockLong$livestock_name[substring(datLivestockLong$livestock_name, 3, 6) == "tang"] <- "fish"
datLivestockLong$livestock_name[datLivestockLong$livestock_name %in% c("guinea pigs", "simbilisi")] <- "guinea_pigs_cavy"
datLivestockLong$livestock_name[datLivestockLong$livestock_name == "guinea fowl"] <- "guinea_fowl"


#paramCarcassWeight <- list(cattle = 150 * 0.6, chicken = 1.2 * 0.6, chicken_poultry = 1.2 * 0.6, otherpoultry = 1 * 0.6, dogs = 3, fish = 1, goats = 11.25, guinea_pigs_cavy = 0.5, pigs = 15, rabbits = 0.5, sheep = 11.25, canard = 1 * 0.6, canards = 1 * 0.6, cobaille = 0.5, cobaye = 0.5, cobayes = 0.5, dacks = 1 * 0.6, duck = 1 * 0.6, ducks = 1 * 0.6, vache = 150 * 0.6)
#@Sources
#Weights - Cattle: https://www.sciencedirect.com/science/article/pii/S1871141315300275 ; poultry: https://repository.up.ac.za/bitstream/handle/2263/45858/Raphulu_Carcass_2015.pdf;sequence=1 ; goats: Simela, 2008 ; 
#liver weights: https://issuu.com/forensicmed/docs/animal_anatomy_compilation
#datLivestockLong$carcassWeight <- as.vector(paramCarcassWeight[datLivestockLong$livestock_name])

datLivestockLong <- left_join(datLivestockLong, carcassWeight)
datLivestockLong$meatSlaughterKG <- as.numeric(datLivestockLong$killed_for_meat) * datLivestockLong$carcassWeight
datLivestockLong$liverSlaughterKG <- as.numeric(datLivestockLong$killed_for_meat) * datLivestockLong$liverWeight

#Some households have no data on the proportion consumed or sold, but do list single uses.
datLivestockLong$meat_consumed_amount <- ifelse(is.na(datLivestockLong$meat_consumed_amount) & datLivestockLong$meat_use == "use", "all", datLivestockLong$meat_consumed_amount) 
datLivestockLong$meatFarmConsumedPerc <- ifelse(datLivestockLong$meat_consumed_amount == "all", 1,
                                                ifelse(datLivestockLong$meat_consumed_amount == "half", 0.5,
                                                       ifelse(datLivestockLong$meat_consumed_amount == "little", 0.05,
                                                              ifelse(datLivestockLong$meat_consumed_amount == "most", 0.7,
                                                                     ifelse(datLivestockLong$meat_consumed_amount == "underhalf", 0.25,
                                                                            ifelse(datLivestockLong$meat_consumed_amount == "none", 0,NA))))))

#Some households have no data on the proportion consumed or sold, but do list single uses.
datLivestockLong$meat_sell_amount <- ifelse(is.na(datLivestockLong$meat_sell_amount) & datLivestockLong$meat_use == "sell", "all", datLivestockLong$meat_sell_amount)
datLivestockLong$meatFarmSoldPerc <- ifelse(datLivestockLong$meat_sell_amount == "all", 1,
                                            ifelse(datLivestockLong$meat_sell_amount == "half", 0.5,
                                                   ifelse(datLivestockLong$meat_sell_amount == "little", 0.05,
                                                          ifelse(datLivestockLong$meat_sell_amount == "most", 0.7,
                                                                 ifelse(datLivestockLong$meat_sell_amount == "underhalf", 0.25,
                                                                        ifelse(datLivestockLong$meat_sell_amount == "none", 0, NA))))))


#!The proportion sold is more likely accurate. Base consumed proportion on the recip of sold where possible
#!There are other uses where the percentage is not enumerated.
#datLivestockLong$meatFarmSoldPerc <- ifelse(is.na(datLivestockLong$meatFarmSoldPerc), 1 - datLivestockLong$meatFarmConsumedPerc, datLivestockLong$meatFarmSoldPerc)
#datLivestockLong$meatFarmConsumedPerc <- ifelse(is.na(datLivestockLong$meatFarmSoldPerc), 1 - datLivestockLong$meatFarmSoldPerc, datLivestockLong$meatFarmConsumedPerc)

datLivestockLong$meatFarmConsumedKG <- datLivestockLong$meatSlaughterKG * datLivestockLong$meatFarmConsumedPerc
datLivestockLong$liverFarmConsumedKG <- datLivestockLong$liverSlaughterKG * datLivestockLong$meatFarmConsumedPerc
datLivestockLong$meatFarmSoldKG <- datLivestockLong$meatSlaughterKG * datLivestockLong$meatFarmSoldPerc
datLivestockLong$liverFarmSoldKG <- datLivestockLong$liverSlaughterKG * (1-datLivestockLong$meatFarmSoldPerc)

datLivestockLong$incomeMeat_lc <- datLivestockLong$meat_sold_income

##Milk - good + lean
paramLactLength <- 270 #Based on Millogo, 2010 https://pub.epsilon.slu.se/2208/1/millogo_v_100107.pdf
paramPropGoodSeason <- 0.8 #Assuming feed availability is also limited in the 'bad season'
#datLivestockLong$daysMilkGood <- ifelse(datLivestockLong$numFoodShortMonth)

#Milk in good and lean season, apportioning across season where relevant, otherwise assumed a 270 day lactation all in 'good' season
#!There are 347 instances of goat milk. All of the per animal enumerations have # also enumerated so no need to use livestock_heads_goats. Similarly with the 4 sheep milk instances
#!On the fer occasions where the number of animals milked is not listed, a coarse proxy has been used - total cattle. This is unrealistic that all animals are milked
datLivestockLong$totalGoodMilkL <- ifelse(datLivestockLong$milk_amount_bad_season >0 & datLivestockLong$milk_units %in% c("l/day", "l_day"), datLivestockLong$milk_amount_good_season * paramLactLength * paramPropGoodSeason,
                                          ifelse(datLivestockLong$milk_amount_bad_season >0 & datLivestockLong$milk_units %in% c("l_animal_day", "l/animal/day"), datLivestockLong$milk_amount_good_season * datLivestockLong$milk_number_animals_milked * paramLactLength * paramPropGoodSeason,
                                                 ifelse(datLivestockLong$milk_amount_bad_season >0 & datLivestockLong$milk_units == "0.3l/animal/day", datLivestockLong$milk_amount_good_season * datLivestockLong$livestock_heads_cattle * paramLactLength * paramPropGoodSeason,
                                                        ifelse(datLivestockLong$milk_amount_bad_season >0 & datLivestockLong$milk_units == "0.3l/day", datLivestockLong$milk_amount_good_season * 0.3 * paramLactLength * paramPropGoodSeason,
                                                               ifelse(datLivestockLong$milk_amount_bad_season >0 & datLivestockLong$milk_amount_units_other %in% c("litres de lait par semaine", "litres de lait par semaine"), datLivestockLong$milk_amount_good_season * paramLactLength * paramPropGoodSeason / 7,
                                                                      ifelse(datLivestockLong$milk_amount_bad_season >0 & datLivestockLong$milk_amount_units_other %in% c("cup", "bottles/day", "cups_half_l/day"), datLivestockLong$milk_amount_good_season * 0.24 * paramLactLength * paramPropGoodSeason,
                                                                             ifelse(datLivestockLong$milk_amount_bad_season >0 & datLivestockLong$milk_amount_units_other == "i bucket", datLivestockLong$milk_amount_good_season * 15 * paramLactLength * paramPropGoodSeason,
                                                                                    ifelse(datLivestockLong$milk_amount_bad_season >0 & datLivestockLong$milk_amount_units_other %in% c("litres pour six mois", "total annuel", "total"), datLivestockLong$milk_amount_good_season * 0.24 * paramLactLength * paramPropGoodSeason,
                                                                                           ifelse(datLivestockLong$milk_amount_bad_season >0 & datLivestockLong$milk_amount_units_other == "per animal per week", datLivestockLong$milk_amount_good_season * datLivestockLong$milk_number_animals_milked * paramLactLength * paramPropGoodSeason / 7, 
                                                                                                  ifelse(datLivestockLong$milk_amount_bad_season == 0 & datLivestockLong$milk_units %in% c("l/day", "l_day"), datLivestockLong$milk_amount_good_season * paramLactLength,
                                                                                                         ifelse(datLivestockLong$milk_amount_bad_season == 0 & datLivestockLong$milk_units %in% c("l_animal_day", "l/animal/day"), datLivestockLong$milk_amount_good_season * datLivestockLong$milk_number_animals_milked * paramLactLength,
                                                                                                                ifelse(datLivestockLong$milk_amount_bad_season == 0 & datLivestockLong$milk_units == "0.3l/animal/day", datLivestockLong$milk_amount_good_season * datLivestockLong$livestock_heads_cattle * paramLactLength,
                                                                                                                       ifelse(datLivestockLong$milk_amount_bad_season == 0 & datLivestockLong$milk_units == "0.3l/day", datLivestockLong$milk_amount_good_season * 0.3 * paramLactLength,
                                                                                                                              ifelse(datLivestockLong$milk_amount_bad_season == 0 & datLivestockLong$milk_amount_units_other %in% c("litres de lait par semaine", "litres de lait par semaine"), datLivestockLong$milk_amount_good_season * paramLactLength / 7,
                                                                                                                                     ifelse(datLivestockLong$milk_amount_bad_season == 0 & datLivestockLong$milk_amount_units_other == "cup", datLivestockLong$milk_amount_good_season * 0.24 * paramLactLength,
                                                                                                                                            ifelse(datLivestockLong$milk_amount_bad_season == 0 & datLivestockLong$milk_amount_units_other == "i bucket", datLivestockLong$milk_amount_good_season * 15 * paramLactLength,
                                                                                                                                                   ifelse(datLivestockLong$milk_amount_bad_season == 0 & datLivestockLong$milk_amount_units_other %in% c("litres pour six mois", "total annuel"), datLivestockLong$milk_amount_good_season * 0.24 * paramLactLength,
                                                                                                                                                          ifelse(datLivestockLong$milk_amount_bad_season == 0 & datLivestockLong$milk_amount_units_other == "per animal per week", datLivestockLong$milk_amount_good_season * datLivestockLong$milk_number_animals_milked * paramLactLength / 7,NA))))))))))))))))))

datLivestockLong$totalBadMilkL <- ifelse(datLivestockLong$milk_amount_bad_season >0 & datLivestockLong$milk_units %in% c("l/day", "l_day"), datLivestockLong$milk_amount_bad_season * paramLactLength * (1- paramPropGoodSeason),
                                         ifelse(datLivestockLong$milk_amount_bad_season >0 & datLivestockLong$milk_units %in% c("l_animal_day", "l/animal/day"), datLivestockLong$milk_amount_bad_season * datLivestockLong$milk_number_animals_milked * paramLactLength * (1- paramPropGoodSeason),
                                                ifelse(datLivestockLong$milk_amount_bad_season >0 & datLivestockLong$milk_units == "0.3l/animal/day", datLivestockLong$milk_amount_bad_season * datLivestockLong$livestock_heads_cattle * paramLactLength * (1- paramPropGoodSeason),
                                                       ifelse(datLivestockLong$milk_amount_bad_season >0 & datLivestockLong$milk_units == "0.3l/day", datLivestockLong$milk_amount_bad_season * 0.3 * paramLactLength * (1- paramPropGoodSeason),
                                                              ifelse(datLivestockLong$milk_amount_bad_season >0 & datLivestockLong$milk_amount_units_other %in% c("litres de lait par semaine", "litres de lait par semaine"), datLivestockLong$milk_amount_bad_season * paramLactLength * (1- paramPropGoodSeason) / 7,
                                                                     ifelse(datLivestockLong$milk_amount_bad_season >0 & datLivestockLong$milk_amount_units_other %in% c("cup", "bottles/day", "cups_half_l/day"), datLivestockLong$milk_amount_bad_season * 0.24 * paramLactLength * (1- paramPropGoodSeason),
                                                                            ifelse(datLivestockLong$milk_amount_bad_season >0 & datLivestockLong$milk_amount_units_other == "i bucket", datLivestockLong$milk_amount_bad_season * 15 * paramLactLength * (1- paramPropGoodSeason),
                                                                                   ifelse(datLivestockLong$milk_amount_bad_season >0 & datLivestockLong$milk_amount_units_other %in% c("litres pour six mois", "total annuel", "total"), datLivestockLong$milk_amount_bad_season * 0.24 * paramLactLength * (1- paramPropGoodSeason),
                                                                                          ifelse(datLivestockLong$milk_amount_bad_season >0 & datLivestockLong$milk_amount_units_other == "per animal per week", datLivestockLong$milk_amount_bad_season * datLivestockLong$milk_number_animals_milked * paramLactLength * (1- paramPropGoodSeason) / 7, NA)))))))))

datLivestockLong$totalMilkL <- rowSums(datLivestockLong[, c("totalGoodMilkL", "totalBadMilkL")], na.rm=T)

#Some households have no data on the proportion consumed or sold, but do list single uses.
datLivestockLong$milk_consumed_amount <- ifelse(is.na(datLivestockLong$milk_consumed_amount) & datLivestockLong$milk_use %in% c("use", "use dairy"), "all", datLivestockLong$milk_consumed_amount) 

datLivestockLong$milkFarmConsumedPerc <- ifelse(datLivestockLong$milk_consumed_amount == "all", 1, 
                                                ifelse(datLivestockLong$milk_consumed_amount == "half", 0.5,
                                                       ifelse(datLivestockLong$milk_consumed_amount == "little", 0.05,
                                                              ifelse(datLivestockLong$milk_consumed_amount == "most", 0.7,
                                                                     ifelse(datLivestockLong$milk_consumed_amount == "underhalf", 0.25,
                                                                            ifelse(datLivestockLong$milk_consumed_amount == "none", 0, NA))))))

#Some households have no data on the proportion consumed or sold, but do list single uses.
datLivestockLong$milk_sell_amount <- ifelse(is.na(datLivestockLong$milk_sell_amount) & datLivestockLong$milk_use == "use", "all", datLivestockLong$milk_sell_amount) 

datLivestockLong$milkFarmSoldPerc <- ifelse(datLivestockLong$milk_sell_amount == "all", 1,
                                            ifelse(datLivestockLong$milk_sell_amount == "half", 0.5,
                                                   ifelse(datLivestockLong$milk_sell_amount == "little", 0.05,
                                                          ifelse(datLivestockLong$milk_sell_amount == "most", 0.7,
                                                                 ifelse(datLivestockLong$milk_sell_amount == "underhalf", 0.25,
                                                                        ifelse(datLivestockLong$milk_sell_amount == "none", 0, NA))))))


#datLivestockLong$milkFarmSoldPerc <- ifelse(is.na(datLivestockLong$milkFarmSoldPerc), 1 - datLivestockLong$milkFarmConsumedPerc, datLivestockLong$milkFarmSoldPerc)
#datLivestockLong$milkFarmConsumedPerc <- ifelse(is.na(datLivestockLong$milkFarmSoldPerc), 1 - datLivestockLong$milkFarmSoldPerc, datLivestockLong$milkFarmConsumedPerc)

datLivestockLong$milkFarmConsumedKG <- datLivestockLong$totalMilkL * datLivestockLong$milkFarmConsumedPerc

datLivestockLong$milkFarmSoldKG <- datLivestockLong$totalMilkL * datLivestockLong$milkFarmSoldPerc

datLivestockLong$goatSheepMilkL <- ifelse(datLivestockLong$livestock_name %in% c("goats", "sheep"), datLivestockLong$totalMilkL, NA)

datLivestockLong$incomeMilk_lc <- ifelse(datLivestockLong$milk_sold_price_timeunits == "day", datLivestockLong$milk_sold_income * paramLactLength,
                                         ifelse(datLivestockLong$milk_sold_price_timeunits == "week", datLivestockLong$milk_sold_income * paramLactLength / 7, 
                                                ifelse(datLivestockLong$milk_sold_price_timeunits == "month", datLivestockLong$milk_sold_income * paramLactLength / 30,
                                                       ifelse(datLivestockLong$milk_sold_price_timeunits == "year", datLivestockLong$milk_sold_income,
                                                              ifelse(datLivestockLong$milk_amount_time_units_other == "2 liters", datLivestockLong$milk_sold_income * datLivestockLong$totalMilkL/2,
                                                                     ifelse(datLivestockLong$milk_amount_time_units_other %in% c("litre", "litres", "per litre"), datLivestockLong$milk_sold_income * datLivestockLong$totalMilkL, NA))))))

paramEggLayingDays <- 250

datLivestockLong$eggNumGood <- ifelse(datLivestockLong$eggs_amount_bad >0 & datLivestockLong$eggs_units == "pieces/animal/day", datLivestockLong$eggs_amount_good * paramEggLayingDays * datLivestockLong$livestock_heads_chicken * paramPropGoodSeason,
                                      ifelse(datLivestockLong$eggs_amount_bad >0 & datLivestockLong$eggs_units == "pieces/day", datLivestockLong$eggs_amount_good * paramEggLayingDays * paramPropGoodSeason,
                                             ifelse(datLivestockLong$eggs_amount_bad >0 & datLivestockLong$eggs_units == "pieces/week", datLivestockLong$eggs_amount_good * paramEggLayingDays/7 * paramPropGoodSeason,
                                                    ifelse(datLivestockLong$eggs_amount_bad >0 & datLivestockLong$eggs_amount_units_other == "at a time ", datLivestockLong$eggs_amount_good * paramEggLayingDays * paramPropGoodSeason,
                                                           ifelse(datLivestockLong$eggs_amount_bad >0 & datLivestockLong$eggs_amount_units_other == "eggs per chicken per month", datLivestockLong$eggs_amount_good * paramEggLayingDays / 30 * datLivestockLong$livestock_heads_chicken * paramPropGoodSeason,
                                                                  ifelse(datLivestockLong$eggs_amount_bad >0 & datLivestockLong$eggs_amount_units_other == "lay eggs in interval not always", datLivestockLong$eggs_amount_good * paramEggLayingDays/3 * paramPropGoodSeason,
                                                                         ifelse(datLivestockLong$eggs_amount_bad >0 & datLivestockLong$eggs_amount_units_other %in% c("tout les 12 dernier mois", "no. other"), datLivestockLong$eggs_amount_good * paramPropGoodSeason,
                                                                                ifelse(datLivestockLong$eggs_amount_bad == 0 & datLivestockLong$eggs_units == "pieces/animal/day", datLivestockLong$eggs_amount_good * paramEggLayingDays * datLivestockLong$livestock_heads_chicken,
                                                                                       ifelse(datLivestockLong$eggs_amount_bad == 0 & datLivestockLong$eggs_units == "pieces/day", datLivestockLong$eggs_amount_good * paramEggLayingDays,
                                                                                              ifelse(datLivestockLong$eggs_amount_bad == 0 & datLivestockLong$eggs_units == "pieces/week", datLivestockLong$eggs_amount_good * paramEggLayingDays/7,
                                                                                                     ifelse(datLivestockLong$eggs_amount_bad == 0 & datLivestockLong$eggs_amount_units_other == "at a time ", datLivestockLong$eggs_amount_good * paramEggLayingDays,
                                                                                                            ifelse(datLivestockLong$eggs_amount_bad == 0 & datLivestockLong$eggs_amount_units_other == "eggs per chicken per month", datLivestockLong$eggs_amount_good * paramEggLayingDays / 30 * datLivestockLong$livestock_heads_chicken,
                                                                                                                   ifelse(datLivestockLong$eggs_amount_bad == 0 & datLivestockLong$eggs_amount_units_other == "lay eggs in interval not always", datLivestockLong$eggs_amount_good * paramEggLayingDays/3,
                                                                                                                          ifelse(datLivestockLong$eggs_amount_bad == 0 & datLivestockLong$eggs_amount_units_other %in% c("tout les 12 dernier mois", "no. other"), datLivestockLong$eggs_amount_good, NA))))))))))))))

datLivestockLong$eggNumBad <- ifelse(datLivestockLong$eggs_units == "pieces/animal/day", datLivestockLong$eggs_amount_bad * paramEggLayingDays * datLivestockLong$livestock_heads_chicken * (1 - paramPropGoodSeason),
                                     ifelse(datLivestockLong$eggs_units == "pieces/day", datLivestockLong$eggs_amount_bad * paramEggLayingDays * (1 - paramPropGoodSeason),
                                            ifelse(datLivestockLong$eggs_units == "pieces/week", datLivestockLong$eggs_amount_bad * paramEggLayingDays/7 * (1 - paramPropGoodSeason),
                                                   ifelse(datLivestockLong$eggs_amount_units_other == "at a time ", datLivestockLong$eggs_amount_bad * paramEggLayingDays * (1 - paramPropGoodSeason),
                                                          ifelse(datLivestockLong$eggs_amount_units_other == "eggs per chicken per month", datLivestockLong$eggs_amount_bad * paramEggLayingDays / 30 * datLivestockLong$livestock_heads_chicken * (1 - paramPropGoodSeason),
                                                                 ifelse(datLivestockLong$eggs_amount_units_other == "lay eggs in interval not always", datLivestockLong$eggs_amount_bad * paramEggLayingDays/3 * (1 - paramPropGoodSeason),
                                                                        ifelse(datLivestockLong$eggs_amount_units_other %in% c("tout les 12 dernier mois", "no. other"), datLivestockLong$eggs_amount_bad * (1 - paramPropGoodSeason), NA)))))))

#Some households have no data on the proportion consumed or sold, but do list single uses.
datLivestockLong$eggs_consumed_amount <- ifelse(is.na(datLivestockLong$eggs_consumed_amount) & datLivestockLong$eggs_use %in% c("use"), "all", datLivestockLong$eggs_consumed_amount) 

datLivestockLong$eggsFarmConsumedPerc <- ifelse(datLivestockLong$eggs_consumed_amount == "all", 1, 
                                                ifelse(datLivestockLong$eggs_consumed_amount == "half", 0.5,
                                                       ifelse(datLivestockLong$eggs_consumed_amount == "little", 0.05,
                                                              ifelse(datLivestockLong$eggs_consumed_amount == "most", 0.7,
                                                                     ifelse(datLivestockLong$eggs_consumed_amount == "underhalf", 0.25,
                                                                            ifelse(datLivestockLong$eggs_consumed_amount == "none", 0, NA))))))

#Some households have no data on the proportion consumed or sold, but do list single uses.
datLivestockLong$eggs_sell_amount <- ifelse(is.na(datLivestockLong$eggs_sell_amount) & datLivestockLong$eggs_use %in% c("use"), "all", datLivestockLong$eggs_sell_amount) 

datLivestockLong$eggsFarmSoldPerc <- ifelse(datLivestockLong$eggs_sell_amount == "all", 1,
                                            ifelse(datLivestockLong$eggs_sell_amount == "half", 0.5,
                                                   ifelse(datLivestockLong$eggs_sell_amount == "little", 0.05,
                                                          ifelse(datLivestockLong$eggs_sell_amount == "most", 0.7,
                                                                 ifelse(datLivestockLong$eggs_sell_amount == "underhalf", 0.25,
                                                                        ifelse(datLivestockLong$eggs_sell_amount =="none", 0, NA))))))

#datLivestockLong$eggsFarmSoldPercAdj <- ifelse(datLivestockLong$eggsFarmSoldPerc)

datLivestockLong$totalEggsNum <- rowSums(datLivestockLong[,c("eggNumGood", "eggNumBad")], na.rm=T)

#datLivestockLong$eggsFarmSoldPerc <- ifelse(is.na(datLivestockLong$eggsFarmSoldPerc), 1 - datLivestockLong$eggsFarmConsumedPerc, datLivestockLong$eggsFarmSoldPerc)
#datLivestockLong$eggsFarmConsumedPerc <- ifelse(is.na(datLivestockLong$eggsFarmSoldPerc), 1 - datLivestockLong$eggsFarmSoldPerc, datLivestockLong$eggsFarmConsumedPerc)

datLivestockLong$eggsFarmConsumedNum <- datLivestockLong$totalEggsNum * datLivestockLong$eggsFarmConsumedPerc
datLivestockLong$eggsFarmSoldNum <- datLivestockLong$totalEggsNum * datLivestockLong$eggsFarmSoldPerc

datLivestockLong$incomeEggs_lc <- ifelse(datLivestockLong$eggs_sold_price_timeunits == "day", datLivestockLong$eggs_sold_income * paramEggLayingDays,
                                         ifelse(datLivestockLong$eggs_sold_price_timeunits == "week", datLivestockLong$eggs_sold_income * paramEggLayingDays/7,
                                                ifelse(datLivestockLong$eggs_sold_price_timeunits == "month", datLivestockLong$eggs_sold_income * paramEggLayingDays/30,
                                                       ifelse(datLivestockLong$eggs_sold_price_timeunits == "year", datLivestockLong$eggs_sold_income,
                                                              ifelse(datLivestockLong$eggs_sold_price_timeunits =="other" & datLivestockLong$eggs_sold_price_timeunits_other == "two weeks", datLivestockLong$eggs_sold_income * paramEggLayingDays/14,
                                                                     ifelse(datLivestockLong$eggs_sold_price_timeunits =="other" & datLivestockLong$eggs_sold_price_timeunits_other %in% c("per egg", "price per egg"), datLivestockLong$eggs_sold_income * datLivestockLong$eggsFarmSoldNum,
                                                                            ifelse(datLivestockLong$eggs_sold_price_timeunits =="other" & datLivestockLong$eggs_sold_price_timeunits_other == "each day when they decide to sell (she does not sell each day)", datLivestockLong$eggs_sold_income * paramLactLength/3, NA)))))))



#!check calabash and deve volumes
datLivestockLong$totalHoneyKG <- ifelse(datLivestockLong$bees_honey_production_units == "calabash", 1.5 * datLivestockLong$bees_honey_production,
                                        ifelse(datLivestockLong$bees_honey_production_units == " debe", 15 * datLivestockLong$bees_honey_production, 1 * datLivestockLong$bees_honey_production))


#Some households have no data on the proportion consumed or sold, but do list single uses.
datLivestockLong$bees_honey_consumed_amount <- ifelse(is.na(datLivestockLong$bees_honey_consumed_amount) & datLivestockLong$bees_honey_use %in% c("use"), "all", datLivestockLong$bees_honey_consumed_amount) 

datLivestockLong$honeyFarmConsumedPerc <- ifelse(datLivestockLong$bees_honey_consumed_amount == "all", 1, 
                                                 ifelse(datLivestockLong$bees_honey_consumed_amount == "half", 0.5,
                                                        ifelse(datLivestockLong$bees_honey_consumed_amount == "little", 0.05,
                                                               ifelse(datLivestockLong$bees_honey_consumed_amount == "most", 0.7,
                                                                      ifelse(datLivestockLong$bees_honey_consumed_amount == "underhalf", 0.25,
                                                                             ifelse(datLivestockLong$bees_honey_consumed_amount == "none", 0, NA))))))

datLivestockLong$honeyFarmConsumedKG <- datLivestockLong$totalHoneyKG * datLivestockLong$honeyFarmConsumedPerc

#Who sells, who controls eating, livestock ownership
datLivestockLong$meatFemale_controlRevenue <- ifelse(grepl(paste(c("\\bmale\\w*?\\b", "man"), collapse="|"), datLivestockLong$livestock_meat_who_sells) == F  & !is.na(datLivestockLong$livestock_meat_who_sells), datLivestockLong$incomeMeat_lc, NA)
datLivestockLong$milkFemale_controlRevenue <- ifelse(grepl(paste(c("\\bmale\\w*?\\b", "man"), collapse="|"), datLivestockLong$milk_who_sells) == F  & !is.na(datLivestockLong$milk_who_sells), datLivestockLong$incomeMilk_lc, NA)
datLivestockLong$eggsFemale_controlRevenue <- ifelse(grepl(paste(c("\\bmale\\w*?\\b", "man"), collapse="|"), datLivestockLong$eggs_who_sells) == F  & !is.na(datLivestockLong$eggs_who_sells), datLivestockLong$incomeEggs_lc, NA)


#!No data from 'mortality' meat that's sold / consumed ?!

datLivestockLong <- group_by(datLivestockLong, HouseholdID)
summaryLivestock <- summarise(datLivestockLong, totalHoneyKG = sum(totalHoneyKG, na.rm=T), totalLiverKG = sum(liverSlaughterKG, na.rm=T), liverFarmConsumedKG = sum(liverFarmConsumedKG, na.rm=T), totalGoodMilkL = sum(totalGoodMilkL, na.rm=T), totalBadMilkL = sum(totalBadMilkL, na.rm=T), eggNumGood = sum(eggNumGood, na.rm=T), eggNumBad = sum(eggNumBad, na.rm=T), incomeLiveAnimal = sum(livestock_sale_income, na.rm=T), totalMeatKG = sum(meatSlaughterKG, na.rm=T), meatFarmConsumedKG = sum(meatFarmConsumedKG, na.rm=T), meatFarmSoldKG = sum(meatFarmSoldKG, na.rm=T), incomeMeat_lc = sum(incomeMeat_lc, na.rm=T), totalMilkL = sum(totalMilkL, na.rm = T), milkFarmConsumedKG = sum(milkFarmConsumedKG, na.rm=T), milkFarmSoldKG = sum(milkFarmSoldKG, na.rm=T), goatSheepMilkL = sum(goatSheepMilkL, na.rm=T), incomeMilk_lc = sum(incomeMilk_lc, na.rm = T), totalEggsNum = sum(totalEggsNum, na.rm=T), eggsFarmSoldNum = sum(eggsFarmSoldNum, na.rm=T), eggsFarmConsumedNum = sum(eggsFarmConsumedNum, na.rm=T), incomeEggs_lc = sum(incomeEggs_lc, na.rm=T), incomeHoney_lc = sum(bees_honey_sold_income, na.rm=T), incomeMeatFemaleControl_lc = sum(meatFemale_controlRevenue, na.rm=T), incomeMilkFemaleControl_lc = sum(milkFemale_controlRevenue, na.rm = T), incomeEggsFemaleControl = sum(eggsFemale_controlRevenue, na.rm=T))
