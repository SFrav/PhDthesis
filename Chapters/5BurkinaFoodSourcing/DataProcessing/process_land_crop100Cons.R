#
#               ,,
#    ,,       ,\\//,          Prepare land area and crop yield and utilisation variables
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
#ASCI credit: 'Pumpkins in the cornfield', anonymous, 1997
###########################################################
library(dplyr)
library(tidyr)

paramUSD <- 0.001665

##Land
Landdat<-read.csv('2_1Bien terriens.csv')
landdatLong <- gather(select(Landdat, Household.ID, ends_with("su")), var, area, ends_with("su")) #area
landdatLong1 <- gather(select(Landdat, Household.ID, ends_with("uni")), var, unit, ends_with("uni")) #unit of area
landdatLong2 <- gather(select(Landdat, Household.ID, ends_with("dis")), var, dist, ends_with("dis")) #distance of plot
landdatLong3 <- gather(select(Landdat, Household.ID, ends_with("fon")), var, ownership, ends_with("fon")) #1 = plot title, 2 = untitled, 4 = rented
landdatLong4 <- gather(select(Landdat, Household.ID, ends_with("app")), var, genderOwn, ends_with("app")) #gender of ownership
landdatLong$var <- gsub('[a-zA-Z]', "", landdatLong$var)
landdatLong1$var <- gsub('[a-zA-Z]', "", landdatLong1$var)
landdatLong2$var <- gsub('[a-zA-Z]', "", landdatLong2$var)
landdatLong3$var <- gsub('[a-zA-Z]', "", landdatLong3$var)
landdatLong4$var <- gsub('[a-zA-Z]', "", landdatLong4$var)

landdatLong <- left_join(landdatLong, landdatLong1)
landdatLong <- left_join(landdatLong, landdatLong2)
landdatLong <- left_join(landdatLong, landdatLong3)
landdatLong <- left_join(landdatLong, landdatLong4)
landdatLong$area <- ifelse(landdatLong$unit == 2, landdatLong$area / 10000, landdatLong$area)
landdatLong$rented <- ifelse(landdatLong$ownership %in% c(4), landdatLong$area, 0)
landdatLong$titled <- ifelse(landdatLong$ownership %in% c(1), landdatLong$area, 0)
landdatLong$femaleOwned <- ifelse(landdatLong$genderOwn == 2, landdatLong$area, 0)

landdatLong <- group_by(landdatLong, Household.ID)
datLand <- summarise(landdatLong, land_area..ha. = sum(area, na.rm=T), land_areaRented = sum(rented, na.rm=T), land_titled = sum(titled, na.rm=T), land_areaFemaleOwned = sum(femaleOwned, na.rm=T), distPlotMean = mean(dist, na.rm=T), distPlotMax = max(dist, na.rm=T))


##Crops
Cropdat<-read.csv('2_4aPrd cultur bonne saison.csv')
Cropdat2<-read.csv('2_4bPrd cultur mauvaise saison.csv')
cropEnergy <- data.frame(crop = c("Mil", "Sor", "Mai", "Riz", "Fon", "Nie", "Ara", "Ses", "Ign", "Man", "Pim", "Leg", "Aut"), 
                         kcal = c(3480, 3440, 3530, 3530, 3530, 3160, 5780*0.75, 5770, 1280, 1530, 450, 500, 600), 
                         protein = c(109, 105, 90, 61, 89, 212, 224*0.75, 182, 19, 12, 19, 20, 15),
                         edibleConvF = c(1, 1, 1, 1, 1, 1, 1, 1, 0.81, 1, 0.73, 1, 1),
                         FAOcode = c("01_015", "01_039", "01_006", "01_036", "01_002", "03_004", "06_010", "06_015", "02_019", "02_001", "04_046", "04", "05"),
                         fullName = c("Millet", "Sorghum", "Maize", "Rice", "Fonio", "Cowpea", "Peanut", "Sesame", "Yam", "Cassava", "chilli pepper", "Vegetables", "Other"))
#FAO, 2012, West african food composition table: http://www.fao.org/docrep/015/i2698b/i2698b00.pdf
#Groundnut value is for shelled. Convert 5780, 224. 25% of peanut yield is shell, so * 0.75 (according to this site https://www.fatsecret.com/calories-nutrition/generic/peanuts-in-shell-(shell-not-eaten)?portionid=9907&portionamount=1.000)
#Vegetable kcal content is between 250 and 550 kcal and 10 to 50 g / kg
#kcal for other crops assumed to be low - 600 kcal/kg. Asumming that this is mostly fruit

cropDatGoodSeasLong <- gather(select(Cropdat, Household.ID, ends_with("sup")), crop, area, ends_with("sup"))
cropDatGoodSeasLong1 <- gather(select(Cropdat, Household.ID, ends_with("rend")), crop, yieldKG, ends_with("rend")) #Data was collected as total yield for crop, not per ha
cropDatGoodSeasLong2 <- gather(select(Cropdat, Household.ID, ends_with("val")), crop, valFCFA, ends_with("val"))
cropDatGoodSeasLong3 <- gather(select(Cropdat, Household.ID, ends_with("ferti")), crop, fert, ends_with("ferti"))
cropDatGoodSeasLong4 <- gather(select(Cropdat, Household.ID, ends_with("irri")), crop, irri, ends_with("irri"))
cropDatGoodSeasLong$crop <- substr(cropDatGoodSeasLong$crop, 1, 3)
cropDatGoodSeasLong1$crop <- substr(cropDatGoodSeasLong2$crop, 1, 3)
cropDatGoodSeasLong2$crop <- substr(cropDatGoodSeasLong2$crop, 1, 3)
cropDatGoodSeasLong3$crop <- substr(cropDatGoodSeasLong3$crop, 1, 3)
cropDatGoodSeasLong4$crop <- substr(cropDatGoodSeasLong4$crop, 1, 3)
cropDatGoodSeasLong <- left_join(cropDatGoodSeasLong, cropDatGoodSeasLong1)
cropDatGoodSeasLong <- left_join(cropDatGoodSeasLong, cropDatGoodSeasLong2)
cropDatGoodSeasLong <- left_join(cropDatGoodSeasLong, cropDatGoodSeasLong3)
cropDatGoodSeasLong <- left_join(cropDatGoodSeasLong, cropDatGoodSeasLong4)
#cropDatGoodSeasLong$season = "good"
cropDatGoodSeasLong$goodYieldKg <- cropDatGoodSeasLong$yieldKG
cropDatGoodSeasLong$goodVal <- cropDatGoodSeasLong$valFCFA
cropDatGoodSeasLong$badYieldKg <- NA
cropDatGoodSeasLong$badVal <- NA

cropDatBadSeasLong <- gather(select(Cropdat2, Household.ID, ends_with("sup")), crop, area, ends_with("sup"))
cropDatBadSeasLong1 <- gather(select(Cropdat2, Household.ID, ends_with("rend")), crop, yieldKG, ends_with("rend")) #Data was collected as total yield for crop, not per ha
cropDatBadSeasLong2 <- gather(select(Cropdat2, Household.ID, ends_with("val")), crop, valFCFA, ends_with("val"))
cropDatBadSeasLong3 <- gather(select(Cropdat2, Household.ID, ends_with("ferti")), crop, fertBad, ends_with("ferti"))
cropDatBadSeasLong4 <- gather(select(Cropdat2, Household.ID, ends_with("irri")), crop, irriBad, ends_with("irri"))
cropDatBadSeasLong$crop <- substr(cropDatBadSeasLong$crop, 1, 3)
cropDatBadSeasLong1$crop <- substr(cropDatBadSeasLong2$crop, 1, 3)
cropDatBadSeasLong2$crop <- substr(cropDatBadSeasLong2$crop, 1, 3)
cropDatBadSeasLong3$crop <- substr(cropDatBadSeasLong3$crop, 1, 3)
cropDatBadSeasLong4$crop <- substr(cropDatBadSeasLong4$crop, 1, 3)
cropDatBadSeasLong <- left_join(cropDatBadSeasLong, cropDatBadSeasLong1)
cropDatBadSeasLong <- left_join(cropDatBadSeasLong, cropDatBadSeasLong2)
cropDatBadSeasLong <- left_join(cropDatBadSeasLong, cropDatBadSeasLong3)
cropDatBadSeasLong <- left_join(cropDatBadSeasLong, cropDatBadSeasLong4)
#cropDatBadSeasLong$season = "bad"
cropDatBadSeasLong$badYieldKg <- cropDatBadSeasLong$yieldKG
cropDatBadSeasLong$badVal <- cropDatBadSeasLong$valFCFA
cropDatBadSeasLong$goodYieldKg <- NA
cropDatBadSeasLong$goodVal <- NA

cropDatLong <- bind_rows(cropDatBadSeasLong, cropDatGoodSeasLong)
#cropDatLong$yieldKG <- cropDatLong$yieldKG * cropDatLong$area
cropDatLong$yieldKG_ha <- cropDatLong$yieldKG / cropDatLong$area
cropDatCompleteLong <- group_by(cropDatLong, Household.ID, crop)
cropDatCompleteLong <- summarise(cropDatCompleteLong, yieldKG_ha = mean(yieldKG_ha, na.rm=T), badYieldKg = sum(badYieldKg, na.rm=T), badVal = sum(badVal, na.rm=T), goodYieldKg = sum(goodYieldKg, na.rm=T), goodVal = sum(goodVal, na.rm=T), yieldKG = sum(yieldKG, na.rm=T), valFCFA = sum(valFCFA, na.rm=T), fertBad = mean(fertBad, na.rm=T), irriBad = mean(irriBad, na.rm=T), fert = mean(fert, na.rm=T), irri = mean(irri, na.rm=T))

cropDatCompleteLong <- left_join(cropDatCompleteLong, cropEnergy)
cropDatCompleteLong$totalfarm_producekcal <- cropDatCompleteLong$yieldKG * cropDatCompleteLong$kcal
cropDatCompleteLong$totalfarm_produceprotein <- cropDatCompleteLong$yieldKG * cropDatCompleteLong$protein
cropDatCompleteLong <- cropDatCompleteLong[cropDatCompleteLong$yieldKG != 0, ]
cropDatCompleteLong$val_kg <- cropDatCompleteLong$valFCFA / cropDatCompleteLong$yieldKG
cropDatCompleteLong$crop <- tolower(cropDatCompleteLong$crop)
#cropDatGoodSeasLong$harvestKG <- cropDatGoodSeasLong$area * cropDatGoodSeasLong$yieldKG_ha
#cropDatGoodSeasLong <- group_by(cropDatGoodSeasLong, Household.ID)
#datCropGood <- summarise


crop_use <- read.csv('7_1 Utilisation produits culture_processed.csv')
crop_useLong <- gather(select(crop_use, Household.ID, ends_with("nom")), index, crop, ends_with("nom"))
crop_useLong1 <- gather(select(crop_use, Household.ID, ends_with("conso")), index, consumedPerc, ends_with("conso"))
crop_useLong2 <- gather(select(crop_use, Household.ID, ends_with("ven")), index, soldPerc, ends_with("ven"))
crop_useLong3 <- gather(select(crop_use, Household.ID, ends_with("bet")), index, livestockPerc, ends_with("bet"))
crop_useLong$index <- gsub('[a-zA-Z]', "", crop_useLong$index)
crop_useLong1$index <- substr(crop_useLong1$index, 5, 5)
crop_useLong2$index <- substr(crop_useLong2$index, 5, 5)
crop_useLong3$index <- substr(crop_useLong3$index, 5, 5)

crop_useLong <- left_join(crop_useLong, crop_useLong1)
crop_useLong <- left_join(crop_useLong, crop_useLong2)
crop_useLong <- left_join(crop_useLong, crop_useLong3)
crop_useLong <- crop_useLong[!is.na(crop_useLong$crop),]
crop_useLong$crop <-  substr(crop_useLong$crop, 1, 3)
crop_useLong$crop <- gsub("maï", "mai", crop_useLong$crop)
crop_useLong$crop <- gsub("Sés", "ses", crop_useLong$crop)
crop_useLong$crop <- gsub("aub", "aut", crop_useLong$crop)
crop_useLong$crop <- tolower(crop_useLong$crop)
crop_useLong <- crop_useLong[crop_useLong$Household.ID !="SESE43" & crop_useLong$crop != "tom",] #SESE43 has cabbage and tomatoes. Same % sold. 
crop_useLong$crop <- ifelse(crop_useLong$crop %in% c("cho", "gom", "oig", "tom", "ose", "pom"), "leg", crop_useLong$crop) #cabbage, okra, onion, tomato, sorrel (spinach dock), potato


cropDatCompleteLong <- left_join(cropDatCompleteLong, select(crop_useLong, -index))

crop_use2<-read.csv('7_2 Productivite agricole.csv')
crop_use2Long <- gather(select(crop_use2, Household.ID, Cult1, Cult2, Cult3, Cult4, Cult5, Cult6), index, crop, c("Cult1","Cult2", "Cult3", "Cult4", "Cult5", "Cult6"))
crop_use2Long1 <- gather(select(crop_use2, Household.ID, ends_with("out")), index, COP_perc, ends_with("out"))
crop_use2Long2 <- gather(select(crop_use2, Household.ID, ends_with("vend")), index, priceKG_LC, ends_with("vend"))
crop_use2Long3 <- gather(select(crop_use2, Household.ID, ends_with("cos")), index, controlsConsumption, ends_with("cos"))
crop_use2Long4 <- gather(select(crop_use2, Household.ID, ends_with("rev")), index, controlsRevenue, ends_with("rev"))
crop_use2Long$index <- gsub('[a-zA-Z]', "", crop_use2Long$index)
crop_use2Long1$index <- substr(crop_use2Long1$index, 5, 5)
crop_use2Long2$index <- substr(crop_use2Long2$index, 5, 5)
crop_use2Long3$index <- substr(crop_use2Long3$index, 5, 5)
crop_use2Long4$index <- substr(crop_use2Long4$index, 5, 5)

crop_use2Long <- left_join(crop_use2Long, crop_use2Long1)
crop_use2Long <- left_join(crop_use2Long, crop_use2Long2)
crop_use2Long <- left_join(crop_use2Long, crop_use2Long3)
crop_use2Long <- left_join(crop_use2Long, crop_use2Long4)
crop_use2Long$crop <-  substr(crop_use2Long$crop, 1, 3)
crop_use2Long$crop <- tolower(crop_use2Long$crop)
crop_use2Long$crop <- ifelse(crop_use2Long$crop %in% c("aub", "cit", "fru"), "leg", crop_use2Long$crop) # voa = a type of bean
crop_use2Long$crop <- ifelse(crop_use2Long$crop %in% c("cho", "gom", "oig", "tom", "ose", "con", "har", "poi", "pom"), "leg", crop_use2Long$crop) #cabbage, okra, onion, tomato, sorrel (spinach dock), cucumber, green bean, potato
cropDatCompleteLong <- left_join(cropDatCompleteLong, select(crop_use2Long, -index))
cropDatCompleteLong$cropType <- ifelse(cropDatCompleteLong$crop %in% c("mai", "mil", "sor", "man", "ign"), "staple", 
                                       ifelse(cropDatCompleteLong$crop %in% c("ara", "leg", "nie", "pim", "riz", "ses", "aut"), "cash", NA))

#Clean data entry errors
cropDatCompleteLong[rowSums(cropDatCompleteLong[, c("consumedPerc", "soldPerc")]) > 100 & !is.na(cropDatCompleteLong$consumedPerc), 13:14] <- c(91, 9)
#!Infer market participation from value of crops sold / total value of crop for some households
cropDatCompleteLong$soldPerc <- ifelse(is.na(cropDatCompleteLong$soldPerc) ==T & cropDatCompleteLong$priceKG_LC >0, cropDatCompleteLong$priceKG_LC / cropDatCompleteLong$valFCFA*100, cropDatCompleteLong$soldPerc)
cropDatCompleteLong$soldPerc[cropDatCompleteLong$soldPerc > 100] <- 100
cropDatCompleteLong$consumedPerc <- ifelse(is.na(cropDatCompleteLong$consumedPerc) ==T & cropDatCompleteLong$priceKG_LC >0, 100-cropDatCompleteLong$soldPerc, cropDatCompleteLong$consumedPerc)

#Convert to USD
cropDatCompleteLong$val_kg <- cropDatCompleteLong$val_kg * paramUSD
cropDatCompleteLong$valFCFA <- cropDatCompleteLong$valFCFA * paramUSD

#median prices and COP for reference and imputation
cropPriceCOP <- group_by(cropDatCompleteLong, fullName)
cropPriceCOP$consumed_percSe <- ifelse(substr(cropPriceCOP$Household.ID, 1, 2) == "SE", cropPriceCOP$consumedPerc, 0)
cropPriceCOP$consumed_percYa <- ifelse(substr(cropPriceCOP$Household.ID, 1, 2) == "YA", cropPriceCOP$consumedPerc, 0)
cropPriceCOP$sold_percSe <- ifelse(substr(cropPriceCOP$Household.ID, 1, 2) == "SE", cropPriceCOP$soldPerc, 0)
cropPriceCOP$sold_percYa <- ifelse(substr(cropPriceCOP$Household.ID, 1, 2) == "YA", cropPriceCOP$soldPerc, 0)
cropPriceCOP$COP_percSe <- ifelse(substr(cropPriceCOP$Household.ID, 1, 2) == "SE", cropPriceCOP$COP_perc, 0)
cropPriceCOP$COP_percYa <- ifelse(substr(cropPriceCOP$Household.ID, 1, 2) == "YA", cropPriceCOP$COP_perc, 0)
cropPriceCOP[cropPriceCOP == 0] <- NA
cropPriceCOP <- summarise(cropPriceCOP, price_kgM = median(val_kg, na.rm=T), COP_percM = median(COP_perc, na.rm=T), COP_percSeM = median(COP_percSe, na.rm=T), COP_percYaM = median(COP_percYa, na.rm=T), consumed_percM = median(consumedPerc, na.rm=T), consumed_percSeM = median(consumed_percSe, na.rm=T), consumed_percYaM = median(consumed_percYa, na.rm=T), sold_percSeM = median(sold_percSe, na.rm=T), sold_percYaM = median(sold_percYa, na.rm=T))

cropDatCompleteLong <- left_join(cropDatCompleteLong, cropPriceCOP)

#Household staple consumption/market participation patterns
cropHHConsCOP <- group_by(cropDatCompleteLong, Household.ID, cropType)
cropHHConsCOP$consumed_percStaple <- ifelse(cropHHConsCOP$cropType == "staple", cropHHConsCOP$consumedPerc , NA)
cropHHConsCOP$consumed_percCash <- ifelse(cropHHConsCOP$cropType == "cash", cropHHConsCOP$consumedPerc , NA)
cropHHConsCOP$sold_percStaple <- ifelse(cropHHConsCOP$cropType == "staple", cropHHConsCOP$soldPerc , NA)
cropHHConsCOP$sold_percCash <- ifelse(cropHHConsCOP$cropType == "cash", cropHHConsCOP$soldPerc , NA)
cropHHConsCOP$COP_percStaple <- ifelse(cropHHConsCOP$cropType == "staple", cropHHConsCOP$COP_perc , NA)
cropHHConsCOP$COP_percCash <- ifelse(cropHHConsCOP$cropType == "cash", cropHHConsCOP$COP_perc , NA)
cropHHConsCOP[cropHHConsCOP == 0] <- NA
cropHHConsCOP <- summarise(cropHHConsCOP, COP_percM_stapleHH = median(consumed_percStaple, na.rm=T), COP_percM_cashHH = median(consumed_percCash, na.rm=T), consumed_percM_stapleHH = median(consumed_percStaple, na.rm=T), consumed_percM_cashHH = median(consumed_percCash, na.rm=T), sold_percM_stapleHH = median(sold_percStaple, na.rm=T), sold_percM_cashHH = median(sold_percCash, na.rm=T))

cropDatCompleteLong <- left_join(cropDatCompleteLong, cropHHConsCOP)

#!Impute missing data using simple rules becoming progressively more coarse
#Fonio only produced by 2 Yatenga HHs. one has price the other all NA YATO41 and YATO20

cropDatCompleteLong$consumedPerc.i <- ifelse(is.na(cropDatCompleteLong$consumedPerc) == T, 100 , cropDatCompleteLong$consumedPerc)
                                             
cropDatCompleteLong$consumedPerc.i <- ifelse(is.na(cropDatCompleteLong$consumedPerc) == T & substr(cropDatCompleteLong$Household.ID, 1, 2) == "SE" & cropDatCompleteLong$cropType == "staple" & cropDatCompleteLong$consumed_percM_stapleHH > 50 & cropDatCompleteLong$consumed_percSeM > 50, cropDatCompleteLong$consumed_percM_stapleHH, 
                                             ifelse(is.na(cropDatCompleteLong$consumedPerc) == T & substr(cropDatCompleteLong$Household.ID, 1, 2) == "YA" & cropDatCompleteLong$cropType == "staple" & cropDatCompleteLong$consumed_percM_stapleHH > 50 & cropDatCompleteLong$consumed_percYaM > 50, cropDatCompleteLong$consumed_percM_stapleHH , 
                                                    ifelse(is.na(cropDatCompleteLong$consumedPerc) == T & substr(cropDatCompleteLong$Household.ID, 1, 2) == "SE" & cropDatCompleteLong$cropType == "cash" & cropDatCompleteLong$consumed_percM_cashHH > 50 & cropDatCompleteLong$consumed_percSeM > 50, cropDatCompleteLong$consumed_percM_cashHH, 
                                                           ifelse(is.na(cropDatCompleteLong$consumedPerc) == T & substr(cropDatCompleteLong$Household.ID, 1, 2) == "YA" & cropDatCompleteLong$cropType == "cash" & cropDatCompleteLong$consumed_percM_cashHH > 50 & cropDatCompleteLong$consumed_percYaM > 50, cropDatCompleteLong$consumed_percM_cashHH, cropDatCompleteLong$consumedPerc))))

cropDatCompleteLong$consumedPerc.i <- ifelse(is.na(cropDatCompleteLong$consumedPerc.i) == T & substr(cropDatCompleteLong$Household.ID, 1, 2) == "SE" & cropDatCompleteLong$cropType == "staple", 100 - cropDatCompleteLong$sold_percM_stapleHH, 
                                             ifelse(is.na(cropDatCompleteLong$consumedPerc.i) == T & substr(cropDatCompleteLong$Household.ID, 1, 2) == "YA" & cropDatCompleteLong$cropType == "staple", 100 -  cropDatCompleteLong$sold_percM_stapleHH, cropDatCompleteLong$consumedPerc.i))

cropDatCompleteLong$consumedPerc.i <- ifelse(is.na(cropDatCompleteLong$consumedPerc.i) == T & substr(cropDatCompleteLong$Household.ID, 1, 2) == "SE" & cropDatCompleteLong$cropType == "cash", 100 - cropDatCompleteLong$sold_percM_cashHH, 
                                             ifelse(is.na(cropDatCompleteLong$consumedPerc.i) == T & substr(cropDatCompleteLong$Household.ID, 1, 2) == "YA" & cropDatCompleteLong$cropType == "cash", 100 -  cropDatCompleteLong$sold_percM_cashHH, cropDatCompleteLong$consumedPerc.i))

cropDatCompleteLong$consumedPerc.i <- ifelse(is.na(cropDatCompleteLong$consumedPerc.i) == T & substr(cropDatCompleteLong$Household.ID, 1, 2) == "SE" & cropDatCompleteLong$cropType == "cash", cropDatCompleteLong$consumed_percM_cashHH, 
                                             ifelse(is.na(cropDatCompleteLong$consumedPerc.i) == T & substr(cropDatCompleteLong$Household.ID, 1, 2) == "YA" & cropDatCompleteLong$cropType == "cash", cropDatCompleteLong$consumed_percM_cashHH, cropDatCompleteLong$consumedPerc.i))

cropDatCompleteLong$consumedPerc.i <- ifelse(is.na(cropDatCompleteLong$consumedPerc.i) == T & substr(cropDatCompleteLong$Household.ID, 1, 2) == "SE", cropDatCompleteLong$consumed_percSeM,
                                             ifelse(is.na(cropDatCompleteLong$consumedPerc.i) == T & substr(cropDatCompleteLong$Household.ID, 1, 2) == "YA", cropDatCompleteLong$consumed_percYaM, cropDatCompleteLong$consumedPerc.i))

cropDatCompleteLong$consumedPerc.i <- ifelse(is.na(cropDatCompleteLong$consumedPerc.i) == T & substr(cropDatCompleteLong$Household.ID, 1, 2) == "SE", 100 - cropDatCompleteLong$sold_percSeM, cropDatCompleteLong$consumedPerc.i)

cropDatCompleteLong$consumedPerc.i <- ifelse(is.na(cropDatCompleteLong$consumedPerc.i) == T , cropDatCompleteLong$consumed_percM, cropDatCompleteLong$consumedPerc.i)

cropDatCompleteLong$soldPerc.i <- ifelse(is.na(cropDatCompleteLong$soldPerc) == T, 100 - cropDatCompleteLong$consumedPerc.i, cropDatCompleteLong$soldPerc)

#!Impute COP - by crop COP (by site) and then by HH staple/cash summary. Fianlly by crop type (staple vs cash) 988 values to impute out of 1579
cropDatCompleteLong$COP_perc.i <- ifelse(is.na(cropDatCompleteLong$COP_perc) == T & substr(cropDatCompleteLong$Household.ID, 1, 2) == "SE", cropDatCompleteLong$COP_percSeM,
                                             ifelse(is.na(cropDatCompleteLong$COP_perc) == T & substr(cropDatCompleteLong$Household.ID, 1, 2) == "YA", cropDatCompleteLong$COP_percYaM, cropDatCompleteLong$COP_perc))
#still 264 values to impute
cropDatCompleteLong$COP_perc.i <- ifelse(is.na(cropDatCompleteLong$COP_perc.i) == T & cropDatCompleteLong$cropType == "staple", cropDatCompleteLong$COP_percM_stapleHH,
                                         ifelse(is.na(cropDatCompleteLong$COP_perc.i) == T & cropDatCompleteLong$cropType == "cash", cropDatCompleteLong$COP_percM_cashHH, cropDatCompleteLong$COP_perc.i))
#16 values to impute
cropDatCompleteLong$COP_perc.i <- ifelse(is.na(cropDatCompleteLong$COP_perc.i) == T & cropDatCompleteLong$cropType == "staple", median(cropDatCompleteLong$COP_perc[cropDatCompleteLong$cropType == "staple"], na.rm=T),
                                         ifelse(is.na(cropDatCompleteLong$COP_perc.i) == T & cropDatCompleteLong$cropType == "cash", median(cropDatCompleteLong$COP_perc[cropDatCompleteLong$cropType == "cash"], na.rm=T), cropDatCompleteLong$COP_perc.i))

#! COP is assumed to be minimal for households with 0 COP
cropDatCompleteLong$COP_perc.i[cropDatCompleteLong$COP_perc.i == 0] <- 1

#Still a few cases where cons and sold are 0
cropDatCompleteLong$cropIncome <- (cropDatCompleteLong$soldPerc.i/100) * cropDatCompleteLong$valFCFA
cropDatCompleteLong$cropIncomeCash <- ifelse(cropDatCompleteLong$cropType == "cash", (cropDatCompleteLong$soldPerc.i/100) * cropDatCompleteLong$valFCFA, NA)
cropDatCompleteLong$COP_value <- (cropDatCompleteLong$COP_perc/100) * cropDatCompleteLong$cropIncome #Only considered in relation to income
cropDatCompleteLong$cons_kcal <- (cropDatCompleteLong$consumedPerc.i/100) * cropDatCompleteLong$totalfarm_producekcal
cropDatCompleteLong$cons_protein <- (cropDatCompleteLong$consumedPerc.i/100) * cropDatCompleteLong$totalfarm_produceprotein
cropDatCompleteLong$sold_kcal <- (cropDatCompleteLong$soldPerc.i/100) * cropDatCompleteLong$totalfarm_producekcal
cropDatCompleteLong$femaleValControl <- ifelse(cropDatCompleteLong$controlsRevenue %in% c(2,4), cropDatCompleteLong$valFCFA, NA)
cropDatCompleteLong$irriStaples <- ifelse(cropDatCompleteLong$cropType == "staple" & cropDatCompleteLong$irri >0, 1, 0)
cropDatCompleteLong$irriCash <- ifelse(cropDatCompleteLong$cropType == "cash" & cropDatCompleteLong$irri >0, 1, 0)
cropDatCompleteLong$fertStaples <- ifelse(cropDatCompleteLong$cropType == "staple" & cropDatCompleteLong$fert >0, 1, 0)
cropDatCompleteLong$fertCash <- ifelse(cropDatCompleteLong$cropType == "cash" & cropDatCompleteLong$fert >0, 1, 0)
#Irrigation is almost identical for 'good' and 'bad' season 3 of the ~89 crops differed.Similar for fert
cropDatCompleteLong$goodScropIncome <- (cropDatCompleteLong$soldPerc.i/100) * cropDatCompleteLong$goodVal
cropDatCompleteLong$goodSsold_kcal <- (cropDatCompleteLong$soldPerc.i/100) * cropDatCompleteLong$goodYieldKg * cropDatCompleteLong$kcal
cropDatCompleteLong$badScropIncome <- (cropDatCompleteLong$soldPerc.i/100) * cropDatCompleteLong$badVal
cropDatCompleteLong$badSsold_kcal <- (cropDatCompleteLong$soldPerc.i/100) * cropDatCompleteLong$badYieldKg * cropDatCompleteLong$kcal
cropDatCompleteLong$FAOcode[substr(cropDatCompleteLong$FAOcode, 1, 2) == "03"] <- "06_010"
cropDatCompleteLong$sorghumYield <- ifelse(cropDatCompleteLong$fullName == "Sorghum", cropDatCompleteLong$yieldKG_ha, NA)
cropDatCompleteLong$milletYield <- ifelse(cropDatCompleteLong$fullName == "Millet", cropDatCompleteLong$yieldKG_ha, NA)
cropDatCompleteLong$maizeYield <- ifelse(cropDatCompleteLong$fullName == "Maize", cropDatCompleteLong$yieldKG_ha, NA)
#@Add crop diversity and HDDS diversity by season

datCrop <- group_by(cropDatCompleteLong, Household.ID)
datCrop <- datCrop %>% summarise(cropProductionKg = sum(yieldKG, na.rm=T), cropTotalValue_US = sum(valFCFA, na.rm=T),
                                 cropFemaleControlVal = sum(femaleValControl, na.rm=T), cropTotalKcal = sum(totalfarm_producekcal, na.rm=T), 
                                 cropIncome_US = sum(cropIncome, na.rm=T), cropCOPvalue_US = sum(COP_value, na.rm=T), 
                                 cropConsKcal = sum(cons_kcal, na.rm=T), cropSoldKcal = sum(sold_kcal, na.rm=T),
                                 cropConsProtein = sum(cons_protein, na.rm=T),
                                 irriStaples = max(irriStaples, na.rm=T), irriCash = max(irriCash, na.rm=T),
                                 fertStaples = max(fertStaples, na.rm=T), fertCash = max(fertCash, na.rm=T),
                                 cropDiv = length(crop), cropHDDSDiv = length(unique(substr(FAOcode, 1, 2))), goodSeasonCropIncome_US = sum(goodScropIncome, na.rm=T),
                                 goodSeasonSoldKcal = sum(goodSsold_kcal, na.rm=T), badSeasonCropIncome_US = sum(badScropIncome, na.rm=T),
                                 badSeasonSoldKcal = sum(badSsold_kcal, na.rm=T), sorghumYield = mean(sorghumYield, na.rm=T),
                                 milletYield = mean(milletYield, na.rm=T), maizeYield = mean(maizeYield, na.rm=T))

datCrop$cropSoldVal_perc <- datCrop$cropIncome_US / datCrop$cropTotalValue_US
datCrop$cropCOPval_perc <- datCrop$cropCOPvalue_US / datCrop$cropTotalValue_US
datCrop$cropFemaleControlVal_perc <- datCrop$cropFemaleControlVal / datCrop$cropTotalValue_US
datCrop$cropsoldKcal_perc <- datCrop$cropSoldKcal / datCrop$cropTotalKcal
datCrop$cropConsKcal_perc <- datCrop$cropConsKcal / datCrop$cropTotalKcal
datCrop$badSeasonCropIncome_perc <- datCrop$badSeasonCropIncome_US / datCrop$cropIncome_US


#rm(list= setdiff(ls(), c("dat", "datAdulteq", "datIncome", "datLand", "datLvstTLU", "datLvstLive", "datLvstProd", "datCrop")))
