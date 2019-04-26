library(dplyr)

datTmp <- read.csv('Outputs/DFs/dat.csv', stringsAsFactors = F)
#datTmp$HouseholdID <- paste0(datTmp$HouseholdID, datTmp$country, datTmp$sublocation)
datProdConsNutrition <- read.csv('Outputs/DFs/datProdConsNutrition.csv', stringsAsFactors = F)
datProducedNutrition <- read.csv('Outputs/DFs/datProducedNutrition.csv', stringsAsFactors = F)

if(weekly_dailyDD != "daily"){
HDDSstackDaily_long <- read.csv('Outputs/DFs/HDDSstack_long.csv', stringsAsFactors = F)
} else{HDDSstackDaily_long <- read.csv('Outputs/DFs/HDDSstackDaily_long.csv', stringsAsFactors = F)}
#datTmp$country <- gsub("burkina$", "burkina faso", datTmp$country)
#datTmp$country <- gsub("eth$", "ethiopia", datTmp$country)
#datTmp$country <- gsub("tnz", "tanzania", datTmp$country)
HDDSstackDaily_long$country <- gsub("burkina$", "burkina faso", HDDSstackDaily_long$country)
HDDSstackDaily_long$country <- gsub("eth$", "ethiopia", HDDSstackDaily_long$country)
HDDSstackDaily_long$country <- gsub("tnz", "tanzania", HDDSstackDaily_long$country)
HDDSstackDaily_long$country <- tolower(HDDSstackDaily_long$country)
HDDSstackDaily_long$region <-  ifelse(HDDSstackDaily_long$country %in% c("burkina faso", "drc", "ghana", "mali"), "westAfrica", "eastAfrica")

outlierObs <- unique(c(as.character(datTmp$HouseholdID[datTmp$landcultivated > 100]),
                       as.character(datTmp$HouseholdID[datTmp$tlu > 250]),
                       as.character(datTmp$HouseholdID[datTmp$adultEq > 30]),
                       as.character(datTmp$HouseholdID[datTmp$quality_reliability %in% c(1)]),
                       as.character(datTmp$HouseholdID[datTmp$HDDSgood == 0]),
                       as.character(datTmp$HouseholdID[datTmp$HDDSbad == 0]),
                       as.character(datTmp$HouseholdID[datTmp$adultEq == 0]),
                       as.character(datTmp$HouseholdID[datTmp$projectname == "TreeAID" & datTmp$country == "ghana"])))

datTmp <- datTmp[!(datTmp$HouseholdID %in% outlierObs),]
datProdConsNutrition <- datProdConsNutrition[!(datProdConsNutrition$HouseholdID %in% outlierObs),]
HDDSstackDaily_long <- HDDSstackDaily_long[!(HDDSstackDaily_long$HouseholdID %in% outlierObs),]

#@Obs with NA HDDScat are non-food products. i.e. table(datProducedNutrition$product[is.na(datProducedNutrition$HDDScat)])
#@Obs with no HDDScat are items with no matching HDDS category or are uncertain i.e. table(datProducedNutrition$product[datProducedNutrition$HDDScat == ""])

#datProducedNutrition$HDDScat <- gsub("vegVitA", "vegvita", datProducedNutrition$HDDScat)
#datProducedNutrition$HDDScat <- gsub("vegLeafy", "vegLeafy", datProducedNutrition$HDDScat)
#datProducedNutrition$HDDScat <- gsub("vegO", "vegOther", datProducedNutrition$HDDScat)
datProducedNutrition$HDDScat <- tolower(datProducedNutrition$HDDScat)

datProducedNutrition <- left_join(datProducedNutrition, select(datTmp, HouseholdID, country))
datProducedNutrition$region <-  ifelse(datProducedNutrition$country %in% c("burkina faso", "drc", "ghana", "mali"), "westAfrica", "eastAfrica")

datProducedNutrition <- group_by(datProducedNutrition, country, HDDScat, var)
HDDSnutritionCountry <- summarise(subset(datProducedNutrition, proportion =="total"), wMeanVal = weighted.mean(val, mass, na.rm=T))
HDDSnutritionCountry <- HDDSnutritionCountry[!(HDDSnutritionCountry$HDDScat %in% c("", NA)), ]

datProducedNutrition <- group_by(datProducedNutrition, region, HDDScat, var)
HDDSnutritionRegion <- summarise(subset(datProducedNutrition, proportion =="total"), wMeanVal = weighted.mean(val, mass, na.rm=T))
HDDSnutritionRegion <- HDDSnutritionRegion[!(HDDSnutritionRegion$HDDScat %in% c("", NA)), ]

datProducedNutrition <- group_by(datProducedNutrition, HDDScat, var)
HDDSnutritionAll <- summarise(subset(datProducedNutrition, proportion =="total"), wMeanVal = weighted.mean(val, mass, na.rm=T))
HDDSnutritionAll <- HDDSnutritionAll[!(HDDSnutritionAll$HDDScat %in% c("", NA)), ]

#Define micronutrient a source or rich based on FAO standards - 15% of DRV or 45% of DRV
DRVlookup <- read.csv('DataParams/nutrientIntakeRequirementsFAO.csv')
DRVlookupAdultM <- subset(DRVlookup, class == "males25to50") #& nutrient %in% datProducedNutrition

richSourceMicroCountry <- left_join(HDDSnutritionCountry, select(DRVlookupAdultM, var = nutrient, requirement))
richSourceMicroCountry$perc <- richSourceMicroCountry$wMeanVal / richSourceMicroCountry$requirement
richSourceMicroCountry$source <- ifelse(richSourceMicroCountry$perc > 0.145 & richSourceMicroCountry$perc < 0.45, 1, 
                                 ifelse(richSourceMicroCountry$perc >0.44, 2, 0))
#richSourceMicroCountry$source[richSourceMicroCountry$HDDScat == "vegvita" & richSourceMicroCountry$var == "VITA"] <- 1 #Assume vitamin A rich veg is a source of vitamin A
#richSourceMicroCountry$source[richSourceMicroCountry$HDDScat == "milk" & richSourceMicroCountry$var == "CA"] <- 1 #Assume vitamin A rich veg is a source of vitamin A


richSourceMicroRegion <- left_join(HDDSnutritionRegion, select(DRVlookupAdultM, var = nutrient, requirement))
richSourceMicroRegion$perc <- richSourceMicroRegion$wMeanVal / richSourceMicroRegion$requirement
richSourceMicroRegion$source <- ifelse(richSourceMicroRegion$perc > 0.145 & richSourceMicroRegion$perc < 0.45, 1, 
                                 ifelse(richSourceMicroRegion$perc >0.44, 2, 0))
richSourceMicroRegion$source[richSourceMicroRegion$HDDScat == "milk" & richSourceMicroRegion$var == "CA"] <- 1 #Assume dairy is a source of calcium
richSourceMicroCountry$source[richSourceMicroCountry$HDDScat == "milk" & richSourceMicroCountry$var == "CA"] <- 1 


#richSourceMicroRegion$source[richSourceMicroRegion$HDDScat == "vegvita" & richSourceMicroRegion$var == "VITA"] <- 1 #Assume vitamin A rich veg is a source of vitamin A
#richSourceMicroCountry$source[richSourceMicroCountry$HDDScat == "vegvita" & richSourceMicroCountry$var == "VITA"] <- 1 #Assume vitamin A rich veg is a source of vitamin A

# richSourceMicroAll <- left_join(HDDSnutritionAll, select(DRVlookupAdultM, var = nutrient, requirement))
# richSourceMicroAll$perc <- richSourceMicroAll$wMeanVal / richSourceMicroAll$requirement
# richSourceMicroAll$source <- ifelse(richSourceMicroAll$perc > 0.15 & richSourceMicroAll$perc < 0.45, 1, 
#                                        ifelse(richSourceMicroAll$perc >0.44, 2, 0))


HDDSstackDaily_long$HDDScat <- tolower(HDDSstackDaily_long$Category)
HDDSstackDaily_long$HDDScat <- gsub("vega", "vegvita", HDDSstackDaily_long$HDDScat)
HDDSstackDaily_long$HDDScat <- gsub("vegl", "vegleafy", HDDSstackDaily_long$HDDScat)
HDDSstackDaily_long$HDDScat <- gsub("vego", "vegother", HDDSstackDaily_long$HDDScat)

HDDSstackDaily_longTmp <- left_join(HDDSstackDaily_long, select(richSourceMicroRegion, region, HDDScat, var, dietarySource = source))
HDDSstackDaily_longTmp <- left_join(HDDSstackDaily_longTmp, select(richSourceMicroCountry, country, HDDScat, var, dietarySourceCountry = source))

HDDSstackDaily_longTmp$dietarySource <- ifelse(!is.na(HDDSstackDaily_longTmp$dietarySourceCountry), HDDSstackDaily_longTmp$dietarySourceCountry, HDDSstackDaily_longTmp$dietarySource)



HDDSstackDaily_longTmp <- subset(HDDSstackDaily_longTmp, !(var %in% c("ENERGY_KC", "PROCNT", "A_PROTEI", "CHOCDF", "FASAT", "FAMS", "FAPU", "FAT", "FIB", "SUCS", "PHYTAC", "A_VITA", "MFP_PROT", "CHOLE", "MFP_FE", "NA.")))

HDDSstackDaily_longTmp$dietarySource[is.na(HDDSstackDaily_longTmp$dietarySource)] <-0
HDDSstackDaily_longTmp <- subset(HDDSstackDaily_longTmp, val ==1) #Only include obs where they do indeed source. val = 1
HDDSstackDaily_longTmp <- group_by(HDDSstackDaily_longTmp, HouseholdID, var)
summaryHDDSnutritionGood <- summarise(subset(HDDSstackDaily_longTmp, Season == "good"), sourceRichGood = max(dietarySource, na.rm=T))
summaryHDDSnutritionLean <- summarise(subset(HDDSstackDaily_longTmp, Season == "bad"), sourceRichLean = max(dietarySource, na.rm=T))


datProdConsNutrition <- left_join(datProdConsNutrition, select(summaryHDDSnutritionGood, HouseholdID, nutrient = var, sourceRichGood))
datProdConsNutrition <- left_join(datProdConsNutrition, select(summaryHDDSnutritionLean, HouseholdID, nutrient = var, sourceRichLean))

datProdConsNutrition$sourceRichGood <- gsub("2", "1", datProdConsNutrition$sourceRichGood)
datProdConsNutrition$sourceRichLean <- gsub("2", "1", datProdConsNutrition$sourceRichLean)

datProdConsNutrition$sourceRichGood[is.na(datProdConsNutrition$sourceRichGood)]<- 0
datProdConsNutrition$sourceRichLean[is.na(datProdConsNutrition$sourceRichLean)]<- 0
datProdConsNutrition$sourceAllYear <- paste0(datProdConsNutrition$sourceRichGood, datProdConsNutrition$sourceRichLean)
datProdConsNutrition$sourceAllYear[datProdConsNutrition$sourceAllYear %in% c("01", "10")] <- "Partial year"
datProdConsNutrition$sourceAllYear[datProdConsNutrition$sourceAllYear == "11"] <- "Full year"
datProdConsNutrition$sourceAllYear[datProdConsNutrition$sourceAllYear == "00"] <- "No source"

datProdConsNutrition$sourceAllYear <- factor(datProdConsNutrition$sourceAllYear, levels = c("Full year", "Partial year", "No source"))


datProdConsNutrition <- left_join(datProdConsNutrition, select(datTmp, HouseholdID, HFIAS, HFIAP, numFoodShortMonth))

datProdConsNutrition$HFIAPallYear <- ifelse(datProdConsNutrition$HFIAP %in% c("Food secure", "Mildly food insecure") & datProdConsNutrition$numFoodShortMonth %in% c(0), "Secure all year", "insecure")
datProdConsNutritionSub <- subset(datProdConsNutrition, !is.na(HFIAPallYear))

#ggplot(subset(datProdConsNutrition, farmSourceDependentGood == 1 & nutrient %in% c("CA", "FE", "MG", "P", "VIT.B12", "VIT.B6", "VITA", "VITE", "ZN")), aes(as.factor(sourceAllYear), nutritionSufficPerc)) + geom_boxplot() + facet_wrap(~nutrient) + xlab("Availability of 'source' in MDD-W") + ylab("Sufficincy of farm produced") + theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12))
#ggplot(subset(datProdConsNutrition, farmSourceDependentGood == 1 & nutrient %in% c("VITA")), aes(as.factor(sourceAllYear), nutritionSufficPerc)) + geom_boxplot() + facet_wrap(~HFIAP)
#ggplot(subset(datProdConsNutritionSub, farmSourceDependentGood == 1 & nutrient == "ENERGY_KC"), aes(as.factor(HFIAP), nutritionSufficPerc)) + geom_boxplot() + facet_wrap(~numFoodShortMonth) + xlab("Food security status") + ylab("Energy sufficiency (kcal)") + theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12))
#ggplot(subset(datProdConsNutrition, farmSourceDependentGood == 1 & nutrient == "ENERGY_KC"), aes(as.factor(HFIAPallYear), nutritionSufficPerc)) + geom_boxplot() + xlab("Food security status") + ylab("Energy sufficiency (kcal)") + theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12))

#table(datProdConsNutrition$sourceAllYear, datProdConsNutrition$nutrient)



subDatNutrition <- subset(datProdConsNutrition, nutrient %in% c("VITA",	"VITE",	"VITC",	"THIA",	"RIBF",	"NIA",	"VIT.B6",	"FOL",	"VIT.B12", "PANT", "CA",	"P",	"MG",	"K",	"FE",	"ZN",	"CU",	"MN"))

#subDatNutrition <- subset(datProdConsNutrition, farmSourceDependentGood == 1 & nutrient %in% c("VITA",	"VITE",	"VITC",	"THIA",	"RIBF",	"NIA",	"VIT.B6",	"FOL",	"VIT.B12", "PANT", "CA",	"P",	"MG",	"K",	"FE",	"ZN",	"CU",	"MN"))


#table(subDatNutrition$sourceAllYear, subDatNutrition$nutrient)
#prop.table(table(subDatNutrition$sourceAllYear, subDatNutrition$nutrient, subDatNutrition$farmType), c(3,2))
#prop.table(table(datProdConsNutrition$HFIAPallYear[datProdConsNutrition$nutrient == "ENERGY_KC"], datProdConsNutrition$nutrient[datProdConsNutrition$nutrient == "ENERGY_KC"], datProdConsNutrition$farmType[datProdConsNutrition$nutrient == "ENERGY_KC"]), c(3,2))

microNplot <- left_join(count(subDatNutrition, sourceAllYear, nutrient, farmType), count(subDatNutrition, nutrient, farmType), by = c("nutrient", "farmType"))
microNplot$prop <- microNplot$n.x / microNplot$n.y
#ggplot(microNplot, aes(prop, nutrient)) + geom_point(aes(shape = sourceAllYear, colour = sourceAllYear)) + facet_wrap(~ farmType)
#ggplot(subset(microNplot, sourceAllYear == "Full year"), aes(prop, farmType)) + geom_point(aes(shape = nutrient, colour = nutrient)) 



###Just HFIAP
datProdConsNutrition$HFIAP <- gsub("Mildly food insecure", "Food secure", datProdConsNutrition$HFIAP)
datProdConsNutrition$HFIAP <- gsub("Moderately food insecure", "Food secure", datProdConsNutrition$HFIAP)
macroNplot <- left_join(count(datProdConsNutrition[datProdConsNutrition$nutrient %in% c("ENERGY_KC", "PROCNT"),], HFIAP, nutrient, farmType), count(datProdConsNutrition[datProdConsNutrition$nutrient %in% c("ENERGY_KC", "PROCNT"),], nutrient, farmType), by = c("nutrient", "farmType"))
macroNplot$prop <- macroNplot$n.x / macroNplot$n.y
colnames(microNplot)[1] <- "avail"
colnames(macroNplot)[1] <- "avail"

allnutrientNplot <- rbind(microNplot, macroNplot)
#allnutrientNplot$farmType <- factor(allnutrientNplot$farmType, levels = c("Cropping", "Horticulture", "Diverse cultivation", "Cropping & livestock", "Diverse cultivation & livestock", "Dairy"), ordered = T)
#allnutrientNplot$nutrient <- factor(allnutrientNplot$nutrient, levels = c("ENERGY_KC", "CA", "FE", "MG", "P", "ZN", "VIT.B6), ordered = T)

#allnutrientNplotSubsist <- rbind(microNplot, macroNplot)
#write.csv(allnutrientNplotSubsist, 'Outputs/DFs/allnutrientNplotSubsist.csv')


