library(dplyr)
library(ggplot2)
source('ImpLite_Lushoto/SF_ImpLite_DietDiversity.R', chdir=T)
source('Rhomis_Lushoto/Comparison_HDDS_SF.R', chdir=T)
HDDSb <- subset(HDDS, household.id..see.mainsurveyinfo. %in% HDDS2015$SECTION_META.householdID)
HDDSb$foodGroup <- gsub("fruit", "Fruits", HDDSb$foodGroup)
HDDSb$foodGroup <- gsub("rootTuber", "Roots_tubers", HDDSb$foodGroup)
HDDSb$foodGroup <- gsub("pulseLegume", "Pulse_legumes", HDDSb$foodGroup)
HDDSb$foodGroup <- gsub("vegetable", "Vegetables", HDDSb$foodGroup)
HDDSb$foodGroup <- gsub("milk", "Milk_Dairy", HDDSb$foodGroup)
HDDSb$foodGroup <- gsub("cereals", "Cereals", HDDSb$foodGroup)
HDDSb$foodGroup <- gsub("fish", "Fish", HDDSb$foodGroup)
HDDSb$foodGroup <- gsub("sugar", "Sugar", HDDSb$foodGroup)
HDDSb$Year <- "2012"
HDDSb <- ungroup(HDDSb)
HDDSbgood <- subset(HDDSb, foodgroupsGood == 1)

HDDS2015goodLong$foodGroup <- gsub("Legumes", "Pulse_legumes", HDDS2015goodLong$foodGroup)
HDDS2015goodLong$foodGroup <- gsub("Grains", "Cereals", HDDS2015goodLong$foodGroup)
HDDS2015goodLong$foodGroup <- gsub("Sweets", "Sugar", HDDS2015goodLong$foodGroup)
HDDS2015goodLong$Year <- "2015"
HDDS2015goodLong <- subset(HDDS2015goodLong, householdID %in% HDDSb$household.id..see.mainsurveyinfo.)


HDDS2012_2015good <- rbind(select(HDDSbgood, foodGroup, Year), select(HDDS2015goodLong, foodGroup, Year))
HDDS2012_2015good$foodGroup <- gsub("Pulse_legumes", "Pulse/legumes", HDDS2012_2015good$foodGroup)
HDDS2012_2015good$foodGroup <- gsub("Milk_Dairy", "Dairy", HDDS2012_2015good$foodGroup)
HDDS2012_2015good$foodGroup <- gsub("Roots_tubers", "Roots/tubers", HDDS2012_2015good$foodGroup)
HDDS2012_2015good$foodGroup <- factor(HDDS2012_2015good$foodGroup, levels = c("Cereals", "Dairy", "Eggs", "Fats", "Fish", "Fruits", "Meat", "Pulse/legumes", "Roots/tubers", "Sugar", "Vegetables", "Other"))

#ggplot(HDDS2012_2015good, aes(x=foodGroup)) + geom_bar() +theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11), axis.text.y = element_text(size=12)) + coord_cartesian(ylim = c(0, 151)) + facet_grid(". ~ Year") + labs(title="Consumption by category - good season", x = "Food group", y = "Count of households consuming")

HDDSbbad <- subset(HDDSb, foodgroupsBad == 1)

HDDS2015badLong$foodGroup <- gsub("Legumes", "Pulse_legumes", HDDS2015badLong$foodGroup)
HDDS2015badLong$foodGroup <- gsub("Grains", "Cereals", HDDS2015badLong$foodGroup)
HDDS2015badLong$foodGroup <- gsub("Sweets", "Sugar", HDDS2015badLong$foodGroup)
HDDS2015badLong$Year <- "2015"
HDDS2015badLong <- subset(HDDS2015badLong, householdID %in% HDDSb$household.id..see.mainsurveyinfo.)

HDDS2012_2015bad <- rbind(select(HDDSbbad, foodGroup, Year), select(HDDS2015badLong, foodGroup, Year))
HDDS2012_2015bad$foodGroup <- gsub("Pulse_legumes", "Pulse/legumes", HDDS2012_2015bad$foodGroup)
HDDS2012_2015bad$foodGroup <- gsub("Milk_Dairy", "Dairy", HDDS2012_2015bad$foodGroup)
HDDS2012_2015bad$foodGroup <- gsub("Roots_tubers", "Roots/tubers", HDDS2012_2015bad$foodGroup)
HDDS2012_2015bad$foodGroup <- factor(HDDS2012_2015bad$foodGroup, levels = c("Cereals", "Dairy", "Eggs", "Fats", "Fish", "Fruits", "Meat", "Pulse/legumes", "Roots/tubers", "Sugar", "Vegetables", "Other"))
#ggplot(HDDS2012_2015bad, aes(x=foodGroup)) + geom_bar() + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11), axis.text.y = element_text(size=12)) + coord_cartesian(ylim = c(0, 151))  + facet_grid(". ~ Year") + labs(title="Consumption by category -  bad season", x = "Food group", y = "Count of households consuming")

