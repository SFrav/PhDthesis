######################################################
# Script to create comparison dataset for LSMS
# and RHoMIS - 2012 and 2015
# R version 3.2.5.
# 1/2/2017
# Aurthors: Simon Fraval, Mark van Wijk and Jim Hammond
######################################################

############ Uganda ################




#Recalculate FA and TLU?

library(dplyr)

comp_LSMS_Uganda_Base <- read.csv('LSMS_Uganda/LSMS_200910_Baseline_merged_281016.csv')
comp_LSMS_Uganda_End <- read.csv('LSMS_Uganda/LSMS_201112_Baseline_merged_281016.csv')
#compHHheadChange <- read.csv('LSMS_Uganda/hh_head_changes_2009_2011_180828 (1).csv')

head2009 <- read.csv('LSMS_Uganda/hh_head_2009_180829.csv')
head2011 <- read.csv('LSMS_Uganda/hh_head_2011_180829.csv')
compHHheadChange <- left_join(select(head2009, HHID, PID_2009 = PID, gender09, age09), select(head2011, HHID, PID_2011 = PID, gender11, age11))

compHHheadChange$headGenderChange <- ifelse(compHHheadChange$PID_2009 != compHHheadChange$PID_2011, 1, 0)
compHHheadChange$headGenderChange <- ifelse(compHHheadChange$headGenderChange == 1 & tolower(compHHheadChange$gender09) =="male" & tolower(compHHheadChange$gender11) =="male"  & abs(compHHheadChange$age09 - compHHheadChange$age11) < 5, 0, compHHheadChange$headGenderChange)

#table(paste(compHHheadChange$gender09[compHHheadChange$headGenderChange ==1 & abs(compHHheadChange$age09 - compHHheadChange$age11) < 5], compHHheadChange$gender11[compHHheadChange$headGenderChange ==1 & abs(compHHheadChange$age09 - compHHheadChange$age11) < 5]))


##Alternative approach to calculating adult equivalent
# adEqCoef <- c(rep(0.5, 6), 0.8, 1, 1, 0.73, rep(0.5, 6), 0.75, 0.8, 0.8, 0.6)  
# 
# comp_LSMS_Uganda_Base$adult_eq2012 <- rowSums(t(apply(comp_LSMS_Uganda_Base[,10:29], 1, "*", adEqCoef)), na.rm=T)
# comp_LSMS_Uganda_End$adult_eq2015 <- rowSums(t(apply(comp_LSMS_Uganda_End[,10:29], 1, "*", adEqCoef)), na.rm=T)

colnames(comp_LSMS_Uganda_Base)[grep("hhh_age", colnames(comp_LSMS_Uganda_Base))] <- "HHheadAge2012"
colnames(comp_LSMS_Uganda_End)[grep("hhh_age", colnames(comp_LSMS_Uganda_End))] <- "HHheadAge2015"
colnames(comp_LSMS_Uganda_Base)[grep("hhh_sex", colnames(comp_LSMS_Uganda_Base))] <- "HHheadGender2012"
colnames(comp_LSMS_Uganda_End)[grep("hhh_sex", colnames(comp_LSMS_Uganda_End))] <- "HHheadGender2015"
colnames(comp_LSMS_Uganda_Base)[grep("tlu", colnames(comp_LSMS_Uganda_Base))] <- "tlu2012"
colnames(comp_LSMS_Uganda_End)[grep("tlu", colnames(comp_LSMS_Uganda_End))] <- "tlu2015"
colnames(comp_LSMS_Uganda_Base)[grep("income_offfarm", colnames(comp_LSMS_Uganda_Base))] <- "offIncome2012"
colnames(comp_LSMS_Uganda_End)[grep("income_offfarm", colnames(comp_LSMS_Uganda_End))] <- "offIncome2015"
colnames(comp_LSMS_Uganda_Base)[grep("Adult_Equivalent", colnames(comp_LSMS_Uganda_Base))] <- "adult_eq2012"
colnames(comp_LSMS_Uganda_End)[grep("Adult_Equivalent", colnames(comp_LSMS_Uganda_End))] <- "adult_eq2015"
colnames(comp_LSMS_Uganda_Base)[grep("tot_energy", colnames(comp_LSMS_Uganda_Base))] <- "FA2012"
colnames(comp_LSMS_Uganda_End)[grep("tot_energy", colnames(comp_LSMS_Uganda_End))] <- "FA2015"
comp_LSMS_Uganda_Base$landOwnedHa2012 <- comp_LSMS_Uganda_Base$area.ac * 0.404686
comp_LSMS_Uganda_End$landOwnedHa2015 <- comp_LSMS_Uganda_End$area.ac * 0.404686
comp_LSMS_Uganda_Base$FSS2012 <- rowSums(comp_LSMS_Uganda_Base[, c("energy_crop_cons", "energy_lvst_cons")], na.rm=T) / comp_LSMS_Uganda_Base$HH_energy_need
comp_LSMS_Uganda_End$FSS2015 <- rowSums(comp_LSMS_Uganda_End[, c("energy_crop_cons", "energy_lvst_cons")], na.rm=T) / comp_LSMS_Uganda_End$HH_energy_need



comp_LSMS_Uganda <- inner_join(select(comp_LSMS_Uganda_Base, hhid, Country = country, HHheadAge2012, HHheadGender2012, tlu2012, offIncome2012, adult_eq2012, FA2012, landOwnedHa2012, FSS2012), 
                              select(comp_LSMS_Uganda_End, hhid, Country = country, HHheadAge2015, HHheadGender2015, tlu2015, offIncome2015, adult_eq2015, FA2015, landOwnedHa2015, FSS2015))

comp_LSMS_Uganda <- left_join(comp_LSMS_Uganda, select(compHHheadChange, hhid = HHID, headGenderChange))

comp_LSMS_Uganda$FA2012 <- (comp_LSMS_Uganda$FA2012/ (365*comp_LSMS_Uganda$adult_eq2012))
comp_LSMS_Uganda$FA2015 <- (comp_LSMS_Uganda$FA2015/ (365*comp_LSMS_Uganda$adult_eq2015))

#comp_LSMS_Uganda$headGenderChange <- ifelse(comp_LSMS_Uganda$headGenderChange == 0 & comp_LSMS_Uganda$HHheadGender2012 != comp_LSMS_Uganda$HHheadGender2015, 1, comp_LSMS_Uganda$headGenderChange)

#Add maize yield
maizeDatLSMS_Base <- read.csv('LSMS_Uganda/LSMS_200910_cropprod_maize_270317.csv', stringsAsFactors = F)
maizeDatLSMS_Base <- subset(maizeDatLSMS_Base, prod.kg > 0 & !is.na(prod.kg))
maizeDatLSMS_Base <- group_by(maizeDatLSMS_Base, hhid)
maizeDatLSMS_Base <- summarise_all(select_if(maizeDatLSMS_Base, is.numeric), funs(sum(., na.rm=T)))
maizeDatLSMS_Base$yieldKG_Ha2012 <- maizeDatLSMS_Base$prod.kg / maizeDatLSMS_Base$area.ha
maizeDatLSMS_Base$yieldKG_Ha2012[is.infinite(maizeDatLSMS_Base$yieldKG_Ha2012)] <- NA

maizeDatLSMS_End <- read.csv('LSMS_Uganda/LSMS_201112_cropprod_maize_270317.csv', stringsAsFactors = F)
maizeDatLSMS_End <- subset(maizeDatLSMS_End, prod.kg > 0 & !is.na(prod.kg))
maizeDatLSMS_End <- group_by(maizeDatLSMS_End, hhid)
maizeDatLSMS_End <- summarise_all(select_if(maizeDatLSMS_End, is.numeric), funs(sum(., na.rm=T)))
maizeDatLSMS_End$yieldKG_Ha2015 <- maizeDatLSMS_End$prod.kg / maizeDatLSMS_End$area.ha
maizeDatLSMS_End$yieldKG_Ha2015[is.infinite(maizeDatLSMS_End$yieldKG_Ha2015)] <- NA

comp_LSMS_Uganda <- left_join(comp_LSMS_Uganda, select(maizeDatLSMS_Base, hhid, yieldKG_Ha2012))
comp_LSMS_Uganda <- left_join(comp_LSMS_Uganda, select(maizeDatLSMS_End, hhid, yieldKG_Ha2015))


write.csv(comp_LSMS_Uganda, file="comp_LSMS_Uganda.csv")
