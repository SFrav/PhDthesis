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
comp_LSMS_Uganda_Mid <- read.csv('LSMS_Uganda/LSMS_201011_Baseline_merged_281016.csv')
comp_LSMS_Uganda_End <- read.csv('LSMS_Uganda/LSMS_201112_Baseline_merged_281016.csv')

##Alternative approach to calculating adult equivalent
# adEqCoef <- c(rep(0.5, 6), 0.8, 1, 1, 0.73, rep(0.5, 6), 0.75, 0.8, 0.8, 0.6)  
# 
# comp_LSMS_Uganda_Base$adult_eq2012 <- rowSums(t(apply(comp_LSMS_Uganda_Base[,10:29], 1, "*", adEqCoef)), na.rm=T)
# comp_LSMS_Uganda_End$adult_eq2015 <- rowSums(t(apply(comp_LSMS_Uganda_End[,10:29], 1, "*", adEqCoef)), na.rm=T)

colnames(comp_LSMS_Uganda_Base)[grep("hhh_age", colnames(comp_LSMS_Uganda_Base))] <- "HHheadAge.Base"
colnames(comp_LSMS_Uganda_Mid)[grep("hhh_age", colnames(comp_LSMS_Uganda_Mid))] <- "HHheadAge.Mid"
colnames(comp_LSMS_Uganda_End)[grep("hhh_age", colnames(comp_LSMS_Uganda_End))] <- "HHheadAge.End"
colnames(comp_LSMS_Uganda_Base)[grep("hhh_sex", colnames(comp_LSMS_Uganda_Base))] <- "HHheadGender.Base"
colnames(comp_LSMS_Uganda_Mid)[grep("hhh_sex", colnames(comp_LSMS_Uganda_Mid))] <- "HHheadGender.Mid"
colnames(comp_LSMS_Uganda_End)[grep("hhh_sex", colnames(comp_LSMS_Uganda_End))] <- "HHheadGender.End"
colnames(comp_LSMS_Uganda_Base)[grep("tlu", colnames(comp_LSMS_Uganda_Base))] <- "tlu.Base"
colnames(comp_LSMS_Uganda_Mid)[grep("tlu", colnames(comp_LSMS_Uganda_Base))] <- "tlu.Mid"
colnames(comp_LSMS_Uganda_End)[grep("tlu", colnames(comp_LSMS_Uganda_End))] <- "tlu.End"
colnames(comp_LSMS_Uganda_Base)[grep("income_offfarm", colnames(comp_LSMS_Uganda_Base))] <- "offIncome.Base"
colnames(comp_LSMS_Uganda_Mid)[grep("income_offfarm", colnames(comp_LSMS_Uganda_Mid))] <- "offIncome.Mid"
colnames(comp_LSMS_Uganda_End)[grep("income_offfarm", colnames(comp_LSMS_Uganda_End))] <- "offIncome.End"
colnames(comp_LSMS_Uganda_Base)[grep("Adult_Equivalent", colnames(comp_LSMS_Uganda_Base))] <- "adult_eq.Base"
colnames(comp_LSMS_Uganda_Mid)[grep("Adult_Equivalent", colnames(comp_LSMS_Uganda_Mid))] <- "adult_eq.Mid"
colnames(comp_LSMS_Uganda_End)[grep("Adult_Equivalent", colnames(comp_LSMS_Uganda_End))] <- "adult_eq.End"
colnames(comp_LSMS_Uganda_Base)[grep("tot_energy", colnames(comp_LSMS_Uganda_Base))] <- "FA.Base"
colnames(comp_LSMS_Uganda_Mid)[grep("tot_energy", colnames(comp_LSMS_Uganda_Mid))] <- "FA.Mid"
colnames(comp_LSMS_Uganda_End)[grep("tot_energy", colnames(comp_LSMS_Uganda_End))] <- "FA.End"
comp_LSMS_Uganda_Base$landOwnedHa.Base <- comp_LSMS_Uganda_Base$area.ac * 0.404686
comp_LSMS_Uganda_Mid$landOwnedHa.Mid <- comp_LSMS_Uganda_Mid$area.ac * 0.404686
comp_LSMS_Uganda_End$landOwnedHa.End <- comp_LSMS_Uganda_End$area.ac * 0.404686
comp_LSMS_Uganda_Base$FSS.Base <- rowSums(comp_LSMS_Uganda_Base[, c("energy_crop_cons", "energy_lvst_cons")], na.rm=T) / comp_LSMS_Uganda_Base$HH_energy_need
comp_LSMS_Uganda_Mid$FSS.Mid <- rowSums(comp_LSMS_Uganda_Mid[, c("energy_crop_cons", "energy_lvst_cons")], na.rm=T) / comp_LSMS_Uganda_Mid$HH_energy_need
comp_LSMS_Uganda_End$FSS.End <- rowSums(comp_LSMS_Uganda_End[, c("energy_crop_cons", "energy_lvst_cons")], na.rm=T) / comp_LSMS_Uganda_End$HH_energy_need


comp_LSMS_Uganda <- inner_join(select(comp_LSMS_Uganda_Base, hhid, Country = country, region, HHheadAge.Base, HHheadGender.Base, tlu.Base, offIncome.Base, adult_eq.Base, FA.Base, FSS.Base, landOwnedHa.Base), 
                               select(comp_LSMS_Uganda_End, hhid, Country = country, region, HHheadAge.End, HHheadGender.End, tlu.End, offIncome.End, adult_eq.End, FA.End, FSS.End, landOwnedHa.End))

comp_LSMS_Uganda <- inner_join(comp_LSMS_Uganda, select(comp_LSMS_Uganda_Mid, hhid, Country = country, region, HHheadAge.Mid, HHheadGender.Mid, tlu.Mid, offIncome.Mid, adult_eq.Mid, FA.Mid, FSS.Mid, landOwnedHa.Mid))

comp_LSMS_Uganda$FA.Base <- (comp_LSMS_Uganda$FA.Base/ (365*comp_LSMS_Uganda$adult_eq.Base))
comp_LSMS_Uganda$FA.Mid <- (comp_LSMS_Uganda$FA.Mid/ (365*comp_LSMS_Uganda$adult_eq.Mid))
comp_LSMS_Uganda$FA.End <- (comp_LSMS_Uganda$FA.End/ (365*comp_LSMS_Uganda$adult_eq.End))

comp_LSMS_Uganda$headGenderChange <- ifelse(comp_LSMS_Uganda$HHheadGender.Base != comp_LSMS_Uganda$HHheadGender.End, 1, 0)

#Add maize yield
maizeDatLSMS_Base <- read.csv('LSMS_Uganda/LSMS_200910_cropprod_maize_270317.csv', stringsAsFactors = F)
maizeDatLSMS_Base <- subset(maizeDatLSMS_Base, prod.kg > 0 & !is.na(prod.kg))
maizeDatLSMS_Base <- group_by(maizeDatLSMS_Base, hhid)
maizeDatLSMS_Base <- summarise_all(select_if(maizeDatLSMS_Base, is.numeric), funs(sum(., na.rm=T)))
maizeDatLSMS_Base$yieldKG_Ha.Base <- maizeDatLSMS_Base$prod.kg / maizeDatLSMS_Base$area.ha
maizeDatLSMS_Base$yieldKG_Ha.Base[is.infinite(maizeDatLSMS_Base$yieldKG_Ha.Base)] <- NA

maizeDatLSMS_Mid <- read.csv('LSMS_Uganda/LSMS_201011_cropprod_maize_270317.csv', stringsAsFactors = F)
maizeDatLSMS_Mid <- subset(maizeDatLSMS_Mid, prod.kg > 0 & !is.na(prod.kg))
maizeDatLSMS_Mid <- group_by(maizeDatLSMS_Mid, hhid)
maizeDatLSMS_Mid <- summarise_all(select_if(maizeDatLSMS_Mid, is.numeric), funs(sum(., na.rm=T)))
maizeDatLSMS_Mid$yieldKG_Ha.Mid <- maizeDatLSMS_Mid$prod.kg / maizeDatLSMS_Mid$area.ha
maizeDatLSMS_Mid$yieldKG_Ha.Mid[is.infinite(maizeDatLSMS_Mid$yieldKG_Ha.Mid)] <- NA

maizeDatLSMS_End <- read.csv('LSMS_Uganda/LSMS_201112_cropprod_maize_270317.csv', stringsAsFactors = F)
maizeDatLSMS_End <- subset(maizeDatLSMS_End, prod.kg > 0 & !is.na(prod.kg))
maizeDatLSMS_End <- group_by(maizeDatLSMS_End, hhid)
maizeDatLSMS_End <- summarise_all(select_if(maizeDatLSMS_End, is.numeric), funs(sum(., na.rm=T)))
maizeDatLSMS_End$yieldKG_Ha.End <- maizeDatLSMS_End$prod.kg / maizeDatLSMS_End$area.ha
maizeDatLSMS_End$yieldKG_Ha.End[is.infinite(maizeDatLSMS_End$yieldKG_Ha.End)] <- NA

comp_LSMS_Uganda <- left_join(comp_LSMS_Uganda, select(maizeDatLSMS_Base, hhid, yieldKG_Ha.Base))
comp_LSMS_Uganda <- left_join(comp_LSMS_Uganda, select(maizeDatLSMS_Mid, hhid, yieldKG_Ha.Mid))
comp_LSMS_Uganda <- left_join(comp_LSMS_Uganda, select(maizeDatLSMS_End, hhid, yieldKG_Ha.End))

#comp_LSMS_Uganda$hhid <- as.character(paste("a", comp_LSMS_Uganda$hhid))
library(reshape)

#comp_LSMS_UgandaGather <- gather(select(subset(comp_LSMS_Uganda, headGenderChange ==0), hhid, HHheadAge.Base, HHheadAge.Mid, HHheadAge.End), term, age, -hhid)
#a <- reshape(comp_LSMS_UgandaGather, direction="wide", timevar = "hhid", idvar=c("term"))
#a$term <- NULL
#icc(a, model = "twoway", type = "consistency")

#comp_LSMS_UgandaGather <- gather(select(comp_LSMS_Uganda, hhid, tlu.Base, tlu.Mid, tlu.End), term, tlu, -hhid)
#a <- reshape(comp_LSMS_UgandaGather, direction="wide", timevar = "hhid", idvar=c("term"))
#a$term <- NULL
#icc_corr(a, model = "twoway", type = "consistency")

#comp_LSMS_UgandaGather <- gather(select(comp_LSMS_Uganda, hhid, yieldKG_Ha.Base, yieldKG_Ha.Mid, yieldKG_Ha.End), term, yield, -hhid)
#a <- spread(select(comp_LSMS_UgandaGather, hhid, yield), hhid, yield)

#Add food self sufficiency