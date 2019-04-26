#####################################################
#
#
#
#
#####################################################
library(dplyr)
library(tidyr)

##Add site info
datHH <- read.csv('1_1_2 Information generale.csv')
dat <- select(datHH, Household.ID, Commune, Village, respGender = Men_sexe, respAge = Men_age, yearsResiding = Residence, hhType = Men_type)
dat$siteProv <- ifelse(dat$Commune %in% c("Bani", "Dori", "Gorgadji", "Seytenga"), "Seno", "Yatenga") #Dori also called M'bamga
dat$Village <- as.factor(dat$Village)

##Process and combine household and farm data
source('process_income_lvst.R')
source('process_land_crop100Cons.R')
#source('process_land_crop.R')
dat <- left_join(dat, datIncome)
dat <- left_join(dat, datLand)
dat <- left_join(dat, datLvstTLU)
dat <- left_join(dat, datLvstLive)
dat <- left_join(dat, datLvstProd)
dat <- left_join(dat, datCrop)
dat$totalIncome_US <- ifelse(dat$totalIncome_US ==0 | is.na(dat$totalIncome_US), rowSums(dat[, c("liveAnimalVal_US", "lvstIncome_US", "cropIncome_US")], na.rm=T), dat$totalIncome_US)
dat$totalIncome_US[is.na(dat$totalIncome_US)] <- 0 
source('process_adulteq.R')
dat <- left_join(dat, datAdulteq)
dat$headGender <- ifelse(dat$hhType %in% c(1,2), "male", "female")

##Add HFIAS, PPI and HDDS
source('process_HFIAS.R')
dat <- left_join(dat, select(rawHFIAS, Household.ID, HFIAS_status_good, HFIAS_status_bad))

source('process_HDDS.R')
dat <- left_join(dat, datHDDS)

source('process_PPI.R')
dat <- left_join(dat, datPPI)

##Add practices
rawPractices <- read.csv('4_1 Invent pratiq intensification.csv')
rawPractices$valueAddBin <- ifelse(rawPractices$Valapra %in% c(1,2,3), 1, 0)
rawPractices$improvedSeedBin <- ifelse(rawPractices$Cerampra %in% c(1,2,3) | rawPractices$Leguapra %in% c(1,2,3), 1, 0)
rawPractices$manureBin <- ifelse(rawPractices$Fumpra %in% c(1,2,3), 1, 0)
rawPractices$fertBin <- ifelse(rawPractices$Engpra %in% c(1,2,3), 1, 0)
rawPractices$fatteningBin <- ifelse(rawPractices$Emboupra %in% c(2,3), 1, 0) #Only HHs doing this moderately and frequently
rawPractices$laboruseBin <- ifelse(rawPractices$Louepra %in% c(1,2,3), 1, 0)
rawPractices$stoverTreatBin <- ifelse(rawPractices$Traitpra %in% c(2,3), 1, 0)
dat <- left_join(dat, select(rawPractices, Household.ID, valueAddBin, improvedSeedBin, fertBin, manureBin, fatteningBin, laboruseBin, stoverTreatBin))

rm(list = setdiff(ls(), "dat"))

##Perceived FS aid and famine
rawFS <- read.csv('5_2a Statu alimentaire.csv')
table(rawFS$Nomrepsai1) # average meals per day in this season

table(rawFS$Stasealisai1) # perceived food security status. 1= not enough, 2= just enough, 3= enough food

table(rawFS$Aidalimsai1) # 1 = received aid, 0 = no aid

table(rawFS$Moifamine) # number of months famine

#rawFS$aidGood <- ifelse(rawFS$Aidalimsai1 == 1 | rawFS$Aidalimsai2 == 1, 1, 0)
#rawFS$aveMealsGood <- rowMeans(rawFS[, c("Nomrepsai1", "Nomrepsai2")], na.rm=T) 

dat <- left_join(dat, select(rawFS, Household.ID, aidFebMay = Aidalimsai3, aidJuneOct = Aidalimsai1, mealsFebMay = Nomrepsai3, mealsJuneOct = Nomrepsai1, mealsNovJan = Nomrepsai2, perceivedFSJuneOct = Stasealisai1, perceivedFSNovJan = Stasealisai2, perceivedFSFebMay = Stasealisai3))

dat$perceivedMonthsFI <- ifelse(dat$perceivedFSJuneOct == 1 & dat$perceivedFSFebMay == 1, "Feb_Oct",
                                ifelse(dat$perceivedFSJuneOct == 1, "June_Oct",
                                       ifelse(dat$perceivedFSFebMay == 1, "Feb_May", 
                                              ifelse(dat$perceivedFSJuneOct == 2 & dat$perceivedFSFebMay == 2, "JustEnoughFeb_Oct",
                                                     ifelse(dat$perceivedFSJuneOct == 2, "JustEnoughJune_Oct",
                                                            ifelse(dat$perceivedFSFebMay == 2, "JustEnoughFebMay", "Secure"))))))
#rawMonthsFoodNotEnough <- read.csv('5_4 Approvisonmnet nouriture.csv')

dat$perceivedHFIAS_status_bad <- ifelse(dat$HFIAS_status_bad == "FoodSecure" & dat$perceivedMonthsFI == "Secure", "SecurePerceived", as.character(dat$HFIAS_status_bad))
dat$perceivedHFIAS_status_good <- ifelse(dat$HFIAS_status_good == "FoodSecure" & dat$perceivedMonthsFI == "Secure", "SecurePerceived", as.character(dat$HFIAS_status_good))

##Add off-income and crop yield dummy
dat$offIncBin <- as.factor(dat$offIncome_US > 100)
dat$yieldBin <- ifelse(!is.na(dat$sorghumYield) & dat$sorghumYield > median(dat$sorghumYield[dat$siteProv == "Seno"], na.rm=T) & dat$siteProv == "Seno", 1,
                       ifelse(!is.na(dat$milletYield) & dat$milletYield > median(dat$milletYield[dat$siteProv == "Seno"], na.rm=T) & dat$siteProv == "Seno", 1,
                              ifelse(!is.na(dat$maizeYield) & dat$maizeYield > median(dat$maizeYield[dat$siteProv == "Seno"], na.rm=T) & dat$siteProv == "Seno", 1,
                              ifelse(!is.na(dat$sorghumYield) & dat$sorghumYield > median(dat$sorghumYield[dat$siteProv == "Yatenga"], na.rm=T) & dat$siteProv == "Yatenga", 1,
                                     ifelse(!is.na(dat$milletYield) & dat$milletYield > median(dat$milletYield[dat$siteProv == "Yatenga"], na.rm=T) & dat$siteProv == "Yatenga", 1,
                                            ifelse(!is.na(dat$maizeYield) & dat$maizeYield > median(dat$maizeYield[dat$siteProv == "Yatenga"], na.rm=T) & dat$siteProv == "Yatenga", 1, 0))))))
#2 households don't have sorghum or millet yields
dat$yieldBin <- as.factor(dat$yieldBin)
#dat$cropYieldkcalHa <- dat$crop_production_Eval..kcal. / dat$
#dat$cropYieldkcalHaBin <- ifelse(dat$cropYieldkcalHa > median(dat$cropYieldkcalHa, na.rm=T), 1, 0)
dat$aidFebMay <- as.factor(dat$aidFebMay)
dat$aidJuneOct <- ifelse(dat$aidJuneOct %in% c(2, 3), 1, dat$aidJuneOct)
dat$aidJuneOct <- as.factor(dat$aidJuneOct)
dat$aidBin <- as.factor(ifelse(dat$aidFebMay == 1 | dat$aidJuneOct == 1, 1, 0))

#wilcox.test(dat$lvstDiv ~ dat$siteProv)

#Add 4.2, 4.3
#dat$cropYieldkcalHaBin[is.na(dat$cropYieldkcalHaBin)] <- 0

dat$cropMarketPartic20Pc <- dat$cropsoldKcal_perc * (100 / 20) 
dat$lvstMarketPartic20Pc <- ifelse(dat$lvstConsProtein_perc == 1, 0, (1 - dat$lvstConsProtein_perc) * (100 / 20))
dat$lvstsoldProtein_perc <- ifelse(dat$lvstConsProtein_perc == 1, 0, (1 - dat$lvstConsProtein_perc)) 
dat$femaleIncomeControl10Pc <- ifelse(dat$femaleIncomeControl_perc == 0, 0, dat$femaleIncomeControl_perc * (100/10))
dat$cropCOPval10Pc <- ifelse(dat$cropCOPval_perc == 0, 0, dat$cropCOPval_perc * (100 / 10))
dat$lvstCOPval10Pc <- ifelse(dat$lvstCOPval_perc == 0, 0, dat$cropCOPval_perc * (100 / 10))
dat$liveLvstCOPval_perc <- dat$COPliveAnimalVal_US / dat$liveAnimalVal_US

#dat$totalIncome_US[dat$totalIncome_US == 0] <- 0.01
dat$logTotalIncome_US <- log1p(dat$totalIncome_US)
dat$totalIncome_US500 <- dat$totalIncome_US/500
#dat$offIncome_US[dat$offIncome_US == 0] <- 0.01
#dat$logOffIncome_US <- log(dat$offIncome_US)
dat$liveAnimalProfitUS <- ifelse(is.na(dat$liveAnimalVal_US), 0, dat$liveAnimalVal_US - dat$COPliveAnimalVal_US)
#dat$liveAnimalProfitUS <- ifelse(is.na(dat$liveAnimalVal_US), 0.01, dat$liveAnimalVal_US - dat$COPliveAnimalVal_US)
#dat$logLiveAnimalProfitUS <- log(dat$liveAnimalProfitUS)
dat$lvstProdProfitUS <- ifelse(is.na(dat$lvstTotalValue_US), 0, dat$lvstTotalValue_US - dat$lvstCOPvalue_US)
#dat$lvstProdProfitUS <- ifelse(is.na(dat$lvstTotalValue_US), 0.01, dat$lvstTotalValue_US - dat$lvstCOPvalue_US)
#dat$logLvstProdProfitUS <- log(dat$lvstProdProfitUS)
#dat$cropIncome_US[dat$cropIncome_US == 0] <- 0.01
#dat$logCropIncome_US <- log(dat$cropIncome_US)
dat$lvstProfitBin <- as.factor(ifelse(dat$lvstProdProfitUS>0.01, 1, 0))

dat$offIncome_US500 <- dat$offIncome_US / 500
dat$liveAnimalProfitUS500 <- dat$liveAnimalProfitUS / 500
dat$lvstProdProfitUS500 <- dat$lvstProdProfitUS / 500
dat$cropIncome_US500 <- dat$cropIncome_US / 500

dat$lvstProfitUS500 <- dat$liveAnimalProfitUS500 + dat$lvstProdProfitUS500

dat$cropConsKcal1Aeq <- dat$cropConsKcal / 365 / dat$adult_equivalent / 2500 # Crop consumed self-sufficincy
dat$lvstConsprotein1Aeq <- dat$lvstConsProtein / 365 / dat$adult_equivalent / 56 # Lvst consumed self-sufficincy
dat$lvstConsprotein1Aeq[is.na(dat$lvstConsprotein1Aeq)] <- 0
dat$farmConsKcal1Aeq <- rowSums(dat[, c("cropConsKcal", "lvstConsKcal")], na.rm = ) / 365 / dat$adult_equivalent / 2500

dat$kcalSelfSufficiency <- rowSums(dat[, c("cropConsKcal", "lvstConsKcal")], na.rm=T) / 365 / dat$adult_equivalent / 2500 
dat$proteinSelfSufficiency <- rowSums(dat[, c("cropConsProtein", "lvstConsProtein")], na.rm=T) / 365 / dat$adult_equivalent / 56 # daily requirement for an adult male is 56g 

dat$kcalTotalSufficiency <- rowSums(dat[, c("cropTotalKcal")], na.rm=T) / 365 / dat$adult_equivalent / 2500
dat$proteinTotalSufficiency <- rowSums(dat[, c("lvstTotalProtein")], na.rm=T) / 365 / dat$adult_equivalent / 56 # daily requirement for an adult male is 56g

dat$tlu[is.na(dat$tlu)] <- 0
dat$tlu[is.na(dat$tlu)] <- 0
dat$cropMarketPartic20Pc[is.na(dat$cropMarketPartic20Pc)] <- 0
dat$lvstMarketPartic20Pc[is.na(dat$lvstMarketPartic20Pc)] <- 0
dat$cropHDDSDiv[is.na(dat$cropHDDSDiv)] <- 0
dat$lvstHDDSDiv[is.na(dat$lvstHDDSDiv)] <- 0
dat$totalProdDiv <- rowSums(dat[, c("cropHDDSDiv", "lvstHDDSDiv")], na.rm=T)
dat$lvstHDDSDivBad[is.na(dat$lvstHDDSDivBad)] <- 0
dat$lvstHDDSDivGood[is.na(dat$lvstHDDSDivGood)] <- 0
dat$tlu[is.na(dat$tlu)] <- 0
dat$irriCash[is.na(dat$irriCash)] <- 0
dat$irriCash <- as.factor(dat$irriCash)
dat$fertBin[is.na(dat$fertBin)] <- 0
dat$cropCOPval10Pc[is.na(dat$cropCOPval10Pc)] <- 0
dat$lvstCOPval10Pc[is.na(dat$lvstCOPval10Pc)] <- 0
dat$lvstDivGood[is.na(dat$lvstDivGood)] <- 0
dat$femaleIncomeControl10Pc[is.na(dat$femaleIncomeControl10Pc)] <- 0
dat <- dat %>% rowwise() %>% mutate(netIncome = totalIncome_US - sum(cropCOPvalue_US, lvstCOPvalue_US, COPliveAnimalVal_US, na.rm=T))
dat$netIncome[dat$netIncome < 0.1] <- 0.1
dat$logNetIncome <- log(dat$netIncome)
dat <- dat %>% rowwise() %>% mutate(netFarmIncome = farmIncome_US - sum(cropCOPvalue_US, lvstCOPvalue_US, COPliveAnimalVal_US, na.rm=T))
#dat$netFarmIncome[dat$netFarmIncome < 0.1] <- 0.1
dat <- dat %>% rowwise() %>% mutate(totalLvstCOP = sum(lvstCOPvalue_US, COPliveAnimalVal_US, na.rm=T))
dat$totalLvstCOP500 <- dat$totalLvstCOP / 500

dat$netFarmIncomeUS500 <- dat$netFarmIncome / 500
dat$logNetFarmIncome_US <- log1p(dat$netFarmIncome)
dat$farmIncome_US[dat$farmIncome_US < 0.1] <- 0.1
dat$logFarmIncome <- log(dat$farmIncome_US)
dat$farmIncomeUS500 <- dat$farmIncome_US / 500
dat$improvedSeedBin <- as.factor(dat$improvedSeedBin)
dat$fertBin <- as.factor(dat$fertBin)
dat$laboruseBin <- as.factor(dat$laboruseBin)
dat$cropConsKcal1Aeq[is.na(dat$cropConsKcal1Aeq)] <- 0
dat$cropIncome_US500[is.na(dat$cropIncome_US500)] <- 0
dat$lvstDiv[is.na(dat$lvstDiv)] <- 0
dat$cropDiv[is.na(dat$cropDiv)] <- 0
dat$totalLvstIncome <- rowSums(dat[, c("liveAnimalVal_US", "lvstIncome_US")], na.rm=T)


dat$HFIAS_status_good <- factor(dat$HFIAS_status_good, levels = c("SeverelyFI", "ModeratelyFI", "MildlyFI", "FoodSecure"))
dat$HFIAS_status_bad <- factor(dat$HFIAS_status_bad, levels = c("SeverelyFI", "ModeratelyFI", "MildlyFI", "FoodSecure"))

dat$HDDSbadTertile <-ifelse(dat$siteProv == "Seno", as.character(cut(dat$HDDS_bad, quantile(dat$HDDS_bad[dat$siteProv == "Seno"], c(0, 1/3, 2/3, 1)),c('Low','Medium','High'), include.lowest = T)), as.character(cut(dat$HDDS_bad, quantile(dat$HDDS_bad[dat$siteProv == "Yatenga"], c(0, 1/3, 2/3, 1)),c('Low','Medium','High'), include.lowest = T)))
dat$HDDSgoodTertile <-ifelse(dat$siteProv == "Seno", as.character(cut(dat$HDDS_good, quantile(dat$HDDS_good[dat$siteProv == "Seno"], c(0, 1/3, 2/3, 1)),c('Low','Medium','High'), include.lowest = T)), as.character(cut(dat$HDDS_good, quantile(dat$HDDS_good[dat$siteProv == "Yatenga"], c(0, 1/3, 2/3, 1)),c('Low','Medium','High'), include.lowest = T)))
#dat$HDDSbadTertile <- as.character(cut(dat$HDDS_bad, quantile(dat$HDDS_bad, c(0, 1/3, 2/3, 1)),c('Low','Medium','High'), include.lowest = T))
#dat$HDDSgoodTertile <- as.character(cut(dat$HDDS_good, quantile(dat$HDDS_good, c(0, 1/3, 2/3, 1)),c('Low','Medium','High'), include.lowest = T))
dat$HDDSbadMedHigh <- ifelse(dat$HDDSbadTertile %in% c("Medium", "High"), "MedHigh", dat$HDDSbadTertile)
dat$HDDSgoodMedHigh <- ifelse(dat$HDDSgoodTertile %in% c("Medium", "High"), "MedHigh", dat$HDDSgoodTertile)
dat$HFIAS_HDDS_bad <- factor(paste(dat$HFIAS_status_bad, dat$HDDSbadMedHigh), levels = c("SeverelyFI Low", "SeverelyFI MedHigh", "ModeratelyFI Low", "ModeratelyFI MedHigh", "MildlyFI Low", "MildlyFI MedHigh", "FoodSecure Low", "FoodSecure MedHigh"))
dat$HFIAS_HDDS_good <- factor(paste(dat$HFIAS_status_good, dat$HDDSbadMedHigh), levels = c("SeverelyFI Low", "SeverelyFI MedHigh", "ModeratelyFI Low", "ModeratelyFI MedHigh", "MildlyFI Low", "MildlyFI MedHigh", "FoodSecure Low", "FoodSecure MedHigh"))

write.csv(dat, '20181030datBurkina.csv')



