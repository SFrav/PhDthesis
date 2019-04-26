#           __-----__
#      ..;;;--'~~~`--;;;..
#    /;-~IN GOD WE TRUST~-.\
#   //      ,;;;;;;;;      \\
# .//      ;;;;;    \       \\
# ||       ;;;;(   /.|       ||
# ||       ;;;;;;;   _\      ||
# ||       ';;  ;;;;=        ||
# ||LIBERTY | ''\;;;;;;      ||
#  \\     ,| '\  '|><| 2018 //
#   \\   |     |      \  A //
#    `;.,|.    |      '\.-'/
#      ~~;;;,._|___.,-;;;~'
#          ''=--'
#
# ASCII art by Daniel C Au

##Income

##Calculate income
dat$incomeCheese_lc <- ifelse(dat$cheese_sold_price_timeunits == "week", dat$cheese_sold_income * (paramLactLength / 7),
                              ifelse(dat$cheese_sold_price_timeunits == "month", dat$cheese_sold_income * (paramLactLength / 30),
                                     ifelse(dat$cheese_sold_price_timeunits == "year", dat$cheese_sold_income, NA)))

dat$incomeButter_lc <- ifelse(dat$butter_sold_price_timeunits == "day", dat$butter_sold_income * paramLactLength,
                              ifelse(dat$butter_sold_price_timeunits == "week", dat$butter_sold_income * (paramLactLength / 7),
                                     ifelse(dat$butter_sold_price_timeunits == "month", dat$butter_sold_income * (paramLactLength /30),
                                            ifelse(dat$butter_sold_price_timeunits == "year", dat$butter_sold_income, NA))))


dat$farmIncome_lc <- rowSums(dat[, c("incomeCrops_lc", "incomeLiveAnimal", "incomeMeat_lc", "incomeMilk_lc", "incomeEggs_lc", "incomeCheese_lc", "incomeButter_lc", "wool_sold_income", "incomeHoney_lc")], na.rm=T)
dat$farmIncome_US <- dat$farmIncome_lc / dat$Exchange.rate

dat$incomeCrops_US <- dat$incomeCrops_lc / dat$Exchange.rate

dat$livestockIncome_lc <- rowSums(dat[, c("incomeLiveAnimal", "incomeMeat_lc", "incomeMilk_lc", "incomeEggs_lc", "incomeCheese_lc", "incomeButter_lc", "wool_sold_income", "incomeHoney_lc")], na.rm=T)
dat$livestockIncome_US <- dat$livestockIncome_lc / dat$Exchange.rate

dat$incomeFarmFemaleControl_US <- rowSums(dat[, c("incomeCropFemaleControl_lc", "incomeMeatFemaleControl_lc", "incomeMilkFemaleControl_lc", "incomeEggsFemaleControl")]) / dat$Exchange.rate
dat$incomeFarmFemaleControl_prop <- dat$incomeFarmFemaleControl_US / dat$farmIncome_US

dat$incomeLivestockFemaleControl_US <- rowSums(dat[, c("incomeMeatFemaleControl_lc", "incomeMilkFemaleControl_lc", "incomeEggsFemaleControl")]) / dat$Exchange.rate
dat$incomeLivestockFemaleControl_prop <- dat$incomeLivestockFemaleControl_US / dat$livestockIncome_US

dat$incomeCropsFemaleControl_prop <- dat$incomeCropFemaleControl_lc / dat$incomeCrops_lc

##Off income
#table(dat$offfarm_income_proportion)
dat$offfarm_income_proportion <- tolower(dat$offfarm_income_proportion)
dat$percOffIncome <- ifelse(dat$offfarm_income_proportion == "all", 1,
                            ifelse(dat$offfarm_income_proportion == "half", 0.5,
                                   ifelse(dat$offfarm_income_proportion == "little", 0.05,
                                          ifelse(dat$offfarm_income_proportion == "most", 0.7,
                                                 ifelse(dat$offfarm_income_proportion == "underhalf", 0.25, NA)))))

dat$percOffIncome[is.na(dat$percOffIncome)] <- 0     
dat$offIncome_lc <- (dat$farmIncome_lc * dat$percOffIncome) / (1 - dat$percOffIncome)                     
dat$offIncome_lc[is.na(dat$offIncome_lc)] <- 0

dat$totalIncome_lc <- rowSums(dat[, c("farmIncome_lc", "offIncome_lc")], na.rm=T)
dat$totalIncome_US <- dat$totalIncome_lc * dat$Exchange.rate

#!Remove households with no farm income
#dat <- subset(dat, percOffIncome != 1)

#table(paste(grepl(paste(c("\\bmale\\w*?\\b", "man"), collapse="|"), dat$offfarm_who_control_revenue_1) == F  & !is.na(dat$offfarm_who_control_revenue_1), grepl(paste(c("\\bmale\\w*?\\b", "man"), collapse="|"), dat$offfarm_who_control_revenue_2) == F  & !is.na(dat$offfarm_who_control_revenue_2), grepl(paste(c("\\bmale\\w*?\\b", "man"), collapse="|"), dat$offfarm_who_control_revenue_3) == F  & !is.na(dat$offfarm_who_control_revenue_3), grepl(paste(c("\\bmale\\w*?\\b", "man"), collapse="|"), dat$offfarm_who_control_revenue_4) == F  & !is.na(dat$offfarm_who_control_revenue_4)))


####PPP
#source('processPPP.R')
PPPcurrent <- read.csv('DataParams/PPPWB.csv')
countryDF <- data.frame(country = c("cambodia", "drc", "eth", "ethiopia", "kenya", "mali", "tanzania", "tnz", "vietnam", "zambia", "burkina", "burkina faso", "malawi", "uganda", "ghana"), fullName = c("Cambodia", "Congo, Dem. Rep.", "Ethiopia", "Ethiopia", "Kenya", "Mali", "Tanzania", "Tanzania", "Vietnam", "Zambia", "Burkina Faso", "Burkina Faso", "Malawi", "Uganda", "Ghana"))
dat <- left_join(dat, countryDF)

dat <- left_join(dat, select(PPPcurrent, fullName = country, PPP = value))

dat$totalIncome_lc <- ifelse(dat$country == "drc" & dat$projectname == "CLiP", dat$totalIncome_lc * 1550, dat$totalIncome_lc) #Convert values enumerated in USD into local currency. In March 2017 it was 1550 CDF for each USD
dat$livestockIncome_lc <- ifelse(dat$country == "drc" & dat$projectname == "CLiP", dat$livestockIncome_lc * 1550, dat$livestockIncome_lc)
dat$incomeCrops_lc <- ifelse(dat$country == "drc" & dat$projectname == "CLiP", dat$incomeCrops_lc * 1550, dat$incomeCrops_lc)
dat$offIncome_lc <- ifelse(dat$country == "drc" & dat$projectname == "CLiP", dat$offIncome_lc * 1550, dat$offIncome_lc)

dat$incomePPP <- dat$totalIncome_lc / dat$PPP
dat$livestockIncomePPP <- dat$livestockIncome_lc / dat$PPP
dat$liveAnimalPPP <- dat$incomeLiveAnimal / dat$PPP
dat$livestockProdPPP <- rowSums(dat[, c("incomeLiveAnimal", "incomeMeat_lc", "incomeMilk_lc", "incomeEggs_lc", "incomeCheese_lc", "incomeButter_lc", "wool_sold_income", "incomeHoney_lc")], na.rm=T) / dat$PPP
dat$cropIncomePPP <- dat$incomeCrops_lc / dat$PPP
dat$offIncomePPP <- dat$offIncome_lc / dat$PPP

dat$livestockPPP_prop <- dat$livestockIncomePPP / dat$incomePPP

dat$fruPrice_kg_PPP <- dat$fruPrice_kg_lc / dat$PPP
#dat$grRTPrice_kg_PPP <- dat$GrRTPrice_kg_lc / dat$PPP
dat$legPrice_kg_PPP <- dat$legPrice_kg_lc / dat$PPP
dat$nutSPrice_kg_PPP <- dat$nutSPrice_kg_lc / dat$PPP
dat$vegLeafyPrice_kg_PPP <- dat$vegLeafyPrice_kg_lc / dat$PPP
dat$vegOtherPrice_kg_PPP <- dat$vegOtherPrice_kg_lc / dat$PPP
dat$vegVitAPrice_kg_PPP <- dat$vegVitAPrice_kg_lc / dat$PPP
dat$meatPrice_kg_PPP <- (dat$incomeMeat_lc / dat$PPP) / dat$totalMeatKG
dat$milkPrice_l_PPP <- (dat$incomeMilk_lc / dat$PPP) / dat$totalMilkL
dat$eggsPrice_each_PPP <- (dat$incomeEggs_lc / dat$PPP) / dat$totalEggsNum 