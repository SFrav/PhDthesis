# 
#                             __.----.___      Prepare income and livestock variables  
# ||            ||  (\(__)/)-'||      ;--` ||  Tested R versions: 3.4.1
#_||____________||___`(QQ)'___||______;____||_ Authors: Simon Fraval & Mark van Wijk
#-||------------||----)  (----||-----------||- Contact: simon.fraval@outlook.com
#_||____________||___(o  o)___||______;____||_
#-||------------||----`--'----||-----------||-
# ||            ||       `|| ||| || ||     ||  ASCI art: "My Barnyard", Joan Stark
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

library(dplyr)
library(tidyr)

paramUSD <- 0.001665

##Income
rawIncomedat<-read.csv('5_1 source de revenu du menage.csv', stringsAsFactors=FALSE)
#rawIncomedat$Trdargerev <- as.numeric(gsub("\\(orpaillage)", "", rawIncomedat$Trdargerev))

rawIncomedatLong <- gather(select(rawIncomedat, Household.ID, ends_with("rev")), var, income, ends_with("rev")) #income
rawIncomedatLong1 <- gather(select(rawIncomedat, Household.ID, ends_with("con")), var, controlsRevenue, ends_with("con")) #Gender control
rawIncomedatLong$var <- tolower(ifelse(rawIncomedatLong$var == "Autrrev", substr(rawIncomedatLong$var, 1, 4), substr(rawIncomedatLong$var, 1, 5)))
rawIncomedatLong1$var <- tolower(ifelse(rawIncomedatLong1$var == "Autrcon", substr(rawIncomedatLong1$var, 1, 4), substr(rawIncomedatLong1$var, 1, 5)))

rawIncomedatLong <- left_join(rawIncomedatLong, rawIncomedatLong1)
rawIncomedatLong$income <- as.numeric(gsub("\\(orpaillage)", "", rawIncomedatLong$income)) #Remove detail referring to gold panning
rawIncomedatLong[rawIncomedatLong$Household.ID == "YAZI21" & rawIncomedatLong$var == "trcha", 3:4] <- c(75000, 5) #data shifted to the right by two spaces (in wide format)
rawIncomedatLong[rawIncomedatLong$Household.ID == "YAZI21" & rawIncomedatLong$var == "veprr", 3] <- 0 # Related to the shift above. The '5' should be indicating gender control rather than revenue
rawIncomedatLong[rawIncomedatLong$Household.ID == "SEGO29" & rawIncomedatLong$var == "comag", 3:4] <- c(45000, 2) #data shifted to the right (in wide format)
rawIncomedatLong[rawIncomedatLong$Household.ID == "SEGO29" & rawIncomedatLong$var == "vepoi", 3:4] <- c(45000, 2) #related to shift above
rawIncomedatLong[rawIncomedatLong$Household.ID == "SEMB40" & rawIncomedatLong$var == "combe", 3:4] <- c(400000, 1) #data shifted to the left (in wide format)
rawIncomedatLong[rawIncomedatLong$Household.ID == "SEBA44" & rawIncomedatLong$var == "locte", 3:4] <- c(108000, 1) #data shifted to the left (in wide format)
rawIncomedatLong[rawIncomedatLong$Household.ID == "SEMB46" & rawIncomedatLong$var == "comag", 3:4] <- c(40000, 1) #data shifted

#Convert to USD
rawIncomedatLong$income <- rawIncomedatLong$income * paramUSD
#create variables of interest. Sale of fish and 'natural resorces' only captured in total income
rawIncomedatLong$femaleControl <- ifelse(rawIncomedatLong$controlsRevenue == 2, rawIncomedatLong$income, 0)
rawIncomedatLong$farmIncome <- ifelse(rawIncomedatLong$var %in% c("vebet", "veprb", "veseb", "vepra", "veprm", "locte"), rawIncomedatLong$income, 0) #including livestock services and land rental
rawIncomedatLong$livestockIncome <- ifelse(rawIncomedatLong$var %in% c("vebet", "veprb"), rawIncomedatLong$income, 0)
rawIncomedatLong$cropIncome <- ifelse(rawIncomedatLong$var %in% c("vepra", "veprm"), rawIncomedatLong$income, 0)
#Off farm = trade in ag products, formal salaried employment, business to business services, working in other fields, pension and other (e.g. gold panning)
rawIncomedatLong$offIncome <- ifelse(rawIncomedatLong$var %in% c("combe", "comag", "salfo", "entco", "trcha", "pensi", "trdar", "autr"), rawIncomedatLong$income, 0)
rawIncomedatLong$farmLaborIncome <- ifelse(rawIncomedatLong$var %in% c("trcha"), rawIncomedatLong$income, 0)
datIncome <- group_by(rawIncomedatLong, Household.ID)
datIncome <- summarise(datIncome, totalIncome_US = sum(income, na.rm=T), offIncome_US = sum(offIncome, na.rm=T), 
                       farmIncome_US = sum(farmIncome, na.rm=T), femaleControlGrossIncome_US = sum(femaleControl, na.rm=T), 
                       livestockIncomeT5_US = sum(livestockIncome, na.rm=T), cropIncomeT5_US = sum(cropIncome, na.rm=T))

datIncome$offIncome_perc <- datIncome$offIncome_US / datIncome$totalIncome_US
datIncome$femaleIncomeControl_perc <- datIncome$femaleControlGrossIncome_US / datIncome$totalIncome_US
datIncome$farmIncome_perc <- datIncome$farmIncome_US / datIncome$totalIncome_US

##Livestock
#livestock holdings
#Based on Njuki, 2011 - gender, livestock and livelihood indicators
tlu_set<-data.frame(species = c('cattle','oxen','goat','donkey','horse','sheep','pigs','chicken','duck','guinea_fowl','pigeon','camel'), 
                    coefTLU = c(1,1.42,0.2,0.8,0.9,0.2,0.3,0.04,0.04,0.04,0.01,1.1))
lvstdat1<-read.csv('2_2Bien betail.csv')

lvstdat1$cattle <- lvstdat1$Bovma+lvstdat1$Bovfe
lvstdat1$oxen <- lvstdat1$Btrama
lvstdat1$sheep <- lvstdat1$Mouma+lvstdat1$Moufe
lvstdat1$goat <- lvstdat1$Caprma+lvstdat1$Caprfe
lvstdat1$chicken <- lvstdat1$Volama+lvstdat1$Volafe
lvstdat1$guinea_fowl <- lvstdat1$Pintma+lvstdat1$Pintfe
lvstdat1$pigs <- lvstdat1$Porcma+lvstdat1$Porcfe
lvstdat1$donkey <- lvstdat1$Anema+lvstdat1$Anefe
lvstdat1$horse <- lvstdat1$Chevma+lvstdat1$Chevfe
lvstdat1$pigeon <- lvstdat1$pigma+lvstdat1$pigfe
lvstdat1$duck <- lvstdat1$canarsma+lvstdat1$canardfe
lvstdat1$camel <- lvstdat1$dromma+lvstdat1$dromfe

lvstdatLong <- gather(lvstdat1[, c(6, 79:90)], species, num, colnames(lvstdat1[, c(79:90)]))
lvstdatLong2 <- gather(select(lvstdat1, Household.ID, cattle = Bopossfe , oxen = Btpossfe, sheep = Mopossfe, goat = Capossfe, chicken = Vopossfe, guinea_fowl = Pipossfe, pigs = Popossfe, donkey = Anpossfe, horse =Chpossfe, pigeon = pigpossfe, duck = canarpossfe, camel = drompossfe), species, femaleOwnNum, c("cattle", "oxen", "sheep", "goat", "chicken", "guinea_fowl", "pigs", "donkey", "horse", "duck", "pigeon", "camel"))

lvstdatLong <- left_join(lvstdatLong, lvstdatLong2)
lvstdatLong <- left_join(lvstdatLong, tlu_set)
lvstdatLong$tlu <- lvstdatLong$num * lvstdatLong$coefTLU
lvstdatLong$tluFemaleOwn <- lvstdatLong$femaleOwnNum * lvstdatLong$coefTLU
lvstdatLong$species <- gsub("oxen", "cattle", lvstdatLong$species)
lvstdatLong <- group_by(lvstdatLong, Household.ID)
lvstdatLong <- lvstdatLong[lvstdatLong$num >0, ]
datLvstTLU <- summarise(lvstdatLong, tlu = sum(tlu, na.rm=T), tluFemaleOwn = sum(tluFemaleOwn, na.rm=T), lvstDiv = length(unique(species)))

#Live animals
lvst_prodLive<-read.csv('2_3_1Achat vente betail.csv')
lvst_prodLiveLong <- gather(select(lvst_prodLive, Household.ID, ends_with("ndu")), species, num, ends_with("ndu"))
lvst_prodLiveLong1 <- gather(select(lvst_prodLive, Household.ID, ends_with("endpri")), species, price, ends_with("endpri"))
lvst_prodLiveLong2 <- gather(select(lvst_prodLive, Household.ID, ends_with("eve")), species, COP_perc, ends_with("eve"))
lvst_prodLiveLong3 <- gather(select(lvst_prodLive, Household.ID, ends_with("rev"), -canarconrev), species, controlsRevenue, ends_with("rev"))
lvst_prodLiveLong$species <- gsub("Pou", "Py", lvst_prodLiveLong$species) #Not sure of the difference in species between Ch and ch
lvst_prodLiveLong1$species <- gsub("Pou", "Py", lvst_prodLiveLong1$species)
lvst_prodLiveLong2$species <- gsub("Pou", "Py", lvst_prodLiveLong2$species)
lvst_prodLiveLong3$species <- gsub("Pou", "Py", lvst_prodLiveLong3$species)
lvst_prodLiveLong$species <- substr(lvst_prodLiveLong$species, 1, 2)
lvst_prodLiveLong1$species <- substr(lvst_prodLiveLong1$species, 1, 2)
lvst_prodLiveLong2$species <- substr(lvst_prodLiveLong2$species, 1, 2)
lvst_prodLiveLong3$species <- substr(lvst_prodLiveLong3$species, 1, 2)
lvst_prodLiveLong <- left_join(lvst_prodLiveLong, lvst_prodLiveLong1)
lvst_prodLiveLong <- left_join(lvst_prodLiveLong, lvst_prodLiveLong2)
lvst_prodLiveLong <- left_join(lvst_prodLiveLong, lvst_prodLiveLong3)
lvst_prodLiveLong <- lvst_prodLiveLong[lvst_prodLiveLong$num > 0,]
lvst_prodLiveLong <- lvst_prodLiveLong[lvst_prodLiveLong$price != 0,] #These instances look like they purchased animals and entered in the incorrect column
lvst_prodLiveLong[lvst_prodLiveLong$Household.ID == "YAZI27" & lvst_prodLiveLong$species == "ch", 3:5] <- c(1, 42500, 25.88) #YAZI27 was: num = 60,000, val = 1, copperc >100
#!COP = 0 is taken to be very low COP i.e. cost of water or opportunity cost of labor. Changed to 1% to indicate this
lvst_prodLiveLong$COP_perc[lvst_prodLiveLong$COP_perc %in% c("(Acheter pour revendre)", "-0.2")] <- 0 
lvst_prodLiveLong$COP_perc <- as.numeric(lvst_prodLiveLong$COP_perc)
lvst_prodLiveLong$COP_perc <- ifelse(lvst_prodLiveLong$COP_perc == 0, 1, lvst_prodLiveLong$COP_perc)
lvst_prodLiveLong <- mutate(lvst_prodLiveLong, liveAnimalVal = num * price * paramUSD, COPliveAnimalVal = liveAnimalVal * (COP_perc/100))
lvst_prodLiveLong$femaleControl <- ifelse(lvst_prodLiveLong$controlsRevenue %in% c(2,3), lvst_prodLiveLong$liveAnimalVal, 0)
datLvstLive <- group_by(lvst_prodLiveLong, Household.ID)
datLvstLive <- summarise(datLvstLive, liveAnimalVal_US = sum(liveAnimalVal, na.rm=T), 
                         COPliveAnimalVal_US = sum(COPliveAnimalVal, na.rm=T), 
                         femaleControlLiveAnVal_US = sum(femaleControl, na.rm=T))

#livestock products
paramLvstNutriVal <- data.frame(species = c("Blai", "Bvia", "Bbeu", "Byao", "Bfro", "Prul", "Pruv", "Por", "Pou", "Oeuf", "pig"), 
                                protein = c(34, 43, 80, 38, 323, 45, mean(175, 165), 168, 236, 126, 236), 
                                edibleConvF = c(1, 1, 1, 1, 1, 1, mean(0.74, 0.82), 1, 0.72, 0.88, 0.72),
                                kcal = c(650, 1260, 7200, 730, 3960, 760, mean(1650, 2570), 2650, 1070, 1390, 1070),
                                fullName = c("Cattle milk", "Cattle meat", "Butter", "Yogurt", "Cheese", "Goat milk", "Goat sheep meat", "Pork", "Chicken", "Eggs", "Guinnea fowl"),
                                FAOcode = c("10_001", "07_009", "11_001", "10_005", "10_006", "10_003", "07_046 / 07_004", "07_006", "07_033", "08_001", "07_033"))
#FAO, 2012, West african food composition table: http://www.fao.org/docrep/015/i2698b/i2698b00.pdf

paramGoatSheepCarcassDebone <- 11.25 #Assuming a carcass weight of 15kg and bone content of 25% - based on Simela, 2008
paramGuineaFowlCarcassDebone <- 0.9 #based on Mareko, 2006 - Botswana. Concrete and earth floors
paramLactLength <- 270 #Based on Millogo, 2010 https://pub.epsilon.slu.se/2208/1/millogo_v_100107.pdf

#good season
lvst_prodgoodSeas <- read.csv('2_3_2a_Prd betail bonne saison.csv')
lvst_prodgoodSeasLong <- gather(select(lvst_prodgoodSeas, Household.ID, ends_with("prod"), ends_with("bat")), species, vol, c(ends_with("prod"), ends_with("bat")))
lvst_prodgoodSeasLong1 <- gather(select(lvst_prodgoodSeas, Household.ID, ends_with("cons"), -ends_with("concons")), species, cons_perc, ends_with("cons"))
lvst_prodgoodSeasLong2 <- gather(select(lvst_prodgoodSeas, Household.ID, ends_with("vent")), species, sold_perc, ends_with("vent"))
lvst_prodgoodSeasLong3 <- gather(select(lvst_prodgoodSeas, Household.ID, ends_with("pri"), ends_with("uni"), ends_with("riu")), species, price_unit, ends_with("pri"), ends_with("uni"), ends_with("riu"))
lvst_prodgoodSeasLong4 <- gather(select(lvst_prodgoodSeas, Household.ID, ends_with("perev")), species, COP_perc, ends_with("perev"))
lvst_prodgoodSeasLong5 <- gather(select(lvst_prodgoodSeas, Household.ID, ends_with("conrev")), species, controlsRevenue, ends_with("conrev"))
#Painful data cleaning
lvst_prodgoodSeasLong$vol <- gsub("Kg 0", "", lvst_prodgoodSeasLong$vol)
lvst_prodgoodSeasLong$vol <- gsub("Kg", "", lvst_prodgoodSeasLong$vol)
lvst_prodgoodSeasLong$vol <- gsub("kg", "", lvst_prodgoodSeasLong$vol)
lvst_prodgoodSeasLong$vol <- gsub(" ", "", lvst_prodgoodSeasLong$vol)
lvst_prodgoodSeasLong$numAnimals <- 0
lvst_prodgoodSeasLong$numAnimals <- ifelse(tolower(lvst_prodgoodSeasLong$vol) %in% c("1moutons", "1mouton", "1chèvre", "1chévre"), 1, lvst_prodgoodSeasLong$num)
lvst_prodgoodSeasLong$vol <- ifelse(tolower(lvst_prodgoodSeasLong$vol) %in% c("1moutons", "1mouton", "1chèvre", "1chévre"), paramGoatSheepCarcassDebone*1, lvst_prodgoodSeasLong$vol)
lvst_prodgoodSeasLong$numAnimals <- ifelse(tolower(lvst_prodgoodSeasLong$vol) %in% c("2(chévres)", "2chévres", "2chèvres", "2mouton", "2moutons", "1chèvre+1moutons", "1chévre,1mouton", "1chèvreet1mouton", "1mouton,1chevre", "1mouton,1chévre", "1moutons1chevre"), 2, lvst_prodgoodSeasLong$num)
lvst_prodgoodSeasLong$vol <- ifelse(tolower(lvst_prodgoodSeasLong$vol) %in% c("2(chévres)", "2chévres", "2chèvres", "2mouton", "2moutons", "1chèvre+1moutons", "1chévre,1mouton", "1chèvreet1mouton", "1mouton,1chevre", "1mouton,1chévre", "1moutons1chevre"), paramGoatSheepCarcassDebone*2, lvst_prodgoodSeasLong$vol)
lvst_prodgoodSeasLong$numAnimals <- ifelse(tolower(lvst_prodgoodSeasLong$vol) %in% c("1chévre+2moutons", "1mouton,2chévres", "2moutons,1chévre", "2moutons1chevre", "3chévres", "3chèvres", "3moutons", "2chèvres+1mouton"), 3, lvst_prodgoodSeasLong$num)
lvst_prodgoodSeasLong$vol <- ifelse(tolower(lvst_prodgoodSeasLong$vol) %in% c("1chévre+2moutons", "1mouton,2chévres", "2moutons,1chévre", "2moutons1chevre", "3chévres", "3chèvres", "3moutons", "2chèvres+1mouton"), paramGoatSheepCarcassDebone*3, lvst_prodgoodSeasLong$vol)
lvst_prodgoodSeasLong$numAnimals <- ifelse(tolower(lvst_prodgoodSeasLong$vol) %in% c("1chévre+3moutons", "1moutons+3chèvres", "2chévres;2moutons", "2moutons,2chévre", "2moutons2chevres", "3chèvres+1moutons", "4chévres", "4chèvres", "4moutns", "4moutons"), 4, lvst_prodgoodSeasLong$num)
lvst_prodgoodSeasLong$vol <- ifelse(tolower(lvst_prodgoodSeasLong$vol) %in% c("1chévre+3moutons", "1moutons+3chèvres", "2chévres;2moutons", "2moutons,2chévre", "2moutons2chevres", "3chèvres+1moutons", "4chévres", "4chèvres", "4moutns", "4moutons"), paramGoatSheepCarcassDebone*4, lvst_prodgoodSeasLong$vol)
lvst_prodgoodSeasLong$numAnimals <- ifelse(tolower(lvst_prodgoodSeasLong$vol) %in% c("2chévres;3moutons", "3moutons2chevres", "4moutons+1chevre", "05chèvres"), 5, lvst_prodgoodSeasLong$num)
lvst_prodgoodSeasLong$vol <- ifelse(tolower(lvst_prodgoodSeasLong$vol) %in% c("2chévres;3moutons", "3moutons2chevres", "4moutons+1chevre", "05chèvres"), paramGoatSheepCarcassDebone*5, lvst_prodgoodSeasLong$vol)
lvst_prodgoodSeasLong$numAnimals <- ifelse(tolower(lvst_prodgoodSeasLong$vol) %in% c("6moutons", "4moutonset2chévres"), 6, lvst_prodgoodSeasLong$num)
lvst_prodgoodSeasLong$vol <- ifelse(tolower(lvst_prodgoodSeasLong$vol) %in% c("6moutons", "4moutonset2chévres"), paramGoatSheepCarcassDebone*6, lvst_prodgoodSeasLong$vol)
lvst_prodgoodSeasLong$numAnimals <- ifelse(tolower(lvst_prodgoodSeasLong$vol) %in% c("4chévres+3moutons", "4moutonset3chévres"), 7, lvst_prodgoodSeasLong$num)
lvst_prodgoodSeasLong$vol <- ifelse(tolower(lvst_prodgoodSeasLong$vol) %in% c("4chévres+3moutons", "4moutonset3chévres"), paramGoatSheepCarcassDebone*7, lvst_prodgoodSeasLong$vol)
lvst_prodgoodSeasLong$numAnimals <- ifelse(tolower(lvst_prodgoodSeasLong$vol) %in% c("6moutons+2chèvres", "2moutons+6chevres"), 8, lvst_prodgoodSeasLong$num)
lvst_prodgoodSeasLong$vol <- ifelse(tolower(lvst_prodgoodSeasLong$vol) %in% c("6moutons+2chèvres", "2moutons+6chevres"), paramGoatSheepCarcassDebone*8, lvst_prodgoodSeasLong$vol)
lvst_prodgoodSeasLong$numAnimals <- ifelse(tolower(lvst_prodgoodSeasLong$vol) %in% c("6moutons,3chévres"), 9, lvst_prodgoodSeasLong$num)
lvst_prodgoodSeasLong$vol <- ifelse(tolower(lvst_prodgoodSeasLong$vol) %in% c("6moutons,3chévres"), paramGoatSheepCarcassDebone*9, lvst_prodgoodSeasLong$vol)
lvst_prodgoodSeasLong$numAnimals <- ifelse(tolower(lvst_prodgoodSeasLong$vol) %in% c("1pintade"), 1, lvst_prodgoodSeasLong$num)
lvst_prodgoodSeasLong$vol[lvst_prodgoodSeasLong$vol == "1pintade"] <- paramGuineaFowlCarcassDebone #Guinea fowl
lvst_prodgoodSeasLong$vol[lvst_prodgoodSeasLong$vol == "3l/an" & lvst_prodgoodSeasLong$Household.ID == "SEMB31"] <- 3*2 #This household has 2 heads of cattle. 3kg of butter per animal per year
lvst_prodgoodSeasLong$vol[lvst_prodgoodSeasLong$vol == "5/mois"] <- 5*8 #5 kg of butter per month (assuming 8 month lactation)
lvst_prodgoodSeasLong$vol[lvst_prodgoodSeasLong$vol == "1L/40Jours"] <- 40 # 1l of butter for 40 days
lvst_prodgoodSeasLong$vol <- as.numeric(lvst_prodgoodSeasLong$vol)
#!Milk is l / day ave. assume an 270 d lactation and the good period is 50% of this lactation. The one HH that produces > 7000 l (24000 l to be exact) has 100 head
lvst_prodgoodSeasLong$vol <- ifelse(grepl("lait", lvst_prodgoodSeasLong$species), as.numeric(lvst_prodgoodSeasLong$vol) * paramLactLength*0.5, lvst_prodgoodSeasLong$vol)

lvst_prodgoodSeasLong4$species[substr(lvst_prodgoodSeasLong4$species, 1, 4) == "Prup"] <- "Prul"

lvst_prodgoodSeasLong$species <- ifelse(substr(lvst_prodgoodSeasLong$species, 1, 2) %in% c("Po", "pi"), substr(lvst_prodgoodSeasLong$species, 1, 3), substr(lvst_prodgoodSeasLong$species, 1, 4))
lvst_prodgoodSeasLong1$species <- ifelse(substr(lvst_prodgoodSeasLong1$species, 1, 2) %in% c("Po", "pi"), substr(lvst_prodgoodSeasLong1$species, 1, 3), substr(lvst_prodgoodSeasLong1$species, 1, 4))
lvst_prodgoodSeasLong2$species <- ifelse(substr(lvst_prodgoodSeasLong2$species, 1, 2) %in% c("Po", "pi"), substr(lvst_prodgoodSeasLong2$species, 1, 3), substr(lvst_prodgoodSeasLong2$species, 1, 4))
lvst_prodgoodSeasLong3$species <- ifelse(substr(lvst_prodgoodSeasLong3$species, 1, 2) %in% c("Po", "pi"), substr(lvst_prodgoodSeasLong3$species, 1, 3), substr(lvst_prodgoodSeasLong3$species, 1, 4))
lvst_prodgoodSeasLong4$species <- ifelse(substr(lvst_prodgoodSeasLong4$species, 1, 2) %in% c("Po", "pi"), substr(lvst_prodgoodSeasLong4$species, 1, 3), substr(lvst_prodgoodSeasLong4$species, 1, 4))
lvst_prodgoodSeasLong5$species <- ifelse(substr(lvst_prodgoodSeasLong5$species, 1, 2) %in% c("Po", "pi"), substr(lvst_prodgoodSeasLong5$species, 1, 3), substr(lvst_prodgoodSeasLong5$species, 1, 4))

lvst_prodgoodSeasLong <- left_join(lvst_prodgoodSeasLong, lvst_prodgoodSeasLong1)
lvst_prodgoodSeasLong <- left_join(lvst_prodgoodSeasLong, lvst_prodgoodSeasLong2)
lvst_prodgoodSeasLong <- left_join(lvst_prodgoodSeasLong, lvst_prodgoodSeasLong3)
lvst_prodgoodSeasLong <- left_join(lvst_prodgoodSeasLong, lvst_prodgoodSeasLong4)
lvst_prodgoodSeasLong <- left_join(lvst_prodgoodSeasLong, lvst_prodgoodSeasLong5)
lvst_prodgoodSeasLong$season <- "good"
lvst_prodgoodSeasLong[lvst_prodgoodSeasLong$Household.ID == "SEGO43" & lvst_prodgoodSeasLong3$species == "Pruv", c(6:9)] <- c(0, 2500, 40, 1) # data entry error - misaligned data

#Bad season
lvst_prodbadSeas <- read.csv('2_3_2b_Prd betail mauvaise saison.csv')
lvst_prodbadSeasLong <- gather(select(lvst_prodbadSeas, Household.ID, ends_with("prod"), ends_with("bat")), species, vol, c(ends_with("prod"), ends_with("bat"))) #No pig column in bad season
lvst_prodbadSeasLong1 <- gather(select(lvst_prodbadSeas, Household.ID, ends_with("cons"), -ends_with("concons")), species, cons_perc, ends_with("cons"))
lvst_prodbadSeasLong2 <- gather(select(lvst_prodbadSeas, Household.ID, ends_with("vent")), species, sold_perc, ends_with("vent"))
lvst_prodbadSeasLong3 <- gather(select(lvst_prodbadSeas, Household.ID, ends_with("pri"), ends_with("uni"), ends_with("riu")), species, price_unit, ends_with("pri"), ends_with("uni"), ends_with("riu"))
lvst_prodbadSeasLong4 <- gather(select(lvst_prodbadSeas, Household.ID, ends_with("perev")), species, COP_perc, ends_with("perev"))
lvst_prodbadSeasLong5 <- gather(select(lvst_prodbadSeas, Household.ID, ends_with("conrev")), species, controlsRevenue, ends_with("conrev"))
#A little bit of data cleaning
lvst_prodbadSeasLong$vol <- gsub("kg", "", lvst_prodbadSeasLong$vol)
lvst_prodbadSeasLong$numAnimals <- 0
lvst_prodbadSeasLong$numAnimals <- ifelse(tolower(lvst_prodbadSeasLong$vol) %in% c("1 mouton", "1 chèvre"), 1, lvst_prodbadSeasLong$num)
lvst_prodbadSeasLong$vol <- ifelse(tolower(lvst_prodbadSeasLong$vol) %in% c("1 mouton", "1 chèvre"), paramGoatSheepCarcassDebone*1, lvst_prodbadSeasLong$vol)
#Milk is l / day ave. assume an 270 d lactation and the good period is 50% of this lactation. 
lvst_prodbadSeasLong$vol <- ifelse(grepl("lait", lvst_prodbadSeasLong$species), as.numeric(lvst_prodbadSeasLong$vol) * paramLactLength*0.5, lvst_prodbadSeasLong$vol)

lvst_prodbadSeasLong$species <- ifelse(substr(lvst_prodbadSeasLong$species, 1, 2) == "Po", substr(lvst_prodbadSeasLong$species, 1, 3), substr(lvst_prodbadSeasLong$species, 1, 4))
lvst_prodbadSeasLong1$species <- ifelse(substr(lvst_prodbadSeasLong1$species, 1, 2) == "Po", substr(lvst_prodbadSeasLong1$species, 1, 3), substr(lvst_prodbadSeasLong1$species, 1, 4))
lvst_prodbadSeasLong2$species <- ifelse(substr(lvst_prodbadSeasLong2$species, 1, 2) == "Po", substr(lvst_prodbadSeasLong2$species, 1, 3), substr(lvst_prodbadSeasLong2$species, 1, 4))
lvst_prodbadSeasLong3$species <- ifelse(substr(lvst_prodbadSeasLong3$species, 1, 2) == "Po", substr(lvst_prodbadSeasLong3$species, 1, 3), substr(lvst_prodbadSeasLong3$species, 1, 4))
lvst_prodbadSeasLong4$species <- ifelse(substr(lvst_prodbadSeasLong4$species, 1, 2) == "Po", substr(lvst_prodbadSeasLong4$species, 1, 3), substr(lvst_prodbadSeasLong4$species, 1, 4))
lvst_prodbadSeasLong5$species <- ifelse(substr(lvst_prodbadSeasLong5$species, 1, 2) == "Po", substr(lvst_prodbadSeasLong5$species, 1, 3), substr(lvst_prodbadSeasLong5$species, 1, 4))

lvst_prodbadSeasLong <- left_join(lvst_prodbadSeasLong, lvst_prodbadSeasLong1)
lvst_prodbadSeasLong <- left_join(lvst_prodbadSeasLong, lvst_prodbadSeasLong2)
lvst_prodbadSeasLong <- left_join(lvst_prodbadSeasLong, lvst_prodbadSeasLong3)
lvst_prodbadSeasLong <- left_join(lvst_prodbadSeasLong, lvst_prodbadSeasLong4)
lvst_prodbadSeasLong <- left_join(lvst_prodbadSeasLong, lvst_prodbadSeasLong5)
lvst_prodbadSeasLong$vol <- as.numeric(lvst_prodbadSeasLong$vol)
lvst_prodbadSeasLong$season <- "bad"

#Join to one complete data frame
lvst_prodCompleteLong <- bind_rows(lvst_prodgoodSeasLong, lvst_prodbadSeasLong)

#more cleaning. Some households consume+sell something other than 100%
lvst_prodCompleteLong$sold_perc[lvst_prodCompleteLong$Household.ID == "SEBA04" & lvst_prodCompleteLong$species == "Oeuf" & lvst_prodCompleteLong$cons_perc == 100] <- 0
lvst_prodCompleteLong$sold_perc[lvst_prodCompleteLong$Household.ID == "SEBA24" & lvst_prodCompleteLong$species == "Pou" & lvst_prodCompleteLong$cons_perc == 100] <- 0
lvst_prodCompleteLong$sold_perc[lvst_prodCompleteLong$Household.ID == "SEBA50" & lvst_prodCompleteLong$species == "Blai" & lvst_prodCompleteLong$cons_perc == 75] <- 25
lvst_prodCompleteLong$sold_perc[lvst_prodCompleteLong$Household.ID == "SEBA39" & lvst_prodCompleteLong$species == "Oeuf" & lvst_prodCompleteLong$cons_perc == 11.11] <- 88.89 #from 88.88
lvst_prodCompleteLong$sold_perc[lvst_prodCompleteLong$Household.ID == "SEGO31" & lvst_prodCompleteLong$species == "Blai" & lvst_prodCompleteLong$cons_perc == 21.42] <- 78.58
lvst_prodCompleteLong$cons_perc[lvst_prodCompleteLong$Household.ID == "YATO04" & lvst_prodCompleteLong$species == "Oeuf" & lvst_prodCompleteLong$sold_perc == 0] <- 100
lvst_prodCompleteLong$cons_perc[lvst_prodCompleteLong$Household.ID == "SEGO14" & lvst_prodCompleteLong$species == "Prul" & lvst_prodCompleteLong$sold_perc == 0] <- 100

lvst_prodCompleteLong <- left_join(lvst_prodCompleteLong, select(paramLvstNutriVal, -edibleConvF))
#All animals enumerated (ie whole animals) are consumed not sold.
#lvst_prodCompleteLong$price_kg <- ifelse(lvst_prodCompleteLong$numAnimals >0, lvst_prodCompleteLong$numAnimals * lvst_prodCompleteLong$price_unit / lvst_prodCompleteLong$vol, lvst_prodCompleteLong$price_unit)
lvst_prodCompleteLong$price_kg <- ifelse(lvst_prodCompleteLong$species == "Oeuf", lvst_prodCompleteLong$price_unit * 20, lvst_prodCompleteLong$price_unit) #Assuming eggs are 20g each.
lvst_prodCompleteLong$volkg <- ifelse(lvst_prodCompleteLong$species == "Oeuf", lvst_prodCompleteLong$vol * 20, lvst_prodCompleteLong$vol) #Assuming eggs are 20g each.

#Convert to USD
lvst_prodCompleteLong$price_unit <- lvst_prodCompleteLong$price_unit * paramUSD

#Get livestock prices and COP 
lvstPriceCOP <- group_by(lvst_prodCompleteLong, fullName)
lvstPriceCOP[lvstPriceCOP < 0.0000000001] <- NA #for some reason 0.00 is different from 0
lvstPriceCOP$COP_percYa <- ifelse(substr(lvstPriceCOP$Household.ID, 1, 2) == "YA", lvstPriceCOP$COP_perc, NA)
lvstPriceCOP$COP_percSe <- ifelse(substr(lvstPriceCOP$Household.ID, 1, 2) == "SE", lvstPriceCOP$COP_perc, NA)
lvstPriceCOP <- summarise(lvstPriceCOP, price_kgM = median(price_kg, na.rm=T), COP_percM = median(COP_perc, na.rm=T), COP_percYaM = median(COP_percYa, na.rm=T), COP_percSeM = median(COP_percSe, na.rm=T), COP_percSeM = min(COP_percSe, na.rm=T))

lvst_prodCompleteLong <- left_join(lvst_prodCompleteLong, lvstPriceCOP)
#Easy imputation of milk price for one household. Milk is not sold in Yatenga, so median milk price is for Seno 
lvst_prodCompleteLong$price_unit <- ifelse(lvst_prodCompleteLong$price_unit ==0 & lvst_prodCompleteLong$species == "Blai" & lvst_prodCompleteLong$Household.ID == "SESE48", lvst_prodCompleteLong$price_kgM, lvst_prodCompleteLong$price_unit) 
#!COP for 12 obs that sold lvst = 0. This is interpreted as low input systems i.e. the respondent didn't need to spend anything on the animals except the cost of water ect.. Revised from 0 to 1 %
lvst_prodCompleteLong$COP_perc <- ifelse(lvst_prodCompleteLong$COP_perc ==0 & lvst_prodCompleteLong$sold_perc >0 & lvst_prodCompleteLong$vol > 0, 1, lvst_prodCompleteLong$COP_perc) 
#Still97 obs of producing and consuming livestock products. COP is not relevant as there is no revenue associated with this

#lvst_prodCompleteLong$cons_vol <- (lvst_prodCompleteLong$cons_perc/100) * lvst_prodCompleteLong$vol 
lvst_prodCompleteLong$value <- lvst_prodCompleteLong$vol * lvst_prodCompleteLong$price_unit
lvst_prodCompleteLong$totalProtein <- lvst_prodCompleteLong$volkg * lvst_prodCompleteLong$protein
lvst_prodCompleteLong$totalKcal <- lvst_prodCompleteLong$volkg * lvst_prodCompleteLong$kcal
lvst_prodCompleteLong$sold_value <- (lvst_prodCompleteLong$sold_perc/100) * lvst_prodCompleteLong$vol * lvst_prodCompleteLong$price_unit
lvst_prodCompleteLong$COP_value <- (lvst_prodCompleteLong$COP_perc/100) * lvst_prodCompleteLong$sold_value #Here COP is only used in relation to income
lvst_prodCompleteLong$cons_protein <- (lvst_prodCompleteLong$cons_perc/100) * lvst_prodCompleteLong$volkg * lvst_prodCompleteLong$protein
lvst_prodCompleteLong$cons_kcal <- (lvst_prodCompleteLong$cons_perc/100) * lvst_prodCompleteLong$volkg * lvst_prodCompleteLong$kcal
lvst_prodCompleteLong$femaleValControl <- ifelse(lvst_prodCompleteLong$controlsRevenue %in% c(2,4), lvst_prodCompleteLong$vol * lvst_prodCompleteLong$price_unit, 0)
lvst_prodCompleteLong$cons_proteinBadSeason <- ifelse(lvst_prodCompleteLong$season == "bad", lvst_prodCompleteLong$cons_protein, NA)
lvst_prodCompleteLong$cons_proteinGoodSeason <- ifelse(lvst_prodCompleteLong$season == "good", lvst_prodCompleteLong$cons_protein, NA)
lvst_prodCompleteLong$soldValBadSeason <- ifelse(lvst_prodCompleteLong$season == "bad", lvst_prodCompleteLong$sold_value, NA)
lvst_prodCompleteLong$soldValGoodSeason <- ifelse(lvst_prodCompleteLong$season == "good", lvst_prodCompleteLong$sold_value, NA)
lvst_prodCompleteLong$COPBadSeason <- ifelse(lvst_prodCompleteLong$season == "bad", lvst_prodCompleteLong$totalKcal, NA)
lvst_prodCompleteLong$COPGoodSeason <- ifelse(lvst_prodCompleteLong$season == "good", lvst_prodCompleteLong$totalKcal, NA)
lvst_prodCompleteLong$productsBadSeason <- ifelse(lvst_prodCompleteLong$season == "bad", as.character(lvst_prodCompleteLong$fullName), NA)
lvst_prodCompleteLong$productsGoodSeason <- ifelse(lvst_prodCompleteLong$season == "good", as.character(lvst_prodCompleteLong$fullName), NA)
lvst_prodCompleteLong$HDDScatBadSeason <- ifelse(lvst_prodCompleteLong$season == "bad", as.character(substr(lvst_prodCompleteLong$FAOcode, 1, 2)), NA)
lvst_prodCompleteLong$HDDScatGoodSeason <- ifelse(lvst_prodCompleteLong$season == "good", as.character(substr(lvst_prodCompleteLong$FAOcode, 1, 2)), NA)

lvst_prodCompleteLong$meatProtein <- ifelse(lvst_prodCompleteLong$FAOcode %in% c("07_006", "07_009", "07_033", "07_046 / 07_004"), lvst_prodCompleteLong$totalProtein, NA)
lvst_prodCompleteLong$dairyProtein <- ifelse(lvst_prodCompleteLong$FAOcode %in% c("10_001", "10_003", "10_005", "10_006", "11_001"), lvst_prodCompleteLong$totalProtein, NA)
lvst_prodCompleteLong$eggsProtein <- ifelse(lvst_prodCompleteLong$FAOcode == "08_001", lvst_prodCompleteLong$totalProtein, NA)


datLvstProd <- group_by(lvst_prodCompleteLong, Household.ID)
datLvstProd <- datLvstProd[datLvstProd$vol >0,] #Only include rows with a volume enumerated. This is for the count of products only.
datLvstProd <- summarise(datLvstProd, lvstTotalKcal = sum(totalKcal, na.rm=T), lvstTotalProtein = sum(totalProtein, na.rm=T), 
                         lvstTotalValue_US = sum(value, na.rm=T), lvstIncome_US = sum(sold_value, na.rm=T), 
                         lvstCOPvalue_US = sum(COP_value, na.rm=T), lvstConsProtein = sum(cons_protein, na.rm=T),
                         lvstConsKcal = sum(cons_kcal, na.rm=T), lvstFemaleControlVal  = sum(femaleValControl, na.rm=T),
                         lvstCons_proteinBadSeason = sum(cons_proteinBadSeason, na.rm=T), lvstCons_proteinGoodSeason = sum(cons_proteinGoodSeason, na.rm=T),
                         lvstSoldValBadSeason = sum(soldValBadSeason, na.rm=T), lvstSoldValGoodSeason = sum(soldValGoodSeason, na.rm=T),
                         lvstCOPBadSeason = sum(COPBadSeason, na.rm=T), lvstCOPGoodSeason = sum(COPGoodSeason, na.rm=T), lvstProdDiv = length(unique(fullName)),
                         lvstDivBad = length(na.omit(productsBadSeason)), lvstDivGood = length(na.omit(productsGoodSeason)),
                         lvstHDDSDivBad = length(unique(na.omit(HDDScatBadSeason))), lvstHDDSDivGood = length(unique(na.omit(HDDScatGoodSeason))),
                         lvstHDDSDiv = length(unique(na.omit(substr(FAOcode, 1,2)))),
                         dairyProtein = sum(dairyProtein, na.rm = T),
                         meatProtein = sum(meatProtein, na.rm=T),
                         eggsProtein = sum(eggsProtein, na.rm=T))

datLvstProd$lvstSoldVal_perc <- datLvstProd$lvstIncome_US / datLvstProd$lvstTotalValue_US
datLvstProd$lvstCOPval_perc <- datLvstProd$lvstCOPvalue_US / datLvstProd$lvstTotalValue_US
datLvstProd$lvstFemaleControlVal_perc <- datLvstProd$lvstFemaleControlVal / datLvstProd$lvstTotalValue_US
datLvstProd$lvstConsProtein_perc <- datLvstProd$lvstConsProtein / datLvstProd$lvstTotalProtein
datLvstProd$lvstConsKcal_perc <- datLvstProd$lvstConsKcal / datLvstProd$lvstTotalKcal
datLvstProd$badSeasonLvstIncome_perc <- datLvstProd$lvstSoldValBadSeason / datLvstProd$lvstIncome_US


#rm(list= setdiff(ls(), c("dat", "datAdulteq", "datIncome", "datLand", "datLvstTLU", "datLvstLive", "datLvstProd", "datCrop")))
