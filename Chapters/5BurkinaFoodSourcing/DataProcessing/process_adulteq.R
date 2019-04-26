#####################################################
#
#
#
#
#####################################################
library(dplyr)
library(tidyr)

HHdat<-read.csv('1_1_2_Membres des menage.csv')
HHhead<-read.csv('1_1_2 Information generale.csv')

#Add HH head to the dataset
HHdat <- left_join(HHdat, select(HHhead, Household.ID, HH0age = Men_age, HH0S = Men_sexe))

HHdatLong <- select(HHdat, -contains("nom"), -contains("Nivedu"), -Pays, -Village, -Commune, -Date, -Men_chef)
HHdatLong1 <- gather(select(HHdatLong, -ends_with("S")), var, age, (contains("age")))
HHdatLong2 <- gather(select(HHdatLong, -ends_with("age")), var, gender, (ends_with("S")))
HHdatLong1$var <- gsub('[a-zA-Z]', "", HHdatLong1$var)
HHdatLong2$var <- gsub('[a-zA-Z]', "", HHdatLong2$var)
HHdatLong <- left_join(HHdatLong1, HHdatLong2)
HHdatLong$gender <- gsub("20", "2", HHdatLong$gender)
HHdatLong$gender <- ifelse(HHdatLong$gender == 0 & HHdatLong$age > 0, "1", HHdatLong$gender) #assume male if not enumerated. 236 obs members of HHs. 4 over the age of 13m As the majority are under 13, gender will not influence the coefficient for these HH members 

#Based on energy requirements relative to the average of males and felames between 25 and 50
adult_eq_param <- data.frame(gender = c(rep("1", 100), rep("2", 100)), age = rep(1:100, 2), coeffAdeq = c(rep(0.5, 13), rep(1.02, 19-13), rep(1.1, 59-19), rep(0.98, 100-59), rep(0.5, 13), rep(0.8, 19-13), rep(0.9, 59-19), rep(0.84, 100-59)))


HHdatLong <- left_join(HHdatLong, adult_eq_param)
HHdatLong <- group_by(HHdatLong, Household.ID)
datAdulteq <- summarise(HHdatLong, adult_equivalent = sum(coeffAdeq, na.rm=T))

rm(HHdat, HHdatLong, HHdatLong1, HHdatLong2)

