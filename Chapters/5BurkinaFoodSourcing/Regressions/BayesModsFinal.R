library(brms)
library(glmmTMB)
library(pwr)

pwr.f2.test(u=17, v = 400, power = 0.8, sig.level = 0.05) #7 predictors, 400 obs, ES of 0.01. 
pwr.t.test(n=400, sig.level = 0.05, type = "two.sample", alternative = "two.sided", power = 0.8)

dat$HFIASbin <- ifelse(dat$HFIAS_status_bad %in% c("ModeratelyFI", "MildlyFI", "FoodSecure"), 0, 1)

dat$logOffIncome <- log1p(dat$offIncome_US)
dat$logOffIncome_US500 <- log1p(dat$offIncome_US500)
dat$logFarmIncomeUS500 <- log1p(dat$farmIncomeUS500)
dat$logCropConsKcal1Aeq <- log1p(dat$cropConsKcal1Aeq)
dat$logLvstConsprotein1Aeq <- log1p(dat$lvstConsprotein1Aeq)
dat$femaleIncomeControlBin <- factor(ifelse(dat$femaleIncomeControl10Pc <4, 0, 1))
dat$logGrossIncome <- log1p(rowSums(dat[, c("offIncome_US", "liveAnimalVal_US", "lvstIncome_US", "cropIncome_US")], na.rm=T))
dat$logLvstCOP <- log1p(rowSums(dat[, c("lvstCOPvalue_US", "COPliveAnimalVal_US")])) 
dat$logFarmIncomeDetailedCalc <- log1p(rowSums(dat[, c("liveAnimalVal_US", "lvstIncome_US", "cropIncome_US")], na.rm=T))
dat$farmIncomeDetailedCalc <- (rowSums(dat[, c("liveAnimalVal_US", "lvstIncome_US", "cropIncome_US")], na.rm=T))
dat$farmIncomeDetailedCalcUS1000 <- dat$farmIncomeDetailedCalc / 1000
dat$offIncomeUS1000 <- dat$offIncome_US/1000
dat$grossIncomeUS1000 <- rowSums(dat[, c("offIncome_US", "liveAnimalVal_US", "lvstIncome_US", "cropIncome_US")], na.rm=T)/1000


dat$lvstCOP_totalVal_perc <- rowSums(dat[, c("lvstCOPvalue_US", "COPliveAnimalVal_US")], na.rm=T) / rowSums(dat[, c("liveAnimalVal_US", "lvstIncome_US")], na.rm=T)
dat$lvstCOPHigh <- factor(ifelse(dat$lvstCOP_totalVal_perc > 0.4, "high", "low"))
dat$lvstCOPHigh[is.na(dat$lvstCOPHigh)] <- "low"
dat$farmConsKcal1Aeq[is.na(dat$farmConsKcal1Aeq)] <- 0
dat$logTLU <- log1p(dat$tlu)

dat$HFIAS_status_bad <- factor(dat$HFIAS_status_bad, levels = c("SeverelyFI", "ModeratelyFI", "MildlyFI", "FoodSecure"))
dat$HFIAS_status_good <- factor(dat$HFIAS_status_good, levels = c("SeverelyFI", "ModeratelyFI", "MildlyFI", "FoodSecure"))
dat$HFIAS_status_badBin <- ifelse(dat$HFIAS_status_bad =="SeverelyFI", "SeverelyFI", "zother")
dat$HFIAS_status_goodBin <- ifelse(dat$HFIAS_status_good =="SeverelyFI", "SeverelyFI", "zother")


###Regressions
rm(list=setdiff(ls(), "dat"))
dat$weightProv <- NULL
weightsVillage <- data.frame(Commune = c("Bani", "Dori", "Gorgadji", "Seytenga", "Namissiguima", "Ouahigouya", "OULA"), weightProv = c(1030/6113*400/50, 655/6113*400/50, 823/6113*400/50, 851/6113*400/50, 687/6113*400/50, 1457/6113*400/100, 610/6113*400/50)) 
dat <- left_join(dat, weightsVillage)
dat$siteProv <- factor(dat$siteProv, levels = c("Yatenga", "Seno"), ordered =T)
modHFIASbad <-  brm(HFIAS_status_badBin|weights(weightProv) ~  grossIncomeUS1000 + farmConsKcal1Aeq + cropDiv + lvstDiv + aidBin + siteProv + grossIncomeUS1000*siteProv + (1 | Village), data = dat, prior = c(set_prior("normal(0.1, 1)", class = "b")), family = bernoulli(link = 'logit'), control = list(adapt_delta = 0.99), chains = 3, iter = 5000, warmup = 500) 
modHFIASgood <-  brm(HFIAS_status_goodBin|weights(weightProv) ~ grossIncomeUS1000 + farmConsKcal1Aeq + cropDiv + lvstDiv + aidBin + siteProv + (1 | Village), data = dat, prior = c(set_prior("normal(0.1, 1)", class = "b")), family = bernoulli(link = 'logit'), control = list(adapt_delta = 0.99), chains = 3, iter = 5000, warmup = 500)

modHDDSBad <- brm(HDDS_bad|weights(weightProv) ~ farmIncomeDetailedCalcUS1000*offIncBin*siteProv + cropDiv + lvstDiv + femaleIncomeControlBin + siteProv + (1 | Village), data = subset(dat, farmIncomeDetailedCalc < 15000), prior = c(set_prior("student_t(3,0.5,5)", class = "b")), family = negbinomial(), control = list(adapt_delta = 0.99, max_treedepth =12), chains = 3, iter = 5000, warmup = 500)
modHDDSGood <- brm(HDDS_good|weights(weightProv) ~ farmIncomeDetailedCalcUS1000 + offIncBin + siteProv + cropDiv + lvstDiv + femaleIncomeControlBin + siteProv + (1 | Village), data = subset(dat, farmIncomeDetailedCalc < 15000), prior = c(set_prior("student_t(3,0.5,5)", class = "b")), family = negbinomial(), control = list(adapt_delta = 0.99, max_treedepth =12), chains = 3, iter = 5000, warmup = 500)

b <- marginal_effects(modHDDSBad, effects = ("farmIncomeDetailedCalcUS1000:offIncBin"), conditions = data.frame(siteProv = c("Seno", "Yatenga")))$`farmIncomeDetailedCalcUS1000:offIncBin`
ggplot(b, aes(farmIncomeDetailedCalcUS1000,estimate__, colour = offIncBin, fill = offIncBin)) + geom_line() + geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = .1, colour = NA) + facet_wrap(~siteProv) + xlab("Gross farm income (log USD)") + coord_cartesian(ylim = c(1, 9)) + ylab("Diet Diversity Score in lean season") + scale_x_continuous(limits = c(0,5)) + scale_y_continuous(breaks = c(1:9)) + themeLegendTrunc + guides(colour=guide_legend(title="Off-farm income"), fill = FALSE)

modHDDSfarmBad <- brm(HDDS_bad_farm|weights(weightProv) ~   + cropDiv*siteProv + lvstDiv + (1 | Village), data = dat, prior = c(set_prior("student_t(3,0.5,5)", class = "b")), family = negbinomial(), control = list(adapt_delta = 0.99), chains = 3, iter = 4000, warmup = 500)

modHDDSfarmGood <- brm(HDDS_good_farm|weights(weightProv) ~  + cropDiv + lvstDiv + siteProv + (1 | Village), data = dat, prior = c(set_prior("student_t(3,0.5,5)", class = "b")), family = negbinomial(), control = list(adapt_delta = 0.99), chains = 3, iter = 4000, warmup = 500)

modHDDSpurchBad <- brm(HDDS_bad_purch|weights(weightProv) ~ farmIncomeDetailedCalcUS1000 + offIncBin + femaleIncomeControlBin + siteProv + farmIncomeDetailedCalcUS1000*offIncBin*siteProv + (1 | Village), data = dat, prior = c(set_prior("student_t(3,0.5,5)", class = "b")), family = negbinomial(), control = list(adapt_delta = 0.99), chains = 4, iter = 4000, warmup = 500)
modHDDSpurchGood <- brm(HDDS_good_purch|weights(weightProv) ~ farmIncomeDetailedCalcUS1000 + offIncBin + femaleIncomeControlBin + siteProv + (1 | Village), data = dat, prior = c(set_prior("student_t(3,0.5,5)", class = "b")), family = negbinomial(), control = list(adapt_delta = 0.99), chains = 4, iter = 4000, warmup = 500)

modHDDSpurchGood2 <- brm(HDDS_good_purch|weights(weightProv) ~ logFarmIncome + offIncBin + femaleIncomeControlBin + siteProv + logFarmIncome*offIncBin + (1 | Village), data = dat, prior = c(set_prior("student_t(3,0.5,5)", class = "b")), family = negbinomial(), control = list(adapt_delta = 0.99), chains = 4, iter = 4000, warmup = 500)

save.image(file = "resubmitRegress_pooledv9.RData")


