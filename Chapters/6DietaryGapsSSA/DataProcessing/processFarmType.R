##Farm type based on specialisation/diversification and livestock component
#1-2 crops cultivated = specialised. Only counting crops that are at least xx% of land
#3+ = diversified
#>1.5 TLU = livestock component

###Yet another farm type, this time using MHDD to make sure they do not source more categories from farm than they produce
dat <- rowwise(mutate(dat, cropDiversity = max(HDDSonFarmCropsBadDaily, HDDSonFarmCropsGoodDaily, cropProdDiv, na.rm=T)))
dat$farmType <- ifelse(dat$cropDiversity > 2 | dat$homegarden == "y", "Diverse cropping",
                       ifelse(dat$cropDiversity < 3, "Specialised cropping", NA))

dat$farmType <- ifelse(dat$tlu > 1.49, paste(dat$farmType, "& livestock"), dat$farmType)
dat$farmType <- ifelse(dat$farmType == "NA & livestock" & dat$tlu > 1.49, "Specialised cropping & livestock", dat$farmType)
