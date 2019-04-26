library(wbstats)

wbVars <- wbcache(lang = "en")

#PA.NUS.PRVT.PP = "PPP conversion factor, private consumption (LCU per international $)"
PPP <- wb(country = "all", indicator = "PA.NUS.PRVT.PP")
PPPcurrent <- subset(PPP, date == 2016 | (date == 2013 & country == "Congo, Dem. Rep."))
#PPPcurrent <- group_by(PPP, country)


#Exchange  DPANUSSPB = Exchange rate, new LCU per USD extended backw
exchange <- wb(indicator = "DPANUSSPB")
exchangeCurrent <- subset(exchange, date == 2016)

#All cross checked with 2011 values from http://databank.worldbank.org/data/reports.aspx?source=international-comparison-program-(icp)-2011# Using individual expenditure consumption by household
#a <- wbindicators()
#a$indicatorDesc[a$indicatorID == "DPANUSSPB"]
