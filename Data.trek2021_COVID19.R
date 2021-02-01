#From https://covid19datahub.io/articles/api/r.html
#Reference : #Guidotti, E., Ardia, D., (2020), "COVID-19 Data Hub", Journal of Open Source Software 5(51):2376, doi: 10.21105/joss.02376.

#Peut-on prévoir la transmission de la COVID19 en fonction du PIB d'un pays et de la mobilité de sa population.

#Données sur la covid19 en format tibble (tidyverse)
library(COVID19)
covid <- covid19(level = 1)

#World Bank Open Data (PIB)
wb <- ("gdp" = "NY.GDP.PCAP.CD")
PIB  <- covid19(wb = wb)

#Google Mobility Reports (très long à mettre à jour)
gmr <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
GoogleMobility   <- covid19(gmr = gmr)

#Apple Mobility Reports
amr <- "https://covid19-static.cdn-apple.com/covid19-mobility-data/"
amr <- paste0(amr, "2025HotfixDev12/v3/en-us/applemobilitytrends-2021-01-24.csv")
AppleMobility   <- covid19(amr = amr)

# Dimension des données
dim(covid)
dim(PIB)
dim(GoogleMobility)
dim(AppleMobility)

#merge Apple with PIB
PIB <- PIB[c("date","id","NY.GDP.PCAP.CD")]
covid <- merge(AppleMobility,PIB, by=c("date","id"), sort = FALSE)

#remplace na par 0
numeric_cols <- colnames(covid)[sapply(covid, is.numeric)]
character_cols <- colnames(covid)[sapply(covid, is.character)]
covid[,numeric_cols][is.na(covid[,numeric_cols])] <- 0
covid[,character_cols][is.na(covid[,character_cols])] <- ""

#ajout colonne cas par M d'habitant
covid$confirmed1M <- round(covid$confirmed / covid$population * 1000000, digits = 2)

# Sommaire par pays
sommaire <- aggregate(data.frame(Pays = covid$administrative_area_level_1, Confirmé_M = covid$confirmed1M, Population = covid$population, PIB = covid$NY.GDP.PCAP.CD), list(value = covid$id), max)

sommaire <- sommaire[order(-sommaire$Confirmé_M),]
sommaire$rangconfirme <- seq.int(nrow(sommaire))
sommaire <- sommaire[order(sommaire$PIB),]
sommaire$rangPIB <- seq.int(nrow(sommaire))
sommaire <- sommaire[order(sommaire$value),]





