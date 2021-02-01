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

#remplace na par 0
numeric_cols <- colnames(covid)[sapply(covid, is.numeric)]
character_cols <- colnames(covid)[sapply(covid, is.character)]
covid[,numeric_cols][is.na(covid[,numeric_cols])] <- 0
covid[,character_cols][is.na(covid[,character_cols])] <- ""







