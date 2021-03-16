#Data.trek project 2021

#Peut-on prévoir la transmission de la COVID19 en fonction du PIB d'un pays et/ou de la mobilité de sa population ?



#--------------------------------Import des données --------------------------

library(COVID19) #format tibble (tidyverse)
covid <- covid19(raw = TRUE, level = 1)

#World Bank Open Data (PIB)
wb <- ("gdp" = "NY.GDP.PCAP.CD")
PIB  <- covid19(raw = TRUE, wb = wb) #doit mettre raw to false et enlever manipulation avec zero

#Google Mobility Reports (très long à mettre à jour- le faire manuellement)
#gmr <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
gmr <- "C:/Users/ericd/Documents/Formation/Projet Continuum/Data.trek 2021/Projet COVID19/Google/Global_Mobility_Report.csv"
GoogleMobility   <- covid19(raw = TRUE, gmr = gmr)


#Apple Mobility Reports - https://covid19.apple.com/mobility - (100 égale la référence donc si moins va être sous 100)
amr <- "https://covid19-static.cdn-apple.com/covid19-mobility-data/"
amr <- paste0(amr, "2103HotfixDev19/v3/en-us/applemobilitytrends-2021-03-14.csv")
AppleMobility   <- covid19(raw = TRUE, amr = amr)





#---------------------------Ajout manipulation ---------------------------

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

#ajout de colonnes de calcul cas par M d'habitant 
covid$confirmed1M <- round(covid$confirmed / covid$population * 1000000, digits = 2)

# Sommaire par pays
sommaire <- aggregate(data.frame(Pays = covid$administrative_area_level_1, 
                                 Confirme_M = covid$confirmed1M, Confirme = covid$confirmed, Tests = covid$tests, 
                                 Population = covid$population, PIB = covid$NY.GDP.PCAP.CD),
                                list(value = covid$id), FUN = max)

#******* Ajout taux positivité au sommaire
sommaire$positivite <- sommaire$Confirme / sommaire$Tests

##ajout colonne rang PIB et rang confirmé
sommaire <- sommaire[order(-sommaire$Confirme_M),]
sommaire$rangconfirme <- seq.int(nrow(sommaire))
sommaire <- sommaire[order(sommaire$PIB),]
sommaire$rangPIB <- seq.int(nrow(sommaire))
sommaire <- sommaire[order(sommaire$value),]

# Sommaire Google mobility par pays
numeric_cols <- colnames(GoogleMobility)[sapply(GoogleMobility, is.numeric)]
character_cols <- colnames(GoogleMobility)[sapply(GoogleMobility, is.character)]
GoogleMobility[,numeric_cols][is.na(GoogleMobility[,numeric_cols])] <- 0
GoogleMobility[,character_cols][is.na(GoogleMobility[,character_cols])] <- ""

sommairegoogle <- aggregate(data.frame(retailMean = GoogleMobility$retail_and_recreation_percent_change_from_baseline,
                                       transitMean = GoogleMobility$transit_stations_percent_change_from_baseline,
                                       groceryMean = GoogleMobility$grocery_and_pharmacy_percent_change_from_baseline,
                                       parksMean = GoogleMobility$parks_percent_change_from_baseline,
                                       workMean = GoogleMobility$workplaces_percent_change_from_baseline, 
                                       homeMean = GoogleMobility$residential_percent_change_from_baseline
                                       ), list(value = GoogleMobility$administrative_area_level_1), mean)



#Graphique PIB par habitant pour tous les pays et Cas confirmés de COVID19 par million d'habitant
library(ggplot2)
library(patchwork)
p1 <- ggplot(data = sommaire, aes( x= rangPIB, y= PIB )) + geom_point() +
 geom_hline(yintercept=mean(sommaire$PIB), linetype="dashed", color = "red") + 
  labs(title = "PIB par habitant pour tous les pays", tag = "A") +  labs(caption = "(--- PIB moyen)") +
  labs(x = "Rang PIB / pays (ordre croissant de richesse)") + labs(y = "PIB par hab. (USD)") + 
  theme(plot.caption = element_text(color = "red", face = "italic"))

library(scales)
p2 <- ggplot(data = subset(sommaire, PIB > 0), aes( x= rangPIB, y= Confirme_M )) + geom_point() +
  geom_hline(yintercept=mean(sommaire$Confirme_M), linetype="dashed", color = "red") +
  scale_y_continuous(labels = number_format()) +
  labs(title = "Cas confirmés de COVID19 par million d'habitant", tag = "B") +
  labs(caption = "(--- Cas moyen covid19 par M/hab.)") + labs(x = "Rang PIB / pays (ordre croissant de richesse)")  +
  labs(y = "Cas covid19 (M/hab.)") + theme(plot.caption = element_text(color = "red", face = "italic"))

p = p1 / p2
p
#ggsave("~/R/Covid.jpg", p, width = 20, height = 10, dpi = 600)

#Graphique rang PIB vs taux positivité ****88 pays avec zero test*****
ggplot(data = subset(sommaire, PIB > 0 & Tests > 1000 & positivite < 0.80), aes( x= rangPIB, y= positivite )) + geom_point() +
  scale_y_continuous(labels = number_format()) +  labs(title = "Taux positivité de covid19 par PIB") +
  labs(x = "Rang PIB / pays (ordre croissant de richesse)")  +  labs(y = "Taux positivite") 

#environ 86 pays sans tests donc ratio de positivité difficile à évaluer
#******** vérifier si ces pays ont des rapports de mobilité de la population ????? **********
test <- sommaire[sommaire$Tests == 0,]
dim(test)

#Graphique PIB vs taux positivité (si PIB = 0 et test < 1000 enleve données)
ggplot(data = subset(sommaire, PIB > 0 & Tests > 1000 & positivite < 0.80), aes( x= PIB, y= positivite )) + geom_point() + 
  labs(title = "PIB vs taux positivité") + labs(x = "PIB par habitant") + labs(y = "Positivité") +
  scale_y_continuous(labels = number_format()) + scale_x_continuous(labels = number_format()) +
  geom_vline(xintercept=mean(sommaire$PIB), linetype="dashed", color = "red")

#Graphique PIB vs cas confirmé
ggplot(data = subset(sommaire, PIB > 0), aes( x= PIB, y= Confirme_M )) + geom_point() + 
  labs(title = "PIB vs cas confirmés") + labs(x = "PIB par habitant") + labs(y = "Cas confirmés (M/hab.)") +
  scale_y_continuous(labels = number_format()) + scale_x_continuous(labels = number_format())


# TOP10 des cas sous la moyenne, PIB sous la moyennne
test <- subset(sommaire, PIB > 0 & PIB < mean(sommaire$PIB) & Confirme_M < mean(sommaire$Confirme_M))
head(test[order(test$rangPIB),],10)
dim(test)


library(plotly)
g <- subset(sommaire, PIB > 0)

fig <- plot_ly(
  g, type="scatter", mode = "markers", x= ~PIB, y= ~Confirme_M,
  color = ~PIB, size = ~PIB
  ) 
  
fig







#----------------------------Données au Canada ------------------------------------

#creation données combinées Apple + google + PIB + covid au Canada
canada <- covid19(country = "CAN", raw = TRUE, level = 1, verbose = FALSE, amr = amr)
gmr <- "C:/Users/ericd/Documents/Formation/Projet Continuum/Data.trek 2021/Projet COVID19/Google/Global_Mobility_Report.csv"
test <- covid19(country = "CAN", raw = TRUE, level = 1, verbose = FALSE, gmr = gmr)
test <- test[c("date","id","retail_and_recreation_percent_change_from_baseline",
               "transit_stations_percent_change_from_baseline", "grocery_and_pharmacy_percent_change_from_baseline",
               "parks_percent_change_from_baseline","workplaces_percent_change_from_baseline",
               "residential_percent_change_from_baseline")]

canada <- merge(canada, test, by=c("date","id"), sort = FALSE)

library(tidyverse)
canada$dailyconfirmed <- diff(c(0,canada$confirmed )) 


par(mfrow = c(3, 3))
plot(canada$date, canada$dailyconfirmed) + plot(canada$date, canada$retail_and_recreation_percent_change_from_baseline)+
  abline(h = 0) + plot(canada$date, canada$transit_stations_percent_change_from_baseline) + 
  abline(h = 0) + plot(canada$date, canada$parks_percent_change_from_baseline) + 
  abline(h = 0)+ plot(canada$date, canada$workplaces_percent_change_from_baseline)+
  abline(h = 0)+ plot(canada$date, canada$transit ) + abline(h = 100) + plot(canada$date, canada$walking )+
  abline(h = 100) + plot(canada$date, canada$driving ) + abline(h = 100)




#----------------------------Données Québec ------------------------------------

#creation données combinées Apple + google + PIB + covid au Quebec
quebec <- covid19(country = "CAN", raw = TRUE, level = 2, verbose = FALSE, amr = amr)
gmr <- "C:/Users/ericd/Documents/Formation/Projet Continuum/Data.trek 2021/Projet COVID19/Google/Global_Mobility_Report.csv"
test <- covid19(country = "CAN", raw = TRUE, level = 2, verbose = FALSE, gmr = gmr)
test <- test[c("date","id","retail_and_recreation_percent_change_from_baseline",
               "transit_stations_percent_change_from_baseline", "grocery_and_pharmacy_percent_change_from_baseline",
               "parks_percent_change_from_baseline","workplaces_percent_change_from_baseline",
               "residential_percent_change_from_baseline")]

quebec <- merge(quebec, test, by=c("date","id"), sort = FALSE)
quebec <- quebec[quebec$administrative_area_level_2 == "Quebec",]

#Import données des tests via INSPQ car package a un outlier de 2.8M - https://www.inspq.qc.ca/covid-19/donnees
test <- read.table("C:/Users/ericd/Documents/Formation/Projet Continuum/Data.trek 2021/Projet COVID19/Covid/INSPQ - Graphique 4.3 - page_principal.csv", sep = ",", header= TRUE)
colnames(test) <- c("date", "dailytest")
test$date <- as.Date(test$date)
quebec <- merge(quebec, test, by = "date", sort = FALSE, all = TRUE)
quebec <- quebec[order(quebec$date),] 
row.names(quebec) <- NULL

#remplace na par 0
numeric_cols <- colnames(quebec)[sapply(quebec, is.numeric)]
character_cols <- colnames(quebec)[sapply(quebec, is.character)]
quebec[,numeric_cols][is.na(quebec[,numeric_cols])] <- 0
quebec[,character_cols][is.na(quebec[,character_cols])] <- ""

quebec <- subset(quebec, administrative_area_level_2 == 'Quebec')
quebec$dailyconfirmed <- diff(c(0,quebec$confirmed )) 
quebec$positivite <- quebec$dailyconfirmed / quebec$dailytest

par(mfrow = c(3, 3))
plot(quebec$date, quebec$dailyconfirmed) + plot(quebec$date, quebec$positivite) + plot(quebec$date, quebec$retail_and_recreation_percent_change_from_baseline)+
  abline(h = 0) + plot(quebec$date, quebec$transit_stations_percent_change_from_baseline) + 
  abline(h = 0) + plot(quebec$date, quebec$parks_percent_change_from_baseline) + 
  abline(h = 0)+ plot(quebec$date, quebec$workplaces_percent_change_from_baseline)+
  abline(h = 0)+ plot(quebec$date, quebec$transit ) + abline(h = 100) + plot(quebec$date, quebec$walking )+
  abline(h = 100) + plot(quebec$date, quebec$driving ) + abline(h = 100)


ggplot(data = quebec, aes(x= date, y= positivite )) + geom_point() + 
  labs(title = "Positivité Québec") + labs(x = "date") + labs(y = "taux positivité") 

# Cleansing of outliers 
par(mfrow = c(1, 1))
outliers <- boxplot(quebec$positivite)$out
outliers #outliers value
which(quebec$positivite %in% outliers) #outliers row number

quebec$positivite[is.na(quebec$positivite)] <- 0
quebec$positivite[quebec$positivite == "Inf"] <- 0
quebec$positivite[quebec$positivite < 0] <- 0

outliers <- boxplot(quebec$dailytest)$out
outliers #outliers value
which(quebec$dailytest %in% outliers) #outliers row number

quebec$dailytest[quebec$dailytest < 0] <- 0


# graph
library(plotly)
g <- subset(quebec, positivite > 0)

fig <- plot_ly(
  g, type="scatter", mode = "markers", x= ~date, y= ~dailyconfirmed,
  color = ~administrative_area_level_2, size = ~dailyconfirmed
) 

fig


library(plotly)
g <- subset(quebec, positivite > 0)
p1 <- plot_ly(g, x= ~date, y= ~positivite) %>%
  add_trace(type="scatter", mode = "markers",  name = 'positivite') 

p2 <- plot_ly(g, x= ~date, y= ~dailyconfirmed) %>%
  add_trace(type="scatter", mode = "markers",   name = 'par jour')
p <- subplot(p1,p2, nrows = 2, shareX = TRUE)  
p



# Compute descriptive statistics
library(pastecs)
test <- format(stat.desc(quebec), big.mark = ",", digits = 2, scientific = FALSE)







#-------------------------Données statistiques ------------------------------------


##**** vérifier les suppositions de normalité des données voir cours stat *************
dataValidation <- quebec[,"dailytest"]
par(mfrow = c(1, 1))
plot(dataValidation)
dataValidation <- dataValidation[order(dataValidation)]
summary((dataValidation))
NROW(dataValidation)
sum(is.na(dataValidation))
sum(dataValidation == 0)

plot(dataValidation)

#*** Test de student pour vérifier la distribution normale****
# H0 : moyenne = 30 000
# Ha : moyenne <> 30 000
# selon INSPQ  : 33 435 prélèvements réalisés le 22 février

var1 <- dataValidation
var1 <- var1[var1 > 0] #remove data with zero
t.test(x = var1, mu = 30000, alternative = "two.sided")
res <- var1 - mean(var1)
library(nortest)
ad.test(res)
par(mfrow = c(2, 1))
qqnorm(res, main = "Graphique quantile-quantile", ylab = "Quantiles de l'échantillon", xlab = "Quantiles théoriques")
qqline(res)

library("car")
qqPlot(var1, main = "Graphique quantile-quantile", xlab = "Quantiles théoriques", ylab = "Tests effectués", 
       distribution = "norm")


library(e1071) 
kurtosis(dataValidation, na.rm = FALSE, type = 3)  #normal = 0
skewness(dataValidation, na.rm = FALSE, type = 3)  #normal = 0

  
#Histogramme avec courbe normale
choix <- dataValidation #données complètes
choix <- var1 #données sans les zéros

par(mfrow = c(1, 1))
h<-hist(choix, breaks=50, col="red", xlab="x", main="Histogram with Normal Curve")
xfit<-seq(min(choix),max(choix),length=40)
yfit<-dnorm(xfit,mean=mean(choix),sd=sd(choix))
yfit <- yfit*diff(h$mids[1:2])*length(choix)
lines(xfit, yfit, col="blue", lwd=2)

#boxplot avec outliers
par(mfrow = c(1, 1))
outliers <- boxplot(dataValidation)$out

boxplot(dataValidation,
        main = "Boxplot",
        xlab = "x",
        ylab = "y",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE)
mtext(paste("Outliers: ", paste(outliers, collapse=", ")), cex=0.8)


outliers #outliers value
which(dataValidation %in% outliers) #outliers row number


library(outliers)
grubbs.test(dataValidation, type=11)


#outliers detection 99%
lower_limit <- quantile(dataValidation, 0.05)
upper_limit <- quantile(dataValidation, 0.95)
outliers <- which(dataValidation < lower_limit | dataValidation > upper_limit)
dataValidation[outliers]


#données Quebec colonne test / confirmed / positivite

summary(quebec$dailytest)
summary(quebec$tests)
summary(quebec$positivite)






#------------------------------- Analyse mobilité ----------------------------------------

cor(quebec$confirmed, quebec$transit_stations_percent_change_from_baseline)
cor(quebec$confirmed, quebec$parks_percent_change_from_baseline)
cor(quebec$confirmed, quebec$retail_and_recreation_percent_change_from_baseline)
cor(quebec$confirmed, quebec$grocery_and_pharmacy_percent_change_from_baseline)
cor(quebec$confirmed, quebec$workplaces_percent_change_from_baseline)
cor(quebec$confirmed, quebec$residential_percent_change_from_baseline)
cor(quebec$confirmed, quebec$driving)
cor(quebec$confirmed, quebec$walking)
cor(quebec$confirmed, quebec$transit)
cor(quebec$positivite, quebec$parks_percent_change_from_baseline)
cor(quebec$positivite, quebec$transit_stations_percent_change_from_baseline)
cor(quebec$positivite, quebec$retail_and_recreation_percent_change_from_baseline)
cor(quebec$positivite, quebec$grocery_and_pharmacy_percent_change_from_baseline)
cor(quebec$positivite, quebec$workplaces_percent_change_from_baseline)
cor(quebec$positivite, quebec$residential_percent_change_from_baseline)
cor(quebec$positivite, quebec$driving)
cor(quebec$positivite, quebec$walking)
cor(quebec$positivite, quebec$transit)


par(mfrow = c(2, 1))
plot( quebec$date, quebec$dailyconfirmed)
plot( quebec$date, quebec$parks_percent_change_from_baseline) + abline(h = 0)

plot( quebec$date, quebec$positivite)
plot( quebec$date, quebec$retail_and_recreation_percent_change_from_baseline) + abline(h = 0)


var1col <- "positivite"
var2col <- "retail_and_recreation_percent_change_from_baseline"

p1 <- subset(quebec, date > "2020-05-01" & date < "2020-07-01" , select=c(var1col ))
p2 <- subset(quebec, date > "2020-05-01" & date < "2020-07-01", select = c(var2col))
cor(p1, p2)

p1 <- subset(quebec, date > "2020-05-07" & date < "2020-07-07" , select=c(var1col ))
p2 <- subset(quebec, date > "2020-05-01" & date < "2020-07-01", select = c(var2col))
cor(p1, p2)

p1 <- subset(quebec, date > "2020-05-14" & date < "2020-07-14" , select=c(var1col ))
p2 <- subset(quebec, date > "2020-05-01" & date < "2020-07-01", select = c(var2col))
cor(p1, p2)

p1 <- subset(quebec, date > "2020-08-14" & date < "2020-11-14" , select=c(var1col ))
p2 <- subset(quebec, date > "2020-08-01" & date < "2020-11-01", select = c(var2col))
cor(p1, p2)

p1 <- subset(quebec, date > "2020-11-14" & date < "2021-01-14" , select=c(var1col ))
p2 <- subset(quebec, date > "2020-11-01" & date < "2021-01-01", select = c(var2col))
cor(p1, p2)

p1 <- subset(quebec, date > "2021-01-07" & date < "2021-03-07" , select=c(var1col ))
p2 <- subset(quebec, date > "2021-01-01" & date < "2021-03-01", select = c(var2col))
cor(p1, p2)

p1 <- subset(quebec, select=c(var1col ))
p2 <- subset(quebec, select = c(var2col))
cor(p1, p2)

p <- cbind(p1,p2)

cor.test(p1[,1], p2[,1])
cor(p1, p2)^2  #Multiple R-squared : identifie le % de données qui suit la régression 


#La régression linéaire
m1 <- lm(p[, 1] ~ p[, 2]  , data = p)
summary(m1)

#vérification de la normalité des résidus
par(mfrow = c(1, 1))
qqnorm(residuals(m1), ylab = "Quantiles observés", xlab = "Quantiles théoriques", main = "Normalité des résidus")
qqline(residuals(m1))

#Vérification de l'homoscédacité (si effet de cone, ce n'est pas correct)
plot(residuals(m1) ~ fitted(m1), ylab = "Résidus", xlab = "Valeurs prédites", main = "Homogénéité des variances")

#vérification de la linéarité
plot(p[, 1] ~ p[, 2], main = "Linéarité")

ord <- coef(m1)[1]
pente <- coef(m1)[2]
SE.pente <- summary(m1)$coefficient[2,2]
df.res <- m1$df.residual
inf95 <- pente + qt(p = 0.025, df = df.res) * SE.pente
sup95 <- pente - qt(p = 0.025, df = df.res) * SE.pente
c(inf95, sup95)
summary(m1)$r.squared

#m1 <- lm(p[, 1] ~ p[, 2]  , data = p)   ***fonctionne pas avec prediction doit écrire nom colonnes
as.name(colnames(p[1]))
as.name(colnames(p[2]))
m1 <- lm(positivite ~ retail_and_recreation_percent_change_from_baseline  , data = p)

#prédiction
jeu.pred <- data.frame(retail_and_recreation_percent_change_from_baseline = c(0,-20,-40,-60,-80))
pred <- predict(m1, newdata = jeu.pred, se.fit = TRUE)
jeu.pred$fit <- pred$fit
jeu.pred$se.fit <- pred$se.fit
jeu.pred$inf95 <- jeu.pred$fit + qt(p = 0.025, df = m1$df.residual) * jeu.pred$se.fit
jeu.pred$sup95 <- jeu.pred$fit - qt(p = 0.025, df = m1$df.residual) * jeu.pred$se.fit
jeu.pred   #fit est le résultat 


#Prédiction pour l'ensemble de l'équation
jeu.pred <- data.frame(retail_and_recreation_percent_change_from_baseline = 
                         seq(from = min(p$retail_and_recreation_percent_change_from_baseline),
                             to = max(p$retail_and_recreation_percent_change_from_baseline), by = 1))
pred <- predict(m1, newdata = jeu.pred, se.fit = TRUE)
jeu.pred$fit <- pred$fit
jeu.pred$se.fit <- pred$se.fit
jeu.pred$inf95 <- jeu.pred$fit + qt(p = 0.025, df = m1$df.residual) * jeu.pred$se.fit
jeu.pred$sup95 <- jeu.pred$fit - qt(p = 0.025, df = m1$df.residual) * jeu.pred$se.fit

#graphique avec points originaux (sans pred)
par(mfrow = c(1, 1))
plot(p$positivite ~ p$retail_and_recreation_percent_change_from_baseline, 
     ylab = "daily confirmed", xlab = "variation retail", ylim = c(min(jeu.pred$inf95), 
                                                                   max(jeu.pred$sup95)), col = "blue")
nombre <- p$retail_and_recreation_percent_change_from_baseline
facteur <- p$positivite
segments(x0 = nombre, y0 = facteur, x1 = nombre, y1 = fitted(m1), col = "blue")

#graphique avec points originaux (avec pred)
plot(jeu.pred$fit ~ jeu.pred$retail_and_recreation_percent_change_from_baseline, type = "n", 
     ylab = "daily confirmed", xlab = "variation retail", ylim = c(min(jeu.pred$inf95), max(jeu.pred$sup95)))
lines(y = jeu.pred$fit, x = jeu.pred$retail_and_recreation_percent_change_from_baseline)
lines(y = jeu.pred$inf95, x = jeu.pred$retail_and_recreation_percent_change_from_baseline, lty = "dotted")
lines(y = jeu.pred$sup95, x = jeu.pred$retail_and_recreation_percent_change_from_baseline, lty = "dotted")

#graphique combiné
plot(p$positivite ~ p$retail_and_recreation_percent_change_from_baseline, ylab = "daily confirmed",
     xlab = "variation retail", col = "blue")
nombre <- p$retail_and_recreation_percent_change_from_baseline
facteur <- p$positivite
segments(x0 = nombre, y0 = facteur, x1 = nombre, y1 = fitted(m1), col = "blue")
lines(y = jeu.pred$fit, x = jeu.pred$retail_and_recreation_percent_change_from_baseline)
lines(y = jeu.pred$inf95, x = jeu.pred$retail_and_recreation_percent_change_from_baseline, lty = "dotted")
lines(y = jeu.pred$sup95, x = jeu.pred$retail_and_recreation_percent_change_from_baseline, lty = "dotted")




#équation de régression linéaire : y = ord + pente*xi
x <- -100
ord + pente * x



m1 <- lm(positivite ~ retail_and_recreation_percent_change_from_baseline  , data = p)
coeff = coefficients(m1)
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1)) 
plot(p$retail_and_recreation_percent_change_from_baseline, p$positivite, main=eq, col="red")
abline(m1, col="blue")
summary(m1)





ggplot(data = quebec, aes(x = date)) + geom_point(aes(y= dailyconfirmed), color = "red") + 
  geom_point(aes(y = parks_percent_change_from_baseline*-10), color = "blue")  + 
  scale_x_date(name = "Date", limits = as.Date(c("2020-12-01","2020-12-31"))) +
  scale_y_continuous(name = "daily confirmed", sec.axis = sec_axis(~./-10, name = "parks", labels = function(b) { paste0(round(b , 0), "%")})) + 
  theme(axis.title.y = element_text(color = "red"),axis.title.y.right = element_text(color = "blue"))





#-------------------------------  Extract data in CSV files ------------------------------

write.csv(quebec,"quebec.csv" , row.names = FALSE)
write.csv(PIB,"PIB.csv" , row.names = FALSE)




#------------------------------Machine learning -----------------------------------------








#------------------------------- Références -----------------------------------------------

#Reference : #Guidotti, E., Ardia, D., (2020), "COVID-19 Data Hub", Journal of Open Source Software 5(51):2376, doi: 10.21105/joss.02376.
#https://covid19datahub.io/articles/api/r.html








#-------------------------------------- À vérifier -------------------------------------------


#____________utile?_______________________

ggplot(data = subset(test, id %in% c("CAN")), aes(date, daily)) + geom_point(color='darkblue') + geom_smooth(method=lm ,se=FALSE) 
ggplot(data = subset(test, id %in% c("USA")), aes(date, daily)) + geom_point(color='darkblue') + geom_smooth(method=lm ,se=FALSE)
ggplot(data = test, aes(date, daily)) + geom_point(color='darkblue') + ylim(0, 5000) 

ggplot(data = subset(test, id %in% c("CAN")), aes(date, daily)) + geom_point(color='darkblue') + geom_smooth(method=lm ,se=FALSE) + scale_x_date(limits = as.Date(c("2020-12-31","2021-01-31")))
ggplot() + geom_point(data = subset(GoogleMobility, id %in% c("CAN")), aes(date, transit_stations_percent_change_from_baseline), color = "blue") + geom_point(data = subset(GoogleMobility, id %in% c("USA")), aes(date, transit_stations_percent_change_from_baseline), color = "red")


par(mfrow = c(2, 1))
plot(test$date, test$transit_stations_percent_change_from_baseline, ylim = c(-150,300), col="blue", pch="o")
abline(h = 0)
points(test$date, test$parks_percent_change_from_baseline, col="red", pch="*")
points(test$date, test$workplaces_percent_change_from_baseline, col="black", pch="+")
plot(test$date, test$daily, col="black", pch="o")

#Produit un graphique animé
# Charge libraries:
library(ggplot2)
library(gganimate)
library(gifski)

# Make a ggplot, but add frame=year: one image per year
ggplot(canada, aes(x= date,y=  transit, size = transit, color = transit)) +
  geom_point() +
  theme_bw() +
  # gganimate specific bits:
  labs(title = 'Date: {frame_time}', x = 'Date', y = 'Transit') +
  transition_time(date) +
  ease_aes('linear')

# Save at gif:
anim_save("ggplot2-animated-gif.gif")


