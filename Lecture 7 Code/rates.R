library(readr)
library(MASS)
VAP<- read_csv("rates.csv")

chg <- glm(ICUPneum ~ CHG + Screening + T + Site + offset(log(ICUVentDays)), family="poisson",data=VAP)
summary(chg)

chg2 <- glm.nb(ICUPneum ~ CHG + Screening + T + Site + offset(log(ICUVentDays)),data=VAP)
summary(chg2)