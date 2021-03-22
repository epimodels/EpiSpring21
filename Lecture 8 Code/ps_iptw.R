library(readr)
titanic <- read_csv("titanic.csv")

# Recode the Sex variable

titanic$Male <- 0
titanic$Male[titanic$Sex=="Male"] <- 1
table(titanic$Male,titanic$Sex)

# Make Propensity Scores
logit.ps <- glm(Male ~ Class + Age, data=titanic, family=binomial(link="logit"))

## Extract propensity score (for those who ps is available)
pscores <- fitted(logit.ps)

## Insert the propensity scores back into the data set
titanic$pscore <- NA
titanic$pscore[as.numeric(names(pscores))] <- pscores

# Visualize These
p <- ggplot(titanic, aes(x=pscore,group=Survived,color=Survived)) + 
  geom_density()
p

# Crude Model
crude <- glm(Survived ~ Male, data=titanic, family=binomial(link="logit"))
crude_coef <- summary(crude)$coefficients[2,1]
crude_se <- summary(crude)$coefficients[2,2]

crude_OR <- exp(crude_coef)
crude_LCL <- exp(crude_coef-(1.96*crude_se))
crude_UCL <- exp(crude_coef+(1.96*crude_se))
cat("Crude OR :", crude_OR, "95% CI: ", crude_LCL,",",crude_UCL)

# Traditional Adjustment
traditional <- glm(Survived ~ Male + Age + Class, data=titanic, family=binomial(link="logit"))
trad_coef <- summary(traditional)$coefficients[2,1]
trad_se <- summary(traditional)$coefficients[2,2]

trad_OR <- exp(trad_coef)
trad_LCL <- exp(trad_coef-(1.96*trad_se))
trad_UCL <- exp(trad_coef+(1.96*trad_se))
cat("Traditional OR :", trad_OR, "95% CI: ", trad_LCL,",",trad_UCL)

# Adjust for Propensity Score
prop_adj <- glm(Survived ~ Male + pscore, data=titanic, family=binomial(link="logit"))
ps_coef <- summary(prop_adj)$coefficients[2,1]
ps_se <- summary(prop_adj)$coefficients[2,2]

ps_OR <- exp(ps_coef)
ps_LCL <- exp(ps_coef-(1.96*ps_se))
ps_UCL <- exp(ps_coef+(1.96*ps_se))
cat("Crude OR :", ps_OR, "95% CI: ", ps_LCL,",",ps_UCL)

y <- c(crude_coef,trad_coef,ps_coef)
x <- c("Crude","Traditional","PS")
se <- c(crude_se,trad_se,ps_se)
qplot(x,exp(y),xlab="Model",ylab="Survival Odds Ratio (Men vs. Women",size=I(5))+geom_errorbar(aes(x=x,ymin=exp(y-1.96*se),ymax=exp(y+1.96*se),width=0.25))

#Inverse Probability of Treatment Weights
titanic$IPTW <- NA
titanic$IPTW[titanic$Male==1] <- (1/titanic$pscore)[titanic$Male==1]
titanic$IPTW[titanic$Male==0] <- (1/(1 - titanic$pscore))[titanic$Male==0]

titanic$SIPTW <- NA
titanic$SIPTW[titanic$Male==1] <- (0.7864607/titanic$pscore)[titanic$Male==1]
titanic$SIPTW[titanic$Male==0] <- (0.7864607/(1 - titanic$pscore))[titanic$Male==0]


library(doBy)
summaryBy(IPTW ~ Male, titanic, FUN = summary)
summaryBy(SIPTW ~ Male, titanic, FUN=summary)

p <- ggplot(titanic, aes(x=IPTW,group=Male,fill=Male)) + 
  geom_density(alpha=0.3)
p

p <- ggplot(titanic, aes(x=SIPTW,group=Male,fill=Male)) + 
  geom_density(alpha=0.3)
p

library(sandwich)
library(lmtest)

IPTW_adj <- glm(Survived ~ Male, data=titanic,weights=IPTW, family=binomial(link="logit"))
robust <- coeftest(IPTW_adj, vcov = sandwich)
IPTW_coef <- robust[2][1]
IPTW_se <- robust[2][2]
IPTW_OR<- exp(robust[2][1])
ps_LCL <- exp(robust[2][1]-(1.96*robust[2][2]))
ps_UCL <- exp(robust[2][1]+(1.96*robust[2][2]))
cat("Crude OR :", ps_OR, "95% CI: ", ps_LCL,",",ps_UCL)

y <- c(crude_coef,trad_coef,ps_coef,IPTW_coef)
x <- c("Crude","Traditional","PS","IPTW")
se <- c(crude_se,trad_se,ps_se,iptw_se)
qplot(x,exp(y),xlab="Model",ylab="Survival Odds Ratio (Men vs. Women",size=I(5))+geom_errorbar(aes(x=x,ymin=exp(y-1.96*se),ymax=exp(y+1.96*se),width=0.25))

