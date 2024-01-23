################################## Load Data

police.dat <- read.csv('policing2.csv', header = T)
policing4 <- read_csv("policing4.csv")
policing <- read.csv("policing.csv")
head(policing)
police <- read.csv("policing2.csv")

################################## Data Cleaning

#Cleaning Data
table(policing$held)
table(policing$race)
table(policing$sex)
table(policing$prior.traffic)
table(policing$region)
table(policing$employed)
table(policing$citizen)
table(policing$databases)

# Combine North/West and South/East
for(i in 1:length(policing4$region)){
  policing4$Region2[i] <- ifelse(policing4$region[i]=="North"||policing4$region[i]=="West", "NW", "SE")
  
}
region2 <- glm(held ~ Region2, data = policing4)
lrtest(intercept_only, region2)

################################## Table 1

library(lmtest)

intercept_only <- glm(held ~ 1, data = police)

only_employed <- glm(held ~ employed, data = police)
lrtest(only_employed, intercept_only)
summary(only_employed)

only_citizen <- glm(held ~ citizen, data = police)
lrtest(only_citizen, intercept_only)
summary(only_citizen)

# employment
# subset for yes employed
yemployed <- subset(police, employed=="Yes")
# total
length(yemployed$employed)
# percent
length(yemployed$employed)/length((police$employed))
# percent held
sum(yemployed$held)/length(yemployed$held)
# not held 
1 - sum(yemployed$held)/length(yemployed$held)
# count held
sum(yemployed$held)

# subset for not employed
nemployed <- subset(police, employed=="No")
# total
length(nemployed$employed)
# percent
length(nemployed$employed)/length((police$employed))
# percent held
sum(nemployed$held)/length(nemployed$held)
# not held 
1 - sum(nemployed$held)/length(nemployed$held)
# count not held
sum(nemployed$held)

# citizen
# subset for yes citizen
ycitizen <- subset(police, citizen=="Yes")
# total
length(ycitizen$citizen)
# percent
length(ycitizen$citizen)/length((police$citizen))
# percent held
sum(ycitizen$held)/length(ycitizen$held)
# not held 
1 - sum(ycitizen$held)/length(ycitizen$held)
# count held
sum(ycitizen$held)

# subset for not citizen
ncitizen <- subset(police, citizen=="No")
# total
length(ncitizen$citizen)
# percent
length(ncitizen$citizen)/length((police$citizen))
# percent held
sum(ncitizen$held)/length(ncitizen$held)
# not held 
1 - sum(ncitizen$held)/length(ncitizen$held)
# count not held
sum(ncitizen$held)

#Total Amount of Data Entries
ntotal <- nrow(policing)
ntotal

#N Totals of Sex (Males and Females)
nmales <- length(which(policing$sex == "Male"))
nfemales <- length(which(policing$sex == "Female"))

#As a percentage
nmales/ntotal
nfemales/ntotal

#Proportion of Males detained
males <- subset(policing, sex == "Male")
males_held <- sum(male$held)
males_held

males_held / ntotal

#Proportion of Females detained
females<- subset(policing, sex == "Female")
females_held <- sum(females$held)
females_held

females_held/ntotal

#Number of Non Detentions for Sex (Males and Females)

#Males not detained
ntotal - males_held
1-males_held / ntotal

#Females not detained
ntotal - females_held
1-females_held / ntotal

#N Totals of Race (Whites and Blacks)
nwhite <- length(which(policing$race == "White"))
nwhite
nblack <- length(which(policing$race == "Black"))
nblack

nwhite/ntotal
nblack/ntotal

#Proportion of Whites detained
whites <- subset(policing, race == "White")
whites_held <- sum(whites$held)
whites_held

#Proportion of Blacks detained
blacks <- subset(policing, race== "Black")
blacks_held <- sum(blacks$held)
blacks_held

whites_held / nwhite

blacks_held / nblack

#Number of non-detentions (Just Court Summons) for Race (Whites and Blacks)

#Whites not detained
ntotal - whites_held
1-whites_held / nwhite

#Blacks not detained
ntotal - blacks_held
1-blacks_held / nblack

#Significance of Predictor Variables Sex and Race
library(lmtest)
only_sex <- glm(held ~ sex, data = policing)
lrtest(only_sex)

only_race <- glm(held ~ race, data = policing)
lrtest(only_race)

chisq.test(policing$sex, policing$held)
chisq.test(policing$race, policing$held)

# Prior Traffic Stops
# subset for zero prior traffic stops
zero_prior <- subset(policing4, prior.traffic==0)
# total
length(zero_prior$prior.traffic)
# percent
length(zero_prior$prior.traffic)/length(policing4$prior.traffic)
# percent held
sum(zero_prior$held)/length(zero_prior$held)
zero_held <- sum(zero_prior$held)
# not held 
1 - sum(zero_prior$held)/length(zero_prior$held)
zero_notheld <- length(zero_prior$held) - sum(zero_prior$held)

# subset for one prior traffic stops
one_prior <- subset(policing4, prior.traffic==1)
# total
length(one_prior$prior.traffic)
# percent
length(one_prior$prior.traffic)/length(policing4$prior.traffic)
# percent held
sum(one_prior$held)/length(one_prior$held)
one_held <- sum(one_prior$held)
# not held 
1 - sum(one_prior$held)/length(one_prior$held)
one_notheld <- length(one_prior$held) - sum(one_prior$held)

# subset for two or more prior traffic stops
two_prior <- subset(policing4, prior.traffic==2)
# total
length(two_prior$prior.traffic)
# percent
length(two_prior$prior.traffic)/length(policing4$prior.traffic)
# percent held
sum(two_prior$held)/length(two_prior$held)
two_held <- sum(two_prior$held)
# not held 
1 - sum(two_prior$held)/length(two_prior$held)
two_notheld <- length(two_prior$held) - sum(two_prior$held)

# prior traffic good predictor?
library(lmtest)
prior <- glm(held ~ prior.traffic, data = policing4, family = binomial)
intercept_only <- glm(held ~ 1, data = policing4, family = binomial)
lrtest(prior, intercept_only)
anova(prior, test = "Chisq")

# Chi-Squared Test
chisq.test(policing4$held, policing4$prior.traffic)

# Region

# subset North
north <- subset(policing4, region=="North")
# total
length(north$region)
# percent
length(north$region)/length(policing4$region)
# percent held
sum(north$held)/length(north$held)
north_held <- sum(north$held)
# not held 
1 - sum(north$held)/length(north$held)
north_notheld <- length(north$held) - sum(north$held)

# subset South
south <- subset(policing4, region=="South")
# total
length(south$region)
# percent
length(south$region)/length(policing4$region)
# percent held
sum(south$held)/length(south$held)
south_held <- sum(south$held)
# not held 
1 - sum(south$held)/length(south$held)
south_notheld <- length(south$held) - sum(south$held)

# subset East
east <- subset(policing4, region=="East")
# total
length(east$region)
# percent
length(east$region)/length(policing4$region)
# percent held
sum(east$held)/length(east$held)
east_held <- sum(east$held)
# not held 
1 - sum(east$held)/length(east$held)
east_notheld <- length(east$held) - sum(east$held)

# subset West
west <- subset(policing4, region=="West")
# total
length(west$region)
# percent
length(west$region)/length(policing4$region)
# percent held
sum(west$held)/length(west$held)
west_held <- sum(west$held)
# not held 
1 - sum(west$held)/length(west$held)
west_notheld <- length(west$held) - sum(west$held)

# region good predictor?
library(lmtest)
region <- glm(held ~ region, data = policing4)
lrtest(intercept_only, region)
anova(region, test = "Chisq")

################################## Table 2

# Min, Median, Max, StDev:
stats <- function(data){
  print(min(data))
  print(median(data))
  print(max(data))
  print(sd(data))
}

stats(police.dat$databases)
stats(police.dat$year)
stats(police.dat$age)

percent_held <- length(which(police.dat$held==1))/length(police.dat$held)*100; percent_held
percent_notheld <- 100-percent_held; percent_notheld

# Likelihood Ratio Tests for Numerical Predictors:
null.fit <- glm(held~1, binomial, data=police.dat)
databases.test.fit <- glm(held~databases, binomial, data=police.dat)
year.test.fit <- glm(held~year, binomial, data=police.dat)
age.test.fit <- glm(held~age, binomial, data=police.dat)

library(lmtest)
lrtest(null.fit, databases.test.fit)
lrtest(null.fit, year.test.fit)
lrtest(null.fit, age.test.fit)

################################## Table 3

# Logistic Regression:
database.fit <- glm(held~databases,binomial,data=police.dat)
summary(database.fit)

# New Year variable
police.dat$year.trans <- sqrt(police.dat$years_since_2000)
year.trans.fit <- glm(held~year.trans,binomial,data=police.dat)
summary(year.trans.fit)

age.fit <- glm(held~age,binomial,data=police.dat)
summary(age.fit)

# Interactions:
# Race and Sex:
race_sex <- glm(held~race*sex,binomial,data=police.dat)
summary(race_sex)
b0 <- race_sex$coefficients[1]
bW <- race_sex$coefficients[2]
bM <- race_sex$coefficients[3]
bWM <- race_sex$coefficients[4]
# Woman by race
plot(c(0,1), c(b0, b0+bW), "l", main = "Log-Odds Held", xlab = "Race",
     ylab = "Log Odds", xaxt = "n", col = "red",ylim=c(-2,-1))
axis(1, at = c(0, 1), labels = c("Black", "White"))
# Man by race
lines(c(0,1),c(b0+bM, b0+bW+bM+bWM))
legend("topright", lty = 1, cex=0.9, col = c("red", "black"), legend = c("Female", "Male"))

# Race and Employment:
race_emp <- glm(held~race*employed,binomial,data=police.dat)
summary(race_emp)
b0 <- race_emp$coefficients[1]
bW <- race_emp$coefficients[2]
bE <- race_emp$coefficients[3]
bWE <- race_emp$coefficients[4]
# Unemployed by Race
plot(c(0,1), c(b0, b0+bW), "l", main = "Log-Odds Held", xlab = "Race",
     ylab = "Log Odds", xaxt = "n", col = "red",ylim=c(-2.2,0))
axis(1, at = c(0, 1), labels = c("Black", "White"))
# Employed by Race
lines(c(0,1),c(b0+bE, b0+bW+bE+bWE))
legend("topright", lty = 1, cex=0.8, col = c("red", "black"), legend = c("Unemployed", "Employed"))

# Race and Citizenship:
race_cit <- glm(held~race*citizen,binomial,data=police.dat)
summary(race_cit)
b0 <- race_cit$coefficients[1]
bW <- race_cit$coefficients[2]
bC <- race_cit$coefficients[3]
bWC <- race_cit$coefficients[4]
# Non-citizen by race:
plot(c(0,1), c(b0, b0+bW), "l", main = "Log-Odds Held", xlab = "Race",
     ylab = "Log Odds", xaxt = "n", col = "red",ylim=c(-2,-0.5))
axis(1, at = c(0, 1), labels = c("Black", "White"))
# Citizen by race:
lines(c(0,1),c(b0+bC, b0+bW+bC+bWC))
legend("topright", lty = 1, cex=0.8, col = c("red", "black"), legend = c("Non-Citizen", "Citizen"))

# Employed and Citizenship:
emp_cit <- glm(held~employed*citizen,binomial,data=police.dat)
summary(emp_cit)
b0 <- race_cit$coefficients[1]
bE <- race_cit$coefficients[2]
bC <- race_cit$coefficients[3]
bEC <- race_cit$coefficients[4]
# Non-citizen by employment:
plot(c(0,1), c(b0, b0+bE), "l", main = "Log-Odds Held", xlab = "Employment Status",
     ylab = "Log Odds", xaxt = "n", col = "red",ylim=c(-2,-0.5))
axis(1, at = c(0, 1), labels = c("Black", "White"))
# Citizen by employment:
lines(c(0,1),c(b0+bC, b0+bE+bC+bEC))
legend("topright", lty = 1, cex=0.8, col = c("red", "black"), legend = c("Non-Citizen", "Citizen"))

# Age and Citizenship:
age_cit <- glm(held~age*citizen,binomial,data=police.dat)
summary(age_cit)
b0 <- age_cit$coefficients[1]
bA <- age_cit$coefficients[2]
bC <- age_cit$coefficients[3]
bAC <- age_cit$coefficients[4]
# Non-citizen by age:
curve(expr = b0+bA*x, xlim=c(12,68),
      main="Log-Odds Held", xlab="Age", ylab = "Log Odds",col='red',ylim=c(-2.5,0.25))
# Citizen by age
curve(expr = b0+bA*x+bC+bAC*x, xlim=c(12,68),
      main="Log-Odds Held", xlab="Age", ylab = "Log Odds",
      col="black", add = TRUE)
legend("topright", lty = 1, cex=0.8, col = c("red", "black"), legend = c("Non-Citizen", "Citizen"))

# Age and Employment:
age_emp <- glm(held~age*employed,binomial,data=police.dat)
summary(age_emp)
b0 <- age_emp$coefficients[1]
bA <- age_emp$coefficients[2]
bE <- age_emp$coefficients[3]
bEA <- age_emp$coefficients[4]
# Employed by age:
curve(expr = b0+bA*x+bE+bEA*x, xlim=c(12,68),
      main="Log-Odds Held", xlab="Age", ylab = "Log Odds",col='red',ylim=c(-2.5,-0.5))
# Unemployed by age:
curve(expr = b0+bA*x, xlim=c(12,68),
      main="Log-Odds Held", xlab="Age", ylab = "Log Odds",
      col="black", add = TRUE)
legend("bottomright", lty = 1, cex=0.8, col = c("red", "black"), legend = c("Employed", "Unemployed"))

# Age and race
age_race <- glm(held~age*race,binomial,data=police.dat)
summary(age_race)
b0 <- age_race$coefficients[1]
bA <- age_race$coefficients[2]
bW <- age_race$coefficients[3]
bAW <- age_race$coefficients[4]
# White by age 
curve(expr = b0+bA*x+bW+bAW*x, xlim=c(12,68),
      main="Log-Odds Held", xlab="Age", ylab = "Log Odds",col='red')
# Black by age:
curve(expr = b0+bA*x, xlim=c(12,68),
      main="Log-Odds Held", xlab="Age", ylab = "Log Odds",
      col="black", add = TRUE)
legend("bottomright", lty = 1, cex=0.8, col = c("red", "black"), legend = c("White", "Black"))

# Age and sex
age_sex <- glm(held~age*sex,binomial,data=police.dat)
summary(age_sex)
b0 <- age_sex$coefficients[1]
bA <- age_sex$coefficients[2]
bM <- age_sex$coefficients[3]
bAM <- age_sex$coefficients[4]
# Male by age:
curve(expr = b0+bA*x+bM+bAM*x, xlim=c(12,68),
      main="Log-Odds Held", xlab="Age", ylab = "Log Odds",col='red',ylim=c(-2.3,-1))
# Female by age:
curve(expr = b0+bA*x, xlim=c(12,68),
      main="Log-Odds Held", xlab="Age", ylab = "Log Odds",
      col="black", add = TRUE)
legend("bottomright", lty = 1, cex=0.8, col = c("red", "black"), legend = c("Male", "Female"))

# Citizen + trans Year
cit_yr <- glm(held~citizen*year.trans,binomial,data=police.dat)
summary(cit_yr)
b0 <- cit_yr$coefficients[1]
bC <- cit_yr$coefficients[2]
bY <- cit_yr$coefficients[3]
bCY <- cit_yr$coefficients[4]
# Citizen by year:
curve(expr = b0+bC+bY*x+bCY*x, xlim=c(0,3),
      main="Log-Odds Held", xlab="Sqrt Of Years Since 2000", ylab = "Log Odds",col='red',ylim=c(-2,0))
# Non-Citizen by year:
curve(expr = b0+bY*x, xlim=c(0,3),
      main="Log-Odds Held", xlab="Sqrt Of Years Since 2000", ylab = "Log Odds",
      col="black", add = TRUE)
legend("topright", lty = 1, cex=0.8, col = c("red", "black"), legend = c("Citizen", "Non-Citizen"))

# Employment + trans Year
emp_yr <- glm(held~employed*year.trans,binomial,data=police.dat)
summary(emp_yr)
b0 <- emp_yr$coefficients[1]
bE <- emp_yr$coefficients[2]
bY <- emp_yr$coefficients[3]
bEY <- emp_yr$coefficients[4]
# Employed by year:
curve(expr = b0+bE+bY*x+bEY*x, xlim=c(0,3),
      main="Log-Odds Held", xlab="Sqrt Of Years Since 2000", ylab = "Log Odds",col='red',ylim=c(-2,0))
# Unemployed by year:
curve(expr = b0+bY*x, xlim=c(0,3),
      main="Log-Odds Held", xlab="Sqrt Of Years Since 2000", ylab = "Log Odds",
      col="black", add = TRUE)
legend("topright", lty = 1, cex=0.8, col = c("red", "black"), legend = c("Employed", "Unemployed"))

# Employment + trans Year
race_yr <- glm(held~race*year.trans,binomial,data=police.dat)
summary(race_yr)
b0 <- race_yr$coefficients[1]
bW <- race_yr$coefficients[2]
bY <- race_yr$coefficients[3]
bWY <- race_yr$coefficients[4]
# White by year:
curve(expr = b0+bW+bY*x+bWY*x, xlim=c(0,3),
      main="Log-Odds Held", xlab="Sqrt Of Years Since 2000", ylab = "Log Odds",col='red',ylim=c(-2,0))
# Black by year:
curve(expr = b0+bY*x, xlim=c(0,3),
      main="Log-Odds Held", xlab="Sqrt of Years Since 2000", ylab = "Log Odds",
      col="black", add = TRUE)
legend("topright", lty = 1, cex=0.8, col = c("red", "black"), legend = c("White", "Black"))

# Sex + trans Year
sex_yr <- glm(held~sex*year.trans,binomial,data=police.dat)
summary(sex_yr)
b0 <- race_yr$coefficients[1]
bM <- race_yr$coefficients[2]
bY <- race_yr$coefficients[3]
bMY <- race_yr$coefficients[4]
# Male by year:
curve(expr = b0+bM+bY*x+bMY*x, xlim=c(0,3),
      main="Log-Odds Held", xlab="Sqrt Of Years Since 2000", ylab = "Log Odds",col='red',ylim=c(-2,0))
# Female by year
curve(expr = b0+bY*x, xlim=c(0,3),
      main="Log-Odds Held", xlab="Sqrt of Years Since 2000", ylab = "Log Odds",
      col="black", add = TRUE)
legend("topright", lty = 1, cex=0.8, col = c("red", "black"), legend = c("Male", "Female"))

# Testing different models
main.fit <- glm(held~race+sex+employed+citizen+databases+year.trans+age,binomial,data=police.dat)
summary(main.fit)
p.main <- 1-pchisq(4221.1,5126); p.main
BIC(main.fit)

inter2.fit <- glm(held~race+sex+employed+citizen+databases+year.trans+age+citizen*databases,binomial,data=police.dat)
summary(inter2.fit)
p.inter2 <- 1-pchisq(4212.2,5125); p.inter2
BIC(inter2.fit)

inter3.fit <- glm(held~race+sex+employed+citizen+databases+year.trans+age+age*employed,binomial,data=police.dat)
summary(inter3.fit)
p.inter3 <- 1-pchisq(4214.1,5125); p.inter3
BIC(inter3.fit)

inter4.fit <- glm(held~race+sex+employed+citizen+databases+year.trans+age+age*race,binomial,data=police.dat)
summary(inter4.fit)
p.inter4 <- 1-pchisq(4208.4,5125); p.inter4
BIC(inter4.fit)

inter5.fit <- glm(held~race+sex+employed+citizen+databases+year.trans+age+sex*year.trans,binomial,data=police.dat)
summary(inter5.fit)
p.inter5 <- 1-pchisq(4217.2,5125); p.inter5
BIC(inter5.fit)

inter6.fit <- glm(held~race+sex+employed+citizen+databases+year.trans+age+citizen*databases+age*employed,binomial,data=police.dat)
summary(inter6.fit)
p.inter6 <- 1-pchisq(4205.4,5124); p.inter6
BIC(inter6.fit)

inter7.fit <- glm(held~race+sex+employed+citizen+databases+year.trans+age+citizen*databases+age*race,binomial,data=police.dat)
summary(inter7.fit)
p.inter7 <- 1-pchisq(4200.2,5124); p.inter7
BIC(inter7.fit)

inter8.fit <- glm(held~race+sex+employed+citizen+databases+year.trans+age+citizen*databases+sex*year.trans,binomial,data=police.dat)
summary(inter8.fit)
p.inter8 <- 1-pchisq(4207.9,5124); p.inter8
BIC(inter8.fit)

inter9.fit <- glm(held~race+sex+employed+citizen+databases+year.trans+age+age*employed+race*age,binomial,data=police.dat)
summary(inter9.fit)
p.inter9 <- 1-pchisq(4202.8,5124); p.inter9
BIC(inter9.fit)

inter10.fit <- glm(held~race+sex+employed+citizen+databases+year.trans+age+age*employed+sex*year.trans,binomial,data=police.dat)
summary(inter10.fit)
p.inter10 <- 1-pchisq(4209.5,5124); p.inter10
BIC(inter10.fit)

inter11.fit <- glm(held~race+sex+employed+citizen+databases+year.trans+age+age*race+sex*year.trans,binomial,data=police.dat)
summary(inter11.fit)
p.inter11 <- 1-pchisq(4204.5,5124); p.inter11
BIC(inter11.fit)

inter12.fit <- glm(held~race+sex+employed+citizen+databases+year.trans+age+citizen*databases+age*employed+race*age,binomial,data=police.dat)
summary(inter12.fit)
p.inter12 <- 1-pchisq(4194.7,5123); p.inter12
BIC(inter12.fit)

inter13.fit <- glm(held~race+sex+employed+citizen+databases+year.trans+age+citizen*databases+age*employed+sex*year.trans,binomial,data=police.dat)
summary(inter13.fit)
p.inter13 <- 1-pchisq(4200.3,5123); p.inter13
BIC(inter13.fit)

inter14.fit <- glm(held~race+sex+employed+citizen+databases+year.trans+age+citizen*databases+age*race+sex*year.trans,binomial,data=police.dat)
summary(inter14.fit)
p.inter14 <- 1-pchisq(4195.9,5123); p.inter14
BIC(inter14.fit)

inter15.fit <- glm(held~race+sex+employed+citizen+databases+year.trans+age+age*employed+age*race+sex*year.trans,binomial,data=police.dat)
summary(inter15.fit)
p.inter15 <- 1-pchisq(4198.2,5123); p.inter15
BIC(inter15.fit)

inter16.fit <- glm(held~race+sex+employed+citizen+databases+year.trans+age+citizen*databases+age*employed+age*race+sex*year.trans,binomial,data=police.dat)
summary(inter16.fit)
p.inter16 <- 1-pchisq(4189.7,5122); p.inter16
BIC(inter16.fit)

# Every possible predictor:
# ROC AUC
library(pROC)
main.predict <- predict(main.fit,police.dat,type='response')
main.roc <- roc(police.dat$held,main.predict)
auc(main.roc)

inter2.predict <- predict(inter2.fit,police.dat,type='response')
inter2.roc <- roc(police.dat$held,inter2.predict)
auc(inter2.roc)

inter3.predict <- predict(inter3.fit,police.dat,type='response')
inter3.roc <- roc(police.dat$held,inter3.predict)
auc(inter3.roc)

inter4.predict <- predict(inter4.fit,police.dat,type='response')
inter4.roc <- roc(police.dat$held,inter4.predict)
auc(inter4.roc)

inter5.predict <- predict(inter5.fit,police.dat,type='response')
inter5.roc <- roc(police.dat$held,inter5.predict)
auc(inter5.roc)

inter6.predict <- predict(inter6.fit,police.dat,type='response')
inter6.roc <- roc(police.dat$held,inter6.predict)
auc(inter6.roc)

inter7.predict <- predict(inter7.fit,police.dat,type='response')
inter7.roc <- roc(police.dat$held,inter7.predict)
auc(inter7.roc)

inter8.predict <- predict(inter8.fit,police.dat,type='response')
inter8.roc <- roc(police.dat$held,inter8.predict)
auc(inter8.roc)

inter9.predict <- predict(inter9.fit,police.dat,type='response')
inter9.roc <- roc(police.dat$held,inter9.predict)
auc(inter9.roc)

inter10.predict <- predict(inter10.fit,police.dat,type='response')
inter10.roc <- roc(police.dat$held,inter10.predict)
auc(inter10.roc)

inter11.predict <- predict(inter11.fit,police.dat,type='response')
inter11.roc <- roc(police.dat$held,inter11.predict)
auc(inter11.roc)

inter12.predict <- predict(inter12.fit,police.dat,type='response')
inter12.roc <- roc(police.dat$held,inter12.predict)
auc(inter12.roc)

inter13.predict <- predict(inter13.fit,police.dat,type='response')
inter13.roc <- roc(police.dat$held,inter13.predict)
auc(inter13.roc)

inter14.predict <- predict(inter14.fit,police.dat,type='response')
inter14.roc <- roc(police.dat$held,inter14.predict)
auc(inter14.roc)

inter15.predict <- predict(inter15.fit,police.dat,type='response')
inter15.roc <- roc(police.dat$held,inter15.predict)
auc(inter15.roc)

inter16.predict <- predict(inter16.fit,police.dat,type='response')
inter16.roc <- roc(police.dat$held,inter16.predict)
auc(inter16.roc)

anova(inter16.fit, test="Chisq")

################################## Table 4

#beta estimates, p values, odds ratios for table 4
final_fit <- glm(held~race+sex+employed+citizen+databases+year.trans+age+citizen*databases+age*employed+age*race+sex*year.trans,binomial,data=police)
summary(final_fit)
cbind("OR" = exp(coef(final_fit)))

################################## Table 5

# Not generated with code

################################## Table 6

fit_vals <- inter16.fit$fitted.values

true_neg <- 0
true_pos <- 0
false_neg <- 0
false_pos <- 0
for(i in 1:length(policing4$held)){
  if(fit_vals[i] >= .5){
    if(policing4$held[i]==1){
      true_pos <- true_pos + 1
    }else{
      false_pos <- false_pos + 1
    }
  }else{
    if(policing4$held[i]==0){
      true_neg <- true_neg + 1
    }else{
      false_neg <- false_neg + 1
    }
  }
}

################################## Figure 1

#Mosaic Plot for Sex and Race

table_sex <- table(policing$sex, policing$held)
mosaicplot(table_sex, main = "Mosaic Plot for Sex", ylab = "Held", col = "gray")

table_race <- table(policing$race, policing$held)
mosaicplot(table_race, main = "Mosaic Plot for Race", ylab = "Held", col = "gray")

# mosaic plot
prior_convs <- c(zero_held, one_held, two_held, zero_notheld, one_notheld, two_notheld)
prior_convs.mat <- matrix(prior_convs, byrow = TRUE, nrow = 2, ncol = 3, dimnames = list(Held = c("Yes", "No"), Prior_Convs = c("One", "Two", "Three")))
par(las=1)
spineplot(t(prior_convs.mat), col = c("black", "white"), main = "Mosaic Plot for Prior Convictions")

# mosaic plot
reg <- c(north_held, south_held, east_held, west_held, north_notheld, south_notheld, east_notheld, west_notheld)
reg.mat <- matrix(reg, byrow = TRUE, nrow = 2, ncol = 4, dimnames = list(Held = c("Yes", "No"), Region = c("N", "S", "E", "W")))
par(las=1)
spineplot(t(reg.mat), col = c("black", "white"), main = "Mosaic Plot for Region")

#mosaic plot for employment
table_employ <- table(police$employed, police$held)
emp.mat <- matrix(table_employ, byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(Held = c("Yes", "No"), Employed = c("Yes", "No")))
par(las=1)
spineplot(t(emp.mat), col = c("dark grey", "gray"), main = "Mosaic Plot for Employment")

#mosaic plot for citizen
table_citizen <- table(police$citizen, police$held)
cit.mat <- matrix(table_citizen, byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(Held = c("Yes", "No"), Citizen = c("Yes", "No")))
par(las=1)
spineplot(t(cit.mat), col = c("dark grey", "gray"), main = "Mosaic Plot for Citizenship")

################################## Figure 2

# Slicing and dicing plots for databases:
database.fac <- factor(cut(police.dat$databases,c(seq(-0.5,4.5,by=1),6.5)))
held.rate <- tapply(police.dat$held,database.fac,mean)
plot(c(0,1,2,3,4,5.5),held.rate,main="Police Detention Rate by Database Appearances",
     pch=16,xlab="Number of Database Appearances", ylab="Proportion Detained")

# Now for empirical log-odds:
held.log.odds <- log(held.rate/(1-held.rate))
plot(c(0,1,2,3,4,5.5),held.log.odds,main="Police Detention Log-Odds by Database Appearances",
     pch=16,xlab="Number of Database Appearances", ylab="Log-Odds of Detention")

# Year
police.dat$years_since_2000 <- police.dat$year-2000
year.fac <- factor(cut(police.dat$years_since_2000,c(seq(0.5,6.5,by=1))))
year.prob <- tapply(police.dat$held,year.fac,mean)
plot(seq(1,6,by=1),year.prob,main="Police Detention Rate by Year",
     pch=16,xlab="Years Since 2000", ylab="Proportion Detained")

year.log.odds <- log(year.prob/(1-year.prob))
plot(seq(1,6,by=1),year.log.odds,main="Police Detention Log-Odds by Year",
     pch=16,xlab="Years Since 2000", ylab="Log-Odds of Detention")

# Age
age.fac <- factor(cut(police.dat$age,c(12.5,seq(14.5,49.5,by=1),67.5)))
age.prob <- tapply(police.dat$held,age.fac,mean)
plot(c(13.5,seq(15,49,by=1),58.5),age.prob,main="Police Detention Rate by Age",
     pch=16,xlab="Age", ylab="Proportion Detained")

age.log.odds <- log(age.prob/(1-age.prob))
plot(c(13.5,seq(15,49,by=1),58.5),age.log.odds,main="Police Detention Log-Odds by Age",
     pch=16,xlab="Age", ylab="Log-Odds of Detention")

################################## Figure 3

# Interactions:
# Employment and Databases
colnames(policing)
employed_databases <- glm(held ~ employed*databases, data = policing, family = binomial)
summary(employed_databases)
b0 <- employed_databases$coefficients[1]
be <- employed_databases$coefficients[2]
bd <- employed_databases$coefficients[3]
bed <- employed_databases$coefficients[4]
curve(expr = b0+be+bd*x+bed*x, xlim=c(0,6),
      main="Log-Odds Held", xlab="Databases", ylab = "Log Odds")
curve(expr = b0+bd*x, xlim=c(0,6),
      main="Log-Odds Held", xlab="Databases", ylab = "Log Odds",
      col="red", add = TRUE)
legend(0, -.05, c("Employed", "Unemployed"), c("Black", "Red"))

#Citizenship and Databases
citizen_databases <- glm(held ~ citizen*databases, data = policing, family = binomial)
summary(citizen_databases)
b0 <- citizen_databases$coefficients[1]
bc <- citizen_databases$coefficients[2]
bd <- citizen_databases$coefficients[3]
bcd <- citizen_databases$coefficients[4]
curve(expr = b0+bc+bd*x+bcd*x, xlim=c(0,6),
      main="Log-Odds Held", xlab="Databases", ylab = "Log Odds")
curve(expr = b0+bd*x, xlim=c(0,6),
      main="Log-Odds Held", xlab="Databases", ylab = "Log Odds",
      col="red", add = TRUE)
legend(0, -.05, c("Citizen", "Not a Citizen"), c("Black", "Red"))

# Testing Interactions
# race and databases

race_databases <- glm(held ~ race*databases, data = policing4, family = binomial)
summary(race_databases)
b0 <- race_databases$coefficients[1]
bw <- race_databases$coefficients[2]
bd <- race_databases$coefficients[3]
bwd <- race_databases$coefficients[4]
curve(expr = b0+bw+bd*x+bwd*x, xlim=c(0,6),
      main="Log-Odds Held", xlab="Databases", ylab = "Log Odds")
curve(expr = b0+bd*x, xlim=c(0,6),
      main="Log-Odds Held", xlab="Databases", ylab = "Log Odds",
      col="red", add = TRUE)
legend(0, -.05, c("White", "Black"), c("Black", "Red"))

# sex and employment

sex_employment <- glm(held ~ sex*employed, data = policing4, family = binomial)
summary(sex_employment)
b0 <- sex_employment$coefficients[1]
bm <- sex_employment$coefficients[2]
be <- sex_employment$coefficients[3]
bse <- sex_employment$coefficients[4]
plot(c(0,1), c(b0, b0+be), "l", main = "Log-Odds Held", xlab = "Employment Status", ylab = "Log Oggs", xaxt = "n")
axis(1, at = c(0, 1), labels = c("Unemployed", "Employed"))
points(c(0,1), c(b0+bm, b0+bm+be+bse), "l", col = "red")
legend(0, -1.5, c("Male", "Female"), c("red", "black"))

# sex and citizenship

sex_citizenship <- glm(held ~ sex*citizen, data = policing4, family = binomial)
summary(sex_citizenship)
b0 <- sex_citizenship$coefficients[1]
bm <- sex_citizenship$coefficients[2]
bc <- sex_citizenship$coefficients[3]
bmc <- sex_citizenship$coefficients[4]
plot(c(0,1), c(b0, b0+bc), ylim = c(-2, -.5), "l", main = "Log Odds Held", xlab = "Citzenship Status", ylab = "Log Oggs", xaxt = "n")
axis(1, at = c(0, 1), labels = c("No", "Yes"))
points(c(0,1), c(b0+bm, b0+bm+bc+bmc), "l", col = "red")
legend(.7, -.55, c("Male", "Female"), c("red", "black"))

#sex and database interaction
fit_sd_int <- glm(held ~ sex + databases + sex*databases,binomial,  data = police)
summary(fit_sd_int)
b0 <- fit_sd_int$coefficients[1]
bs <- fit_sd_int$coefficients[2]
bd <- fit_sd_int$coefficients[3]
bsd <- fit_sd_int$coefficients[4]

curve(expr = b0+bs*x+bd*x+bsd*x, xlim=c(0,6),
      main="Log-Odds Held", xlab="Databases", ylab = "Log Odds")
curve(expr = b0+bd*x, xlim=c(0,6),
      main="Log-Odds Held", xlab="Databases", ylab = "Log Odds",
      col="red", add = TRUE)
legend(0, -.05, c("Male", "Female"), c("Black", "Red"))

#employment and citizenship interaction
fit_ec_int <- glm(held ~ employed + citizen + employed*citizen,binomial,  data = police)
summary(fit_ec_int)
b0 <- fit_sd_int$coefficients[1]
be <- fit_sd_int$coefficients[2]
bc <- fit_sd_int$coefficients[3]
bec <- fit_sd_int$coefficients[4]

plot(c(0,1), c(b0, b0+be), "l", main = "Log-Odds Held", xlab = "Employment Status",
     ylab = "Log Odds", xaxt = "n", col = "red",ylim=c(-2.5,-2))
axis(1, at = c(0, 1), labels = c("Unemployed", "Employed"))
lines(c(0,1),c(b0+be, b0+be+bc+bec))
legend(0, -2, c("Not a Citizen", "Citizen"), c("Red", "Black"))

################################## Figure 4

# Final model assessment
plot.roc(inter16.roc,print.auc=T,main="ROC Curve for Final Model")

held.fitted <- inter16.fit$fitted.values
held.hat <- as.numeric(held.fitted>0.5)
x<-table(police.dat$held,held.hat)

################################## Figure 5

policing4$years_since_2000 <- policing4$year-2000
policing4$year.trans <- sqrt(policing4$years_since_2000)
inter16.fit <- glm(held~race+sex+employed+citizen+databases+year.trans+age+citizen*databases+age*employed+age*race+sex*year.trans,binomial,data=policing4)
summary(inter16.fit)

b0 <- inter16.fit$coefficients[1]
bw <- inter16.fit$coefficients[2]
bm <- inter16.fit$coefficients[3]
be <- inter16.fit$coefficients[4]
bc <- inter16.fit$coefficients[5]
bd <- inter16.fit$coefficients[6]
by <- inter16.fit$coefficients[7]
ba <- inter16.fit$coefficients[8]
bcd <- inter16.fit$coefficients[9]
bea <- inter16.fit$coefficients[10]
bwa <- inter16.fit$coefficients[11]
bmy <- inter16.fit$coefficients[12]

# Probability plot age and race

curve(expr = exp(b0+bw+bm+be+bc+bd+by*2+ba*x+bcd+bea*x+bwa*x+bmy*2)/
        (1 + exp(b0+bw+bm+be+bc+bd+by*2+ba*x+bcd+bea*x+bwa*x+bmy*2)), xlim=c(13, 67), 
      main="Probability of Being Held", xlab="Age", ylab = "Probability", col="black")

curve(expr = exp(b0+bm+be+bc+bd+by*2+ba*x+bcd+bea*x+bmy*2)/
        (1 + exp(b0+bm+be+bc+bd+by*2+ba*x+bcd+bea*x+bmy*2)), xlim=c(13, 67), 
      main="Probability of Being Held", xlab="Age", ylab = "Probability", col="red", add = TRUE)

legend(30, .155, c("White", "Black"), c("black", "red"))

# Probability plot citizen and databases

curve(expr = exp(b0+bw+bm+be+bc+bd*x+by*2+ba*22+bcd*x+bea*22+bwa*22+bmy*2)/
        (1 + exp(b0+bw+bm+be+bc+bd*x+by*2+ba*22+bcd*x+bea*22+bwa*22+bmy*2)), xlim=c(0, 6), 
      main="Probability of Being Held", xlab="Databases", ylab = "Probability", col="black")

curve(expr = exp(b0+bw+bm+be+bd*x+by*2+ba*22+bea*22+bwa*22+bmy*2)/
        (1 + exp(b0+bw+bm+be+bd*x+by*2+ba*22+bea*22+bwa*22+bmy*2)), xlim=c(0, 6), 
      main="Probability of Being Held", xlab="Databases", ylab = "Probability", col="red", add = TRUE)

legend(.1, .35, c("Citizen", "Non-Citizen"), c("black", "red"))

# Probability plot age and employment

curve(expr = exp(b0+bw+bm+be+bc+bd+by*2+ba*x+bcd+bea*x+bwa*x+bmy*2)/
        (1 + exp(b0+bw+bm+be+bc+bd+by*2+ba*x+bcd+bea*x+bwa*x+bmy*2)), xlim=c(13, 67), ylim = c(0, .4),
      main="Probability of Being Held", xlab="Age", ylab = "Probability", col="black")
curve(expr = exp(b0+bw+bm+bc+bd+by*2+ba*x+bcd+bwa*x+bmy*2)/
        (1 + exp(b0+bw+bm+bc+bd+by*2+ba*x+bcd+bwa*x+bmy*2)), xlim=c(13, 67), 
      main="Probability of Being Held", xlab="Age", ylab = "Probability", col="red", add = TRUE)

legend(30, .38, c("Employed", "Unemployed"), c("black", "red"))

# Probability plot sex and years

curve(expr = exp(b0+bw+bm+be+bc+bd+by*sqrt(x)+ba*22+bcd*x+bea*22+bwa*22+bmy*sqrt(x))/
        (1 + exp(b0+bw+bm+be+bc+bd+by*sqrt(x)+ba*22+bcd*x+bea*22+bwa*22+bmy*sqrt(x))), xlim=c(0, 6), ylim = c(0, .3),
      main="Probability of Being Held", xlab="Years Since 2000", ylab = "Probability", col="black")

curve(expr = exp(b0+bw+be+bc+bd+by*sqrt(x)+ba*22+bcd*x+bea*22+bwa*22)/
        (1 + exp(b0+bw+be+bc+bd+by*sqrt(x)+ba*22+bcd*x+bea*22+bwa*22)), xlim=c(0, 6), 
      main="Probability of Being Held", xlab="Years Since 2000", ylab = "Probability", col="red", add = TRUE)

legend(.1, .3, c("Male", "Female"), c("black", "red"))







