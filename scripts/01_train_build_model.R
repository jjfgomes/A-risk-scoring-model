rm(list=ls())
#setwd(...your directory...)

#### Load Libraries:
library(base)
extrafont::loadfonts(device="win")
library(ggplot2)
library(lubridate)
library(MASS)
library(plyr)
library(ROCR)


#### Import train data:
train <- read.table("train_data.txt", h=T)
names(train) <- tolower(names(train))
head(train,20)
cor(train[,5:9])


#### Scale variables:
head(train)
strain <- data.frame(id = train[,1],death = train[,2],scale(train[,5:9]))

# For consistency, place all numbers with the same decimal places:
strain$age <- round(strain$age, 7)
strain$ph_mean <- round(strain$ph_mean, 7)
strain$dc_af_sc_sum <- round(strain$dc_af_sc_sum, 7)
strain$gen_sc1 <- round(strain$gen_sc1, 7)
strain$hosp_sc1 <- round(strain$hosp_sc1, 7)

head(strain)
center_train <- attr(scale(train[,5:9]), "scaled:center")
sd_train <- attr(scale(train[,5:9]), "scaled:scale")
summary(strain)
length(unique(train$hosp_c))


#### Univariate analysis:
# age:
m_age <- glm(death ~   age , binomial, strain)
summary(m_age)
exp(cbind("Odds ratio" = coef(m_age), confint.default(m_age, level = 0.95)))

# gender:
m_gen <- glm(death ~   gen_sc1 , binomial, strain)
summary(m_gen)
exp(cbind("Odds ratio" = coef(m_gen), confint.default(m_gen, level = 0.95)))

# hospital-pressure:
m_hp <- glm(death ~   ph_mean , binomial, strain)
summary(m_hp)
exp(cbind("Odds ratio" = coef(m_hp), confint.default(m_hp, level = 0.95)))

# hospital:
m_hosp <- glm(death ~   hosp_sc1 , binomial, strain)
summary(m_hosp)
exp(cbind("Odds ratio" = coef(m_hosp), confint.default(m_hosp, level = 0.95)))

# clinical diagnostics:
m_dc <- glm(death ~   dc_af_sc_sum , binomial, strain)
summary(m_dc)
exp(cbind("Odds ratio" = coef(m_dc), confint.default(m_dc, level = 0.95)))


#### Train initial model (no interactions):
m0<- glm(death ~   age + ph_mean + dc_af_sc_sum + gen_sc1 + hosp_sc1
         , binomial, strain)
summary(m0)
exp(cbind("Odds ratio" = coef(m0), confint.default(m0, level = 0.95)))


#### Additional model with interactions:
m1<- glm(death ~   age + ph_mean + dc_af_sc_sum + gen_sc1 + hosp_sc1 + age:gen_sc1 +
         age:dc_af_sc_sum , binomial, strain)
summary(m1)
exp(cbind("Odds ratio" = coef(m1), confint.default(m1, level = 0.95)))



#### Aditional auxiliary models:
# Model with just age:
m_age<- glm(death ~   age , binomial, strain)
summary(m_age)
exp(cbind("Odds ratio" = coef(m_age), confint.default(m_age, level = 0.95)))


#### Model m0 analysis:
# Confidence interval ODDS-RATIO
exp(cbind("Odds ratio" = coef(m0), confint.default(m0, level = 0.95)))
coef(m0)/sum(coef(m0)[-1])


#### Model m1 analysis:
# Get predictions:
fit <- m1$fitted 
par(mfrow = c(1,1))
pred <- prediction(fit, strain$death)

sensitivity <- performance(pred,"tpr")
plot(sensitivity)
specificity <- performance(pred,"tnr")
plot(specificity,add=T)

# Area under the curve:
(area <- performance(pred, "auc"))
rm("pred","sensitivity", "specificity") #0.807

