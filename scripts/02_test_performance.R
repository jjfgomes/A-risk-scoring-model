
#setwd(...your directory...)


#### Run previous scripts:
source ("01_train_build_model.R")


#### Load test data:
test <- read.table("test.txt", h=T)
names(test) <- tolower(names(test))
head(test,20)
# This dataset has the original variables. To apply our model, we need to 
# join the score variables (hospital, comorbidity and gender).

# Load comorbidity <-> score data:
score_dc_af <- read.table("score_dc_af.txt", h=T)
head(score_dc_af,20)

# Join score_dc_af information with test:
test <- join(test, score_dc_af, by = "dc_af")
head(test,10)

# Save this version of the test (comorbidities ungrouped by patient id)
test_com <- test
head(test_com,30)
# Note: test_com will be used in the 03_plots.R script


#### Aggregate by patient id and join additional score variables: 

# Sum comorbidity score by patient id:
score_sum <- aggregate(test$score_dc_af, by = list(test$id), sum)
names(score_sum) = c("id","dc_af_sc_sum")
head(score_sum)


# Load gender <-> score data:
score_gen<- read.table("score_gen.txt", h=T)
head(score_gen)

# Group data by patient id by getting the unique values (excluding the comorbidity and
# corresponding scores, which are the columns 7 and 8): we do this because each
# patient can have multiple lines in the dataset (one for each diferent 
# comorbidity), and we want to have a single line per patient:
test2 <- unique(test[,-c(7,8)])
head(test2)

# Join the comorbidity scores (already grouped by patient id):
test3 <- join(test2, score_sum, by ="id")
head(test3)

# Join the gender scores:
test4 <-  join(test3, score_gen, type = "inner")
head(test4)

# Load and join the gender scores:
score_hosp<- read.table("score_hosp.txt", h=T)
test5 <-  join(test4, score_hosp, type = "inner")
head(test5)

# Update our test image:
test <- test5
str(test)

# Convert to factor the hospital and gender:
test$hosp_c <- factor(test$hosp_c)
test$gender <- factor(test$gender)
str(test)

# [Check] Frequency of each hospital code in the test:
summary(test$hosp_c)



#### Scale test set:

#Scale variables based in train average and std (assume we don't have test information)
stest <- data.frame(test[1:6], scale(test[,5:9], center = center_train, scale = sd_train) )
head(stest)

# Place all variables with 7 decimal places (same treatment as train):
stest$age.1 <- round(stest$age.1, 7)
stest$ph_mean.1 <- round(stest$ph_mean.1, 7)
stest$dc_af_sc_sum <- round(stest$dc_af_sc_sum, 7)
stest$gen_sc1 <- round(stest$gen_sc1, 7)
stest$hosp_sc1 <- round(stest$hosp_sc1, 7)

# Update names:
names(stest)[c(5,6,7,8)] <- c("age.1","ph_mean.1","age","ph_mean")
head(stest)

# Update test image:
test <- stest
head(test)


#### Compute predictions (probabilities) on test (standardized) using model
# with interactions (m1 model):
fit_test<-predict(m1,test, type="response")


#### Evaluation on standardized test:

# Compute prediction object(0/1) to # get sensitivity and specificity:
par(mfrow=c(1,1))
pred <- prediction(fit_test,test$death)  

# Compute sensitivity and specificity on test:
sensitivity <- performance(pred,"tpr")
specificity <- performance(pred,"tnr")

# Area sob a Curva ROC
(area_test <- performance(pred, "auc")) #0.815
rm("pred","sensitivity", "specificity")



#### Probability confidence interval using 3 standard deviations of the logit
# predictions:

library(faraway) # to get function ilogit (inverse logit function)

# Add prediction column to test dataframeÇ:
test$fit <- fit_test

# Compute the logit values and corresponding standard errors:
fit <- predict(m1,test, se.fit=TRUE)

# Convert logit to probability space using ilogit() and add to test df:
test$fit <- ilogit(fit$fit)

# In logit space we can add/subtract the standard error to get IC:
score_li <- fit$fit-3*fit$se.fit # lower bound
test$fit_low <- ilogit(score_li ) # convert to probability and add to test df

score_ls <- fit$fit+3*fit$se.fit # upper bound
test$fit_high <- ilogit(score_ls ) # convert to probability and add to test df



#### Cut-off probability using train:

# configurations:
par(mfrow=c(1,1),mgp=c(2,1,0),font.axis=2,font.lab=1.2,lwd=1,cex.lab=1.2)
par(mai=c(1,1,1,1))

library(ROCR)

# Compute probabilities in the train (standardized) set:
fit_train<-predict(m1,strain, type="response")
# Note: type = 'response' tells R to output P(Y=1|X), as opposed to logit quantities

# Compute predictions (0/1) for the train probabilities:
par(mfrow=c(1,1))
pred <- prediction(fit_train, train$death)
perf=performance(pred,"acc")


# List of possible cut-off values:
cut_off=pred@cutoffs
cf=cut_off[[1]]

# Compute sensitivity and specificity for all those cut-offs:
#'tpr"->true positive rate (sensitivity), P(y^=1|y=1)
#'tnr"->true negative rate (specificity),P(y^=0|y=0)
sens=performance(pred,"sens")
espec=performance(pred,"spec")
sensitivity_array <- sens@y.values[[1]]
specificity_array=espec@y.values[[1]]

# Optimal cut-off: cut-off probability for which sensitivity ~= specificity:
optimo=cf[which.min(abs(sensitivity_array-specificity_array))] #0.2889917 
sens_opt = sensitivity_array[which.min(abs(sensitivity_array-specificity_array))]
sens_opt = specificity_array[which.min(abs(sensitivity_array-specificity_array))]



