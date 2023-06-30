
# Script to split the data into train (75%) and test (25%)

rm(list=ls())
library(base)
library(MASS)

data <- read.table("T3 - data_com.txt", h=T)
names(data) <- tolower(names(data))
head(data,20)


#split dataset
##############################################################################

## 75% of the sample size
length(unique(data$id))
smp_size <- floor(0.75 * length(unique(data$id)))
## set the seed to make your partition reproducible
set.seed(126)
train_ind <- sample(unique(data$id), size = smp_size)
train <- data[ data$id%in%train_ind, ]
test <- data[ !(data$id%in%train_ind), ]


write.csv(train,"T4 - data train.csv",row.names = FALSE)
write.table(test,"T4 - data test.txt")

