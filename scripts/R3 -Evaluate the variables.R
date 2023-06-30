rm(list=ls())
library(base)
library(lubridate)
library(MASS)
library(plyr)

# Load data from R2 script:
data <- read.table("T2 - data_ph.txt", h=T)
dim(data)
head(data,53)


#######################################################################
str(data)

# Factors - hosp_c, gender, death, cod_diag, date_cat  ----------------------
data$hosp_c = as.factor(data$hosp_c)
data$gender = as.factor(data$gender)
data$dc_af = as.factor(data$dc_af)
str(data)




#healthcare units #### ---------------------------------
length(levels(data$hosp_c))
(tab1<- with(data,table(death,hosp_c)))
#sum(tab1) --- > 52403 patients
mtab1 <- margin.table(tab1,2)
round(100*mtab1/margin.table(tab1),2)
(nn <- tab1[1,] + tab1[2,])
sort(nn)
(pp <- tab1[2,]/(tab1[1,] + tab1[2,]))
sort(pp)
cbind(nn,round(100*pp,1))[order(pp,nn),]

#gender
#########################################
#----------------------------------------
(t_gen <- with(data,table(death,gender)))
colSums(t_gen)
pgen <- prop.table(t_gen,2)[2,]
table(pgen,t_gen[,2])

#los ( length of stay)
#########################################
#----------------------------------------
summary(data$los)
dim(data)

#death
#########################################
#----------------------------------------
summary(as.factor(data$death))
12546/(39857 + 12546)


#age 
#########################################
summary(data$age)
(t_age <- with(data,tapply(age, death, mean)))
catage <- c(mean(c(0,40)), mean(c(40,50)), mean(c(50,60)),
                mean(c(60,70)),mean(c(70,80)),mean(c(80,85)),
                    mean(c(85,90)),mean(c(90,95)),mean(c(95,100)),mean(c(100,105)))
data$agec <- cut(data$age, 
                   breaks=c(-Inf, 40, 50, 60, 70, 80, 85, 90, 95, 100, Inf), 
                   labels=catage)
max(data$age)
tab <- with(data,by(death, agec, mean))
a1 <- with(data,by(death, agec, sum))
a2 <- with(data,by(death, agec, length))
cbind(a2,a1)
prop <- as.vector(tab)
plot(catage, prop, pch = 19, ylim = c(0, .6))
lines(lowess(catage, prop), col = 2)
tab

#ph - occupancy healthcare --------------

summary(data$ph_mean)
sd(data$ph_mean)

(t_ph <- with(data,tapply(ph_mean, death, mean)))
catph <- c(mean(c(0,10)), mean(c(10,20)), mean(c(20,30)),mean(c(30,81)))
data$catph <- cut(data$ph, 
                 breaks=c(-Inf, 10, 20, 30, Inf), 
                 labels=catph)
tab <- with(data,tapply(death, catph, mean))
a1 <- with(data,by(death, catph, sum))
a2 <- with(data,by(death, catph, length))
cbind(a2,a1,a1/a2)


mean_ph <- with(data,round(tapply(ph_mean,hosp_c, mean),2))
mean_dim <- with(data,round(tapply(hosp_dim,hosp_c, mean),2))
cbind(mean_dim,mean_ph)

#death rate with ph_mean
(q <- quantile(with(data,ph_mean,probs = seq(0, 1, 0.25))))
qq <- c(-1,q[2:4],100)
data$phc <- with(data,cut(ph_mean, breaks = qq ,
                            labels =c("q_1","q_2","q_3","q_4")))
(tab <- with(data,tapply(death, phc, mean)))
prop <- as.vector(tab)
mq<-c(mean(q[1]:q[2]),mean(q[2]:q[3]),mean(q[3]:q[4]),mean(q[4]:q[5]))
plot(mq,prop, pch = 19)
lines(lowess(mq, prop), col = 2)
data <- subset(data, select = -c(agec,phc) )

#removing unnecessary information
namessort <- c("id", "death", "hosp_c" ,"gender", "age",  "ph_mean")  
data <- data[,namessort]
summary(data)

# join clinical diagnosis

# data ------------------------------------------------------------------------
library(readxl)
data_com <- read_excel("E1 - R_INPUT.xlsx", na = c("","NULL"))
names(data_com) <- tolower(names(data_com))
data_com <- as.data.frame(data_com, h=T)
str(data_com)

data_com <- data_com[,c("id","dc_af")]

data_com$dc_af <- as.factor(data_com$dc_af)
summary(data_com)
str(data_com)
data_com$dc_af <- droplevels(data_com$dc_af)
head(data_com)


data<- join(data,data_com, by="id")
write.table(data,"T3 - data_com.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)



