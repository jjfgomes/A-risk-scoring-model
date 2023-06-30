
#getwd()
rm(list=ls())
library(readxl)
#library(ggplot2)
library(lubridate)
library(MASS)
library(plyr)

# Load data ------------------------------------------------------------------------
data <- read_excel("E1 - R_INPUT.xlsx", na = c("","NULL"))
names(data) <- tolower(names(data))
data <- as.data.frame(data, h=T)
datac <- data[!duplicated(data$id), ]

# AGE -----------------------------------------------------------------------------
datac$age <- round(time_length(interval(as.Date(ymd(datac$b_date)),
                            as.Date(ymd(datac$in_date))),"years"),0)


# LOS - L(enght) O(f) S(tay) -------------------------------------------------------
datac$los <- as.numeric(difftime(as.Date(ymd(datac$out_date)),
                            as.Date(ymd(datac$in_date)),unit="days"))


# Factors - hospital (healthcare unit), gender and comorbidities  ----------------------
datac$hosp_c = as.factor(datac$hosp_c)
datac$gender = as.factor(datac$gender)
datac$dc_af = as.factor(datac$dc_af)


#Subsets - LOS>0 and in_date > "March - 1 (2020)"----------------------------------------------------
#> COVID & hospital stay less than one day without death ----------------- 
# (probably hemodialise patients with daily attendance)
datac <- subset(datac, as.Date(ymd(in_date),unit="days")>="2020-03-01")
dim(datac)
#54266

length(levels(datac$hosp_c))
sort(summary(datac$hosp_c))
datac <- subset(datac, los > 0 | death == "1")
dim(datac)
#54266 - 53181
#1085


#> Removes small healthcare units (<30 episodes)
length(levels(datac$hosp_c))
#53 helthcare units

datac$hosp_c <- droplevels(datac$hosp_c)
sort(summary(datac$hosp_c))

row_to_rm<-which(datac$hosp_c == "HAG"|datac$hosp_c == "HAP"|datac$hosp_c == "HI"|
                   datac$hosp_c=="HW"| datac$hosp_c=="HAE"|
                  datac$hosp_c=="HAH"|datac$hosp_c== "HAR")
datac <- datac[-row_to_rm,]
datac$hosp_c <- droplevels(datac$hosp_c)
sort(summary(datac$hosp_c))
dim(datac)
53181 - 53087
#94 episodes small hosp
data18 <- subset(datac, age<18)
dim(data18)
#684 adolescent or child patients
summary(as.factor(data18$death))
#with 3 deaths but none caused by covid 

#> remove patients who are less than 18
datac<-subset(datac,age>17)
dim(datac)
#52403 episodes adults
datac$hosp_c <- droplevels(datac$hosp_c)
sort(summary(datac$hosp_c))
length(levels(datac$hosp_c))

#remove some variables to clean data ----------------------------------------
datac <- subset(datac, select = - b_date)
names(datac)
namessort <- c("id", "death", "hosp_c", "hosp_dim", "age", "gender", 
               "in_date", "out_date", "los","dc_af")  
datac <- datac[,namessort]
names(datac)
summary(as.factor(datac$death))


#print clean unique data by id
write.table(datac, "T1 - data.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)



