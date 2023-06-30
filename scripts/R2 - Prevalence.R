
# Script to compute the healthcare unit(hospital) pressure(occupancy rate)

rm(list=ls())
library(readxl)
library(lubridate)
library(MASS)
library(plyr)
library(incidence)

# data ------------------------------------------------------------------------
data <- read.table("T1 - data.txt", h=T)
names(data) <- tolower(names(data))
data <- as.data.frame(data, h=T)
head(data)
str(data)
data$hosp_c = as.factor(data$hosp_c)

#prevalence by healthcare unit------------------------------------------
#seq(as.Date("2020-03-01"), as.Date("2021-03-13"), by="days")
dim(data)
#how many
k <- length(levels(data$hosp_c))

#incidence
i <- incidence(data$in_date, first_date = as.Date("2020-03-01"), 
               last_date =as.Date(max(data$out_date)), groups = data$hosp_c)

#out
o <- incidence(data$out_date, first_date = as.Date("2020-03-01"), 
               last_date =as.Date(max(data$out_date)), groups = data$hosp_c )

# Compute prevalence:
in_hosp <- data.frame(datas = i$dates, i_ic = cumulate(i)$counts  )
out_hosp <- data.frame(datas = o$dates,  i_oc = cumulate(o)$counts)
oc_hosp <- join(in_hosp, out_hosp, by = "datas")
oc_hosp2 <- data.frame(oc_hosp$datas,oc_hosp[,2:(k+1)] - oc_hosp[,(k+2):(2*k+1)])
nc <- dim(oc_hosp2)[2]
colnames(oc_hosp2)[1:nc] <- c("datas",colnames(i$counts))

# To calculate healthcare pressure:
dataph <- subset(data, select = c(id, hosp_c, hosp_dim, in_date, out_date))
ph <- oc_hosp2
vv <- names(ph)[-1]
rm("oc_hosp" , "oc_hosp2")


library(reshape2)
str(data)
str(ph)
ph <- melt(ph, id.vars = 'datas',measure.vars =vv, variable.name = 'hosp_c', value.name = 'co')
head(ph, 10)
head(dataph)
data2 <- join(dataph, ph, type = "inner")
head(data2)

#Healthcare pressure
data2$ph <- with(data2,round(100*co/hosp_dim,0))
data3 <- with(data2,data2[datas >= as.Date(ymd(in_date)) & datas<=as.Date(ymd(out_date)),])
str(data3)
rm("data2")
data4 <- aggregate(ph ~ id , data3, mean)
dataph <- join(dataph,data4, by="id")
names(dataph)[names(dataph) == "ph"] <- "ph_mean"
dataph <- subset(dataph, select = - c(hosp_c, hosp_dim, in_date, out_date))
data_final <- join(data,dataph, by="id")
data<-data_final
data$ph_mean <- round(data$ph_mean,2)
head(data,50)
summary(data)
write.table(data, "T2 - data_ph.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)

