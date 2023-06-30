
# Script to eval the main model + plots for paper:

rm(list =ls())
#setwd('C:/Users/LEGION Y520/Desktop/covid data/novos_dados_8') #Afonso
source ("R5 - train_build_model.R")
library(faraway)
library(ROCR)
library(extrafont)
library(plyr)
library(ggplot2)

#                    TEST - 1
###################################################################



# Load test data:
test <- read.table("T4 - data test.txt", h=T)
names(test) <- tolower(names(test))
head(test,20)


#Join comorbidity data:
score_dc_af <- as.data.frame(read_excel("E2 - train data to score.xlsx", 
                                        sheet = "score_dc_af"))
test <- join(test, score_dc_af, by = "dc_af")
test_com <- test


#Score comorbity by id
score_sum <- aggregate(test$score_dc_af, by = list(test$id), sum)
names(score_sum) = c("id","dc_af_sc_sum")
head(score_sum)
test <- join(test, score_sum, by = "id")
test <- subset(test, select = - c(dc_af,score_dc_af)) 


test <- unique(test)
head(test)


#Add Gender and healthcare units (scores computed in train data)
score_gen <- as.data.frame(read_excel("E2 - train data to score.xlsx", 
                                        sheet = "score_gen"))

test <-  join(test, score_gen, type = "inner")
head(test)

score_hosp<- as.data.frame(read_excel("E2 - train data to score.xlsx", 
                                      sheet = "score_hosp"))
test <-  join(test, score_hosp, type = "inner")
head(test,10)


#                         TEST - 2
#Scale and return to model
###################################################################

#Scale variables based in train data 
stest <- data.frame(test[,1:6], scale(test[,5:9], center = center_train, scale = sd_train) )
head(stest)
summary(stest)
#almost zero 

# For higher consistency, place all values with the same decimal places (7):
stest$age.1 <- round(stest$age.1, 7)
stest$ph_mean.1 <- round(stest$ph_mean.1, 7)
stest$dc_af_sc_sum <- round(stest$dc_af_sc_sum, 7)
stest$gen_sc1 <- round(stest$gen_sc1, 7)
stest$hosp_sc1 <- round(stest$hosp_sc1, 7)

# Update names:
names(stest)[c(5,6,7,8)] <- c("age.1","ph_mean.1","age","ph_mean")
head(stest,10)

test <- stest
rm(stest)
# Generate fit object:
fit_test<-predict(m1,test, type="response")

#                    TEST - 3 - Evaluation
###################################################################

#ROC curve to test data                                         - 1
par(mfrow=c(1,1))
pred <- prediction(fit_test, test$death)

# Area sob a Curva ROC
(area <- performance(pred, "auc"))
rm("pred")
#0.8


## quantile observ vs quantile fitted                            - 2

###################
# by fitted values - 2.1
fit <- predict.glm(m1, test, se.fit=TRUE)
v <- cut(fit$fit, breaks=quantile(fit$fit,probs = seq(0, 1, 0.1)), right = TRUE)
marginSums(table(test$death,v),2)
p1 <- prop.table(table(test$death,v),2)[2,]
plot(p1,pch=19, cex=0.5,ylim=c(0,1))
score_li <- fit$fit-3*fit$se.fit
p2 <- tapply(ilogit(score_li ),v,mean)
points(p2,col=2,t="l")
score_ls <- fit$fit+3*fit$se.fit
p3 <- tapply(ilogit(score_ls ),v,mean)
points(p3,col=3,t="l")
score <- fit$fit
p4 <- tapply(ilogit(score),v,mean)
points(p4,col=6,t="p")



############# PLOTS - 3. ##########################

q_cuts <- unique(v)
q_cuts <- as.character(q_cuts)
q_cuts <- q_cuts[1:10]
q_cuts

max_pred <- tapply(ilogit(score), v, max)
df_qq_plot <- data.frame(max_pred=max_pred)
df_qq_plot$max_pred <- round(df_qq_plot$max_pred,3)

df_qq_plot$idx <- seq(1,10,1)
df_qq_plot$max_pred_str <- as.character(df_qq_plot$max_pred)
df_qq_plot$max_pred_str <- paste("[",df_qq_plot$idx,"]"," <=",df_qq_plot$max_pred_str, sep = "")

df_qq_plot <- df_qq_plot[order(df_qq_plot$max_pred),]

df_qq_plot$obs <- p1
df_qq_plot$pred <- p4
df_qq_plot$pred_li <- p2
df_qq_plot$pred_ls <- p3
#Add labels to plot:
df_qq_plot$legend_lab <- 'Observed'
df_qq_plot$legend_lab2 <- 'Predicted'

df_qq_plot$max_pred_str <- factor(df_qq_plot$max_pred_str, levels = df_qq_plot$max_pred_str[order(df_qq_plot$obs)])

df_qq_plot


# Import fonts (you may need to run this code):
#font_import() 
#loadfonts(device = "win")


windowsFonts("Times" = windowsFont("TT Times New Roman"))
my_gg_theme_qq <- theme(
  text = element_text(family = "Times"), #text font 
  axis.line = element_line(),
  panel.grid = element_line(color = "gray60",
                            size = 0.20,
                            linetype = 'dotted'), #grid style
  panel.background = element_blank(),             #background
  axis.title.x = element_text(size = 13),         #x label style
  axis.text.x = element_text(size = 9, angle=45, hjust=1),         #x axis values style
  axis.title.y = element_text(size = 13),         #y label style
  axis.text.y = element_text(size = 9),         #y axis values style
  legend.position='bottom',                      #legend position
  plot.title = element_text(size=14, face='bold'),
  plot.subtitle = element_text(size=13)
)
ggplot(df_qq_plot, aes(max_pred_str, obs, group=1)) +
  #Predicted: ---
  geom_line(data=df_qq_plot,aes(max_pred_str, pred),colour = "gray85", size=0.3)+
  geom_point(data=df_qq_plot,aes(max_pred_str, pred, col=legend_lab2, shape=legend_lab2), size=2.3) +
  #Predicted IC: ---
  geom_line(data=df_qq_plot,aes(max_pred_str, pred_ls), color='gray65', linetype='dashed', size=0.3) + #ls IC
  geom_line(data=df_qq_plot,aes(max_pred_str, pred_li), color='gray65', linetype='dashed', size=0.3) +  #li IC
  geom_ribbon(data=df_qq_plot, aes(ymin=pred_li,ymax=pred_ls), fill="gray96", alpha=0.5) + #fill IC
  #Observed: ---
  geom_line(color='gray70', size=0.3)+
  geom_point(data=df_qq_plot, aes(max_pred_str, obs, group=1, col=legend_lab, shape=legend_lab), size=2.3)  +
  #MISC: ---
  my_gg_theme_qq +
  xlab('Probability (deciles)') +
  ylab('Propensity')+
  scale_color_manual(name='',values=c("gray18", 'gray50')) +
  scale_shape_manual(name='',values=c(15, 17)) +
  scale_y_continuous(breaks=c(0,0.1,0.2,0.3, 0.4, 0.5, 0.6)) +
  labs(title="Fit quality: qq-plot", subtitle='using Test data')
#ggsave('qq_plot_fit_quality.png', dpi=750, plot=last_plot(), width = 5*1.1, height = 4.3*1.1) #save









##################### PLOTS - independent variables  - 4. ######

#Age - class 4.1
##########################################
# Aggregate predicted probability and observed death propensity by
# age intervals. Also compute pseudo confidence intervals:

head(test)
summary(test$age.1)
test$agec <- with(test, cut(age.1, 
             breaks=c(18,40,50,60,65,70,75,80,85,90,105), right =FALSE ), labels = c())
summary(test$agec)
head(test)
fit <- predict(m1,test, se.fit=TRUE)
test$fit <- ilogit(fit$fit)
score_li <- fit$fit-3*fit$se.fit
test$fit_low <- ilogit(score_li )
score_ls <- fit$fit+3*fit$se.fit
test$fit_high <- ilogit(score_ls )
tab <- with(test,by(death, agec, mean))
prop <- as.vector(tab)
tab2 <- with(test,by(fit, agec, mean))
prop_fit <- as.vector(tab2)
age_class <- names(tab)
dataage <- data.frame(age_class, prop)
dataage$legend_lab <- 'Observed' # label for legend
modage <- data.frame(age_class, prop_fit)
modage$legend_lab2 <- 'Predicted' # label for legend

# Add CI (confidence interval):
prob_low <- with(test, by(fit_low, agec, mean))
modage$prob_low <- as.vector(prob_low)
prob_high <- with(test, by(fit_high, agec, mean))
modage$prob_high <- as.vector(prob_high)


# Import fonts (you may need to run this code):
#font_import()  
#loadfonts(device = "win")

windowsFonts("Times" = windowsFont("TT Times New Roman"))
my_gg_theme <- theme(
                    text = element_text(family = "Times"), #text font 
                    axis.line = element_line(),
                    panel.grid = element_line(color = "gray60",
                                              size = 0.20,
                                              linetype = 'dotted'), #grid style
                    panel.background = element_blank(),             #background
                    axis.title.x = element_text(size = 13),         #x label style
                    axis.text.x = element_text(size = 9),         #x axis values style
                    axis.title.y = element_text(size = 13),         #y label style
                    axis.text.y = element_text(size = 9),         #y axis values style
                    legend.position='bottom',                      #legend position
                    plot.title = element_text(size=14, face='bold'),
                    plot.subtitle = element_text(size=13)
                    )
ggplot(dataage, aes(age_class, prop, group=1)) +
  #ylim(0, 0.6) +
  #Predicted: ---
  geom_line(data=modage,aes(age_class, prop_fit),colour = "gray85", size=0.3)+
  geom_point(data=modage,aes(age_class, prop_fit, col=legend_lab2, shape=legend_lab2), size=2.3) +
  #Predicted IC: ---
  geom_line(data=modage,aes(age_class, prob_high), color='gray65', linetype='dashed', size=0.3) + #ls IC
  geom_line(data=modage,aes(age_class, prob_low), color='gray65', linetype='dashed', size=0.3) +  #li IC
  geom_ribbon(data=modage, aes(ymin=prob_low,ymax=prob_high), fill="gray96", alpha=0.5) + #fill IC
  #Observed: ---
  geom_line(color='gray70', size=0.3)+
  geom_point(data=dataage, aes(age_class, prop, group=1, col=legend_lab, shape=legend_lab), size=2.3)  +
  #MISC: ---
  my_gg_theme +
  xlab('Age') +
  ylab('Propensity')+
  scale_color_manual(name='',values=c("gray18", 'gray50')) +
  scale_shape_manual(name='',values=c(15, 17)) +
  scale_y_continuous(breaks=c(0,0.1,0.2,0.3, 0.4, 0.5, 0.6)) +
  labs(title="Propensity of death - observed vs predicted", subtitle='by Age, using Test data')
#ggsave('Age_class_obs_pred.png', dpi=750, plot=last_plot(), width = 5*1.1, height = 4.3*1.1) #save
#__________________________________________________________________________________


#occupancy - class 4.2
##########################################
# Aggregate predicted probability and observed death propensity by
# hcu occupancy intervals. Also compute pseudo confidence intervals:

head(test)
summary(test$ph_mean.1)
test$phc <- with(test, cut(ph_mean.1, 
                            breaks=c(0,10,17,25,35,81), right =FALSE ), labels = c())
summary(test$phc)
tab <- with(test,by(death, phc, mean))
prop <- as.vector(tab)
tab2 <- with(test,by(fit, phc, mean))
prop_fit <- as.vector(tab2)
ph_class <- names(tab)
dataph <- data.frame(ph_class, prop)
dataph$legend_lab <- 'Observed' #label for legend
modph <- data.frame(ph_class, prop_fit)
modph$legend_lab2 <- 'Predicted' #label for legend

# Add confidence interval (CI):
prob_low <- with(test, by(fit_low, phc, mean))
modph$prob_low <- as.vector(prob_low)
prob_high <- with(test, by(fit_high, phc, mean))
modph$prob_high <- as.vector(prob_high)

my_gg_theme_ph <- theme(
  text = element_text(family = "Times"), #text font 
  axis.line = element_line(),
  panel.grid = element_line(color = "gray60",
                            size = 0.20,
                            linetype = 'dotted'), #grid style
  panel.background = element_blank(),             #background
  axis.title.x = element_text(size = 13),         #x label style
  axis.text.x = element_text(size = 11),         #x axis values style
  axis.title.y = element_text(size = 13),         #y label style
  axis.text.y = element_text(size = 10),         #y axis values style
  legend.position='bottom',                      #legend position
  plot.title = element_text(size=14, face='bold'),
  plot.subtitle = element_text(size=13)
)

ggplot(dataph, aes(ph_class, prop, group=1)) +
  #ylim(0, 0.6) +
  #Predicted: ---
  geom_line(data=modph,aes(ph_class, prop_fit),colour = "gray85", size=0.3)+
  geom_point(data=modph,aes(ph_class, prop_fit, col=legend_lab2, shape=legend_lab2), size=2.3) +
  #Predicted IC: ---
  geom_line(data=modph,aes(ph_class, prob_high), color='gray65', linetype='dashed', size=0.3) + #ls IC
  geom_line(data=modph,aes(ph_class, prob_low), color='gray65', linetype='dashed', size=0.3) +  #li IC
  geom_ribbon(data=modph, aes(ymin=prob_low,ymax=prob_high), fill="gray96", alpha=0.5) + #fill IC
  #Observed: ---
  geom_line(color='gray70', size=0.3)+
  geom_point(data=dataph, aes(ph_class, prop, group=1, col=legend_lab, 
                              shape=legend_lab), size=2.3)  +
  #MISC: ---
  my_gg_theme_ph +
  xlab('hcu occupancy') +
  ylab('Propensity')+
  scale_color_manual(name='',values=c("gray18", 'gray50')) +
  scale_shape_manual(name='',values=c(15, 17)) +
  scale_y_continuous(breaks=c(0,0.2,0.225,0.25, 0.275, 0.3, 0.325)) +
  labs(title="Propensity of death - observed vs predicted", subtitle='by hcu occupancy, using Test data')
#ggsave('ph_class_obs_pred.tiff', dpi=300, plot=last_plot(), width = 5.4*1.1, height = 3.8*1.1) #save
#__________________________________________________________________________________



#Comorbities        -    4.3
#####################################
# Aggregate predicted probability and observed death propensity 
# individual comorbidity. Also compute pseudo confidence intervals:

head(test_com)
test_com$freq <- as.numeric(table(test_com$dc_af)[test_com$dc_af])
test_com$dc_af <- factor(test_com$dc_af)
head(test)
head(test_com,15)
test_com2 <- join(test_com[,c(1,7,8)],test,by ="id")
head(test_com2,30)
com <- aggregate(list(p_death = test_com2$death,p_fit = test_com2$fit, p_low = test_com2$fit_low,
                      p_high = test_com2$fit_high ),
                  list(comc =test_com2$dc_af), FUN=mean) 
com <- com[order(com$p_death),]
com
summary(com)
com$legend_lab <- 'Observed' #label for legend
com$legend_lab2 <- 'Predicted' #label for legend
head(com)

################################################ 
my_gg_theme_com <- theme(
  text = element_text(family = "Times"), #text font 
  axis.line = element_line(),
  panel.grid = element_line(color = "gray60",
                            size = 0.20,
                            linetype = 'dotted'), #grid style
  panel.background = element_blank(),             #background
  axis.title.x = element_text(size = 13),         #x label style
  axis.text.x = element_text(size = 9, angle=60, hjust=1),         #x axis values style
  axis.title.y = element_text(size = 13),         #y label style
  axis.text.y = element_text(size = 9),         #y axis values style
  legend.position='bottom',                      #legend position
  plot.title = element_text(size=14, face='bold'),
  plot.subtitle = element_text(size=13)
)


ggplot(com, aes(reorder(comc,p_death), p_death) ) +
  #Predicted errorbar: ---
  geom_errorbar(data=com,aes(ymin = p_low, ymax = p_high), color='gray75', width = 0.35, size = 0.9) +
  #Predicted: ---
  geom_point(data=com,aes(reorder(comc,p_death), p_fit, col=legend_lab2, shape=legend_lab2), size=2.3) +
  #Observed: ---
  geom_point(data=com, aes(reorder(comc,p_death), p_death, group=1, col=legend_lab, shape=legend_lab), size=2.3)  +
  #MISC: ---
  my_gg_theme_com +
  xlab('Comorbidity') +
  ylab('Propensity') +
  scale_color_manual(name='',values=c("gray18", 'gray68')) +
  scale_shape_manual(name='',values=c(15, 17)) +
  scale_y_continuous(breaks=c(0,0.1,0.2,0.3, 0.4, 0.5, 0.6)) +
  labs(title="Propensity of death - observed vs predicted", subtitle='by Comorbidity, using Test data')

#ggsave('com_class_obs_pred_shifted.tiff', dpi=300, plot=last_plot(), width = 5.5*1.1, height = 4.3*1.1) #save
rm(test_com2)



############### Comparative boxplots    -   5. ###########
# Compare distribution of a varible for the subpopulations that
# lived Vs died:


############### SENS VS SPEC    -   5. ###########
# Compare distribution of a varible for the subpopulations that
# lived Vs died:


#### CUT_OFF - 5.1 #######
# (we need it to perform death predictions (0 or 1)):
# Note: to be rigorous, we obtained the cut-off point from train data
par(mfrow=c(1,1),mgp=c(2,1,0),font.axis=2,font.lab=1.2,lwd=1,cex.lab=1.2)
par(mai=c(1,1,1,1))

fit_train<-predict(m1,strain, type="response")
par(mfrow=c(1,1))
pred <- prediction(fit_train, train$death)
perf=performance(pred,"acc")
cut_off=pred@cutoffs
cf=cut_off[[1]] # array of possible cut-offs

sens=performance(pred,"sens")
espec=performance(pred,"spec")

sensibilidade <- sens@y.values[[1]]
especificidade=espec@y.values[[1]]

optimo=cf[which.min(abs(sensibilidade-especificidade))]
#0.2873345
sens_opt = sensibilidade[which.min(abs(sensibilidade-especificidade))]
#0.7316815


#### PLOT SENS VS SPEC  -  5.2 ####

df_plot_sens_spec = data.frame(sensibilidade = sensibilidade)
df_plot_sens_spec$cf <- cf
df_plot_sens_spec$especificidade <- especificidade
df_plot_sens_spec$sens_lab <- 'Sensitivity' #label para a legenda
df_plot_sens_spec$spec_lab <- 'Specificity' #label para a legenda

# Plot configuration:
windowsFonts("Times" = windowsFont("TT Times New Roman")) #Load font
my_gg_theme2 <- theme(
  text = element_text(family = "Times"), #text font 
  axis.line = element_line(),
  panel.grid = element_line(color = "gray60",
                            size = 0.20,
                            linetype = 'dotted'), #grid style
  panel.background = element_blank(),             #background
  axis.title.x = element_text(size = 13),         #x label style
  axis.text.x = element_text(size = 9),         #x axis values style
  axis.title.y = element_text(size = 13),         #y label style
  axis.text.y = element_text(size = 9),         #y axis values style
  legend.position='bottom',                      #legend position
  plot.title = element_text(size=14, face='bold'),
  plot.subtitle = element_text(size=13)
)

ggplot(df_plot_sens_spec, aes(cf, sensibilidade, group=1)) +
  ylim(0, 1) +
  #Sensitivity: ---
  geom_line(data=df_plot_sens_spec, aes(cf, sensibilidade, col=sens_lab, linetype=sens_lab), size=0.5)+
  #Specificity: ---
  geom_line(data=df_plot_sens_spec, aes(cf, especificidade, col=spec_lab, linetype=spec_lab), size=0.6)+
  #Vertical cut-off line: ---
  geom_linerange(data=df_plot_sens_spec,aes(x=optimo, ymin = 0, ymax = sens_opt), linetype='dotted') +
  #Add text:
  annotate("text", x = 0.71, y = 0.72, label = "Optimal cut-off: 0.287", hjust = 0) +
  annotate("text", x = 0.71, y = 0.72-0.06, label = "AUC(Train): 0.811", hjust = 0) +
  #MISC: ---
  my_gg_theme2 +
  xlab('Cut-off probability') +
  ylab('Metric')+
  scale_color_manual(name='',values=c("gray18", 'gray50')) +
  scale_linetype_manual(name='', values=c('solid', 'dashed'))+
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(expand=c(0.01,0.013), breaks=c(0,0.2,round(optimo[[1]],3),0.4,0.6,0.8,1)) +
  labs(title="Fit metrics - sensitivity vs specificity", subtitle='optimal cut-off, using Train data')

#ggsave('opt_cutoff_metrics_train.tiff', dpi=350, plot=last_plot(), width = 5.3*1.1, height = 4.3*1.05) #save
#__________________________________________________________________________________
