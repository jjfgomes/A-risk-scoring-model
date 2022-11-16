
#setwd(...your directory...)


#### Run previous scripts:
source ("01_train_build_model.R")
source ("02_test_performance.R")
library(faraway)

################################################################################
#### Plot 1: qq-plot

# Logit predictions with standard error with test:
fit <- predict.glm(m1, test, se.fit=TRUE) 
# Divide into deciles:
v <- cut(fit$fit, breaks=quantile(fit$fit,probs = seq(0, 1, 0.1)), right = TRUE)
marginSums(table(test$death,v),2)

# Compute average observed in each decile:
p1 <- prop.table(table(test$death,v),2)[2,]

# Compute lower bound in each decile:
score_li <- fit$fit-3*fit$se.fit
p2 <- tapply(ilogit(score_li ),v,mean)

# Compute upper bound in each decile:
score_ls <- fit$fit+3*fit$se.fit
p3 <- tapply(ilogit(score_ls ),v,mean)

# Compute actual probability in each decile:
score <- fit$fit
p4 <- tapply(ilogit(score),v,mean)


# Build dataframe for plot:
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

# Add labels to plot:
df_qq_plot$legend_lab <- 'Observed'
df_qq_plot$legend_lab2 <- 'Predicted'

# Transform x into factor (fix variable order):
df_qq_plot$max_pred_str <- factor(df_qq_plot$max_pred_str, levels = df_qq_plot$max_pred_str[order(df_qq_plot$obs)])
df_qq_plot


# Library to use other font (Times New Roman):
library(extrafont)
#font_import()  
#loadfonts(device = "win")

# Plot:
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
  #plot.title = element_text (hjust = 0.5)       #title adjust to center
  #plot.subtitle = element_text (hjust = 0.5)    #subtitle adjust to center
  plot.title = element_text(size=13, face='bold'),
  plot.subtitle = element_text(size=12)
)
ggplot(df_qq_plot, aes(max_pred_str, obs, group=1)) +
  #Predicted: ---
  geom_point(data=df_qq_plot,aes(max_pred_str, pred, col=legend_lab2, shape=legend_lab2), size=2.3) +
  #Predicted IC: ---
  geom_line(data=df_qq_plot,aes(max_pred_str, pred_ls), color='gray65', linetype='dashed', size=0.3) + #ls IC
  geom_line(data=df_qq_plot,aes(max_pred_str, pred_li), color='gray65', linetype='dashed', size=0.3) +  #li IC
  geom_ribbon(data=df_qq_plot, aes(ymin=pred_li,ymax=pred_ls), fill="gray96", alpha=0.5) + #fill IC
  #Observed: ---
  geom_point(data=df_qq_plot, aes(max_pred_str, obs, group=1, col=legend_lab, shape=legend_lab), size=2.3)  +
  #MISC: ---
  my_gg_theme_qq +
  xlab('Model probability (deciles)') +
  ylab('Observed')+
  scale_color_manual(name='', values=c("gray18", 'gray50')) +
  scale_shape_manual(name='', values=c(15, 17)) +
  scale_y_continuous(breaks=c(0,0.1,0.2,0.3, 0.4, 0.5, 0.6)) +
  labs(title="Fit quality: qq-plot", subtitle='using deciles and Test data')

# Save:
ggsave('qq_plot_fit_quality.png', dpi=750, plot=last_plot(), width = 5*1.1, height = 4.3*1.1)

#Legend (example): observed average level against the predicted probabilities grouped by deciles.
#The lower and upper CI limits correspond to the avg(fit+-3xsd(fit)) for each decile.




################################################################################
#### Plot 2: obs. vs pred. - Age

# Build age categories as a new variable. Age cut-offs are 18,40,50,60,65,70,75,
# 80,85,90,105:
test$agec <- with(test, cut(age.1, 
                            breaks=c(18,40,50,60,65,70,75,80,85,90,105),
                            right =FALSE ),
                  labels = c())
summary(test$agec)
head(test)

# Compute predictions and build dataframe for plot:
fit <- predict(m1,test, se.fit=TRUE)
test$fit <- ilogit(fit$fit)
score_li <- fit$fit-3*fit$se.fit
test$fit_low <- ilogit(score_li )
score_ls <- fit$fit+3*fit$se.fit
test$fit_high <- ilogit(score_ls )

# Compute observed mean by each age category:
tab <- with(test,by(death, agec, mean))
prop <- as.vector(tab)

# Compute probability mean by each age category:
tab2 <- with(test,by(fit, agec, mean))
prop_fit <- as.vector(tab2)
age_class <- names(tab)


dataage <- data.frame(age_class, prop)
dataage$legend_lab <- 'Observed' #label for legend

modage <- data.frame(age_class, prop_fit)
modage$legend_lab2 <- 'Predicted' #label for legend
#ADD CI:
prob_low <- with(test, by(fit_low, agec, mean))
modage$prob_low <- as.vector(prob_low)
prob_high <- with(test, by(fit_high, agec, mean))
modage$prob_high <- as.vector(prob_high)

# Plot:
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
  #plot.title = element_text (hjust = 0.5)       #title adjust to center
  #plot.subtitle = element_text (hjust = 0.5)    #subtitle adjust to center
  plot.title = element_text(size=13, face='bold'),
  plot.subtitle = element_text(size=12)
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
# Save:
ggsave('Age_class_obs_pred.png', dpi=750, plot=last_plot(), width = 5*1.1, height = 4.3*1.1) #save


#Legend (example): model adjustment to Age using test data. The lower and upper CI
# limits correspond to the avg(fit+-3xsd(fit)) for each age group.



################################################################################
#### Plot 3: obs. vs pred. - Hospital pressure

# Divide hospital pressure values into different categories.
# cut-off values are 0,10,20,30,50,81:
test$phc <- with(test, cut(ph_mean.1, 
                           breaks=c(0,10,20,30,50,81), right =FALSE ),
                 labels = c())
summary(test$phc)

# Get mean observed for each pressure category:
tab <- with(test,by(death, phc, mean))
prop <- as.vector(tab)

# Get mean probability for each pressure category:
tab2 <- with(test,by(fit, phc, mean))
prop_fit <- as.vector(tab2)

# Create dataframes for plot:
ph_class <- names(tab)
dataph <- data.frame(ph_class, prop)
dataph$legend_lab <- 'Observed' #label para a legenda

modph <- data.frame(ph_class, prop_fit)
modph$legend_lab2 <- 'Predicted' #label para a legenda
# Add CI:
prob_low <- with(test, by(fit_low, phc, mean))
modph$prob_low <- as.vector(prob_low)
prob_high <- with(test, by(fit_high, phc, mean))
modph$prob_high <- as.vector(prob_high)

# Plot:
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
  #plot.title = element_text (hjust = 0.5)       #title adjust to center
  #plot.subtitle = element_text (hjust = 0.5)    #subtitle adjust to center
  plot.title = element_text(size=13, face='bold'),
  plot.subtitle = element_text(size=12)
)

ggplot(dataph, aes(ph_class, prop, group=1)) +
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
  xlab('Hcu pressure') +
  ylab('Propensity')+
  scale_color_manual(name='',values=c("gray18", 'gray50')) +
  scale_shape_manual(name='',values=c(15, 17)) +
  scale_y_continuous(breaks=c(0,0.2,0.225,0.25, 0.275, 0.3, 0.325)) +
  labs(title="Propensity of death - observed vs predicted", subtitle='by Hcu pressure, using Test data')
# Save:
ggsave('ph_class_obs_pred.png', dpi=750, plot=last_plot(), width = 5*1.1, height = 4.3*1.1) #save

#Legend (example): model adjustment to ph (hospital pressure) using test data. 
# The lower and upper CI limits correspond to the avg(fit+-3xsd(fit)) for each 
# age group.




################################################################################
#### Plot 4: obs. vs pred. - comorbidities

# Using the test_com dataframe created in the beginning of 02_test_performance.R:
# (this dataframe stills has multiple lines per patient id, one for each comorbidity)

# Compute average observed and average probability by comorbidity:
head(test_com)
test_com$freq <- as.numeric(table(test_com$dc_af)[test_com$dc_af])
test_com$dc_af <- factor(test_com$dc_af)
head(test_com,15)

test_com2 <- join(test_com[,c(1,7,8)],test,by ="id")
head(test_com2,30)
com <- aggregate(list(p_death = test_com2$death,p_fit = test_com2$fit, p_low = test_com2$fit_low,
                      p_high = test_com2$fit_high ),
                 list(comc =test_com2$dc_af), FUN=mean) 
# Order by ascending observed probability:
com <- com[order(com$p_death),]
com
summary(com)
com$legend_lab <- 'Observed' #label for legend
com$legend_lab2 <- 'Predicted' #label for legend
head(com)


# Plot: 
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
  #plot.title = element_text (hjust = 0.5)       #title adjust to center
  #plot.subtitle = element_text (hjust = 0.5)    #subtitle adjust to center
  plot.title = element_text(size=13, face='bold'),
  plot.subtitle = element_text(size=12)
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

# Save
ggsave('com_class_obs_pred.png', dpi=750, plot=last_plot(), width = 5*1.1, height = 4.3*1.1)
rm(test_com2)

#Legend (example): model adjustment to each comorbidity using test data. The
# lower and upper error limits correspond to the avg(fit+-3xsd(fit)) for each 
# age group.



################################################################################
#### Plot 5: obs. vs pred. - gender

# Group avg observed and probability by gender:
test_gen <- test
gen <- aggregate(list(p_death = test_gen$death, p_fit = test_gen$fit, p_low = test_gen$fit_low,
                      p_high = test_gen$fit_high ),
                 list(gender =test_gen$gender), FUN=mean) 
gen <- gen[order(gen$gender),]
gen
summary(gen)
gen$legend_lab <- 'Observed' #label for legend
gen$legend_lab2 <- 'Predicted' #label for legend
head(gen)

# Plot: 
my_gg_theme_gen <- theme(
  text = element_text(family = "Times"), #text font 
  axis.line = element_line(),
  panel.grid = element_line(color = "gray60",
                            size = 0.20,
                            linetype = 'dotted'), #grid style
  panel.background = element_blank(),             #background
  axis.title.x = element_text(size = 13),         #x label style
  axis.text.x = element_text(size = 11),         #x axis values style
  axis.title.y = element_text(size = 13),         #y label style
  axis.text.y = element_text(size = 9),         #y axis values style
  legend.position='bottom',                      #legend position
  #plot.title = element_text (hjust = 0.5)       #title adjust to center
  #plot.subtitle = element_text (hjust = 0.5)    #subtitle adjust to center
  plot.title = element_text(size=13, face='bold'),
  plot.subtitle = element_text(size=12)
)


ggplot(gen, aes(gender, p_death) ) +
  #Predicted errorbar: ---
  geom_errorbar(data=gen,aes(ymin = p_low, ymax = p_high), color='gray75', width = 0.35, size = 0.9) +
  #Predicted: ---
  geom_point(data=gen,aes(gender, p_fit, col=legend_lab2, shape=legend_lab2), size=2.3) +
  #Observed: ---
  geom_point(data=gen, aes(gender, p_death, group=1, col=legend_lab, shape=legend_lab), size=2.3)  +
  #MISC: ---
  my_gg_theme_gen +
  xlab('Gender') +
  ylab('Propensity') +
  scale_color_manual(name='',values=c("gray18", 'gray68')) +
  scale_shape_manual(name='',values=c(15, 17)) +
  labs(title="Propensity of death - observed vs predicted", subtitle='by Gender, using Test data')

# Save:
ggsave('gen_class_obs_pred.png', dpi=750, plot=last_plot(), width = 5*1.1, height = 4.3*1.1) 

#Legend (example: model adjustment to each gender using test data. The lower and
# upper error limits correspond to the avg(fit+-3xsd(fit)) for each age group.



################################################################################
#### Plot 6: obs. vs pred. - sensitivity Vs specificity

df_plot_sens_spec = data.frame(sensibilidade = sensibilidade)
df_plot_sens_spec$cf <- cf
df_plot_sens_spec$especificidade <- especificidade
df_plot_sens_spec$sens_lab <- 'Sensitivity' #label para a legenda
df_plot_sens_spec$spec_lab <- 'Specificity' #label para a legenda


# PLot:
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
  #plot.title = element_text (hjust = 0.5)       #title adjust to center
  #plot.subtitle = element_text (hjust = 0.5)    #subtitle adjust to center
  plot.title = element_text(size=13, face='bold'),
  plot.subtitle = element_text(size=12)
)

ggplot(df_plot_sens_spec, aes(cf, sensibilidade, group=1)) +
  ylim(0, 1) +
  #sensitivity: ---
  geom_line(data=df_plot_sens_spec, aes(cf, sensibilidade, col=sens_lab, linetype=sens_lab), size=0.5)+
  #specificity: ---
  geom_line(data=df_plot_sens_spec, aes(cf, especificidade, col=spec_lab, linetype=spec_lab), size=0.6)+
  #Vertical line cut-off: ---
  geom_linerange(data=head(df_plot_sens_spec,1),aes(x=optimo, ymin = 0, ymax = sens_opt), linetype='dotted') +
  #Add text:
  annotate("text", x = 0.69, y = 0.72, label = "Optimal cut-off: 0.289", hjust = 0) +
  annotate("text", x = 0.69, y = 0.72-0.06, label = "AUC(Train): 0.807", hjust = 0) +
  #MISC: ---
  my_gg_theme2 +
  xlab('Cut-off probability') +
  ylab('Metric')+
  scale_color_manual(name='',values=c("gray18", 'gray50')) +
  scale_linetype_manual(name='', values=c('solid', 'dashed'))+
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(expand=c(0.01,0.013), breaks=c(0,0.2,round(optimo[[1]],3),0.4,0.6,0.8,1)) +
  labs(title="Fit metrics - sensitivity vs specificity", subtitle='optimal cut-off, using Train data')

# Save:
ggsave('opt_cutoff_metrics_train.png', dpi=750, plot=last_plot(), width = 5*1.1, height = 4.3*1.1)

#Legend (example: sensitivity and specificity of the fitted model, using Train
# data. Optimal cut-off corresponds to the probability where sens. = spec..




################################################################################
#### Plot 7: comorbidity patient profiles


# Get top most frequent comorbidity combinations:
if (!require(reshape2)){
  install.packages('reshape2')
  library(reshape2)
}
train_original <- read.table("train.txt", h=T)
comorb_combinations = dcast(train_original,id~dc_af,value.var = "dc_af") #transpose by id the dc_af values
comorb_combinations <- plyr::count(comorb_combinations[-1]) #get the combinations
comorb_combinations <- comorb_combinations[order(-comorb_combinations$freq), ] #sort by descending frequency
head(comorb_combinations,5)

comorb_combinations6 <- comorb_combinations[comorb_combinations$COVID == 1,]
comorb_combinations6 <- comorb_combinations6[comorb_combinations6$AC == 1,]



# Read txt with commorbidities and corresponding score:
score_dc_af <- read.table("score_dc_af.txt", h=T)

#dc_af	score_dc_af (between 0 and 1)
#TABACO	0
#OB	0.002044844
#HTA	0.07735981
#PV	0.096705896
#COVID	0.111618768
#TEP	0.146376539
#HIPONA	0.175054969
#DM	0.198229416
#PCOV	0.235093067
#ILNP	0.251208352
#DAA	0.286556361
#ANE	0.286596768
#DPCO	0.292985055
#AVC	0.340849677
#EM	0.360187086
#HTP	0.377077809
#PB	0.410984437
#RESPA	0.432897765
#RC	0.433655974
#IC	0.447119801
#FA	0.45293565
#IRA	0.527524987
#IH	0.567820132
#AC	0.593422299
#NEO	0.620059741
#PF	0.754642209
#SEPSIS	1

#COVID	COVID-19
#IRC	Chronic kidney failure
#DM	Diabetes mellitus
#FA	Atrial fibrillation
#ANE	Anemia
#NEO	Neoplastic disease (cancer)
#HTA	Arterial hypertension
#SEPSIS	Septicemia
#DPCO	Chronic respiratory disease
#HIPONA	Hyponatremia
#PB	Bacterial pneumonia
#PCOV	Pneumonia to SARSCoV
#IRESPA	Accute breathing insufficiency
#IC	Cardiac insufficiency
#AVC	Acute cerebrovascular disease
#ILNP	Non-pulmonary localized infection
#PV	Viral pneumonia
#OB	Obesity
#IRA	Acute kidney failure
#TEP	Pulmonary thromboembolism
#EM	Ischemic heart disease
#DAA	Acute abdominal disease
#IH	Liver failure
#TABACO	Smoking
#PF	Fungal pneumonia
#HTP	Pulmonary hypertension
#AC	Coagulation changes

# Interesting profiles:
#> Perfil 2: Chronic respiratory disease (DPCO) (diferente de asma)
#> Perfil 3: Diabetes mellitus(DM) + 	Arterial hypertension (HTA) + Obesity (OB)
#> Perfil 4: Bacterial pneumonia (PB) + Pneumonia to SARSCoV (PCOV)
#> Perfil 5: Pulmonary thromboembolism (TEP) + (-Coagulation changes (AC)) + PCOV + HTA
#> Perfil 6: Accute breathing insufficiency (IRESPA) + Cardiac insufficiency (IC) + PCOV



# Build vector of age for the profiles:
age_profiles_vec <- c(50,60,70,75,80,85,90)
age_profiles_vec_scaled <- age_profiles_vec
for(i in 1:length(age_profiles_vec)) {
  age_profiles_vec_scaled[i] = (age_profiles_vec_scaled[i]-mean(train$age))/sd(train$age)
}
age_profiles_vec_scaled

# To get probabilities of each profile we will use the average hospital pressure
# and average hospital score (obtained in train):
avg_score_hops_scaled <- mean(strain$hosp_sc1)
avg_pressure_scaled <- mean(strain$ph_mean)

score_female <- unique(strain$gen_sc1)[1] #-1.055945
score_male <- unique(strain$gen_sc1)[2]   #0.9469954

#>> Profile 1: only covid (most common profile) ---------------------
profile1_com_score <- sum(subset(score_dc_af, dc_af  %in% c("COVID"))$score_dc_af) #0.111618768
profile1_com_score_scaled <- (profile1_com_score-mean(train$dc_af_sc_sum))/sd(train$dc_af_sc_sum) #scale
profile1_com_score_scaled

#>> Profile 2: covid + DPCO  ---------------------
profile2_com_score <- sum(subset(score_dc_af, dc_af  %in% c("COVID", "DPCO"))$score_dc_af) #0.111618768 + 0.292985055
profile2_com_score_scaled <- (profile2_com_score-mean(train$dc_af_sc_sum))/sd(train$dc_af_sc_sum) #scale
profile2_com_score_scaled

#>> Profile 3: covid + DM + HTA + OB ---------------------
profile3_com_score <- sum(subset(score_dc_af, dc_af  %in% c("COVID", "DM", "HTA", "OB"))$score_dc_af) #0.111618768 + 0.198229416 + 0.07735981 + 0.002044844
profile3_com_score_scaled <- (profile3_com_score-mean(train$dc_af_sc_sum))/sd(train$dc_af_sc_sum) #scale
profile3_com_score_scaled

#>> Profile 4: covid + PB + PCOV ---------------------
profile4_com_score <- sum(subset(score_dc_af, dc_af  %in% c("COVID", "PB", "PCOV"))$score_dc_af) #0.111618768 + 0.410984437 + 0.235093067
profile4_com_score_scaled <- (profile4_com_score-mean(train$dc_af_sc_sum))/sd(train$dc_af_sc_sum) #scale
profile4_com_score_scaled

#>> Profile 5: covid + TEP + HTA ---------------------
profile5_com_score <- sum(subset(score_dc_af, dc_af  %in% c("COVID", "HTA", "TEP"))$score_dc_af)
profile5_com_score_scaled <- (profile5_com_score-mean(train$dc_af_sc_sum))/sd(train$dc_af_sc_sum) #scale
profile5_com_score_scaled

#>> Profile 6: covid + IRESPA + IC ---------------------
profile6_com_score <- sum(subset(score_dc_af, dc_af  %in% c("COVID", "IRESPA", "IC"))$score_dc_af)
profile6_com_score_scaled <- (profile6_com_score-mean(train$dc_af_sc_sum))/sd(train$dc_af_sc_sum) #scale
profile6_com_score_scaled


# Vector profiles comrbidities:
profile_com_vec <- c(profile1_com_score_scaled, profile2_com_score_scaled, profile3_com_score_scaled,
                     profile4_com_score_scaled, profile5_com_score_scaled, profile6_com_score_scaled)
profile_df_male <- data.frame(age=age_profiles_vec)
profile_df_female <- profile_df_male


# LOOP: for each profile,we will vary the age:
for(i in 1:length(profile_com_vec)) { 
  # Initialize vectors:
  col_female <- seq(1, length(age_profiles_vec_scaled), 1)
  col_male <- seq(1, length(age_profiles_vec_scaled), 1)
  for(j in 1:length(age_profiles_vec_scaled)) { #loop de age
    #Female probability:
    prob_female <- data.frame(age=age_profiles_vec_scaled[j], ph_mean = avg_pressure_scaled,
                              dc_af_sc_sum = profile_com_vec[i], gen_sc1 = score_female,
                              hosp_sc1 = avg_score_hops_scaled)
    prob_female <- predict.glm(m1, prob_female, type="response")[[1]]
    col_female[j] <- round(prob_female, 3)
    #Male probability:
    prob_male <- data.frame(age=age_profiles_vec_scaled[j], ph_mean = avg_pressure_scaled,
                              dc_af_sc_sum = profile_com_vec[i], gen_sc1 = score_male,
                              hosp_sc1 = avg_score_hops_scaled)
    prob_male <- predict.glm(m1, prob_male, type="response")[[1]]
    col_male[j] <- round(prob_male, 3)
  }
  if (i==1){ #Profile 1
    profile_df_female$profile_1 <- col_female
    profile_df_male$profile_1 <- col_male
  }
  if (i==2){ #Profile 2
    profile_df_female$profile_2 <- col_female
    profile_df_male$profile_2 <- col_male
  }
  if (i==3){ #Profile 3
    profile_df_female$profile_3 <- col_female
    profile_df_male$profile_3 <- col_male
  }
  if (i==4){ #Profile 4
    profile_df_female$profile_4 <- col_female
    profile_df_male$profile_4 <- col_male
  }
  if (i==5){ #Profile 5
    profile_df_female$profile_5 <- col_female
    profile_df_male$profile_5 <- col_male
  }
  if (i==6){ #Profile 6
    profile_df_female$profile_6 <- col_female
    profile_df_male$profile_6 <- col_male
  }
}
profile_df_female
profile_df_male

#Legend (example): Probability of death for multiple comorbidity profiles,
# differentiated by gender.


###### -- PLOT -- ######

profile_df_male$prof1_lab <- "Profile 1 (M)"
profile_df_male$prof2_lab <- "Profile 2 (M)"
profile_df_male$prof3_lab <- "Profile 3 (M)"
profile_df_male$prof4_lab <- "Profile 4 (M)"
profile_df_male$prof5_lab <- "Profile 5 (M)"
profile_df_male$prof6_lab <- "Profile 6 (M)"

profile_df_female$prof1_lab <- "Profile 1 (F)"
profile_df_female$prof2_lab <- "Profile 2 (F)"
profile_df_female$prof3_lab <- "Profile 3 (F)"
profile_df_female$prof4_lab <- "Profile 4 (F)"
profile_df_female$prof5_lab <- "Profile 5 (F)"
profile_df_female$prof6_lab <- "Profile 6 (F)"


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
  #plot.title = element_text (hjust = 0.5)       #title adjust to center
  #plot.subtitle = element_text (hjust = 0.5)    #subtitle adjust to center
  plot.title = element_text(size=13, face='bold'),
  plot.subtitle = element_text(size=12)
)

ggplot(profile_df_female, aes(age, profile_1, group=1)) +
  #> Profile 1:  
  geom_line(data=profile_df_female,aes(age, profile_1),colour = "gray85", size=0.3)+
  geom_point(data=profile_df_female,aes(age, profile_1, col=prof1_lab, shape=prof1_lab), size=2.3) +  
  geom_line(data=profile_df_male,aes(age, profile_1),colour = "gray85", size=0.3, linetype = 'dashed')+
  geom_point(data=profile_df_male,aes(age, profile_1, col=prof1_lab, shape=prof1_lab), size=2.3) +
  
  #> Profile 2:
  geom_line(data=profile_df_female,aes(age, profile_2),colour = "gray75", size=0.3)+
  geom_point(data=profile_df_female,aes(age, profile_2, col=prof2_lab, shape=prof2_lab), size=2.3) +  
  geom_line(data=profile_df_male,aes(age, profile_2),colour = "gray75", size=0.3, linetype = 'dashed')+
  geom_point(data=profile_df_male,aes(age, profile_2, col=prof2_lab, shape=prof2_lab), size=2.3) +
    
  #> Profile 3:
  geom_line(data=profile_df_female,aes(age, profile_3),colour = "gray75", size=0.3)+
  geom_point(data=profile_df_female,aes(age, profile_3, col=prof3_lab, shape=prof3_lab), size=2.3) +  
  geom_line(data=profile_df_male,aes(age, profile_3),colour = "gray75", size=0.3, linetype = 'dashed')+
  geom_point(data=profile_df_male,aes(age, profile_3, col=prof3_lab, shape=prof3_lab), size=2.3) +
  
  #> Profile 4:
  geom_line(data=profile_df_female,aes(age, profile_4),colour = "gray75", size=0.3)+
  geom_point(data=profile_df_female,aes(age, profile_4, col=prof4_lab, shape=prof4_lab), size=2.3) +  
  geom_line(data=profile_df_male,aes(age, profile_4),colour = "gray75", size=0.3, linetype = 'dashed')+
  geom_point(data=profile_df_male,aes(age, profile_4, col=prof4_lab, shape=prof4_lab), size=2.3) +
  
  #> Profile 5:
  geom_line(data=profile_df_female,aes(age, profile_5),colour = "gray75", size=0.3)+
  geom_point(data=profile_df_female,aes(age, profile_5, col=prof5_lab, shape=prof5_lab), size=2.3) +  
  geom_line(data=profile_df_male,aes(age, profile_5),colour = "gray75", size=0.3, linetype = 'dashed')+
  geom_point(data=profile_df_male,aes(age, profile_5, col=prof5_lab, shape=prof5_lab), size=2.3) +
  
  #> Profile 6:
  geom_line(data=profile_df_female,aes(age, profile_6),colour = "gray75", size=0.3)+
  geom_point(data=profile_df_female,aes(age, profile_6, col=prof6_lab, shape=prof6_lab), size=2.3) +  
  geom_line(data=profile_df_male,aes(age, profile_6),colour = "gray75", size=0.3, linetype = 'dashed')+
  geom_point(data=profile_df_male,aes(age, profile_6, col=prof6_lab, shape=prof6_lab), size=2.3) +

  #MISC: ---
  my_gg_theme2 +
  xlab('Age') +
  ylab('Estimated Probability')+
  scale_color_manual(name='',values=c("gray14", 'gray65', 'gray14', 'gray65', 'gray14', 'gray65',
                                      'gray14', 'gray65', 'gray14', 'gray65', 'gray14', 'gray65')) +
  scale_shape_manual(name='',values=c(1, 1, 0, 0, 2, 2, 5, 5, 6, 6, 8, 8)) +
  labs(title="Disease profiles by Age", subtitle='using Train data')
# Save:
ggsave('disease_prof_by_age.png', dpi=750, plot=last_plot(), width = 5*1.1, height = 4.3*1.1)



####################################################################################
############### Graphs 8,9: 2d plots ###############################################


##############################################
#Afonso, 20/01/2022

#1) Create 2D levels prob:

#Using model m1 coefs., create func to generate probabilities:
f_m1=function(age_s,dc_sum_s,hosp_s,pressure_s,gen_s)
{
  eta = coef(m1)[[1]] +
    coef(m1)[[2]]*age_s +
    coef(m1)[[3]]*pressure_s +
    coef(m1)[[4]]*dc_sum_s +
    coef(m1)[[5]]*gen_s +
    coef(m1)[[6]]*hosp_s
  out = 1/(1+exp(-eta))
}

#Pressure key values:
pressure_s_50_train = median(strain$ph_mean)
pressure_s_25_train = quantile(strain$ph_mean, 0.25)[[1]]
pressure_s_75_train = quantile(strain$ph_mean, 0.75)[[1]]

#Hospital key values:
hosp_s_50_train = median(strain$hosp_sc1)
hosp_s_25_train = quantile(strain$hosp_sc1, 0.25)[[1]]
hosp_s_75_train = quantile(strain$hosp_sc1, 0.75)[[1]]

#Pressure key values:
gen_train_values <- as.data.frame(table(strain$gen_sc1))
gen_s_femin_train = as.double(as.vector(gen_train_values$Var1[1]))
gen_s_male_train = as.double(as.vector(gen_train_values$Var1[2]))

#Find out the dc_sum_s score range:
min_dc_sum_s_train = min(strain$dc_af_sc_sum)
max_dc_sum_s_train = max(strain$dc_af_sc_sum)

# Get mean / std from train:
age_mean = mean(train$age)
age_sd = sd(train$age)

# Function to manually rescale the age vector:
f_age=function(age)
{
  age_rescaled = (age - age_mean)/(age_sd)
}

# Initialize the vectors:
# Age granularity: 0.5 years
age_vector = seq(18,95,0.5)
# Comorbidity score granularity: 0.03
dc_sum_s_vector = seq(min_dc_sum_s_train, max_dc_sum_s_train, 0.03)

# Create structure of the matrix probabilities:
prob_50_values_train_fem=matrix(0,nrow=length(age_vector),ncol=length(dc_sum_s_vector))
prob_75_values_train_fem=matrix(0,nrow=length(age_vector),ncol=length(dc_sum_s_vector))
prob_25_values_train_fem=matrix(0,nrow=length(age_vector),ncol=length(dc_sum_s_vector))
#>> prob_50_values_train_fem: going to get prob. by using the median values of hops_s and pressure_s + gen=fem
#>> prob_75_values_train_fem: going to get prob. by using the 75q values of hops_s and pressure_s + gen=fem
#>> prob_25_values_train_fem: going to get prob. by using the 25q values of hops_s and pressure_s + gen=fem
prob_50_values_train_mas=matrix(0,nrow=length(age_vector),ncol=length(dc_sum_s_vector))
prob_75_values_train_mas=matrix(0,nrow=length(age_vector),ncol=length(dc_sum_s_vector))
prob_25_values_train_mas=matrix(0,nrow=length(age_vector),ncol=length(dc_sum_s_vector))
#>> prob_50_values_train_mas: going to get prob. by using the median values of hops_s and pressure_s + gen=male
#>> prob_75_values_train_mas: going to get prob. by using the 75q values of hops_s and pressure_s + gen=male
#>> prob_25_values_train_mas: going to get prob. by using the 25q values of hops_s and pressure_s + gen=male


# LOOP: for each age and score, compute the male/female probabilities for 3 cases:
# - good scenario: hospital pressure and hospital score are at the 25q values;
# - normal scenario: hospital pressure and hospital score are at the 50q(median) values;
# - bad scenario: hospital pressure and hospital score are at the 75q values;
# Output: two matrixes of probabilities for each age/score by gender.
for(i in 1:length(age_vector)) {
  for (j in 1:length(dc_sum_s_vector)) {
    #Feminine:
    df_50_values_train_fem <- data.frame(age=f_age(age_vector[i]), ph_mean = pressure_s_50_train,
                              dc_af_sc_sum = dc_sum_s_vector[j], gen_sc1 = gen_s_femin_train,
                              hosp_sc1 = hosp_s_50_train)
    prob_50_values_train_fem[i,j] = predict.glm(m1, df_50_values_train_fem, type="response")[[1]]
    
    df_75_values_train_fem <- data.frame(age=f_age(age_vector[i]), ph_mean = pressure_s_75_train,
                                         dc_af_sc_sum = dc_sum_s_vector[j], gen_sc1 = gen_s_femin_train,
                                         hosp_sc1 = hosp_s_75_train)
    prob_75_values_train_fem[i,j] = predict.glm(m1, df_75_values_train_fem, type="response")[[1]]
    
    df_25_values_train_fem <- data.frame(age=f_age(age_vector[i]), ph_mean = pressure_s_25_train,
                                         dc_af_sc_sum = dc_sum_s_vector[j], gen_sc1 = gen_s_femin_train,
                                         hosp_sc1 = hosp_s_25_train)
    prob_25_values_train_fem[i,j] = predict.glm(m1, df_25_values_train_fem, type="response")[[1]]
    
    #Masculine:
    df_50_values_train_mas <- data.frame(age=f_age(age_vector[i]), ph_mean = pressure_s_50_train,
                                         dc_af_sc_sum = dc_sum_s_vector[j], gen_sc1 = gen_s_male_train,
                                         hosp_sc1 = hosp_s_50_train)
    prob_50_values_train_mas[i,j] = predict.glm(m1, df_50_values_train_mas, type="response")[[1]]
    
    df_75_values_train_mas <- data.frame(age=f_age(age_vector[i]), ph_mean = pressure_s_75_train,
                                         dc_af_sc_sum = dc_sum_s_vector[j], gen_sc1 = gen_s_male_train,
                                         hosp_sc1 = hosp_s_75_train)
    prob_75_values_train_mas[i,j] = predict.glm(m1, df_75_values_train_mas, type="response")[[1]]
    
    df_25_values_train_mas <- data.frame(age=f_age(age_vector[i]), ph_mean = pressure_s_25_train,
                                         dc_af_sc_sum = dc_sum_s_vector[j], gen_sc1 = gen_s_male_train,
                                         hosp_sc1 = hosp_s_25_train)
    prob_25_values_train_mas[i,j] = predict.glm(m1, df_25_values_train_mas, type="response")[[1]]
  }
}

# For each age, find the comorbidity score for which probability = optimal cut-off probability:
cut_50_values_train_fem=matrix(0,nrow=length(age_vector),ncol=1)
cut_75_values_train_fem=matrix(0,nrow=length(age_vector),ncol=1)
cut_25_values_train_fem=matrix(0,nrow=length(age_vector),ncol=1)

cut_50_values_train_mas=matrix(0,nrow=length(age_vector),1)
cut_75_values_train_mas=matrix(0,nrow=length(age_vector),1)
cut_25_values_train_mas=matrix(0,nrow=length(age_vector),1)



#Note: variable "optimo" is the optimal cut-off point for which sensitivity =
# specificity (obtained) using train data.


# LOOP: for each age , find the score whose probability is closest to the 
# optimal cut-off probability:
for(i in 1:length(age_vector)) {
  for (j in 1:length(dc_sum_s_vector)) {
    # Initialize minimums and errors:
    if (j == 1) {
      #Feminine:
      score_50_fem_optimo = dc_sum_s_vector[1]
      error_50_fem_optimo = abs(prob_50_values_train_fem[i,1] - optimo[[1]])
      
      score_75_fem_optimo = dc_sum_s_vector[1]
      error_75_fem_optimo = abs(prob_75_values_train_fem[i,1] - optimo[[1]])
      
      score_25_fem_optimo = dc_sum_s_vector[1]
      error_25_fem_optimo = abs(prob_25_values_train_fem[i,1] - optimo[[1]])
      
      #Masculine:
      score_50_mas_optimo = dc_sum_s_vector[1]
      error_50_mas_optimo = abs(prob_50_values_train_mas[i,1] - optimo[[1]])
      
      score_75_mas_optimo = dc_sum_s_vector[1]
      error_75_mas_optimo = abs(prob_75_values_train_mas[i,1] - optimo[[1]])
      
      score_25_mas_optimo = dc_sum_s_vector[1]
      error_25_mas_optimo = abs(prob_25_values_train_mas[i,1] - optimo[[1]])
    }
    else { # fix minimums if we find a probability closer to the optimal value
      if (error_50_fem_optimo > abs(prob_50_values_train_fem[i,j] - optimo[[1]])) {
        score_50_fem_optimo = dc_sum_s_vector[j]
        error_50_fem_optimo = abs(prob_50_values_train_fem[i,j] - optimo[[1]])
      }
      if (error_75_fem_optimo > abs(prob_75_values_train_fem[i,j] - optimo[[1]])) {
        score_75_fem_optimo = dc_sum_s_vector[j]
        error_75_fem_optimo = abs(prob_75_values_train_fem[i,j] - optimo[[1]])
      }
      if (error_25_fem_optimo > abs(prob_25_values_train_fem[i,j] - optimo[[1]])) {
        score_25_fem_optimo = dc_sum_s_vector[j]
        error_25_fem_optimo = abs(prob_25_values_train_fem[i,j] - optimo[[1]])
      }
      if (error_50_mas_optimo > abs(prob_50_values_train_mas[i,j] - optimo[[1]])) {
        score_50_mas_optimo = dc_sum_s_vector[j]
        error_50_mas_optimo = abs(prob_50_values_train_mas[i,j] - optimo[[1]])
      }
      if (error_75_mas_optimo > abs(prob_75_values_train_mas[i,j] - optimo[[1]])) {
        score_75_mas_optimo = dc_sum_s_vector[j]
        error_75_mas_optimo = abs(prob_75_values_train_mas[i,j] - optimo[[1]])
      }
      if (error_25_mas_optimo > abs(prob_25_values_train_mas[i,j] - optimo[[1]])) {
        score_25_mas_optimo = dc_sum_s_vector[j]
        error_25_mas_optimo = abs(prob_25_values_train_mas[i,j] - optimo[[1]])
      }
      
    }
  }
  # For each age value, update the best comorbidity score:
  cut_50_values_train_fem[i,1] =  score_50_fem_optimo
  cut_75_values_train_fem[i,1] =  score_75_fem_optimo
  cut_25_values_train_fem[i,1] =  score_25_fem_optimo
  
  cut_50_values_train_mas[i,1] =  score_50_mas_optimo
  cut_75_values_train_mas[i,1] =  score_75_mas_optimo
  cut_25_values_train_mas[i,1] =  score_25_mas_optimo
}


#Prepare df for plotting:
#Convert score to the unscaled original space:

df_for_plot = data.frame(cut_50_values_train_fem2=cut_50_values_train_fem*sd(train$dc_af_sc_sum) + mean(train$dc_af_sc_sum))
df_for_plot$cut_75_values_train_fem2 <- cut_75_values_train_fem*sd(train$dc_af_sc_sum) + mean(train$dc_af_sc_sum)
df_for_plot$cut_25_values_train_fem2 <- cut_25_values_train_fem*sd(train$dc_af_sc_sum) + mean(train$dc_af_sc_sum)
df_for_plot$cut_50_values_train_mas2 <- cut_50_values_train_mas*sd(train$dc_af_sc_sum) + mean(train$dc_af_sc_sum)
df_for_plot$cut_75_values_train_mas2 <- cut_75_values_train_mas*sd(train$dc_af_sc_sum) + mean(train$dc_af_sc_sum)
df_for_plot$cut_25_values_train_mas2 <- cut_25_values_train_mas*sd(train$dc_af_sc_sum) + mean(train$dc_af_sc_sum)

df_for_plot$age <- age_vector
head(df_for_plot,5)
#Plot:
nrow(df_for_plot)


################################################################################
##### Plor: version with color

plot <- ggplot(df_for_plot, aes(x = age, y = cut_50_values_train_fem2, group = 1)) + 
  geom_line(col='orangered2', size=1, alpha=0.4) + 
  geom_ribbon(aes(ymin = cut_25_values_train_fem2, ymax = cut_75_values_train_fem2, colour='female thr. + IC',
                  fill='female thr. + IC'), alpha = 0.1,  size=0.7, linetype="dotdash")+
  geom_line(aes(x = age, y = cut_50_values_train_mas2, group = 1), color = "navyblue", size=1, alpha=0.4)+
  geom_ribbon(aes(ymin = cut_25_values_train_mas2, ymax = cut_75_values_train_mas2, colour='male thr. + IC', fill='male thr. + IC'),
              alpha = 0.1,  size=0.7, linetype="dotdash") +
  scale_colour_manual(name='', values=c("female thr. + IC" = "coral1", "male thr. + IC" = "lightsteelblue4"))+
  scale_fill_manual(name='', values=c("female thr. + IC" = "coral1", "male thr. + IC" = "lightsteelblue4"))

#Shuffle data:(plot some train data points in the image too)
set.seed(42)
rows <- sample(nrow(train))
train_shufled <-train[sample(nrow(train)),]

train_shufled$death_gender <- paste(train_shufled$death,"/",train_shufled$gender)
death_gender <- as.factor(head(train_shufled,150)$death_gender)
train_shufled = subset(train_shufled, select = -c(death_gender) )

# Plot:
plot +
  geom_point(data=head(train_shufled,150),aes(x=age, y=dc_af_sc_sum, shape=death_gender), alpha=0.3, color='grey3')+
  xlab('Age')+ylab('Comorbidity score')+ggtitle("Age, Comorbidity score and optimal cut-off = 0.266")+
  theme(plot.title = element_text(hjust = 0.5, face='bold'), axis.text=element_text(size=12)) +
  theme(axis.text.x = element_text(size=7.5)) +
  theme(axis.text.y = element_text(size=7.5)) +
  scale_x_continuous(breaks=c(18,25,40,55,70,85,95))+
  coord_cartesian(xlim = c(42, 95), ylim = c(-0, 2.5)) + 
  scale_shape_manual(name='Death/Gender', values=c('0 / F'=16, '0 / M'=18,'1 / F'=17, '1 / M'=4))

#Save:
ggsave('2d_cutoff_plot_color.png', dpi=750, plot=last_plot(), width = 5*1.3, height = 4.3*1.15) #save


################################################################################
##### Plot: version without color

plot <- ggplot(df_for_plot, aes(x = age, y = cut_50_values_train_fem2, group = 1)) + 
  geom_line(col='gray68', size=0.8, alpha=0.6) + 
  geom_ribbon(aes(ymin = cut_25_values_train_fem2, ymax = cut_75_values_train_fem2, colour='fem. thr. + band',
                  fill='fem. thr. + band'), alpha = 0.1,  size=0.3, linetype="dotdash")+
  geom_line(aes(x = age, y = cut_50_values_train_mas2, group = 1), color = "gray21", size=0.8, alpha=0.6)+
  geom_ribbon(aes(ymin = cut_25_values_train_mas2, ymax = cut_75_values_train_mas2, colour='male thr. + band', fill='male thr. + band'),
              alpha = 0.1,  size=0.3, linetype="dotdash"
              ) +
  scale_colour_manual(name='', values=c("fem. thr. + band" = "gray80", "male thr. + band" = "gray70"))+
  scale_fill_manual(name='', values=c("fem. thr. + band" = "gray70", "male thr. + band" = "gray15"))

#Shuffle data:(plot some train data points in the image too)
set.seed(42)
rows <- sample(nrow(train))
train_shufled <-train[sample(nrow(train)),]

train_shufled$death_gender <- paste(train_shufled$death,"/",train_shufled$gender)
death_gender <- as.factor(head(train_shufled,150)$death_gender)
train_shufled = subset(train_shufled, select = -c(death_gender) )

# Plot:
my_gg_theme2d_1 <- theme(
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
  #plot.title = element_text (hjust = 0.5)       #title adjust to center
  #plot.subtitle = element_text (hjust = 0.5)    #subtitle adjust to center
  plot.title = element_text(size=13, face='bold'),
  plot.subtitle = element_text(size=12)
)
my_gg_theme2d_1 <- theme(
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
  #plot.title = element_text (hjust = 0.5)       #title adjust to center
  #plot.subtitle = element_text (hjust = 0.5)    #subtitle adjust to center
  plot.title = element_text(size=13, face='bold'),
  plot.subtitle = element_text(size=12)
)



plot +
  geom_point(data=head(train_shufled,150),aes(x=age, y=dc_af_sc_sum, shape=death_gender), alpha=0.5, color='grey3')+
  xlab('Age')+ylab('Comorbidity score') +
  labs(title="Optimal probability boundary", subtitle='by Comorbidity score, Age and gender') +
  my_gg_theme2d_1 +
  scale_x_continuous(breaks=c(18,25,40,55,70,85,95))+
  scale_y_continuous(breaks=c(0.12,0.5,1,1.5,2,2.5))+
  coord_cartesian(xlim = c(38, 95), ylim = c(0.22, 2.4)) +
  scale_shape_manual(name='Death/Gender',values=c('0 / F'=16, '0 / M'=18,'1 / F'=17, '1 / M'=4))


#Save:
ggsave('2d_cutoff_plot_nocolor.png', dpi=750, plot=last_plot(), width = 5*1.5,height = 4.3*1.12) #save




################################################################################
##### Plot: version with just female

plot <- ggplot(df_for_plot, aes(x = age, y = cut_50_values_train_fem2, group = 1)) + 
  geom_line(col='gray42', size=0.7, alpha=0.7) + 
  geom_ribbon(aes(ymin = cut_25_values_train_fem2, ymax = cut_75_values_train_fem2),
              alpha = 0.1, linetype="dotdash", size=0.7)

#Shuffle data:(plot some train data points in the image too)
set.seed(42)
rows <- sample(nrow(test5))
test_shufled <-train[sample(nrow(test5)),]
test_shufled <- test_shufled[test_shufled$gender == 'F',]

death <- as.factor(head(test_shufled,150)$death)
test_shufled = subset(test_shufled, select = -c(death) )


# Plot:
my_gg_theme2d_2 <- theme(
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
  #plot.title = element_text (hjust = 0.5)       #title adjust to center
  #plot.subtitle = element_text (hjust = 0.5)    #subtitle adjust to center
  plot.title = element_text(size=13, face='bold'),
  plot.subtitle = element_text(size=12)
)


plot + 
  geom_point(data=head(test_shufled,150),aes(x=age, y=dc_af_sc_sum, shape=death), alpha=0.3, color='grey3')+
  xlab('Age')+
  ylab('Comorbidity score sum')+
  labs(title="Optimal probability boundary", subtitle='by comorbidity score sum, Age (female individuals)')+
  my_gg_theme2d_2 +
  scale_x_continuous(breaks=c(18,25,40,55,70,85,95))+
  scale_y_continuous(breaks=c(0.12,0.5,1,1.5,2,2.5))+
  coord_cartesian(xlim = c(38, 95), ylim = c(0.225, 2.4)) +
  scale_shape_manual(values=c('0'=16, '1'=17))

#Save:
ggsave('2d_cutoff_plot_nocolor_female.png', dpi=750, plot=last_plot(), width = 5*1.3, height = 4.3*1.15) #save

#Legend: Boundary using cut-off = 0.289 for female individuals, as a function of Comorbidity score and Age.
#The left, center and right band limits correspond to the 25th, 50th and 75th quantile values of the 
#hospital pressure and hospital scores.



################################################################################
##### Plot: version with just male

my_gg_theme2d_3 <- theme(
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
  #plot.title = element_text (hjust = 0.5)       #title adjust to center
  #plot.subtitle = element_text (hjust = 0.5)    #subtitle adjust to center
  plot.title = element_text(size=13, face='bold'),
  plot.subtitle = element_text(size=12)
)


plot <- ggplot(df_for_plot, aes(x = age, y = cut_50_values_train_mas2, group = 1)) + 
  geom_line(col='gray42', size=0.7, alpha=0.7) + 
  geom_ribbon(aes(ymin = cut_25_values_train_mas2, ymax = cut_75_values_train_mas2),
              alpha = 0.1,  size=0.7, linetype="dotdash") 

#Shuffle data:(plot some train data points in the image too)
set.seed(42)
rows <- sample(nrow(test5))
test_shufled <-train[sample(nrow(test5)),]
test_shufled <- test_shufled[test_shufled$gender == 'F',]

death <- as.factor(head(test_shufled,150)$death)
test_shufled = subset(test_shufled, select = -c(death) )

# Plot:
plot +
  geom_point(data=head(test_shufled,150),aes(x=age, y=dc_af_sc_sum, shape=death), alpha=0.3, color='grey3')+
  xlab('Age')+
  ylab('Comorbidity score sum')+
  my_gg_theme2d_3 +
  labs(title="Optimal probability boundary", subtitle='by Comorbidity score sum, Age (male individuals)')+
  scale_x_continuous(breaks=c(18,25,40,55,70,85,95))+
  scale_y_continuous(breaks=c(0.12,0.5,1,1.5,2,2.5))+
  coord_cartesian(xlim = c(38, 95), ylim = c(0.225, 2.4)) +
  scale_shape_manual(values=c('0'=18, '1'=4))

#Save:
ggsave('2d_cutoff_plot_nocolor_male.png', dpi=750, plot=last_plot(), width = 5*1.3, height = 4.3*1.15) #save)

#Legend: Boundary using cut-off = 0.289 for male individuals, as a function of Comorbidity score and Age.
#The left, center and right band limits correspond to the 25th, 50th and 75th quantile values of the 
#hospital pressure and hospital scores.








