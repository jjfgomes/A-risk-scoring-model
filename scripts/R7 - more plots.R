
# Script to create additional profile analysis + multi variable plots:


#### Run previous scripts:
rm(list =ls())
source ("R6 - test model.R")
library(faraway)
library(ROCR)
library(extrafont)


####  Most frequent comorbidity combinations: ----


if (!require(reshape2)){
  install.packages('reshape2')
  library(reshape2)
}
train_original <- as.data.frame(read_excel("E2 - train data to score.xlsx", 
                                           sheet = "T4 - data train"))
comorb_combinations = dcast(train_original,id~dc_af,value.var = "dc_af")       #transpose by id the dc_af values
comorb_combinations <- plyr::count(comorb_combinations[-1])                    #get the combinations
comorb_combinations <- comorb_combinations[order(-comorb_combinations$freq), ] #sort by descending frequency
# Display to console:
head(comorb_combinations,5)

# > List of comorbidities + scaled score (for reference):
#TABACO	0.00
#OB	0.00
#HTA	0.09
#PV	0.11
#COVID	0.12
#TEP	0.16
#HIPONA	0.19
#DM	0.21
#PCOV	0.24
#ILNP	0.26
#ANE	0.30
#DAA	0.31
#DPCO	0.33
#AVC	0.38
#EM	0.39
#PB	0.42
#HTP	0.44
#IRC	0.45
#IRESPA	0.45
#IC	0.46
#FA	0.47
#AC	0.51
#IRA	0.55
#NEO	0.60
#IH	0.60
#PF	0.85
#SEPSIS	1.00

# > List of com. tags meaning:
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


#### Compute predict probability for some interesting comorbity profiles: ----
# > Various ages, male and female, average hospital score and occupancy

# Interesting profiles:
#> Profile 1: just covid (most common profile)
#> Profile 2: Chronic respiratory disease (DPCO) (diferente de asma)
#> Profile 3: Diabetes mellitus(DM) + 	Arterial hypertension (HTA) + Obesity (OB)
#> Profile 4: Bacterial pneumonia (PB) + Pneumonia to SARSCoV (PCOV)
#> Profile 5: Pulmonary thromboembolism (TEP) + Coagulation changes (AC) + PCOV + HTA
#> Profile 6: Accute breathing insufficiency (IRESPA) + Cardiac insufficiency (IC) + PCOV


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

score_female <- unique(strain$gen_sc1)[1] #-1.0583778
score_male <- unique(strain$gen_sc1)[2]   #0.9448182

#>> Profile 1: only covid (most common profile)
profile1_com_score <- sum(subset(score_dc_af, dc_af  %in% c("COVID"))$score_dc_af) #0.12
profile1_com_score_scaled <- (profile1_com_score-mean(train$dc_af_sc_sum))/sd(train$dc_af_sc_sum) #scale
profile1_com_score_scaled

#>> Profile 2: covid + DPCO 
profile2_com_score <- sum(subset(score_dc_af, dc_af  %in% c("COVID", "DPCO"))$score_dc_af) #0.12 + 0.33
profile2_com_score_scaled <- (profile2_com_score-mean(train$dc_af_sc_sum))/sd(train$dc_af_sc_sum) #scale
profile2_com_score_scaled

#>> Profile 3: covid + DM + HTA + OB 
profile3_com_score <- sum(subset(score_dc_af, dc_af  %in% c("COVID", "DM", "HTA", "OB"))$score_dc_af) #0.12 + 0.21 + 0.09 + 0
profile3_com_score_scaled <- (profile3_com_score-mean(train$dc_af_sc_sum))/sd(train$dc_af_sc_sum) #scale
profile3_com_score_scaled

#>> Profile 4: covid + PB + PCOV 
profile4_com_score <- sum(subset(score_dc_af, dc_af  %in% c("COVID", "PB", "PCOV"))$score_dc_af) #0.12 + 0.42 + 0.24
profile4_com_score_scaled <- (profile4_com_score-mean(train$dc_af_sc_sum))/sd(train$dc_af_sc_sum) #scale
profile4_com_score_scaled

#>> Profile 5: covid + TEP + HTA
profile5_com_score <- sum(subset(score_dc_af, dc_af  %in% c("COVID", "HTA", "TEP"))$score_dc_af) #0.12 + 0.09 + 0.16
profile5_com_score_scaled <- (profile5_com_score-mean(train$dc_af_sc_sum))/sd(train$dc_af_sc_sum) #scale
profile5_com_score_scaled

#>> Profile 6: covid + IRESPA + IC
profile6_com_score <- sum(subset(score_dc_af, dc_af  %in% c("COVID", "IRESPA", "IC"))$score_dc_af) #0.12 + 0.45 + 0.46
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



#### PLOT different profiles by Age and comorbidity score: ----

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
ggsave('disease_prof_by_age.tiff', dpi=350, plot=last_plot(), width = 5*1.1, height = 4.3*1.1)






#### 2D PLOTS: model decision boundary using optimal cut-off probability ----
# (opt. cut-off prob. defined as the predicted probability for which 
# Sensitivity = Specificity)

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
# - good scenario: hospital occupancy and hospital score are at the 25q values;
# - normal scenario: hospital occupancy and hospital score are at the 50q(median) values;
# - bad scenario: hospital occupancy and hospital score are at the 75q values;
# Output: two matrices of probabilities for each age/score by gender.
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
# specificity (obtained using train data)


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




#### Plot v1: version with color ----

plot <- ggplot(df_for_plot, aes(x = age, y = cut_50_values_train_fem2, group = 1)) + 
  geom_line(col='red', size=0.8, alpha=0.6) + 
  geom_ribbon(aes(ymin = cut_25_values_train_fem2, ymax = cut_75_values_train_fem2, colour='fem. thr. + band',
                  fill='fem. thr. + band'), alpha = 0.1,  size=0.3, linetype="dotdash")+
  geom_line(aes(x = age, y = cut_50_values_train_mas2, group = 1), color = "blue", size=0.8, alpha=0.6)+
  geom_ribbon(aes(ymin = cut_25_values_train_mas2, ymax = cut_75_values_train_mas2, colour='male thr. + band', fill='male thr. + band'),
              alpha = 0.1,  size=0.3, linetype="dotdash"
  ) +
  scale_colour_manual(name='', values=c("fem. thr. + band" = "red", "male thr. + band" = "blue"))+
  scale_fill_manual(name='', values=c("fem. thr. + band" = "red", "male thr. + band" = "blue"))


#Shuffle data (plot some train data points in the image too)
set.seed(42)
rows <- sample(nrow(train))
train_shufled <-train[sample(nrow(train)),]

train_shufled$death_gender <- paste(train_shufled$death,"/",train_shufled$gender)
death_gender <- as.factor(head(train_shufled,150)$death_gender)
train_shufled = subset(train_shufled, select = -c(death_gender) )


# Plot:
my_gg_theme2d_w_color <- theme(
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
  plot.title = element_text(size=13, face='bold'),
  plot.subtitle = element_text(size=12)
)

train_shufled$death <-train_shufled$death*1.00001

library(ggnewscale)

plot +
  new_scale_color() +
  geom_point(data=head(train_shufled,150),aes(x=age, y=dc_af_sc_sum, shape=death_gender, color=death_gender), alpha=0.5)+
  xlab('Age')+ylab('Comorbidity score') +
  labs(title="Optimal probability boundary", subtitle='by Comorbidity score, Age and gender') +
  my_gg_theme2d_w_color +
  scale_x_continuous(breaks=c(18,25,40,55,70,85,95))+
  scale_y_continuous(breaks=c(0.15,0.5,1,1.5,2,2.5))+
  coord_cartesian(xlim = c(38, 95), ylim = c(0.25, 2.4)) +
  #scale_color_manual(name='Death/Gender',values=c('0 / F'='black', '0 / M'='black', '1 / F'='green', '1 / M'='green')) +
  #scale_fill_manual(values=c('Yellow','Green','Yellow','Green','Yellow','Green')) +
  scale_shape_manual(name='Death/Gender',values=c('0 / F'=1, '0 / M'=16,'1 / F'=1, '1 / M'=16))+
  #new_scale_color() +
  scale_color_manual(name='Death/Gender',values=c('0 / F'='green4', '0 / M'='green4','1 / F'='#000000', '1 / M'='#000000'))


# Save
ggsave('2d_cutoff_plot_color.tiff', dpi=350, plot=last_plot(), width = 5*1.45, height = 4.3*1.15) #save



#### Plot v2: version without color ----

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


#Shuffle data (plot some train data points in the image too)
set.seed(42)
rows <- sample(nrow(train))
train_shufled <-train[sample(nrow(train)),]

train_shufled$death_gender <- paste(train_shufled$death,"/",train_shufled$gender)
death_gender <- as.factor(head(train_shufled,150)$death_gender)
train_shufled = subset(train_shufled, select = -c(death_gender) )


# Plot:
my_gg_theme2d_wo_color <- theme(
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
  my_gg_theme2d_wo_color +
  scale_x_continuous(breaks=c(18,25,40,55,70,85,95))+
  scale_y_continuous(breaks=c(0.15,0.5,1,1.5,2,2.5))+
  coord_cartesian(xlim = c(38, 95), ylim = c(0.25, 2.4)) +
  scale_shape_manual(name='Death/Gender',values=c('0 / F'=16, '0 / M'=18,'1 / F'=17, '1 / M'=4))

#Save:
#ggsave('2d_cutoff_plot_nocolor.tiff', dpi=350, plot=last_plot(), width = 5*1.5,height = 4.3*1.12) #save

#Legend: Boundary using cut-off = 0.287 for female individuals, as a function of Comorbidity score and Age.
#The left, center and right band limits correspond to the 25th, 50th and 75th quantile values of the 
#hospital pressure and hospital scores.



#### Plot v3: version with just females ----

plot <- ggplot(df_for_plot, aes(x = age, y = cut_50_values_train_fem2, group = 1)) + 
  geom_line(col='gray42', size=0.7, alpha=0.7) + 
  geom_ribbon(aes(ymin = cut_25_values_train_fem2, ymax = cut_75_values_train_fem2),
              alpha = 0.1, linetype="dotdash", size=0.7)


#Shuffle data (plot some train data points in the image too)
set.seed(42)
rows <- sample(nrow(train))
train_shufled <-train[sample(nrow(train)),]
train_shufled <- train_shufled[train_shufled$gender == 'F',]

death <- as.factor(head(train_shufled,150)$death)
train_shufled = subset(train_shufled, select = -c(death) )


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
  geom_point(data=head(train_shufled,150),aes(x=age, y=dc_af_sc_sum, shape=death), alpha=0.3, color='grey3')+
  xlab('Age')+
  ylab('Comorbidity score sum')+
  labs(title="Optimal probability boundary", subtitle='by comorbidity score sum, Age (female individuals)')+
  my_gg_theme2d_wo_color +
  scale_x_continuous(breaks=c(18,25,40,55,70,85,95))+
  scale_y_continuous(breaks=c(0.15,0.5,1,1.5,2,2.5))+
  coord_cartesian(xlim = c(38, 95), ylim = c(0.25, 2.4)) +
  scale_shape_manual(values=c('0'=16, '1'=17))

#Save:
ggsave('2d_cutoff_plot_nocolor_female.tiff', dpi=350, plot=last_plot(), width = 5*1.3, height = 4.3*1.15) #save



#### Plot v4: version with just females ----


plot <- ggplot(df_for_plot, aes(x = age, y = cut_50_values_train_mas2, group = 1)) + 
  geom_line(col='gray42', size=0.7, alpha=0.7) + 
  geom_ribbon(aes(ymin = cut_25_values_train_mas2, ymax = cut_75_values_train_mas2),
              alpha = 0.1,  size=0.7, linetype="dotdash") 


#Shuffle data (plot some train data points in the image too)
set.seed(42)
rows <- sample(nrow(train))
train_shufled <-train[sample(nrow(train)),]
train_shufled <- train_shufled[train_shufled$gender == 'M',]

death <- as.factor(head(train_shufled,150)$death)
train_shufled = subset(train_shufled, select = -c(death) )


# Plot:
plot +
  geom_point(data=head(train_shufled,150),aes(x=age, y=dc_af_sc_sum, shape=death), alpha=0.3, color='grey3')+
  xlab('Age')+
  ylab('Comorbidity score sum')+
  my_gg_theme2d_wo_color +
  labs(title="Optimal probability boundary", subtitle='by Comorbidity score sum, Age (male individuals)')+
  scale_x_continuous(breaks=c(18,25,40,55,70,85,95))+
  scale_y_continuous(breaks=c(0.15,0.5,1,1.5,2,2.5))+
  coord_cartesian(xlim = c(38, 95), ylim = c(0.25, 2.4)) +
  scale_shape_manual(values=c('0'=18, '1'=4))

#Save:
ggsave('2d_cutoff_plot_nocolor_male.tiff', dpi=350, plot=last_plot(), width = 5*1.3, height = 4.3*1.15) #save)

