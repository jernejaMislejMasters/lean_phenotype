library(ppcor)
library(Hmisc)
library(car)
library(glmulti)
library(leaps)
library(modelUtils)
library(DSA)
library(glmnet)

#load entire cleaned data (154009 subjects)
VIP_data_all <- read.csv("../../VIP_data/VIP_170206_cleaned.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)

#load the subset
VIP_data_subset <- read.csv("../../VIP_data/VIP_170206_cleaned_subset.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)
attach(VIP_data_subset)
#extract only swedish....66228 (33114 for each visit)
#get those that have the ursprungsland in both visits and the value is 1 for Swedish
VIP_data_subset<-VIP_data_subset[!is.na(ursprungsland[visit==1]) & !is.na(ursprungsland[visit==2]),]
VIP_data_subset<-VIP_data_subset[VIP_data_subset$ursprungsland==1,]
detach(VIP_data_subset)

#create the independant dataset of the first visit that is not in the subset already with Swedish only
attach(VIP_data_all)
VIP_data_independant<-VIP_data_all[!is.na(besok1) & besok1==1 & !(Subject_id %in% VIP_data_subset$Subject_id) & !is.na(ursprungsland) & ursprungsland==1, ]
detach(VIP_data_all)
#length(VIP_data_independant[,1])#....47107

#------------------------------INDEPENDANT DATA--------------------------------------------------------------------

source(file="../load_variables_independent_dataset_TEI_adjusted.R")

#independent variables
macronutrients_variables<-c("POLYsum1_TEI_adjusted_norm_sd","MONOsum1_TEI_adjusted_norm_sd","mfetsum1_TEI_adjusted_norm_sd",
		"fettsum1_TEI_adjusted_norm_sd","sacksum1_TEI_adjusted_norm_sd","kolhsum1_TEI_adjusted_norm_sd","FA_TEI_adjusted_norm_sd",
		"protsum1_TEI_adjusted_norm_sd","protsum1_anim_TEI_adjusted_norm_sd","protsum1_veg_TEI_adjusted_norm_sd",
		"fibesum1_TEI_adjusted_norm_sd","DISAsum1_TEI_adjusted_norm_sd","MOSAsum1_TEI_adjusted_norm_sd","TRANSsum1_TEI_adjusted_norm_sd",
		"NATRsum1_TEI_adjusted_norm_sd","kolesum1_TEI_adjusted_norm_sd")

#check pair corr
rcorr( cbind(age,agesq,gender_factor,year,ffq_factor,bmi_norm_sd,POLYsum1_TEI_adjusted_norm_sd,MONOsum1_TEI_adjusted_norm_sd,
				mfetsum1_TEI_adjusted_norm_sd,fettsum1_TEI_adjusted_norm_sd,sacksum1_TEI_adjusted_norm_sd,kolhsum1_TEI_adjusted_norm_sd,
				FA_TEI_adjusted_norm_sd,protsum1_TEI_adjusted_norm_sd,protsum1_anim_TEI_adjusted_norm_sd,protsum1_veg_TEI_adjusted_norm_sd,
				fibesum1_TEI_adjusted_norm_sd,DISAsum1_TEI_adjusted_norm_sd,MOSAsum1_TEI_adjusted_norm_sd,TRANSsum1_TEI_adjusted_norm_sd,
				NATRsum1_TEI_adjusted_norm_sd,kolesum1_TEI_adjusted_norm_sd,ensum1_norm_sd))



#MACRONUTRIENTS

#continous.....getting different numbers than if fitting with basic covariates, some quite different, there is a lot of colinearity with the basic covariates also

full_model_macronutrients<-glm(basic_residuals_bmi~POLYsum1_TEI_adjusted_norm_sd+MONOsum1_TEI_adjusted_norm_sd+mfetsum1_TEI_adjusted_norm_sd+
				fettsum1_TEI_adjusted_norm_sd+sacksum1_TEI_adjusted_norm_sd+kolhsum1_TEI_adjusted_norm_sd+FA_TEI_adjusted_norm_sd+
				protsum1_TEI_adjusted_norm_sd+protsum1_anim_TEI_adjusted_norm_sd+protsum1_veg_TEI_adjusted_norm_sd+
				fibesum1_TEI_adjusted_norm_sd+DISAsum1_TEI_adjusted_norm_sd+MOSAsum1_TEI_adjusted_norm_sd+TRANSsum1_TEI_adjusted_norm_sd+
				NATRsum1_TEI_adjusted_norm_sd+kolesum1_TEI_adjusted_norm_sd+ensum1_norm_sd, family = gaussian(link = "identity"))


#check results for all variables:
#together
summary(full_model_macronutrients)
#> summary(full_model_macronutrients)
#
#Call:
#		glm(formula = basic_residuals_bmi ~ POLYsum1_TEI_adjusted_norm_sd + 
#						MONOsum1_TEI_adjusted_norm_sd + mfetsum1_TEI_adjusted_norm_sd + 
#						fettsum1_TEI_adjusted_norm_sd + sacksum1_TEI_adjusted_norm_sd + 
#						kolhsum1_TEI_adjusted_norm_sd + FA_TEI_adjusted_norm_sd + 
#						protsum1_TEI_adjusted_norm_sd + protsum1_anim_TEI_adjusted_norm_sd + 
#						protsum1_veg_TEI_adjusted_norm_sd + fibesum1_TEI_adjusted_norm_sd + 
#						DISAsum1_TEI_adjusted_norm_sd + MOSAsum1_TEI_adjusted_norm_sd + 
#						TRANSsum1_TEI_adjusted_norm_sd + NATRsum1_TEI_adjusted_norm_sd + 
#						kolesum1_TEI_adjusted_norm_sd + ensum1_norm_sd, family = gaussian(link = "identity"))
#
#Deviance Residuals: 
#		Min       1Q   Median       3Q      Max  
#-3.6904  -0.6583  -0.0876   0.5546   6.0222  
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                        -0.001676   0.004537  -0.369 0.711843    
#		POLYsum1_TEI_adjusted_norm_sd       0.134088   0.034234   3.917 8.99e-05 ***
#		MONOsum1_TEI_adjusted_norm_sd       0.019605   0.023281   0.842 0.399731    
#		mfetsum1_TEI_adjusted_norm_sd      -0.415390   0.036278 -11.450  < 2e-16 ***
#		fettsum1_TEI_adjusted_norm_sd       0.518570   0.053337   9.722  < 2e-16 ***
#		sacksum1_TEI_adjusted_norm_sd      -0.075868   0.011558  -6.564 5.28e-11 ***
#		kolhsum1_TEI_adjusted_norm_sd       0.113754   0.029567   3.847 0.000120 ***
#		FA_TEI_adjusted_norm_sd            -0.239155   0.028875  -8.282  < 2e-16 ***
#		protsum1_TEI_adjusted_norm_sd       0.005710   0.041809   0.137 0.891368    
#		protsum1_anim_TEI_adjusted_norm_sd  0.122156   0.031600   3.866 0.000111 ***
#		protsum1_veg_TEI_adjusted_norm_sd  -0.134439   0.020473  -6.567 5.20e-11 ***
#		fibesum1_TEI_adjusted_norm_sd       0.018643   0.013573   1.374 0.169601    
#		DISAsum1_TEI_adjusted_norm_sd       0.017506   0.012816   1.366 0.171952    
#		MOSAsum1_TEI_adjusted_norm_sd       0.046413   0.010269   4.520 6.21e-06 ***
#		TRANSsum1_TEI_adjusted_norm_sd     -0.008177   0.007320  -1.117 0.263964    
#		NATRsum1_TEI_adjusted_norm_sd       0.139644   0.013079  10.677  < 2e-16 ***
#		kolesum1_TEI_adjusted_norm_sd      -0.070962   0.018619  -3.811 0.000138 ***
#		ensum1_norm_sd                      0.014667   0.004544   3.227 0.001250 ** 
#		---
#		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.9398549)
#
#Null deviance: 44234  on 45651  degrees of freedom
#Residual deviance: 42889  on 45634  degrees of freedom
#(1455 observations deleted due to missingness)
#AIC: 126743
#
#Number of Fisher Scoring iterations: 2
#

vif(full_model_macronutrients)
#POLYsum1_TEI_adjusted_norm_sd      MONOsum1_TEI_adjusted_norm_sd 
#28.710592                           8.563991 
#mfetsum1_TEI_adjusted_norm_sd      fettsum1_TEI_adjusted_norm_sd 
#23.077406                          38.686445 
#sacksum1_TEI_adjusted_norm_sd      kolhsum1_TEI_adjusted_norm_sd 
#4.428606                          11.789901 
#FA_TEI_adjusted_norm_sd      protsum1_TEI_adjusted_norm_sd 
#21.969886                          20.507899 
#protsum1_anim_TEI_adjusted_norm_sd  protsum1_veg_TEI_adjusted_norm_sd 
#22.117292                           9.464288 
#fibesum1_TEI_adjusted_norm_sd      DISAsum1_TEI_adjusted_norm_sd 
#5.604080                           4.372699 
#MOSAsum1_TEI_adjusted_norm_sd     TRANSsum1_TEI_adjusted_norm_sd 
#4.121886                           1.766258 
#NATRsum1_TEI_adjusted_norm_sd      kolesum1_TEI_adjusted_norm_sd 
#2.277953                           7.195116 
#ensum1_norm_sd 
#1.000003 





macronutrients_no_missing<-na.omit(VIP_data_independant[,c("basic_residuals_bmi",macronutrients_variables)])

#r²
variable_count<-1
for (variable in macronutrients_variables){
	
	partial_corr<-pcor.test(macronutrients_no_missing$basic_residuals_bmi,macronutrients_no_missing[,c(variable)],
			macronutrients_no_missing[,macronutrients_variables[-variable_count]])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
	variable_count<-variable_count+1
	
}

#
#POLYsum1_TEI_adjusted_norm_sd
#0.000337
#8.87536094081692e-05
#MONOsum1_TEI_adjusted_norm_sd
#1.6e-05
#0.399407941190658
#mfetsum1_TEI_adjusted_norm_sd
#0.002863
#2.65967736598958e-30
#fettsum1_TEI_adjusted_norm_sd
#0.002066
#2.61396866039095e-22
#sacksum1_TEI_adjusted_norm_sd
#0.000942
#5.40317756475877e-11
#kolhsum1_TEI_adjusted_norm_sd
#0.000324
#0.000120317277313031
#FA_TEI_adjusted_norm_sd
#0.001502
#1.21689560349343e-16
#protsum1_TEI_adjusted_norm_sd
#0
#0.891448552043068
#protsum1_anim_TEI_adjusted_norm_sd
#0.000327
#0.000110912116179327
#protsum1_veg_TEI_adjusted_norm_sd
#0.000944
#5.2204863452048e-11
#fibesum1_TEI_adjusted_norm_sd
#4.1e-05
#0.169384227339063
#DISAsum1_TEI_adjusted_norm_sd
#4.1e-05
#0.172209724076462
#MOSAsum1_TEI_adjusted_norm_sd
#0.000447
#6.25373379394375e-06
#TRANSsum1_TEI_adjusted_norm_sd
#2.7e-05
#0.263003006635378
#NATRsum1_TEI_adjusted_norm_sd
#0.002491
#1.41606699895416e-26
#kolesum1_TEI_adjusted_norm_sd
#0.000318
#0.000138146982110904
#


#separately
for (variable in macronutrients_variables){
	
	associations<-glm(bmi_norm_sd~age + agesq + gender_factor + year + ffq_factor + VIP_data_independant[,c(variable)], family = gaussian(link = "identity"))
	associations_summary<-summary(associations)
	
	message(variable)
	message(round(associations_summary$coefficients[[7]],7))
	message(round(associations_summary$coefficients[[28]],7))
	
}

#
#POLYsum1_TEI_adjusted_norm_sd
#0.0524156
#0
#MONOsum1_TEI_adjusted_norm_sd
#0.1254105
#0
#mfetsum1_TEI_adjusted_norm_sd
#-0.0215578
#0.0088063
#fettsum1_TEI_adjusted_norm_sd
#0.0543386
#0
#sacksum1_TEI_adjusted_norm_sd
#-0.0648873
#0
#kolhsum1_TEI_adjusted_norm_sd
#-0.1251423
#0
#FA_TEI_adjusted_norm_sd
#0.0248272
#0.0001542
#protsum1_TEI_adjusted_norm_sd
#0.2386595
#0
#protsum1_anim_TEI_adjusted_norm_sd
#0.1833085
#0
#protsum1_veg_TEI_adjusted_norm_sd
#-0.0787885
#0
#fibesum1_TEI_adjusted_norm_sd
#-0.0464713
#0
#DISAsum1_TEI_adjusted_norm_sd
#-0.0238866
#0.000242
#MOSAsum1_TEI_adjusted_norm_sd
#-0.0251565
#9.4e-06
#TRANSsum1_TEI_adjusted_norm_sd
#-0.070205
#0
#NATRsum1_TEI_adjusted_norm_sd
#0.2320336
#0
#kolesum1_TEI_adjusted_norm_sd
#0.0970942
#0




#r²
for (variable in macronutrients_variables){
	
	partial_corr<-pcor.test(bmi_norm_sd[(!is.na(bmi_norm_sd) & !is.na(VIP_data_independant[,c(variable)]))],VIP_data_independant[(!is.na(bmi_norm_sd) & !is.na(VIP_data_independant[,c(variable)])),c(variable)],
			VIP_data_independant[(!is.na(bmi_norm_sd) & !is.na(VIP_data_independant[,c(variable)])),c("age","agesq","year","ffq","gender")])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
}

#
#POLYsum1_TEI_adjusted_norm_sd
#0.001257
#3.55624715941539e-14
#MONOsum1_TEI_adjusted_norm_sd
#0.003924
#6.42249419796618e-41
#mfetsum1_TEI_adjusted_norm_sd
#0.00015
#0.00880634855533985
#fettsum1_TEI_adjusted_norm_sd
#0.000696
#1.74484051294601e-08
#sacksum1_TEI_adjusted_norm_sd
#0.002715
#8.05529491215047e-29
#kolhsum1_TEI_adjusted_norm_sd
#0.003459
#2.86179920432413e-36
#FA_TEI_adjusted_norm_sd
#0.000314
#0.000154238753090359
#protsum1_TEI_adjusted_norm_sd
#0.013582
#9.18214046307455e-138
#protsum1_anim_TEI_adjusted_norm_sd
#0.015029
#2.4315834249795e-152
#protsum1_veg_TEI_adjusted_norm_sd
#0.002709
#9.21334001641059e-29
#fibesum1_TEI_adjusted_norm_sd
#0.001148
#4.50327055217572e-13
#DISAsum1_TEI_adjusted_norm_sd
#0.000295
#0.000241959051473309
#MOSAsum1_TEI_adjusted_norm_sd
#0.00043
#9.40881110831622e-06
#TRANSsum1_TEI_adjusted_norm_sd
#0.002473
#2.12857487236445e-26
#NATRsum1_TEI_adjusted_norm_sd
#0.014072
#1.07346580461119e-142
#kolesum1_TEI_adjusted_norm_sd
#0.003443
#4.11220379881668e-36



#estimate best model based on different scores


#AIC, glmulti


#exhaustive search
model_selection_macronutrients_e <- glmulti(full_model_macronutrients, level = 1, crit="aicc", confsetsize=131072)
#After 38500 models:
#		Best model: basic_residuals_bmi~1+POLYsum1_TEI_adjusted_norm_sd+mfetsum1_TEI_adjusted_norm_sd+fettsum1_TEI_adjusted_norm_sd+
#										sacksum1_TEI_adjusted_norm_sd+kolhsum1_TEI_adjusted_norm_sd+FA_TEI_adjusted_norm_sd+
#										protsum1_anim_TEI_adjusted_norm_sd+protsum1_veg_TEI_adjusted_norm_sd+DISAsum1_TEI_adjusted_norm_sd+
#										MOSAsum1_TEI_adjusted_norm_sd+TRANSsum1_TEI_adjusted_norm_sd+NATRsum1_TEI_adjusted_norm_sd
#Crit= 126760.144093861
#Mean crit= 127206.874021464

#After 46600 models:
#		Best model: basic_residuals_bmi~1+POLYsum1_TEI_adjusted_norm_sd+mfetsum1_TEI_adjusted_norm_sd+fettsum1_TEI_adjusted_norm_sd+
#											sacksum1_TEI_adjusted_norm_sd+kolhsum1_TEI_adjusted_norm_sd+FA_TEI_adjusted_norm_sd+
#											protsum1_anim_TEI_adjusted_norm_sd+protsum1_veg_TEI_adjusted_norm_sd+DISAsum1_TEI_adjusted_norm_sd+
#											MOSAsum1_TEI_adjusted_norm_sd+TRANSsum1_TEI_adjusted_norm_sd+NATRsum1_TEI_adjusted_norm_sd
#Crit= 126760.144093861
#Mean crit= 127213.585074377


summary(model_selection_macronutrients_e)

capture.output(summary(model_selection_macronutrients_e), file = "model_selection_macronutrients_e")

#weightable(model_selection_macronutrients_e)

#genetic algorithm
model_selection_macronutrients_g <- glmulti(full_model_macronutrients, level = 1, crit="aicc", method="g")
#After 630 generations:
#		Best model: basic_residuals_bmi~1+POLYsum1_TEI_adjusted_norm_sd+mfetsum1_TEI_adjusted_norm_sd+fettsum1_TEI_adjusted_norm_sd+sacksum1_TEI_adjusted_norm_sd+kolhsum1_TEI_adjusted_norm_sd+FA_TEI_adjusted_norm_sd+protsum1_anim_TEI_adjusted_norm_sd+protsum1_veg_TEI_adjusted_norm_sd+MOSAsum1_TEI_adjusted_norm_sd+NATRsum1_TEI_adjusted_norm_sd+kolesum1_TEI_adjusted_norm_sd+ensum1_norm_sd
#Crit= 126738.21492507
#Mean crit= 126814.819116584
#Change in best IC: 0 / Change in mean IC: -1.07682883426605

#After 1330 generations:
#		Best model: basic_residuals_bmi~1+POLYsum1_TEI_adjusted_norm_sd+mfetsum1_TEI_adjusted_norm_sd+fettsum1_TEI_adjusted_norm_sd+sacksum1_TEI_adjusted_norm_sd+kolhsum1_TEI_adjusted_norm_sd+FA_TEI_adjusted_norm_sd+protsum1_anim_TEI_adjusted_norm_sd+protsum1_veg_TEI_adjusted_norm_sd+MOSAsum1_TEI_adjusted_norm_sd+NATRsum1_TEI_adjusted_norm_sd+kolesum1_TEI_adjusted_norm_sd+ensum1_norm_sd
#Crit= 126738.21492507
#Mean crit= 127170.97886303
#Change in best IC: 0 / Change in mean IC: -0.0109754493169021


summary(model_selection_macronutrients_g)

capture.output(summary(model_selection_macronutrients_g), file = "model_selection_macronutrients_g")
#$bestmodel
#[1] "basic_residuals_bmi ~ 1 + POLYsum1_TEI_adjusted_norm_sd + mfetsum1_TEI_adjusted_norm_sd + "
#[2] "    fettsum1_TEI_adjusted_norm_sd + sacksum1_TEI_adjusted_norm_sd + "                      
#[3] "    kolhsum1_TEI_adjusted_norm_sd + FA_TEI_adjusted_norm_sd + "                            
#[4] "    protsum1_anim_TEI_adjusted_norm_sd + protsum1_veg_TEI_adjusted_norm_sd + "             
#[5] "    MOSAsum1_TEI_adjusted_norm_sd + NATRsum1_TEI_adjusted_norm_sd + "                      
#[6] "    kolesum1_TEI_adjusted_norm_sd + ensum1_norm_sd"                                        
#
#$bestic
#[1] 126738.2

#$confsetsize
#[1] 100
#
#$generations
#[1] 1080


#after 40000  generations.....same thing

#$bestic
#[1] 126738.2

#$bestmodel
#[1] "basic_residuals_bmi ~ 1 + POLYsum1_TEI_adjusted_norm_sd + mfetsum1_TEI_adjusted_norm_sd + "
#[2] "    fettsum1_TEI_adjusted_norm_sd + sacksum1_TEI_adjusted_norm_sd + "                      
#[3] "    kolhsum1_TEI_adjusted_norm_sd + FA_TEI_adjusted_norm_sd + "                            
#[4] "    protsum1_anim_TEI_adjusted_norm_sd + protsum1_veg_TEI_adjusted_norm_sd + "             
#[5] "    MOSAsum1_TEI_adjusted_norm_sd + NATRsum1_TEI_adjusted_norm_sd + "                      
#[6] "    kolesum1_TEI_adjusted_norm_sd + ensum1_norm_sd"                                        


#weightable(model_selection_macronutrients_g)


#branch-and-bound algorithm
model_selection_macronutrients_l <- glmulti(full_model_macronutrients, level = 1, crit="aicc", method="l")

summary(model_selection_macronutrients_l)

capture.output(summary(model_selection_macronutrients_l), file = "model_selection_macronutrients_l")

#weightable(model_selection_macronutrients_l)

#if not specified , confsetsize=100, which means only 100 models are checked(out of 131072 possible combinations)
#$bestmodel
#[1] "basic_residuals_bmi ~ 1 + mfetsum1_TEI_adjusted_norm_sd + fettsum1_TEI_adjusted_norm_sd + "
#[2] "    sacksum1_TEI_adjusted_norm_sd + FA_TEI_adjusted_norm_sd + "                            
#[3] "    protsum1_anim_TEI_adjusted_norm_sd + protsum1_veg_TEI_adjusted_norm_sd + "             
#[4] "    MOSAsum1_TEI_adjusted_norm_sd + NATRsum1_TEI_adjusted_norm_sd"                         
model_selection_macronutrients_l <- glmulti(full_model_macronutrients, level = 1, crit="aicc", method="l", confsetsize=20000)

#same model is chosen having 100, 1000, 10000 , 20000 possible models

summary(model_selection_macronutrients_l)

capture.output(summary(model_selection_macronutrients_l), file = "model_selection_macronutrients_l")


#weightable(model_selection_macronutrients_l)


#L2 with cross-validation, DSA
model_selection_macronutrients_d<-DSA(basic_residuals_bmi~POLYsum1_TEI_adjusted_norm_sd+MONOsum1_TEI_adjusted_norm_sd+mfetsum1_TEI_adjusted_norm_sd+
								fettsum1_TEI_adjusted_norm_sd+sacksum1_TEI_adjusted_norm_sd+kolhsum1_TEI_adjusted_norm_sd+FA_TEI_adjusted_norm_sd+
								protsum1_TEI_adjusted_norm_sd+protsum1_anim_TEI_adjusted_norm_sd+protsum1_veg_TEI_adjusted_norm_sd+
								fibesum1_TEI_adjusted_norm_sd+DISAsum1_TEI_adjusted_norm_sd+MOSAsum1_TEI_adjusted_norm_sd+TRANSsum1_TEI_adjusted_norm_sd+
								NATRsum1_TEI_adjusted_norm_sd+kolesum1_TEI_adjusted_norm_sd+ensum1_norm_sd, data=VIP_data_independant, maxsumofpow = 1,
								maxsize=17, maxorderint = 1, nsplits=2,vfold=10)

						
capture.output(summary(model_selection_macronutrients_d), file = "model_selection_macronutrients_d")
						

#LASSO

#make a complete cases dataframe, taking residauls of bmi~basic covariates
VIP_data_independant_complete_cases <- na.omit(VIP_data_independant[,c("basic_residuals_bmi", "POLYsum1_TEI_adjusted_norm_sd","MONOsum1_TEI_adjusted_norm_sd","mfetsum1_TEI_adjusted_norm_sd",
						"fettsum1_TEI_adjusted_norm_sd","sacksum1_TEI_adjusted_norm_sd","kolhsum1_TEI_adjusted_norm_sd","FA_TEI_adjusted_norm_sd",
						"protsum1_TEI_adjusted_norm_sd","protsum1_anim_TEI_adjusted_norm_sd","protsum1_veg_TEI_adjusted_norm_sd",
						"fibesum1_TEI_adjusted_norm_sd","DISAsum1_TEI_adjusted_norm_sd","MOSAsum1_TEI_adjusted_norm_sd","TRANSsum1_TEI_adjusted_norm_sd",
						"NATRsum1_TEI_adjusted_norm_sd","kolesum1_TEI_adjusted_norm_sd","ensum1_norm_sd")])

predictors <- as.matrix(VIP_data_independant_complete_cases[,-1])
dependent <- VIP_data_independant_complete_cases$basic_residuals_bmi

#just cross-validation(default is 10fold)

model_selection_macronutrients_lasso <- cv.glmnet(predictors,dependent, family="gaussian")

coef(model_selection_macronutrients_lasso)
#18 x 1 sparse Matrix of class "dgCMatrix"
#1
#(Intercept)                        -0.001705103
#POLYsum1_TEI_adjusted_norm_sd       .          
#MONOsum1_TEI_adjusted_norm_sd       0.015277941
#mfetsum1_TEI_adjusted_norm_sd      -0.052855131
#fettsum1_TEI_adjusted_norm_sd       .          
#sacksum1_TEI_adjusted_norm_sd       .          
#kolhsum1_TEI_adjusted_norm_sd       .          
#FA_TEI_adjusted_norm_sd             .          
#protsum1_TEI_adjusted_norm_sd       .          
#protsum1_anim_TEI_adjusted_norm_sd  0.102559527
#protsum1_veg_TEI_adjusted_norm_sd  -0.030111701
#fibesum1_TEI_adjusted_norm_sd       .          
#DISAsum1_TEI_adjusted_norm_sd       .          
#MOSAsum1_TEI_adjusted_norm_sd       .          
#TRANSsum1_TEI_adjusted_norm_sd     -0.013442728
#NATRsum1_TEI_adjusted_norm_sd       0.121243925
#kolesum1_TEI_adjusted_norm_sd       .          
#ensum1_norm_sd                      .          
#



#-----taking bmi without basic covariates

#18 x 1 sparse Matrix of class "dgCMatrix"
#1
#(Intercept)                        -0.0007306138
#POLYsum1_TEI_adjusted_norm_sd       .           
#MONOsum1_TEI_adjusted_norm_sd       0.0728009294
#mfetsum1_TEI_adjusted_norm_sd      -0.0231155965
#fettsum1_TEI_adjusted_norm_sd       .           
#sacksum1_TEI_adjusted_norm_sd       .           
#kolhsum1_TEI_adjusted_norm_sd       .           
#FA_TEI_adjusted_norm_sd            -0.0317505005
#protsum1_TEI_adjusted_norm_sd       .           
#protsum1_anim_TEI_adjusted_norm_sd  0.0994903407
#protsum1_veg_TEI_adjusted_norm_sd  -0.0798699548
#fibesum1_TEI_adjusted_norm_sd       .           
#DISAsum1_TEI_adjusted_norm_sd      -0.0376917352
#MOSAsum1_TEI_adjusted_norm_sd      -0.0116777017
#TRANSsum1_TEI_adjusted_norm_sd     -0.1167359988
#NATRsum1_TEI_adjusted_norm_sd       0.0793255752
#kolesum1_TEI_adjusted_norm_sd      -0.0521793922
#ensum1_norm_sd                      0.0531255570


#elastic net with cross-validation

model_selection_macronutrients_elastic_net <- cv.glmnet(predictors,dependent,alpha= 0.5, family="gaussian")

coef(model_selection_macronutrients_elastic_net)
#18 x 1 sparse Matrix of class "dgCMatrix"
#1
#(Intercept)                        -0.001704365
#POLYsum1_TEI_adjusted_norm_sd       .          
#MONOsum1_TEI_adjusted_norm_sd       0.038085180
#mfetsum1_TEI_adjusted_norm_sd      -0.083282126
#fettsum1_TEI_adjusted_norm_sd       .          
#sacksum1_TEI_adjusted_norm_sd       .          
#kolhsum1_TEI_adjusted_norm_sd       .          
#FA_TEI_adjusted_norm_sd             .          
#protsum1_TEI_adjusted_norm_sd       .          
#protsum1_anim_TEI_adjusted_norm_sd  0.104740544
#protsum1_veg_TEI_adjusted_norm_sd  -0.040630327
#fibesum1_TEI_adjusted_norm_sd       .          
#DISAsum1_TEI_adjusted_norm_sd       .          
#MOSAsum1_TEI_adjusted_norm_sd       .          
#TRANSsum1_TEI_adjusted_norm_sd     -0.012507289
#NATRsum1_TEI_adjusted_norm_sd       0.120169983
#kolesum1_TEI_adjusted_norm_sd       .          
#ensum1_norm_sd                      0.002018945
#

#-----taking bmi without basic covariates


#18 x 1 sparse Matrix of class "dgCMatrix"
#1
#(Intercept)                        -0.0007287506
#POLYsum1_TEI_adjusted_norm_sd       .           
#MONOsum1_TEI_adjusted_norm_sd       0.0453352935
#mfetsum1_TEI_adjusted_norm_sd      -0.0119214821
#fettsum1_TEI_adjusted_norm_sd       .           
#sacksum1_TEI_adjusted_norm_sd       .           
#kolhsum1_TEI_adjusted_norm_sd       .           
#FA_TEI_adjusted_norm_sd            -0.0169671588
#protsum1_TEI_adjusted_norm_sd       .           
#protsum1_anim_TEI_adjusted_norm_sd  0.0862256003
#protsum1_veg_TEI_adjusted_norm_sd  -0.0681246438
#fibesum1_TEI_adjusted_norm_sd       .           
#DISAsum1_TEI_adjusted_norm_sd      -0.0259269777
#MOSAsum1_TEI_adjusted_norm_sd      -0.0131121405
#TRANSsum1_TEI_adjusted_norm_sd     -0.1165449872
#NATRsum1_TEI_adjusted_norm_sd       0.0784602385
#kolesum1_TEI_adjusted_norm_sd      -0.0216244928
#ensum1_norm_sd                      0.0504983738


#ridge regression with cross-validation 

model_selection_macronutrients_ridge_regression <- cv.glmnet(predictors,dependent,alpha= 0, family="gaussian")

coef(model_selection_macronutrients_ridge_regression)
#18 x 1 sparse Matrix of class "dgCMatrix"
#1
#(Intercept)                        -0.001714950
#POLYsum1_TEI_adjusted_norm_sd       0.012992766
#MONOsum1_TEI_adjusted_norm_sd       0.034170059
#mfetsum1_TEI_adjusted_norm_sd      -0.043418541
#fettsum1_TEI_adjusted_norm_sd       0.001016224
#sacksum1_TEI_adjusted_norm_sd      -0.012679395
#kolhsum1_TEI_adjusted_norm_sd      -0.003832229
#FA_TEI_adjusted_norm_sd            -0.006299341
#protsum1_TEI_adjusted_norm_sd       0.060021218
#protsum1_anim_TEI_adjusted_norm_sd  0.053549180
#protsum1_veg_TEI_adjusted_norm_sd  -0.035546378
#fibesum1_TEI_adjusted_norm_sd      -0.005418635
#DISAsum1_TEI_adjusted_norm_sd       0.002470987
#MOSAsum1_TEI_adjusted_norm_sd       0.012443309
#TRANSsum1_TEI_adjusted_norm_sd     -0.020389046
#NATRsum1_TEI_adjusted_norm_sd       0.081170659
#kolesum1_TEI_adjusted_norm_sd      -0.004646340
#ensum1_norm_sd                      0.008560330


#-----taking bmi without basic covariates


#18 x 1 sparse Matrix of class "dgCMatrix"
#1
#(Intercept)                        -0.0007152195
#POLYsum1_TEI_adjusted_norm_sd      -0.0011944041
#MONOsum1_TEI_adjusted_norm_sd       0.0596807896
#mfetsum1_TEI_adjusted_norm_sd      -0.0466166732
#fettsum1_TEI_adjusted_norm_sd       0.0030098874
#sacksum1_TEI_adjusted_norm_sd      -0.0113215753
#kolhsum1_TEI_adjusted_norm_sd      -0.0070601911
#FA_TEI_adjusted_norm_sd            -0.0234766251
#protsum1_TEI_adjusted_norm_sd       0.0426667918
#protsum1_anim_TEI_adjusted_norm_sd  0.0587276449
#protsum1_veg_TEI_adjusted_norm_sd  -0.0630041323
#fibesum1_TEI_adjusted_norm_sd      -0.0111087265
#DISAsum1_TEI_adjusted_norm_sd      -0.0289615465
#MOSAsum1_TEI_adjusted_norm_sd      -0.0151484553
#TRANSsum1_TEI_adjusted_norm_sd     -0.0845824888
#NATRsum1_TEI_adjusted_norm_sd       0.0637516969
#kolesum1_TEI_adjusted_norm_sd      -0.0313831282
#ensum1_norm_sd                      0.0444360874



#trying different alpha settings
model_selection_macronutrients_elastic_net_alpha_01 <- cv.glmnet(predictors,dependent,alpha= 0.1, family="gaussian")

coef(model_selection_macronutrients_elastic_net_alpha_01)
#
#18 x 1 sparse Matrix of class "dgCMatrix"
#1
#(Intercept)                        -0.001707236
#POLYsum1_TEI_adjusted_norm_sd       .          
#MONOsum1_TEI_adjusted_norm_sd       0.034884512
#mfetsum1_TEI_adjusted_norm_sd      -0.066189344
#fettsum1_TEI_adjusted_norm_sd       .          
#sacksum1_TEI_adjusted_norm_sd       .          
#kolhsum1_TEI_adjusted_norm_sd       .          
#FA_TEI_adjusted_norm_sd             .          
#protsum1_TEI_adjusted_norm_sd       0.031862228
#protsum1_anim_TEI_adjusted_norm_sd  0.078671429
#protsum1_veg_TEI_adjusted_norm_sd  -0.041716986
#fibesum1_TEI_adjusted_norm_sd       .          
#DISAsum1_TEI_adjusted_norm_sd       .          
#MOSAsum1_TEI_adjusted_norm_sd       .          
#TRANSsum1_TEI_adjusted_norm_sd     -0.016244395
#NATRsum1_TEI_adjusted_norm_sd       0.110397447
#kolesum1_TEI_adjusted_norm_sd       .          
#ensum1_norm_sd                      0.002868971

#-----taking bmi without basic covariates

#
#1
#(Intercept)                        -0.0007246366
#POLYsum1_TEI_adjusted_norm_sd       .           
#MONOsum1_TEI_adjusted_norm_sd       0.0609471122
#mfetsum1_TEI_adjusted_norm_sd      -0.0329784402
#fettsum1_TEI_adjusted_norm_sd       .           
#sacksum1_TEI_adjusted_norm_sd      -0.0028340403
#kolhsum1_TEI_adjusted_norm_sd       .           
#FA_TEI_adjusted_norm_sd            -0.0234593980
#protsum1_TEI_adjusted_norm_sd       .           
#protsum1_anim_TEI_adjusted_norm_sd  0.0864610430
#protsum1_veg_TEI_adjusted_norm_sd  -0.0717876322
#fibesum1_TEI_adjusted_norm_sd       .           
#DISAsum1_TEI_adjusted_norm_sd      -0.0290478547
#MOSAsum1_TEI_adjusted_norm_sd      -0.0154610112
#TRANSsum1_TEI_adjusted_norm_sd     -0.1079571213
#NATRsum1_TEI_adjusted_norm_sd       0.0753101264
#kolesum1_TEI_adjusted_norm_sd      -0.0269254118
#ensum1_norm_sd                      0.0499528173


model_selection_macronutrients_elastic_net_alpha_02 <- cv.glmnet(predictors,dependent,alpha= 0.2, family="gaussian")

coef(model_selection_macronutrients_elastic_net_alpha_02)
#18 x 1 sparse Matrix of class "dgCMatrix"
#1
#(Intercept)                        -0.001706267
#POLYsum1_TEI_adjusted_norm_sd       .          
#MONOsum1_TEI_adjusted_norm_sd       0.005823492
#mfetsum1_TEI_adjusted_norm_sd      -0.030766491
#fettsum1_TEI_adjusted_norm_sd       .          
#sacksum1_TEI_adjusted_norm_sd       .          
#kolhsum1_TEI_adjusted_norm_sd       .          
#FA_TEI_adjusted_norm_sd             .          
#protsum1_TEI_adjusted_norm_sd       0.006071089
#protsum1_anim_TEI_adjusted_norm_sd  0.092082168
#protsum1_veg_TEI_adjusted_norm_sd  -0.023152486
#fibesum1_TEI_adjusted_norm_sd       .          
#DISAsum1_TEI_adjusted_norm_sd       .          
#MOSAsum1_TEI_adjusted_norm_sd       .          
#TRANSsum1_TEI_adjusted_norm_sd     -0.015613462
#NATRsum1_TEI_adjusted_norm_sd       0.113976019
#kolesum1_TEI_adjusted_norm_sd       .          
#ensum1_norm_sd                      .          

#-----taking bmi without basic covariates

#
#1
#(Intercept)                        -0.0007285847
#POLYsum1_TEI_adjusted_norm_sd       .           
#MONOsum1_TEI_adjusted_norm_sd       0.0803123511
#mfetsum1_TEI_adjusted_norm_sd      -0.0364826016
#fettsum1_TEI_adjusted_norm_sd       .           
#sacksum1_TEI_adjusted_norm_sd       .           
#kolhsum1_TEI_adjusted_norm_sd       .           
#FA_TEI_adjusted_norm_sd            -0.0348621831
#protsum1_TEI_adjusted_norm_sd       .           
#protsum1_anim_TEI_adjusted_norm_sd  0.0986743705
#protsum1_veg_TEI_adjusted_norm_sd  -0.0817543690
#fibesum1_TEI_adjusted_norm_sd       .           
#DISAsum1_TEI_adjusted_norm_sd      -0.0392399089
#MOSAsum1_TEI_adjusted_norm_sd      -0.0144515469
#TRANSsum1_TEI_adjusted_norm_sd     -0.1126444605
#NATRsum1_TEI_adjusted_norm_sd       0.0777992061
#kolesum1_TEI_adjusted_norm_sd      -0.0520940445
#ensum1_norm_sd                      0.0530635620




model_selection_macronutrients_elastic_net_alpha_03 <- cv.glmnet(predictors,dependent,alpha= 0.3, family="gaussian")

coef(model_selection_macronutrients_elastic_net_alpha_03)
#
#18 x 1 sparse Matrix of class "dgCMatrix"
#1
#(Intercept)                        -0.0017046638
#POLYsum1_TEI_adjusted_norm_sd       .           
#MONOsum1_TEI_adjusted_norm_sd       0.0285943450
#mfetsum1_TEI_adjusted_norm_sd      -0.0679309764
#fettsum1_TEI_adjusted_norm_sd       .           
#sacksum1_TEI_adjusted_norm_sd       .           
#kolhsum1_TEI_adjusted_norm_sd       .           
#FA_TEI_adjusted_norm_sd             .           
#protsum1_TEI_adjusted_norm_sd       .           
#protsum1_anim_TEI_adjusted_norm_sd  0.1021963092
#protsum1_veg_TEI_adjusted_norm_sd  -0.0356105829
#fibesum1_TEI_adjusted_norm_sd       .           
#DISAsum1_TEI_adjusted_norm_sd       .           
#MOSAsum1_TEI_adjusted_norm_sd       .           
#TRANSsum1_TEI_adjusted_norm_sd     -0.0140774865
#NATRsum1_TEI_adjusted_norm_sd       0.1190266912
#kolesum1_TEI_adjusted_norm_sd       .           
#ensum1_norm_sd                      0.0007892007
#
#-----taking bmi without basic covariates

#
#1
#(Intercept)                        -0.0007279347
#POLYsum1_TEI_adjusted_norm_sd       .           
#MONOsum1_TEI_adjusted_norm_sd       0.0549283994
#mfetsum1_TEI_adjusted_norm_sd      -0.0202957322
#fettsum1_TEI_adjusted_norm_sd       .           
#sacksum1_TEI_adjusted_norm_sd       .           
#kolhsum1_TEI_adjusted_norm_sd       .           
#FA_TEI_adjusted_norm_sd            -0.0213894674
#protsum1_TEI_adjusted_norm_sd       .           
#protsum1_anim_TEI_adjusted_norm_sd  0.0888888177
#protsum1_veg_TEI_adjusted_norm_sd  -0.0710592545
#fibesum1_TEI_adjusted_norm_sd       .           
#DISAsum1_TEI_adjusted_norm_sd      -0.0291271688
#MOSAsum1_TEI_adjusted_norm_sd      -0.0138399510
#TRANSsum1_TEI_adjusted_norm_sd     -0.1143626149
#NATRsum1_TEI_adjusted_norm_sd       0.0775132090
#kolesum1_TEI_adjusted_norm_sd      -0.0281506194
#ensum1_norm_sd                      0.0508709183



model_selection_macronutrients_elastic_net_alpha_04 <- cv.glmnet(predictors,dependent,alpha= 0.4, family="gaussian")

coef(model_selection_macronutrients_elastic_net_alpha_04)
#18 x 1 sparse Matrix of class "dgCMatrix"
#1
#(Intercept)                        -0.001707528
#POLYsum1_TEI_adjusted_norm_sd       .          
#MONOsum1_TEI_adjusted_norm_sd       .          
#mfetsum1_TEI_adjusted_norm_sd      -0.024276832
#fettsum1_TEI_adjusted_norm_sd       .          
#sacksum1_TEI_adjusted_norm_sd       .          
#kolhsum1_TEI_adjusted_norm_sd       .          
#FA_TEI_adjusted_norm_sd             .          
#protsum1_TEI_adjusted_norm_sd       .          
#protsum1_anim_TEI_adjusted_norm_sd  0.098498808
#protsum1_veg_TEI_adjusted_norm_sd  -0.018038273
#fibesum1_TEI_adjusted_norm_sd       .          
#DISAsum1_TEI_adjusted_norm_sd       .          
#MOSAsum1_TEI_adjusted_norm_sd       .          
#TRANSsum1_TEI_adjusted_norm_sd     -0.015006160
#NATRsum1_TEI_adjusted_norm_sd       0.116571704
#kolesum1_TEI_adjusted_norm_sd       .          
#ensum1_norm_sd                      .          
#
#-----taking bmi without basic covariates

#
#1
#(Intercept)                        -0.0007281705
#POLYsum1_TEI_adjusted_norm_sd       .           
#MONOsum1_TEI_adjusted_norm_sd       0.0445139811
#mfetsum1_TEI_adjusted_norm_sd      -0.0127523459
#fettsum1_TEI_adjusted_norm_sd       .           
#sacksum1_TEI_adjusted_norm_sd       .           
#kolhsum1_TEI_adjusted_norm_sd       .           
#FA_TEI_adjusted_norm_sd            -0.0162314686
#protsum1_TEI_adjusted_norm_sd       .           
#protsum1_anim_TEI_adjusted_norm_sd  0.0853306743
#protsum1_veg_TEI_adjusted_norm_sd  -0.0672951041
#fibesum1_TEI_adjusted_norm_sd       .           
#DISAsum1_TEI_adjusted_norm_sd      -0.0253180628
#MOSAsum1_TEI_adjusted_norm_sd      -0.0134314756
#TRANSsum1_TEI_adjusted_norm_sd     -0.1157566640
#NATRsum1_TEI_adjusted_norm_sd       0.0779555469
#kolesum1_TEI_adjusted_norm_sd      -0.0195415060
#ensum1_norm_sd                      0.0502026958




#add micronutrient to elastic net



#check the full model first

full_model_macro_micronutrients<-glm(basic_residuals_bmi~POLYsum1_TEI_adjusted_norm_sd+MONOsum1_TEI_adjusted_norm_sd+mfetsum1_TEI_adjusted_norm_sd+
				fettsum1_TEI_adjusted_norm_sd+sacksum1_TEI_adjusted_norm_sd+kolhsum1_TEI_adjusted_norm_sd+FA_TEI_adjusted_norm_sd+
				protsum1_TEI_adjusted_norm_sd+protsum1_anim_TEI_adjusted_norm_sd+protsum1_veg_TEI_adjusted_norm_sd+
				fibesum1_TEI_adjusted_norm_sd+DISAsum1_TEI_adjusted_norm_sd+MOSAsum1_TEI_adjusted_norm_sd+TRANSsum1_TEI_adjusted_norm_sd+
				NATRsum1_TEI_adjusted_norm_sd+kolesum1_TEI_adjusted_norm_sd+ensum1_norm_sd+ MAGNsum1_TEI_adjusted_norm_sd+
				FOSFsum1_TEI_adjusted_norm_sd+selesum1_TEI_adjusted_norm_sd+ZINCsum1_TEI_adjusted_norm_sd+retisum1_TEI_adjusted_norm_sd+
				karosum1_TEI_adjusted_norm_sd+TIAMsum1_TEI_adjusted_norm_sd+Folasum1_TEI_adjusted_norm_sd+B2sum1_TEI_adjusted_norm_sd+
				NIACsum1_TEI_adjusted_norm_sd+B6sum1_TEI_adjusted_norm_sd+B12sum1_TEI_adjusted_norm_sd+askosum1_TEI_adjusted_norm_sd+
				Dsum1_TEI_adjusted_norm_sd+tokosum1_TEI_adjusted_norm_sd+VITKsum1_TEI_adjusted_norm_sd+jernsum1_TEI_adjusted_norm_sd+
				JODIsum1_TEI_adjusted_norm_sd+kalcsum1_TEI_adjusted_norm_sd+KALIsum1_TEI_adjusted_norm_sd, family = gaussian(link = "identity"))


summary(full_model_macro_micronutrients)
#Deviance Residuals: 
#		Min       1Q   Median       3Q      Max  
#-3.7450  -0.6527  -0.0848   0.5472   6.1758  
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                        -0.001668   0.004501  -0.371 0.710986    
#POLYsum1_TEI_adjusted_norm_sd       0.175072   0.036489   4.798 1.61e-06 ***
#		MONOsum1_TEI_adjusted_norm_sd       0.054753   0.025663   2.133 0.032890 *  
#		mfetsum1_TEI_adjusted_norm_sd      -0.177329   0.050686  -3.499 0.000468 ***
#		fettsum1_TEI_adjusted_norm_sd       0.035261   0.067792   0.520 0.602966    
#		sacksum1_TEI_adjusted_norm_sd      -0.109302   0.016115  -6.783 1.19e-11 ***
#		kolhsum1_TEI_adjusted_norm_sd      -0.064728   0.034491  -1.877 0.060569 .  
#		FA_TEI_adjusted_norm_sd            -0.182352   0.031240  -5.837 5.35e-09 ***
#		protsum1_TEI_adjusted_norm_sd       0.192471   0.052874   3.640 0.000273 ***
#		protsum1_anim_TEI_adjusted_norm_sd  0.132498   0.034461   3.845 0.000121 ***
#		protsum1_veg_TEI_adjusted_norm_sd  -0.010667   0.023374  -0.456 0.648136    
#		fibesum1_TEI_adjusted_norm_sd       0.081418   0.018788   4.334 1.47e-05 ***
#		DISAsum1_TEI_adjusted_norm_sd       0.185153   0.021335   8.678  < 2e-16 ***
#		MOSAsum1_TEI_adjusted_norm_sd      -0.028401   0.012844  -2.211 0.027020 *  
#		TRANSsum1_TEI_adjusted_norm_sd     -0.029048   0.008315  -3.494 0.000477 ***
#		NATRsum1_TEI_adjusted_norm_sd       0.202021   0.016324  12.376  < 2e-16 ***
#		kolesum1_TEI_adjusted_norm_sd      -0.027310   0.027573  -0.990 0.321956    
#		ensum1_norm_sd                      0.014572   0.004508   3.233 0.001228 ** 
#		MAGNsum1_TEI_adjusted_norm_sd      -0.298142   0.029340 -10.162  < 2e-16 ***
#		FOSFsum1_TEI_adjusted_norm_sd      -0.186144   0.039614  -4.699 2.62e-06 ***
#		selesum1_TEI_adjusted_norm_sd      -0.061941   0.017121  -3.618 0.000297 ***
#		ZINCsum1_TEI_adjusted_norm_sd       0.089230   0.018015   4.953 7.33e-07 ***
#		retisum1_TEI_adjusted_norm_sd       0.074042   0.013012   5.690 1.27e-08 ***
#		karosum1_TEI_adjusted_norm_sd      -0.008236   0.007408  -1.112 0.266235    
#		TIAMsum1_TEI_adjusted_norm_sd       0.120588   0.013624   8.851  < 2e-16 ***
#		Folasum1_TEI_adjusted_norm_sd      -0.078545   0.016738  -4.693 2.70e-06 ***
#		B2sum1_TEI_adjusted_norm_sd        -0.057871   0.023507  -2.462 0.013827 *  
#		NIACsum1_TEI_adjusted_norm_sd      -0.059744   0.020291  -2.944 0.003237 ** 
#		B6sum1_TEI_adjusted_norm_sd         0.076718   0.016175   4.743 2.11e-06 ***
#		B12sum1_TEI_adjusted_norm_sd       -0.021219   0.012094  -1.755 0.079344 .  
#		askosum1_TEI_adjusted_norm_sd       0.043935   0.009129   4.813 1.49e-06 ***
#		Dsum1_TEI_adjusted_norm_sd         -0.006684   0.013340  -0.501 0.616338    
#		tokosum1_TEI_adjusted_norm_sd       0.005199   0.011602   0.448 0.654060    
#		VITKsum1_TEI_adjusted_norm_sd      -0.011232   0.007129  -1.575 0.115166    
#		jernsum1_TEI_adjusted_norm_sd      -0.082972   0.022424  -3.700 0.000216 ***
#		JODIsum1_TEI_adjusted_norm_sd      -0.057414   0.008815  -6.513 7.45e-11 ***
#		kalcsum1_TEI_adjusted_norm_sd      -0.055141   0.019222  -2.869 0.004125 ** 
#		KALIsum1_TEI_adjusted_norm_sd       0.209853   0.023158   9.062  < 2e-16 ***
#		---
#		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.9247557)
#
#Null deviance: 44234  on 45651  degrees of freedom
#Residual deviance: 42182  on 45614  degrees of freedom
#(1455 observations deleted due to missingness)
#AIC: 126024
#
#Number of Fisher Scoring iterations: 2




vif(full_model_macro_micronutrients)
#> vif(full_model_macro_micronutrients)
#POLYsum1_TEI_adjusted_norm_sd      MONOsum1_TEI_adjusted_norm_sd 
#33.150767                          10.576669 
#mfetsum1_TEI_adjusted_norm_sd      fettsum1_TEI_adjusted_norm_sd 
#45.784520                          63.515798 
#sacksum1_TEI_adjusted_norm_sd      kolhsum1_TEI_adjusted_norm_sd 
#8.750017                          16.305520 
#FA_TEI_adjusted_norm_sd      protsum1_TEI_adjusted_norm_sd 
#26.135955                          33.334864 
#protsum1_anim_TEI_adjusted_norm_sd  protsum1_veg_TEI_adjusted_norm_sd 
#26.733509                          12.538124 
#fibesum1_TEI_adjusted_norm_sd      DISAsum1_TEI_adjusted_norm_sd 
#10.912324                          12.316709 
#MOSAsum1_TEI_adjusted_norm_sd     TRANSsum1_TEI_adjusted_norm_sd 
#6.552456                           2.316074 
#NATRsum1_TEI_adjusted_norm_sd      kolesum1_TEI_adjusted_norm_sd 
#3.606329                          16.036211 
#ensum1_norm_sd      MAGNsum1_TEI_adjusted_norm_sd 
#1.000009                          14.485045 
#FOSFsum1_TEI_adjusted_norm_sd      selesum1_TEI_adjusted_norm_sd 
#23.109413                           7.507299 
#ZINCsum1_TEI_adjusted_norm_sd      retisum1_TEI_adjusted_norm_sd 
#5.298599                           3.687160 
#karosum1_TEI_adjusted_norm_sd      TIAMsum1_TEI_adjusted_norm_sd 
#2.673827                           3.968945 
#Folasum1_TEI_adjusted_norm_sd        B2sum1_TEI_adjusted_norm_sd 
#9.579023                          11.426734 
#NIACsum1_TEI_adjusted_norm_sd        B6sum1_TEI_adjusted_norm_sd 
#8.144608                           5.247855 
#B12sum1_TEI_adjusted_norm_sd      askosum1_TEI_adjusted_norm_sd 
#4.015008                           3.164409 
#Dsum1_TEI_adjusted_norm_sd      tokosum1_TEI_adjusted_norm_sd 
#4.214939                           3.004444 
#VITKsum1_TEI_adjusted_norm_sd      jernsum1_TEI_adjusted_norm_sd 
#1.586324                           5.252960 
#JODIsum1_TEI_adjusted_norm_sd      kalcsum1_TEI_adjusted_norm_sd 
#2.322044                          10.813244 
#KALIsum1_TEI_adjusted_norm_sd 
#12.305460 
#

VIP_data_independant_complete_cases <- na.omit(VIP_data_independant[,c("basic_residuals_bmi", "POLYsum1_TEI_adjusted_norm_sd","MONOsum1_TEI_adjusted_norm_sd","mfetsum1_TEI_adjusted_norm_sd",
						"fettsum1_TEI_adjusted_norm_sd","sacksum1_TEI_adjusted_norm_sd","kolhsum1_TEI_adjusted_norm_sd","FA_TEI_adjusted_norm_sd",
						"protsum1_TEI_adjusted_norm_sd","protsum1_anim_TEI_adjusted_norm_sd","protsum1_veg_TEI_adjusted_norm_sd",
						"fibesum1_TEI_adjusted_norm_sd","DISAsum1_TEI_adjusted_norm_sd","MOSAsum1_TEI_adjusted_norm_sd","TRANSsum1_TEI_adjusted_norm_sd",
						"NATRsum1_TEI_adjusted_norm_sd","kolesum1_TEI_adjusted_norm_sd","ensum1_norm_sd", "MAGNsum1_TEI_adjusted_norm_sd","FOSFsum1_TEI_adjusted_norm_sd",
						"selesum1_TEI_adjusted_norm_sd","ZINCsum1_TEI_adjusted_norm_sd","retisum1_TEI_adjusted_norm_sd",
						"karosum1_TEI_adjusted_norm_sd","TIAMsum1_TEI_adjusted_norm_sd","Folasum1_TEI_adjusted_norm_sd",
						"B2sum1_TEI_adjusted_norm_sd","NIACsum1_TEI_adjusted_norm_sd","B6sum1_TEI_adjusted_norm_sd","B12sum1_TEI_adjusted_norm_sd",
						"askosum1_TEI_adjusted_norm_sd","Dsum1_TEI_adjusted_norm_sd","tokosum1_TEI_adjusted_norm_sd",
						"VITKsum1_TEI_adjusted_norm_sd","jernsum1_TEI_adjusted_norm_sd","JODIsum1_TEI_adjusted_norm_sd",
						"kalcsum1_TEI_adjusted_norm_sd","KALIsum1_TEI_adjusted_norm_sd")])

predictors <- as.matrix(VIP_data_independant_complete_cases[,-1])
dependent <- VIP_data_independant_complete_cases$basic_residuals_bmi


model_selection_macro_micronutrients_elastic_net <- cv.glmnet(predictors,dependent,alpha= 0.5, family="gaussian")

coef(model_selection_macro_micronutrients_elastic_net)
#38 x 1 sparse Matrix of class "dgCMatrix"
#1
#(Intercept)                        -0.001675756
#POLYsum1_TEI_adjusted_norm_sd       .          
#MONOsum1_TEI_adjusted_norm_sd       0.043025226
#mfetsum1_TEI_adjusted_norm_sd      -0.072816190
#fettsum1_TEI_adjusted_norm_sd       .          
#sacksum1_TEI_adjusted_norm_sd      -0.001103022
#kolhsum1_TEI_adjusted_norm_sd       .          
#FA_TEI_adjusted_norm_sd            -0.012864472
#protsum1_TEI_adjusted_norm_sd       .          
#protsum1_anim_TEI_adjusted_norm_sd  0.079095850
#protsum1_veg_TEI_adjusted_norm_sd  -0.017723867
#fibesum1_TEI_adjusted_norm_sd       .          
#DISAsum1_TEI_adjusted_norm_sd       0.012613715
#MOSAsum1_TEI_adjusted_norm_sd       .          
#TRANSsum1_TEI_adjusted_norm_sd     -0.032879713
#NATRsum1_TEI_adjusted_norm_sd       0.159995972
#kolesum1_TEI_adjusted_norm_sd       .          
#ensum1_norm_sd                      0.009607752
#MAGNsum1_TEI_adjusted_norm_sd      -0.226833408
#FOSFsum1_TEI_adjusted_norm_sd      -0.038259515
#selesum1_TEI_adjusted_norm_sd      -0.009401695
#ZINCsum1_TEI_adjusted_norm_sd       0.099361222
#retisum1_TEI_adjusted_norm_sd       0.039897752
#karosum1_TEI_adjusted_norm_sd      -0.007044902
#TIAMsum1_TEI_adjusted_norm_sd       0.087039710
#Folasum1_TEI_adjusted_norm_sd      -0.053031084
#B2sum1_TEI_adjusted_norm_sd         .          
#NIACsum1_TEI_adjusted_norm_sd       .          
#B6sum1_TEI_adjusted_norm_sd         0.046570087
#B12sum1_TEI_adjusted_norm_sd        .          
#askosum1_TEI_adjusted_norm_sd       0.012817108
#Dsum1_TEI_adjusted_norm_sd          0.017654985
#tokosum1_TEI_adjusted_norm_sd       0.034033806
#VITKsum1_TEI_adjusted_norm_sd      -0.023035518
#jernsum1_TEI_adjusted_norm_sd      -0.009878221
#JODIsum1_TEI_adjusted_norm_sd      -0.054679396
#kalcsum1_TEI_adjusted_norm_sd      -0.009959457
#KALIsum1_TEI_adjusted_norm_sd       0.150600736
#
#
#



#-----taking bmi without basic covariates


#
#
#38 x 1 sparse Matrix of class "dgCMatrix"
#1
#(Intercept)                        -0.0007269646
#POLYsum1_TEI_adjusted_norm_sd       .           
#MONOsum1_TEI_adjusted_norm_sd       0.0530249068
#mfetsum1_TEI_adjusted_norm_sd      -0.0214043501
#fettsum1_TEI_adjusted_norm_sd       .           
#sacksum1_TEI_adjusted_norm_sd      -0.0216445478
#kolhsum1_TEI_adjusted_norm_sd       .           
#FA_TEI_adjusted_norm_sd            -0.0393836740
#protsum1_TEI_adjusted_norm_sd       .           
#protsum1_anim_TEI_adjusted_norm_sd  0.0766010530
#protsum1_veg_TEI_adjusted_norm_sd  -0.0031866230
#fibesum1_TEI_adjusted_norm_sd       .           
#DISAsum1_TEI_adjusted_norm_sd       .           
#MOSAsum1_TEI_adjusted_norm_sd      -0.0430981809
#TRANSsum1_TEI_adjusted_norm_sd     -0.1150261842
#NATRsum1_TEI_adjusted_norm_sd       0.1407787522
#kolesum1_TEI_adjusted_norm_sd      -0.0294835575
#ensum1_norm_sd                      0.0550592802
#MAGNsum1_TEI_adjusted_norm_sd      -0.1441553030
#FOSFsum1_TEI_adjusted_norm_sd      -0.0161183853
#selesum1_TEI_adjusted_norm_sd       .           
#ZINCsum1_TEI_adjusted_norm_sd       0.0608472311
#retisum1_TEI_adjusted_norm_sd       .           
#karosum1_TEI_adjusted_norm_sd      -0.0075446523
#TIAMsum1_TEI_adjusted_norm_sd       0.0716013371
#Folasum1_TEI_adjusted_norm_sd      -0.0927870312
#B2sum1_TEI_adjusted_norm_sd         .           
#NIACsum1_TEI_adjusted_norm_sd       .           
#B6sum1_TEI_adjusted_norm_sd         0.0217456768
#B12sum1_TEI_adjusted_norm_sd        0.0109133523
#askosum1_TEI_adjusted_norm_sd       0.0980104641
#Dsum1_TEI_adjusted_norm_sd          0.0382862917
#tokosum1_TEI_adjusted_norm_sd       0.0166582121
#VITKsum1_TEI_adjusted_norm_sd      -0.0159346806
#jernsum1_TEI_adjusted_norm_sd      -0.0538978234
#JODIsum1_TEI_adjusted_norm_sd      -0.0741232330
#kalcsum1_TEI_adjusted_norm_sd       .           
#KALIsum1_TEI_adjusted_norm_sd       0.0522729300


#try regular lm for the variables selected
selected_model_macro_micronutrients<-glm(basic_residuals_bmi~MONOsum1_TEI_adjusted_norm_sd+mfetsum1_TEI_adjusted_norm_sd+sacksum1_TEI_adjusted_norm_sd+
				FA_TEI_adjusted_norm_sd+protsum1_anim_TEI_adjusted_norm_sd+protsum1_veg_TEI_adjusted_norm_sd+
				MOSAsum1_TEI_adjusted_norm_sd+TRANSsum1_TEI_adjusted_norm_sd+
				NATRsum1_TEI_adjusted_norm_sd+kolesum1_TEI_adjusted_norm_sd+ensum1_norm_sd+ MAGNsum1_TEI_adjusted_norm_sd+
				FOSFsum1_TEI_adjusted_norm_sd+ZINCsum1_TEI_adjusted_norm_sd+
				karosum1_TEI_adjusted_norm_sd+TIAMsum1_TEI_adjusted_norm_sd+Folasum1_TEI_adjusted_norm_sd+B2sum1_TEI_adjusted_norm_sd+
				B12sum1_TEI_adjusted_norm_sd+askosum1_TEI_adjusted_norm_sd+
				Dsum1_TEI_adjusted_norm_sd+tokosum1_TEI_adjusted_norm_sd+VITKsum1_TEI_adjusted_norm_sd+jernsum1_TEI_adjusted_norm_sd+
				JODIsum1_TEI_adjusted_norm_sd+KALIsum1_TEI_adjusted_norm_sd, family = gaussian(link = "identity"))


summary(selected_model_macro_micronutrients)

#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                        -0.001693   0.004510  -0.376 0.707269    
#		MONOsum1_TEI_adjusted_norm_sd       0.100125   0.020322   4.927 8.39e-07 ***
#		mfetsum1_TEI_adjusted_norm_sd      -0.117977   0.025764  -4.579 4.68e-06 ***
#		sacksum1_TEI_adjusted_norm_sd      -0.014111   0.010275  -1.373 0.169676    #####n.s.
#		FA_TEI_adjusted_norm_sd            -0.038521   0.009106  -4.230 2.34e-05 ***
#		protsum1_anim_TEI_adjusted_norm_sd  0.117078   0.022378   5.232 1.68e-07 ***
#		protsum1_veg_TEI_adjusted_norm_sd  -0.003728   0.015022  -0.248 0.803999    #####n.s.
#		MOSAsum1_TEI_adjusted_norm_sd       0.001044   0.011498   0.091 0.927659    #####n.s.
#		TRANSsum1_TEI_adjusted_norm_sd     -0.016387   0.007764  -2.111 0.034801 *  
#		NATRsum1_TEI_adjusted_norm_sd       0.185397   0.015788  11.743  < 2e-16 ***
#		kolesum1_TEI_adjusted_norm_sd      -0.051853   0.022324  -2.323 0.020197 *  
#		ensum1_norm_sd                      0.014605   0.004517   3.234 0.001223 ** 
#		MAGNsum1_TEI_adjusted_norm_sd      -0.295201   0.027343 -10.796  < 2e-16 ***
#		FOSFsum1_TEI_adjusted_norm_sd      -0.106468   0.032969  -3.229 0.001242 ** 
#		ZINCsum1_TEI_adjusted_norm_sd       0.153258   0.015669   9.781  < 2e-16 ***
#		karosum1_TEI_adjusted_norm_sd      -0.014900   0.007155  -2.082 0.037305 *  
#		TIAMsum1_TEI_adjusted_norm_sd       0.107562   0.012334   8.721  < 2e-16 ***
#		Folasum1_TEI_adjusted_norm_sd      -0.084328   0.015150  -5.566 2.62e-08 ***
#		B2sum1_TEI_adjusted_norm_sd        -0.003431   0.018763  -0.183 0.854917    #####n.s.
#		B12sum1_TEI_adjusted_norm_sd       -0.010727   0.011477  -0.935 0.349967    #####n.s.
#		askosum1_TEI_adjusted_norm_sd       0.029644   0.008598   3.448 0.000566 ***
#		Dsum1_TEI_adjusted_norm_sd          0.035730   0.009816   3.640 0.000273 ***
#		tokosum1_TEI_adjusted_norm_sd       0.037164   0.009684   3.838 0.000124 ***
#		VITKsum1_TEI_adjusted_norm_sd      -0.025739   0.006886  -3.738 0.000186 ***
#		jernsum1_TEI_adjusted_norm_sd      -0.060245   0.019936  -3.022 0.002513 ** 
#		JODIsum1_TEI_adjusted_norm_sd      -0.067276   0.007925  -8.490  < 2e-16 ***
#		KALIsum1_TEI_adjusted_norm_sd       0.234569   0.018305  12.814  < 2e-16 ***
#		---
#		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.9283819)
#
#Null deviance: 44234  on 45651  degrees of freedom
#Residual deviance: 42357  on 45625  degrees of freedom
#(1455 observations deleted due to missingness)
#AIC: 126191
#
#Number of Fisher Scoring iterations: 2
#
#> 

#
#> vif(selected_model_macro_micronutrients)
#MONOsum1_TEI_adjusted_norm_sd      mfetsum1_TEI_adjusted_norm_sd 
#6.606398                          11.783190 
#sacksum1_TEI_adjusted_norm_sd            FA_TEI_adjusted_norm_sd 
#3.543788                           2.212097 
#protsum1_anim_TEI_adjusted_norm_sd  protsum1_veg_TEI_adjusted_norm_sd 
#11.228620                           5.158614 
#MOSAsum1_TEI_adjusted_norm_sd     TRANSsum1_TEI_adjusted_norm_sd 
#5.231356                           2.011489 
#NATRsum1_TEI_adjusted_norm_sd      kolesum1_TEI_adjusted_norm_sd 
#3.360197                          10.470687 
#ensum1_norm_sd      MAGNsum1_TEI_adjusted_norm_sd 
#1.000007                          12.531216 
#FOSFsum1_TEI_adjusted_norm_sd      ZINCsum1_TEI_adjusted_norm_sd 
#15.944473                           3.992602 
#karosum1_TEI_adjusted_norm_sd      TIAMsum1_TEI_adjusted_norm_sd 
#2.484882                           3.240158 
#Folasum1_TEI_adjusted_norm_sd        B2sum1_TEI_adjusted_norm_sd 
#7.816920                           7.251337 
#B12sum1_TEI_adjusted_norm_sd      askosum1_TEI_adjusted_norm_sd 
#3.601901                           2.796372 
#Dsum1_TEI_adjusted_norm_sd      tokosum1_TEI_adjusted_norm_sd 
#2.273276                           2.084865 
#VITKsum1_TEI_adjusted_norm_sd      jernsum1_TEI_adjusted_norm_sd 
#1.474044                           4.135960 
#JODIsum1_TEI_adjusted_norm_sd      KALIsum1_TEI_adjusted_norm_sd 
#1.869147                           7.658726 
#> 
		

		