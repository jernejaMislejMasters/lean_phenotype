library(ppcor)
library(Hmisc)
library(car)
library(glmulti)
library(leaps)
library(modelUtils)
library(DSA)


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
#POLYsum1_TEI_adjusted_norm_sd       0.134088   0.034234   3.917 8.99e-05 ***
#		MONOsum1_TEI_adjusted_norm_sd       0.019605   0.023281   0.842 0.399731    
#mfetsum1_TEI_adjusted_norm_sd      -0.415390   0.036278 -11.450  < 2e-16 ***
#		fettsum1_TEI_adjusted_norm_sd       0.518570   0.053337   9.722  < 2e-16 ***
#		sacksum1_TEI_adjusted_norm_sd      -0.075868   0.011558  -6.564 5.28e-11 ***
#		kolhsum1_TEI_adjusted_norm_sd       0.113754   0.029567   3.847 0.000120 ***
#		FA_TEI_adjusted_norm_sd            -0.239155   0.028875  -8.282  < 2e-16 ***
#		protsum1_TEI_adjusted_norm_sd       0.005710   0.041809   0.137 0.891368    
#protsum1_anim_TEI_adjusted_norm_sd  0.122156   0.031600   3.866 0.000111 ***
#		protsum1_veg_TEI_adjusted_norm_sd  -0.134439   0.020473  -6.567 5.20e-11 ***
#		fibesum1_TEI_adjusted_norm_sd       0.018643   0.013573   1.374 0.169601    
#DISAsum1_TEI_adjusted_norm_sd       0.017506   0.012816   1.366 0.171952    
#MOSAsum1_TEI_adjusted_norm_sd       0.046413   0.010269   4.520 6.21e-06 ***
#		TRANSsum1_TEI_adjusted_norm_sd     -0.008177   0.007320  -1.117 0.263964    
#NATRsum1_TEI_adjusted_norm_sd       0.139644   0.013079  10.677  < 2e-16 ***
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

model_selection_macronutrients_e <- glmulti(full_model_macronutrients, level = 1, crit="aicc", confsetsize=131072)

summary(model_selection_macronutrients_e)

capture.output(summary(model_selection_macronutrients_e), file = "model_selection_macronutrients_e")

