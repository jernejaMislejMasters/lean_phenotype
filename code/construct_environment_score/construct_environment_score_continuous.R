library(ppcor)
library(Hmisc)
library(car)
library(glmulti)
library(leaps)
library(modelUtils)
library(DSA)


#load entire cleaned data (154009 subjects)
VIP_data_all <- read.csv("VIP_data/VIP_170206_cleaned.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)

#load the subset
VIP_data_subset <- read.csv("VIP_data/VIP_170206_cleaned_subset.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)
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

source(file="Code/load_variables_independent_dataset_TEI_adjusted.R")


#obtain the residuals from the basic covariates
basic_model<-glm(bmi_norm_sd~age + agesq + gender_factor + year + ffq_factor, family = gaussian(link = "identity"))
basic_residuals<-bmi_norm_sd
basic_residuals[!is.na(bmi_norm_sd)]<-basic_model$residuals


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
				NATRsum1_TEI_adjusted_norm_sd,kolesum1_TEI_adjusted_norm_sd))



#MACRONUTRIENTS

#continous.....getting different numbers than if fitting with basic covariates, some quite different, there is a lot of colinearity with the basic covariates also

full_model_macronutrients<-glm(basic_residuals~POLYsum1_TEI_adjusted_norm_sd+MONOsum1_TEI_adjusted_norm_sd+mfetsum1_TEI_adjusted_norm_sd+
				fettsum1_TEI_adjusted_norm_sd+sacksum1_TEI_adjusted_norm_sd+kolhsum1_TEI_adjusted_norm_sd+FA_TEI_adjusted_norm_sd+
				protsum1_TEI_adjusted_norm_sd+protsum1_anim_TEI_adjusted_norm_sd+protsum1_veg_TEI_adjusted_norm_sd+
				fibesum1_TEI_adjusted_norm_sd+DISAsum1_TEI_adjusted_norm_sd+MOSAsum1_TEI_adjusted_norm_sd+TRANSsum1_TEI_adjusted_norm_sd+
				NATRsum1_TEI_adjusted_norm_sd+kolesum1_TEI_adjusted_norm_sd, family = gaussian(link = "identity"))


#check results for all variables:
#together
summary(full_model_macronutrients)
vif(full_model_macronutrients)

macronutrients_no_missing<-na.omit(VIP_data_independant[,c("age","agesq","year","ffq","gender","bmi_norm_sd",macronutrients_variables)])

#r²
variable_count<-1
for (variable in macronutrients_variables){
	
	partial_corr<-pcor.test(macronutrients_no_missing$bmi_norm_sd,macronutrients_no_missing[,c(variable)],
			macronutrients_no_missing[,c("age","agesq","year","ffq","gender",macronutrients_variables[-variable_count])])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
	variable_count<-variable_count+1
	
}

#separately
for (variable in macronutrients_variables){
	
	associations<-glm(bmi_norm_sd~age + agesq + gender_factor + year + ffq_factor + VIP_data_independant[,c(variable)], family = gaussian(link = "identity"))
	associations_summary<-summary(associations)
	
	message(variable)
	message(round(associations_summary$coefficients[[7]],7))
	message(round(associations_summary$coefficients[[28]],7))
	
}

#r²
for (variable in macronutrients_variables){
	
	partial_corr<-pcor.test(bmi_norm_sd[(!is.na(bmi_norm_sd) & !is.na(VIP_data_independant[,c(variable)]))],VIP_data_independant[(!is.na(bmi_norm_sd) & !is.na(VIP_data_independant[,c(variable)])),c(variable)],
			VIP_data_independant[(!is.na(bmi_norm_sd) & !is.na(VIP_data_independant[,c(variable)])),c("age","agesq","year","ffq","gender")])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
}



#estimate best model based on different scores


#AIC, glmulti


#exhaustive search
model_selection_macronutrients_e <- glmulti(full_model_macronutrients, level = 1, crit="aicc", confsetsize=65536)

summary(model_selection_macronutrients_e)

weightable(model_selection_macronutrients_e)

#genetic algorithm
model_selection_macronutrients_g <- glmulti(full_model_macronutrients, level = 1, crit="aicc", method="g")

summary(model_selection_macronutrients_g)

weightable(model_selection_macronutrients_g)


#branch-and-bound algorithm
model_selection_macronutrients_l <- glmulti(full_model_macronutrients, level = 1, crit="aicc", method="l")

summary(model_selection_macronutrients_l)

weightable(model_selection_macronutrients_l)


#L2 with cross-validation, DSA
model_selection_macronutrients_d<-DSA(basic_residuals~POLYsum1_TEI_adjusted_norm_sd+MONOsum1_TEI_adjusted_norm_sd+mfetsum1_TEI_adjusted_norm_sd+
								fettsum1_TEI_adjusted_norm_sd+sacksum1_TEI_adjusted_norm_sd+kolhsum1_TEI_adjusted_norm_sd+FA_TEI_adjusted_norm_sd+
								protsum1_TEI_adjusted_norm_sd+protsum1_anim_TEI_adjusted_norm_sd+protsum1_veg_TEI_adjusted_norm_sd+
								fibesum1_TEI_adjusted_norm_sd+DISAsum1_TEI_adjusted_norm_sd+MOSAsum1_TEI_adjusted_norm_sd+TRANSsum1_TEI_adjusted_norm_sd+
								NATRsum1_TEI_adjusted_norm_sd+kolesum1_TEI_adjusted_norm_sd, data=VIP_data_independant, maxsumofpow = 1,
								maxsize=16, maxorderint = 1, nsplits=10,vfold=10)

						
						
						
