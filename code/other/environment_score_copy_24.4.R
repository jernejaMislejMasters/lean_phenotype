source(file="Code/load_indp_variables_MAD_based.R")

# pairwise correlations

#original

# continuous
rcorr( cbind(bmi_norm_sd,POLYsum1_norm_sd,MONOsum1_norm_sd,mfetsum1_norm_sd,fettsum1_norm_sd,sacksum1_norm_sd,kolhsum1_norm_sd,FA_norm_sd,protsum1_norm_sd,fibesum1_norm_sd,NATRsum1_norm_sd),type="pearson")

# % of TEI
rcorr( cbind(bmi_norm_sd,POLYsum1_ofTEI_norm_sd,MONOsum1_ofTEI_norm_sd,mfetsum1_ofTEI_norm_sd,fettsum1_ofTEI_norm_sd,sacksum1_ofTEI_norm_sd,kolhsum1_ofTEI_norm_sd,FA_ofTEI_norm_sd,protsum1_ofTEI_norm_sd,fibesum1_ofTEI_norm_sd,NATRsum1_ofTEI_norm_sd),type="pearson")

# % of TEI and categorized
rcorr( cbind(bmi_norm_sd,POLYsum1_ofTEI_categorized_g,MONOsum1_ofTEI_categorized_g,mfetsum1_ofTEI_categorized_g,fettsum1_ofTEI_categorized_g,sacksum1_ofTEI_categorized_g,kolhsum1_ofTEI_categorized_g,FA_ofTEI_categorized_g,protsum1_ofTEI_categorized_g,fibesum1_categorized_g,NATRsum1_categorized_g),type="pearson")


#filtered

# continuous
rcorr( cbind(bmi_3MAD_norm_sd,POLYsum1_3MAD_norm_sd,MONOsum1_3MAD_norm_sd,mfetsum1_3MAD_norm_sd,fettsum1_3MAD_norm_sd,sacksum1_3MAD_norm_sd,kolhsum1_3MAD_norm_sd,FA_3MAD_norm_sd,protsum1_3MAD_norm_sd,fibesum1_3MAD_norm_sd,NATRsum1_3MAD_norm_sd),type="pearson")

# % of TEI
rcorr( cbind(bmi_3MAD_norm_sd,POLYsum1_ofTEI_3MAD_norm_sd,MONOsum1_ofTEI_3MAD_norm_sd,mfetsum1_ofTEI_3MAD_norm_sd,fettsum1_ofTEI_3MAD_norm_sd,sacksum1_ofTEI_3MAD_norm_sd,kolhsum1_ofTEI_norm_sd,FA_ofTEI_3MAD_norm_sd,protsum1_ofTEI_3MAD_norm_sd,fibesum1_ofTEI_3MAD_norm_sd,NATRsum1_ofTEI_3MAD_norm_sd),type="pearson")

# % of TEI and categorized
rcorr( cbind(bmi_3MAD_norm_sd,POLYsum1_ofTEI_3MAD_categorized_g,MONOsum1_ofTEI_3MAD_categorized_g,mfetsum1_ofTEI_3MAD_categorized_g,fettsum1_ofTEI_categorized_g,sacksum1_ofTEI_3MAD_categorized_g,kolhsum1_ofTEI_3MAD_categorized_g,FA_ofTEI_3MAD_categorized_g,protsum1_ofTEI_3MAD_categorized_g,fibesum1_3MAD_categorized_g,NATRsum1_3MAD_categorized_g),type="pearson")



# regressions 

#original

# continuous

#together
associations<-glm(bmi_norm_sd~age + agesq + gender_factor + year + ffq_factor + POLYsum1_norm_sd + MONOsum1_norm_sd + mfetsum1_norm_sd + fettsum1_norm_sd + sacksum1_norm_sd
				+ kolhsum1_norm_sd + FA_norm_sd + protsum1_norm_sd + fibesum1_norm_sd + NATRsum1_norm_sd, family = gaussian(link = "identity"))

summary(associations)
vif(associations)

#r²
all_variables<-c("POLYsum1_norm_sd","MONOsum1_norm_sd","mfetsum1_norm_sd","fettsum1_norm_sd","sacksum1_norm_sd",
		"kolhsum1_norm_sd","FA_norm_sd","protsum1_norm_sd","fibesum1_norm_sd","NATRsum1_norm_sd")

independant_no_missing<-na.omit(VIP_data_independant[,c("age","agesq","year","ffq","gender","bmi_norm_sd",all_variables)])

variable_count<-1
for (variable in all_variables){
	
	partial_corr<-pcor.test(independant_no_missing$bmi_norm_sd,independant_no_missing[,c(variable)],
			independant_no_missing[,c("age","agesq","year","ffq","gender",all_variables[-variable_count])])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
	variable_count<-variable_count+1
	
}


#separately
for (variable in all_variables){
	
	associations<-glm(bmi_norm_sd~age + agesq + gender_factor + year + ffq_factor + VIP_data_independant[,c(variable)], family = gaussian(link = "identity"))
	associations_summary<-summary(associations)
	
	message(variable)
	message(round(associations_summary$coefficients[[7]],7))
	message(round(associations_summary$coefficients[[28]],7))
	
}

#r²
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi_norm_sd[(!is.na(bmi_norm_sd) & !is.na(VIP_data_independant[,c(variable)]))],VIP_data_independant[(!is.na(bmi_norm_sd) & !is.na(VIP_data_independant[,c(variable)])),c(variable)],
			VIP_data_independant[(!is.na(bmi_norm_sd) & !is.na(VIP_data_independant[,c(variable)])),c("age","agesq","year","ffq","gender")])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
}



# % of TEI

#together
associations<-glm(bmi_norm_sd~age + agesq + gender_factor + year + ffq_factor + POLYsum1_ofTEI_norm_sd + MONOsum1_ofTEI_norm_sd + mfetsum1_ofTEI_norm_sd + fettsum1_ofTEI_norm_sd + sacksum1_ofTEI_norm_sd
				+ kolhsum1_ofTEI_norm_sd + FA_ofTEI_norm_sd + protsum1_ofTEI_norm_sd + fibesum1_ofTEI_norm_sd + NATRsum1_ofTEI_norm_sd, family = gaussian(link = "identity"))

summary(associations)
vif(associations)

all_variables<-c("POLYsum1_ofTEI_norm_sd","MONOsum1_ofTEI_norm_sd","mfetsum1_ofTEI_norm_sd","fettsum1_ofTEI_norm_sd","sacksum1_ofTEI_norm_sd",
		"kolhsum1_ofTEI_norm_sd","FA_ofTEI_norm_sd","protsum1_ofTEI_norm_sd","fibesum1_ofTEI_norm_sd","NATRsum1_ofTEI_norm_sd")

independant_no_missing<-na.omit(VIP_data_independant[,c("age","agesq","year","ffq","gender","bmi_norm_sd",all_variables)])

#r²
variable_count<-1
for (variable in all_variables){
	
	partial_corr<-pcor.test(independant_no_missing$bmi_norm_sd,independant_no_missing[,c(variable)],
			independant_no_missing[,c("age","agesq","year","ffq","gender",all_variables[-variable_count])])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
	variable_count<-variable_count+1
	
}


#separately
for (variable in all_variables){
	
	associations<-glm(bmi_norm_sd~age + agesq + gender_factor + year + ffq_factor + VIP_data_independant[,c(variable)], family = gaussian(link = "identity"))
	associations_summary<-summary(associations)
	
	message(variable)
	message(round(associations_summary$coefficients[[7]],7))
	message(round(associations_summary$coefficients[[28]],7))
	
}

#r²
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi_norm_sd[(!is.na(bmi_norm_sd) & !is.na(VIP_data_independant[,c(variable)]))],VIP_data_independant[(!is.na(bmi_norm_sd) & !is.na(VIP_data_independant[,c(variable)])),c(variable)],
			VIP_data_independant[(!is.na(bmi_norm_sd) & !is.na(VIP_data_independant[,c(variable)])),c("age","agesq","year","ffq","gender")])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
}







#  % of TEI and categorized
associations<-glm(bmi_norm_sd~age + agesq + gender_factor + year + ffq_factor + POLYsum1_ofTEI_categorized_g + MONOsum1_ofTEI_categorized_g + mfetsum1_ofTEI_categorized_g + fettsum1_ofTEI_categorized_g + sacksum1_ofTEI_categorized_g
				+ kolhsum1_ofTEI_categorized_g + FA_ofTEI_categorized_g + protsum1_ofTEI_categorized_g + fibesum1_categorized_g + NATRsum1_categorized_g, family = gaussian(link = "identity"))

summary(associations)
vif(associations)

all_variables<-c("POLYsum1_ofTEI_categorized_g","MONOsum1_ofTEI_categorized_g","mfetsum1_ofTEI_categorized_g","fettsum1_ofTEI_categorized_g","sacksum1_ofTEI_categorized_g",
		"kolhsum1_ofTEI_categorized_g","FA_ofTEI_categorized_g","protsum1_ofTEI_categorized_g","fibesum1_categorized_g","NATRsum1_categorized_g")

independant_no_missing<-na.omit(VIP_data_independant[,c("age","agesq","year","ffq","gender","bmi_norm_sd",all_variables)])

#r²
variable_count<-1
for (variable in all_variables){
	
	partial_corr<-pcor.test(independant_no_missing$bmi_norm_sd,independant_no_missing[,c(variable)],
			independant_no_missing[,c("age","agesq","year","ffq","gender",all_variables[-variable_count])])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
	variable_count<-variable_count+1
	
}


# separately
for (variable in all_variables){
	
	associations<-glm(bmi_norm_sd~age + agesq + gender_factor + year + ffq_factor + VIP_data_independant[,c(variable)], family = gaussian(link = "identity"))
	associations_summary<-summary(associations)
	
	message(variable)
	message(round(associations_summary$coefficients[[7]],7))
	message(round(associations_summary$coefficients[[28]],7))
	
}

#r²
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi_norm_sd[(!is.na(bmi_norm_sd) & !is.na(VIP_data_independant[,c(variable)]))],VIP_data_independant[(!is.na(bmi_norm_sd) & !is.na(VIP_data_independant[,c(variable)])),c(variable)],
			VIP_data_independant[(!is.na(bmi_norm_sd) & !is.na(VIP_data_independant[,c(variable)])),c("age","agesq","year","ffq","gender")])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
}





#filtered



# continuous

#together
associations<-glm(bmi_3MAD_norm_sd~age + agesq + gender_factor + year + ffq_factor + POLYsum1_3MAD_norm_sd + MONOsum1_3MAD_norm_sd + mfetsum1_3MAD_norm_sd + fettsum1_3MAD_norm_sd + sacksum1_3MAD_norm_sd
				+ kolhsum1_3MAD_norm_sd + FA_3MAD_norm_sd + protsum1_3MAD_norm_sd + fibesum1_3MAD_norm_sd + NATRsum1_3MAD_norm_sd, family = gaussian(link = "identity"))

summary(associations)
vif(associations)

#r²
all_variables<-c("POLYsum1_3MAD_norm_sd","MONOsum1_3MAD_norm_sd","mfetsum1_3MAD_norm_sd","fettsum1_3MAD_norm_sd","sacksum1_3MAD_norm_sd",
		"kolhsum1_3MAD_norm_sd","FA_3MAD_norm_sd","protsum1_3MAD_norm_sd","fibesum1_3MAD_norm_sd","NATRsum1_3MAD_norm_sd")

independant_no_missing<-na.omit(VIP_data_independant[,c("age","agesq","year","ffq","gender","bmi_3MAD_norm_sd",all_variables)])

variable_count<-1
for (variable in all_variables){
	
	partial_corr<-pcor.test(independant_no_missing$bmi_3MAD_norm_sd,independant_no_missing[,c(variable)],
			independant_no_missing[,c("age","agesq","year","ffq","gender",all_variables[-variable_count])])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
	variable_count<-variable_count+1
	
}


#separately
for (variable in all_variables){
	
	associations<-glm(bmi_3MAD_norm_sd~age + agesq + gender_factor + year + ffq_factor + VIP_data_independant[,c(variable)], family = gaussian(link = "identity"))
	associations_summary<-summary(associations)
	
	message(variable)
	message(round(associations_summary$coefficients[[7]],7))
	message(round(associations_summary$coefficients[[28]],7))
	
}

#r²
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi_3MAD_norm_sd[(!is.na(bmi_3MAD_norm_sd) & !is.na(VIP_data_independant[,c(variable)]))],VIP_data_independant[(!is.na(bmi_3MAD_norm_sd) & !is.na(VIP_data_independant[,c(variable)])),c(variable)],
			VIP_data_independant[(!is.na(bmi_3MAD_norm_sd) & !is.na(VIP_data_independant[,c(variable)])),c("age","agesq","year","ffq","gender")])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
}



# % of TEI

#together
associations<-glm(bmi_3MAD_norm_sd~age + agesq + gender_factor + year + ffq_factor + POLYsum1_ofTEI_3MAD_norm_sd + MONOsum1_ofTEI_3MAD_norm_sd + mfetsum1_ofTEI_3MAD_norm_sd + fettsum1_ofTEI_3MAD_norm_sd + sacksum1_ofTEI_3MAD_norm_sd
				+ kolhsum1_ofTEI_3MAD_norm_sd + FA_ofTEI_3MAD_norm_sd + protsum1_ofTEI_3MAD_norm_sd + fibesum1_ofTEI_3MAD_norm_sd + NATRsum1_ofTEI_3MAD_norm_sd, family = gaussian(link = "identity"))

summary(associations)
vif(associations)

all_variables<-c("POLYsum1_ofTEI_3MAD_norm_sd","MONOsum1_ofTEI_3MAD_norm_sd","mfetsum1_ofTEI_3MAD_norm_sd","fettsum1_ofTEI_3MAD_norm_sd","sacksum1_ofTEI_3MAD_norm_sd",
		"kolhsum1_ofTEI_3MAD_norm_sd","FA_ofTEI_3MAD_norm_sd","protsum1_ofTEI_3MAD_norm_sd","fibesum1_ofTEI_3MAD_norm_sd","NATRsum1_ofTEI_3MAD_norm_sd")

independant_no_missing<-na.omit(VIP_data_independant[,c("age","agesq","year","ffq","gender","bmi_3MAD_norm_sd",all_variables)])

#r²
variable_count<-1
for (variable in all_variables){
	
	partial_corr<-pcor.test(independant_no_missing$bmi_3MAD_norm_sd,independant_no_missing[,c(variable)],
			independant_no_missing[,c("age","agesq","year","ffq","gender",all_variables[-variable_count])])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
	variable_count<-variable_count+1
	
}


#separately
for (variable in all_variables){
	
	associations<-glm(bmi_3MAD_norm_sd~age + agesq + gender_factor + year + ffq_factor + VIP_data_independant[,c(variable)], family = gaussian(link = "identity"))
	associations_summary<-summary(associations)
	
	message(variable)
	message(round(associations_summary$coefficients[[7]],7))
	message(round(associations_summary$coefficients[[28]],7))
	
}

#r²
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi_3MAD_norm_sd[(!is.na(bmi_3MAD_norm_sd) & !is.na(VIP_data_independant[,c(variable)]))],VIP_data_independant[(!is.na(bmi_3MAD_norm_sd) & !is.na(VIP_data_independant[,c(variable)])),c(variable)],
			VIP_data_independant[(!is.na(bmi_3MAD_norm_sd) & !is.na(VIP_data_independant[,c(variable)])),c("age","agesq","year","ffq","gender")])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
}







#  % of TEI and categorized
associations<-glm(bmi_3MAD_norm_sd~age + agesq + gender_factor + year + ffq_factor + POLYsum1_ofTEI_3MAD_categorized_g + MONOsum1_ofTEI_3MAD_categorized_g + mfetsum1_ofTEI_3MAD_categorized_g + fettsum1_ofTEI_3MAD_categorized_g + sacksum1_ofTEI_3MAD_categorized_g
				+ kolhsum1_ofTEI_3MAD_categorized_g + FA_ofTEI_3MAD_categorized_g + protsum1_ofTEI_3MAD_categorized_g + fibesum1_3MAD_categorized_g + NATRsum1_3MAD_categorized_g, family = gaussian(link = "identity"))

summary(associations)
vif(associations)

all_variables<-c("POLYsum1_ofTEI_3MAD_categorized_g","MONOsum1_ofTEI_3MAD_categorized_g","mfetsum1_ofTEI_3MAD_categorized_g","fettsum1_ofTEI_3MAD_categorized_g","sacksum1_ofTEI_3MAD_categorized_g",
		"kolhsum1_ofTEI_3MAD_categorized_g","FA_ofTEI_3MAD_categorized_g","protsum1_ofTEI_3MAD_categorized_g","fibesum1_3MAD_categorized_g","NATRsum1_3MAD_categorized_g")

independant_no_missing<-na.omit(VIP_data_independant[,c("age","agesq","year","ffq","gender","bmi_3MAD_norm_sd",all_variables)])

#r²
variable_count<-1
for (variable in all_variables){
	
	partial_corr<-pcor.test(independant_no_missing$bmi_3MAD_norm_sd,independant_no_missing[,c(variable)],
			independant_no_missing[,c("age","agesq","year","ffq","gender",all_variables[-variable_count])])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
	variable_count<-variable_count+1
	
}


# separately
for (variable in all_variables){
	
	associations<-glm(bmi_3MAD_norm_sd~age + agesq + gender_factor + year + ffq_factor + VIP_data_independant[,c(variable)], family = gaussian(link = "identity"))
	associations_summary<-summary(associations)
	
	message(variable)
	message(round(associations_summary$coefficients[[7]],7))
	message(round(associations_summary$coefficients[[28]],7))
	
}

#r²
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi_3MAD_norm_sd[(!is.na(bmi_3MAD_norm_sd) & !is.na(VIP_data_independant[,c(variable)]))],VIP_data_independant[(!is.na(bmi_3MAD_norm_sd) & !is.na(VIP_data_independant[,c(variable)])),c(variable)],
			VIP_data_independant[(!is.na(bmi_3MAD_norm_sd) & !is.na(VIP_data_independant[,c(variable)])),c("age","agesq","year","ffq","gender")])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
}






#check glmulti for continuous

#obtain the residuals from the basic covariates
basic_model<-glm(bmi_norm_sd~age + agesq + gender_factor + year + ffq_factor, family = gaussian(link = "identity"))
basic_residuals<-bmi_norm_sd
basic_residuals[!is.na(bmi_norm_sd)]<-basic_model$residuals



rcorr( cbind(age,agesq,gender_factor,year,ffq_factor,bmi_norm_sd,POLYsum1_norm_sd,MONOsum1_norm_sd,mfetsum1_norm_sd,fettsum1_norm_sd,sacksum1_norm_sd,kolhsum1_norm_sd,FA_norm_sd,protsum1_norm_sd,fibesum1_norm_sd,NATRsum1_norm_sd),type="pearson")

#continous.....getting different numbers than if fitting with basic covariates, some quite different, there is a lot of colinearity with the basic covariates also

full_model<-glm(basic_residuals~POLYsum1_norm_sd + MONOsum1_norm_sd + mfetsum1_norm_sd + fettsum1_norm_sd + sacksum1_norm_sd
				+ kolhsum1_norm_sd + FA_norm_sd + protsum1_norm_sd + fibesum1_norm_sd + NATRsum1_norm_sd, family = gaussian(link = "identity"))

model_selection <- glmulti(full_model, level = 1, crit="aicc", confsetsize=1024)

summary(model_selection)

#$bestmodel....missing sugar
#[1] "basic_residuals ~ 1 + POLYsum1_norm_sd + MONOsum1_norm_sd + mfetsum1_norm_sd + "
#[2] "    fettsum1_norm_sd + kolhsum1_norm_sd + FA_norm_sd + protsum1_norm_sd + "     
#[3] "    fibesum1_norm_sd + NATRsum1_norm_sd"                                        

# $bestic
#[1] 127014.6


weightable(model_selection)

# check partial model, exclude the individual fatts, leave only total fat, oterwise there is too much multicolinearity, exclude also carbs, since there is correlation with sugar
# fiber
partial_model<-glm(basic_residuals~fettsum1_norm_sd + sacksum1_norm_sd
				+ protsum1_norm_sd + fibesum1_norm_sd + NATRsum1_norm_sd, family = gaussian(link = "identity"))

model_selection <- glmulti(partial_model, level = 1, crit="aicc")

summary(model_selection)
#best model is all 
#$bestic
#[1] 127422.4


weightable(model_selection)


# % of TEI

full_model<-glm(basic_residuals~POLYsum1_ofTEI_norm_sd + MONOsum1_ofTEI_norm_sd + mfetsum1_ofTEI_norm_sd + fettsum1_ofTEI_norm_sd + sacksum1_ofTEI_norm_sd
				+ kolhsum1_ofTEI_norm_sd + FA_ofTEI_norm_sd + protsum1_ofTEI_norm_sd + fibesum1_ofTEI_norm_sd + NATRsum1_ofTEI_norm_sd, family = gaussian(link = "identity"))

model_selection <- glmulti(full_model, level = 1, crit="aicc", confsetsize=1024)

summary(model_selection)

#$bestmodel...missing carbs and sugar
#[1] "basic_residuals ~ 1 + POLYsum1_ofTEI_norm_sd + MONOsum1_ofTEI_norm_sd + "    
#[2] "    mfetsum1_ofTEI_norm_sd + fettsum1_ofTEI_norm_sd + FA_ofTEI_norm_sd + "   
#[3] "    protsum1_ofTEI_norm_sd + fibesum1_ofTEI_norm_sd + NATRsum1_ofTEI_norm_sd"

#$bestic
#[1]  127037.9



weightable(model_selection)

# check partial model, exclude the individual fatts, leave only total fat, oterwise there is too much multicolinearity, exclude also carbs, since there is correlation with sugar
# fiber
partial_model<-glm(basic_residuals~fettsum1_ofTEI_norm_sd + sacksum1_ofTEI_norm_sd
				+ protsum1_ofTEI_norm_sd + fibesum1_ofTEI_norm_sd + NATRsum1_ofTEI_norm_sd, family = gaussian(link = "identity"))

model_selection <- glmulti(partial_model, level = 1, crit="aicc")

summary(model_selection)
#$bestmodel...total fat missing
#[1] "basic_residuals ~ 1 + sacksum1_ofTEI_norm_sd + protsum1_ofTEI_norm_sd + "
#[2] "    fibesum1_ofTEI_norm_sd + NATRsum1_ofTEI_norm_sd"                     

#$bestic
#[1]  127346.5



weightable(model_selection)



#categorized

full_model<-glm(basic_residuals~POLYsum1_ofTEI_categorized_g + MONOsum1_ofTEI_categorized_g + mfetsum1_ofTEI_categorized_g + fettsum1_ofTEI_categorized_g + sacksum1_ofTEI_categorized_g
				+ kolhsum1_ofTEI_categorized_g + FA_ofTEI_categorized_g + protsum1_ofTEI_categorized_g + fibesum1_categorized_g + NATRsum1_categorized_g, family = gaussian(link = "identity"))

model_selection <- glmulti(full_model, level = 1, crit="aicc", confsetsize=1024)

summary(model_selection)
#$bestmodel...missing carbs and sugar
#[1] "basic_residuals ~ 1 + POLYsum1_ofTEI_categorized_g + MONOsum1_ofTEI_categorized_g + "
#[2] "    mfetsum1_ofTEI_categorized_g + fettsum1_ofTEI_categorized_g + "                  
#[3] "    protsum1_ofTEI_categorized_g + fibesum1_categorized_g + NATRsum1_categorized_g"  

#$bestic
#[1]  127792.5



weightable(model_selection)

# check partial model, exclude the individual fatts, leave only total fat, oterwise there is too much multicolinearity, exclude also carbs, since there is correlation with sugar
# fiber
partial_model<-glm(basic_residuals~fettsum1_ofTEI_categorized_g + sacksum1_ofTEI_categorized_g
				+ protsum1_ofTEI_categorized_g + fibesum1_categorized_g + NATRsum1_categorized_g, family = gaussian(link = "identity"))

model_selection <- glmulti(partial_model, level = 1, crit="aicc")

summary(model_selection)
#$bestmodel...missing sugar
#[1] "basic_residuals ~ 1 + fettsum1_ofTEI_categorized_g + protsum1_ofTEI_categorized_g + "
#[2] "    fibesum1_categorized_g + NATRsum1_categorized_g"                                 

#$bestic
#[1] 127849.5



weightable(model_selection)









#check glmulti for filtered

#obtain the residuals from the basic covariates
basic_model<-glm(bmi_3MAD_norm_sd~age + agesq + gender_factor + year + ffq_factor, family = gaussian(link = "identity"))
basic_residuals<-bmi_3MAD_norm_sd
basic_residuals[!is.na(bmi_3MAD_norm_sd)]<-basic_model$residuals



rcorr( cbind(age,agesq,gender_factor,year,ffq_factor,bmi_3MAD_norm_sd,POLYsum1_3MAD_norm_sd,MONOsum1_3MAD_norm_sd,mfetsum1_3MAD_norm_sd,fettsum1_3MAD_norm_sd,sacksum1_3MAD_norm_sd,kolhsum1_3MAD_norm_sd,FA_3MAD_norm_sd,protsum1_3MAD_norm_sd,fibesum1_3MAD_norm_sd,NATRsum1_3MAD_norm_sd),type="pearson")

#continous.....getting different numbers than if fitting with basic covariates, some quite different, there is a lot of colinearity with the basic covariates also

full_model<-glm(basic_residuals~POLYsum1_3MAD_norm_sd + MONOsum1_3MAD_norm_sd + mfetsum1_3MAD_norm_sd + fettsum1_3MAD_norm_sd + sacksum1_3MAD_norm_sd
				+ kolhsum1_3MAD_norm_sd + FA_3MAD_norm_sd + protsum1_3MAD_norm_sd + fibesum1_3MAD_norm_sd + NATRsum1_3MAD_norm_sd, family = gaussian(link = "identity"))

model_selection <- glmulti(full_model, level = 1, crit="aicc", confsetsize=1024)

summary(model_selection)

#best model is all 
# $bestic
#[1] 107778.7

weightable(model_selection)

# check partial model, exclude the individual fatts, leave only total fat, oterwise there is too much multicolinearity, exclude also carbs, since there is correlation with sugar
# fiber
partial_model<-glm(basic_residuals~fettsum1_3MAD_norm_sd + sacksum1_3MAD_norm_sd
				+ protsum1_3MAD_norm_sd + fibesum1_3MAD_norm_sd + NATRsum1_3MAD_norm_sd, family = gaussian(link = "identity"))

model_selection <- glmulti(partial_model, level = 1, crit="aicc")

summary(model_selection)
#best model is all 
#$bestic
#[1] 113166.8

weightable(model_selection)


# % of TEI

full_model<-glm(basic_residuals~POLYsum1_ofTEI_3MAD_norm_sd + MONOsum1_ofTEI_3MAD_norm_sd + mfetsum1_ofTEI_3MAD_norm_sd + fettsum1_ofTEI_3MAD_norm_sd + sacksum1_ofTEI_3MAD_norm_sd
				+ kolhsum1_ofTEI_3MAD_norm_sd + FA_ofTEI_3MAD_norm_sd + protsum1_ofTEI_3MAD_norm_sd + fibesum1_ofTEI_3MAD_norm_sd + NATRsum1_ofTEI_3MAD_norm_sd, family = gaussian(link = "identity"))

model_selection <- glmulti(full_model, level = 1, crit="aicc", confsetsize=1024)

summary(model_selection)

#best model is all 
#$bestic
#[1] 107752.9

weightable(model_selection)

# check partial model, exclude the individual fatts, leave only total fat, oterwise there is too much multicolinearity, exclude also carbs, since there is correlation with sugar
# fiber
partial_model<-glm(basic_residuals~fettsum1_ofTEI_3MAD_norm_sd + sacksum1_ofTEI_3MAD_norm_sd
				+ protsum1_ofTEI_3MAD_norm_sd + fibesum1_ofTEI_3MAD_norm_sd + NATRsum1_ofTEI_3MAD_norm_sd, family = gaussian(link = "identity"))

model_selection <- glmulti(partial_model, level = 1, crit="aicc")

summary(model_selection)
#best model is all 
#$bestic
#[1]  113040.8

weightable(model_selection)



#categorized

full_model<-glm(basic_residuals~POLYsum1_ofTEI_3MAD_categorized_g + MONOsum1_ofTEI_3MAD_categorized_g + mfetsum1_ofTEI_3MAD_categorized_g + fettsum1_ofTEI_3MAD_categorized_g + sacksum1_ofTEI_3MAD_categorized_g
				+ kolhsum1_ofTEI_3MAD_categorized_g + FA_ofTEI_3MAD_categorized_g + protsum1_ofTEI_3MAD_categorized_g + fibesum1_3MAD_categorized_g + NATRsum1_3MAD_categorized_g, family = gaussian(link = "identity"))

model_selection <- glmulti(full_model, level = 1, crit="aicc", confsetsize=1024)

summary(model_selection)
#best model is all 
#$bestic
#[1] 108416.6


weightable(model_selection)

# check partial model, exclude the individual fatts, leave only total fat, oterwise there is too much multicolinearity, exclude also carbs, since there is correlation with sugar
# fiber
partial_model<-glm(basic_residuals~fettsum1_ofTEI_3MAD_categorized_g + sacksum1_ofTEI_3MAD_categorized_g
				+ protsum1_ofTEI_3MAD_categorized_g + fibesum1_3MAD_categorized_g + NATRsum1_3MAD_categorized_g, family = gaussian(link = "identity"))

model_selection <- glmulti(partial_model, level = 1, crit="aicc")

summary(model_selection)
#best model is all 
#$bestic
#[1] 113535.3

weightable(model_selection)



#try DSA(local installation)
library(modelUtils)
library(DSA)



basic_model<-glm(bmi_norm_sd~age + agesq + gender_factor + year + ffq_factor, family = gaussian(link = "identity"))
basic_residuals<-bmi_norm_sd
basic_residuals[!is.na(bmi_norm_sd)]<-basic_model$residuals

DSA_model<-DSA(basic_residuals~POLYsum1_norm_sd + MONOsum1_norm_sd + mfetsum1_norm_sd + fettsum1_norm_sd + sacksum1_norm_sd
				+ kolhsum1_norm_sd + FA_norm_sd + protsum1_norm_sd + fibesum1_norm_sd + NATRsum1_norm_sd, data=VIP_data_independant, maxsumofpow = 1, maxsize=10, 
		maxorderint = 1, nsplits=1,vfold=10)




#	
#	Model selected:
#			basic_residuals ~ I(POLYsum1_norm_sd^1) + I(MONOsum1_norm_sd^1) + 
#			I(mfetsum1_norm_sd^1) + I(fettsum1_norm_sd^1) + I(sacksum1_norm_sd^1) + 
#			I(kolhsum1_norm_sd^1) + I(FA_norm_sd^1) + I(protsum1_norm_sd^1) + 
#			I(fibesum1_norm_sd^1) + I(NATRsum1_norm_sd^1)
#	<environment: 0x149453e0>
#			
#			(Intercept) I(POLYsum1_norm_sd^1) I(MONOsum1_norm_sd^1) 
#	-0.001701270           0.141666430           0.050531330 
#	I(mfetsum1_norm_sd^1) I(fettsum1_norm_sd^1) I(sacksum1_norm_sd^1) 
#	-0.410973371           0.332255511          -0.001799162 
#	I(kolhsum1_norm_sd^1)       I(FA_norm_sd^1) I(protsum1_norm_sd^1) 
#	-0.081239404          -0.236229501           0.135928048 
#	I(fibesum1_norm_sd^1) I(NATRsum1_norm_sd^1) 
#	-0.034950315           0.064749450 
#	
#	Base model: basic_residuals ~ POLYsum1_norm_sd + MONOsum1_norm_sd + mfetsum1_norm_sd + 
#			fettsum1_norm_sd + sacksum1_norm_sd + kolhsum1_norm_sd + 
#			FA_norm_sd + protsum1_norm_sd + fibesum1_norm_sd + NATRsum1_norm_sd
#	Family: gaussian
#	Total number of experimental units: 47107
#	Number of experimental units used to fit the model selected: 45652
#	Total number of observations: 47107
#	Number of observations used to fit the model selected: 45652
#	
#	Dimension reduction: no
#	Number of candidate variables considered: 15103
#	Minimum average cross-validated risk for model size 11, interaction order 1: 0.945824
#	Average cross-validated risk for the model selected: 0.9459571
#	
#	

# try partDSA to see the partion of the covariate space, (doesnt seem to handle NA in the dependant variable)

library(partDSA)

#original

#continuous
basic_model<-glm(bmi_norm_sd~age + agesq + gender_factor + year + ffq_factor, family = gaussian(link = "identity"))
basic_residuals<-basic_model$residuals

variables_data_frame<-data.frame(POLYsum1_norm_sd[!is.na(bmi_norm_sd)],MONOsum1_norm_sd[!is.na(bmi_norm_sd)],mfetsum1_norm_sd[!is.na(bmi_norm_sd)],
		fettsum1_norm_sd[!is.na(bmi_norm_sd)],sacksum1_norm_sd[!is.na(bmi_norm_sd)],kolhsum1_norm_sd[!is.na(bmi_norm_sd)],FA_norm_sd[!is.na(bmi_norm_sd)],protsum1_norm_sd[!is.na(bmi_norm_sd)],
		fibesum1_norm_sd[!is.na(bmi_norm_sd)],NATRsum1_norm_sd[!is.na(bmi_norm_sd)])

colnames(variables_data_frame)<-c("POLYsum1_norm_sd","MONOsum1_norm_sd","mfetsum1_norm_sd","fettsum1_norm_sd","sacksum1_norm_sd",
		"kolhsum1_norm_sd","FA_norm_sd","protsum1_norm_sd","fibesum1_norm_sd","NATRsum1_norm_sd")


partDSA_model<-partDSA(x=variables_data_frame,y=basic_residuals,control=DSA.control(missing="impute.at.split", loss.function="L2"))




#check for sugar, meat, fat etc food...


#foods with added sugar
#long ffq: da14 coffe buns rusk, da28 cornflakes, da48 pancakes vaffles, da65 icecream, da66 sweets and chocolate, da67 sugar honey, da 68 jam, da69 cookies, da74 soft drinks, da75 sodas, da 76 juice
#short ffq: dat13 coffe buns rusk, dat23 cornflakes, DAT35  pancakes vaffles, dat48 icecream, dat49 sweets and chocolate, DAT50 sugar honey jam, dat51 cookies, dat 56 soft drinks sodas juice

#rcorr(cbind(bmi,dat49,DAT50,dat51,dat56,da66,da67,da68,da69,da74,da75,dat24,DAT25,dat26,da29, da30,da31,da32,dat27,dat28,DAT29,da33,da34,da35,da36,da37,da38))

VIP_data_independant$sugar_food[ffq==0]<-apply(cbind(dat13,dat23,DAT35,dat48,dat49,DAT50,dat51,dat56),1,FUN=sum,na.rm=TRUE)[ffq==0]
VIP_data_independant$sugar_food[ffq==1]<-apply(cbind(da14,da28,da48,da65,da66,da67,da68,da69,da74,da75,da76),1,FUN=sum,na.rm=TRUE)[ffq==1]
VIP_data_independant$sugar_food_norm_sd[!is.na(VIP_data_independant$sugar_food)]<-log(VIP_data_independant$sugar_food[!is.na(VIP_data_independant$sugar_food)]+0.0001)
VIP_data_independant$sugar_food_norm_sd[!is.na(VIP_data_independant$sugar_food_norm_sd)]<-(VIP_data_independant$sugar_food_norm_sd[!is.na(VIP_data_independant$sugar_food_norm_sd)]-mean(VIP_data_independant$sugar_food_norm_sd[!is.na(VIP_data_independant$sugar_food_norm_sd)]))/
		(sd(VIP_data_independant$sugar_food_norm_sd[!is.na(VIP_data_independant$sugar_food_norm_sd)]))



VIP_data_independant$sugar_food2[ffq==0]<-apply(cbind(dat49, dat56),1,FUN=sum,na.rm=TRUE)[ffq==0]
VIP_data_independant$sugar_food2[ffq==1]<-apply(cbind(da66, da75),1,FUN=sum,na.rm=TRUE)[ffq==1]
VIP_data_independant$sugar_food2_norm_sd[!is.na(VIP_data_independant$sugar_food2)]<-log(VIP_data_independant$sugar_food2[!is.na(VIP_data_independant$sugar_food2)]+0.0001)
VIP_data_independant$sugar_food2_norm_sd[!is.na(VIP_data_independant$sugar_food2_norm_sd)]<-(VIP_data_independant$sugar_food2_norm_sd[!is.na(VIP_data_independant$sugar_food2_norm_sd)]-mean(VIP_data_independant$sugar_food2_norm_sd[!is.na(VIP_data_independant$sugar_food2_norm_sd)]))/
		(sd(VIP_data_independant$sugar_food2_norm_sd[!is.na(VIP_data_independant$sugar_food2_norm_sd)]))



VIP_data_independant$fruit[ffq==0]<-apply(cbind(dat24,DAT25,dat26),1,FUN=sum,na.rm=TRUE)[ffq==0]
VIP_data_independant$fruit[ffq==1]<-apply(cbind(da29, da30,da31,da32),1,FUN=sum,na.rm=TRUE)[ffq==1]
VIP_data_independant$fruit_norm_sd[!is.na(VIP_data_independant$fruit)]<-log(VIP_data_independant$fruit[!is.na(VIP_data_independant$fruit)]+0.0001)
VIP_data_independant$fruit_norm_sd[!is.na(VIP_data_independant$fruit)]<-(VIP_data_independant$fruit_norm_sd-mean(VIP_data_independant$fruit_norm_sd))/
		(sd(VIP_data_independant$fruit_norm_sd))



VIP_data_independant$vegetables[ffq==0]<-apply(cbind(dat27,dat28,DAT29,dat34),1,FUN=sum,na.rm=TRUE)[ffq==0]
VIP_data_independant$vegetables[ffq==1]<-apply(cbind(da33,da34,da35,da36,da37,da38, da46),1,FUN=sum,na.rm=TRUE)[ffq==1]
VIP_data_independant$vegetables_norm_sd[!is.na(VIP_data_independant$vegetables)]<-log(VIP_data_independant$vegetables[!is.na(VIP_data_independant$vegetables)]+0.0001)
VIP_data_independant$vegetables_norm_sd[!is.na(VIP_data_independant$vegetables)]<-(VIP_data_independant$vegetables_norm_sd-mean(VIP_data_independant$vegetables_norm_sd))/
		(sd(VIP_data_independant$vegetables_norm_sd))



detach(VIP_data_independant)
attach(VIP_data_independant)

#check R² and regression coefficent for original bmi(without outliers, not log transformed and standardized)
associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + sugar_food, family = gaussian(link = "identity"))

summary(associations)
vif(associations)


associations<-glm(bmi_norm_sd~age + agesq + gender_factor + year + ffq_factor + sugar_food_norm_sd, family = gaussian(link = "identity"))

summary(associations)
vif(associations)


#r²

partial_corr<-pcor.test(bmi[!is.na(bmi) & !is.na(sugar_food)],sugar_food[!is.na(bmi) & !is.na(sugar_food)],
		VIP_data_independant[!is.na(bmi) & !is.na(sugar_food),c("age","agesq","year","ffq","gender")])

partial_corr[[1]]	

round(partial_corr[[1]]*partial_corr[[1]],4)

partial_corr[[2]]



associations<-glm(bmi_norm_sd~age + agesq + gender_factor + year + ffq_factor + sugar_food_norm_sd + fruit_norm_sd + vegetables_norm_sd, family = gaussian(link = "identity"))

summary(associations)
vif(associations)

associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + sugar_food + fruit + vegetables, family = gaussian(link = "identity"))

summary(associations)
vif(associations)


#r²

partial_corr<-pcor.test(bmi[!is.na(bmi) & !is.na(sugar_food) & !is.na(fruit) & !is.na(vegetables)],sugar_food[!is.na(bmi) & !is.na(sugar_food) & !is.na(fruit) & !is.na(vegetables)],
		VIP_data_independant[!is.na(bmi) & !is.na(sugar_food) & !is.na(fruit) & !is.na(vegetables),c("age","agesq","year","ffq","gender","fruit","vegetables")])

partial_corr[[1]]	

round(partial_corr[[1]]*partial_corr[[1]],4)

partial_corr[[2]]




#2
#check R² and regression coefficent for original bmi(without outliers, not log transformed and standardized)
associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + sugar_food2, family = gaussian(link = "identity"))

summary(associations)
vif(associations)


associations<-glm(bmi_norm_sd~age + agesq + gender_factor + year + ffq_factor + sugar_food2_norm_sd, family = gaussian(link = "identity"))

summary(associations)
vif(associations)


#r²

partial_corr<-pcor.test(bmi[!is.na(bmi) & !is.na(sugar_food2)],sugar_food2[!is.na(bmi) & !is.na(sugar_food2)],
		VIP_data_independant[!is.na(bmi) & !is.na(sugar_food2),c("age","agesq","year","ffq","gender")])

partial_corr[[1]]	

round(partial_corr[[1]]*partial_corr[[1]],4)

partial_corr[[2]]



associations<-glm(bmi_norm_sd~age + agesq + gender_factor + year + ffq_factor + sugar_food2_norm_sd + fruit_norm_sd + vegetables_norm_sd, family = gaussian(link = "identity"))

summary(associations)
vif(associations)

associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + sugar_food2 + fruit + vegetables, family = gaussian(link = "identity"))

summary(associations)
vif(associations)


#r²

partial_corr<-pcor.test(bmi[!is.na(bmi) & !is.na(sugar_food2) & !is.na(fruit) & !is.na(vegetables)],sugar_food2[!is.na(bmi) & !is.na(sugar_food2) & !is.na(fruit) & !is.na(vegetables)],
		VIP_data_independant[!is.na(bmi) & !is.na(sugar_food2) & !is.na(fruit) & !is.na(vegetables),c("age","agesq","year","ffq","gender","fruit","vegetables")])

partial_corr[[1]]	

round(partial_corr[[1]]*partial_corr[[1]],4)

partial_corr[[2]]


#------------------------------INDEPENDANT DATA--------------------------------------------------------------------


