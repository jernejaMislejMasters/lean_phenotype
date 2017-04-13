library(ppcor)
library(Hmisc)
library(car)
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

# run separate regression to get the association effect sizes for bmi, check they have the same direction and use the ones from the "independant" dataset to multiply the scores
# for the subset

#------------------------------INDEPENDANT DATA--------------------------------------------------------------------

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
				+ kolhsum1_ofTEI_3MAD_categorized_g + FA_ofTEI_3MAD_categorized_g + protsum1_ofTEI_3MAD_categorized_g + fibesum1_categorized_g + NATRsum1_categorized_g, family = gaussian(link = "identity"))

summary(associations)
vif(associations)

all_variables<-c("POLYsum1_ofTEI_3MAD_categorized_g","MONOsum1_ofTEI_3MAD_categorized_g","mfetsum1_ofTEI_3MAD_categorized_g","fettsum1_ofTEI_3MAD_categorized_g","sacksum1_ofTEI_3MAD_categorized_g",
		"kolhsum1_ofTEI_3MAD_categorized_g","FA_ofTEI_3MAD_categorized_g","protsum1_ofTEI_3MAD_categorized_g","fibesum1_categorized_g","NATRsum1_categorized_g")

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






#------------------------------INDEPENDANT DATA--------------------------------------------------------------------


