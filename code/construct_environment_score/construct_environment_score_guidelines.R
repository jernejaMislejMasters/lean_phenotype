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

source(file="Code/load_indp_variables.R")

#run several different regressions 

# check all the paired correlations for filtered bmi4 and filtered nutrients
rcorr( cbind(bmi_4SD,POLYsum1_transformed_4SD,MONOsum1_transformed_4SD,mfetsum1_transformed_4SD,fettsum1_transformed_4SD,sacksum1_transformed_4SD,kolhsum1_transformed_4SD,FA_transformed_4SD,protsum1_transformed_4SD,fibesum1_transformed_4SD,NATRsum1_transformed_4SD),type="pearson")

# check all the paired correlations for filtered bmi4 and filtered nutrients expressed in % of TEI
rcorr( cbind(bmi_4SD,POLYsum1_ofTEI_transformed,MONOsum1_ofTEI_transformed,mfetsum1_ofTEI_transformed,fettsum1_ofTEI_transformed,sacksum1_ofTEI_transformed,kolhsum1_ofTEI_transformed,FA_ofTEI_transformed,protsum1_ofTEI_transformed,fibesum1_ofTEI_transformed,NATRsum1_ofTEI_transformed),type="pearson")

# check all the paired correlations for filtered bmi4 and filtered nutrients expressed in % of TEI and categorized
rcorr( cbind(bmi_4SD,POLYsum1_ofTEI_categorized_g,MONOsum1_ofTEI_categorized_g,mfetsum1_ofTEI_categorized_g,fettsum1_ofTEI_categorized_g,sacksum1_ofTEI_categorized_g,kolhsum1_ofTEI_categorized_g,FA_ofTEI_categorized_g,protsum1_ofTEI_categorized_g,fibesum1_categorized_g,NATRsum1_categorized_g),type="pearson")



#for raw continous, fitted together

#filtered bmi and other variables with cutpoint 4SD and log transformed with other variables being standardized also (BMI AND NUTRIENTS)
associations<-glm(bmi_4SD_sd~age + agesq + gender_factor + year + ffq_factor + POLYsum1_transformed_4SD + MONOsum1_transformed_4SD + mfetsum1_transformed_4SD + fettsum1_transformed_4SD + sacksum1_transformed_4SD
				+ kolhsum1_transformed_4SD + FA_transformed_4SD + protsum1_transformed_4SD + fibesum1_transformed_4SD + NATRsum1_transformed_4SD, family = gaussian(link = "identity"))

summary(associations)
vif(associations)

#r² filtered bmi and other variables with cutpoint 4SD and log transformed with other variables being standardized also (BMI AND NUTRIENTS)
all_variables<-c("POLYsum1_transformed_4SD","MONOsum1_transformed_4SD","mfetsum1_transformed_4SD","fettsum1_transformed_4SD","sacksum1_transformed_4SD",
		"kolhsum1_transformed_4SD","FA_transformed_4SD","protsum1_transformed_4SD","fibesum1_transformed_4SD","NATRsum1_transformed_4SD")

independant_no_missing<-na.omit(VIP_data_independant[,c("age","agesq","year","ffq","gender","bmi_4SD_sd",all_variables)])

variable_count<-1
for (variable in all_variables){
	
	partial_corr<-pcor.test(independant_no_missing$bmi_4SD_sd,independant_no_missing[,c(variable)],
			independant_no_missing[,c("age","agesq","year","ffq","gender",all_variables[-variable_count])])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
	variable_count<-variable_count+1
	
}


#for raw continous, fitted separately
for (variable in all_variables){

	associations<-glm(bmi_4SD_sd~age + agesq + gender_factor + year + ffq_factor + VIP_data_independant[,c(variable)], family = gaussian(link = "identity"))
	associations_summary<-summary(associations)

	message(variable)
	message(round(associations_summary$coefficients[[7]],7))
	message(round(associations_summary$coefficients[[28]],7))
	
}

#r²
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi_4SD_sd[(!is.na(bmi_4SD_sd) & !is.na(VIP_data_independant[,c(variable)]))],VIP_data_independant[(!is.na(bmi_4SD_sd) & !is.na(VIP_data_independant[,c(variable)])),c(variable)],
			VIP_data_independant[(!is.na(bmi_4SD_sd) & !is.na(VIP_data_independant[,c(variable)])),c("age","agesq","year","ffq","gender")])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
		
}





# for continuous expressed in % of TEI, fitted together
associations<-glm(bmi_4SD_sd~age + agesq + gender_factor + year + ffq_factor + POLYsum1_ofTEI_transformed + MONOsum1_ofTEI_transformed + mfetsum1_ofTEI_transformed + fettsum1_ofTEI_transformed + sacksum1_ofTEI_transformed
				+ kolhsum1_ofTEI_transformed + FA_ofTEI_transformed + protsum1_ofTEI_transformed + fibesum1_ofTEI_transformed + NATRsum1_ofTEI_transformed, family = gaussian(link = "identity"))

summary(associations)
vif(associations)

all_variables<-c("POLYsum1_ofTEI_transformed","MONOsum1_ofTEI_transformed","mfetsum1_ofTEI_transformed","fettsum1_ofTEI_transformed","sacksum1_ofTEI_transformed",
		"kolhsum1_ofTEI_transformed","FA_ofTEI_transformed","protsum1_ofTEI_transformed","fibesum1_ofTEI_transformed","NATRsum1_ofTEI_transformed")

independant_no_missing<-na.omit(VIP_data_independant[,c("age","agesq","year","ffq","gender","bmi_4SD_sd",all_variables)])

#r²
variable_count<-1
for (variable in all_variables){
	
	partial_corr<-pcor.test(independant_no_missing$bmi_4SD_sd,independant_no_missing[,c(variable)],
			independant_no_missing[,c("age","agesq","year","ffq","gender",all_variables[-variable_count])])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
	variable_count<-variable_count+1
	
}

# for continuous expressed in % of TEI, fitted separately
for (variable in all_variables){
	
	associations<-glm(bmi_4SD_sd~age + agesq + gender_factor + year + ffq_factor + VIP_data_independant[,c(variable)], family = gaussian(link = "identity"))
	associations_summary<-summary(associations)
	
	message(variable)
	message(round(associations_summary$coefficients[[7]],7))
	message(round(associations_summary$coefficients[[28]],7))
	
}

#r²
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi_4SD_sd[(!is.na(bmi_4SD_sd) & !is.na(VIP_data_independant[,c(variable)]))],VIP_data_independant[(!is.na(bmi_4SD_sd) & !is.na(VIP_data_independant[,c(variable)])),c(variable)],
			VIP_data_independant[(!is.na(bmi_4SD_sd) & !is.na(VIP_data_independant[,c(variable)])),c("age","agesq","year","ffq","gender")])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
}







# categorized based on guidelines, fitted together
associations<-glm(bmi_4SD_sd~age + agesq + gender_factor + year + ffq_factor + POLYsum1_ofTEI_categorized_g + MONOsum1_ofTEI_categorized_g + mfetsum1_ofTEI_categorized_g + fettsum1_ofTEI_categorized_g + sacksum1_ofTEI_categorized_g
				+ kolhsum1_ofTEI_categorized_g + FA_ofTEI_categorized_g + protsum1_ofTEI_categorized_g + fibesum1_categorized_g + NATRsum1_categorized_g, family = gaussian(link = "identity"))

summary(associations)
vif(associations)

all_variables<-c("POLYsum1_ofTEI_categorized_g","MONOsum1_ofTEI_categorized_g","mfetsum1_ofTEI_categorized_g","fettsum1_ofTEI_categorized_g","sacksum1_ofTEI_categorized_g",
		"kolhsum1_ofTEI_categorized_g","FA_ofTEI_categorized_g","protsum1_ofTEI_categorized_g","fibesum1_categorized_g","NATRsum1_categorized_g")

independant_no_missing<-na.omit(VIP_data_independant[,c("age","agesq","year","ffq","gender","bmi_4SD_sd",all_variables)])

#r²
variable_count<-1
for (variable in all_variables){
	
	partial_corr<-pcor.test(independant_no_missing$bmi_4SD_sd,independant_no_missing[,c(variable)],
			independant_no_missing[,c("age","agesq","year","ffq","gender",all_variables[-variable_count])])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
	variable_count<-variable_count+1
	
}


# categorized based on guidelines, fitted separately
for (variable in all_variables){
	
	associations<-glm(bmi_4SD_sd~age + agesq + gender_factor + year + ffq_factor + VIP_data_independant[,c(variable)], family = gaussian(link = "identity"))
	associations_summary<-summary(associations)
	
	message(variable)
	message(round(associations_summary$coefficients[[7]],7))
	message(round(associations_summary$coefficients[[28]],7))
	
}

#r²
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi_4SD_sd[(!is.na(bmi_4SD_sd) & !is.na(VIP_data_independant[,c(variable)]))],VIP_data_independant[(!is.na(bmi_4SD_sd) & !is.na(VIP_data_independant[,c(variable)])),c(variable)],
			VIP_data_independant[(!is.na(bmi_4SD_sd) & !is.na(VIP_data_independant[,c(variable)])),c("age","agesq","year","ffq","gender")])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
}

#try several models with sugar:
associations<-glm(bmi_4SD_sd~age + agesq + gender_factor + year + ffq_factor + sacksum1_transformed_4SD, family = gaussian(link = "identity"))

summary(associations)
vif(associations)

associations<-glm(bmi_4SD_sd~age + agesq + gender_factor + year + ffq_factor + kolhsum1_transformed_4SD, family = gaussian(link = "identity"))

summary(associations)
vif(associations)

associations<-glm(bmi_4SD_sd~age + agesq + gender_factor + year + ffq_factor + fibesum1_transformed_4SD, family = gaussian(link = "identity"))

summary(associations)
vif(associations)

associations<-glm(bmi_4SD_sd~age + agesq + gender_factor + year + ffq_factor + mfetsum1_transformed_4SD, family = gaussian(link = "identity"))

summary(associations)
vif(associations)


associations<-glm(bmi_4SD_sd~age + agesq + gender_factor + year + ffq_factor + fettsum1_transformed_4SD, family = gaussian(link = "identity"))

summary(associations)
vif(associations)


associations<-glm(bmi_4SD_sd~age + agesq + gender_factor + year + ffq_factor + sacksum1_transformed_4SD+ kolhsum1_transformed_4SD, family = gaussian(link = "identity"))

summary(associations)
vif(associations)

associations<-glm(bmi_4SD_sd~age + agesq + gender_factor + year + ffq_factor + sacksum1_transformed_4SD+ kolhsum1_transformed_4SD + sacksum1_transformed_4SD*kolhsum1_transformed_4SD, family = gaussian(link = "identity"))

summary(associations)
vif(associations)


associations<-glm(bmi_4SD_sd~age + agesq + gender_factor + year + ffq_factor + sacksum1_transformed_4SD+ fibesum1_transformed_4SD, family = gaussian(link = "identity"))

summary(associations)
vif(associations)

associations<-glm(bmi_4SD_sd~age + agesq + gender_factor + year + ffq_factor + sacksum1_transformed_4SD+ fibesum1_transformed_4SD + sacksum1_transformed_4SD*fibesum1_transformed_4SD, family = gaussian(link = "identity"))

summary(associations)
vif(associations)


associations<-glm(bmi_4SD_sd~age + agesq + gender_factor + year + ffq_factor + sacksum1_transformed_4SD+ mfetsum1_transformed_4SD, family = gaussian(link = "identity"))

summary(associations)
vif(associations)


associations<-glm(bmi_4SD_sd~age + agesq + gender_factor + year + ffq_factor + sacksum1_transformed_4SD+ fettsum1_transformed_4SD, family = gaussian(link = "identity"))

summary(associations)
vif(associations)



associations<-glm(bmi_4SD_sd~age + agesq + gender_factor + year + ffq_factor + sacksum1_transformed_4SD+ mfetsum1_transformed_4SD + sacksum1_transformed_4SD*mfetsum1_transformed_4SD, family = gaussian(link = "identity"))

summary(associations)
vif(associations)

associations<-glm(bmi_4SD_sd~age + agesq + gender_factor + year + ffq_factor + sacksum1_transformed_4SD+ fettsum1_transformed_4SD + sacksum1_transformed_4SD*fettsum1_transformed_4SD, family = gaussian(link = "identity"))

summary(associations)
vif(associations)


associations<-glm(bmi_4SD_sd~age + agesq + gender_factor + year + ffq_factor + sacksum1_transformed_4SD+ mfetsum1_transformed_4SD + fibesum1_transformed_4SD + kolhsum1_transformed_4SD, family = gaussian(link = "identity"))

summary(associations)
vif(associations)



associations<-glm(bmi_4SD_sd~age + agesq + gender_factor + year + ffq_factor + sacksum1_transformed_4SD+ fettsum1_transformed_4SD + fibesum1_transformed_4SD + kolhsum1_transformed_4SD, family = gaussian(link = "identity"))

summary(associations)
vif(associations)


associations<-glm(bmi_4SD_sd~age + agesq + gender_factor + year + ffq_factor + sacksum1_transformed_4SD+ mfetsum1_transformed_4SD+ fettsum1_transformed_4SD + fibesum1_transformed_4SD + kolhsum1_transformed_4SD, family = gaussian(link = "identity"))

summary(associations)
vif(associations)


associations<-glm(bmi_4SD_sd~age + agesq + gender_factor + year + ffq_factor + sacksum1_transformed_4SD + fibesum1_transformed_4SD + kolhsum1_transformed_4SD, family = gaussian(link = "identity"))

summary(associations)
vif(associations)


associations<-glm(bmi_4SD_sd~age + agesq + gender_factor + year + ffq_factor  + fibesum1_transformed_4SD + kolhsum1_transformed_4SD, family = gaussian(link = "identity"))

summary(associations)
vif(associations)



associations<-glm(sacksum1_transformed_4SD~age + agesq + gender_factor + year + ffq_factor + fettsum1_transformed_4SD  + kolhsum1_transformed_4SD, family = gaussian(link = "identity"))

summary(associations)
vif(associations)



associations<-glm(sacksum1_transformed_4SD~age + agesq + gender_factor + year + ffq_factor + fettsum1_transformed_4SD  + fibesum1_transformed_4SD, family = gaussian(link = "identity"))

summary(associations)
vif(associations)


associations<-glm(sacksum1_transformed_4SD~age + agesq + gender_factor + year + ffq_factor  + kolhsum1_transformed_4SD, family = gaussian(link = "identity"))

summary(associations)
vif(associations)


associations<-glm(sacksum1_transformed_4SD~age + agesq + gender_factor + year + ffq_factor + fettsum1_transformed_4SD , family = gaussian(link = "identity"))

summary(associations)
vif(associations)


associations<-glm(sacksum1_transformed_4SD~age + agesq + gender_factor + year + ffq_factor  + fibesum1_transformed_4SD, family = gaussian(link = "identity"))

summary(associations)
vif(associations)


associations<-glm(sacksum1_transformed_4SD~age + agesq + gender_factor + year + ffq_factor + mfetsum1_transformed_4SD , family = gaussian(link = "identity"))

summary(associations)
vif(associations)



associations<-glm(sacksum1_transformed_4SD~age + agesq + gender_factor + year + ffq_factor + kolhsum1_transformed_4SD  + fibesum1_transformed_4SD, family = gaussian(link = "identity"))

summary(associations)
vif(associations)



associations<-glm(sacksum1_transformed_4SD~age + agesq + gender_factor + year + ffq_factor + kolhsum1_transformed_4SD  + fibesum1_transformed_4SD + fettsum1_transformed_4SD, family = gaussian(link = "identity"))

summary(associations)
vif(associations)


associations<-glm(sacksum1_transformed_4SD~age + agesq + gender_factor + year + ffq_factor  + kolhsum1_transformed_4SD + fettsum1_transformed_4SD, family = gaussian(link = "identity"))

summary(associations)
vif(associations)


all_variables<-c("sacksum1_transformed_4SD","kolhsum1_transformed_4SD","fibesum1_transformed_4SD")

independant_no_missing<-na.omit(VIP_data_independant[,c("age","agesq","year","ffq","gender","bmi_4SD_sd",all_variables)])

pcor.test(independant_no_missing$bmi_4SD_sd,independant_no_missing[,c("sacksum1_transformed_4SD")],independant_no_missing[,c("age","agesq","year","ffq","gender")])
pcor.test(independant_no_missing$bmi_4SD_sd,independant_no_missing[,c("sacksum1_transformed_4SD")],independant_no_missing[,c("age","agesq","year","ffq","gender","kolhsum1_transformed_4SD","fibesum1_transformed_4SD")])
pcor.test(independant_no_missing$bmi_4SD_sd,independant_no_missing[,c("sacksum1_transformed_4SD")],independant_no_missing[,c("age","agesq","year","ffq","gender","kolhsum1_transformed_4SD")])
pcor.test(independant_no_missing$bmi_4SD_sd,independant_no_missing[,c("sacksum1_transformed_4SD")],independant_no_missing[,c("age","agesq","year","ffq","gender","fibesum1_transformed_4SD")])

pcor.test(independant_no_missing$bmi_4SD_sd,independant_no_missing[,c("fibesum1_transformed_4SD")],independant_no_missing[,c("age","agesq","year","ffq","gender")])
pcor.test(independant_no_missing$bmi_4SD_sd,independant_no_missing[,c("fibesum1_transformed_4SD")],independant_no_missing[,c("age","agesq","year","ffq","gender","kolhsum1_transformed_4SD","sacksum1_transformed_4SD")])
pcor.test(independant_no_missing$bmi_4SD_sd,independant_no_missing[,c("fibesum1_transformed_4SD")],independant_no_missing[,c("age","agesq","year","ffq","gender","kolhsum1_transformed_4SD")])
pcor.test(independant_no_missing$bmi_4SD_sd,independant_no_missing[,c("fibesum1_transformed_4SD")],independant_no_missing[,c("age","agesq","year","ffq","gender","sacksum1_transformed_4SD")])

pcor.test(independant_no_missing$bmi_4SD_sd,independant_no_missing[,c("kolhsum1_transformed_4SD")],independant_no_missing[,c("age","agesq","year","ffq","gender")])
pcor.test(independant_no_missing$bmi_4SD_sd,independant_no_missing[,c("kolhsum1_transformed_4SD")],independant_no_missing[,c("age","agesq","year","ffq","gender","sacksum1_transformed_4SD","fibesum1_transformed_4SD")])
pcor.test(independant_no_missing$bmi_4SD_sd,independant_no_missing[,c("kolhsum1_transformed_4SD")],independant_no_missing[,c("age","agesq","year","ffq","gender","sacksum1_transformed_4SD")])
pcor.test(independant_no_missing$bmi_4SD_sd,independant_no_missing[,c("kolhsum1_transformed_4SD")],independant_no_missing[,c("age","agesq","year","ffq","gender","fibesum1_transformed_4SD")])

#fit sugar against bmi in stratified carbs

associations<-glm(bmi_4SD_sd[!is.na(kolhsum1_ofTEI_categorized_g) & kolhsum1_ofTEI_categorized_g==0]~age[!is.na(kolhsum1_ofTEI_categorized_g) & kolhsum1_ofTEI_categorized_g==0] + agesq[!is.na(kolhsum1_ofTEI_categorized_g) & kolhsum1_ofTEI_categorized_g==0] + gender_factor[!is.na(kolhsum1_ofTEI_categorized_g) & kolhsum1_ofTEI_categorized_g==0] + year[!is.na(kolhsum1_ofTEI_categorized_g) & kolhsum1_ofTEI_categorized_g==0] + ffq_factor[!is.na(kolhsum1_ofTEI_categorized_g) & kolhsum1_ofTEI_categorized_g==0] + sacksum1_transformed_4SD[!is.na(kolhsum1_ofTEI_categorized_g) & kolhsum1_ofTEI_categorized_g==0], family = gaussian(link = "identity"))

summary(associations)
vif(associations)



associations<-glm(bmi_4SD_sd[!is.na(kolhsum1_ofTEI_categorized_g) & kolhsum1_ofTEI_categorized_g==1]~age[!is.na(kolhsum1_ofTEI_categorized_g) & kolhsum1_ofTEI_categorized_g==1] + agesq[!is.na(kolhsum1_ofTEI_categorized_g) & kolhsum1_ofTEI_categorized_g==1] + gender_factor[!is.na(kolhsum1_ofTEI_categorized_g) & kolhsum1_ofTEI_categorized_g==1] + year[!is.na(kolhsum1_ofTEI_categorized_g) & kolhsum1_ofTEI_categorized_g==1] + ffq_factor[!is.na(kolhsum1_ofTEI_categorized_g) & kolhsum1_ofTEI_categorized_g==1] + sacksum1_transformed_4SD[!is.na(kolhsum1_ofTEI_categorized_g) & kolhsum1_ofTEI_categorized_g==1], family = gaussian(link = "identity"))

summary(associations)
vif(associations)


associations<-glm(bmi_4SD_sd[!is.na(kolhsum1_ofTEI_categorized_g) & kolhsum1_ofTEI_categorized_g==2]~age[!is.na(kolhsum1_ofTEI_categorized_g) & kolhsum1_ofTEI_categorized_g==2] + agesq[!is.na(kolhsum1_ofTEI_categorized_g) & kolhsum1_ofTEI_categorized_g==2] + gender_factor[!is.na(kolhsum1_ofTEI_categorized_g) & kolhsum1_ofTEI_categorized_g==2] + year[!is.na(kolhsum1_ofTEI_categorized_g) & kolhsum1_ofTEI_categorized_g==2] + ffq_factor[!is.na(kolhsum1_ofTEI_categorized_g) & kolhsum1_ofTEI_categorized_g==2] + sacksum1_transformed_4SD[!is.na(kolhsum1_ofTEI_categorized_g) & kolhsum1_ofTEI_categorized_g==2], family = gaussian(link = "identity"))

summary(associations)
vif(associations)




#------------------------------INDEPENDANT DATA--------------------------------------------------------------------


