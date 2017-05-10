attach(VIP_data_subset_visit1)

#create factor for the questionare
VIP_data_subset_visit1$ffq<-0
VIP_data_subset_visit1$ffq[enkver2=="long"]<-1
VIP_data_subset_visit1$ffq[enkver2=="apri"]<-1
VIP_data_subset_visit1$ffq_factor<-as.factor(VIP_data_subset_visit1$ffq)

#age squared
VIP_data_subset_visit1$agesq<-age*age

#factorize gender
VIP_data_subset_visit1$gender_factor<-as.factor(VIP_data_subset_visit1$gender)

#make a log transformed bmi, making sure it is really normal then standardize it
VIP_data_subset_visit1$bmi_norm<-log(bmi)
VIP_data_subset_visit1$bmi_norm_sd[!is.na(bmi)]<-(VIP_data_subset_visit1$bmi_norm[!is.na(bmi)] - mean(VIP_data_subset_visit1$bmi_norm[!is.na(bmi)]))/(sd(VIP_data_subset_visit1$bmi_norm[!is.na(bmi)]))

#obtain the residuals from the basic covariates
basic_model<-glm(VIP_data_subset_visit1$bmi_norm_sd~age + VIP_data_subset_visit1$agesq + VIP_data_subset_visit1$gender_factor + year + VIP_data_subset_visit1$ffq_factor, family = gaussian(link = "identity"))
VIP_data_subset_visit1$basic_residuals_bmi<-VIP_data_subset_visit1$bmi_norm_sd
VIP_data_subset_visit1$basic_residuals_bmi[!is.na(VIP_data_subset_visit1$bmi_norm_sd)]<-basic_model$residuals

# create several new "transformed" variables, either just log transformed and standaridized or adjusted for TEI, by obtaining the
# residuals

macronutrients<-c("POLYsum1","MONOsum1","mfetsum1","fettsum1","sacksum1","kolhsum1","FA","protsum1","protsum1_anim","protsum1_veg",
		"fibesum1","DISAsum1","MOSAsum1","TRANSsum1")

micronutrients<-c("NATRsum1","MAGNsum1","FOSFsum1","selesum1","ZINCsum1","retisum1","karosum1","TIAMsum1","Folasum1","B2sum1","NIACsum1",
		"B6sum1","B12sum1","askosum1","Dsum1","tokosum1","VITKsum1","jernsum1","JODIsum1","kalcsum1","KALIsum1")

other<-c("FULLKsum1","kolesum1","alkosum1")

VIP_data_subset_visit1$FA<-FA183_sum1 + FA205_sum1 + FA226_sum1 + FA182_sum1 + FA204_sum1 #essential fatty acids
# add little to ensure no zeros
VIP_data_subset_visit1[,c(macronutrients, micronutrients, other)]<-VIP_data_subset_visit1[,c(macronutrients, micronutrients, other)]+0.000001

#standardize ensum1
VIP_data_subset_visit1$ensum1_norm_sd<-log(ensum1)
VIP_data_subset_visit1$ensum1_norm_sd[!is.na(ensum1)]<-(VIP_data_subset_visit1$ensum1_norm_sd[!is.na(ensum1)]-mean(VIP_data_subset_visit1$ensum1_norm_sd[!is.na(ensum1)]))/
		(sd(VIP_data_subset_visit1$ensum1_norm_sd[!is.na(ensum1)]))

detach(VIP_data_subset_visit1)
attach(VIP_data_subset_visit1)


for (nutrient in c(macronutrients, micronutrients, other)){
	
	#original
	#continuous log transformed and standardized
	transformed_nutrient<-log(VIP_data_subset_visit1[,c(nutrient)])
	
	transformed_nutrient[!is.na(transformed_nutrient)]<-(transformed_nutrient[!is.na(transformed_nutrient)] -
				mean(transformed_nutrient[!is.na(transformed_nutrient)]))/(sd(transformed_nutrient[!is.na(transformed_nutrient)]))
	
	
	VIP_data_subset_visit1<-cbind(VIP_data_subset_visit1,transformed_nutrient)
	colnames(VIP_data_subset_visit1)[length(colnames(VIP_data_subset_visit1))]<-paste0(nutrient,"_norm_sd")
	
	
	# adjusted for TEI
	nutrient_residuals_of_TEI<-transformed_nutrient+ensum1	
	nutrient_residuals_of_TEI[!is.na(transformed_nutrient) & !is.na(ensum1_norm_sd)]<-lm(transformed_nutrient~ensum1_norm_sd)$residuals
	
	VIP_data_subset_visit1<-cbind(VIP_data_subset_visit1,nutrient_residuals_of_TEI)
	colnames(VIP_data_subset_visit1)[length(colnames(VIP_data_subset_visit1))]<-paste0(nutrient,"_TEI_adjusted_norm_sd")
	
}

detach(VIP_data_subset_visit1)
