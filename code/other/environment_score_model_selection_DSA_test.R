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



model_selection_macronutrients_d<-DSA(basic_residuals_bmi~POLYsum1_TEI_adjusted_norm_sd+MONOsum1_TEI_adjusted_norm_sd, data=VIP_data_independant, maxsumofpow = 1, maxsize=2, 
maxorderint = 1, nsplits=2,vfold=10)

capture.output(summary(model_selection_macronutrients_d), file = "model_selection_macronutrients_d_test")

