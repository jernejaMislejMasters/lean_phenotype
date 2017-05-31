library(glmnet)
library(pROC)
library(nnet)
library(stargazer)

#load entire cleaned data (154009 subjects)
VIP_data_all <- read.csv("../../VIP_data/VIP_170206_cleaned.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)

#load the subset
VIP_data_subset <- read.csv("../../VIP_data/VIP_170206_cleaned_subset.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)
attach(VIP_data_subset)
#extract only swedish....66228 (33114 for each visit)
#get those that have the ursprungsland in both visits and the value is 1 for Swedish
VIP_data_subset<-VIP_data_subset[!is.na(ursprungsland[visit==1]) & !is.na(ursprungsland[visit==2]),]
VIP_data_subset<-VIP_data_subset[VIP_data_subset$ursprungsland==1,]
VIP_data_subset_visit1<-VIP_data_subset[VIP_data_subset$visit==1,]
VIP_data_subset_visit2<-VIP_data_subset[VIP_data_subset$visit==2,]
detach(VIP_data_subset)

#create the independant dataset of the first visit that is not in the subset already with Swedish only
attach(VIP_data_all)
VIP_data_independant<-VIP_data_all[!is.na(besok1) & besok1==1 & !(Subject_id %in% VIP_data_subset$Subject_id) & !is.na(ursprungsland) & ursprungsland==1, ]
detach(VIP_data_all)
#length(VIP_data_independant[,1])#....47107

#select the best alpha, compare the prediction results for the selected variables with the results when using regression coefficients from the models fitted separetly for each selected nutrient
#or the regression coefficients from the model fitted together for all selected nutrients, non significant nutrients get regression coefficient 0

#load independent variables
source(file="../load_variables_independent_dataset_TEI_adjusted.R")


#take only complete cases....45652(44582 with alkosum1 and utbild and g6)

VIP_data_independant_complete_cases <- na.omit(VIP_data_independant[,c("basic_residuals_bmi", "POLYsum1_TEI_adjusted_norm_sd","MONOsum1_TEI_adjusted_norm_sd","mfetsum1_TEI_adjusted_norm_sd",
						"fettsum1_TEI_adjusted_norm_sd","sacksum1_TEI_adjusted_norm_sd","kolhsum1_TEI_adjusted_norm_sd","FA_TEI_adjusted_norm_sd",
						"protsum1_anim_TEI_adjusted_norm_sd","protsum1_veg_TEI_adjusted_norm_sd",
						"fibesum1_TEI_adjusted_norm_sd","DISAsum1_TEI_adjusted_norm_sd","MOSAsum1_TEI_adjusted_norm_sd","TRANSsum1_TEI_adjusted_norm_sd",
						"NATRsum1_TEI_adjusted_norm_sd","kolesum1_TEI_adjusted_norm_sd","ensum1_norm_sd", "MAGNsum1_TEI_adjusted_norm_sd","FOSFsum1_TEI_adjusted_norm_sd",
						"selesum1_TEI_adjusted_norm_sd","ZINCsum1_TEI_adjusted_norm_sd","retisum1_TEI_adjusted_norm_sd",
						"karosum1_TEI_adjusted_norm_sd","TIAMsum1_TEI_adjusted_norm_sd","Folasum1_TEI_adjusted_norm_sd",
						"B2sum1_TEI_adjusted_norm_sd","NIACsum1_TEI_adjusted_norm_sd","B6sum1_TEI_adjusted_norm_sd","B12sum1_TEI_adjusted_norm_sd",
						"askosum1_TEI_adjusted_norm_sd","Dsum1_TEI_adjusted_norm_sd","tokosum1_TEI_adjusted_norm_sd",
						"VITKsum1_TEI_adjusted_norm_sd","jernsum1_TEI_adjusted_norm_sd","JODIsum1_TEI_adjusted_norm_sd",
						"kalcsum1_TEI_adjusted_norm_sd","KALIsum1_TEI_adjusted_norm_sd")])

#divide in predictors and dependent
predictors <- as.matrix(VIP_data_independant_complete_cases[,-1])
dependent <- VIP_data_independant_complete_cases$basic_residuals_bmi

#selected variables based on elastic net, try different alphas with 100 repetitions and pick the best fit:

# !takes time, results are already available, so code below is in a comment:
regression_coefficients<-scan("regression_coefficients")
regression_coefficients<-matrix(regression_coefficients,11,byrow=F)

#set vector for coefficients
#regression_coefficients<-c()#36x100
#repetition<-100
#repetition_half<-repetition/2
#for (set_alpha in 0:10) {
#	
#	replicate_reg_coeff<-c()
#	
#	#repeat 100 times and save the reg coef
#	for(it in 1:repetition){
#		
#		model_macro_micronutrients_elastic_net <- cv.glmnet(predictors,dependent,alpha= set_alpha/10, family="gaussian")
#		
#		replicate_reg_coeff<-rbind(replicate_reg_coeff,coef(model_macro_micronutrients_elastic_net)[-1])
#		
#	}
#	
#	#set zero to those that are selected less then half of repetition times
##	replicate_reg_coeff<-apply(replicate_reg_coeff,2,function(x){
##				if(length(x[x==0])>=repetition_half){
##					return(rep(0,times=length(x)))
##				}else{
##					return(x)	
##				}
##			})
#	
#	#take mean of 100 values for all 36 coefficients
#	regression_coefficients<-rbind(regression_coefficients,apply(replicate_reg_coeff,2,FUN=mean))
#}

#for simulation, when waiting for the real coefficients
#regression_coefficients<-matrix(rexp(10*36), 10)


#load visit1 variables
source(file="../load_variables_visit1_dataset_TEI_adjusted.R")

#take only complete cases
VIP_data_subset_visit1_complete_cases <- na.omit(VIP_data_subset_visit1[,c("Subject_id","bmi","bmi_norm_sd","age","agesq","gender_factor","year","ffq_factor",
						"basic_residuals_bmi", "POLYsum1_TEI_adjusted_norm_sd","MONOsum1_TEI_adjusted_norm_sd","mfetsum1_TEI_adjusted_norm_sd",
						"fettsum1_TEI_adjusted_norm_sd","sacksum1_TEI_adjusted_norm_sd","kolhsum1_TEI_adjusted_norm_sd","FA_TEI_adjusted_norm_sd",
						"protsum1_anim_TEI_adjusted_norm_sd","protsum1_veg_TEI_adjusted_norm_sd",
						"fibesum1_TEI_adjusted_norm_sd","DISAsum1_TEI_adjusted_norm_sd","MOSAsum1_TEI_adjusted_norm_sd","TRANSsum1_TEI_adjusted_norm_sd",
						"NATRsum1_TEI_adjusted_norm_sd","kolesum1_TEI_adjusted_norm_sd","ensum1_norm_sd", "MAGNsum1_TEI_adjusted_norm_sd","FOSFsum1_TEI_adjusted_norm_sd",
						"selesum1_TEI_adjusted_norm_sd","ZINCsum1_TEI_adjusted_norm_sd","retisum1_TEI_adjusted_norm_sd",
						"karosum1_TEI_adjusted_norm_sd","TIAMsum1_TEI_adjusted_norm_sd","Folasum1_TEI_adjusted_norm_sd",
						"B2sum1_TEI_adjusted_norm_sd","NIACsum1_TEI_adjusted_norm_sd","B6sum1_TEI_adjusted_norm_sd","B12sum1_TEI_adjusted_norm_sd",
						"askosum1_TEI_adjusted_norm_sd","Dsum1_TEI_adjusted_norm_sd","tokosum1_TEI_adjusted_norm_sd",
						"VITKsum1_TEI_adjusted_norm_sd","jernsum1_TEI_adjusted_norm_sd","JODIsum1_TEI_adjusted_norm_sd",
						"kalcsum1_TEI_adjusted_norm_sd","KALIsum1_TEI_adjusted_norm_sd")])
VIP_data_subset_visit1_complete_cases$bmi_category[VIP_data_subset_visit1_complete_cases$bmi < 25]<-0
VIP_data_subset_visit1_complete_cases$bmi_category[VIP_data_subset_visit1_complete_cases$bmi >= 25 & VIP_data_subset_visit1_complete_cases$bmi < 30]<-1
VIP_data_subset_visit1_complete_cases$bmi_category[VIP_data_subset_visit1_complete_cases$bmi >= 30]<-2
VIP_data_subset_visit1_complete_cases$bmi_category<-as.factor(VIP_data_subset_visit1_complete_cases$bmi_category)

#load visit2 variables
source(file="../load_variables_visit2_dataset_TEI_adjusted.R")

#take only complete cases
VIP_data_subset_visit2_complete_cases <- na.omit(VIP_data_subset_visit2[,c("Subject_id","bmi","bmi_norm_sd","age","agesq","gender_factor","year","ffq_factor",
						"basic_residuals_bmi", "POLYsum1_TEI_adjusted_norm_sd","MONOsum1_TEI_adjusted_norm_sd","mfetsum1_TEI_adjusted_norm_sd",
						"fettsum1_TEI_adjusted_norm_sd","sacksum1_TEI_adjusted_norm_sd","kolhsum1_TEI_adjusted_norm_sd","FA_TEI_adjusted_norm_sd",
						"protsum1_anim_TEI_adjusted_norm_sd","protsum1_veg_TEI_adjusted_norm_sd",
						"fibesum1_TEI_adjusted_norm_sd","DISAsum1_TEI_adjusted_norm_sd","MOSAsum1_TEI_adjusted_norm_sd","TRANSsum1_TEI_adjusted_norm_sd",
						"NATRsum1_TEI_adjusted_norm_sd","kolesum1_TEI_adjusted_norm_sd","ensum1_norm_sd", "MAGNsum1_TEI_adjusted_norm_sd","FOSFsum1_TEI_adjusted_norm_sd",
						"selesum1_TEI_adjusted_norm_sd","ZINCsum1_TEI_adjusted_norm_sd","retisum1_TEI_adjusted_norm_sd",
						"karosum1_TEI_adjusted_norm_sd","TIAMsum1_TEI_adjusted_norm_sd","Folasum1_TEI_adjusted_norm_sd",
						"B2sum1_TEI_adjusted_norm_sd","NIACsum1_TEI_adjusted_norm_sd","B6sum1_TEI_adjusted_norm_sd","B12sum1_TEI_adjusted_norm_sd",
						"askosum1_TEI_adjusted_norm_sd","Dsum1_TEI_adjusted_norm_sd","tokosum1_TEI_adjusted_norm_sd",
						"VITKsum1_TEI_adjusted_norm_sd","jernsum1_TEI_adjusted_norm_sd","JODIsum1_TEI_adjusted_norm_sd",
						"kalcsum1_TEI_adjusted_norm_sd","KALIsum1_TEI_adjusted_norm_sd")])
VIP_data_subset_visit2_complete_cases$bmi_category[VIP_data_subset_visit2_complete_cases$bmi < 25]<-0
VIP_data_subset_visit2_complete_cases$bmi_category[VIP_data_subset_visit2_complete_cases$bmi >= 25 & VIP_data_subset_visit2_complete_cases$bmi < 30]<-1
VIP_data_subset_visit2_complete_cases$bmi_category[VIP_data_subset_visit2_complete_cases$bmi >= 30]<-2
VIP_data_subset_visit2_complete_cases$bmi_category<-as.factor(VIP_data_subset_visit2_complete_cases$bmi_category)


#keep subjects that were in the complete cases in both visits...31183(30118 with alkosum1 and utbild and g6)
VIP_data_subset_visit2_complete_cases<-VIP_data_subset_visit2_complete_cases[VIP_data_subset_visit2_complete_cases$Subject_id %in% VIP_data_subset_visit1_complete_cases$Subject_id,]
VIP_data_subset_visit1_complete_cases<-VIP_data_subset_visit1_complete_cases[VIP_data_subset_visit1_complete_cases$Subject_id %in% VIP_data_subset_visit2_complete_cases$Subject_id,]


#construct a diet score and make a model for each alpha level
bmi_category_models_log_reg <- vector("list",10)

#make matrix of bmi classes visit1
bmi_classes_visit1<-c()
bmi_classes_visit1[VIP_data_subset_visit1_complete_cases$bmi_category==0]<-1
bmi_classes_visit1[VIP_data_subset_visit1_complete_cases$bmi_category!=0]<-0
bmi_classes_visit1<-cbind(bmi_classes_visit1,bmi_classes_visit1, bmi_classes_visit1)
bmi_classes_visit1[VIP_data_subset_visit1_complete_cases$bmi_category!=1,2]<-0
bmi_classes_visit1[VIP_data_subset_visit1_complete_cases$bmi_category==1,2]<-1
bmi_classes_visit1[VIP_data_subset_visit1_complete_cases$bmi_category!=2,3]<-0
bmi_classes_visit1[VIP_data_subset_visit1_complete_cases$bmi_category==2,3]<-1
colnames(bmi_classes_visit1)<-c(1,2,3)

likelihood_null_model_visit1<-logLik(multinom(bmi_category ~ 1,data=VIP_data_subset_visit1_complete_cases))

auc_visit1_01 <- vector("list",16)
auc_visit1_02 <- vector("list",16)
auc_visit1_12 <- vector("list",16)


for (set_alpha in 1:11){
	
	VIP_data_subset_visit1_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit1_complete_cases[,10:45])%*%
					diag(regression_coefficients[set_alpha,]),1,sum,na.rm=T)
	
	message(paste0("min diet score: "),min(VIP_data_subset_visit1_complete_cases$diet_score))
	message(paste0("max diet score: "),max(VIP_data_subset_visit1_complete_cases$diet_score))
	message(paste0("number of unique values in diet score: "),length(unique(sort(VIP_data_subset_visit1_complete_cases$diet_score))))
	
	png(paste("../../Results/environment_risk_score/visit1_diet_score_vs_bmi_category_alpha_level",set_alpha,".png", sep=""),width=1000)
	plot(VIP_data_subset_visit1_complete_cases$bmi_category,VIP_data_subset_visit1_complete_cases$diet_score,xlab="bmi_category",ylab="diet score")
	dev.off()
	
	message(paste0("alpha level : ",(set_alpha-1)/10))
	message("\n")
		
	bmi_category_models_log_reg[[set_alpha]]<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor + diet_score,
			data=VIP_data_subset_visit1_complete_cases)
	message(paste0(capture.output(stargazer(bmi_category_models_log_reg[[set_alpha]],type="text")),collapse="\n"))
	
	# calculate average AUC for classes
	
	auc_visit1_01[[set_alpha]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,1],bmi_category_models_log_reg[[set_alpha]]$fitted.value[,1]))
	auc_visit1_02[[set_alpha]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,2],bmi_category_models_log_reg[[set_alpha]]$fitted.value[,2]))
	auc_visit1_12[[set_alpha]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,3],bmi_category_models_log_reg[[set_alpha]]$fitted.value[,3]))
	
	message(paste0("multi class AUC : ",mean(auc_visit1_01[[set_alpha]],auc_visit1_02[[set_alpha]],auc_visit1_12[[set_alpha]])))
	
	
	message(paste0("\nr² : ",1-logLik(bmi_category_models_log_reg[[set_alpha]])/likelihood_null_model_visit1))
	
	message("\n\n\n")
	
}

#add safety check including diet scores contructed from all nutrients with normal regression coefficients

#linear regression, separete nutrients
attach(VIP_data_independant_complete_cases)

#separate
separate_coefficients<-c()

for (variable in colnames(predictors)){
	
	associations<-glm(basic_residuals_bmi~VIP_data_independant_complete_cases[,c(variable)], family = gaussian(link = "identity"))
	associations_summary<-summary(associations)
	
	message(variable)
	message(round(associations_summary$coefficients[[2]],7))
	message(round(associations_summary$coefficients[[8]],7))
	
	#if non significant set coefficient to 0
	if(associations_summary$coefficients[[8]]>=0.05){
		separate_coefficients<-c(separate_coefficients, 0)
	}else{
		separate_coefficients<-c(separate_coefficients, associations_summary$coefficients[[2]])	
	}
	
}


VIP_data_subset_visit1_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit1_complete_cases[,10:45])%*%
				diag(separate_coefficients),1,sum,na.rm=T)

png(paste("../../Results/environment_risk_score/visit1_diet_score_vs_bmi_category_separate_lm.png", sep=""),width=1000)
plot(VIP_data_subset_visit1_complete_cases$bmi_category,VIP_data_subset_visit1_complete_cases$diet_score,xlab="bmi_category",ylab="diet score")
dev.off()

bmi_category_model_sep_log_reg<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor + diet_score,
		data=VIP_data_subset_visit1_complete_cases)

stargazer(bmi_category_model_sep_log_reg,type="text")

# calculate average AUC for classes 

auc_visit1_01[[12]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,1],bmi_category_model_sep_log_reg$fitted.value[,1]))
auc_visit1_02[[12]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,2],bmi_category_model_sep_log_reg$fitted.value[,2]))
auc_visit1_12[[12]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,3],bmi_category_model_sep_log_reg$fitted.value[,3]))

mean(auc_visit1_01[[12]],auc_visit1_02[[12]],auc_visit1_12[[12]])

1-logLik(bmi_category_model_sep_log_reg)/likelihood_null_model_visit1


#together
multiple_coefficients<-glm(basic_residuals_bmi~POLYsum1_TEI_adjusted_norm_sd+MONOsum1_TEI_adjusted_norm_sd+mfetsum1_TEI_adjusted_norm_sd+
				fettsum1_TEI_adjusted_norm_sd+sacksum1_TEI_adjusted_norm_sd+kolhsum1_TEI_adjusted_norm_sd+FA_TEI_adjusted_norm_sd+
				protsum1_anim_TEI_adjusted_norm_sd+protsum1_veg_TEI_adjusted_norm_sd+
				fibesum1_TEI_adjusted_norm_sd+DISAsum1_TEI_adjusted_norm_sd+MOSAsum1_TEI_adjusted_norm_sd+TRANSsum1_TEI_adjusted_norm_sd+
				NATRsum1_TEI_adjusted_norm_sd+kolesum1_TEI_adjusted_norm_sd+ensum1_norm_sd+ MAGNsum1_TEI_adjusted_norm_sd+
				FOSFsum1_TEI_adjusted_norm_sd+selesum1_TEI_adjusted_norm_sd+ZINCsum1_TEI_adjusted_norm_sd+retisum1_TEI_adjusted_norm_sd+
				karosum1_TEI_adjusted_norm_sd+TIAMsum1_TEI_adjusted_norm_sd+Folasum1_TEI_adjusted_norm_sd+B2sum1_TEI_adjusted_norm_sd+
				NIACsum1_TEI_adjusted_norm_sd+B6sum1_TEI_adjusted_norm_sd+B12sum1_TEI_adjusted_norm_sd+askosum1_TEI_adjusted_norm_sd+
				Dsum1_TEI_adjusted_norm_sd+tokosum1_TEI_adjusted_norm_sd+VITKsum1_TEI_adjusted_norm_sd+jernsum1_TEI_adjusted_norm_sd+
				JODIsum1_TEI_adjusted_norm_sd+kalcsum1_TEI_adjusted_norm_sd+KALIsum1_TEI_adjusted_norm_sd, family=gaussian(link="identity"))$coefficients[-1]

multiple_p_values<-summary(glm(basic_residuals_bmi~POLYsum1_TEI_adjusted_norm_sd+MONOsum1_TEI_adjusted_norm_sd+mfetsum1_TEI_adjusted_norm_sd+
						fettsum1_TEI_adjusted_norm_sd+sacksum1_TEI_adjusted_norm_sd+kolhsum1_TEI_adjusted_norm_sd+FA_TEI_adjusted_norm_sd+
						protsum1_anim_TEI_adjusted_norm_sd+protsum1_veg_TEI_adjusted_norm_sd+
						fibesum1_TEI_adjusted_norm_sd+DISAsum1_TEI_adjusted_norm_sd+MOSAsum1_TEI_adjusted_norm_sd+TRANSsum1_TEI_adjusted_norm_sd+
						NATRsum1_TEI_adjusted_norm_sd+kolesum1_TEI_adjusted_norm_sd+ensum1_norm_sd+ MAGNsum1_TEI_adjusted_norm_sd+
						FOSFsum1_TEI_adjusted_norm_sd+selesum1_TEI_adjusted_norm_sd+ZINCsum1_TEI_adjusted_norm_sd+retisum1_TEI_adjusted_norm_sd+
						karosum1_TEI_adjusted_norm_sd+TIAMsum1_TEI_adjusted_norm_sd+Folasum1_TEI_adjusted_norm_sd+B2sum1_TEI_adjusted_norm_sd+
						NIACsum1_TEI_adjusted_norm_sd+B6sum1_TEI_adjusted_norm_sd+B12sum1_TEI_adjusted_norm_sd+askosum1_TEI_adjusted_norm_sd+
						Dsum1_TEI_adjusted_norm_sd+tokosum1_TEI_adjusted_norm_sd+VITKsum1_TEI_adjusted_norm_sd+jernsum1_TEI_adjusted_norm_sd+
						JODIsum1_TEI_adjusted_norm_sd+kalcsum1_TEI_adjusted_norm_sd+KALIsum1_TEI_adjusted_norm_sd, family=gaussian(link="identity")))$coefficients[113:148]

detach(VIP_data_independant_complete_cases)

multiple_coefficients[multiple_p_values>=0.05]<-0


VIP_data_subset_visit1_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit1_complete_cases[,10:45])%*%
				diag(multiple_coefficients),1,sum,na.rm=T)

png(paste("../../Results/environment_risk_score/visit1_diet_score_vs_bmi_category_multiple_lm.png", sep=""),width=1000)
plot(VIP_data_subset_visit1_complete_cases$bmi_category,VIP_data_subset_visit1_complete_cases$diet_score,xlab="bmi_category",ylab="diet score")
dev.off()

bmi_category_model_multi_log_reg<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor + diet_score,
		data=VIP_data_subset_visit1_complete_cases)

stargazer(bmi_category_model_multi_log_reg,type="text")

# calculate average AUC for classes


auc_visit1_01[[13]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,1],bmi_category_model_multi_log_reg$fitted.value[,1]))
auc_visit1_02[[13]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,2],bmi_category_model_multi_log_reg$fitted.value[,2]))
auc_visit1_12[[13]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,3],bmi_category_model_multi_log_reg$fitted.value[,3]))


mean(auc_visit1_01[[13]],auc_visit1_02[[13]],auc_visit1_12[[13]])

1-logLik(bmi_category_model_multi_log_reg)/likelihood_null_model_visit1


#check  the AIC and AUC of model with basic covariates

bmi_category_model_basic<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor,
		data=VIP_data_subset_visit1_complete_cases)

stargazer(bmi_category_model_basic,type="text")


# calculate average AUC for classes

auc_visit1_01[[14]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,1],bmi_category_model_basic$fitted.value[,1]))
auc_visit1_02[[14]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,2],bmi_category_model_basic$fitted.value[,2]))
auc_visit1_12[[14]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,3],bmi_category_model_basic$fitted.value[,3]))


mean(auc_visit1_01[[14]],auc_visit1_02[[14]],auc_visit1_12[[14]])

1-logLik(bmi_category_model_basic)/likelihood_null_model_visit1





#test on visit 2
#make matrix of bmi classes visit2
bmi_classes_visit2<-c()
bmi_classes_visit2[VIP_data_subset_visit2_complete_cases$bmi_category==0]<-1
bmi_classes_visit2[VIP_data_subset_visit2_complete_cases$bmi_category!=0]<-0
bmi_classes_visit2<-cbind(bmi_classes_visit2,bmi_classes_visit2, bmi_classes_visit2)
bmi_classes_visit2[VIP_data_subset_visit2_complete_cases$bmi_category!=1,2]<-0
bmi_classes_visit2[VIP_data_subset_visit2_complete_cases$bmi_category==1,2]<-1
bmi_classes_visit2[VIP_data_subset_visit2_complete_cases$bmi_category!=2,3]<-0
bmi_classes_visit2[VIP_data_subset_visit2_complete_cases$bmi_category==2,3]<-1
colnames(bmi_classes_visit2)<-c(1,2,3)


likelihood_null_model_visit2<-logLik(multinom(bmi_category ~ 1,data=VIP_data_subset_visit2_complete_cases))


#test all 10 models, to determine the best alpha and therefor the best coefficients
for (set_alpha in 1:11){
	
	VIP_data_subset_visit2_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit2_complete_cases[,10:45])%*%
					diag(regression_coefficients[set_alpha,]),1,sum,na.rm=T)
	
	VIP_data_subset_visit1_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit1_complete_cases[,10:45])%*%
					diag(regression_coefficients[set_alpha,]),1,sum,na.rm=T)
	
	
	message(paste0("alpha level : ",(set_alpha-1)/10))
	message("\n")
	
	
	#multinomial regression
	bmi_category_prediction_lr<-predict(bmi_category_models_log_reg[[set_alpha]], 
			VIP_data_subset_visit2_complete_cases[,c("age","agesq","gender_factor","year","ffq_factor","diet_score")],type="probs")
	

	message("multinomial regression")
	
	# calculate average AUC for classes
	
	auc01<-pROC::auc(pROC::roc(bmi_classes_visit2[,1],bmi_category_prediction_lr[,1]))
	auc02<-pROC::auc(pROC::roc(bmi_classes_visit2[,2],bmi_category_prediction_lr[,2]))
	auc12<-pROC::auc(pROC::roc(bmi_classes_visit2[,3],bmi_category_prediction_lr[,3]))
	
	message(paste0("multi class AUC : ",mean(auc01,auc02,auc12)))
	message("\n")
	
	
	
	#multinomial regression in time
	bmi_category_prediction_lr_time<-multinom(VIP_data_subset_visit2_complete_cases$bmi_category ~ VIP_data_subset_visit2_complete_cases$age + VIP_data_subset_visit2_complete_cases$agesq + 
					VIP_data_subset_visit2_complete_cases$gender_factor + VIP_data_subset_visit2_complete_cases$year + VIP_data_subset_visit2_complete_cases$ffq_factor +
					VIP_data_subset_visit1_complete_cases$diet_score, type="probs")
	
	
		
	message("multinomial regression in time")
	
	message(paste0(capture.output(stargazer(bmi_category_prediction_lr_time,type="text")),collapse="\n"))
	
	
	# calculate average AUC for classes
	
	auc01<-pROC::auc(pROC::roc(bmi_classes_visit2[,1],bmi_category_prediction_lr_time$fitted.values[,1]))
	auc02<-pROC::auc(pROC::roc(bmi_classes_visit2[,2],bmi_category_prediction_lr_time$fitted.values[,2]))
	auc12<-pROC::auc(pROC::roc(bmi_classes_visit2[,3],bmi_category_prediction_lr_time$fitted.values[,3]))
	
	
	message(paste0("AUC : ",mean(auc01,auc02,auc12)))
	
	message(paste0("\nr² : ",1-logLik(bmi_category_prediction_lr_time)/likelihood_null_model_visit2))
	
	
	message("\n\n\n")
	
	
}



#for the best alpha, check which variables were selected
selected_nutrients<-colnames(predictors)[regression_coefficients[4,]!=0]
selected_nutrients


#for these variables, check if the prediction is better by using the regression coefficients from multiple regression or separate regression

#linear regression separetly

regression_coefficients_lin_reg_sep<-c()

for (variable in selected_nutrients){
	
	associations<-glm(VIP_data_independant$basic_residuals_bmi~VIP_data_independant[,c(variable)], family = gaussian(link = "identity"))
	associations_summary<-summary(associations)
	
	message(variable)
	message(round(associations_summary$coefficients[[2]],7))
	message(round(associations_summary$coefficients[[8]],7))
	
	#if non significant set coefficient to 0
	if(associations_summary$coefficients[[8]]>=0.05){
		regression_coefficients_lin_reg_sep<-c(regression_coefficients_lin_reg_sep, 0)
	}else{
		regression_coefficients_lin_reg_sep<-c(regression_coefficients_lin_reg_sep, associations_summary$coefficients[[2]])	
	}
	
	
}


#make diet scores
VIP_data_subset_visit1_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit1_complete_cases[,selected_nutrients])%*%
				diag(regression_coefficients_lin_reg_sep),1,sum,na.rm=T)

VIP_data_subset_visit2_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit2_complete_cases[,selected_nutrients])%*%
				diag(regression_coefficients_lin_reg_sep),1,sum,na.rm=T)

#model

#logistic regression
bmi_category_model_log_reg<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor + diet_score,
		data=VIP_data_subset_visit1_complete_cases)

stargazer(bmi_category_model_log_reg, type="text")



# calculate average AUC for classes

auc_visit1_01[[15]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,1],bmi_category_model_log_reg$fitted.values[,1]))
auc_visit1_02[[15]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,2],bmi_category_model_log_reg$fitted.values[,2]))
auc_visit1_12[[15]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,3],bmi_category_model_log_reg$fitted.values[,3]))

mean(auc_visit1_01[[15]],auc_visit1_02[[15]],auc_visit1_12[[15]])

message(paste0("\nr² : ",1-logLik(bmi_category_model_log_reg)/likelihood_null_model_visit1))

#predict

#logistic regression
bmi_category_prediction_log_reg<-predict(bmi_category_model_log_reg, VIP_data_subset_visit2_complete_cases[,c("age","agesq","gender_factor","year","ffq_factor","diet_score")],
		,type="probs")

# calculate average AUC for classes

auc01<-pROC::auc(pROC::roc(bmi_classes_visit2[,1],bmi_category_prediction_log_reg[,1]))
auc02<-pROC::auc(pROC::roc(bmi_classes_visit2[,2],bmi_category_prediction_log_reg[,2]))
auc12<-pROC::auc(pROC::roc(bmi_classes_visit2[,3],bmi_category_prediction_log_reg[,3]))

mean(auc01,auc02,auc12)


#logistic regression in time
bmi_category_prediction_log_reg_time<-multinom(VIP_data_subset_visit2_complete_cases$bmi_category ~ VIP_data_subset_visit2_complete_cases$age + VIP_data_subset_visit2_complete_cases$agesq + 
				VIP_data_subset_visit2_complete_cases$gender_factor + VIP_data_subset_visit2_complete_cases$year + VIP_data_subset_visit2_complete_cases$ffq_factor +
				VIP_data_subset_visit1_complete_cases$diet_score)

stargazer(bmi_category_prediction_log_reg_time, type="text")

# calculate average AUC for classes

auc01<-pROC::auc(pROC::roc(bmi_classes_visit2[,1],bmi_category_prediction_log_reg_time$fitted.values[,1]))
auc02<-pROC::auc(pROC::roc(bmi_classes_visit2[,2],bmi_category_prediction_log_reg_time$fitted.values[,2]))
auc12<-pROC::auc(pROC::roc(bmi_classes_visit2[,3],bmi_category_prediction_log_reg_time$fitted.values[,3]))

mean(auc01,auc02,auc12)

message(paste0("\nr² : ",1-logLik(bmi_category_prediction_log_reg_time)/likelihood_null_model_visit2))


#CHECK WHICH VARIABLES WERE SELECTED AND CORRECT THIS IF NECESSARY, sometimes kalcsum1 is selected at level 0.3 alpha, somethime not

#multiple linear regression, all together 
regression_coefficients_lin_reg_multi<-glm(basic_residuals_bmi~MONOsum1_TEI_adjusted_norm_sd+mfetsum1_TEI_adjusted_norm_sd+
				FA_TEI_adjusted_norm_sd+protsum1_anim_TEI_adjusted_norm_sd+protsum1_veg_TEI_adjusted_norm_sd+
				DISAsum1_TEI_adjusted_norm_sd+TRANSsum1_TEI_adjusted_norm_sd+NATRsum1_TEI_adjusted_norm_sd+ensum1_norm_sd+
				MAGNsum1_TEI_adjusted_norm_sd+FOSFsum1_TEI_adjusted_norm_sd+ZINCsum1_TEI_adjusted_norm_sd+retisum1_TEI_adjusted_norm_sd+
				karosum1_TEI_adjusted_norm_sd+TIAMsum1_TEI_adjusted_norm_sd+Folasum1_TEI_adjusted_norm_sd+
				B6sum1_TEI_adjusted_norm_sd+B12sum1_TEI_adjusted_norm_sd+askosum1_TEI_adjusted_norm_sd+Dsum1_TEI_adjusted_norm_sd+tokosum1_TEI_adjusted_norm_sd+
				VITKsum1_TEI_adjusted_norm_sd+JODIsum1_TEI_adjusted_norm_sd+KALIsum1_TEI_adjusted_norm_sd,
		data=VIP_data_independant, family = gaussian(link = "identity"))$coefficients[-1]

p_values_lin_reg_multi<-summary(glm(basic_residuals_bmi~MONOsum1_TEI_adjusted_norm_sd+mfetsum1_TEI_adjusted_norm_sd+
						FA_TEI_adjusted_norm_sd+protsum1_anim_TEI_adjusted_norm_sd+protsum1_veg_TEI_adjusted_norm_sd+
						DISAsum1_TEI_adjusted_norm_sd+TRANSsum1_TEI_adjusted_norm_sd+NATRsum1_TEI_adjusted_norm_sd+ensum1_norm_sd+
						MAGNsum1_TEI_adjusted_norm_sd+FOSFsum1_TEI_adjusted_norm_sd+ZINCsum1_TEI_adjusted_norm_sd+retisum1_TEI_adjusted_norm_sd+
						karosum1_TEI_adjusted_norm_sd+TIAMsum1_TEI_adjusted_norm_sd+Folasum1_TEI_adjusted_norm_sd+
						B6sum1_TEI_adjusted_norm_sd+B12sum1_TEI_adjusted_norm_sd+askosum1_TEI_adjusted_norm_sd+Dsum1_TEI_adjusted_norm_sd+tokosum1_TEI_adjusted_norm_sd+
						VITKsum1_TEI_adjusted_norm_sd+JODIsum1_TEI_adjusted_norm_sd+KALIsum1_TEI_adjusted_norm_sd,
				data=VIP_data_independant, family = gaussian(link = "identity")))$coefficients[77:100]#check the indexes are ok for the variables selected


regression_coefficients_lin_reg_multi[p_values_lin_reg_multi>=0.05]<-0


#make diet scores
VIP_data_subset_visit1_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit1_complete_cases[,selected_nutrients])%*%
				diag(regression_coefficients_lin_reg_multi),1,sum,na.rm=T)

VIP_data_subset_visit2_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit2_complete_cases[,selected_nutrients])%*%
				diag(regression_coefficients_lin_reg_multi),1,sum,na.rm=T)

#model

#logistic regression
bmi_category_model_log_reg<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor + diet_score,
		data=VIP_data_subset_visit1_complete_cases)

stargazer(bmi_category_model_log_reg, type="text")

# calculate average AUC for classes

auc_visit1_01[[16]]<-pROC::auc(pROC::roc(bmi_classes_visit2[,1],bmi_category_model_log_reg$fitted.values[,1]))
auc_visit1_02[[16]]<-pROC::auc(pROC::roc(bmi_classes_visit2[,2],bmi_category_model_log_reg$fitted.values[,2]))
auc_visit1_12[[16]]<-pROC::auc(pROC::roc(bmi_classes_visit2[,3],bmi_category_model_log_reg$fitted.values[,3]))

mean(auc_visit1_01[[16]],auc_visit1_02[[16]],auc_visit1_12[[16]])

1-logLik(bmi_category_model_log_reg)/likelihood_null_model_visit1


#predict 

#logistic regression
bmi_category_prediction_log_reg<-predict(bmi_category_model_log_reg, VIP_data_subset_visit2_complete_cases[,c("age","agesq","gender_factor","year","ffq_factor","diet_score")],
		,type="probs")


# calculate average AUC for classes

auc01<-pROC::auc(pROC::roc(bmi_classes_visit2[,1],bmi_category_prediction_log_reg[,1]))
auc02<-pROC::auc(pROC::roc(bmi_classes_visit2[,2],bmi_category_prediction_log_reg[,2]))
auc12<-pROC::auc(pROC::roc(bmi_classes_visit2[,3],bmi_category_prediction_log_reg[,3]))

mean(auc01,auc02,auc12)


#logistic regression in time
bmi_category_prediction_log_reg_time<-multinom(VIP_data_subset_visit2_complete_cases$bmi_category ~ VIP_data_subset_visit2_complete_cases$age + VIP_data_subset_visit2_complete_cases$agesq + 
				VIP_data_subset_visit2_complete_cases$gender_factor + VIP_data_subset_visit2_complete_cases$year + VIP_data_subset_visit2_complete_cases$ffq_factor +
				VIP_data_subset_visit1_complete_cases$diet_score)

stargazer(bmi_category_prediction_log_reg_time,type="text")


# calculate average AUC for classes

auc01<-pROC::auc(pROC::roc(bmi_classes_visit2[,1],bmi_category_prediction_log_reg_time$fitted.values[,1]))
auc02<-pROC::auc(pROC::roc(bmi_classes_visit2[,2],bmi_category_prediction_log_reg_time$fitted.values[,2]))
auc12<-pROC::auc(pROC::roc(bmi_classes_visit2[,3],bmi_category_prediction_log_reg_time$fitted.values[,3]))

mean(auc01,auc02,auc12)

1-logLik(bmi_category_prediction_log_reg_time)/likelihood_null_model_visit2






#also check the results when using coefficients from independent to make a diet score in visit2

#elastic net coefficients

#make diet score
VIP_data_subset_visit2_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit2_complete_cases[,10:45])%*%
				diag(regression_coefficients[4,]),1,sum,na.rm=T)


bmi_category_prediction_log_reg_visit2<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor + diet_score, 
		data=VIP_data_subset_visit2_complete_cases)

stargazer(bmi_category_prediction_log_reg_visit2,type="text")

# calculate average AUC for classes

auc01<-pROC::auc(pROC::roc(bmi_classes_visit2[,1],bmi_category_prediction_log_reg_visit2$fitted.values[,1]))
auc02<-pROC::auc(pROC::roc(bmi_classes_visit2[,2],bmi_category_prediction_log_reg_visit2$fitted.values[,2]))
auc12<-pROC::auc(pROC::roc(bmi_classes_visit2[,3],bmi_category_prediction_log_reg_visit2$fitted.values[,3]))

mean(auc01,auc02,auc12)

1-logLik(bmi_category_prediction_log_reg_visit2)/likelihood_null_model_visit2


#separate lm coefficients

#make diet score
VIP_data_subset_visit2_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit2_complete_cases[,selected_nutrients])%*%
				diag(regression_coefficients_lin_reg_sep),1,sum,na.rm=T)


bmi_category_prediction_log_reg_visit2<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor + diet_score, 
		data=VIP_data_subset_visit2_complete_cases)

stargazer(bmi_category_prediction_log_reg_visit2, type="text")

auc01<-pROC::auc(pROC::roc(bmi_classes_visit2[,1],bmi_category_prediction_log_reg_visit2$fitted.values[,1]))
auc02<-pROC::auc(pROC::roc(bmi_classes_visit2[,2],bmi_category_prediction_log_reg_visit2$fitted.values[,2]))
auc12<-pROC::auc(pROC::roc(bmi_classes_visit2[,3],bmi_category_prediction_log_reg_visit2$fitted.values[,3]))

mean(auc01,auc02,auc12)

1-logLik(bmi_category_prediction_log_reg_visit2)/likelihood_null_model_visit2



#multiple linear regression, all together 

#make diet score
VIP_data_subset_visit2_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit2_complete_cases[,selected_nutrients])%*%
				diag(regression_coefficients_lin_reg_multi),1,sum,na.rm=T)


bmi_category_prediction_log_reg_visit2<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor + diet_score, 
		data=VIP_data_subset_visit2_complete_cases)

stargazer(bmi_category_prediction_log_reg_visit2, type="text")

auc01<-pROC::auc(pROC::roc(bmi_classes_visit2[,1],bmi_category_prediction_log_reg_visit2$fitted.values[,1]))
auc02<-pROC::auc(pROC::roc(bmi_classes_visit2[,2],bmi_category_prediction_log_reg_visit2$fitted.values[,2]))
auc12<-pROC::auc(pROC::roc(bmi_classes_visit2[,3],bmi_category_prediction_log_reg_visit2$fitted.values[,3]))

mean(auc01,auc02,auc12)

1-logLik(bmi_category_prediction_log_reg_visit2)/likelihood_null_model_visit2


#check the AIC and AUC for the basic model in visit 2 also


bmi_category_model_basic<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor,
		data=VIP_data_subset_visit2_complete_cases)

stargazer(bmi_category_model_basic, type="text")

auc01<-pROC::auc(pROC::roc(bmi_classes_visit2[,1],bmi_category_model_basic$fitted.values[,1]))
auc02<-pROC::auc(pROC::roc(bmi_classes_visit2[,2],bmi_category_model_basic$fitted.values[,2]))
auc12<-pROC::auc(pROC::roc(bmi_classes_visit2[,3],bmi_category_model_basic$fitted.values[,3]))

mean(auc01,auc02,auc12)

1-logLik(bmi_category_model_basic)/likelihood_null_model_visit2



#test the difference in the AUC for visit1
for ( AUC_diff1 in c(1:16))
{
	for ( AUC_diff2 in c(1:16)){
		
		if(roc.test(auc_visit1_01[[AUC_diff1]], auc_visit1_01[[AUC_diff2]])$p.value >= 0.05){
			
			message(paste0("model comparison: ", AUC_diff1, " , " , AUC_diff2))
			
			message(paste0("AUC difference significance: ", roc.test(auc_visit1_01[[AUC_diff1]], auc_visit1_01[[AUC_diff2]])$p.value))
		}
		
	}
	
}


#test the difference in the AUC for visit1
for ( AUC_diff1 in c(1:16))
{
	for ( AUC_diff2 in c(1:16)){
		
		if(roc.test(auc_visit1_02[[AUC_diff1]], auc_visit1_02[[AUC_diff2]])$p.value >= 0.05){
			
			message(paste0("model comparison: ", AUC_diff1, " , " , AUC_diff2))
			
			message(paste0("AUC difference significance: ", roc.test(auc_visit1_02[[AUC_diff1]], auc_visit1_02[[AUC_diff2]])$p.value))
		}
		
	}
	
}



#test the difference in the AUC for visit1
for ( AUC_diff1 in c(1:16))
{
	for ( AUC_diff2 in c(1:16)){
		
		if(roc.test(auc_visit1_12[[AUC_diff1]], auc_visit1_12[[AUC_diff2]])$p.value >= 0.05){
			
			message(paste0("model comparison: ", AUC_diff1, " , " , AUC_diff2))
			
			message(paste0("AUC difference significance: ", roc.test(auc_visit1_12[[AUC_diff1]], auc_visit1_12[[AUC_diff2]])$p.value))
		}
		
	}
	
}



#for the best alpha 0.3 and the multi lm check the predictions table:


#check tables for best alpha 0.3 and fitting all together with multi lm based on ols

#elastic net

#visit1


VIP_data_subset_visit1_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit1_complete_cases[,10:45])%*%
				diag(regression_coefficients[4,]),1,sum,na.rm=T)


bmi_category_prediction_visit1<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor + diet_score, 
		data=VIP_data_subset_visit1_complete_cases)

obesity_prediction_visit1<-predict(bmi_category_prediction_visit1, VIP_data_subset_visit1_complete_cases[,c("age","agesq","gender_factor","year","ffq_factor","diet_score")],
		,type="class")

elastic_net_visit1_table<-table(obesity_prediction_visit1,VIP_data_subset_visit1_complete_cases$bmi_category)
#
#obesity_prediction_visit1     0     1     2
#0 							14501  7514  1910
#1  						 2825  3656   774
#2     							2     0     1
#

#micro precision 0.5823045
(elastic_net_visit1_table[1]+elastic_net_visit1_table[5]+elastic_net_visit1_table[9])/sum(elastic_net_visit1_table)

#macro precision 0.6061024
mean(elastic_net_visit1_table[1]/(elastic_net_visit1_table[1]+elastic_net_visit1_table[4]+elastic_net_visit1_table[7]),
		elastic_net_visit1_table[5]/(elastic_net_visit1_table[5]+elastic_net_visit1_table[2]+elastic_net_visit1_table[8]),
		elastic_net_visit1_table[9]/(elastic_net_visit1_table[9]+elastic_net_visit1_table[3]+elastic_net_visit1_table[6]))

#visit2


VIP_data_subset_visit2_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit2_complete_cases[,10:45])%*%
				diag(regression_coefficients[4,]),1,sum,na.rm=T)

bmi_category_prediction_visit2<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor + diet_score, 
		data=VIP_data_subset_visit2_complete_cases)

obesity_prediction_visit2<-predict(bmi_category_prediction_visit2, VIP_data_subset_visit2_complete_cases[,c("age","agesq","gender_factor","year","ffq_factor","diet_score")],
		,type="class")

elastic_net_visit2_table<-table(obesity_prediction_visit2,VIP_data_subset_visit2_complete_cases$bmi_category)
#
#obesity_prediction_visit2    0    1    2
#0							 8251 5477 2143
#1 							 5002 7694 2501
#2   						  37   44   34
#micro precision 0.5124266
(elastic_net_visit2_table[1]+elastic_net_visit2_table[5]+elastic_net_visit2_table[9])/sum(elastic_net_visit2_table)

#macro precision 0.519879
mean(elastic_net_visit2_table[1]/(elastic_net_visit2_table[1]+elastic_net_visit2_table[4]+elastic_net_visit2_table[7]),
		elastic_net_visit2_table[5]/(elastic_net_visit2_table[5]+elastic_net_visit2_table[2]+elastic_net_visit2_table[8]),
		elastic_net_visit2_table[9]/(elastic_net_visit2_table[9]+elastic_net_visit2_table[3]+elastic_net_visit2_table[6]))



#multi all

#visit1


VIP_data_subset_visit1_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit1_complete_cases[,10:45])%*%
				diag(multiple_coefficients),1,sum,na.rm=T)


bmi_category_prediction_visit1<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor + diet_score, 
		data=VIP_data_subset_visit1_complete_cases)


obesity_prediction_visit1<-predict(bmi_category_models_log_reg[[4]], VIP_data_subset_visit1_complete_cases[,c("age","agesq","gender_factor","year","ffq_factor","diet_score")],
		type="class")

multi_visit1_table<-table(obesity_prediction_visit1,VIP_data_subset_visit1_complete_cases$bmi_category)
#obesity_prediction_visit1     0     1     2
#0 							14545  7560  1819
#1  						 2761  3585   849
#2    						   22    25    17

#micro precision 0.5819517
(multi_visit1_table[1]+multi_visit1_table[5]+multi_visit1_table[9])/sum(multi_visit1_table)

#macro precision 0.6079669
mean(multi_visit1_table[1]/(multi_visit1_table[1]+multi_visit1_table[4]+multi_visit1_table[7]),
		multi_visit1_table[5]/(multi_visit1_table[5]+multi_visit1_table[2]+multi_visit1_table[8]),
		multi_visit1_table[9]/(multi_visit1_table[9]+multi_visit1_table[3]+multi_visit1_table[6]))



#visit2

VIP_data_subset_visit2_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit2_complete_cases[,10:45])%*%
				diag(multiple_coefficients),1,sum,na.rm=T)


bmi_category_prediction_visit2<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor + diet_score, 
		data=VIP_data_subset_visit2_complete_cases)


obesity_prediction_visit2<-predict(bmi_category_models_log_reg[[4]], VIP_data_subset_visit2_complete_cases[,c("age","agesq","gender_factor","year","ffq_factor","diet_score")],
		,type="class")

multi_visit2_table<-table(obesity_prediction_visit2,VIP_data_subset_visit2_complete_cases$bmi_category)

#obesity_prediction_visit2    0    1    2
#0 							6347 3984 1406
#1 							6752 9005 3114
#2  						 191  226  158

#micro precision 0.4973864
(multi_visit2_table[1]+multi_visit2_table[5]+multi_visit2_table[9])/sum(multi_visit2_table)

#macro precision 0.5407685
mean(multi_visit2_table[1]/(multi_visit2_table[1]+multi_visit2_table[4]+multi_visit2_table[7]),
		multi_visit2_table[5]/(multi_visit2_table[5]+multi_visit2_table[2]+multi_visit2_table[8]),
		multi_visit2_table[9]/(multi_visit2_table[9]+multi_visit2_table[3]+multi_visit2_table[6]))




#for alpha is 0.3 and multi ols check the consistency of misclassified subjects:


#elastic net

#visit1


VIP_data_subset_visit1_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit1_complete_cases[,10:45])%*%
				diag(regression_coefficients[4,]),1,sum,na.rm=T)

bmi_category_prediction_visit1<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor + diet_score, 
		data=VIP_data_subset_visit1_complete_cases)

obesity_prediction_visit1<-predict(bmi_category_prediction_visit1, VIP_data_subset_visit1_complete_cases[,c("age","agesq","gender_factor","year","ffq_factor","diet_score")],
		,type="class")

#get those subjects who are normal weight but were classified as overweight or obese
Subject_ids_overweight_visit1<-VIP_data_subset_visit1_complete_cases$Subject_id[obesity_prediction_visit1==1 & VIP_data_subset_visit1_complete_cases$bmi_category==0]

Subject_ids_obese_visit1<-VIP_data_subset_visit1_complete_cases$Subject_id[obesity_prediction_visit1==2 & VIP_data_subset_visit1_complete_cases$bmi_category==0]




#visit2


VIP_data_subset_visit2_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit2_complete_cases[,10:45])%*%
				diag(regression_coefficients[4,]),1,sum,na.rm=T)

bmi_category_prediction_visit2<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor + diet_score, 
		data=VIP_data_subset_visit2_complete_cases)

obesity_prediction_visit2<-predict(bmi_category_prediction_visit2, VIP_data_subset_visit2_complete_cases[,c("age","agesq","gender_factor","year","ffq_factor","diet_score")],
		,type="class")

#get those subjects who are normal weight but were classified as overweight or obese
Subject_ids_overweight_visit2<-VIP_data_subset_visit2_complete_cases$Subject_id[obesity_prediction_visit2==1 & VIP_data_subset_visit2_complete_cases$bmi_category==0]

Subject_ids_obese_visit2<-VIP_data_subset_visit2_complete_cases$Subject_id[obesity_prediction_visit2==2 & VIP_data_subset_visit2_complete_cases$bmi_category==0]


#check how many are the same subjects

outliers_Subjects_shrinkage_overweight<-Subject_ids_overweight_visit2[Subject_ids_overweight_visit2 %in% Subject_ids_overweight_visit1]

#overweight
length(Subject_ids_overweight_visit2[Subject_ids_overweight_visit2 %in% Subject_ids_overweight_visit1])
#1827
# 0.3652539% of visit2(5002) and 0.6467257% of visit1(2825)

#obese
length(Subject_ids_obese_visit2[Subject_ids_obese_visit2 %in% Subject_ids_obese_visit1])
#0
# 0% of visit2(37) and 0% of visit1(2)




#multi all

#visit1

VIP_data_subset_visit1_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit1_complete_cases[,10:45])%*%
				diag(multiple_coefficients),1,sum,na.rm=T)

bmi_category_prediction_visit1<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor + diet_score, 
		data=VIP_data_subset_visit1_complete_cases)

obesity_prediction_visit1<-predict(bmi_category_models_log_reg[[4]], VIP_data_subset_visit1_complete_cases[,c("age","agesq","gender_factor","year","ffq_factor","diet_score")],
		type="class")

#get those subjects who are normal weight but were classified as overweight or obese
Subject_ids_overweight_visit1_multi<-VIP_data_subset_visit1_complete_cases$Subject_id[obesity_prediction_visit1==1 & VIP_data_subset_visit1_complete_cases$bmi_category==0]

Subject_ids_obese_visit1_multi<-VIP_data_subset_visit1_complete_cases$Subject_id[obesity_prediction_visit1==2 & VIP_data_subset_visit1_complete_cases$bmi_category==0]




#visit2

VIP_data_subset_visit2_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit2_complete_cases[,10:45])%*%
				diag(multiple_coefficients),1,sum,na.rm=T)

bmi_category_prediction_visit2<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor + diet_score, 
		data=VIP_data_subset_visit2_complete_cases)

obesity_prediction_visit2<-predict(bmi_category_models_log_reg[[4]], VIP_data_subset_visit2_complete_cases[,c("age","agesq","gender_factor","year","ffq_factor","diet_score")],
		,type="class")

#get those subjects who are normal weight but were classified as overweight or obese
Subject_ids_overweight_visit2_multi<-VIP_data_subset_visit2_complete_cases$Subject_id[obesity_prediction_visit2==1 & VIP_data_subset_visit2_complete_cases$bmi_category==0]

Subject_ids_obese_visit2_multi<-VIP_data_subset_visit2_complete_cases$Subject_id[obesity_prediction_visit2==2 & VIP_data_subset_visit2_complete_cases$bmi_category==0]


#check how many are the same subjects

outliers_Subjects_multi_overweight<-Subject_ids_overweight_visit2_multi[Subject_ids_overweight_visit2_multi %in% Subject_ids_overweight_visit1_multi]

#overweight
length(Subject_ids_overweight_visit2_multi[Subject_ids_overweight_visit2_multi %in% Subject_ids_overweight_visit1_multi])
#1718
# 0.2544431% of visit2_multi(6752) and 0.6222383% of visit1_multi(2761)

#obese
length(Subject_ids_obese_visit2_multi[Subject_ids_obese_visit2_multi %in% Subject_ids_obese_visit1_multi])
#2
# 0.0104712% of visit2_multi(191) and 0.09090909% of visit1_multi(22)




#check consistency between the models

#visit1

#overweight
length(Subject_ids_overweight_visit1[Subject_ids_overweight_visit1 %in% Subject_ids_overweight_visit1_multi])
#2285
# 0.8088496% of visit1 shrinkage(2825) and 0.8275987% of visit1 multi ols(2761)

#obese
length(Subject_ids_obese_visit1[Subject_ids_obese_visit1 %in% Subject_ids_obese_visit1_multi])
#2
# 100% of visit1 shrinkage(2) and 0.09090909% of visit1 multi ols(22)




#visit2

#overweight
length(Subject_ids_overweight_visit2[Subject_ids_overweight_visit2 %in% Subject_ids_overweight_visit2_multi])
#4321
#0.8638545% of visit2 shrinkage(5002) and 0.6399585% of visit2 multi ols(6752)

#obese
length(Subject_ids_obese_visit2[Subject_ids_obese_visit2 %in% Subject_ids_obese_visit2_multi])
#36
# 0.972973% of visit2 shrinkage(37) and 0.1884817% of visit2 multi ols(191)



#out of those that are outliers in shrinkage model in both visit, check how many are also outlier in both visits of multi ols model
length(outliers_Subjects_shrinkage_overweight[outliers_Subjects_shrinkage_overweight %in% outliers_Subjects_multi_overweight])
#1454
# 0.7958402% of shrinkage(1827) and 0.8463329% of multi ols(1718)
