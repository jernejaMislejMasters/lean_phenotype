library(glmnet)
library(pROC)
library(ppcor)

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


#take only complete cases

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

##set vector for coefficients
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
#	replicate_reg_coeff<-apply(replicate_reg_coeff,2,function(x){
#				if(length(x[x==0])>=repetition_half){
#					return(rep(0,times=length(x)))
#				}else{
#					return(x)	
#				}
#			})
#	
#	#take mean of 100 values for all 36 coefficients
#	regression_coefficients<-rbind(regression_coefficients,apply(replicate_reg_coeff,2,FUN=mean))
#}
#
##for simulation, when waiting for the real coefficients
##regression_coefficients<-matrix(rexp(10*36), 10)


#load visit1 variables
source(file="../load_variables_visit1_dataset_TEI_adjusted.R")

#take only complete cases
VIP_data_subset_visit1_complete_cases <- na.omit(VIP_data_subset_visit1[,c("Subject_id","bmi","bmi_norm_sd","age","agesq","gender_factor","year","ffq_factor","basic_residuals_bmi", "POLYsum1_TEI_adjusted_norm_sd","MONOsum1_TEI_adjusted_norm_sd","mfetsum1_TEI_adjusted_norm_sd",
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
VIP_data_subset_visit1_complete_cases$obesity[VIP_data_subset_visit1_complete_cases$bmi < 30]<-0
VIP_data_subset_visit1_complete_cases$obesity[VIP_data_subset_visit1_complete_cases$bmi >= 30]<-1
VIP_data_subset_visit1_complete_cases$obesity<-as.factor(VIP_data_subset_visit1_complete_cases$obesity)

#load visit2 variables
source(file="../load_variables_visit2_dataset_TEI_adjusted.R")

#take only complete cases
VIP_data_subset_visit2_complete_cases <- na.omit(VIP_data_subset_visit2[,c("Subject_id","bmi","bmi_norm_sd","age","agesq","gender_factor","year","ffq_factor","basic_residuals_bmi", "POLYsum1_TEI_adjusted_norm_sd","MONOsum1_TEI_adjusted_norm_sd","mfetsum1_TEI_adjusted_norm_sd",
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
VIP_data_subset_visit2_complete_cases$obesity[VIP_data_subset_visit2_complete_cases$bmi < 30]<-0
VIP_data_subset_visit2_complete_cases$obesity[VIP_data_subset_visit2_complete_cases$bmi >= 30]<-1
VIP_data_subset_visit2_complete_cases$obesity<-as.factor(VIP_data_subset_visit2_complete_cases$obesity)

#keep subjects that were in the complete cases in both visits
VIP_data_subset_visit2_complete_cases<-VIP_data_subset_visit2_complete_cases[VIP_data_subset_visit2_complete_cases$Subject_id %in% VIP_data_subset_visit1_complete_cases$Subject_id,]
VIP_data_subset_visit1_complete_cases<-VIP_data_subset_visit1_complete_cases[VIP_data_subset_visit1_complete_cases$Subject_id %in% VIP_data_subset_visit2_complete_cases$Subject_id,]


#construct a diet score and make a model for each alpha level
obesity_models_log_reg <- vector("list",10)

auc_visit1 <- vector("list",16)

for (set_alpha in 1:11){
		
	VIP_data_subset_visit1_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit1_complete_cases[,10:45])%*%
					diag(regression_coefficients[set_alpha,]),1,sum,na.rm=T)
	
	png(paste("../../Results/environment_risk_score/visit1_diet_score_vs_obesity_alpha_level",set_alpha,".png", sep=""),width=1000)
	plot(VIP_data_subset_visit1_complete_cases$obesity,VIP_data_subset_visit1_complete_cases$diet_score,xlab="obesity",ylab="diet score")
	dev.off()
	
	message(paste0("alpha level : ",(set_alpha-1)/10))
	message("\n")
	
	obesity_models_log_reg[[set_alpha]]<-glm(obesity ~ age + agesq + gender_factor + year + ffq_factor + diet_score,
			data=VIP_data_subset_visit1_complete_cases,family=binomial)
	message(paste0(capture.output(summary(obesity_models_log_reg[[set_alpha]])),collapse="\n"))
	#check AUC here already...
	
	auc_visit1[[set_alpha]]<-pROC::auc(pROC::roc(as.numeric(VIP_data_subset_visit1_complete_cases$obesity), 
					obesity_models_log_reg[[set_alpha]]$fitted.value))
	
	message(paste0("AUC : ",auc_visit1[[set_alpha]]))
	
	message(paste0("\nr² : ",lrm(obesity ~ age + agesq + gender_factor + year + ffq_factor + diet_score,
					data=VIP_data_subset_visit1_complete_cases)$stats[[10]]))
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

png(paste("../../Results/environment_risk_score/visit1_diet_score_vs_obesity_separate_lm.png", sep=""),width=1000)
plot(VIP_data_subset_visit1_complete_cases$obesity,VIP_data_subset_visit1_complete_cases$diet_score,xlab="obesity",ylab="diet score")
dev.off()

obesity_model_sep_log_reg<-glm(obesity ~ age + agesq + gender_factor + year + ffq_factor + diet_score,
		data=VIP_data_subset_visit1_complete_cases,family=binomial)

summary(obesity_model_sep_log_reg)

auc_visit1[[12]]<-pROC::auc(pROC::roc(as.numeric(VIP_data_subset_visit1_complete_cases$obesity), 
				obesity_model_sep_log_reg$fitted.value))
auc_visit1[[12]]

lrm(obesity ~ age + agesq + gender_factor + year + ffq_factor + diet_score,
		data=VIP_data_subset_visit1_complete_cases)$stats[[10]]


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

png(paste("../../Results/environment_risk_score/visit1_diet_score_vs_obesity_multiple_lm.png", sep=""),width=1000)
plot(VIP_data_subset_visit1_complete_cases$obesity,VIP_data_subset_visit1_complete_cases$diet_score,xlab="obesity",ylab="diet score")
dev.off()

obesity_model_multi_log_reg<-glm(obesity ~ age + agesq + gender_factor + year + ffq_factor + diet_score,
		data=VIP_data_subset_visit1_complete_cases,family=binomial)

summary(obesity_model_multi_log_reg)

auc_visit1[[13]]<-pROC::auc(pROC::roc(as.numeric(VIP_data_subset_visit1_complete_cases$obesity), 
				obesity_model_multi_log_reg$fitted.value))
auc_visit1[[13]]

lrm(obesity ~ age + agesq + gender_factor + year + ffq_factor + diet_score,
		data=VIP_data_subset_visit1_complete_cases)$stats[[10]]


#check  the AIC and AUC of model with basic covariates

obesity_model_basic<-glm(obesity ~ age + agesq + gender_factor + year + ffq_factor,
		data=VIP_data_subset_visit1_complete_cases, family=binomial)

summary(obesity_model_basic)

auc_visit1[[14]]<-pROC::auc(pROC::roc(as.numeric(VIP_data_subset_visit1_complete_cases$obesity), 
				obesity_model_basic$fitted.value))
auc_visit1[[14]]

lrm(obesity ~ age + agesq + gender_factor + year + ffq_factor,
		data=VIP_data_subset_visit1_complete_cases)$stats[[10]]






#test on visit 2

#test all 10 models, to determine the best alpha and therefor the best coefficients
for (set_alpha in 1:11){

	VIP_data_subset_visit2_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit2_complete_cases[,10:45])%*%
					diag(regression_coefficients[set_alpha,]),1,sum,na.rm=T)
	
	VIP_data_subset_visit1_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit1_complete_cases[,10:45])%*%
					diag(regression_coefficients[set_alpha,]),1,sum,na.rm=T)
	
	
	message(paste0("alpha level : ",(set_alpha-1)/10))
	message("\n")
	
	
	#logistic regression
	obesity_prediction_lr<-predict(obesity_models_log_reg[[set_alpha]], VIP_data_subset_visit2_complete_cases[,c("age","agesq","gender_factor","year","ffq_factor","diet_score")],
			,type="response")
		
	message("logistic regression")

	message(paste0("AUC : ",pROC::auc(pROC::roc(as.numeric(VIP_data_subset_visit2_complete_cases$obesity), as.numeric(obesity_prediction_lr)))))
	message("\n")
	
	
	#logistic regression in time
	obesity_prediction_lr_time<-glm(VIP_data_subset_visit2_complete_cases$obesity ~ VIP_data_subset_visit2_complete_cases$age + VIP_data_subset_visit2_complete_cases$agesq + 
							VIP_data_subset_visit2_complete_cases$gender_factor + VIP_data_subset_visit2_complete_cases$year + VIP_data_subset_visit2_complete_cases$ffq_factor +
							VIP_data_subset_visit1_complete_cases$diet_score,family=binomial)
			
			
		
	message("logistic regression in time")
	
	message(paste0(capture.output(summary(obesity_prediction_lr_time)),collapse="\n"))
	
	message(paste0("AUC : ",pROC::auc(pROC::roc(as.numeric(VIP_data_subset_visit2_complete_cases$obesity), as.numeric(obesity_prediction_lr_time$fitted.value)))))
	
	cbind_visit2_visit1<-as.data.frame(cbind(VIP_data_subset_visit2_complete_cases[,c("obesity","age","agesq","gender_factor","year","ffq_factor")],
			VIP_data_subset_visit1_complete_cases$diet_score))
	colnames(cbind_visit2_visit1)<-c("obesity","age","agesq","gender_factor","year","ffq_factor","diet_score")
	cbind_visit2_visit1$year<-as.factor(cbind_visit2_visit1$year)
	
	message(paste0("\nr² : ",lrm(obesity ~ age + agesq + gender_factor + year + ffq_factor + diet_score,
							data=cbind_visit2_visit1)$stats[[10]]))
	
	
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
obesity_model_log_reg<-glm(obesity ~ age + agesq + gender_factor + year + ffq_factor + diet_score,
		data=VIP_data_subset_visit1_complete_cases,family=binomial)

summary(obesity_model_log_reg)

auc_visit1[[15]]<-pROC::auc(pROC::roc(as.numeric(VIP_data_subset_visit1_complete_cases$obesity), 
				obesity_model_log_reg$fitted.value))

auc_visit1[[15]]

lrm(obesity ~ age + agesq + gender_factor + year + ffq_factor + diet_score,
		data=VIP_data_subset_visit1_complete_cases)$stats[[10]]


#predict

#logistic regression
obesity_prediction_log_reg<-predict(obesity_model_log_reg, VIP_data_subset_visit2_complete_cases[,c("age","agesq","gender_factor","year","ffq_factor","diet_score")],
		,type="response")

pROC::auc(pROC::roc(as.numeric(VIP_data_subset_visit2_complete_cases$obesity), as.numeric(obesity_prediction_log_reg)))


#logistic regression in time
obesity_prediction_log_reg_time<-glm(VIP_data_subset_visit2_complete_cases$obesity ~ VIP_data_subset_visit2_complete_cases$age + VIP_data_subset_visit2_complete_cases$agesq + 
				VIP_data_subset_visit2_complete_cases$gender_factor + VIP_data_subset_visit2_complete_cases$year + VIP_data_subset_visit2_complete_cases$ffq_factor +
				VIP_data_subset_visit1_complete_cases$diet_score,family=binomial)

summary(obesity_prediction_log_reg_time)

pROC::auc(pROC::roc(as.numeric(VIP_data_subset_visit2_complete_cases$obesity), as.numeric(obesity_prediction_log_reg_time$fitted.value)))

cbind_visit2_visit1<-as.data.frame(cbind(VIP_data_subset_visit2_complete_cases[,c("obesity","age","agesq","gender_factor","year","ffq_factor")],
				VIP_data_subset_visit1_complete_cases$diet_score))
colnames(cbind_visit2_visit1)<-c("obesity","age","agesq","gender_factor","year","ffq_factor","diet_score")
cbind_visit2_visit1$year<-as.factor(cbind_visit2_visit1$year)

lrm(obesity ~ age + agesq + gender_factor + year + ffq_factor + diet_score,
		data=cbind_visit2_visit1)$stats[[10]]



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
obesity_model_log_reg<-glm(obesity ~ age + agesq + gender_factor + year + ffq_factor + diet_score,
		data=VIP_data_subset_visit1_complete_cases,family=binomial)

summary(obesity_model_log_reg)

auc_visit1[[16]]<-pROC::auc(pROC::roc(as.numeric(VIP_data_subset_visit1_complete_cases$obesity), 
				obesity_model_log_reg$fitted.value))
auc_visit1[[16]]


lrm(obesity ~ age + agesq + gender_factor + year + ffq_factor + diet_score,
		data=VIP_data_subset_visit1_complete_cases)$stats[[10]]


#predict 

#logistic regression
obesity_prediction_log_reg<-predict(obesity_model_log_reg, VIP_data_subset_visit2_complete_cases[,c("age","agesq","gender_factor","year","ffq_factor","diet_score")],
		,type="response")

pROC::auc(pROC::roc(as.numeric(VIP_data_subset_visit2_complete_cases$obesity), as.numeric(obesity_prediction_log_reg)))


#logistic regression in time
obesity_prediction_log_reg_time<-glm(VIP_data_subset_visit2_complete_cases$obesity ~ VIP_data_subset_visit2_complete_cases$age + VIP_data_subset_visit2_complete_cases$agesq + 
				VIP_data_subset_visit2_complete_cases$gender_factor + VIP_data_subset_visit2_complete_cases$year + VIP_data_subset_visit2_complete_cases$ffq_factor +
				VIP_data_subset_visit1_complete_cases$diet_score,family=binomial)

summary(obesity_prediction_log_reg_time)

pROC::auc(pROC::roc(as.numeric(VIP_data_subset_visit2_complete_cases$obesity), as.numeric(obesity_prediction_log_reg_time$fitted.value)))


cbind_visit2_visit1<-as.data.frame(cbind(VIP_data_subset_visit2_complete_cases[,c("obesity","age","agesq","gender_factor","year","ffq_factor")],
				VIP_data_subset_visit1_complete_cases$diet_score))
colnames(cbind_visit2_visit1)<-c("obesity","age","agesq","gender_factor","year","ffq_factor","diet_score")
cbind_visit2_visit1$year<-as.factor(cbind_visit2_visit1$year)

lrm(obesity ~ age + agesq + gender_factor + year + ffq_factor + diet_score,
		data=cbind_visit2_visit1)$stats[[10]]





#also check the results when using coefficients from independent to make a diet score in visit2

#elastic net coefficients

#make diet score
VIP_data_subset_visit2_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit2_complete_cases[,10:45])%*%
				diag(regression_coefficients[4,]),1,sum,na.rm=T)


obesity_prediction_log_reg_visit2<-glm(obesity ~ age + agesq + gender_factor + year + ffq_factor + diet_score, data=VIP_data_subset_visit2_complete_cases, family=binomial)

summary(obesity_prediction_log_reg_visit2)

pROC::auc(pROC::roc(as.numeric(VIP_data_subset_visit2_complete_cases$obesity), as.numeric(obesity_prediction_log_reg_visit2$fitted.value)))

VIP_data_subset_visit2_complete_cases_year_factor<-VIP_data_subset_visit2_complete_cases
VIP_data_subset_visit2_complete_cases_year_factor$year<-as.factor(VIP_data_subset_visit2_complete_cases_year_factor$year)

lrm(obesity ~ age + agesq + gender_factor + year + ffq_factor + diet_score,
		data=VIP_data_subset_visit2_complete_cases_year_factor)$stats[[10]]



#separate lm coefficients

#make diet score
VIP_data_subset_visit2_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit2_complete_cases[,selected_nutrients])%*%
				diag(regression_coefficients_lin_reg_sep),1,sum,na.rm=T)


obesity_prediction_log_reg_visit2<-glm(obesity ~ age + agesq + gender_factor + year + ffq_factor + diet_score, data=VIP_data_subset_visit2_complete_cases, family=binomial)

summary(obesity_prediction_log_reg_visit2)

pROC::auc(pROC::roc(as.numeric(VIP_data_subset_visit2_complete_cases$obesity), as.numeric(obesity_prediction_log_reg_visit2$fitted.value)))

VIP_data_subset_visit2_complete_cases_year_factor<-VIP_data_subset_visit2_complete_cases
VIP_data_subset_visit2_complete_cases_year_factor$year<-as.factor(VIP_data_subset_visit2_complete_cases_year_factor$year)

lrm(obesity ~ age + agesq + gender_factor + year + ffq_factor + diet_score,
		data=VIP_data_subset_visit2_complete_cases_year_factor)$stats[[10]]



#multiple linear regression, all together 

#make diet score
VIP_data_subset_visit2_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit2_complete_cases[,selected_nutrients])%*%
				diag(regression_coefficients_lin_reg_multi),1,sum,na.rm=T)


obesity_prediction_log_reg_visit2<-glm(obesity ~ age + agesq + gender_factor + year + ffq_factor + diet_score, data=VIP_data_subset_visit2_complete_cases, family=binomial)

summary(obesity_prediction_log_reg_visit2)

pROC::auc(pROC::roc(as.numeric(VIP_data_subset_visit2_complete_cases$obesity), as.numeric(obesity_prediction_log_reg_visit2$fitted.value)))

VIP_data_subset_visit2_complete_cases_year_factor<-VIP_data_subset_visit2_complete_cases
VIP_data_subset_visit2_complete_cases_year_factor$year<-as.factor(VIP_data_subset_visit2_complete_cases_year_factor$year)

lrm(obesity ~ age + agesq + gender_factor + year + ffq_factor + diet_score,
		data=VIP_data_subset_visit2_complete_cases_year_factor)$stats[[10]]


#check the AIC and AUC for the basic model in visit 2 also


obesity_model_basic<-glm(obesity ~ age + agesq + gender_factor + year + ffq_factor,
		data=VIP_data_subset_visit2_complete_cases, family=binomial)

summary(obesity_model_basic)

pROC::auc(pROC::roc(as.numeric(VIP_data_subset_visit2_complete_cases$obesity), 
				obesity_model_basic$fitted.value))

lrm(obesity ~ age + agesq + gender_factor + year + ffq_factor ,
		data=cbind_visit2_visit1)$stats[[10]]



#test the difference in the AUC for visit1
for ( AUC_diff1 in c(1:16))
{
	for ( AUC_diff2 in c(1:16)){
		
		if(roc.test(auc_visit1[[AUC_diff1]], auc_visit1[[AUC_diff2]])$p.value >= 0.05){
		
			message(paste0("model comparison: ", AUC_diff1, " , " , AUC_diff2))
		
			message(paste0("AUC difference significance: ", roc.test(auc_visit1[[AUC_diff1]], auc_visit1[[AUC_diff2]])$p.value))
		}
		
	}
	
}
