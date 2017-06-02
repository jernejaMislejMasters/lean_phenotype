library(nnet)
library(stargazer)
library(pROC)

#load entire cleaned data (154009 subjects)
VIP_data_all <- read.csv("../VIP_data/VIP_170206_cleaned.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)

#load the subset
VIP_data_subset <- read.csv("../VIP_data/VIP_170206_cleaned_subset.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)
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

#load all variables, adjusted for TEI

#independent
source(file="load_variables_independent_dataset_TEI_adjusted.R")

#visit1
source(file="load_variables_visit1_dataset_TEI_adjusted.R")

#visit2
source(file="load_variables_visit2_dataset_TEI_adjusted.R")


#make a diet score with a OLS multi lm

#take only complete cases

# independent....44735

VIP_data_independant_complete_cases <- na.omit(VIP_data_independant[,c("Subject_id","bmi","bmi_norm_sd","age","agesq","gender_factor","year","ffq_factor",
						"basic_residuals_bmi", "alkosum1_TEI_adjusted_norm_sd", "g6", "POLYsum1_TEI_adjusted_norm_sd","MONOsum1_TEI_adjusted_norm_sd","mfetsum1_TEI_adjusted_norm_sd",
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


#visit1 & visit2....30380

VIP_data_subset_visit1_complete_cases <- na.omit(VIP_data_subset_visit1[,c("Subject_id","bmi","bmi_norm_sd","age","agesq","gender_factor","year","ffq_factor",
						"basic_residuals_bmi","alkosum1_TEI_adjusted_norm_sd","g6",
						"POLYsum1_TEI_adjusted_norm_sd","MONOsum1_TEI_adjusted_norm_sd","mfetsum1_TEI_adjusted_norm_sd",
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


VIP_data_subset_visit2_complete_cases <- na.omit(VIP_data_subset_visit2[,c("Subject_id","bmi","bmi_norm_sd","age","agesq","gender_factor","year","ffq_factor",
						"basic_residuals_bmi","alkosum1_TEI_adjusted_norm_sd","g6",
						"POLYsum1_TEI_adjusted_norm_sd","MONOsum1_TEI_adjusted_norm_sd","mfetsum1_TEI_adjusted_norm_sd",
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


#keep subjects that were in the complete cases in both visits
VIP_data_subset_visit2_complete_cases<-VIP_data_subset_visit2_complete_cases[VIP_data_subset_visit2_complete_cases$Subject_id %in% VIP_data_subset_visit1_complete_cases$Subject_id,]
VIP_data_subset_visit1_complete_cases<-VIP_data_subset_visit1_complete_cases[VIP_data_subset_visit1_complete_cases$Subject_id %in% VIP_data_subset_visit2_complete_cases$Subject_id,]

#one guy ends up with very low environment score and goes from being normal weight in visit 1 to overweight in visit2, he is excluded
VIP_data_subset_visit2_complete_cases<-VIP_data_subset_visit2_complete_cases[VIP_data_subset_visit2_complete_cases$Subject_id !=63335,]
VIP_data_subset_visit1_complete_cases<-VIP_data_subset_visit1_complete_cases[VIP_data_subset_visit1_complete_cases$Subject_id !=63335,]


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

auc_visit1_01 <- vector("list",2)
auc_visit1_02 <- vector("list",2)
auc_visit1_12 <- vector("list",2)

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

auc_visit2_01 <- vector("list",2)
auc_visit2_02 <- vector("list",2)
auc_visit2_12 <- vector("list",2)

#make basic model for comparison

#visit1
bmi_category_model_basic<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor,
		data=VIP_data_subset_visit1_complete_cases)

stargazer(bmi_category_model_basic,type="text")


# calculate average AUC for classes

auc_visit1_01[[1]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,1],bmi_category_model_basic$fitted.value[,1]))
auc_visit1_02[[1]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,2],bmi_category_model_basic$fitted.value[,2]))
auc_visit1_12[[1]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,3],bmi_category_model_basic$fitted.value[,3]))


mean(auc_visit1_01[[1]],auc_visit1_02[[1]],auc_visit1_12[[1]])#0.6201842


1-logLik(bmi_category_model_basic)/likelihood_null_model_visit1#0.02853794


#visit2
bmi_category_model_basic<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor,
		data=VIP_data_subset_visit2_complete_cases)

stargazer(bmi_category_model_basic,type="text")


# calculate average AUC for classes

auc_visit2_01[[1]]<-pROC::auc(pROC::roc(bmi_classes_visit2[,1],bmi_category_model_basic$fitted.value[,1]))
auc_visit2_02[[1]]<-pROC::auc(pROC::roc(bmi_classes_visit2[,2],bmi_category_model_basic$fitted.value[,2]))
auc_visit2_12[[1]]<-pROC::auc(pROC::roc(bmi_classes_visit2[,3],bmi_category_model_basic$fitted.value[,3]))


mean(auc_visit2_01[[1]],auc_visit2_02[[1]],auc_visit2_12[[1]])#0.6043139

1-logLik(bmi_category_model_basic)/likelihood_null_model_visit2#0.01997332






#make diet score variables, get effect sizes from the independent dataset first for diet
attach(VIP_data_independant_complete_cases)

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

VIP_data_independant_complete_cases$diet_score<-apply(as.matrix(VIP_data_independant_complete_cases[,12:47])%*%
				diag(multiple_coefficients),1,sum,na.rm=T)

VIP_data_subset_visit1_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit1_complete_cases[,12:47])%*%
				diag(multiple_coefficients),1,sum,na.rm=T)

VIP_data_subset_visit2_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit2_complete_cases[,12:47])%*%
				diag(multiple_coefficients),1,sum,na.rm=T)

#now take diet and PA and alcohol and get the effect sizes for each and make a final environment score
attach(VIP_data_independant_complete_cases)

full_model<-glm(basic_residuals_bmi~diet_score+g6+alkosum1_TEI_adjusted_norm_sd, family=gaussian(link="identity"))
#
#> summary(full_model)
#
#Call:
#		glm(formula = basic_residuals_bmi ~ diet_score + g6 + alkosum1_TEI_adjusted_norm_sd, 
#				family = gaussian(link = "identity"))
#
#Deviance Residuals: 
#		Min       1Q   Median       3Q      Max  
#-3.4940  -0.6421  -0.0768   0.5428   6.1189  
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                    0.188691   0.009142  20.640   <2e-16 ***
#		diet_score                     0.876992   0.020018  43.811   <2e-16 ***
#		g6                            -0.081397   0.003376 -24.107   <2e-16 ***
#		alkosum1_TEI_adjusted_norm_sd -0.041880   0.004598  -9.107   <2e-16 ***
#		---
#		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.9133826)
#
#Null deviance: 43281  on 44734  degrees of freedom
#Residual deviance: 40857  on 44731  degrees of freedom
#AIC: 122905
#
#Number of Fisher Scoring iterations: 2
#> vif(full_model)
#diet_score                            g6 
#1.005146                      1.003445 
#alkosum1_TEI_adjusted_norm_sd 
#1.008257 


multiple_coefficients<-full_model$coefficients[-1]
#they are all significant

detach(VIP_data_independant_complete_cases)


#construct a final environment score and do the test

VIP_data_independant_complete_cases$environment_score<-apply(as.matrix(VIP_data_independant_complete_cases[,c(48,11,10)])%*%
				diag(multiple_coefficients),1,sum,na.rm=T)

VIP_data_subset_visit1_complete_cases$environment_score<-apply(as.matrix(VIP_data_subset_visit1_complete_cases[,c(49,11,10)])%*%
				diag(multiple_coefficients),1,sum,na.rm=T)

VIP_data_subset_visit2_complete_cases$environment_score<-apply(as.matrix(VIP_data_subset_visit2_complete_cases[,c(49,11,10)])%*%
				diag(multiple_coefficients),1,sum,na.rm=T)

#check the continous in independent, visit1 and visit2
attach(VIP_data_independant_complete_cases)

full_model<-glm(basic_residuals_bmi~environment_score, family=gaussian(link="identity"))
#> summary(full_model)
#
#Call:
#		glm(formula = basic_residuals_bmi ~ environment_score, family = gaussian(link = "identity"))
#
#Deviance Residuals: 
#		Min       1Q   Median       3Q      Max  
#-3.4940  -0.6421  -0.0768   0.5428   6.1189  
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#		(Intercept)       0.188691   0.005854   32.23   <2e-16 ***
#		environment_score 1.000000   0.019411   51.52   <2e-16 ***
#		---
#		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.9133418)
#
#Null deviance: 43281  on 44734  degrees of freedom
#Residual deviance: 40857  on 44733  degrees of freedom
#AIC: 122901
#
#Number of Fisher Scoring iterations: 2
#
#> 
#		
#Residual standard error: 0.9557 on 44733 degrees of freedom
#Multiple R-squared:  0.05601,	Adjusted R-squared:  0.05599 
#F-statistic:  2654 on 1 and 44733 DF,  p-value: < 2.2e-16

detach(VIP_data_independant_complete_cases)

attach(VIP_data_subset_visit1_complete_cases)
full_model<-glm(basic_residuals_bmi~environment_score, family=gaussian(link="identity"))

#> summary(full_model)
#
#Call:
#		glm(formula = basic_residuals_bmi ~ environment_score, family = gaussian(link = "identity"))
#
#Deviance Residuals: 
#		Min       1Q   Median       3Q      Max  
#-3.6297  -0.6372  -0.0738   0.5496   7.7628  
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#		(Intercept)       0.148786   0.006911   21.53   <2e-16 ***
#		environment_score 0.866377   0.024164   35.85   <2e-16 ***
#		---
#		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.9124448)
#
#Null deviance: 28892  on 30380  degrees of freedom
#Residual deviance: 27719  on 30379  degrees of freedom
#AIC: 83438
#
#Number of Fisher Scoring iterations: 2
#
#> 
#		
#Residual standard error: 0.9552 on 30379 degrees of freedom
#Multiple R-squared:  0.04058,	Adjusted R-squared:  0.04055 
#F-statistic:  1285 on 1 and 30379 DF,  p-value: < 2.2e-16
#
detach(VIP_data_subset_visit1_complete_cases)
attach(VIP_data_subset_visit2_complete_cases)
full_model<-glm(basic_residuals_bmi~environment_score, family=gaussian(link="identity"))
#Call:
#		glm(formula = basic_residuals_bmi ~ environment_score, family = gaussian(link = "identity"))
#
#Deviance Residuals: 
#		Min       1Q   Median       3Q      Max  
#-3.9244  -0.6328  -0.0685   0.5544   6.4032  
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       0.189462   0.007098   26.69   <2e-16 ***
#		environment_score 1.002757   0.023464   42.69   <2e-16 ***
#		---
#		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.9225726)
#
#Null deviance: 29708  on 30380  degrees of freedom
#Residual deviance: 28027  on 30379  degrees of freedom
#AIC: 83773
#
#Number of Fisher Scoring iterations: 2
#Residual standard error: 0.9605 on 30379 degrees of freedom
#Multiple R-squared:  0.0566,	Adjusted R-squared:  0.05657 
#F-statistic:  1823 on 1 and 30379 DF,  p-value: < 2.2e-16
#
detach(VIP_data_subset_visit2_complete_cases)


#model bmi categories to get predictive ability and such

#visit1
#model bmi categories

#with all
bmi_category_model_multi_log_reg<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor + environment_score,
		data=VIP_data_subset_visit1_complete_cases)

stargazer(bmi_category_model_multi_log_reg,type="text")
#> stargazer(bmi_category_model_multi_log_reg,type="text")
#
#==============================================
#		Dependent variable:     
#		----------------------------
#		1              2      
#(1)            (2)     
#----------------------------------------------
#		age                 -0.062***      -0.080***  
#		(0.022)        (0.004)   
#
#agesq                0.001***      0.001***   
#		(0.0003)      (0.0001)   
#
#gender_factor2      -0.943***      -0.445***  
#		(0.023)        (0.005)   
#
#year                 0.022***      0.054***   
#		(0.0002)      (0.0001)   
#
#ffq_factor1         -0.123***      -0.236***  
#		(0.024)        (0.005)   
#
#environment_score    1.369***      2.893***   
#		(0.003)        (0.001)   
#
#Constant            -43.433***    -107.840*** 
#		(0.0001)      (0.00002)  
#
#----------------------------------------------
#		Akaike Inf. Crit.   52,271.360    52,271.360  
#==============================================
#		Note:              *p<0.1; **p<0.05; ***p<0.01
#> 
#		
#
# calculate average AUC for classes
auc_visit1_01[[2]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,1],bmi_category_model_multi_log_reg$fitted.value[,1]))
auc_visit1_02[[2]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,2],bmi_category_model_multi_log_reg$fitted.value[,2]))
auc_visit1_12[[2]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,3],bmi_category_model_multi_log_reg$fitted.value[,3]))


mean(auc_visit1_01[[2]],auc_visit1_02[[2]],auc_visit1_12[[2]])
#0.655383
1-logLik(bmi_category_model_multi_log_reg)/likelihood_null_model_visit1
#0.0496214


#check signficance of difference of AUC from the basic model
#combination 1
roc.test(auc_visit1_01[[1]], auc_visit1_01[[2]])
#DeLong's test for two correlated ROC curves
#		
#		data:  auc_visit1_01[[1]] and auc_visit1_01[[2]]
#		Z = -16.33, p-value < 2.2e-16
#		alternative hypothesis: true difference in AUC is not equal to 0
#		sample estimates:
#		AUC of roc1 AUC of roc2 
#		0.6191597   0.6553830 
#		
		
#combination 2
roc.test(auc_visit1_02[[1]], auc_visit1_02[[2]])
#DeLong's test for two correlated ROC curves
#		
#		data:  auc_visit1_02[[1]] and auc_visit1_02[[2]]
#		Z = -11.19, p-value < 2.2e-16
#		alternative hypothesis: true difference in AUC is not equal to 0
#		sample estimates:
#		AUC of roc1 AUC of roc2 
#		0.6177759   0.6384122 
#		
		
#combination 3
roc.test(auc_visit1_12[[1]], auc_visit1_12[[2]])
#DeLong's test for two correlated ROC curves
#		
#		data:  auc_visit1_12[[1]] and auc_visit1_12[[2]]
#		Z = -13.906, p-value < 2.2e-16
#		alternative hypothesis: true difference in AUC is not equal to 0
#		sample estimates:
#		AUC of roc1 AUC of roc2 
#		0.5666478   0.6544369 
		

#plot

png("../Results/environment_score/visit1_bmi_categories_vs_environment_score.png",width=1500,height=750)
boxplot(VIP_data_subset_visit1_complete_cases$environment_score~VIP_data_subset_visit1_complete_cases$bmi_category,
		main=" Environment score per\nBMI category in first visit",xlab="BMI categories",ylab="environment score",
		col=c("gray80","gray60","gray40"), xaxt='n')
axis(side=1, at=c(1,2,3),labels=c("normal","overweight","obese"))
dev.off()

#visit2
#model bmi categories

#with all
bmi_category_model_multi_log_reg<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor + environment_score,
		data=VIP_data_subset_visit2_complete_cases)

stargazer(bmi_category_model_multi_log_reg,type="text")
#> stargazer(bmi_category_model_multi_log_reg,type="text")
#
#==============================================
#		Dependent variable:     
#		----------------------------
#		1              2      
#(1)            (2)     
#----------------------------------------------
#		age                   0.008          0.012    
#(0.023)        (0.010)   
#
#agesq                 0.0001        -0.0001   
#(0.0002)      (0.0001)   
#
#gender_factor2      -0.916***      -0.590***  
#		(0.020)        (0.009)   
#
#year                 0.011***      0.051***   
#		(0.0003)      (0.0001)   
#
#ffq_factor1          0.692***      -0.334***  
#		(0.00003)      (0.00000)  
#
#environment_score    1.248***      2.976***   
#		(0.002)        (0.001)   
#
#Constant            -21.275***    -103.591*** 
#		(0.00001)      (0.00000)  
#
#----------------------------------------------
#		Akaike Inf. Crit.   58,746.880    58,746.880  
#==============================================
#		Note:              *p<0.1; **p<0.05; ***p<0.01


# calculate average AUC for classes
auc_visit2_01[[2]]<-pROC::auc(pROC::roc(bmi_classes_visit2[,1],bmi_category_model_multi_log_reg$fitted.value[,1]))
auc_visit2_02[[2]]<-pROC::auc(pROC::roc(bmi_classes_visit2[,2],bmi_category_model_multi_log_reg$fitted.value[,2]))
auc_visit2_12[[2]]<-pROC::auc(pROC::roc(bmi_classes_visit2[,3],bmi_category_model_multi_log_reg$fitted.value[,3]))


mean(auc_visit2_01[[2]],auc_visit2_02[[2]],auc_visit2_12[[2]])
# 0.6491333

1-logLik(bmi_category_model_multi_log_reg)/likelihood_null_model_visit2
#0.04508734


#check signficance of difference of AUC from the basic model
#combination 1
roc.test(auc_visit2_01[[1]], auc_visit2_01[[2]])
#DeLong's test for two correlated ROC curves
#		
#		data:  auc_visit2_01[[1]] and auc_visit2_01[[2]]
#		Z = -17.934, p-value < 2.2e-16
#		alternative hypothesis: true difference in AUC is not equal to 0
#		sample estimates:
#		AUC of roc1 AUC of roc2 
#		0.6042948   0.6491333 
		
#combination 2
roc.test(auc_visit2_02[[1]], auc_visit2_02[[2]])
#DeLong's test for two correlated ROC curves
#		
#		data:  auc_visit2_02[[1]] and auc_visit2_02[[2]]
#		Z = -5.6039, p-value = 2.096e-08
#		alternative hypothesis: true difference in AUC is not equal to 0
#		sample estimates:
#		AUC of roc1 AUC of roc2 
#		0.6044150   0.6113651 
		
#combination 3
roc.test(auc_visit2_12[[1]], auc_visit2_12[[2]])
#DeLong's test for two correlated ROC curves
#		
#		data:  auc_visit2_12[[1]] and auc_visit2_12[[2]]
#		Z = -20.903, p-value < 2.2e-16
#		alternative hypothesis: true difference in AUC is not equal to 0
#		sample estimates:
#		AUC of roc1 AUC of roc2 
#		0.5342924   0.6519206 
#		
		
png("../Results/environment_score/visit2_bmi_categories_vs_environment_score.png",width=1500,height=750)
boxplot(VIP_data_subset_visit2_complete_cases$environment_score~VIP_data_subset_visit2_complete_cases$bmi_category,
		main=" Environment score per\nBMI category in second visit",xlab="BMI categories",ylab="environment score",
		col=c("gray80","gray60","gray40"), xaxt='n')
axis(side=1, at=c(1,2,3),labels=c("normal","overweight","obese"))
dev.off()




#take z-scores of the evironment score and continous bmi for both visits

VIP_data_subset_visit1_complete_cases$environment_score_norm_sd<-scale(VIP_data_subset_visit1_complete_cases$environment_score)
VIP_data_subset_visit2_complete_cases$environment_score_norm_sd<-scale(VIP_data_subset_visit2_complete_cases$environment_score)

#plot results
d<-density(VIP_data_subset_visit1_complete_cases$environment_score_norm_sd)
png("../Results/environment_score/visit1_environment_z_score.png",width=1500,height=750)
plot(d, main=" Environment Z-score distribution in frist vist")
polygon(d, col="gray90", border="gray20") 
dev.off()

d<-density(VIP_data_subset_visit1_complete_cases$bmi_norm_sd)
png("../Results/environment_score/visit1_bmi_z_score.png",width=1500,height=750)
plot(d, main=" BMI Z-score distribution in first visit")
polygon(d, col="gray90", border="gray20") 
dev.off()


d<-density(VIP_data_subset_visit2_complete_cases$environment_score_norm_sd)
png("../Results/environment_score/visit2_environment_z_score.png",width=1500,height=750)
plot(d, main=" Environment Z-score distribution in second vist")
polygon(d, col="gray90", border="gray20") 
dev.off()

d<-density(VIP_data_subset_visit2_complete_cases$bmi_norm_sd)
png("../Results/environment_score/visit2_bmi_z_score.png",width=1500,height=750)
plot(d, main=" BMI Z-score distribution in second visit")
polygon(d, col="gray90", border="gray20") 
dev.off()


#create a variable of there ratio (bmi/environment)
VIP_data_subset_visit1_complete_cases$ratio_bmi_environment_z_score<-VIP_data_subset_visit1_complete_cases$bmi_norm_sd/VIP_data_subset_visit1_complete_cases$environment_score_norm_sd
VIP_data_subset_visit2_complete_cases$ratio_bmi_environment_z_score<-VIP_data_subset_visit2_complete_cases$bmi_norm_sd/VIP_data_subset_visit2_complete_cases$environment_score_norm_sd

#get subjects that have the ratio smaller then one...one might be a bit big
visit1_subjects<-VIP_data_subset_visit1_complete_cases$Subject_id[VIP_data_subset_visit1_complete_cases$ratio_bmi_environment_z_score<0.1]
length(visit1_subjects)#14459

#what is their bmi and environment like compared to those that are above one
summary(VIP_data_subset_visit1_complete_cases$environment_score_norm_sd[VIP_data_subset_visit1_complete_cases$ratio_bmi_environment_z_score<0.1])
summary(VIP_data_subset_visit1_complete_cases$bmi_norm_sd[VIP_data_subset_visit1_complete_cases$ratio_bmi_environment_z_score<1])

summary(VIP_data_subset_visit1_complete_cases$environment_score_norm_sd[VIP_data_subset_visit1_complete_cases$ratio_bmi_environment_z_score>=0.1])
summary(VIP_data_subset_visit1_complete_cases$bmi_norm_sd[VIP_data_subset_visit1_complete_cases$ratio_bmi_environment_z_score>=1])

#they are not the same but both cover all distribution

visit2_subjects<-VIP_data_subset_visit2_complete_cases$Subject_id[VIP_data_subset_visit2_complete_cases$ratio_bmi_environment_z_score<0.1]
length(visit2_subjects)#14083


persistently_lean_subjects<-visit2_subjects[visit2_subjects %in% visit1_subjects]
length(persistently_lean_subjects)#8253
