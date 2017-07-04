library(nnet)
library(stargazer)
library(pROC)
library(car)

#load entire cleaned data (154009 lines)
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


#make a diet score 

#take only complete cases

# independent....44735

VIP_data_independant_complete_cases <- na.omit(VIP_data_independant[,c("Subject_id","bmi","bmi_norm_sd","age","agesq","gender_factor","year","ffq_factor",
						"basic_residuals_bmi", "g6", "POLYsum1_TEI_adjusted_norm_sd","MONOsum1_TEI_adjusted_norm_sd","mfetsum1_TEI_adjusted_norm_sd",
						"fettsum1_TEI_adjusted_norm_sd","sacksum1_TEI_adjusted_norm_sd","kolhsum1_TEI_adjusted_norm_sd","FA_TEI_adjusted_norm_sd",
						"protsum1_anim_TEI_adjusted_norm_sd","protsum1_veg_TEI_adjusted_norm_sd",
						"fibesum1_TEI_adjusted_norm_sd","DISAsum1_TEI_adjusted_norm_sd","MOSAsum1_TEI_adjusted_norm_sd","TRANSsum1_TEI_adjusted_norm_sd",
						"NATRsum1_TEI_adjusted_norm_sd","kolesum1_TEI_adjusted_norm_sd","ensum1_norm_sd", "MAGNsum1_TEI_adjusted_norm_sd","FOSFsum1_TEI_adjusted_norm_sd",
						"selesum1_TEI_adjusted_norm_sd","ZINCsum1_TEI_adjusted_norm_sd","retisum1_TEI_adjusted_norm_sd",
						"karosum1_TEI_adjusted_norm_sd","TIAMsum1_TEI_adjusted_norm_sd","Folasum1_TEI_adjusted_norm_sd",
						"B2sum1_TEI_adjusted_norm_sd","NIACsum1_TEI_adjusted_norm_sd","B6sum1_TEI_adjusted_norm_sd","B12sum1_TEI_adjusted_norm_sd",
						"askosum1_TEI_adjusted_norm_sd","Dsum1_TEI_adjusted_norm_sd","tokosum1_TEI_adjusted_norm_sd",
						"VITKsum1_TEI_adjusted_norm_sd","jernsum1_TEI_adjusted_norm_sd","JODIsum1_TEI_adjusted_norm_sd",
						"kalcsum1_TEI_adjusted_norm_sd","KALIsum1_TEI_adjusted_norm_sd","enummer")])


#visit1 & visit2....30380

VIP_data_subset_visit1_complete_cases <- na.omit(VIP_data_subset_visit1[,c("Subject_id","bmi","bmi_norm_sd","age","agesq","gender_factor","year","ffq_factor",
						"basic_residuals_bmi","g6",
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
						"kalcsum1_TEI_adjusted_norm_sd","KALIsum1_TEI_adjusted_norm_sd","enummer")])
VIP_data_subset_visit1_complete_cases$bmi_category[VIP_data_subset_visit1_complete_cases$bmi < 25]<-0
VIP_data_subset_visit1_complete_cases$bmi_category[VIP_data_subset_visit1_complete_cases$bmi >= 25 & VIP_data_subset_visit1_complete_cases$bmi < 30]<-1
VIP_data_subset_visit1_complete_cases$bmi_category[VIP_data_subset_visit1_complete_cases$bmi >= 30]<-2
VIP_data_subset_visit1_complete_cases$bmi_category<-as.factor(VIP_data_subset_visit1_complete_cases$bmi_category)


VIP_data_subset_visit2_complete_cases <- na.omit(VIP_data_subset_visit2[,c("Subject_id","bmi","bmi_norm_sd","age","agesq","gender_factor","year","ffq_factor",
						"basic_residuals_bmi","g6",
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
						"kalcsum1_TEI_adjusted_norm_sd","KALIsum1_TEI_adjusted_norm_sd","enummer")])
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

multiple_coefficients<-glm(scale(basic_residuals_bmi)~POLYsum1_TEI_adjusted_norm_sd+MONOsum1_TEI_adjusted_norm_sd+mfetsum1_TEI_adjusted_norm_sd+
				fettsum1_TEI_adjusted_norm_sd+sacksum1_TEI_adjusted_norm_sd+kolhsum1_TEI_adjusted_norm_sd+FA_TEI_adjusted_norm_sd+
				protsum1_anim_TEI_adjusted_norm_sd+protsum1_veg_TEI_adjusted_norm_sd+
				fibesum1_TEI_adjusted_norm_sd+DISAsum1_TEI_adjusted_norm_sd+MOSAsum1_TEI_adjusted_norm_sd+TRANSsum1_TEI_adjusted_norm_sd+
				NATRsum1_TEI_adjusted_norm_sd+kolesum1_TEI_adjusted_norm_sd+ensum1_norm_sd+ MAGNsum1_TEI_adjusted_norm_sd+
				FOSFsum1_TEI_adjusted_norm_sd+selesum1_TEI_adjusted_norm_sd+ZINCsum1_TEI_adjusted_norm_sd+retisum1_TEI_adjusted_norm_sd+
				karosum1_TEI_adjusted_norm_sd+TIAMsum1_TEI_adjusted_norm_sd+Folasum1_TEI_adjusted_norm_sd+B2sum1_TEI_adjusted_norm_sd+
				NIACsum1_TEI_adjusted_norm_sd+B6sum1_TEI_adjusted_norm_sd+B12sum1_TEI_adjusted_norm_sd+askosum1_TEI_adjusted_norm_sd+
				Dsum1_TEI_adjusted_norm_sd+tokosum1_TEI_adjusted_norm_sd+VITKsum1_TEI_adjusted_norm_sd+jernsum1_TEI_adjusted_norm_sd+
				JODIsum1_TEI_adjusted_norm_sd+kalcsum1_TEI_adjusted_norm_sd+KALIsum1_TEI_adjusted_norm_sd, family=gaussian(link="identity"))$coefficients[-1]

multiple_p_values<-summary(glm(scale(basic_residuals_bmi)~POLYsum1_TEI_adjusted_norm_sd+MONOsum1_TEI_adjusted_norm_sd+mfetsum1_TEI_adjusted_norm_sd+
						fettsum1_TEI_adjusted_norm_sd+sacksum1_TEI_adjusted_norm_sd+kolhsum1_TEI_adjusted_norm_sd+FA_TEI_adjusted_norm_sd+
						protsum1_anim_TEI_adjusted_norm_sd+protsum1_veg_TEI_adjusted_norm_sd+
						fibesum1_TEI_adjusted_norm_sd+DISAsum1_TEI_adjusted_norm_sd+MOSAsum1_TEI_adjusted_norm_sd+TRANSsum1_TEI_adjusted_norm_sd+
						NATRsum1_TEI_adjusted_norm_sd+kolesum1_TEI_adjusted_norm_sd+ensum1_norm_sd+ MAGNsum1_TEI_adjusted_norm_sd+
						FOSFsum1_TEI_adjusted_norm_sd+selesum1_TEI_adjusted_norm_sd+ZINCsum1_TEI_adjusted_norm_sd+retisum1_TEI_adjusted_norm_sd+
						karosum1_TEI_adjusted_norm_sd+TIAMsum1_TEI_adjusted_norm_sd+Folasum1_TEI_adjusted_norm_sd+B2sum1_TEI_adjusted_norm_sd+
						NIACsum1_TEI_adjusted_norm_sd+B6sum1_TEI_adjusted_norm_sd+B12sum1_TEI_adjusted_norm_sd+askosum1_TEI_adjusted_norm_sd+
						Dsum1_TEI_adjusted_norm_sd+tokosum1_TEI_adjusted_norm_sd+VITKsum1_TEI_adjusted_norm_sd+jernsum1_TEI_adjusted_norm_sd+
						JODIsum1_TEI_adjusted_norm_sd+kalcsum1_TEI_adjusted_norm_sd+KALIsum1_TEI_adjusted_norm_sd, family=gaussian(link="identity")))$coefficients[113:148]

#dont rerun with significant only,since the rest of the variables might not effect bmi, but effect the rest of the variables
#multiple_coefficients2<-glm(basic_residuals_bmi~POLYsum1_TEI_adjusted_norm_sd+mfetsum1_TEI_adjusted_norm_sd+sacksum1_TEI_adjusted_norm_sd+kolhsum1_TEI_adjusted_norm_sd+
#				FA_TEI_adjusted_norm_sd+protsum1_anim_TEI_adjusted_norm_sd+fibesum1_TEI_adjusted_norm_sd+DISAsum1_TEI_adjusted_norm_sd+TRANSsum1_TEI_adjusted_norm_sd+
#				NATRsum1_TEI_adjusted_norm_sd+ensum1_norm_sd+ MAGNsum1_TEI_adjusted_norm_sd+FOSFsum1_TEI_adjusted_norm_sd+selesum1_TEI_adjusted_norm_sd+
#				ZINCsum1_TEI_adjusted_norm_sd+retisum1_TEI_adjusted_norm_sd+TIAMsum1_TEI_adjusted_norm_sd+Folasum1_TEI_adjusted_norm_sd+B2sum1_TEI_adjusted_norm_sd+
#				NIACsum1_TEI_adjusted_norm_sd+B6sum1_TEI_adjusted_norm_sd+askosum1_TEI_adjusted_norm_sd+jernsum1_TEI_adjusted_norm_sd+
#				JODIsum1_TEI_adjusted_norm_sd+kalcsum1_TEI_adjusted_norm_sd+KALIsum1_TEI_adjusted_norm_sd, family=gaussian(link="identity"))$coefficients[-1]


detach(VIP_data_independant_complete_cases)

multiple_coefficients[multiple_p_values>=0.05]<-0

#multiple_coefficients[multiple_p_values<0.05]<-multiple_coefficients2 #only if we rerun with significant only

VIP_data_independant_complete_cases$diet_score<-apply(as.matrix(VIP_data_independant_complete_cases[,11:46])%*%
				diag(multiple_coefficients),1,sum,na.rm=T)

VIP_data_subset_visit1_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit1_complete_cases[,11:46])%*%
				diag(multiple_coefficients),1,sum,na.rm=T)

VIP_data_subset_visit2_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit2_complete_cases[,11:46])%*%
				diag(multiple_coefficients),1,sum,na.rm=T)


#scale diet score
VIP_data_independant_complete_cases$diet_score_scale<-scale(VIP_data_independant_complete_cases$diet_score)
VIP_data_subset_visit1_complete_cases$diet_score_scale<-scale(VIP_data_subset_visit1_complete_cases$diet_score)
VIP_data_subset_visit2_complete_cases$diet_score_scale<-scale(VIP_data_subset_visit2_complete_cases$diet_score)


#subtract 1 from PA and scale it 
VIP_data_independant_complete_cases$PA_scale<-scale(VIP_data_independant_complete_cases$g6-1)
VIP_data_subset_visit1_complete_cases$PA_scale<-scale(VIP_data_subset_visit1_complete_cases$g6-1)
VIP_data_subset_visit2_complete_cases$PA_scale<-scale(VIP_data_subset_visit2_complete_cases$g6-1)




#now take diet and PA and get the effect sizes for each and make a final environment score
attach(VIP_data_independant_complete_cases)

full_model<-lm(scale(basic_residuals_bmi)~diet_score_scale+PA_scale)
#
#> summary(full_model)
#Call:
#		lm(formula = basic_residuals_bmi ~ diet_score_scale + PA_scale)
#
#Residuals:
#		Min      1Q  Median      3Q     Max 
#-3.5309 -0.6429 -0.0802  0.5422  6.1060 
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      -0.003038   0.004523  -0.672    0.502    
#diet_score_scale  0.201390   0.004523  44.525   <2e-16 ***
#		PA_scale         -0.111493   0.004523 -24.650   <2e-16 ***
#		---
#		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.9566 on 44732 degrees of freedom
#Multiple R-squared:  0.05426,	Adjusted R-squared:  0.05421 
#F-statistic:  1283 on 2 and 44732 DF,  p-value: < 2.2e-16
#



multiple_coefficients<-full_model$coefficients[-1]
#they are all significant


#make boxplots for each component and look at the values

VIP_data_independant_complete_cases$bmi_category[VIP_data_independant_complete_cases$bmi < 25]<-0
VIP_data_independant_complete_cases$bmi_category[VIP_data_independant_complete_cases$bmi >= 25 & VIP_data_independant_complete_cases$bmi < 30]<-1
VIP_data_independant_complete_cases$bmi_category[VIP_data_independant_complete_cases$bmi >= 30]<-2
VIP_data_independant_complete_cases$bmi_category<-as.factor(VIP_data_independant_complete_cases$bmi_category)

#diet
png("../Results/environment_risk_score/diet_score_independent_data.png",width=1000,height=800)
boxplot(diet_score_scale~VIP_data_independant_complete_cases$bmi_category,
main=" Diet score per\nBMI category in independent data",xlab="BMI categories",ylab="diet score",
col=c("gray80","gray60","gray40"), xaxt='n')
axis(side=1, at=c(1,2,3),labels=c("normal","overweight","obese"))
dev.off()

#PA
png("../Results/environment_risk_score/PA_independent_data.png",width=1000,height=800)
boxplot(bmi_norm_sd~PA_scale,
		main="continous BMI per PA indication in independent data",xlab="PA indication",ylab="continous BMI",
		col=c("gray90","gray75","gray60","gray45","gray30"), xaxt='n')
axis(side=1, at=c(1,2,3,4,5),labels=c("never","occasionaly","1-2 times/week","2-3 times/week","3+ times/week"))
dev.off()

#check the same, but when doing the multiplication

#diet
min((diet_score_scale*multiple_coefficients[1])) # -1.508262
max((diet_score_scale*multiple_coefficients[1])) # 1.213135


#PA
unique(sort(PA_scale*multiple_coefficients[2]))
#0.11260205  0.02943515 -0.05373175 -0.13689864 -0.22006554



detach(VIP_data_independant_complete_cases)


#construct a final environment score and do the test, the environment score needs to be scaled

VIP_data_independant_complete_cases$environment_score<-apply(as.matrix(VIP_data_independant_complete_cases[,c(49,50)])%*%
				diag(multiple_coefficients),1,sum,na.rm=T)
VIP_data_independant_complete_cases$environment_score_scaled<-scale(VIP_data_independant_complete_cases$environment_score)

VIP_data_subset_visit1_complete_cases$environment_score<-apply(as.matrix(VIP_data_subset_visit1_complete_cases[,c(50,51)])%*%
				diag(multiple_coefficients),1,sum,na.rm=T)
VIP_data_subset_visit1_complete_cases$environment_score_scaled<-scale(VIP_data_subset_visit1_complete_cases$environment_score)

VIP_data_subset_visit2_complete_cases$environment_score<-apply(as.matrix(VIP_data_subset_visit2_complete_cases[,c(50,51)])%*%
				diag(multiple_coefficients),1,sum,na.rm=T)
VIP_data_subset_visit2_complete_cases$environment_score_scaled<-scale(VIP_data_subset_visit2_complete_cases$environment_score)


#check the continous in independent, visit1 and visit2
attach(VIP_data_independant_complete_cases)

full_model<-lm(scale(basic_residuals_bmi)~environment_score_scaled)
#> summary(full_model)
#Call:
#		lm(formula = basic_residuals_bmi ~ environment_score)
#
#Residuals:
#		Min      1Q  Median      3Q     Max 
#-3.5309 -0.6429 -0.0802  0.5422  6.1060 
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       -0.003038   0.004523  -0.672    0.502    
#environment_score  1.000000   0.019740  50.658   <2e-16 ***
#		---
#		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.9566 on 44733 degrees of freedom
#Multiple R-squared:  0.05426,	Adjusted R-squared:  0.05424 
#F-statistic:  2566 on 1 and 44733 DF,  p-value: < 2.2e-16
#
VIP_data_independant_complete_cases$bmi_environment_score_fitted<-environment_score_scaled*full_model$coefficients[2]


detach(VIP_data_independant_complete_cases)

attach(VIP_data_subset_visit1_complete_cases)
full_model<-lm(scale(basic_residuals_bmi)~environment_score_scaled)

#> summary(full_model)
#Call:
#		lm(formula = basic_residuals_bmi ~ environment_score)
#
#Residuals:
#		Min      1Q  Median      3Q     Max 
#-3.5009 -0.6365 -0.0761  0.5471  7.7551 
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       -0.002094   0.005484  -0.382    0.703    
#environment_score  0.843124   0.023881  35.306   <2e-16 ***
#		---
#		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.9558 on 30378 degrees of freedom
#Multiple R-squared:  0.03942,	Adjusted R-squared:  0.03938 
#F-statistic:  1247 on 1 and 30378 DF,  p-value: < 2.2e-16
#
#
VIP_data_subset_visit1_complete_cases$bmi_environment_score_fitted<-environment_score_scaled*full_model$coefficients[2]


detach(VIP_data_subset_visit1_complete_cases)
attach(VIP_data_subset_visit2_complete_cases)
full_model<-lm(scale(basic_residuals_bmi)~environment_score_scaled)
#Call:
#		lm(formula = basic_residuals_bmi ~ environment_score)
#
#Residuals:
#		Min      1Q  Median      3Q     Max 
#-3.9463 -0.6360 -0.0707  0.5522  6.4219 
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       -0.001526   0.005516  -0.277    0.782    
#environment_score  1.009263   0.024043  41.977   <2e-16 ***
#		---
#		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.9614 on 30378 degrees of freedom
#Multiple R-squared:  0.05482,	Adjusted R-squared:  0.05479 
#F-statistic:  1762 on 1 and 30378 DF,  p-value: < 2.2e-16
#
#

VIP_data_subset_visit2_complete_cases$bmi_environment_score_fitted<-environment_score_scaled*full_model$coefficients[2]


detach(VIP_data_subset_visit2_complete_cases)


#model bmi categories to get predictive ability and such

#visit1
#model bmi categories

bmi_category_model_multi_log_reg<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor + environment_score,
		data=VIP_data_subset_visit1_complete_cases)

stargazer(bmi_category_model_multi_log_reg,type="text")
##> stargazer(bmi_category_model_multi_log_reg,type="text")
#==============================================
#		Dependent variable:     
#		----------------------------
#		1              2      
#(1)            (2)     
#----------------------------------------------
#		age                 -0.065***      -0.086***  
#		(0.022)        (0.004)   
#
#agesq                0.001***      0.001***   
#		(0.0003)      (0.0001)   
#
#gender_factor2      -0.924***      -0.400***  
#		(0.024)        (0.005)   
#
#year                 0.023***      0.056***   
#		(0.0002)      (0.0001)   
#
#ffq_factor1         -0.111***      -0.216***  
#		(0.024)        (0.005)   
#
#environment_score    1.355***      2.784***   
#		(0.003)        (0.001)   
#
#Constant            -45.707***    -111.410*** 
#		(0.0001)      (0.00002)  
#
#----------------------------------------------
#		Akaike Inf. Crit.   52,308.790    52,308.790  
#==============================================
#		Note:              *p<0.1; **p<0.05; ***p<0.01
#
# calculate average AUC for classes
auc_visit1_01[[2]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,1],bmi_category_model_multi_log_reg$fitted.value[,1]))
auc_visit1_02[[2]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,2],bmi_category_model_multi_log_reg$fitted.value[,2]))
auc_visit1_12[[2]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,3],bmi_category_model_multi_log_reg$fitted.value[,3]))


mean(auc_visit1_01[[2]],auc_visit1_02[[2]],auc_visit1_12[[2]])
#0.6546982
1-logLik(bmi_category_model_multi_log_reg)/likelihood_null_model_visit1
#0.04892033


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

png("../Results/environment_risk_score/visit1_bmi_categories_vs_environment_score.png",width=1500,height=750)
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
#		age                   0.004          0.002    
#(0.023)        (0.010)   
#
#agesq                 0.0001       -0.00001   
#(0.0002)      (0.0001)   
#
#gender_factor2      -0.906***      -0.553***  
#		(0.020)        (0.009)   
#
#year                 0.011***      0.052***   
#		(0.0003)      (0.0001)   
#
#ffq_factor1          0.698***      -0.307***  
#		(0.00003)      (0.00001)  
#
#environment_score    1.302***      2.956***   
#		(0.001)        (0.001)   
#
#Constant            -22.269***    -104.285*** 
#		(0.00001)      (0.00000)  
#
#----------------------------------------------
#		Akaike Inf. Crit.   58,811.200    58,811.200  
#==============================================
#		Note:              *p<0.1; **p<0.05; ***p<0.01
#
		

# calculate average AUC for classes
auc_visit2_01[[2]]<-pROC::auc(pROC::roc(bmi_classes_visit2[,1],bmi_category_model_multi_log_reg$fitted.value[,1]))
auc_visit2_02[[2]]<-pROC::auc(pROC::roc(bmi_classes_visit2[,2],bmi_category_model_multi_log_reg$fitted.value[,2]))
auc_visit2_12[[2]]<-pROC::auc(pROC::roc(bmi_classes_visit2[,3],bmi_category_model_multi_log_reg$fitted.value[,3]))


mean(auc_visit2_01[[2]],auc_visit2_02[[2]],auc_visit2_12[[2]])
# 0.6485699

1-logLik(bmi_category_model_multi_log_reg)/likelihood_null_model_visit2
# 0.044014


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
		
png("../Results/environment_risk_score/visit2_bmi_categories_vs_environment_score.png",width=1500,height=750)
boxplot(VIP_data_subset_visit2_complete_cases$environment_score~VIP_data_subset_visit2_complete_cases$bmi_category,
		main=" Environment score per\nBMI category in second visit",xlab="BMI categories",ylab="environment score",
		col=c("gray80","gray60","gray40"), xaxt='n')
axis(side=1, at=c(1,2,3),labels=c("normal","overweight","obese"))
dev.off()


VIP_data_subset_visit1_complete_cases$bmi_residuals_scaled<-scale(VIP_data_subset_visit1_complete_cases$basic_residuals_bmi)
VIP_data_subset_visit2_complete_cases$bmi_residuals_scaled<-scale(VIP_data_subset_visit2_complete_cases$basic_residuals_bmi)


#create a variable of ratio of scaled bmi_residuals and the fitted values of the scaled environment score, but make them positive first, by adding the minimum of the 
#two mulitplied by minus one, also add a little to avoid division with zero or very small numbers
VIP_data_subset_visit1_complete_cases$fitted_values_positive<-VIP_data_subset_visit1_complete_cases$bmi_environment_score_fitted+(-1)*
		min(c(VIP_data_subset_visit1_complete_cases$bmi_environment_score_fitted, VIP_data_subset_visit1_complete_cases$bmi_residuals_scaled))+1

VIP_data_subset_visit2_complete_cases$fitted_values_positive<-VIP_data_subset_visit2_complete_cases$bmi_environment_score_fitted+(-1)*
		min(c(VIP_data_subset_visit2_complete_cases$bmi_environment_score_fitted, VIP_data_subset_visit2_complete_cases$bmi_residuals_scaled))+1

VIP_data_subset_visit1_complete_cases$bmi_residuals_scaled_positive<-VIP_data_subset_visit1_complete_cases$bmi_residuals_scaled+(-1)*
		min(c(VIP_data_subset_visit1_complete_cases$bmi_environment_score_fitted, VIP_data_subset_visit1_complete_cases$bmi_residuals_scaled))+1

VIP_data_subset_visit2_complete_cases$bmi_residuals_scaled_positive<-VIP_data_subset_visit2_complete_cases$bmi_residuals_scaled+(-1)*
		min(c(VIP_data_subset_visit2_complete_cases$bmi_environment_score_fitted, VIP_data_subset_visit2_complete_cases$bmi_residuals_scaled))+1

#divide
VIP_data_subset_visit1_complete_cases$continuous_lean_phenotype<-VIP_data_subset_visit1_complete_cases$bmi_residuals_scaled_positive/VIP_data_subset_visit1_complete_cases$fitted_values_positive
VIP_data_subset_visit2_complete_cases$continuous_lean_phenotype<-VIP_data_subset_visit2_complete_cases$bmi_residuals_scaled_positive/VIP_data_subset_visit2_complete_cases$fitted_values_positive

#save the datasets for further analysis in selection
write.csv(VIP_data_subset_visit1_complete_cases, "../VIP_data/VIP_data_subset_visit1_complete_cases.csv", row.names=FALSE, na="")
write.csv(VIP_data_subset_visit2_complete_cases, "../VIP_data/VIP_data_subset_visit2_complete_cases.csv", row.names=FALSE, na="")


