#load cleaned subset of subjects with the right two visits and first extract those that have all the values for the right variables
VIP_data <- read.csv("VIP_data/VIP_170206_cleaned_subset.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)


attach(VIP_data)


#no missing gender
#no missing age
#141 missing vikt
#101 missing langd
#no missing enkver
#no missing enkver2
#no missing exclude
#1977 missing ensum1....all the rest of *sum1 variables missing are the same as for the ensum1
#1977 missing protsum1
#1977 missing kolhsum1
#1977 missing sacksum1
#1977 missing fibesum1
#1977 missing fettsum1
#1977 missing MONOsum1
#1977 missing POLYsum1
#1977 missing FA183_sum1
#1977 missing FA182_sum1
#1977 missing FA205_sum1
#1977 missing FA226_sum1
#1768 missing FA204_sum1....except these, which are a subset
#1977 missing MAGNsum1
#1977 missing NATRsum1
#1977 missing FOSFsum1
#1977 missing selesum1
#1977 missing ZINCsum1
#1977 missing retisum1
#1977 missing TIAMsum1
#1977 missing POLYsum1
#1977 missing Folasum1
#1977 missing B2sum1
#1977 missing B6sum1
#1977 missing NIACsum1
#1977 missing B12sum1
#1977 missing askosum1
#1977 missing Dsum1
#1977 missing tokosum1
#1977 missing jernsum1
#1977 missing JODIsum1
#1977 missing kalcsum1
#1977 missing KALIsum1


#805 missing sm_status
#161 missing bmi
#no missing alkosum1
#936 missing g6
#51334 missing all daN  
#0 missing all datN except these:
#DAT12
#DAT16
#DAT18
#DAT25
#DAT29
#DAT31
#DAT35
#DAT50
#DAT65
#DAT66
# missing 17656

#4770 missing g1_a
#5306 missing g1_b
#4923 missing g1_c
#4868 missing g1_d
#2645 missing g3_a
#12453 missing g3_b


#explore the unhealthy, obesogenic environment, using all of the variables 

#create factor for the questionare
VIP_data$ffq<-0
VIP_data$ffq[enkver2=="long"]<-1
VIP_data$ffq[enkver2=="apri"]<-1
VIP_data$ffq<-as.factor(VIP_data$ffq)

#age squared
VIP_data$agesq<-age*age

#factorize gender and visit
VIP_data$gender<-as.factor(VIP_data$gender)
VIP_data$visit<-as.factor(VIP_data$visit)


#explore the diet
detach(VIP_data)
attach(VIP_data)

#create new values for some, others stay the same

#POLYsum1
PUFAE<-(100*9*POLYsum1)/ensum1

#MONOsum1
MUFAE<-(100*9*MONOsum1)/ensum1

#mfetsum1
SATFAT<-(100*9*mfetsum1)/ensum1

#fettsum1
TOTFAT<-(100*9*fettsum1)/ensum1

#acids
FA<-FA183_sum1 + FA205_sum1 + FA226_sum1 + FA182_sum1 + FA204_sum1
FA<-(100*9*FA)/ensum1

#kolhsum1
CARBOHYDRATES<-(100*4*kolhsum1)/ensum1

#sacksum1
SUGAR<-(100*4*sacksum1)/ensum1

#protsum1
PROTEIN<-(100*4*protsum1)/ensum1

#TRANSsum1
TRANFAT<-(100*9*TRANSsum1)/ensum1

#protsum1_anim
PROTEIN_ANIMAL<-(100*4*protsum1_anim)/ensum1

#kolesum1
CHOLESTEROL<-(100*4*kolesum1)/ensum1

#FULLKsum1
WHOLEGRAIN<-(100*4*FULLKsum1)/ensum1

#NATRsum1
SALT<-NATRsum1/1000

#check the directions of the associations:

#combined
#associations<-glm(log(bmi)~age + agesq + gender + year + ffq + visit+ PUFAE+ MUFAE + SATFAT + TOTFAT + SUGAR + CARBOHYDRATES + PROTEIN 
#	+ TRANFAT + FA + PROTEIN_ANIMAL + CHOLESTEROL + WHOLEGRAIN + SALT, family = gaussian(link = "identity"))


#-------------------------------------------------------------------------------------------------------------------------------

#> summary(associations)

#Call:
#glm(formula = log(bmi) ~ age + agesq + gender + year + ffq + 
#    visit + PUFAE + MUFAE + SATFAT + TOTFAT + SUGAR + CARBOHYDRATES + 
#    PROTEIN + TRANFAT + FA + PROTEIN_ANIMAL + CHOLESTEROL + WHOLEGRAIN + 
#    SALT, family = gaussian(link = "identity"))

#Deviance Residuals: 
#     Min        1Q    Median        3Q       Max  
#-0.57945  -0.09263  -0.01152   0.07955   1.04229  

#Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     1.484e+00  4.077e-01   3.641 0.000272 ***
#age             4.986e-03  7.255e-04   6.873 6.36e-12 ***
#agesq          -3.696e-05  7.693e-06  -4.804 1.56e-06 ***
#gender2        -3.955e-02  1.298e-03 -30.477  < 2e-16 ***
#year            5.883e-04  2.031e-04   2.896 0.003776 ** 
#ffq1           -6.250e-03  2.183e-03  -2.863 0.004202 ** 
#visit2          1.129e-02  1.887e-03   5.983 2.21e-09 ***
#PUFAE           1.026e-02  1.596e-03   6.429 1.29e-10 ***
#MUFAE           1.534e-03  5.716e-04   2.685 0.007262 ** 
#SATFAT         -8.434e-03  6.328e-04 -13.329  < 2e-16 ***
#TOTFAT          7.620e-03  5.475e-04  13.919  < 2e-16 ***
#SUGAR          -1.771e-03  3.034e-04  -5.836 5.38e-09 ***
#CARBOHYDRATES   4.351e-03  3.134e-04  13.884  < 2e-16 ***
#PROTEIN         1.939e-03  1.126e-03   1.722 0.085022 .  
#TRANFAT        -1.218e-02  2.108e-03  -5.779 7.54e-09 ***
#FA             -1.375e-02  1.421e-03  -9.681  < 2e-16 ***
#PROTEIN_ANIMAL  8.071e-03  1.018e-03   7.927 2.28e-15 ***
#CHOLESTEROL    -5.056e-01  1.092e-01  -4.632 3.64e-06 ***
#WHOLEGRAIN     -8.234e-04  1.018e-04  -8.086 6.27e-16 ***
#SALT            1.108e-02  9.130e-04  12.133  < 2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for gaussian family taken to be 0.0192282)

#    Null deviance: 1397.7  on 66854  degrees of freedom
#Residual deviance: 1285.1  on 66835  degrees of freedom
#  (2135 observations deleted due to missingness)
#AIC: -74421

#Number of Fisher Scoring iterations: 2
#-------------------------------------------------------------------------------------------------------------------------------


##baseline
#VIP_data_baseline<-VIP_data[visit==1,]
#detach(VIP_data)
#attach(VIP_data_baseline)
#
##POLYsum1
#PUFAE<-(100*9*POLYsum1)/ensum1
#
##MONOsum1
#MUFAE<-(100*9*MONOsum1)/ensum1
#
##mfetsum1
#SATFAT<-(100*9*mfetsum1)/ensum1
#
##fettsum1
#TOTFAT<-(100*9*fettsum1)/ensum1
#
##acids
#FA<-FA183_sum1 + FA205_sum1 + FA226_sum1 + FA182_sum1 + FA204_sum1
#FA<-(100*9*FA)/ensum1
#
##kolhsum1
#CARBOHYDRATES<-(100*4*kolhsum1)/ensum1
#
##sacksum1
#SUGAR<-(100*4*sacksum1)/ensum1
#
##protsum1
#PROTEIN<-(100*4*protsum1)/ensum1
#
##TRANSsum1
#TRANFAT<-(100*9*TRANSsum1)/ensum1
#
##protsum1_anim
#PROTEIN_ANIMAL<-(100*4*protsum1_anim)/ensum1
#
##kolesum1
#CHOLESTEROL<-(100*4*kolesum1)/ensum1
#
##FULLKsum1
#WHOLEGRAIN<-(100*4*FULLKsum1)/ensum1
#
##NATRsum1
#SALT<-NATRsum1/1000
#
#
#associations<-glm(log(bmi)~age + agesq + gender + year + ffq + PUFAE+ MUFAE + SATFAT + TOTFAT + SUGAR + CARBOHYDRATES + PROTEIN 
#				+ TRANFAT + FA + PROTEIN_ANIMAL + CHOLESTEROL + WHOLEGRAIN + SALT, family = gaussian(link = "identity"))
#
##-------------------------------------------------------------------------------------------------------------------------------
#
##> summary(associations)
##
##Call:
##		glm(formula = log(bmi) ~ age + agesq + gender + year + ffq + 
##						PUFAE + MUFAE + SATFAT + TOTFAT + SUGAR + CARBOHYDRATES + 
##						PROTEIN + TRANFAT + FA + PROTEIN_ANIMAL + CHOLESTEROL + WHOLEGRAIN + 
##						SALT, family = gaussian(link = "identity"))
##
##Deviance Residuals: 
##		Min        1Q    Median        3Q       Max  
##-0.45529  -0.08976  -0.01186   0.07588   1.05180  
##
##Coefficients:
##		Estimate Std. Error t value Pr(>|t|)    
##		(Intercept)     6.951e-02  7.952e-01   0.087 0.930344    
##		age            -5.259e-03  1.592e-03  -3.303 0.000956 ***
##		agesq           8.766e-05  1.875e-05   4.676 2.94e-06 ***
##		gender2        -4.372e-02  1.933e-03 -22.614  < 2e-16 ***
##		year            1.455e-03  3.976e-04   3.661 0.000252 ***
##		ffq1           -2.381e-03  2.885e-03  -0.825 0.409324    
##		PUFAE           9.862e-03  2.155e-03   4.577 4.73e-06 ***
##		MUFAE           1.612e-03  7.261e-04   2.220 0.026451 *  
##		SATFAT         -8.117e-03  8.706e-04  -9.323  < 2e-16 ***
##		TOTFAT          5.799e-03  7.876e-04   7.363 1.84e-13 ***
##		SUGAR          -6.924e-04  4.021e-04  -1.722 0.085082 .  
##		CARBOHYDRATES   2.860e-03  4.723e-04   6.056 1.41e-09 ***
##		PROTEIN         5.166e-03  1.646e-03   3.139 0.001698 ** 
##		TRANFAT        -9.193e-03  2.417e-03  -3.804 0.000143 ***
##		FA             -1.233e-02  1.904e-03  -6.475 9.60e-11 ***
##		PROTEIN_ANIMAL  4.363e-03  1.509e-03   2.892 0.003832 ** 
##		CHOLESTEROL    -5.330e-01  1.406e-01  -3.793 0.000149 ***
##		WHOLEGRAIN     -7.730e-04  1.355e-04  -5.707 1.16e-08 ***
##		SALT            8.505e-03  1.215e-03   6.999 2.63e-12 ***
##		---
##		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
##(Dispersion parameter for gaussian family taken to be 0.01767671)
##
##Null deviance: 628.02  on 33160  degrees of freedom
##Residual deviance: 585.84  on 33142  degrees of freedom
##(1334 observations deleted due to missingness)
##AIC: -39694
##
##Number of Fisher Scoring iterations: 2
##
##> 
##		
##-------------------------------------------------------------------------------------------------------------------------------
#
#
##follow up
#detach(VIP_data_baseline)
#attach(VIP_data)
#VIP_data_follow<-VIP_data[visit==2,]
#detach(VIP_data)
#attach(VIP_data_follow)
#
##POLYsum1
#PUFAE<-(100*9*POLYsum1)/ensum1
#
##MONOsum1
#MUFAE<-(100*9*MONOsum1)/ensum1
#
##mfetsum1
#SATFAT<-(100*9*mfetsum1)/ensum1
#
##fettsum1
#TOTFAT<-(100*9*fettsum1)/ensum1
#
##acids
#FA<-FA183_sum1 + FA205_sum1 + FA226_sum1 + FA182_sum1 + FA204_sum1
#FA<-(100*9*FA)/ensum1
#
##kolhsum1
#CARBOHYDRATES<-(100*4*kolhsum1)/ensum1
#
##sacksum1
#SUGAR<-(100*4*sacksum1)/ensum1
#
##protsum1
#PROTEIN<-(100*4*protsum1)/ensum1
#
##TRANSsum1
#TRANFAT<-(100*9*TRANSsum1)/ensum1
#
##protsum1_anim
#PROTEIN_ANIMAL<-(100*4*protsum1_anim)/ensum1
#
##kolesum1
#CHOLESTEROL<-(100*4*kolesum1)/ensum1
#
##FULLKsum1
#WHOLEGRAIN<-(100*4*FULLKsum1)/ensum1
#
##NATRsum1
#SALT<-NATRsum1/1000
#
#
#associations<-glm(log(bmi)~age + agesq + gender + year + ffq + PUFAE+ MUFAE + SATFAT + TOTFAT + SUGAR + CARBOHYDRATES + PROTEIN 
#				+ TRANFAT + FA + PROTEIN_ANIMAL + CHOLESTEROL + WHOLEGRAIN + SALT, family = gaussian(link = "identity"))
#
##-------------------------------------------------------------------------------------------------------------------------------
##> summary(associations)
##
##Call:
##		glm(formula = log(bmi) ~ age + agesq + gender + year + ffq + 
##						PUFAE + MUFAE + SATFAT + TOTFAT + SUGAR + CARBOHYDRATES + 
##						PROTEIN + TRANFAT + FA + PROTEIN_ANIMAL + CHOLESTEROL + WHOLEGRAIN + 
##						SALT, family = gaussian(link = "identity"))
##
##Deviance Residuals: 
##		Min        1Q    Median        3Q       Max  
##-0.57579  -0.09590  -0.01131   0.08316   0.98868  
##
##Coefficients:
##		Estimate Std. Error t value Pr(>|t|)    
##		(Intercept)     1.897e+00  5.732e-01   3.309 0.000938 ***
##		age             1.815e-03  1.830e-03   0.992 0.321311    
##		agesq          -8.120e-06  1.779e-05  -0.456 0.648050    
##		gender2        -3.560e-02  1.884e-03 -18.897  < 2e-16 ***
##		year            3.797e-04  2.861e-04   1.327 0.184458    
##		ffq1            1.859e-02  1.731e-02   1.074 0.282924    
##		PUFAE           9.444e-03  2.470e-03   3.823 0.000132 ***
##		MUFAE           3.307e-03  1.176e-03   2.812 0.004921 ** 
##		SATFAT         -8.258e-03  1.055e-03  -7.829 5.07e-15 ***
##		TOTFAT          8.315e-03  8.828e-04   9.419  < 2e-16 ***
##		SUGAR          -3.103e-03  4.770e-04  -6.506 7.84e-11 ***
##		CARBOHYDRATES   5.711e-03  4.309e-04  13.253  < 2e-16 ***
##		PROTEIN         1.331e-04  1.594e-03   0.084 0.933436    
##		TRANFAT        -1.320e-02  5.599e-03  -2.357 0.018418 *  
##		FA             -1.351e-02  2.252e-03  -5.999 2.01e-09 ***
##		PROTEIN_ANIMAL  1.009e-02  1.412e-03   7.145 9.19e-13 ***
##		CHOLESTEROL    -3.940e-01  1.745e-01  -2.258 0.023962 *  
##		WHOLEGRAIN     -9.997e-04  1.554e-04  -6.432 1.27e-10 ***
##		SALT            1.306e-02  1.381e-03   9.456  < 2e-16 ***
##		---
##		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
##(Dispersion parameter for gaussian family taken to be 0.02066975)
##
##Null deviance: 737.47  on 33693  degrees of freedom
##Residual deviance: 696.05  on 33675  degrees of freedom
##(801 observations deleted due to missingness)
##AIC: -35061
##
##Number of Fisher Scoring iterations: 2
##
##> 
##-------------------------------------------------------------------------------------------------------------------------------
#
#
#detach(VIP_data_follow)
#attach(VIP_data)

#Sat fat, trans fat, sugar and cholesterol have a unsensible direction of association in all and protein total is insignificant in the combined and in the follow up,
#	not sure if it could be due to also including the animal protein which contains subinformation, if I exclude the animal protein then the protein
#	positevly associated and significant


#take the ones which are sensible and significant

#construct the diet score, look at the upper limit only, since we are focusing on the obesogenic unhealthy environment
#if no limit cutpoints given, make tertiles and rank accordingly


#POLYsum1
VIP_data$POLYsum1_score[!is.na(PUFAE)]<-0
VIP_data$POLYsum1_score[!is.na(PUFAE) & (PUFAE >= 5) & (PUFAE <= 10) ]<-1
VIP_data$POLYsum1_score[!is.na(PUFAE) & PUFAE > 10]<-2

#MONOsum1
VIP_data$MONOsum1_score[!is.na(MUFAE)]<-0
VIP_data$MONOsum1_score[!is.na(MUFAE) & (MUFAE >= 10) & (MUFAE <= 20) ]<-1
VIP_data$MONOsum1_score[!is.na(MUFAE) & MUFAE > 20]<-2

#fettsum1
VIP_data$fettsum1_score[!is.na(TOTFAT)]<-2
VIP_data$fettsum1_score[!is.na(TOTFAT) & (TOTFAT >= 25) & (TOTFAT <= 40) ]<-1
VIP_data$fettsum1_score[!is.na(TOTFAT) & TOTFAT > 40]<-0

#NATRsum1
VIP_data$NATRsum1_score[!is.na(NATRsum1)]<-1
SALT<-NATRsum1/1000
VIP_data$NATRsum1_score[!is.na(NATRsum1) & SALT <= 6]<-0

#kolhsum1
VIP_data$kolhsum1_score[!is.na(CARBOHYDRATES)]<-0
VIP_data$kolhsum1_score[!is.na(CARBOHYDRATES) & (CARBOHYDRATES >= 45) & (CARBOHYDRATES <= 60) ]<-1
VIP_data$kolhsum1_score[!is.na(CARBOHYDRATES) & CARBOHYDRATES > 60]<-2

#FA
VIP_data$FA_score[!is.na(FA)]<-1
VIP_data$FA_score[!is.na(FA) & FA < 3]<-0

#> 
#		> length(PUFAE[!is.na(SALT) & SALT<=6])
#[1] 66976
#> length(PUFAE[!is.na(SALT) & SALT>6])
#[1] 37
#> 
#		> 
#		> length(PUFAE[!is.na(MUFAE) & MUFAE>20])
#[1] 179
#> length(PUFAE[!is.na(MUFAE) & MUFAE<10])
#[1] 14779
#> 
#		> length(PUFAE[!is.na(PUFAE) & PUFAE<5])
#[1] 30629
#> length(PUFAE[!is.na(PUFAE) & PUFAE>10])
#[1] 1861
#> 
#		> 
#		> length(PUFAE[!is.na(TOTFAT) & TOTFAT>40])
#[1] 12335
#> length(PUFAE[!is.na(TOTFAT) & TOTFAT<25])
#[1] 3953
#> 
#		> 
#		> length(PUFAE[!is.na(CARBOHYDRATES) & CARBOHYDRATES<45])
#[1] 20159
#> 
#		> length(PUFAE[!is.na(CARBOHYDRATES) & CARBOHYDRATES>60])
#[1] 2180
#> 
#		> 
#		> length(PUFAE[!is.na(FA) & FA<3])
#[1] 2478
#> 


#for those where there are no guide lines, get tertiles and rank accordingly, work whitin each of the two visits

#ensum1
ensum1_tertiles_visit1<-quantile(ensum1[!is.na(ensum1) & visit==1], prob = c(0.33, 0.66, 1),na.rm=TRUE)
ensum1_tertiles_visit2<-quantile(ensum1[!is.na(ensum1) & visit==2], prob = c(0.33, 0.66, 1),na.rm=TRUE)

VIP_data$ensum1_score[!is.na(ensum1) & visit==1]<-0

VIP_data$ensum1_score[!is.na(ensum1) & visit==1 & ensum1>=ensum1_tertiles_visit1[[1]] & ensum1<ensum1_tertiles_visit1[[2]]]<-1

VIP_data$ensum1_score[!is.na(ensum1) & visit==1 & ensum1>=ensum1_tertiles_visit1[[2]]]<-2


VIP_data$ensum1_score[!is.na(ensum1) & visit==2]<-0

VIP_data$ensum1_score[!is.na(ensum1) & visit==2 & ensum1>=ensum1_tertiles_visit2[[1]] & ensum1<ensum1_tertiles_visit2[[2]] ]<-1

VIP_data$ensum1_score[!is.na(ensum1) & visit==2 & ensum1>=ensum1_tertiles_visit2[[2]]]<-2


#protsum1_anim
PROTEIN_ANIMAL<-(100*4*protsum1_anim)/ensum1
PROTEIN_ANIMAL_tertiles_visit1<-quantile(PROTEIN_ANIMAL[!is.na(PROTEIN_ANIMAL) & visit==1], prob = c(0.33, 0.66, 1),na.rm=TRUE)
PROTEIN_ANIMAL_tertiles_visit2<-quantile(PROTEIN_ANIMAL[!is.na(PROTEIN_ANIMAL) & visit==2], prob = c(0.33, 0.66, 1),na.rm=TRUE)

VIP_data$protsum1_anim_score[!is.na(protsum1) & visit==1]<-0

VIP_data$protsum1_anim_score[!is.na(protsum1) & visit==1 & PROTEIN_ANIMAL>=PROTEIN_ANIMAL_tertiles_visit1[[1]]
				& PROTEIN_ANIMAL<PROTEIN_ANIMAL_tertiles_visit1[[2]] ]<-1

VIP_data$protsum1_anim_score[!is.na(protsum1) & visit==1 & PROTEIN_ANIMAL>=PROTEIN_ANIMAL_tertiles_visit1[[2]]]<-2


VIP_data$protsum1_anim_score[!is.na(protsum1) & visit==2]<-0

VIP_data$protsum1_anim_score[!is.na(protsum1) & visit==2 & PROTEIN_ANIMAL>=PROTEIN_ANIMAL_tertiles_visit2[[1]]
				& PROTEIN_ANIMAL<PROTEIN_ANIMAL_tertiles_visit2[[2]] ]<-1

VIP_data$protsum1_anim_score[!is.na(protsum1) & visit==2 & PROTEIN_ANIMAL>=PROTEIN_ANIMAL_tertiles_visit2[[2]]]<-2


#FULLKsum1
WHOLEGRAIN<-(100*4*FULLKsum1)/ensum1
WHOLEGRAIN_tertiles_visit1<-quantile(WHOLEGRAIN[!is.na(WHOLEGRAIN) & visit==1], prob = c(0.33, 0.66, 1),na.rm=TRUE)
WHOLEGRAIN_tertiles_visit2<-quantile(WHOLEGRAIN[!is.na(WHOLEGRAIN) & visit==2], prob = c(0.33, 0.66, 1),na.rm=TRUE)

VIP_data$FULLKsum1_score[!is.na(FULLKsum1) & visit==1]<-2

VIP_data$FULLKsum1_score[!is.na(FULLKsum1) & visit==1 & WHOLEGRAIN>=WHOLEGRAIN_tertiles_visit1[[1]] & 
				WHOLEGRAIN<WHOLEGRAIN_tertiles_visit1[[2]] ]<-1

VIP_data$FULLKsum1_score[!is.na(FULLKsum1) & visit==1 & WHOLEGRAIN>=WHOLEGRAIN_tertiles_visit1[[3]]]<-0


VIP_data$FULLKsum1_score[!is.na(FULLKsum1) & visit==2]<-2

VIP_data$FULLKsum1_score[!is.na(FULLKsum1) & visit==2 & WHOLEGRAIN>=WHOLEGRAIN_tertiles_visit2[[1]] & 
				WHOLEGRAIN<WHOLEGRAIN_tertiles_visit2[[2]] ]<-1

VIP_data$FULLKsum1_score[!is.na(FULLKsum1) & visit==2 & WHOLEGRAIN>=WHOLEGRAIN_tertiles_visit2[[3]]]<-0


detach(VIP_data)
attach(VIP_data)

#within each visit, check the variation explained for the separate variables or for the categorized in scores
library(ppcor)
   
pcor.test(log(bmi[visit==1 & !is.na(bmi) & !is.na(PUFAE)]),PUFAE[visit==1 & !is.na(bmi) & !is.na(PUFAE)],cbind(age[visit==1 & !is.na(bmi) & !is.na(PUFAE)],
	agesq[visit==1 & !is.na(bmi) & !is.na(PUFAE)],gender[visit==1 & !is.na(bmi) & !is.na(PUFAE)],year[visit==1 & !is.na(bmi) & !is.na(PUFAE)],
	ffq[visit==1 & !is.na(bmi) & !is.na(PUFAE)],MUFAE[visit==1 & !is.na(bmi) & !is.na(PUFAE)],TOTFAT[visit==1 & !is.na(bmi) & !is.na(PUFAE)],
	CARBOHYDRATES[visit==1 & !is.na(bmi) & !is.na(PUFAE)],SALT[visit==1 & !is.na(bmi) & !is.na(PUFAE)],FA[visit==1 & !is.na(bmi) & !is.na(PUFAE)],
	PROTEIN_ANIMAL[visit==1 & !is.na(bmi) & !is.na(PUFAE)],WHOLEGRAIN[visit==1 & !is.na(bmi) & !is.na(PUFAE)]))
	
pcor.test(log(bmi[visit==1 & !is.na(bmi) & !is.na(MUFAE)]),MUFAE[visit==1 & !is.na(bmi) & !is.na(MUFAE)],cbind(age[visit==1 & !is.na(bmi) & !is.na(MUFAE)],
				agesq[visit==1 & !is.na(bmi) & !is.na(MUFAE)],gender[visit==1 & !is.na(bmi) & !is.na(MUFAE)],year[visit==1 & !is.na(bmi) & !is.na(MUFAE)],
				ffq[visit==1 & !is.na(bmi) & !is.na(MUFAE)],PUFAE[visit==1 & !is.na(bmi) & !is.na(MUFAE)],TOTFAT[visit==1 & !is.na(bmi) & !is.na(MUFAE)],
				CARBOHYDRATES[visit==1 & !is.na(bmi) & !is.na(MUFAE)],SALT[visit==1 & !is.na(bmi) & !is.na(MUFAE)],FA[visit==1 & !is.na(bmi) & !is.na(MUFAE)],
				PROTEIN_ANIMAL[visit==1 & !is.na(bmi) & !is.na(MUFAE)],WHOLEGRAIN[visit==1 & !is.na(bmi) & !is.na(MUFAE)]))

pcor.test(log(bmi[visit==1 & !is.na(bmi) & !is.na(TOTFAT)]),TOTFAT[visit==1 & !is.na(bmi) & !is.na(TOTFAT)],cbind(age[visit==1 & !is.na(bmi) & !is.na(TOTFAT)],
				agesq[visit==1 & !is.na(bmi) & !is.na(TOTFAT)],gender[visit==1 & !is.na(bmi) & !is.na(TOTFAT)],year[visit==1 & !is.na(bmi) & !is.na(TOTFAT)],
				ffq[visit==1 & !is.na(bmi) & !is.na(TOTFAT)],PUFAE[visit==1 & !is.na(bmi) & !is.na(TOTFAT)],MUFAE[visit==1 & !is.na(bmi) & !is.na(TOTFAT)],
				CARBOHYDRATES[visit==1 & !is.na(bmi) & !is.na(TOTFAT)],SALT[visit==1 & !is.na(bmi) & !is.na(TOTFAT)],FA[visit==1 & !is.na(bmi) & !is.na(TOTFAT)],
				PROTEIN_ANIMAL[visit==1 & !is.na(bmi) & !is.na(TOTFAT)],WHOLEGRAIN[visit==1 & !is.na(bmi) & !is.na(TOTFAT)]))

pcor.test(log(bmi[visit==1 & !is.na(bmi) & !is.na(CARBOHYDRATES)]),CARBOHYDRATES[visit==1 & !is.na(bmi) & !is.na(CARBOHYDRATES)],cbind(age[visit==1 & !is.na(bmi) & !is.na(CARBOHYDRATES)],
				agesq[visit==1 & !is.na(bmi) & !is.na(CARBOHYDRATES)],gender[visit==1 & !is.na(bmi) & !is.na(CARBOHYDRATES)],year[visit==1 & !is.na(bmi) & !is.na(CARBOHYDRATES)],
				ffq[visit==1 & !is.na(bmi) & !is.na(CARBOHYDRATES)],PUFAE[visit==1 & !is.na(bmi) & !is.na(CARBOHYDRATES)],MUFAE[visit==1 & !is.na(bmi) & !is.na(CARBOHYDRATES)],
				TOTFAT[visit==1 & !is.na(bmi) & !is.na(CARBOHYDRATES)],SALT[visit==1 & !is.na(bmi) & !is.na(CARBOHYDRATES)],FA[visit==1 & !is.na(bmi) & !is.na(CARBOHYDRATES)],
				PROTEIN_ANIMAL[visit==1 & !is.na(bmi) & !is.na(CARBOHYDRATES)],WHOLEGRAIN[visit==1 & !is.na(bmi) & !is.na(CARBOHYDRATES)]))

pcor.test(log(bmi[visit==1 & !is.na(bmi) & !is.na(SALT)]),SALT[visit==1 & !is.na(bmi) & !is.na(SALT)],cbind(age[visit==1 & !is.na(bmi) & !is.na(SALT)],
				agesq[visit==1 & !is.na(bmi) & !is.na(SALT)],gender[visit==1 & !is.na(bmi) & !is.na(SALT)],year[visit==1 & !is.na(bmi) & !is.na(SALT)],
				ffq[visit==1 & !is.na(bmi) & !is.na(SALT)],PUFAE[visit==1 & !is.na(bmi) & !is.na(SALT)],MUFAE[visit==1 & !is.na(bmi) & !is.na(SALT)],
				TOTFAT[visit==1 & !is.na(bmi) & !is.na(SALT)],CARBOHYDRATES[visit==1 & !is.na(bmi) & !is.na(SALT)],FA[visit==1 & !is.na(bmi) & !is.na(SALT)],
				PROTEIN_ANIMAL[visit==1 & !is.na(bmi) & !is.na(SALT)],WHOLEGRAIN[visit==1 & !is.na(bmi) & !is.na(SALT)]))

pcor.test(log(bmi[visit==1 & !is.na(bmi) & !is.na(FA)]),FA[visit==1 & !is.na(bmi) & !is.na(FA)],cbind(age[visit==1 & !is.na(bmi) & !is.na(FA)],
				agesq[visit==1 & !is.na(bmi) & !is.na(FA)],gender[visit==1 & !is.na(bmi) & !is.na(FA)],year[visit==1 & !is.na(bmi) & !is.na(FA)],
				ffq[visit==1 & !is.na(bmi) & !is.na(FA)],PUFAE[visit==1 & !is.na(bmi) & !is.na(FA)],MUFAE[visit==1 & !is.na(bmi) & !is.na(FA)],
				TOTFAT[visit==1 & !is.na(bmi) & !is.na(FA)],CARBOHYDRATES[visit==1 & !is.na(bmi) & !is.na(FA)],SALT[visit==1 & !is.na(bmi) & !is.na(FA)],
				PROTEIN_ANIMAL[visit==1 & !is.na(bmi) & !is.na(FA)],WHOLEGRAIN[visit==1 & !is.na(bmi) & !is.na(FA)]))

pcor.test(log(bmi[visit==1 & !is.na(bmi) & !is.na(WHOLEGRAIN)]),WHOLEGRAIN[visit==1 & !is.na(bmi) & !is.na(WHOLEGRAIN)],cbind(age[visit==1 & !is.na(bmi) & !is.na(WHOLEGRAIN)],
				agesq[visit==1 & !is.na(bmi) & !is.na(WHOLEGRAIN)],gender[visit==1 & !is.na(bmi) & !is.na(WHOLEGRAIN)],year[visit==1 & !is.na(bmi) & !is.na(WHOLEGRAIN)],
				ffq[visit==1 & !is.na(bmi) & !is.na(WHOLEGRAIN)],PUFAE[visit==1 & !is.na(bmi) & !is.na(WHOLEGRAIN)],MUFAE[visit==1 & !is.na(bmi) & !is.na(WHOLEGRAIN)],
				TOTFAT[visit==1 & !is.na(bmi) & !is.na(WHOLEGRAIN)],CARBOHYDRATES[visit==1 & !is.na(bmi) & !is.na(WHOLEGRAIN)],SALT[visit==1 & !is.na(bmi) & !is.na(WHOLEGRAIN)],
				PROTEIN_ANIMAL[visit==1 & !is.na(bmi) & !is.na(WHOLEGRAIN)],FA[visit==1 & !is.na(bmi) & !is.na(WHOLEGRAIN)]))

pcor.test(log(bmi[visit==1 & !is.na(bmi) & !is.na(PROTEIN_ANIMAL)]),PROTEIN_ANIMAL[visit==1 & !is.na(bmi) & !is.na(PROTEIN_ANIMAL)],cbind(age[visit==1 & !is.na(bmi) & !is.na(PROTEIN_ANIMAL)],
				agesq[visit==1 & !is.na(bmi) & !is.na(PROTEIN_ANIMAL)],gender[visit==1 & !is.na(bmi) & !is.na(PROTEIN_ANIMAL)],year[visit==1 & !is.na(bmi) & !is.na(PROTEIN_ANIMAL)],
				ffq[visit==1 & !is.na(bmi) & !is.na(PROTEIN_ANIMAL)],PUFAE[visit==1 & !is.na(bmi) & !is.na(PROTEIN_ANIMAL)],MUFAE[visit==1 & !is.na(bmi) & !is.na(PROTEIN_ANIMAL)],
				TOTFAT[visit==1 & !is.na(bmi) & !is.na(PROTEIN_ANIMAL)],CARBOHYDRATES[visit==1 & !is.na(bmi) & !is.na(PROTEIN_ANIMAL)],SALT[visit==1 & !is.na(bmi) & !is.na(PROTEIN_ANIMAL)],
				WHOLEGRAIN[visit==1 & !is.na(bmi) & !is.na(PROTEIN_ANIMAL)],FA[visit==1 & !is.na(bmi) & !is.na(PROTEIN_ANIMAL)]))


pcor.test(log(bmi[visit==2 & !is.na(bmi) & !is.na(PUFAE)]),PUFAE[visit==2 & !is.na(bmi) & !is.na(PUFAE)],cbind(age[visit==2 & !is.na(bmi) & !is.na(PUFAE)],
				agesq[visit==2 & !is.na(bmi) & !is.na(PUFAE)],gender[visit==2 & !is.na(bmi) & !is.na(PUFAE)],year[visit==2 & !is.na(bmi) & !is.na(PUFAE)],
				ffq[visit==2 & !is.na(bmi) & !is.na(PUFAE)],MUFAE[visit==2 & !is.na(bmi) & !is.na(PUFAE)],TOTFAT[visit==2 & !is.na(bmi) & !is.na(PUFAE)],
				CARBOHYDRATES[visit==2 & !is.na(bmi) & !is.na(PUFAE)],SALT[visit==2 & !is.na(bmi) & !is.na(PUFAE)],FA[visit==2 & !is.na(bmi) & !is.na(PUFAE)],
				PROTEIN_ANIMAL[visit==2 & !is.na(bmi) & !is.na(PUFAE)],WHOLEGRAIN[visit==2 & !is.na(bmi) & !is.na(PUFAE)]))

pcor.test(log(bmi[visit==2 & !is.na(bmi) & !is.na(MUFAE)]),MUFAE[visit==2 & !is.na(bmi) & !is.na(MUFAE)],cbind(age[visit==2 & !is.na(bmi) & !is.na(MUFAE)],
				agesq[visit==2 & !is.na(bmi) & !is.na(MUFAE)],gender[visit==2 & !is.na(bmi) & !is.na(MUFAE)],year[visit==2 & !is.na(bmi) & !is.na(MUFAE)],
				ffq[visit==2 & !is.na(bmi) & !is.na(MUFAE)],PUFAE[visit==2 & !is.na(bmi) & !is.na(MUFAE)],TOTFAT[visit==2 & !is.na(bmi) & !is.na(MUFAE)],
				CARBOHYDRATES[visit==2 & !is.na(bmi) & !is.na(MUFAE)],SALT[visit==2 & !is.na(bmi) & !is.na(MUFAE)],FA[visit==2 & !is.na(bmi) & !is.na(MUFAE)],
				PROTEIN_ANIMAL[visit==2 & !is.na(bmi) & !is.na(MUFAE)],WHOLEGRAIN[visit==2 & !is.na(bmi) & !is.na(MUFAE)]))

pcor.test(log(bmi[visit==2 & !is.na(bmi) & !is.na(TOTFAT)]),TOTFAT[visit==2 & !is.na(bmi) & !is.na(TOTFAT)],cbind(age[visit==2 & !is.na(bmi) & !is.na(TOTFAT)],
				agesq[visit==2 & !is.na(bmi) & !is.na(TOTFAT)],gender[visit==2 & !is.na(bmi) & !is.na(TOTFAT)],year[visit==2 & !is.na(bmi) & !is.na(TOTFAT)],
				ffq[visit==2 & !is.na(bmi) & !is.na(TOTFAT)],PUFAE[visit==2 & !is.na(bmi) & !is.na(TOTFAT)],MUFAE[visit==2 & !is.na(bmi) & !is.na(TOTFAT)],
				CARBOHYDRATES[visit==2 & !is.na(bmi) & !is.na(TOTFAT)],SALT[visit==2 & !is.na(bmi) & !is.na(TOTFAT)],FA[visit==2 & !is.na(bmi) & !is.na(TOTFAT)],
				PROTEIN_ANIMAL[visit==2 & !is.na(bmi) & !is.na(TOTFAT)],WHOLEGRAIN[visit==2 & !is.na(bmi) & !is.na(TOTFAT)]))

pcor.test(log(bmi[visit==2 & !is.na(bmi) & !is.na(CARBOHYDRATES)]),CARBOHYDRATES[visit==2 & !is.na(bmi) & !is.na(CARBOHYDRATES)],cbind(age[visit==2 & !is.na(bmi) & !is.na(CARBOHYDRATES)],
				agesq[visit==2 & !is.na(bmi) & !is.na(CARBOHYDRATES)],gender[visit==2 & !is.na(bmi) & !is.na(CARBOHYDRATES)],year[visit==2 & !is.na(bmi) & !is.na(CARBOHYDRATES)],
				ffq[visit==2 & !is.na(bmi) & !is.na(CARBOHYDRATES)],PUFAE[visit==2 & !is.na(bmi) & !is.na(CARBOHYDRATES)],MUFAE[visit==2 & !is.na(bmi) & !is.na(CARBOHYDRATES)],
				TOTFAT[visit==2 & !is.na(bmi) & !is.na(CARBOHYDRATES)],SALT[visit==2 & !is.na(bmi) & !is.na(CARBOHYDRATES)],FA[visit==2 & !is.na(bmi) & !is.na(CARBOHYDRATES)],
				PROTEIN_ANIMAL[visit==2 & !is.na(bmi) & !is.na(CARBOHYDRATES)],WHOLEGRAIN[visit==2 & !is.na(bmi) & !is.na(CARBOHYDRATES)]))

pcor.test(log(bmi[visit==2 & !is.na(bmi) & !is.na(SALT)]),SALT[visit==2 & !is.na(bmi) & !is.na(SALT)],cbind(age[visit==2 & !is.na(bmi) & !is.na(SALT)],
				agesq[visit==2 & !is.na(bmi) & !is.na(SALT)],gender[visit==2 & !is.na(bmi) & !is.na(SALT)],year[visit==2 & !is.na(bmi) & !is.na(SALT)],
				ffq[visit==2 & !is.na(bmi) & !is.na(SALT)],PUFAE[visit==2 & !is.na(bmi) & !is.na(SALT)],MUFAE[visit==2 & !is.na(bmi) & !is.na(SALT)],
				TOTFAT[visit==2 & !is.na(bmi) & !is.na(SALT)],CARBOHYDRATES[visit==2 & !is.na(bmi) & !is.na(SALT)],FA[visit==2 & !is.na(bmi) & !is.na(SALT)],
				PROTEIN_ANIMAL[visit==2 & !is.na(bmi) & !is.na(SALT)],WHOLEGRAIN[visit==2 & !is.na(bmi) & !is.na(SALT)]))

pcor.test(log(bmi[visit==2 & !is.na(bmi) & !is.na(FA)]),FA[visit==2 & !is.na(bmi) & !is.na(FA)],cbind(age[visit==2 & !is.na(bmi) & !is.na(FA)],
				agesq[visit==2 & !is.na(bmi) & !is.na(FA)],gender[visit==2 & !is.na(bmi) & !is.na(FA)],year[visit==2 & !is.na(bmi) & !is.na(FA)],
				ffq[visit==2 & !is.na(bmi) & !is.na(FA)],PUFAE[visit==2 & !is.na(bmi) & !is.na(FA)],MUFAE[visit==2 & !is.na(bmi) & !is.na(FA)],
				TOTFAT[visit==2 & !is.na(bmi) & !is.na(FA)],CARBOHYDRATES[visit==2 & !is.na(bmi) & !is.na(FA)],SALT[visit==2 & !is.na(bmi) & !is.na(FA)],
				PROTEIN_ANIMAL[visit==2 & !is.na(bmi) & !is.na(FA)],WHOLEGRAIN[visit==2 & !is.na(bmi) & !is.na(FA)]))

pcor.test(log(bmi[visit==2 & !is.na(bmi) & !is.na(WHOLEGRAIN)]),WHOLEGRAIN[visit==2 & !is.na(bmi) & !is.na(WHOLEGRAIN)],cbind(age[visit==2 & !is.na(bmi) & !is.na(WHOLEGRAIN)],
				agesq[visit==2 & !is.na(bmi) & !is.na(WHOLEGRAIN)],gender[visit==2 & !is.na(bmi) & !is.na(WHOLEGRAIN)],year[visit==2 & !is.na(bmi) & !is.na(WHOLEGRAIN)],
				ffq[visit==2 & !is.na(bmi) & !is.na(WHOLEGRAIN)],PUFAE[visit==2 & !is.na(bmi) & !is.na(WHOLEGRAIN)],MUFAE[visit==2 & !is.na(bmi) & !is.na(WHOLEGRAIN)],
				TOTFAT[visit==2 & !is.na(bmi) & !is.na(WHOLEGRAIN)],CARBOHYDRATES[visit==2 & !is.na(bmi) & !is.na(WHOLEGRAIN)],SALT[visit==2 & !is.na(bmi) & !is.na(WHOLEGRAIN)],
				PROTEIN_ANIMAL[visit==2 & !is.na(bmi) & !is.na(WHOLEGRAIN)],FA[visit==2 & !is.na(bmi) & !is.na(WHOLEGRAIN)]))

pcor.test(log(bmi[visit==2 & !is.na(bmi) & !is.na(PROTEIN_ANIMAL)]),PROTEIN_ANIMAL[visit==2 & !is.na(bmi) & !is.na(PROTEIN_ANIMAL)],cbind(age[visit==2 & !is.na(bmi) & !is.na(PROTEIN_ANIMAL)],
				agesq[visit==2 & !is.na(bmi) & !is.na(PROTEIN_ANIMAL)],gender[visit==2 & !is.na(bmi) & !is.na(PROTEIN_ANIMAL)],year[visit==2 & !is.na(bmi) & !is.na(PROTEIN_ANIMAL)],
				ffq[visit==2 & !is.na(bmi) & !is.na(PROTEIN_ANIMAL)],PUFAE[visit==2 & !is.na(bmi) & !is.na(PROTEIN_ANIMAL)],MUFAE[visit==2 & !is.na(bmi) & !is.na(PROTEIN_ANIMAL)],
				TOTFAT[visit==2 & !is.na(bmi) & !is.na(PROTEIN_ANIMAL)],CARBOHYDRATES[visit==2 & !is.na(bmi) & !is.na(PROTEIN_ANIMAL)],SALT[visit==2 & !is.na(bmi) & !is.na(PROTEIN_ANIMAL)],
				WHOLEGRAIN[visit==2 & !is.na(bmi) & !is.na(PROTEIN_ANIMAL)],FA[visit==2 & !is.na(bmi) & !is.na(PROTEIN_ANIMAL)]))



#SCORE
pcor.test(log(bmi[visit==1 & !is.na(bmi) & !is.na(POLYsum1_score)]),POLYsum1_score[visit==1 & !is.na(bmi) & !is.na(POLYsum1_score)],cbind(age[visit==1 & !is.na(bmi) & !is.na(POLYsum1_score)],
				agesq[visit==1 & !is.na(bmi) & !is.na(POLYsum1_score)],gender[visit==1 & !is.na(bmi) & !is.na(POLYsum1_score)],year[visit==1 & !is.na(bmi) & !is.na(POLYsum1_score)],
				ffq[visit==1 & !is.na(bmi) & !is.na(POLYsum1_score)],MONOsum1_score[visit==1 & !is.na(bmi) & !is.na(POLYsum1_score)],fettsum1_score[visit==1 & !is.na(bmi) & !is.na(POLYsum1_score)],
				kolhsum1_score[visit==1 & !is.na(bmi) & !is.na(POLYsum1_score)],NATRsum1_score[visit==1 & !is.na(bmi) & !is.na(POLYsum1_score)],FA_score[visit==1 & !is.na(bmi) & !is.na(POLYsum1_score)],
				protsum1_anim_score[visit==1 & !is.na(bmi) & !is.na(POLYsum1_score)],FULLKsum1_score[visit==1 & !is.na(bmi) & !is.na(POLYsum1_score)]))

pcor.test(log(bmi[visit==1 & !is.na(bmi) & !is.na(MONOsum1_score)]),MONOsum1_score[visit==1 & !is.na(bmi) & !is.na(MONOsum1_score)],cbind(age[visit==1 & !is.na(bmi) & !is.na(MONOsum1_score)],
				agesq[visit==1 & !is.na(bmi) & !is.na(MONOsum1_score)],gender[visit==1 & !is.na(bmi) & !is.na(MONOsum1_score)],year[visit==1 & !is.na(bmi) & !is.na(MONOsum1_score)],
				ffq[visit==1 & !is.na(bmi) & !is.na(MONOsum1_score)],POLYsum1_score[visit==1 & !is.na(bmi) & !is.na(MONOsum1_score)],fettsum1_score[visit==1 & !is.na(bmi) & !is.na(MONOsum1_score)],
				kolhsum1_score[visit==1 & !is.na(bmi) & !is.na(MONOsum1_score)],NATRsum1_score[visit==1 & !is.na(bmi) & !is.na(MONOsum1_score)],FA_score[visit==1 & !is.na(bmi) & !is.na(MONOsum1_score)],
				protsum1_anim_score[visit==1 & !is.na(bmi) & !is.na(MONOsum1_score)],FULLKsum1_score[visit==1 & !is.na(bmi) & !is.na(MONOsum1_score)]))

pcor.test(log(bmi[visit==1 & !is.na(bmi) & !is.na(fettsum1_score)]),fettsum1_score[visit==1 & !is.na(bmi) & !is.na(fettsum1_score)],cbind(age[visit==1 & !is.na(bmi) & !is.na(fettsum1_score)],
				agesq[visit==1 & !is.na(bmi) & !is.na(fettsum1_score)],gender[visit==1 & !is.na(bmi) & !is.na(fettsum1_score)],year[visit==1 & !is.na(bmi) & !is.na(fettsum1_score)],
				ffq[visit==1 & !is.na(bmi) & !is.na(fettsum1_score)],POLYsum1_score[visit==1 & !is.na(bmi) & !is.na(fettsum1_score)],MONOsum1_score[visit==1 & !is.na(bmi) & !is.na(fettsum1_score)],
				kolhsum1_score[visit==1 & !is.na(bmi) & !is.na(fettsum1_score)],NATRsum1_score[visit==1 & !is.na(bmi) & !is.na(fettsum1_score)],FA_score[visit==1 & !is.na(bmi) & !is.na(fettsum1_score)],
				protsum1_anim_score[visit==1 & !is.na(bmi) & !is.na(fettsum1_score)],FULLKsum1_score[visit==1 & !is.na(bmi) & !is.na(fettsum1_score)]))

pcor.test(log(bmi[visit==1 & !is.na(bmi) & !is.na(kolhsum1_score)]),kolhsum1_score[visit==1 & !is.na(bmi) & !is.na(kolhsum1_score)],cbind(age[visit==1 & !is.na(bmi) & !is.na(kolhsum1_score)],
				agesq[visit==1 & !is.na(bmi) & !is.na(kolhsum1_score)],gender[visit==1 & !is.na(bmi) & !is.na(kolhsum1_score)],year[visit==1 & !is.na(bmi) & !is.na(kolhsum1_score)],
				ffq[visit==1 & !is.na(bmi) & !is.na(kolhsum1_score)],POLYsum1_score[visit==1 & !is.na(bmi) & !is.na(kolhsum1_score)],MONOsum1_score[visit==1 & !is.na(bmi) & !is.na(kolhsum1_score)],
				fettsum1_score[visit==1 & !is.na(bmi) & !is.na(kolhsum1_score)],NATRsum1_score[visit==1 & !is.na(bmi) & !is.na(kolhsum1_score)],FA_score[visit==1 & !is.na(bmi) & !is.na(kolhsum1_score)],
				protsum1_anim_score[visit==1 & !is.na(bmi) & !is.na(kolhsum1_score)],FULLKsum1_score[visit==1 & !is.na(bmi) & !is.na(kolhsum1_score)]))

pcor.test(log(bmi[visit==1 & !is.na(bmi) & !is.na(NATRsum1_score)]),NATRsum1_score[visit==1 & !is.na(bmi) & !is.na(NATRsum1_score)],cbind(age[visit==1 & !is.na(bmi) & !is.na(NATRsum1_score)],
				agesq[visit==1 & !is.na(bmi) & !is.na(NATRsum1_score)],gender[visit==1 & !is.na(bmi) & !is.na(NATRsum1_score)],year[visit==1 & !is.na(bmi) & !is.na(NATRsum1_score)],
				ffq[visit==1 & !is.na(bmi) & !is.na(NATRsum1_score)],POLYsum1_score[visit==1 & !is.na(bmi) & !is.na(NATRsum1_score)],MONOsum1_score[visit==1 & !is.na(bmi) & !is.na(NATRsum1_score)],
				fettsum1_score[visit==1 & !is.na(bmi) & !is.na(NATRsum1_score)],kolhsum1_score[visit==1 & !is.na(bmi) & !is.na(NATRsum1_score)],FA_score[visit==1 & !is.na(bmi) & !is.na(NATRsum1_score)],
				protsum1_anim_score[visit==1 & !is.na(bmi) & !is.na(NATRsum1_score)],FULLKsum1_score[visit==1 & !is.na(bmi) & !is.na(NATRsum1_score)]))

pcor.test(log(bmi[visit==1 & !is.na(bmi) & !is.na(FA_score)]),FA_score[visit==1 & !is.na(bmi) & !is.na(FA_score)],cbind(age[visit==1 & !is.na(bmi) & !is.na(FA_score)],
				agesq[visit==1 & !is.na(bmi) & !is.na(FA_score)],gender[visit==1 & !is.na(bmi) & !is.na(FA_score)],year[visit==1 & !is.na(bmi) & !is.na(FA_score)],
				ffq[visit==1 & !is.na(bmi) & !is.na(FA_score)],POLYsum1_score[visit==1 & !is.na(bmi) & !is.na(FA_score)],MONOsum1_score[visit==1 & !is.na(bmi) & !is.na(FA_score)],
				fettsum1_score[visit==1 & !is.na(bmi) & !is.na(FA_score)],kolhsum1_score[visit==1 & !is.na(bmi) & !is.na(FA_score)],NATRsum1_score[visit==1 & !is.na(bmi) & !is.na(FA_score)],
				protsum1_anim_score[visit==1 & !is.na(bmi) & !is.na(FA_score)],FULLKsum1_score[visit==1 & !is.na(bmi) & !is.na(FA_score)]))

pcor.test(log(bmi[visit==1 & !is.na(bmi) & !is.na(FULLKsum1_score)]),FULLKsum1_score[visit==1 & !is.na(bmi) & !is.na(FULLKsum1_score)],cbind(age[visit==1 & !is.na(bmi) & !is.na(FULLKsum1_score)],
				agesq[visit==1 & !is.na(bmi) & !is.na(FULLKsum1_score)],gender[visit==1 & !is.na(bmi) & !is.na(FULLKsum1_score)],year[visit==1 & !is.na(bmi) & !is.na(FULLKsum1_score)],
				ffq[visit==1 & !is.na(bmi) & !is.na(FULLKsum1_score)],POLYsum1_score[visit==1 & !is.na(bmi) & !is.na(FULLKsum1_score)],MONOsum1_score[visit==1 & !is.na(bmi) & !is.na(FULLKsum1_score)],
				fettsum1_score[visit==1 & !is.na(bmi) & !is.na(FULLKsum1_score)],kolhsum1_score[visit==1 & !is.na(bmi) & !is.na(FULLKsum1_score)],NATRsum1_score[visit==1 & !is.na(bmi) & !is.na(FULLKsum1_score)],
				protsum1_anim_score[visit==1 & !is.na(bmi) & !is.na(FULLKsum1_score)],FA_score[visit==1 & !is.na(bmi) & !is.na(FULLKsum1_score)]))

pcor.test(log(bmi[visit==1 & !is.na(bmi) & !is.na(protsum1_anim_score)]),protsum1_anim_score[visit==1 & !is.na(bmi) & !is.na(protsum1_anim_score)],cbind(age[visit==1 & !is.na(bmi) & !is.na(protsum1_anim_score)],
				agesq[visit==1 & !is.na(bmi) & !is.na(protsum1_anim_score)],gender[visit==1 & !is.na(bmi) & !is.na(protsum1_anim_score)],year[visit==1 & !is.na(bmi) & !is.na(protsum1_anim_score)],
				ffq[visit==1 & !is.na(bmi) & !is.na(protsum1_anim_score)],POLYsum1_score[visit==1 & !is.na(bmi) & !is.na(protsum1_anim_score)],MONOsum1_score[visit==1 & !is.na(bmi) & !is.na(protsum1_anim_score)],
				fettsum1_score[visit==1 & !is.na(bmi) & !is.na(protsum1_anim_score)],kolhsum1_score[visit==1 & !is.na(bmi) & !is.na(protsum1_anim_score)],NATRsum1_score[visit==1 & !is.na(bmi) & !is.na(protsum1_anim_score)],
				FULLKsum1_score[visit==1 & !is.na(bmi) & !is.na(protsum1_anim_score)],FA_score[visit==1 & !is.na(bmi) & !is.na(protsum1_anim_score)]))


pcor.test(log(bmi[visit==2 & !is.na(bmi) & !is.na(POLYsum1_score)]),POLYsum1_score[visit==2 & !is.na(bmi) & !is.na(POLYsum1_score)],cbind(age[visit==2 & !is.na(bmi) & !is.na(POLYsum1_score)],
				agesq[visit==2 & !is.na(bmi) & !is.na(POLYsum1_score)],gender[visit==2 & !is.na(bmi) & !is.na(POLYsum1_score)],year[visit==2 & !is.na(bmi) & !is.na(POLYsum1_score)],
				ffq[visit==2 & !is.na(bmi) & !is.na(POLYsum1_score)],MONOsum1_score[visit==2 & !is.na(bmi) & !is.na(POLYsum1_score)],fettsum1_score[visit==2 & !is.na(bmi) & !is.na(POLYsum1_score)],
				kolhsum1_score[visit==2 & !is.na(bmi) & !is.na(POLYsum1_score)],NATRsum1_score[visit==2 & !is.na(bmi) & !is.na(POLYsum1_score)],FA_score[visit==2 & !is.na(bmi) & !is.na(POLYsum1_score)],
				protsum1_anim_score[visit==2 & !is.na(bmi) & !is.na(POLYsum1_score)],FULLKsum1_score[visit==2 & !is.na(bmi) & !is.na(POLYsum1_score)]))

pcor.test(log(bmi[visit==2 & !is.na(bmi) & !is.na(MONOsum1_score)]),MONOsum1_score[visit==2 & !is.na(bmi) & !is.na(MONOsum1_score)],cbind(age[visit==2 & !is.na(bmi) & !is.na(MONOsum1_score)],
				agesq[visit==2 & !is.na(bmi) & !is.na(MONOsum1_score)],gender[visit==2 & !is.na(bmi) & !is.na(MONOsum1_score)],year[visit==2 & !is.na(bmi) & !is.na(MONOsum1_score)],
				ffq[visit==2 & !is.na(bmi) & !is.na(MONOsum1_score)],POLYsum1_score[visit==2 & !is.na(bmi) & !is.na(MONOsum1_score)],fettsum1_score[visit==2 & !is.na(bmi) & !is.na(MONOsum1_score)],
				kolhsum1_score[visit==2 & !is.na(bmi) & !is.na(MONOsum1_score)],NATRsum1_score[visit==2 & !is.na(bmi) & !is.na(MONOsum1_score)],FA_score[visit==2 & !is.na(bmi) & !is.na(MONOsum1_score)],
				protsum1_anim_score[visit==2 & !is.na(bmi) & !is.na(MONOsum1_score)],FULLKsum1_score[visit==2 & !is.na(bmi) & !is.na(MONOsum1_score)]))

pcor.test(log(bmi[visit==2 & !is.na(bmi) & !is.na(fettsum1_score)]),fettsum1_score[visit==2 & !is.na(bmi) & !is.na(fettsum1_score)],cbind(age[visit==2 & !is.na(bmi) & !is.na(fettsum1_score)],
				agesq[visit==2 & !is.na(bmi) & !is.na(fettsum1_score)],gender[visit==2 & !is.na(bmi) & !is.na(fettsum1_score)],year[visit==2 & !is.na(bmi) & !is.na(fettsum1_score)],
				ffq[visit==2 & !is.na(bmi) & !is.na(fettsum1_score)],POLYsum1_score[visit==2 & !is.na(bmi) & !is.na(fettsum1_score)],MONOsum1_score[visit==2 & !is.na(bmi) & !is.na(fettsum1_score)],
				kolhsum1_score[visit==2 & !is.na(bmi) & !is.na(fettsum1_score)],NATRsum1_score[visit==2 & !is.na(bmi) & !is.na(fettsum1_score)],FA_score[visit==2 & !is.na(bmi) & !is.na(fettsum1_score)],
				protsum1_anim_score[visit==2 & !is.na(bmi) & !is.na(fettsum1_score)],FULLKsum1_score[visit==2 & !is.na(bmi) & !is.na(fettsum1_score)]))

pcor.test(log(bmi[visit==2 & !is.na(bmi) & !is.na(kolhsum1_score)]),kolhsum1_score[visit==2 & !is.na(bmi) & !is.na(kolhsum1_score)],cbind(age[visit==2 & !is.na(bmi) & !is.na(kolhsum1_score)],
				agesq[visit==2 & !is.na(bmi) & !is.na(kolhsum1_score)],gender[visit==2 & !is.na(bmi) & !is.na(kolhsum1_score)],year[visit==2 & !is.na(bmi) & !is.na(kolhsum1_score)],
				ffq[visit==2 & !is.na(bmi) & !is.na(kolhsum1_score)],POLYsum1_score[visit==2 & !is.na(bmi) & !is.na(kolhsum1_score)],MONOsum1_score[visit==2 & !is.na(bmi) & !is.na(kolhsum1_score)],
				fettsum1_score[visit==2 & !is.na(bmi) & !is.na(kolhsum1_score)],NATRsum1_score[visit==2 & !is.na(bmi) & !is.na(kolhsum1_score)],FA_score[visit==2 & !is.na(bmi) & !is.na(kolhsum1_score)],
				protsum1_anim_score[visit==2 & !is.na(bmi) & !is.na(kolhsum1_score)],FULLKsum1_score[visit==2 & !is.na(bmi) & !is.na(kolhsum1_score)]))

pcor.test(log(bmi[visit==2 & !is.na(bmi) & !is.na(NATRsum1_score)]),NATRsum1_score[visit==2 & !is.na(bmi) & !is.na(NATRsum1_score)],cbind(age[visit==2 & !is.na(bmi) & !is.na(NATRsum1_score)],
				agesq[visit==2 & !is.na(bmi) & !is.na(NATRsum1_score)],gender[visit==2 & !is.na(bmi) & !is.na(NATRsum1_score)],year[visit==2 & !is.na(bmi) & !is.na(NATRsum1_score)],
				ffq[visit==2 & !is.na(bmi) & !is.na(NATRsum1_score)],POLYsum1_score[visit==2 & !is.na(bmi) & !is.na(NATRsum1_score)],MONOsum1_score[visit==2 & !is.na(bmi) & !is.na(NATRsum1_score)],
				fettsum1_score[visit==2 & !is.na(bmi) & !is.na(NATRsum1_score)],kolhsum1_score[visit==2 & !is.na(bmi) & !is.na(NATRsum1_score)],FA_score[visit==2 & !is.na(bmi) & !is.na(NATRsum1_score)],
				protsum1_anim_score[visit==2 & !is.na(bmi) & !is.na(NATRsum1_score)],FULLKsum1_score[visit==2 & !is.na(bmi) & !is.na(NATRsum1_score)]))

pcor.test(log(bmi[visit==2 & !is.na(bmi) & !is.na(FA_score)]),FA_score[visit==2 & !is.na(bmi) & !is.na(FA_score)],cbind(age[visit==2 & !is.na(bmi) & !is.na(FA_score)],
				agesq[visit==2 & !is.na(bmi) & !is.na(FA_score)],gender[visit==2 & !is.na(bmi) & !is.na(FA_score)],year[visit==2 & !is.na(bmi) & !is.na(FA_score)],
				ffq[visit==2 & !is.na(bmi) & !is.na(FA_score)],POLYsum1_score[visit==2 & !is.na(bmi) & !is.na(FA_score)],MONOsum1_score[visit==2 & !is.na(bmi) & !is.na(FA_score)],
				fettsum1_score[visit==2 & !is.na(bmi) & !is.na(FA_score)],kolhsum1_score[visit==2 & !is.na(bmi) & !is.na(FA_score)],NATRsum1_score[visit==2 & !is.na(bmi) & !is.na(FA_score)],
				protsum1_anim_score[visit==2 & !is.na(bmi) & !is.na(FA_score)],FULLKsum1_score[visit==2 & !is.na(bmi) & !is.na(FA_score)]))

pcor.test(log(bmi[visit==2 & !is.na(bmi) & !is.na(FULLKsum1_score)]),FULLKsum1_score[visit==2 & !is.na(bmi) & !is.na(FULLKsum1_score)],cbind(age[visit==2 & !is.na(bmi) & !is.na(FULLKsum1_score)],
				agesq[visit==2 & !is.na(bmi) & !is.na(FULLKsum1_score)],gender[visit==2 & !is.na(bmi) & !is.na(FULLKsum1_score)],year[visit==2 & !is.na(bmi) & !is.na(FULLKsum1_score)],
				ffq[visit==2 & !is.na(bmi) & !is.na(FULLKsum1_score)],POLYsum1_score[visit==2 & !is.na(bmi) & !is.na(FULLKsum1_score)],MONOsum1_score[visit==2 & !is.na(bmi) & !is.na(FULLKsum1_score)],
				fettsum1_score[visit==2 & !is.na(bmi) & !is.na(FULLKsum1_score)],kolhsum1_score[visit==2 & !is.na(bmi) & !is.na(FULLKsum1_score)],NATRsum1_score[visit==2 & !is.na(bmi) & !is.na(FULLKsum1_score)],
				protsum1_anim_score[visit==2 & !is.na(bmi) & !is.na(FULLKsum1_score)],FA_score[visit==2 & !is.na(bmi) & !is.na(FULLKsum1_score)]))

pcor.test(log(bmi[visit==2 & !is.na(bmi) & !is.na(protsum1_anim_score)]),protsum1_anim_score[visit==2 & !is.na(bmi) & !is.na(protsum1_anim_score)],cbind(age[visit==2 & !is.na(bmi) & !is.na(protsum1_anim_score)],
				agesq[visit==2 & !is.na(bmi) & !is.na(protsum1_anim_score)],gender[visit==2 & !is.na(bmi) & !is.na(protsum1_anim_score)],year[visit==2 & !is.na(bmi) & !is.na(protsum1_anim_score)],
				ffq[visit==2 & !is.na(bmi) & !is.na(protsum1_anim_score)],POLYsum1_score[visit==2 & !is.na(bmi) & !is.na(protsum1_anim_score)],MONOsum1_score[visit==2 & !is.na(bmi) & !is.na(protsum1_anim_score)],
				fettsum1_score[visit==2 & !is.na(bmi) & !is.na(protsum1_anim_score)],kolhsum1_score[visit==2 & !is.na(bmi) & !is.na(protsum1_anim_score)],NATRsum1_score[visit==2 & !is.na(bmi) & !is.na(protsum1_anim_score)],
				FULLKsum1_score[visit==2 & !is.na(bmi) & !is.na(protsum1_anim_score)],FA_score[visit==2 & !is.na(bmi) & !is.na(protsum1_anim_score)]))


#final score
pcor.test(log(bmi[visit ==1 & !is.na(bmi) & !is.na(diet_score)]),diet_score[visit ==1 & !is.na(bmi) & !is.na(diet_score)],cbind(age[visit ==1 & !is.na(bmi) & !is.na(diet_score)],
				agesq[visit ==1 & !is.na(bmi) & !is.na(diet_score)],gender[visit ==1 & !is.na(bmi) & !is.na(diet_score)],year[visit ==1 & !is.na(bmi) & !is.na(diet_score)],
				ffq[visit ==1 & !is.na(bmi) & !is.na(diet_score)]))

pcor.test(log(bmi[visit==2 & !is.na(bmi) & !is.na(diet_score)]),diet_score[visit==2 & !is.na(bmi) & !is.na(diet_score)],cbind(age[visit==2 & !is.na(bmi) & !is.na(diet_score)],
				agesq[visit==2 & !is.na(bmi) & !is.na(diet_score)],gender[visit==2 & !is.na(bmi) & !is.na(diet_score)],year[visit==2 & !is.na(bmi) & !is.na(diet_score)],
				ffq[visit==2 & !is.na(bmi) & !is.na(diet_score)]))

#CONTINOUS

#visit==1
#PUFAE R2=0.002612747
#MUFAE R2=0.0008475796
#TOTFAT R2=0.0003314814(negative r)
#CARBOHYDRATES R2=0.0007374663
#SALT R2=0.002423302
#FA R2=0.001340275(negative r)
#WHOLEGRAIN R2=0.0008932023(negative r)
#ANIMAL PROTEIN R2=0.006759744

#visit==2
#PUFAE R2=0.001835798
#MUFAE R2=0.001530591
#TOTFAT R2=8.261014e-10(negative r)
#CARBOHYDRATES R2=0.003758521
#SALT R2=0.0001264089
#FA R2=0.001275221(negative r)
#WHOLEGRAIN R2=0.0008198624(negative r)
#ANIMAL PROTEIN R2=0.01122455


#CATEGORIZED

#visit==1
#PUFAE R2=0.0007221076
#MUFAE R2=9.283197e-05(negative r, insignificant)
#TOTFAT R2=0.0003148957
#CARBOHYDRATES R2=2.318038e-07(negative r, insignificant)
#SALT R2=0.000182247
#FA R2=0.0001259661
#WHOLEGRAIN R2=6.384496e-07(negative r, insignificant)
#ANIMAL PROTEIN R2=0.006943587

#visit==2
#PUFAE R2=0.0007405153
#MUFAE R2=1.638821e-05(insignificant)
#TOTFAT R2=0.0003148957
#CARBOHYDRATES R2=0.0005611545
#SALT R2=0.000182247
#FA R2=8.789381e-06(negative r, insignificant)
#WHOLEGRAIN R2=8.283039e-06(negative r, insignificant)
#ANIMAL PROTEIN R2=0.01206467

#final score visit==1 R2=0.004978254
#final score visit==1 R2=0.006556415


#final first diet score
#VIP_data$diet_score<-apply(VIP_data[,c(613:621)], 1, FUN=sum)

#SALT and acids influence the plot a lot, but not so much in the regression, then

# ensum1 and WHOLEGRAIN influence the regression a lot, the best is to include the macronutrients and animal protein, although the effect size is 
#  actually smaller then PUFAE alone

#final first diet score
VIP_data$diet_score<-apply(VIP_data[,c(613,614,615,617,620)], 1, FUN=sum)
detach(VIP_data)
attach(VIP_data)

png("Results/environment_risk_score/bmi_vs_diet_score/bmi_vs_diet_score1.png",width=2000, height=1000)
boxplot(bmi~diet_score, main="bmi vs first diet score", xlab="first diet score", ylab="bmi")
dev.off()

bmi_vs_diet<-glm(log(bmi) ~ age + agesq + gender + year + ffq + visit + diet_score, family = gaussian(link = "identity"))
				
#RESULTS
#> summary(bmi_vs_diet)
#
#Call:
#		glm(formula = log(bmi) ~ age + agesq + gender + year + ffq + 
#						visit + diet_score, family = gaussian(link = "identity"))
#
#Deviance Residuals: 
#		Min        1Q    Median        3Q       Max  
#-0.56831  -0.09376  -0.01217   0.08021   1.04410  
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  7.097e-01  3.564e-01   1.991  0.04647 *  
#		age          3.158e-03  6.368e-04   4.958 7.13e-07 ***
#		agesq       -1.967e-05  6.755e-06  -2.912  0.00359 ** 
#		gender2     -4.330e-02  1.096e-03 -39.513  < 2e-16 ***
#		year         1.197e-03  1.782e-04   6.714 1.91e-11 ***
#		ffq1        -8.557e-03  1.915e-03  -4.469 7.87e-06 ***
#		visit2       1.479e-02  1.900e-03   7.786 7.00e-15 ***
#		diet_score   1.017e-02  5.221e-04  19.485  < 2e-16 ***
#		---
#		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.01972582)
#
#Null deviance: 1397.7  on 66854  degrees of freedom
#Residual deviance: 1318.6  on 66847  degrees of freedom
#(2135 observations deleted due to missingness)
#AIC: -72725
#
#Number of Fisher Scoring iterations: 2

#library(ppcor)
#> pcor.test(log(bmi[!is.na(bmi) & !is.na(diet_score)]),diet_score[!is.na(bmi) & !is.na(diet_score)],cbind(age[!is.na(bmi) & !is.na(diet_score)],agesq[!is.na(bmi) & !is.na(diet_score)],gender[!is.na(bmi) & !is.na(diet_score)],year[!is.na(bmi) & !is.na(diet_score)],ffq[!is.na(bmi) & !is.na(diet_score)],visit[!is.na(bmi) & !is.na(diet_score)]))
#estimate      p.value statistic     n gp  Method
#1 0.07515013 2.525128e-84    19.485 66855  6 pearson


#second diet score
VIP_data$wholegrain[enkver2=="apri" | enkver2=="long"]<-apply(cbind(da10,da11,da22,da23,da27,0.25*da70),1,FUN=sum,na.rm=TRUE)[enkver2=="apri" | enkver2=="long"]
VIP_data$fish[enkver2=="apri" | enkver2=="long"]<-apply(cbind(da60,da61),1,FUN=sum,na.rm=TRUE)[enkver2=="apri" | enkver2=="long"]
VIP_data$fruit[enkver2=="apri" | enkver2=="long"]<-apply(cbind(da29,da30,da31,da32),1,FUN=sum,na.rm=TRUE)[enkver2=="apri" | enkver2=="long"]
VIP_data$vegetables[enkver2=="apri" | enkver2=="long"]<-apply(cbind(da33,da34,da35,da36,da37,da38),1,FUN=sum,na.rm=TRUE)[enkver2=="apri" | enkver2=="long"]
VIP_data$redmeat[enkver2=="apri" | enkver2=="long"]<-apply(cbind(da51,da52,da53,da54,da55,da56),1,FUN=sum,na.rm=TRUE)[enkver2=="apri" | enkver2=="long"]
VIP_data$desserts[enkver2=="apri" | enkver2=="long"]<-apply(cbind(da65,da66,da67,da68,da69),1,FUN=sum,na.rm=TRUE)[enkver2=="apri" | enkver2=="long"]
VIP_data$sugardrink[enkver2=="apri" | enkver2=="long"]<-apply(cbind(da74,da75),1,FUN=sum,na.rm=TRUE)[enkver2=="apri" | enkver2=="long"]
VIP_data$friedpotato[enkver2=="apri" | enkver2=="long"]<-apply(cbind(da40,da41),1,FUN=sum,na.rm=TRUE)[enkver2=="apri" | enkver2=="long"]

VIP_data$wholegrain[enkver2=="short"]<-apply(cbind(dat10,dat11,DAT18,dat22,0.25*dat52),1,FUN=sum,na.rm=TRUE)[enkver2=="short"]
VIP_data$fish[enkver2=="short"]<-apply(cbind(dat44,dat45),1,FUN=sum,na.rm=TRUE)[enkver2=="short"]
VIP_data$fruit[enkver2=="short"]<-apply(cbind(dat24,DAT25,dat26),1,FUN=sum,na.rm=TRUE)[enkver2=="short"]
VIP_data$vegetables[enkver2=="short"]<-apply(cbind(dat27,dat28,DAT29),1,FUN=sum,na.rm=TRUE)[enkver2=="short"]
VIP_data$redmeat[enkver2=="short"]<-apply(cbind(dat37,dat38,dat39,dat40,dat41,dat42),1,FUN=sum,na.rm=TRUE)[enkver2=="short"]
VIP_data$desserts[enkver2=="short"]<-apply(cbind(dat48,dat49,DAT50,dat51),1,FUN=sum,na.rm=TRUE)[enkver2=="short"]
VIP_data$sugardrink[enkver2=="short"]<-apply(cbind(dat56),1,FUN=sum,na.rm=TRUE)[enkver2=="short"]
VIP_data$friedpotato[enkver2=="short"]<-apply(cbind(DAT31),1,FUN=sum,na.rm=TRUE)[enkver2=="short"]

attach(VIP_data)

#check the direction of the associations
associations<-glm(log(bmi)~age + agesq + gender + year + ffq + wholegrain + fish + fruit + vegetables + redmeat + desserts + sugardrink 
				+ friedpotato, family = gaussian(link = "identity"))
#> summary(associations)
#
#Call:
#		glm(formula = log(bmi) ~ age + agesq + gender + year + ffq + 
#						wholegrain + fish + fruit + vegetables + redmeat + desserts + 
#						sugardrink + friedpotato, family = gaussian(link = "identity"))
#
#Deviance Residuals: 
#		Min        1Q    Median        3Q       Max  
#-0.58516  -0.09402  -0.01261   0.07938   1.04233  
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  2.336e-01  2.911e-01   0.802 0.422288    
#age          3.692e-03  6.174e-04   5.980 2.24e-09 ***
#		agesq       -1.903e-05  6.523e-06  -2.917 0.003535 ** 
#		gender2     -4.223e-02  1.180e-03 -35.796  < 2e-16 ***
#		year         1.438e-03  1.449e-04   9.926  < 2e-16 ***
#		ffq1        -7.053e-03  1.903e-03  -3.706 0.000211 ***
#		wholegrain  -3.040e-03  3.975e-04  -7.649 2.06e-14 ***
#		fish        -1.900e-02  4.296e-03  -4.422 9.82e-06 ***
#		fruit        1.910e-03  5.255e-04   3.634 0.000279 ***
#		vegetables   1.440e-03  4.970e-04   2.897 0.003771 ** 
#		redmeat      3.764e-02  1.789e-03  21.042  < 2e-16 ***
#		desserts    -8.986e-03  4.902e-04 -18.331  < 2e-16 ***
#		sugardrink   5.536e-03  1.260e-03   4.395 1.11e-05 ***
#		friedpotato  2.404e-02  4.940e-03   4.867 1.14e-06 ***
#		---
#		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.01962719)
#
#Null deviance: 1441.7  on 68828  degrees of freedom
#Residual deviance: 1350.6  on 68815  degrees of freedom
#(161 observations deleted due to missingness)
#AIC: -75212
#
#Number of Fisher Scoring iterations: 2
#

#same inapproporiate results for vegetables, fruit and desserts :(

#wholegrain score
VIP_data$wholegrain_score<-3
VIP_data$wholegrain_score[ (wholegrain>=quantile(wholegrain)[[2]]) & (wholegrain<quantile(wholegrain)[[3]]) ]<-2
VIP_data$wholegrain_score[ (wholegrain>=quantile(wholegrain)[[3]]) & (wholegrain<quantile(wholegrain)[[4]]) ]<-1
VIP_data$wholegrain_score[ (wholegrain>=quantile(wholegrain)[[4]])]<-0

#fish score
VIP_data$fish_score<-3
VIP_data$fish_score[ (fish>=quantile(fish)[[2]]) & (fish<quantile(fish)[[3]]) ]<-2
VIP_data$fish_score[ (fish>=quantile(fish)[[3]]) & (fish<quantile(fish)[[4]]) ]<-1
VIP_data$fish_score[ (fish>=quantile(fish)[[4]])]<-0

##fruit score
#VIP_data$fruit_score<-0
#VIP_data$fruit_score[ (fruit>=quantile(fruit)[[2]]) & (fruit<quantile(fruit)[[3]]) ]<-1
#VIP_data$fruit_score[ (fruit>=quantile(fruit)[[3]]) & (fruit<quantile(fruit)[[4]]) ]<-2
#VIP_data$fruit_score[ (fruit>=quantile(fruit)[[4]])]<-3
#
##vegetables score
#VIP_data$vegetables_score<-0
#VIP_data$vegetables_score[ (vegetables>=quantile(vegetables)[[2]]) & (vegetables<quantile(vegetables)[[3]]) ]<-1
#VIP_data$vegetables_score[ (vegetables>=quantile(vegetables)[[3]]) & (vegetables<quantile(vegetables)[[4]]) ]<-2
#VIP_data$vegetables_score[ (vegetables>=quantile(vegetables)[[4]])]<-3

#redmeat score
VIP_data$redmeat_score<-0
VIP_data$redmeat_score[ (redmeat>=quantile(redmeat)[[2]]) & (redmeat<quantile(redmeat)[[3]]) ]<-1
VIP_data$redmeat_score[ (redmeat>=quantile(redmeat)[[3]]) & (redmeat<quantile(redmeat)[[4]]) ]<-2
VIP_data$redmeat_score[ (redmeat>=quantile(redmeat)[[4]])]<-3

##desserts score
#VIP_data$desserts_score<-3
#VIP_data$desserts_score[ (desserts>=quantile(desserts)[[2]]) & (desserts<quantile(desserts)[[3]]) ]<-2
#VIP_data$desserts_score[ (desserts>=quantile(desserts)[[3]]) & (desserts<quantile(desserts)[[4]]) ]<-1
#VIP_data$desserts_score[ (desserts>=quantile(desserts)[[4]])]<-0

#sugardrink score
VIP_data$sugardrink_score<-0
VIP_data$sugardrink_score[ (sugardrink>=quantile(sugardrink)[[2]]) & (sugardrink<quantile(sugardrink)[[3]]) ]<-1
VIP_data$sugardrink_score[ (sugardrink>=quantile(sugardrink)[[3]]) & (sugardrink<quantile(sugardrink)[[4]]) ]<-2
VIP_data$sugardrink_score[ (sugardrink>=quantile(sugardrink)[[4]])]<-3

#friedpotato score
VIP_data$friedpotato_score<-0
VIP_data$friedpotato_score[ (friedpotato>=quantile(friedpotato)[[2]]) & (friedpotato<quantile(friedpotato)[[3]]) ]<-1
VIP_data$friedpotato_score[ (friedpotato>=quantile(friedpotato)[[3]]) & (friedpotato<quantile(friedpotato)[[4]]) ]<-2
VIP_data$friedpotato_score[ (friedpotato>=quantile(friedpotato)[[4]])]<-3

#final second diet score
VIP_data$diet_score2<-apply(VIP_data[,c(631:635)], 1, FUN=sum)
attach(VIP_data)

png("Results/environment_risk_score/bmi_vs_diet_score/bmi_vs_diet_score2.png",width=2000, height=1000)
boxplot(bmi~diet_score2, main="bmi vs second diet score", xlab="second diet score", ylab="bmi")
dev.off()

#smaller total effect size then redmeat or fried potato alone

#RESULTS
#> bmi_vs_diet<-glm(log(bmi)~age+agesq+gender+year+ffq+visit+diet_score2,family=gaussian(link="identity"))
#> summary(bmi_vs_diet)
#
#Call:
#		glm(formula = log(bmi) ~ age + agesq + gender + year + ffq + 
#						visit + diet_score2, family = gaussian(link = "identity"))
#
#Deviance Residuals: 
#		Min        1Q    Median        3Q       Max  
#-0.55858  -0.09410  -0.01274   0.08047   1.04893  
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  4.931e-01  3.531e-01   1.396    0.163    
#age          4.157e-03  6.247e-04   6.654 2.88e-11 ***
#		agesq       -2.823e-05  6.640e-06  -4.251 2.13e-05 ***
#		gender2     -3.802e-02  1.116e-03 -34.083  < 2e-16 ***
#		year         1.294e-03  1.765e-04   7.329 2.35e-13 ***
#		ffq1        -8.539e-03  1.901e-03  -4.491 7.10e-06 ***
#		visit2       1.542e-02  1.878e-03   8.209 2.26e-16 ***
#		diet_score2  1.955e-03  1.675e-04  11.670  < 2e-16 ***
#		---
#		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.01982927)
#
#Null deviance: 1441.7  on 68828  degrees of freedom
#Residual deviance: 1364.7  on 68821  degrees of freedom
#(161 observations deleted due to missingness)
#AIC: -74512
#
#Number of Fisher Scoring iterations: 2
#
#> pcor.test(log(bmi[!is.na(bmi) & !is.na(diet_score2)]),diet_score[!is.na(bmi) & !is.na(diet_score2)],cbind(age[!is.na(bmi) & !is.na(diet_score2)],
#	agesq[!is.na(bmi) & !is.na(diet_score2)],gender[!is.na(bmi) & !is.na(diet_score2)],year[!is.na(bmi) & !is.na(diet_score2)],ffq[!is.na(bmi) & !is.na(diet_score2)],
#   visit[!is.na(bmi) & !is.na(diet_score2)]))
#estimate      p.value statistic     n gp  Method
#1 0.04443905 1.952441e-31  11.66956 68829  6 pearson


##third diet score
#
#
#VIP_data$meat_on_bread[enkver2=="apri" | enkver2=="long"]<-apply(cbind(da19,da20),1,FUN=sum,na.rm=TRUE)[enkver2=="apri" | enkver2=="long"]
#VIP_data$bacon[enkver2=="apri" | enkver2=="long"]<-apply(cbind(da54),1,FUN=sum,na.rm=TRUE)[enkver2=="apri" | enkver2=="long"]
#VIP_data$other_meat[enkver2=="apri" | enkver2=="long"]<-apply(cbind(da51,da53,da55,da56),1,FUN=sum,na.rm=TRUE)[enkver2=="apri" | enkver2=="long"]
#VIP_data$white_meat[enkver2=="apri" | enkver2=="long"]<-apply(cbind(da57),1,FUN=sum,na.rm=TRUE)[enkver2=="apri" | enkver2=="long"]
#VIP_data$fried_potato[enkver2=="apri" | enkver2=="long"]<-apply(cbind(da40,da41),1,FUN=sum,na.rm=TRUE)[enkver2=="apri" | enkver2=="long"]
#VIP_data$boiled_potato[enkver2=="apri" | enkver2=="long"]<-apply(cbind(da39,da42),1,FUN=sum,na.rm=TRUE)[enkver2=="apri" | enkver2=="long"]
#
#VIP_data$meat_on_bread[enkver2=="short"]<-apply(cbind(DAT16,dat17),1,FUN=sum,na.rm=TRUE)[enkver2=="short"]
#VIP_data$bacon[enkver2=="short"]<-apply(cbind(dat40),1,FUN=sum,na.rm=TRUE)[enkver2=="short"]
#VIP_data$other_meat[enkver2=="short"]<-apply(cbind(dat37,dat39,dat41,dat42),1,FUN=sum,na.rm=TRUE)[enkver2=="short"]
#VIP_data$white_meat[enkver2=="short"]<-apply(cbind(dat43),1,FUN=sum,na.rm=TRUE)[enkver2=="short"]
#VIP_data$fried_potato[enkver2=="short"]<-apply(cbind(DAT31),1,FUN=sum,na.rm=TRUE)[enkver2=="short"]
#VIP_data$boiled_potato[enkver2=="short"]<-apply(cbind(dat30),1,FUN=sum,na.rm=TRUE)[enkver2=="short"]
#
#attach(VIP_data)
#
##meat_on_bread
#VIP_data$meat_on_bread_score<-3
#VIP_data$meat_on_bread_score[ (meat_on_bread>=quantile(meat_on_bread)[[2]]) & (meat_on_bread<quantile(meat_on_bread)[[3]]) ]<-2
#VIP_data$meat_on_bread_score[ (meat_on_bread>=quantile(meat_on_bread)[[3]]) & (meat_on_bread<quantile(meat_on_bread)[[4]]) ]<-1
#VIP_data$meat_on_bread_score[ (meat_on_bread>=quantile(meat_on_bread)[[4]])]<-0
#
##bacon
#VIP_data$bacon_score<-3
#VIP_data$bacon_score[ (bacon>=quantile(bacon)[[2]]) & (bacon<quantile(bacon)[[3]]) ]<-2
#VIP_data$bacon_score[ (bacon>=quantile(bacon)[[3]]) & (bacon<quantile(bacon)[[4]]) ]<-1
#VIP_data$bacon_score[ (bacon>=quantile(bacon)[[4]])]<-0
#
##other_meat
#VIP_data$other_meat_score<-3
#VIP_data$other_meat_score[ (other_meat>=quantile(other_meat)[[2]]) & (other_meat<quantile(other_meat)[[3]]) ]<-2
#VIP_data$other_meat_score[ (other_meat>=quantile(other_meat)[[3]]) & (other_meat<quantile(other_meat)[[4]]) ]<-1
#VIP_data$other_meat_score[ (other_meat>=quantile(other_meat)[[4]])]<-0
#
##white_meat
#VIP_data$white_meat_score<-3
#VIP_data$white_meat_score[ (white_meat>=quantile(white_meat)[[2]]) & (white_meat<quantile(white_meat)[[3]]) ]<-2
#VIP_data$white_meat_score[ (white_meat>=quantile(white_meat)[[3]]) & (white_meat<quantile(white_meat)[[4]]) ]<-1
#VIP_data$white_meat_score[ (white_meat>=quantile(white_meat)[[4]])]<-0
#
##fried_potato
#VIP_data$fried_potato_score<-3
#VIP_data$fried_potato_score[ (fried_potato>=quantile(fried_potato)[[2]]) & (fried_potato<quantile(fried_potato)[[3]]) ]<-2
#VIP_data$fried_potato_score[ (fried_potato>=quantile(fried_potato)[[3]]) & (fried_potato<quantile(fried_potato)[[4]]) ]<-1
#VIP_data$fried_potato_score[ (fried_potato>=quantile(fried_potato)[[4]])]<-0
#
##boiled_potato
#VIP_data$boiled_potato_score<-3
#VIP_data$boiled_potato_score[ (boiled_potato>=quantile(boiled_potato)[[2]]) & (boiled_potato<quantile(boiled_potato)[[3]]) ]<-2
#VIP_data$boiled_potato_score[ (boiled_potato>=quantile(boiled_potato)[[3]]) & (boiled_potato<quantile(boiled_potato)[[4]]) ]<-1
#VIP_data$boiled_potato_score[ (boiled_potato>=quantile(boiled_potato)[[4]])]<-0
#
##final second diet score
#VIP_data$diet_score3<-apply(VIP_data[,c(646:651)], 1, FUN=sum)
#
##RESULTS
##> bmi_vs_diet<-glm(log(bmi)~age+agesq+gender+year+ffq+visit+diet_score3,family=gaussian(link="identity"))
##> summary(bmi_vs_diet)
##Call:
##		glm(formula = log(bmi) ~ age + agesq + gender + year + ffq + 
##						visit + diet_score3, family = gaussian(link = "identity"))
##
##Deviance Residuals: 
##		Min        1Q    Median        3Q       Max  
##-0.56407  -0.09352  -0.01310   0.08016   1.06037  
##
##Coefficients:
##		Estimate Std. Error t value Pr(>|t|)    
##(Intercept)  7.968e-01  3.759e-01   2.120 0.034052 *  
##		age          3.944e-03  6.586e-04   5.988 2.14e-09 ***
##		agesq       -2.666e-05  6.996e-06  -3.811 0.000138 ***
##		gender2     -3.773e-02  1.144e-03 -32.971  < 2e-16 ***
##		year         1.173e-03  1.879e-04   6.240 4.39e-10 ***
##		ffq1        -8.791e-03  1.996e-03  -4.405 1.06e-05 ***
##		visit2       1.388e-02  1.986e-03   6.987 2.83e-12 ***
##		diet_score3 -3.393e-03  1.814e-04 -18.707  < 2e-16 ***
##		---
##		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
##(Dispersion parameter for gaussian family taken to be 0.01970182)
##
##Null deviance: 1293.1  on 61923  degrees of freedom
##Residual deviance: 1219.9  on 61916  degrees of freedom
##AIC: -67436
##
##Number of Fisher Scoring iterations: 2
##
##> pcor.test(log(bmi),diet_score3,cbind(age,agesq,gender,year,ffq,visit))
##estimate      p.value statistic     n gp  Method
##1 -0.07496719 7.168927e-78 -18.70667 61924  6 pearson
##>
#
#

# physical activity score

associations<-glm(log(bmi)~age + agesq + gender + year + ffq + g1_a + g1_b + g1_c + g1_d + g3_a + g3_b + g6 , family = gaussian(link = "identity"))

#> summary(associations)
#
#Call:
#		glm(formula = log(bmi) ~ age + agesq + gender + year + ffq + 
#						g1_a + g1_b + g1_c + g1_d + g3_a + g3_b + g6, family = gaussian(link = "identity"))
#
#Deviance Residuals: 
#		Min        1Q    Median        3Q       Max  
#-0.52648  -0.09119  -0.01180   0.07821   1.04594  
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -1.787e+00  3.339e-01  -5.352 8.75e-08 ***
#		age          3.564e-03  7.073e-04   5.039 4.70e-07 ***
#		agesq       -2.391e-05  7.544e-06  -3.169 0.001531 ** 
#		gender2     -3.502e-02  1.297e-03 -26.998  < 2e-16 ***
#		year         2.485e-03  1.662e-04  14.948  < 2e-16 ***
#		ffq1        -7.461e-03  2.157e-03  -3.458 0.000544 ***
#		g1_a        -3.508e-03  1.112e-03  -3.155 0.001603 ** 
#		g1_b         1.513e-05  8.460e-04   0.018 0.985729    
#		g1_c        -7.149e-04  1.089e-03  -0.656 0.511706    
#		g1_d        -6.788e-03  9.698e-04  -6.999 2.62e-12 ***
#		g3_a        -3.005e-03  6.019e-04  -4.992 5.98e-07 ***
#		g3_b        -1.660e-03  5.634e-04  -2.947 0.003208 ** 
#		g6          -8.582e-03  5.059e-04 -16.964  < 2e-16 ***
#		---
#		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.01907641)
#
#Null deviance: 1038.09  on 50669  degrees of freedom
#Residual deviance:  966.35  on 50657  degrees of freedom
#(18320 observations deleted due to missingness)
#AIC: -56808
#
#Number of Fisher Scoring iterations: 2
#
#> 

# g1_b and g1_c are problematic, inappropriate direction and/or insignificant, looking at the effect size of combined travel to work it is smaller with those two,
# so only a and d can be used 

#travel to work
VIP_data$travel_to_work<-(g1_a-1) + (g1_d-1)

#leisure time
VIP_data$leisure_walk<-g3_a
VIP_data$leisure_cycle<-g3_b

#PA_frequency
VIP_data$PA_frequency<-g6-1

# none of the combinations result in a biger effect size than g6 alone

#final PA score
VIP_data$PA_score<-PA_frequency

attach(VIP_data)

#RESULTS
bmi_vs_PA<-glm(log(bmi)~age + agesq + gender + year + ffq + PA_score, family = gaussian(link = "identity"))

#> summary(bmi_vs_PA)
#
#Call:
#		glm(formula = log(bmi) ~ age + agesq + gender + year + ffq + 
#						PA_score, family = gaussian(link = "identity"))
#
#Deviance Residuals: 
#		Min        1Q    Median        3Q       Max  
#-0.54557  -0.09317  -0.01230   0.07992   1.05915  
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -1.943e+00  2.909e-01  -6.680 2.41e-11 ***
#		age          3.764e-03  6.214e-04   6.057 1.40e-09 ***
#		agesq       -2.611e-05  6.568e-06  -3.975 7.05e-05 ***
#		gender2     -4.040e-02  1.082e-03 -37.325  < 2e-16 ***
#		year         2.544e-03  1.448e-04  17.570  < 2e-16 ***
#		ffq1        -6.351e-03  1.905e-03  -3.333 0.000859 ***
#		PA_score    -1.032e-02  4.310e-04 -23.937  < 2e-16 ***
#		---
#		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.01972202)
#
#Null deviance: 1421.9  on 67894  degrees of freedom
#Residual deviance: 1338.9  on 67888  degrees of freedom
#(1095 observations deleted due to missingness)
#AIC: -73870
#
#Number of Fisher Scoring iterations: 2

#> pcor.test(log(bmi[!is.na(bmi) & !is.na(PA_score)]),PA_score[!is.na(bmi) & !is.na(PA_score)],cbind(age[!is.na(bmi) & !is.na(PA_score)],
#  agesq[!is.na(bmi) & !is.na(PA_score)],gender[!is.na(bmi) & !is.na(PA_score)],year[!is.na(bmi) & !is.na(PA_score)],ffq[!is.na(bmi) & !is.na(PA_score)],
#  visit[!is.na(bmi) & !is.na(PA_score)]))
#estimate       p.value statistic     n gp  Method
#1 -0.09213679 6.974231e-128 -24.10892 67895  6 pearson

#change the direction
PA_score_aux<-PA_score
VIP_data$PA_score[PA_score_aux==0]<-4
VIP_data$PA_score[PA_score_aux==1]<-3
VIP_data$PA_score[PA_score_aux==2]<-2
VIP_data$PA_score[PA_score_aux==3]<-1
VIP_data$PA_score[PA_score_aux==4]<-0

attach(VIP_data)

VIP_data$diet2_PA<-diet_score2+VIP_data$PA_score
VIP_data$diet1_PA<-diet_score+VIP_data$PA_score

detach(VIP_data)
attach(VIP_data)

png("Results/environment_risk_score/bmi_vs_diet_score/bmi_vs_diet_score2_and_PA.png",width=2000, height=1000)
boxplot(bmi~diet2_PA, main="bmi vs second diet score and PA", xlab="second diet score plus PA", ylab="bmi")
dev.off()

png("Results/environment_risk_score/bmi_vs_diet_score/bmi_vs_diet_score1_and_PA.png",width=2000, height=1000)
boxplot(bmi~diet1_PA, main="bmi vs first diet score and PA", xlab="first diet score plus PA", ylab="bmi")
dev.off()

#look at the effect size of combined diet and PA scores



#make it proportionate to the scores
VIP_data$PA_score1<-PA_score*(7/4)
VIP_data$PA_score2<-PA_score*(15/4)

# taking the proportionate PA_score or diet score makes the results and boxplots worst, suming the scores as they are gives better results

#for smoking, only sm_status == 2 (former smokers) is positevly associated with bmi, effect size 2.059e-02, smokers, non-smokers, former occasional smokers are negatively associated
# while occasional smokers are insignificant

#for alcohol alkosum1 and j1 are negatively asociated, while j2 and j3 are positevly associated, but there are ~50000 missing values, out of ~68000 for the jN variables

detach(VIP_data)

attach(VIP_data)



png("scatterplot_visit1_bmi_vs_score1.png",width=2000, height=1000)
plot(environment_score1[visit==1],bmi[visit==1])
dev.off()

png("scatterplot_visit2_bmi_vs_score1.png",width=2000, height=1000)
plot(environment_score1[visit==2],bmi[visit==2])
dev.off()

png("boxplot_visit1_bmi_vs_score1.png",width=2000, height=1000)
boxplot(bmi[visit==1]~environment_score1[visit==1])
dev.off()

png("boxplot_visit2_bmi_vs_score1.png",width=2000, height=1000)
boxplot(bmi[visit==2]~environment_score1[visit==2])
dev.off()



png("scatterplot_visit1_bmi_vs_score2.png",width=2000, height=1000)
plot(environment_score2[visit==1],bmi[visit==1])
dev.off()

png("scatterplot_visit2_bmi_vs_score2.png",width=2000, height=1000)
plot(environment_score2[visit==2],bmi[visit==2])
dev.off()

png("boxplot_visit1_bmi_vs_score2.png",width=2000, height=1000)
boxplot(bmi[visit==1]~environment_score2[visit==1])
dev.off()

png("boxplot_visit2_bmi_vs_score2.png",width=2000, height=1000)
boxplot(bmi[visit==2]~environment_score2[visit==2])
dev.off()

