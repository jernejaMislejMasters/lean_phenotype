#load cleaned subset of subjects with the right two visits and first extract those that have all the values for the right variables
VIP_data <- read.csv("VIP_data/VIP_170206_cleaned_subset.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)

#variables used for obesity associated unhealthy environment

attach(VIP_data)
#get subject ids where any of the variables have na
exclude_subject_id<-VIP_data[is.na(gender) | is.na(age) | is.na(g6) | is.na(enkver) | is.na(enkver2) | is.na(exclude) | is.na(ensum1) | 
				is.na(protsum1) | is.na(kolhsum1) | is.na(sacksum1) | is.na(fibesum1) | is.na(alkosum1) | is.na(fettsum1) | is.na(mfetsum1) | 
				is.na(MONOsum1) | is.na(POLYsum1) | is.na(FA182_sum1) | is.na(FA183_sum1) | is.na(FA204_sum1) | is.na(FA205_sum1) | is.na(FA226_sum1)
				| is.na(MAGNsum1) | is.na(NATRsum1) | is.na(FOSFsum1) | is.na(selesum1) | is.na(ZINCsum1) | is.na(retisum1) | is.na(TIAMsum1) | 
				is.na(Folasum1) | is.na(B2sum1) | is.na(NIACsum1) | is.na(B6sum1) | is.na(B12sum1) | is.na(askosum1) | is.na(Dsum1) | 
				is.na(tokosum1) | is.na(jernsum1) | is.na(JODIsum1) | is.na(kalcsum1) | is.na(KALIsum1) | is.na(besok1) | is.na(efter_090901) 
				| is.na(sm_status) | is.na(datum) | is.na(vikt) | is.na(bmi) | is.na(alkosum1),c("Subject_id")]

#exclude those subjects, even if in the other visit all the values are there
VIP_data_subset<-VIP_data[!(Subject_id %in% exclude_subject_id),]

detach(VIP_data)

#30962 subjects have all the data at both visits

attach(VIP_data_subset)

#explore the unhealthy, obesogenic environment, using all of the variables 

#create factor for the questionare
VIP_data_subset$ffq<-0
VIP_data_subset$ffq[enkver2=="long"]<-1
VIP_data_subset$ffq[enkver2=="apri"]<-1
VIP_data_subset$ffq<-as.factor(VIP_data_subset$ffq)

#age squared
VIP_data_subset$agesq<-age*age

#factorize gender and visit
VIP_data_subset$gender<-as.factor(VIP_data_subset$gender)
VIP_data_subset$visit<-as.factor(VIP_data_subset$visit)


#explore the diet

attach(VIP_data_subset)

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

#construct the diet score, look at the upper limit only, since we are focusing on the obesogenic unhealthy environment
#if no limit cutpoints given, make quartiles and rank




#POLYsum1
VIP_data_subset$POLYsum1_score<-2
VIP_data_subset$POLYsum1_score[(PUFAE >= 5) & (PUFAE <= 10) ]<-1
VIP_data_subset$POLYsum1_score[PUFAE > 10]<-0

#MONOsum1
VIP_data_subset$MONOsum1_score<-2
VIP_data_subset$MONOsum1_score[(MUFAE >= 10) & (MUFAE <= 20) ]<-1
VIP_data_subset$MONOsum1_score[MUFAE > 20]<-0


#VIP_data_subset$mfetsum1_score<-0
#VIP_data_subset$mfetsum1_score[SATFAT <= 10]<-1

#fettsum1
VIP_data_subset$fettsum1_score<-2
VIP_data_subset$fettsum1_score[(TOTFAT >= 25) & (TOTFAT <= 40) ]<-1
VIP_data_subset$fettsum1_score[TOTFAT > 40]<-0


#VIP_data_subset$fibesum1_score<-0
#VIP_data_subset$fibesum1_score[fibesum1 >= 25]<-1


#VIP_data_subset$FA_score<-0
#VIP_data_subset$FA_score[FA >= 3]<-1


#VIP_data_subset$NATRsum1_score<-0
#VIP_data_subset$NATRsum1_score[NATRsum1 <= 6]<-1

#kolhsum1
VIP_data_subset$kolhsum1_score<-2
VIP_data_subset$kolhsum1_score[(CARBOHYDRATES >= 45) & (CARBOHYDRATES <= 60) ]<-1
VIP_data_subset$kolhsum1_score[CARBOHYDRATES > 60]<-0


#VIP_data_subset$sacksum1_score<-0
#VIP_data_subset$sacksum1_score[SUGAR < 10]<-1

#protsum1
VIP_data_subset$protsum1_score<-2
VIP_data_subset$protsum1_score[(PROTEIN >= 10) & (PROTEIN <= 20) ]<-1
VIP_data_subset$protsum1_score[PROTEIN > 20]<-0

#for those where there are no guide lines, get quartiles and rank accordingly

#ensum1
VIP_data_subset$ensum1_score<-3
VIP_data_subset$ensum1_score[ (ensum1>=quantile(ensum1)[[2]]) & (ensum1<quantile(ensum1)[[3]]) ]<-2
VIP_data_subset$ensum1_score[ (ensum1>=quantile(ensum1)[[3]]) & (ensum1<quantile(ensum1)[[4]]) ]<-1
VIP_data_subset$ensum1_score[ (ensum1>=quantile(ensum1)[[4]])]<-0

#protsum1_anim
VIP_data_subset$protsum1_anim_score<-3
VIP_data_subset$protsum1_anim_score[ (protsum1_anim>=quantile(protsum1_anim)[[2]]) & (protsum1_anim<quantile(protsum1_anim)[[3]]) ]<-2
VIP_data_subset$protsum1_anim_score[ (protsum1_anim>=quantile(protsum1_anim)[[3]]) & (protsum1_anim<quantile(protsum1_anim)[[4]]) ]<-1
VIP_data_subset$protsum1_anim_score[ (protsum1_anim>=quantile(protsum1_anim)[[4]])]<-0

#kolesum1
VIP_data_subset$kolesum1_score<-3
VIP_data_subset$kolesum1_score[ (kolesum1>=quantile(kolesum1)[[2]]) & (kolesum1<quantile(kolesum1)[[3]]) ]<-2
VIP_data_subset$kolesum1_score[ (kolesum1>=quantile(kolesum1)[[3]]) & (kolesum1<quantile(kolesum1)[[4]]) ]<-1
VIP_data_subset$kolesum1_score[ (kolesum1>=quantile(kolesum1)[[4]])]<-0

#FULLKsum1
VIP_data_subset$FULLKsum1_score<-0
VIP_data_subset$FULLKsum1_score[ (FULLKsum1>=quantile(FULLKsum1)[[2]]) & (FULLKsum1<quantile(FULLKsum1)[[3]]) ]<-1
VIP_data_subset$FULLKsum1_score[ (FULLKsum1>=quantile(FULLKsum1)[[3]]) & (FULLKsum1<quantile(FULLKsum1)[[4]]) ]<-2
VIP_data_subset$FULLKsum1_score[ (FULLKsum1>=quantile(FULLKsum1)[[4]])]<-3

attach(VIP_data_subset)

#final first diet score
VIP_data_subset$diet_score<-apply(VIP_data_subset[,c(613:621)], 1, FUN=sum)

attach(VIP_data_subset)

#RESULTS
#> bmi_vs_diet<-glm(log(bmi)~age+agesq+gender+year+ffq+visit+diet_score,family=gaussian(link="identity"))
#> summary(bmi_vs_diet)
#
#Call:
#		glm(formula = log(bmi) ~ age + agesq + gender + year + ffq + 
#						visit + diet_score, family = gaussian(link = "identity"))
#
#Deviance Residuals: 
#		Min        1Q    Median        3Q       Max  
#-0.54974  -0.09387  -0.01268   0.08073   1.04244  
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.095e+00  3.792e-01   2.887  0.00389 ** 
#		age          4.974e-03  6.602e-04   7.535 4.96e-14 ***
#		agesq       -3.661e-05  7.000e-06  -5.230 1.70e-07 ***
#		gender2     -3.331e-02  1.255e-03 -26.537  < 2e-16 ***
#		year         1.016e-03  1.896e-04   5.358 8.46e-08 ***
#		ffq1        -1.228e-02  2.016e-03  -6.090 1.14e-09 ***
#		visit2       1.524e-02  1.991e-03   7.655 1.95e-14 ***
#		diet_score  -2.921e-03  2.111e-04 -13.835  < 2e-16 ***
#		---
#		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.0197521)
#
#Null deviance: 1293.1  on 61923  degrees of freedom
#Residual deviance: 1223.0  on 61916  degrees of freedom
#AIC: -67278
#
#Number of Fisher Scoring iterations: 2
#
#library(ppcor)
#> pcor.test(log(bmi),diet_score,cbind(age,agesq,gender,year,ffq,visit))
#estimate      p.value statistic     n gp  Method
#1 -0.05551657 1.807337e-43 -13.83548 61924  6 pearson


#second diet score
VIP_data_subset$wholegrain[enkver2=="apri" | enkver2=="long"]<-apply(cbind(da10,da11,da22,da23,da27,0.25*da70),1,FUN=sum,na.rm=TRUE)[enkver2=="apri" | enkver2=="long"]
VIP_data_subset$fish[enkver2=="apri" | enkver2=="long"]<-apply(cbind(da60,da61),1,FUN=sum,na.rm=TRUE)[enkver2=="apri" | enkver2=="long"]
VIP_data_subset$fruit[enkver2=="apri" | enkver2=="long"]<-apply(cbind(da29,da30,da31,da32),1,FUN=sum,na.rm=TRUE)[enkver2=="apri" | enkver2=="long"]
VIP_data_subset$vegetables[enkver2=="apri" | enkver2=="long"]<-apply(cbind(da33,da34,da35,da36,da37,da38),1,FUN=sum,na.rm=TRUE)[enkver2=="apri" | enkver2=="long"]
VIP_data_subset$redmeat[enkver2=="apri" | enkver2=="long"]<-apply(cbind(da51,da52,da53,da54,da55,da56),1,FUN=sum,na.rm=TRUE)[enkver2=="apri" | enkver2=="long"]
VIP_data_subset$desserts[enkver2=="apri" | enkver2=="long"]<-apply(cbind(da65,da66,da67,da68,da69),1,FUN=sum,na.rm=TRUE)[enkver2=="apri" | enkver2=="long"]
VIP_data_subset$sugardrink[enkver2=="apri" | enkver2=="long"]<-apply(cbind(da74,da75),1,FUN=sum,na.rm=TRUE)[enkver2=="apri" | enkver2=="long"]
VIP_data_subset$friedpotato[enkver2=="apri" | enkver2=="long"]<-apply(cbind(da40,da41),1,FUN=sum,na.rm=TRUE)[enkver2=="apri" | enkver2=="long"]

VIP_data_subset$wholegrain[enkver2=="short"]<-apply(cbind(dat10,dat11,DAT18,dat22,0.25*dat52),1,FUN=sum,na.rm=TRUE)[enkver2=="short"]
VIP_data_subset$fish[enkver2=="short"]<-apply(cbind(dat44,dat45),1,FUN=sum,na.rm=TRUE)[enkver2=="short"]
VIP_data_subset$fruit[enkver2=="short"]<-apply(cbind(dat24,DAT25,dat26),1,FUN=sum,na.rm=TRUE)[enkver2=="short"]
VIP_data_subset$vegetables[enkver2=="short"]<-apply(cbind(dat27,dat28,DAT29),1,FUN=sum,na.rm=TRUE)[enkver2=="short"]
VIP_data_subset$redmeat[enkver2=="short"]<-apply(cbind(dat37,dat38,dat39,dat40,dat41,dat42),1,FUN=sum,na.rm=TRUE)[enkver2=="short"]
VIP_data_subset$desserts[enkver2=="short"]<-apply(cbind(dat48,dat49,DAT50,dat51),1,FUN=sum,na.rm=TRUE)[enkver2=="short"]
VIP_data_subset$sugardrink[enkver2=="short"]<-apply(cbind(dat56),1,FUN=sum,na.rm=TRUE)[enkver2=="short"]
VIP_data_subset$friedpotato[enkver2=="short"]<-apply(cbind(DAT31),1,FUN=sum,na.rm=TRUE)[enkver2=="short"]

attach(VIP_data_subset)

#wholegrain score
VIP_data_subset$wholegrain_score<-0
VIP_data_subset$wholegrain_score[ (wholegrain>=quantile(wholegrain)[[2]]) & (wholegrain<quantile(wholegrain)[[3]]) ]<-1
VIP_data_subset$wholegrain_score[ (wholegrain>=quantile(wholegrain)[[3]]) & (wholegrain<quantile(wholegrain)[[4]]) ]<-2
VIP_data_subset$wholegrain_score[ (wholegrain>=quantile(wholegrain)[[4]])]<-3

#fish score
VIP_data_subset$fish_score<-0
VIP_data_subset$fish_score[ (fish>=quantile(fish)[[2]]) & (fish<quantile(fish)[[3]]) ]<-1
VIP_data_subset$fish_score[ (fish>=quantile(fish)[[3]]) & (fish<quantile(fish)[[4]]) ]<-2
VIP_data_subset$fish_score[ (fish>=quantile(fish)[[4]])]<-3

#fruit score
VIP_data_subset$fruit_score<-0
VIP_data_subset$fruit_score[ (fruit>=quantile(fruit)[[2]]) & (fruit<quantile(fruit)[[3]]) ]<-1
VIP_data_subset$fruit_score[ (fruit>=quantile(fruit)[[3]]) & (fruit<quantile(fruit)[[4]]) ]<-2
VIP_data_subset$fruit_score[ (fruit>=quantile(fruit)[[4]])]<-3

#vegetables score
VIP_data_subset$vegetables_score<-0
VIP_data_subset$vegetables_score[ (vegetables>=quantile(vegetables)[[2]]) & (vegetables<quantile(vegetables)[[3]]) ]<-1
VIP_data_subset$vegetables_score[ (vegetables>=quantile(vegetables)[[3]]) & (vegetables<quantile(vegetables)[[4]]) ]<-2
VIP_data_subset$vegetables_score[ (vegetables>=quantile(vegetables)[[4]])]<-3

#redmeat score
VIP_data_subset$redmeat_score<-3
VIP_data_subset$redmeat_score[ (redmeat>=quantile(redmeat)[[2]]) & (redmeat<quantile(redmeat)[[3]]) ]<-2
VIP_data_subset$redmeat_score[ (redmeat>=quantile(redmeat)[[3]]) & (redmeat<quantile(redmeat)[[4]]) ]<-1
VIP_data_subset$redmeat_score[ (redmeat>=quantile(redmeat)[[4]])]<-0

#desserts score
VIP_data_subset$desserts_score<-3
VIP_data_subset$desserts_score[ (desserts>=quantile(desserts)[[2]]) & (desserts<quantile(desserts)[[3]]) ]<-2
VIP_data_subset$desserts_score[ (desserts>=quantile(desserts)[[3]]) & (desserts<quantile(desserts)[[4]]) ]<-1
VIP_data_subset$desserts_score[ (desserts>=quantile(desserts)[[4]])]<-0

#sugardrink score
VIP_data_subset$sugardrink_score<-0
VIP_data_subset$sugardrink_score[ (sugardrink>=quantile(sugardrink)[[2]]) & (sugardrink<quantile(sugardrink)[[3]]) ]<-1
VIP_data_subset$sugardrink_score[ (sugardrink>=quantile(sugardrink)[[3]]) & (sugardrink<quantile(sugardrink)[[4]]) ]<-2
VIP_data_subset$sugardrink_score[ (sugardrink>=quantile(sugardrink)[[4]])]<-3

#friedpotato score
VIP_data_subset$friedpotato_score<-0
VIP_data_subset$friedpotato_score[ (friedpotato>=quantile(friedpotato)[[2]]) & (friedpotato<quantile(friedpotato)[[3]]) ]<-1
VIP_data_subset$friedpotato_score[ (friedpotato>=quantile(friedpotato)[[3]]) & (friedpotato<quantile(friedpotato)[[4]]) ]<-2
VIP_data_subset$friedpotato_score[ (friedpotato>=quantile(friedpotato)[[4]])]<-3

#final second diet score
VIP_data_subset$diet_score2<-apply(VIP_data_subset[,c(631:638)], 1, FUN=sum)


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
#-0.55364  -0.09387  -0.01312   0.07982   1.05104  
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  3.756e-01  3.764e-01   0.998  0.31829    
#age          4.338e-03  6.605e-04   6.568 5.13e-11 ***
#		agesq       -3.270e-05  7.008e-06  -4.665 3.09e-06 ***
#		gender2     -4.147e-02  1.161e-03 -35.726  < 2e-16 ***
#		year         1.367e-03  1.882e-04   7.263 3.84e-13 ***
#		ffq1        -8.659e-03  2.001e-03  -4.327 1.52e-05 ***
#		visit2       1.394e-02  1.992e-03   7.001 2.56e-12 ***
#		diet_score2  5.584e-04  1.831e-04   3.050  0.00229 ** 
#		---
#		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.01981019)
#
#Null deviance: 1293.1  on 61923  degrees of freedom
#Residual deviance: 1226.6  on 61916  degrees of freedom
#AIC: -67096
#
#Number of Fisher Scoring iterations: 2
#
#> pcor.test(log(bmi),diet_score2,cbind(age,agesq,gender,year,ffq,visit))
#estimate     p.value statistic     n gp  Method
#1 0.0122558 0.002290672  3.049831 61924  6 pearson

#third diet score


VIP_data_subset$meat_on_bread[enkver2=="apri" | enkver2=="long"]<-apply(cbind(da19,da20),1,FUN=sum,na.rm=TRUE)[enkver2=="apri" | enkver2=="long"]
VIP_data_subset$bacon[enkver2=="apri" | enkver2=="long"]<-apply(cbind(da54),1,FUN=sum,na.rm=TRUE)[enkver2=="apri" | enkver2=="long"]
VIP_data_subset$other_meat[enkver2=="apri" | enkver2=="long"]<-apply(cbind(da51,da53,da55,da56),1,FUN=sum,na.rm=TRUE)[enkver2=="apri" | enkver2=="long"]
VIP_data_subset$white_meat[enkver2=="apri" | enkver2=="long"]<-apply(cbind(da57),1,FUN=sum,na.rm=TRUE)[enkver2=="apri" | enkver2=="long"]
VIP_data_subset$fried_potato[enkver2=="apri" | enkver2=="long"]<-apply(cbind(da40,da41),1,FUN=sum,na.rm=TRUE)[enkver2=="apri" | enkver2=="long"]
VIP_data_subset$boiled_potato[enkver2=="apri" | enkver2=="long"]<-apply(cbind(da39,da42),1,FUN=sum,na.rm=TRUE)[enkver2=="apri" | enkver2=="long"]

VIP_data_subset$meat_on_bread[enkver2=="short"]<-apply(cbind(DAT16,dat17),1,FUN=sum,na.rm=TRUE)[enkver2=="short"]
VIP_data_subset$bacon[enkver2=="short"]<-apply(cbind(dat40),1,FUN=sum,na.rm=TRUE)[enkver2=="short"]
VIP_data_subset$other_meat[enkver2=="short"]<-apply(cbind(dat37,dat39,dat41,dat42),1,FUN=sum,na.rm=TRUE)[enkver2=="short"]
VIP_data_subset$white_meat[enkver2=="short"]<-apply(cbind(dat43),1,FUN=sum,na.rm=TRUE)[enkver2=="short"]
VIP_data_subset$fried_potato[enkver2=="short"]<-apply(cbind(DAT31),1,FUN=sum,na.rm=TRUE)[enkver2=="short"]
VIP_data_subset$boiled_potato[enkver2=="short"]<-apply(cbind(dat30),1,FUN=sum,na.rm=TRUE)[enkver2=="short"]

attach(VIP_data_subset)

#meat_on_bread
VIP_data_subset$meat_on_bread_score<-3
VIP_data_subset$meat_on_bread_score[ (meat_on_bread>=quantile(meat_on_bread)[[2]]) & (meat_on_bread<quantile(meat_on_bread)[[3]]) ]<-2
VIP_data_subset$meat_on_bread_score[ (meat_on_bread>=quantile(meat_on_bread)[[3]]) & (meat_on_bread<quantile(meat_on_bread)[[4]]) ]<-1
VIP_data_subset$meat_on_bread_score[ (meat_on_bread>=quantile(meat_on_bread)[[4]])]<-0

#bacon
VIP_data_subset$bacon_score<-3
VIP_data_subset$bacon_score[ (bacon>=quantile(bacon)[[2]]) & (bacon<quantile(bacon)[[3]]) ]<-2
VIP_data_subset$bacon_score[ (bacon>=quantile(bacon)[[3]]) & (bacon<quantile(bacon)[[4]]) ]<-1
VIP_data_subset$bacon_score[ (bacon>=quantile(bacon)[[4]])]<-0

#other_meat
VIP_data_subset$other_meat_score<-3
VIP_data_subset$other_meat_score[ (other_meat>=quantile(other_meat)[[2]]) & (other_meat<quantile(other_meat)[[3]]) ]<-2
VIP_data_subset$other_meat_score[ (other_meat>=quantile(other_meat)[[3]]) & (other_meat<quantile(other_meat)[[4]]) ]<-1
VIP_data_subset$other_meat_score[ (other_meat>=quantile(other_meat)[[4]])]<-0

#white_meat
VIP_data_subset$white_meat_score<-3
VIP_data_subset$white_meat_score[ (white_meat>=quantile(white_meat)[[2]]) & (white_meat<quantile(white_meat)[[3]]) ]<-2
VIP_data_subset$white_meat_score[ (white_meat>=quantile(white_meat)[[3]]) & (white_meat<quantile(white_meat)[[4]]) ]<-1
VIP_data_subset$white_meat_score[ (white_meat>=quantile(white_meat)[[4]])]<-0

#fried_potato
VIP_data_subset$fried_potato_score<-3
VIP_data_subset$fried_potato_score[ (fried_potato>=quantile(fried_potato)[[2]]) & (fried_potato<quantile(fried_potato)[[3]]) ]<-2
VIP_data_subset$fried_potato_score[ (fried_potato>=quantile(fried_potato)[[3]]) & (fried_potato<quantile(fried_potato)[[4]]) ]<-1
VIP_data_subset$fried_potato_score[ (fried_potato>=quantile(fried_potato)[[4]])]<-0

#boiled_potato
VIP_data_subset$boiled_potato_score<-3
VIP_data_subset$boiled_potato_score[ (boiled_potato>=quantile(boiled_potato)[[2]]) & (boiled_potato<quantile(boiled_potato)[[3]]) ]<-2
VIP_data_subset$boiled_potato_score[ (boiled_potato>=quantile(boiled_potato)[[3]]) & (boiled_potato<quantile(boiled_potato)[[4]]) ]<-1
VIP_data_subset$boiled_potato_score[ (boiled_potato>=quantile(boiled_potato)[[4]])]<-0

#final second diet score
VIP_data_subset$diet_score3<-apply(VIP_data_subset[,c(646:651)], 1, FUN=sum)

#RESULTS
#> bmi_vs_diet<-glm(log(bmi)~age+agesq+gender+year+ffq+visit+diet_score3,family=gaussian(link="identity"))
#> summary(bmi_vs_diet)
#Call:
#		glm(formula = log(bmi) ~ age + agesq + gender + year + ffq + 
#						visit + diet_score3, family = gaussian(link = "identity"))
#
#Deviance Residuals: 
#		Min        1Q    Median        3Q       Max  
#-0.56407  -0.09352  -0.01310   0.08016   1.06037  
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  7.968e-01  3.759e-01   2.120 0.034052 *  
#		age          3.944e-03  6.586e-04   5.988 2.14e-09 ***
#		agesq       -2.666e-05  6.996e-06  -3.811 0.000138 ***
#		gender2     -3.773e-02  1.144e-03 -32.971  < 2e-16 ***
#		year         1.173e-03  1.879e-04   6.240 4.39e-10 ***
#		ffq1        -8.791e-03  1.996e-03  -4.405 1.06e-05 ***
#		visit2       1.388e-02  1.986e-03   6.987 2.83e-12 ***
#		diet_score3 -3.393e-03  1.814e-04 -18.707  < 2e-16 ***
#		---
#		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.01970182)
#
#Null deviance: 1293.1  on 61923  degrees of freedom
#Residual deviance: 1219.9  on 61916  degrees of freedom
#AIC: -67436
#
#Number of Fisher Scoring iterations: 2
#
#> pcor.test(log(bmi),diet_score3,cbind(age,agesq,gender,year,ffq,visit))
#estimate      p.value statistic     n gp  Method
#1 -0.07496719 7.168927e-78 -18.70667 61924  6 pearson
#>



# physical activity score
#travel to work
VIP_data_subset$travel_to_work<-0
VIP_data_subset$travel_to_work[(g1_a==3 | g1_a==4) & !is.na(g1_a)]<-1
VIP_data_subset$travel_to_work[(g1_b==3 | g1_b==4) & !is.na(g1_b)]<-VIP_data_subset$travel_to_work[(g1_b==3 | g1_b==4) & !is.na(g1_b)]+1
VIP_data_subset$travel_to_work[(g1_c==3 | g1_c==4) & !is.na(g1_c)]<-VIP_data_subset$travel_to_work[(g1_c==3 | g1_c==4) & !is.na(g1_c)]+1
VIP_data_subset$travel_to_work[(g1_d==3 | g1_d==4) & !is.na(g1_d)]<-VIP_data_subset$travel_to_work[(g1_d==3 | g1_d==4) & !is.na(g1_d)]+1

#leisure time
VIP_data_subset$leisure_walk<-g3_a
VIP_data_subset$leisure_walk[is.na(VIP_data_subset$leisure_walk)]<-0
VIP_data_subset$leisure_cycle<-g3_b
VIP_data_subset$leisure_cycle[is.na(VIP_data_subset$leisure_cycle)]<-0

#PA_frequency
VIP_data_subset$PA_frequency<-0
VIP_data_subset$PA_frequency[g6==3]<-1
VIP_data_subset$PA_frequency[g6==4 | g6==5]<-2

#final PA score
VIP_data_subset$PA_score<-apply(VIP_data_subset[,c(653:656)], 1, FUN=sum)

attach(VIP_data_subset)

#RESULTS
#> bmi_vs_PA<-glm(log(bmi)~age+agesq+gender+year+ffq+visit+PA_score,family=gaussian(link="identity"))
#> summary(bmi_vs_PA)
#Call:
#		glm(formula = log(bmi) ~ age + agesq + gender + year + ffq + 
#						visit + PA_score, family = gaussian(link = "identity"))
#
#Deviance Residuals: 
#		Min        1Q    Median        3Q       Max  
#-0.52418  -0.09265  -0.01233   0.07858   1.03940  
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  2.146e-01  4.149e-01   0.517 0.604982    
#age          4.084e-03  7.182e-04   5.686 1.31e-08 ***
#		agesq       -2.899e-05  7.670e-06  -3.779 0.000157 ***
#		gender2     -3.139e-02  1.290e-03 -24.332  < 2e-16 ***
#		year         1.466e-03  2.074e-04   7.067 1.61e-12 ***
#		ffq1        -8.824e-03  2.192e-03  -4.026 5.68e-05 ***
#		visit2       1.493e-02  2.157e-03   6.922 4.51e-12 ***
#		PA_score    -5.282e-03  1.939e-04 -27.241  < 2e-16 ***
#		---
#		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.01950215)
#
#Null deviance: 1054.21  on 50560  degrees of freedom
#Residual deviance:  985.89  on 50553  degrees of freedom
#(11363 observations deleted due to missingness)
#AIC: -55574
#
#Number of Fisher Scoring iterations: 2
#> pcor.test(log(bmi),PA_score,cbind(age,agesq,gender,year,ffq,visit))
#estimate       p.value statistic     n gp  Method
#1 -0.1201556 7.980428e-198 -30.11643 61924  6 pearson
#



VIP_data_subset$PA_score<-0
VIP_data_subset$PA_score[ (g6==3) | (g6==4) | (g6==5)]<-1


#alcohol score
VIP_data_subset$ALKO_score<-0
VIP_data_subset$ALKO_score[(gender==1) & (alkosum1 < 20) ]<-1
VIP_data_subset$ALKO_score[(gender==2) & (alkosum1 < 10) ]<-1


#smoking score
#years from smoking stop
time_from_sm_stop<-(age-sm_stop)
VIP_data_subset$SMOKING_score<-0
VIP_data_subset$SMOKING_score[(sm_status==3) | ((sm_status==2) & (!is.na(time_from_sm_stop)) & (time_from_sm_stop>=10)) |
				((sm_status==5) & (!is.na(time_from_sm_stop)) & (time_from_sm_stop>=10))]<-1


#final_environment_score, 1. with all diet score and 2. with only macro nutrients
VIP_data_subset$environment_score1<-apply(VIP_data_subset[,c(640,642:644)], 1, FUN=sum)
VIP_data_subset$environment_score2<-apply(VIP_data_subset[,c(641:644)], 1, FUN=sum)


detach(VIP_data_subset)

attach(VIP_data_subset)



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

