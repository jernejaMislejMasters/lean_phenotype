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


#explore the unhealthy, obesogenic environment, using all of the variables 

#create factor for the questionare
VIP_data_subset$ffq<-0
VIP_data_subset$ffq[enkver2=="long"]<-1
VIP_data_subset$ffq[enkver2=="apri"]<-1
VIP_data_subset$ffq<-as.factor(VIP_data_subset$ffq)

#age squared
VIP_data_subset$agesq<-age*age

#factorize gender
VIP_data_subset$gender<-as.factor(VIP_data_subset$gender)


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

#create a model with all the variables, look for significant ones, remove insignificant, check for multicolinearity
library(glmulti)

full_model<-glm(bmi~age+agesq+gender+year+ffq+PUFAE+ MUFAE+ SATFAT+ TOTFAT+ NATRsum1+ SUGAR+ CARBOHYDRATES+ PROTEIN+ TRANFAT+
				protsum1+ protsum1_anim+protsum1_veg, family=gaussian(link="identity"))

model_selection <- glmulti(full_model, level = 2, crit="aic")

summary(model_selection)

weightable(model_selection)


