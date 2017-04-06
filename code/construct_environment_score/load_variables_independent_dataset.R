attach(VIP_data_independant)

#create factor for the questionare
VIP_data_independant$ffq<-0
VIP_data_independant$ffq[enkver2=="long"]<-1
VIP_data_independant$ffq[enkver2=="apri"]<-1
VIP_data_independant$ffq_factor<-as.factor(VIP_data_independant$ffq)

#age squared
VIP_data_independant$agesq<-age*age

#factorize gender
VIP_data_independant$gender_factor<-as.factor(VIP_data_independant$gender)

#make a few other bmi variables, that are more cleaned from extreme values / possible outliers, there are several ways to do this using different cutpoints, first start with simple, then try more
#complicated. Simple way is to just take the bottom and top % of the distribtuion, checking several cutpoints for the %, since bmi is normaly distributed, it might be good to 
#take this into account, but can try other way aswell.

#original bmi
png("Results/distributions/bmi_distribution.png")
qqnorm(bmi)
qqline(bmi)
dev.off()
png("Results/distributions/bmi_histogram.png")
hist(bmi)
dev.off()

#make a log transformed bmi, making sure it is really normal then standardize it to get the extremes
VIP_data_independant$bmi_norm<-log(bmi)
VIP_data_independant$bmi_sd_norm[!is.na(bmi)]<-(VIP_data_independant$bmi_norm[!is.na(bmi)] - mean(VIP_data_independant$bmi_norm[!is.na(bmi)]))/(sd(VIP_data_independant$bmi_norm[!is.na(bmi)]))
png("Results/distributions/log_sd_bmi_distribution.png")
qqnorm(VIP_data_independant$bmi_sd_norm)
qqline(VIP_data_independant$bmi_sd_norm)
dev.off()
png("Results/distributions/log_sd_bmi_histogram.png")
hist(VIP_data_independant$bmi_sd_norm)
dev.off()


#take 3SD for cutpoint
VIP_data_independant$bmi_3SD<-bmi
length(VIP_data_independant$bmi_3SD[!is.na(bmi) & (VIP_data_independant$bmi_sd > 3 | VIP_data_independant$bmi_sd < -3)])#313
VIP_data_independant$bmi_3SD[!is.na(bmi) & (VIP_data_independant$bmi_sd > 3 | VIP_data_independant$bmi_sd < -3)]<-NA
VIP_data_independant$bmi_3SD<-log(VIP_data_independant$bmi_3SD)
VIP_data_independant$bmi_3SD_sd[!is.na(VIP_data_independant$bmi_3SD)]<-(VIP_data_independant$bmi_3SD[!is.na(VIP_data_independant$bmi_3SD)] - mean(VIP_data_independant$bmi_3SD[!is.na(VIP_data_independant$bmi_3SD)]))/(sd(VIP_data_independant$bmi_3SD[!is.na(VIP_data_independant$bmi_3SD)]))

png("Results/distributions/filtered_3SD_bmi_distribution.png")
qqnorm(VIP_data_independant$bmi_3SD_sd)
qqline(VIP_data_independant$bmi_3SD_sd)
dev.off()
png("Results/distributions/filtered_3SD_bmi_histogram.png")
hist(VIP_data_independant$bmi_3SD_sd)
dev.off()


#take 4SD for cutpoint
VIP_data_independant$bmi_4SD<-bmi
length(VIP_data_independant$bmi_4SD[!is.na(bmi) & (VIP_data_independant$bmi_sd > 4 | VIP_data_independant$bmi_sd < -4)])#38
VIP_data_independant$bmi_4SD[!is.na(bmi) & (VIP_data_independant$bmi_sd > 4 | VIP_data_independant$bmi_sd < -4)]<-NA
VIP_data_independant$bmi_4SD<-log(VIP_data_independant$bmi_4SD)
VIP_data_independant$bmi_4SD_sd[!is.na(VIP_data_independant$bmi_4SD)]<-(VIP_data_independant$bmi_4SD[!is.na(VIP_data_independant$bmi_4SD)] - mean(VIP_data_independant$bmi_4SD[!is.na(VIP_data_independant$bmi_4SD)]))/(sd(VIP_data_independant$bmi_4SD[!is.na(VIP_data_independant$bmi_4SD)]))

png("Results/distributions/filtered_4SD_bmi_distribution.png")
qqnorm(VIP_data_independant$bmi_4SD_sd)
qqline(VIP_data_independant$bmi_4SD_sd)
dev.off()
png("Results/distributions/filtered_4SD_bmi_histogram.png")
hist(VIP_data_independant$bmi_4SD_sd)
dev.off()


#take 35SD for cutpoint
VIP_data_independant$bmi_35SD<-bmi
length(VIP_data_independant$bmi_35SD[!is.na(bmi) & (VIP_data_independant$bmi_sd > 3.5 | VIP_data_independant$bmi_sd < -3.5)])#116
VIP_data_independant$bmi_35SD[!is.na(bmi) & (VIP_data_independant$bmi_sd > 3.5 | VIP_data_independant$bmi_sd < -3.5)]<-NA
VIP_data_independant$bmi_35SD<-log(VIP_data_independant$bmi_35SD)
VIP_data_independant$bmi_35SD_sd[!is.na(VIP_data_independant$bmi_35SD)]<-(VIP_data_independant$bmi_35SD[!is.na(VIP_data_independant$bmi_35SD)] - mean(VIP_data_independant$bmi_35SD[!is.na(VIP_data_independant$bmi_35SD)]))/(sd(VIP_data_independant$bmi_35SD[!is.na(VIP_data_independant$bmi_35SD)]))

png("Results/distributions/filtered_35SD_bmi_distribution.png")
qqnorm(VIP_data_independant$bmi_35SD_sd)
qqline(VIP_data_independant$bmi_35SD_sd)
dev.off()
png("Results/distributions/filtered_35SD_bmi_histogram.png")
hist(VIP_data_independant$bmi_35SD_sd)
dev.off()



#create several new "transformed" variables, either log transformed and standaridized or categorized based on the recommendations

# POLYsum1 is raw, POLYsum1_transformed is raw log tranformed and standardized, POLYsum1_transformed_4SD cleaned from extremes, POLYsum1_ofTEI is expressed in % of total energy intake, POLYsum1_ofTEI_transformed is log transformed and 
# standardized expressed in % of TEI, POLYsum1_ofTEI_categorized is expressed in % of TEI and categorized based on guidelines

#continuous log transformed and standardized
VIP_data_independant$POLYsum1_transformed<-log(POLYsum1)
VIP_data_independant$POLYsum1_transformed[!is.na(POLYsum1)]<-(VIP_data_independant$POLYsum1_transformed[!is.na(POLYsum1)]-
			mean(VIP_data_independant$POLYsum1_transformed[!is.na(POLYsum1)]))/sd(VIP_data_independant$POLYsum1_transformed[!is.na(POLYsum1)])

VIP_data_independant$POLYsum1_transformed_4SD <- POLYsum1
length(VIP_data_independant$POLYsum1[!is.na(POLYsum1) & (VIP_data_independant$POLYsum1_transformed > 4 | VIP_data_independant$POLYsum1_transformed < -4)])#13
VIP_data_independant$POLYsum1_transformed_4SD[!is.na(POLYsum1) & (VIP_data_independant$POLYsum1_transformed > 4 | VIP_data_independant$POLYsum1_transformed < -4)]<-NA

# % of TEI
VIP_data_independant$POLYsum1_ofTEI<-(100*9*VIP_data_independant$POLYsum1_transformed_4SD)/ensum1

#log and standardize raw with exclusion
VIP_data_independant$POLYsum1_transformed_4SD<-log(VIP_data_independant$POLYsum1_transformed_4SD)
VIP_data_independant$POLYsum1_transformed_4SD[!is.na(VIP_data_independant$POLYsum1_transformed_4SD)]<-(VIP_data_independant$POLYsum1_transformed_4SD[!is.na(VIP_data_independant$POLYsum1_transformed_4SD)] - 
			mean(VIP_data_independant$POLYsum1_transformed_4SD[!is.na(VIP_data_independant$POLYsum1_transformed_4SD)]))/(sd(VIP_data_independant$POLYsum1_transformed_4SD[!is.na(VIP_data_independant$POLYsum1_transformed_4SD)]))

# % of TEI log transformed and standardized
VIP_data_independant$POLYsum1_ofTEI_transformed<-log(VIP_data_independant$POLYsum1_ofTEI)		
VIP_data_independant$POLYsum1_ofTEI_transformed[!is.na(VIP_data_independant$POLYsum1_transformed_4SD)]<-(VIP_data_independant$POLYsum1_ofTEI_transformed[!is.na(VIP_data_independant$POLYsum1_transformed_4SD)]-
			mean(VIP_data_independant$POLYsum1_ofTEI_transformed[!is.na(VIP_data_independant$POLYsum1_transformed_4SD)]))/sd(VIP_data_independant$POLYsum1_ofTEI_transformed[!is.na(VIP_data_independant$POLYsum1_transformed_4SD)])

# categorizing the % of TEI based on guidelines
VIP_data_independant$POLYsum1_ofTEI_categorized_g[!is.na(POLYsum1)]<-0
VIP_data_independant$POLYsum1_ofTEI_categorized_g[!is.na(POLYsum1) & VIP_data_independant$POLYsum1_ofTEI >= 5 & VIP_data_independant$POLYsum1_ofTEI <= 10 ]<-1
VIP_data_independant$POLYsum1_ofTEI_categorized_g[!is.na(POLYsum1) & VIP_data_independant$POLYsum1_ofTEI > 10]<-2




# MONOsum1 is raw, MONOsum1_transformed is raw log tranformed and standardized, MONOsum1_transformed_4SD cleaned from extremes, MONOsum1_ofTEI is expressed in % of total energy intake, MONOsum1_ofTEI_transformed is log transformed and 
# standardized expressed in % of TEI, MONOsum1_ofTEI_categorized is expressed in % of TEI and categorized based on guidelines

#continuous log transformed and standardized
VIP_data_independant$MONOsum1_transformed<-log(MONOsum1)
VIP_data_independant$MONOsum1_transformed[!is.na(MONOsum1)]<-(VIP_data_independant$MONOsum1_transformed[!is.na(MONOsum1)]-
			mean(VIP_data_independant$MONOsum1_transformed[!is.na(MONOsum1)]))/sd(VIP_data_independant$MONOsum1_transformed[!is.na(MONOsum1)])

VIP_data_independant$MONOsum1_transformed_4SD <- MONOsum1
length(VIP_data_independant$MONOsum1[!is.na(MONOsum1) & (VIP_data_independant$MONOsum1_transformed > 4 | VIP_data_independant$MONOsum1_transformed < -4)])#11
VIP_data_independant$MONOsum1_transformed_4SD[!is.na(MONOsum1) & (VIP_data_independant$MONOsum1_transformed > 4 | VIP_data_independant$MONOsum1_transformed < -4)]<-NA

# % of TEI
VIP_data_independant$MONOsum1_ofTEI<-(100*9*VIP_data_independant$MONOsum1_transformed_4SD)/ensum1

#log and standardize raw with exclusion
VIP_data_independant$MONOsum1_transformed_4SD<-log(VIP_data_independant$MONOsum1_transformed_4SD)
VIP_data_independant$MONOsum1_transformed_4SD[!is.na(VIP_data_independant$MONOsum1_transformed_4SD)]<-(VIP_data_independant$MONOsum1_transformed_4SD[!is.na(VIP_data_independant$MONOsum1_transformed_4SD)] - 
			mean(VIP_data_independant$MONOsum1_transformed_4SD[!is.na(VIP_data_independant$MONOsum1_transformed_4SD)]))/(sd(VIP_data_independant$MONOsum1_transformed_4SD[!is.na(VIP_data_independant$MONOsum1_transformed_4SD)]))

# % of TEI log transformed and standardized
VIP_data_independant$MONOsum1_ofTEI_transformed<-log(VIP_data_independant$MONOsum1_ofTEI)		
VIP_data_independant$MONOsum1_ofTEI_transformed[!is.na(VIP_data_independant$MONOsum1_transformed_4SD)]<-(VIP_data_independant$MONOsum1_ofTEI_transformed[!is.na(VIP_data_independant$MONOsum1_transformed_4SD)]-
			mean(VIP_data_independant$MONOsum1_ofTEI_transformed[!is.na(VIP_data_independant$MONOsum1_transformed_4SD)]))/sd(VIP_data_independant$MONOsum1_ofTEI_transformed[!is.na(VIP_data_independant$MONOsum1_transformed_4SD)])

# categorizing the % of TEI based on guidelines
VIP_data_independant$MONOsum1_ofTEI_categorized_g[!is.na(MONOsum1)]<-0
VIP_data_independant$MONOsum1_ofTEI_categorized_g[!is.na(MONOsum1) & VIP_data_independant$MONOsum1_ofTEI >= 10 & VIP_data_independant$MONOsum1_ofTEI <= 20 ]<-1
VIP_data_independant$MONOsum1_ofTEI_categorized_g[!is.na(MONOsum1) & VIP_data_independant$MONOsum1_ofTEI > 20]<-2



# mfetsum1 is raw, mfetsum1_transformed is raw log tranformed and standardized, mfetsum1_transformed_4SD cleaned from extremes, mfetsum1_ofTEI is expressed in % of total energy intake, mfetsum1_ofTEI_transformed is log transformed and 
# standardized expressed in % of TEI, mfetsum1_ofTEI_categorized is expressed in % of TEI and categorized based on guidelines

#continuous log transformed and standardized
VIP_data_independant$mfetsum1_transformed<-log(mfetsum1)
VIP_data_independant$mfetsum1_transformed[!is.na(mfetsum1)]<-(VIP_data_independant$mfetsum1_transformed[!is.na(mfetsum1)]-
			mean(VIP_data_independant$mfetsum1_transformed[!is.na(mfetsum1)]))/sd(VIP_data_independant$mfetsum1_transformed[!is.na(mfetsum1)])

VIP_data_independant$mfetsum1_transformed_4SD <- mfetsum1
length(VIP_data_independant$mfetsum1[!is.na(mfetsum1) & (VIP_data_independant$mfetsum1_transformed > 4 | VIP_data_independant$mfetsum1_transformed < -4)])#12
VIP_data_independant$mfetsum1_transformed_4SD[!is.na(mfetsum1) & (VIP_data_independant$mfetsum1_transformed > 4 | VIP_data_independant$mfetsum1_transformed < -4)]<-NA

# % of TEI
VIP_data_independant$mfetsum1_ofTEI<-(100*9*VIP_data_independant$mfetsum1_transformed_4SD)/ensum1

#log and standardize raw with exclusion
VIP_data_independant$mfetsum1_transformed_4SD<-log(VIP_data_independant$mfetsum1_transformed_4SD)
VIP_data_independant$mfetsum1_transformed_4SD[!is.na(VIP_data_independant$mfetsum1_transformed_4SD)]<-(VIP_data_independant$mfetsum1_transformed_4SD[!is.na(VIP_data_independant$mfetsum1_transformed_4SD)] - 
			mean(VIP_data_independant$mfetsum1_transformed_4SD[!is.na(VIP_data_independant$mfetsum1_transformed_4SD)]))/(sd(VIP_data_independant$mfetsum1_transformed_4SD[!is.na(VIP_data_independant$mfetsum1_transformed_4SD)]))

# % of TEI log transformed and standardized
VIP_data_independant$mfetsum1_ofTEI_transformed<-log(VIP_data_independant$mfetsum1_ofTEI)		
VIP_data_independant$mfetsum1_ofTEI_transformed[!is.na(VIP_data_independant$mfetsum1_transformed_4SD)]<-(VIP_data_independant$mfetsum1_ofTEI_transformed[!is.na(VIP_data_independant$mfetsum1_transformed_4SD)]-
			mean(VIP_data_independant$mfetsum1_ofTEI_transformed[!is.na(VIP_data_independant$mfetsum1_transformed_4SD)]))/sd(VIP_data_independant$mfetsum1_ofTEI_transformed[!is.na(VIP_data_independant$mfetsum1_transformed_4SD)])

# categorizing the % of TEI based on guidelines
VIP_data_independant$mfetsum1_ofTEI_categorized_g[!is.na(mfetsum1)]<-2
VIP_data_independant$mfetsum1_ofTEI_categorized_g[!is.na(mfetsum1) & VIP_data_independant$mfetsum1_ofTEI <= 10 ]<-1




# fettsum1 is raw, fettsum1_transformed is raw log tranformed and standardized, fettsum1_transformed_4SD cleaned from extremes, fettsum1_ofTEI is expressed in % of total energy intake, fettsum1_ofTEI_transformed is log transformed and 
# standardized expressed in % of TEI, fettsum1_ofTEI_categorized is expressed in % of TEI and categorized based on guidelines

#continuous log transformed and standardized
VIP_data_independant$fettsum1_transformed<-log(fettsum1)
VIP_data_independant$fettsum1_transformed[!is.na(fettsum1)]<-(VIP_data_independant$fettsum1_transformed[!is.na(fettsum1)]-
			mean(VIP_data_independant$fettsum1_transformed[!is.na(fettsum1)]))/sd(VIP_data_independant$fettsum1_transformed[!is.na(fettsum1)])

VIP_data_independant$fettsum1_transformed_4SD <- fettsum1
length(VIP_data_independant$fettsum1[!is.na(fettsum1) & (VIP_data_independant$fettsum1_transformed > 4 | VIP_data_independant$fettsum1_transformed < -4)])#9
VIP_data_independant$fettsum1_transformed_4SD[!is.na(fettsum1) & (VIP_data_independant$fettsum1_transformed > 4 | VIP_data_independant$fettsum1_transformed < -4)]<-NA

# % of TEI
VIP_data_independant$fettsum1_ofTEI<-(100*9*VIP_data_independant$fettsum1_transformed_4SD)/ensum1

#log and standardize raw with exclusion
VIP_data_independant$fettsum1_transformed_4SD<-log(VIP_data_independant$fettsum1_transformed_4SD)
VIP_data_independant$fettsum1_transformed_4SD[!is.na(VIP_data_independant$fettsum1_transformed_4SD)]<-(VIP_data_independant$fettsum1_transformed_4SD[!is.na(VIP_data_independant$fettsum1_transformed_4SD)] - 
			mean(VIP_data_independant$fettsum1_transformed_4SD[!is.na(VIP_data_independant$fettsum1_transformed_4SD)]))/(sd(VIP_data_independant$fettsum1_transformed_4SD[!is.na(VIP_data_independant$fettsum1_transformed_4SD)]))

# % of TEI log transformed and standardized
VIP_data_independant$fettsum1_ofTEI_transformed<-log(VIP_data_independant$fettsum1_ofTEI)		
VIP_data_independant$fettsum1_ofTEI_transformed[!is.na(VIP_data_independant$fettsum1_transformed_4SD)]<-(VIP_data_independant$fettsum1_ofTEI_transformed[!is.na(VIP_data_independant$fettsum1_transformed_4SD)]-
			mean(VIP_data_independant$fettsum1_ofTEI_transformed[!is.na(VIP_data_independant$fettsum1_transformed_4SD)]))/sd(VIP_data_independant$fettsum1_ofTEI_transformed[!is.na(VIP_data_independant$fettsum1_transformed_4SD)])

# categorizing the % of TEI based on guidelines
VIP_data_independant$fettsum1_ofTEI_categorized_g[!is.na(fettsum1)]<-0
VIP_data_independant$fettsum1_ofTEI_categorized_g[!is.na(fettsum1) & VIP_data_independant$fettsum1_ofTEI >= 25 & VIP_data_independant$fettsum1_ofTEI <= 40 ]<-1
VIP_data_independant$fettsum1_ofTEI_categorized_g[!is.na(fettsum1) & VIP_data_independant$fettsum1_ofTEI > 40]<-2



# FA is raw, FA_transformed is raw log tranformed and standardized, FA_transformed_4SD cleaned from extremes, FA_ofTEI is expressed in % of total energy intake, FA_ofTEI_transformed is log transformed and 
# standardized expressed in % of TEI, FA_ofTEI_categorized is expressed in % of TEI and categorized based on guidelines

#acids are a combination of several variables
VIP_data_independant$FA<-FA183_sum1 + FA205_sum1 + FA226_sum1 + FA182_sum1 + FA204_sum1
detach(VIP_data_independant)
attach(VIP_data_independant)

#continuous log transformed and standardized
VIP_data_independant$FA_transformed<-log(FA)
VIP_data_independant$FA_transformed[!is.na(FA)]<-(VIP_data_independant$FA_transformed[!is.na(FA)]-
			mean(VIP_data_independant$FA_transformed[!is.na(FA)]))/sd(VIP_data_independant$FA_transformed[!is.na(FA)])

VIP_data_independant$FA_transformed_4SD <- FA
length(VIP_data_independant$FA[!is.na(FA) & (VIP_data_independant$FA_transformed > 4 | VIP_data_independant$FA_transformed < -4)])#15
VIP_data_independant$FA_transformed_4SD[!is.na(FA) & (VIP_data_independant$FA_transformed > 4 | VIP_data_independant$FA_transformed < -4)]<-NA

# % of TEI
VIP_data_independant$FA_ofTEI<-(100*9*VIP_data_independant$FA_transformed_4SD)/ensum1

#log and standardize raw with exclusion
VIP_data_independant$FA_transformed_4SD<-log(VIP_data_independant$FA_transformed_4SD)
VIP_data_independant$FA_transformed_4SD[!is.na(VIP_data_independant$FA_transformed_4SD)]<-(VIP_data_independant$FA_transformed_4SD[!is.na(VIP_data_independant$FA_transformed_4SD)] - 
			mean(VIP_data_independant$FA_transformed_4SD[!is.na(VIP_data_independant$FA_transformed_4SD)]))/(sd(VIP_data_independant$FA_transformed_4SD[!is.na(VIP_data_independant$FA_transformed_4SD)]))

# % of TEI log transformed and standardized
VIP_data_independant$FA_ofTEI_transformed<-log(VIP_data_independant$FA_ofTEI)		
VIP_data_independant$FA_ofTEI_transformed[!is.na(VIP_data_independant$FA_transformed_4SD)]<-(VIP_data_independant$FA_ofTEI_transformed[!is.na(VIP_data_independant$FA_transformed_4SD)]-
			mean(VIP_data_independant$FA_ofTEI_transformed[!is.na(VIP_data_independant$FA_transformed_4SD)]))/sd(VIP_data_independant$FA_ofTEI_transformed[!is.na(VIP_data_independant$FA_transformed_4SD)])

# categorizing the % of TEI based on guidelines
VIP_data_independant$FA_ofTEI_categorized_g[!is.na(FA)]<-1
VIP_data_independant$FA_ofTEI_categorized_g[!is.na(FA) & VIP_data_independant$FA_ofTEI < 3 ]<-2




# kolhsum1 is raw, kolhsum1_transformed is raw log tranformed and standardized, kolhsum1_transformed_4SD cleaned from extremes, kolhsum1_ofTEI is expressed in % of total energy intake, kolhsum1_ofTEI_transformed is log transformed and 
# standardized expressed in % of TEI, kolhsum1_ofTEI_categorized is expressed in % of TEI and categorized based on guidelines

#continuous log transformed and standardized
VIP_data_independant$kolhsum1_transformed<-log(kolhsum1)
VIP_data_independant$kolhsum1_transformed[!is.na(kolhsum1)]<-(VIP_data_independant$kolhsum1_transformed[!is.na(kolhsum1)]-
			mean(VIP_data_independant$kolhsum1_transformed[!is.na(kolhsum1)]))/sd(VIP_data_independant$kolhsum1_transformed[!is.na(kolhsum1)])

VIP_data_independant$kolhsum1_transformed_4SD <- kolhsum1
length(VIP_data_independant$kolhsum1[!is.na(kolhsum1) & (VIP_data_independant$kolhsum1_transformed > 4 | VIP_data_independant$kolhsum1_transformed < -4)])#47
VIP_data_independant$kolhsum1_transformed_4SD[!is.na(kolhsum1) & (VIP_data_independant$kolhsum1_transformed > 4 | VIP_data_independant$kolhsum1_transformed < -4)]<-NA

# % of TEI
VIP_data_independant$kolhsum1_ofTEI<-(100*4*VIP_data_independant$kolhsum1_transformed_4SD)/ensum1

#log and standardize raw with exclusion
VIP_data_independant$kolhsum1_transformed_4SD<-log(VIP_data_independant$kolhsum1_transformed_4SD)
VIP_data_independant$kolhsum1_transformed_4SD[!is.na(VIP_data_independant$kolhsum1_transformed_4SD)]<-(VIP_data_independant$kolhsum1_transformed_4SD[!is.na(VIP_data_independant$kolhsum1_transformed_4SD)] - 
			mean(VIP_data_independant$kolhsum1_transformed_4SD[!is.na(VIP_data_independant$kolhsum1_transformed_4SD)]))/(sd(VIP_data_independant$kolhsum1_transformed_4SD[!is.na(VIP_data_independant$kolhsum1_transformed_4SD)]))

# % of TEI log transformed and standardized
VIP_data_independant$kolhsum1_ofTEI_transformed<-log(VIP_data_independant$kolhsum1_ofTEI)		
VIP_data_independant$kolhsum1_ofTEI_transformed[!is.na(VIP_data_independant$kolhsum1_transformed_4SD)]<-(VIP_data_independant$kolhsum1_ofTEI_transformed[!is.na(VIP_data_independant$kolhsum1_transformed_4SD)]-
			mean(VIP_data_independant$kolhsum1_ofTEI_transformed[!is.na(VIP_data_independant$kolhsum1_transformed_4SD)]))/sd(VIP_data_independant$kolhsum1_ofTEI_transformed[!is.na(VIP_data_independant$kolhsum1_transformed_4SD)])

# categorizing the % of TEI based on guidelines
VIP_data_independant$kolhsum1_ofTEI_categorized_g[!is.na(kolhsum1)]<-0
VIP_data_independant$kolhsum1_ofTEI_categorized_g[!is.na(kolhsum1) & VIP_data_independant$kolhsum1_ofTEI >= 45 & VIP_data_independant$kolhsum1_ofTEI <= 60 ]<-1
VIP_data_independant$kolhsum1_ofTEI_categorized_g[!is.na(kolhsum1) & VIP_data_independant$kolhsum1_ofTEI > 60]<-2



# sacksum1 is raw, sacksum1_transformed is raw log tranformed and standardized, sacksum1_transformed_4SD cleaned from extremes, sacksum1_ofTEI is expressed in % of total energy intake, sacksum1_ofTEI_transformed is log transformed and 
# standardized expressed in % of TEI, sacksum1_ofTEI_categorized is expressed in % of TEI and categorized based on guidelines

#continuous log transformed and standardized
VIP_data_independant$sacksum1_transformed<-log(sacksum1)
VIP_data_independant$sacksum1_transformed[!is.na(sacksum1)]<-(VIP_data_independant$sacksum1_transformed[!is.na(sacksum1)]-
			mean(VIP_data_independant$sacksum1_transformed[!is.na(sacksum1)]))/sd(VIP_data_independant$sacksum1_transformed[!is.na(sacksum1)])

VIP_data_independant$sacksum1_transformed_4SD <- sacksum1
length(VIP_data_independant$sacksum1[!is.na(sacksum1) & (VIP_data_independant$sacksum1_transformed > 4 | VIP_data_independant$sacksum1_transformed < -4)])#37
VIP_data_independant$sacksum1_transformed_4SD[!is.na(sacksum1) & (VIP_data_independant$sacksum1_transformed > 4 | VIP_data_independant$sacksum1_transformed < -4)]<-NA

# % of TEI
VIP_data_independant$sacksum1_ofTEI<-(100*4*VIP_data_independant$sacksum1_transformed_4SD)/ensum1

#log and standardize raw with exclusion
VIP_data_independant$sacksum1_transformed_4SD<-log(VIP_data_independant$sacksum1_transformed_4SD)
VIP_data_independant$sacksum1_transformed_4SD[!is.na(VIP_data_independant$sacksum1_transformed_4SD)]<-(VIP_data_independant$sacksum1_transformed_4SD[!is.na(VIP_data_independant$sacksum1_transformed_4SD)] - 
			mean(VIP_data_independant$sacksum1_transformed_4SD[!is.na(VIP_data_independant$sacksum1_transformed_4SD)]))/(sd(VIP_data_independant$sacksum1_transformed_4SD[!is.na(VIP_data_independant$sacksum1_transformed_4SD)]))

# % of TEI log transformed and standardized
VIP_data_independant$sacksum1_ofTEI_transformed<-log(VIP_data_independant$sacksum1_ofTEI)		
VIP_data_independant$sacksum1_ofTEI_transformed[!is.na(VIP_data_independant$sacksum1_transformed_4SD)]<-(VIP_data_independant$sacksum1_ofTEI_transformed[!is.na(VIP_data_independant$sacksum1_transformed_4SD)]-
			mean(VIP_data_independant$sacksum1_ofTEI_transformed[!is.na(VIP_data_independant$sacksum1_transformed_4SD)]))/sd(VIP_data_independant$sacksum1_ofTEI_transformed[!is.na(VIP_data_independant$sacksum1_transformed_4SD)])

# categorizing the % of TEI based on guidelines
VIP_data_independant$sacksum1_ofTEI_categorized_g[!is.na(sacksum1)]<-2
VIP_data_independant$sacksum1_ofTEI_categorized_g[!is.na(sacksum1) & VIP_data_independant$sacksum1_ofTEI < 10 ]<-1




# protsum1 is raw, protsum1_transformed is raw log tranformed and standardized, protsum1_transformed_4SD cleaned from extremes, protsum1_ofTEI is expressed in % of total energy intake, protsum1_ofTEI_transformed is log transformed and 
# standardized expressed in % of TEI, protsum1_ofTEI_categorized is expressed in % of TEI and categorized based on guidelines

#continuous log transformed and standardized
VIP_data_independant$protsum1_transformed<-log(protsum1)
VIP_data_independant$protsum1_transformed[!is.na(protsum1)]<-(VIP_data_independant$protsum1_transformed[!is.na(protsum1)]-
			mean(VIP_data_independant$protsum1_transformed[!is.na(protsum1)]))/sd(VIP_data_independant$protsum1_transformed[!is.na(protsum1)])

VIP_data_independant$protsum1_transformed_4SD <- protsum1
length(VIP_data_independant$protsum1[!is.na(protsum1) & (VIP_data_independant$protsum1_transformed > 4 | VIP_data_independant$protsum1_transformed < -4)])#5
VIP_data_independant$protsum1_transformed_4SD[!is.na(protsum1) & (VIP_data_independant$protsum1_transformed > 4 | VIP_data_independant$protsum1_transformed < -4)]<-NA

# % of TEI
VIP_data_independant$protsum1_ofTEI<-(100*4*VIP_data_independant$protsum1_transformed_4SD)/ensum1

#log and standardize raw with exclusion
VIP_data_independant$protsum1_transformed_4SD<-log(VIP_data_independant$protsum1_transformed_4SD)
VIP_data_independant$protsum1_transformed_4SD[!is.na(VIP_data_independant$protsum1_transformed_4SD)]<-(VIP_data_independant$protsum1_transformed_4SD[!is.na(VIP_data_independant$protsum1_transformed_4SD)] - 
			mean(VIP_data_independant$protsum1_transformed_4SD[!is.na(VIP_data_independant$protsum1_transformed_4SD)]))/(sd(VIP_data_independant$protsum1_transformed_4SD[!is.na(VIP_data_independant$protsum1_transformed_4SD)]))

# % of TEI log transformed and standardized
VIP_data_independant$protsum1_ofTEI_transformed<-log(VIP_data_independant$protsum1_ofTEI)		
VIP_data_independant$protsum1_ofTEI_transformed[!is.na(VIP_data_independant$protsum1_transformed_4SD)]<-(VIP_data_independant$protsum1_ofTEI_transformed[!is.na(VIP_data_independant$protsum1_transformed_4SD)]-
			mean(VIP_data_independant$protsum1_ofTEI_transformed[!is.na(VIP_data_independant$protsum1_transformed_4SD)]))/sd(VIP_data_independant$protsum1_ofTEI_transformed[!is.na(VIP_data_independant$protsum1_transformed_4SD)])

# categorizing the % of TEI based on guidelines
VIP_data_independant$protsum1_ofTEI_categorized_g[!is.na(protsum1)]<-0
VIP_data_independant$protsum1_ofTEI_categorized_g[!is.na(protsum1) & VIP_data_independant$protsum1_ofTEI >= 10 & VIP_data_independant$protsum1_ofTEI <= 20 ]<-1
VIP_data_independant$protsum1_ofTEI_categorized_g[!is.na(protsum1) & VIP_data_independant$protsum1_ofTEI > 20]<-2




# fibesum1 is raw, fibesum1_transformed is raw log tranformed and standardized, fibesum1_transformed_4SD cleaned from extremes, fibesum1_ofTEI is expressed in % of total energy intake, fibesum1_ofTEI_transformed is log transformed and 
# standardized expressed in % of TEI, fibesum1_ofTEI_categorized is expressed in % of TEI and categorized based on guidelines

#continuous log transformed and standardized
VIP_data_independant$fibesum1_transformed<-log(fibesum1)
VIP_data_independant$fibesum1_transformed[!is.na(fibesum1)]<-(VIP_data_independant$fibesum1_transformed[!is.na(fibesum1)]-
			mean(VIP_data_independant$fibesum1_transformed[!is.na(fibesum1)]))/sd(VIP_data_independant$fibesum1_transformed[!is.na(fibesum1)])

VIP_data_independant$fibesum1_transformed_4SD <- fibesum1
length(VIP_data_independant$fibesum1[!is.na(fibesum1) & (VIP_data_independant$fibesum1_transformed > 4 | VIP_data_independant$fibesum1_transformed < -4)])#24
VIP_data_independant$fibesum1_transformed_4SD[!is.na(fibesum1) & (VIP_data_independant$fibesum1_transformed > 4 | VIP_data_independant$fibesum1_transformed < -4)]<-NA

# % of TEI
VIP_data_independant$fibesum1_ofTEI<-(100*4*VIP_data_independant$fibesum1_transformed_4SD)/ensum1

#log and standardize raw with exclusion
VIP_data_independant$fibesum1_transformed_4SD<-log(VIP_data_independant$fibesum1_transformed_4SD)
VIP_data_independant$fibesum1_transformed_4SD[!is.na(VIP_data_independant$fibesum1_transformed_4SD)]<-(VIP_data_independant$fibesum1_transformed_4SD[!is.na(VIP_data_independant$fibesum1_transformed_4SD)] - 
			mean(VIP_data_independant$fibesum1_transformed_4SD[!is.na(VIP_data_independant$fibesum1_transformed_4SD)]))/(sd(VIP_data_independant$fibesum1_transformed_4SD[!is.na(VIP_data_independant$fibesum1_transformed_4SD)]))

# % of TEI log transformed and standardized
VIP_data_independant$fibesum1_ofTEI_transformed<-log(VIP_data_independant$fibesum1_ofTEI)		
VIP_data_independant$fibesum1_ofTEI_transformed[!is.na(VIP_data_independant$fibesum1_transformed_4SD)]<-(VIP_data_independant$fibesum1_ofTEI_transformed[!is.na(VIP_data_independant$fibesum1_transformed_4SD)]-
			mean(VIP_data_independant$fibesum1_ofTEI_transformed[!is.na(VIP_data_independant$fibesum1_transformed_4SD)]))/sd(VIP_data_independant$fibesum1_ofTEI_transformed[!is.na(VIP_data_independant$fibesum1_transformed_4SD)])

# categorizing based on guidelines
VIP_data_independant$fibesum1_categorized_g[!is.na(fibesum1)]<-1
VIP_data_independant$fibesum1_categorized_g[!is.na(fibesum1) & fibesum1 < 25 ]<-2



# NATRsum1 is raw, NATRsum1_transformed is raw log tranformed and standardized, NATRsum1_transformed_4SD cleaned from extremes, NATRsum1_ofTEI is expressed in % of total energy intake, NATRsum1_ofTEI_transformed is log transformed and 
# standardized expressed in % of TEI, NATRsum1_ofTEI_categorized is expressed in % of TEI and categorized based on guidelines

#continuous log transformed and standardized
VIP_data_independant$NATRsum1_transformed<-log(NATRsum1)
VIP_data_independant$NATRsum1_transformed[!is.na(NATRsum1)]<-(VIP_data_independant$NATRsum1_transformed[!is.na(NATRsum1)]-
			mean(VIP_data_independant$NATRsum1_transformed[!is.na(NATRsum1)]))/sd(VIP_data_independant$NATRsum1_transformed[!is.na(NATRsum1)])

VIP_data_independant$NATRsum1_transformed_4SD <- NATRsum1
length(VIP_data_independant$NATRsum1[!is.na(NATRsum1) & (VIP_data_independant$NATRsum1_transformed > 4 | VIP_data_independant$NATRsum1_transformed < -4)])#7
VIP_data_independant$NATRsum1_transformed_4SD[!is.na(NATRsum1) & (VIP_data_independant$NATRsum1_transformed > 4 | VIP_data_independant$NATRsum1_transformed < -4)]<-NA

# % of TEI
VIP_data_independant$NATRsum1_ofTEI<-(100*4*VIP_data_independant$NATRsum1_transformed_4SD)/ensum1

#log and standardize raw with exclusion
VIP_data_independant$NATRsum1_transformed_4SD<-log(VIP_data_independant$NATRsum1_transformed_4SD)
VIP_data_independant$NATRsum1_transformed_4SD[!is.na(VIP_data_independant$NATRsum1_transformed_4SD)]<-(VIP_data_independant$NATRsum1_transformed_4SD[!is.na(VIP_data_independant$NATRsum1_transformed_4SD)] - 
			mean(VIP_data_independant$NATRsum1_transformed_4SD[!is.na(VIP_data_independant$NATRsum1_transformed_4SD)]))/(sd(VIP_data_independant$NATRsum1_transformed_4SD[!is.na(VIP_data_independant$NATRsum1_transformed_4SD)]))

# % of TEI log transformed and standardized
VIP_data_independant$NATRsum1_ofTEI_transformed<-log(VIP_data_independant$NATRsum1_ofTEI)		
VIP_data_independant$NATRsum1_ofTEI_transformed[!is.na(VIP_data_independant$NATRsum1_transformed_4SD)]<-(VIP_data_independant$NATRsum1_ofTEI_transformed[!is.na(VIP_data_independant$NATRsum1_transformed_4SD)]-
			mean(VIP_data_independant$NATRsum1_ofTEI_transformed[!is.na(VIP_data_independant$NATRsum1_transformed_4SD)]))/sd(VIP_data_independant$NATRsum1_ofTEI_transformed[!is.na(VIP_data_independant$NATRsum1_transformed_4SD)])

# categorizing based on guidelines
VIP_data_independant$NATRsum1_categorized_g[!is.na(NATRsum1)]<-2
VIP_data_independant$NATRsum1_categorized_g[!is.na(NATRsum1) & NATRsum1/1000 <= 2.4 ]<-1

detach(VIP_data_independant)
attach(VIP_data_independant)
