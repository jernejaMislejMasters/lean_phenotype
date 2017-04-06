library(ppcor)
library(Hmisc)

#load entire cleaned data (154009 subjects)
VIP_data_all <- read.csv("VIP_data/VIP_170206_cleaned.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)

#load the subset
VIP_data_subset <- read.csv("VIP_data/VIP_170206_cleaned_subset.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)
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

# run separate regression to get the association effect sizes for bmi, check they have the same direction and use the ones from the "independant" dataset to multiply the scores
# for the subset

#------------------------------INDEPENDANT DATA--------------------------------------------------------------------

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
VIP_data_independant$bmi_3SD[!is.na(VIP_data_independant$bmi_3SD)]<-(VIP_data_independant$bmi_3SD[!is.na(VIP_data_independant$bmi_3SD)] - mean(VIP_data_independant$bmi_3SD[!is.na(VIP_data_independant$bmi_3SD)]))/(sd(VIP_data_independant$bmi_3SD[!is.na(VIP_data_independant$bmi_3SD)]))

png("Results/distributions/filtered_3SD_bmi_distribution.png")
qqnorm(VIP_data_independant$bmi_3SD)
qqline(VIP_data_independant$bmi_3SD)
dev.off()
png("Results/distributions/filtered_3SD_bmi_histogram.png")
hist(VIP_data_independant$bmi_3SD)
dev.off()


#take 4SD for cutpoint
VIP_data_independant$bmi_4SD<-bmi
length(VIP_data_independant$bmi_4SD[!is.na(bmi) & (VIP_data_independant$bmi_sd > 4 | VIP_data_independant$bmi_sd < -4)])#38
VIP_data_independant$bmi_4SD[!is.na(bmi) & (VIP_data_independant$bmi_sd > 4 | VIP_data_independant$bmi_sd < -4)]<-NA
VIP_data_independant$bmi_4SD<-log(VIP_data_independant$bmi_4SD)
VIP_data_independant$bmi_4SD[!is.na(VIP_data_independant$bmi_4SD)]<-(VIP_data_independant$bmi_4SD[!is.na(VIP_data_independant$bmi_4SD)] - mean(VIP_data_independant$bmi_4SD[!is.na(VIP_data_independant$bmi_4SD)]))/(sd(VIP_data_independant$bmi_4SD[!is.na(VIP_data_independant$bmi_4SD)]))

png("Results/distributions/filtered_4SD_bmi_distribution.png")
qqnorm(VIP_data_independant$bmi_4SD)
qqline(VIP_data_independant$bmi_4SD)
dev.off()
png("Results/distributions/filtered_4SD_bmi_histogram.png")
hist(VIP_data_independant$bmi_4SD)
dev.off()


#take 35SD for cutpoint
VIP_data_independant$bmi_35SD<-bmi
length(VIP_data_independant$bmi_35SD[!is.na(bmi) & (VIP_data_independant$bmi_sd > 3.5 | VIP_data_independant$bmi_sd < -3.5)])#116
VIP_data_independant$bmi_35SD[!is.na(bmi) & (VIP_data_independant$bmi_sd > 3.5 | VIP_data_independant$bmi_sd < -3.5)]<-NA
VIP_data_independant$bmi_35SD<-log(VIP_data_independant$bmi_35SD)
VIP_data_independant$bmi_35SD[!is.na(VIP_data_independant$bmi_35SD)]<-(VIP_data_independant$bmi_35SD[!is.na(VIP_data_independant$bmi_35SD)] - mean(VIP_data_independant$bmi_35SD[!is.na(VIP_data_independant$bmi_35SD)]))/(sd(VIP_data_independant$bmi_35SD[!is.na(VIP_data_independant$bmi_35SD)]))

png("Results/distributions/filtered_35SD_bmi_distribution.png")
qqnorm(VIP_data_independant$bmi_35SD)
qqline(VIP_data_independant$bmi_35SD)
dev.off()
png("Results/distributions/filtered_35SD_bmi_histogram.png")
hist(VIP_data_independant$bmi_35SD)
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
VIP_data_independant$POLYsum1_ofTEI_transformed[!is.na(POLYsum1)]<-(VIP_data_independant$POLYsum1_ofTEI_transformed[!is.na(POLYsum1)]-
			mean(VIP_data_independant$POLYsum1_ofTEI_transformed[!is.na(POLYsum1)]))/sd(VIP_data_independant$POLYsum1_ofTEI_transformed[!is.na(POLYsum1)])

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
VIP_data_independant$MONOsum1_ofTEI_transformed[!is.na(MONOsum1)]<-(VIP_data_independant$MONOsum1_ofTEI_transformed[!is.na(MONOsum1)]-
			mean(VIP_data_independant$MONOsum1_ofTEI_transformed[!is.na(MONOsum1)]))/sd(VIP_data_independant$MONOsum1_ofTEI_transformed[!is.na(MONOsum1)])

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
VIP_data_independant$mfetsum1_ofTEI_transformed[!is.na(mfetsum1)]<-(VIP_data_independant$mfetsum1_ofTEI_transformed[!is.na(mfetsum1)]-
			mean(VIP_data_independant$mfetsum1_ofTEI_transformed[!is.na(mfetsum1)]))/sd(VIP_data_independant$mfetsum1_ofTEI_transformed[!is.na(mfetsum1)])

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
VIP_data_independant$fettsum1_ofTEI_transformed[!is.na(fettsum1)]<-(VIP_data_independant$fettsum1_ofTEI_transformed[!is.na(fettsum1)]-
			mean(VIP_data_independant$fettsum1_ofTEI_transformed[!is.na(fettsum1)]))/sd(VIP_data_independant$fettsum1_ofTEI_transformed[!is.na(fettsum1)])

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
VIP_data_independant$FA_ofTEI_transformed[!is.na(FA)]<-(VIP_data_independant$FA_ofTEI_transformed[!is.na(FA)]-
			mean(VIP_data_independant$FA_ofTEI_transformed[!is.na(FA)]))/sd(VIP_data_independant$FA_ofTEI_transformed[!is.na(FA)])

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
VIP_data_independant$kolhsum1_ofTEI_transformed[!is.na(kolhsum1)]<-(VIP_data_independant$kolhsum1_ofTEI_transformed[!is.na(kolhsum1)]-
			mean(VIP_data_independant$kolhsum1_ofTEI_transformed[!is.na(kolhsum1)]))/sd(VIP_data_independant$kolhsum1_ofTEI_transformed[!is.na(kolhsum1)])

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
VIP_data_independant$sacksum1_ofTEI_transformed[!is.na(sacksum1)]<-(VIP_data_independant$sacksum1_ofTEI_transformed[!is.na(sacksum1)]-
			mean(VIP_data_independant$sacksum1_ofTEI_transformed[!is.na(sacksum1)]))/sd(VIP_data_independant$sacksum1_ofTEI_transformed[!is.na(sacksum1)])

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
VIP_data_independant$protsum1_ofTEI_transformed[!is.na(protsum1)]<-(VIP_data_independant$protsum1_ofTEI_transformed[!is.na(protsum1)]-
			mean(VIP_data_independant$protsum1_ofTEI_transformed[!is.na(protsum1)]))/sd(VIP_data_independant$protsum1_ofTEI_transformed[!is.na(protsum1)])

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
VIP_data_independant$fibesum1_ofTEI_transformed[!is.na(fibesum1)]<-(VIP_data_independant$fibesum1_ofTEI_transformed[!is.na(fibesum1)]-
			mean(VIP_data_independant$fibesum1_ofTEI_transformed[!is.na(fibesum1)]))/sd(VIP_data_independant$fibesum1_ofTEI_transformed[!is.na(fibesum1)])

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
VIP_data_independant$NATRsum1_ofTEI_transformed[!is.na(NATRsum1)]<-(VIP_data_independant$NATRsum1_ofTEI_transformed[!is.na(NATRsum1)]-
			mean(VIP_data_independant$NATRsum1_ofTEI_transformed[!is.na(NATRsum1)]))/sd(VIP_data_independant$NATRsum1_ofTEI_transformed[!is.na(NATRsum1)])

# categorizing based on guidelines
VIP_data_independant$NATRsum1_categorized_g[!is.na(NATRsum1)]<-2
VIP_data_independant$NATRsum1_categorized_g[!is.na(NATRsum1) & NATRsum1/1000 <= 2.4 ]<-1

detach(VIP_data_independant)
attach(VIP_data_independant)

#run several different regressions 

# first check all the paired correlations
rcorr( cbind(bmi_norm,POLYsum1_transformed,MONOsum1_transformed,mfetsum1_transformed,fettsum1_transformed,sacksum1_transformed,kolhsum1_transformed,FA_transformed,protsum1_transformed,fibesum1_transformed,NATRsum1_transformed),type="pearson")

# check all the paired correlations for filtered bmi3
rcorr( cbind(bmi_3SD,POLYsum1_transformed,MONOsum1_transformed,mfetsum1_transformed,fettsum1_transformed,sacksum1_transformed,kolhsum1_transformed,FA_transformed,protsum1_transformed,fibesum1_transformed,NATRsum1_transformed),type="pearson")

# check all the paired correlations for filtered bmi4
rcorr( cbind(bmi_4SD,POLYsum1_transformed,MONOsum1_transformed,mfetsum1_transformed,fettsum1_transformed,sacksum1_transformed,kolhsum1_transformed,FA_transformed,protsum1_transformed,fibesum1_transformed,NATRsum1_transformed),type="pearson")

#check all the paired correlations for filtered bmi2
rcorr( cbind(bmi_35SD,POLYsum1_transformed,MONOsum1_transformed,mfetsum1_transformed,fettsum1_transformed,sacksum1_transformed,kolhsum1_transformed,FA_transformed,protsum1_transformed,fibesum1_transformed,NATRsum1_transformed),type="pearson")

# check all the paired correlations for filtered bmi4 and filtered nutrients
rcorr( cbind(bmi_4SD,POLYsum1_transformed_4SD,MONOsum1_transformed_4SD,mfetsum1_transformed_4SD,fettsum1_transformed_4SD,sacksum1_transformed_4SD,kolhsum1_transformed_4SD,FA_transformed_4SD,protsum1_transformed_4SD,fibesum1_transformed_4SD,NATRsum1_transformed_4SD),type="pearson")

# check all the paired correlations for bmi and filtered nutrients
rcorr( cbind(bmi_norm,POLYsum1_transformed_4SD,MONOsum1_transformed_4SD,mfetsum1_transformed_4SD,fettsum1_transformed_4SD,sacksum1_transformed_4SD,kolhsum1_transformed_4SD,FA_transformed_4SD,protsum1_transformed_4SD,fibesum1_transformed_4SD,NATRsum1_transformed_4SD),type="pearson")




#for raw continous, fitted together

#all bmi (NONE)
associations<-glm(bmi_sd_norm~age + agesq + gender_factor + year + ffq_factor + POLYsum1_transformed + MONOsum1_transformed + mfetsum1_transformed + fettsum1_transformed + sacksum1_transformed
				+ kolhsum1_transformed + FA_transformed + protsum1_transformed + fibesum1_transformed + NATRsum1_transformed, family = gaussian(link = "identity"))

summary(associations)

#filtered bmi with cutpoint 4SD and log transformed (ONLY BMI)
associations<-glm(bmi_4SD~age + agesq + gender_factor + year + ffq_factor + POLYsum1_transformed + MONOsum1_transformed + mfetsum1_transformed + fettsum1_transformed + sacksum1_transformed
				+ kolhsum1_transformed + FA_transformed + protsum1_transformed + fibesum1_transformed + NATRsum1_transformed, family = gaussian(link = "identity"))

summary(associations)

#filtered bmi and other variables with cutpoint 4SD and log transformed with other variables being standardized also (BMI AND NUTRIENTS)
associations<-glm(bmi_transformed_4SD~age + agesq + gender_factor + year + ffq_factor + POLYsum1_transformed_4SD + MONOsum1_transformed_4SD + mfetsum1_transformed_4SD + fettsum1_transformed_4SD + sacksum1_transformed_4SD
				+ kolhsum1_transformed_4SD + FA_transformed_4SD + protsum1_transformed_4SD + fibesum1_transformed_4SD + NATRsum1_transformed_4SD, family = gaussian(link = "identity"))

summary(associations)

#log bmi and filtered other variables with cutpoint 4SD and log transformed with other variables being standardized also (ONLY NUTRIENTS)
associations<-glm(bmi_norm~age + agesq + gender_factor + year + ffq_factor + POLYsum1_transformed_4SD + MONOsum1_transformed_4SD + mfetsum1_transformed_4SD + fettsum1_transformed_4SD + sacksum1_transformed_4SD
				+ kolhsum1_transformed_4SD + FA_transformed_4SD + protsum1_transformed_4SD + fibesum1_transformed_4SD + NATRsum1_transformed_4SD, family = gaussian(link = "identity"))

summary(associations)


all_variables<-c("POLYsum1_transformed","MONOsum1_transformed","mfetsum1_transformed","fettsum1_transformed","sacksum1_transformed",
		"kolhsum1_transformed","FA_transformed","protsum1_transformed","fibesum1_transformed","NATRsum1_transformed")

#r² (NONE)
variable_count<-1
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi_norm[(!is.na(bmi) & !is.na(VIP_data_independant[,c(variable)]))],VIP_data_independant[(!is.na(bmi) & !is.na(VIP_data_independant[,c(variable)])),c(variable)],
			VIP_data_independant[(!is.na(bmi) & !is.na(VIP_data_independant[,c(variable)])),c("age","agesq","year","ffq","gender",all_variables[-variable_count])])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
	variable_count<-variable_count+1
	
}

#r² filtered bmi with cutpoint 4SD and log transformed (ONLY BMI)
variable_count<-1
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi_4SD[(!is.na(bmi_4SD) & !is.na(VIP_data_independant[,c(variable)]))],VIP_data_independant[(!is.na(bmi_4SD) & !is.na(VIP_data_independant[,c(variable)])),c(variable)],
			VIP_data_independant[(!is.na(bmi_4SD) & !is.na(VIP_data_independant[,c(variable)])),c("age","agesq","year","ffq","gender",all_variables[-variable_count])])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
	variable_count<-variable_count+1
	
}

#r² filtered bmi and other variables with cutpoint 4SD and log transformed with other variables being standardized also (BMI AND NUTRIENTS)
all_variables<-c("POLYsum1_transformed_4SD","MONOsum1_transformed_4SD","mfetsum1_transformed_4SD","fettsum1_transformed_4SD","sacksum1_transformed_4SD",
		"kolhsum1_transformed_4SD","FA_transformed_4SD","protsum1_transformed_4SD","fibesum1_transformed_4SD","NATRsum1_transformed_4SD")
variable_count<-1
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi_4SD[(!is.na(bmi_4SD) & !is.na(VIP_data_independant[,c(variable)]))],VIP_data_independant[(!is.na(bmi_4SD) & !is.na(VIP_data_independant[,c(variable)])),c(variable)],
			VIP_data_independant[(!is.na(bmi_4SD) & !is.na(VIP_data_independant[,c(variable)])),c("age","agesq","year","ffq","gender",all_variables[-variable_count])])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
	variable_count<-variable_count+1
	
}


#for raw continous, fitted separately
for (variable in all_variables){
	
	associations<-glm(bmi_4SD_test~age + agesq + gender_factor + year + ffq_factor + VIP_data_independant[,c(variable)], family = gaussian(link = "identity"))
	associations_summary<-summary(associations)
	
	message(variable)
	message(round(associations_summary$coefficients[[7]],7))
	message(round(associations_summary$coefficients[[28]],7))
	
}

#r²
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi[(!is.na(bmi) & !is.na(VIP_data_independant[,c(variable)]))],VIP_data_independant[(!is.na(bmi) & !is.na(VIP_data_independant[,c(variable)])),c(variable)],
			VIP_data_independant[(!is.na(bmi) & !is.na(VIP_data_independant[,c(variable)])),c("age","agesq","year","ffq","gender")])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
}





# for continuous expressed in % of TEI, fitted together
associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + POLYsum1_ofTEI_transformed + MONOsum1_ofTEI_transformed + mfetsum1_ofTEI_transformed + fettsum1_ofTEI_transformed + sacksum1_ofTEI_transformed
				+ kolhsum1_ofTEI_transformed + FA_ofTEI_transformed + protsum1_ofTEI_transformed + fibesum1_ofTEI_transformed + NATRsum1_ofTEI_transformed, family = gaussian(link = "identity"))

summary(associations)

all_variables<-c("POLYsum1_ofTEI_transformed","MONOsum1_ofTEI_transformed","mfetsum1_ofTEI_transformed","fettsum1_ofTEI_transformed","sacksum1_ofTEI_transformed",
		"kolhsum1_ofTEI_transformed","FA_ofTEI_transformed","protsum1_ofTEI_transformed","fibesum1_ofTEI_transformed","NATRsum1_ofTEI_transformed")

#r²
variable_count<-1
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi[(!is.na(bmi) & !is.na(VIP_data_independant[,c(variable)]))],VIP_data_independant[(!is.na(bmi) & !is.na(VIP_data_independant[,c(variable)])),c(variable)],
			VIP_data_independant[(!is.na(bmi) & !is.na(VIP_data_independant[,c(variable)])),c("age","agesq","year","ffq","gender",all_variables[-variable_count])])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
	variable_count<-variable_count+1
	
}

# for continuous expressed in % of TEI, fitted separately
for (variable in all_variables){
	
	associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + VIP_data_independant[,c(variable)], family = gaussian(link = "identity"))
	associations_summary<-summary(associations)
	
	message(variable)
	message(round(associations_summary$coefficients[[7]],7))
	message(round(associations_summary$coefficients[[28]],7))
	
}

#r²
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi[(!is.na(bmi) & !is.na(VIP_data_independant[,c(variable)]))],VIP_data_independant[(!is.na(bmi) & !is.na(VIP_data_independant[,c(variable)])),c(variable)],
			VIP_data_independant[(!is.na(bmi) & !is.na(VIP_data_independant[,c(variable)])),c("age","agesq","year","ffq","gender")])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
}




# categorized based on guidelines, fitted together
associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + POLYsum1_ofTEI_categorized_g + MONOsum1_ofTEI_categorized_g + mfetsum1_ofTEI_categorized_g + fettsum1_ofTEI_categorized_g + sacksum1_ofTEI_categorized_g
				+ kolhsum1_ofTEI_categorized_g + FA_ofTEI_categorized_g + protsum1_ofTEI_categorized_g + fibesum1_categorized_g + NATRsum1_categorized_g, family = gaussian(link = "identity"))

summary(associations)

all_variables<-c("POLYsum1_ofTEI_categorized_g","MONOsum1_ofTEI_categorized_g","mfetsum1_ofTEI_categorized_g","fettsum1_ofTEI_categorized_g","sacksum1_ofTEI_categorized_g",
		"kolhsum1_ofTEI_categorized_g","FA_ofTEI_categorized_g","protsum1_ofTEI_categorized_g","fibesum1_categorized_g","NATRsum1_categorized_g")

#r²
variable_count<-1
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi[(!is.na(bmi) & !is.na(VIP_data_independant[,c(variable)]))],VIP_data_independant[(!is.na(bmi) & !is.na(VIP_data_independant[,c(variable)])),c(variable)],
			VIP_data_independant[(!is.na(bmi) & !is.na(VIP_data_independant[,c(variable)])),c("age","agesq","year","ffq","gender",all_variables[-variable_count])])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
	variable_count<-variable_count+1
	
}

# categorized based on guidelines, fitted separately
for (variable in all_variables){
	
	associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + VIP_data_independant[,c(variable)], family = gaussian(link = "identity"))
	associations_summary<-summary(associations)
	
	message(variable)
	message(round(associations_summary$coefficients[[7]],7))
	message(round(associations_summary$coefficients[[28]],7))
	
}

#r²
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi[(!is.na(bmi) & !is.na(VIP_data_independant[,c(variable)]))],VIP_data_independant[(!is.na(bmi) & !is.na(VIP_data_independant[,c(variable)])),c(variable)],
			VIP_data_independant[(!is.na(bmi) & !is.na(VIP_data_independant[,c(variable)])),c("age","agesq","year","ffq","gender")])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
}




# categorized the % of TEI based on location in standardized normal distribution, fitted together
associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + POLYsum1_ofTEI_categorized_n + MONOsum1_ofTEI_categorized_n + mfetsum1_ofTEI_categorized_n + fettsum1_ofTEI_categorized_n + sacksum1_ofTEI_categorized_n
				+ kolhsum1_ofTEI_categorized_n + FA_ofTEI_categorized_n + protsum1_ofTEI_categorized_n + fibesum1_categorized_n + NATRsum1_categorized_n, family = gaussian(link = "identity"))

summary(associations)

all_variables<-c("POLYsum1_ofTEI_categorized_n","MONOsum1_ofTEI_categorized_n","mfetsum1_ofTEI_categorized_n","fettsum1_ofTEI_categorized_n","sacksum1_ofTEI_categorized_n",
		"kolhsum1_ofTEI_categorized_n","FA_ofTEI_categorized_n","protsum1_ofTEI_categorized_n","fibesum1_categorized_n","NATRsum1_categorized_n")

#r²
variable_count<-1
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi[(!is.na(bmi) & !is.na(VIP_data_independant[,c(variable)]))],VIP_data_independant[(!is.na(bmi) & !is.na(VIP_data_independant[,c(variable)])),c(variable)],
			VIP_data_independant[(!is.na(bmi) & !is.na(VIP_data_independant[,c(variable)])),c("age","agesq","year","ffq","gender",all_variables[-variable_count])])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
	variable_count<-variable_count+1
	
}

# categorized the % of TEI based on location in standardized normal distribution, fitted separately
for (variable in all_variables){
	
	associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + VIP_data_independant[,c(variable)], family = gaussian(link = "identity"))
	associations_summary<-summary(associations)
	
	message(variable)
	message(round(associations_summary$coefficients[[7]],7))
	message(round(associations_summary$coefficients[[28]],7))
	
}

#r²
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi[(!is.na(bmi) & !is.na(VIP_data_independant[,c(variable)]))],VIP_data_independant[(!is.na(bmi) & !is.na(VIP_data_independant[,c(variable)])),c(variable)],
			VIP_data_independant[(!is.na(bmi) & !is.na(VIP_data_independant[,c(variable)])),c("age","agesq","year","ffq","gender")])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
}



# categorized the raw based on location in standardized normal distribution, fitted together
associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + POLYsum1_categorized_n + MONOsum1_categorized_n + fettsum1_categorized_n + mfetsum1_categorized_n + sacksum1_categorized_n
				+ kolhsum1_categorized_n + FA_categorized_n + protsum1_categorized_n + fibesum1_categorized_n + NATRsum1_categorized_n, family = gaussian(link = "identity"))

summary(associations)

all_variables<-c("POLYsum1_categorized_n","MONOsum1_categorized_n","mfetsum1_categorized_n","fettsum1_categorized_n","sacksum1_categorized_n",
		"kolhsum1_categorized_n","FA_categorized_n","protsum1_categorized_n","fibesum1_categorized_n","NATRsum1_categorized_n")

#r²
variable_count<-1
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi[(!is.na(bmi) & !is.na(VIP_data_independant[,c(variable)]))],VIP_data_independant[(!is.na(bmi) & !is.na(VIP_data_independant[,c(variable)])),c(variable)],
			VIP_data_independant[(!is.na(bmi) & !is.na(VIP_data_independant[,c(variable)])),c("age","agesq","year","ffq","gender",all_variables[-variable_count])])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
	variable_count<-variable_count+1
	
}

# categorized the raw based on location in standardized normal distribution, fitted separately
for (variable in all_variables){
	
	associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + VIP_data_independant[,c(variable)], family = gaussian(link = "identity"))
	associations_summary<-summary(associations)
	
	message(variable)
	message(round(associations_summary$coefficients[[7]],7))
	message(round(associations_summary$coefficients[[28]],7))
	
}

#r²
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi[(!is.na(bmi) & !is.na(VIP_data_independant[,c(variable)]))],VIP_data_independant[(!is.na(bmi) & !is.na(VIP_data_independant[,c(variable)])),c(variable)],
			VIP_data_independant[(!is.na(bmi) & !is.na(VIP_data_independant[,c(variable)])),c("age","agesq","year","ffq","gender")])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
}


#------------------------------INDEPENDANT DATA--------------------------------------------------------------------


#------------------------------SUBSET DATA-------------------------------------------------------------------------

#do the same for the subset

detach(VIP_data_independant)
attach(VIP_data_subset)

#create factor for the questionare
VIP_data_subset$ffq<-0
VIP_data_subset$ffq[enkver2=="long"]<-1
VIP_data_subset$ffq[enkver2=="apri"]<-1
VIP_data_subset$ffq_factor<-as.factor(VIP_data_subset$ffq)

#age squared
VIP_data_subset$agesq<-age*age

#factorize gender
VIP_data_subset$gender_factor<-as.factor(VIP_data_subset$gender)

#create several new "transformed" variables, either log transformed and standaridized or categorized based on the recommendations or based on the location in the standard normal distribution

# POLYsum1 is raw, POLYsum1_transformed is raw log tranformed and standardized, POLYsum1_ofTEI is expressed in % of total energy intake, POLYsum1_ofTEI_transformed is log transformed and 
# standardized expressed in % of TEI, POLYsum1_ofTEI_categorized_g is expressed in % of TEI and categorized based on guidelines, POLYsum1_ofTEI_categorized_n is expressed in % of 
# TEI and categorized based on location in the standard normal distribution, POLYsum1_categorized_n is raw categorized based on location in the standard normal distribution

#continuous log transformed and standardized
VIP_data_subset$POLYsum1_transformed<-log(POLYsum1)
VIP_data_subset$POLYsum1_transformed[!is.na(POLYsum1) & visit==1]<-(VIP_data_subset$POLYsum1_transformed[!is.na(POLYsum1) & visit==1]-
			mean(VIP_data_subset$POLYsum1_transformed[!is.na(POLYsum1) & visit==1]))/sd(VIP_data_subset$POLYsum1_transformed[!is.na(POLYsum1) & visit==1])
VIP_data_subset$POLYsum1_transformed[!is.na(POLYsum1) & visit==2]<-(VIP_data_subset$POLYsum1_transformed[!is.na(POLYsum1) & visit==2]-
			mean(VIP_data_subset$POLYsum1_transformed[!is.na(POLYsum1) & visit==2]))/sd(VIP_data_subset$POLYsum1_transformed[!is.na(POLYsum1) & visit==2])

# % of TEI
VIP_data_subset$POLYsum1_ofTEI<-(100*9*POLYsum1)/ensum1

# % of TEI log transformed and standardized
VIP_data_subset$POLYsum1_ofTEI_transformed<-log(VIP_data_subset$POLYsum1_ofTEI)		
VIP_data_subset$POLYsum1_ofTEI_transformed[!is.na(POLYsum1) & visit==1]<-(VIP_data_subset$POLYsum1_ofTEI_transformed[!is.na(POLYsum1) & visit==1]-
			mean(VIP_data_subset$POLYsum1_ofTEI_transformed[!is.na(POLYsum1) & visit==1]))/sd(VIP_data_subset$POLYsum1_ofTEI_transformed[!is.na(POLYsum1) & visit==1])
VIP_data_subset$POLYsum1_ofTEI_transformed[!is.na(POLYsum1) & visit==2]<-(VIP_data_subset$POLYsum1_ofTEI_transformed[!is.na(POLYsum1) & visit==2]-
			mean(VIP_data_subset$POLYsum1_ofTEI_transformed[!is.na(POLYsum1) & visit==2]))/sd(VIP_data_subset$POLYsum1_ofTEI_transformed[!is.na(POLYsum1) & visit==2])

# categorizing the % of TEI based on guidelines
VIP_data_subset$POLYsum1_ofTEI_categorized_g[!is.na(POLYsum1)]<-0
VIP_data_subset$POLYsum1_ofTEI_categorized_g[!is.na(POLYsum1) & VIP_data_subset$POLYsum1_ofTEI >= 5 & VIP_data_subset$POLYsum1_ofTEI <= 10 ]<-1
VIP_data_subset$POLYsum1_ofTEI_categorized_g[!is.na(POLYsum1) & VIP_data_subset$POLYsum1_ofTEI > 10]<-2

# categorizing the % of TEI based on location in standard normal distribution
VIP_data_subset$POLYsum1_ofTEI_categorized_n[!is.na(POLYsum1)] <-0
VIP_data_subset$POLYsum1_ofTEI_categorized_n[!is.na(POLYsum1) & VIP_data_subset$POLYsum1_ofTEI_transformed >= -1 & VIP_data_subset$POLYsum1_ofTEI_transformed < 0] <- 1
VIP_data_subset$POLYsum1_ofTEI_categorized_n[!is.na(POLYsum1) & VIP_data_subset$POLYsum1_ofTEI_transformed >= 0 & VIP_data_subset$POLYsum1_ofTEI_transformed < 1] <- 2
VIP_data_subset$POLYsum1_ofTEI_categorized_n[!is.na(POLYsum1) & VIP_data_subset$POLYsum1_ofTEI_transformed >= 1 & VIP_data_subset$POLYsum1_ofTEI_transformed < 2] <- 3
VIP_data_subset$POLYsum1_ofTEI_categorized_n[!is.na(POLYsum1) & VIP_data_subset$POLYsum1_ofTEI_transformed >= 2] <- 4

# categorizing raw based on location in standard normal distribution
VIP_data_subset$POLYsum1_categorized_n[!is.na(POLYsum1)] <-0
VIP_data_subset$POLYsum1_categorized_n[!is.na(POLYsum1) & VIP_data_subset$POLYsum1_transformed >= -1 & VIP_data_subset$POLYsum1_transformed < 0] <- 1
VIP_data_subset$POLYsum1_categorized_n[!is.na(POLYsum1) & VIP_data_subset$POLYsum1_transformed >= 0 & VIP_data_subset$POLYsum1_transformed < 1] <- 2
VIP_data_subset$POLYsum1_categorized_n[!is.na(POLYsum1) & VIP_data_subset$POLYsum1_transformed >= 1 & VIP_data_subset$POLYsum1_transformed < 2] <- 3
VIP_data_subset$POLYsum1_categorized_n[!is.na(POLYsum1) & VIP_data_subset$POLYsum1_transformed >= 2] <- 4




# MONOsum1 is raw, MONOsum1_transformed is raw log tranformed and standardized, MONOsum1_ofTEI is expressed in % of total energy intake, MONOsum1_ofTEI_transformed is log transformed and 
# standardized expressed in % of TEI, MONOsum1_ofTEI_categorized_g is expressed in % of TEI and categorized based on guidelines, MONOsum1_ofTEI_categorized_n is expressed in % of 
# TEI and categorized based on location in the standard normal distribution, MONOsum1_categorized_n is raw categorized based on location in the standard normal distribution

#continuous log transformed and standardized
VIP_data_subset$MONOsum1_transformed<-log(MONOsum1)
VIP_data_subset$MONOsum1_transformed[!is.na(MONOsum1) & visit==1]<-(VIP_data_subset$MONOsum1_transformed[!is.na(MONOsum1) & visit==1]-
			mean(VIP_data_subset$MONOsum1_transformed[!is.na(MONOsum1) & visit==1]))/sd(VIP_data_subset$MONOsum1_transformed[!is.na(MONOsum1) & visit==1])
VIP_data_subset$MONOsum1_transformed[!is.na(MONOsum1) & visit==2]<-(VIP_data_subset$MONOsum1_transformed[!is.na(MONOsum1) & visit==2]-
			mean(VIP_data_subset$MONOsum1_transformed[!is.na(MONOsum1) & visit==2]))/sd(VIP_data_subset$MONOsum1_transformed[!is.na(MONOsum1) & visit==2])

# % of TEI
VIP_data_subset$MONOsum1_ofTEI<-(100*9*MONOsum1)/ensum1

# % of TEI log transformed and standardized
VIP_data_subset$MONOsum1_ofTEI_transformed<-log(VIP_data_subset$MONOsum1_ofTEI)		
VIP_data_subset$MONOsum1_ofTEI_transformed[!is.na(MONOsum1) & visit==1]<-(VIP_data_subset$MONOsum1_ofTEI_transformed[!is.na(MONOsum1) & visit==1]-
			mean(VIP_data_subset$MONOsum1_ofTEI_transformed[!is.na(MONOsum1) & visit==1]))/sd(VIP_data_subset$MONOsum1_ofTEI_transformed[!is.na(MONOsum1) & visit==1])
VIP_data_subset$MONOsum1_ofTEI_transformed[!is.na(MONOsum1) & visit==2]<-(VIP_data_subset$MONOsum1_ofTEI_transformed[!is.na(MONOsum1) & visit==2]-
			mean(VIP_data_subset$MONOsum1_ofTEI_transformed[!is.na(MONOsum1) & visit==2]))/sd(VIP_data_subset$MONOsum1_ofTEI_transformed[!is.na(MONOsum1) & visit==2])

# categorizing the % of TEI based on guidelines
VIP_data_subset$MONOsum1_ofTEI_categorized_g[!is.na(MONOsum1)]<-0
VIP_data_subset$MONOsum1_ofTEI_categorized_g[!is.na(MONOsum1) & VIP_data_subset$MONOsum1_ofTEI >= 10 & VIP_data_subset$MONOsum1_ofTEI <= 20 ]<-1
VIP_data_subset$MONOsum1_ofTEI_categorized_g[!is.na(MONOsum1) & VIP_data_subset$MONOsum1_ofTEI > 20]<-2

# categorizing the % of TEI based on location in standard normal distribution
VIP_data_subset$MONOsum1_ofTEI_categorized_n[!is.na(MONOsum1)] <-0
VIP_data_subset$MONOsum1_ofTEI_categorized_n[!is.na(MONOsum1) & VIP_data_subset$MONOsum1_ofTEI_transformed >= -1 & VIP_data_subset$MONOsum1_ofTEI_transformed < 0] <- 1
VIP_data_subset$MONOsum1_ofTEI_categorized_n[!is.na(MONOsum1) & VIP_data_subset$MONOsum1_ofTEI_transformed >= 0 & VIP_data_subset$MONOsum1_ofTEI_transformed < 1] <- 2
VIP_data_subset$MONOsum1_ofTEI_categorized_n[!is.na(MONOsum1) & VIP_data_subset$MONOsum1_ofTEI_transformed >= 1 & VIP_data_subset$MONOsum1_ofTEI_transformed < 2] <- 3
VIP_data_subset$MONOsum1_ofTEI_categorized_n[!is.na(MONOsum1) & VIP_data_subset$MONOsum1_ofTEI_transformed >= 2] <- 4

# categorizing raw based on location in standard normal distribution
VIP_data_subset$MONOsum1_categorized_n[!is.na(MONOsum1)] <-0
VIP_data_subset$MONOsum1_categorized_n[!is.na(MONOsum1) & VIP_data_subset$MONOsum1_transformed >= -1 & VIP_data_subset$MONOsum1_transformed < 0] <- 1
VIP_data_subset$MONOsum1_categorized_n[!is.na(MONOsum1) & VIP_data_subset$MONOsum1_transformed >= 0 & VIP_data_subset$MONOsum1_transformed < 1] <- 2
VIP_data_subset$MONOsum1_categorized_n[!is.na(MONOsum1) & VIP_data_subset$MONOsum1_transformed >= 1 & VIP_data_subset$MONOsum1_transformed < 2] <- 3
VIP_data_subset$MONOsum1_categorized_n[!is.na(MONOsum1) & VIP_data_subset$MONOsum1_transformed >= 2] <- 4




# mfetsum1 is raw, mfetsum1_transformed is raw log tranformed and standardized, mfetsum1_ofTEI is expressed in % of total energy intake, mfetsum1_ofTEI_transformed is log transformed and 
# standardized expressed in % of TEI, mfetsum1_ofTEI_categorized_g is expressed in % of TEI and categorized based on guidelines, mfetsum1_ofTEI_categorized_n is expressed in % of 
# TEI and categorized based on location in the standard normal distribution, mfetsum1_categorized_n is raw categorized based on location in the standard normal distribution

#continuous log transformed and standardized
VIP_data_subset$mfetsum1_transformed<-log(mfetsum1)
VIP_data_subset$mfetsum1_transformed[!is.na(mfetsum1) & visit==1]<-(VIP_data_subset$mfetsum1_transformed[!is.na(mfetsum1) & visit==1]-
			mean(VIP_data_subset$mfetsum1_transformed[!is.na(mfetsum1) & visit==1]))/sd(VIP_data_subset$mfetsum1_transformed[!is.na(mfetsum1) & visit==1])
VIP_data_subset$mfetsum1_transformed[!is.na(mfetsum1) & visit==2]<-(VIP_data_subset$mfetsum1_transformed[!is.na(mfetsum1) & visit==2]-
			mean(VIP_data_subset$mfetsum1_transformed[!is.na(mfetsum1) & visit==2]))/sd(VIP_data_subset$mfetsum1_transformed[!is.na(mfetsum1) & visit==2])

# % of TEI
VIP_data_subset$mfetsum1_ofTEI<-(100*9*mfetsum1)/ensum1

# % of TEI log transformed and standardized
VIP_data_subset$mfetsum1_ofTEI_transformed<-log(VIP_data_subset$mfetsum1_ofTEI)		
VIP_data_subset$mfetsum1_ofTEI_transformed[!is.na(mfetsum1) & visit==1]<-(VIP_data_subset$mfetsum1_ofTEI_transformed[!is.na(mfetsum1) & visit==1]-
			mean(VIP_data_subset$mfetsum1_ofTEI_transformed[!is.na(mfetsum1) & visit==1]))/sd(VIP_data_subset$mfetsum1_ofTEI_transformed[!is.na(mfetsum1) & visit==1])
VIP_data_subset$mfetsum1_ofTEI_transformed[!is.na(mfetsum1) & visit==2]<-(VIP_data_subset$mfetsum1_ofTEI_transformed[!is.na(mfetsum1) & visit==2]-
			mean(VIP_data_subset$mfetsum1_ofTEI_transformed[!is.na(mfetsum1) & visit==2]))/sd(VIP_data_subset$mfetsum1_ofTEI_transformed[!is.na(mfetsum1) & visit==2])

# categorizing the % of TEI based on guidelines
VIP_data_subset$mfetsum1_ofTEI_categorized_g[!is.na(mfetsum1)]<-2
VIP_data_subset$mfetsum1_ofTEI_categorized_g[!is.na(mfetsum1) & VIP_data_subset$mfetsum1_ofTEI <= 10 ]<-1

# categorizing the % of TEI based on location in standard normal distribution
VIP_data_subset$mfetsum1_ofTEI_categorized_n[!is.na(mfetsum1)] <-0
VIP_data_subset$mfetsum1_ofTEI_categorized_n[!is.na(mfetsum1) & VIP_data_subset$mfetsum1_ofTEI_transformed >= -1 & VIP_data_subset$mfetsum1_ofTEI_transformed < 0] <- 1
VIP_data_subset$mfetsum1_ofTEI_categorized_n[!is.na(mfetsum1) & VIP_data_subset$mfetsum1_ofTEI_transformed >= 0 & VIP_data_subset$mfetsum1_ofTEI_transformed < 1] <- 2
VIP_data_subset$mfetsum1_ofTEI_categorized_n[!is.na(mfetsum1) & VIP_data_subset$mfetsum1_ofTEI_transformed >= 1 & VIP_data_subset$mfetsum1_ofTEI_transformed < 2] <- 3
VIP_data_subset$mfetsum1_ofTEI_categorized_n[!is.na(mfetsum1) & VIP_data_subset$mfetsum1_ofTEI_transformed >= 2] <- 4

# categorizing raw based on location in standard normal distribution
VIP_data_subset$mfetsum1_categorized_n[!is.na(mfetsum1)] <-0
VIP_data_subset$mfetsum1_categorized_n[!is.na(mfetsum1) & VIP_data_subset$mfetsum1_transformed >= -1 & VIP_data_subset$mfetsum1_transformed < 0] <- 1
VIP_data_subset$mfetsum1_categorized_n[!is.na(mfetsum1) & VIP_data_subset$mfetsum1_transformed >= 0 & VIP_data_subset$mfetsum1_transformed < 1] <- 2
VIP_data_subset$mfetsum1_categorized_n[!is.na(mfetsum1) & VIP_data_subset$mfetsum1_transformed >= 1 & VIP_data_subset$mfetsum1_transformed < 2] <- 3
VIP_data_subset$mfetsum1_categorized_n[!is.na(mfetsum1) & VIP_data_subset$mfetsum1_transformed >= 2] <- 4



# fettsum1 is raw, fettsum1_transformed is raw log tranformed and standardized, fettsum1_ofTEI is expressed in % of total energy intake, fettsum1_ofTEI_transformed is log transformed and 
# standardized expressed in % of TEI, fettsum1_ofTEI_categorized_g is expressed in % of TEI and categorized based on guidelines, fettsum1_ofTEI_categorized_n is expressed in % of 
# TEI and categorized based on location in the standard normal distribution, fettsum1_categorized_n is raw categorized based on location in the standard normal distribution

#continuous log transformed and standardized
VIP_data_subset$fettsum1_transformed<-log(fettsum1)
VIP_data_subset$fettsum1_transformed[!is.na(fettsum1) & visit==1]<-(VIP_data_subset$fettsum1_transformed[!is.na(fettsum1) & visit==1]-
			mean(VIP_data_subset$fettsum1_transformed[!is.na(fettsum1) & visit==1]))/sd(VIP_data_subset$fettsum1_transformed[!is.na(fettsum1) & visit==1])
VIP_data_subset$fettsum1_transformed[!is.na(fettsum1) & visit==2]<-(VIP_data_subset$fettsum1_transformed[!is.na(fettsum1) & visit==2]-
			mean(VIP_data_subset$fettsum1_transformed[!is.na(fettsum1) & visit==2]))/sd(VIP_data_subset$fettsum1_transformed[!is.na(fettsum1) & visit==2])

# % of TEI
VIP_data_subset$fettsum1_ofTEI<-(100*9*fettsum1)/ensum1

# % of TEI log transformed and standardized
VIP_data_subset$fettsum1_ofTEI_transformed<-log(VIP_data_subset$fettsum1_ofTEI)		
VIP_data_subset$fettsum1_ofTEI_transformed[!is.na(fettsum1) & visit==1]<-(VIP_data_subset$fettsum1_ofTEI_transformed[!is.na(fettsum1) & visit==1]-
			mean(VIP_data_subset$fettsum1_ofTEI_transformed[!is.na(fettsum1) & visit==1]))/sd(VIP_data_subset$fettsum1_ofTEI_transformed[!is.na(fettsum1) & visit==1])
VIP_data_subset$fettsum1_ofTEI_transformed[!is.na(fettsum1) & visit==2]<-(VIP_data_subset$fettsum1_ofTEI_transformed[!is.na(fettsum1) & visit==2]-
			mean(VIP_data_subset$fettsum1_ofTEI_transformed[!is.na(fettsum1) & visit==2]))/sd(VIP_data_subset$fettsum1_ofTEI_transformed[!is.na(fettsum1) & visit==2])

# categorizing the % of TEI based on guidelines
VIP_data_subset$fettsum1_ofTEI_categorized_g[!is.na(fettsum1)]<-0
VIP_data_subset$fettsum1_ofTEI_categorized_g[!is.na(fettsum1) & VIP_data_subset$fettsum1_ofTEI >= 25 & VIP_data_subset$fettsum1_ofTEI <= 40 ]<-1
VIP_data_subset$fettsum1_ofTEI_categorized_g[!is.na(fettsum1) & VIP_data_subset$fettsum1_ofTEI > 40]<-2

## categorizing the % of TEI based on location in standard normal distribution
VIP_data_subset$fettsum1_ofTEI_categorized_n[!is.na(fettsum1)] <-0
VIP_data_subset$fettsum1_ofTEI_categorized_n[!is.na(fettsum1) & VIP_data_subset$fettsum1_ofTEI_transformed >= -1 & VIP_data_subset$fettsum1_ofTEI_transformed < 0] <- 1
VIP_data_subset$fettsum1_ofTEI_categorized_n[!is.na(fettsum1) & VIP_data_subset$fettsum1_ofTEI_transformed >= 0 & VIP_data_subset$fettsum1_ofTEI_transformed < 1] <- 2
VIP_data_subset$fettsum1_ofTEI_categorized_n[!is.na(fettsum1) & VIP_data_subset$fettsum1_ofTEI_transformed >= 1 & VIP_data_subset$fettsum1_ofTEI_transformed < 2] <- 3
VIP_data_subset$fettsum1_ofTEI_categorized_n[!is.na(fettsum1) & VIP_data_subset$fettsum1_ofTEI_transformed >= 2] <- 4

# categorizing raw based on location in standard normal distribution
VIP_data_subset$fettsum1_categorized_n[!is.na(fettsum1)] <-0
VIP_data_subset$fettsum1_categorized_n[!is.na(fettsum1) & VIP_data_subset$fettsum1_transformed >= -1 & VIP_data_subset$fettsum1_transformed < 0] <- 1
VIP_data_subset$fettsum1_categorized_n[!is.na(fettsum1) & VIP_data_subset$fettsum1_transformed >= 0 & VIP_data_subset$fettsum1_transformed < 1] <- 2
VIP_data_subset$fettsum1_categorized_n[!is.na(fettsum1) & VIP_data_subset$fettsum1_transformed >= 1 & VIP_data_subset$fettsum1_transformed < 2] <- 3
VIP_data_subset$fettsum1_categorized_n[!is.na(fettsum1) & VIP_data_subset$fettsum1_transformed >= 2] <- 4




# FA is raw, FA_transformed is raw log tranformed and standardized, FA_ofTEI is expressed in % of total energy intake, FA_ofTEI_transformed is log transformed and 
# standardized expressed in % of TEI, FA_ofTEI_categorized_g is expressed in % of TEI and categorized based on guidelines, FA_ofTEI_categorized_n is expressed in % of 
# TEI and categorized based on location in the standard normal distribution, FA_categorized_n is raw categorized based on location in the standard normal distribution

#acids are a combination of several variables
VIP_data_subset$FA<-FA183_sum1 + FA205_sum1 + FA226_sum1 + FA182_sum1 + FA204_sum1
detach(VIP_data_subset)
attach(VIP_data_subset)

#continuous log transformed and standardized
VIP_data_subset$FA_transformed<-log(FA)
VIP_data_subset$FA_transformed[!is.na(FA) & visit==1]<-(VIP_data_subset$FA_transformed[!is.na(FA) & visit==1]-
			mean(VIP_data_subset$FA_transformed[!is.na(FA) & visit==1]))/sd(VIP_data_subset$FA_transformed[!is.na(FA) & visit==1])
VIP_data_subset$FA_transformed[!is.na(FA) & visit==2]<-(VIP_data_subset$FA_transformed[!is.na(FA) & visit==2]-
			mean(VIP_data_subset$FA_transformed[!is.na(FA) & visit==2]))/sd(VIP_data_subset$FA_transformed[!is.na(FA) & visit==2])

# % of TEI
VIP_data_subset$FA_ofTEI<-(100*9*FA)/ensum1

# % of TEI log transformed and standardized
VIP_data_subset$FA_ofTEI_transformed<-log(VIP_data_subset$FA_ofTEI)		
VIP_data_subset$FA_ofTEI_transformed[!is.na(FA) & visit==1]<-(VIP_data_subset$FA_ofTEI_transformed[!is.na(FA) & visit==1]-
			mean(VIP_data_subset$FA_ofTEI_transformed[!is.na(FA) & visit==1]))/sd(VIP_data_subset$FA_ofTEI_transformed[!is.na(FA) & visit==1])
VIP_data_subset$FA_ofTEI_transformed[!is.na(FA) & visit==2]<-(VIP_data_subset$FA_ofTEI_transformed[!is.na(FA) & visit==2]-
			mean(VIP_data_subset$FA_ofTEI_transformed[!is.na(FA) & visit==2]))/sd(VIP_data_subset$FA_ofTEI_transformed[!is.na(FA) & visit==2])

# categorizing the % of TEI based on guidelines
VIP_data_subset$FA_ofTEI_categorized_g[!is.na(FA)]<-1
VIP_data_subset$FA_ofTEI_categorized_g[!is.na(FA) & VIP_data_subset$FA_ofTEI < 3 ]<-2

# categorizing the % of TEI based on location in standard normal distribution
VIP_data_subset$FA_ofTEI_categorized_n[!is.na(FA)] <-0
VIP_data_subset$FA_ofTEI_categorized_n[!is.na(FA) & VIP_data_subset$FA_ofTEI_transformed >= -1 & VIP_data_subset$FA_ofTEI_transformed < 0] <- 1
VIP_data_subset$FA_ofTEI_categorized_n[!is.na(FA) & VIP_data_subset$FA_ofTEI_transformed >= 0 & VIP_data_subset$FA_ofTEI_transformed < 1] <- 2
VIP_data_subset$FA_ofTEI_categorized_n[!is.na(FA) & VIP_data_subset$FA_ofTEI_transformed >= 1 & VIP_data_subset$FA_ofTEI_transformed < 2] <- 3
VIP_data_subset$FA_ofTEI_categorized_n[!is.na(FA) & VIP_data_subset$FA_ofTEI_transformed >= 2] <- 4

# categorizing raw based on location in standard normal distribution
VIP_data_subset$FA_categorized_n[!is.na(FA)] <-0
VIP_data_subset$FA_categorized_n[!is.na(FA) & VIP_data_subset$FA_transformed >= -1 & VIP_data_subset$FA_transformed < 0] <- 1
VIP_data_subset$FA_categorized_n[!is.na(FA) & VIP_data_subset$FA_transformed >= 0 & VIP_data_subset$FA_transformed < 1] <- 2
VIP_data_subset$FA_categorized_n[!is.na(FA) & VIP_data_subset$FA_transformed >= 1 & VIP_data_subset$FA_transformed < 2] <- 3
VIP_data_subset$FA_categorized_n[!is.na(FA) & VIP_data_subset$FA_transformed >= 2] <- 4




# kolhsum1 is raw, kolhsum1_transformed is raw log tranformed and standardized, kolhsum1_ofTEI is expressed in % of total energy intake, kolhsum1_ofTEI_transformed is log transformed and 
# standardized expressed in % of TEI, kolhsum1_ofTEI_categorized_g is expressed in % of TEI and categorized based on guidelines, kolhsum1_ofTEI_categorized_n is expressed in % of 
# TEI and categorized based on location in the standard normal distribution, kolhsum1_categorized_n is raw categorized based on location in the standard normal distribution

#continuous log transformed and standardized
VIP_data_subset$kolhsum1_transformed<-log(kolhsum1)
VIP_data_subset$kolhsum1_transformed[!is.na(kolhsum1) & visit==1]<-(VIP_data_subset$kolhsum1_transformed[!is.na(kolhsum1) & visit==1]-
			mean(VIP_data_subset$kolhsum1_transformed[!is.na(kolhsum1) & visit==1]))/sd(VIP_data_subset$kolhsum1_transformed[!is.na(kolhsum1) & visit==1])
VIP_data_subset$kolhsum1_transformed[!is.na(kolhsum1) & visit==2]<-(VIP_data_subset$kolhsum1_transformed[!is.na(kolhsum1) & visit==2]-
			mean(VIP_data_subset$kolhsum1_transformed[!is.na(kolhsum1) & visit==2]))/sd(VIP_data_subset$kolhsum1_transformed[!is.na(kolhsum1) & visit==2])

# % of TEI
VIP_data_subset$kolhsum1_ofTEI<-(100*4*kolhsum1)/ensum1

# % of TEI log transformed and standardized
VIP_data_subset$kolhsum1_ofTEI_transformed<-log(VIP_data_subset$kolhsum1_ofTEI)		
VIP_data_subset$kolhsum1_ofTEI_transformed[!is.na(kolhsum1) & visit==1]<-(VIP_data_subset$kolhsum1_ofTEI_transformed[!is.na(kolhsum1) & visit==1]-
			mean(VIP_data_subset$kolhsum1_ofTEI_transformed[!is.na(kolhsum1) & visit==1]))/sd(VIP_data_subset$kolhsum1_ofTEI_transformed[!is.na(kolhsum1) & visit==1])
VIP_data_subset$kolhsum1_ofTEI_transformed[!is.na(kolhsum1) & visit==2]<-(VIP_data_subset$kolhsum1_ofTEI_transformed[!is.na(kolhsum1) & visit==2]-
			mean(VIP_data_subset$kolhsum1_ofTEI_transformed[!is.na(kolhsum1) & visit==2]))/sd(VIP_data_subset$kolhsum1_ofTEI_transformed[!is.na(kolhsum1) & visit==2])

# categorizing the % of TEI based on guidelines
VIP_data_subset$kolhsum1_ofTEI_categorized_g[!is.na(kolhsum1)]<-0
VIP_data_subset$kolhsum1_ofTEI_categorized_g[!is.na(kolhsum1) & VIP_data_subset$kolhsum1_ofTEI >= 45 & VIP_data_subset$kolhsum1_ofTEI <= 60 ]<-1
VIP_data_subset$kolhsum1_ofTEI_categorized_g[!is.na(kolhsum1) & VIP_data_subset$kolhsum1_ofTEI > 60]<-2

# categorizing the % of TEI based on location in standard normal distribution
VIP_data_subset$kolhsum1_ofTEI_categorized_n[!is.na(kolhsum1)] <-0
VIP_data_subset$kolhsum1_ofTEI_categorized_n[!is.na(kolhsum1) & VIP_data_subset$kolhsum1_ofTEI_transformed >= -1 & VIP_data_subset$kolhsum1_ofTEI_transformed < 0] <- 1
VIP_data_subset$kolhsum1_ofTEI_categorized_n[!is.na(kolhsum1) & VIP_data_subset$kolhsum1_ofTEI_transformed >= 0 & VIP_data_subset$kolhsum1_ofTEI_transformed < 1] <- 2
VIP_data_subset$kolhsum1_ofTEI_categorized_n[!is.na(kolhsum1) & VIP_data_subset$kolhsum1_ofTEI_transformed >= 1 & VIP_data_subset$kolhsum1_ofTEI_transformed < 2] <- 3
VIP_data_subset$kolhsum1_ofTEI_categorized_n[!is.na(kolhsum1) & VIP_data_subset$kolhsum1_ofTEI_transformed >= 2] <- 4

# categorizing raw based on location in standard normal distribution
VIP_data_subset$kolhsum1_categorized_n[!is.na(kolhsum1)] <-0
VIP_data_subset$kolhsum1_categorized_n[!is.na(kolhsum1) & VIP_data_subset$kolhsum1_transformed >= -1 & VIP_data_subset$kolhsum1_transformed < 0] <- 1
VIP_data_subset$kolhsum1_categorized_n[!is.na(kolhsum1) & VIP_data_subset$kolhsum1_transformed >= 0 & VIP_data_subset$kolhsum1_transformed < 1] <- 2
VIP_data_subset$kolhsum1_categorized_n[!is.na(kolhsum1) & VIP_data_subset$kolhsum1_transformed >= 1 & VIP_data_subset$kolhsum1_transformed < 2] <- 3
VIP_data_subset$kolhsum1_categorized_n[!is.na(kolhsum1) & VIP_data_subset$kolhsum1_transformed >= 2] <- 4




# sacksum1 is raw, sacksum1_transformed is raw log tranformed and standardized, sacksum1_ofTEI is expressed in % of total energy intake, sacksum1_ofTEI_transformed is log transformed and 
# standardized expressed in % of TEI, sacksum1_ofTEI_categorized_g is expressed in % of TEI and categorized based on guidelines, sacksum1_ofTEI_categorized_n is expressed in % of 
# TEI and categorized based on location in the standard normal distribution, sacksum1_categorized_n is raw categorized based on location in the standard normal distribution

#continuous log transformed and standardized
VIP_data_subset$sacksum1_transformed<-log(sacksum1)
VIP_data_subset$sacksum1_transformed[!is.na(sacksum1) & visit==1]<-(VIP_data_subset$sacksum1_transformed[!is.na(sacksum1) & visit==1]-
			mean(VIP_data_subset$sacksum1_transformed[!is.na(sacksum1) & visit==1]))/sd(VIP_data_subset$sacksum1_transformed[!is.na(sacksum1) & visit==1])
VIP_data_subset$sacksum1_transformed[!is.na(sacksum1) & visit==2]<-(VIP_data_subset$sacksum1_transformed[!is.na(sacksum1) & visit==2]-
			mean(VIP_data_subset$sacksum1_transformed[!is.na(sacksum1) & visit==2]))/sd(VIP_data_subset$sacksum1_transformed[!is.na(sacksum1) & visit==2])

# % of TEI
VIP_data_subset$sacksum1_ofTEI<-(100*4*sacksum1)/ensum1

# % of TEI log transformed and standardized
VIP_data_subset$sacksum1_ofTEI_transformed<-log(VIP_data_subset$sacksum1_ofTEI)		
VIP_data_subset$sacksum1_ofTEI_transformed[!is.na(sacksum1) & visit==1]<-(VIP_data_subset$sacksum1_ofTEI_transformed[!is.na(sacksum1) & visit==1]-
			mean(VIP_data_subset$sacksum1_ofTEI_transformed[!is.na(sacksum1) & visit==1]))/sd(VIP_data_subset$sacksum1_ofTEI_transformed[!is.na(sacksum1) & visit==1])
VIP_data_subset$sacksum1_ofTEI_transformed[!is.na(sacksum1) & visit==2]<-(VIP_data_subset$sacksum1_ofTEI_transformed[!is.na(sacksum1) & visit==2]-
			mean(VIP_data_subset$sacksum1_ofTEI_transformed[!is.na(sacksum1) & visit==2]))/sd(VIP_data_subset$sacksum1_ofTEI_transformed[!is.na(sacksum1) & visit==2])

# categorizing the % of TEI based on guidelines
VIP_data_subset$sacksum1_ofTEI_categorized_g[!is.na(sacksum1)]<-2
VIP_data_subset$sacksum1_ofTEI_categorized_g[!is.na(sacksum1) & VIP_data_subset$sacksum1_ofTEI < 10 ]<-1

# categorizing the % of TEI based on location in standard normal distribution
VIP_data_subset$sacksum1_ofTEI_categorized_n[!is.na(sacksum1)] <-0
VIP_data_subset$sacksum1_ofTEI_categorized_n[!is.na(sacksum1) & VIP_data_subset$sacksum1_ofTEI_transformed >= -1 & VIP_data_subset$sacksum1_ofTEI_transformed < 0] <- 1
VIP_data_subset$sacksum1_ofTEI_categorized_n[!is.na(sacksum1) & VIP_data_subset$sacksum1_ofTEI_transformed >= 0 & VIP_data_subset$sacksum1_ofTEI_transformed < 1] <- 2
VIP_data_subset$sacksum1_ofTEI_categorized_n[!is.na(sacksum1) & VIP_data_subset$sacksum1_ofTEI_transformed >= 1 & VIP_data_subset$sacksum1_ofTEI_transformed < 2] <- 3
VIP_data_subset$sacksum1_ofTEI_categorized_n[!is.na(sacksum1) & VIP_data_subset$sacksum1_ofTEI_transformed >= 2] <- 4

# categorizing raw based on location in standard normal distribution
VIP_data_subset$sacksum1_categorized_n[!is.na(sacksum1)] <-0
VIP_data_subset$sacksum1_categorized_n[!is.na(sacksum1) & VIP_data_subset$sacksum1_transformed >= -1 & VIP_data_subset$sacksum1_transformed < 0] <- 1
VIP_data_subset$sacksum1_categorized_n[!is.na(sacksum1) & VIP_data_subset$sacksum1_transformed >= 0 & VIP_data_subset$sacksum1_transformed < 1] <- 2
VIP_data_subset$sacksum1_categorized_n[!is.na(sacksum1) & VIP_data_subset$sacksum1_transformed >= 1 & VIP_data_subset$sacksum1_transformed < 2] <- 3
VIP_data_subset$sacksum1_categorized_n[!is.na(sacksum1) & VIP_data_subset$sacksum1_transformed >= 2] <- 4




# protsum1 is raw, protsum1_transformed is raw log tranformed and standardized, protsum1_ofTEI is expressed in % of total energy intake, protsum1_ofTEI_transformed is log transformed and 
# standardized expressed in % of TEI, protsum1_ofTEI_categorized_g is expressed in % of TEI and categorized based on guidelines, protsum1_ofTEI_categorized_n is expressed in % of 
# TEI and categorized based on location in the standard normal distribution, protsum1_categorized_n is raw categorized based on location in the standard normal distribution

#continuous log transformed and standardized
VIP_data_subset$protsum1_transformed<-log(protsum1)
VIP_data_subset$protsum1_transformed[!is.na(protsum1) & visit==1]<-(VIP_data_subset$protsum1_transformed[!is.na(protsum1) & visit==1]-
			mean(VIP_data_subset$protsum1_transformed[!is.na(protsum1) & visit==1]))/sd(VIP_data_subset$protsum1_transformed[!is.na(protsum1) & visit==1])
VIP_data_subset$protsum1_transformed[!is.na(protsum1) & visit==2]<-(VIP_data_subset$protsum1_transformed[!is.na(protsum1) & visit==2]-
			mean(VIP_data_subset$protsum1_transformed[!is.na(protsum1) & visit==2]))/sd(VIP_data_subset$protsum1_transformed[!is.na(protsum1) & visit==2])

# % of TEI
VIP_data_subset$protsum1_ofTEI<-(100*4*protsum1)/ensum1

# % of TEI log transformed and standardized
VIP_data_subset$protsum1_ofTEI_transformed<-log(VIP_data_subset$protsum1_ofTEI)		
VIP_data_subset$protsum1_ofTEI_transformed[!is.na(protsum1) & visit==1]<-(VIP_data_subset$protsum1_ofTEI_transformed[!is.na(protsum1) & visit==1]-
			mean(VIP_data_subset$protsum1_ofTEI_transformed[!is.na(protsum1) & visit==1]))/sd(VIP_data_subset$protsum1_ofTEI_transformed[!is.na(protsum1) & visit==1])
VIP_data_subset$protsum1_ofTEI_transformed[!is.na(protsum1) & visit==2]<-(VIP_data_subset$protsum1_ofTEI_transformed[!is.na(protsum1) & visit==2]-
			mean(VIP_data_subset$protsum1_ofTEI_transformed[!is.na(protsum1) & visit==2]))/sd(VIP_data_subset$protsum1_ofTEI_transformed[!is.na(protsum1) & visit==2])

# categorizing the % of TEI based on guidelines
VIP_data_subset$protsum1_ofTEI_categorized_g[!is.na(protsum1)]<-0
VIP_data_subset$protsum1_ofTEI_categorized_g[!is.na(protsum1) & VIP_data_subset$protsum1_ofTEI >= 10 & VIP_data_subset$protsum1_ofTEI <= 20 ]<-1
VIP_data_subset$protsum1_ofTEI_categorized_g[!is.na(protsum1) & VIP_data_subset$protsum1_ofTEI > 20]<-2

# categorizing the % of TEI based on location in standard normal distribution
VIP_data_subset$protsum1_ofTEI_categorized_n[!is.na(protsum1)] <-0
VIP_data_subset$protsum1_ofTEI_categorized_n[!is.na(protsum1) & VIP_data_subset$protsum1_ofTEI_transformed >= -1 & VIP_data_subset$protsum1_ofTEI_transformed < 0] <- 1
VIP_data_subset$protsum1_ofTEI_categorized_n[!is.na(protsum1) & VIP_data_subset$protsum1_ofTEI_transformed >= 0 & VIP_data_subset$protsum1_ofTEI_transformed < 1] <- 2
VIP_data_subset$protsum1_ofTEI_categorized_n[!is.na(protsum1) & VIP_data_subset$protsum1_ofTEI_transformed >= 1 & VIP_data_subset$protsum1_ofTEI_transformed < 2] <- 3
VIP_data_subset$protsum1_ofTEI_categorized_n[!is.na(protsum1) & VIP_data_subset$protsum1_ofTEI_transformed >= 2] <- 4

# categorizing raw based on location in standard normal distribution
VIP_data_subset$protsum1_categorized_n[!is.na(protsum1)] <-0
VIP_data_subset$protsum1_categorized_n[!is.na(protsum1) & VIP_data_subset$protsum1_transformed >= -1 & VIP_data_subset$protsum1_transformed < 0] <- 1
VIP_data_subset$protsum1_categorized_n[!is.na(protsum1) & VIP_data_subset$protsum1_transformed >= 0 & VIP_data_subset$protsum1_transformed < 1] <- 2
VIP_data_subset$protsum1_categorized_n[!is.na(protsum1) & VIP_data_subset$protsum1_transformed >= 1 & VIP_data_subset$protsum1_transformed < 2] <- 3
VIP_data_subset$protsum1_categorized_n[!is.na(protsum1) & VIP_data_subset$protsum1_transformed >= 2] <- 4




# fibesum1 is raw, fibesum1_transformed is raw log tranformed and standardized, fibesum1_ofTEI is expressed in % of total energy intake, fibesum1_ofTEI_transformed is log transformed and 
# standardized expressed in % of TEI, fibesum1_categorized_g is raw variable, categorized based on guidelines, since the guidelines are not in % of TEI for this one, 
# fibesum1_ofTEI_categorized_n is expressed in % of TEI and categorized based on location in the standard normal distribution, fibesum1_categorized_n is raw categorized based on 
# location in the standard normal distribution

#continuous log transformed and standardized
VIP_data_subset$fibesum1_transformed<-log(fibesum1)
VIP_data_subset$fibesum1_transformed[!is.na(fibesum1) & visit==1]<-(VIP_data_subset$fibesum1_transformed[!is.na(fibesum1) & visit==1]-
			mean(VIP_data_subset$fibesum1_transformed[!is.na(fibesum1) & visit==1]))/sd(VIP_data_subset$fibesum1_transformed[!is.na(fibesum1) & visit==1])
VIP_data_subset$fibesum1_transformed[!is.na(fibesum1) & visit==2]<-(VIP_data_subset$fibesum1_transformed[!is.na(fibesum1) & visit==2]-
			mean(VIP_data_subset$fibesum1_transformed[!is.na(fibesum1) & visit==2]))/sd(VIP_data_subset$fibesum1_transformed[!is.na(fibesum1) & visit==2])

# % of TEI
VIP_data_subset$fibesum1_ofTEI<-(100*4*fibesum1)/ensum1

# % of TEI log transformed and standardized
VIP_data_subset$fibesum1_ofTEI_transformed<-log(VIP_data_subset$fibesum1_ofTEI)		
VIP_data_subset$fibesum1_ofTEI_transformed[!is.na(fibesum1) & visit==1]<-(VIP_data_subset$fibesum1_ofTEI_transformed[!is.na(fibesum1) & visit==1]-
			mean(VIP_data_subset$fibesum1_ofTEI_transformed[!is.na(fibesum1) & visit==1]))/sd(VIP_data_subset$fibesum1_ofTEI_transformed[!is.na(fibesum1) & visit==1])
VIP_data_subset$fibesum1_ofTEI_transformed[!is.na(fibesum1) & visit==2]<-(VIP_data_subset$fibesum1_ofTEI_transformed[!is.na(fibesum1) & visit==2]-
			mean(VIP_data_subset$fibesum1_ofTEI_transformed[!is.na(fibesum1) & visit==2]))/sd(VIP_data_subset$fibesum1_ofTEI_transformed[!is.na(fibesum1) & visit==2])

# categorizing based on guidelines
VIP_data_subset$fibesum1_categorized_g[!is.na(fibesum1)]<-1
VIP_data_subset$fibesum1_categorized_g[!is.na(fibesum1) & fibesum1 < 25 ]<-2

# categorizing the % of TEI based on location in standard normal distribution
VIP_data_subset$fibesum1_ofTEI_categorized_n[!is.na(fibesum1)] <-0
VIP_data_subset$fibesum1_ofTEI_categorized_n[!is.na(fibesum1) & VIP_data_subset$fibesum1_ofTEI_transformed >= -1 & VIP_data_subset$fibesum1_ofTEI_transformed < 0] <- 1
VIP_data_subset$fibesum1_ofTEI_categorized_n[!is.na(fibesum1) & VIP_data_subset$fibesum1_ofTEI_transformed >= 0 & VIP_data_subset$fibesum1_ofTEI_transformed < 1] <- 2
VIP_data_subset$fibesum1_ofTEI_categorized_n[!is.na(fibesum1) & VIP_data_subset$fibesum1_ofTEI_transformed >= 1 & VIP_data_subset$fibesum1_ofTEI_transformed < 2] <- 3
VIP_data_subset$fibesum1_ofTEI_categorized_n[!is.na(fibesum1) & VIP_data_subset$fibesum1_ofTEI_transformed >= 2] <- 4

# categorizing raw based on location in standard normal distribution
VIP_data_subset$fibesum1_categorized_n[!is.na(fibesum1)] <-0
VIP_data_subset$fibesum1_categorized_n[!is.na(fibesum1) & VIP_data_subset$fibesum1_transformed >= -1 & VIP_data_subset$fibesum1_transformed < 0] <- 1
VIP_data_subset$fibesum1_categorized_n[!is.na(fibesum1) & VIP_data_subset$fibesum1_transformed >= 0 & VIP_data_subset$fibesum1_transformed < 1] <- 2
VIP_data_subset$fibesum1_categorized_n[!is.na(fibesum1) & VIP_data_subset$fibesum1_transformed >= 1 & VIP_data_subset$fibesum1_transformed < 2] <- 3
VIP_data_subset$fibesum1_categorized_n[!is.na(fibesum1) & VIP_data_subset$fibesum1_transformed >= 2] <- 4




# NATRsum1 is raw, NATRsum1_transformed is raw log tranformed and standardized, NATRsum1_ofTEI is expressed in % of total energy intake, NATRsum1_ofTEI_transformed is log transformed and 
# standardized expressed in % of TEI, NATRsum1_categorized_g is raw variable, categorized based on guidelines, since the guidelines are not in % of TEI for this one, 
# NATRsum1_ofTEI_categorized_n is expressed in % of TEI and categorized based on location in the standard normal distribution, NATRsum1_categorized_n is raw categorized based on 
# location in the standard normal distribution

#continuous log transformed and standardized
VIP_data_subset$NATRsum1_transformed<-log(NATRsum1/1000)
VIP_data_subset$NATRsum1_transformed[!is.na(NATRsum1) & visit==1]<-(VIP_data_subset$NATRsum1_transformed[!is.na(NATRsum1) & visit==1]-
			mean(VIP_data_subset$NATRsum1_transformed[!is.na(NATRsum1) & visit==1]))/sd(VIP_data_subset$NATRsum1_transformed[!is.na(NATRsum1) & visit==1])
VIP_data_subset$NATRsum1_transformed[!is.na(NATRsum1) & visit==2]<-(VIP_data_subset$NATRsum1_transformed[!is.na(NATRsum1) & visit==2]-
			mean(VIP_data_subset$NATRsum1_transformed[!is.na(NATRsum1) & visit==2]))/sd(VIP_data_subset$NATRsum1_transformed[!is.na(NATRsum1) & visit==2])

# % of TEI
VIP_data_subset$NATRsum1_ofTEI<-(100*4*(NATRsum1/1000))/ensum1

# % of TEI log transformed and standardized
VIP_data_subset$NATRsum1_ofTEI_transformed<-log(VIP_data_subset$NATRsum1_ofTEI)		
VIP_data_subset$NATRsum1_ofTEI_transformed[!is.na(NATRsum1) & visit==1]<-(VIP_data_subset$NATRsum1_ofTEI_transformed[!is.na(NATRsum1) & visit==1]-
			mean(VIP_data_subset$NATRsum1_ofTEI_transformed[!is.na(NATRsum1) & visit==1]))/sd(VIP_data_subset$NATRsum1_ofTEI_transformed[!is.na(NATRsum1) & visit==1])
VIP_data_subset$NATRsum1_ofTEI_transformed[!is.na(NATRsum1) & visit==2]<-(VIP_data_subset$NATRsum1_ofTEI_transformed[!is.na(NATRsum1) & visit==2]-
			mean(VIP_data_subset$NATRsum1_ofTEI_transformed[!is.na(NATRsum1) & visit==2]))/sd(VIP_data_subset$NATRsum1_ofTEI_transformed[!is.na(NATRsum1) & visit==2])

# categorizing based on guidelines
VIP_data_subset$NATRsum1_categorized_g[!is.na(NATRsum1)]<-2
VIP_data_subset$NATRsum1_categorized_g[!is.na(NATRsum1) & NATRsum1/1000 <= 2.4 ]<-1

# categorizing the % of TEI based on location in standard normal distribution
VIP_data_subset$NATRsum1_ofTEI_categorized_n[!is.na(NATRsum1)] <-0
VIP_data_subset$NATRsum1_ofTEI_categorized_n[!is.na(NATRsum1) & VIP_data_subset$NATRsum1_ofTEI_transformed >= -1 & VIP_data_subset$NATRsum1_ofTEI_transformed < 0] <- 1
VIP_data_subset$NATRsum1_ofTEI_categorized_n[!is.na(NATRsum1) & VIP_data_subset$NATRsum1_ofTEI_transformed >= 0 & VIP_data_subset$NATRsum1_ofTEI_transformed < 1] <- 2
VIP_data_subset$NATRsum1_ofTEI_categorized_n[!is.na(NATRsum1) & VIP_data_subset$NATRsum1_ofTEI_transformed >= 1 & VIP_data_subset$NATRsum1_ofTEI_transformed < 2] <- 3
VIP_data_subset$NATRsum1_ofTEI_categorized_n[!is.na(NATRsum1) & VIP_data_subset$NATRsum1_ofTEI_transformed >= 2] <- 4

# categorizing raw based on location in standard normal distribution
VIP_data_subset$NATRsum1_categorized_n[!is.na(NATRsum1)] <-0
VIP_data_subset$NATRsum1_categorized_n[!is.na(NATRsum1) & VIP_data_subset$NATRsum1_transformed >= -1 & VIP_data_subset$NATRsum1_transformed < 0] <- 1
VIP_data_subset$NATRsum1_categorized_n[!is.na(NATRsum1) & VIP_data_subset$NATRsum1_transformed >= 0 & VIP_data_subset$NATRsum1_transformed < 1] <- 2
VIP_data_subset$NATRsum1_categorized_n[!is.na(NATRsum1) & VIP_data_subset$NATRsum1_transformed >= 1 & VIP_data_subset$NATRsum1_transformed < 2] <- 3
VIP_data_subset$NATRsum1_categorized_n[!is.na(NATRsum1) & VIP_data_subset$NATRsum1_transformed >= 2] <- 4


detach(VIP_data_subset)

#----------------------------------------------VISIT==1------------------------------------------------------------------
attach(VIP_data_subset[VIP_data_subset$visit==1,])

# check for collinearity
rcorr( cbind(bmi,POLYsum1_transformed,MONOsum1_transformed,mfetsum1_transformed,fettsum1_transformed,sacksum1_transformed,kolhsum1_transformed,FA_transformed,protsum1_transformed,fibesum1_transformed,NATRsum1_transformed),type="pearson")

#run several different regressions 

#for raw continous, fitted together

associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + POLYsum1_transformed + MONOsum1_transformed + mfetsum1_transformed + fettsum1_transformed + sacksum1_transformed
				+ kolhsum1_transformed + FA_transformed + protsum1_transformed + fibesum1_transformed + NATRsum1_transformed, family = gaussian(link = "identity"))

summary(associations)

all_variables<-c("POLYsum1_transformed","MONOsum1_transformed","mfetsum1_transformed","fettsum1_transformed","sacksum1_transformed",
		"kolhsum1_transformed","FA_transformed","protsum1_transformed","fibesum1_transformed","NATRsum1_transformed")

#r²
variable_count<-1
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi[(!is.na(bmi) & !is.na(VIP_data_subset[VIP_data_subset$visit==1,c(variable)]))],VIP_data_subset[VIP_data_subset$visit==1 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c(variable)],
			VIP_data_subset[VIP_data_subset$visit==1 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c("age","agesq","year","ffq","gender",all_variables[-variable_count])])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
	variable_count<-variable_count+1
	
}

#for raw continous, fitted separately
for (variable in all_variables){
	
	associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + VIP_data_subset[VIP_data_subset$visit==1,c(variable)], family = gaussian(link = "identity"))
	associations_summary<-summary(associations)
	
	message(variable)
	message(round(associations_summary$coefficients[[7]],7))
	message(round(associations_summary$coefficients[[28]],7))
	
}

#r²
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi[(!is.na(bmi) & !is.na(VIP_data_subset[VIP_data_subset$visit==1,c(variable)]))],VIP_data_subset[VIP_data_subset$visit==1 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c(variable)],
			VIP_data_subset[VIP_data_subset$visit==1 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c("age","agesq","year","ffq","gender")])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
}




# for continuous expressed in % of TEI, fitted together
associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + POLYsum1_ofTEI_transformed + MONOsum1_ofTEI_transformed + mfetsum1_ofTEI_transformed + fettsum1_ofTEI_transformed + sacksum1_ofTEI_transformed
				+ kolhsum1_ofTEI_transformed + FA_ofTEI_transformed + protsum1_ofTEI_transformed + fibesum1_ofTEI_transformed + NATRsum1_ofTEI_transformed, family = gaussian(link = "identity"))

summary(associations)

all_variables<-c("POLYsum1_ofTEI_transformed","MONOsum1_ofTEI_transformed","mfetsum1_ofTEI_transformed","fettsum1_ofTEI_transformed","sacksum1_ofTEI_transformed",
		"kolhsum1_ofTEI_transformed","FA_ofTEI_transformed","protsum1_ofTEI_transformed","fibesum1_ofTEI_transformed","NATRsum1_ofTEI_transformed")

#r²
variable_count<-1
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi[(!is.na(bmi) & !is.na(VIP_data_subset[VIP_data_subset$visit==1,c(variable)]))],VIP_data_subset[VIP_data_subset$visit==1 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c(variable)],
			VIP_data_subset[VIP_data_subset$visit==1 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c("age","agesq","year","ffq","gender",all_variables[-variable_count])])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
	variable_count<-variable_count+1
	
}

# for continuous expressed in % of TEI, fitted separately
for (variable in all_variables){
	
	associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + VIP_data_subset[VIP_data_subset$visit==1,c(variable)], family = gaussian(link = "identity"))
	associations_summary<-summary(associations)
	
	message(variable)
	message(round(associations_summary$coefficients[[7]],7))
	message(round(associations_summary$coefficients[[28]],7))
	
}

#r²
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi[(!is.na(bmi) & !is.na(VIP_data_subset[VIP_data_subset$visit==1,c(variable)]))],VIP_data_subset[VIP_data_subset$visit==1 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c(variable)],
			VIP_data_subset[VIP_data_subset$visit==1 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c("age","agesq","year","ffq","gender")])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
}




# categorized based on guidelines, fitted together
associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + POLYsum1_ofTEI_categorized_g + MONOsum1_ofTEI_categorized_g + mfetsum1_ofTEI_categorized_g + fettsum1_ofTEI_categorized_g + sacksum1_ofTEI_categorized_g
				+ kolhsum1_ofTEI_categorized_g + FA_ofTEI_categorized_g + protsum1_ofTEI_categorized_g + fibesum1_categorized_g + NATRsum1_categorized_g, family = gaussian(link = "identity"))

summary(associations)

all_variables<-c("POLYsum1_ofTEI_categorized_g","MONOsum1_ofTEI_categorized_g","mfetsum1_ofTEI_categorized_g","fettsum1_ofTEI_categorized_g","sacksum1_ofTEI_categorized_g",
		"kolhsum1_ofTEI_categorized_g","FA_ofTEI_categorized_g","protsum1_ofTEI_categorized_g","fibesum1_categorized_g","NATRsum1_categorized_g")

#r²
variable_count<-1
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi[(!is.na(bmi) & !is.na(VIP_data_subset[VIP_data_subset$visit==1,c(variable)]))],VIP_data_subset[VIP_data_subset$visit==1 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c(variable)],
			VIP_data_subset[VIP_data_subset$visit==1 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c("age","agesq","year","ffq","gender",all_variables[-variable_count])])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
	variable_count<-variable_count+1
	
}

# categorized based on guidelines, fitted separately
for (variable in all_variables){
	
	associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + VIP_data_subset[VIP_data_subset$visit==1,c(variable)], family = gaussian(link = "identity"))
	associations_summary<-summary(associations)
	
	message(variable)
	message(round(associations_summary$coefficients[[7]],7))
	message(round(associations_summary$coefficients[[28]],7))
	
}

#r²
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi[(!is.na(bmi) & !is.na(VIP_data_subset[VIP_data_subset$visit==1,c(variable)]))],VIP_data_subset[VIP_data_subset$visit==1 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c(variable)],
			VIP_data_subset[VIP_data_subset$visit==1 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c("age","agesq","year","ffq","gender")])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
}




# categorized the % of TEI based on location in standardized normal distribution, fitted together
associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + POLYsum1_ofTEI_categorized_n + MONOsum1_ofTEI_categorized_n + mfetsum1_ofTEI_categorized_n + fettsum1_ofTEI_categorized_n + sacksum1_ofTEI_categorized_n
				+ kolhsum1_ofTEI_categorized_n + FA_ofTEI_categorized_n + protsum1_ofTEI_categorized_n + fibesum1_categorized_n + NATRsum1_categorized_n, family = gaussian(link = "identity"))

summary(associations)

all_variables<-c("POLYsum1_ofTEI_categorized_n","MONOsum1_ofTEI_categorized_n","mfetsum1_ofTEI_categorized_n","fettsum1_ofTEI_categorized_n","sacksum1_ofTEI_categorized_n",
		"kolhsum1_ofTEI_categorized_n","FA_ofTEI_categorized_n","protsum1_ofTEI_categorized_n","fibesum1_categorized_n","NATRsum1_categorized_n")

#r²
variable_count<-1
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi[(!is.na(bmi) & !is.na(VIP_data_subset[VIP_data_subset$visit==1,c(variable)]))],VIP_data_subset[VIP_data_subset$visit==1 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c(variable)],
			VIP_data_subset[VIP_data_subset$visit==1 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c("age","agesq","year","ffq","gender",all_variables[-variable_count])])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
	variable_count<-variable_count+1
	
}

# categorized the % of TEI based on location in standardized normal distribution, fitted separately
for (variable in all_variables){
	
	associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + VIP_data_subset[VIP_data_subset$visit==1,c(variable)], family = gaussian(link = "identity"))
	associations_summary<-summary(associations)
	
	message(variable)
	message(round(associations_summary$coefficients[[7]],7))
	message(round(associations_summary$coefficients[[28]],7))
	
}

#r²
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi[(!is.na(bmi) & !is.na(VIP_data_subset[VIP_data_subset$visit==1,c(variable)]))],VIP_data_subset[VIP_data_subset$visit==1 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c(variable)],
			VIP_data_subset[VIP_data_subset$visit==1 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c("age","agesq","year","ffq","gender")])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
}



# categorized the raw based on location in standardized normal distribution, fitted together
associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + POLYsum1_categorized_n + MONOsum1_categorized_n + fettsum1_categorized_n + mfetsum1_categorized_n + sacksum1_categorized_n
				+ kolhsum1_categorized_n + FA_categorized_n + protsum1_categorized_n + fibesum1_categorized_n + NATRsum1_categorized_n, family = gaussian(link = "identity"))

summary(associations)

all_variables<-c("POLYsum1_categorized_n","MONOsum1_categorized_n","mfetsum1_categorized_n","fettsum1_categorized_n","sacksum1_categorized_n",
		"kolhsum1_categorized_n","FA_categorized_n","protsum1_categorized_n","fibesum1_categorized_n","NATRsum1_categorized_n")

#r²
variable_count<-1
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi[(!is.na(bmi) & !is.na(VIP_data_subset[VIP_data_subset$visit==1,c(variable)]))],VIP_data_subset[VIP_data_subset$visit==1 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c(variable)],
			VIP_data_subset[VIP_data_subset$visit==1 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c("age","agesq","year","ffq","gender",all_variables[-variable_count])])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
	variable_count<-variable_count+1
	
}

# categorized the raw based on location in standardized normal distribution, fitted separately
for (variable in all_variables){
	
	associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + VIP_data_subset[VIP_data_subset$visit==1,c(variable)], family = gaussian(link = "identity"))
	associations_summary<-summary(associations)
	
	message(variable)
	message(round(associations_summary$coefficients[[7]],7))
	message(round(associations_summary$coefficients[[28]],7))
	
}

#r²
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi[(!is.na(bmi) & !is.na(VIP_data_subset[VIP_data_subset$visit==1,c(variable)]))],VIP_data_subset[VIP_data_subset$visit==1 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c(variable)],
			VIP_data_subset[VIP_data_subset$visit==1 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c("age","agesq","year","ffq","gender")])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
}
detach(VIP_data_subset[VIP_data_subset$visit==1,])

#----------------------------------------------VISIT==1------------------------------------------------------------------

#----------------------------------------------VISIT==2------------------------------------------------------------------
attach(VIP_data_subset[VIP_data_subset$visit==2,])

# check for collinearity
rcorr( cbind(bmi,POLYsum1_transformed,MONOsum1_transformed,mfetsum1_transformed,fettsum1_transformed,sacksum1_transformed,kolhsum1_transformed,FA_transformed,protsum1_transformed,fibesum1_transformed,NATRsum1_transformed),type="pearson")

#run several different regressions 

#for raw continous, fitted together

associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + POLYsum1_transformed + MONOsum1_transformed + mfetsum1_transformed + fettsum1_transformed + sacksum1_transformed
				+ kolhsum1_transformed + FA_transformed + protsum1_transformed + fibesum1_transformed + NATRsum1_transformed, family = gaussian(link = "identity"))

summary(associations)

all_variables<-c("POLYsum1_transformed","MONOsum1_transformed","mfetsum1_transformed","fettsum1_transformed","sacksum1_transformed",
		"kolhsum1_transformed","FA_transformed","protsum1_transformed","fibesum1_transformed","NATRsum1_transformed")

#r²
variable_count<-1
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi[(!is.na(bmi) & !is.na(VIP_data_subset[VIP_data_subset$visit==2,c(variable)]))],VIP_data_subset[VIP_data_subset$visit==2 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c(variable)],
			VIP_data_subset[VIP_data_subset$visit==2 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c("age","agesq","year","ffq","gender",all_variables[-variable_count])])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
	variable_count<-variable_count+1
	
}

#for raw continous, fitted separately
for (variable in all_variables){
	
	associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + VIP_data_subset[VIP_data_subset$visit==2,c(variable)], family = gaussian(link = "identity"))
	associations_summary<-summary(associations)
	
	message(variable)
	message(round(associations_summary$coefficients[[7]],7))
	message(round(associations_summary$coefficients[[28]],7))
	
}

#r²
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi[(!is.na(bmi) & !is.na(VIP_data_subset[VIP_data_subset$visit==2,c(variable)]))],VIP_data_subset[VIP_data_subset$visit==2 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c(variable)],
			VIP_data_subset[VIP_data_subset$visit==2 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c("age","agesq","year","ffq","gender")])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
}




# for continuous expressed in % of TEI, fitted together
associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + POLYsum1_ofTEI_transformed + MONOsum1_ofTEI_transformed + mfetsum1_ofTEI_transformed + fettsum1_ofTEI_transformed + sacksum1_ofTEI_transformed
				+ kolhsum1_ofTEI_transformed + FA_ofTEI_transformed + protsum1_ofTEI_transformed + fibesum1_ofTEI_transformed + NATRsum1_ofTEI_transformed, family = gaussian(link = "identity"))

summary(associations)

all_variables<-c("POLYsum1_ofTEI_transformed","MONOsum1_ofTEI_transformed","mfetsum1_ofTEI_transformed","fettsum1_ofTEI_transformed","sacksum1_ofTEI_transformed",
		"kolhsum1_ofTEI_transformed","FA_ofTEI_transformed","protsum1_ofTEI_transformed","fibesum1_ofTEI_transformed","NATRsum1_ofTEI_transformed")

#r²
variable_count<-1
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi[(!is.na(bmi) & !is.na(VIP_data_subset[VIP_data_subset$visit==2,c(variable)]))],VIP_data_subset[VIP_data_subset$visit==2 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c(variable)],
			VIP_data_subset[VIP_data_subset$visit==2 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c("age","agesq","year","ffq","gender",all_variables[-variable_count])])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
	variable_count<-variable_count+1
	
}

# for continuous expressed in % of TEI, fitted separately
for (variable in all_variables){
	
	associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + VIP_data_subset[VIP_data_subset$visit==2,c(variable)], family = gaussian(link = "identity"))
	associations_summary<-summary(associations)
	
	message(variable)
	message(round(associations_summary$coefficients[[7]],7))
	message(round(associations_summary$coefficients[[28]],7))
	
}

#r²
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi[(!is.na(bmi) & !is.na(VIP_data_subset[VIP_data_subset$visit==2,c(variable)]))],VIP_data_subset[VIP_data_subset$visit==2 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c(variable)],
			VIP_data_subset[VIP_data_subset$visit==2 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c("age","agesq","year","ffq","gender")])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
}




# categorized based on guidelines, fitted together
associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + POLYsum1_ofTEI_categorized_g + MONOsum1_ofTEI_categorized_g + mfetsum1_ofTEI_categorized_g + fettsum1_ofTEI_categorized_g + sacksum1_ofTEI_categorized_g
				+ kolhsum1_ofTEI_categorized_g + FA_ofTEI_categorized_g + protsum1_ofTEI_categorized_g + fibesum1_categorized_g + NATRsum1_categorized_g, family = gaussian(link = "identity"))

summary(associations)

all_variables<-c("POLYsum1_ofTEI_categorized_g","MONOsum1_ofTEI_categorized_g","mfetsum1_ofTEI_categorized_g","fettsum1_ofTEI_categorized_g","sacksum1_ofTEI_categorized_g",
		"kolhsum1_ofTEI_categorized_g","FA_ofTEI_categorized_g","protsum1_ofTEI_categorized_g","fibesum1_categorized_g","NATRsum1_categorized_g")

#r²
variable_count<-1
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi[(!is.na(bmi) & !is.na(VIP_data_subset[VIP_data_subset$visit==2,c(variable)]))],VIP_data_subset[VIP_data_subset$visit==2 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c(variable)],
			VIP_data_subset[VIP_data_subset$visit==2 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c("age","agesq","year","ffq","gender",all_variables[-variable_count])])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
	variable_count<-variable_count+1
	
}

# categorized based on guidelines, fitted separately
for (variable in all_variables){
	
	associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + VIP_data_subset[VIP_data_subset$visit==2,c(variable)], family = gaussian(link = "identity"))
	associations_summary<-summary(associations)
	
	message(variable)
	message(round(associations_summary$coefficients[[7]],7))
	message(round(associations_summary$coefficients[[28]],7))
	
}

#r²
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi[(!is.na(bmi) & !is.na(VIP_data_subset[VIP_data_subset$visit==2,c(variable)]))],VIP_data_subset[VIP_data_subset$visit==2 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c(variable)],
			VIP_data_subset[VIP_data_subset$visit==2 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c("age","agesq","year","ffq","gender")])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
}




# categorized the % of TEI based on location in standardized normal distribution, fitted together
associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + POLYsum1_ofTEI_categorized_n + MONOsum1_ofTEI_categorized_n + mfetsum1_ofTEI_categorized_n + fettsum1_ofTEI_categorized_n + sacksum1_ofTEI_categorized_n
				+ kolhsum1_ofTEI_categorized_n + FA_ofTEI_categorized_n + protsum1_ofTEI_categorized_n + fibesum1_categorized_n + NATRsum1_categorized_n, family = gaussian(link = "identity"))

summary(associations)

all_variables<-c("POLYsum1_ofTEI_categorized_n","MONOsum1_ofTEI_categorized_n","mfetsum1_ofTEI_categorized_n","fettsum1_ofTEI_categorized_n","sacksum1_ofTEI_categorized_n",
		"kolhsum1_ofTEI_categorized_n","FA_ofTEI_categorized_n","protsum1_ofTEI_categorized_n","fibesum1_categorized_n","NATRsum1_categorized_n")

#r²
variable_count<-1
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi[(!is.na(bmi) & !is.na(VIP_data_subset[VIP_data_subset$visit==2,c(variable)]))],VIP_data_subset[VIP_data_subset$visit==2 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c(variable)],
			VIP_data_subset[VIP_data_subset$visit==2 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c("age","agesq","year","ffq","gender",all_variables[-variable_count])])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
	variable_count<-variable_count+1
	
}

# categorized the % of TEI based on location in standardized normal distribution, fitted separately
for (variable in all_variables){
	
	associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + VIP_data_subset[VIP_data_subset$visit==2,c(variable)], family = gaussian(link = "identity"))
	associations_summary<-summary(associations)
	
	message(variable)
	message(round(associations_summary$coefficients[[7]],7))
	message(round(associations_summary$coefficients[[28]],7))
	
}

#r²
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi[(!is.na(bmi) & !is.na(VIP_data_subset[VIP_data_subset$visit==2,c(variable)]))],VIP_data_subset[VIP_data_subset$visit==2 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c(variable)],
			VIP_data_subset[VIP_data_subset$visit==2 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c("age","agesq","year","ffq","gender")])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
}



# categorized the raw based on location in standardized normal distribution, fitted together
associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + POLYsum1_categorized_n + MONOsum1_categorized_n + fettsum1_categorized_n + mfetsum1_categorized_n + sacksum1_categorized_n
				+ kolhsum1_categorized_n + FA_categorized_n + protsum1_categorized_n + fibesum1_categorized_n + NATRsum1_categorized_n, family = gaussian(link = "identity"))

summary(associations)

all_variables<-c("POLYsum1_categorized_n","MONOsum1_categorized_n","mfetsum1_categorized_n","fettsum1_categorized_n","sacksum1_categorized_n",
		"kolhsum1_categorized_n","FA_categorized_n","protsum1_categorized_n","fibesum1_categorized_n","NATRsum1_categorized_n")

#r²
variable_count<-1
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi[(!is.na(bmi) & !is.na(VIP_data_subset[VIP_data_subset$visit==2,c(variable)]))],VIP_data_subset[VIP_data_subset$visit==2 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c(variable)],
			VIP_data_subset[VIP_data_subset$visit==2 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c("age","agesq","year","ffq","gender",all_variables[-variable_count])])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
	variable_count<-variable_count+1
	
}

# categorized the raw based on location in standardized normal distribution, fitted separately
for (variable in all_variables){
	
	associations<-glm(bmi~age + agesq + gender_factor + year + ffq_factor + VIP_data_subset[VIP_data_subset$visit==2,c(variable)], family = gaussian(link = "identity"))
	associations_summary<-summary(associations)
	
	message(variable)
	message(round(associations_summary$coefficients[[7]],7))
	message(round(associations_summary$coefficients[[28]],7))
	
}

#r²
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi[(!is.na(bmi) & !is.na(VIP_data_subset[VIP_data_subset$visit==2,c(variable)]))],VIP_data_subset[VIP_data_subset$visit==2 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c(variable)],
			VIP_data_subset[VIP_data_subset$visit==2 & (!is.na(bmi) & !is.na(VIP_data_subset[,c(variable)])),c("age","agesq","year","ffq","gender")])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
}
detach(VIP_data_subset[VIP_data_subset$visit==2,])


#----------------------------------------------VISIT==2------------------------------------------------------------------

attach(VIP_data_subset)
#check the number of subjects in each visit for each category based on guidelines

#visit 1:

#POLYsum1
length(POLYsum1_ofTEI_categorized_g[!is.na(POLYsum1_ofTEI_categorized_g) & POLYsum1_ofTEI_categorized_g==0 & visit==1])
length(POLYsum1_ofTEI_categorized_g[!is.na(POLYsum1_ofTEI_categorized_g) & POLYsum1_ofTEI_categorized_g==1 & visit==1])
length(POLYsum1_ofTEI_categorized_g[!is.na(POLYsum1_ofTEI_categorized_g) & POLYsum1_ofTEI_categorized_g==2 & visit==1])

#MONOsum1
length(MONOsum1_ofTEI_categorized_g[!is.na(MONOsum1_ofTEI_categorized_g) & MONOsum1_ofTEI_categorized_g==0 & visit==1])
length(MONOsum1_ofTEI_categorized_g[!is.na(MONOsum1_ofTEI_categorized_g) & MONOsum1_ofTEI_categorized_g==1 & visit==1])
length(MONOsum1_ofTEI_categorized_g[!is.na(MONOsum1_ofTEI_categorized_g) & MONOsum1_ofTEI_categorized_g==2 & visit==1])

#mfetsum1
length(mfetsum1_ofTEI_categorized_g[!is.na(mfetsum1_ofTEI_categorized_g) & mfetsum1_ofTEI_categorized_g==1 & visit==1])
length(mfetsum1_ofTEI_categorized_g[!is.na(mfetsum1_ofTEI_categorized_g) & mfetsum1_ofTEI_categorized_g==2 & visit==1])

#fettsum1
length(fettsum1_ofTEI_categorized_g[!is.na(fettsum1_ofTEI_categorized_g) & fettsum1_ofTEI_categorized_g==0 & visit==1])
length(fettsum1_ofTEI_categorized_g[!is.na(fettsum1_ofTEI_categorized_g) & fettsum1_ofTEI_categorized_g==1 & visit==1])
length(fettsum1_ofTEI_categorized_g[!is.na(fettsum1_ofTEI_categorized_g) & fettsum1_ofTEI_categorized_g==2 & visit==1])

#acids
length(FA_ofTEI_categorized_g[!is.na(FA_ofTEI_categorized_g) & FA_ofTEI_categorized_g==1 & visit==1])
length(FA_ofTEI_categorized_g[!is.na(FA_ofTEI_categorized_g) & FA_ofTEI_categorized_g==2 & visit==1])

#kolhsum1
length(kolhsum1_ofTEI_categorized_g[!is.na(kolhsum1_ofTEI_categorized_g) & kolhsum1_ofTEI_categorized_g==0 & visit==1])
length(kolhsum1_ofTEI_categorized_g[!is.na(kolhsum1_ofTEI_categorized_g) & kolhsum1_ofTEI_categorized_g==1 & visit==1])
length(kolhsum1_ofTEI_categorized_g[!is.na(kolhsum1_ofTEI_categorized_g) & kolhsum1_ofTEI_categorized_g==2 & visit==1])

#sacksum1_
length(sacksum1_ofTEI_categorized_g[!is.na(sacksum1_ofTEI_categorized_g) & sacksum1_ofTEI_categorized_g==1 & visit==1])
length(sacksum1_ofTEI_categorized_g[!is.na(sacksum1_ofTEI_categorized_g) & sacksum1_ofTEI_categorized_g==2 & visit==1])

#NATRsum1
length(NATRsum1_categorized_g[!is.na(NATRsum1_categorized_g) & NATRsum1_categorized_g==1 & visit==1])
length(NATRsum1_categorized_g[!is.na(NATRsum1_categorized_g) & NATRsum1_categorized_g==2 & visit==1])

#protsum1
length(protsum1_ofTEI_categorized_g[!is.na(protsum1_ofTEI_categorized_g) & protsum1_ofTEI_categorized_g==0 & visit==1])
length(protsum1_ofTEI_categorized_g[!is.na(protsum1_ofTEI_categorized_g) & protsum1_ofTEI_categorized_g==1 & visit==1])
length(protsum1_ofTEI_categorized_g[!is.na(protsum1_ofTEI_categorized_g) & protsum1_ofTEI_categorized_g==2 & visit==1])

#fibesum1
length(fibesum1_categorized_g[!is.na(fibesum1_categorized_g) & fibesum1_categorized_g==0 & visit==1])
length(fibesum1_categorized_g[!is.na(fibesum1_categorized_g) & fibesum1_categorized_g==1 & visit==1])
length(fibesum1_categorized_g[!is.na(fibesum1_categorized_g) & fibesum1_categorized_g==2 & visit==1])

#look at the bmi also
length(bmi[!is.na(bmi) & bmi<18.5 & visit==1]) #underweight
length(bmi[!is.na(bmi) & bmi>=18.5 & bmi<25 & visit==1])#normal
length(bmi[!is.na(bmi) & bmi>=25 & bmi<30 & visit==1])#overweight
length(bmi[!is.na(bmi) & bmi>=30 & visit==1])#obese

#visit 2:

#POLYsum1
length(POLYsum1_ofTEI_categorized_g[!is.na(POLYsum1_ofTEI_categorized_g) & POLYsum1_ofTEI_categorized_g==0 & visit==2])
length(POLYsum1_ofTEI_categorized_g[!is.na(POLYsum1_ofTEI_categorized_g) & POLYsum1_ofTEI_categorized_g==1 & visit==2])
length(POLYsum1_ofTEI_categorized_g[!is.na(POLYsum1_ofTEI_categorized_g) & POLYsum1_ofTEI_categorized_g==2 & visit==2])

#MONOsum1
length(MONOsum1_ofTEI_categorized_g[!is.na(MONOsum1_ofTEI_categorized_g) & MONOsum1_ofTEI_categorized_g==0 & visit==2])
length(MONOsum1_ofTEI_categorized_g[!is.na(MONOsum1_ofTEI_categorized_g) & MONOsum1_ofTEI_categorized_g==1 & visit==2])
length(MONOsum1_ofTEI_categorized_g[!is.na(MONOsum1_ofTEI_categorized_g) & MONOsum1_ofTEI_categorized_g==2 & visit==2])

#mfetsum1
length(mfetsum1_ofTEI_categorized_g[!is.na(mfetsum1_ofTEI_categorized_g) & mfetsum1_ofTEI_categorized_g==1 & visit==2])
length(mfetsum1_ofTEI_categorized_g[!is.na(mfetsum1_ofTEI_categorized_g) & mfetsum1_ofTEI_categorized_g==2 & visit==2])

#fettsum1
length(fettsum1_ofTEI_categorized_g[!is.na(fettsum1_ofTEI_categorized_g) & fettsum1_ofTEI_categorized_g==0 & visit==2])
length(fettsum1_ofTEI_categorized_g[!is.na(fettsum1_ofTEI_categorized_g) & fettsum1_ofTEI_categorized_g==1 & visit==2])
length(fettsum1_ofTEI_categorized_g[!is.na(fettsum1_ofTEI_categorized_g) & fettsum1_ofTEI_categorized_g==2 & visit==2])

#acids
length(FA_ofTEI_categorized_g[!is.na(FA_ofTEI_categorized_g) & FA_ofTEI_categorized_g==1 & visit==2])
length(FA_ofTEI_categorized_g[!is.na(FA_ofTEI_categorized_g) & FA_ofTEI_categorized_g==2 & visit==2])

#kolhsum1
length(kolhsum1_ofTEI_categorized_g[!is.na(kolhsum1_ofTEI_categorized_g) & kolhsum1_ofTEI_categorized_g==0 & visit==2])
length(kolhsum1_ofTEI_categorized_g[!is.na(kolhsum1_ofTEI_categorized_g) & kolhsum1_ofTEI_categorized_g==1 & visit==2])
length(kolhsum1_ofTEI_categorized_g[!is.na(kolhsum1_ofTEI_categorized_g) & kolhsum1_ofTEI_categorized_g==2 & visit==2])

#sacksum1_
length(sacksum1_ofTEI_categorized_g[!is.na(sacksum1_ofTEI_categorized_g) & sacksum1_ofTEI_categorized_g==1 & visit==2])
length(sacksum1_ofTEI_categorized_g[!is.na(sacksum1_ofTEI_categorized_g) & sacksum1_ofTEI_categorized_g==2 & visit==2])

#NATRsum1
length(NATRsum1_categorized_g[!is.na(NATRsum1_categorized_g) & NATRsum1_categorized_g==1 & visit==2])
length(NATRsum1_categorized_g[!is.na(NATRsum1_categorized_g) & NATRsum1_categorized_g==2 & visit==2])

#protsum1
length(protsum1_ofTEI_categorized_g[!is.na(protsum1_ofTEI_categorized_g) & protsum1_ofTEI_categorized_g==0 & visit==2])
length(protsum1_ofTEI_categorized_g[!is.na(protsum1_ofTEI_categorized_g) & protsum1_ofTEI_categorized_g==1 & visit==2])
length(protsum1_ofTEI_categorized_g[!is.na(protsum1_ofTEI_categorized_g) & protsum1_ofTEI_categorized_g==2 & visit==2])

#fibesum1
length(fibesum1_categorized_g[!is.na(fibesum1_categorized_g) & fibesum1_categorized_g==0 & visit==2])
length(fibesum1_categorized_g[!is.na(fibesum1_categorized_g) & fibesum1_categorized_g==1 & visit==2])
length(fibesum1_categorized_g[!is.na(fibesum1_categorized_g) & fibesum1_categorized_g==2 & visit==2])

#look at the bmi also
length(bmi[!is.na(bmi) & bmi<18.5 & visit==2]) #underweight
length(bmi[!is.na(bmi) & bmi>=18.5 & bmi<25 & visit==2])#normal
length(bmi[!is.na(bmi) & bmi>=25 & bmi<30 & visit==2])#overweight
length(bmi[!is.na(bmi) & bmi>=30 & visit==2])#obese




#---------------------------------COMBINE SCORES INTO DIET SCORE---------------------------------------------


#checking different diet scores, to compare them


#first, simply combining all the scores
VIP_data_subset$diet_score1<-apply(VIP_data_subset[,c("POLYsum1_ofTEI_categorized_g","MONOsum1_ofTEI_categorized_g","mfetsum1_ofTEI_categorized_g","fettsum1_ofTEI_categorized_g",
						"sacksum1_ofTEI_categorized_g","kolhsum1_ofTEI_categorized_g","FA_ofTEI_categorized_g","protsum1_ofTEI_categorized_g","fibesum1_categorized_g",
						"NATRsum1_categorized_g")], 1, FUN=sum)

detach(VIP_data_subset)
attach(VIP_data_subset)

#estimate the association of first diet score with the bmi
#visit 1
associations<-glm(bmi[visit==1]~age[visit==1] + agesq[visit==1] + gender_factor[visit==1] + year[visit==1] + ffq_factor[visit==1] + 
				diet_score1[visit==1], family = gaussian(link = "identity"))

summary(associations)


#check variation explained

partial_corr<-pcor.test(bmi[(visit==1 & !is.na(bmi) & !is.na(diet_score1))],diet_score1[(visit==1 & !is.na(bmi) & !is.na(diet_score1))],
		VIP_data_subset[(visit==1 & !is.na(bmi) & !is.na(diet_score1)),c("age","agesq","year","ffq","gender")])

round(partial_corr[[1]]*partial_corr[[1]],6)
partial_corr[[2]]

#visit 2
associations<-glm(bmi[visit==2]~age[visit==2] + agesq[visit==2] + gender_factor[visit==2] + year[visit==2] + ffq_factor[visit==2] + 
				diet_score1[visit==2], family = gaussian(link = "identity"))

summary(associations)


#check variation explained

partial_corr<-pcor.test(bmi[(visit==2 & !is.na(bmi) & !is.na(diet_score1))],diet_score1[(visit==2 & !is.na(bmi) & !is.na(diet_score1))],
		VIP_data_subset[(visit==2 & !is.na(bmi) & !is.na(diet_score1)),c("age","agesq","year","ffq","gender")])

round(partial_corr[[1]]*partial_corr[[1]],6)
partial_corr[[2]]

#check the number of subjects in each categorory and also the distribution of bmi inside the category

#categories
sort(unique(diet_score1))
#visit 1
length(diet_score1[!is.na(diet_score1) & visit==1 & diet_score1==7])
length(diet_score1[!is.na(diet_score1) & visit==1 & diet_score1==8])
length(diet_score1[!is.na(diet_score1) & visit==1 & diet_score1==9])
length(diet_score1[!is.na(diet_score1) & visit==1 & diet_score1==10])
length(diet_score1[!is.na(diet_score1) & visit==1 & diet_score1==11])
length(diet_score1[!is.na(diet_score1) & visit==1 & diet_score1==12])
length(diet_score1[!is.na(diet_score1) & visit==1 & diet_score1==13])
length(diet_score1[!is.na(diet_score1) & visit==1 & diet_score1==14])
length(diet_score1[!is.na(diet_score1) & visit==1 & diet_score1==15])
length(diet_score1[!is.na(diet_score1) & visit==1 & diet_score1==16])

#visit 2
length(diet_score1[!is.na(diet_score1) & visit==2 & diet_score1==7])
length(diet_score1[!is.na(diet_score1) & visit==2 & diet_score1==8])
length(diet_score1[!is.na(diet_score1) & visit==2 & diet_score1==9])
length(diet_score1[!is.na(diet_score1) & visit==2 & diet_score1==10])
length(diet_score1[!is.na(diet_score1) & visit==2 & diet_score1==11])
length(diet_score1[!is.na(diet_score1) & visit==2 & diet_score1==12])
length(diet_score1[!is.na(diet_score1) & visit==2 & diet_score1==13])
length(diet_score1[!is.na(diet_score1) & visit==2 & diet_score1==14])
length(diet_score1[!is.na(diet_score1) & visit==2 & diet_score1==15])
length(diet_score1[!is.na(diet_score1) & visit==2 & diet_score1==16])


#bmi
message("underweight")
for (diet_category in 7:16){
	message(length(bmi[!is.na(diet_score1) & visit==1 & diet_score1==diet_category & !is.na(bmi) & bmi<18.5]))
}
message("normal")
for (diet_category in 7:16){
	message(length(bmi[!is.na(diet_score1) & visit==1 & diet_score1==diet_category & !is.na(bmi) & bmi>=18.5 & bmi<25]))
}
message("overweight")
for (diet_category in 7:16){
	message(length(bmi[!is.na(diet_score1) & visit==1 & diet_score1==diet_category & !is.na(bmi) & bmi>=25 & bmi<30]))
}
message("obese")
for (diet_category in 7:16){
	message(length(bmi[!is.na(diet_score1) & visit==1 & diet_score1==diet_category & !is.na(bmi) & bmi>=30]))
}


#bmi
message("underweight")
for (diet_category in 7:16){
	message(length(bmi[!is.na(diet_score1) & visit==2 & diet_score1==diet_category & !is.na(bmi) & bmi<18.5]))
}
message("normal")
for (diet_category in 7:16){
	message(length(bmi[!is.na(diet_score1) & visit==2 & diet_score1==diet_category & !is.na(bmi) & bmi>=18.5 & bmi<25]))
}
message("overweight")
for (diet_category in 7:16){
	message(length(bmi[!is.na(diet_score1) & visit==2 & diet_score1==diet_category & !is.na(bmi) & bmi>=25 & bmi<30]))
}
message("obese")
for (diet_category in 7:16){
	message(length(bmi[!is.na(diet_score1) & visit==2 & diet_score1==diet_category & !is.na(bmi) & bmi>=30]))
}


#second, combining all the scores multiplied with the standardized beta coefficient of the categorized variables from an independant dataset
VIP_data_subset$POLYsum1_ofTEI_categorized_g_multiplied<-POLYsum1_ofTEI_categorized_g*0.2215384
VIP_data_subset$MONOsum1_ofTEI_categorized_g_multiplied<-MONOsum1_ofTEI_categorized_g*0.3588432
VIP_data_subset$mfetsum1_ofTEI_categorized_g_multiplied<-mfetsum1_ofTEI_categorized_g*-0.1572909
VIP_data_subset$fettsum1_ofTEI_categorized_g_multiplied<-fettsum1_ofTEI_categorized_g*0.2623853
VIP_data_subset$FA_ofTEI_categorized_g_multiplied<-FA_ofTEI_categorized_g*-0.1876798
VIP_data_subset$kolhsum1_ofTEI_categorized_g_multiplied<-kolhsum1_ofTEI_categorized_g*-0.3118413
VIP_data_subset$sacksum1_ofTEI_categorized_g_multiplied<-sacksum1_ofTEI_categorized_g*-0.159644
VIP_data_subset$protsum1_ofTEI_categorized_g_multiplied<-protsum1_ofTEI_categorized_g*1.2827382
VIP_data_subset$fibesum1_categorized_g_multiplied<-fibesum1_categorized_g*0.1398369
VIP_data_subset$NATRsum1_categorized_g_multiplied<-NATRsum1_categorized_g*0.487762

detach(VIP_data_subset)
attach(VIP_data_subset)

VIP_data_subset$diet_score2<-apply(VIP_data_subset[,c("POLYsum1_ofTEI_categorized_g_multiplied","MONOsum1_ofTEI_categorized_g_multiplied","mfetsum1_ofTEI_categorized_g_multiplied","fettsum1_ofTEI_categorized_g_multiplied",
						"sacksum1_ofTEI_categorized_g_multiplied","kolhsum1_ofTEI_categorized_g_multiplied","FA_ofTEI_categorized_g_multiplied","protsum1_ofTEI_categorized_g_multiplied","fibesum1_categorized_g_multiplied",
						"NATRsum1_categorized_g_multiplied")], 1, FUN=sum)

detach(VIP_data_subset)
attach(VIP_data_subset)

#estimate the association of second diet score with the bmi
#visit 1
associations<-glm(bmi[visit==1]~age[visit==1] + agesq[visit==1] + gender_factor[visit==1] + year[visit==1] + ffq_factor[visit==1] + 
				diet_score2[visit==1], family = gaussian(link = "identity"))

summary(associations)


#check variation explained

partial_corr<-pcor.test(bmi[(visit==1 & !is.na(bmi) & !is.na(diet_score2))],diet_score2[(visit==1 & !is.na(bmi) & !is.na(diet_score2))],
		VIP_data_subset[(visit==1 & !is.na(bmi) & !is.na(diet_score2)),c("age","agesq","year","ffq","gender")])

round(partial_corr[[1]]*partial_corr[[1]],6)
partial_corr[[2]]

#visit 2
associations<-glm(bmi[visit==2]~age[visit==2] + agesq[visit==2] + gender_factor[visit==2] + year[visit==2] + ffq_factor[visit==2] + 
				diet_score2[visit==2], family = gaussian(link = "identity"))

summary(associations)


#check variation explained

partial_corr<-pcor.test(bmi[(visit==2 & !is.na(bmi) & !is.na(diet_score2))],diet_score2[(visit==2 & !is.na(bmi) & !is.na(diet_score2))],
		VIP_data_subset[(visit==2 & !is.na(bmi) & !is.na(diet_score2)),c("age","agesq","year","ffq","gender")])

round(partial_corr[[1]]*partial_corr[[1]],6)
partial_corr[[2]]

#check the number of subjects in each categorory and also the distribution of bmi inside the category

#divide into 10 categories as the original score would have been, but dont divide as if it has uniform distribution, but divide as normal distribution
VIP_data_subset$diet_score2_sd[visit==1 & !is.na(diet_score2)]<-(diet_score2[visit==1 & !is.na(diet_score2)]-mean(diet_score2[visit==1 & !is.na(diet_score2)]))/sd(diet_score2[visit==1 & !is.na(diet_score2)])
VIP_data_subset$diet_score2_sd[visit==2 & !is.na(diet_score2)]<-(diet_score2[visit==2 & !is.na(diet_score2)]-mean(diet_score2[visit==2 & !is.na(diet_score2)]))/sd(diet_score2[visit==2 & !is.na(diet_score2)])
detach(VIP_data_subset)
attach(VIP_data_subset)


#visit 1
length(diet_score2_sd[!is.na(diet_score2_sd) & visit==1 & diet_score2_sd < -4])
length(diet_score2_sd[!is.na(diet_score2_sd) & visit==1 & diet_score2_sd>= -4 & diet_score2_sd< -3])
length(diet_score2_sd[!is.na(diet_score2_sd) & visit==1 & diet_score2_sd>= -3 & diet_score2_sd< -2])
length(diet_score2_sd[!is.na(diet_score2_sd) & visit==1 & diet_score2_sd>= -2 & diet_score2_sd< -1])
length(diet_score2_sd[!is.na(diet_score2_sd) & visit==1 & diet_score2_sd>= -1 & diet_score2_sd< 0])
length(diet_score2_sd[!is.na(diet_score2_sd) & visit==1 & diet_score2_sd>= 0 & diet_score2_sd< 1])
length(diet_score2_sd[!is.na(diet_score2_sd) & visit==1 & diet_score2_sd>= 1 & diet_score2_sd< 2])
length(diet_score2_sd[!is.na(diet_score2_sd) & visit==1 & diet_score2_sd>= 2 & diet_score2_sd< 3])
length(diet_score2_sd[!is.na(diet_score2_sd) & visit==1 & diet_score2_sd>= 3 & diet_score2_sd< 4])
length(diet_score2_sd[!is.na(diet_score2_sd) & visit==1 & diet_score2_sd>= 4])

#visit 2
length(diet_score2_sd[!is.na(diet_score2_sd) & visit==2 & diet_score2_sd < -4])
length(diet_score2_sd[!is.na(diet_score2_sd) & visit==2 & diet_score2_sd>= -4 & diet_score2_sd< -3])
length(diet_score2_sd[!is.na(diet_score2_sd) & visit==2 & diet_score2_sd>= -3 & diet_score2_sd< -2])
length(diet_score2_sd[!is.na(diet_score2_sd) & visit==2 & diet_score2_sd>= -2 & diet_score2_sd< -1])
length(diet_score2_sd[!is.na(diet_score2_sd) & visit==2 & diet_score2_sd>= -1 & diet_score2_sd< 0])
length(diet_score2_sd[!is.na(diet_score2_sd) & visit==2 & diet_score2_sd>= 0 & diet_score2_sd< 1])
length(diet_score2_sd[!is.na(diet_score2_sd) & visit==2 & diet_score2_sd>= 1 & diet_score2_sd< 2])
length(diet_score2_sd[!is.na(diet_score2_sd) & visit==2 & diet_score2_sd>= 2 & diet_score2_sd< 3])
length(diet_score2_sd[!is.na(diet_score2_sd) & visit==2 & diet_score2_sd>= 3 & diet_score2_sd< 4])
length(diet_score2_sd[!is.na(diet_score2_sd) & visit==2 & diet_score2_sd>= 4])

#bmi
message("underweight")
length(bmi[!is.na(diet_score2_sd) & visit==1 & diet_score2_sd < -4 & !is.na(bmi) & bmi<18.5])
for (diet_category in -3:4){
	message(length(bmi[!is.na(diet_score2_sd) & visit==1 & diet_score2_sd>=(diet_category-1) & diet_score2_sd < diet_category & !is.na(bmi) & bmi<18.5]))
}
length(bmi[!is.na(diet_score2_sd) & visit==1 & diet_score2_sd >= 4 & !is.na(bmi) & bmi<18.5])

message("normal")
length(bmi[!is.na(diet_score2_sd) & visit==1 & diet_score2_sd < -4 & !is.na(bmi) & bmi>=18.5 & bmi<25])
for (diet_category in -3:4){
	message(length(bmi[!is.na(diet_score2_sd) & visit==1 & diet_score2_sd>=(diet_category-1) & diet_score2_sd < diet_category & !is.na(bmi) & bmi>=18.5 & bmi<25]))
}
length(bmi[!is.na(diet_score2_sd) & visit==1 & diet_score2_sd >= 4 & !is.na(bmi) & bmi>=18.5 & bmi<25])

message("overweight")
length(bmi[!is.na(diet_score2_sd) & visit==1 & diet_score2_sd < -4 & !is.na(bmi) & bmi>=25 & bmi<30])
for (diet_category in -3:4){
	message(length(bmi[!is.na(diet_score2_sd) & visit==1 & diet_score2_sd>=(diet_category-1) & diet_score2_sd < diet_category & !is.na(bmi) & bmi>=25 & bmi<30]))
}
length(bmi[!is.na(diet_score2_sd) & visit==1 & diet_score2_sd >= 4 & !is.na(bmi) & bmi>=25 & bmi<30])

message("obese")
length(bmi[!is.na(diet_score2_sd) & visit==1 & diet_score2_sd < -4 & !is.na(bmi) & bmi>=30])
for (diet_category in -3:4){
	message(length(bmi[!is.na(diet_score2_sd) & visit==1 & diet_score2_sd>=(diet_category-1) & diet_score2_sd < diet_category & !is.na(bmi) & bmi>=30]))
}
length(bmi[!is.na(diet_score2_sd) & visit==1 & diet_score2_sd >= 4 & !is.na(bmi) & bmi>=30])


#bmi
message("underweight")
length(bmi[!is.na(diet_score2_sd) & visit==2 & diet_score2_sd < -4 & !is.na(bmi) & bmi<18.5])
for (diet_category in -3:4){
	message(length(bmi[!is.na(diet_score2_sd) & visit==2 & diet_score2_sd>=(diet_category-1) & diet_score2_sd < diet_category & !is.na(bmi) & bmi<18.5]))
}
length(bmi[!is.na(diet_score2_sd) & visit==2 & diet_score2_sd >= 4 & !is.na(bmi) & bmi<18.5])

message("normal")
length(bmi[!is.na(diet_score2_sd) & visit==2 & diet_score2_sd < -4 & !is.na(bmi) & bmi>=18.5 & bmi<25])
for (diet_category in -3:4){
	message(length(bmi[!is.na(diet_score2_sd) & visit==2 & diet_score2_sd>=(diet_category-1) & diet_score2_sd < diet_category & !is.na(bmi) & bmi>=18.5 & bmi<25]))
}
length(bmi[!is.na(diet_score2_sd) & visit==2 & diet_score2_sd >= 4 & !is.na(bmi) & bmi>=18.5 & bmi<25])

message("overweight")
length(bmi[!is.na(diet_score2_sd) & visit==2 & diet_score2_sd < -4 & !is.na(bmi) & bmi>=25 & bmi<30])
for (diet_category in -3:4){
	message(length(bmi[!is.na(diet_score2_sd) & visit==2 & diet_score2_sd>=(diet_category-1) & diet_score2_sd < diet_category & !is.na(bmi) & bmi>=25 & bmi<30]))
}
length(bmi[!is.na(diet_score2_sd) & visit==2 & diet_score2_sd >= 4 & !is.na(bmi) & bmi>=25 & bmi<30])

message("obese")
length(bmi[!is.na(diet_score2_sd) & visit==2 & diet_score2_sd < -4 & !is.na(bmi) & bmi>=30])
for (diet_category in -3:4){
	message(length(bmi[!is.na(diet_score2_sd) & visit==2 & diet_score2_sd>=(diet_category-1) & diet_score2_sd < diet_category & !is.na(bmi) & bmi>=30]))
}
length(bmi[!is.na(diet_score2_sd) & visit==2 & diet_score2_sd >= 4 & !is.na(bmi) & bmi>=30])




#third, combining only the scores from the variables which complied with the guidelines on which the scores are based and are not very highly correlated with others
#(fettsum1, protsum1, NATRsum1, fibesum1) multiplied with the beta coefficient 
VIP_data_subset$diet_score3<-apply(VIP_data_subset[,c("fettsum1_ofTEI_categorized_g_multiplied", "protsum1_ofTEI_categorized_g_multiplied", 
						"NATRsum1_categorized_g_multiplied", "fibesum1_categorized_g_multiplied")], 1, FUN=sum)

detach(VIP_data_subset)
attach(VIP_data_subset)

#estimate the association of third diet score with the bmi
#visit 1
associations<-glm(bmi[visit==1]~age[visit==1] + agesq[visit==1] + gender_factor[visit==1] + year[visit==1] + ffq_factor[visit==1] + 
				diet_score3[visit==1], family = gaussian(link = "identity"))

summary(associations)


#check variation explained

partial_corr<-pcor.test(bmi[(visit==1 & !is.na(bmi) & !is.na(diet_score3))],diet_score3[(visit==1 & !is.na(bmi) & !is.na(diet_score3))],
		VIP_data_subset[(visit==1 & !is.na(bmi) & !is.na(diet_score3)),c("age","agesq","year","ffq","gender")])

round(partial_corr[[1]]*partial_corr[[1]],6)
partial_corr[[2]]

#visit 2
associations<-glm(bmi[visit==2]~age[visit==2] + agesq[visit==2] + gender_factor[visit==2] + year[visit==2] + ffq_factor[visit==2] + 
				diet_score3[visit==2], family = gaussian(link = "identity"))

summary(associations)


#check variation explained

partial_corr<-pcor.test(bmi[(visit==2 & !is.na(bmi) & !is.na(diet_score3))],diet_score3[(visit==2 & !is.na(bmi) & !is.na(diet_score3))],
		VIP_data_subset[(visit==2 & !is.na(bmi) & !is.na(diet_score3)),c("age","agesq","year","ffq","gender")])

round(partial_corr[[1]]*partial_corr[[1]],6)
partial_corr[[2]]


#divide into 10 categories as the original score would have been, but dont divide as if it has uniform distribution, but divide as normal distribution
VIP_data_subset$diet_score3_sd[visit==1 & !is.na(diet_score3)]<-(diet_score3[visit==1 & !is.na(diet_score3)]-mean(diet_score3[visit==1 & !is.na(diet_score3)]))/sd(diet_score3[visit==1 & !is.na(diet_score3)])
VIP_data_subset$diet_score3_sd[visit==2 & !is.na(diet_score3)]<-(diet_score3[visit==2 & !is.na(diet_score3)]-mean(diet_score3[visit==2 & !is.na(diet_score3)]))/sd(diet_score3[visit==2 & !is.na(diet_score3)])
detach(VIP_data_subset)
attach(VIP_data_subset)


#visit 1
length(diet_score3_sd[!is.na(diet_score3_sd) & visit==1 & diet_score3_sd < -4])
length(diet_score3_sd[!is.na(diet_score3_sd) & visit==1 & diet_score3_sd>= -4 & diet_score3_sd< -3])
length(diet_score3_sd[!is.na(diet_score3_sd) & visit==1 & diet_score3_sd>= -3 & diet_score3_sd< -2])
length(diet_score3_sd[!is.na(diet_score3_sd) & visit==1 & diet_score3_sd>= -2 & diet_score3_sd< -1])
length(diet_score3_sd[!is.na(diet_score3_sd) & visit==1 & diet_score3_sd>= -1 & diet_score3_sd< 0])
length(diet_score3_sd[!is.na(diet_score3_sd) & visit==1 & diet_score3_sd>= 0 & diet_score3_sd< 1])
length(diet_score3_sd[!is.na(diet_score3_sd) & visit==1 & diet_score3_sd>= 1 & diet_score3_sd< 2])
length(diet_score3_sd[!is.na(diet_score3_sd) & visit==1 & diet_score3_sd>= 2 & diet_score3_sd< 3])
length(diet_score3_sd[!is.na(diet_score3_sd) & visit==1 & diet_score3_sd>= 3 & diet_score3_sd< 4])
length(diet_score3_sd[!is.na(diet_score3_sd) & visit==1 & diet_score3_sd>= 4])

#visit 2
length(diet_score3_sd[!is.na(diet_score3_sd) & visit==2 & diet_score3_sd < -4])
length(diet_score3_sd[!is.na(diet_score3_sd) & visit==2 & diet_score3_sd>= -4 & diet_score3_sd< -3])
length(diet_score3_sd[!is.na(diet_score3_sd) & visit==2 & diet_score3_sd>= -3 & diet_score3_sd< -2])
length(diet_score3_sd[!is.na(diet_score3_sd) & visit==2 & diet_score3_sd>= -2 & diet_score3_sd< -1])
length(diet_score3_sd[!is.na(diet_score3_sd) & visit==2 & diet_score3_sd>= -1 & diet_score3_sd< 0])
length(diet_score3_sd[!is.na(diet_score3_sd) & visit==2 & diet_score3_sd>= 0 & diet_score3_sd< 1])
length(diet_score3_sd[!is.na(diet_score3_sd) & visit==2 & diet_score3_sd>= 1 & diet_score3_sd< 2])
length(diet_score3_sd[!is.na(diet_score3_sd) & visit==2 & diet_score3_sd>= 2 & diet_score3_sd< 3])
length(diet_score3_sd[!is.na(diet_score3_sd) & visit==2 & diet_score3_sd>= 3 & diet_score3_sd< 4])
length(diet_score3_sd[!is.na(diet_score3_sd) & visit==2 & diet_score3_sd>= 4])

#bmi
message("underweight")
length(bmi[!is.na(diet_score3_sd) & visit==1 & diet_score3_sd < -4 & !is.na(bmi) & bmi<18.5])
for (diet_category in -3:4){
	message(length(bmi[!is.na(diet_score3_sd) & visit==1 & diet_score3_sd>=(diet_category-1) & diet_score3_sd < diet_category & !is.na(bmi) & bmi<18.5]))
}
length(bmi[!is.na(diet_score3_sd) & visit==1 & diet_score3_sd >= 4 & !is.na(bmi) & bmi<18.5])

message("normal")
length(bmi[!is.na(diet_score3_sd) & visit==1 & diet_score3_sd < -4 & !is.na(bmi) & bmi>=18.5 & bmi<25])
for (diet_category in -3:4){
	message(length(bmi[!is.na(diet_score3_sd) & visit==1 & diet_score3_sd>=(diet_category-1) & diet_score3_sd < diet_category & !is.na(bmi) & bmi>=18.5 & bmi<25]))
}
length(bmi[!is.na(diet_score3_sd) & visit==1 & diet_score3_sd >= 4 & !is.na(bmi) & bmi>=18.5 & bmi<25])

message("overweight")
length(bmi[!is.na(diet_score3_sd) & visit==1 & diet_score3_sd < -4 & !is.na(bmi) & bmi>=25 & bmi<30])
for (diet_category in -3:4){
	message(length(bmi[!is.na(diet_score3_sd) & visit==1 & diet_score3_sd>=(diet_category-1) & diet_score3_sd < diet_category & !is.na(bmi) & bmi>=25 & bmi<30]))
}
length(bmi[!is.na(diet_score3_sd) & visit==1 & diet_score3_sd >= 4 & !is.na(bmi) & bmi>=25 & bmi<30])

message("obese")
length(bmi[!is.na(diet_score3_sd) & visit==1 & diet_score3_sd < -4 & !is.na(bmi) & bmi>=30])
for (diet_category in -3:4){
	message(length(bmi[!is.na(diet_score3_sd) & visit==1 & diet_score3_sd>=(diet_category-1) & diet_score3_sd < diet_category & !is.na(bmi) & bmi>=30]))
}
length(bmi[!is.na(diet_score3_sd) & visit==1 & diet_score3_sd >= 4 & !is.na(bmi) & bmi>=30])


#bmi
message("underweight")
length(bmi[!is.na(diet_score3_sd) & visit==2 & diet_score3_sd < -4 & !is.na(bmi) & bmi<18.5])
for (diet_category in -3:4){
	message(length(bmi[!is.na(diet_score3_sd) & visit==2 & diet_score3_sd>=(diet_category-1) & diet_score3_sd < diet_category & !is.na(bmi) & bmi<18.5]))
}
length(bmi[!is.na(diet_score3_sd) & visit==2 & diet_score3_sd >= 4 & !is.na(bmi) & bmi<18.5])

message("normal")
length(bmi[!is.na(diet_score3_sd) & visit==2 & diet_score3_sd < -4 & !is.na(bmi) & bmi>=18.5 & bmi<25])
for (diet_category in -3:4){
	message(length(bmi[!is.na(diet_score3_sd) & visit==2 & diet_score3_sd>=(diet_category-1) & diet_score3_sd < diet_category & !is.na(bmi) & bmi>=18.5 & bmi<25]))
}
length(bmi[!is.na(diet_score3_sd) & visit==2 & diet_score3_sd >= 4 & !is.na(bmi) & bmi>=18.5 & bmi<25])

message("overweight")
length(bmi[!is.na(diet_score3_sd) & visit==2 & diet_score3_sd < -4 & !is.na(bmi) & bmi>=25 & bmi<30])
for (diet_category in -3:4){
	message(length(bmi[!is.na(diet_score3_sd) & visit==2 & diet_score3_sd>=(diet_category-1) & diet_score3_sd < diet_category & !is.na(bmi) & bmi>=25 & bmi<30]))
}
length(bmi[!is.na(diet_score3_sd) & visit==2 & diet_score3_sd >= 4 & !is.na(bmi) & bmi>=25 & bmi<30])

message("obese")
length(bmi[!is.na(diet_score3_sd) & visit==2 & diet_score3_sd < -4 & !is.na(bmi) & bmi>=30])
for (diet_category in -3:4){
	message(length(bmi[!is.na(diet_score3_sd) & visit==2 & diet_score3_sd>=(diet_category-1) & diet_score3_sd < diet_category & !is.na(bmi) & bmi>=30]))
}
length(bmi[!is.na(diet_score3_sd) & visit==2 & diet_score3_sd >= 4 & !is.na(bmi) & bmi>=30])




#----------------------------------------------PA score----------------------------------------------------------
#correct g1_N and g6
VIP_data_subset$g1_a<-VIP_data_subset$g1_a-1
VIP_data_subset$g1_b<-VIP_data_subset$g1_b-1
VIP_data_subset$g1_c<-VIP_data_subset$g1_c-1
VIP_data_subset$g1_d<-VIP_data_subset$g1_d-1
VIP_data_subset$g6<-VIP_data_subset$g6-1

detach(VIP_data_subset)
attach(VIP_data_subset)

#standardize
VIP_data_subset$g1_a_sd_norm[!is.na(g1_a) & visit==1]<-(g1_a[!is.na(g1_a) & visit==1]-mean(g1_a[!is.na(g1_a) & visit==1]))/sd(g1_a[!is.na(g1_a) & visit==1])
VIP_data_subset$g1_a_sd_norm[!is.na(g1_a) & visit==2]<-(g1_a[!is.na(g1_a) & visit==2]-mean(g1_a[!is.na(g1_a) & visit==2]))/sd(g1_a[!is.na(g1_a) & visit==2])
VIP_data_subset$g1_b_sd_norm[!is.na(g1_b) & visit==1]<-(g1_b[!is.na(g1_b) & visit==1]-mean(g1_b[!is.na(g1_b) & visit==1]))/sd(g1_b[!is.na(g1_b) & visit==1])
VIP_data_subset$g1_b_sd_norm[!is.na(g1_b) & visit==2]<-(g1_b[!is.na(g1_b) & visit==2]-mean(g1_b[!is.na(g1_b) & visit==2]))/sd(g1_b[!is.na(g1_b) & visit==2])
VIP_data_subset$g1_c_sd_norm[!is.na(g1_c) & visit==1]<-(g1_c[!is.na(g1_c) & visit==1]-mean(g1_c[!is.na(g1_c) & visit==1]))/sd(g1_c[!is.na(g1_c) & visit==1])
VIP_data_subset$g1_c_sd_norm[!is.na(g1_c) & visit==2]<-(g1_c[!is.na(g1_c) & visit==2]-mean(g1_c[!is.na(g1_c) & visit==2]))/sd(g1_c[!is.na(g1_c) & visit==2])
VIP_data_subset$g1_d_sd_norm[!is.na(g1_d) & visit==1]<-(g1_d[!is.na(g1_d) & visit==1]-mean(g1_d[!is.na(g1_d) & visit==1]))/sd(g1_d[!is.na(g1_d) & visit==1])
VIP_data_subset$g1_d_sd_norm[!is.na(g1_d) & visit==2]<-(g1_d[!is.na(g1_d) & visit==2]-mean(g1_d[!is.na(g1_d) & visit==2]))/sd(g1_d[!is.na(g1_d) & visit==2])
VIP_data_subset$g3_a_sd_norm[!is.na(g3_a) & visit==1]<-(g3_a[!is.na(g3_a) & visit==1]-mean(g3_a[!is.na(g3_a) & visit==1]))/sd(g3_a[!is.na(g3_a) & visit==1])
VIP_data_subset$g3_a_sd_norm[!is.na(g3_a) & visit==2]<-(g3_a[!is.na(g3_a) & visit==2]-mean(g3_a[!is.na(g3_a) & visit==2]))/sd(g3_a[!is.na(g3_a) & visit==2])
VIP_data_subset$g3_b_sd_norm[!is.na(g3_b) & visit==1]<-(g3_b[!is.na(g3_b) & visit==1]-mean(g3_b[!is.na(g3_b) & visit==1]))/sd(g3_b[!is.na(g3_b) & visit==1])
VIP_data_subset$g3_b_sd_norm[!is.na(g3_b) & visit==2]<-(g3_b[!is.na(g3_b) & visit==2]-mean(g3_b[!is.na(g3_b) & visit==2]))/sd(g3_b[!is.na(g3_b) & visit==2])
VIP_data_subset$g6_sd_norm[!is.na(g6) & visit==1]<-(g6[!is.na(g6) & visit==1]-mean(g6[!is.na(g6) & visit==1]))/sd(g6[!is.na(g6) & visit==1])
VIP_data_subset$g6_sd_norm[!is.na(g6) & visit==2]<-(g6[!is.na(g6) & visit==2]-mean(g6[!is.na(g6) & visit==2]))/sd(g6[!is.na(g6) & visit==2])

VIP_data_subset$g1_abcd<-g1_a
VIP_data_subset$g1_abcd[!is.na(g1_b)]<-VIP_data_subset$g1_abcd[!is.na(g1_b)]+g1_b[!is.na(g1_b)]
VIP_data_subset$g1_abcd[!is.na(g1_c)]<-VIP_data_subset$g1_abcd[!is.na(g1_c)]+g1_c[!is.na(g1_c)]
VIP_data_subset$g1_abcd[!is.na(g1_d)]<-VIP_data_subset$g1_abcd[!is.na(g1_d)]+g1_d[!is.na(g1_d)]

VIP_data_subset$g1_abcd_sd_norm[!is.na(VIP_data_subset$g1_abcd) & visit==1]<-(VIP_data_subset$g1_abcd[!is.na(VIP_data_subset$g1_abcd) & visit==1]-
			mean(VIP_data_subset$g1_abcd[!is.na(VIP_data_subset$g1_abcd) & visit==1]))/sd(VIP_data_subset$g1_abcd[!is.na(VIP_data_subset$g1_abcd) & visit==1])
VIP_data_subset$g1_abcd_sd_norm[!is.na(VIP_data_subset$g1_abcd) & visit==2]<-(VIP_data_subset$g1_abcd[!is.na(VIP_data_subset$g1_abcd) & visit==2]-
			mean(VIP_data_subset$g1_abcd[!is.na(VIP_data_subset$g1_abcd) & visit==2]))/sd(VIP_data_subset$g1_abcd[!is.na(VIP_data_subset$g1_abcd) & visit==2])

VIP_data_subset$g1_ad<-g1_a
VIP_data_subset$g1_ad[!is.na(g1_d)]<-VIP_data_subset$g1_ad[!is.na(g1_d)]+g1_d[!is.na(g1_d)]

VIP_data_subset$g1_ad_sd_norm[!is.na(VIP_data_subset$g1_ad) & visit==1]<-(VIP_data_subset$g1_ad[!is.na(VIP_data_subset$g1_ad) & visit==1]-
			mean(VIP_data_subset$g1_ad[!is.na(VIP_data_subset$g1_ad) & visit==1]))/sd(VIP_data_subset$g1_ad[!is.na(VIP_data_subset$g1_ad) & visit==1])
VIP_data_subset$g1_ad_sd_norm[!is.na(VIP_data_subset$g1_ad) & visit==2]<-(VIP_data_subset$g1_ad[!is.na(VIP_data_subset$g1_ad) & visit==2]-
			mean(VIP_data_subset$g1_ad[!is.na(VIP_data_subset$g1_ad) & visit==2]))/sd(VIP_data_subset$g1_ad[!is.na(VIP_data_subset$g1_ad) & visit==2])

detach(VIP_data_subset)
attach(VIP_data_subset)

#check association of PA with bmi

#visit 1

#unstandardized

#separate g1_N
associations<-glm(bmi_log_sd_norm[visit==1]~age_sd_norm[visit==1] + agesq_sd_norm[visit==1] + gender_factor[visit==1] + year_sd_norm[visit==1] + ffq_factor[visit==1]
				+ g1_a[visit==1] + g1_b[visit==1] + g1_c[visit==1] + g1_d[visit==1] + g3_a[visit==1] + g3_b[visit==1] + g6[visit==1] , family = gaussian(link = "identity"))

summary(associations)

#combining the g1_abcd variable
associations<-glm(bmi_log_sd_norm[visit==1]~age_sd_norm[visit==1] + agesq_sd_norm[visit==1] + gender_factor[visit==1] + year_sd_norm[visit==1] + ffq_factor[visit==1]
				+ g1_abcd[visit==1] + g3_a[visit==1] + g3_b[visit==1] + g6[visit==1] , family = gaussian(link = "identity"))

summary(associations)

#combining the g1_ad variable
associations<-glm(bmi_log_sd_norm[visit==1]~age_sd_norm[visit==1] + agesq_sd_norm[visit==1] + gender_factor[visit==1] + year_sd_norm[visit==1] + ffq_factor[visit==1]
				+ g1_ad[visit==1] + g3_a[visit==1] + g3_b[visit==1] + g6[visit==1] , family = gaussian(link = "identity"))

summary(associations)


#standardized

#separate g1_N
associations<-glm(bmi_log_sd_norm[visit==1]~age_sd_norm[visit==1] + agesq_sd_norm[visit==1] + gender_factor[visit==1] + year_sd_norm[visit==1] + ffq_factor[visit==1]
				+ g1_a_sd_norm[visit==1] + g1_b_sd_norm[visit==1] + g1_c_sd_norm[visit==1] + g1_d_sd_norm[visit==1] + g3_a_sd_norm[visit==1] + g3_b_sd_norm[visit==1]
				+ g6_sd_norm[visit==1] , family = gaussian(link = "identity"))

summary(associations)

#combining the g1_abcd variable
associations<-glm(bmi_log_sd_norm[visit==1]~age_sd_norm[visit==1] + agesq_sd_norm[visit==1] + gender_factor[visit==1] + year_sd_norm[visit==1] + ffq_factor[visit==1]
				+ g1_abcd_sd_norm[visit==1] + g3_a_sd_norm[visit==1] + g3_b_sd_norm[visit==1] + g6_sd_norm[visit==1] , family = gaussian(link = "identity"))

summary(associations)

#combining the g1_ad variable
associations<-glm(bmi_log_sd_norm[visit==1]~age_sd_norm[visit==1] + agesq_sd_norm[visit==1] + gender_factor[visit==1] + year_sd_norm[visit==1] + ffq_factor[visit==1]
				+ g1_ad_sd_norm[visit==1] + g3_a_sd_norm[visit==1] + g3_b_sd_norm[visit==1] + g6_sd_norm[visit==1] , family = gaussian(link = "identity"))

summary(associations)


#check variation explained for each, but adjusting for the basic variables, but not the rest of PA variables, since they have missing values in mixed places

all_variables<-c("g1_a","g1_b","g1_c","g1_d","g3_a","g3_b","g6","g1_abcd","g1_ad")

variable_count<-1
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi_log_sd_norm[(visit==1 & !is.na(bmi_log_sd_norm) & !is.na(VIP_data_subset[,c(variable)]))],VIP_data_subset[(visit==1 & !is.na(bmi_log_sd_norm) & !is.na(VIP_data_subset[,c(variable)])),c(variable)],
			VIP_data_subset[(visit==1 & !is.na(bmi_log_sd_norm) & !is.na(VIP_data_subset[,c(variable)])),c("age_sd_norm","agesq_sd_norm","year_sd_norm","ffq","gender")])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
	variable_count<-variable_count+1
	
}


#visit 2

#unstandardized

#separate g1_N
associations<-glm(bmi_log_sd_norm[visit==2]~age_sd_norm[visit==2] + agesq_sd_norm[visit==2] + gender_factor[visit==2] + year_sd_norm[visit==2] + ffq_factor[visit==2]
				+ g1_a[visit==2] + g1_b[visit==2] + g1_c[visit==2] + g1_d[visit==2] + g3_a[visit==2] + g3_b[visit==2] + g6[visit==2] , family = gaussian(link = "identity"))

summary(associations)

#combining the g1_abcd variable
associations<-glm(bmi_log_sd_norm[visit==2]~age_sd_norm[visit==2] + agesq_sd_norm[visit==2] + gender_factor[visit==2] + year_sd_norm[visit==2] + ffq_factor[visit==2]
				+ g1_abcd[visit==2] + g3_a[visit==2] + g3_b[visit==2] + g6[visit==2] , family = gaussian(link = "identity"))

summary(associations)

#combining the g1_ad variable
associations<-glm(bmi_log_sd_norm[visit==2]~age_sd_norm[visit==2] + agesq_sd_norm[visit==2] + gender_factor[visit==2] + year_sd_norm[visit==2] + ffq_factor[visit==2]
				+ g1_ad[visit==2] + g3_a[visit==2] + g3_b[visit==2] + g6[visit==2] , family = gaussian(link = "identity"))

summary(associations)


#standardized

#separate g1_N
associations<-glm(bmi_log_sd_norm[visit==2]~age_sd_norm[visit==2] + agesq_sd_norm[visit==2] + gender_factor[visit==2] + year_sd_norm[visit==2] + ffq_factor[visit==2]
				+ g1_a_sd_norm[visit==2] + g1_b_sd_norm[visit==2] + g1_c_sd_norm[visit==2] + g1_d_sd_norm[visit==2] + g3_a_sd_norm[visit==2] + g3_b_sd_norm[visit==2]
				+ g6_sd_norm[visit==2] , family = gaussian(link = "identity"))

summary(associations)

#combining the g1_abcd variable
associations<-glm(bmi_log_sd_norm[visit==2]~age_sd_norm[visit==2] + agesq_sd_norm[visit==2] + gender_factor[visit==2] + year_sd_norm[visit==2] + ffq_factor[visit==2]
				+ g1_abcd_sd_norm[visit==2] + g3_a_sd_norm[visit==2] + g3_b_sd_norm[visit==2] + g6_sd_norm[visit==2] , family = gaussian(link = "identity"))

summary(associations)

#combining the g1_ad variable
associations<-glm(bmi_log_sd_norm[visit==2]~age_sd_norm[visit==2] + agesq_sd_norm[visit==2] + gender_factor[visit==2] + year_sd_norm[visit==2] + ffq_factor[visit==2]
				+ g1_ad_sd_norm[visit==2] + g3_a_sd_norm[visit==2] + g3_b_sd_norm[visit==2] + g6_sd_norm[visit==2] , family = gaussian(link = "identity"))

summary(associations)


#check variation explained for each, but adjusting for the basic variables, but not the rest of PA variables, since they have missing values in mixed places

all_variables<-c("g1_a","g1_b","g1_c","g1_d","g3_a","g3_b","g6","g1_abcd","g1_ad")

variable_count<-1
for (variable in all_variables){
	
	partial_corr<-pcor.test(bmi_log_sd_norm[(visit==2 & !is.na(bmi_log_sd_norm) & !is.na(VIP_data_subset[,c(variable)]))],VIP_data_subset[(visit==2 & !is.na(bmi_log_sd_norm) & !is.na(VIP_data_subset[,c(variable)])),c(variable)],
			VIP_data_subset[(visit==2 & !is.na(bmi_log_sd_norm) & !is.na(VIP_data_subset[,c(variable)])),c("age_sd_norm","agesq_sd_norm","year_sd_norm","ffq","gender")])
	message(variable)
	message(round(partial_corr[[1]]*partial_corr[[1]],6))
	message(partial_corr[[2]])
	
	variable_count<-variable_count+1
	
}


#final score, best to keep only g1_a and g1_d combined and g6
VIP_data_subset$PA_score<-g1_a
VIP_data_subset$PA_score[!is.na(g1_d)]<-VIP_data_subset$PA_score[!is.na(g1_d)]+g1_d[!is.na(g1_d)]
VIP_data_subset$PA_score[!is.na(g6)]<-VIP_data_subset$PA_score[!is.na(g6)]+g6[!is.na(g6)]

VIP_data_subset$PA_score_sd_norm[!is.na(VIP_data_subset$PA_score) & visit==1]<-(VIP_data_subset$PA_score[!is.na(VIP_data_subset$PA_score) & visit==1]-
			mean(VIP_data_subset$PA_score[!is.na(VIP_data_subset$PA_score) & visit==1]))/sd(VIP_data_subset$PA_score[!is.na(VIP_data_subset$PA_score) & visit==1])
VIP_data_subset$PA_score_sd_norm[!is.na(VIP_data_subset$PA_score) & visit==2]<-(VIP_data_subset$PA_score[!is.na(VIP_data_subset$PA_score) & visit==2]-
			mean(VIP_data_subset$PA_score[!is.na(VIP_data_subset$PA_score) & visit==2]))/sd(VIP_data_subset$PA_score[!is.na(VIP_data_subset$PA_score) & visit==2])


detach(VIP_data_subset)
attach(VIP_data_subset)

#estimate the association of PA score with the bmi
#visit 1
associations<-glm(bmi_log_sd_norm[visit==1]~age_sd_norm[visit==1] + agesq_sd_norm[visit==1] + gender_factor[visit==1] + year_sd_norm[visit==1] + ffq_factor[visit==1] + 
				PA_score[visit==1], family = gaussian(link = "identity"))

summary(associations)

#visit 1 standardized
associations<-glm(bmi_log_sd_norm[visit==1]~age_sd_norm[visit==1] + agesq_sd_norm[visit==1] + gender_factor[visit==1] + year_sd_norm[visit==1] + ffq_factor[visit==1] + 
				PA_score_sd_norm[visit==1], family = gaussian(link = "identity"))

summary(associations)


#check variation explained

partial_corr<-pcor.test(bmi_log_sd_norm[(visit==1 & !is.na(bmi_log_sd_norm) & !is.na(PA_score))],PA_score[(visit==1 & !is.na(bmi_log_sd_norm) & !is.na(PA_score))],
		VIP_data_subset[(visit==1 & !is.na(bmi_log_sd_norm) & !is.na(PA_score)),c("age_sd_norm","agesq_sd_norm","year_sd_norm","ffq","gender")])

round(partial_corr[[1]]*partial_corr[[1]],6)
partial_corr[[2]]

#visit 2
associations<-glm(bmi_log_sd_norm[visit==2]~age_sd_norm[visit==2] + agesq_sd_norm[visit==2] + gender_factor[visit==2] + year_sd_norm[visit==2] + ffq_factor[visit==2] + 
				PA_score[visit==2], family = gaussian(link = "identity"))

summary(associations)

#visit 2 standardized
associations<-glm(bmi_log_sd_norm[visit==2]~age_sd_norm[visit==2] + agesq_sd_norm[visit==2] + gender_factor[visit==2] + year_sd_norm[visit==2] + ffq_factor[visit==2] + 
				PA_score_sd_norm[visit==2], family = gaussian(link = "identity"))

summary(associations)


#check variation explained

partial_corr<-pcor.test(bmi_log_sd_norm[(visit==2 & !is.na(bmi_log_sd_norm) & !is.na(PA_score))],PA_score[(visit==2 & !is.na(bmi_log_sd_norm) & !is.na(PA_score))],
		VIP_data_subset[(visit==2 & !is.na(bmi_log_sd_norm) & !is.na(PA_score)),c("age_sd_norm","agesq_sd_norm","year_sd_norm","ffq","gender")])

round(partial_corr[[1]]*partial_corr[[1]],6)
partial_corr[[2]]

#PA score details

#categories
sort(unique(PA_score))

#visit 1
length(PA_score[!is.na(PA_score) & visit==1 & PA_score==0])
length(PA_score[!is.na(PA_score) & visit==1 & PA_score==1])
length(PA_score[!is.na(PA_score) & visit==1 & PA_score==2])
length(PA_score[!is.na(PA_score) & visit==1 & PA_score==3])
length(PA_score[!is.na(PA_score) & visit==1 & PA_score==4])
length(PA_score[!is.na(PA_score) & visit==1 & PA_score==5])
length(PA_score[!is.na(PA_score) & visit==1 & PA_score==6])
length(PA_score[!is.na(PA_score) & visit==1 & PA_score==7])
length(PA_score[!is.na(PA_score) & visit==1 & PA_score==8])
length(PA_score[!is.na(PA_score) & visit==1 & PA_score==9])
length(PA_score[!is.na(PA_score) & visit==1 & PA_score==10])

#visit 2
length(PA_score[!is.na(PA_score) & visit==2 & PA_score==0])
length(PA_score[!is.na(PA_score) & visit==2 & PA_score==1])
length(PA_score[!is.na(PA_score) & visit==2 & PA_score==2])
length(PA_score[!is.na(PA_score) & visit==2 & PA_score==3])
length(PA_score[!is.na(PA_score) & visit==2 & PA_score==4])
length(PA_score[!is.na(PA_score) & visit==2 & PA_score==5])
length(PA_score[!is.na(PA_score) & visit==2 & PA_score==6])
length(PA_score[!is.na(PA_score) & visit==2 & PA_score==7])
length(PA_score[!is.na(PA_score) & visit==2 & PA_score==8])
length(PA_score[!is.na(PA_score) & visit==2 & PA_score==9])
length(PA_score[!is.na(PA_score) & visit==2 & PA_score==10])

#bmi
for (PA_category in 0:10){
	message(length(bmi[!is.na(PA_score) & visit==1 & PA_score==PA_category & !is.na(bmi) & bmi<18.5]))
	#message(length(bmi[!is.na(PA_score) & visit==1 & PA_score==PA_category & !is.na(bmi) & bmi>=18.5 & bmi<25]))
	#message(length(bmi[!is.na(PA_score) & visit==1 & PA_score==PA_category & !is.na(bmi) & bmi>=25 & bmi<30]))
	#message(length(bmi[!is.na(PA_score) & visit==1 & PA_score==PA_category & !is.na(bmi) & bmi>=30]))
}
#bmi
for (PA_category in 0:10){
	message(length(bmi[!is.na(PA_score) & visit==2 & PA_score==PA_category & !is.na(bmi) & bmi<18.5]))
	#message(length(bmi[!is.na(PA_score) & visit==2 & PA_score==PA_category & !is.na(bmi) & bmi>=18.5 & bmi<25]))
	#message(length(bmi[!is.na(PA_score) & visit==2 & PA_score==PA_category & !is.na(bmi) & bmi>=25 & bmi<30]))
	#message(length(bmi[!is.na(PA_score) & visit==2 & PA_score==PA_category & !is.na(bmi) & bmi>=30]))
}




#check other combinations as well, g1_a, g1_b, g1_c, g3_a, g3_b, g1_d and g6
VIP_data_subset$PA_score2<-g1_a
VIP_data_subset$PA_score2[!is.na(g1_d)]<-VIP_data_subset$PA_score2[!is.na(g1_d)]+g1_d[!is.na(g1_d)]
VIP_data_subset$PA_score2[!is.na(g1_b)]<-VIP_data_subset$PA_score2[!is.na(g1_b)]+g1_b[!is.na(g1_b)]
VIP_data_subset$PA_score2[!is.na(g1_c)]<-VIP_data_subset$PA_score2[!is.na(g1_c)]+g1_c[!is.na(g1_c)]
VIP_data_subset$PA_score2[!is.na(g6)]<-VIP_data_subset$PA_score2[!is.na(g6)]+g6[!is.na(g6)]
VIP_data_subset$PA_score2[!is.na(g3_a)]<-VIP_data_subset$PA_score2[!is.na(g3_a)]+g3_a[!is.na(g3_a)]
VIP_data_subset$PA_score2[!is.na(g3_b)]<-VIP_data_subset$PA_score2[!is.na(g3_b)]+g3_b[!is.na(g3_b)]

VIP_data_subset$PA_score2_sd_norm[!is.na(VIP_data_subset$PA_score2) & visit==1]<-(VIP_data_subset$PA_score2[!is.na(VIP_data_subset$PA_score2) & visit==1]-
			mean(VIP_data_subset$PA_score2[!is.na(VIP_data_subset$PA_score2) & visit==1]))/sd(VIP_data_subset$PA_score2[!is.na(VIP_data_subset$PA_score2) & visit==1])
VIP_data_subset$PA_score2_sd_norm[!is.na(VIP_data_subset$PA_score2) & visit==2]<-(VIP_data_subset$PA_score2[!is.na(VIP_data_subset$PA_score2) & visit==2]-
			mean(VIP_data_subset$PA_score2[!is.na(VIP_data_subset$PA_score2) & visit==2]))/sd(VIP_data_subset$PA_score2[!is.na(VIP_data_subset$PA_score2) & visit==2])


detach(VIP_data_subset)
attach(VIP_data_subset)

#estimate the association of PA score with the bmi
#visit 1
associations<-glm(bmi_log_sd_norm[visit==1]~age_sd_norm[visit==1] + agesq_sd_norm[visit==1] + gender_factor[visit==1] + year_sd_norm[visit==1] + ffq_factor[visit==1] + 
				PA_score2[visit==1], family = gaussian(link = "identity"))

summary(associations)

#visit 1 standardized
associations<-glm(bmi_log_sd_norm[visit==1]~age_sd_norm[visit==1] + agesq_sd_norm[visit==1] + gender_factor[visit==1] + year_sd_norm[visit==1] + ffq_factor[visit==1] + 
				PA_score2_sd_norm[visit==1], family = gaussian(link = "identity"))

summary(associations)


#check variation explained

partial_corr<-pcor.test(bmi_log_sd_norm[(visit==1 & !is.na(bmi_log_sd_norm) & !is.na(PA_score2))],PA_score2[(visit==1 & !is.na(bmi_log_sd_norm) & !is.na(PA_score2))],
		VIP_data_subset[(visit==1 & !is.na(bmi_log_sd_norm) & !is.na(PA_score2)),c("age_sd_norm","agesq_sd_norm","year_sd_norm","ffq","gender")])

round(partial_corr[[1]]*partial_corr[[1]],6)
partial_corr[[2]]

#visit 2
associations<-glm(bmi_log_sd_norm[visit==2]~age_sd_norm[visit==2] + agesq_sd_norm[visit==2] + gender_factor[visit==2] + year_sd_norm[visit==2] + ffq_factor[visit==2] + 
				PA_score2[visit==2], family = gaussian(link = "identity"))

summary(associations)

#visit 2 standardized
associations<-glm(bmi_log_sd_norm[visit==2]~age_sd_norm[visit==2] + agesq_sd_norm[visit==2] + gender_factor[visit==2] + year_sd_norm[visit==2] + ffq_factor[visit==2] + 
				PA_score2_sd_norm[visit==2], family = gaussian(link = "identity"))

summary(associations)


#check variation explained

partial_corr<-pcor.test(bmi_log_sd_norm[(visit==2 & !is.na(bmi_log_sd_norm) & !is.na(PA_score2))],PA_score2[(visit==2 & !is.na(bmi_log_sd_norm) & !is.na(PA_score2))],
		VIP_data_subset[(visit==2 & !is.na(bmi_log_sd_norm) & !is.na(PA_score2)),c("age_sd_norm","agesq_sd_norm","year_sd_norm","ffq","gender")])

round(partial_corr[[1]]*partial_corr[[1]],6)
partial_corr[[2]]


#check other combinations as well, g1_a, g1_b, g1_c, g1_d and g6
VIP_data_subset$PA_score3<-g1_a
VIP_data_subset$PA_score3[!is.na(g1_d)]<-VIP_data_subset$PA_score3[!is.na(g1_d)]+g1_d[!is.na(g1_d)]
VIP_data_subset$PA_score3[!is.na(g1_b)]<-VIP_data_subset$PA_score3[!is.na(g1_b)]+g1_b[!is.na(g1_b)]
VIP_data_subset$PA_score3[!is.na(g1_c)]<-VIP_data_subset$PA_score3[!is.na(g1_c)]+g1_c[!is.na(g1_c)]
VIP_data_subset$PA_score3[!is.na(g6)]<-VIP_data_subset$PA_score3[!is.na(g6)]+g6[!is.na(g6)]

VIP_data_subset$PA_score3_sd_norm[!is.na(VIP_data_subset$PA_score3) & visit==1]<-(VIP_data_subset$PA_score3[!is.na(VIP_data_subset$PA_score3) & visit==1]-
			mean(VIP_data_subset$PA_score3[!is.na(VIP_data_subset$PA_score3) & visit==1]))/sd(VIP_data_subset$PA_score3[!is.na(VIP_data_subset$PA_score3) & visit==1])
VIP_data_subset$PA_score3_sd_norm[!is.na(VIP_data_subset$PA_score3) & visit==2]<-(VIP_data_subset$PA_score3[!is.na(VIP_data_subset$PA_score3) & visit==2]-
			mean(VIP_data_subset$PA_score3[!is.na(VIP_data_subset$PA_score3) & visit==2]))/sd(VIP_data_subset$PA_score3[!is.na(VIP_data_subset$PA_score3) & visit==2])


detach(VIP_data_subset)
attach(VIP_data_subset)

#estimate the association of PA score with the bmi
#visit 1
associations<-glm(bmi_log_sd_norm[visit==1]~age_sd_norm[visit==1] + agesq_sd_norm[visit==1] + gender_factor[visit==1] + year_sd_norm[visit==1] + ffq_factor[visit==1] + 
				PA_score3[visit==1], family = gaussian(link = "identity"))

summary(associations)

#visit 1 standardized
associations<-glm(bmi_log_sd_norm[visit==1]~age_sd_norm[visit==1] + agesq_sd_norm[visit==1] + gender_factor[visit==1] + year_sd_norm[visit==1] + ffq_factor[visit==1] + 
				PA_score3_sd_norm[visit==1], family = gaussian(link = "identity"))

summary(associations)


#check variation explained

partial_corr<-pcor.test(bmi_log_sd_norm[(visit==1 & !is.na(bmi_log_sd_norm) & !is.na(PA_score3))],PA_score3[(visit==1 & !is.na(bmi_log_sd_norm) & !is.na(PA_score3))],
		VIP_data_subset[(visit==1 & !is.na(bmi_log_sd_norm) & !is.na(PA_score3)),c("age_sd_norm","agesq_sd_norm","year_sd_norm","ffq","gender")])

round(partial_corr[[1]]*partial_corr[[1]],6)
partial_corr[[2]]

#visit 2
associations<-glm(bmi_log_sd_norm[visit==2]~age_sd_norm[visit==2] + agesq_sd_norm[visit==2] + gender_factor[visit==2] + year_sd_norm[visit==2] + ffq_factor[visit==2] + 
				PA_score3[visit==2], family = gaussian(link = "identity"))

summary(associations)

#visit 2 standardized
associations<-glm(bmi_log_sd_norm[visit==2]~age_sd_norm[visit==2] + agesq_sd_norm[visit==2] + gender_factor[visit==2] + year_sd_norm[visit==2] + ffq_factor[visit==2] + 
				PA_score3_sd_norm[visit==2], family = gaussian(link = "identity"))

summary(associations)


#check variation explained

partial_corr<-pcor.test(bmi_log_sd_norm[(visit==2 & !is.na(bmi_log_sd_norm) & !is.na(PA_score3))],PA_score3[(visit==2 & !is.na(bmi_log_sd_norm) & !is.na(PA_score3))],
		VIP_data_subset[(visit==2 & !is.na(bmi_log_sd_norm) & !is.na(PA_score3)),c("age_sd_norm","agesq_sd_norm","year_sd_norm","ffq","gender")])

round(partial_corr[[1]]*partial_corr[[1]],6)
partial_corr[[2]]

#----------------------------------------------PA score----------------------------------------------------------

#----------------------------------------------COMBINING DIET AND PA SCORE---------------------------------------

#look at the associations with bmi for all the nutrients and PA variables
#run multiple regression for all the components
#associations<-glm(bmi_log_sd_norm[visit==1]~age_sd_norm[visit==1] + agesq_sd_norm[visit==1] + gender_factor[visit==1] + year_sd_norm[visit==1] + ffq_factor[visit==1] + PUFAE_sd_norm[visit==1] + 
#				MUFAE_sd_norm[visit==1] + SATFAT_sd_norm[visit==1] + TOTFAT_sd_norm[visit==1] + SUGAR_sd_norm[visit==1] + CARBOHYDRATES_sd_norm[visit==1] + TRANFAT_sd_norm[visit==1] + 
#				FA_sd_norm[visit==1] + PROTEIN_ANIMAL_sd_norm[visit==1] + CHOLESTEROL_sd_norm[visit==1]+ WHOLEGRAIN_sd_norm[visit==1] + SALT_sd_norm[visit==1]+ g1_ad_sd_norm[visit==1] + 
#				g6_sd_norm[visit==1] , family = gaussian(link = "identity"))
#
#summary(associations)


#combine just the final scores

#reverse the score for PA and standardize it
PA_score_copy<-PA_score
VIP_data_subset$PA_score[!is.na(PA_score) & PA_score_copy==0]<-10
VIP_data_subset$PA_score[!is.na(PA_score) & PA_score_copy==1]<-9
VIP_data_subset$PA_score[!is.na(PA_score) & PA_score_copy==2]<-8
VIP_data_subset$PA_score[!is.na(PA_score) & PA_score_copy==3]<-7
VIP_data_subset$PA_score[!is.na(PA_score) & PA_score_copy==4]<-6
VIP_data_subset$PA_score[!is.na(PA_score) & PA_score_copy==5]<-5
VIP_data_subset$PA_score[!is.na(PA_score) & PA_score_copy==6]<-4
VIP_data_subset$PA_score[!is.na(PA_score) & PA_score_copy==7]<-3
VIP_data_subset$PA_score[!is.na(PA_score) & PA_score_copy==8]<-2
VIP_data_subset$PA_score[!is.na(PA_score) & PA_score_copy==9]<-1
VIP_data_subset$PA_score[!is.na(PA_score) & PA_score_copy==10]<-0
VIP_data_subset$PA_score_sd_norm[!is.na(VIP_data_subset$PA_score) & visit==1]<-(VIP_data_subset$PA_score[!is.na(VIP_data_subset$PA_score) & visit==1]-
			mean(VIP_data_subset$PA_score[!is.na(VIP_data_subset$PA_score) & visit==1]))/sd(VIP_data_subset$PA_score[!is.na(VIP_data_subset$PA_score) & visit==1])
VIP_data_subset$PA_score_sd_norm[!is.na(VIP_data_subset$PA_score) & visit==2]<-(VIP_data_subset$PA_score[!is.na(VIP_data_subset$PA_score) & visit==2]-
			mean(VIP_data_subset$PA_score[!is.na(VIP_data_subset$PA_score) & visit==2]))/sd(VIP_data_subset$PA_score[!is.na(VIP_data_subset$PA_score) & visit==2])

detach(VIP_data_subset)
attach(VIP_data_subset)

##sum the diet and PA score by rescaling first
## diet score 1
#VIP_data_subset$diet_score1_PA<-diet_score1
##scale the PA score accordingly to diet score 1
#PA_score_temp1<-(PA_score+5)*(21/15)
#VIP_data_subset$diet_score1_PA[!is.na(PA_score_temp1)]<-VIP_data_subset$diet_score1_PA[!is.na(PA_score_temp1)]+PA_score_temp1[!is.na(PA_score_temp1)]
#VIP_data_subset$diet_score1_PA_sd_norm[!is.na(VIP_data_subset$diet_score1_PA) & visit==1]<-(VIP_data_subset$diet_score1_PA[!is.na(VIP_data_subset$diet_score1_PA) & visit==1]-
#			mean(VIP_data_subset$diet_score1_PA[!is.na(VIP_data_subset$diet_score1_PA) & visit==1]))/sd(VIP_data_subset$diet_score1_PA[!is.na(VIP_data_subset$diet_score1_PA) & visit==1])
#VIP_data_subset$diet_score1_PA_sd_norm[!is.na(VIP_data_subset$diet_score1_PA) & visit==2]<-(VIP_data_subset$diet_score1_PA[!is.na(VIP_data_subset$diet_score1_PA) & visit==2]-
#			mean(VIP_data_subset$diet_score1_PA[!is.na(VIP_data_subset$diet_score1_PA) & visit==2]))/sd(VIP_data_subset$diet_score1_PA[!is.na(VIP_data_subset$diet_score1_PA) & visit==2])
#
## diet score 2
##shift diet score 2 to have only positive scoring
#VIP_data_subset$diet_score2_PA<-diet_score2+(-1*min(diet_score2[!is.na(diet_score2)]))
##scale the PA score accordingly to diet score 1
#PA_score_temp2<-PA_score*(max(VIP_data_subset$diet_score2_PA[!is.na(VIP_data_subset$diet_score2_PA)])/10)
#VIP_data_subset$diet_score2_PA[!is.na(PA_score_temp2)]<-VIP_data_subset$diet_score2_PA[!is.na(PA_score_temp2)]+PA_score_temp2[!is.na(PA_score_temp2)]
#VIP_data_subset$diet_score2_PA_sd_norm[!is.na(VIP_data_subset$diet_score2_PA) & visit==1]<-(VIP_data_subset$diet_score2_PA[!is.na(VIP_data_subset$diet_score2_PA) & visit==1]-
#			mean(VIP_data_subset$diet_score2_PA[!is.na(VIP_data_subset$diet_score2_PA) & visit==1]))/sd(VIP_data_subset$diet_score2_PA[!is.na(VIP_data_subset$diet_score2_PA) & visit==1])
#VIP_data_subset$diet_score2_PA_sd_norm[!is.na(VIP_data_subset$diet_score2_PA) & visit==2]<-(VIP_data_subset$diet_score2_PA[!is.na(VIP_data_subset$diet_score2_PA) & visit==2]-
#			mean(VIP_data_subset$diet_score2_PA[!is.na(VIP_data_subset$diet_score2_PA) & visit==2]))/sd(VIP_data_subset$diet_score2_PA[!is.na(VIP_data_subset$diet_score2_PA) & visit==2])
#
## diet score 3
#VIP_data_subset$diet_score3_PA<-diet_score3_sd_norm
##scale the PA score accordingly to diet score 3
#PA_score_temp3<-PA_score+2
#VIP_data_subset$diet_score3_PA[!is.na(PA_score_temp3)]<-VIP_data_subset$diet_score3_PA[!is.na(PA_score_temp3)]+PA_score_temp3[!is.na(PA_score_temp3)]
#VIP_data_subset$diet_score3_PA_sd_norm[!is.na(VIP_data_subset$diet_score3_PA) & visit==1]<-(VIP_data_subset$diet_score3_PA[!is.na(VIP_data_subset$diet_score3_PA) & visit==1]-
#			mean(VIP_data_subset$diet_score3_PA[!is.na(VIP_data_subset$diet_score3_PA) & visit==1]))/sd(VIP_data_subset$diet_score3_PA[!is.na(VIP_data_subset$diet_score3_PA) & visit==1])
#VIP_data_subset$diet_score3_PA_sd_norm[!is.na(VIP_data_subset$diet_score3_PA) & visit==2]<-(VIP_data_subset$diet_score3_PA[!is.na(VIP_data_subset$diet_score3_PA) & visit==2]-
#			mean(VIP_data_subset$diet_score3_PA[!is.na(VIP_data_subset$diet_score3_PA) & visit==2]))/sd(VIP_data_subset$diet_score3_PA[!is.na(VIP_data_subset$diet_score3_PA) & visit==2])
#
#detach(VIP_data_subset)
#attach(VIP_data_subset)

#sum the standardized diet and scaled standardized PA score....best results for now
# diet score 1
VIP_data_subset$diet_score1_PA<-diet_score1_sd_norm
VIP_data_subset$diet_score1_PA[!is.na(PA_score_sd_norm)]<-VIP_data_subset$diet_score1_PA[!is.na(PA_score_sd_norm)]+PA_score_sd_norm[!is.na(PA_score_sd_norm)]*(max(diet_score1_sd_norm[!is.na(diet_score1_sd_norm)])/max(PA_score_sd_norm[!is.na(PA_score_sd_norm)]))
VIP_data_subset$diet_score1_PA_sd_norm[!is.na(VIP_data_subset$diet_score1_PA) & visit==1]<-(VIP_data_subset$diet_score1_PA[!is.na(VIP_data_subset$diet_score1_PA) & visit==1]-
			mean(VIP_data_subset$diet_score1_PA[!is.na(VIP_data_subset$diet_score1_PA) & visit==1]))/sd(VIP_data_subset$diet_score1_PA[!is.na(VIP_data_subset$diet_score1_PA) & visit==1])
VIP_data_subset$diet_score1_PA_sd_norm[!is.na(VIP_data_subset$diet_score1_PA) & visit==2]<-(VIP_data_subset$diet_score1_PA[!is.na(VIP_data_subset$diet_score1_PA) & visit==2]-
			mean(VIP_data_subset$diet_score1_PA[!is.na(VIP_data_subset$diet_score1_PA) & visit==2]))/sd(VIP_data_subset$diet_score1_PA[!is.na(VIP_data_subset$diet_score1_PA) & visit==2])

# diet score 2
#add a standardized rescaled diet score2 and a rescaled standardized PA score
VIP_data_subset$diet_score2_PA<-diet_score2_sd_norm
VIP_data_subset$diet_score2_PA[!is.na(PA_score_sd_norm)]<-VIP_data_subset$diet_score2_PA[!is.na(PA_score_sd_norm)]+PA_score_sd_norm[!is.na(PA_score_sd_norm)]*(max(diet_score2_sd_norm[!is.na(diet_score2_sd_norm)])/max(PA_score_sd_norm[!is.na(PA_score_sd_norm)]))
VIP_data_subset$diet_score2_PA_sd_norm[!is.na(VIP_data_subset$diet_score2_PA) & visit==1]<-(VIP_data_subset$diet_score2_PA[!is.na(VIP_data_subset$diet_score2_PA) & visit==1]-
			mean(VIP_data_subset$diet_score2_PA[!is.na(VIP_data_subset$diet_score2_PA) & visit==1]))/sd(VIP_data_subset$diet_score2_PA[!is.na(VIP_data_subset$diet_score2_PA) & visit==1])
VIP_data_subset$diet_score2_PA_sd_norm[!is.na(VIP_data_subset$diet_score2_PA) & visit==2]<-(VIP_data_subset$diet_score2_PA[!is.na(VIP_data_subset$diet_score2_PA) & visit==2]-
			mean(VIP_data_subset$diet_score2_PA[!is.na(VIP_data_subset$diet_score2_PA) & visit==2]))/sd(VIP_data_subset$diet_score2_PA[!is.na(VIP_data_subset$diet_score2_PA) & visit==2])

# diet score 3
VIP_data_subset$diet_score3_PA<-diet_score3_sd_norm
VIP_data_subset$diet_score3_PA[!is.na(PA_score_sd_norm)]<-VIP_data_subset$diet_score3_PA[!is.na(PA_score_sd_norm)]+PA_score_sd_norm[!is.na(PA_score_sd_norm)]*(max(diet_score3_sd_norm[!is.na(diet_score3_sd_norm)])/max(PA_score_sd_norm[!is.na(PA_score_sd_norm)]))
VIP_data_subset$diet_score3_PA_sd_norm[!is.na(VIP_data_subset$diet_score3_PA) & visit==1]<-(VIP_data_subset$diet_score3_PA[!is.na(VIP_data_subset$diet_score3_PA) & visit==1]-
			mean(VIP_data_subset$diet_score3_PA[!is.na(VIP_data_subset$diet_score3_PA) & visit==1]))/sd(VIP_data_subset$diet_score3_PA[!is.na(VIP_data_subset$diet_score3_PA) & visit==1])
VIP_data_subset$diet_score3_PA_sd_norm[!is.na(VIP_data_subset$diet_score3_PA) & visit==2]<-(VIP_data_subset$diet_score3_PA[!is.na(VIP_data_subset$diet_score3_PA) & visit==2]-
			mean(VIP_data_subset$diet_score3_PA[!is.na(VIP_data_subset$diet_score3_PA) & visit==2]))/sd(VIP_data_subset$diet_score3_PA[!is.na(VIP_data_subset$diet_score3_PA) & visit==2])

detach(VIP_data_subset)
attach(VIP_data_subset)

#visit 1

#diet score 1

#separate
associations<-glm(bmi_log_sd_norm[visit==1]~age_sd_norm[visit==1] + agesq_sd_norm[visit==1] + gender_factor[visit==1] + year_sd_norm[visit==1] + ffq_factor[visit==1] + 
				diet_score1_sd_norm[visit==1] + PA_score_sd_norm[visit==1], family = gaussian(link = "identity"))

summary(associations)

#summed
associations<-glm(bmi_log_sd_norm[visit==1]~age_sd_norm[visit==1] + agesq_sd_norm[visit==1] + gender_factor[visit==1] + year_sd_norm[visit==1] + ffq_factor[visit==1] + 
				diet_score1_PA_sd_norm[visit==1], family = gaussian(link = "identity"))

summary(associations)

partial_corr<-pcor.test(bmi_log_sd_norm[(visit==1 & !is.na(bmi_log_sd_norm) & !is.na(diet_score1_PA))],diet_score1_PA[(visit==1 & !is.na(bmi_log_sd_norm) & !is.na(diet_score1_PA))],
		VIP_data_subset[(visit==1 & !is.na(bmi_log_sd_norm) & !is.na(diet_score1_PA)),c("age_sd_norm","agesq_sd_norm","year_sd_norm","ffq","gender")])

round(partial_corr[[1]]*partial_corr[[1]],6)
partial_corr[[2]]



#diet score 2

#separate
associations<-glm(bmi_log_sd_norm[visit==1]~age_sd_norm[visit==1] + agesq_sd_norm[visit==1] + gender_factor[visit==1] + year_sd_norm[visit==1] + ffq_factor[visit==1] + 
				diet_score2_sd_norm[visit==1] + PA_score_sd_norm[visit==1], family = gaussian(link = "identity"))

summary(associations)

#summed
associations<-glm(bmi_log_sd_norm[visit==1]~age_sd_norm[visit==1] + agesq_sd_norm[visit==1] + gender_factor[visit==1] + year_sd_norm[visit==1] + ffq_factor[visit==1] + 
				diet_score2_PA_sd_norm[visit==1], family = gaussian(link = "identity"))

summary(associations)

partial_corr<-pcor.test(bmi_log_sd_norm[(visit==1 & !is.na(bmi_log_sd_norm) & !is.na(diet_score2_PA))],diet_score2_PA[(visit==1 & !is.na(bmi_log_sd_norm) & !is.na(diet_score2_PA))],
		VIP_data_subset[(visit==1 & !is.na(bmi_log_sd_norm) & !is.na(diet_score2_PA)),c("age_sd_norm","agesq_sd_norm","year_sd_norm","ffq","gender")])

round(partial_corr[[1]]*partial_corr[[1]],6)
partial_corr[[2]]



#diet score 3

#separate
associations<-glm(bmi_log_sd_norm[visit==1]~age_sd_norm[visit==1] + agesq_sd_norm[visit==1] + gender_factor[visit==1] + year_sd_norm[visit==1] + ffq_factor[visit==1] + 
				diet_score3_sd_norm[visit==1] + PA_score_sd_norm[visit==1], family = gaussian(link = "identity"))

summary(associations)

#summed
associations<-glm(bmi_log_sd_norm[visit==1]~age_sd_norm[visit==1] + agesq_sd_norm[visit==1] + gender_factor[visit==1] + year_sd_norm[visit==1] + ffq_factor[visit==1] + 
				diet_score3_PA_sd_norm[visit==1], family = gaussian(link = "identity"))

summary(associations)


partial_corr<-pcor.test(bmi_log_sd_norm[(visit==1 & !is.na(bmi_log_sd_norm) & !is.na(diet_score3_PA))],diet_score3_PA[(visit==1 & !is.na(bmi_log_sd_norm) & !is.na(diet_score3_PA))],
		VIP_data_subset[(visit==1 & !is.na(bmi_log_sd_norm) & !is.na(diet_score3_PA)),c("age_sd_norm","agesq_sd_norm","year_sd_norm","ffq","gender")])

round(partial_corr[[1]]*partial_corr[[1]],6)
partial_corr[[2]]



#visit 2

#diet score 1

#separate
associations<-glm(bmi_log_sd_norm[visit==2]~age_sd_norm[visit==2] + agesq_sd_norm[visit==2] + gender_factor[visit==2] + year_sd_norm[visit==2] + ffq_factor[visit==2] + 
				diet_score1_sd_norm[visit==2] + PA_score_sd_norm[visit==2], family = gaussian(link = "identity"))

summary(associations)

#summed
associations<-glm(bmi_log_sd_norm[visit==2]~age_sd_norm[visit==2] + agesq_sd_norm[visit==2] + gender_factor[visit==2] + year_sd_norm[visit==2] + ffq_factor[visit==2] + 
				diet_score1_PA_sd_norm[visit==2], family = gaussian(link = "identity"))

summary(associations)

partial_corr<-pcor.test(bmi_log_sd_norm[(visit==2 & !is.na(bmi_log_sd_norm) & !is.na(diet_score1_PA))],diet_score1_PA[(visit==2 & !is.na(bmi_log_sd_norm) & !is.na(diet_score1_PA))],
		VIP_data_subset[(visit==2 & !is.na(bmi_log_sd_norm) & !is.na(diet_score1_PA)),c("age_sd_norm","agesq_sd_norm","year_sd_norm","ffq","gender")])

round(partial_corr[[1]]*partial_corr[[1]],6)
partial_corr[[2]]



#diet score 2

#separate
associations<-glm(bmi_log_sd_norm[visit==2]~age_sd_norm[visit==2] + agesq_sd_norm[visit==2] + gender_factor[visit==2] + year_sd_norm[visit==2] + ffq_factor[visit==2] + 
				diet_score2_sd_norm[visit==2] + PA_score_sd_norm[visit==2], family = gaussian(link = "identity"))

summary(associations)

#summed
associations<-glm(bmi_log_sd_norm[visit==2]~age_sd_norm[visit==2] + agesq_sd_norm[visit==2] + gender_factor[visit==2] + year_sd_norm[visit==2] + ffq_factor[visit==2] + 
				diet_score2_PA_sd_norm[visit==2], family = gaussian(link = "identity"))

summary(associations)

partial_corr<-pcor.test(bmi_log_sd_norm[(visit==2 & !is.na(bmi_log_sd_norm) & !is.na(diet_score2_PA))],diet_score2_PA[(visit==2 & !is.na(bmi_log_sd_norm) & !is.na(diet_score2_PA))],
		VIP_data_subset[(visit==2 & !is.na(bmi_log_sd_norm) & !is.na(diet_score2_PA)),c("age_sd_norm","agesq_sd_norm","year_sd_norm","ffq","gender")])

round(partial_corr[[1]]*partial_corr[[1]],6)
partial_corr[[2]]



#diet score 3

#separate
associations<-glm(bmi_log_sd_norm[visit==2]~age_sd_norm[visit==2] + agesq_sd_norm[visit==2] + gender_factor[visit==2] + year_sd_norm[visit==2] + ffq_factor[visit==2] + 
				diet_score3_sd_norm[visit==2] + PA_score_sd_norm[visit==2], family = gaussian(link = "identity"))

summary(associations)

#summed
associations<-glm(bmi_log_sd_norm[visit==2]~age_sd_norm[visit==2] + agesq_sd_norm[visit==2] + gender_factor[visit==2] + year_sd_norm[visit==2] + ffq_factor[visit==2] + 
				diet_score3_PA_sd_norm[visit==2], family = gaussian(link = "identity"))

summary(associations)


partial_corr<-pcor.test(bmi_log_sd_norm[(visit==2 & !is.na(bmi_log_sd_norm) & !is.na(diet_score3_PA))],diet_score3_PA[(visit==2 & !is.na(bmi_log_sd_norm) & !is.na(diet_score3_PA))],
		VIP_data_subset[(visit==2 & !is.na(bmi_log_sd_norm) & !is.na(diet_score3_PA)),c("age_sd_norm","agesq_sd_norm","year_sd_norm","ffq","gender")])

round(partial_corr[[1]]*partial_corr[[1]],6)
partial_corr[[2]]


