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

#make a few other bmi variables, that are more cleaned from extreme values / possible outliers, MAD based here

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
VIP_data_independant$bmi_norm_sd[!is.na(bmi)]<-(VIP_data_independant$bmi_norm[!is.na(bmi)] - mean(VIP_data_independant$bmi_norm[!is.na(bmi)]))/(sd(VIP_data_independant$bmi_norm[!is.na(bmi)]))
#png("Results/distributions/log_sd_bmi_distribution.png")
#qqnorm(VIP_data_independant$bmi_norm_sd)
#qqline(VIP_data_independant$bmi_norm_sd)
#dev.off()
#png("Results/distributions/log_sd_bmi_histogram.png")
#hist(VIP_data_independant$bmi_norm_sd)
#dev.off()


#take 3MAD for cutpoint
VIP_data_independant$bmi_3MAD<-bmi
length(VIP_data_independant$bmi_3MAD[!is.na(bmi) & (bmi <= (median(bmi[!is.na(bmi)])-3*mad(bmi[!is.na(bmi)])) | bmi >= (median(bmi[!is.na(bmi)])+3*mad(bmi[!is.na(bmi)])))])#1067
VIP_data_independant$bmi_3MAD[!is.na(bmi) & (bmi <= (median(bmi[!is.na(bmi)])-3*mad(bmi[!is.na(bmi)])) | bmi >= (median(bmi[!is.na(bmi)])+3*mad(bmi[!is.na(bmi)])))]<-NA
VIP_data_independant$bmi_3MAD<-log(VIP_data_independant$bmi_3MAD)
VIP_data_independant$bmi_3MAD_norm_sd[!is.na(VIP_data_independant$bmi_3MAD)]<-(VIP_data_independant$bmi_3MAD[!is.na(VIP_data_independant$bmi_3MAD)] - mean(VIP_data_independant$bmi_3MAD[!is.na(VIP_data_independant$bmi_3MAD)]))/(sd(VIP_data_independant$bmi_3MAD[!is.na(VIP_data_independant$bmi_3MAD)]))

png("Results/distributions/filtered_3MAD_bmi_distribution.png")
qqnorm(VIP_data_independant$bmi_3MAD_norm_sd)
qqline(VIP_data_independant$bmi_3MAD_norm_sd)
dev.off()
png("Results/distributions/filtered_3MAD_bmi_histogram.png")
hist(VIP_data_independant$bmi_3MAD_norm_sd)
dev.off()

#create several new "transformed" variables, either log transformed and standaridized or categorized based on the recommendations

# POLYsum1 is original, POLYsum1_norm_sd is original log tranformed and standardized, POLYsum1_3MAD_norm_sd cleaned from extremes and log tranformed and standardized, 
# POLYsum1_ofTEI_norm_sd is expressed in % of total energy intake and log transformed and standardized,POLYsum1_ofTEI_categorized is expressed in % of TEI and categorized 
# based on guidelines, POLYsum1_ofTEI_3MAD_norm_sd is cleaned from extremes and expressed in % of total energy intake and log transformed and standardized,
# POLYsum1_ofTEI_3MAD_categorized is cleaned from extremes and expressed in % of TEI and categorized based on guidelines


#original
#continuous log transformed and standardized
VIP_data_independant$POLYsum1_norm_sd<-log(POLYsum1)
VIP_data_independant$POLYsum1_norm_sd[!is.na(POLYsum1)]<-(VIP_data_independant$POLYsum1_norm_sd[!is.na(POLYsum1)]-
			mean(VIP_data_independant$POLYsum1_norm_sd[!is.na(POLYsum1)]))/sd(VIP_data_independant$POLYsum1_norm_sd[!is.na(POLYsum1)])

# % of TEI
VIP_data_independant$POLYsum1_ofTEI<-(100*9*VIP_data_independant$POLYsum1)/ensum1

# % of TEI log transformed and standardized
VIP_data_independant$POLYsum1_ofTEI_norm_sd<-log(VIP_data_independant$POLYsum1_ofTEI)		
VIP_data_independant$POLYsum1_ofTEI_norm_sd[!is.na(VIP_data_independant$POLYsum1_ofTEI_norm_sd)]<-(VIP_data_independant$POLYsum1_ofTEI_norm_sd[!is.na(VIP_data_independant$POLYsum1_ofTEI_norm_sd)]-
			mean(VIP_data_independant$POLYsum1_ofTEI_norm_sd[!is.na(VIP_data_independant$POLYsum1_ofTEI_norm_sd)]))/sd(VIP_data_independant$POLYsum1_ofTEI_norm_sd[!is.na(VIP_data_independant$POLYsum1_ofTEI_norm_sd)])

# categorizing the % of TEI based on guidelines
VIP_data_independant$POLYsum1_ofTEI_categorized_g[!is.na(POLYsum1)]<-0
VIP_data_independant$POLYsum1_ofTEI_categorized_g[!is.na(POLYsum1) & VIP_data_independant$POLYsum1_ofTEI >= 5 & VIP_data_independant$POLYsum1_ofTEI <= 10 ]<-1
VIP_data_independant$POLYsum1_ofTEI_categorized_g[!is.na(POLYsum1) & VIP_data_independant$POLYsum1_ofTEI > 10]<-2



#filtered
VIP_data_independant$POLYsum1_3MAD <- POLYsum1
length(VIP_data_independant$POLYsum1[!is.na(POLYsum1) & (POLYsum1 <= (median(POLYsum1[!is.na(POLYsum1)])-3*mad(POLYsum1[!is.na(POLYsum1)])) | POLYsum1 >= (median(POLYsum1[!is.na(POLYsum1)])+
								3*mad(POLYsum1[!is.na(POLYsum1)])))])
VIP_data_independant$POLYsum1_3MAD[!is.na(POLYsum1) & (POLYsum1 <= (median(POLYsum1[!is.na(POLYsum1)])-3*mad(POLYsum1[!is.na(POLYsum1)])) | POLYsum1 >= (median(POLYsum1[!is.na(POLYsum1)])+
						3*mad(POLYsum1[!is.na(POLYsum1)])))]<-NA

# % of TEI
VIP_data_independant$POLYsum1_ofTEI_3MAD<-(100*9*VIP_data_independant$POLYsum1_3MAD)/ensum1

#log and standardize filtered
VIP_data_independant$POLYsum1_3MAD_norm_sd<-log(VIP_data_independant$POLYsum1_3MAD)
VIP_data_independant$POLYsum1_3MAD_norm_sd[!is.na(VIP_data_independant$POLYsum1_3MAD_norm_sd)]<-(VIP_data_independant$POLYsum1_3MAD_norm_sd[!is.na(VIP_data_independant$POLYsum1_3MAD_norm_sd)] - 
			mean(VIP_data_independant$POLYsum1_3MAD_norm_sd[!is.na(VIP_data_independant$POLYsum1_3MAD_norm_sd)]))/(sd(VIP_data_independant$POLYsum1_3MAD_norm_sd[!is.na(VIP_data_independant$POLYsum1_3MAD_norm_sd)]))

# % of TEI log transformed and standardized
VIP_data_independant$POLYsum1_ofTEI_3MAD_norm_sd<-log(VIP_data_independant$POLYsum1_ofTEI_3MAD)		
VIP_data_independant$POLYsum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$POLYsum1_ofTEI_3MAD_norm_sd)]<-(VIP_data_independant$POLYsum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$POLYsum1_ofTEI_3MAD_norm_sd)]-
			mean(VIP_data_independant$POLYsum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$POLYsum1_ofTEI_3MAD_norm_sd)]))/sd(VIP_data_independant$POLYsum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$POLYsum1_ofTEI_3MAD_norm_sd)])

# categorizing the % of TEI based on guidelines
VIP_data_independant$POLYsum1_ofTEI_3MAD_categorized_g[!is.na(VIP_data_independant$POLYsum1_3MAD)]<-0
VIP_data_independant$POLYsum1_ofTEI_3MAD_categorized_g[!is.na(VIP_data_independant$POLYsum1_3MAD) & VIP_data_independant$POLYsum1_ofTEI_3MAD >= 5 & VIP_data_independant$POLYsum1_ofTEI_3MAD <= 10 ]<-1
VIP_data_independant$POLYsum1_ofTEI_3MAD_categorized_g[!is.na(VIP_data_independant$POLYsum1_3MAD) & VIP_data_independant$POLYsum1_ofTEI_3MAD > 10]<-2




# MONOsum1 is original, MONOsum1_norm_sd is original log tranformed and standardized, MONOsum1_3MAD_norm_sd cleaned from extremes and log tranformed and standardized, 
# MONOsum1_ofTEI_norm_sd is expressed in % of total energy intake and log transformed and standardized,MONOsum1_ofTEI_categorized is expressed in % of TEI and categorized 
# based on guidelines, MONOsum1_ofTEI_3MAD_norm_sd is cleaned from extremes and expressed in % of total energy intake and log transformed and standardized,
# MONOsum1_ofTEI_3MAD_categorized is cleaned from extremes and expressed in % of TEI and categorized based on guidelines


#original
#continuous log transformed and standardized
VIP_data_independant$MONOsum1_norm_sd<-log(MONOsum1)
VIP_data_independant$MONOsum1_norm_sd[!is.na(MONOsum1)]<-(VIP_data_independant$MONOsum1_norm_sd[!is.na(MONOsum1)]-
			mean(VIP_data_independant$MONOsum1_norm_sd[!is.na(MONOsum1)]))/sd(VIP_data_independant$MONOsum1_norm_sd[!is.na(MONOsum1)])

# % of TEI
VIP_data_independant$MONOsum1_ofTEI<-(100*9*VIP_data_independant$MONOsum1)/ensum1

# % of TEI log transformed and standardized
VIP_data_independant$MONOsum1_ofTEI_norm_sd<-log(VIP_data_independant$MONOsum1_ofTEI)		
VIP_data_independant$MONOsum1_ofTEI_norm_sd[!is.na(VIP_data_independant$MONOsum1_ofTEI_norm_sd)]<-(VIP_data_independant$MONOsum1_ofTEI_norm_sd[!is.na(VIP_data_independant$MONOsum1_ofTEI_norm_sd)]-
			mean(VIP_data_independant$MONOsum1_ofTEI_norm_sd[!is.na(VIP_data_independant$MONOsum1_ofTEI_norm_sd)]))/sd(VIP_data_independant$MONOsum1_ofTEI_norm_sd[!is.na(VIP_data_independant$MONOsum1_ofTEI_norm_sd)])

# categorizing the % of TEI based on guidelines
VIP_data_independant$MONOsum1_ofTEI_categorized_g[!is.na(MONOsum1)]<-0
VIP_data_independant$MONOsum1_ofTEI_categorized_g[!is.na(MONOsum1) & VIP_data_independant$MONOsum1_ofTEI >= 10 & VIP_data_independant$MONOsum1_ofTEI <= 20 ]<-1
VIP_data_independant$MONOsum1_ofTEI_categorized_g[!is.na(MONOsum1) & VIP_data_independant$MONOsum1_ofTEI > 20]<-2



#filtered
VIP_data_independant$MONOsum1_3MAD <- MONOsum1
length(VIP_data_independant$MONOsum1[!is.na(MONOsum1) & (MONOsum1 <= (median(MONOsum1[!is.na(MONOsum1)])-3*mad(MONOsum1[!is.na(MONOsum1)])) | MONOsum1 >= (median(MONOsum1[!is.na(MONOsum1)])+
								3*mad(MONOsum1[!is.na(MONOsum1)])))])
VIP_data_independant$MONOsum1_3MAD[!is.na(MONOsum1) & (MONOsum1 <= (median(MONOsum1[!is.na(MONOsum1)])-3*mad(MONOsum1[!is.na(MONOsum1)])) | MONOsum1 >= (median(MONOsum1[!is.na(MONOsum1)])+
						3*mad(MONOsum1[!is.na(MONOsum1)])))]<-NA

# % of TEI
VIP_data_independant$MONOsum1_ofTEI_3MAD<-(100*9*VIP_data_independant$MONOsum1_3MAD)/ensum1

#log and standardize filtered
VIP_data_independant$MONOsum1_3MAD_norm_sd<-log(VIP_data_independant$MONOsum1_3MAD)
VIP_data_independant$MONOsum1_3MAD_norm_sd[!is.na(VIP_data_independant$MONOsum1_3MAD_norm_sd)]<-(VIP_data_independant$MONOsum1_3MAD_norm_sd[!is.na(VIP_data_independant$MONOsum1_3MAD_norm_sd)] - 
			mean(VIP_data_independant$MONOsum1_3MAD_norm_sd[!is.na(VIP_data_independant$MONOsum1_3MAD_norm_sd)]))/(sd(VIP_data_independant$MONOsum1_3MAD_norm_sd[!is.na(VIP_data_independant$MONOsum1_3MAD_norm_sd)]))

# % of TEI log transformed and standardized
VIP_data_independant$MONOsum1_ofTEI_3MAD_norm_sd<-log(VIP_data_independant$MONOsum1_ofTEI_3MAD)		
VIP_data_independant$MONOsum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$MONOsum1_ofTEI_3MAD_norm_sd)]<-(VIP_data_independant$MONOsum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$MONOsum1_ofTEI_3MAD_norm_sd)]-
			mean(VIP_data_independant$MONOsum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$MONOsum1_ofTEI_3MAD_norm_sd)]))/sd(VIP_data_independant$MONOsum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$MONOsum1_ofTEI_3MAD_norm_sd)])

# categorizing the % of TEI based on guidelines
VIP_data_independant$MONOsum1_ofTEI_3MAD_categorized_g[!is.na(VIP_data_independant$MONOsum1_3MAD)]<-0
VIP_data_independant$MONOsum1_ofTEI_3MAD_categorized_g[!is.na(VIP_data_independant$MONOsum1_3MAD) & VIP_data_independant$MONOsum1_ofTEI_3MAD >= 10 & VIP_data_independant$MONOsum1_ofTEI_3MAD <= 20 ]<-1
VIP_data_independant$MONOsum1_ofTEI_3MAD_categorized_g[!is.na(VIP_data_independant$MONOsum1_3MAD) & VIP_data_independant$MONOsum1_ofTEI_3MAD > 20]<-2



# mfetsum1 is original, mfetsum1_norm_sd is original log tranformed and standardized, mfetsum1_3MAD_norm_sd cleaned from extremes and log tranformed and standardized, 
# mfetsum1_ofTEI_norm_sd is expressed in % of total energy intake and log transformed and standardized,mfetsum1_ofTEI_categorized is expressed in % of TEI and categorized 
# based on guidelines, mfetsum1_ofTEI_3MAD_norm_sd is cleaned from extremes and expressed in % of total energy intake and log transformed and standardized,
# mfetsum1_ofTEI_3MAD_categorized is cleaned from extremes and expressed in % of TEI and categorized based on guidelines


#original
#continuous log transformed and standardized
VIP_data_independant$mfetsum1_norm_sd<-log(mfetsum1)
VIP_data_independant$mfetsum1_norm_sd[!is.na(mfetsum1)]<-(VIP_data_independant$mfetsum1_norm_sd[!is.na(mfetsum1)]-
			mean(VIP_data_independant$mfetsum1_norm_sd[!is.na(mfetsum1)]))/sd(VIP_data_independant$mfetsum1_norm_sd[!is.na(mfetsum1)])

# % of TEI
VIP_data_independant$mfetsum1_ofTEI<-(100*9*VIP_data_independant$mfetsum1)/ensum1

# % of TEI log transformed and standardized
VIP_data_independant$mfetsum1_ofTEI_norm_sd<-log(VIP_data_independant$mfetsum1_ofTEI)		
VIP_data_independant$mfetsum1_ofTEI_norm_sd[!is.na(VIP_data_independant$mfetsum1_ofTEI_norm_sd)]<-(VIP_data_independant$mfetsum1_ofTEI_norm_sd[!is.na(VIP_data_independant$mfetsum1_ofTEI_norm_sd)]-
			mean(VIP_data_independant$mfetsum1_ofTEI_norm_sd[!is.na(VIP_data_independant$mfetsum1_ofTEI_norm_sd)]))/sd(VIP_data_independant$mfetsum1_ofTEI_norm_sd[!is.na(VIP_data_independant$mfetsum1_ofTEI_norm_sd)])

# categorizing the % of TEI based on guidelines
VIP_data_independant$mfetsum1_ofTEI_categorized_g[!is.na(mfetsum1)]<-2
VIP_data_independant$mfetsum1_ofTEI_categorized_g[!is.na(mfetsum1) & VIP_data_independant$mfetsum1_ofTEI <= 10 ]<-1



#filtered
VIP_data_independant$mfetsum1_3MAD <- mfetsum1
length(VIP_data_independant$mfetsum1[!is.na(mfetsum1) & (mfetsum1 <= (median(mfetsum1[!is.na(mfetsum1)])-3*mad(mfetsum1[!is.na(mfetsum1)])) | mfetsum1 >= (median(mfetsum1[!is.na(mfetsum1)])+
								3*mad(mfetsum1[!is.na(mfetsum1)])))])
VIP_data_independant$mfetsum1_3MAD[!is.na(mfetsum1) & (mfetsum1 <= (median(mfetsum1[!is.na(mfetsum1)])-3*mad(mfetsum1[!is.na(mfetsum1)])) | mfetsum1 >= (median(mfetsum1[!is.na(mfetsum1)])+
						3*mad(mfetsum1[!is.na(mfetsum1)])))]<-NA

# % of TEI
VIP_data_independant$mfetsum1_ofTEI_3MAD<-(100*9*VIP_data_independant$mfetsum1_3MAD)/ensum1

#log and standardize filtered
VIP_data_independant$mfetsum1_3MAD_norm_sd<-log(VIP_data_independant$mfetsum1_3MAD)
VIP_data_independant$mfetsum1_3MAD_norm_sd[!is.na(VIP_data_independant$mfetsum1_3MAD_norm_sd)]<-(VIP_data_independant$mfetsum1_3MAD_norm_sd[!is.na(VIP_data_independant$mfetsum1_3MAD_norm_sd)] - 
			mean(VIP_data_independant$mfetsum1_3MAD_norm_sd[!is.na(VIP_data_independant$mfetsum1_3MAD_norm_sd)]))/(sd(VIP_data_independant$mfetsum1_3MAD_norm_sd[!is.na(VIP_data_independant$mfetsum1_3MAD_norm_sd)]))

# % of TEI log transformed and standardized
VIP_data_independant$mfetsum1_ofTEI_3MAD_norm_sd<-log(VIP_data_independant$mfetsum1_ofTEI_3MAD)		
VIP_data_independant$mfetsum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$mfetsum1_ofTEI_3MAD_norm_sd)]<-(VIP_data_independant$mfetsum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$mfetsum1_ofTEI_3MAD_norm_sd)]-
			mean(VIP_data_independant$mfetsum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$mfetsum1_ofTEI_3MAD_norm_sd)]))/sd(VIP_data_independant$mfetsum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$mfetsum1_ofTEI_3MAD_norm_sd)])

# categorizing the % of TEI based on guidelines
VIP_data_independant$mfetsum1_ofTEI_3MAD_categorized_g[!is.na(VIP_data_independant$mfetsum1_3MAD)]<-2
VIP_data_independant$mfetsum1_ofTEI_3MAD_categorized_g[!is.na(VIP_data_independant$mfetsum1_3MAD) & VIP_data_independant$mfetsum1_ofTEI_3MAD <= 10 ]<-1





# fettsum1 is original, fettsum1_norm_sd is original log tranformed and standardized, fettsum1_3MAD_norm_sd cleaned from extremes and log tranformed and standardized, 
# fettsum1_ofTEI_norm_sd is expressed in % of total energy intake and log transformed and standardized,fettsum1_ofTEI_categorized is expressed in % of TEI and categorized 
# based on guidelines, fettsum1_ofTEI_3MAD_norm_sd is cleaned from extremes and expressed in % of total energy intake and log transformed and standardized,
# fettsum1_ofTEI_3MAD_categorized is cleaned from extremes and expressed in % of TEI and categorized based on guidelines


#original
#continuous log transformed and standardized
VIP_data_independant$fettsum1_norm_sd<-log(fettsum1)
VIP_data_independant$fettsum1_norm_sd[!is.na(fettsum1)]<-(VIP_data_independant$fettsum1_norm_sd[!is.na(fettsum1)]-
			mean(VIP_data_independant$fettsum1_norm_sd[!is.na(fettsum1)]))/sd(VIP_data_independant$fettsum1_norm_sd[!is.na(fettsum1)])

# % of TEI
VIP_data_independant$fettsum1_ofTEI<-(100*9*VIP_data_independant$fettsum1)/ensum1

# % of TEI log transformed and standardized
VIP_data_independant$fettsum1_ofTEI_norm_sd<-log(VIP_data_independant$fettsum1_ofTEI)		
VIP_data_independant$fettsum1_ofTEI_norm_sd[!is.na(VIP_data_independant$fettsum1_ofTEI_norm_sd)]<-(VIP_data_independant$fettsum1_ofTEI_norm_sd[!is.na(VIP_data_independant$fettsum1_ofTEI_norm_sd)]-
			mean(VIP_data_independant$fettsum1_ofTEI_norm_sd[!is.na(VIP_data_independant$fettsum1_ofTEI_norm_sd)]))/sd(VIP_data_independant$fettsum1_ofTEI_norm_sd[!is.na(VIP_data_independant$fettsum1_ofTEI_norm_sd)])

# categorizing the % of TEI based on guidelines
VIP_data_independant$fettsum1_ofTEI_categorized_g[!is.na(fettsum1)]<-0
VIP_data_independant$fettsum1_ofTEI_categorized_g[!is.na(fettsum1) & VIP_data_independant$fettsum1_ofTEI >= 25 & VIP_data_independant$fettsum1_ofTEI <= 40 ]<-1
VIP_data_independant$fettsum1_ofTEI_categorized_g[!is.na(fettsum1) & VIP_data_independant$fettsum1_ofTEI > 40]<-2



#filtered
VIP_data_independant$fettsum1_3MAD <- fettsum1
length(VIP_data_independant$fettsum1[!is.na(fettsum1) & (fettsum1 <= (median(fettsum1[!is.na(fettsum1)])-3*mad(fettsum1[!is.na(fettsum1)])) | fettsum1 >= (median(fettsum1[!is.na(fettsum1)])+
								3*mad(fettsum1[!is.na(fettsum1)])))])
VIP_data_independant$fettsum1_3MAD[!is.na(fettsum1) & (fettsum1 <= (median(fettsum1[!is.na(fettsum1)])-3*mad(fettsum1[!is.na(fettsum1)])) | fettsum1 >= (median(fettsum1[!is.na(fettsum1)])+
						3*mad(fettsum1[!is.na(fettsum1)])))]<-NA

# % of TEI
VIP_data_independant$fettsum1_ofTEI_3MAD<-(100*9*VIP_data_independant$fettsum1_3MAD)/ensum1

#log and standardize filtered
VIP_data_independant$fettsum1_3MAD_norm_sd<-log(VIP_data_independant$fettsum1_3MAD)
VIP_data_independant$fettsum1_3MAD_norm_sd[!is.na(VIP_data_independant$fettsum1_3MAD_norm_sd)]<-(VIP_data_independant$fettsum1_3MAD_norm_sd[!is.na(VIP_data_independant$fettsum1_3MAD_norm_sd)] - 
			mean(VIP_data_independant$fettsum1_3MAD_norm_sd[!is.na(VIP_data_independant$fettsum1_3MAD_norm_sd)]))/(sd(VIP_data_independant$fettsum1_3MAD_norm_sd[!is.na(VIP_data_independant$fettsum1_3MAD_norm_sd)]))

# % of TEI log transformed and standardized
VIP_data_independant$fettsum1_ofTEI_3MAD_norm_sd<-log(VIP_data_independant$fettsum1_ofTEI_3MAD)		
VIP_data_independant$fettsum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$fettsum1_ofTEI_3MAD_norm_sd)]<-(VIP_data_independant$fettsum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$fettsum1_ofTEI_3MAD_norm_sd)]-
			mean(VIP_data_independant$fettsum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$fettsum1_ofTEI_3MAD_norm_sd)]))/sd(VIP_data_independant$fettsum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$fettsum1_ofTEI_3MAD_norm_sd)])

# categorizing the % of TEI based on guidelines
VIP_data_independant$fettsum1_ofTEI_3MAD_categorized_g[!is.na(VIP_data_independant$fettsum1_3MAD)]<-0
VIP_data_independant$fettsum1_ofTEI_3MAD_categorized_g[!is.na(VIP_data_independant$fettsum1_3MAD) & VIP_data_independant$fettsum1_ofTEI_3MAD >= 25 & VIP_data_independant$fettsum1_ofTEI_3MAD <= 25 ]<-1
VIP_data_independant$fettsum1_ofTEI_3MAD_categorized_g[!is.na(VIP_data_independant$fettsum1_3MAD) & VIP_data_independant$fettsum1_ofTEI_3MAD > 40]<-2




# FA is original, FA_norm_sd is original log tranformed and standardized, FA_3MAD_norm_sd cleaned from extremes and log tranformed and standardized, 
# FA_ofTEI_norm_sd is expressed in % of total energy intake and log transformed and standardized,FA_ofTEI_categorized is expressed in % of TEI and categorized 
# based on guidelines, FA_ofTEI_3MAD_norm_sd is cleaned from extremes and expressed in % of total energy intake and log transformed and standardized,
# FA_ofTEI_3MAD_categorized is cleaned from extremes and expressed in % of TEI and categorized based on guidelines

#acids are a combination of several variables
VIP_data_independant$FA<-FA183_sum1 + FA205_sum1 + FA226_sum1 + FA182_sum1 + FA204_sum1
detach(VIP_data_independant)
attach(VIP_data_independant)

#original
#continuous log transformed and standardized
VIP_data_independant$FA_norm_sd<-log(FA)
VIP_data_independant$FA_norm_sd[!is.na(FA)]<-(VIP_data_independant$FA_norm_sd[!is.na(FA)]-
			mean(VIP_data_independant$FA_norm_sd[!is.na(FA)]))/sd(VIP_data_independant$FA_norm_sd[!is.na(FA)])

# % of TEI
VIP_data_independant$FA_ofTEI<-(100*9*VIP_data_independant$FA)/ensum1

# % of TEI log transformed and standardized
VIP_data_independant$FA_ofTEI_norm_sd<-log(VIP_data_independant$FA_ofTEI)		
VIP_data_independant$FA_ofTEI_norm_sd[!is.na(VIP_data_independant$FA_ofTEI_norm_sd)]<-(VIP_data_independant$FA_ofTEI_norm_sd[!is.na(VIP_data_independant$FA_ofTEI_norm_sd)]-
			mean(VIP_data_independant$FA_ofTEI_norm_sd[!is.na(VIP_data_independant$FA_ofTEI_norm_sd)]))/sd(VIP_data_independant$FA_ofTEI_norm_sd[!is.na(VIP_data_independant$FA_ofTEI_norm_sd)])

# categorizing the % of TEI based on guidelines
VIP_data_independant$FA_ofTEI_categorized_g[!is.na(FA)]<-1
VIP_data_independant$FA_ofTEI_categorized_g[!is.na(FA) & VIP_data_independant$FA_ofTEI <3 ]<-2



#filtered
VIP_data_independant$FA_3MAD <- FA
length(VIP_data_independant$FA[!is.na(FA) & (FA <= (median(FA[!is.na(FA)])-3*mad(FA[!is.na(FA)])) | FA >= (median(FA[!is.na(FA)])+
								3*mad(FA[!is.na(FA)])))])
VIP_data_independant$FA_3MAD[!is.na(FA) & (FA <= (median(FA[!is.na(FA)])-3*mad(FA[!is.na(FA)])) | FA >= (median(FA[!is.na(FA)])+
						3*mad(FA[!is.na(FA)])))]<-NA

# % of TEI
VIP_data_independant$FA_ofTEI_3MAD<-(100*9*VIP_data_independant$FA_3MAD)/ensum1

#log and standardize filtered
VIP_data_independant$FA_3MAD_norm_sd<-log(VIP_data_independant$FA_3MAD)
VIP_data_independant$FA_3MAD_norm_sd[!is.na(VIP_data_independant$FA_3MAD_norm_sd)]<-(VIP_data_independant$FA_3MAD_norm_sd[!is.na(VIP_data_independant$FA_3MAD_norm_sd)] - 
			mean(VIP_data_independant$FA_3MAD_norm_sd[!is.na(VIP_data_independant$FA_3MAD_norm_sd)]))/(sd(VIP_data_independant$FA_3MAD_norm_sd[!is.na(VIP_data_independant$FA_3MAD_norm_sd)]))

# % of TEI log transformed and standardized
VIP_data_independant$FA_ofTEI_3MAD_norm_sd<-log(VIP_data_independant$FA_ofTEI_3MAD)		
VIP_data_independant$FA_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$FA_ofTEI_3MAD_norm_sd)]<-(VIP_data_independant$FA_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$FA_ofTEI_3MAD_norm_sd)]-
			mean(VIP_data_independant$FA_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$FA_ofTEI_3MAD_norm_sd)]))/sd(VIP_data_independant$FA_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$FA_ofTEI_3MAD_norm_sd)])

# categorizing the % of TEI based on guidelines
VIP_data_independant$FA_ofTEI_3MAD_categorized_g[!is.na(VIP_data_independant$FA_3MAD)]<-1
VIP_data_independant$FA_ofTEI_3MAD_categorized_g[!is.na(VIP_data_independant$FA_3MAD) & VIP_data_independant$FA_ofTEI_3MAD <3 ]<-2





# kolhsum1 is original, kolhsum1_norm_sd is original log tranformed and standardized, kolhsum1_3MAD_norm_sd cleaned from extremes and log tranformed and standardized, 
# kolhsum1_ofTEI_norm_sd is expressed in % of total energy intake and log transformed and standardized,kolhsum1_ofTEI_categorized is expressed in % of TEI and categorized 
# based on guidelines, kolhsum1_ofTEI_3MAD_norm_sd is cleaned from extremes and expressed in % of total energy intake and log transformed and standardized,
# kolhsum1_ofTEI_3MAD_categorized is cleaned from extremes and expressed in % of TEI and categorized based on guidelines


#original
#continuous log transformed and standardized
VIP_data_independant$kolhsum1_norm_sd<-log(kolhsum1)
VIP_data_independant$kolhsum1_norm_sd[!is.na(kolhsum1)]<-(VIP_data_independant$kolhsum1_norm_sd[!is.na(kolhsum1)]-
			mean(VIP_data_independant$kolhsum1_norm_sd[!is.na(kolhsum1)]))/sd(VIP_data_independant$kolhsum1_norm_sd[!is.na(kolhsum1)])

# % of TEI
VIP_data_independant$kolhsum1_ofTEI<-(100*4*VIP_data_independant$kolhsum1)/ensum1

# % of TEI log transformed and standardized
VIP_data_independant$kolhsum1_ofTEI_norm_sd<-log(VIP_data_independant$kolhsum1_ofTEI)		
VIP_data_independant$kolhsum1_ofTEI_norm_sd[!is.na(VIP_data_independant$kolhsum1_ofTEI_norm_sd)]<-(VIP_data_independant$kolhsum1_ofTEI_norm_sd[!is.na(VIP_data_independant$kolhsum1_ofTEI_norm_sd)]-
			mean(VIP_data_independant$kolhsum1_ofTEI_norm_sd[!is.na(VIP_data_independant$kolhsum1_ofTEI_norm_sd)]))/sd(VIP_data_independant$kolhsum1_ofTEI_norm_sd[!is.na(VIP_data_independant$kolhsum1_ofTEI_norm_sd)])

# categorizing the % of TEI based on guidelines
VIP_data_independant$kolhsum1_ofTEI_categorized_g[!is.na(kolhsum1)]<-0
VIP_data_independant$kolhsum1_ofTEI_categorized_g[!is.na(kolhsum1) & VIP_data_independant$kolhsum1_ofTEI >= 45 & VIP_data_independant$kolhsum1_ofTEI <= 60 ]<-1
VIP_data_independant$kolhsum1_ofTEI_categorized_g[!is.na(kolhsum1) & VIP_data_independant$kolhsum1_ofTEI > 60]<-2



#filtered
VIP_data_independant$kolhsum1_3MAD <- kolhsum1
length(VIP_data_independant$kolhsum1[!is.na(kolhsum1) & (kolhsum1 <= (median(kolhsum1[!is.na(kolhsum1)])-3*mad(kolhsum1[!is.na(kolhsum1)])) | kolhsum1 >= (median(kolhsum1[!is.na(kolhsum1)])+
								3*mad(kolhsum1[!is.na(kolhsum1)])))])
VIP_data_independant$kolhsum1_3MAD[!is.na(kolhsum1) & (kolhsum1 <= (median(kolhsum1[!is.na(kolhsum1)])-3*mad(kolhsum1[!is.na(kolhsum1)])) | kolhsum1 >= (median(kolhsum1[!is.na(kolhsum1)])+
						3*mad(kolhsum1[!is.na(kolhsum1)])))]<-NA

# % of TEI
VIP_data_independant$kolhsum1_ofTEI_3MAD<-(100*4*VIP_data_independant$kolhsum1_3MAD)/ensum1

#log and standardize filtered
VIP_data_independant$kolhsum1_3MAD_norm_sd<-log(VIP_data_independant$kolhsum1_3MAD)
VIP_data_independant$kolhsum1_3MAD_norm_sd[!is.na(VIP_data_independant$kolhsum1_3MAD_norm_sd)]<-(VIP_data_independant$kolhsum1_3MAD_norm_sd[!is.na(VIP_data_independant$kolhsum1_3MAD_norm_sd)] - 
			mean(VIP_data_independant$kolhsum1_3MAD_norm_sd[!is.na(VIP_data_independant$kolhsum1_3MAD_norm_sd)]))/(sd(VIP_data_independant$kolhsum1_3MAD_norm_sd[!is.na(VIP_data_independant$kolhsum1_3MAD_norm_sd)]))

# % of TEI log transformed and standardized
VIP_data_independant$kolhsum1_ofTEI_3MAD_norm_sd<-log(VIP_data_independant$kolhsum1_ofTEI_3MAD)		
VIP_data_independant$kolhsum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$kolhsum1_ofTEI_3MAD_norm_sd)]<-(VIP_data_independant$kolhsum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$kolhsum1_ofTEI_3MAD_norm_sd)]-
			mean(VIP_data_independant$kolhsum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$kolhsum1_ofTEI_3MAD_norm_sd)]))/sd(VIP_data_independant$kolhsum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$kolhsum1_ofTEI_3MAD_norm_sd)])

# categorizing the % of TEI based on guidelines
VIP_data_independant$kolhsum1_ofTEI_3MAD_categorized_g[!is.na(VIP_data_independant$kolhsum1_3MAD)]<-0
VIP_data_independant$kolhsum1_ofTEI_3MAD_categorized_g[!is.na(VIP_data_independant$kolhsum1_3MAD) & VIP_data_independant$kolhsum1_ofTEI_3MAD >= 45 & VIP_data_independant$kolhsum1_ofTEI_3MAD <= 60 ]<-1
VIP_data_independant$kolhsum1_ofTEI_3MAD_categorized_g[!is.na(VIP_data_independant$kolhsum1_3MAD) & VIP_data_independant$kolhsum1_ofTEI_3MAD > 60]<-2




# sacksum1 is original, sacksum1_norm_sd is original log tranformed and standardized, sacksum1_3MAD_norm_sd cleaned from extremes and log tranformed and standardized, 
# sacksum1_ofTEI_norm_sd is expressed in % of total energy intake and log transformed and standardized,sacksum1_ofTEI_categorized is expressed in % of TEI and categorized 
# based on guidelines, sacksum1_ofTEI_3MAD_norm_sd is cleaned from extremes and expressed in % of total energy intake and log transformed and standardized,
# sacksum1_ofTEI_3MAD_categorized is cleaned from extremes and expressed in % of TEI and categorized based on guidelines


#original
#continuous log transformed and standardized
VIP_data_independant$sacksum1_norm_sd<-log(sacksum1)
VIP_data_independant$sacksum1_norm_sd[!is.na(sacksum1)]<-(VIP_data_independant$sacksum1_norm_sd[!is.na(sacksum1)]-
			mean(VIP_data_independant$sacksum1_norm_sd[!is.na(sacksum1)]))/sd(VIP_data_independant$sacksum1_norm_sd[!is.na(sacksum1)])

# % of TEI
VIP_data_independant$sacksum1_ofTEI<-(100*4*VIP_data_independant$sacksum1)/ensum1

# % of TEI log transformed and standardized
VIP_data_independant$sacksum1_ofTEI_norm_sd<-log(VIP_data_independant$sacksum1_ofTEI)		
VIP_data_independant$sacksum1_ofTEI_norm_sd[!is.na(VIP_data_independant$sacksum1_ofTEI_norm_sd)]<-(VIP_data_independant$sacksum1_ofTEI_norm_sd[!is.na(VIP_data_independant$sacksum1_ofTEI_norm_sd)]-
			mean(VIP_data_independant$sacksum1_ofTEI_norm_sd[!is.na(VIP_data_independant$sacksum1_ofTEI_norm_sd)]))/sd(VIP_data_independant$sacksum1_ofTEI_norm_sd[!is.na(VIP_data_independant$sacksum1_ofTEI_norm_sd)])

# categorizing the % of TEI based on guidelines
VIP_data_independant$sacksum1_ofTEI_categorized_g[!is.na(sacksum1)]<-2
VIP_data_independant$sacksum1_ofTEI_categorized_g[!is.na(sacksum1) & VIP_data_independant$sacksum1_ofTEI < 10]<-1



#filtered
VIP_data_independant$sacksum1_3MAD <- sacksum1
length(VIP_data_independant$sacksum1[!is.na(sacksum1) & (sacksum1 <= (median(sacksum1[!is.na(sacksum1)])-3*mad(sacksum1[!is.na(sacksum1)])) | sacksum1 >= (median(sacksum1[!is.na(sacksum1)])+
								3*mad(sacksum1[!is.na(sacksum1)])))])
VIP_data_independant$sacksum1_3MAD[!is.na(sacksum1) & (sacksum1 <= (median(sacksum1[!is.na(sacksum1)])-3*mad(sacksum1[!is.na(sacksum1)])) | sacksum1 >= (median(sacksum1[!is.na(sacksum1)])+
						3*mad(sacksum1[!is.na(sacksum1)])))]<-NA

# % of TEI
VIP_data_independant$sacksum1_ofTEI_3MAD<-(100*4*VIP_data_independant$sacksum1_3MAD)/ensum1

#log and standardize filtered
VIP_data_independant$sacksum1_3MAD_norm_sd<-log(VIP_data_independant$sacksum1_3MAD)
VIP_data_independant$sacksum1_3MAD_norm_sd[!is.na(VIP_data_independant$sacksum1_3MAD_norm_sd)]<-(VIP_data_independant$sacksum1_3MAD_norm_sd[!is.na(VIP_data_independant$sacksum1_3MAD_norm_sd)] - 
			mean(VIP_data_independant$sacksum1_3MAD_norm_sd[!is.na(VIP_data_independant$sacksum1_3MAD_norm_sd)]))/(sd(VIP_data_independant$sacksum1_3MAD_norm_sd[!is.na(VIP_data_independant$sacksum1_3MAD_norm_sd)]))

# % of TEI log transformed and standardized
VIP_data_independant$sacksum1_ofTEI_3MAD_norm_sd<-log(VIP_data_independant$sacksum1_ofTEI_3MAD)		
VIP_data_independant$sacksum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$sacksum1_ofTEI_3MAD_norm_sd)]<-(VIP_data_independant$sacksum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$sacksum1_ofTEI_3MAD_norm_sd)]-
			mean(VIP_data_independant$sacksum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$sacksum1_ofTEI_3MAD_norm_sd)]))/sd(VIP_data_independant$sacksum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$sacksum1_ofTEI_3MAD_norm_sd)])

# categorizing the % of TEI based on guidelines
VIP_data_independant$sacksum1_ofTEI_3MAD_categorized_g[!is.na(VIP_data_independant$sacksum1_3MAD)]<-2
VIP_data_independant$sacksum1_ofTEI_3MAD_categorized_g[!is.na(VIP_data_independant$sacksum1_3MAD) & VIP_data_independant$sacksum1_ofTEI_3MAD < 10 ]<-1





# protsum1 is original, protsum1_norm_sd is original log tranformed and standardized, protsum1_3MAD_norm_sd cleaned from extremes and log tranformed and standardized, 
# protsum1_ofTEI_norm_sd is expressed in % of total energy intake and log transformed and standardized,protsum1_ofTEI_categorized is expressed in % of TEI and categorized 
# based on guidelines, protsum1_ofTEI_3MAD_norm_sd is cleaned from extremes and expressed in % of total energy intake and log transformed and standardized,
# protsum1_ofTEI_3MAD_categorized is cleaned from extremes and expressed in % of TEI and categorized based on guidelines


#original
#continuous log transformed and standardized
VIP_data_independant$protsum1_norm_sd<-log(protsum1)
VIP_data_independant$protsum1_norm_sd[!is.na(protsum1)]<-(VIP_data_independant$protsum1_norm_sd[!is.na(protsum1)]-
			mean(VIP_data_independant$protsum1_norm_sd[!is.na(protsum1)]))/sd(VIP_data_independant$protsum1_norm_sd[!is.na(protsum1)])

# % of TEI
VIP_data_independant$protsum1_ofTEI<-(100*4*VIP_data_independant$protsum1)/ensum1

# % of TEI log transformed and standardized
VIP_data_independant$protsum1_ofTEI_norm_sd<-log(VIP_data_independant$protsum1_ofTEI)		
VIP_data_independant$protsum1_ofTEI_norm_sd[!is.na(VIP_data_independant$protsum1_ofTEI_norm_sd)]<-(VIP_data_independant$protsum1_ofTEI_norm_sd[!is.na(VIP_data_independant$protsum1_ofTEI_norm_sd)]-
			mean(VIP_data_independant$protsum1_ofTEI_norm_sd[!is.na(VIP_data_independant$protsum1_ofTEI_norm_sd)]))/sd(VIP_data_independant$protsum1_ofTEI_norm_sd[!is.na(VIP_data_independant$protsum1_ofTEI_norm_sd)])

# categorizing the % of TEI based on guidelines
VIP_data_independant$protsum1_ofTEI_categorized_g[!is.na(protsum1)]<-0
VIP_data_independant$protsum1_ofTEI_categorized_g[!is.na(protsum1) & VIP_data_independant$protsum1_ofTEI >= 10 & VIP_data_independant$protsum1_ofTEI <= 20 ]<-1
VIP_data_independant$protsum1_ofTEI_categorized_g[!is.na(protsum1) & VIP_data_independant$protsum1_ofTEI > 20]<-2



#filtered
VIP_data_independant$protsum1_3MAD <- protsum1
length(VIP_data_independant$protsum1[!is.na(protsum1) & (protsum1 <= (median(protsum1[!is.na(protsum1)])-3*mad(protsum1[!is.na(protsum1)])) | protsum1 >= (median(protsum1[!is.na(protsum1)])+
								3*mad(protsum1[!is.na(protsum1)])))])
VIP_data_independant$protsum1_3MAD[!is.na(protsum1) & (protsum1 <= (median(protsum1[!is.na(protsum1)])-3*mad(protsum1[!is.na(protsum1)])) | protsum1 >= (median(protsum1[!is.na(protsum1)])+
						3*mad(protsum1[!is.na(protsum1)])))]<-NA

# % of TEI
VIP_data_independant$protsum1_ofTEI_3MAD<-(100*4*VIP_data_independant$protsum1_3MAD)/ensum1

#log and standardize filtered
VIP_data_independant$protsum1_3MAD_norm_sd<-log(VIP_data_independant$protsum1_3MAD)
VIP_data_independant$protsum1_3MAD_norm_sd[!is.na(VIP_data_independant$protsum1_3MAD_norm_sd)]<-(VIP_data_independant$protsum1_3MAD_norm_sd[!is.na(VIP_data_independant$protsum1_3MAD_norm_sd)] - 
			mean(VIP_data_independant$protsum1_3MAD_norm_sd[!is.na(VIP_data_independant$protsum1_3MAD_norm_sd)]))/(sd(VIP_data_independant$protsum1_3MAD_norm_sd[!is.na(VIP_data_independant$protsum1_3MAD_norm_sd)]))

# % of TEI log transformed and standardized
VIP_data_independant$protsum1_ofTEI_3MAD_norm_sd<-log(VIP_data_independant$protsum1_ofTEI_3MAD)		
VIP_data_independant$protsum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$protsum1_ofTEI_3MAD_norm_sd)]<-(VIP_data_independant$protsum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$protsum1_ofTEI_3MAD_norm_sd)]-
			mean(VIP_data_independant$protsum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$protsum1_ofTEI_3MAD_norm_sd)]))/sd(VIP_data_independant$protsum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$protsum1_ofTEI_3MAD_norm_sd)])

# categorizing the % of TEI based on guidelines
VIP_data_independant$protsum1_ofTEI_3MAD_categorized_g[!is.na(VIP_data_independant$protsum1_3MAD)]<-0
VIP_data_independant$protsum1_ofTEI_3MAD_categorized_g[!is.na(VIP_data_independant$protsum1_3MAD) & VIP_data_independant$protsum1_ofTEI_3MAD >= 10 & VIP_data_independant$protsum1_ofTEI_3MAD <= 20 ]<-1
VIP_data_independant$protsum1_ofTEI_3MAD_categorized_g[!is.na(VIP_data_independant$protsum1_3MAD) & VIP_data_independant$protsum1_ofTEI_3MAD > 20]<-2





# fibesum1 is original, fibesum1_norm_sd is original log tranformed and standardized, fibesum1_3MAD_norm_sd cleaned from extremes and log tranformed and standardized, 
# fibesum1_ofTEI_norm_sd is expressed in % of total energy intake and log transformed and standardized,fibesum1_ofTEI_categorized is expressed in % of TEI and categorized 
# based on guidelines, fibesum1_ofTEI_3MAD_norm_sd is cleaned from extremes and expressed in % of total energy intake and log transformed and standardized,
# fibesum1_ofTEI_3MAD_categorized is cleaned from extremes and expressed in % of TEI and categorized based on guidelines


#original
#continuous log transformed and standardized
VIP_data_independant$fibesum1_norm_sd<-log(fibesum1)
VIP_data_independant$fibesum1_norm_sd[!is.na(fibesum1)]<-(VIP_data_independant$fibesum1_norm_sd[!is.na(fibesum1)]-
			mean(VIP_data_independant$fibesum1_norm_sd[!is.na(fibesum1)]))/sd(VIP_data_independant$fibesum1_norm_sd[!is.na(fibesum1)])

# % of TEI
VIP_data_independant$fibesum1_ofTEI<-(100*4*VIP_data_independant$fibesum1)/ensum1

# % of TEI log transformed and standardized
VIP_data_independant$fibesum1_ofTEI_norm_sd<-log(VIP_data_independant$fibesum1_ofTEI)		
VIP_data_independant$fibesum1_ofTEI_norm_sd[!is.na(VIP_data_independant$fibesum1_ofTEI_norm_sd)]<-(VIP_data_independant$fibesum1_ofTEI_norm_sd[!is.na(VIP_data_independant$fibesum1_ofTEI_norm_sd)]-
			mean(VIP_data_independant$fibesum1_ofTEI_norm_sd[!is.na(VIP_data_independant$fibesum1_ofTEI_norm_sd)]))/sd(VIP_data_independant$fibesum1_ofTEI_norm_sd[!is.na(VIP_data_independant$fibesum1_ofTEI_norm_sd)])

# categorizing the % of TEI based on guidelines
VIP_data_independant$fibesum1_categorized_g[!is.na(fibesum1)]<-1
VIP_data_independant$fibesum1_categorized_g[!is.na(fibesum1) & VIP_data_independant$fibesum1 < 25 ]<-2



#filtered
VIP_data_independant$fibesum1_3MAD <- fibesum1
length(VIP_data_independant$fibesum1[!is.na(fibesum1) & (fibesum1 <= (median(fibesum1[!is.na(fibesum1)])-3*mad(fibesum1[!is.na(fibesum1)])) | fibesum1 >= (median(fibesum1[!is.na(fibesum1)])+
								3*mad(fibesum1[!is.na(fibesum1)])))])
VIP_data_independant$fibesum1_3MAD[!is.na(fibesum1) & (fibesum1 <= (median(fibesum1[!is.na(fibesum1)])-3*mad(fibesum1[!is.na(fibesum1)])) | fibesum1 >= (median(fibesum1[!is.na(fibesum1)])+
						3*mad(fibesum1[!is.na(fibesum1)])))]<-NA

# % of TEI
VIP_data_independant$fibesum1_ofTEI_3MAD<-(100*4*VIP_data_independant$fibesum1_3MAD)/ensum1

#log and standardize filtered
VIP_data_independant$fibesum1_3MAD_norm_sd<-log(VIP_data_independant$fibesum1_3MAD)
VIP_data_independant$fibesum1_3MAD_norm_sd[!is.na(VIP_data_independant$fibesum1_3MAD_norm_sd)]<-(VIP_data_independant$fibesum1_3MAD_norm_sd[!is.na(VIP_data_independant$fibesum1_3MAD_norm_sd)] - 
			mean(VIP_data_independant$fibesum1_3MAD_norm_sd[!is.na(VIP_data_independant$fibesum1_3MAD_norm_sd)]))/(sd(VIP_data_independant$fibesum1_3MAD_norm_sd[!is.na(VIP_data_independant$fibesum1_3MAD_norm_sd)]))

# % of TEI log transformed and standardized
VIP_data_independant$fibesum1_ofTEI_3MAD_norm_sd<-log(VIP_data_independant$fibesum1_ofTEI_3MAD)		
VIP_data_independant$fibesum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$fibesum1_ofTEI_3MAD_norm_sd)]<-(VIP_data_independant$fibesum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$fibesum1_ofTEI_3MAD_norm_sd)]-
			mean(VIP_data_independant$fibesum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$fibesum1_ofTEI_3MAD_norm_sd)]))/sd(VIP_data_independant$fibesum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$fibesum1_ofTEI_3MAD_norm_sd)])

# categorizing the % of TEI based on guidelines
VIP_data_independant$fibesum1_3MAD_categorized_g[!is.na(VIP_data_independant$fibesum1_3MAD)]<-1
VIP_data_independant$fibesum1_3MAD_categorized_g[!is.na(VIP_data_independant$fibesum1_3MAD) & VIP_data_independant$fibesum1_3MAD < 25 ]<-2






# NATRsum1 is original, NATRsum1_norm_sd is original log tranformed and standardized, NATRsum1_3MAD_norm_sd cleaned from extremes and log tranformed and standardized, 
# NATRsum1_ofTEI_norm_sd is expressed in % of total energy intake and log transformed and standardized,NATRsum1_ofTEI_categorized is expressed in % of TEI and categorized 
# based on guidelines, NATRsum1_ofTEI_3MAD_norm_sd is cleaned from extremes and expressed in % of total energy intake and log transformed and standardized,
# NATRsum1_ofTEI_3MAD_categorized is cleaned from extremes and expressed in % of TEI and categorized based on guidelines


#original
#continuous log transformed and standardized
VIP_data_independant$NATRsum1_norm_sd<-log(NATRsum1)
VIP_data_independant$NATRsum1_norm_sd[!is.na(NATRsum1)]<-(VIP_data_independant$NATRsum1_norm_sd[!is.na(NATRsum1)]-
			mean(VIP_data_independant$NATRsum1_norm_sd[!is.na(NATRsum1)]))/sd(VIP_data_independant$NATRsum1_norm_sd[!is.na(NATRsum1)])

# % of TEI
VIP_data_independant$NATRsum1_ofTEI<-(100*4*VIP_data_independant$NATRsum1)/ensum1

# % of TEI log transformed and standardized
VIP_data_independant$NATRsum1_ofTEI_norm_sd<-log(VIP_data_independant$NATRsum1_ofTEI)		
VIP_data_independant$NATRsum1_ofTEI_norm_sd[!is.na(VIP_data_independant$NATRsum1_ofTEI_norm_sd)]<-(VIP_data_independant$NATRsum1_ofTEI_norm_sd[!is.na(VIP_data_independant$NATRsum1_ofTEI_norm_sd)]-
			mean(VIP_data_independant$NATRsum1_ofTEI_norm_sd[!is.na(VIP_data_independant$NATRsum1_ofTEI_norm_sd)]))/sd(VIP_data_independant$NATRsum1_ofTEI_norm_sd[!is.na(VIP_data_independant$NATRsum1_ofTEI_norm_sd)])

# categorizing the % of TEI based on guidelines
VIP_data_independant$NATRsum1_categorized_g[!is.na(NATRsum1)]<-2
VIP_data_independant$NATRsum1_categorized_g[!is.na(NATRsum1) & VIP_data_independant$NATRsum1/1000 <= 2.4 ]<-1



#filtered
VIP_data_independant$NATRsum1_3MAD <- NATRsum1
length(VIP_data_independant$NATRsum1[!is.na(NATRsum1) & (NATRsum1 <= (median(NATRsum1[!is.na(NATRsum1)])-3*mad(NATRsum1[!is.na(NATRsum1)])) | NATRsum1 >= (median(NATRsum1[!is.na(NATRsum1)])+
								3*mad(NATRsum1[!is.na(NATRsum1)])))])
VIP_data_independant$NATRsum1_3MAD[!is.na(NATRsum1) & (NATRsum1 <= (median(NATRsum1[!is.na(NATRsum1)])-3*mad(NATRsum1[!is.na(NATRsum1)])) | NATRsum1 >= (median(NATRsum1[!is.na(NATRsum1)])+
						3*mad(NATRsum1[!is.na(NATRsum1)])))]<-NA

# % of TEI
VIP_data_independant$NATRsum1_ofTEI_3MAD<-(100*4*VIP_data_independant$NATRsum1_3MAD)/ensum1

#log and standardize filtered
VIP_data_independant$NATRsum1_3MAD_norm_sd<-log(VIP_data_independant$NATRsum1_3MAD)
VIP_data_independant$NATRsum1_3MAD_norm_sd[!is.na(VIP_data_independant$NATRsum1_3MAD_norm_sd)]<-(VIP_data_independant$NATRsum1_3MAD_norm_sd[!is.na(VIP_data_independant$NATRsum1_3MAD_norm_sd)] - 
			mean(VIP_data_independant$NATRsum1_3MAD_norm_sd[!is.na(VIP_data_independant$NATRsum1_3MAD_norm_sd)]))/(sd(VIP_data_independant$NATRsum1_3MAD_norm_sd[!is.na(VIP_data_independant$NATRsum1_3MAD_norm_sd)]))

# % of TEI log transformed and standardized
VIP_data_independant$NATRsum1_ofTEI_3MAD_norm_sd<-log(VIP_data_independant$NATRsum1_ofTEI_3MAD)		
VIP_data_independant$NATRsum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$NATRsum1_ofTEI_3MAD_norm_sd)]<-(VIP_data_independant$NATRsum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$NATRsum1_ofTEI_3MAD_norm_sd)]-
			mean(VIP_data_independant$NATRsum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$NATRsum1_ofTEI_3MAD_norm_sd)]))/sd(VIP_data_independant$NATRsum1_ofTEI_3MAD_norm_sd[!is.na(VIP_data_independant$NATRsum1_ofTEI_3MAD_norm_sd)])

# categorizing the % of TEI based on guidelines
VIP_data_independant$NATRsum1_3MAD_categorized_g[!is.na(VIP_data_independant$NATRsum1_3MAD)]<-2
VIP_data_independant$NATRsum1_3MAD_categorized_g[!is.na(VIP_data_independant$NATRsum1_3MAD) & VIP_data_independant$NATRsum1_3MAD/1000 <= 2.4]<-1


detach(VIP_data_independant)
attach(VIP_data_independant)
