#load datasets

#load entire cleaned data (154009 lines)
VIP_data_all <- read.csv("../VIP_data/VIP_170206_cleaned.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)

#environment scores for visit 1 and visit2
VIP_data_subset_visit1_complete_cases<- read.csv("../VIP_data/VIP_data_subset_visit1_complete_cases.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)
VIP_data_subset_visit2_complete_cases<- read.csv("../VIP_data/VIP_data_subset_visit2_complete_cases.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)

#plot results
d<-density(VIP_data_subset_visit1_complete_cases$environment_score_scaled)
png("../Results/environment_risk_score/visit1_environment_z_score.png",width=1500,height=750)
plot(d, main=" Environment Z-score distribution in frist vist")
polygon(d, col="gray90", border="gray20") 
dev.off()

d<-density(VIP_data_subset_visit1_complete_cases$bmi_norm_sd)
png("../Results/environment_risk_score/visit1_bmi_z_score.png",width=1500,height=750)
plot(d, main=" BMI Z-score distribution in first visit")
polygon(d, col="gray90", border="gray20") 
dev.off()


d<-density(VIP_data_subset_visit2_complete_cases$environment_score_scaled)
png("../Results/environment_risk_score/visit2_environment_z_score.png",width=1500,height=750)
plot(d, main=" Environment Z-score distribution in second vist")
polygon(d, col="gray90", border="gray20") 
dev.off()

d<-density(VIP_data_subset_visit2_complete_cases$bmi_norm_sd)
png("../Results/environment_risk_score/visit2_bmi_z_score.png",width=1500,height=750)
plot(d, main=" BMI Z-score distribution in second visit")
polygon(d, col="gray90", border="gray20") 
dev.off()


#check the results of the ratio
head(sort(unique(VIP_data_subset_visit1_complete_cases$continuous_lean_phenotype)),n=10)#first 10
#[1] 0.3143463 0.3203931 0.3260743 0.3963080 0.4022453 0.4110082 0.4141605
#[8] 0.4156270 0.4212915 0.4247347
tail(sort(unique(VIP_data_subset_visit1_complete_cases$continuous_lean_phenotype)),n=10)#last 10
#[1] 2.899288 2.961561 2.963085 3.066674 3.098992 3.163626 3.199369 3.471812
#[9] 3.748436 4.550327

summary(VIP_data_subset_visit1_complete_cases$continuous_lean_phenotype)
hist(VIP_data_subset_visit1_complete_cases$continuous_lean_phenotype)


#different ways to construct the phenotype and select subjects, in all the opposite must be selected as well:
#Continous:
#take the ratio of as the phenotype, presenting the response to environment induced weight gain, values close to one mean that the weight is gained as expected according to
#environment exposure, values far below one mean that the subject is not gaining as much weight as expected, values far above one mean that the subject is gaining more weight
#than expected

# to select subjects from the continous phenotype, cut off points need to be selected, this is done in several different ways:
#1.take those that have ratio smaller then 1 and are normal/underweight(very broad)
#2.take bottom 25% of ratio z-score and those that are normal/undrweight(less broad)
#3. set cut off point for the environment score only and take those that are normal/underweight(much less broad)
#3.1 top 25% of environment score 
#3.2 top 30% of environment score 
#3.3 top 35% of environment score 


#Discrete:
#model classes of bmi and then construct the phenotype based on the model missclassifications:
#-3 obsese but should be normal/underweight
#-2 overweight but should be normal/underweight 
#-1 obese but should be overweight only
# 0 as expected
# 1 overweight, but should be obese
# 2 normal/underweight but should be overweight
# 3 normal/underweight but should be obese

#to selected subjects, simple combine different classes:
#1.take only those that had been missclassified as overweight and obese, but are normal
#2.take those that have been classified into a higher class then they are

#C.1:
#get subjects that have the ratio smaller then one and are normal/underweight
visit1_subjects_C1<-VIP_data_subset_visit1_complete_cases$Subject_id[VIP_data_subset_visit1_complete_cases$bmi<25 & VIP_data_subset_visit1_complete_cases$continuous_lean_phenotype<1]
length(visit1_subjects_C1)#15124

visit2_subjects_C1<-VIP_data_subset_visit2_complete_cases$Subject_id[VIP_data_subset_visit2_complete_cases$bmi<25 & VIP_data_subset_visit2_complete_cases$continuous_lean_phenotype<1]
length(visit2_subjects_C1)# 12643


persistently_lean_subjects_C1<-visit2_subjects_C1[visit2_subjects_C1 %in% visit1_subjects_C1]
length(persistently_lean_subjects_C1)#10986

#check how many of them are current smokers
persistently_lean_subjects_C1_not_smokers<-persistently_lean_subjects_C1[!(persistently_lean_subjects_C1 %in% 
					VIP_data_all$Subject_id[VIP_data_all$sm_status==1 & !is.na(VIP_data_all$sm_status)])]
length(persistently_lean_subjects_C1_not_smokers)#8898

#select the opposite ones
#those that are obese/overweight and have ratio one or above
visit1_subjects_C1_op<-VIP_data_subset_visit1_complete_cases$Subject_id[VIP_data_subset_visit1_complete_cases$bmi>=25 & VIP_data_subset_visit1_complete_cases$continuous_lean_phenotype>=1]
length(visit1_subjects_C1_op)#12322

visit2_subjects_C1_op<-VIP_data_subset_visit2_complete_cases$Subject_id[VIP_data_subset_visit2_complete_cases$bmi>=25 & VIP_data_subset_visit2_complete_cases$continuous_lean_phenotype>=1]
length(visit2_subjects_C1_op)# 13850


persistently_lean_subjects_C1_op<-visit2_subjects_C1_op[visit2_subjects_C1_op %in% visit1_subjects_C1_op]
length(persistently_lean_subjects_C1_op)#10263


#C.2:
#get subjects that are in the bottom 25% of ratio z-score and are normal/underweight
visit1_subjects_C2<-VIP_data_subset_visit1_complete_cases$Subject_id[VIP_data_subset_visit1_complete_cases$bmi<25 & scale(VIP_data_subset_visit1_complete_cases$continuous_lean_phenotype)<(-0.675)]
length(visit1_subjects_C2)#7545

visit2_subjects_C2<-VIP_data_subset_visit2_complete_cases$Subject_id[VIP_data_subset_visit2_complete_cases$bmi<25 & scale(VIP_data_subset_visit2_complete_cases$continuous_lean_phenotype)<(-0.675)]
length(visit2_subjects_C2)# 7424


persistently_lean_subjects_C2<-visit2_subjects_C2[visit2_subjects_C2 %in% visit1_subjects_C2]
length(persistently_lean_subjects_C2)#5176

#check how many of them are current smokers
persistently_lean_subjects_C2_not_smokers<-persistently_lean_subjects_C2[!(persistently_lean_subjects_C2 %in% 
					VIP_data_all$Subject_id[VIP_data_all$sm_status==1 & !is.na(VIP_data_all$sm_status)])]
length(persistently_lean_subjects_C2_not_smokers)#4103


#select the opposite ones
#those that are obese/overweight and have ratio one or above

visit1_subjects_C2_op<-VIP_data_subset_visit1_complete_cases$Subject_id[VIP_data_subset_visit1_complete_cases$bmi>=25 & VIP_data_subset_visit1_complete_cases$continuous_lean_phenotype>=1]
length(visit1_subjects_C2_op)#12322

visit2_subjects_C2_op<-VIP_data_subset_visit2_complete_cases$Subject_id[VIP_data_subset_visit2_complete_cases$bmi>=25 & VIP_data_subset_visit2_complete_cases$continuous_lean_phenotype>=1]
length(visit2_subjects_C2_op)# 13850


persistently_lean_subjects_C2_op<-visit2_subjects_C2_op[visit2_subjects_C2_op %in% visit1_subjects_C2_op]
length(persistently_lean_subjects_C2_op)#10263



#C.3.1:
#get subjects that are normal/underweight and are in the top 25% of environment score
visit1_subjects_C31<-VIP_data_subset_visit1_complete_cases$Subject_id[VIP_data_subset_visit1_complete_cases$bmi<25  & VIP_data_subset_visit1_complete_cases$environment_score_scaled>=0.675]
length(visit1_subjects_C31)#3517

visit2_subjects_C31<-VIP_data_subset_visit2_complete_cases$Subject_id[VIP_data_subset_visit2_complete_cases$bmi<25  & VIP_data_subset_visit2_complete_cases$environment_score_scaled>=0.675]
length(visit2_subjects_C31)# 2423


persistently_lean_subjects_C31<-visit2_subjects_C31[visit2_subjects_C31 %in% visit1_subjects_C31]
length(persistently_lean_subjects_C31)#860

#check how many of them are current smokers
persistently_lean_subjects_C31_not_smokers<-persistently_lean_subjects_C31[!(persistently_lean_subjects_C31 %in% 
					VIP_data_all$Subject_id[VIP_data_all$sm_status==1 & !is.na(VIP_data_all$sm_status)])]
length(persistently_lean_subjects_C31_not_smokers)#606


#select the opposite ones
#those that are obese/overweight and are in the top 25% of environment score
visit1_subjects_C31_op<-VIP_data_subset_visit1_complete_cases$Subject_id[VIP_data_subset_visit1_complete_cases$bmi>=25  & VIP_data_subset_visit1_complete_cases$environment_score_scaled>=0.675]
length(visit1_subjects_C31_op)#4069

visit2_subjects_C31_op<-VIP_data_subset_visit2_complete_cases$Subject_id[VIP_data_subset_visit2_complete_cases$bmi>=25  & VIP_data_subset_visit2_complete_cases$environment_score_scaled>=0.675]
length(visit2_subjects_C31_op)# 5082


persistently_lean_subjects_C31_op<-visit2_subjects_C31_op[visit2_subjects_C31_op %in% visit1_subjects_C31_op]
length(persistently_lean_subjects_C31_op)#1844




#C.3.2:
#get subjects that normal/underweight and top 25% of environment
visit1_subjects_C32<-VIP_data_subset_visit1_complete_cases$Subject_id[VIP_data_subset_visit1_complete_cases$bmi<25 & VIP_data_subset_visit1_complete_cases$environment_score_scaled>=0.52]
length(visit1_subjects_C32)#4336

visit2_subjects_C32<-VIP_data_subset_visit2_complete_cases$Subject_id[VIP_data_subset_visit2_complete_cases$bmi<25 & VIP_data_subset_visit2_complete_cases$environment_score_scaled>=0.52]
length(visit2_subjects_C32)# 3020

persistently_lean_subjects_C32<-visit2_subjects_C32[visit2_subjects_C32 %in% visit1_subjects_C32]
length(persistently_lean_subjects_C32)#1242

#check how many of them are current smokers
persistently_lean_subjects_C32_not_smokers<-persistently_lean_subjects_C32[!(persistently_lean_subjects_C32 %in% 
					VIP_data_all$Subject_id[VIP_data_all$sm_status==1 & !is.na(VIP_data_all$sm_status)])]
length(persistently_lean_subjects_C32_not_smokers)#879


#select the opposite ones
#those that are obese/overweight and are in the top 30% of environment score
visit1_subjects_C32_op<-VIP_data_subset_visit1_complete_cases$Subject_id[VIP_data_subset_visit1_complete_cases$bmi>=25  & VIP_data_subset_visit1_complete_cases$environment_score_scaled>=0.52]
length(visit1_subjects_C32_op)#4812

visit2_subjects_C32_op<-VIP_data_subset_visit2_complete_cases$Subject_id[VIP_data_subset_visit2_complete_cases$bmi>=25  & VIP_data_subset_visit2_complete_cases$environment_score_scaled>=0.52]
length(visit2_subjects_C32_op)# 6068


persistently_lean_subjects_C32_op<-visit2_subjects_C32_op[visit2_subjects_C32_op %in% visit1_subjects_C32_op]
length(persistently_lean_subjects_C32_op)#2389




#C.3.3:
#get subjects that normal/underweight and top 25% of environment
visit1_subjects_C33<-VIP_data_subset_visit1_complete_cases$Subject_id[VIP_data_subset_visit1_complete_cases$bmi<25 & VIP_data_subset_visit1_complete_cases$environment_score_scaled>=0.38]
length(visit1_subjects_C33)#5111

visit2_subjects_C33<-VIP_data_subset_visit2_complete_cases$Subject_id[VIP_data_subset_visit2_complete_cases$bmi<25 & VIP_data_subset_visit2_complete_cases$environment_score_scaled>=0.38]
length(visit2_subjects_C33)# 3625

persistently_lean_subjects_C33<-visit2_subjects_C33[visit2_subjects_C33 %in% visit1_subjects_C33]
length(persistently_lean_subjects_C33)#1591

#check how many of them are current smokers
persistently_lean_subjects_C33_not_smokers<-persistently_lean_subjects_C33[!(persistently_lean_subjects_C33 %in% 
					VIP_data_all$Subject_id[VIP_data_all$sm_status==1 & !is.na(VIP_data_all$sm_status)])]
length(persistently_lean_subjects_C33_not_smokers)#1147

#select the opposite ones
#those that are obese/overweight and are in the top 35% of environment score
visit1_subjects_C33_op<-VIP_data_subset_visit1_complete_cases$Subject_id[VIP_data_subset_visit1_complete_cases$bmi>=25  & VIP_data_subset_visit1_complete_cases$environment_score_scaled>=0.38]
length(visit1_subjects_C33_op)#5614

visit2_subjects_C33_op<-VIP_data_subset_visit2_complete_cases$Subject_id[VIP_data_subset_visit2_complete_cases$bmi>=25  & VIP_data_subset_visit2_complete_cases$environment_score_scaled>=0.38]
length(visit2_subjects_C33_op)# 7009


persistently_lean_subjects_C33_op<-visit2_subjects_C33_op[visit2_subjects_C33_op %in% visit1_subjects_C33_op]
length(persistently_lean_subjects_C33_op)#3008




#save the results
write.table(persistently_lean_subjects_C1_not_smokers, "../Results/persistantly_lean_subjects/subjects_C1",row.names=F,col.names=F)
write.table(persistently_lean_subjects_C2_not_smokers, "../Results/persistantly_lean_subjects/subjects_C2",row.names=F,col.names=F)
write.table(persistently_lean_subjects_C31_not_smokers, "../Results/persistantly_lean_subjects/subjects_C31",row.names=F,col.names=F)
write.table(persistently_lean_subjects_C32_not_smokers, "../Results/persistantly_lean_subjects/subjects_C32",row.names=F,col.names=F)
write.table(persistently_lean_subjects_C33_not_smokers, "../Results/persistantly_lean_subjects/subjects_C33",row.names=F,col.names=F)

write.table(VIP_data_subset_visit1_complete_cases[VIP_data_subset_visit1_complete_cases$Subject_id %in% persistently_lean_subjects_C1_not_smokers,"enummer"], "../Results/persistantly_lean_subjects/enummers_C1",row.names=F,col.names=F)
write.table(VIP_data_subset_visit1_complete_cases[VIP_data_subset_visit1_complete_cases$Subject_id %in% persistently_lean_subjects_C2_not_smokers,"enummer"], "../Results/persistantly_lean_subjects/enummers_C2",row.names=F,col.names=F)
write.table(VIP_data_subset_visit1_complete_cases[VIP_data_subset_visit1_complete_cases$Subject_id %in% persistently_lean_subjects_C31_not_smokers,"enummer"], "../Results/persistantly_lean_subjects/enummers_C31",row.names=F,col.names=F)
write.table(VIP_data_subset_visit1_complete_cases[VIP_data_subset_visit1_complete_cases$Subject_id %in% persistently_lean_subjects_C32_not_smokers,"enummer"], "../Results/persistantly_lean_subjects/enummers_C32",row.names=F,col.names=F)
write.table(VIP_data_subset_visit1_complete_cases[VIP_data_subset_visit1_complete_cases$Subject_id %in% persistently_lean_subjects_C33_not_smokers,"enummer"], "../Results/persistantly_lean_subjects/enummers_C33",row.names=F,col.names=F)


write.table(persistently_lean_subjects_C1_op, "../Results/persistantly_lean_subjects/subjects_op_C1",row.names=F,col.names=F)
write.table(persistently_lean_subjects_C2_op, "../Results/persistantly_lean_subjects/subjects_op_C2",row.names=F,col.names=F)
write.table(persistently_lean_subjects_C31_op, "../Results/persistantly_lean_subjects/subjects_op_C31",row.names=F,col.names=F)
write.table(persistently_lean_subjects_C32_op, "../Results/persistantly_lean_subjects/subjects_op_C32",row.names=F,col.names=F)
write.table(persistently_lean_subjects_C33_op, "../Results/persistantly_lean_subjects/subjects_op_C33",row.names=F,col.names=F)

write.table(VIP_data_subset_visit1_complete_cases[VIP_data_subset_visit1_complete_cases$Subject_id %in% persistently_lean_subjects_C1_op,"enummer"], "../Results/persistantly_lean_subjects/enummers_op_C1",row.names=F,col.names=F)
write.table(VIP_data_subset_visit1_complete_cases[VIP_data_subset_visit1_complete_cases$Subject_id %in% persistently_lean_subjects_C2_op,"enummer"], "../Results/persistantly_lean_subjects/enummers_op_C2",row.names=F,col.names=F)
write.table(VIP_data_subset_visit1_complete_cases[VIP_data_subset_visit1_complete_cases$Subject_id %in% persistently_lean_subjects_C31_op,"enummer"], "../Results/persistantly_lean_subjects/enummers_op_C31",row.names=F,col.names=F)
write.table(VIP_data_subset_visit1_complete_cases[VIP_data_subset_visit1_complete_cases$Subject_id %in% persistently_lean_subjects_C32_op,"enummer"], "../Results/persistantly_lean_subjects/enummers_op_C32",row.names=F,col.names=F)
write.table(VIP_data_subset_visit1_complete_cases[VIP_data_subset_visit1_complete_cases$Subject_id %in% persistently_lean_subjects_C33_op,"enummer"], "../Results/persistantly_lean_subjects/enummers_op_C33",row.names=F,col.names=F)



#D:select based on missclassification

