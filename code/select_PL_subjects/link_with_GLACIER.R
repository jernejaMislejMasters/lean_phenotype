persistantly_lean_C1<-unlist(read.table("../Results/persistantly_lean_subjects/enummers_C1",row.names=NULL, header = FALSE))
persistantly_lean_C2<-unlist(read.table("../Results/persistantly_lean_subjects/enummers_C2",row.names=NULL, header = FALSE))
persistantly_lean_C31<-unlist(read.table("../Results/persistantly_lean_subjects/enummers_C31",row.names=NULL, header = FALSE))
persistantly_lean_C32<-unlist(read.table("../Results/persistantly_lean_subjects/enummers_C32",row.names=NULL, header = FALSE))
persistantly_lean_C33<-unlist(read.table("../Results/persistantly_lean_subjects/enummers_C33",row.names=NULL, header = FALSE))

persistantly_lean_op_C1<-unlist(read.table("../Results/persistantly_lean_subjects/enummers_op_C1",row.names=NULL, header = FALSE))
persistantly_lean_op_C2<-unlist(read.table("../Results/persistantly_lean_subjects/enummers_op_C2",row.names=NULL, header = FALSE))
persistantly_lean_op_C31<-unlist(read.table("../Results/persistantly_lean_subjects/enummers_op_C31",row.names=NULL, header = FALSE))
persistantly_lean_op_C32<-unlist(read.table("../Results/persistantly_lean_subjects/enummers_op_C32",row.names=NULL, header = FALSE))
persistantly_lean_op_C33<-unlist(read.table("../Results/persistantly_lean_subjects/enummers_op_C33",row.names=NULL, header = FALSE))

VIP_data_subset_visit1_complete_cases<- read.csv("../VIP_data/VIP_data_subset_visit1_complete_cases.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)
VIP_data_subset_visit2_complete_cases<- read.csv("../VIP_data/VIP_data_subset_visit2_complete_cases.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)

link_data<- read.table("../Documents/femvfemk_qdates.txt", header = TRUE,row.names=NULL,sep="\t")


GLACIER_ids_C1<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% persistantly_lean_C1]#all below reg. line and are normal/underweight
length(GLACIER_ids_C1)#1071

GLACIER_ids_C2<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% persistantly_lean_C2]#bottom 25% of ratio  and are normal/underweight
length(GLACIER_ids_C2)#467

GLACIER_ids_C31<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% persistantly_lean_C31]#top 25% of environment  and are normal/underweight
length(GLACIER_ids_C31)#92

GLACIER_ids_C32<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% persistantly_lean_C32]#top 30% of environment  and are normal/underweight
length(GLACIER_ids_C32)#128

GLACIER_ids_C33<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% persistantly_lean_C33]#top 35% of environment  and are normal/underweight
length(GLACIER_ids_C33)#166


GLACIER_ids_op_C1<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% persistantly_lean_op_C1]#all below reg. line and are normal/underweight
length(GLACIER_ids_op_C1)#1306

GLACIER_ids_op_C2<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% persistantly_lean_op_C2]#bottom 25% of ratio  and are normal/underweight
length(GLACIER_ids_op_C2)#1306

GLACIER_ids_op_C31<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% persistantly_lean_op_C31]#top 25% of environment  and are normal/underweight
length(GLACIER_ids_op_C31)#294

GLACIER_ids_op_C32<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% persistantly_lean_op_C32]#top 30% of environment  and are normal/underweight
length(GLACIER_ids_op_C32)#373

GLACIER_ids_op_C33<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% persistantly_lean_op_C33]#top 35% of environment  and are normal/underweight
length(GLACIER_ids_op_C33)#449

#construct new dataset with merged information from all

#first get all subject from vip subset that are in glacier 
subjects_total1<-VIP_data_subset_visit1_complete_cases$Subject_id[VIP_data_subset_visit1_complete_cases$enummer %in% link_data$enummer]

subjects_total2<-VIP_data_subset_visit2_complete_cases$Subject_id[VIP_data_subset_visit2_complete_cases$enummer %in% link_data$enummer]

subject_total<-intersect(subjects_total1,subjects_total2)

#get enummers also
enummers_total1<-VIP_data_subset_visit1_complete_cases$enummer[VIP_data_subset_visit1_complete_cases$Subject_id %in% subject_total]

enummers_total2<-VIP_data_subset_visit2_complete_cases$enummer[VIP_data_subset_visit2_complete_cases$Subject_id %in% subject_total]

#final dataset:

#glacier_id  persistant phenotype  identifierC1 identifierC2 ....
final_dataset<-data.frame(matrix(ncol=10,nrow=length(enummers_total1)))
colnames(final_dataset)<-c("glacier_ID", "pat_code_dna", "mean_age", "gender", "resistance_phenotype", "id_C1","id_C2","id_C31","id_C32","id_C33")

#id columns
final_dataset$glacier_ID<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% enummers_total1]
final_dataset$pat_code_dna<-link_data$pat_code_dna[!is.na(link_data$pat_code_dna) & link_data$enummer %in% enummers_total1]

#covariants
final_dataset$mean_age<-(VIP_data_subset_visit1_complete_cases$age[VIP_data_subset_visit1_complete_cases$Subject_id %in% subject_total]+
			VIP_data_subset_visit2_complete_cases$age[VIP_data_subset_visit2_complete_cases$Subject_id %in% subject_total])/2
final_dataset$gender<-VIP_data_subset_visit1_complete_cases$gender[VIP_data_subset_visit1_complete_cases$Subject_id %in% subject_total]

#make a new continous phenotype, taking the mean of the two phenotypes at each visit
final_dataset$resistance_phenotype<-(VIP_data_subset_visit1_complete_cases$continuous_lean_phenotype[VIP_data_subset_visit1_complete_cases$Subject_id %in% subject_total]+
			VIP_data_subset_visit2_complete_cases$continuous_lean_phenotype[VIP_data_subset_visit2_complete_cases$Subject_id %in% subject_total])/2

final_dataset$id_C1<-NA
final_dataset$id_C1[final_dataset$glacier_ID %in% GLACIER_ids_C1]<-1
final_dataset$id_C1[final_dataset$glacier_ID %in% GLACIER_ids_op_C1]<-0
 

final_dataset$id_C2<-NA
final_dataset$id_C2[final_dataset$glacier_ID %in% GLACIER_ids_C2]<-1
final_dataset$id_C2[final_dataset$glacier_ID %in% GLACIER_ids_op_C2]<-0


final_dataset$id_C31<-NA
final_dataset$id_C31[final_dataset$glacier_ID %in% GLACIER_ids_C31]<-1
final_dataset$id_C31[final_dataset$glacier_ID %in% GLACIER_ids_op_C31]<-0


final_dataset$id_C32<-NA
final_dataset$id_C32[final_dataset$glacier_ID %in% GLACIER_ids_C32]<-1
final_dataset$id_C32[final_dataset$glacier_ID %in% GLACIER_ids_op_C32]<-0


final_dataset$id_C33<-NA
final_dataset$id_C33[final_dataset$glacier_ID %in% GLACIER_ids_C33]<-1
final_dataset$id_C33[final_dataset$glacier_ID %in% GLACIER_ids_op_C33]<-0


#save the final dataset
write.csv(final_dataset, "../final_persistant_phenotype_dataset.csv", row.names=FALSE, na="")

