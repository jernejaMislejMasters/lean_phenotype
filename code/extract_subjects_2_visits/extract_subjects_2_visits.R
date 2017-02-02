VIP_data <- read.csv("VIP_161102.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)

#get only those that have the besok value
VIP_data_all_besok<-VIP_data[!is.na(VIP_data$besok),]



#merge by the subject id to get the difference in years for the besok==1 and besok==2 (THERE IS 36754)
besok_1_2_merged<-merge(VIP_data_all_besok[VIP_data_all_besok$besok==1,c(1,2,3)],
		VIP_data_all_besok[VIP_data_all_besok$besok==2,c(1,2,3)],by="Subject_id")

year_differences<-as.numeric(substr(besok_1_2_merged[,5],7,10))-
		as.numeric(substr(besok_1_2_merged[,3],7,10))

#take those that have 9,10,11 years difference
subject_id_9_to_11_year_difference<-besok_1_2_merged[year_differences<12 & year_differences>8,1]

#save the enummer to be able to access the right row
enummers_9_11_year_difference<-cbind(besok_1_2_merged[year_differences<12 & year_differences>8,2],besok_1_2_merged[year_differences<12 & year_differences>8,4])
colnames(enummers_9_11_year_difference)<-c("Enummer_visit1","Enummer_visit2")



#check for subjects that have around 10 years difference between the besok==2 and besok==3 and see if 
#there are any that are not in the first subset already(THERE IS 122)
besok_2_3_merged<-merge(VIP_data_all_besok[VIP_data_all_besok$besok==2,c(1,2,3)],
		VIP_data_all_besok[VIP_data_all_besok$besok==3,c(1,2,3)],by="Subject_id")

year_differences<-as.numeric(substr(besok_2_3_merged[,5],7,10))-
		as.numeric(substr(besok_2_3_merged[,3],7,10))

#take those that have 9,10,11 years difference, but are not already in the subset
subject_id_9_to_11_year_difference23<-besok_2_3_merged[year_differences<12 & year_differences>8,1]
enummers_9_11_year_difference23<-besok_2_3_merged[year_differences<12 & year_differences>8,c(2,4)]
colnames(enummers_9_11_year_difference23)<-c("Enummer_visit1","Enummer_visit2")

enummers_9_11_year_difference<-rbind(enummers_9_11_year_difference,
		enummers_9_11_year_difference23[!(subject_id_9_to_11_year_difference23 %in% subject_id_9_to_11_year_difference),])




#check for subjects that have around 10 years difference between the besok==1 and besok==3 and see if 
#there are any that are not in the first subset already(THERE IS 15)
besok_1_3_merged<-merge(VIP_data_all_besok[VIP_data_all_besok$besok==1,c(1,2,3)],
		VIP_data_all_besok[VIP_data_all_besok$besok==3,c(1,2,3)],by="Subject_id")

year_differences<-as.numeric(substr(besok_1_3_merged[,5],7,10))-
		as.numeric(substr(besok_1_3_merged[,3],7,10))

#take those that have 9,10,11 years difference, but are not already in the subset
subject_id_9_to_11_year_difference13<-besok_1_3_merged[year_differences<12 & year_differences>8,1]
enummers_9_11_year_difference13<-besok_2_3_merged[year_differences<12 & year_differences>8,c(2,4)]
colnames(enummers_9_11_year_difference13)<-c("Enummer_visit1","Enummer_visit2")

enummers_9_11_year_difference<-rbind(enummers_9_11_year_difference,
		enummers_9_11_year_difference13[!(subject_id_9_to_11_year_difference13 %in% subject_id_9_to_11_year_difference),])



#check for subjects that have around 10 years difference between the besok==2 and besok==4 and see if 
#there are any that are not in the first subset already (THERE IS NONE)
besok_2_4_merged<-merge(VIP_data_all_besok[VIP_data_all_besok$besok==2,c(1,2,3)],
		VIP_data_all_besok[VIP_data_all_besok$besok==4,c(1,2,3)],by="Subject_id")

year_differences<-as.numeric(substr(besok_2_4_merged[,5],7,10))-
		as.numeric(substr(besok_2_4_merged[,3],7,10))

#take those that have 9,10,11 years difference, but are not already in the subset
subject_id_9_to_11_year_difference24<-besok_2_4_merged[year_differences<12 & year_differences>8,1]
enummers_9_11_year_difference24<-besok_2_4_merged[year_differences<12 & year_differences>8,c(2,4)]
colnames(enummers_9_11_year_difference24)<-c("Enummer_visit1","Enummer_visit2")

enummers_9_11_year_difference<-rbind(enummers_9_11_year_difference,
		enummers_9_11_year_difference24[!(subject_id_9_to_11_year_difference24 %in% subject_id_9_to_11_year_difference),])



#check for subjects that have around 10 years difference between the besok==1 and besok==4 and see if 
#there are any that are not in the first subset already (THERE IS NONE)
besok_1_4_merged<-merge(VIP_data_all_besok[VIP_data_all_besok$besok==1,c(1,2,3)],
		VIP_data_all_besok[VIP_data_all_besok$besok==4,c(1,2,3)],by="Subject_id")

year_differences<-as.numeric(substr(besok_1_4_merged[,5],7,10))-
		as.numeric(substr(besok_1_4_merged[,3],7,10))

#take those that have 9,10,11 years difference, but are not already in the subset
subject_id_9_to_11_year_difference14<-besok_1_4_merged[year_differences<12 & year_differences>8,1]
enummers_9_11_year_difference14<-besok_1_4_merged[year_differences<12 & year_differences>8,c(2,4)]
colnames(enummers_9_11_year_difference14)<-c("Enummer_visit1","Enummer_visit2")

enummers_9_11_year_difference<-rbind(enummers_9_11_year_difference,
		enummers_9_11_year_difference14[!(subject_id_9_to_11_year_difference14 %in% subject_id_9_to_11_year_difference),])



#check for subjects that have around 10 years difference between the besok==3 and besok==4 and see if 
#there are any that are not in the first subset already (THERE IS ONLY ONE)
besok_3_4_merged<-merge(VIP_data_all_besok[VIP_data_all_besok$besok==3,c(1,2,3)],
		VIP_data_all_besok[VIP_data_all_besok$besok==4,c(1,2,3)],by="Subject_id")

year_differences<-as.numeric(substr(besok_3_4_merged[,5],7,10))-
		as.numeric(substr(besok_3_4_merged[,3],7,10))

#take those that have 9,10,11 years difference, but are not already in the subset
subject_id_9_to_11_year_difference34<-besok_3_4_merged[year_differences<12 & year_differences>8,1]
enummers_9_11_year_difference34<-besok_3_4_merged[year_differences<12 & year_differences>8,c(2,4)]
colnames(enummers_9_11_year_difference34)<-c("Enummer_visit1","Enummer_visit2")

enummers_9_11_year_difference<-rbind(enummers_9_11_year_difference,
		enummers_9_11_year_difference34[!(subject_id_9_to_11_year_difference34 %in% subject_id_9_to_11_year_difference),])
