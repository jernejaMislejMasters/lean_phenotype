VIP_data <- read.csv("VIP_data/VIP_170206_cleaned.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)

#get only those that have the besok1 value
VIP_data_all_besok1<-VIP_data[!is.na(VIP_data$besok1),]



#merge by the subject id to get the difference in years for the besok1==1 and besok1==2 
#(THERE WAS 36754 BEFORE IN VIP_161102.csv, NOW IN VIP_170206_cleaned.csv THERE IS 33906)
besok1_1_2_merged<-merge(VIP_data_all_besok1[VIP_data_all_besok1$besok1==1,c(1,3,4)],
		VIP_data_all_besok1[VIP_data_all_besok1$besok1==2,c(1,3,4)],by="Subject_id")

year_differences<-as.numeric(substr(besok1_1_2_merged[,5],7,10))-
		as.numeric(substr(besok1_1_2_merged[,3],7,10))

#take those that have 9,10,11 years difference
subject_id_9_to_11_year_difference<-besok1_1_2_merged[year_differences<12 & year_differences>8,1]

#save the enummer to be able to access the right row
enummers_9_11_year_difference<-cbind(besok1_1_2_merged[year_differences<12 & year_differences>8,2],besok1_1_2_merged[year_differences<12 & year_differences>8,4])
colnames(enummers_9_11_year_difference)<-c("Enummer_visit1","Enummer_visit2")



#check for subjects that have around 10 years difference between the besok1==2 and besok1==3 and see if 
#there are any that are not in the first subset already(BEFORE 122, NOW 584)
besok1_2_3_merged<-merge(VIP_data_all_besok1[VIP_data_all_besok1$besok1==2,c(1,3,4)],
		VIP_data_all_besok1[VIP_data_all_besok1$besok1==3,c(1,3,4)],by="Subject_id")

year_differences<-as.numeric(substr(besok1_2_3_merged[,5],7,10))-
		as.numeric(substr(besok1_2_3_merged[,3],7,10))

#take those that have 9,10,11 years difference, but are not already in the subset
subject_id_9_to_11_year_difference23<-besok1_2_3_merged[year_differences<12 & year_differences>8,1]
enummers_9_11_year_difference23<-besok1_2_3_merged[year_differences<12 & year_differences>8,c(2,4)]
colnames(enummers_9_11_year_difference23)<-c("Enummer_visit1","Enummer_visit2")

enummers_9_11_year_difference<-rbind(enummers_9_11_year_difference,
		enummers_9_11_year_difference23[!(subject_id_9_to_11_year_difference23 %in% subject_id_9_to_11_year_difference),])

#update the list of subject_ids
subject_id_9_to_11_year_difference<-c(subject_id_9_to_11_year_difference,
		subject_id_9_to_11_year_difference23[!(subject_id_9_to_11_year_difference23 %in% subject_id_9_to_11_year_difference)])


#check for subjects that have around 10 years difference between the besok1==1 and besok1==3 and see if 
#there are any that are not in the first subset already(BEFORE 15, NOW 8)
besok1_1_3_merged<-merge(VIP_data_all_besok1[VIP_data_all_besok1$besok1==1,c(1,3,4)],
		VIP_data_all_besok1[VIP_data_all_besok1$besok1==3,c(1,3,4)],by="Subject_id")

year_differences<-as.numeric(substr(besok1_1_3_merged[,5],7,10))-
		as.numeric(substr(besok1_1_3_merged[,3],7,10))

#take those that have 9,10,11 years difference, but are not already in the subset
subject_id_9_to_11_year_difference13<-besok1_1_3_merged[year_differences<12 & year_differences>8,1]
enummers_9_11_year_difference13<-besok1_1_3_merged[year_differences<12 & year_differences>8,c(2,4)]
colnames(enummers_9_11_year_difference13)<-c("Enummer_visit1","Enummer_visit2")

enummers_9_11_year_difference<-rbind(enummers_9_11_year_difference,
		enummers_9_11_year_difference13[!(subject_id_9_to_11_year_difference13 %in% subject_id_9_to_11_year_difference),])

#update the list of subject_ids
subject_id_9_to_11_year_difference<-c(subject_id_9_to_11_year_difference,
		subject_id_9_to_11_year_difference13[!(subject_id_9_to_11_year_difference13 %in% subject_id_9_to_11_year_difference)])


#check for subjects that have around 10 years difference between the besok1==2 and besok1==4 and see if 
#there are any that are not in the first subset already (BEFORE NONE, NOW NONE)
besok1_2_4_merged<-merge(VIP_data_all_besok1[VIP_data_all_besok1$besok1==2,c(1,3,4)],
		VIP_data_all_besok1[VIP_data_all_besok1$besok1==4,c(1,3,4)],by="Subject_id")

year_differences<-as.numeric(substr(besok1_2_4_merged[,5],7,10))-
		as.numeric(substr(besok1_2_4_merged[,3],7,10))

#take those that have 9,10,11 years difference, but are not already in the subset
subject_id_9_to_11_year_difference24<-besok1_2_4_merged[year_differences<12 & year_differences>8,1]
enummers_9_11_year_difference24<-besok1_2_4_merged[year_differences<12 & year_differences>8,c(2,4)]
colnames(enummers_9_11_year_difference24)<-c("Enummer_visit1","Enummer_visit2")

enummers_9_11_year_difference<-rbind(enummers_9_11_year_difference,
		enummers_9_11_year_difference24[!(subject_id_9_to_11_year_difference24 %in% subject_id_9_to_11_year_difference),])



#check for subjects that have around 10 years difference between the besok1==1 and besok1==4 and see if 
#there are any that are not in the first subset already (BEFORE NONE, NOW NONE)
besok1_1_4_merged<-merge(VIP_data_all_besok1[VIP_data_all_besok1$besok1==1,c(1,3,4)],
		VIP_data_all_besok1[VIP_data_all_besok1$besok1==4,c(1,3,4)],by="Subject_id")

year_differences<-as.numeric(substr(besok1_1_4_merged[,5],7,10))-
		as.numeric(substr(besok1_1_4_merged[,3],7,10))

#take those that have 9,10,11 years difference, but are not already in the subset
subject_id_9_to_11_year_difference14<-besok1_1_4_merged[year_differences<12 & year_differences>8,1]
enummers_9_11_year_difference14<-besok1_1_4_merged[year_differences<12 & year_differences>8,c(2,4)]
colnames(enummers_9_11_year_difference14)<-c("Enummer_visit1","Enummer_visit2")

enummers_9_11_year_difference<-rbind(enummers_9_11_year_difference,
		enummers_9_11_year_difference14[!(subject_id_9_to_11_year_difference14 %in% subject_id_9_to_11_year_difference),])



#check for subjects that have around 10 years difference between the besok1==3 and besok1==4 and see if 
#there are any that are not in the first subset already (BEFORE ONE, NOW NONE)
besok1_3_4_merged<-merge(VIP_data_all_besok1[VIP_data_all_besok1$besok1==3,c(1,3,4)],
		VIP_data_all_besok1[VIP_data_all_besok1$besok1==4,c(1,3,4)],by="Subject_id")

year_differences<-as.numeric(substr(besok1_3_4_merged[,5],7,10))-
		as.numeric(substr(besok1_3_4_merged[,3],7,10))

#take those that have 9,10,11 years difference, but are not already in the subset
subject_id_9_to_11_year_difference34<-besok1_3_4_merged[year_differences<12 & year_differences>8,1]
enummers_9_11_year_difference34<-besok1_3_4_merged[year_differences<12 & year_differences>8,c(2,4)]
colnames(enummers_9_11_year_difference34)<-c("Enummer_visit1","Enummer_visit2")

enummers_9_11_year_difference<-rbind(enummers_9_11_year_difference,
		enummers_9_11_year_difference34[!(subject_id_9_to_11_year_difference34 %in% subject_id_9_to_11_year_difference),])


#ALL TOGETHER = 34495
#save the list of enummers:
write.csv(enummers_9_11_year_difference, "Visit1_Visit2_enummers.csv", row.names=FALSE, na="")


#subset the data and remove besok and besok2 as only besok1 should be used, add a variable visit, so we know which visit is 
#regarded as the first and which second
VIP_data_subset<-rbind(VIP_data[VIP_data$enummer %in% enummers_9_11_year_difference[,1],],
		VIP_data[VIP_data$enummer %in% enummers_9_11_year_difference[,2],])

VIP_data_subset <- within(VIP_data_subset, rm(besok,besok2))

#add extra variable visit
VIP_data_subset$visit<-1
VIP_data_subset$visit[VIP_data_subset$enummer %in% enummers_9_11_year_difference[,2]]<-2

#still having 2 more variables, which are the date and efter_090901, which might be good to keep
#save the subset
write.csv(VIP_data_subset, "VIP_data/VIP_170206_cleaned_subset.csv", row.names=FALSE, na="")

