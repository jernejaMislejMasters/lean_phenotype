VIP_data <- read.csv("VIP_161102.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)

VIP_enummer_missing_besok<-VIP_data[is.na(VIP_data$besok),2]


missing_besoks_ids<-VIP_data$Subject_id[is.na(VIP_data$besok)][!duplicated(VIP_data$Subject_id[is.na(VIP_data$besok)])]

for (subject_id in 1:length(missing_besoks_ids)){
	
	besoks<-VIP_data[VIP_data$Subject_id==missing_besoks_ids[subject_id],228]
	
	datums<-as.numeric(substr(VIP_data[VIP_data$Subject_id==missing_besoks_ids[subject_id],3],7,10))
	
	write(paste0(missing_besoks_ids[subject_id],":",paste0(besoks[sort(datums, index.return=TRUE)$ix],collapse="")), file="temporal_checking_missing_besok", append = TRUE, sep = ",")
	
	
}


#with the temporal file do : 

#jerneja_m@purple Private]$ cat temporal_checking_missing_besok | cut -d":" -f2| sort | uniq -c

#results, NA is for missing besok:


#      1 123NA
#   3813 12NA
#   3722 1NA
#      1 1NANA
#      6 2NA
#  10415 NA
#   3359 NA1
#   2268 NA12
#      3 NA123
#     27 NA12NA
#     58 NA1NA
#     18 NA2
#     16 NA23
#      1 NA2NANA
#     26 NANA
#      2 NANA1
#      2 NANA12

