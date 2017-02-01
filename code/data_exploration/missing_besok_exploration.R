



VIP_data <- read.csv("VIP_161102.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)

VIP_data_missing_besok<-VIP_data[is.na(VIP_data$besok),]
length(VIP_data_missing_besok[,1])
#23856
VIP_data_not_missing_besok<-VIP_data[!is.na(VIP_data$besok),]

#while this shows:
#[jerneja_m@purple Private]$ cat VIP_161102.csv | cut -d"," -f228 | sort -n | uniq -c 
#23858 
#1 			besok
#100835 	1
#38506 		2
#5126 		3
#3 			4
#2 			8888 ....when searching for these two in R with besok, they are not there, they have values 1,2 for besok, looks like
                    # the lines are shifted right


repeated_subjects<-aggregate(enummer~Subject_id, data=VIP_data_missing_besok, FUN=length)


length(repeated_subjects[repeated_subjects[,2]==1,1])
#23621
length(repeated_subjects[repeated_subjects[,2]==2,1])
#116
length(repeated_subjects[repeated_subjects[,2]==3,1])
#1
length(repeated_subjects[repeated_subjects[,2]==4,1])
#0


#when looking at the besok for those Subject_id that are repeated 4 times, usually two of the "visits" will be missing the besok value.

#23621+2*116+3*1
#23856 

#while this again shows three more:
#[jerneja_m@purple Private]$ cat VIP_161102.csv | cut -d"," -f1,228 | grep -v ",8888" | grep -v ",1" | grep -v ",2" | grep -v ",3" | grep -v ",4"| cut -d"," -f1 | sort -n | uniq -c | cut -d" " -f7| sort -n | uniq -c
#23624 	1
#116 	2
#1 		3


#get the amount of missing data
VIP_data_missing_besok_copy<-VIP_data_missing_besok

for (variable in c(4,6:length(colnames(VIP_data_missing_besok)))) {
	
	missing_values=length(VIP_data_missing_besok[is.na(VIP_data_missing_besok[,variable]),1])
	non_missing_VIP_data_missing_besok=VIP_data_missing_besok[!is.na(VIP_data_missing_besok[,variable]),variable]
	missing_values=missing_values+length(non_missing_VIP_data_missing_besok[non_missing_VIP_data_missing_besok=='5555'])+
			length(non_missing_VIP_data_missing_besok[non_missing_VIP_data_missing_besok=='6666'])+
			length(non_missing_VIP_data_missing_besok[non_missing_VIP_data_missing_besok=='7777'])+
			length(non_missing_VIP_data_missing_besok[non_missing_VIP_data_missing_besok=='8888'])+
			length(non_missing_VIP_data_missing_besok[non_missing_VIP_data_missing_besok=='9999'])
	message(paste(colnames(VIP_data_missing_besok)[variable],":",missing_values, "missing values "))

}
	

#all of those that are missing the besok value, had been questioned with the short questionare and within those 11600 are missing most of the diet
#variable values




occurences<-aggregate(1:length(VIP_data_missing_besok$datum)~substr(VIP_data_missing_besok$datum,7,10),FUN=length)
colnames(occurences)<-c("year","number of subjects")
occurences


occurences<-aggregate(1:length(VIP_data_not_missing_besok$datum[VIP_data_not_missing_besok$besok==1])~substr(VIP_data_not_missing_besok$datum[VIP_data_not_missing_besok$besok==1],7,10),FUN=length)
colnames(occurences)<-c("year","number of subjects")
occurences

occurences<-aggregate(1:length(VIP_data_not_missing_besok$datum[VIP_data_not_missing_besok$besok==2])~substr(VIP_data_not_missing_besok$datum[VIP_data_not_missing_besok$besok==2],7,10),FUN=length)
colnames(occurences)<-c("year","number of subjects")
occurences

occurences<-aggregate(1:length(VIP_data_not_missing_besok$datum[VIP_data_not_missing_besok$besok==3])~substr(VIP_data_not_missing_besok$datum[VIP_data_not_missing_besok$besok==3],7,10),FUN=length)
colnames(occurences)<-c("year","number of subjects")
occurences

occurences<-aggregate(1:length(VIP_data_not_missing_besok$datum[VIP_data_not_missing_besok$besok==4])~substr(VIP_data_not_missing_besok$datum[VIP_data_not_missing_besok$besok==4],7,10),FUN=length)
colnames(occurences)<-c("year","number of subjects")
occurences
	

#1-2
year_differences<-as.numeric(substr(merge(VIP_data_not_missing_besok[VIP_data_not_missing_besok$besok==1,c(1,3)],
								VIP_data_not_missing_besok[VIP_data_not_missing_besok$besok==2,c(1,3)],by="Subject_id")[,3],7,10))-
		as.numeric(substr(merge(VIP_data_not_missing_besok[VIP_data_not_missing_besok$besok==1,c(1,3)],
								VIP_data_not_missing_besok[VIP_data_not_missing_besok$besok==2,c(1,3)],by="Subject_id")[,2],7,10))
count_year_differences<-aggregate(1:length(year_differences)~year_differences,FUN=length)
count_year_differences



#2-3
year_differences<-as.numeric(substr(merge(VIP_data_not_missing_besok[VIP_data_not_missing_besok$besok==2,c(1,3)],
								VIP_data_not_missing_besok[VIP_data_not_missing_besok$besok==3,c(1,3)],by="Subject_id")[,3],7,10))-
		as.numeric(substr(merge(VIP_data_not_missing_besok[VIP_data_not_missing_besok$besok==2,c(1,3)],
								VIP_data_not_missing_besok[VIP_data_not_missing_besok$besok==3,c(1,3)],by="Subject_id")[,2],7,10))
count_year_differences<-aggregate(1:length(year_differences)~year_differences,FUN=length)
count_year_differences


#3-4
year_differences<-as.numeric(substr(merge(VIP_data_not_missing_besok[VIP_data_not_missing_besok$besok==3,c(1,3)],
								VIP_data_not_missing_besok[VIP_data_not_missing_besok$besok==4,c(1,3)],by="Subject_id")[,3],7,10))-
		as.numeric(substr(merge(VIP_data_not_missing_besok[VIP_data_not_missing_besok$besok==3,c(1,3)],
								VIP_data_not_missing_besok[VIP_data_not_missing_besok$besok==4,c(1,3)],by="Subject_id")[,2],7,10))
count_year_differences<-aggregate(1:length(year_differences)~year_differences,FUN=length)
count_year_differences


#1-3
year_differences<-as.numeric(substr(merge(VIP_data_not_missing_besok[VIP_data_not_missing_besok$besok==1,c(1,3)],
								VIP_data_not_missing_besok[VIP_data_not_missing_besok$besok==3,c(1,3)],by="Subject_id")[,3],7,10))-
		as.numeric(substr(merge(VIP_data_not_missing_besok[VIP_data_not_missing_besok$besok==1,c(1,3)],
								VIP_data_not_missing_besok[VIP_data_not_missing_besok$besok==3,c(1,3)],by="Subject_id")[,2],7,10))
count_year_differences<-aggregate(1:length(year_differences)~year_differences,FUN=length)
count_year_differences


#1-4
year_differences<-as.numeric(substr(merge(VIP_data_not_missing_besok[VIP_data_not_missing_besok$besok==1,c(1,3)],
								VIP_data_not_missing_besok[VIP_data_not_missing_besok$besok==4,c(1,3)],by="Subject_id")[,3],7,10))-
		as.numeric(substr(merge(VIP_data_not_missing_besok[VIP_data_not_missing_besok$besok==1,c(1,3)],
								VIP_data_not_missing_besok[VIP_data_not_missing_besok$besok==4,c(1,3)],by="Subject_id")[,2],7,10))
count_year_differences<-aggregate(1:length(year_differences)~year_differences,FUN=length)
count_year_differences


#2-4
year_differences<-as.numeric(substr(merge(VIP_data_not_missing_besok[VIP_data_not_missing_besok$besok==2,c(1,3)],
								VIP_data_not_missing_besok[VIP_data_not_missing_besok$besok==4,c(1,3)],by="Subject_id")[,3],7,10))-
		as.numeric(substr(merge(VIP_data_not_missing_besok[VIP_data_not_missing_besok$besok==2,c(1,3)],
								VIP_data_not_missing_besok[VIP_data_not_missing_besok$besok==4,c(1,3)],by="Subject_id")[,2],7,10))
count_year_differences<-aggregate(1:length(year_differences)~year_differences,FUN=length)
count_year_differences




