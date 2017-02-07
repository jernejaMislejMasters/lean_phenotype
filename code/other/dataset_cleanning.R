#IMPORT FILE
vip <- read.table ("/Volumes/DISCOENCR/BMI_bimodality/VIP/VIP_161102.csv", header=TRUE, sep=',', fileEncoding="latin1")
vip$date <- as.Date(vip$datum, format="%m/%d/%Y")
summary(vip)
#MERGE NEW BESOKS WITH OLD DATASET
#read correction files and change date format
besok1 <- read.table ("/Volumes/DISCOENCR/BMI_bimodality/VIP/multimodal_besok_170202B.csv", header=TRUE, sep=',', fileEncoding="latin1")
besok1$date <- as.Date(besok1$datum2, format="%Y/%m/%d")
besok2 <- read.table ("/Volumes/DISCOENCR/BMI_bimodality/VIP/multimodal_besok_170202.csv", header=TRUE, sep=',', fileEncoding="latin1")
besok2$date <- as.Date(besok2$datum2, format="%Y/%m/%d")
#merge the two new besoks with old dataset
#merge besok1 with VIP
vip.besok1 <- merge(vip, besok1, by=c("Subject_id","date"), all=TRUE)#merge
summary(vip.besok1)
#drop variables that are not needed
vip.besok1 <- within(vip.besok1, rm(datum.y, datum2, datum3, kostdata.y))
#change names
which(colnames(vip.besok1)=="kostdata.x")
which(colnames(vip.besok1)=="datum.x")
which(colnames(vip.besok1)=="besok2")

colnames(vip.besok1)[606] <- "kostdata"
colnames(vip.besok1)[4] <- "datum"
colnames(vip.besok1)[607] <- "besok1"

#merge besok2 with vip.besok1
vip.besok2 <- merge(vip.besok1, besok2, by=c("Subject_id","date"), all=TRUE)#merge
summary(vip.besok2)
#drop variables that are not needed
vip.besok2 <- within(vip.besok2, rm(datum.y, datum2, datum3))
#change names
which(colnames(vip.besok2)=="datum.x")
colnames(vip.besok2)[4] <- "datum"

#Check that all participants with missing besok1 have no diet data. Checked.
pr <- subset(vip.besok2, is.na(vip.besok2$besok1) & !is.na(vip.besok2$FIL))
pr <- subset(vip.besok2, is.na(vip.besok2$besok1) & !is.na(vip.besok2$ensum1))
#Check how many participants with data on besok1 have no diet data (N= 5633 FIL/5292 ensum1/0 exclude)
pr <- subset(vip.besok2, !is.na(vip.besok2$besok1) & is.na(vip.besok2$exclude))
pr <- subset(vip.besok2, !is.na(vip.besok2$besok1) & is.na(vip.besok2$ensum1))
#Check how mant participants without besok1 (now 11606, before 23862)
summary(vip.besok2$besok)
#Check participants missing besok but having q_date in which visit do they have the q_date.
vip.besok_rd <- vip.besok2 [c("Subject_id", "gender","besok1", "age","date")] 
attach(vip.besok_rd)
vip.besok_rd_sort <- vip.besok_rd[order(Subject_id,date),]
detach(vip.besok_rd)

vip.besok_rdna <- vip.besok_rd[which(is.na(vip.besok_rd$besok1)),]#N= 11606
vip.besok_rd1 <- vip.besok_rd[which(vip.besok_rd$besok1==1),]#N= 106675
vip.besok_rd2 <- vip.besok_rd[which(vip.besok_rd$besok1==2),]#N= 41578
vip.besok_rd3 <- vip.besok_rd[which(vip.besok_rd$besok1==3),]#N= 8485
vip.besok_rd4 <- vip.besok_rd[which(vip.besok_rd$besok==4),]#N= 4

#change names in each visit
colnames(vip.besok_rdna) <- c("Subject_id", "genderna","besok1na", "agena","datena") 
colnames(vip.besok_rd1) <- c("Subject_id", "gender1","besok11", "age1","date1") 
colnames(vip.besok_rd2) <- c("Subject_id", "gender2","besok12","age2","date2")
colnames(vip.besok_rd3) <- c("Subject_id", "gender3","besok13","age3","date3")
colnames(vip.besok_rd4) <- c("Subject_id", "gender4","besok14","age4","date4")

#merge the visits
vip.besok_rd_merge1 <- merge(vip.besok_rdna,vip.besok_rd1, by=c("Subject_id"), all=TRUE) #merges datasets with each visit on id 
vip.besok_rd_merge2 <- merge(vip.besok_rd_merge1,vip.besok_rd2, by=c("Subject_id"), all=TRUE) #merges datasets with each visit on id 
vip.besok_rd_merge3 <- merge(vip.besok_rd_merge2,vip.besok_rd3, by=c("Subject_id"), all=TRUE) #merges datasets with each visit on id 
vip.besok_rd_merge <- merge(vip.besok_rd_merge3,vip.besok_rd4, by=c("Subject_id"), all=TRUE) #merges datasets with each visit on id 

#exlcude participants with no na visit 
vip.besok_noNA <- subset(vip.besok_rd_merge, !is.na(vip.besok_rd_merge$datena)) 

#participants with 4 visit and NA N=0
vip.besok_4 <- subset(vip.besok_noNA, vip.besok_noNA$besok14==4)
#participants with 3 visit and NA N= 25. For all of them the NA visit is the first when no diet questionaire existed
vip.besok_3 <- subset(vip.besok_noNA, vip.besok_noNA$besok13==3)
#participants with 2 visits and NA N= 2734. 388 have NA visit after besok=2 which I think it is a problem of updating the dataset since all of them are afetr 2013
#2346 have NA visit before besok=1 which is because there was no diet questionnaire only lifestyle questionnaire
vip.besok_2 <- subset(vip.besok_noNA, is.na(vip.besok_noNA$besok13) & vip.besok_noNA$besok12==2)
summary(vip.besok_2$follow1na)
vip.besok_2$follow21<- vip.besok_2$age2 - vip.besok_2$age1
vip.besok_2$follow2na<- vip.besok_2$age2 - vip.besok_2$agena
vip.besok_2$follow1na<- vip.besok_2$age1 - vip.besok_2$agena

vip.besok_2_2na <- subset(vip.besok_2, vip.besok_2$follow2na<=0)
vip.besok_2_1na <- subset(vip.besok_2_2na, vip.besok_2_2na$follow1na<=0)
vip.besok_2_2naplus <- subset(vip.besok_2, vip.besok_2$follow2na>=0)
summary(vip.besok_2_1na$datena)
#participants with 1 visits and NA N= 4009. 599 have NA visit after besok=1. All of them after 2011, only 9 of them before 2013 and one of the subjects has 8 equal rows
vip.besok_1 <- subset(vip.besok_noNA, is.na(vip.besok_noNA$besok13) & is.na(vip.besok_noNA$besok12) & vip.besok_noNA$besok11==1)

vip.besok_1$follow1na<- vip.besok_1$age1 - vip.besok_1$agena
summary(vip.besok_1$follow1na)
vip.besok_1_1na <- subset(vip.besok_1, vip.besok_1$follow1na<=0)
summary(vip.besok_1_1na$datena)

#export dataset
write.csv(vip.besok2, "/Volumes/DISCOENCR/BMI_bimodality/VIP/VIP_170206.csv", row.names=FALSE, na="")
