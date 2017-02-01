vip <- read.table ("/Volumes/DISCOENCR/BMI_bimodality/VIP/VIP_161102.csv", header=TRUE, sep=',', fileEncoding="latin1")
vip$date <- as.Date(vip$datum, format="%m/%d/%Y")
#Check participants missing besok but having q_date in which visit do they have the q_date.
vip_rd <- vip [c("Subject_id", "gender","besok", "age","date")] 
attach(vip_rd)
vip_rd_sort <- vip_rd[order(Subject_id,date),]
detach(vip_rd)

vip_rdna <- vip_rd[which(is.na(vip_rd$besok)),]#N= 23856
vip_rd1 <- vip_rd[which(vip_rd$besok==1),]#N= 100836
vip_rd2 <- vip_rd[which(vip_rd$besok==2),]#N= 38509
vip_rd3 <- vip_rd[which(vip_rd$besok==3),]#N= 5126
vip_rd4 <- vip_rd[which(vip_rd$besok==4),]#N= 3

#change names in each visit
colnames(vip_rdna) <- c("Subject_id", "genderna","besokna", "agena","datena") 
colnames(vip_rd1) <- c("Subject_id", "gender1","besok1", "age1","date1") 
colnames(vip_rd2) <- c("Subject_id", "gender2","besok2","age2","date2")
colnames(vip_rd3) <- c("Subject_id", "gender3","besok3","age3","date3")
colnames(vip_rd4) <- c("Subject_id", "gender4","besok4","age4","date4")

#merge the visits
vip_rd_merge1 <- merge(vip_rdna,vip_rd1, by=c("Subject_id"), all=TRUE) #merges datasets with each visit on id 
vip_rd_merge2 <- merge(vip_rd_merge1,vip_rd2, by=c("Subject_id"), all=TRUE) #merges datasets with each visit on id 
vip_rd_merge3 <- merge(vip_rd_merge2,vip_rd3, by=c("Subject_id"), all=TRUE) #merges datasets with each visit on id 
vip_rd_merge <- merge(vip_rd_merge3,vip_rd4, by=c("Subject_id"), all=TRUE) #merges datasets with each visit on id 

#exlcude participants with no na visit N=87767
vip_noNA <- subset(vip_rd_merge, !is.na(vip_rd_merge$datena)) 

#participants with 4 visit and NA N=0
vip_4 <- subset(vip_noNA, vip_noNA$besok4==4)
#participants with 3 visit and NA N= 20. All except for 1 (id=51600) is the first visit missing
vip_3 <- subset(vip_noNA, vip_noNA$besok3==3)
#participants with 2 visits and NA N= 6166. 3848 have NA visit after besok=2 and 8 have the NA visit between the first and the second visit.
vip_2 <- subset(vip_noNA, is.na(vip_noNA$besok3) & vip_noNA$besok2==2)
summary(vip_2$follow1na)
vip_2$follow21<- vip_2$age2 - vip_2$age1
vip_2$follow2na<- vip_2$age2 - vip_2$agena
vip_2$follow1na<- vip_2$age1 - vip_2$agena

vip_2_2na <- subset(vip_2, vip_2$follow2na<=0)
vip_2_1na <- subset(vip_2_2na, vip_2_2na$follow1na<=0)
#participants with 1 visits and NA N= 7203. 3782 have NA visit after besok=1
vip_1 <- subset(vip_noNA, is.na(vip_noNA$besok3) & is.na(vip_noNA$besok2) & vip_noNA$besok1==1)

vip_1$follow1na<- vip_1$age1 - vip_1$agena
summary(vip_1$follow1na)
vip_1_1na <- subset(vip_1, vip_1$follow1na<=0)

