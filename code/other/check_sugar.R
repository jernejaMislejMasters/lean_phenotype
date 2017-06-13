library(nnet)
library(stargazer)
library(pROC)
library(car)

#load entire cleaned data (154009 subjects)
VIP_data_all <- read.csv("../VIP_data/VIP_170206_cleaned.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)

#load the subset
VIP_data_subset <- read.csv("../VIP_data/VIP_170206_cleaned_subset.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)
attach(VIP_data_subset)
#extract only swedish....66228 (33114 for each visit)
#get those that have the ursprungsland in both visits and the value is 1 for Swedish
VIP_data_subset<-VIP_data_subset[!is.na(ursprungsland[visit==1]) & !is.na(ursprungsland[visit==2]),]
VIP_data_subset<-VIP_data_subset[VIP_data_subset$ursprungsland==1,]
VIP_data_subset_visit1<-VIP_data_subset[VIP_data_subset$visit==1,]
VIP_data_subset_visit2<-VIP_data_subset[VIP_data_subset$visit==2,]
detach(VIP_data_subset)

#create the independant dataset of the first visit that is not in the subset already with Swedish only
attach(VIP_data_all)
VIP_data_independant<-VIP_data_all[!is.na(besok1) & besok1==1 & !(Subject_id %in% VIP_data_subset$Subject_id) & !is.na(ursprungsland) & ursprungsland==1, ]
detach(VIP_data_all)
#length(VIP_data_independant[,1])#....47107

#load all variables, adjusted for TEI

#independent
source(file="load_variables_independent_dataset_TEI_adjusted.R")

attach(VIP_data_independant)

#sugar
#long ffq: da14 coffe buns rusk, da28 cornflakes, da48 pancakes vaffles, da65 icecream, da66 sweets and chocolate, da67 sugar honey, da 68 jam, da69 cookies, da74 soft drinks, da75 sodas, da 76 juice
#short ffq: dat13 coffe buns rusk, dat23 cornflakes, DAT35  pancakes vaffles, dat48 icecream, dat49 sweets and chocolate, DAT50 sugar honey jam, dat51 cookies, dat 56 soft drinks sodas juice

VIP_data_independant$cornflakes[ffq==1]<-gramlong28[ffq==1]
VIP_data_independant$cornflakes[ffq==0]<-gramshort23[ffq==0]

VIP_data_independant$pancakes_vaffles[ffq==1]<-gramlong48[ffq==1]
VIP_data_independant$pancakes_vaffles[ffq==0]<-gramshort35[ffq==0]

VIP_data_independant$icecream[ffq==1]<-gramlong65[ffq==1]
VIP_data_independant$icecream[ffq==0]<-gramshort48[ffq==0]

VIP_data_independant$sweets_chocolate[ffq==1]<-gramlong66[ffq==1]
VIP_data_independant$sweets_chocolate[ffq==0]<-gramshort49[ffq==0]

VIP_data_independant$sugar_honey_jam[ffq==1]<-apply(cbind(gramlong67,gramlong68),1,FUN=sum,na.rm=TRUE)[ffq==1]
VIP_data_independant$sugar_honey_jam[ffq==0]<-gramshort50[ffq==0]

VIP_data_independant$cookies[ffq==1]<-gramlong69[ffq==1]
VIP_data_independant$cookies[ffq==0]<-gramshort51[ffq==0]

VIP_data_independant$softdrinks_sodas_juice[ffq==1]<-apply(cbind(gramlong74,gramlong75,gramlong76),1,FUN=sum,na.rm=TRUE)[ffq==1]
VIP_data_independant$softdrinks_sodas_juice[ffq==0]<-gramshort56[ffq==0]

VIP_data_independant$fruit[ffq==0]<-apply(cbind(gramshort24,gramshort25,gramshort26),1,FUN=sum,na.rm=TRUE)[ffq==0]
VIP_data_independant$fruit[ffq==1]<-apply(cbind(gramlong29, gramlong30,gramlong31,gramlong32),1,FUN=sum,na.rm=TRUE)[ffq==1]

VIP_data_independant$vegetables[ffq==0]<-apply(cbind(gramshort27,gramshort28,gramshort29,gramshort34),1,FUN=sum,na.rm=TRUE)[ffq==0]
VIP_data_independant$vegetables[ffq==1]<-apply(cbind(gramlong33,gramlong34,gramlong35,gramlong36,gramlong37,gramlong38, gramlong46),1,FUN=sum,na.rm=TRUE)[ffq==1]

VIP_data_independant$sugar_food[ffq==0]<-apply(cbind(gramshort23,gramshort35,gramshort48,gramshort49,gramshort50,gramshort51,gramshort56),1,FUN=sum,na.rm=TRUE)[ffq==0]
VIP_data_independant$sugar_food[ffq==1]<-apply(cbind(gramlong28,gramlong48,gramlong65,gramlong66,gramlong67,gramlong68,gramlong69,gramlong74,gramlong75,gramlong76),1,FUN=sum,na.rm=TRUE)[ffq==1]


detach(VIP_data_independant)

complete_data<-na.omit(VIP_data_independant[,c("age","agesq","year","ffq","gender","bmi","cornflakes","pancakes_vaffles","icecream","sweets_chocolate","sugar_honey_jam","cookies","softdrinks_sodas_juice","fruit","vegetables")])

attach(complete_data)
for (variable in 7:length(complete_data[1,])){
	
	partial_corr<-pcor.test(scale(bmi),complete_data[,variable],
			complete_data[,c("age","agesq","year","ffq","gender",colnames(complete_data)[c(-1,-variable)])])
	message(paste0(colnames(complete_data)[variable],"  R : ",round(partial_corr[[1]],3)," , p-value : ",round(partial_corr[[2]],3)))
	
}
detach(complete_data)

complete_data<-na.omit(VIP_data_independant[,c("age","agesq","year","ffq","gender","bmi","fruit","vegetables","sugar_food")])
attach(complete_data)
partial_corr<-pcor.test(scale(bmi),sugar_food,cbind(age,agesq,year,ffq,gender,fruit,vegetables))
detach(complete_data)



complete_data<-na.omit(VIP_data_independant[,c("age","agesq","year","ffq","gender","bmi","cornflakes","pancakes_vaffles","icecream","sweets_chocolate","sugar_honey_jam","cookies","softdrinks_sodas_juice","fruit","vegetables")])

attach(complete_data)
for (variable in 7:length(complete_data[1,])){
	
	partial_corr<-pcor.test(bmi,complete_data[,variable],
			complete_data[,c("age","agesq","year","ffq","gender",colnames(complete_data)[c(-1,-variable)])], method="spearman")
	message(paste0(colnames(complete_data)[variable],"  R : ",round(partial_corr[[1]],3)," , p-value : ",round(partial_corr[[2]],3)))
	
}
detach(complete_data)

complete_data<-na.omit(VIP_data_independant[,c("age","agesq","year","ffq","gender","bmi","fruit","vegetables","sugar_food")])
attach(complete_data)
partial_corr<-pcor.test(bmi,sugar_food,cbind(age,agesq,year,ffq,gender,fruit,vegetables), method="spearman")
detach(complete_data)


complete_data<-na.omit(VIP_data_independant[,c("age","agesq","year","ffq","gender","bmi","sacksum1","fruit","vegetables")])
attach(complete_data)
partial_corr<-pcor.test(scale(sacksum1),fruit,cbind(age,agesq,year,ffq,gender))
detach(complete_data)


attach(VIP_data_independant)
VIP_data_independant$sugar_food2[ffq==0]<-apply(cbind(gramshort35,gramshort56),1,FUN=sum,na.rm=TRUE)[ffq==0]
VIP_data_independant$sugar_food2[ffq==1]<-apply(cbind(gramlong48,gramlong74,gramlong75,gramlong76),1,FUN=sum,na.rm=TRUE)[ffq==1]
detach(VIP_data_independant)

complete_data<-na.omit(VIP_data_independant[,c("age","agesq","year","ffq","gender","bmi","sugar_food2")])


attach(complete_data)
partial_corr<-pcor.test(scale(bmi),sugar_food2,cbind(age,agesq,year,ffq,gender))


 
associations<-glm(bmi_norm_sd~age + agesq + gender_factor + year + ffq_factor + sugar_food_norm_sd, family = gaussian(link = "identity"))

summary(associations)
vif(associations)



associations<-glm(bmi_norm_sd~age + agesq + gender_factor + year + ffq_factor + sugar_food_norm_sd + fruit_norm_sd + vegetables_norm_sd, family = gaussian(link = "identity"))

summary(associations)
vif(associations)

