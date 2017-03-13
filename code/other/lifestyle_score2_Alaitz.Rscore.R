#IMPORT FILE load cleaned subset of subjects with the right two visits
vip <- read.table ("/Volumes/DISCOENCR/BMI_bimodality/VIP/VIP_170206_cleaned_subset.csv", header=TRUE, sep=',', fileEncoding="latin1")
summary(vip)

#variable transformation
vip$date <- as.Date(vip$datum, format="%m/%d/%Y")
vip$year <- as.numeric(format(vip$date,"%Y"))
vip$agesq <- vip$age * vip$age
vip$ffq[vip$enkver2 == "apri"]  <- 1 
vip$ffq[vip$enkver2 == "long"]  <- 1 
vip$ffq[vip$enkver2 == "short"]  <- 0
vip$ffq<-as.factor(vip$ffq)
vip$gender<-as.factor(vip$gender)
vip$fa <- vip$FA182_sum1 + vip$FA183_sum1 + vip$FA204_sum1 + vip$FA205_sum1 + vip$FA226_sum1

#MACRONUTRIENTS
#%E
#protein
vip$prote <- (100*4*vip$protsum1)/vip$ensum1
vip$protane <- (100*4*vip$protsum1_anim)/vip$ensum1
vip$protvege <- (100*4*vip$protsum1_veg)/vip$ensum1

#carbohydrates and sugars
vip$che <- (100*4*vip$kolhsum1)/vip$ensum1
vip$suge <- (100*3.94*vip$sacksum1)/vip$ensum1
vip$disae <- (100*4*vip$DISAsum1)/vip$ensum1
vip$mosae <- (100*4*vip$MOSAsum1)/vip$ensum1
vip$fullke <- (100*3*vip$FULLKsum1)/vip$ensum1
#vip$fibere <- (100*2*vip$fibesum1)/vip$ensum1

#fat
vip$fate <- (100*9*vip$fettsum1)/vip$ensum1
vip$satfe <- (100*9*vip$mfetsum1)/vip$ensum1
vip$pufae <- (100*9*vip$POLYsum1)/vip$ensum1
vip$mufae <- (100*9*vip$MONOsum1)/vip$ensum1
vip$transe <- (100*9*vip$TRANSsum1)/vip$ensum1
vip$chole <- (100*9*vip$kolesum1)/vip$ensum1
vip$fae <- (100*9*vip$fa)/vip$ensum1


#divide baseline and follow-up
#vip.bas <- subset(vip, vip$visit==1) 
#vip.fup <- subset(vip, vip$visit==2)

#linear regressions to test direction of association
diete <- as.vector(c("prote", "protane", "protvege", "che", "suge", "disae", "mosae", "fibesum1", "fullke","fate", "satfe", "mufae", "pufae", "transe", "chole", "fae"))

diet.e <- lapply(diete, function(x) { glm(substitute(bmi ~ age + agesq + gender +  year + ffq + i, list(i = as.name(x))), family=gaussian(link="identity"), data = vip)
})

sink('/Users/alaitzpoveda/Documents/GAME 2017/Unhealthy_lean-Jerneja/results/diet.e.txt')
lapply(diet.e, summary)
sink()

#all combined
all.bas <- glm(bmi ~ age + agesq + gender + year + ffq + prote + protane + protvege + che + suge + disae + mosae + fibere + fullke + fate + 
      satfe + mufae + pufae + transe + chole + fae, family=gaussian(link="identity"), data=vip.bas)
summary(all.bas)



#CATEGORIES based on guidelines thresholds. If no threshold try with quartiles.
#protein
vip$catpro[vip$prote < 10] <- 0
vip$catpro[vip$prote >= 10 & vip$prote <= 20] <- 1
vip$catpro[vip$prote > 20] <- 2

#ch
#vip$catch[vip$che < 45] <- 0
#vip$catch[vip$che >=45 & vip$che<= 60] <- 1
#vip$catch[vip$che>60] <- 2

#fiber
#vip$catfib[vip$fibesum1 < 25] <- 0
#vip$catfib[vip$fibesum1 >= 25 & vip$fibesum1 >= 35] <- 1
#vip$catfib[vip$fibesum1 >= 35] <- 2

#total fat
vip$catfat[vip$fate < 25] <- 0
vip$catfat[vip$fate >= 25 & vip$fate <= 40] <- 1
vip$catfat[vip$fate > 40] <- 2

#mufa
vip$catmufa[vip$mufae < 10] <- 0
vip$catmufa[vip$mufae >= 10 & vip$mufae <= 20] <- 1
vip$catmufa[vip$mufae > 20] <- 2

#pufa
vip$catpufa[vip$pufae < 5] <- 0
vip$catpufa[vip$pufae >= 5 & vip$pufae <= 10] <- 1
vip$catpufa[vip$pufae > 10] <- 2

#for those where there are no guide lines, get quartiles/tertiles and rank accordingly

#total energy intake
vip$catensum1[(vip$ensum1<quantile(vip$ensum1, prob = c(0.33, 0.66, 1),na.rm=TRUE)[[1]])] <-0
vip$catensum1[(vip$ensum1>=quantile(vip$ensum1, prob = c(0.33, 0.66, 1),na.rm=TRUE)[[1]]) & (vip$ensum1<quantile(vip$ensum1, prob = c(0.33, 0.66, 1),na.rm=TRUE)[[2]])] <- 1
vip$catensum1[(vip$ensum1>=quantile(vip$ensum1, prob = c(0.33, 0.66, 1),na.rm=TRUE)[[2]])] <-2

#animal protein
vip$catprotane[(vip$protane<quantile(vip$protane, prob = c(0.33, 0.66, 1),na.rm=TRUE)[[1]])] <-0
vip$catprotane[(vip$protane>=quantile(vip$protane, prob = c(0.33, 0.66, 1),na.rm=TRUE)[[1]]) & (vip$protane<quantile(vip$protane, prob = c(0.33, 0.66, 1),na.rm=TRUE)[[2]])] <- 1
vip$catprotane[(vip$protane>=quantile(vip$protane, prob = c(0.33, 0.66, 1),na.rm=TRUE)[[2]])] <-2


#cholesterol
vip$catchole[(vip$chole<quantile(vip$chole, prob = c(0.33, 0.66, 1),na.rm=TRUE)[[1]])] <-0
vip$catchole[(vip$chole>=quantile(vip$chole, prob = c(0.33, 0.66, 1),na.rm=TRUE)[[1]]) & (vip$chole<quantile(vip$protane, prob = c(0.33, 0.66, 1),na.rm=TRUE)[[2]])] <- 1
vip$catchole[(vip$chole>=quantile(vip$chole, prob = c(0.33, 0.66, 1),na.rm=TRUE)[[2]])] <-2


#fullcorn
vip$catfullke[(vip$fullke<quantile(vip$fullke, prob = c(0.33, 0.66, 1),na.rm=TRUE)[[1]])] <-2
vip$catfullke[(vip$fullke>=quantile(vip$fullke, prob = c(0.33, 0.66, 1),na.rm=TRUE)[[1]]) & (vip$fullke<quantile(vip$fullke, prob = c(0.33, 0.66, 1),na.rm=TRUE)[[2]])] <- 1
vip$catfullke[(vip$fullke>=quantile(vip$fullke, prob = c(0.33, 0.66, 1),na.rm=TRUE)[[2]])] <-0

#macronutrient diet score
vip$macr_sc <- vip$catpro + vip$catfat + vip$catmufa + vip$catpufa + vip$catprotane + vip$catfullke
vip$macr_sc[is.na(vip$catpro) | is.na(vip$catfat) | is.na(vip$catmufa) | is.na(vip$catpufa) | is.na(vip$catprotane) | is.na(vip$catfullke) ] <- NA
summary(vip$macr_sc)

#linear regression to check the correctness of the direction
#individual macronutrients
dietcat <- as.vector(c("catpro", "catfat", "catmufa", "catpufa", "catprotane", "catfullke"))


diet.cat <- lapply(dietcat, function(x) { glm(substitute(bmi ~ age + agesq + gender +  year + ffq + i, list(i = as.name(x))), family=gaussian(link="identity"), data = vip)
})

sink('/Users/alaitzpoveda/Documents/GAME 2017/Unhealthy_lean-Jerneja/results/diet.cat.txt')
lapply(diet.cat, summary)
sink()

#macronutrient score
reg.macr_sc <- glm(bmi ~ age + agesq + gender + year + ffq + macr_sc, family=gaussian(link="identity"), data=vip)
summary(reg.macr_sc)

#only guidelines macronutrients macr_sc: regression:  effect size: 1.103e-01  p-value= 1.46e-14 *** "R2": 1-(965120/1009541):0.04400118469
#guidelines + catensum1: effect size: 8.267E-02  p-value= 2.73E-12 *** "R2": 1-(965268/1009541):0.04385458341
#guidelines + catprotane: effect size: 2.177E-01 p-value= <2E-16*** "R2": 1-(960093/1009541):0.04898067537
#guidelines + catprotane + chole: effect size: 1.336E-01 p-value= <2E-16*** "R2": 1-(961713/1009541):0.04737598572
#guidelines + catprotane + fullke: effect size: 1.756E-01 p-value= <2E-16** "R2": 1-(959693/1009541):0.04937689504

#FOOD ITEMS 
vip$wholegrain[vip$ffq==1]<-apply(cbind(vip$da10,vip$da11,vip$da22,vip$da23,vip$da27,0.25*vip$da70),1,FUN=sum,na.rm=TRUE)[vip$ffq==1]
vip$fish[vip$ffq==1]<-apply(cbind(vip$da60,vip$da61),1,FUN=sum,na.rm=TRUE)[vip$ffq==1]
vip$fruit[vip$ffq==1]<-apply(cbind(vip$da29, vip$da30,vip$da31,vip$da32),1,FUN=sum,na.rm=TRUE)[vip$ffq==1]
vip$vegetables[vip$ffq==1]<-apply(cbind(vip$da33,vip$da34,vip$da35,vip$da36,vip$da37,vip$da38),1,FUN=sum,na.rm=TRUE)[vip$ffq==1]
vip$redmeat[vip$ffq==1]<-apply(cbind(vip$da51,vip$da52,vip$da53,vip$da54,vip$da55,vip$da56),1,FUN=sum,na.rm=TRUE)[vip$ffq==1]
vip$desserts[vip$ffq==1]<-apply(cbind(vip$da65,vip$da66,vip$da67,vip$da68,vip$da69),1,FUN=sum,na.rm=TRUE)[vip$ffq==1]
vip$sugardrink[vip$ffq==1]<-apply(cbind(vip$da74,vip$da75),1,FUN=sum,na.rm=TRUE)[vip$ffq==1]
vip$friedpotato[vip$ffq==1]<-apply(cbind(vip$da40,vip$da41),1,FUN=sum,na.rm=TRUE)[vip$ffq==1]

vip$wholegrain[vip$ffq==0]<-apply(cbind(vip$dat10,vip$dat11,vip$DAT18,vip$dat22,0.25*vip$dat52),1,FUN=sum,na.rm=TRUE)[vip$ffq==0]
vip$fish[vip$ffq==0]<-apply(cbind(vip$dat44,vip$dat45),1,FUN=sum,na.rm=TRUE)[vip$ffq==0]
vip$fruit[vip$ffq==0]<-apply(cbind(vip$dat24,vip$DAT25,vip$dat26),1,FUN=sum,na.rm=TRUE)[vip$ffq==0]
vip$vegetables[vip$ffq==0]<-apply(cbind(vip$dat27,vip$dat28,vip$DAT29),1,FUN=sum,na.rm=TRUE)[vip$ffq==0]
vip$redmeat[vip$ffq==0]<-apply(cbind(vip$dat37,vip$dat38,vip$dat39,vip$dat40,vip$dat41,vip$dat42),1,FUN=sum,na.rm=TRUE)[vip$ffq==0]
vip$desserts[vip$ffq==0]<-apply(cbind(vip$dat48,vip$dat49,vip$DAT50,vip$dat51),1,FUN=sum,na.rm=TRUE)[vip$ffq==0]
vip$sugardrink[vip$ffq==0]<-apply(cbind(vip$dat56),1,FUN=sum,na.rm=TRUE)[vip$ffq==0]
vip$friedpotato[vip$ffq==0]<-apply(cbind(vip$DAT31),1,FUN=sum,na.rm=TRUE)[vip$ffq==0]

#linear regressions to test direction of association
healthdiet <- as.vector(c("wholegrain", "fish", "fruit", "vegetables", "redmeat", "desserts", "sugardrink", "friedpotato"))

health.diet <- lapply(healthdiet, function(x) { glm(substitute(bmi ~ age + agesq + gender +  year + ffq + i, list(i = as.name(x))), family=gaussian(link="identity"), data = vip)
})

sink('/Users/alaitzpoveda/Documents/GAME 2017/Unhealthy_lean-Jerneja/results/health.diet.txt')
lapply(health.diet, summary)
sink()

#fruit and vegetables positive association with BMI and desserts negative
#create quartiles
vip$cat.wholegrain[(vip$wholegrain<quantile(vip$wholegrain, na.rm=TRUE)[[2]])] <-3
vip$cat.wholegrain[(vip$wholegrain>=quantile(vip$wholegrain, na.rm=TRUE)[[2]]) & (vip$wholegrain<quantile(vip$wholegrain, na.rm=TRUE)[[3]])] <- 2
vip$cat.wholegrain[(vip$wholegrain>=quantile(vip$wholegrain, na.rm=TRUE)[[3]]) & (vip$wholegrain<quantile(vip$wholegrain, na.rm=TRUE)[[4]])] <- 1
vip$cat.wholegrain[(vip$wholegrain>=quantile(vip$wholegrain, na.rm=TRUE)[[4]])] <-0

vip$cat.fish[(vip$fish<quantile(vip$fish, na.rm=TRUE)[[2]])] <-3
vip$cat.fish[(vip$fish>=quantile(vip$fish, na.rm=TRUE)[[2]]) & (vip$fish<quantile(vip$fish, na.rm=TRUE)[[3]])] <- 2
vip$cat.fish[(vip$fish>=quantile(vip$fish, na.rm=TRUE)[[3]]) & (vip$fish<quantile(vip$fish, na.rm=TRUE)[[4]])] <- 1
vip$cat.fish[(vip$fish>=quantile(vip$fish, na.rm=TRUE)[[4]])] <-0

#vip$cat.fruit[(vip$fruit<quantile(vip$fruit, na.rm=TRUE)[[2]])] <-3
#vip$cat.fruit[(vip$fruit>=quantile(vip$fruit, na.rm=TRUE)[[2]]) & (vip$fruit<quantile(vip$fruit, na.rm=TRUE)[[3]])] <- 2
#vip$cat.fruit[(vip$fruit>=quantile(vip$fruit, na.rm=TRUE)[[3]]) & (vip$fruit<quantile(vip$fruit, na.rm=TRUE)[[4]])] <- 1
#vip$cat.fruit[(vip$fruit>=quantile(vip$fruit, na.rm=TRUE)[[4]])] <-0

#vip$cat.vegetables[(vip$vegetables<quantile(vip$vegetables, na.rm=TRUE)[[2]])] <-3
#vip$cat.vegetables[(vip$vegetables>=quantile(vip$vegetables, na.rm=TRUE)[[2]]) & (vip$vegetables<quantile(vip$vegetables, na.rm=TRUE)[[3]])] <- 2
#vip$cat.vegetables[(vip$vegetables>=quantile(vip$vegetables, na.rm=TRUE)[[3]]) & (vip$vegetables<quantile(vip$vegetables, na.rm=TRUE)[[4]])] <- 1
#vip$cat.vegetables[(vip$vegetables>=quantile(vip$vegetables, na.rm=TRUE)[[4]])] <-0

vip$cat.redmeat[(vip$redmeat<quantile(vip$redmeat, na.rm=TRUE)[[2]])] <-0
vip$cat.redmeat[(vip$redmeat>=quantile(vip$redmeat, na.rm=TRUE)[[2]]) & (vip$redmeat<quantile(vip$redmeat, na.rm=TRUE)[[3]])] <- 1
vip$cat.redmeat[(vip$redmeat>=quantile(vip$redmeat, na.rm=TRUE)[[3]]) & (vip$redmeat<quantile(vip$redmeat, na.rm=TRUE)[[4]])] <- 2
vip$cat.redmeat[(vip$redmeat>=quantile(vip$redmeat, na.rm=TRUE)[[4]])] <-3

#vip$cat.desserts[(vip$desserts<quantile(vip$desserts, na.rm=TRUE)[[2]])] <-0
#vip$cat.desserts[(vip$desserts>=quantile(vip$desserts, na.rm=TRUE)[[2]]) & (vip$desserts<quantile(vip$desserts, na.rm=TRUE)[[3]])] <- 1
#vip$cat.desserts[(vip$desserts>=quantile(vip$desserts, na.rm=TRUE)[[3]]) & (vip$desserts<quantile(vip$desserts, na.rm=TRUE)[[4]])] <- 2
#vip$cat.desserts[(vip$desserts>=quantile(vip$desserts, na.rm=TRUE)[[4]])] <-3

vip$cat.sugardrink[(vip$sugardrink<quantile(vip$sugardrink, na.rm=TRUE)[[2]])] <-0
vip$cat.sugardrink[(vip$sugardrink>=quantile(vip$sugardrink, na.rm=TRUE)[[2]]) & (vip$sugardrink<quantile(vip$sugardrink, na.rm=TRUE)[[3]])] <- 1
vip$cat.sugardrink[(vip$sugardrink>=quantile(vip$sugardrink, na.rm=TRUE)[[3]]) & (vip$sugardrink<quantile(vip$sugardrink, na.rm=TRUE)[[4]])] <- 2
vip$cat.sugardrink[(vip$sugardrink>=quantile(vip$sugardrink, na.rm=TRUE)[[4]])] <-3

vip$cat.friedpotato[(vip$friedpotato<quantile(vip$friedpotato, na.rm=TRUE)[[2]])] <-0
vip$cat.friedpotato[(vip$friedpotato>=quantile(vip$friedpotato, na.rm=TRUE)[[2]]) & (vip$friedpotato<quantile(vip$friedpotato, na.rm=TRUE)[[3]])] <- 1
vip$cat.friedpotato[(vip$friedpotato>=quantile(vip$friedpotato, na.rm=TRUE)[[3]]) & (vip$friedpotato<quantile(vip$friedpotato, na.rm=TRUE)[[4]])] <- 2
vip$cat.friedpotato[(vip$friedpotato>=quantile(vip$friedpotato, na.rm=TRUE)[[4]])] <-3

#healthy diet score
vip$healthy_sc <- vip$cat.wholegrain + vip$cat.fish +  vip$cat.redmeat + vip$cat.sugardrink + vip$cat.friedpotato
vip$healthy_sc[is.na(vip$cat.wholegrain) | is.na(vip$cat.fish) | is.na(vip$cat.redmeat)| is.na(vip$cat.sugardrink | is.na(vip$cat.friedpotato)) ] <- NA
summary(vip$healthy_sc)

#linear regressions to test direction of association
healthdietcat <- as.vector(c("cat.wholegrain", "cat.fish", "cat.fruit", "cat.vegetables", "cat.redmeat", "cat.desserts", "cat.sugardrink", "cat.friedpotato"))

health.diet.cat <- lapply(healthdietcat, function(x) { glm(substitute(bmi ~ age + agesq + gender +  year + ffq + i, list(i = as.name(x))), family=gaussian(link="identity"), data = vip)
})

sink('/Users/alaitzpoveda/Documents/GAME 2017/Unhealthy_lean-Jerneja/results/health.diet.cat.txt')
lapply(health.diet.cat, summary)
sink()

#healthydiet score
reg.healthy_sc <- glm(bmi ~ age + agesq + gender + year + ffq + healthy_sc, family=gaussian(link="identity"), data=vip)
summary(reg.healthy_sc)

#all food items in healthy diet score: regression:  effect size: 3.190e-02  p-value= <2E-16 *** "R2": 1-(994717/1040834):0.04430773783
#food items with correct direction: effect size: 1.021E-01  p-value= <2E-16*** "R2": 1-(990687/1040834):0.04817963287

#combined diet score 
vip$combined_sc <- vip$catpro + vip$catfat + vip$catmufa + vip$catpufa + vip$catprotane + vip$catfullke + vip$cat.redmeat + vip$cat.sugardrink
vip$combined_sc[is.na(vip$catpro) | is.na(vip$catfat) | is.na(vip$catmufa) | is.na(vip$catpufa) | is.na(vip$catprotane) | is.na(vip$catfullke) | is.na(vip$cat.redmeat) | is.na(vip$cat.sugardrink) ] <- NA
summary(vip$combined_sc)

reg.combined_sc <- glm(bmi ~ age + agesq + gender + year + ffq + combined_sc, family=gaussian(link="identity"), data=vip)
summary(reg.combined_sc)

#macronutrients score: effect size: 1.756E-01 p-value= <2E-16** "R2": 1-(959693/1009541):0.04937689504
#macronutrients + fish: effect size: 1.545e-01  p-value= <2E-16 *** "R2": 1-(959917/1009541):0.04915501203
#macronutrients + redmeat: effect size: 1.521e-01  p-value= <2E-16 *** "R2": 1-(957623/1009541):0.05142733182
#macronutrients + redmeat + sugardrink: effect size: 1.525e-01  p-value= <2E-16 *** "R2": 1-(957304/1009541):0.05174331701##
#macronutrients + friedpotato: effect size: 1.446E-01 p-value= <2E-16 *** "R2": 1-(959493/1009541):0.049575005487
#macronutrients + redmeat + sugardrink + friedpotato: effect size: 1.271e-01  p-value= <2E-16 *** "R2": 1-(957600/1009541):0.05145011445

#potatoes
vip$boiled_potato[vip$ffq==1]<-apply(cbind(vip$da39,vip$da42),1,FUN=sum,na.rm=TRUE)[vip$ffq==1]
vip$boiled_potato[vip$ffq==0]<-apply(cbind(vip$dat30),1,FUN=sum,na.rm=TRUE)[vip$ffq==0]

vip$cat.boiled_potato[(vip$boiled_potato<quantile(vip$boiled_potato, na.rm=TRUE)[[2]])] <-0
vip$cat.boiled_potato[(vip$boiled_potato>=quantile(vip$boiled_potato, na.rm=TRUE)[[2]]) & (vip$boiled_potato<quantile(vip$boiled_potato, na.rm=TRUE)[[3]])] <- 1
vip$cat.boiled_potato[(vip$boiled_potato>=quantile(vip$boiled_potato, na.rm=TRUE)[[3]]) & (vip$boiled_potato<quantile(vip$boiled_potato, na.rm=TRUE)[[4]])] <- 2
vip$cat.boiled_potato[(vip$boiled_potato>=quantile(vip$boiled_potato, na.rm=TRUE)[[4]])] <-3

vip$potatoes <- vip$boiled_potato + vip$friedpotato
vip$cat.potatoes[(vip$potatoes<quantile(vip$potatoes, na.rm=TRUE)[[2]])] <-0
vip$cat.potatoes[(vip$potatoes>=quantile(vip$potatoes, na.rm=TRUE)[[2]]) & (vip$potatoes<quantile(vip$potatoes, na.rm=TRUE)[[3]])] <- 1
vip$cat.potatoes[(vip$potatoes>=quantile(vip$potatoes, na.rm=TRUE)[[3]]) & (vip$potatoes<quantile(vip$potatoes, na.rm=TRUE)[[4]])] <- 2
vip$cat.potatoes[(vip$potatoes>=quantile(vip$potatoes, na.rm=TRUE)[[4]])] <-3

#combined diet score 
vip$combined_sc1 <- vip$catpro + vip$catfat + vip$catmufa + vip$catpufa + vip$catprotane + vip$catfullke + vip$cat.redmeat + vip$cat.sugardrink + vip$cat.potatoes
vip$combined_sc1[is.na(vip$catpro) | is.na(vip$catfat) | is.na(vip$catmufa) | is.na(vip$catpufa) | is.na(vip$catprotane) | is.na(vip$catfullke) | is.na(vip$cat.redmeat) | is.na(vip$cat.sugardrink) | is.na(vip$cat.potatoes) ] <- NA
summary(vip$combined_sc1)

reg.combined_sc1 <- glm(bmi ~ age + agesq + gender + year + ffq + combined_sc1, family=gaussian(link="identity"), data=vip)
summary(reg.combined_sc1)

#macronutrients + redmeat + sugardrink: effect size: 1.525e-01  p-value= <2E-16 *** "R2": 1-(957304/1009541):0.05174331701
#macronutrients + redmeat + sugardrink + boiledpotato: effect size: 1.382E-01 p-value= <2E-16 *** "R2": 1-(956151/1009541):0.0528854202
#macronutrients + redmeat + sugardrink + potatoes: effect size: 1.353E-01 p-value= <2E-16 *** "R2": 1-(955922/1009541):0.05311225596##





