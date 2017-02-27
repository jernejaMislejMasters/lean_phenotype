#IMPORT FILE
vip <- read.table ("/Volumes/DISCOENCR/BMI_bimodality/VIP/VIP_170206_cleaned_subset.csv", header=TRUE, sep=',', fileEncoding="latin1")
summary(vip)

#variable transformation
vip$date <- as.Date(vip$datum, format="%m/%d/%Y")
vip$year <- as.numeric(format(vip$date,"%Y"))
vip$agesq <- vip$age * vip$age
vip$ffq[vip$enkver2 == "apri"]  <- 1 
vip$ffq[vip$enkver2 == "long"]  <- 1 
vip$ffq[vip$enkver2 == "short"]  <- 0
vip$fa <- vip$FA182_sum1 + vip$FA183_sum1 + vip$FA204_sum1 + vip$FA205_sum1 + vip$FA226_sum1

#MACRONUTRIENTS
#%E
vip$protcal <- 4*vip$protsum1
vip$prote <- (100*vip$protcal)/vip$ensum1
vip$protancal <- 4*vip$protsum1_anim
vip$protane <- (100*vip$protancal)/vip$ensum1
vip$protvegcal <- 4*vip$protsum1_veg
vip$protvege <- (100*vip$protancal)/vip$ensum1

vip$chcal <- 4*vip$kolhsum1
vip$che <- (100*vip$chcal)/vip$ensum1
vip$sugcal <- 3.94*vip$sacksum1
vip$suge <- (100*vip$sugcal)/vip$ensum1
vip$disacal <- 4*vip$DISAsum1
vip$disae <- (100*vip$disacal)/vip$ensum1
vip$mosacal <- 4*vip$MOSAsum1
vip$mosae <- (100*vip$mosacal)/vip$ensum1
vip$fullkcal <- 3*vip$FULLKsum1
vip$fullke <- (100*vip$fullkcal)/vip$ensum1
#vip$fibecal <- 2*vip$fibesum1
#vip$fibere <- (100*vip$fibecal)/vip$ensum1

vip$fatcal <- 9*vip$fettsum1
vip$fate <- (100*vip$fatcal)/vip$ensum1
vip$satfcal <- 9*vip$mfetsum1
vip$satfe <- (100*vip$satfcal)/vip$ensum1
vip$pufacal <- (9*vip$POLYsum1)
vip$pufae <- (100*vip$pufacal)/vip$ensum1
vip$mufacal <- (9*vip$MONOsum1)
vip$mufae <- (100*vip$mufacal)/vip$ensum1
vip$transcal <- (9*vip$TRANSsum1)
vip$transe <- (100*vip$transcal)/vip$ensum1
vip$cholcal <- (9*vip$kolesum1)
vip$chole <- (100*vip$cholcal)/vip$ensum1
vip$facal <- (9*vip$fa)
vip$fae <- (100*vip$facal)/vip$ensum1

#CATEGORIES
#protein
vip$catpro[vip$prote < 10] <- 0
vip$catpro[vip$prote >= 10 & vip$prote <= 20] <- 1
vip$catpro[vip$prote > 20] <- 2

#ch
vip$cat[vip$che < 45] <- 0
vip$catch[vip$che >=45 & vip$che<= 60] <- 1
vip$catch[vip$che>60] <- 2

#fiber
vip$catfib[vip$fibesum1 < 25] <- 0
vip$catfib[vip$fibesum1 >= 25 & vip$fibesum1 >= 35] <- 1
vip$catfib[vip$fibesum1 >= 35] <- 2

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

#divide baseline and follow-up
#vip.bas <- subset(vip, vip$visit==1) 
#vip.fup <- subset(vip, vip$visit==2)

#NUTRIENTS
#continous
diete <- as.vector(c("prote", "protane", "protvege", "che", "suge", "disae", "mosae", "fibesum1", "fullke",
                    "fate", "satfe", "mufae", "pufae", "transe", "chole", "fae"))


diet.e <- lapply(diete, function(x) { glm(substitute(bmi ~ age + agesq + gender +  year + ffq + i, list(i = as.name(x))), family=gaussian(link="identity"), data = vip)
})

sink('/Users/alaitzpoveda/Documents/GAME 2017/Unhealthy_lean-Jerneja/results/diet.e.txt')
lapply(diet.e, summary)
sink()

#all
all.bas <- glm(bmi ~ age + agesq + gender + year + ffq + prote + protane + protvege + che + suge + disae + mosae + fibere + fullke + fate + 
      satfe + mufae + pufae + transe + chole + fae, family=gaussian(link="identity"), data=vip.bas)
summary(all.bas)

#categorized
dietcat <- as.vector(c("catpro", "catch","catfat", "catmufa", "catpufa"))


diet.cat <- lapply(dietcat, function(x) { glm(substitute(bmi ~ age + agesq + gender +  year + ffq + i, list(i = as.name(x))), family=gaussian(link="identity"), data = vip)
})

sink('/Users/alaitzpoveda/Documents/GAME 2017/Unhealthy_lean-Jerneja/results/diet.cat.txt')
lapply(diet.cat, summary)
sink()

#all
all.bas <- glm(bmi ~ age + agesq + gender + year + ffq + catpro + catch + catfat +  catmufa +  catpufa, family=gaussian(link="identity"), data=vip)
summary(all.bas)


