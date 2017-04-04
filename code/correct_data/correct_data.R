#VIP

#----------------------------ALAITZ---------------------------------------------------

#IMPORT FILE
#VIP_data <- read.csv("VIP_161102.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)
#NEW FILE
VIP_data <- read.csv("VIP_data/VIP_170206.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)

VIP_data$date <- as.Date(VIP_data$datum, format="%m/%d/%Y")

#ADAPT THE DATASET
#Change numbers for NA on phenotypes
#create a list of phenotypes to change
variablelist <- as.vector(c("fasta_enk", "civil", "utbild", "sambo", "skiftarbete", "sjukskriven", "ansttyp_a", "ansttyp_b", "ansttyp_c", "ansttyp_d", "ansttyp_e", "ansttyp_f", "ansttyp_g", "ansttyp_h", "ansttyp_i",
				"ursprungsland", "halsojf", "halsoal", "halsoar", "hjartinf_foraldrar_syskon", "diab_foraldrar_syskon", "beskbltr", "med_C5a", "med_C5b", "med_C5c", "med_C5d", "med_C5e", "med_C5f",
				"smartmed", "andra_mediciner", "diabet", "diabetesbehandling_a", "diabetesbehandling_b", "diabetesbehandling_c", "diabetesbehandling_d", "graviditetsdiabetes", "infarkt_sjukhus", "infarkt_sjukhus_ar", "sf_1", 
				"sf_2", "sf_3a", "sf_3b", "sf_3c", "sf_3d", "sf_3e", "sf_3f", "sf_3g", "sf_3h", "sf_3i", "sf_3j", "sf_4a", "sf_4b", "sf_4c", "sf_4d", "sf_5a", "sf_5b", "sf_5c", "sf_6", "sf_7", "sf_8", "sf_9a", "sf_9b", "sf_9c", "sf_9d", 
				"sf_9e", "sf_9f", "sf_9g", "sf_9h", "sf_9i", "sf_10", "sf_11a", "sf_11b", "sf_11c", "sf_11d", "livskvalitet_d1", "livskvalitet_d2", "livskvalitet_d3", "livskvalitet_d4", "livskvalitet_d5", "livskvalitet_d6", 
				"livskvalitet_d7", "livskvalitet_d8", "livskvalitet_d9", "livskvalitet_d10", "livskvalitet_d11", "livskvalitet_d12", "livskvalitet_d13", "livskvalitet_d14", "livskvalitet_d15", "livskvalitet_d16", "livskvalitet_d17", 
				"sockont", "socsam", "soclago", "sochem", "soctala", "socstod", "socnara", "soclyck", "socanfo", "soctrost", "socupps", "soclana", "sochelp", "socdelta", "socofta", "socforening_a", "socforening_b", "socforening_c",
				"socforening_d", "socforening_e", "arbfys", "arbfort", "arbpsyk", "arbhin", "arbkrav", "arbnytt", "arbski",  "arbide", "arbrut", "arbhur", "arbvad", "arbtala", "arblamna", "arbkontakt", "arbfritid", "arbbesok",
				"g1_a", "g1_b", "g1_c", "g1_d", "g2_a", "g2_b", "g2_c", "g2_d", "g2_e", "g3_a","g3_b", "g3_c", "g3_d", "g3_e", "g3_f", "g3_g", "g4", "g5", "g6", "g7", "g8", "g9", "g10", "g11a_h", "g11a_m", "g11a_ej", "g11b_h",
				"g11b_m", "g11b_ej", "motion", "motion2", "sleep_h7a", "sleep_h7b", "sleep_h7c", "sleep_h7d", "sleep_h7e", "sleep_h7f", "sleep_h7g", "sleep_h7h", "sleep_h8a", "sleep_h8b", "i1", "i2", "i3", "i4", "i5", "j1", "j2",
				"j3", "j4", "j5", "j6", "j7", "j8", "j9", "j10", "sm_status", "sm_cig_groups", "sm_num_cig", "sm_num_cigar", "sm_gr_tobacco", "sm_how_often", "sm_start", "sm_stop", "sm_duration", "sm_whystop_1", "sm_whystop_2", 
				"sm_whystop_3", "sm_whystop_4", "sm_whystop_5", "sn_status", "sn_quantity", "sn_time", "sn_stopsmoke_a", "sn_stopsmoke_b", "sn_nicotine_replace", "sm_nicotine_replace", "nicotine_replace", "sm_yes_no", "sn_yes_no"))
#loop to change to NA numbers higher or equal to 5555
for(k in variablelist)
{ VIP_data[k][VIP_data[k] >= 5555] = NA }

#Define categorical variables#
VIP_data[,variablelist] <- lapply(VIP_data[,variablelist] , factor)

#CORRECT PHENOTYPES VALUES
#correct for change in measurement method on blood pressure traits and lipid traits
#create new blood pressure and lipids variables
VIP_data$sbpc <- VIP_data$sbt
VIP_data$dbpc <- VIP_data$dbt
VIP_data$cholc <- VIP_data$skol
VIP_data$tryglc <- VIP_data$stg

#create variable efter_090901
VIP_data$efter_090901[VIP_data$date >"2009-09-01"] <- 1
VIP_data$efter_090901[VIP_data$date <="2009-09-01"] <- 0
VIP_data$efter_090901 <- as.factor(VIP_data$efter_090901)

#sbp change from sitting to supine
#there are no formulas to convert values from sitting to supine position for individuals <=35 years or > 65 years. For them, I have use the formula closest to their age in order not to lose participants but we may need to delete them
VIP_data$sbpc [VIP_data$efter_090901 == 1  & VIP_data$age <= 35 & VIP_data$gender == 1 & !is.na(VIP_data$sbt)] <- 24.595 + (0.792*VIP_data$sbt[VIP_data$efter_090901 == 1  & VIP_data$age <= 35  & VIP_data$gender == 1 & !is.na(VIP_data$sbt)])
VIP_data$sbpc [VIP_data$efter_090901 == 1  & VIP_data$age >35   & VIP_data$age <= 45 & VIP_data$gender == 1 & !is.na(VIP_data$sbt)] <- 24.595 + (0.792*VIP_data$sbt[VIP_data$efter_090901 == 1  & VIP_data$age > 35  & VIP_data$age <= 45 & VIP_data$gender == 1 & !is.na(VIP_data$sbt)])
VIP_data$sbpc [VIP_data$efter_090901 == 1  & VIP_data$age >45   & VIP_data$age <= 55 & VIP_data$gender == 1 & !is.na(VIP_data$sbt)] <- 9.850 + (0.910*VIP_data$sbt[VIP_data$efter_090901 == 1  & VIP_data$age > 45  & VIP_data$age <= 55 & VIP_data$gender == 1 & !is.na(VIP_data$sbt)])
VIP_data$sbpc [VIP_data$efter_090901 == 1  & VIP_data$age >55   & VIP_data$age <= 65 & VIP_data$gender == 1 & !is.na(VIP_data$sbt)] <- 7.763 + (0.936*VIP_data$sbt[VIP_data$efter_090901 == 1  & VIP_data$age > 55  & VIP_data$age <= 65 & VIP_data$gender == 1 & !is.na(VIP_data$sbt)])
VIP_data$sbpc [VIP_data$efter_090901 == 1  & VIP_data$age > 65 & VIP_data$gender == 1 & !is.na(VIP_data$sbt)] <- 7.763 + (0.936*VIP_data$sbt[VIP_data$efter_090901 == 1  & VIP_data$age > 65  & VIP_data$gender == 1 & !is.na(VIP_data$sbt)])

VIP_data$sbpc [VIP_data$efter_090901 == 1  & VIP_data$age <= 35 & VIP_data$gender == 2 & !is.na(VIP_data$sbt)] <- 8.669 + (0.919*VIP_data$sbt[VIP_data$efter_090901 == 1  & VIP_data$age <= 35 & VIP_data$gender == 2 & !is.na(VIP_data$sbt)])
VIP_data$sbpc [VIP_data$efter_090901 == 1  & VIP_data$age >35   & VIP_data$age <= 45 & VIP_data$gender == 2 & !is.na(VIP_data$sbt)] <- 8.669 + (0.919*VIP_data$sbt[VIP_data$efter_090901 == 1  & VIP_data$age > 35  & VIP_data$age <= 45 & VIP_data$gender == 2 & !is.na(VIP_data$sbt)])
VIP_data$sbpc [VIP_data$efter_090901 == 1  & VIP_data$age >45   & VIP_data$age <= 55 & VIP_data$gender == 2 & !is.na(VIP_data$sbt)] <- 16.051 + (0.859*VIP_data$sbt[VIP_data$efter_090901 == 1  & VIP_data$age > 45  & VIP_data$age <= 55 & VIP_data$gender == 2 & !is.na(VIP_data$sbt)])
VIP_data$sbpc [VIP_data$efter_090901 == 1  & VIP_data$age >55   & VIP_data$age <= 65 & VIP_data$gender == 2 & !is.na(VIP_data$sbt)] <- 9.999 + (0.914*VIP_data$sbt[VIP_data$efter_090901 == 1  & VIP_data$age > 55  & VIP_data$age <= 65 & VIP_data$gender == 2 & !is.na(VIP_data$sbt)])
VIP_data$sbpc [VIP_data$efter_090901 == 1  & VIP_data$age > 65 & VIP_data$gender == 2 & !is.na(VIP_data$sbt)] <- 9.999 + (0.914*VIP_data$sbt[VIP_data$efter_090901 == 1  & VIP_data$age > 65 & VIP_data$gender == 2 & !is.na(VIP_data$sbt)])

#dbp change from sitting to supine
VIP_data$dbpc [VIP_data$efter_090901 == 1  & VIP_data$age <= 35 & VIP_data$gender == 1 & !is.na(VIP_data$dbt)] <- 17.282 + (0.753*VIP_data$dbt[VIP_data$efter_090901 == 1  & VIP_data$age <= 35 & VIP_data$gender == 1 & !is.na(VIP_data$dbt)])
VIP_data$dbpc [VIP_data$efter_090901 == 1  & VIP_data$age >35   & VIP_data$age <= 45 & VIP_data$gender == 1 & !is.na(VIP_data$dbt)] <- 17.282 + (0.753*VIP_data$dbt[VIP_data$efter_090901 == 1  & VIP_data$age > 35  & VIP_data$age <= 45 & VIP_data$gender == 1 & !is.na(VIP_data$dbt)])
VIP_data$dbpc [VIP_data$efter_090901 == 1  & VIP_data$age >45   & VIP_data$age <= 55 & VIP_data$gender == 1 & !is.na(VIP_data$dbt)] <- 12.363 + (0.812*VIP_data$dbt[VIP_data$efter_090901 == 1  & VIP_data$age > 45  & VIP_data$age <= 55 & VIP_data$gender == 1 & !is.na(VIP_data$dbt)])
VIP_data$dbpc [VIP_data$efter_090901 == 1  & VIP_data$age >55   & VIP_data$age <= 65 & VIP_data$gender == 1 & !is.na(VIP_data$dbt)] <- 9.029 + (0.864*VIP_data$dbt[VIP_data$efter_090901 == 1  & VIP_data$age > 55  & VIP_data$age <= 65 & VIP_data$gender == 1 & !is.na(VIP_data$dbt)])
VIP_data$dbpc [VIP_data$efter_090901 == 1  & VIP_data$age > 65 & VIP_data$gender == 1 & !is.na(VIP_data$dbt)] <-  9.029 + (0.864*VIP_data$dbt[VIP_data$efter_090901 == 1  & VIP_data$age > 65 & VIP_data$gender == 1 & !is.na(VIP_data$dbt)])

VIP_data$dbpc [VIP_data$efter_090901 == 1  & VIP_data$age <= 35 & VIP_data$gender == 2 & !is.na(VIP_data$dbt)] <- 5.784 + (0.890*VIP_data$dbt[VIP_data$efter_090901 == 1  & VIP_data$age <= 35 & VIP_data$gender == 2 & !is.na(VIP_data$dbt)])
VIP_data$dbpc [VIP_data$efter_090901 == 1  & VIP_data$age >35   & VIP_data$age <= 45 & VIP_data$gender == 2 & !is.na(VIP_data$dbt)] <- 5.784 + (0.890*VIP_data$dbt[VIP_data$efter_090901 == 1  & VIP_data$age > 35  & VIP_data$age <= 45 & VIP_data$gender == 2 & !is.na(VIP_data$dbt)])
VIP_data$dbpc [VIP_data$efter_090901 == 1  & VIP_data$age >45   & VIP_data$age <= 55 & VIP_data$gender == 2 & !is.na(VIP_data$dbt)] <- 13.566 + (0.798*VIP_data$dbt[VIP_data$efter_090901 == 1  & VIP_data$age > 45  & VIP_data$age <= 55 & VIP_data$gender == 2 & !is.na(VIP_data$dbt)])
VIP_data$dbpc [VIP_data$efter_090901 == 1  & VIP_data$age >55   & VIP_data$age <= 65 & VIP_data$gender == 2 & !is.na(VIP_data$dbt)] <- 7.992 + (0.870*VIP_data$dbt[VIP_data$efter_090901 == 1  & VIP_data$age > 55  & VIP_data$age <= 65 & VIP_data$gender == 2 & !is.na(VIP_data$dbt)])
VIP_data$dbpc [VIP_data$efter_090901 == 1  & VIP_data$age > 65 & VIP_data$gender == 2 & !is.na(VIP_data$dbt)] <-  7.992 + (0.870*VIP_data$dbt[VIP_data$efter_090901 == 1  & VIP_data$age > 65 & VIP_data$gender == 2 & !is.na(VIP_data$dbt)])

VIP_data$sbpc <- round(VIP_data$sbpc, digits=0)
VIP_data$dbpc <- round(VIP_data$dbpc, digits=0)

#chol change from laboratory to reflotron
VIP_data$cholc [VIP_data$efter_090901 == 1 & !is.na(VIP_data$skol)] <- 0.170 + (0.939*VIP_data$skol[VIP_data$efter_090901 == 1 & !is.na(VIP_data$skol)])

#trygl change from laboratory to reflotron
VIP_data$tryglc [VIP_data$efter_090901 == 1 & !is.na(VIP_data$stg)] <- 0.177 + (0.932*VIP_data$stg[VIP_data$efter_090901 == 1 & !is.na(VIP_data$stg)])

VIP_data$cholc <- round(VIP_data$cholc, digits=2)
VIP_data$tryglc <- round(VIP_data$tryglc, digits=2)

# correct for medication on blood pressure and lipid traits 
#medication blood pressure
VIP_data$medibl <- VIP_data$med_C5a
VIP_data$medibl = as.character(VIP_data$medibl)
VIP_data$medibl[is.na(VIP_data$medibl)] = 0
VIP_data$sbp [VIP_data$medibl == 1]  <- VIP_data$sbpc [VIP_data$medibl == 1] +15
VIP_data$sbp [VIP_data$medibl == 0] <- VIP_data$sbpc [VIP_data$medibl == 0]
VIP_data$dbp [VIP_data$medibl == 1] <- VIP_data$dbpc [VIP_data$medibl == 1] +10
VIP_data$dbp [VIP_data$medibl == 0] <- VIP_data$dbpc [VIP_data$medibl == 0] 

#medication lipids 
#Use Friedewall formula to calculate ldl in individuals in which we don't have the data
VIP_data$ldlf <- VIP_data$ldl
VIP_data$ldlf[is.na(VIP_data$ldl)] <- VIP_data$cholc[is.na(VIP_data$ldl)] - VIP_data$hdl[is.na(VIP_data$ldl)] - (VIP_data$tryglc[is.na(VIP_data$ldl)]/2.2)
#correct ldl to not have a negative value
VIP_data$ldlf [VIP_data$ldlf < 0] <- NA

VIP_data$medilip <- VIP_data$med_C5e
VIP_data$medilip = as.character(VIP_data$medilip)
VIP_data$medilip[is.na(VIP_data$medilip)] = 0
VIP_data$chol [VIP_data$medilip == 1]  <- VIP_data$cholc [VIP_data$medilip == 1] +1.347
VIP_data$chol [VIP_data$medilip == 0] <- VIP_data$cholc [VIP_data$medilip == 0]
VIP_data$chdl [VIP_data$medilip == 1] <- VIP_data$hdl [VIP_data$medilip == 1] -0.060
VIP_data$chdl [VIP_data$medilip == 0] <- VIP_data$hdl [VIP_data$medilip == 0] 
VIP_data$tryg [VIP_data$medilip == 1] <- VIP_data$tryglc [VIP_data$medilip == 1] +0.208
VIP_data$tryg [VIP_data$medilip == 0] <- VIP_data$tryglc [VIP_data$medilip == 0] 
VIP_data$cldl [VIP_data$medilip == 1] <- VIP_data$ldlf [VIP_data$medilip == 1] +1.290
VIP_data$cldl [VIP_data$medilip == 0] <- VIP_data$ldlf [VIP_data$medilip == 0] 

#exclude individuals outside limit values
VIP_data$langdm <- VIP_data$langd /100
VIP_data$bmi <- VIP_data$vikt / (VIP_data$langdm*VIP_data$langdm)
VIP_data$bmi[VIP_data$bmi < 15 | VIP_data$bmi > 70 ] = NA#49
VIP_data$bmi[VIP_data$langd < 130 | VIP_data$langd > 210 ] = NA#22
VIP_data$bmi[VIP_data$vikt < 35] = NA#5
VIP_data$langd[VIP_data$langd < 130 | VIP_data$langd > 210 ] = NA#22
VIP_data$vikt[VIP_data$vikt < 35] = NA#5
VIP_data$midja[VIP_data$midja < 60 | VIP_data$midja > 160 ] = NA#97
VIP_data$chol[VIP_data$chol < 0.5 | VIP_data$chol > 15 ] = NA#7
VIP_data$chdl[VIP_data$chdl < 0.15 | VIP_data$chdl > 7 ] = NA#400
VIP_data$tryg[VIP_data$tryg < 0.8 | VIP_data$tryg > 20 ] = NA#9769
VIP_data$blods0[VIP_data$blods0 < 2 | VIP_data$blods0 > 25 ] = NA#56
VIP_data$blods2[VIP_data$blods2 < 1 | VIP_data$blods2 > 35 ] = NA#1028
VIP_data$sbp[VIP_data$sbp < 20 | VIP_data$sbp > 300 ] = NA#0
VIP_data$dbp[VIP_data$dbp < 20 | VIP_data$dbp > 250 ] = NA#1

#----------------------------ALAITZ---------------------------------------------------

#----------------------------JERNEJA---------------------------------------------------

#remove the auxilliar varibles/columns
VIP_data <- within(VIP_data, rm(medilip, ldlf, sbpc, dbpc, medibl,cholc,tryglc,langdm,ldl,hdl,sbt,dbt,skol,stg))

#rename the corrected version, to have the same name as in the variable description
colnames(VIP_data)[604:609]<-c("sbt","dbt","skol","hdl","stg","ldl")


#exclude based on the insufficient diet data
#exclude those that have exclude 1 (3343)
VIP_data<-VIP_data[!(!is.na(VIP_data$exclude) & VIP_data$exclude==1),]

#CAREFUL DONT DO THIS TWICE!
#exclude the bottom 5% (7441 entries in VIP_170206)
VIP_data<-VIP_data[order(VIP_data$FIL),][-c(1:round(5*length(VIP_data$FIL[!is.na(VIP_data$FIL)])/100)),]

#and the top 2.5% (3534 entries in VIP_170206)
VIP_data<-VIP_data[order(-VIP_data$FIL),][-c(1:round(2.5*length(VIP_data$FIL[!is.na(VIP_data$FIL)])/100)),]


#final number of entries is 154009, corresponding to 104378 unique subjects

#correct the missing ursprungsland variable if it is present in other visits and if more visits have different information for ursprungsland, than set it it NA
#work with data where besok1 is avaialbe
VIP_data<-VIP_data[order(Subject_id),]
attach(VIP_data)

#those that have besok1==4 have either besok1 missing for the first visit or have all 4 besok1 and all the same information for the rest, except the first is missing, the value is
# always 1, so I just hard coded this one
VIP_data[Subject_id %in% Subject_id[!is.na(besok1) & besok1==4] & is.na(ursprungsland),c("ursprungsland")]<-1

#those that have besok1==3 ursprungsland and have either besok1 == 1 or 2 ursprungsland missing
VIP_data[Subject_id %in% Subject_id[!is.na(ursprungsland) & !is.na(besok1) & besok1==3] & is.na(ursprungsland) & !is.na(besok1) & besok1==1,c("ursprungsland")]<-
		VIP_data[!is.na(ursprungsland) & !is.na(besok1) & besok1==3 & Subject_id %in% Subject_id[is.na(ursprungsland) & !is.na(besok1) & besok1==1],c("ursprungsland")]

VIP_data[Subject_id %in% Subject_id[!is.na(ursprungsland) & !is.na(besok1) & besok1==3] & is.na(ursprungsland) & !is.na(besok1) & besok1==2,c("ursprungsland")]<-
		VIP_data[!is.na(ursprungsland) & !is.na(besok1) & besok1==3 & Subject_id %in% Subject_id[is.na(ursprungsland) & !is.na(besok1) & besok1==2],c("ursprungsland")]

#those that have besok1==2 and have either besok1 == 1 or 3 missing
VIP_data[Subject_id %in% Subject_id[!is.na(ursprungsland) & !is.na(besok1) & besok1==2] & is.na(ursprungsland) & !is.na(besok1) & besok1==1,c("ursprungsland")]<-
		VIP_data[!is.na(ursprungsland) & !is.na(besok1) & besok1==2 & Subject_id %in% Subject_id[is.na(ursprungsland) & !is.na(besok1) & besok1==1],c("ursprungsland")]

VIP_data[Subject_id %in% Subject_id[!is.na(ursprungsland) & !is.na(besok1) & besok1==2] & is.na(ursprungsland) & !is.na(besok1) & besok1==3,c("ursprungsland")]<-
		VIP_data[!is.na(ursprungsland) & !is.na(besok1) & besok1==2 & Subject_id %in% Subject_id[is.na(ursprungsland) & !is.na(besok1) & besok1==3],c("ursprungsland")]

#those that have besok1==1 and have either besok1 == 2 or 3 missing
VIP_data[Subject_id %in% Subject_id[!is.na(ursprungsland) & !is.na(besok1) & besok1==1] & is.na(ursprungsland) & !is.na(besok1) & besok1==2,c("ursprungsland")]<-
		VIP_data[!is.na(ursprungsland) & !is.na(besok1) & besok1==1 & Subject_id %in% Subject_id[is.na(ursprungsland) & !is.na(besok1) & besok1==2],c("ursprungsland")]

VIP_data[Subject_id %in% Subject_id[!is.na(ursprungsland) & !is.na(besok1) & besok1==1] & is.na(ursprungsland) & !is.na(besok1) & besok1==3,c("ursprungsland")]<-
		VIP_data[!is.na(ursprungsland) & !is.na(besok1) & besok1==1 & Subject_id %in% Subject_id[is.na(ursprungsland) & !is.na(besok1) & besok1==3],c("ursprungsland")]

#check those that have mixed information, for besok1==4 there arent any

#those that have besok1==3 ursprungsland and have either besok1 == 1 or 2 ursprungsland different
besok13_subjects<-VIP_data[!is.na(ursprungsland) & !is.na(besok1) & besok1==3 & Subject_id %in% Subject_id[!is.na(ursprungsland) & !is.na(besok1) & besok1==1],c("Subject_id")]
besok13_subjects<-besok13_subjects[VIP_data[Subject_id %in% Subject_id[!is.na(ursprungsland) & !is.na(besok1) & besok1==3] & !is.na(ursprungsland) & !is.na(besok1) & besok1==1,c("ursprungsland")]!=
				VIP_data[!is.na(ursprungsland) & !is.na(besok1) & besok1==3 & Subject_id %in% Subject_id[!is.na(ursprungsland) & !is.na(besok1) & besok1==1],c("ursprungsland")]]
VIP_data[Subject_id %in% besok13_subjects,c("ursprungsland")]<-NA

besok23_subjects<-VIP_data[!is.na(ursprungsland) & !is.na(besok1) & besok1==3 & Subject_id %in% Subject_id[!is.na(ursprungsland) & !is.na(besok1) & besok1==2],c("Subject_id")]
besok23_subjects<-besok23_subjects[VIP_data[Subject_id %in% Subject_id[!is.na(ursprungsland) & !is.na(besok1) & besok1==3] & !is.na(ursprungsland) & !is.na(besok1) & besok1==2,c("ursprungsland")]!=
				VIP_data[!is.na(ursprungsland) & !is.na(besok1) & besok1==3 & Subject_id %in% Subject_id[!is.na(ursprungsland) & !is.na(besok1) & besok1==2],c("ursprungsland")]]
VIP_data[Subject_id %in% besok23_subjects,c("ursprungsland")]<-NA

#those that have besok1==2 ursprungsland and have besok1 == 1 ursprungsland different
besok12_subjects<-VIP_data[!is.na(ursprungsland) & !is.na(besok1) & besok1==2 & Subject_id %in% Subject_id[!is.na(ursprungsland) & !is.na(besok1) & besok1==1],c("Subject_id")]
besok12_subjects<-besok12_subjects[VIP_data[Subject_id %in% Subject_id[!is.na(ursprungsland) & !is.na(besok1) & besok1==2] & !is.na(ursprungsland) & !is.na(besok1) & besok1==1,c("ursprungsland")]!=
				VIP_data[!is.na(ursprungsland) & !is.na(besok1) & besok1==2 & Subject_id %in% Subject_id[!is.na(ursprungsland) & !is.na(besok1) & besok1==1],c("ursprungsland")]]
VIP_data[Subject_id %in% besok12_subjects,c("ursprungsland")]<-NA
detach(VIP_data)

#save the final file
write.csv(VIP_data, "VIP_data/VIP_170206_cleaned.csv", row.names=FALSE, na="")

#----------------------------JERNEJA---------------------------------------------------

