#HYPERTENSION INCIDENCE
# AUCs #
library(pROC)

glacierExclude <- subset(glacier, is.na(excludefup))
glacierFullHt <- glacierExclude[complete.cases(glacierExclude [,c("htni")]),] ###2943

EnvComp <- glacierFullHt[complete.cases(glacierFullHt[,c("htni", "smoking1" , "pa1" , "enk1" , "tei1" , "mufa1" , "pufa1", "carboh1" , "sugar1" , "protein1" , "satfat1" , "totfat1" , "fiber1" , "fa1" ,
                                                         "alc1" , "vitA1"  ,"vitD1" , "vitE1" , "thia1" , "ribo1" , "niac1" , "vitB61" , "folate1" , "vitB121" , "vitC1" , "calc1" , "phosp1" , "pota1" ,
                                                         "magn1" , "iron1", "zinc1" , "iodine1" , "selenium1", "salt1")]),]  ### 2559

#GENETIC model:###Use EnvComp dataset###
lm.ht.SNPs <- glm((htni) ~ age1 + age21 + follow + sex + rs17367504 + rs2493134 + rs1458038 + rs1173771 + rs1799945 + rs805303 + rs12243859 + rs12246717 + rs10786152 + rs943037 +
                    rs604723 + rs2681472 + rs1378942 + rs13333226 + rs6015450, data=EnvComp, family=binomial)

EnvComp$PredSNP <- predict(lm.ht.SNPs, type="response") #obtain the predicted values based on genetic model



roc.Pred.ht.SNPs <- roc(EnvComp$htni, EnvComp$PredSNP, ci=TRUE) #test the rocAUC
roc.Pred.ht.SNPs #64.86%
plot(roc.Pred.ht.SNPs, print.thres = TRUE)
coords(roc.Pred.ht.SNPs, "all", ret=c("threshold", "sensitivity", "specificity","npv","ppv") )

#LIFESTYLE model:
lm.ht.Env <- glm((htni) ~age1 + age21 + follow + sex + smoking1 + pa1 + enk1 + tei1 + mufa1 + pufa1 + carboh1 + sugar1 + protein1 + satfat1 + totfat1 + fiber1 + fa1 + alc1 + vitA1  + vitD1 + vitE1 +
                   thia1 + ribo1 + niac1 + vitB61 + folate1 + vitB121 + vitC1 + calc1 + phosp1 + pota1 + magn1 + iron1 + zinc1 + iodine1 + selenium1 + salt1, data=EnvComp, family=binomial )

EnvComp$PredEnv <- predict(lm.ht.Env, type="response")

roc.Pred.ht.Env <- roc(EnvComp$htni, EnvComp$PredEnv, ci=TRUE)
roc.Pred.ht.Env#66.78%
lines(roc.Pred.ht.Env, print.thres = TRUE, col="blue")
coords(roc.Pred.ht.Env, "all", ret=c("threshold", "sensitivity", "specificity","npv","ppv") )


#Lifestyle models with categories of pa and smoking were also run but the results did not differ much.

#COMBINED model (Genetic + lifestyle):
lm.ht.EnvGen <- glm((htni) ~age1 + age21 + follow + sex + rs17367504 + rs2493134 + rs1458038 + rs1173771 + rs1799945 + rs805303 + rs12243859 + rs12246717 + rs10786152 + rs943037 + 
                      rs604723 + rs2681472 + rs1378942 + rs13333226 + rs6015450 + smoking1 + pa1 + enk1 + tei1 + mufa1 + pufa1 + carboh1 + sugar1 + protein1 + satfat1 + totfat1 + 
                      fiber1 + fa1 + alc1 + vitA1  + vitD1 + vitE1 + thia1 + ribo1 + niac1 + vitB61 + folate1 + vitB121 + vitC1 + calc1 + phosp1 + pota1 + magn1 + iron1 + zinc1 + 
                      iodine1 + selenium1 + salt1, data=EnvComp, family=binomial )
EnvComp$PredEnvGen <- predict(lm.ht.EnvGen, type="response")

roc.Pred.ht.EnvGen <- roc(EnvComp$htni, EnvComp$PredEnvGen, ci=TRUE)
roc.Pred.ht.EnvGen #67.7%
lines(roc.Pred.ht.EnvGen,col="green")
coords(roc.Pred.ht.EnvGen, "all", ret=c("threshold", "sensitivity", "specificity","npv","ppv") )

#COMPARISON OF THE MODELS
#Genetic vs lifestyle
roc.test(roc.Pred.ht.SNPs, roc.Pred.ht.Env) ##p-value = 0.043

#Combined vs lifestyle
roc.test(roc.Pred.ht.EnvGen, roc.Pred.ht.Env) ##p-value = 0.02198

