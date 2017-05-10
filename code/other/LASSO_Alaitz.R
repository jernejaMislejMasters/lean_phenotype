####LASSO, ELASTIC NET, RIDGE REGRESSION
library(glmnet)
#create the input matrix and response vector
#create predictors matrix
summary(all)

#predictors = as.matrix(cbind(protsum1, protsum1_anim, protsum1_veg, kolhsum1))#transpose

# Covariate matrix: add 1st column as intercept   
#predictors1 = cbind(1, t(predictors))    #add 1st column as intercept

#model matrix with no NA
VIP_full <- na.omit(VIP_independent[,c("bmi", "sacksum1" , "fibesum1" , "protsum1")])

predictors <- as.matrix(VIP_full[,c('sacksum1', 'fibesum1', 'protsum1')])
bmi <- VIP_full$bmi

#LASSO
#run lasso with cross-validation of the model
lasso <- cv.glmnet(predictors,bmi, family="gaussian")
#check te coefficients at lambda.1se
coef(lasso)
#predict coefficients
#predict(lasso, type='coef')

#ELASTIC NET
#run elastic net with cross-validation of the model. alpha value can be changed from 0 to 1.
elastic.net <- cv.glmnet(predictors,bmi, alpha= 0.5, family="gaussian")
#check te coefficients at lambda.1se
coef(elastic.net)
#predict coefficients
#predict(elastic.net, type='coef')

#RIDGE REGRESSION
#run ridge regression with cross-validation of the model
ridge.regression <- cv.glmnet(predictors,bmi, alpha= 0, family="gaussian")
#check te coefficients at lambda.1se
coef(ridge.regression)

