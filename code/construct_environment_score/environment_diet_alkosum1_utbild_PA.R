library(car)
library(Hmisc)
library(pscl)

#load entire cleaned data (154009 subjects)
VIP_data_all <- read.csv("../../VIP_data/VIP_170206_cleaned.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)

#load the subset
VIP_data_subset <- read.csv("../../VIP_data/VIP_170206_cleaned_subset.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)
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


#check if alkosum1 can be improved with utbild
source(file="../load_variables_independent_dataset_TEI_adjusted.R")

attach(VIP_data_independant)

#alkosum1 has a messy zero inflated distribution
hist(alkosum1)

#if modeled with utbild(to get residuals), would probably be best to transform alkosum1 in mg per day and round it, model it as count data, using zero inflated negative binomial
#although looking at just these results, does not seem to make a big difference:

#no utbild
associations<-lm(bmi_norm_sd~age+agesq+gender_factor+year+ffq_factor+alkosum1)

summary(associations)
#Call:
#		lm(formula = bmi_norm_sd ~ age + agesq + gender_factor + year + 
#						ffq_factor + alkosum1)
#
#Residuals:
#		Min      1Q  Median      3Q     Max 
#-3.3296 -0.6686 -0.0868  0.5612  6.1408 
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    -5.145e+00  1.870e+00  -2.752  0.00593 ** 
#		age             5.751e-02  6.969e-03   8.253  < 2e-16 ***
#		agesq          -5.001e-04  7.138e-05  -7.006 2.48e-12 ***
#		gender_factor2 -3.060e-01  9.545e-03 -32.061  < 2e-16 ***
#		year            1.898e-03  9.306e-04   2.040  0.04139 *  
#		ffq_factor1    -1.535e-01  2.171e-02  -7.069 1.59e-12 ***
#		alkosum1       -1.331e-02  9.514e-04 -13.988  < 2e-16 ***
#		---
#		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.9836 on 46973 degrees of freedom
#(127 observations deleted due to missingness)
#Multiple R-squared:  0.03272,	Adjusted R-squared:  0.0326 
#F-statistic: 264.9 on 6 and 46973 DF,  p-value: < 2.2e-16
#

vif(associations)
#age         agesq gender_factor          year    ffq_factor 
#195.853288    195.796560      1.106015      1.843766      1.801359 
#alkosum1 
#1.109547 






#with utbild
associations<-lm(bmi_norm_sd~age+agesq+gender_factor+year+ffq_factor+alkosum1+utbild)

summary(associations)
#Call:
#		lm(formula = bmi_norm_sd ~ age + agesq + gender_factor + year + 
#						ffq_factor + alkosum1 + utbild)
#
#Residuals:
#		Min      1Q  Median      3Q     Max 
#-3.2975 -0.6643 -0.0860  0.5578  6.0943 
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    -1.398e+01  1.908e+00  -7.328 2.37e-13 ***
#		age             6.073e-02  6.951e-03   8.736  < 2e-16 ***
#		agesq          -5.725e-04  7.127e-05  -8.033 9.76e-16 ***
#		gender_factor2 -2.771e-01  9.615e-03 -28.821  < 2e-16 ***
#		year            6.449e-03  9.513e-04   6.779 1.22e-11 ***
#		ffq_factor1    -1.443e-01  2.171e-02  -6.645 3.07e-11 ***
#		alkosum1       -9.848e-03  9.615e-04 -10.242  < 2e-16 ***
#		utbild         -1.042e-01  4.924e-03 -21.153  < 2e-16 ***
#		---
#		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.9783 on 46789 degrees of freedom
#(310 observations deleted due to missingness)
#Multiple R-squared:  0.04194,	Adjusted R-squared:  0.04179 
#F-statistic: 292.6 on 7 and 46789 DF,  p-value: < 2.2e-16

vif(associations)
#age         agesq gender_factor          year    ffq_factor 
#196.038158    196.392069      1.129894      1.933400      1.795514 
#alkosum1        utbild 
#1.141107      1.331501 



#checking stratified in utbild
associations<-lm(bmi_norm_sd~age+agesq+gender_factor+year+ffq_factor+alkosum1,data=VIP_data_independant[utbild==1,])
summary(associations)
#
#Call:
#		lm(formula = bmi_norm_sd ~ age + agesq + gender_factor + year + 
#						ffq_factor + alkosum1, data = VIP_data_independant[utbild == 
#								1, ])
#
#Residuals:
#		Min      1Q  Median      3Q     Max 
#-3.3515 -0.6547 -0.0452  0.5724  4.9341 
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    -2.388e+01  4.982e+00  -4.794 1.67e-06 ***
#		age             4.684e-02  1.892e-02   2.475 0.013326 *  
#		agesq          -4.912e-04  1.894e-04  -2.594 0.009502 ** 
#		gender_factor2 -1.405e-01  2.315e-02  -6.068 1.36e-09 ***
#		year            1.153e-02  2.475e-03   4.658 3.25e-06 ***
#		ffq_factor1    -3.172e-02  4.176e-02  -0.760 0.447464    
#		alkosum1       -8.759e-03  2.510e-03  -3.490 0.000486 ***
#		---
#		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.9716 on 7849 degrees of freedom
#(221 observations deleted due to missingness)
#Multiple R-squared:  0.0134,	Adjusted R-squared:  0.01265 
#F-statistic: 17.77 on 6 and 7849 DF,  p-value: < 2.2e-16


associations<-lm(bmi_norm_sd~age+agesq+gender_factor+year+ffq_factor+alkosum1,data=VIP_data_independant[utbild==2,])
summary(associations)
#
#Call:
#		lm(formula = bmi_norm_sd ~ age + agesq + gender_factor + year + 
#						ffq_factor + alkosum1, data = VIP_data_independant[utbild == 
#								2, ])
#
#Residuals:
#		Min      1Q  Median      3Q     Max 
#-3.2422 -0.6300 -0.0465  0.5515  5.9352 
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    -2.855e+01  5.160e+00  -5.534 3.26e-08 ***
#		age             8.622e-02  2.057e-02   4.192 2.80e-05 ***
#		agesq          -8.503e-04  2.036e-04  -4.176 3.01e-05 ***
#		gender_factor2 -1.910e-01  2.535e-02  -7.534 5.62e-14 ***
#		year            1.333e-02  2.526e-03   5.276 1.37e-07 ***
#		ffq_factor1    -1.378e-01  4.920e-02  -2.801  0.00512 ** 
#		alkosum1       -9.854e-03  2.530e-03  -3.895 9.93e-05 ***
#		---
#		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.9443 on 6358 degrees of freedom
#(212 observations deleted due to missingness)
#Multiple R-squared:  0.02901,	Adjusted R-squared:  0.02809 
#F-statistic: 31.66 on 6 and 6358 DF,  p-value: < 2.2e-16
#



associations<-lm(bmi_norm_sd~age+agesq+gender_factor+year+ffq_factor+alkosum1,data=VIP_data_independant[utbild==3,])
summary(associations)
#Call:
#		lm(formula = bmi_norm_sd ~ age + agesq + gender_factor + year + 
#						ffq_factor + alkosum1, data = VIP_data_independant[utbild == 
#								3, ])
#
#Residuals:
#		Min      1Q  Median      3Q     Max 
#-3.3328 -0.6897 -0.1015  0.5763  6.0037 
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    -1.667e+01  3.300e+00  -5.052 4.41e-07 ***
#		age             4.877e-02  1.175e-02   4.149 3.35e-05 ***
#		agesq          -4.546e-04  1.237e-04  -3.675 0.000239 ***
#		gender_factor2 -2.644e-01  1.652e-02 -16.005  < 2e-16 ***
#		year            7.813e-03  1.652e-03   4.729 2.27e-06 ***
#		ffq_factor1    -1.841e-01  4.280e-02  -4.302 1.70e-05 ***
#		alkosum1       -1.033e-02  1.724e-03  -5.989 2.15e-09 ***
#		---
#		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 1.011 on 16911 degrees of freedom
#(217 observations deleted due to missingness)
#Multiple R-squared:  0.02872,	Adjusted R-squared:  0.02838 
#F-statistic: 83.35 on 6 and 16911 DF,  p-value: < 2.2e-16
#



associations<-lm(bmi_norm_sd~age+agesq+gender_factor+year+ffq_factor+alkosum1,data=VIP_data_independant[utbild==4,])
summary(associations)

#Call:
#		lm(formula = bmi_norm_sd ~ age + agesq + gender_factor + year + 
#						ffq_factor + alkosum1, data = VIP_data_independant[utbild == 
#								4, ])
#
#Residuals:
#		Min      1Q  Median      3Q     Max 
#-3.2170 -0.6448 -0.0976  0.5255  4.8518 
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    -2.0167295  3.2308326  -0.624  0.53250    
#age             0.0592129  0.0132887   4.456 8.41e-06 ***
#		agesq          -0.0004660  0.0001359  -3.428  0.00061 ***
#		gender_factor2 -0.3548804  0.0163215 -21.743  < 2e-16 ***
#		year            0.0001963  0.0015969   0.123  0.90217    
#		ffq_factor1    -0.1799050  0.0436049  -4.126 3.71e-05 ***
#		alkosum1       -0.0108333  0.0015031  -7.207 5.98e-13 ***
#		---
#		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.9467 on 15651 degrees of freedom
#(227 observations deleted due to missingness)
#Multiple R-squared:  0.04709,	Adjusted R-squared:  0.04673 
#F-statistic: 128.9 on 6 and 15651 DF,  p-value: < 2.2e-16




#but alkosum1 and utbild are correlated
rcorr(alkosum1,utbild,type="spearman")
#x    y
#x 1.00 0.13
#y 0.13 1.00
#
#n
#x     y
#x 47107 46918
#y 46918 46918
#
#P
#x  y 
#x     0
#y  0   



#it even seems apparent on the boxplot
boxplot(alkosum1~utbild)

#and bmi looks very correlated with utbild
boxplot(bmi_norm_sd~utbild)

rcorr(bmi_norm_sd,utbild,type="spearman")
#x     y
#x  1.00 -0.14
#y -0.14  1.00
#
#n
#x     y
#x 46980 46797
#y 46797 46918
#
#P
#x  y 
#x     0
#y  0   


#turn into count data of mg per day
alkosum1_mg<-round(1000*alkosum1)

#obtain residuals
#keep missing values first
alkosum1_mg_residuals<-alkosum1_mg+utbild
model_znb<-zeroinfl(alkosum1_mg~utbild, dist="negbin")
summary(model_znb)
#Call:
#		zeroinfl(formula = alkosum1_mg ~ utbild, dist = "negbin")
#
#Pearson residuals:
#		Min      1Q  Median      3Q     Max 
#-0.8463 -0.6855 -0.2537  0.3573 21.5992 
#
#Count model coefficients (negbin with log link):
#		Estimate Std. Error z value Pr(>|z|)    
#		(Intercept)  8.144205   0.015677  519.49   <2e-16 ***
#		utbild       0.108589   0.005109   21.26   <2e-16 ***
#		Log(theta)  -0.231564   0.006035  -38.37   <2e-16 ***
#		
#		Zero-inflation model coefficients (binomial with logit link):
#		Estimate Std. Error z value Pr(>|z|)    
#		(Intercept) -1.91002    0.04609  -41.44   <2e-16 ***
#		utbild      -0.22618    0.01613  -14.03   <2e-16 ***
#		---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#
#Theta = 0.7933 
#Number of iterations in BFGS optimization: 10 
#Log-likelihood: -4.222e+05 on 5 Df

#get residuals
alkosum1_mg_residuals[!is.na(VIP_data_independant$alkosum1) & !is.na(VIP_data_independant$utbild)]<-model_znb$residuals

#model bmi standardize to be comparable
associations<-lm(bmi_norm_sd~age+agesq+gender_factor+year+ffq_factor+scale(alkosum1_mg_residuals))
summary(associations)
#Call:
#		lm(formula = bmi_norm_sd ~ age + agesq + gender_factor + year + 
#						ffq_factor + scale(alkosum1_mg_residuals))
#
#Residuals:
#		Min      1Q  Median      3Q     Max 
#-3.3327 -0.6692 -0.0877  0.5609  6.1445 
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                  -4.720e+00  1.873e+00  -2.520   0.0117 *  
#		age                           5.657e-02  6.986e-03   8.098 5.73e-16 ***
#		agesq                        -4.880e-04  7.155e-05  -6.820 9.23e-12 ***
#		gender_factor2               -3.012e-01  9.607e-03 -31.346  < 2e-16 ***
#		year                          1.663e-03  9.321e-04   1.784   0.0744 .  
#		ffq_factor1                  -1.512e-01  2.183e-02  -6.925 4.40e-12 ***
#		scale(alkosum1_mg_residuals) -5.561e-02  4.817e-03 -11.544  < 2e-16 ***
#		---
#		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.9836 on 46790 degrees of freedom
#(310 observations deleted due to missingness)
#Multiple R-squared:  0.03155,	Adjusted R-squared:  0.03143 
#F-statistic: 254.1 on 6 and 46790 DF,  p-value: < 2.2e-16
#
#> 

#compare with the standardized alkosum1 and the standarsized alkosum1 with utbild
associations<-lm(bmi_norm_sd~age+agesq+gender_factor+year+ffq_factor+scale(alkosum1))
summary(associations)
#
#Call:
#		lm(formula = bmi_norm_sd ~ age + agesq + gender_factor + year + 
#						ffq_factor + scale(alkosum1))
#
#Residuals:
#		Min      1Q  Median      3Q     Max 
#-3.3296 -0.6686 -0.0868  0.5612  6.1408 
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     -5.203e+00  1.870e+00  -2.783  0.00539 ** 
#		age              5.751e-02  6.969e-03   8.253  < 2e-16 ***
#		agesq           -5.001e-04  7.138e-05  -7.006 2.48e-12 ***
#		gender_factor2  -3.060e-01  9.545e-03 -32.061  < 2e-16 ***
#		year             1.898e-03  9.306e-04   2.040  0.04139 *  
#		ffq_factor1     -1.535e-01  2.171e-02  -7.069 1.59e-12 ***
#		scale(alkosum1) -6.686e-02  4.780e-03 -13.988  < 2e-16 ***
#		---
#		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.9836 on 46973 degrees of freedom
#(127 observations deleted due to missingness)
#Multiple R-squared:  0.03272,	Adjusted R-squared:  0.0326 
#F-statistic: 264.9 on 6 and 46973 DF,  p-value: < 2.2e-16


associations<-lm(bmi_norm_sd~age+agesq+gender_factor+year+ffq_factor+scale(alkosum1)+utbild)
summary(associations)
#
#Call:
#		lm(formula = bmi_norm_sd ~ age + agesq + gender_factor + year + 
#						ffq_factor + scale(alkosum1) + utbild)
#
#Residuals:
#		Min      1Q  Median      3Q     Max 
#-3.2975 -0.6643 -0.0860  0.5578  6.0943 
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     -1.403e+01  1.908e+00  -7.351 2.00e-13 ***
#		age              6.073e-02  6.951e-03   8.736  < 2e-16 ***
#		agesq           -5.725e-04  7.127e-05  -8.033 9.76e-16 ***
#		gender_factor2  -2.771e-01  9.615e-03 -28.821  < 2e-16 ***
#		year             6.449e-03  9.513e-04   6.779 1.22e-11 ***
#		ffq_factor1     -1.443e-01  2.171e-02  -6.645 3.07e-11 ***
#		scale(alkosum1) -4.948e-02  4.831e-03 -10.242  < 2e-16 ***
#		utbild          -1.042e-01  4.924e-03 -21.153  < 2e-16 ***
#		---
#		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.9783 on 46789 degrees of freedom
#(310 observations deleted due to missingness)
#Multiple R-squared:  0.04194,	Adjusted R-squared:  0.04179 
#F-statistic: 292.6 on 7 and 46789 DF,  p-value: < 2.2e-16
#

detach(VIP_data_independant)

#make a diet score with a OLS multi lm

#load independent variables
source(file="../load_variables_independent_dataset_TEI_adjusted.R")


#take only complete cases....45652(44582 with alkosum1 and utbild and g6)

VIP_data_independant_complete_cases <- na.omit(VIP_data_independant[,c("basic_residuals_bmi", "utbild", "alkosum1", "g6", "POLYsum1_TEI_adjusted_norm_sd","MONOsum1_TEI_adjusted_norm_sd","mfetsum1_TEI_adjusted_norm_sd",
						"fettsum1_TEI_adjusted_norm_sd","sacksum1_TEI_adjusted_norm_sd","kolhsum1_TEI_adjusted_norm_sd","FA_TEI_adjusted_norm_sd",
						"protsum1_anim_TEI_adjusted_norm_sd","protsum1_veg_TEI_adjusted_norm_sd",
						"fibesum1_TEI_adjusted_norm_sd","DISAsum1_TEI_adjusted_norm_sd","MOSAsum1_TEI_adjusted_norm_sd","TRANSsum1_TEI_adjusted_norm_sd",
						"NATRsum1_TEI_adjusted_norm_sd","kolesum1_TEI_adjusted_norm_sd","ensum1_norm_sd", "MAGNsum1_TEI_adjusted_norm_sd","FOSFsum1_TEI_adjusted_norm_sd",
						"selesum1_TEI_adjusted_norm_sd","ZINCsum1_TEI_adjusted_norm_sd","retisum1_TEI_adjusted_norm_sd",
						"karosum1_TEI_adjusted_norm_sd","TIAMsum1_TEI_adjusted_norm_sd","Folasum1_TEI_adjusted_norm_sd",
						"B2sum1_TEI_adjusted_norm_sd","NIACsum1_TEI_adjusted_norm_sd","B6sum1_TEI_adjusted_norm_sd","B12sum1_TEI_adjusted_norm_sd",
						"askosum1_TEI_adjusted_norm_sd","Dsum1_TEI_adjusted_norm_sd","tokosum1_TEI_adjusted_norm_sd",
						"VITKsum1_TEI_adjusted_norm_sd","jernsum1_TEI_adjusted_norm_sd","JODIsum1_TEI_adjusted_norm_sd",
						"kalcsum1_TEI_adjusted_norm_sd","KALIsum1_TEI_adjusted_norm_sd")])

#load visit1 variables
source(file="../load_variables_visit1_dataset_TEI_adjusted.R")

#take only complete cases
VIP_data_subset_visit1_complete_cases <- na.omit(VIP_data_subset_visit1[,c("Subject_id","bmi","bmi_norm_sd","age","agesq","gender_factor","year","ffq_factor",
						"basic_residuals_bmi", "utbild","alkosum1","g6",
						"POLYsum1_TEI_adjusted_norm_sd","MONOsum1_TEI_adjusted_norm_sd","mfetsum1_TEI_adjusted_norm_sd",
						"fettsum1_TEI_adjusted_norm_sd","sacksum1_TEI_adjusted_norm_sd","kolhsum1_TEI_adjusted_norm_sd","FA_TEI_adjusted_norm_sd",
						"protsum1_anim_TEI_adjusted_norm_sd","protsum1_veg_TEI_adjusted_norm_sd",
						"fibesum1_TEI_adjusted_norm_sd","DISAsum1_TEI_adjusted_norm_sd","MOSAsum1_TEI_adjusted_norm_sd","TRANSsum1_TEI_adjusted_norm_sd",
						"NATRsum1_TEI_adjusted_norm_sd","kolesum1_TEI_adjusted_norm_sd","ensum1_norm_sd", "MAGNsum1_TEI_adjusted_norm_sd","FOSFsum1_TEI_adjusted_norm_sd",
						"selesum1_TEI_adjusted_norm_sd","ZINCsum1_TEI_adjusted_norm_sd","retisum1_TEI_adjusted_norm_sd",
						"karosum1_TEI_adjusted_norm_sd","TIAMsum1_TEI_adjusted_norm_sd","Folasum1_TEI_adjusted_norm_sd",
						"B2sum1_TEI_adjusted_norm_sd","NIACsum1_TEI_adjusted_norm_sd","B6sum1_TEI_adjusted_norm_sd","B12sum1_TEI_adjusted_norm_sd",
						"askosum1_TEI_adjusted_norm_sd","Dsum1_TEI_adjusted_norm_sd","tokosum1_TEI_adjusted_norm_sd",
						"VITKsum1_TEI_adjusted_norm_sd","jernsum1_TEI_adjusted_norm_sd","JODIsum1_TEI_adjusted_norm_sd",
						"kalcsum1_TEI_adjusted_norm_sd","KALIsum1_TEI_adjusted_norm_sd")])
VIP_data_subset_visit1_complete_cases$bmi_category[VIP_data_subset_visit1_complete_cases$bmi < 25]<-0
VIP_data_subset_visit1_complete_cases$bmi_category[VIP_data_subset_visit1_complete_cases$bmi >= 25 & VIP_data_subset_visit1_complete_cases$bmi < 30]<-1
VIP_data_subset_visit1_complete_cases$bmi_category[VIP_data_subset_visit1_complete_cases$bmi >= 30]<-2
VIP_data_subset_visit1_complete_cases$bmi_category<-as.factor(VIP_data_subset_visit1_complete_cases$bmi_category)

#load visit2 variables
source(file="../load_variables_visit2_dataset_TEI_adjusted.R")

#take only complete cases
VIP_data_subset_visit2_complete_cases <- na.omit(VIP_data_subset_visit2[,c("Subject_id","bmi","bmi_norm_sd","age","agesq","gender_factor","year","ffq_factor",
						"basic_residuals_bmi", "utbild","alkosum1","g6",
						"POLYsum1_TEI_adjusted_norm_sd","MONOsum1_TEI_adjusted_norm_sd","mfetsum1_TEI_adjusted_norm_sd",
						"fettsum1_TEI_adjusted_norm_sd","sacksum1_TEI_adjusted_norm_sd","kolhsum1_TEI_adjusted_norm_sd","FA_TEI_adjusted_norm_sd",
						"protsum1_anim_TEI_adjusted_norm_sd","protsum1_veg_TEI_adjusted_norm_sd",
						"fibesum1_TEI_adjusted_norm_sd","DISAsum1_TEI_adjusted_norm_sd","MOSAsum1_TEI_adjusted_norm_sd","TRANSsum1_TEI_adjusted_norm_sd",
						"NATRsum1_TEI_adjusted_norm_sd","kolesum1_TEI_adjusted_norm_sd","ensum1_norm_sd", "MAGNsum1_TEI_adjusted_norm_sd","FOSFsum1_TEI_adjusted_norm_sd",
						"selesum1_TEI_adjusted_norm_sd","ZINCsum1_TEI_adjusted_norm_sd","retisum1_TEI_adjusted_norm_sd",
						"karosum1_TEI_adjusted_norm_sd","TIAMsum1_TEI_adjusted_norm_sd","Folasum1_TEI_adjusted_norm_sd",
						"B2sum1_TEI_adjusted_norm_sd","NIACsum1_TEI_adjusted_norm_sd","B6sum1_TEI_adjusted_norm_sd","B12sum1_TEI_adjusted_norm_sd",
						"askosum1_TEI_adjusted_norm_sd","Dsum1_TEI_adjusted_norm_sd","tokosum1_TEI_adjusted_norm_sd",
						"VITKsum1_TEI_adjusted_norm_sd","jernsum1_TEI_adjusted_norm_sd","JODIsum1_TEI_adjusted_norm_sd",
						"kalcsum1_TEI_adjusted_norm_sd","KALIsum1_TEI_adjusted_norm_sd")])
VIP_data_subset_visit2_complete_cases$bmi_category[VIP_data_subset_visit2_complete_cases$bmi < 25]<-0
VIP_data_subset_visit2_complete_cases$bmi_category[VIP_data_subset_visit2_complete_cases$bmi >= 25 & VIP_data_subset_visit2_complete_cases$bmi < 30]<-1
VIP_data_subset_visit2_complete_cases$bmi_category[VIP_data_subset_visit2_complete_cases$bmi >= 30]<-2
VIP_data_subset_visit2_complete_cases$bmi_category<-as.factor(VIP_data_subset_visit2_complete_cases$bmi_category)


#keep subjects that were in the complete cases in both visits...31183(30118 with alkosum1 and utbild and g6)
VIP_data_subset_visit2_complete_cases<-VIP_data_subset_visit2_complete_cases[VIP_data_subset_visit2_complete_cases$Subject_id %in% VIP_data_subset_visit1_complete_cases$Subject_id,]
VIP_data_subset_visit1_complete_cases<-VIP_data_subset_visit1_complete_cases[VIP_data_subset_visit1_complete_cases$Subject_id %in% VIP_data_subset_visit2_complete_cases$Subject_id,]


#make matrix of bmi classes visit1
bmi_classes_visit1<-c()
bmi_classes_visit1[VIP_data_subset_visit1_complete_cases$bmi_category==0]<-1
bmi_classes_visit1[VIP_data_subset_visit1_complete_cases$bmi_category!=0]<-0
bmi_classes_visit1<-cbind(bmi_classes_visit1,bmi_classes_visit1, bmi_classes_visit1)
bmi_classes_visit1[VIP_data_subset_visit1_complete_cases$bmi_category!=1,2]<-0
bmi_classes_visit1[VIP_data_subset_visit1_complete_cases$bmi_category==1,2]<-1
bmi_classes_visit1[VIP_data_subset_visit1_complete_cases$bmi_category!=2,3]<-0
bmi_classes_visit1[VIP_data_subset_visit1_complete_cases$bmi_category==2,3]<-1
colnames(bmi_classes_visit1)<-c(1,2,3)


likelihood_null_model_visit1<-logLik(multinom(bmi_category ~ 1,data=VIP_data_subset_visit1_complete_cases))

auc_visit1_01 <- vector("list",4)
auc_visit1_02 <- vector("list",4)
auc_visit1_12 <- vector("list",4)

#make matrix of bmi classes visit2
bmi_classes_visit2<-c()
bmi_classes_visit2[VIP_data_subset_visit2_complete_cases$bmi_category==0]<-1
bmi_classes_visit2[VIP_data_subset_visit2_complete_cases$bmi_category!=0]<-0
bmi_classes_visit2<-cbind(bmi_classes_visit2,bmi_classes_visit2, bmi_classes_visit2)
bmi_classes_visit2[VIP_data_subset_visit2_complete_cases$bmi_category!=1,2]<-0
bmi_classes_visit2[VIP_data_subset_visit2_complete_cases$bmi_category==1,2]<-1
bmi_classes_visit2[VIP_data_subset_visit2_complete_cases$bmi_category!=2,3]<-0
bmi_classes_visit2[VIP_data_subset_visit2_complete_cases$bmi_category==2,3]<-1
colnames(bmi_classes_visit2)<-c(1,2,3)

likelihood_null_model_visit2<-logLik(multinom(bmi_category ~ 1,data=VIP_data_subset_visit2_complete_cases))

auc_visit2_01 <- vector("list",4)
auc_visit2_02 <- vector("list",4)
auc_visit2_12 <- vector("list",4)

#make diet score variables
attach(VIP_data_independant_complete_cases)

multiple_coefficients<-glm(basic_residuals_bmi~POLYsum1_TEI_adjusted_norm_sd+MONOsum1_TEI_adjusted_norm_sd+mfetsum1_TEI_adjusted_norm_sd+
				fettsum1_TEI_adjusted_norm_sd+sacksum1_TEI_adjusted_norm_sd+kolhsum1_TEI_adjusted_norm_sd+FA_TEI_adjusted_norm_sd+
				protsum1_anim_TEI_adjusted_norm_sd+protsum1_veg_TEI_adjusted_norm_sd+
				fibesum1_TEI_adjusted_norm_sd+DISAsum1_TEI_adjusted_norm_sd+MOSAsum1_TEI_adjusted_norm_sd+TRANSsum1_TEI_adjusted_norm_sd+
				NATRsum1_TEI_adjusted_norm_sd+kolesum1_TEI_adjusted_norm_sd+ensum1_norm_sd+ MAGNsum1_TEI_adjusted_norm_sd+
				FOSFsum1_TEI_adjusted_norm_sd+selesum1_TEI_adjusted_norm_sd+ZINCsum1_TEI_adjusted_norm_sd+retisum1_TEI_adjusted_norm_sd+
				karosum1_TEI_adjusted_norm_sd+TIAMsum1_TEI_adjusted_norm_sd+Folasum1_TEI_adjusted_norm_sd+B2sum1_TEI_adjusted_norm_sd+
				NIACsum1_TEI_adjusted_norm_sd+B6sum1_TEI_adjusted_norm_sd+B12sum1_TEI_adjusted_norm_sd+askosum1_TEI_adjusted_norm_sd+
				Dsum1_TEI_adjusted_norm_sd+tokosum1_TEI_adjusted_norm_sd+VITKsum1_TEI_adjusted_norm_sd+jernsum1_TEI_adjusted_norm_sd+
				JODIsum1_TEI_adjusted_norm_sd+kalcsum1_TEI_adjusted_norm_sd+KALIsum1_TEI_adjusted_norm_sd, family=gaussian(link="identity"))$coefficients[-1]

multiple_p_values<-summary(glm(basic_residuals_bmi~POLYsum1_TEI_adjusted_norm_sd+MONOsum1_TEI_adjusted_norm_sd+mfetsum1_TEI_adjusted_norm_sd+
						fettsum1_TEI_adjusted_norm_sd+sacksum1_TEI_adjusted_norm_sd+kolhsum1_TEI_adjusted_norm_sd+FA_TEI_adjusted_norm_sd+
						protsum1_anim_TEI_adjusted_norm_sd+protsum1_veg_TEI_adjusted_norm_sd+
						fibesum1_TEI_adjusted_norm_sd+DISAsum1_TEI_adjusted_norm_sd+MOSAsum1_TEI_adjusted_norm_sd+TRANSsum1_TEI_adjusted_norm_sd+
						NATRsum1_TEI_adjusted_norm_sd+kolesum1_TEI_adjusted_norm_sd+ensum1_norm_sd+ MAGNsum1_TEI_adjusted_norm_sd+
						FOSFsum1_TEI_adjusted_norm_sd+selesum1_TEI_adjusted_norm_sd+ZINCsum1_TEI_adjusted_norm_sd+retisum1_TEI_adjusted_norm_sd+
						karosum1_TEI_adjusted_norm_sd+TIAMsum1_TEI_adjusted_norm_sd+Folasum1_TEI_adjusted_norm_sd+B2sum1_TEI_adjusted_norm_sd+
						NIACsum1_TEI_adjusted_norm_sd+B6sum1_TEI_adjusted_norm_sd+B12sum1_TEI_adjusted_norm_sd+askosum1_TEI_adjusted_norm_sd+
						Dsum1_TEI_adjusted_norm_sd+tokosum1_TEI_adjusted_norm_sd+VITKsum1_TEI_adjusted_norm_sd+jernsum1_TEI_adjusted_norm_sd+
						JODIsum1_TEI_adjusted_norm_sd+kalcsum1_TEI_adjusted_norm_sd+KALIsum1_TEI_adjusted_norm_sd, family=gaussian(link="identity")))$coefficients[113:148]

detach(VIP_data_independant_complete_cases)

multiple_coefficients[multiple_p_values>=0.05]<-0


VIP_data_subset_visit1_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit1_complete_cases[,13:48])%*%
				diag(multiple_coefficients),1,sum,na.rm=T)

VIP_data_subset_visit2_complete_cases$diet_score<-apply(as.matrix(VIP_data_subset_visit2_complete_cases[,13:48])%*%
				diag(multiple_coefficients),1,sum,na.rm=T)

#visit1
#model bmi classes

#with all
bmi_category_model_multi_log_reg<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor + diet_score + alkosum1+ g6+ utbild,
		data=VIP_data_subset_visit1_complete_cases)

stargazer(bmi_category_model_multi_log_reg,type="text")


# calculate average AUC for classes
auc_visit1_01[[1]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,1],bmi_category_model_multi_log_reg$fitted.value[,1]))
auc_visit1_02[[1]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,2],bmi_category_model_multi_log_reg$fitted.value[,2]))
auc_visit1_12[[1]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,3],bmi_category_model_multi_log_reg$fitted.value[,3]))


mean(auc_visit1_01[[1]],auc_visit1_02[[1]],auc_visit1_12[[1]])
#0.6580812
1-logLik(bmi_category_model_multi_log_reg)/likelihood_null_model_visit1
#0.05211517


#without utbild
bmi_category_model_multi_log_reg<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor + diet_score + alkosum1+ g6,
		data=VIP_data_subset_visit1_complete_cases)

stargazer(bmi_category_model_multi_log_reg,type="text")


# calculate average AUC for classes
auc_visit1_01[[2]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,1],bmi_category_model_multi_log_reg$fitted.value[,1]))
auc_visit1_02[[2]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,2],bmi_category_model_multi_log_reg$fitted.value[,2]))
auc_visit1_12[[2]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,3],bmi_category_model_multi_log_reg$fitted.value[,3]))


mean(auc_visit1_01[[2]],auc_visit1_02[[2]],auc_visit1_12[[2]])
#0.6553299
1-logLik(bmi_category_model_multi_log_reg)/likelihood_null_model_visit1
#0.0498074


#without g6
bmi_category_model_multi_log_reg<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor + diet_score + alkosum1+ utbild,
		data=VIP_data_subset_visit1_complete_cases)

stargazer(bmi_category_model_multi_log_reg,type="text")


# calculate average AUC for classes
auc_visit1_01[[3]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,1],bmi_category_model_multi_log_reg$fitted.value[,1]))
auc_visit1_02[[3]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,2],bmi_category_model_multi_log_reg$fitted.value[,2]))
auc_visit1_12[[3]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,3],bmi_category_model_multi_log_reg$fitted.value[,3]))


mean(auc_visit1_01[[3]],auc_visit1_02[[3]],auc_visit1_12[[3]])
#0.6530721
1-logLik(bmi_category_model_multi_log_reg)/likelihood_null_model_visit1
#0.04863714



#without alkosum1
bmi_category_model_multi_log_reg<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor + diet_score + g6 + utbild,
		data=VIP_data_subset_visit1_complete_cases)

stargazer(bmi_category_model_multi_log_reg,type="text")


# calculate average AUC for classes
auc_visit1_01[[4]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,1],bmi_category_model_multi_log_reg$fitted.value[,1]))
auc_visit1_02[[4]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,2],bmi_category_model_multi_log_reg$fitted.value[,2]))
auc_visit1_12[[4]]<-pROC::auc(pROC::roc(bmi_classes_visit1[,3],bmi_category_model_multi_log_reg$fitted.value[,3]))


mean(auc_visit1_01[[4]],auc_visit1_02[[4]],auc_visit1_12[[4]])
#0.6576916
1-logLik(bmi_category_model_multi_log_reg)/likelihood_null_model_visit1
#0.05162163

#test the difference in the AUC for visit1
for ( AUC_diff1 in c(1:4))
{
	for ( AUC_diff2 in c(1:4)){
		
		if(roc.test(auc_visit1_01[[AUC_diff1]], auc_visit1_01[[AUC_diff2]])$p.value >= 0.05){
			
			message(paste0("model comparison: ", AUC_diff1, " , " , AUC_diff2))
			
			message(paste0("AUC difference significance: ", roc.test(auc_visit1_01[[AUC_diff1]], auc_visit1_01[[AUC_diff2]])$p.value))
		}
		
	}
	
}


#test the difference in the AUC for visit1
for ( AUC_diff1 in c(1:4))
{
	for ( AUC_diff2 in c(1:4)){
		
		if(roc.test(auc_visit1_02[[AUC_diff1]], auc_visit1_02[[AUC_diff2]])$p.value >= 0.05){
			
			message(paste0("model comparison: ", AUC_diff1, " , " , AUC_diff2))
			
			message(paste0("AUC difference significance: ", roc.test(auc_visit1_02[[AUC_diff1]], auc_visit1_02[[AUC_diff2]])$p.value))
		}
		
	}
	
}



#test the difference in the AUC for visit1
for ( AUC_diff1 in c(1:4))
{
	for ( AUC_diff2 in c(1:4)){
		
		if(roc.test(auc_visit1_12[[AUC_diff1]], auc_visit1_12[[AUC_diff2]])$p.value >= 0.05){
			
			message(paste0("model comparison: ", AUC_diff1, " , " , AUC_diff2))
			
			message(paste0("AUC difference significance: ", roc.test(auc_visit1_12[[AUC_diff1]], auc_visit1_12[[AUC_diff2]])$p.value))
		}
		
	}
	
}

#important is to look at all against the ones where one is left out, where alkosum1 is left out the p-value is non-significant in some classes ~ 0.07



#visit2
#with all
bmi_category_model_multi_log_reg<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor + diet_score + alkosum1+ g6+ utbild,
		data=VIP_data_subset_visit2_complete_cases)

stargazer(bmi_category_model_multi_log_reg,type="text")


# calculate average AUC for classes
auc_visit2_01[[1]]<-pROC::auc(pROC::roc(bmi_classes_visit2[,1],bmi_category_model_multi_log_reg$fitted.value[,1]))
auc_visit2_02[[1]]<-pROC::auc(pROC::roc(bmi_classes_visit2[,2],bmi_category_model_multi_log_reg$fitted.value[,2]))
auc_visit2_12[[1]]<-pROC::auc(pROC::roc(bmi_classes_visit2[,3],bmi_category_model_multi_log_reg$fitted.value[,3]))


mean(auc_visit2_01[[1]],auc_visit2_02[[1]],auc_visit2_12[[1]])
#0.6524016
1-logLik(bmi_category_model_multi_log_reg)/likelihood_null_model_visit2
#0.04756398


#without utbild
bmi_category_model_multi_log_reg<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor + diet_score + alkosum1+ g6,
		data=VIP_data_subset_visit2_complete_cases)

stargazer(bmi_category_model_multi_log_reg,type="text")


# calculate average AUC for classes
auc_visit2_01[[2]]<-pROC::auc(pROC::roc(bmi_classes_visit2[,1],bmi_category_model_multi_log_reg$fitted.value[,1]))
auc_visit2_02[[2]]<-pROC::auc(pROC::roc(bmi_classes_visit2[,2],bmi_category_model_multi_log_reg$fitted.value[,2]))
auc_visit2_12[[2]]<-pROC::auc(pROC::roc(bmi_classes_visit2[,3],bmi_category_model_multi_log_reg$fitted.value[,3]))


mean(auc_visit2_01[[2]],auc_visit2_02[[2]],auc_visit2_12[[2]])
#0.6499973
1-logLik(bmi_category_model_multi_log_reg)/likelihood_null_model_visit2
#0.04580637


#without g6
bmi_category_model_multi_log_reg<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor + diet_score + alkosum1+ utbild,
		data=VIP_data_subset_visit2_complete_cases)

stargazer(bmi_category_model_multi_log_reg,type="text")


# calculate average AUC for classes
auc_visit2_01[[3]]<-pROC::auc(pROC::roc(bmi_classes_visit2[,1],bmi_category_model_multi_log_reg$fitted.value[,1]))
auc_visit2_02[[3]]<-pROC::auc(pROC::roc(bmi_classes_visit2[,2],bmi_category_model_multi_log_reg$fitted.value[,2]))
auc_visit2_12[[3]]<-pROC::auc(pROC::roc(bmi_classes_visit2[,3],bmi_category_model_multi_log_reg$fitted.value[,3]))


mean(auc_visit2_01[[3]],auc_visit2_02[[3]],auc_visit2_12[[3]])
#0.6456903
1-logLik(bmi_category_model_multi_log_reg)/likelihood_null_model_visit2
#0.04210992



#without alkosum1
bmi_category_model_multi_log_reg<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor + diet_score + g6 + utbild,
		data=VIP_data_subset_visit2_complete_cases)

stargazer(bmi_category_model_multi_log_reg,type="text")


# calculate average AUC for classes
auc_visit2_01[[4]]<-pROC::auc(pROC::roc(bmi_classes_visit2[,1],bmi_category_model_multi_log_reg$fitted.value[,1]))
auc_visit2_02[[4]]<-pROC::auc(pROC::roc(bmi_classes_visit2[,2],bmi_category_model_multi_log_reg$fitted.value[,2]))
auc_visit2_12[[4]]<-pROC::auc(pROC::roc(bmi_classes_visit2[,3],bmi_category_model_multi_log_reg$fitted.value[,3]))


mean(auc_visit2_01[[4]],auc_visit2_02[[4]],auc_visit2_12[[4]])
# 0.6519583
1-logLik(bmi_category_model_multi_log_reg)/likelihood_null_model_visit2
#0.04672227 



#test the difference in the AUC for visit2
for ( AUC_diff1 in c(1:4))
{
	for ( AUC_diff2 in c(1:4)){
		
		if(roc.test(auc_visit2_01[[AUC_diff1]], auc_visit2_01[[AUC_diff2]])$p.value >= 0.05){
			
			message(paste0("model comparison: ", AUC_diff1, " , " , AUC_diff2))
			
			message(paste0("AUC difference significance: ", roc.test(auc_visit2_01[[AUC_diff1]], auc_visit2_01[[AUC_diff2]])$p.value))
		}
		
	}
	
}


#test the difference in the AUC for visit2
for ( AUC_diff1 in c(1:4))
{
	for ( AUC_diff2 in c(1:4)){
		
		if(roc.test(auc_visit2_02[[AUC_diff1]], auc_visit2_02[[AUC_diff2]])$p.value >= 0.05){
			
			message(paste0("model comparison: ", AUC_diff1, " , " , AUC_diff2))
			
			message(paste0("AUC difference significance: ", roc.test(auc_visit2_02[[AUC_diff1]], auc_visit2_02[[AUC_diff2]])$p.value))
		}
		
	}
	
}



#test the difference in the AUC for visit2
for ( AUC_diff1 in c(1:4))
{
	for ( AUC_diff2 in c(1:4)){
		
		if(roc.test(auc_visit2_12[[AUC_diff1]], auc_visit2_12[[AUC_diff2]])$p.value >= 0.05){
			
			message(paste0("model comparison: ", AUC_diff1, " , " , AUC_diff2))
			
			message(paste0("AUC difference significance: ", roc.test(auc_visit2_12[[AUC_diff1]], auc_visit2_12[[AUC_diff2]])$p.value))
		}
		
	}
	
}

#one class non-significant difference from with all and missing g6



#tables

# visit1
#with all
bmi_category_model_multi_log_reg<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor + diet_score + alkosum1+ g6+ utbild,
		data=VIP_data_subset_visit1_complete_cases)

obesity_prediction_visit1<-predict(bmi_category_model_multi_log_reg, VIP_data_subset_visit1_complete_cases[,c("age","agesq","gender_factor","year","ffq_factor","diet_score",
						"alkosum1","g6","utbild")],type="class")

table(obesity_prediction_visit1,VIP_data_subset_visit1_complete_cases$bmi_category)

#obesity_prediction_visit1     0     1     2
#0 							14090  7122  1771
#1  						 2688  3632   809
#2     						   5     0     1



# visit2
#with all
bmi_category_model_multi_log_reg<-multinom(bmi_category ~ age + agesq + gender_factor + year + ffq_factor + diet_score + alkosum1+ g6+ utbild,
		data=VIP_data_subset_visit2_complete_cases)

obesity_prediction_visit2<-predict(bmi_category_model_multi_log_reg, VIP_data_subset_visit2_complete_cases[,c("age","agesq","gender_factor","year","ffq_factor","diet_score",
						"alkosum1","g6","utbild")],type="class")

table(obesity_prediction_visit2,VIP_data_subset_visit2_complete_cases$bmi_category)
#
#obesity_prediction_visit2    0    1    2
#0 							7998 5144 1899
#1 							4818 7524 2523
#2   						 46   77   89


