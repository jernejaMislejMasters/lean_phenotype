libname stuff 'N:\GLACIER\gene_vs_lifestyle\'; 

data A0; /*extract neccesary data from the whole database and change names*//*N= 21,717*/
set stuff.glacier_diet_111111;
DATUM = .;
DATUM = provdat;
FORMAT DATUM mmddyy8.; /*Date format change*/ 
age= .;  age=alder;
weight= .; weight= vikt;
SEX = .;
IF gender eq 'M' THEN SEX = 1;
IF gender eq 'K' THEN SEX = 0;
SMOKE = .; 
if rokning= 1 then smoke=3 ;
if rokning= 2 then smoke= 2;
if rokning= 3 then smoke= 1;
agesq = .; agesq = age*age;
visit = .; visit= besok;
EDUCATION = .; 
if 1 le B2 le 3 then education= 1;
if 4 le B2 le 8 then education= 2; 
if B2 eq 9 then education= 3;
PHYSACT=.;
if g6 = 0 then PHYSACT= .; 
if g6 = 1 then PHYSACT = 0;
if g6 = 2 then PHYSACT = 0;
if g6 = 3 then PHYSACT= 1;
if g6 = 4 then PHYSACT= 1;
if g6 = 5 then PHYSACT= 1;
MUFA= .; MUFA= monosum1; 
PUFA= .; PUFA= polysum1; 
CARBOHYDRATES= .; CARBOHYDRATES= kolhsum1;
SUGARS= .; SUGARS= sacksum1;
PROTEIN= .; PROTEIN= protsum1;
TEI= .; TEI= ensum1;
SATFAT= .; SATFAT= mfetsum1;
TOTFAT=.; TOTFAT= fettsum1;
FIBER= .; FIBER= fibesum1;
N3= .; N3= FA183_sum1 + FA205_sum1 + FA226_sum1;
N6= .; N6= FA182_sum1 + FA204_sum1;
FA= .; FA= (N3+N6);
ALC= .; ALC= alkosum1;
SALT= .; SALT= NATRsum1/1000;
VitA= .; VitA= retisum1;
VitD= .; VitD= dsum1;
VitE= .; VitE= tokosum1;
Thia= .; Thia= TIAMsum1;
Ribo= .; Ribo= b2sum1;
Niac= .; Niac= NIACsum1;
VitB6= .; VitB6= B6sum1;
Folate= .; Folate= folasum1;
VitB12= .; VitB12= B12sum1;
VitC= .; VitC= askosum1;
Calc= .; Calc= kalcsum1;
Phosp= .; Phosp= FOSFsum1;
Pota= .; Pota= KALIsum1;
Magn= .; Magn= MAGNsum1;
Iron= .; Iron= jernsum1;
Zinc= .; Zinc= ZINCsum1;
Iodine= .; Iodine= JODIsum1;
Selenium= .; Selenium= selesum1;
enk=.;
if enkver2 eq "short" then enk= 0;
if enkver2 ne "short" then enk= 1;
if 15 le BMI lt 90;
if 18 le age lt 100; 
if exclude ne 1;
if 500 le TEI le 4500 or TEI eq .;
fglu= .; fglu= blods0;
if fglu gt 25 then fglu = .;
if fglu lt 1 then fglu = .;
twoglu= .; twoglu= blods2;
if twoglu gt 35 then twoglu = .;
if twoglu lt 1 then twoglu = .;
FG= .;
if 1 le fglu lt 6.1 then FG = 1;
if 6.1 le fglu lt 7.0 then FG = 2;
if 7.0 le fglu lt 25 then FG = 3;
GT= .;
if 1 le twoglu lt 7.8 then GT = 1;
if 7.8 le twoglu lt 11.1 then GT = 2;
if 11.1 le twoglu lt 35 then GT = 3;
T2D= .;
if GT eq 1 and FG eq 1 then T2D = 0;
if GT eq 2 and FG eq 1 then T2D = .;
if GT eq 3 and FG eq 1 then T2D = 1;
if GT eq . and FG eq 1 then T2D = 0;
if GT eq 1 and FG eq 2 then T2D = .;
if GT eq 1 and FG eq 3 then T2D = 1;
if GT eq 1 and FG eq . then T2D = 0;
if GT eq 2 and FG eq 2 then T2D = .;
if GT eq 2 and FG eq 3 then T2D = 1;
if GT eq 2 and FG eq . then T2D = .;
if GT eq 3 and FG eq 2 then T2D = 1;
if GT eq 3 and FG eq 3 then T2D = 1;
if GT eq 3 and FG eq . then T2D = 1;
if GT eq . and FG eq 2 then T2D = .;
if GT eq . and FG eq 3 then T2D = 1;
obov = .;
if 18.5 le BMI lt 25 then obov = 0;
if 25 le BMI lt 30 then obov= 1;
if  BMI ge 30 then obov = 2;
obes = .;
if obov eq 0 then obes= 0;
if obov eq 2 then obes= 1;
fasta4to8h=.; fasta4to8h= 0; if fasta = 2 then fasta4to8h = 1;
fasta4h=.; fasta4h= 0; if fasta = 3 then fasta4h = 1;
fastamiss=.; fastamiss= 0; if fasta = . then fastamiss = 1;
run; 
proc freq data= A0; table FG GT FG*GT T2D;run;
/*create quartiles for alcohol intake*/
proc rank data= A0 group = 4 out=A0;
VAR ALC;
ranks QT_alc;
run;

data long short apri; set A0; /*extract data to construct the healthy diet score variables*//*split the three different questionaries*/
if enkver2 = 'long' then output long;
if enkver2 = 'short' then output short;
if enkver2 = 'apri' then output apri;
run;

data apri; set apri; /*construct variables in apri questionnaire*/ /*N= 1,731*/
wholegrain_s= da10+da11+da22+da23+da27+0.25*da70;
fish_s= da60+da61;
fruit_s= da29+da30+da31+da32;
vegetables_s= da33+da34+da35+da36+da37+da38;
redmeat_s= da51+da52+da53+da54+da55+da56;
desserts_s= da65+da66+da67+da68+da69;
sugardrink_s= da74+da75;
friedpot_s= da40+da41;
run;

data long; set long; /*construct variables in long questionnaire*/ /*N= 8,155*/
wholegrain_s = da10+da11+da22+da23+da27+0.25*da70;
fish_s = da60+da61;
fruit_s = da29+da30+da31+da32;
vegetables_s = da33+da34+da35+da36+da37+da38;
redmeat_s = da51+da52+da53+da54+da55+da56;
desserts_s = da65+da66+da67+da68+da69;
sugardrink_s = da74+da75;
friedpot_s = da40+da41;
run;

data short; set short;/*construct variables in short questionnaire*/ /*N= 11,831*/
wholegrain_s = dat10+dat11+dat18+dat22+0.25*dat52;
fish_s = dat44+dat45;
fruit_s = dat24+dat25+dat26;
vegetables_s = dat27+dat28+dat29;
redmeat_s = dat37+dat38+dat39+dat40+dat41+dat42;
desserts_s = dat48+dat49+dat50+dat51;
sugardrink_s = dat56;
friedpot_s = dat31;
run;

data testall; set apri long short; run; /*merge the three databases corresponding to each questionnaire*/

proc freq data= testall;
tables visit;
run;


/*********************
   BASELINE (VISIT 1) N= 16,808
**********************/
/*split the visits: extract information on baseline visit*/
data A2; set testall; 
visit1=.; if visit eq 1 then visit1= 1;  
datum1=.; datum1=datum; 
FORMAT DATUM1 mmddyy8.;
age1=.; age1=age; 
weight1= .; weight1= weight; 
BMI1=.; BMI1=BMI; 
AGESQ01=.; AGESQ01=AGESQ; 
SMOKE1 = .; SMOKE1= SMOKE;
EDUCATION1 = .; EDUCATION1 = EDUCATION;
PHYSACT1=.; PHYSACT1 = PHYSACT;
MUFA1= .; MUFA1= MUFA; 
PUFA1= .; PUFA1= PUFA; 
CARBOHYDRATES1= .; CARBOHYDRATES1= CARBOHYDRATES;
SUGARS1= .; SUGARS1= SUGARS;
PROTEIN1= .; PROTEIN1= PROTEIN;
TEI1= .; TEI1= TEI;
SATFAT1= .; SATFAT1= SATFAT;
TOTFAT1=.; TOTFAT1= TOTFAT;
FIBER1= .; FIBER1= FIBER;
FA1= .; FA1= FA;
ALC1= .; ALC1= ALC;
SALT1= .; SALT1= SALT;
VitA1= .; VitA1= VitA;
VitD1= .; VitD1= VitD;
VitE1= .; VitE1= VitE;
Thia1= .; Thia1= Thia;
Ribo1= .; Ribo1= Ribo;
Niac1= .; Niac1= Niac;
VitB61= .; VitB61= VitB6;
Folate1= .; Folate1= Folate;
VitB121= .; VitB121= VitB12;
VitC1= .; VitC1= VitC;
Calc1= .; Calc1= Calc;
Phosp1= .; Phosp1= Phosp;
Pota1= .; Pota1= Pota;
Magn1= .; Magn1= Magn;
Iron1= .; Iron1= Iron;
Zinc1= .; Zinc1= Zinc;
Iodine1= .; Iodine1= Iodine;
Selenium1= .; Selenium1= Selenium;
wholegrain1= .; wholegrain1= wholegrain_s; 
fish1 = .; fish1= fish_s;
fruit1 = .; fruit1 = fruit_s;
vegetables1 = .; vegetables1 = vegetables_s;
redmeat1 = .; redmeat1= redmeat_s;
desserts1 = .; desserts1 = desserts_s;
sugardrink1 = .; sugardrink1 = sugardrink_s;
friedpot1 = .; friedpot1= friedpot_s;
QT_alc1= .; QT_alc1= QT_alc;
enk1 = .; enk1= enk;
fglu1 = .; fglu1= fglu;
twoglu1 = .; twoglu1= twoglu;
FG1 = .; FG1= FG;
GT1=.; GT1= GT;
T2D1=.; T2D1= T2D;
obov1= .; obov1= obov;
obes1= .; obes1= obes;
fastamiss1 = .; fastamiss1 = fastamiss;
fasta4h1 = .; fasta4h1 = fasta4h;
fasta4to8h1 = .; fasta4to8h1 = fasta4to8h;
C61= .; C61= C6;
C71= .; C71= C7;
if visit eq 1 ;
keep id C61 C71 fasta fastamiss1 fasta4h1 fasta4to8h1 fglu1 twoglu1 FG1 GT1 T2D1 obov1 obes1 QT_alc1 enk1 DATUM1 AGE1 SEX weight1 BMI1 visit1 enkver2 AGESQ01 SMOKE1 EDUCATION1 PHYSACT1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 TEI1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 VitA1 VitD1 VitE1 Thia1 Ribo1 Niac1 VitB61 Folate1 VitB121 VitC1 Calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 wholegrain1 fish1 fruit1 vegetables1 redmeat1 desserts1 sugardrink1 friedpot1;  
run; 
proc means data = A2; var fglu1 twoglu1 bmi1 age1 sex QT_alc1 SMOKE1 EDUCATION1 PHYSACT1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 TEI1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 VitA1 VitD1 VitE1 Thia1 Ribo1 Niac1 VitB61 Folate1 VitB121 VitC1 Calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 wholegrain1 fish1 fruit1 vegetables1 redmeat1 desserts1 sugardrink1 friedpot1; run; 
proc freq data= A2; tables sex FG1 GT1 T2D1 obov1 obes1 fastamiss1 fasta4h1 fasta4to8h1; run;


/*********************
   BASELINE (VISIT 2) N= 4,902
**********************/
/*split the visits: extract information on follow-up visit*/
Data A3; set testall; 
visit2=.; if visit eq 2 then visit2= 2; /*** Visit 2***/
datum2=.; datum2=datum; 
FORMAT DATUM2 mmddyy8.;
age2=.; age2=age; 
AGESQ02=.; AGESQ02=AGESQ; 
weight2= .; weight2= weight;
BMI2=.; BMI2=BMI;
SMOKE2 = .; SMOKE2= SMOKE;
EDUCATION2 = .; EDUCATION2 = EDUCATION;
PHYSACT2 =.; PHYSACT2 = PHYSACT;
MUFA2= .; MUFA2= MUFA; 
PUFA2= .; PUFA2= PUFA; 
CARBOHYDRATES2= .; CARBOHYDRATES2= CARBOHYDRATES;
SUGARS2= .; SUGARS2= SUGARS;
PROTEIN2= .; PROTEIN2= PROTEIN;
TEI2= .; TEI2= TEI;
SATFAT2= .; SATFAT2= SATFAT;
TOTFAT2=.; TOTFAT2= TOTFAT;
FIBER2= .; FIBER2= FIBER;
FA2= .; FA2= FA;
ALC2= .; ALC2= ALC;
SALT2= .; SALT2= SALT;
VitA2= .; VitA2= VitA;
VitD2= .; VitD2= VitD;
VitE2= .; VitE2= VitE;
Thia2= .; Thia2= Thia;
Ribo2= .; Ribo2= Ribo;
Niac2= .; Niac2= Niac;
VitB62= .; VitB62= VitB6;
Folate2= .; Folate2= Folate;
VitB122= .; VitB122= VitB12;
VitC2= .; VitC2= VitC;
Calc2= .; Calc2= Calc;
Phosp2= .; Phosp2= Phosp;
Pota2= .; Pota2= Pota;
Magn2= .; Magn2= Magn;
Iron2= .; Iron2= Iron;
Zinc2= .; Zinc2= Zinc;
Iodine2= .; Iodine2= Iodine;
Selenium2= .; Selenium2= Selenium;
wholegrain2= .; wholegrain2= wholegrain_s; 
fish2 = .; fish2= fish_s;
fruit2 = .; fruit2 = fruit_s;
vegetables2 = .; vegetables2 = vegetables_s;
redmeat2 = .; redmeat2= redmeat_s;
desserts2 = .; desserts2 = desserts_s;
sugardrink2 = .; sugardrink2 = sugardrink_s;
friedpot2 = .; friedpot2= friedpot_s;
QT_alc2= .; QT_alc2= QT_alc;
enk2 = .; enk2 = enk;
fglu2 = .; fglu2= fglu;
twoglu2 = .; twoglu2= twoglu;
FG2 = .; FG2= FG;
GT2=.; GT2= GT;
T2D2=.; T2D2= T2D;
obov2= .; obov2= obov;
obes2= .; obes2= obes;
fastamiss2 = .; fastamiss2 = fastamiss;
fasta4h2 = .; fasta4h2 = fasta4h;
fasta4to8h2 = .; fasta4to8h2= fasta4to8h;
C62= .; C62= C6;
C72= .; C72= C7;
if visit eq 2 ; 
keep fasta id C62 C72 fastamiss2 fasta4h2 fasta4to8h2 fglu2 twoglu2 FG2 GT2 T2D2 obov2 obes2 QT_alc2 enk2 DATUM2 AGE2 SEX weight2 BMI2 visit2 enkver2  AGESQ02 SMOKE2 EDUCATION2 PHYSACT2 MUFA2 PUFA2 CARBOHYDRATES2 SUGARS2 PROTEIN2 TEI2 SATFAT2 TOTFAT2 FIBER2 FA2 ALC2 SALT2 VitA2 VitD2 VitE2 Thia2 Ribo2 Niac2 VitB62 Folate2 VitB122 VitC2 Calc2 Phosp2 Pota2 Magn2 Iron2 Zinc2 Iodine2 Selenium2 wholegrain2 fish2 fruit2 vegetables2 redmeat2 desserts2 sugardrink2 friedpot2; 
run; 
proc means data= A3; var fglu2 twoglu2 bmi2 age2 sex QT_alc2 SMOKE2 EDUCATION2 PHYSACT2 MUFA2 PUFA2 CARBOHYDRATES2 SUGARS2 PROTEIN2 TEI2 SATFAT2 TOTFAT2 FIBER2 FA2 ALC2 SALT2 VitA2 VitD2 VitE2 Thia2 Ribo2 Niac2 VitB62 Folate2 VitB122 VitC2 Calc2 Phosp2 Pota2 Magn2 Iron2 Zinc2 Iodine2 Selenium2 wholegrain2 fish2 fruit2 vegetables2 redmeat2 desserts2 sugardrink2 friedpot2; run; 
proc freq data= A3; tables FG2 GT2 T2D2 obov2 obes2 sex fastamiss2 fasta4h2 fasta4to8h2; run;
proc sort data=A2; by id; run; 
proc sort data=A3; by id; run; 

/******************************
MERGE VISIT AND VISIT 2 DATASET
*******************************/
data stuff.A4; /*PROSPECTIVE N= 16,803*/
merge  A2 A3; 
if visit1 eq . and visit2 eq 2 then delete; /*delete individuals with no baseline visit*/
lyear = age2 - age1;/*create follow-up time variable*/
deltaBMI= .;
deltaBMI=BMI2-BMI1;/*create delta variables*/
if BMI1 eq . then deltaBMI= .;
if BMI2 eq . then deltaBMI= .;
deltaweight= weight2-weight1;
if weight1 eq . then deltaweight= .;
if weight2 eq . then deltaweight= .;
deltafglu= .; 
deltafglu= fglu2-fglu1;
if fglu1 eq . then deltafglu= .; 
if fglu2 eq . then deltafglu= .; 
deltatwoglu= .;
deltatwoglu= twoglu2-twoglu1;
if twoglu1 eq . then deltafglu= .; 
if twoglu2 eq . then deltafglu= .;
FGworsen= .;
if FG2 eq 1  and FG1 eq 1 then FGworsen = 0;
if FG2 eq 2  and FG1 eq 1 then FGworsen = 1;
if FG2 eq 3  and FG1 eq 1 then FGworsen = 1;
if FG2 eq .  and FG1 eq 1 then FGworsen = .;
if FG2 eq 1  and FG1 eq 2 then FGworsen = 0;
if FG2 eq 2  and FG1 eq 2 then FGworsen = 0;
if FG2 eq 3  and FG1 eq 2 then FGworsen = 1;
if FG2 eq .  and FG1 eq 2 then FGworsen = .;
if FG2 eq 1  and FG1 eq 3 then FGworsen = 0;
if FG2 eq 2  and FG1 eq 3 then FGworsen = 0;
if FG2 eq 3  and FG1 eq 3 then FGworsen = 0;
if FG2 eq .  and FG1 eq 3 then FGworsen = .;
if FG2 eq 1  and FG1 eq . then FGworsen = .;
if FG2 eq 2  and FG1 eq . then FGworsen = .;
if FG2 eq 3  and FG1 eq . then FGworsen = .;
if FG2 eq .  and FG1 eq . then FGworsen = .;
GTworsen=.;
if GT2 eq 1  and GT1 eq 1 then GTworsen = 0;
if GT2 eq 2  and GT1 eq 1 then GTworsen = 1;
if GT2 eq 3  and GT1 eq 1 then GTworsen = 1;
if GT2 eq .  and GT1 eq 1 then GTworsen = .;
if GT2 eq 1  and GT1 eq 2 then GTworsen = 0;
if GT2 eq 2  and GT1 eq 2 then GTworsen = 0;
if GT2 eq 3  and GT1 eq 2 then GTworsen = 1;
if GT2 eq .  and GT1 eq 2 then GTworsen = .;
if GT2 eq 1  and GT1 eq 3 then GTworsen = 0;
if GT2 eq 2  and GT1 eq 3 then GTworsen = 0;
if GT2 eq 3  and GT1 eq 3 then GTworsen = 0;
if GT2 eq .  and GT1 eq 3 then GTworsen = .;
if GT2 eq 1  and GT1 eq . then GTworsen = .;
if GT2 eq 2  and GT1 eq . then GTworsen = .;
if GT2 eq 3  and GT1 eq . then GTworsen = .;
if GT2 eq .  and GT1 eq . then GTworsen = .;
T2Dincid=.;
if GT1 eq 1 and T2D2 eq 1 and fasta4to8h2 eq 0 then T2Dincid = 1;
if GT1 eq 2 and T2D2 eq 1 and fasta4to8h2 eq 0 then T2Dincid = 1;
if FG1 eq 1 and T2D2 eq 1 and fasta4to8h2 eq 0 then T2Dincid = 1;
if FG1 eq 2 and T2D2 eq 1 and fasta4to8h2 eq 0 then T2Dincid = 1;
if GT1 eq 1 and T2D2 eq 0 then T2Dincid = 0;
if GT1 eq 2 and T2D2 eq 0 then T2Dincid = 0;
if FG1 eq 1 and T2D2 eq 0 then T2Dincid = 0;
if FG1 eq 2 and T2D2 eq 0 then T2Dincid = 0;
if C61 eq 2 and C62 eq 1 then T2Dincid = 1;
if C61 eq 1 and C62 eq 1 then T2Dincid = .;
if C61 eq 1 and C62 eq 2 then T2Dincid = .;
IDFGincid=.;
if FG1 eq 1 and FG2 eq 2 and fastamiss2 eq 0 and fasta4to8h2 eq 0 then IDFGincid = 1;
if FG1 eq 1 and FG2 eq 3 then IDFGincid = .;
if FG1 eq 1 and FG2 eq 1 and C62 eq 2 then IDFGincid = 0;
if FG1 eq 1 and FG2 eq 1 and C62 eq 1 then IDFGincid = .;
if FG1 eq 1 and FG2 eq 1 and C62 eq . then IDFGincid = 0;
IDGTincid=.;
if GT1 eq 1 and GT2 eq 2 and fastamiss2 eq 0 and fasta4to8h2 eq 0 then IDGTincid = 1;
if GT1 eq 1 and GT2 eq 3 then IDGTincid = .;
if GT1 eq 1 and GT2 eq 1 then IDGTincid = 0;
obesincid=.;
if obov1 eq 0 and obes2 eq 1 then obesincid = 1;
if obov1 eq 1 and obes2 eq 1 then obesincid = 1;
if obov1 eq 0 and obes2 eq 0 then obesincid = 0; 
if obov1 eq 1 and obes2 eq 0 then obesincid = 0; 
if lyear ne 0; 
by id;
run; 
proc freq data= stuff.A4;
tables T2Dincid GT1*T2D2*fasta4to8h2 FG1 FG2 GT1 GT2 T2D1 FG1*GT1 FG2*GT2 obes1*obes2 obov1*obov2 visit1 visit2 FG1*FG2 GT1*GT2 T2D1*T2D2 FGworsen*GTworsen T2Dincid obesincid fastamiss1*fastamiss2 fasta4h1*fasta4h2 fasta4to8h1*fasta4to8h2;
run;
proc means data= stuff.A4; var deltaBMI fglu1 twoglu1 fglu2 twoglu2 deltafglu deltatwoglu;run; 

/*************************************
             GENOTYPE DATA
**************************************/
/*GLYCEMIC SNPS*/
PROC IMPORT /*import the list of 9SNPs for 2glu*/
DATAFILE= "N:\GLACIER\gene_vs_lifestyle\snps_9_2glu_Scott2012_tab.txt" 
DBMS= TAB
REPLACE
OUT= stuff.twoglu_SNPs;  
RUN;
proc freq data= stuff.twoglu_SNPs; run; 
data A5a; set stuff.twoglu_SNPs; 
/*code the SNPs by the number of risk alleles*/
rs1260326= .; 	if rs1260326_t		eq 2    then rs1260326   =2;   if   rs1260326_t    eq 1   then rs1260326  =1;   if    rs1260326_t   eq 0	then rs1260326  =0;
rs11717195t= .;  if rs11717195_c		eq 2    then rs11717195t  =0;   if   rs11717195_c   eq 1   then rs11717195t =1;   if    rs11717195_c   eq 0	then rs11717195t  =2;
rs12255372= .;  if rs12255372_t		eq 2    then rs12255372  =2;   if   rs12255372_t   eq 1   then rs12255372 =1;   if    rs12255372_t   eq 0	then rs12255372  =0;
rs1436958= .;  	if rs1436958_t		eq 2    then rs1436958   =2;   if   rs1436958_t    eq 1   then rs1436958  =1;   if    rs1436958_t   eq 0	then rs1436958  =0;
rs11672660= .;	if rs11672660_t		eq 2    then rs11672660  =2;   if   rs11672660_t   eq 1   then rs11672660 =1;   if    rs11672660_t   eq 0	then rs11672660  =0;
rs6975024= .;	if rs6975024_c		eq 2    then rs6975024   =2;   if   rs6975024_c    eq 1   then rs6975024  =1;   if    rs6975024_c   eq 0	then rs6975024  =0;
rs11782386= .;	if rs11782386_t		eq 2    then rs11782386  =0;   if   rs11782386_t   eq 1   then rs11782386 =1;   if    rs11782386_t   eq 0	then rs11782386  =2;
rs1019503= .;	if rs1019503_a		eq 2    then rs1019503   =2;   if   rs1019503_a    eq 1   then rs1019503  =1;   if    rs1019503_a   eq 0	then rs1019503  =0;
rs7651090t= .;	if rs7651090_g		eq 2    then rs7651090t   =2;   if   rs7651090_g    eq 1   then rs7651090t  =1;   if    rs7651090_g   eq 0	then rs7651090t  =0; 

keep 
IID
rs1260326
rs11717195t
rs12255372
rs1436958
rs11672660
rs6975024
rs11782386
rs1019503
rs7651090t; 
run;

PROC IMPORT /*import the list of 36SNPs for fglu*/
DATAFILE= "N:\GLACIER\gene_vs_lifestyle\snps_36_fglu_Scott2012_tab.txt" 
DBMS= TAB
REPLACE
OUT= stuff.fglu_SNPs;  
RUN;
proc freq data= stuff.fglu_SNPs; run; 
data A5b; set stuff.fglu_SNPs; 
/*code the SNPs by the number of risk alleles*/

rs10811661f= .; 	if rs10811661_c		eq 2    then rs10811661f  =0;   if   rs10811661_c	eq 1   then rs10811661f =1;   if    rs10811661_c		eq 0	then rs10811661f  =2;
rs4869272= .; 	if rs4869272_c		eq 2    then rs4869272   =0;   if   rs4869272_c 	eq 1   then rs4869272  =1;   if    rs4869272_c		eq 0	then rs4869272   =2;
rs11619319= .; 	if rs11619319_g		eq 2    then rs11619319  =2;   if	rs11619319_g	eq 1   then rs11619319 =1;   if    rs11619319_g		eq 0	then rs11619319  =0;
rs983309= .; 	if rs983309_t		eq 2    then rs983309    =2;   if   rs983309_t		eq 1   then rs983309   =1;   if    rs983309_t		eq 0	then rs983309    =0;
rs6943153= .; 	if rs6943153_t		eq 2    then rs6943153   =2;   if   rs6943153_t		eq 1   then rs6943153  =1;   if    rs6943153_t		eq 0	then rs6943153   =0;
rs11603334= .; 	if rs11603334_a		eq 2    then rs11603334  =0;   if   rs11603334_a 	eq 1   then rs11603334 =1;   if    rs11603334_a		eq 0	then rs11603334  =2;
rs6113722= .; 	if rs6113722_a		eq 2    then rs6113722   =0;   if   rs6113722_a		eq 1   then rs6113722  =1;   if    rs6113722_a		eq 0	then rs6113722   =2;
rs16913693= .; 	if rs16913693_g		eq 2    then rs16913693  =0;   if   rs16913693_g	eq 1   then rs16913693 =1;   if    rs16913693_g		eq 0	then rs16913693  =2;
rs3829109= .; 	if rs3829109_a		eq 2    then rs3829109   =0;   if   rs3829109_a		eq 1   then rs3829109  =1;   if    rs3829109_a		eq 0	then rs3829109   =2;
rs3783347= .; 	if rs3783347_t		eq 2    then rs3783347   =0;   if   rs3783347_t		eq 1   then rs3783347  =1;   if    rs3783347_t		eq 0	then rs3783347   =2;
rs2302593= .; 	if rs2302593_c		eq 2    then rs2302593   =2;   if   rs2302593_c		eq 1   then rs2302593  =1;   if    rs2302593_c		eq 0	then rs2302593   =0;
rs9368222= .; 	if rs9368222_a		eq 2    then rs9368222   =2;   if   rs9368222_a		eq 1   then rs9368222  =1;   if    rs9368222_a		eq 0	then rs9368222   =0;
rs10747083= .; 	if rs10747083_g		eq 2    then rs10747083  =0;   if   rs10747083_g	eq 1   then rs10747083 =1;   if    rs10747083_g		eq 0	then rs10747083  =2;
rs6072275= .; 	if rs6072275_a		eq 2    then rs6072275   =2;   if   rs6072275_a		eq 1   then rs6072275  =1;   if    rs6072275_a		eq 0	then rs6072275   =0;
rs7651090f= .; 	if rs7651090_g		eq 2    then rs7651090f   =2;   if   rs7651090_g		eq 1   then rs7651090f  =1;   if    rs7651090_g		eq 0	then rs7651090f   =0;
rs576674= .; 	if rs576674_g		eq 2    then rs576674    =2;   if   rs576674_g		eq 1   then rs576674   =1;   if    rs576674_g		eq 0	then rs576674    =0;
rs11715915= .; 	if rs11715915_t		eq 2    then rs11715915  =0;   if   rs11715915_t	eq 1   then rs11715915 =1;   if    rs11715915_t		eq 0	then rs11715915  =2;
rs17762454= .; 	if rs17762454_t		eq 2    then rs17762454  =2;   if   rs17762454_t	eq 1   then rs17762454 =1;   if    rs17762454_t		eq 0	then rs17762454  =0;
rs7708285= .; 	if rs7708285_g		eq 2    then rs7708285   =2;   if   rs7708285_g		eq 1   then rs7708285  =1;   if    rs7708285_g		eq 0	then rs7708285   =0;
rs2657879= .; 	if rs2657879_g		eq 2    then rs2657879   =2;   if   rs2657879_g		eq 1   then rs2657879  =1;   if    rs2657879_g		eq 0	then rs2657879   =0;
rs340874f= .; 	if rs340874_t		eq 2    then rs340874f    =0;   if   rs340874_t		eq 1   then rs340874f   =1;   if    rs340874_t		eq 0	then rs340874f    =2;
rs780094f= .; 	if rs780094_t		eq 2    then rs780094f    =0;   if   rs780094_t		eq 1   then rs780094f   =1;   if    rs780094_t		eq 0	then rs780094f    =2;
rs560887= .; 	if rs560887_t		eq 2    then rs560887    =0;   if   rs560887_t		eq 1   then rs560887   =1;   if    rs560887_t		eq 0	then rs560887    =2;
rs11708067= .; 	if rs11708067_g		eq 2    then rs11708067  =0;   if   rs11708067_g	eq 1   then rs11708067 =1;   if    rs11708067_g		eq 0	then rs11708067  =2;
rs1280= .; 		if rs1280_c			eq 2    then rs1280      =0;   if   rs1280_c		eq 1   then rs1280     =1;   if    rs1280_c			eq 0	then rs1280      =2;
rs2191349= .; 	if rs2191349_g		eq 2    then rs2191349   =0;   if   rs2191349_g		eq 1   then rs2191349  =1;   if    rs2191349_g		eq 0	then rs2191349   =2;
rs2908289= .; 	if rs2908289_a		eq 2    then rs2908289   =2;   if   rs2908289_a		eq 1   then rs2908289  =1;   if    rs2908289_a		eq 0	then rs2908289   =0;
rs11558471= .; 	if rs11558471_g		eq 2    then rs11558471  =0;   if   rs11558471_g	eq 1   then rs11558471 =1;   if    rs11558471_g		eq 0	then rs11558471  =2;
rs10814916= .; 	if rs10814916_c		eq 2    then rs10814916  =2;   if   rs10814916_c	eq 1   then rs10814916 =1;   if    rs10814916_c		eq 0	then rs10814916  =0;
rs11195502= .; 	if rs11195502_t		eq 2    then rs11195502  =0;   if   rs11195502_t	eq 1   then rs11195502 =1;   if    rs11195502_t		eq 0	then rs11195502  =2;
rs7903146f= .; 	if rs7903146_t		eq 2    then rs7903146f   =2;   if   rs7903146_t		eq 1   then rs7903146f  =1;   if    rs7903146_t		eq 0	then rs7903146f   =0;
rs11607883= .; 	if rs11607883_g		eq 2    then rs11607883  =2;   if   rs11607883_g	eq 1   then rs11607883 =1;   if    rs11607883_g		eq 0	then rs11607883  =0;
rs11039182= .; 	if rs11039182_c		eq 2    then rs11039182  =0;   if   rs11039182_c	eq 1   then rs11039182 =1;   if    rs11039182_c		eq 0	then rs11039182  =2;
rs174576= .; 	if rs174576_a		eq 2    then rs174576    =0;   if   rs174576_a		eq 1   then rs174576   =1;   if    rs174576_a		eq 0	then rs174576    =2;
rs10830963f= .; 	if rs10830963_g		eq 2    then rs10830963f =2;   if   rs10830963_g	eq 1   then rs10830963f =1;   if    rs10830963_g		eq 0	then rs10830963f  =0;
rs4502156f= .; 	if rs4502156_c		eq 2    then rs4502156f  =0;   if   rs4502156_c		eq 1   then rs4502156f  =1;   if    rs4502156_c		eq 0	then rs4502156f   =2;

keep 
IID
rs10811661f
rs4869272
rs11619319
rs983309
rs6943153
rs11603334
rs6113722
rs16913693
rs3829109
rs3783347
rs2302593
rs9368222
rs10747083
rs6072275
rs7651090f
rs576674
rs11715915
rs17762454
rs7708285
rs2657879
rs340874f
rs780094f
rs560887
rs11708067
rs1280
rs2191349
rs2908289
rs11558471
rs10814916
rs11195502
rs7903146f
rs11607883
rs11039182
rs174576
rs10830963f
rs4502156f; 
run;

PROC IMPORT /*import the list of 65SNPs for T2D*/
DATAFILE= "N:\GLACIER\gene_vs_lifestyle\snps_65_T2D_Morris2012_MC_tab.txt" 
DBMS= TAB
REPLACE
OUT= stuff.TtwoD_SNPs;  
RUN;
proc freq data= stuff.TtwoD_SNPs; run; 
data A5c; set stuff.TtwoD_SNPs; 
/*code the SNPs by the number of risk alleles*/

rs340874d	=	.; 	if rs340874_t	eq 2	then rs340874d	=0;	if	rs340874_t		eq 1	then rs340874d	=1;	if	rs340874_t		eq 0	then rs340874d	=2;
rs7515431	=	.; 	if rs7515431_t	eq 2	then rs7515431	=2;	if	rs7515431_t		eq 1	then rs7515431	=1;	if	rs7515431_t		eq 0	then rs7515431	=0;
rs7903146d	=	.; 	if rs7903146_t	eq 2	then rs7903146d	=2;	if	rs7903146_t		eq 1	then rs7903146d	=1;	if	rs7903146_t		eq 0	then rs7903146d	=0;
rs12571751	=	.; 	if rs12571751_g	eq 2	then rs12571751	=0;	if	rs12571751_g	eq 1	then rs12571751	=1;	if	rs12571751_g	eq 0	then rs12571751	=2;
rs12242953	=	.; 	if rs12242953_a	eq 2	then rs12242953	=0;	if	rs12242953_a	eq 1	then rs12242953	=1;	if	rs12242953_a	eq 0	then rs12242953	=2;
rs11257655	=	.; 	if rs11257655_t	eq 2	then rs11257655	=2;	if	rs11257655_t	eq 1	then rs11257655	=1;	if	rs11257655_t	eq 0	then rs11257655	=0;
rs1111875	=	.; 	if rs1111875_t	eq 2	then rs1111875	=0;	if	rs1111875_t		eq 1	then rs1111875	=1;	if	rs1111875_t		eq 0	then rs1111875	=2;
rs5215		=	.; 	if rs5215_c		eq 2	then rs5215		=2;	if	rs5215_c		eq 1	then rs5215		=1;	if	rs5215_c		eq 0	then rs5215		=0;
rs2334499	=	.; 	if rs2334499_t	eq 2	then rs2334499	=2;	if	rs2334499_t		eq 1	then rs2334499	=1;	if	rs2334499_t		eq 0	then rs2334499	=0;
rs163184	=	.; 	if rs163184_t	eq 2	then rs163184	=0; if	rs163184_t		eq 1	then rs163184	=1;	if	rs163184_t		eq 0	then rs163184	=2;
rs1552224	=	.; 	if rs1552224_c	eq 2	then rs1552224	=0;	if	rs1552224_c		eq 1	then rs1552224	=1;	if	rs1552224_c		eq 0	then rs1552224	=2;
rs10830963d	=	.; 	if rs10830963_g	eq 2	then rs10830963d	=2;	if	rs10830963_g	eq 1	then rs10830963d	=1;	if	rs10830963_g	eq 0	then rs10830963d	=0;
rs7955901	=	.; 	if rs7955901_c	eq 2	then rs7955901	=2;	if	rs7955901_c		eq 1	then rs7955901	=1;	if	rs7955901_c		eq 0	then rs7955901	=0;
rs2261181	=	.; 	if rs2261181_t	eq 2	then rs2261181	=2;	if	rs2261181_t		eq 1	then rs2261181	=1;	if	rs2261181_t		eq 0	then rs2261181	=0;
rs7965349	=	.; 	if rs7965349_t	eq 2	then rs7965349	=0;	if	rs7965349_t		eq 1	then rs7965349	=1;	if	rs7965349_t		eq 0	then rs7965349	=2;
rs11063069	=	.; 	if rs11063069_g	eq 2	then rs11063069	=2;	if	rs11063069_g	eq 1	then rs11063069	=1;	if	rs11063069_g	eq 0	then rs11063069	=0;
rs10842994	=	.; 	if rs10842994_t	eq 2	then rs10842994	=0;	if	rs10842994_t	eq 1	then rs10842994	=1;	if	rs10842994_t	eq 0	then rs10842994	=2;
rs1359790	=	.; 	if rs1359790_a	eq 2	then rs1359790	=0;	if	rs1359790_a		eq 1	then rs1359790	=1;	if	rs1359790_a		eq 0	then rs1359790	=2;
rs7177055	=	.; 	if rs7177055_g	eq 2	then rs7177055	=0;	if	rs7177055_g		eq 1	then rs7177055	=1;	if	rs7177055_g		eq 0	then rs7177055	=2;
rs4502156d	=	.; 	if rs4502156_c	eq 2	then rs4502156d	=0;	if	rs4502156_c		eq 1	then rs4502156d	=1;	if	rs4502156_c		eq 0	then rs4502156d	=2;
rs2007084	=	.; 	if rs2007084_a	eq 2	then rs2007084	=0;	if	rs2007084_a		eq 1	then rs2007084	=1;	if	rs2007084_a		eq 0	then rs2007084	=2;
rs12899811	=	.; 	if rs12899811_g	eq 2	then rs12899811	=2;	if	rs12899811_g	eq 1	then rs12899811	=1;	if	rs12899811_g	eq 0	then rs12899811	=0;
rs11634397	=	.; 	if rs11634397_a	eq 2	then rs11634397	=0;	if	rs11634397_a	eq 1	then rs11634397	=1;	if	rs11634397_a	eq 0	then rs11634397	=2;
rs9923233	=	.; 	if rs9923233_c	eq 2	then rs9923233	=2;	if	rs9923233_c		eq 1	then rs9923233	=1;	if	rs9923233_c		eq 0	then rs9923233	=0;
rs7202877	=	.; 	if rs7202877_g	eq 2	then rs7202877	=0;	if	rs7202877_g		eq 1	then rs7202877	=1;	if	rs7202877_g		eq 0	then rs7202877	=2;
rs11651755	=	.; 	if rs11651755_c	eq 2	then rs11651755	=2;	if	rs11651755_c	eq 1	then rs11651755	=1;	if	rs11651755_c	eq 0	then rs11651755	=0;
rs2447090	=	.; 	if rs2447090_g	eq 2	then rs2447090	=0;	if	rs2447090_g		eq 1	then rs2447090	=1;	if	rs2447090_g		eq 0	then rs2447090	=2;
rs11663816	=	.; 	if rs11663816_c	eq 2	then rs11663816	=2;	if	rs11663816_c	eq 1	then rs11663816	=1;	if	rs11663816_c	eq 0	then rs11663816	=0;
rs8182584	=	.; 	if rs8182584_t	eq 2	then rs8182584	=2;	if	rs8182584_t		eq 1	then rs8182584	=1;	if	rs8182584_t		eq 0	then rs8182584	=0;
rs8108269	=	.; 	if rs8108269_g	eq 2	then rs8108269	=2;	if	rs8108269_g		eq 1	then rs8108269	=1;	if	rs8108269_g		eq 0	then rs8108269	=0;
rs10401969	=	.; 	if rs10401969_c	eq 2	then rs10401969	=2;	if	rs10401969_c	eq 1	then rs10401969	=1;	if	rs10401969_c	eq 0	then rs10401969	=0;
rs780094d	=	.; 	if rs780094_t	eq 2	then rs780094d	=0;	if	rs780094_t		eq 1	then rs780094d	=1;	if	rs780094_t		eq 0	then rs780094d	=2;
rs7569522	=	.; 	if rs7569522_a	eq 2	then rs7569522	=2;	if	rs7569522_a		eq 1	then rs7569522	=1;	if	rs7569522_a		eq 0	then rs7569522	=0;
rs2943640	=	.; 	if rs2943640_a	eq 2	then rs2943640	=0;	if	rs2943640_a		eq 1	then rs2943640	=1;	if	rs2943640_a		eq 0	then rs2943640	=2;
rs243083	=	.; 	if rs243083_g	eq 2	then rs243083	=2;	if	rs243083_g		eq 1	then rs243083	=1;	if	rs243083_g		eq 0	then rs243083	=0;
rs1128249	=	.; 	if rs1128249_t	eq 2	then rs1128249	=0;	if	rs1128249_t		eq 1	then rs1128249	=1;	if	rs1128249_t		eq 0	then rs1128249	=2;
rs10203174	=	.; 	if rs10203174_t	eq 2	then rs10203174	=0;	if	rs10203174_t	eq 1	then rs10203174	=1;	if	rs10203174_t	eq 0	then rs10203174	=2;
rs4812829	=	.; 	if rs4812829_a	eq 2	then rs4812829	=2;	if	rs4812829_a		eq 1	then rs4812829	=1;	if	rs4812829_a		eq 0	then rs4812829	=0;
rs6795735	=	.; 	if rs6795735_t	eq 2	then rs6795735	=0;	if	rs6795735_t		eq 1	then rs6795735	=1;	if	rs6795735_t		eq 0	then rs6795735	=2;
rs4402960	=	.; 	if rs4402960_t	eq 2	then rs4402960	=2;	if	rs4402960_t		eq 1	then rs4402960	=1;	if	rs4402960_t		eq 0	then rs4402960	=0;
rs2197423	=	.; 	if rs2197423_a	eq 2	then rs2197423	=0;	if	rs2197423_a		eq 1	then rs2197423	=1;	if	rs2197423_a		eq 0	then rs2197423	=2;
rs17301514	=	.; 	if rs17301514_a	eq 2	then rs17301514	=2;	if	rs17301514_a	eq 1	then rs17301514	=1;	if	rs17301514_a	eq 0	then rs17301514	=0;
rs1496653	=	.; 	if rs1496653_g	eq 2	then rs1496653	=0;	if	rs1496653_g		eq 1	then rs1496653	=1;	if	rs1496653_g		eq 0	then rs1496653	=2;
rs12497268	=	.; 	if rs12497268_c	eq 2	then rs12497268	=0;	if	rs12497268_c	eq 1	then rs12497268	=1;	if	rs12497268_c	eq 0	then rs12497268	=2;
rs11717195d	=	.; 	if rs11717195_c	eq 2	then rs11717195d	=0;	if	rs11717195_c	eq 1	then rs11717195d	=1;	if	rs11717195_c	eq 0	then rs11717195d	=2;
rs6819243	=	.; 	if rs6819243_c	eq 2	then rs6819243	=0;	if	rs6819243_c		eq 1	then rs6819243	=1;	if	rs6819243_c		eq 0	then rs6819243	=2;
rs10012946	=	.; 	if rs10012946_t	eq 2	then rs10012946	=0;	if	rs10012946_t	eq 1	then rs10012946	=1;	if	rs10012946_t	eq 0	then rs10012946	=2;
rs6878122	=	.; 	if rs6878122_g	eq 2	then rs6878122	=2;	if	rs6878122_g		eq 1	then rs6878122	=1;	if	rs6878122_g		eq 0	then rs6878122	=0;
rs459193	=	.; 	if rs459193_a	eq 2	then rs459193	=0;	if	rs459193_a		eq 1	then rs459193	=1;	if	rs459193_a		eq 0	then rs459193	=2;
rs7756992	=	.; 	if rs7756992_g	eq 2	then rs7756992	=2;	if	rs7756992_g		eq 1	then rs7756992	=1;	if	rs7756992_g		eq 0	then rs7756992	=0;
rs4299828	=	.; 	if rs4299828_g	eq 2	then rs4299828	=0;	if	rs4299828_g		eq 1	then rs4299828	=1;	if	rs4299828_g		eq 0	then rs4299828	=2;
rs3734621	=	.; 	if rs3734621_c	eq 2	then rs3734621	=2;	if	rs3734621_c		eq 1	then rs3734621	=1;	if	rs3734621_c		eq 0	then rs3734621	=0;
rs849135	=	.; 	if rs849135_a	eq 2	then rs849135	=0;	if	rs849135_a		eq 1	then rs849135	=1;	if	rs849135_a		eq 0	then rs849135	=2;
rs10229583	=	.; 	if rs10229583_a	eq 2	then rs10229583	=0;	if	rs10229583_a	eq 1	then rs10229583	=1;	if	rs10229583_a	eq 0	then rs10229583	=2;
rs17168486	=	.; 	if rs17168486_t	eq 2	then rs17168486	=2;	if	rs17168486_t	eq 1	then rs17168486	=1;	if	rs17168486_t	eq 0	then rs17168486	=0;
rs13233731	=	.; 	if rs13233731_g	eq 2	then rs13233731	=2;	if	rs13233731_g	eq 1	then rs13233731	=1;	if	rs13233731_g	eq 0	then rs13233731	=0;
rs3824065	=	.; 	if rs3824065_t	eq 2	then rs3824065	=0;	if	rs3824065_t		eq 1	then rs3824065	=1;	if	rs3824065_t		eq 0	then rs3824065	=2;
rs7845219	=	.; 	if rs7845219_c	eq 2	then rs7845219	=0;	if	rs7845219_c		eq 1	then rs7845219	=1;	if	rs7845219_c		eq 0	then rs7845219	=2;
rs516946	=	.; 	if rs516946_t	eq 2	then rs516946	=0;	if	rs516946_t		eq 1	then rs516946	=1;	if	rs516946_t		eq 0	then rs516946	=2;
rs3802177	=	.; 	if rs3802177_a	eq 2	then rs3802177	=0;	if	rs3802177_a		eq 1	then rs3802177	=1;	if	rs3802177_a		eq 0	then rs3802177	=2;
rs2796441	=	.; 	if rs2796441_a	eq 2	then rs2796441	=0;	if	rs2796441_a		eq 1	then rs2796441	=1;	if	rs2796441_a		eq 0	then rs2796441	=2;
rs17791513	=	.; 	if rs17791513_g	eq 2	then rs17791513	=0;	if	rs17791513_g	eq 1	then rs17791513	=1;	if	rs17791513_g	eq 0	then rs17791513	=2;
rs16927668	=	.; 	if rs16927668_t	eq 2	then rs16927668	=2;	if	rs16927668_t	eq 1	then rs16927668	=1;	if	rs16927668_t	eq 0	then rs16927668	=0;
rs10811661d	=	.; 	if rs10811661_c	eq 2	then rs10811661d	=0;	if	rs10811661_c	eq 1	then rs10811661d	=1;	if	rs10811661_c	eq 0	then rs10811661d	=2;
rs10758593	=	.; 	if rs10758593_a	eq 2	then rs10758593	=2;	if	rs10758593_a	eq 1	then rs10758593	=1;	if	rs10758593_a	eq 0	then rs10758593	=0;

keep 
IID
rs340874d
rs7515431
rs7903146d
rs12571751
rs12242953
rs11257655
rs1111875
rs5215
rs2334499
rs163184
rs1552224
rs10830963d
rs7955901
rs2261181
rs7965349
rs11063069
rs10842994
rs1359790
rs7177055
rs4502156d
rs2007084
rs12899811
rs11634397
rs9923233
rs7202877
rs11651755
rs2447090
rs11663816
rs8182584
rs8108269
rs10401969
rs780094d
rs7569522
rs2943640
rs243083
rs1128249
rs10203174
rs4812829
rs6795735
rs4402960
rs2197423
rs17301514
rs1496653
rs12497268
rs11717195d
rs6819243
rs10012946
rs6878122
rs459193
rs7756992
rs4299828
rs3734621
rs849135
rs10229583
rs17168486
rs13233731
rs3824065
rs7845219
rs516946
rs3802177
rs2796441
rs17791513
rs16927668
rs10811661d
rs10758593; 
run;

/*BMI SNPS*/
PROC IMPORT /*import the list of 97SNPs for BMI*/
DATAFILE= "N:\GLACIER\gene_vs_lifestyle\BMI_list_12OCT2014.txt" 
DBMS= DLM
OUT= stuff.BMI_SNPs;  
RUN;
proc freq data= stuff.BMI_SNPs; table rs4787491_G; run; 
data A5; set stuff.BMI_SNPs; 

if rs13107325_T eq 'NA' then rs13107325_T = .; /*change NA values to missing values in SAS*/
if rs2820292_A  eq 'NA' then  rs2820292_A = .; 
if rs2121279_T eq 'NA' then  rs2121279_T = .; 

/*code the SNPs by the number of risk alleles*/
rs977747    = .;  if   rs977747_T      eq 2    then rs977747   =2;   if   rs977747_T    eq 1   then rs977747  =1;   if    rs977747_T   eq 0           then rs977747  =0;
rs11583200  = .;  if    rs11583200_C   eq 2    then rs11583200 =2;   if   rs11583200_C  eq 1   then rs11583200 =1;   if    rs11583200_C  eq 0         then rs11583200  =0;
rs3101336   = .;  if    rs3101336_T    eq 2    then rs3101336  =0;   if   rs3101336_T    eq 1  then rs3101336  =1;   if    rs3101336_T   eq 0         then rs3101336   =2;
rs12401738  = .;  if    rs12401738_A   eq 2    then rs12401738  =2;   if   rs12401738_A  eq 1  then rs12401738  =1;   if    rs12401738_A   eq 0       then rs12401738   =0;
rs11165643 = .;   if    rs11165643_C   eq 2    then rs11165643 =0;   if   rs11165643_C eq 1  then rs11165643 =1;   if    rs11165643_C  eq 0           then rs11165643  =2;
rs17024393  = .;  if    rs17024393_C   eq 2    then rs17024393  =2;   if   rs17024393_C  eq 1  then rs17024393  =1;   if    rs17024393_C   eq 0       then rs17024393   =0;
rs543874    = .;  if    rs543874_G     eq 2    then rs543874  =2;   if   rs543874_G    eq 1    then rs543874  =1;   if    rs543874_G   eq 0           then rs543874   =0;
rs2820292  = .;   if     rs2820292_A   eq 2    then rs2820292  =0;   if   rs2820292_A    eq 1  then rs2820292  =1;   if    rs2820292_A   eq 0         then rs2820292   =2;
rs13021737  = .;  if    rs13021737_A   eq 2    then rs13021737  =0;   if   rs13021737_A   eq 1 then rs13021737  =1;   if    rs13021737_A   eq 0       then rs13021737   =2;
rs10182181  = .;  if    rs10182181_G   eq 2    then  rs10182181 =2;   if   rs10182181_G   eq 1 then rs10182181  =1;   if    rs10182181_G   eq 0       then rs10182181   =0;
rs11126666 = .;   if    rs11126666_A   eq 2    then rs11126666 =2;   if   rs11126666_A   eq 1  then rs11126666 =1;   if    rs11126666_A  eq 0         then rs11126666  =0;
rs1016287   = .;  if    rs1016287_T    eq 2    then rs1016287 =2;   if   rs1016287_T   eq 1    then rs1016287 =1;   if    rs1016287_T  eq 0           then rs1016287  =0;
rs1528435  = .;  if    rs1528435_C     eq 2     then rs1528435  =0;   if   rs1528435_C    eq 1  then rs1528435  =1;   if    rs1528435_C   eq 0         then rs1528435   =2;
rs17203016  = .;  if    rs17203016_G   eq 2    then rs17203016  =2;   if   rs17203016_G  eq 1  then rs17203016  =1;   if    rs17203016_G   eq 0       then rs17203016   =0;
rs7599312  = .;   if    rs7599312_A    eq 2    then rs7599312  =0;   if   rs7599312_A    eq 1  then rs7599312 =1;   if    rs7599312_A   eq 0          then rs7599312   =2;
rs492400  = .;    if    rs492400_C     eq 2    then rs492400  =2;   if   rs492400_C    eq 1    then rs492400  =1;   if    rs492400_C   eq 0           then rs492400   =0;
rs6804842  = .;   if    rs6804842_A    eq 2    then rs6804842  =0;   if   rs6804842_A   eq 1   then rs6804842  =1;   if    rs6804842_A  eq 0          then rs6804842  =2;
rs2365389   = .;  if    rs2365389_T    eq 2    then rs2365389   =0;   if   rs2365389_T  eq 1   then rs2365389   =1;   if    rs2365389_T   eq 0        then rs2365389   =2;
rs3849570 = .;    if    rs3849570_A    eq 2    then rs3849570 =2;   if   rs3849570_A  eq 1     then rs3849570 =1;   if    rs3849570_A  eq 0           then rs3849570  =0;
rs10938397  = .;  if    rs10938397_G   eq 2    then rs10938397  =2;   if   rs10938397_G  eq 1  then rs10938397  =1;   if    rs10938397_G   eq 0       then rs10938397   =0;
rs13107325  = .;  if    rs13107325_T   eq 2    then rs13107325  =2;   if   rs13107325_T  eq 1  then rs13107325  =1;   if    rs13107325_T   eq 0       then rs13107325   =0;
rs11727676  = .;  if    rs11727676_C   eq 2    then rs11727676  =0;   if   rs11727676_C  eq 1  then rs11727676  =1;   if    rs11727676_C   eq 0       then rs11727676   =2;
rs2112347  = .;   if    rs2112347_G    eq 2    then rs2112347  =0;   if   rs2112347_G    eq 1  then rs2112347  =1;   if    rs2112347_G  eq 0          then rs2112347   =2;
rs7715256  = .;   if    rs7715256_G    eq 2    then rs7715256  =2;   if   rs7715256_G    eq 1  then rs7715256  =1;   if    rs7715256_G   eq 0         then rs7715256   =0;
rs205262 = .;     if    rs205262_G     eq 2    then rs205262 =2;   if   rs205262_G   eq 1      then rs205262 =1;   if    rs205262_G  eq 0             then rs205262  =0;
rs2033529  = .;   if    rs2033529_G    eq 2    then rs2033529  =2;   if   rs2033529_G    eq 1  then rs2033529  =1;   if    rs2033529_G   eq 0         then rs2033529  =0;
rs9400239 = .;    if    rs9400239_T    eq 2    then rs9400239 =0;   if   rs9400239_T   eq 1    then rs9400239 =1;   if    rs9400239_T  eq 0           then rs9400239  =2;
rs9374842  = .;   if    rs9374842_C    eq 2    then rs9374842  =0;   if   rs9374842_C    eq 1  then rs9374842  =1;   if    rs9374842_C   eq 0         then rs9374842   =2;
rs13201877  = .;  if    rs13201877_G   eq 2    then rs13201877  =2;   if   rs13201877_G   eq 1 then rs13201877  =1;   if    rs13201877_G   eq 0       then rs13201877   =0;
rs13191362   = .; if    rs13191362_G   eq 2    then rs13191362   =0;   if   rs13191362_G  eq 1 then rs13191362   =1;   if    rs13191362_G   eq 0      then rs13191362   =2;
rs1167827  = .;   if    rs1167827_A    eq 2    then rs1167827  =0;   if   rs1167827_A    eq 1  then rs1167827  =1;   if    rs1167827_A   eq 0         then rs1167827   =2;
rs2245368 = .;    if    rs2245368_C    eq 2    then rs2245368 =2;   if   rs2245368_C    eq 1   then rs2245368  =1;   if    rs2245368_C   eq 0         then rs2245368   =0;
rs6465468 = .;    if    rs6465468_T    eq 2    then rs6465468 =2;   if   rs6465468_T   eq 1    then rs6465468 =1;   if    rs6465468_T  eq 0           then rs6465468  =0;
rs17405819  = .;  if    rs17405819_C   eq 2    then rs17405819  =0;   if   rs17405819_C  eq 1  then rs17405819  =1;   if    rs17405819_C   eq 0       then rs17405819   =2;
rs16907751 = .;   if    rs16907751_T   eq 2    then rs16907751 =0;   if   rs16907751_T   eq 1  then rs16907751 =1;   if    rs16907751_T  eq 0         then rs16907751  =2;
rs2033732  = .;   if    rs2033732_T    eq 2    then rs2033732  =0;   if   rs2033732_T    eq 1  then rs2033732  =1;   if    rs2033732_T   eq 0         then rs2033732   =2;
rs4740619  = .;   if    rs4740619_C    eq 2    then rs4740619  =0;   if   rs4740619_C    eq 1  then rs4740619  =1;   if    rs4740619_C   eq 0         then rs4740619   =2;
rs10968576  = .;  if    rs10968576_G   eq 2    then rs10968576  =2;   if   rs10968576_G   eq 1 then rs10968576  =1;   if    rs10968576_G   eq 0       then rs10968576   =0;
rs6477694  = .;   if    rs6477694_C    eq 2    then rs6477694  =2;   if   rs6477694_C    eq 1  then rs6477694  =1;   if    rs6477694_C   eq 0         then rs6477694   =0;
rs1928295  = .;   if    rs1928295_C    eq 2    then rs1928295  =0;   if   rs1928295_C    eq 1  then rs1928295  =1;   if    rs1928295_C   eq 0         then rs1928295   =2;
rs10733682 = .;   if    rs10733682_A   eq 2    then rs10733682 =2;   if   rs10733682_A   eq 1  then rs10733682 =1;   if    rs10733682_A  eq 0         then rs10733682  =0;
rs7899106  = .;   if    rs7899106_G    eq 2    then rs7899106  =2;   if   rs7899106_G eq 1     then rs7899106  =1;   if    rs7899106_G eq 0           then rs7899106   =0;
rs17094222  = .;  if    rs17094222_C   eq 2    then rs17094222  =2;   if   rs17094222_C eq 1   then rs17094222  =1;   if    rs17094222_C eq 0         then rs17094222   =0;
rs11191560 = .;   if    rs11191560_C   eq 2    then rs11191560 =2;   if   rs11191560_C   eq 1  then rs11191560 =1;   if    rs11191560_C  eq 0         then rs11191560  =0;
rs7903146b  = .;   if    rs7903146_T    eq 2    then rs7903146b  =0;   if   rs7903146_T    eq 1  then rs7903146b  =1;   if    rs7903146_T   eq 0         then rs7903146b   =2;
rs2176598 = .;    if    rs2176598_T    eq 2    then rs2176598 =2;   if   rs2176598_T   eq 1    then rs2176598 =1;   if    rs2176598_T  eq 0           then rs2176598  =0;
rs3817334   = .;  if    rs3817334_T    eq 2    then rs3817334   =2;   if   rs3817334_T    eq 1 then rs3817334   =1;   if    rs3817334_T   eq 0        then rs3817334   =0;
rs12286929   = .; if    rs12286929_A   eq 2    then rs12286929   =0;   if   rs12286929_A eq 1  then rs12286929   =1;   if    rs12286929_A   eq 0      then rs12286929   =2;
rs7138803  = .;   if    rs7138803_A    eq 2    then rs7138803  =2;   if   rs7138803_A    eq 1  then rs7138803  =1;   if    rs7138803_A   eq 0         then rs7138803   =0;
rs11057405  = .;  if    rs11057405_A   eq 2    then rs11057405  =0;   if   rs11057405_A  eq 1  then rs11057405  =1;   if    rs11057405_A   eq 0       then rs11057405   =2;
rs12429545  = .;  if    rs12429545_A   eq 2    then rs12429545  =2;   if   rs12429545_A   eq 1 then rs12429545  =1;   if   rs12429545_A   eq 0        then rs12429545   =0;
rs9540493 = .;    if    rs9540493_A    eq 2    then rs9540493 =2;   if   rs9540493_A   eq 1    then rs9540493 =1;   if    rs9540493_A  eq 0           then rs9540493  =0;
rs1441264  = .;   if    rs1441264_G    eq 2    then rs1441264  =0;   if   rs1441264_G    eq 1  then rs1441264  =1;   if    rs1441264_G   eq 0         then rs1441264   =2;
rs10132280 = .;   if    rs10132280_A   eq 2    then rs10132280 =0;   if   rs10132280_A   eq 1  then rs10132280 =1;   if    rs10132280_A  eq 0         then rs10132280  =2;
rs12885454  = .;  if    rs12885454_A   eq 2    then rs12885454  =0;   if   rs12885454_A   eq 1 then rs12885454  =1;   if    rs12885454_A   eq 0       then rs12885454   =2;
rs7141420  = .;   if    rs7141420_C    eq 2    then rs7141420  =0;   if   rs7141420_C    eq 1  then rs7141420  =1;   if    rs7141420_C   eq 0         then rs7141420   =2;
rs3736485  = .;   if    rs3736485_A    eq 2    then rs3736485  =2;   if   rs3736485_A    eq 1  then rs3736485  =1;   if    rs3736485_A   eq 0         then rs3736485   =0;
rs7164727  = .;   if    rs7164727_C    eq 2    then rs7164727  =0;   if   rs7164727_C    eq 1  then rs7164727  =1;   if    rs7164727_C   eq 0         then rs7164727   =2;
rs758747  = .;    if    rs758747_T     eq 2    then rs758747  =2;   if   rs758747_T    eq 1    then rs758747  =1;   if   rs758747_T   eq 0            then rs758747   =0;
rs12446632 = .;   if    rs12446632_A   eq 2    then rs12446632 =0;   if   rs12446632_A   eq 1  then rs12446632 =1;   if    rs12446632_A  eq 0         then rs12446632  =2;
rs2650492  = .;   if    rs2650492_A    eq 2    then rs2650492  =2;   if   rs2650492_A    eq 1  then rs2650492  =1;   if    rs2650492_A   eq 0         then rs2650492   =0;
rs3888190  = .;   if    rs3888190_A    eq 2    then rs3888190  =2;   if   rs3888190_A    eq 1  then rs3888190  =1;   if    rs3888190_A   eq 0         then rs3888190   =0;
rs4787491 = .;    if    rs4787491_G    eq 2    then rs4787491 =2;   if   rs4787491_G   eq 1    then rs4787491 =1;   if    rs4787491_G  eq 0           then rs4787491  =0;
rs9925964  = .;   if    rs9925964_G    eq 2    then rs9925964  =0;   if   rs9925964_G    eq 1  then rs9925964  =1;   if    rs9925964_G   eq 0         then rs9925964   =2;
rs2080454 = .;    if    rs2080454_C    eq 2    then rs2080454 =2;   if   rs2080454_C   eq 1    then rs2080454 =1;   if   rs2080454_C  eq 0            then rs2080454  =0;
rs9914578  = .;   if    rs9914578_G    eq 2    then rs9914578  =2;   if   rs9914578_G    eq 1  then rs9914578  =1;   if    rs9914578_G   eq 0         then rs9914578   =0;
rs1000940  = .;   if    rs1000940_G    eq 2    then rs1000940 =2;   if   rs1000940_G    eq 1   then rs1000940  =1;   if    rs1000940_G   eq 0         then rs1000940   =0;
rs12940622  = .;  if    rs12940622_A   eq 2    then rs12940622  =0;   if   rs12940622_A   eq 1 then rs12940622  =1;   if    rs12940622_A   eq 0       then rs12940622   =2;
rs1808579  = .;   if    rs1808579_T    eq 2    then rs1808579  =0;   if   rs1808579_T    eq 1  then rs1808579  =1;   if   rs1808579_T  eq 0           then rs1808579   =2;
rs7239883 = .;    if    rs7239883_G    eq 2    then rs7239883 =2;   if   rs7239883_G   eq 1    then rs7239883 =1;   if    rs7239883_G  eq 0           then rs7239883  =0;
rs7243357  = .;   if    rs7243357_G    eq 2    then rs7243357 =0;   if   rs7243357_G    eq 1   then rs7243357  =1;   if   rs7243357_G   eq 0          then rs7243357   =2;
rs6567160 = .;    if    rs6567160_C    eq 2    then rs6567160 =2;   if   rs6567160_C   eq 1    then rs6567160 =1;   if   rs6567160_C  eq 0            then rs6567160  =0;
rs17724992   = .; if    rs17724992_G   eq 2    then rs17724992   =0;   if   rs17724992_G  eq 1 then rs17724992   =1;   if    rs17724992_G   eq 0      then rs17724992   =2;
rs29941  = .;     if    rs29941_A      eq 2    then rs29941  =0;   if   rs29941_A    eq 1      then rs29941  =1;   if   rs29941_A   eq 0              then rs29941   =2;
rs2075650  = .;   if    rs2075650_G    eq 2    then rs2075650  =0;   if   rs2075650_G    eq 1  then rs2075650  =1;   if    rs2075650_G   eq 0         then rs2075650   =2;
rs2287019  = .;   if    rs2287019_T    eq 2    then rs2287019  =0;   if   rs2287019_T    eq 1  then rs2287019  =1;   if   rs2287019_T   eq 0          then rs2287019   =2;
rs3810291  = .;   if    rs3810291_G    eq 2    then rs3810291  =0;   if   rs3810291_G    eq 1  then rs3810291  =1;   if    rs3810291_G   eq 0         then rs3810291   =2;
rs6091540 = .;    if    rs6091540_T    eq 2    then rs6091540 =0;   if   rs6091540_T   eq 1    then rs6091540 =1;   if    rs6091540_T  eq 0           then rs6091540  =2;
rs2836754  = .;   if    rs2836754_T    eq 2    then rs2836754  =0;   if   rs2836754_T    eq 1  then rs2836754  =1;   if    rs2836754_T   eq 0         then rs2836754   =2;
rs657452  = .;   if    rs657452_A    eq 2    then rs657452  =2;   if   rs657452_A    eq 1  then rs657452  =1;   if    rs657452_A   eq 0             then rs657452   =0;
rs2035935  = .;   if    rs2035935_G    eq 2    then rs2035935  =2;   if   rs2035935_G    eq 1  then rs2035935  =1;   if    rs2035935_G   eq 0         then rs2035935   =0;
rs17001561 = .;   if    rs17001561_A   eq 2    then rs17001561 =2;   if   rs17001561_A   eq 1  then rs17001561 =1;   if    rs17001561_A  eq 0         then rs17001561  =0;
rs11688816  = .;  if    rs11688816_A   eq 2    then rs11688816 =0;   if   rs11688816_A   eq 1  then rs11688816  =1;   if    rs11688816_A   eq 0       then rs11688816  =2;
rs1421085 = .;    if    rs1421085_C    eq 2    then rs1421085 =2;   if   rs1421085_C   eq 1    then rs1421085 =1;   if   rs1421085_C  eq 0            then rs1421085  =0;
rs7103411  = .;   if    rs7103411_C    eq 2    then rs7103411  =0;   if   rs7103411_C   eq 1   then rs7103411  =1;   if    rs7103411_C   eq 0         then rs7103411   =2;
rs4234589  = .;   if   rs4234589_G     eq 2    then rs4234589 =0;   if   rs4234589_G    eq 1   then rs4234589  =1;   if    rs4234589_G  eq 0          then rs4234589   =2;
rs2241420  = .;   if    rs2241420_A    eq 2    then rs2241420  =0;   if   rs2241420_A    eq 1  then rs2241420  =1;   if    rs2241420_A   eq 0         then rs2241420   =2;
rs1514175  = .;   if    rs1514175_A    eq 2    then rs1514175  =2;   if   rs1514175_A    eq 1  then rs1514175  =1;   if   rs1514175_A  eq 0           then rs1514175   =0;
rs7622475 = .;    if    rs7622475_C    eq 2    then rs7622475 =2;   if  rs7622475_C   eq 1     then rs7622475 =1;   if    rs7622475_C  eq 0           then rs7622475  =0;
rs7113874  = .;   if    rs7113874_T    eq 2    then rs7113874 =0;   if   rs7113874_T    eq 1   then rs7113874  =1;   if   rs7113874_T   eq 0          then rs7113874   =2;
rs1885988 = .;    if    rs1885988_C    eq 2    then rs1885988 =2;   if   rs1885988_C   eq 1    then rs1885988 =1;   if   rs1885988_C  eq 0            then rs1885988  =0;
rs2121279   = .; if   rs2121279_T    eq 2      then rs2121279   =2;   if  rs2121279_T  eq 1    then rs2121279  =1;   if    rs2121279_T  eq 0          then rs2121279   =0;
rs5014937 = .;    if    rs5014937_C    eq 2    then rs5014937  =2;   if   rs5014937_C    eq 1  then rs5014937  =1;   if   rs5014937_C   eq 0          then rs5014937   =0;
rs1460676  = .;  if    rs1460676_C   eq 2      then rs1460676  =2;   if   rs1460676_C   eq 1   then rs1460676  =1;   if    rs1460676_C   eq 0         then rs1460676   =0;
rs2972143  = .;   if    rs2972143_A    eq 2    then rs2972143  =2;   if   rs2972143_A    eq 1  then rs2972143  =1;   if   rs2972143_A   eq 0          then rs2972143   =0;
rs10134820  = .;  if    rs10134820_T   eq 2    then rs10134820  =2;   if   rs10134820_T   eq 1 then rs10134820  =1;   if    rs10134820_T   eq 0       then rs10134820   =0;
rs734597  = .;    if   rs734597_A      eq 2    then rs734597  =2;   if   rs734597_A    eq 1    then rs734597  =1;   if    rs734597_A   eq 0           then rs734597   =0;


keep 
FID 
IID
rs1000940
rs10132280
rs1016287
rs10182181
rs10733682
rs10938397
rs10968576
rs7103411
rs11057405
rs11126666
rs11165643
rs11191560
rs11583200
rs1167827
rs11688816
rs11727676
rs10134820
rs1885988
rs12286929
rs12401738
rs12429545
rs12446632
rs1514175
rs12885454
rs12940622
rs13021737
rs7622475
rs13107325
rs13191362
rs13201877
rs1441264
rs1460676
rs4234589
rs1528435
rs1421085
rs2035935
rs16907751
rs2241420
rs17001561
rs17024393
rs17094222
rs17203016
rs17405819
rs17724992
rs1808579
rs1928295
rs2033529
rs2033732
rs205262
rs2075650
rs2080454
rs2112347
rs2121279
rs2972143
rs2176598
rs734597
rs2245368
rs2287019
rs2365389
rs2650492
rs2820292
rs2836754
rs29941
rs3101336
rs3736485
rs3810291
rs3817334
rs3849570
rs3888190
rs7113874
rs4740619
rs4787491
rs492400
rs543874
rs6091540
rs6465468
rs6477694
rs6567160
rs657452
rs6804842
rs7138803
rs7141420
rs7164727
rs7239883
rs7243357
rs758747
rs7599312
rs7715256
rs7899106
rs7903146b
rs9374842
rs9400239
rs9540493
rs5014937
rs977747
rs9914578
rs9925964; 
 run;

 /*********************************
 GETTING RIGHT ID FORMAT FOR MC AND PCs
 *********************************/
PROC IMPORT 
DATAFILE='N:\GLACIER\97BMI_SNPs\combined_Sanger_vs_VIP.txt' 
OUT=stuff.sanger;
run;

PROC IMPORT 
DATAFILE='N:\GLACIER\97BMI_SNPs\PC_6250.txt' 
OUT=stuff.PC_COMPONENTS;
run;
		
data stuff.sanger (drop=met_mag);
set stuff.sanger;
length IID $ 24;
newid = put(glacierid,10.0);
drop glacierid;
rename newid=id;
IID = longid;
rename longid = FID;
run; 
proc sort data = stuff.sanger;
by FID ;  
run;
proc sort data = stuff.PC_COMPONENTS;
by FID ;
run;
data stuff.G1; 
merge  stuff.sanger stuff.pc_components (in = inpc_components) ;
by FID;
if inpc_components ; 
run; 
proc sort data=A5a;
by IID;
proc sort data=A5b;
by IID;
proc sort data=A5c;
by IID;
proc sort data=A5;
by IID FID;
proc sort data=stuff.G1; 
by IID FID;
RUN;
data A6; 
merge  stuff.G1 A5;
by IID FID;
run; 
data A6a; 
merge  A6 A5a ;
by IID;
run;
data A6ab; 
merge  A6a A5b ;
by IID;
run;
data A6abc; 
merge  A6ab A5c ;
by IID;
keep
IID 
id 
c1 
c2
c3 
c4
rs1000940
rs10132280
rs1016287
rs10182181
rs10733682
rs10938397
rs10968576
rs7103411
rs11057405
rs11126666
rs11165643
rs11191560
rs11583200
rs1167827
rs11688816
rs11727676
rs10134820
rs1885988
rs12286929
rs12401738
rs12429545
rs12446632
rs1514175
rs12885454
rs12940622
rs13021737
rs7622475
rs13107325
rs13191362
rs13201877
rs1441264
rs1460676
rs4234589
rs1528435
rs1421085
rs2035935
rs16907751
rs2241420
rs17001561
rs17024393
rs17094222
rs17203016
rs17405819
rs17724992
rs1808579
rs1928295
rs2033529
rs2033732
rs205262
rs2075650
rs2080454
rs2112347
rs2121279
rs2972143
rs2176598
rs734597
rs2245368
rs2287019
rs2365389
rs2650492
rs2820292
rs2836754
rs29941
rs3101336
rs3736485
rs3810291
rs3817334
rs3849570
rs3888190
rs7113874
rs4740619
rs4787491
rs492400
rs543874
rs6091540
rs6465468
rs6477694
rs6567160
rs657452
rs6804842
rs7138803
rs7141420
rs7164727
rs7239883
rs7243357
rs758747
rs7599312
rs7715256
rs7899106
rs7903146b
rs9374842
rs9400239
rs9540493
rs5014937
rs977747
rs9914578
rs9925964
rs340874d
rs7515431
rs7903146d
rs12571751
rs12242953
rs11257655
rs1111875
rs5215
rs2334499
rs163184
rs1552224
rs10830963d
rs7955901
rs2261181
rs7965349
rs11063069
rs10842994
rs1359790
rs7177055
rs4502156d
rs2007084
rs12899811
rs11634397
rs9923233
rs7202877
rs11651755
rs2447090
rs11663816
rs8182584
rs8108269
rs10401969
rs780094d
rs7569522
rs2943640
rs243083
rs1128249
rs10203174
rs4812829
rs6795735
rs4402960
rs2197423
rs17301514
rs1496653
rs12497268
rs11717195d
rs6819243
rs10012946
rs6878122
rs459193
rs7756992
rs4299828
rs3734621
rs849135
rs10229583
rs17168486
rs13233731
rs3824065
rs7845219
rs516946
rs3802177
rs2796441
rs17791513
rs16927668
rs10811661d
rs10758593
rs10811661f
rs4869272
rs11619319
rs983309
rs6943153
rs11603334
rs6113722
rs16913693
rs3829109
rs3783347
rs2302593
rs9368222
rs10747083
rs6072275
rs7651090f
rs576674
rs11715915
rs17762454
rs7708285
rs2657879
rs340874f
rs780094f
rs560887
rs11708067
rs1280
rs2191349
rs2908289
rs11558471
rs10814916
rs11195502
rs7903146f
rs11607883
rs11039182
rs174576
rs10830963f
rs4502156f
rs1260326
rs11717195t
rs12255372
rs1436958
rs11672660
rs6975024
rs11782386
rs1019503
rs7651090t;  
run; 

/*********************************
MERGING GENOTYPE AND PHENOTYPE DATA
**********************************/
proc sort data=stuff.A4;
by ID;
proc sort data= A6abc; 
by ID;
RUN;
data A7; /*N= 5,726*/
merge  A6abc (in=inA6abc) stuff.A4; 
by id; 
if sex ne .;
if age1 ne .; 
check = .; /*check that the number of SNPs missing on each individual is less or equal to 20% for ALL SNPs*/
pc1		=	.;	if	rs1000940	eq	.	then	pc1		=	1;
pc2		=	.;	if	rs10132280	eq	.	then	pc2		=	1;
pc3		=	.;	if	rs1016287	eq	.	then	pc3		=	1;
pc4		=	.;	if	rs10182181	eq	.	then	pc4		=	1;
pc5		=	.;	if	rs10733682	eq	.	then	pc5		=	1;
pc6		=	.;	if	rs10938397	eq	.	then	pc6		=	1;
pc7		=	.;	if	rs10968576	eq	.	then	pc7		=	1;
pc8		=	.;	if	rs7103411	eq	.	then	pc8		=	1;
pc9		=	.;	if	rs11057405	eq	.	then	pc9		=	1;
pc10	=	.;	if	rs11126666	eq	.	then	pc10	=	1;
pc11	=	.;	if	rs11165643	eq	.	then	pc11	=	1;
pc12	=	.;	if	rs11191560	eq	.	then	pc12	=	1;
pc13	=	.;	if	rs11583200	eq	.	then	pc13	=	1;
pc14	=	.;	if	rs1167827	eq	.	then	pc14	=	1;
pc15	=	.;	if	rs11688816	eq	.	then	pc15	=	1;
pc16	=	.;	if	rs11727676	eq	.	then	pc16	=	1;
pc17	=	.;	if	rs10134820	eq	.	then	pc17	=	1;
pc18	=	.;	if	rs1885988	eq	.	then	pc18	=	1;
pc19	=	.;	if	rs12286929	eq	.	then	pc19	=	1;
pc20	=	.;	if	rs12401738	eq	.	then	pc20	=	1;
pc21	=	.;	if	rs12429545	eq	.	then	pc21	=	1;
pc22	=	.;	if	rs12446632	eq	.	then	pc22	=	1;
pc23	=	.;	if	rs1514175	eq	.	then	pc23	=	1;
pc24	=	.;	if	rs12885454	eq	.	then	pc24	=	1;
pc25	=	.;	if	rs12940622	eq	.	then	pc25	=	1;
pc26	=	.;	if	rs13021737	eq	.	then	pc26	=	1;
pc27	=	.;	if	rs7622475	eq	.	then	pc27	=	1;
pc28	=	.;	if	rs13107325	eq	.	then	pc28	=	1;
pc29	=	.;	if	rs13191362	eq	.	then	pc29	=	1;
pc30	=	.;	if	rs13201877	eq	.	then	pc30	=	1;
pc31	=	.;	if	rs1441264	eq	.	then	pc31	=	1;
pc32	=	.;	if	rs1460676	eq	.	then	pc32	=	1;
pc33	=	.;	if	rs4234589	eq	.	then	pc33	=	1;
pc34	=	.;	if	rs1528435	eq	.	then	pc34	=	1;
pc35	=	.;	if	rs1421085	eq	.	then	pc35	=	1;
pc36	=	.;	if	rs2035935	eq	.	then	pc36	=	1;
pc37	=	.;	if	rs16907751	eq	.	then	pc37	=	1;
pc38	=	.;	if	rs2241420	eq	.	then	pc38	=	1;
pc39	=	.;	if	rs17001561	eq	.	then	pc39	=	1;
pc40	=	.;	if	rs17024393	eq	.	then	pc40	=	1;
pc41	=	.;	if	rs17094222	eq	.	then	pc41	=	1;
pc42	=	.;	if	rs17203016	eq	.	then	pc42	=	1;
pc43	=	.;	if	rs17405819	eq	.	then	pc43	=	1;
pc44	=	.;	if	rs17724992	eq	.	then	pc44	=	1;
pc45	=	.;	if	rs1808579	eq	.	then	pc45	=	1;
pc46	=	.;	if	rs1928295	eq	.	then	pc46	=	1;
pc47	=	.;	if	rs2033529	eq	.	then	pc47	=	1;
pc48	=	.;	if	rs2033732	eq	.	then	pc48	=	1;
pc49	=	.;	if	rs205262	eq	.	then	pc49	=	1;
pc50	=	.;	if	rs2075650	eq	.	then	pc50	=	1;
pc51	=	.;	if	rs2080454	eq	.	then	pc51	=	1;
pc52	=	.;	if	rs2112347	eq	.	then	pc52	=	1;
pc53	=	.;	if	rs2121279	eq	.	then	pc53	=	1;
pc54	=	.;	if	rs2972143	eq	.	then	pc54	=	1;
pc55	=	.;	if	rs2176598	eq	.	then	pc55	=	1;
pc56	=	.;	if	rs734597	eq	.	then	pc56	=	1;
pc57	=	.;	if	rs2245368	eq	.	then	pc57	=	1;
pc58	=	.;	if	rs2287019	eq	.	then	pc58	=	1;
pc59	=	.;	if	rs2365389	eq	.	then	pc59	=	1;
pc60	=	.;	if	rs2650492	eq	.	then	pc60	=	1;
pc61	=	.;	if	rs2820292	eq	.	then	pc61	=	1;
pc62	=	.;	if	rs2836754	eq	.	then	pc62	=	1;
pc63	=	.;	if	rs29941		eq	.	then	pc63	=	1;
pc64	=	.;	if	rs3101336	eq	.	then	pc64	=	1;
pc65	=	.;	if	rs3736485	eq	.	then	pc65	=	1;
pc66	=	.;	if	rs3810291	eq	.	then	pc66	=	1;
pc67	=	.;	if	rs3817334	eq	.	then	pc67	=	1;
pc68	=	.;	if	rs3849570	eq	.	then	pc68	=	1;
pc69	=	.;	if	rs3888190	eq	.	then	pc69	=	1;
pc70	=	.;	if	rs7113874	eq	.	then	pc70	=	1;
pc71	=	.;	if	rs4740619	eq	.	then	pc71	=	1;
pc72	=	.;	if	rs4787491	eq	.	then	pc72	=	1;
pc73	=	.;	if	rs492400	eq	.	then	pc73	=	1;
pc74	=	.;	if	rs543874	eq	.	then	pc74	=	1;
pc75	=	.;	if	rs6091540	eq	.	then	pc75	=	1;
pc76	=	.;	if	rs6465468	eq	.	then	pc76	=	1;
pc77	=	.;	if	rs6477694	eq	.	then	pc77	=	1;
pc78	=	.;	if	rs6567160	eq	.	then	pc78	=	1;
pc79	=	.;	if	rs657452	eq	.	then	pc79	=	1;
pc80	=	.;	if	rs6804842	eq	.	then	pc80	=	1;
pc81	=	.;	if	rs7138803	eq	.	then	pc81	=	1;
pc82	=	.;	if	rs7141420	eq	.	then	pc82	=	1;
pc83	=	.;	if	rs7164727	eq	.	then	pc83	=	1;
pc84	=	.;	if	rs7239883	eq	.	then	pc84	=	1;
pc85	=	.;	if	rs7243357	eq	.	then	pc85	=	1;
pc86	=	.;	if	rs758747	eq	.	then	pc86	=	1;
pc87	=	.;	if	rs7599312	eq	.	then	pc87	=	1;
pc88	=	.;	if	rs7715256	eq	.	then	pc88	=	1;
pc89	=	.;	if	rs7899106	eq	.	then	pc89	=	1;
pc90	=	.;	if	rs7903146b	eq	.	then	pc90	=	1;
pc91	=	.;	if	rs9374842	eq	.	then	pc91	=	1;
pc92	=	.;	if	rs9400239	eq	.	then	pc92	=	1;
pc93	=	.;	if	rs9540493	eq	.	then	pc93	=	1;
pc94	=	.;	if	rs5014937	eq	.	then	pc94	=	1;
pc95	=	.;	if	rs977747	eq	.	then	pc95	=	1;
pc96	=	.;	if	rs9914578	eq	.	then	pc96	=	1;
pc97	=	.;	if	rs9925964	eq	.	then	pc97	=	1;

checka = .;
apc1	=	.;	if	rs1260326	eq	.	then	apc1	=	1;
apc2	=	.;	if	rs11717195t	eq	.	then	apc2	=	1;
apc3	=	.;	if	rs12255372	eq	.	then	apc3	=	1;
apc4	=	.;	if	rs1436958	eq	.	then	apc4	=	1;
apc5	=	.;	if	rs11672660	eq	.	then	apc5	=	1;
apc6	=	.;	if	rs6975024	eq	.	then	apc6	=	1;
apc7	=	.;	if	rs11782386	eq	.	then	apc7	=	1;
apc8	=	.;	if	rs1019503	eq	.	then	apc8	=	1;
apc9	=	.;	if	rs7651090t	eq	.	then	apc9	=	1;

checkb = .;
bpc1	=	.;	if	rs340874f	eq	.	then	bpc1	=	1;
bpc2	=	.;	if	rs780094f	eq	.	then	bpc2	=	1;
bpc3	=	.;	if	rs560887	eq	.	then	bpc3	=	1;
bpc4	=	.;	if	rs11715915	eq	.	then	bpc4	=	1;
bpc5	=	.;	if	rs11708067	eq	.	then	bpc5	=	1;
bpc6	=	.;	if	rs1280		eq	.	then	bpc6	=	1;
bpc7	=	.;	if	rs7651090f	eq	.	then	bpc7	=	1;
bpc8	=	.;	if	rs7708285	eq	.	then	bpc8	=	1;
bpc9	=	.;	if	rs4869272	eq	.	then	bpc9	=	1;
bpc10	=	.;	if	rs17762454	eq	.	then	bpc10	=	1;
bpc11	=	.;	if	rs9368222	eq	.	then	bpc11	=	1;
bpc12	=	.;	if	rs2191349	eq	.	then	bpc12	=	1;
bpc13	=	.;	if	rs2908289	eq	.	then	bpc13	=	1;
bpc14	=	.;	if	rs6943153	eq	.	then	bpc14	=	1;
bpc15	=	.;	if	rs983309	eq	.	then	bpc15	=	1;
bpc16	=	.;	if	rs11558471	eq	.	then	bpc16	=	1;
bpc17	=	.;	if	rs10814916	eq	.	then	bpc17	=	1;
bpc18	=	.;	if	rs10811661f	eq	.	then	bpc18	=	1;
bpc19	=	.;	if	rs16913693	eq	.	then	bpc19	=	1;
bpc20	=	.;	if	rs3829109	eq	.	then	bpc20	=	1;
bpc21	=	.;	if	rs11195502	eq	.	then	bpc21	=	1;
bpc22	=	.;	if	rs7903146f	eq	.	then	bpc22	=	1;
bpc23	=	.;	if	rs11607883	eq	.	then	bpc23	=	1;
bpc24	=	.;	if	rs11039182	eq	.	then	bpc24	=	1;
bpc25	=	.;	if	rs174576	eq	.	then	bpc25	=	1;
bpc26	=	.;	if	rs11603334	eq	.	then	bpc26	=	1;
bpc27	=	.;	if	rs10830963f	eq	.	then	bpc27	=	1;
bpc28	=	.;	if	rs2657879	eq	.	then	bpc28	=	1;
bpc29	=	.;	if	rs10747083	eq	.	then	bpc29	=	1;
bpc30	=	.;	if	rs11619319	eq	.	then	bpc30	=	1;
bpc31	=	.;	if	rs576674	eq	.	then	bpc31	=	1;
bpc32	=	.;	if	rs3783347	eq	.	then	bpc32	=	1;
bpc33	=	.;	if	rs4502156f	eq	.	then	bpc33	=	1;
bpc34	=	.;	if	rs2302593	eq	.	then	bpc34	=	1;
bpc35	=	.;	if	rs6113722	eq	.	then	bpc35	=	1;
bpc36	=	.;	if	rs6072275	eq	.	then	bpc36	=	1;

checkc = .;
cpc1	=	.;	if	rs7515431	eq	.	then	cpc1	=	1;
cpc2	=	.;	if	rs340874d	eq	.	then	cpc2	=	1;
cpc3	=	.;	if	rs780094d	eq	.	then	cpc3	=	1;
cpc4	=	.;	if	rs10203174	eq	.	then	cpc4	=	1;
cpc5	=	.;	if	rs243083	eq	.	then	cpc5	=	1;
cpc6	=	.;	if	rs7569522	eq	.	then	cpc6	=	1;
cpc7	=	.;	if	rs1128249	eq	.	then	cpc7	=	1;
cpc8	=	.;	if	rs2943640	eq	.	then	cpc8	=	1;
cpc9	=	.;	if	rs2197423	eq	.	then	cpc9	=	1;
cpc10	=	.;	if	rs1496653	eq	.	then	cpc10	=	1;
cpc11	=	.;	if	rs12497268	eq	.	then	cpc11	=	1;
cpc12	=	.;	if	rs6795735	eq	.	then	cpc12	=	1;
cpc13	=	.;	if	rs11717195d	eq	.	then	cpc13	=	1;
cpc14	=	.;	if	rs4402960	eq	.	then	cpc14	=	1;
cpc15	=	.;	if	rs17301514	eq	.	then	cpc15	=	1;
cpc16	=	.;	if	rs6819243	eq	.	then	cpc16	=	1;
cpc17	=	.;	if	rs10012946	eq	.	then	cpc17	=	1;
cpc18	=	.;	if	rs459193	eq	.	then	cpc18	=	1;
cpc19	=	.;	if	rs6878122	eq	.	then	cpc19	=	1;
cpc20	=	.;	if	rs7756992	eq	.	then	cpc20	=	1;
cpc21	=	.;	if	rs4299828	eq	.	then	cpc21	=	1;
cpc22	=	.;	if	rs3734621	eq	.	then	cpc22	=	1;
cpc23	=	.;	if	rs17168486	eq	.	then	cpc23	=	1;
cpc24	=	.;	if	rs849135	eq	.	then	cpc24	=	1;
cpc25	=	.;	if	rs3824065	eq	.	then	cpc25	=	1;
cpc26	=	.;	if	rs10229583	eq	.	then	cpc26	=	1;
cpc27	=	.;	if	rs13233731	eq	.	then	cpc27	=	1;
cpc28	=	.;	if	rs516946	eq	.	then	cpc28	=	1;
cpc29	=	.;	if	rs7845219	eq	.	then	cpc29	=	1;
cpc30	=	.;	if	rs3802177	eq	.	then	cpc30	=	1;
cpc31	=	.;	if	rs10758593	eq	.	then	cpc31	=	1;
cpc32	=	.;	if	rs16927668	eq	.	then	cpc32	=	1;
cpc33	=	.;	if	rs10811661d	eq	.	then	cpc33	=	1;
cpc34	=	.;	if	rs17791513	eq	.	then	cpc34	=	1;
cpc35	=	.;	if	rs2796441	eq	.	then	cpc35	=	1;
cpc36	=	.;	if	rs11257655	eq	.	then	cpc36	=	1;
cpc37	=	.;	if	rs12242953	eq	.	then	cpc37	=	1;
cpc38	=	.;	if	rs12571751	eq	.	then	cpc38	=	1;
cpc39	=	.;	if	rs1111875	eq	.	then	cpc39	=	1;
cpc40	=	.;	if	rs7903146d	eq	.	then	cpc40	=	1;
cpc41	=	.;	if	rs2334499	eq	.	then	cpc41	=	1;
cpc42	=	.;	if	rs163184	eq	.	then	cpc42	=	1;
cpc43	=	.;	if	rs5215		eq	.	then	cpc43	=	1;
cpc44	=	.;	if	rs1552224	eq	.	then	cpc44	=	1;
cpc45	=	.;	if	rs10830963d	eq	.	then	cpc45	=	1;
cpc46	=	.;	if	rs11063069	eq	.	then	cpc46	=	1;
cpc47	=	.;	if	rs10842994	eq	.	then	cpc47	=	1;
cpc48	=	.;	if	rs2261181	eq	.	then	cpc48	=	1;
cpc49	=	.;	if	rs7955901	eq	.	then	cpc49	=	1;
cpc50	=	.;	if	rs7965349	eq	.	then	cpc50	=	1;
cpc51	=	.;	if	rs1359790	eq	.	then	cpc51	=	1;
cpc52	=	.;	if	rs4502156d	eq	.	then	cpc52	=	1;
cpc53	=	.;	if	rs7177055	eq	.	then	cpc53	=	1;
cpc54	=	.;	if	rs11634397	eq	.	then	cpc54	=	1;
cpc55	=	.;	if	rs2007084	eq	.	then	cpc55	=	1;
cpc56	=	.;	if	rs12899811	eq	.	then	cpc56	=	1;
cpc57	=	.;	if	rs9923233	eq	.	then	cpc57	=	1;
cpc58	=	.;	if	rs7202877	eq	.	then	cpc58	=	1;
cpc59	=	.;	if	rs2447090	eq	.	then	cpc59	=	1;
cpc60	=	.;	if	rs11651755	eq	.	then	cpc60	=	1;
cpc61	=	.;	if	rs11663816	eq	.	then	cpc61	=	1;
cpc62	=	.;	if	rs10401969	eq	.	then	cpc62	=	1;
cpc63	=	.;	if	rs8182584	eq	.	then	cpc63	=	1;
cpc64	=	.;	if	rs8108269	eq	.	then	cpc64	=	1;
cpc65	=	.;	if	rs4812829	eq	.	then	cpc65	=	1;
check = SUM (of pc1-pc97);	
if check = . then check= 0;
checka = SUM (of apc1-apc9);
if checka = . then checka= 0;
checkb = SUM (of bpc1-bpc36);
if checkb = . then checkb= 0;
checkc = SUM (of cpc1-cpc65);
if checkc = . then checkc= 0;
if check le 19;
if checka le 2;	
if checkb le 7;	
if checkc le 13;
run;
proc means data= A7; var check checka checkb checkc; run; 


/***************************************
       HARDY-WEINBERG EQUILIBRUIM 
*****************************************/
%macro runhwe(rsx,SNP); 
PROC FREQ DATA = A7 ; 
TABLES &rsx /OUT = HWE_1;
RUN;

data HWE_2;
set HWE_1;
if &rsx NE .;
run;

data out1;
set HWE_2;
SNPname = _N_;
SNP = &rsx;
keep SNPname SNP;
run;

proc sort data = out1; by SNP; run;

data out2;
set HWE_2;
SNP = &rsx;
keep SNP COUNT;
run;

proc sort data = out2; by SNP; run;

data outfile;
merge out1 out2;
by SNP;
run;

proc print;
 run;

proc transpose data = outfile out=outfile2;
   run;
   proc print data = outfile2;
   run;
   data HWE_PVAL;
    set outfile2;
      if _name_ = 'COUNT';
      n=col1+col2+col3;
      p=(2*col1+col2)/(2*n);
      q=1-p;
      E1=p*p*n;
      E2=2*p*q*n;
      E3=q*q*n;
      chisquare=((col1-e1)**2/e1) + ((col2-e2)**2/e2) + ((col3-e3)**2/e3);
      prob=probchi(chisquare,1);
      run;
      PROC PRINT;
      run;

data HWE_PVAL_2;
set HWE_PVAL;
	EFFECT = .;
	if COL1 gt COL3 then EFFECT = 22;
	if COL3 gt COL1 then EFFECT = 11;

	NONEFFECT = .;
	if COL1 gt COL3 then NONEFFECT = 11;
	if COL3 gt COL1 then NONEFFECT = 22;

	HOMMAJ_11 = .; HET = .; HOMMIN_22 = .;
    if EFFECT = 11 then HOMMAJ_11 = COL3;
	if EFFECT = 22 then HOMMAJ_11 = COL1;
    if EFFECT = 11 then HOMMIN_22 = COL1;
	if EFFECT = 22 then HOMMIN_22 = COL3;
	HET = COL2;

	EXHOMMAJ_11 = .; EXHET = .; EXHOMMIN_22 = .;
    if EFFECT = 11 then EXHOMMAJ_11 = e3;
	if EFFECT = 22 then EXHOMMAJ_11 = e1;
    if EFFECT = 11 then EXHOMMIN_22 = e1;
	if EFFECT = 22 then EXHOMMIN_22 = e3;
	EXHET = e2;

	Freq_p = .; MAF = .;
	if EFFECT = 11 then Freq_p = q;
	if EFFECT = 22 then Freq_p = p;
	if EFFECT = 11 then MAF = p;
	if EFFECT = 22 then MAF = q;
	drop chisquare prob p q E1 E2 E3 col1 col2 col3 e1 e2 e3 ;
run;

   data HWE_PVAL_2;
    set HWE_PVAL_2;
      chisquare=((HOMMAJ_11-EXHOMMAJ_11)**2/EXHOMMAJ_11) + ((HET-EXHET)**2/EXHET) + ((HOMMIN_22-EXHOMMIN_22)**2/EXHOMMIN_22);
      prob=1-probchi(chisquare,1);

	SUCCRATE = .; SUCCRATE = n/5858;
	rename n = SAMPLE_N;
	rename prob = HWE_PVAL;
	SNP = "&rsx.";
	
	drop _LABEL_ _Name_ EXHOMMAJ_11 EXHET EXHOMMIN_22 freq_p;
run;
      PROC PRINT;
      run;

proc append force data= HWE_PVAL_2 base = HWE_PVAL_3; 
run;
%mend;
%runhwe	(	rs1000940	)
%runhwe	(	rs10132280	)
%runhwe	(	rs1016287	)
%runhwe	(	rs10182181	)
%runhwe	(	rs10733682	)
%runhwe	(	rs10938397	)
%runhwe	(	rs10968576	)
%runhwe	(	rs7103411	)
%runhwe	(	rs11057405	)
%runhwe	(	rs11126666	)
%runhwe	(	rs11165643	)
%runhwe	(	rs11191560	)
%runhwe	(	rs11583200	)
%runhwe	(	rs1167827	)
%runhwe	(	rs11688816	)
%runhwe	(	rs11727676	)
%runhwe	(	rs10134820	)
%runhwe	(	rs1885988	)
%runhwe	(	rs12286929	)
%runhwe	(	rs12401738	)
%runhwe	(	rs12429545	)
%runhwe	(	rs12446632	)
%runhwe	(	rs1514175	)
%runhwe	(	rs12885454	)
%runhwe	(	rs12940622	)
%runhwe	(	rs13021737	)
%runhwe	(	rs7622475	)
%runhwe	(	rs13107325	)
%runhwe	(	rs13191362	)
%runhwe	(	rs13201877	)
%runhwe	(	rs1441264	)
%runhwe	(	rs1460676	)
%runhwe	(	rs4234589	)
%runhwe	(	rs1528435	)
%runhwe	(	rs1421085	)
%runhwe	(	rs2035935	)
%runhwe	(	rs16907751	)
%runhwe	(	rs2241420	)
%runhwe	(	rs17001561	)
%runhwe	(	rs17024393	)
%runhwe	(	rs17094222	)
%runhwe	(	rs17203016	)
%runhwe	(	rs17405819	)
%runhwe	(	rs17724992	)
%runhwe	(	rs1808579	)
%runhwe	(	rs1928295	)
%runhwe	(	rs2033529	)
%runhwe	(	rs2033732	)
%runhwe	(	rs205262	)
%runhwe	(	rs2075650	)
%runhwe	(	rs2080454	)
%runhwe	(	rs2112347	)
%runhwe	(	rs2121279	)
%runhwe	(	rs2972143	)
%runhwe	(	rs2176598	)
%runhwe	(	rs734597	)
%runhwe	(	rs2245368	)
%runhwe	(	rs2287019	)
%runhwe	(	rs2365389	)
%runhwe	(	rs2650492	)
%runhwe	(	rs2820292	)
%runhwe	(	rs2836754	)
%runhwe	(	rs29941		)
%runhwe	(	rs3101336	)
%runhwe	(	rs3736485	)
%runhwe	(	rs3810291	)
%runhwe	(	rs3817334	)
%runhwe	(	rs3849570	)
%runhwe	(	rs3888190	)
%runhwe	(	rs7113874	)
%runhwe	(	rs4740619	)
%runhwe	(	rs4787491	)
%runhwe	(	rs492400	)
%runhwe	(	rs543874	)
%runhwe	(	rs6091540	)
%runhwe	(	rs6465468	)
%runhwe	(	rs6477694	)
%runhwe	(	rs6567160	)
%runhwe	(	rs657452	)
%runhwe	(	rs6804842	)
%runhwe	(	rs7138803	)
%runhwe	(	rs7141420	)
%runhwe	(	rs7164727	)
%runhwe	(	rs7239883	)
%runhwe	(	rs7243357	)
%runhwe	(	rs758747	)
%runhwe	(	rs7599312	)
%runhwe	(	rs7715256	)
%runhwe	(	rs7899106	)
%runhwe	(	rs7903146b	)
%runhwe	(	rs9374842	)
%runhwe	(	rs9400239	)
%runhwe	(	rs9540493	)
%runhwe	(	rs5014937	)
%runhwe	(	rs977747	)
%runhwe	(	rs9914578	)
%runhwe	(	rs9925964	)

%runhwe	(rs1260326	)
%runhwe	(rs11717195t )
%runhwe	(rs12255372	)
%runhwe	(rs1436958	)
%runhwe	(rs11672660	)
%runhwe	(rs6975024	)
%runhwe	(rs11782386	)
%runhwe	(rs1019503	)
%runhwe	(rs7651090t	)

%runhwe	(rs10811661f )
%runhwe	(rs4869272	)
%runhwe	(rs11619319	)
%runhwe	(rs983309	)
%runhwe	(rs6943153	)
%runhwe	(rs11603334	)
%runhwe	(rs6113722	)
%runhwe	(rs16913693	)
%runhwe	(rs3829109	)
%runhwe	(rs3783347	)
%runhwe	(rs2302593	)
%runhwe	(rs9368222	)
%runhwe	(rs10747083	)
%runhwe	(rs6072275	)
%runhwe	(rs7651090f	)
%runhwe	(rs576674	)
%runhwe	(rs11715915	)
%runhwe	(rs17762454	)
%runhwe	(rs7708285	)
%runhwe	(rs2657879	)
%runhwe	(rs340874f	)
%runhwe	(rs780094f	)
%runhwe	(rs560887	)
%runhwe	(rs11708067	)
%runhwe	(rs1280		)
%runhwe	(rs2191349	)
%runhwe	(rs2908289	)
%runhwe	(rs11558471	)
%runhwe	(rs10814916	)
%runhwe	(rs11195502	)
%runhwe	(rs7903146f	)
%runhwe	(rs11607883	)
%runhwe	(rs11039182	)
%runhwe	(rs174576	)
%runhwe	(rs10830963f )
%runhwe	(rs4502156f	)

%runhwe	(rs340874d	)
%runhwe	(rs7515431	)
%runhwe	(rs7903146d	)
%runhwe	(rs12571751	)
%runhwe	(rs12242953	)
%runhwe	(rs11257655	)
%runhwe	(rs1111875	)	
%runhwe	(rs5215		)
%runhwe	(rs2334499	)
%runhwe	(rs163184	)
%runhwe	(rs1552224	)
%runhwe	(rs10830963d )
%runhwe	(rs7955901	)
%runhwe	(rs2261181	)
%runhwe	(rs7965349	)
%runhwe	(rs11063069	)
%runhwe	(rs10842994	)
%runhwe	(rs1359790	)
%runhwe	(rs7177055	)
%runhwe	(rs4502156d	)
%runhwe	(rs2007084	)
%runhwe	(rs12899811	)
%runhwe	(rs11634397	)
%runhwe	(rs9923233	)
%runhwe	(rs7202877	)
%runhwe	(rs11651755	)
%runhwe	(rs2447090	)
%runhwe	(rs11663816	)
%runhwe	(rs8182584	)
%runhwe	(rs8108269	)
%runhwe	(rs10401969	)
%runhwe	(rs780094d	)
%runhwe	(rs7569522	)
%runhwe	(rs2943640	)
%runhwe	(rs243083	)
%runhwe	(rs1128249	)
%runhwe	(rs10203174	)
%runhwe	(rs4812829	)
%runhwe	(rs6795735	)
%runhwe	(rs4402960	)
%runhwe	(rs2197423	)
%runhwe	(rs17301514	)
%runhwe	(rs1496653	)
%runhwe	(rs12497268	)
%runhwe	(rs11717195d )
%runhwe	(rs6819243	)
%runhwe	(rs10012946	)
%runhwe	(rs6878122	)
%runhwe	(rs459193	)
%runhwe	(rs7756992	)
%runhwe	(rs4299828	)
%runhwe	(rs3734621	)
%runhwe	(rs849135	)
%runhwe	(rs10229583	)
%runhwe	(rs17168486	)
%runhwe	(rs13233731	)
%runhwe	(rs3824065	)
%runhwe	(rs7845219	)
%runhwe	(rs516946	)
%runhwe	(rs3802177	)
%runhwe	(rs2796441	)
%runhwe	(rs17791513	)
%runhwe	(rs16927668	)
%runhwe	(rs10811661d )
%runhwe	(rs10758593	)
; 

proc export data = HWE_PVAL_3
outfile = 'N:\GLACIER\gene_vs_lifestyle\HWE.xls' Replace; /*do remember to change the folder*/
run; 


/**********************************
     MISSING GENOTYPE IMPUTATION
***********************************/
%macro runimpute(rsx,mean,meansum);

PROC MEANS DATA= A7;
   VAR &rsx;
   OUTPUT OUT=&mean;
   RUN;
data &mean;
set &mean;
if _STAT_ = 'MEAN';
&mean = .; &mean = &rsx;
keep &mean;
run;
%mend;

%runimpute	(	rs1000940	,	rs1000940_mean	,	rs1000940_meansum	)
%runimpute	(	rs10132280	,	rs10132280_mean	,	rs10132280_meansum	)
%runimpute	(	rs1016287	,	rs1016287_mean	,	rs1016287_meansum	)
%runimpute	(	rs10182181	,	rs10182181_mean	,	rs10182181_meansum	)
%runimpute	(	rs10733682	,	rs10733682_mean	,	rs10733682_meansum	)
%runimpute	(	rs10938397	,	rs10938397_mean	,	rs10938397_meansum	)
%runimpute	(	rs10968576	,	rs10968576_mean	,	rs10968576_meansum	)
%runimpute	(	rs7103411	,	rs7103411_mean	,	rs7103411_meansum	)
%runimpute	(	rs11057405	,	rs11057405_mean	,	rs11057405_meansum	)
%runimpute	(	rs11126666	,	rs11126666_mean	,	rs11126666_meansum	)
%runimpute	(	rs11165643	,	rs11165643_mean	,	rs11165643_meansum	)
%runimpute	(	rs11191560	,	rs11191560_mean	,	rs11191560_meansum	)
%runimpute	(	rs11583200	,	rs11583200_mean	,	rs11583200_meansum	)
%runimpute	(	rs1167827	,	rs1167827_mean	,	rs1167827_meansum	)
%runimpute	(	rs11688816	,	rs11688816_mean	,	rs11688816_meansum	)
%runimpute	(	rs11727676	,	rs11727676_mean	,	rs11727676_meansum	)
%runimpute	(	rs10134820	,	rs10134820_mean	,	rs10134820_meansum	)
%runimpute	(	rs1885988	,	rs1885988_mean	,	rs1885988_meansum	)
%runimpute	(	rs12286929	,	rs12286929_mean	,	rs12286929_meansum	)
%runimpute	(	rs12401738	,	rs12401738_mean	,	rs12401738_meansum	)
%runimpute	(	rs12429545	,	rs12429545_mean	,	rs12429545_meansum	)
%runimpute	(	rs12446632	,	rs12446632_mean	,	rs12446632_meansum	)
%runimpute	(	rs1514175	,	rs1514175_mean	,	rs1514175_meansum	)
%runimpute	(	rs12885454	,	rs12885454_mean	,	rs12885454_meansum	)
%runimpute	(	rs12940622	,	rs12940622_mean	,	rs12940622_meansum	)
%runimpute	(	rs13021737	,	rs13021737_mean	,	rs13021737_meansum	)
%runimpute	(	rs7622475	,	rs7622475_mean	,	rs7622475_meansum	)
%runimpute	(	rs13107325	,	rs13107325_mean	,	rs13107325_meansum	)
%runimpute	(	rs13191362	,	rs13191362_mean	,	rs13191362_meansum	)
%runimpute	(	rs13201877	,	rs13201877_mean	,	rs13201877_meansum	)
%runimpute	(	rs1441264	,	rs1441264_mean	,	rs1441264_meansum	)
%runimpute	(	rs1460676	,	rs1460676_mean	,	rs1460676_meansum	)
%runimpute	(	rs4234589	,	rs4234589_mean	,	rs4234589_meansum	)
%runimpute	(	rs1528435	,	rs1528435_mean	,	rs1528435_meansum	)
%runimpute	(	rs1421085	,	rs1421085_mean	,	rs1421085_meansum	)
%runimpute	(	rs2033529	,	rs2033529_mean	,	rs2033529_meansum	)
%runimpute	(	rs16907751	,	rs16907751_mean	,	rs16907751_meansum	)
%runimpute	(	rs2241420	,	rs2241420_mean	,	rs2241420_meansum	)
%runimpute	(	rs17001561	,	rs17001561_mean	,	rs17001561_meansum	)
%runimpute	(	rs17024393	,	rs17024393_mean	,	rs17024393_meansum	)
%runimpute	(	rs17094222	,	rs17094222_mean	,	rs17094222_meansum	)
%runimpute	(	rs17203016	,	rs17203016_mean	,	rs17203016_meansum	)
%runimpute	(	rs17405819	,	rs17405819_mean	,	rs17405819_meansum	)
%runimpute	(	rs17724992	,	rs17724992_mean	,	rs17724992_meansum	)
%runimpute	(	rs1808579	,	rs1808579_mean	,	rs1808579_meansum	)
%runimpute	(	rs1928295	,	rs1928295_mean	,	rs1928295_meansum	)
%runimpute	(	rs2035935	,	rs2035935_mean	,	rs2035935_meansum	)
%runimpute	(	rs2033732	,	rs2033732_mean	,	rs2033732_meansum	)
%runimpute	(	rs205262	,	rs205262_mean	,	rs205262_meansum	)
%runimpute	(	rs2075650	,	rs2075650_mean	,	rs2075650_meansum	)
%runimpute	(	rs2080454	,	rs2080454_mean	,	rs2080454_meansum	)
%runimpute	(	rs2112347	,	rs2112347_mean	,	rs2112347_meansum	)
%runimpute	(	rs2121279	,	rs2121279_mean	,	rs2121279_meansum	)
%runimpute	(	rs2972143	,	rs2972143_mean	,	rs2972143_meansum	)
%runimpute	(	rs2176598	,	rs2176598_mean	,	rs2176598_meansum	)
%runimpute	(	rs734597	,	rs734597_mean	,	rs734597_meansum	)
%runimpute	(	rs2245368	,	rs2245368_mean	,	rs2245368_meansum	)
%runimpute	(	rs2287019	,	rs2287019_mean	,	rs2287019_meansum	)
%runimpute	(	rs2365389	,	rs2365389_mean	,	rs2365389_meansum	)
%runimpute	(	rs2650492	,	rs2650492_mean	,	rs2650492_meansum	)
%runimpute	(	rs2820292	,	rs2820292_mean	,	rs2820292_meansum	)
%runimpute	(	rs2836754	,	rs2836754_mean	,	rs2836754_meansum	)
%runimpute	(	rs29941	,		rs29941_mean	,	rs29941_meansum	)
%runimpute	(	rs3101336	,	rs3101336_mean	,	rs3101336_meansum	)
%runimpute	(	rs3736485	,	rs3736485_mean	,	rs3736485_meansum	)
%runimpute	(	rs3810291	,	rs3810291_mean	,	rs3810291_meansum	)
%runimpute	(	rs3817334	,	rs3817334_mean	,	rs3817334_meansum	)
%runimpute	(	rs3849570	,	rs3849570_mean	,	rs3849570_meansum	)
%runimpute	(	rs3888190	,	rs3888190_mean	,	rs3888190_meansum	)
%runimpute	(	rs7113874	,	rs7113874_mean	,	rs7113874_meansum	)
%runimpute	(	rs4740619	,	rs4740619_mean	,	rs4740619_meansum	)
%runimpute	(	rs4787491	,	rs4787491_mean	,	rs4787491_meansum	)
%runimpute	(	rs492400	,	rs492400_mean	,	rs492400_meansum	)
%runimpute	(	rs543874	,	rs543874_mean	,	rs543874_meansum	)
%runimpute	(	rs6091540	,	rs6091540_mean	,	rs6091540_meansum	)
%runimpute	(	rs6465468	,	rs6465468_mean	,	rs6465468_meansum	)
%runimpute	(	rs6477694	,	rs6477694_mean	,	rs6477694_meansum	)
%runimpute	(	rs6567160	,	rs6567160_mean	,	rs6567160_meansum	)
%runimpute	(	rs657452	,	rs3127553_mean	,	rs3127553_meansum	)
%runimpute	(	rs6804842	,	rs6804842_mean	,	rs6804842_meansum	)
%runimpute	(	rs7138803	,	rs7138803_mean	,	rs7138803_meansum	)
%runimpute	(	rs7141420	,	rs7141420_mean	,	rs7141420_meansum	)
%runimpute	(	rs7164727	,	rs7164727_mean	,	rs7164727_meansum	)
%runimpute	(	rs7239883	,	rs7239883_mean	,	rs7239883_meansum	)
%runimpute	(	rs7243357	,	rs7243357_mean	,	rs7243357_meansum	)
%runimpute	(	rs758747	,	rs758747_mean	,	rs758747_meansum	)
%runimpute	(	rs7599312	,	rs7599312_mean	,	rs7599312_meansum	)
%runimpute	(	rs7715256	,	rs7715256_mean	,	rs7715256_meansum	)
%runimpute	(	rs7899106	,	rs7899106_mean	,	rs7899106_meansum	)
%runimpute	(	rs7903146b	,	rs7903146b_mean	,	rs7903146b_meansum	)
%runimpute	(	rs9374842	,	rs9374842_mean	,	rs9374842_meansum	)
%runimpute	(	rs9400239	,	rs9400239_mean	,	rs9400239_meansum	)
%runimpute	(	rs9540493	,	rs9540493_mean	,	rs9540493_meansum	)
%runimpute	(	rs5014937	,	rs5014937_mean	,	rs5014937_meansum	)
%runimpute	(	rs977747	,	rs977747_mean	,	rs977747_meansum	)
%runimpute	(	rs9914578	,	rs9914578_mean	,	rs9914578_meansum	)
%runimpute	(	rs9925964	,	rs9925964_mean	,	rs9925964_meansum	)

%runimpute	(rs1260326		,	rs1260326_mean		,	rs1260326_meansum)
%runimpute	(rs11717195t	,	rs11717195t_mean	,	rs11717195t_meansum)
%runimpute	(rs12255372		,	rs12255372_mean		,	rs12255372_meansum)
%runimpute	(rs1436958		,	rs1436958_mean		,	rs1436958_meansum)
%runimpute	(rs11672660		,	rs11672660_mean		,	rs11672660_meansum)
%runimpute	(rs6975024		,	rs6975024_mean		,	rs6975024_meansum)
%runimpute	(rs11782386		,	rs11782386_mean		,	rs11782386_meansum)
%runimpute	(rs1019503		,	rs1019503_mean		,	rs1019503_meansum)
%runimpute	(rs7651090t		,	rs7651090t_mean		,	rs7651090t_meansum)

%runimpute	(rs10811661f	,	rs10811661f_mean	,	rs10811661f_meansum)
%runimpute	(rs4869272		,	rs4869272_mean		,	rs4869272_meansum)
%runimpute	(rs11619319		,	rs11619319_mean		,	rs11619319_meansum)
%runimpute	(rs983309		,	rs983309_mean		,	rs983309_meansum)
%runimpute	(rs6943153		,	rs6943153_mean		,	rs6943153_meansum)
%runimpute	(rs11603334		,	rs11603334_mean		,	rs11603334_meansum)
%runimpute	(rs6113722		,	rs6113722_mean		,	rs6113722_meansum)
%runimpute	(rs16913693		,	rs16913693_mean		,	rs16913693_meansum)
%runimpute	(rs3829109		,	rs3829109_mean		,	rs3829109_meansum)
%runimpute	(rs3783347		,	rs3783347_mean		,	rs3783347_meansum)
%runimpute	(rs2302593		,	rs2302593_mean		,	rs2302593_meansum)
%runimpute	(rs9368222		,	rs9368222_mean		,	rs9368222_meansum)
%runimpute	(rs10747083		,	rs10747083_mean		,	rs10747083_meansum)
%runimpute	(rs6072275		,	rs6072275_mean		,	rs6072275_meansum)
%runimpute	(rs7651090f		,	rs7651090f_mean		,	rs7651090f_meansum)
%runimpute	(rs576674		,	rs576674_mean		,	rs576674_meansum)
%runimpute	(rs11715915		,	rs11715915_mean		,	rs11715915_meansum)
%runimpute	(rs17762454		,	rs17762454_mean		,	rs17762454_meansum)
%runimpute	(rs7708285		,	rs7708285_mean		,	rs7708285_meansum)
%runimpute	(rs2657879		,	rs2657879_mean		,	rs2657879_meansum)
%runimpute	(rs340874f		,	rs340874f_mean		,	rs340874f_meansum)
%runimpute	(rs780094f		,	rs780094f_mean		,	rs780094f_meansum)
%runimpute	(rs560887		,	rs560887_mean		,	rs560887_meansum)
%runimpute	(rs11708067		,	rs11708067_mean	,	rs11708067_meansum)
%runimpute	(rs1280			,	rs1280_mean		,	rs1280_meansum)
%runimpute	(rs2191349		,	rs2191349_mean		,	rs2191349_meansum)
%runimpute	(rs2908289		,	rs2908289_mean		,	rs2908289_meansum)
%runimpute	(rs11558471		,	rs11558471_mean		,	rs11558471_meansum)
%runimpute	(rs10814916		,	rs10814916_mean		,	rs10814916_meansum)
%runimpute	(rs11195502		,	rs11195502_mean		,	rs11195502_meansum)
%runimpute	(rs7903146f		,	rs7903146f_mean		,	rs7903146f_meansum)
%runimpute	(rs11607883		,	rs11607883_mean		,	rs11607883_meansum)
%runimpute	(rs11039182		,	rs11039182_mean		,	rs11039182_meansum)
%runimpute	(rs174576		,	rs174576_mean		,	rs174576_meansum)
%runimpute	(rs10830963f	,	rs10830963f_mean	,	rs10830963f_meansum)
%runimpute	(rs4502156f		,	rs4502156f_mean		,	rs4502156f_meansum)

%runimpute	(rs340874d		,	rs340874d_mean		,	rs340874d_meansum)
%runimpute	(rs7515431		,	rs7515431_mean		,	rs7515431_meansum)
%runimpute	(rs7903146d		,	rs7903146d_mean		,	rs7903146d_meansum)
%runimpute	(rs12571751		,	rs12571751_mean		,	rs12571751_meansum)
%runimpute	(rs12242953		,	rs12242953_mean		,	rs12242953_meansum)
%runimpute	(rs11257655		,	rs11257655_mean		,	rs11257655_meansum)
%runimpute	(rs1111875		,	rs1111875_mean		,	rs1111875_meansum)	
%runimpute	(rs5215			,	rs5215_mean			,	rs5215_meansum)
%runimpute	(rs2334499		,	rs2334499_mean		,	rs2334499_meansum)
%runimpute	(rs163184		,	rs163184_mean		,	rs163184_meansum)
%runimpute	(rs1552224		,	rs1552224_mean		,	rs1552224_meansum)
%runimpute	(rs10830963d	,	rs10830963d_mean	,	rs10830963d_meansum)
%runimpute	(rs7955901		,	rs7955901_mean		,	rs7955901_meansum)
%runimpute	(rs2261181		,	rs2261181_mean		,	rs2261181_meansum)
%runimpute	(rs7965349		,	rs7965349_mean		,	rs7965349_meansum)
%runimpute	(rs11063069		,	rs11063069_mean		,	rs11063069_meansum)
%runimpute	(rs10842994		,	rs10842994_mean		,	rs10842994_meansum)
%runimpute	(rs1359790		,	rs1359790_mean		,	rs1359790_meansum)
%runimpute	(rs7177055		,	rs7177055_mean		,	rs7177055_meansum)
%runimpute	(rs4502156d		,	rs4502156d_mean		,	rs4502156d_meansum)
%runimpute	(rs2007084		,	rs2007084_mean		,	rs2007084_meansum)
%runimpute	(rs12899811		,	rs12899811_mean		,	rs12899811_meansum)
%runimpute	(rs11634397		,	rs11634397_mean		,	rs11634397_meansum)
%runimpute	(rs9923233		,	rs9923233_mean		,	rs9923233_meansum)
%runimpute	(rs7202877		,	rs7202877_mean		,	rs7202877_meansum)
%runimpute	(rs11651755		,	rs11651755_mean		,	rs11651755_meansum)
%runimpute	(rs2447090		,	rs2447090_mean		,	rs2447090_meansum)
%runimpute	(rs11663816		,	rs11663816_mean		,	rs11663816_meansum)
%runimpute	(rs8182584		,	rs8182584_mean		,	rs8182584_meansum)
%runimpute	(rs8108269		,	rs8108269_mean		,	rs8108269_meansum)
%runimpute	(rs10401969		,	rs10401969_mean		,	rs10401969_meansum)
%runimpute	(rs780094d		,	rs780094d_mean		,	rs780094d_meansum)
%runimpute	(rs7569522		,	rs7569522_mean		,	rs7569522_meansum)
%runimpute	(rs2943640		,	rs2943640_mean		,	rs2943640_meansum)
%runimpute	(rs243083		,	rs243083_mean		,	rs243083_meansum)
%runimpute	(rs1128249		,	rs1128249_mean		,	rs1128249_meansum)
%runimpute	(rs10203174		,	rs10203174_mean		,	rs10203174_meansum)
%runimpute	(rs4812829		,	rs4812829_mean		,	rs4812829_meansum)
%runimpute	(rs6795735		,	rs6795735_mean		,	rs6795735_meansum)
%runimpute	(rs4402960		,	rs4402960_mean		,	rs4402960_meansum)
%runimpute	(rs2197423		,	rs2197423_mean		,	rs2197423_meansum)
%runimpute	(rs17301514		,	rs17301514_mean		,	rs17301514_meansum)
%runimpute	(rs1496653		,	rs1496653_mean		,	rs1496653_meansum)
%runimpute	(rs12497268		,	rs12497268_mean		,	rs12497268_meansum)
%runimpute	(rs11717195d	,	rs11717195d_mean	,	rs11717195d_meansum)
%runimpute	(rs6819243		,	rs6819243_mean		,	rs6819243_meansum)
%runimpute	(rs10012946		,	rs10012946_mean		,	rs10012946_meansum)
%runimpute	(rs6878122		,	rs6878122_mean		,	rs6878122_meansum)
%runimpute	(rs459193		,	rs459193_mean		,	rs459193_meansum)
%runimpute	(rs7756992		,	rs7756992_mean		,	rs7756992_meansum)
%runimpute	(rs4299828		,	rs4299828_mean		,	rs4299828_meansum)
%runimpute	(rs3734621		,	rs3734621_mean		,	rs3734621_meansum)
%runimpute	(rs849135		,	rs849135_mean		,	rs849135_meansum)
%runimpute	(rs10229583		,	rs10229583_mean		,	rs10229583_meansum)
%runimpute	(rs17168486		,	rs17168486_mean		,	rs17168486_meansum)
%runimpute	(rs13233731		,	rs13233731_mean		,	rs13233731_meansum)
%runimpute	(rs3824065		,	rs3824065_mean		,	rs3824065_meansum)
%runimpute	(rs7845219		,	rs7845219_mean		,	rs7845219_meansum)
%runimpute	(rs516946		,	rs516946_mean		,	rs516946_meansum)
%runimpute	(rs3802177		,	rs3802177_mean		,	rs3802177_meansum)
%runimpute	(rs2796441		,	rs2796441_mean		,	rs2796441_meansum)
%runimpute	(rs17791513		,	rs17791513_mean		,	rs17791513_meansum)
%runimpute	(rs16927668		,	rs16927668_mean		,	rs16927668_meansum)
%runimpute	(rs10811661d	,	rs10811661d_mean	,	rs10811661d_meansum)
%runimpute	(rs10758593		,	rs10758593_mean		,	rs10758593_meansum)
;  

data gtmeans;
merge 
rs1000940_mean
rs10132280_mean
rs1016287_mean
rs10182181_mean
rs10733682_mean
rs10938397_mean
rs10968576_mean
rs7103411_mean
rs11057405_mean
rs11126666_mean
rs11165643_mean
rs11191560_mean
rs11583200_mean
rs1167827_mean
rs11688816_mean
rs11727676_mean
rs10134820_mean
rs1885988_mean
rs12286929_mean
rs12401738_mean
rs12429545_mean
rs12446632_mean
rs1514175_mean
rs12885454_mean
rs12940622_mean
rs13021737_mean
rs7622475_mean
rs13107325_mean
rs13191362_mean
rs13201877_mean
rs1441264_mean
rs1460676_mean
rs4234589_mean
rs1528435_mean
rs1421085_mean
rs2035935_mean
rs16907751_mean
rs2241420_mean
rs17001561_mean
rs17024393_mean
rs17094222_mean
rs17203016_mean
rs17405819_mean
rs17724992_mean
rs1808579_mean
rs1928295_mean
rs2033529_mean
rs2033732_mean
rs205262_mean
rs2075650_mean
rs2080454_mean
rs2112347_mean
rs2121279_mean
rs2972143_mean
rs2176598_mean
rs734597_mean
rs2245368_mean
rs2287019_mean
rs2365389_mean
rs2650492_mean
rs2820292_mean
rs2836754_mean
rs29941_mean
rs3101336_mean
rs3736485_mean
rs3810291_mean
rs3817334_mean
rs3849570_mean
rs3888190_mean
rs7113874_mean
rs4740619_mean
rs4787491_mean
rs492400_mean
rs543874_mean
rs6091540_mean
rs6465468_mean
rs6477694_mean
rs6567160_mean
rs3127553_mean
rs6804842_mean
rs7138803_mean
rs7141420_mean
rs7164727_mean
rs7239883_mean
rs7243357_mean
rs758747_mean
rs7599312_mean
rs7715256_mean
rs7899106_mean
rs7903146b_mean
rs9374842_mean
rs9400239_mean
rs9540493_mean
rs5014937_mean
rs977747_mean
rs9914578_mean
rs9925964_mean

rs1260326_mean
rs11717195t_mean
rs12255372_mean
rs1436958_mean
rs11672660_mean
rs6975024_mean
rs11782386_mean
rs1019503_mean
rs7651090t_mean

rs10811661f_mean
rs4869272_mean
rs11619319_mean
rs983309_mean
rs6943153_mean
rs11603334_mean
rs6113722_mean
rs16913693_mean
rs3829109_mean
rs3783347_mean
rs2302593_mean
rs9368222_mean
rs10747083_mean
rs6072275_mean
rs7651090f_mean
rs576674_mean
rs11715915_mean
rs17762454_mean
rs7708285_mean
rs2657879_mean
rs340874f_mean
rs780094f_mean
rs560887_mean
rs11708067_mean
rs1280_mean
rs2191349_mean
rs2908289_mean
rs11558471_mean
rs10814916_mean
rs11195502_mean
rs7903146f_mean
rs11607883_mean
rs11039182_mean
rs174576_mean
rs10830963f_mean
rs4502156f_mean

rs340874d_mean
rs7515431_mean
rs7903146d_mean
rs12571751_mean
rs12242953_mean
rs11257655_mean
rs1111875_mean
rs5215_mean
rs2334499_mean
rs163184_mean
rs1552224_mean
rs10830963d_mean
rs7955901_mean
rs2261181_mean
rs7965349_mean
rs11063069_mean
rs10842994_mean
rs1359790_mean
rs7177055_mean
rs4502156d_mean
rs2007084_mean
rs12899811_mean
rs11634397_mean
rs9923233_mean
rs7202877_mean
rs11651755_mean
rs2447090_mean
rs11663816_mean
rs8182584_mean
rs8108269_mean
rs10401969_mean
rs780094d_mean
rs7569522_mean
rs2943640_mean
rs243083_mean
rs1128249_mean
rs10203174_mean
rs4812829_mean
rs6795735_mean
rs4402960_mean
rs2197423_mean
rs17301514_mean
rs1496653_mean
rs12497268_mean
rs11717195d_mean
rs6819243_mean
rs10012946_mean
rs6878122_mean
rs459193_mean
rs7756992_mean
rs4299828_mean
rs3734621_mean
rs849135_mean
rs10229583_mean
rs17168486_mean
rs13233731_mean
rs3824065_mean
rs7845219_mean
rs516946_mean
rs3802177_mean
rs2796441_mean
rs17791513_mean
rs16927668_mean
rs10811661d_mean
rs10758593_mean; 
RUN;  
%macro runmerge(rsx,mean,meansum);

data &rsx;
merge A7 gtmeans;
&meansum+&mean;
if &rsx = . then &rsx = &meansum; 
keep id &rsx;
run;

%mend;
%runmerge	(	rs1000940	,	rs1000940_mean	,	rs1000940_meansum	)
%runmerge	(	rs10132280	,	rs10132280_mean	,	rs10132280_meansum	)
%runmerge	(	rs1016287	,	rs1016287_mean	,	rs1016287_meansum	)
%runmerge	(	rs10182181	,	rs10182181_mean	,	rs10182181_meansum	)
%runmerge	(	rs10733682	,	rs10733682_mean	,	rs10733682_meansum	)
%runmerge	(	rs10938397	,	rs10938397_mean	,	rs10938397_meansum	)
%runmerge	(	rs10968576	,	rs10968576_mean	,	rs10968576_meansum	)
%runmerge	(	rs7103411	,	rs7103411_mean	,	rs7103411_meansum	)
%runmerge	(	rs11057405	,	rs11057405_mean	,	rs11057405_meansum	)
%runmerge	(	rs11126666	,	rs11126666_mean	,	rs11126666_meansum	)
%runmerge	(	rs11165643	,	rs11165643_mean	,	rs11165643_meansum	)
%runmerge	(	rs11191560	,	rs11191560_mean	,	rs11191560_meansum	)
%runmerge	(	rs11583200	,	rs11583200_mean	,	rs11583200_meansum	)
%runmerge	(	rs1167827	,	rs1167827_mean	,	rs1167827_meansum	)
%runmerge	(	rs11688816	,	rs11688816_mean	,	rs11688816_meansum	)
%runmerge	(	rs11727676	,	rs11727676_mean	,	rs11727676_meansum	)
%runmerge	(	rs10134820	,	rs10134820_mean	,	rs10134820_meansum	)
%runmerge	(	rs1885988	,	rs1885988_mean	,	rs1885988_meansum	)
%runmerge	(	rs12286929	,	rs12286929_mean	,	rs12286929_meansum	)
%runmerge	(	rs12401738	,	rs12401738_mean	,	rs12401738_meansum	)
%runmerge	(	rs12429545	,	rs12429545_mean	,	rs12429545_meansum	)
%runmerge	(	rs12446632	,	rs12446632_mean	,	rs12446632_meansum	)
%runmerge	(	rs1514175	,	rs1514175_mean	,	rs1514175_meansum	)
%runmerge	(	rs12885454	,	rs12885454_mean	,	rs12885454_meansum	)
%runmerge	(	rs12940622	,	rs12940622_mean	,	rs12940622_meansum	)
%runmerge	(	rs13021737	,	rs13021737_mean	,	rs13021737_meansum	)
%runmerge	(	rs7622475	,	rs7622475_mean	,	rs7622475_meansum	)
%runmerge	(	rs13107325	,	rs13107325_mean	,	rs13107325_meansum	)
%runmerge	(	rs13191362	,	rs13191362_mean	,	rs13191362_meansum	)
%runmerge	(	rs13201877	,	rs13201877_mean	,	rs13201877_meansum	)
%runmerge	(	rs1441264	,	rs1441264_mean	,	rs1441264_meansum	)
%runmerge	(	rs1460676	,	rs1460676_mean	,	rs1460676_meansum	)
%runmerge	(	rs4234589	,	rs4234589_mean	,	rs4234589_meansum	)
%runmerge	(	rs1528435	,	rs1528435_mean	,	rs1528435_meansum	)
%runmerge	(	rs1421085	,	rs1421085_mean	,	rs1421085_meansum	)
%runmerge	(	rs2035935	,	rs2035935_mean	,	rs2035935_meansum	)
%runmerge	(	rs16907751	,	rs16907751_mean	,	rs16907751_meansum	)
%runmerge	(	rs2241420	,	rs2241420_mean	,	rs2241420_meansum	)
%runmerge	(	rs17001561	,	rs17001561_mean	,	rs17001561_meansum	)
%runmerge	(	rs17024393	,	rs17024393_mean	,	rs17024393_meansum	)
%runmerge	(	rs17094222	,	rs17094222_mean	,	rs17094222_meansum	)
%runmerge	(	rs17203016	,	rs17203016_mean	,	rs17203016_meansum	)
%runmerge	(	rs17405819	,	rs17405819_mean	,	rs17405819_meansum	)
%runmerge	(	rs17724992	,	rs17724992_mean	,	rs17724992_meansum	)
%runmerge	(	rs1808579	,	rs1808579_mean	,	rs1808579_meansum	)
%runmerge	(	rs1928295	,	rs1928295_mean	,	rs1928295_meansum	)
%runmerge	(	rs2033529	,	rs2033529_mean	,	rs2033529_meansum	)
%runmerge	(	rs2033732	,	rs2033732_mean	,	rs2033732_meansum	)
%runmerge	(	rs205262	,	rs205262_mean	,	rs205262_meansum	)
%runmerge	(	rs2075650	,	rs2075650_mean	,	rs2075650_meansum	)
%runmerge	(	rs2080454	,	rs2080454_mean	,	rs2080454_meansum	)
%runmerge	(	rs2112347	,	rs2112347_mean	,	rs2112347_meansum	)
%runmerge	(	rs2121279	,	rs2121279_mean	,	rs2121279_meansum	)
%runmerge	(	rs2972143	,	rs2972143_mean	,	rs2972143_meansum	)
%runmerge	(	rs2176598	,	rs2176598_mean	,	rs2176598_meansum	)
%runmerge	(	rs734597	,	rs734597_mean	,	rs734597_meansum	)
%runmerge	(	rs2245368	,	rs2245368_mean	,	rs2245368_meansum	)
%runmerge	(	rs2287019	,	rs2287019_mean	,	rs2287019_meansum	)
%runmerge	(	rs2365389	,	rs2365389_mean	,	rs2365389_meansum	)
%runmerge	(	rs2650492	,	rs2650492_mean	,	rs2650492_meansum	)
%runmerge	(	rs2820292	,	rs2820292_mean	,	rs2820292_meansum	)
%runmerge	(	rs2836754	,	rs2836754_mean	,	rs2836754_meansum	)
%runmerge	(	rs29941	,	rs29941_mean	,	rs29941_meansum	)
%runmerge	(	rs3101336	,	rs3101336_mean	,	rs3101336_meansum	)
%runmerge	(	rs3736485	,	rs3736485_mean	,	rs3736485_meansum	)
%runmerge	(	rs3810291	,	rs3810291_mean	,	rs3810291_meansum	)
%runmerge	(	rs3817334	,	rs3817334_mean	,	rs3817334_meansum	)
%runmerge	(	rs3849570	,	rs3849570_mean	,	rs3849570_meansum	)
%runmerge	(	rs3888190	,	rs3888190_mean	,	rs3888190_meansum	)
%runmerge	(	rs7113874	,	rs7113874_mean	,	rs7113874_meansum	)
%runmerge	(	rs4740619	,	rs4740619_mean	,	rs4740619_meansum	)
%runmerge	(	rs4787491	,	rs4787491_mean	,	rs4787491_meansum	)
%runmerge	(	rs492400	,	rs492400_mean	,	rs492400_meansum	)
%runmerge	(	rs543874	,	rs543874_mean	,	rs543874_meansum	)
%runmerge	(	rs6091540	,	rs6091540_mean	,	rs6091540_meansum	)
%runmerge	(	rs6465468	,	rs6465468_mean	,	rs6465468_meansum	)
%runmerge	(	rs6477694	,	rs6477694_mean	,	rs6477694_meansum	)
%runmerge	(	rs6567160	,	rs6567160_mean	,	rs6567160_meansum	)
%runmerge	(	rs657452	,	rs3127553_mean	,	rs3127553_meansum	)
%runmerge	(	rs6804842	,	rs6804842_mean	,	rs6804842_meansum	)
%runmerge	(	rs7138803	,	rs7138803_mean	,	rs7138803_meansum	)
%runmerge	(	rs7141420	,	rs7141420_mean	,	rs7141420_meansum	)
%runmerge	(	rs7164727	,	rs7164727_mean	,	rs7164727_meansum	)
%runmerge	(	rs7239883	,	rs7239883_mean	,	rs7239883_meansum	)
%runmerge	(	rs7243357	,	rs7243357_mean	,	rs7243357_meansum	)
%runmerge	(	rs758747	,	rs758747_mean	,	rs758747_meansum	)
%runmerge	(	rs7599312	,	rs7599312_mean	,	rs7599312_meansum	)
%runmerge	(	rs7715256	,	rs7715256_mean	,	rs7715256_meansum	)
%runmerge	(	rs7899106	,	rs7899106_mean	,	rs7899106_meansum	)
%runmerge	(	rs7903146b	,	rs7903146b_mean	,	rs7903146b_meansum	)
%runmerge	(	rs9374842	,	rs9374842_mean	,	rs9374842_meansum	)
%runmerge	(	rs9400239	,	rs9400239_mean	,	rs9400239_meansum	)
%runmerge	(	rs9540493	,	rs9540493_mean	,	rs9540493_meansum	)
%runmerge	(	rs5014937	,	rs5014937_mean	,	rs5014937_meansum	)
%runmerge	(	rs977747	,	rs977747_mean	,	rs977747_meansum	)
%runmerge	(	rs9914578	,	rs9914578_mean	,	rs9914578_meansum	)
%runmerge	(	rs9925964	,	rs9925964_mean	,	rs9925964_meansum	)

%runmerge	(rs1260326		,	rs1260326_mean		,	rs1260326_meansum)
%runmerge	(rs11717195t	,	rs11717195t_mean	,	rs11717195t_meansum)
%runmerge	(rs12255372		,	rs12255372_mean		,	rs12255372_meansum)
%runmerge	(rs1436958		,	rs1436958_mean		,	rs1436958_meansum)
%runmerge	(rs11672660		,	rs11672660_mean		,	rs11672660_meansum)
%runmerge	(rs6975024		,	rs6975024_mean		,	rs6975024_meansum)
%runmerge	(rs11782386		,	rs11782386_mean		,	rs11782386_meansum)
%runmerge	(rs1019503		,	rs1019503_mean		,	rs1019503_meansum)
%runmerge	(rs7651090t		,	rs7651090t_mean		,	rs7651090t_meansum)

%runmerge	(rs10811661f	,	rs10811661f_mean	,	rs10811661f_meansum)
%runmerge	(rs4869272		,	rs4869272_mean		,	rs4869272_meansum)
%runmerge	(rs11619319		,	rs11619319_mean		,	rs11619319_meansum)
%runmerge	(rs983309		,	rs983309_mean		,	rs983309_meansum)
%runmerge	(rs6943153		,	rs6943153_mean		,	rs6943153_meansum)
%runmerge	(rs11603334		,	rs11603334_mean		,	rs11603334_meansum)
%runmerge	(rs6113722		,	rs6113722_mean		,	rs6113722_meansum)
%runmerge	(rs16913693		,	rs16913693_mean		,	rs16913693_meansum)
%runmerge	(rs3829109		,	rs3829109_mean		,	rs3829109_meansum)
%runmerge	(rs3783347		,	rs3783347_mean		,	rs3783347_meansum)
%runmerge	(rs2302593		,	rs2302593_mean		,	rs2302593_meansum)
%runmerge	(rs9368222		,	rs9368222_mean		,	rs9368222_meansum)
%runmerge	(rs10747083		,	rs10747083_mean		,	rs10747083_meansum)
%runmerge	(rs6072275		,	rs6072275_mean		,	rs6072275_meansum)
%runmerge	(rs7651090f		,	rs7651090f_mean		,	rs7651090f_meansum)
%runmerge	(rs576674		,	rs576674_mean		,	rs576674_meansum)
%runmerge	(rs11715915		,	rs11715915_mean		,	rs11715915_meansum)
%runmerge	(rs17762454		,	rs17762454_mean		,	rs17762454_meansum)
%runmerge	(rs7708285		,	rs7708285_mean		,	rs7708285_meansum)
%runmerge	(rs2657879		,	rs2657879_mean		,	rs2657879_meansum)
%runmerge	(rs340874f		,	rs340874f_mean		,	rs340874f_meansum)
%runmerge	(rs780094f		,	rs780094f_mean		,	rs780094f_meansum)
%runmerge	(rs560887		,	rs560887_mean		,	rs560887_meansum)
%runmerge	(rs11708067		,	rs11708067_mean	,	rs11708067_meansum)
%runmerge	(rs1280			,	rs1280_mean		,	rs1280_meansum)
%runmerge	(rs2191349		,	rs2191349_mean		,	rs2191349_meansum)
%runmerge	(rs2908289		,	rs2908289_mean		,	rs2908289_meansum)
%runmerge	(rs11558471		,	rs11558471_mean		,	rs11558471_meansum)
%runmerge	(rs10814916		,	rs10814916_mean		,	rs10814916_meansum)
%runmerge	(rs11195502		,	rs11195502_mean		,	rs11195502_meansum)
%runmerge	(rs7903146f		,	rs7903146f_mean		,	rs7903146f_meansum)
%runmerge	(rs11607883		,	rs11607883_mean		,	rs11607883_meansum)
%runmerge	(rs11039182		,	rs11039182_mean		,	rs11039182_meansum)
%runmerge	(rs174576		,	rs174576_mean		,	rs174576_meansum)
%runmerge	(rs10830963f	,	rs10830963f_mean	,	rs10830963f_meansum)
%runmerge	(rs4502156f		,	rs4502156f_mean		,	rs4502156f_meansum)

%runmerge	(rs340874d		,	rs340874d_mean		,	rs340874d_meansum)
%runmerge	(rs7515431		,	rs7515431_mean		,	rs7515431_meansum)
%runmerge	(rs7903146d		,	rs7903146d_mean		,	rs7903146d_meansum)
%runmerge	(rs12571751		,	rs12571751_mean		,	rs12571751_meansum)
%runmerge	(rs12242953		,	rs12242953_mean		,	rs12242953_meansum)
%runmerge	(rs11257655		,	rs11257655_mean		,	rs11257655_meansum)
%runmerge	(rs1111875		,	rs1111875_mean		,	rs1111875_meansum)	
%runmerge	(rs5215			,	rs5215_mean			,	rs5215_meansum)
%runmerge	(rs2334499		,	rs2334499_mean		,	rs2334499_meansum)
%runmerge	(rs163184		,	rs163184_mean		,	rs163184_meansum)
%runmerge	(rs1552224		,	rs1552224_mean		,	rs1552224_meansum)
%runmerge	(rs10830963d	,	rs10830963d_mean	,	rs10830963d_meansum)
%runmerge	(rs7955901		,	rs7955901_mean		,	rs7955901_meansum)
%runmerge	(rs2261181		,	rs2261181_mean		,	rs2261181_meansum)
%runmerge	(rs7965349		,	rs7965349_mean		,	rs7965349_meansum)
%runmerge	(rs11063069		,	rs11063069_mean		,	rs11063069_meansum)
%runmerge	(rs10842994		,	rs10842994_mean		,	rs10842994_meansum)
%runmerge	(rs1359790		,	rs1359790_mean		,	rs1359790_meansum)
%runmerge	(rs7177055		,	rs7177055_mean		,	rs7177055_meansum)
%runmerge	(rs4502156d		,	rs4502156d_mean		,	rs4502156d_meansum)
%runmerge	(rs2007084		,	rs2007084_mean		,	rs2007084_meansum)
%runmerge	(rs12899811		,	rs12899811_mean		,	rs12899811_meansum)
%runmerge	(rs11634397		,	rs11634397_mean		,	rs11634397_meansum)
%runmerge	(rs9923233		,	rs9923233_mean		,	rs9923233_meansum)
%runmerge	(rs7202877		,	rs7202877_mean		,	rs7202877_meansum)
%runmerge	(rs11651755		,	rs11651755_mean		,	rs11651755_meansum)
%runmerge	(rs2447090		,	rs2447090_mean		,	rs2447090_meansum)
%runmerge	(rs11663816		,	rs11663816_mean		,	rs11663816_meansum)
%runmerge	(rs8182584		,	rs8182584_mean		,	rs8182584_meansum)
%runmerge	(rs8108269		,	rs8108269_mean		,	rs8108269_meansum)
%runmerge	(rs10401969		,	rs10401969_mean		,	rs10401969_meansum)
%runmerge	(rs780094d		,	rs780094d_mean		,	rs780094d_meansum)
%runmerge	(rs7569522		,	rs7569522_mean		,	rs7569522_meansum)
%runmerge	(rs2943640		,	rs2943640_mean		,	rs2943640_meansum)
%runmerge	(rs243083		,	rs243083_mean		,	rs243083_meansum)
%runmerge	(rs1128249		,	rs1128249_mean		,	rs1128249_meansum)
%runmerge	(rs10203174		,	rs10203174_mean		,	rs10203174_meansum)
%runmerge	(rs4812829		,	rs4812829_mean		,	rs4812829_meansum)
%runmerge	(rs6795735		,	rs6795735_mean		,	rs6795735_meansum)
%runmerge	(rs4402960		,	rs4402960_mean		,	rs4402960_meansum)
%runmerge	(rs2197423		,	rs2197423_mean		,	rs2197423_meansum)
%runmerge	(rs17301514		,	rs17301514_mean		,	rs17301514_meansum)
%runmerge	(rs1496653		,	rs1496653_mean		,	rs1496653_meansum)
%runmerge	(rs12497268		,	rs12497268_mean		,	rs12497268_meansum)
%runmerge	(rs11717195d	,	rs11717195d_mean	,	rs11717195d_meansum)
%runmerge	(rs6819243		,	rs6819243_mean		,	rs6819243_meansum)
%runmerge	(rs10012946		,	rs10012946_mean		,	rs10012946_meansum)
%runmerge	(rs6878122		,	rs6878122_mean		,	rs6878122_meansum)
%runmerge	(rs459193		,	rs459193_mean		,	rs459193_meansum)
%runmerge	(rs7756992		,	rs7756992_mean		,	rs7756992_meansum)
%runmerge	(rs4299828		,	rs4299828_mean		,	rs4299828_meansum)
%runmerge	(rs3734621		,	rs3734621_mean		,	rs3734621_meansum)
%runmerge	(rs849135		,	rs849135_mean		,	rs849135_meansum)
%runmerge	(rs10229583		,	rs10229583_mean		,	rs10229583_meansum)
%runmerge	(rs17168486		,	rs17168486_mean		,	rs17168486_meansum)
%runmerge	(rs13233731		,	rs13233731_mean		,	rs13233731_meansum)
%runmerge	(rs3824065		,	rs3824065_mean		,	rs3824065_meansum)
%runmerge	(rs7845219		,	rs7845219_mean		,	rs7845219_meansum)
%runmerge	(rs516946		,	rs516946_mean		,	rs516946_meansum)
%runmerge	(rs3802177		,	rs3802177_mean		,	rs3802177_meansum)
%runmerge	(rs2796441		,	rs2796441_mean		,	rs2796441_meansum)
%runmerge	(rs17791513		,	rs17791513_mean		,	rs17791513_meansum)
%runmerge	(rs16927668		,	rs16927668_mean		,	rs16927668_meansum)
%runmerge	(rs10811661d	,	rs10811661d_mean	,	rs10811661d_meansum)
%runmerge	(rs10758593		,	rs10758593_mean		,	rs10758593_meansum);  
 data core;
set A7;
drop 
rs1000940
rs10132280
rs1016287
rs10182181
rs10733682
rs10938397
rs10968576
rs7103411
rs11057405
rs11126666
rs11165643
rs11191560
rs11583200
rs1167827
rs11688816
rs11727676
rs10134820
rs1885988
rs12286929
rs12401738
rs12429545
rs12446632
rs1514175
rs12885454
rs12940622
rs13021737
rs7622475
rs13107325
rs13191362
rs13201877
rs1441264
rs1460676
rs4234589
rs1528435
rs1421085
rs2035935
rs16907751
rs2241420
rs17001561
rs17024393
rs17094222
rs17203016
rs17405819
rs17724992
rs1808579
rs1928295
rs2033529
rs2033732
rs205262
rs2075650
rs2080454
rs2112347
rs2121279
rs2972143
rs2176598
rs734597
rs2245368
rs2287019
rs2365389
rs2650492
rs2820292
rs2836754
rs29941
rs3101336
rs3736485
rs3810291
rs3817334
rs3849570
rs3888190
rs7113874
rs4740619
rs4787491
rs492400
rs543874
rs6091540
rs6465468
rs6477694
rs6567160
rs657452
rs6804842
rs7138803
rs7141420
rs7164727
rs7239883
rs7243357
rs758747
rs7599312
rs7715256
rs7899106
rs7903146b
rs9374842
rs9400239
rs9540493
rs5014937
rs977747
rs9914578
rs9925964

rs1260326
rs11717195t
rs12255372
rs1436958
rs11672660
rs6975024
rs11782386
rs1019503
rs7651090t

rs10811661f
rs4869272
rs11619319
rs983309
rs6943153
rs11603334
rs6113722
rs16913693
rs3829109
rs3783347
rs2302593
rs9368222
rs10747083
rs6072275
rs7651090f
rs576674
rs11715915
rs17762454
rs7708285
rs2657879
rs340874f
rs780094f
rs560887
rs11708067
rs1280
rs2191349
rs2908289
rs11558471
rs10814916
rs11195502
rs7903146f
rs11607883
rs11039182
rs174576
rs10830963f
rs4502156f

rs340874d
rs7515431
rs7903146d
rs12571751
rs12242953
rs11257655
rs1111875
rs5215
rs2334499
rs163184
rs1552224
rs10830963d
rs7955901
rs2261181
rs7965349
rs11063069
rs10842994
rs1359790
rs7177055
rs4502156d
rs2007084
rs12899811
rs11634397
rs9923233
rs7202877
rs11651755
rs2447090
rs11663816
rs8182584
rs8108269
rs10401969
rs780094d
rs7569522
rs2943640
rs243083
rs1128249
rs10203174
rs4812829
rs6795735
rs4402960
rs2197423
rs17301514
rs1496653
rs12497268
rs11717195d
rs6819243
rs10012946
rs6878122
rs459193
rs7756992
rs4299828
rs3734621
rs849135
rs10229583
rs17168486
rs13233731
rs3824065
rs7845219
rs516946
rs3802177
rs2796441
rs17791513
rs16927668
rs10811661d
rs10758593; 
run;  
proc	sort	data	=	core;	by	id;	run; 
proc	sort	data	=	rs1000940;	by	id;	
proc	sort	data	=	rs10132280;	by	id;	
proc	sort	data	=	rs1016287;	by	id;	
proc	sort	data	=	rs10182181;	by	id;	
proc	sort	data	=	rs10733682;	by	id;	
proc	sort	data	=	rs10938397;	by	id;	
proc	sort	data	=	rs10968576;	by	id;	
proc	sort	data	=	rs7103411;	by	id;	
proc	sort	data	=	rs11057405;	by	id;	
proc	sort	data	=	rs11126666;	by	id;	
proc	sort	data	=	rs11165643;	by	id;	
proc	sort	data	=	rs11191560;	by	id;	
proc	sort	data	=	rs11583200;	by	id;	
proc	sort	data	=	rs1167827;	by	id;	
proc	sort	data	=	rs11688816;	by	id;	
proc	sort	data	=	rs11727676;	by	id;	
proc	sort	data	=	rs10134820;	by	id;	
proc	sort	data	=	rs1885988;	by	id;	
proc	sort	data	=	rs12286929;	by	id;	
proc	sort	data	=	rs12401738;	by	id;	
proc	sort	data	=	rs12429545;	by	id;	
proc	sort	data	=	rs12446632;	by	id;	
proc	sort	data	=	rs1514175;	by	id;	
proc	sort	data	=	rs12885454;	by	id;	
proc	sort	data	=	rs12940622;	by	id;	
proc	sort	data	=	rs13021737;	by	id;	
proc	sort	data	=	rs7622475;	by	id;	
proc	sort	data	=	rs13107325;	by	id;	
proc	sort	data	=	rs13191362;	by	id;	
proc	sort	data	=	rs13201877;	by	id;	
proc	sort	data	=	rs1441264;	by	id;	
proc	sort	data	=	rs1460676;	by	id;	
proc	sort	data	=	rs4234589;	by	id;	
proc	sort	data	=	rs1528435;	by	id;	
proc	sort	data	=	rs1421085;	by	id;	
proc	sort	data	=	rs2035935;	by	id;	
proc	sort	data	=	rs16907751;	by	id;	
proc	sort	data	=	rs2241420;	by	id;	
proc	sort	data	=	rs17001561;	by	id;	
proc	sort	data	=	rs17024393;	by	id;	
proc	sort	data	=	rs17094222;	by	id;	
proc	sort	data	=	rs17203016;	by	id;	
proc	sort	data	=	rs17405819;	by	id;	
proc	sort	data	=	rs17724992;	by	id;	
proc	sort	data	=	rs1808579;	by	id;	
proc	sort	data	=	rs1928295;	by	id;	
proc	sort	data	=	rs2033529;	by	id;	
proc	sort	data	=	rs2033732;	by	id;	
proc	sort	data	=	rs205262;	by	id;	
proc	sort	data	=	rs2075650;	by	id;	
proc	sort	data	=	rs2080454;	by	id;	
proc	sort	data	=	rs2112347;	by	id;	
proc	sort	data	=	rs2121279;	by	id;	
proc	sort	data	=	rs2972143;	by	id;
proc	sort	data	=	rs2176598;	by	id;	
proc	sort	data	=	rs734597;	by	id;	
proc	sort	data	=	rs2245368;	by	id;	
proc	sort	data	=	rs2287019;	by	id;	
proc	sort	data	=	rs2365389;	by	id;	
proc	sort	data	=	rs2650492;	by	id;	
proc	sort	data	=	rs2820292;	by	id;	
proc	sort	data	=	rs2836754;	by	id;	
proc	sort	data	=	rs29941 ;	by	id;	
proc	sort	data	=	rs3101336;	by	id;	
proc	sort	data	=	rs3736485;	by	id;	
proc	sort	data	=	rs3810291;	by	id;	
proc	sort	data	=	rs3817334;	by	id;	
proc	sort	data	=	rs3849570;	by	id;	
proc	sort	data	=	rs3888190;	by	id;	
proc	sort	data	=	rs7113874;	by	id;	
proc	sort	data	=	rs4740619;	by	id;	
proc	sort	data	=	rs4787491;	by	id;	
proc	sort	data	=	rs492400;	by	id;	
proc	sort	data	=	rs543874;	by	id;	
proc	sort	data	=	rs6091540;	by	id;	
proc	sort	data	=	rs6465468;	by	id;	
proc	sort	data	=	rs6477694;	by	id;	
proc	sort	data	=	rs6567160;	by	id;	
proc	sort	data	=	rs657452;	by	id;	
proc	sort	data	=	rs6804842;	by	id;	
proc	sort	data	=	rs7138803;	by	id;	
proc	sort	data	=	rs7141420;	by	id;	
proc	sort	data	=	rs7164727;	by	id;	
proc	sort	data	=	rs7239883;	by	id;	
proc	sort	data	=	rs7243357;	by	id;	
proc	sort	data	=	rs758747;	by	id;	
proc	sort	data	=	rs7599312;	by	id;	
proc	sort	data	=	rs7715256;	by	id;	
proc	sort	data	=	rs7899106;	by	id;	
proc	sort	data	=	rs7903146b;	by	id;	
proc	sort	data	=	rs9374842;	by	id;	
proc	sort	data	=	rs9400239;	by	id;	
proc	sort	data	=	rs9540493;	by	id;	
proc	sort	data	=	rs5014937;	by	id;	
proc	sort	data	=	rs977747;	by	id;	
proc	sort	data	=	rs9914578;	by	id;	
proc	sort	data	=	rs9925964;	by	id;	

proc	sort	data	=	rs1260326;	by	id;	
proc	sort	data	=	rs11717195t;	by	id;	
proc	sort	data	=	rs12255372;	by	id;	
proc	sort	data	=	rs1436958;	by	id;	
proc	sort	data	=	rs11672660;	by	id;	
proc	sort	data	=	rs6975024;	by	id;	
proc	sort	data	=	rs11782386;	by	id;	
proc	sort	data	=	rs1019503;	by	id;	
proc	sort	data	=	rs7651090t;	by	id;	

proc	sort	data	=	rs10811661f; by	id;	
proc	sort	data	=	rs4869272;	by	id;	
proc	sort	data	=	rs11619319;	by	id;	
proc	sort	data	=	rs983309;	by	id;	
proc	sort	data	=	rs6943153;	by	id;	
proc	sort	data	=	rs11603334;	by	id;	
proc	sort	data	=	rs6113722;	by	id;	
proc	sort	data	=	rs16913693;	by	id;	
proc	sort	data	=	rs3829109;	by	id;	
proc	sort	data	=	rs3783347;	by	id;	
proc	sort	data	=	rs2302593;	by	id;	
proc	sort	data	=	rs9368222;	by	id;	
proc	sort	data	=	rs10747083;	by	id;	
proc	sort	data	=	rs6072275;	by	id;	
proc	sort	data	=	rs7651090f;	by	id;	
proc	sort	data	=	rs576674;	by	id;	
proc	sort	data	=	rs11715915;	by	id;	
proc	sort	data	=	rs17762454;	by	id;	
proc	sort	data	=	rs7708285;	by	id;	
proc	sort	data	=	rs2657879;	by	id;	
proc	sort	data	=	rs340874f;	by	id;	
proc	sort	data	=	rs780094f;	by	id;	
proc	sort	data	=	rs560887;	by	id;	
proc	sort	data	=	rs11708067;	by	id;	
proc	sort	data	=	rs1280;	by	id;	
proc	sort	data	=	rs2191349;	by	id;	
proc	sort	data	=	rs2908289;	by	id;	
proc	sort	data	=	rs11558471;	by	id;	
proc	sort	data	=	rs10814916;	by	id;	
proc	sort	data	=	rs11195502;	by	id;	
proc	sort	data	=	rs7903146f;	by	id;	
proc	sort	data	=	rs11607883;	by	id;	
proc	sort	data	=	rs11039182;	by	id;	
proc	sort	data	=	rs174576;	by	id;	
proc	sort	data	=	rs10830963f;	by	id;	
proc	sort	data	=	rs4502156f;	by	id;	

proc	sort	data	=	rs340874d;	by	id;	
proc	sort	data	=	rs7515431;	by	id;	
proc	sort	data	=	rs7903146d;	by	id;	
proc	sort	data	=	rs12571751;	by	id;	
proc	sort	data	=	rs12242953;	by	id;	
proc	sort	data	=	rs11257655;	by	id;	
proc	sort	data	=	rs1111875;	by	id;	
proc	sort	data	=	rs5215;	by	id;	
proc	sort	data	=	rs2334499;	by	id;	
proc	sort	data	=	rs163184;	by	id;	
proc	sort	data	=	rs1552224;	by	id;	
proc	sort	data	=	rs10830963d; by	id;	
proc	sort	data	=	rs7955901;	by	id;	
proc	sort	data	=	rs2261181;	by	id;	
proc	sort	data	=	rs7965349;	by	id;	
proc	sort	data	=	rs11063069;	by	id;	
proc	sort	data	=	rs10842994;	by	id;	
proc	sort	data	=	rs1359790;	by	id;	
proc	sort	data	=	rs7177055;	by	id;	
proc	sort	data	=	rs4502156d;	by	id;	
proc	sort	data	=	rs2007084;	by	id;	
proc	sort	data	=	rs12899811;	by	id;	
proc	sort	data	=	rs11634397;	by	id;	
proc	sort	data	=	rs9923233;	by	id;	
proc	sort	data	=	rs7202877;	by	id;	
proc	sort	data	=	rs11651755;	by	id;	
proc	sort	data	=	rs2447090;	by	id;	
proc	sort	data	=	rs11663816;	by	id;	
proc	sort	data	=	rs8182584;	by	id;	
proc	sort	data	=	rs8108269;	by	id;	
proc	sort	data	=	rs10401969;	by	id;	
proc	sort	data	=	rs780094d;	by	id;	
proc	sort	data	=	rs7569522;	by	id;	
proc	sort	data	=	rs2943640;	by	id;	
proc	sort	data	=	rs243083;	by	id;	
proc	sort	data	=	rs1128249;	by	id;	
proc	sort	data	=	rs10203174;	by	id;	
proc	sort	data	=	rs4812829;	by	id;	
proc	sort	data	=	rs6795735;	by	id;	
proc	sort	data	=	rs4402960;	by	id;	
proc	sort	data	=	rs2197423;	by	id;	
proc	sort	data	=	rs17301514;	by	id;	
proc	sort	data	=	rs1496653;	by	id;	
proc	sort	data	=	rs12497268;	by	id;	
proc	sort	data	=	rs11717195d;	by	id;	
proc	sort	data	=	rs6819243;	by	id;	
proc	sort	data	=	rs10012946;	by	id;	
proc	sort	data	=	rs6878122;	by	id;	
proc	sort	data	=	rs459193;	by	id;	
proc	sort	data	=	rs7756992;	by	id;	
proc	sort	data	=	rs4299828;	by	id;	
proc	sort	data	=	rs3734621;	by	id;	
proc	sort	data	=	rs849135;	by	id;	
proc	sort	data	=	rs10229583;	by	id;	
proc	sort	data	=	rs17168486;	by	id;	
proc	sort	data	=	rs13233731;	by	id;	
proc	sort	data	=	rs3824065;	by	id;	
proc	sort	data	=	rs7845219;	by	id;	
proc	sort	data	=	rs516946;	by	id;	
proc	sort	data	=	rs3802177;	by	id;	
proc	sort	data	=	rs2796441;	by	id;	
proc	sort	data	=	rs17791513;	by	id;	
proc	sort	data	=	rs16927668;	by	id;	
proc	sort	data	=	rs10811661d;	by	id;	
proc	sort	data	=	rs10758593;	by	id;	
run; 


data a3_impute;
merge 
core 
rs1000940
rs10132280
rs1016287
rs10182181
rs10733682
rs10938397
rs10968576
rs7103411
rs11057405
rs11126666
rs11165643
rs11191560
rs11583200
rs1167827
rs11688816
rs11727676
rs10134820
rs1885988
rs12286929
rs12401738
rs12429545
rs12446632
rs1514175
rs12885454
rs12940622
rs13021737
rs7622475
rs13107325
rs13191362
rs13201877
rs1441264
rs1460676
rs4234589
rs1528435
rs1421085
rs2035935
rs16907751
rs2241420
rs17001561
rs17024393
rs17094222
rs17203016
rs17405819
rs17724992
rs1808579
rs1928295
rs2033529
rs2033732
rs205262
rs2075650
rs2080454
rs2112347
rs2121279
rs2972143
rs2176598
rs734597
rs2245368
rs2287019
rs2365389
rs2650492
rs2820292
rs2836754
rs29941
rs3101336
rs3736485
rs3810291
rs3817334
rs3849570
rs3888190
rs7113874
rs4740619
rs4787491
rs492400
rs543874
rs6091540
rs6465468
rs6477694
rs6567160
rs657452
rs6804842
rs7138803
rs7141420
rs7164727
rs7239883
rs7243357
rs758747
rs7599312
rs7715256
rs7899106
rs7903146b
rs9374842
rs9400239
rs9540493
rs5014937
rs977747
rs9914578
rs9925964

rs1260326
rs11717195t
rs12255372
rs1436958
rs11672660
rs6975024
rs11782386
rs1019503
rs7651090t

rs10811661f
rs4869272
rs11619319
rs983309
rs6943153
rs11603334
rs6113722
rs16913693
rs3829109
rs3783347
rs2302593
rs9368222
rs10747083
rs6072275
rs7651090f
rs576674
rs11715915
rs17762454
rs7708285
rs2657879
rs340874f
rs780094f
rs560887
rs11708067
rs1280
rs2191349
rs2908289
rs11558471
rs10814916
rs11195502
rs7903146f
rs11607883
rs11039182
rs174576
rs10830963f
rs4502156f

rs340874d
rs7515431
rs7903146d
rs12571751
rs12242953
rs11257655
rs1111875
rs5215
rs2334499
rs163184
rs1552224
rs10830963d
rs7955901
rs2261181
rs7965349
rs11063069
rs10842994
rs1359790
rs7177055
rs4502156d
rs2007084
rs12899811
rs11634397
rs9923233
rs7202877
rs11651755
rs2447090
rs11663816
rs8182584
rs8108269
rs10401969
rs780094d
rs7569522
rs2943640
rs243083
rs1128249
rs10203174
rs4812829
rs6795735
rs4402960
rs2197423
rs17301514
rs1496653
rs12497268
rs11717195d
rs6819243
rs10012946
rs6878122
rs459193
rs7756992
rs4299828
rs3734621
rs849135
rs10229583
rs17168486
rs13233731
rs3824065
rs7845219
rs516946
rs3802177
rs2796441
rs17791513
rs16927668
rs10811661d
rs10758593; 
run;

data stuff.A8_impute;/*N=5,726*/
set a3_impute;

x1	=	.; 
x2	=	.; 
x3	=	.; 
x4	=	.; 
x5	=	.; 
x6	=	.; 
x7	=	.; 
x8	=	.; 
x9	=	.; 
x10	=	.; 
x11	=	.; 
x12	=	.; 
x13	=	.; 
x14	=	.; 
x15	=	.; 
x16	=	.; 
x17	=	.; 
x18	=	.; 
x19	=	.; 
x20	=	.; 
x21	=	.; 
x22	=	.; 
x23	=	.; 
x24	=	.; 
x25	=	.; 
x26	=	.; 
x27	=	.; 
x28	=	.; 
x29	=	.; 
x30	=	.; 
x31	=	.; 
x32	=	.; 
x33	=	.; 
x34	=	.; 
x35	=	.; 
x36	=	.; 
x37	=	.; 
x38	=	.; 
x39	=	.; 
x40	=	.; 
x41	=	.; 
x42	=	.; 
x43	=	.; 
x44	=	.; 
x45	=	.; 
x46	=	.; 
x47	=	.; 
x48	=	.; 
x49	=	.; 
x50	=	.; 
x51	=	.; 
x52	=	.; 
x53	=	.; 
x54	=	.; 
x55	=	.; 
x56	=	.; 
x57	=	.; 
x58	=	.; 
x59	=	.; 
x60	=	.; 
x61	=	.; 
x62	=	.; 
x63	=	.; 
x64	=	.; 
x65	=	.; 
x66	=	.; 
x67	=	.; 
x68	=	.; 
x69	=	.; 
x70	=	.; 
x71	=	.; 
x72	=	.; 
x73	=	.; 
x74	=	.; 
x75	=	.; 
x76	=	.; 
x77	=	.; 
x78	=	.; 
x79	=	.; 
x80	=	.; 
x81	=	.; 
x82	=	.; 
x83	=	.; 
x84	=	.; 
x85	=	.; 
x86	=	.; 
x87	=	.; 
x88	=	.; 
x89	=	.; 
x90	=	.; 
x91	=	.; 
x92	=	.; 
x93	=	.; 
x94	=	.; 
x95	=	.; 
x96	=	.; 
x97	=	.; 

xa1	=	.; 
xa2	=	.; 
xa3	=	.; 
xa4	=	.; 
xa5	=	.; 
xa6	=	.; 
xa7	=	.; 
xa8	=	.; 
xa9	=	.; 

xb1	=	.; 
xb2	=	.; 
xb3	=	.; 
xb4	=	.; 
xb5	=	.; 
xb6	=	.; 
xb7	=	.; 
xb8	=	.; 
xb9	=	.; 
xb10	=	.; 
xb11	=	.; 
xb12	=	.; 
xb13	=	.; 
xb14	=	.; 
xb15	=	.; 
xb16	=	.; 
xb17	=	.; 
xb18	=	.; 
xb19	=	.; 
xb20	=	.; 
xb21	=	.; 
xb22	=	.; 
xb23	=	.; 
xb24	=	.; 
xb25	=	.; 
xb26	=	.; 
xb27	=	.; 
xb28	=	.; 
xb29	=	.; 
xb30	=	.; 
xb31	=	.; 
xb32	=	.; 
xb33	=	.; 
xb34	=	.; 
xb35	=	.; 
xb36	=	.; 


xc1	=	.; 
xc2	=	.; 
xc3	=	.; 
xc4	=	.; 
xc5	=	.; 
xc6	=	.; 
xc7	=	.; 
xc8	=	.; 
xc9	=	.; 
xc10	=	.; 
xc11	=	.; 
xc12	=	.; 
xc13	=	.; 
xc14	=	.; 
xc15	=	.; 
xc16	=	.; 
xc17	=	.; 
xc18	=	.; 
xc19	=	.; 
xc20	=	.; 
xc21	=	.; 
xc22	=	.; 
xc23	=	.; 
xc24	=	.; 
xc25	=	.; 
xc26	=	.; 
xc27	=	.; 
xc28	=	.; 
xc29	=	.; 
xc30	=	.; 
xc31	=	.; 
xc32	=	.; 
xc33	=	.; 
xc34	=	.; 
xc35	=	.; 
xc36	=	.; 
xc37	=	.; 
xc38	=	.; 
xc39	=	.; 
xc40	=	.; 
xc41	=	.; 
xc42	=	.; 
xc43	=	.; 
xc44	=	.; 
xc45	=	.; 
xc46	=	.; 
xc47	=	.; 
xc48	=	.; 
xc49	=	.; 
xc50	=	.; 
xc51	=	.; 
xc52	=	.; 
xc53	=	.; 
xc54	=	.; 
xc55	=	.; 
xc56	=	.; 
xc57	=	.; 
xc58	=	.; 
xc59	=	.; 
xc60	=	.; 
xc61	=	.; 
xc62	=	.; 
xc63	=	.; 
xc64	=	.; 
xc65	=	.; 

if	0	le	rs1000940	lt	0.5	then	x1	=	0;	if	0.5	le	rs1000940	lt	1.5	then	x1	=	1;	if	1.5	le	rs1000940	le	2	then	x1	=	2;
if	0	le	rs10132280	lt	0.5	then	x2	=	0;	if	0.5	le	rs10132280	lt	1.5	then	x2	=	1;	if	1.5	le	rs10132280	le	2	then	x2	=	2;
if	0	le	rs1016287	lt	0.5	then	x3	=	0;	if	0.5	le	rs1016287	lt	1.5	then	x3	=	1;	if	1.5	le	rs1016287	le	2	then	x3	=	2;
if	0	le	rs10182181	lt	0.5	then	x4	=	0;	if	0.5	le	rs10182181	lt	1.5	then	x4	=	1;	if	1.5	le	rs10182181	le	2	then	x4	=	2;
if	0	le	rs10733682	lt	0.5	then	x5	=	0;	if	0.5	le	rs10733682	lt	1.5	then	x5	=	1;	if	1.5	le	rs10733682	le	2	then	x5	=	2;
if	0	le	rs10938397	lt	0.5	then	x6	=	0;	if	0.5	le	rs10938397	lt	1.5	then	x6	=	1;	if	1.5	le	rs10938397	le	2	then	x6	=	2;
if	0	le	rs10968576	lt	0.5	then	x7	=	0;	if	0.5	le	rs10968576	lt	1.5	then	x7	=	1;	if	1.5	le	rs10968576	le	2	then	x7	=	2;
if	0	le	rs7103411	lt	0.5	then	x8	=	0;	if	0.5	le	rs7103411	lt	1.5	then	x8	=	1;	if	1.5	le	rs7103411	le	2	then	x8	=	2;
if	0	le	rs11057405	lt	0.5	then	x9	=	0;	if	0.5	le	rs11057405	lt	1.5	then	x9	=	1;	if	1.5	le	rs11057405	le	2	then	x9	=	2;
if	0	le	rs11126666	lt	0.5	then	x10	=	0;	if	0.5	le	rs11126666	lt	1.5	then	x10	=	1;	if	1.5	le	rs11126666	le	2	then	x10	=	2;
if	0	le	rs11165643	lt	0.5	then	x11	=	0;	if	0.5	le	rs11165643	lt	1.5	then	x11	=	1;	if	1.5	le	rs11165643	le	2	then	x11	=	2;
if	0	le	rs11191560	lt	0.5	then	x12	=	0;	if	0.5	le	rs11191560	lt	1.5	then	x12	=	1;	if	1.5	le	rs11191560	le	2	then	x12	=	2;
if	0	le	rs11583200	lt	0.5	then	x13	=	0;	if	0.5	le	rs11583200	lt	1.5	then	x13	=	1;	if	1.5	le	rs11583200	le	2	then	x13	=	2;
if	0	le	rs1167827	lt	0.5	then	x14	=	0;	if	0.5	le	rs1167827	lt	1.5	then	x14	=	1;	if	1.5	le	rs1167827	le	2	then	x14	=	2;
if	0	le	rs11688816	lt	0.5	then	x15	=	0;	if	0.5	le	rs11688816	lt	1.5	then	x15	=	1;	if	1.5	le	rs11688816	le	2	then	x15	=	2;
if	0	le	rs11727676	lt	0.5	then	x16	=	0;	if	0.5	le	rs11727676	lt	1.5	then	x16	=	1;	if	1.5	le	rs11727676	le	2	then	x16	=	2;
if	0	le	rs10134820	lt	0.5	then	x17	=	0;	if	0.5	le	rs10134820	lt	1.5	then	x17	=	1;	if	1.5	le	rs10134820	le	2	then	x17	=	2;
if	0	le	rs1885988	lt	0.5	then	x18	=	0;	if	0.5	le	rs1885988	lt	1.5	then	x18	=	1;	if	1.5	le	rs1885988	le	2	then	x18	=	2;
if	0	le	rs12286929	lt	0.5	then	x19	=	0;	if	0.5	le	rs12286929	lt	1.5	then	x19	=	1;	if	1.5	le	rs12286929	le	2	then	x19	=	2;
if	0	le	rs12401738	lt	0.5	then	x20	=	0;	if	0.5	le	rs12401738	lt	1.5	then	x20	=	1;	if	1.5	le	rs12401738	le	2	then	x20	=	2;
if	0	le	rs12429545	lt	0.5	then	x21	=	0;	if	0.5	le	rs12429545	lt	1.5	then	x21	=	1;	if	1.5	le	rs12429545	le	2	then	x21	=	2;
if	0	le	rs12446632	lt	0.5	then	x22	=	0;	if	0.5	le	rs12446632	lt	1.5	then	x22	=	1;	if	1.5	le	rs12446632	le	2	then	x22	=	2;
if	0	le	rs1514175	lt	0.5	then	x23	=	0;	if	0.5	le	rs1514175	lt	1.5	then	x23	=	1;	if	1.5	le	rs1514175	le	2	then	x23	=	2;
if	0	le	rs12885454	lt	0.5	then	x24	=	0;	if	0.5	le	rs12885454	lt	1.5	then	x24	=	1;	if	1.5	le	rs12885454	le	2	then	x24	=	2;
if	0	le	rs12940622	lt	0.5	then	x25	=	0;	if	0.5	le	rs12940622	lt	1.5	then	x25	=	1;	if	1.5	le	rs12940622	le	2	then	x25	=	2;
if	0	le	rs13021737	lt	0.5	then	x26	=	0;	if	0.5	le	rs13021737	lt	1.5	then	x26	=	1;	if	1.5	le	rs13021737	le	2	then	x26	=	2;
if	0	le	rs7622475	lt	0.5	then	x27	=	0;	if	0.5	le	rs7622475	lt	1.5	then	x27	=	1;	if	1.5	le	rs7622475	le	2	then	x27	=	2;
if	0	le	rs13107325	lt	0.5	then	x28	=	0;	if	0.5	le	rs13107325	lt	1.5	then	x28	=	1;	if	1.5	le	rs13107325	le	2	then	x28	=	2;
if	0	le	rs13191362	lt	0.5	then	x29	=	0;	if	0.5	le	rs13191362	lt	1.5	then	x29	=	1;	if	1.5	le	rs13191362	le	2	then	x29	=	2;
if	0	le	rs13201877	lt	0.5	then	x30	=	0;	if	0.5	le	rs13201877	lt	1.5	then	x30	=	1;	if	1.5	le	rs13201877	le	2	then	x30	=	2;
if	0	le	rs1441264	lt	0.5	then	x31	=	0;	if	0.5	le	rs1441264	lt	1.5	then	x31	=	1;	if	1.5	le	rs1441264	le	2	then	x31	=	2;
if	0	le	rs1460676	lt	0.5	then	x32	=	0;	if	0.5	le	rs1460676	lt	1.5	then	x32	=	1;	if	1.5	le	rs1460676	le	2	then	x32	=	2;
if	0	le	rs4234589	lt	0.5	then	x33	=	0;	if	0.5	le	rs4234589	lt	1.5	then	x33	=	1;	if	1.5	le	rs4234589	le	2	then	x33	=	2;
if	0	le	rs1528435	lt	0.5	then	x34	=	0;	if	0.5	le	rs1528435	lt	1.5	then	x34	=	1;	if	1.5	le	rs1528435	le	2	then	x34	=	2;
if	0	le	rs1421085	lt	0.5	then	x35	=	0;	if	0.5	le	rs1421085	lt	1.5	then	x35	=	1;	if	1.5	le	rs1421085	le	2	then	x35	=	2;
if	0	le	rs2035935	lt	0.5	then	x36	=	0;	if	0.5	le	rs2035935	lt	1.5	then	x36	=	1;	if	1.5	le	rs2035935	le	2	then	x36	=	2;
if	0	le	rs16907751	lt	0.5	then	x37	=	0;	if	0.5	le	rs16907751	lt	1.5	then	x37	=	1;	if	1.5	le	rs16907751	le	2	then	x37	=	2;
if	0	le	rs2241420	lt	0.5	then	x38	=	0;	if	0.5	le	rs2241420	lt	1.5	then	x38	=	1;	if	1.5	le	rs2241420	le	2	then	x38	=	2;
if	0	le	rs17001561	lt	0.5	then	x39	=	0;	if	0.5	le	rs17001561	lt	1.5	then	x39	=	1;	if	1.5	le	rs17001561	le	2	then	x39	=	2;
if	0	le	rs17024393	lt	0.5	then	x40	=	0;	if	0.5	le	rs17024393	lt	1.5	then	x40	=	1;	if	1.5	le	rs17024393	le	2	then	x40	=	2;
if	0	le	rs17094222	lt	0.5	then	x41	=	0;	if	0.5	le	rs17094222	lt	1.5	then	x41	=	1;	if	1.5	le	rs17094222	le	2	then	x41	=	2;
if	0	le	rs17203016	lt	0.5	then	x42	=	0;	if	0.5	le	rs17203016	lt	1.5	then	x42	=	1;	if	1.5	le	rs17203016	le	2	then	x42	=	2;
if	0	le	rs17405819	lt	0.5	then	x43	=	0;	if	0.5	le	rs17405819	lt	1.5	then	x43	=	1;	if	1.5	le	rs17405819	le	2	then	x43	=	2;
if	0	le	rs17724992	lt	0.5	then	x44	=	0;	if	0.5	le	rs17724992	lt	1.5	then	x44	=	1;	if	1.5	le	rs17724992	le	2	then	x44	=	2;
if	0	le	rs1808579	lt	0.5	then	x45	=	0;	if	0.5	le	rs1808579	lt	1.5	then	x45	=	1;	if	1.5	le	rs1808579	le	2	then	x45	=	2;
if	0	le	rs1928295	lt	0.5	then	x46	=	0;	if	0.5	le	rs1928295	lt	1.5	then	x46	=	1;	if	1.5	le	rs1928295	le	2	then	x46	=	2;
if	0	le	rs2033529	lt	0.5	then	x47	=	0;	if	0.5	le	rs2033529	lt	1.5	then	x47	=	1;	if	1.5	le	rs2033529	le	2	then	x47	=	2;
if	0	le	rs2033732	lt	0.5	then	x48	=	0;	if	0.5	le	rs2033732	lt	1.5	then	x48	=	1;	if	1.5	le	rs2033732	le	2	then	x48	=	2;
if	0	le	rs205262	lt	0.5	then	x49	=	0;	if	0.5	le	rs205262	lt	1.5	then	x49	=	1;	if	1.5	le	rs205262	le	2	then	x49	=	2;
if	0	le	rs2075650	lt	0.5	then	x50	=	0;	if	0.5	le	rs2075650	lt	1.5	then	x50	=	1;	if	1.5	le	rs2075650	le	2	then	x50	=	2;
if	0	le	rs2080454	lt	0.5	then	x51	=	0;	if	0.5	le	rs2080454	lt	1.5	then	x51	=	1;	if	1.5	le	rs2080454	le	2	then	x51	=	2;
if	0	le	rs2112347	lt	0.5	then	x52	=	0;	if	0.5	le	rs2112347	lt	1.5	then	x52	=	1;	if	1.5	le	rs2112347	le	2	then	x52	=	2;
if	0	le	rs2121279	lt	0.5	then	x53	=	0;	if	0.5	le	rs2121279	lt	1.5	then	x53	=	1;	if	1.5	le	rs2121279	le	2	then	x53	=	2;
if	0	le	rs2972143	lt	0.5	then	x54	=	0;	if	0.5	le	rs2972143	lt	1.5	then	x54	=	1;	if	1.5	le	rs2972143	le	2	then	x54	=	2;
if	0	le	rs2176598	lt	0.5	then	x55	=	0;	if	0.5	le	rs2176598	lt	1.5	then	x55	=	1;	if	1.5	le	rs2176598	le	2	then	x55	=	2;
if	0	le	rs734597	lt	0.5	then	x56	=	0;	if	0.5	le	rs734597	lt	1.5	then	x56	=	1;	if	1.5	le	rs734597	le	2	then	x56	=	2;
if	0	le	rs2245368	lt	0.5	then	x57	=	0;	if	0.5	le	rs2245368	lt	1.5	then	x57	=	1;	if	1.5	le	rs2245368	le	2	then	x57	=	2;
if	0	le	rs2287019	lt	0.5	then	x58	=	0;	if	0.5	le	rs2287019	lt	1.5	then	x58	=	1;	if	1.5	le	rs2287019	le	2	then	x58	=	2;
if	0	le	rs2365389	lt	0.5	then	x59	=	0;	if	0.5	le	rs2365389	lt	1.5	then	x59	=	1;	if	1.5	le	rs2365389	le	2	then	x59	=	2;
if	0	le	rs2650492	lt	0.5	then	x60	=	0;	if	0.5	le	rs2650492	lt	1.5	then	x60	=	1;	if	1.5	le	rs2650492	le	2	then	x60	=	2;
if	0	le	rs2820292	lt	0.5	then	x61	=	0;	if	0.5	le	rs2820292	lt	1.5	then	x61	=	1;	if	1.5	le	rs2820292	le	2	then	x61	=	2;
if	0	le	rs2836754	lt	0.5	then	x62	=	0;	if	0.5	le	rs2836754	lt	1.5	then	x62	=	1;	if	1.5	le	rs2836754	le	2	then	x62	=	2;
if	0	le	rs29941		lt	0.5	then	x63	=	0;	if	0.5	le	rs29941		lt	1.5	then	x63	=	1;	if	1.5	le	rs29941		le	2	then	x63	=	2;
if	0	le	rs3101336	lt	0.5	then	x64	=	0;	if	0.5	le	rs3101336	lt	1.5	then	x64	=	1;	if	1.5	le	rs3101336	le	2	then	x64	=	2;
if	0	le	rs3736485	lt	0.5	then	x65	=	0;	if	0.5	le	rs3736485	lt	1.5	then	x65	=	1;	if	1.5	le	rs3736485	le	2	then	x65	=	2;
if	0	le	rs3810291	lt	0.5	then	x66	=	0;	if	0.5	le	rs3810291	lt	1.5	then	x66	=	1;	if	1.5	le	rs3810291	le	2	then	x66	=	2;
if	0	le	rs3817334	lt	0.5	then	x67	=	0;	if	0.5	le	rs3817334	lt	1.5	then	x67	=	1;	if	1.5	le	rs3817334	le	2	then	x67	=	2;
if	0	le	rs3849570	lt	0.5	then	x68	=	0;	if	0.5	le	rs3849570	lt	1.5	then	x68	=	1;	if	1.5	le	rs3849570	le	2	then	x68	=	2;
if	0	le	rs3888190	lt	0.5	then	x69	=	0;	if	0.5	le	rs3888190	lt	1.5	then	x69	=	1;	if	1.5	le	rs3888190	le	2	then	x69	=	2;
if	0	le	rs7113874	lt	0.5	then	x70	=	0;	if	0.5	le	rs7113874	lt	1.5	then	x70	=	1;	if	1.5	le	rs7113874	le	2	then	x70	=	2;
if	0	le	rs4740619	lt	0.5	then	x71	=	0;	if	0.5	le	rs4740619	lt	1.5	then	x71	=	1;	if	1.5	le	rs4740619	le	2	then	x71	=	2;
if	0	le	rs4787491	lt	0.5	then	x72	=	0;	if	0.5	le	rs4787491	lt	1.5	then	x72	=	1;	if	1.5	le	rs4787491	le	2	then	x72	=	2;
if	0	le	rs492400	lt	0.5	then	x73	=	0;	if	0.5	le	rs492400	lt	1.5	then	x73	=	1;	if	1.5	le	rs492400	le	2	then	x73	=	2;
if	0	le	rs543874	lt	0.5	then	x74	=	0;	if	0.5	le	rs543874	lt	1.5	then	x74	=	1;	if	1.5	le	rs543874	le	2	then	x74	=	2;
if	0	le	rs6091540	lt	0.5	then	x75	=	0;	if	0.5	le	rs6091540	lt	1.5	then	x75	=	1;	if	1.5	le	rs6091540	le	2	then	x75	=	2;
if	0	le	rs6465468	lt	0.5	then	x76	=	0;	if	0.5	le	rs6465468	lt	1.5	then	x76	=	1;	if	1.5	le	rs6465468	le	2	then	x76	=	2;
if	0	le	rs6477694	lt	0.5	then	x77	=	0;	if	0.5	le	rs6477694	lt	1.5	then	x77	=	1;	if	1.5	le	rs6477694	le	2	then	x77	=	2;
if	0	le	rs6567160	lt	0.5	then	x78	=	0;	if	0.5	le	rs6567160	lt	1.5	then	x78	=	1;	if	1.5	le	rs6567160	le	2	then	x78	=	2;
if	0	le	rs657452	lt	0.5	then	x79	=	0;	if	0.5	le	rs657452	lt	1.5	then	x79	=	1;	if	1.5	le	rs657452	le	2	then	x79	=	2;
if	0	le	rs6804842	lt	0.5	then	x80	=	0;	if	0.5	le	rs6804842	lt	1.5	then	x80	=	1;	if	1.5	le	rs6804842	le	2	then	x80	=	2;
if	0	le	rs7138803	lt	0.5	then	x81	=	0;	if	0.5	le	rs7138803	lt	1.5	then	x81	=	1;	if	1.5	le	rs7138803	le	2	then	x81	=	2;
if	0	le	rs7141420	lt	0.5	then	x82	=	0;	if	0.5	le	rs7141420	lt	1.5	then	x82	=	1;	if	1.5	le	rs7141420	le	2	then	x82	=	2;
if	0	le	rs7164727	lt	0.5	then	x83	=	0;	if	0.5	le	rs7164727	lt	1.5	then	x83	=	1;	if	1.5	le	rs7164727	le	2	then	x83	=	2;
if	0	le	rs7239883	lt	0.5	then	x84	=	0;	if	0.5	le	rs7239883	lt	1.5	then	x84	=	1;	if	1.5	le	rs7239883	le	2	then	x84	=	2;
if	0	le	rs7243357	lt	0.5	then	x85	=	0;	if	0.5	le	rs7243357	lt	1.5	then	x85	=	1;	if	1.5	le	rs7243357	le	2	then	x85	=	2;
if	0	le	rs758747	lt	0.5	then	x86	=	0;	if	0.5	le	rs758747	lt	1.5	then	x86	=	1;	if	1.5	le	rs758747	le	2	then	x86	=	2;
if	0	le	rs7599312	lt	0.5	then	x87	=	0;	if	0.5	le	rs7599312	lt	1.5	then	x87	=	1;	if	1.5	le	rs7599312	le	2	then	x87	=	2;
if	0	le	rs7715256	lt	0.5	then	x88	=	0;	if	0.5	le	rs7715256	lt	1.5	then	x88	=	1;	if	1.5	le	rs7715256	le	2	then	x88	=	2;
if	0	le	rs7899106	lt	0.5	then	x89	=	0;	if	0.5	le	rs7899106	lt	1.5	then	x89	=	1;	if	1.5	le	rs7899106	le	2	then	x89	=	2;
if	0	le	rs7903146b	lt	0.5	then	x90	=	0;	if	0.5	le	rs7903146b	lt	1.5	then	x90	=	1;	if	1.5	le	rs7903146b	le	2	then	x90	=	2;
if	0	le	rs9374842	lt	0.5	then	x91	=	0;	if	0.5	le	rs9374842	lt	1.5	then	x91	=	1;	if	1.5	le	rs9374842	le	2	then	x91	=	2;
if	0	le	rs9400239	lt	0.5	then	x92	=	0;	if	0.5	le	rs9400239	lt	1.5	then	x92	=	1;	if	1.5	le	rs9400239	le	2	then	x92	=	2;
if	0	le	rs9540493	lt	0.5	then	x93	=	0;	if	0.5	le	rs9540493	lt	1.5	then	x93	=	1;	if	1.5	le	rs9540493	le	2	then	x93	=	2;
if	0	le	rs5014937	lt	0.5	then	x94	=	0;	if	0.5	le	rs5014937	lt	1.5	then	x94	=	1;	if	1.5	le	rs5014937	le	2	then	x94	=	2;
if	0	le	rs977747	lt	0.5	then	x95	=	0;	if	0.5	le	rs977747	lt	1.5	then	x95	=	1;	if	1.5	le	rs977747	le	2	then	x95	=	2;
if	0	le	rs9914578	lt	0.5	then	x96	=	0;	if	0.5	le	rs9914578	lt	1.5	then	x96	=	1;	if	1.5	le	rs9914578	le	2	then	x96	=	2;
if	0	le	rs9925964	lt	0.5	then	x97	=	0;	if	0.5	le	rs9925964	lt	1.5	then	x97	=	1;	if	1.5	le	rs9925964	le	2	then	x97	=	2;

if 0 le rs1260326 lt 0.5 then xa1 = 0; if 0.5 le rs1260326 lt 1.5 then xa1 = 1; if 1.5 le rs1260326 le 2 then xa1 = 2;
if 0 le rs11717195t lt 0.5 then xa2 = 0; if 0.5 le rs11717195t lt 1.5 then xa2 = 1; if 1.5 le rs11717195t le 2 then xa2 = 2;
if 0 le rs12255372 lt 0.5 then xa3 = 0; if 0.5 le rs12255372 lt 1.5 then xa3 = 1; if 1.5 le rs12255372 le 2 then xa3 = 2;
if 0 le rs1436958 lt 0.5 then xa4 = 0; if 0.5 le rs1436958 lt 1.5 then xa4 = 1; if 1.5 le rs1436958 le 2 then xa4 = 2;
if 0 le rs11672660 lt 0.5 then xa5 = 0; if 0.5 le rs11672660 lt 1.5 then xa5 = 1; if 1.5 le rs11672660 le 2 then xa5 = 2;
if 0 le rs6975024 lt 0.5 then xa6 = 0; if 0.5 le rs6975024 lt 1.5 then xa6 = 1; if 1.5 le rs6975024 le 2 then xa6 = 2;
if 0 le rs11782386 lt 0.5 then xa7 = 0; if 0.5 le rs11782386 lt 1.5 then xa7 = 1; if 1.5 le rs11782386 le 2 then xa7 = 2;
if 0 le rs1019503 lt 0.5 then xa8 = 0; if 0.5 le rs1019503 lt 1.5 then xa8 = 1; if 1.5 le rs1019503 le 2 then xa8 = 2;
if 0 le rs7651090t lt 0.5 then xa9 = 0; if 0.5 le rs7651090t lt 1.5 then xa9 = 1; if 1.5 le rs7651090t le 2 then xa9 = 2;

if 0 le rs10811661f lt 0.5 then xb1 = 0; if 0.5 le rs10811661f lt 1.5 then xb1 = 1; if 1.5 le rs10811661f le 2 then xb1 = 2;
if 0 le rs4869272 lt 0.5 then xb2 = 0; if 0.5 le rs4869272 lt 1.5 then xb2 = 1; if 1.5 le rs4869272 le 2 then xb2 = 2;
if 0 le rs11619319 lt 0.5 then xb3 = 0; if 0.5 le rs11619319 lt 1.5 then xb3 = 1; if 1.5 le rs11619319 le 2 then xb3 = 2;
if 0 le rs983309 lt 0.5 then xb4 = 0; if 0.5 le rs983309 lt 1.5 then xb4 = 1; if 1.5 le rs983309 le 2 then xb4 = 2;
if 0 le rs6943153 lt 0.5 then xb5 = 0; if 0.5 le rs6943153 lt 1.5 then xb5 = 1; if 1.5 le rs6943153 le 2 then xb5 = 2;
if 0 le rs11603334 lt 0.5 then xb6 = 0; if 0.5 le rs11603334 lt 1.5 then xb6 = 1; if 1.5 le rs11603334 le 2 then xb6 = 2;
if 0 le rs6113722 lt 0.5 then xb7 = 0; if 0.5 le rs6113722 lt 1.5 then xb7 = 1; if 1.5 le rs6113722 le 2 then xb7 = 2;
if 0 le rs16913693 lt 0.5 then xb8 = 0; if 0.5 le rs16913693 lt 1.5 then xb8 = 1; if 1.5 le rs16913693 le 2 then xb8 = 2;
if 0 le rs3829109 lt 0.5 then xb9 = 0; if 0.5 le rs3829109 lt 1.5 then xb9 = 1; if 1.5 le rs3829109 le 2 then xb9 = 2;
if 0 le rs3783347 lt 0.5 then xb10 = 0; if 0.5 le rs3783347 lt 1.5 then xb10 = 1; if 1.5 le rs3783347 le 2 then xb10 = 2;
if 0 le rs2302593 lt 0.5 then xb11 = 0; if 0.5 le rs2302593 lt 1.5 then xb11 = 1; if 1.5 le rs2302593 le 2 then xb11 = 2;
if 0 le rs9368222 lt 0.5 then xb12 = 0; if 0.5 le rs9368222 lt 1.5 then xb12 = 1; if 1.5 le rs9368222 le 2 then xb12 = 2;
if 0 le rs10747083 lt 0.5 then xb13 = 0; if 0.5 le rs10747083 lt 1.5 then xb13 = 1; if 1.5 le rs10747083 le 2 then xb13 = 2;
if 0 le rs6072275 lt 0.5 then xb14 = 0; if 0.5 le rs6072275 lt 1.5 then xb14 = 1; if 1.5 le rs6072275 le 2 then xb14 = 2;
if 0 le rs7651090f lt 0.5 then xb15 = 0; if 0.5 le rs7651090f lt 1.5 then xb15 = 1; if 1.5 le rs7651090f le 2 then xb15 = 2;
if 0 le rs576674 lt 0.5 then xb16 = 0; if 0.5 le rs576674 lt 1.5 then xb16 = 1; if 1.5 le rs576674 le 2 then xb16 = 2;
if 0 le rs11715915 lt 0.5 then xb17 = 0; if 0.5 le rs11715915 lt 1.5 then xb17 = 1; if 1.5 le rs11715915 le 2 then xb17 = 2;
if 0 le rs17762454 lt 0.5 then xb18 = 0; if 0.5 le rs17762454 lt 1.5 then xb18 = 1; if 1.5 le rs17762454 le 2 then xb18 = 2;
if 0 le rs7708285 lt 0.5 then xb19 = 0; if 0.5 le rs7708285 lt 1.5 then xb19 = 1; if 1.5 le rs7708285 le 2 then xb19 = 2;
if 0 le rs2657879 lt 0.5 then xb20 = 0; if 0.5 le rs2657879 lt 1.5 then xb20 = 1; if 1.5 le rs2657879 le 2 then xb20 = 2;
if 0 le rs340874f lt 0.5 then xb21 = 0; if 0.5 le rs340874f lt 1.5 then xb21 = 1; if 1.5 le rs340874f le 2 then xb21 = 2;
if 0 le rs780094f lt 0.5 then xb22 = 0; if 0.5 le rs780094f lt 1.5 then xb22 = 1; if 1.5 le rs780094f le 2 then xb22 = 2;
if 0 le rs560887 lt 0.5 then xb23 = 0; if 0.5 le rs560887 lt 1.5 then xb23 = 1; if 1.5 le rs560887 le 2 then xb23 = 2;
if 0 le rs11708067 lt 0.5 then xb24 = 0; if 0.5 le rs11708067 lt 1.5 then xb24 = 1; if 1.5 le rs11708067 le 2 then xb24 = 2;
if 0 le rs1280 lt 0.5 then xb25 = 0; if 0.5 le rs1280 lt 1.5 then xb25 = 1; if 1.5 le rs1280 le 2 then xb25 = 2;
if 0 le rs2191349 lt 0.5 then xb26 = 0; if 0.5 le rs2191349 lt 1.5 then xb26 = 1; if 1.5 le rs2191349 le 2 then xb26 = 2;
if 0 le rs2908289 lt 0.5 then xb27 = 0; if 0.5 le rs2908289 lt 1.5 then xb27 = 1; if 1.5 le rs2908289 le 2 then xb27 = 2;
if 0 le rs11558471 lt 0.5 then xb28 = 0; if 0.5 le rs11558471 lt 1.5 then xb28 = 1; if 1.5 le rs11558471 le 2 then xb28 = 2;
if 0 le rs10814916 lt 0.5 then xb29 = 0; if 0.5 le rs10814916 lt 1.5 then xb29 = 1; if 1.5 le rs10814916 le 2 then xb29 = 2;
if 0 le rs11195502 lt 0.5 then xb30 = 0; if 0.5 le rs11195502 lt 1.5 then xb30 = 1; if 1.5 le rs11195502 le 2 then xb30 = 2;
if 0 le rs7903146f lt 0.5 then xb31 = 0; if 0.5 le rs7903146f lt 1.5 then xb31 = 1; if 1.5 le rs7903146f le 2 then xb31 = 2;
if 0 le rs11607883 lt 0.5 then xb32 = 0; if 0.5 le rs11607883 lt 1.5 then xb32 = 1; if 1.5 le rs11607883 le 2 then xb32 = 2;
if 0 le rs11039182 lt 0.5 then xb33 = 0; if 0.5 le rs11039182 lt 1.5 then xb33 = 1; if 1.5 le rs11039182 le 2 then xb33 = 2;
if 0 le rs174576 lt 0.5 then xb34 = 0; if 0.5 le rs174576 lt 1.5 then xb34 = 1; if 1.5 le rs174576 le 2 then xb34 = 2;
if 0 le rs10830963f lt 0.5 then xb35 = 0; if 0.5 le rs10830963f lt 1.5 then xb35 = 1; if 1.5 le rs10830963f le 2 then xb35 = 2;
if 0 le rs4502156f lt 0.5 then xb36 = 0; if 0.5 le rs4502156f lt 1.5 then xb36 = 1; if 1.5 le rs4502156f le 2 then xb36 = 2;

if 0 le rs340874d lt 0.5 then xc1 = 0; if 0.5 le rs340874d lt 1.5 then xc1 = 1; if 1.5 le rs340874d le 2 then xc1 = 2;
if 0 le rs7515431 lt 0.5 then xc2 = 0; if 0.5 le rs7515431 lt 1.5 then xc2 = 1; if 1.5 le rs7515431 le 2 then xc2 = 2;
if 0 le rs7903146d lt 0.5 then xc3 = 0; if 0.5 le rs7903146d lt 1.5 then xc3 = 1; if 1.5 le rs7903146d le 2 then xc3 = 2;
if 0 le rs12571751 lt 0.5 then xc4 = 0; if 0.5 le rs12571751 lt 1.5 then xc4 = 1; if 1.5 le rs12571751 le 2 then xc4 = 2;
if 0 le rs12242953 lt 0.5 then xc5 = 0; if 0.5 le rs12242953 lt 1.5 then xc5 = 1; if 1.5 le rs12242953 le 2 then xc5 = 2;
if 0 le rs11257655 lt 0.5 then xc6 = 0; if 0.5 le rs11257655 lt 1.5 then xc6 = 1; if 1.5 le rs11257655 le 2 then xc6 = 2;
if 0 le rs1111875 lt 0.5 then xc7 = 0; if 0.5 le rs1111875 lt 1.5 then xc7 = 1; if 1.5 le rs1111875 le 2 then xc7 = 2;
if 0 le rs5215 lt 0.5 then xc8 = 0; if 0.5 le rs5215 lt 1.5 then xc8 = 1; if 1.5 le rs5215 le 2 then xc8 = 2;
if 0 le rs2334499 lt 0.5 then xc9 = 0; if 0.5 le rs2334499 lt 1.5 then xc9 = 1; if 1.5 le rs2334499 le 2 then xc9 = 2;
if 0 le rs163184 lt 0.5 then xc10 = 0; if 0.5 le rs163184 lt 1.5 then xc10 = 1; if 1.5 le rs163184 le 2 then xc10 = 2;
if 0 le rs1552224 lt 0.5 then xc11 = 0; if 0.5 le rs1552224 lt 1.5 then xc11 = 1; if 1.5 le rs1552224 le 2 then xc11 = 2;
if 0 le rs10830963d lt 0.5 then xc12 = 0; if 0.5 le rs10830963d lt 1.5 then xc12 = 1; if 1.5 le rs10830963d le 2 then xc12 = 2;
if 0 le rs7955901 lt 0.5 then xc13 = 0; if 0.5 le rs7955901 lt 1.5 then xc13 = 1; if 1.5 le rs7955901 le 2 then xc13 = 2;
if 0 le rs2261181 lt 0.5 then xc14 = 0; if 0.5 le rs2261181 lt 1.5 then xc14 = 1; if 1.5 le rs2261181 le 2 then xc14 = 2;
if 0 le rs7965349 lt 0.5 then xc15 = 0; if 0.5 le rs7965349 lt 1.5 then xc15 = 1; if 1.5 le rs7965349 le 2 then xc15 = 2;
if 0 le rs11063069 lt 0.5 then xc16 = 0; if 0.5 le rs11063069 lt 1.5 then xc16 = 1; if 1.5 le rs11063069 le 2 then xc16 = 2;
if 0 le rs10842994 lt 0.5 then xc17 = 0; if 0.5 le rs10842994 lt 1.5 then xc17 = 1; if 1.5 le rs10842994 le 2 then xc17 = 2;
if 0 le rs1359790 lt 0.5 then xc18 = 0; if 0.5 le rs1359790 lt 1.5 then xc18 = 1; if 1.5 le rs1359790 le 2 then xc18 = 2;
if 0 le rs7177055 lt 0.5 then xc19 = 0; if 0.5 le rs7177055 lt 1.5 then xc19 = 1; if 1.5 le rs7177055 le 2 then xc19 = 2;
if 0 le rs4502156d lt 0.5 then xc20 = 0; if 0.5 le rs4502156d lt 1.5 then xc20 = 1; if 1.5 le rs4502156d le 2 then xc20 = 2;
if 0 le rs2007084 lt 0.5 then xc21 = 0; if 0.5 le rs2007084 lt 1.5 then xc21 = 1; if 1.5 le rs2007084 le 2 then xc21 = 2;
if 0 le rs12899811 lt 0.5 then xc22 = 0; if 0.5 le rs12899811 lt 1.5 then xc22 = 1; if 1.5 le rs12899811 le 2 then xc22 = 2;
if 0 le rs11634397 lt 0.5 then xc23 = 0; if 0.5 le rs11634397 lt 1.5 then xc23 = 1; if 1.5 le rs11634397 le 2 then xc23 = 2;
if 0 le rs9923233 lt 0.5 then xc24 = 0; if 0.5 le rs9923233 lt 1.5 then xc24 = 1; if 1.5 le rs9923233 le 2 then xc24 = 2;
if 0 le rs7202877 lt 0.5 then xc25 = 0; if 0.5 le rs7202877 lt 1.5 then xc25 = 1; if 1.5 le rs7202877 le 2 then xc25 = 2;
if 0 le rs11651755 lt 0.5 then xc26 = 0; if 0.5 le rs11651755 lt 1.5 then xc26 = 1; if 1.5 le rs11651755 le 2 then xc26 = 2;
if 0 le rs2447090 lt 0.5 then xc27 = 0; if 0.5 le rs2447090 lt 1.5 then xc27 = 1; if 1.5 le rs2447090 le 2 then xc27 = 2;
if 0 le rs11663816 lt 0.5 then xc28 = 0; if 0.5 le rs11663816 lt 1.5 then xc28 = 1; if 1.5 le rs11663816 le 2 then xc28 = 2;
if 0 le rs8182584 lt 0.5 then xc29 = 0; if 0.5 le rs8182584 lt 1.5 then xc29 = 1; if 1.5 le rs8182584 le 2 then xc29 = 2;
if 0 le rs8108269 lt 0.5 then xc30 = 0; if 0.5 le rs8108269 lt 1.5 then xc30 = 1; if 1.5 le rs8108269 le 2 then xc30 = 2;
if 0 le rs10401969 lt 0.5 then xc31 = 0; if 0.5 le rs10401969 lt 1.5 then xc31 = 1; if 1.5 le rs10401969 le 2 then xc31 = 2;
if 0 le rs780094d lt 0.5 then xc32 = 0; if 0.5 le rs780094d lt 1.5 then xc32 = 1; if 1.5 le rs780094d le 2 then xc32 = 2;
if 0 le rs7569522 lt 0.5 then xc33 = 0; if 0.5 le rs7569522 lt 1.5 then xc33 = 1; if 1.5 le rs7569522 le 2 then xc33 = 2;
if 0 le rs2943640 lt 0.5 then xc34 = 0; if 0.5 le rs2943640 lt 1.5 then xc34 = 1; if 1.5 le rs2943640 le 2 then xc34 = 2;
if 0 le rs243083 lt 0.5 then xc35 = 0; if 0.5 le rs243083 lt 1.5 then xc35 = 1; if 1.5 le rs243083 le 2 then xc35 = 2;
if 0 le rs1128249 lt 0.5 then xc36 = 0; if 0.5 le rs1128249 lt 1.5 then xc36 = 1; if 1.5 le rs1128249 le 2 then xc36 = 2;
if 0 le rs10203174 lt 0.5 then xc37 = 0; if 0.5 le rs10203174 lt 1.5 then xc37 = 1; if 1.5 le rs10203174 le 2 then xc37 = 2;
if 0 le rs4812829 lt 0.5 then xc38 = 0; if 0.5 le rs4812829 lt 1.5 then xc38 = 1; if 1.5 le rs4812829 le 2 then xc38 = 2;
if 0 le rs6795735 lt 0.5 then xc39 = 0; if 0.5 le rs6795735 lt 1.5 then xc39 = 1; if 1.5 le rs6795735 le 2 then xc39 = 2;
if 0 le rs4402960 lt 0.5 then xc40 = 0; if 0.5 le rs4402960 lt 1.5 then xc40 = 1; if 1.5 le rs4402960 le 2 then xc40 = 2;
if 0 le rs2197423 lt 0.5 then xc41 = 0; if 0.5 le rs2197423 lt 1.5 then xc41 = 1; if 1.5 le rs2197423 le 2 then xc41 = 2;
if 0 le rs17301514 lt 0.5 then xc42 = 0; if 0.5 le rs17301514 lt 1.5 then xc42 = 1; if 1.5 le rs17301514 le 2 then xc42 = 2;
if 0 le rs1496653 lt 0.5 then xc43 = 0; if 0.5 le rs1496653 lt 1.5 then xc43 = 1; if 1.5 le rs1496653 le 2 then xc43 = 2;
if 0 le rs12497268 lt 0.5 then xc44 = 0; if 0.5 le rs12497268 lt 1.5 then xc44 = 1; if 1.5 le rs12497268 le 2 then xc44 = 2;
if 0 le rs11717195d lt 0.5 then xc45 = 0; if 0.5 le rs11717195d lt 1.5 then xc45 = 1; if 1.5 le rs11717195d le 2 then xc45 = 2;
if 0 le rs6819243 lt 0.5 then xc46 = 0; if 0.5 le rs6819243 lt 1.5 then xc46 = 1; if 1.5 le rs6819243 le 2 then xc46 = 2;
if 0 le rs10012946 lt 0.5 then xc47 = 0; if 0.5 le rs10012946 lt 1.5 then xc47 = 1; if 1.5 le rs10012946 le 2 then xc47 = 2;
if 0 le rs6878122 lt 0.5 then xc48 = 0; if 0.5 le rs6878122 lt 1.5 then xc48 = 1; if 1.5 le rs6878122 le 2 then xc48 = 2;
if 0 le rs459193 lt 0.5 then xc49 = 0; if 0.5 le rs459193 lt 1.5 then xc49 = 1; if 1.5 le rs459193 le 2 then xc49 = 2;
if 0 le rs7756992 lt 0.5 then xc50 = 0; if 0.5 le rs7756992 lt 1.5 then xc50 = 1; if 1.5 le rs7756992 le 2 then xc50 = 2;
if 0 le rs4299828 lt 0.5 then xc51 = 0; if 0.5 le rs4299828 lt 1.5 then xc51 = 1; if 1.5 le rs4299828 le 2 then xc51 = 2;
if 0 le rs3734621 lt 0.5 then xc52 = 0; if 0.5 le rs3734621 lt 1.5 then xc52 = 1; if 1.5 le rs3734621 le 2 then xc52 = 2;
if 0 le rs849135 lt 0.5 then xc53 = 0; if 0.5 le rs849135 lt 1.5 then xc53 = 1; if 1.5 le rs849135 le 2 then xc53 = 2;
if 0 le rs10229583 lt 0.5 then xc54 = 0; if 0.5 le rs10229583 lt 1.5 then xc54 = 1; if 1.5 le rs10229583 le 2 then xc54 = 2;
if 0 le rs17168486 lt 0.5 then xc55 = 0; if 0.5 le rs17168486 lt 1.5 then xc55 = 1; if 1.5 le rs17168486 le 2 then xc55 = 2;
if 0 le rs13233731 lt 0.5 then xc56 = 0; if 0.5 le rs13233731 lt 1.5 then xc56 = 1; if 1.5 le rs13233731 le 2 then xc56 = 2;
if 0 le rs3824065 lt 0.5 then xc57 = 0; if 0.5 le rs3824065 lt 1.5 then xc57 = 1; if 1.5 le rs3824065 le 2 then xc57 = 2;
if 0 le rs7845219 lt 0.5 then xc58 = 0; if 0.5 le rs7845219 lt 1.5 then xc58 = 1; if 1.5 le rs7845219 le 2 then xc58 = 2;
if 0 le rs516946 lt 0.5 then xc59 = 0; if 0.5 le rs516946 lt 1.5 then xc59 = 1; if 1.5 le rs516946 le 2 then xc59 = 2;
if 0 le rs3802177 lt 0.5 then xc60 = 0; if 0.5 le rs3802177 lt 1.5 then xc60 = 1; if 1.5 le rs3802177 le 2 then xc60 = 2;
if 0 le rs2796441 lt 0.5 then xc61 = 0; if 0.5 le rs2796441 lt 1.5 then xc61 = 1; if 1.5 le rs2796441 le 2 then xc61 = 2;
if 0 le rs17791513 lt 0.5 then xc62 = 0; if 0.5 le rs17791513 lt 1.5 then xc62 = 1; if 1.5 le rs17791513 le 2 then xc62 = 2;
if 0 le rs16927668 lt 0.5 then xc63 = 0; if 0.5 le rs16927668 lt 1.5 then xc63 = 1; if 1.5 le rs16927668 le 2 then xc63 = 2;
if 0 le rs10811661d lt 0.5 then xc64 = 0; if 0.5 le rs10811661d lt 1.5 then xc64 = 1; if 1.5 le rs10811661d le 2 then xc64 = 2;
if 0 le rs10758593 lt 0.5 then xc65 = 0; if 0.5 le rs10758593 lt 1.5 then xc65 = 1; if 1.5 le rs10758593 le 2 then xc65 = 2;

/*construct the genetic risk score for BMI*/
ob_GRS = .;   ob_GRS = SUM (of x1-x97);
if ob_GRS gt 0;

/*construct the genetic risk score for 2glu*/
tglu_GRS = .;   tglu_GRS = SUM (of xa1-xa9);
if tglu_GRS gt 0;

/*construct the genetic risk score for fglu*/
fglu_GRS = .;   fglu_GRS = SUM (of xb1-xb36);
if fglu_GRS gt 0;

/*construct the genetic risk score for t2d*/
d_GRS = .;   d_GRS = SUM (of xc1-xc65);
if d_GRS gt 0;

/*construct the multi-trait GRS*/

m_GRS1 = .; m_GRS1= SUM (of x1-x89);
m_GRS2 = .; m_GRS2= SUM (of x91-x97);
m_GRS3 = .; m_GRS3= xa1;
m_GRS4 = .; m_GRS4= SUM (of xa3-xa8);
m_GRS5 = .; m_GRS5= SUM (of xb2-xb20);
m_GRS6 = .; m_GRS6= SUM (of xb23-xb30);
m_GRS7 = .; m_GRS7= SUM (of xb32-xb34);
m_GRS8 = .; m_GRS8= SUM (of xc1-xc2);
m_GRS9 = .; m_GRS9= SUM (of xc4-xc65);


m_GRS= .; m_GRS= SUM (of m_GRS1-m_GRS9);
if m_GRS gt 0;


KEEP id fasta fglu1 fglu2 twoglu1 twoglu2 DATUM1 DATUM2 age1 age2 weight1 AGESQ01 AGESQ02 sex enk1 enk2 BMI1 SMOKE1 EDUCATION1 QT_alc1 QT_alc2 PHYSACT1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 TEI1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 VitA1 VitD1 VitE1 Thia1 Ribo1 Niac1 VitB61 Folate1 VitB121 VitC1 Calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 BMI2  visit1 visit2  SMOKE2 EDUCATION2 PHYSACT2 MUFA2 PUFA2 CARBOHYDRATES2 SUGARS2 PROTEIN2 TEI2 SATFAT2 TOTFAT2 FIBER2 FA2 ALC2 SALT2 VitA2 VitD2 VitE2 Thia2 Ribo2 Niac2 VitB62 Folate2 VitB122 VitC2 Calc2 Phosp2 Pota2 Magn2 Iron2 Zinc2 Iodine2 Selenium2  
deltaBMI deltaweight lyear ob_GRS tglu_GRS fglu_GRS d_GRS m_GRS obov1 obes1 obov2 obes2 QT_alc1 QT_alc2 c1 c2 c3 c4 wholegrain1 fish1 fruit1 vegetables1 redmeat1 desserts1 sugardrink1 friedpot1 wholegrain2 fish2 fruit2 vegetables2 redmeat2 desserts2 sugardrink2 friedpot2
 C61 C62 C71 C72 fastamiss1 fasta4h1 fasta4to8h1
fastamiss2 fasta4h2 fasta4to8h2
deltafglu
deltatwoglu
FG1
FG2
GT1
GT2
IDFGincid
IDGTincid
FGworsen
GTworsen
T2D1
T2D2
T2Dincid
obesincid
rs1000940
rs10132280
rs1016287
rs10182181
rs10733682
rs10938397
rs10968576
rs7103411
rs11057405
rs11126666
rs11165643
rs11191560
rs11583200
rs1167827
rs11688816
rs11727676
rs10134820
rs1885988
rs12286929
rs12401738
rs12429545
rs12446632
rs1514175
rs12885454
rs12940622
rs13021737
rs7622475
rs13107325
rs13191362
rs13201877
rs1441264
rs1460676
rs4234589
rs1528435
rs1421085
rs2035935
rs16907751
rs2241420
rs17001561
rs17024393
rs17094222
rs17203016
rs17405819
rs17724992
rs1808579
rs1928295
rs2033529
rs2033732
rs205262
rs2075650
rs2080454
rs2112347
rs2121279
rs2972143
rs2176598
rs734597
rs2245368
rs2287019
rs2365389
rs2650492
rs2820292
rs2836754
rs29941
rs3101336
rs3736485
rs3810291
rs3817334
rs3849570
rs3888190
rs7113874
rs4740619
rs4787491
rs492400
rs543874
rs6091540
rs6465468
rs6477694
rs6567160
rs657452
rs6804842
rs7138803
rs7141420
rs7164727
rs7239883
rs7243357
rs758747
rs7599312
rs7715256
rs7899106
rs7903146b
rs9374842
rs9400239
rs9540493
rs5014937
rs977747
rs9914578
rs9925964

rs1260326
rs11717195t
rs12255372
rs1436958
rs11672660
rs6975024
rs11782386
rs1019503
rs7651090t

rs10811661f
rs4869272
rs11619319
rs983309
rs6943153
rs11603334
rs6113722
rs16913693
rs3829109
rs3783347
rs2302593
rs9368222
rs10747083
rs6072275
rs7651090f
rs576674
rs11715915
rs17762454
rs7708285
rs2657879
rs340874f
rs780094f
rs560887
rs11708067
rs1280
rs2191349
rs2908289
rs11558471
rs10814916
rs11195502
rs7903146f
rs11607883
rs11039182
rs174576
rs10830963f
rs4502156f

rs340874d
rs7515431
rs7903146d
rs12571751
rs12242953
rs11257655
rs1111875
rs5215
rs2334499
rs163184
rs1552224
rs10830963d
rs7955901
rs2261181
rs7965349
rs11063069
rs10842994
rs1359790
rs7177055
rs4502156d
rs2007084
rs12899811
rs11634397
rs9923233
rs7202877
rs11651755
rs2447090
rs11663816
rs8182584
rs8108269
rs10401969
rs780094d
rs7569522
rs2943640
rs243083
rs1128249
rs10203174
rs4812829
rs6795735
rs4402960
rs2197423
rs17301514
rs1496653
rs12497268
rs11717195d
rs6819243
rs10012946
rs6878122
rs459193
rs7756992
rs4299828
rs3734621
rs849135
rs10229583
rs17168486
rs13233731
rs3824065
rs7845219
rs516946
rs3802177
rs2796441
rs17791513
rs16927668
rs10811661d
rs10758593;
run; 
/******exclude individuals with lyear less than 4***/
data stuff.a8_impute;  set stuff.a8_impute; 
if lyear ge 4;
run;

/**************************************
     DESCRIPTIVE STATISTICS BASELINE 
***************************************/
data baseline1; set stuff.a8_impute; 
if visit1 eq 1; 
run; 
proc univariate data=baseline1; var bmi1 fglu1 twoglu1 ob_grs tglu_GRS fglu_GRS d_GRS;histogram; run;
proc freq data= baseline1; table sex smoke1 education1 physact1 obes1 obov1 FG1 GT1 FG1*GT1 T2D1 fastamiss1 fasta4h1 fasta4to8h1; run; 
proc means data=baseline1; var fglu1 twoglu1 age1 bmi1 ob_grs tglu_GRS fglu_GRS d_GRS m_GRS QT_alc1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 TEI1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 VitA1 VitD1 VitE1 Thia1 Ribo1 Niac1 VitB61 Folate1 VitB121 VitC1 Calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 wholegrain1 fish1 fruit1 vegetables1 redmeat1 desserts1 sugardrink1 friedpot1; run;

/**************************************
     DESCRIPTIVE STATISTICS FOLLOW-UP
***************************************/
data followup1; set stuff.a8_impute; 
if visit2 eq 2;
run; 
proc univariate data= followup1; var bmi1 fglu1 twoglu1 bmi2 fglu2 twoglu2 ob_grs tglu_GRS fglu_GRS d_GRS m_GRS ;histogram; run;
proc freq data= followup1; table obesincid T2Dincid IDFGincid IDGTincid enk1 sex smoke1 education1 physact1 obes1 obov1 FG1 GT1 T2D1 fastamiss1 fasta4h1 fasta4to8h1 smoke2 education2 physact2 obes2 obov2 FG2 GT2 T2D2 fastamiss2 fasta4h2 fasta4to8h2; run; 
proc means data= followup1; var lyear fglu1 twoglu1 age1 bmi1 QT_alc1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 TEI1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 VitA1 VitD1 VitE1 Thia1 Ribo1 Niac1 VitB61 Folate1 VitB121 VitC1 Calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 wholegrain1 fish1 fruit1 vegetables1 redmeat1 desserts1 sugardrink1 friedpot1 fglu2 twoglu2 lyear age2 BMI2 ob_grs tglu_GRS fglu_GRS d_GRS m_GRS QT_alc2 MUFA2 PUFA2 CARBOHYDRATES2 SUGARS2 PROTEIN2 TEI2 SATFAT2 TOTFAT2 FIBER2 FA2 ALC2 SALT2 VitA2 VitD2 VitE2 Thia2 Ribo2 Niac2 VitB62 Folate2 VitB122 VitC2 Calc2 Phosp2 Pota2 Magn2 Iron2 Zinc2 Iodine2 Selenium2 wholegrain2 fish2 fruit2 vegetables2 redmeat2 desserts2 sugardrink2 friedpot2; run; 
proc sort data= followup1; by sex; run;
proc freq data= followup1; table fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 obesincid T2Dincid IDFGincid IDGTincid; by sex; run;
proc means data= followup1; var lyear age1 fglu1 twoglu1 bmi1 ; by sex; run;
proc freq data= followup1; table smoke1 education1 physact1 QT_alc1; by sex; run;
proc means data= followup1; var MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 TEI1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 VitA1 VitD1 VitE1 Thia1 Ribo1 Niac1 VitB61 Folate1 VitB121 VitC1 Calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 wholegrain1 fish1 fruit1 vegetables1 redmeat1 desserts1 sugardrink1 friedpot1 ob_grs tglu_GRS fglu_GRS d_GRS m_GRS; by sex; run;
proc sort data= followup1; by id; run;


proc freq data= stuff.a8_impute; table obes1*obes2 obov1*obov2 obesincid; run; 
proc freq data= stuff.a8_impute; table FG1*FG2 GT1*GT2 FGworsen GTworsen T2D1*T2D2 fasta4h2 T2Dincid; run; 
proc freq data= followup1; table DATUM1 DATUM2; run;

data pr1; set followup1; keep id DATUM1 age1 DATUM2 age2; run;
proc freq data= pr1;table DATUM1 DATUM2; run;
proc sort data= pr1; by DATUM2; run;

/*comprobaciones de T2D incid*/
proc freq data= stuff.a8_impute; table T2Dincid T2Dincid*C62*C61 T2Dincid*fastamiss2 T2Dincid*fasta4h2 T2Dincid*fasta4to8h2 T2Dincid*fasta4to8h2*FG2 T2Dincid*fasta4to8h2*GT2 T2Dincid*fasta4to8h2*C62 C62*T2D2 C62*T2Dincid*C72 C61*C62*T2Dincid C71*C61; run; 
proc freq data= stuff.a8_impute; table T2D1 T2D2*fasta4to8h2 T2D1*fastamiss1 T2D1*fasta4h1 T2D1*fasta4to8h1 T2D1*fastamiss1*FG1 T2D1*fastamiss1*GT1 T2D1*fastamiss1*C61 C62*T2D2 C62*T2Dincid*C72 C61*C62*T2Dincid C71*C61; run; 
proc freq data= stuff.a8_impute; table fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2; run; 
proc freq data= stuff.a8_impute; table T2Dincid*C62 T2Dincid*C62*C61 T2Dincid*C62*C72; run; 

/*comprobaciones de IDFGincid and IDGTincid*/
proc freq data= stuff.a8_impute; table IDFGincid IDGTincid IDFGincid*IDGTincid IDFGincid*fastamiss2 IDFGincid*fasta4h2 IDFGincid*fasta4to8h2 IDFGincid*C62 IDGTincid*fastamiss2 IDGTincid*fasta4h2 IDGTincid*fasta4to8h2 IDGTincid*C62; run; 


/***************************************************
DIFFERENCES IN ANALYSED TRAITS BETWEEN BASELINE AND FOLLOW UP
******************************************************/

data diffbf; set followup1;
fasta1= .;
if visit1 eq 1 then fasta1= fasta;
fasta2= .;
if visit2 eq 2 then fasta2= fasta;
ob_grs1=.;
if visit1 eq 1 then ob_grs1= ob_grs;
ob_grs2=.;
if visit2 eq 2 then ob_grs2= ob_grs;
tglu_grs1=.;
if visit1 eq 1 then tglu_grs1= tglu_grs;
tglu_grs2=.;
if visit2 eq 2 then tglu_grs2= tglu_grs;
fglu_grs1=.;
if visit1 eq 1 then fglu_grs1= fglu_grs;
fglu_grs2=.;
if visit2 eq 2 then fglu_grs2= fglu_grs;
d_grs1=.;
if visit1 eq 1 then d_grs1= d_grs;
d_grs2=.;
if visit2 eq 2 then d_grs2= d_grs;
sex1= .;
if visit1 eq 1 then sex1= sex;
sex2= .;
if visit2 eq 2 then sex2= sex;
difffasta= fasta1-fasta2;
diffsex= sex1-sex2;
diffage= age1-age2;
diffbmi= bmi1-bmi2;
diffobov= obov1-obov2;
diffobes= obes1-obes2;
difffglu= fglu1-fglu2;
difftwoglu= twoglu1-twoglu2;
diffFG= FG1-FG2;
diffGT= GT1-GT2;
diffT2D= T2D1-T2D2;
diffob_grs= ob_grs1-ob_grs2;
difftglu_grs= tglu_grs1-tglu_grs2;
difffglu_grs= fglu_grs1-fglu_grs2;
diffd_grs= d_grs1-d_grs2;
diffsmoke= smoke1-smoke2;
diffeducation= education1-education2;
diffphysact= physact1-physact2;
difftei= tei1-tei2;
diffcarbohydrates= carbohydrates1-carbohydrates2;
diffsugars= sugars1-sugars2;
diffprotein= protein1-protein2;
difftotfat= totfat1-totfat2;
diffsatfat= satfat1-satfat2;
difffa= fa1-fa2;
diffmufa= MUFA1-MUFA2;
diffpufa= PUFA1-PUFA2;
difffiber= fiber1-fiber2;
diffalc= alc1-alc2; 
diffsalt= salt1-salt2;
diffvita= VitA1-VitA2;
diffvitd= VitD1-VitD2;
diffvite= VitE1-VitE2;
diffthia= thia1-thia2;
diffribo= ribo1-ribo2;
diffniac= niac1-niac2;
diffvitb6= vitb61-VitB62;
difffolate= folate1-folate2;
diffvitb12= vitb121-vitb122;
diffvitc= Vitc1-vitc2;
diffcalc= calc1-calc2; 
diffphosp= phosp1-phosp2;
diffpota= pota1-pota2;
diffmagn= magn1-magn2;
diffiron= iron1-iron2;
diffzinc= zinc1-zinc2;
diffiodine= iodine1-iodine2;
diffselenium= selenium1-selenium2;
diffwholegrain= wholegrain1-wholegrain2;
difffish= fish1-fish2;
difffruit= fruit1-fruit2;
diffvegetables= vegetables1-vegetables2;
diffredmeat= redmeat1-redmeat2;
diffdesserts= desserts1-desserts2;
diffsugardrink= sugardrink1-sugardrink2;
difffriedpot= friedpot1-friedpot2;
run;

proc univariate data= diffbf;
var difffasta diffsex diffage diffbmi diffobes diffobov difffglu difftwoglu diffFG diffGT diffT2D diffob_grs difftglu_grs difffglu_grs diffd_grs diffsmoke diffeducation diffphysact difftei diffcarbohydrates diffsugars diffprotein difftotfat diffsatfat difffa diffmufa diffpufa difffiber diffalc diffsalt diffvita diffvitd diffvite diffthia diffribo diffniac diffvitb6 difffolate diffvitb12 diffvitc diffcalc diffphosp diffpota diffmagn diffiron diffzinc diffiodine diffselenium diffwholegrain difffish difffruit diffvegetables diffredmeat diffdesserts diffsugardrink difffriedpot;
run;

/******************************************************
SIGNIFICANT DIFFERENCES BETWEEN: 
-OBESE AN NON-OBESE AT FOLLOW-UP (Obesity incidence)
-Becoming Diabetic (T2D incidence)
IGUAL AQUI TB METER LOS WORSENING
*******************************************************/
data sdiff; set stuff.a8_impute; run; 
proc freq data= sdiff;
tables obesincid*smoke1 obesincid*education1 obesincid*physact1/ chisq measures;
run;
proc npar1way data= sdiff wilcoxon;
class obesincid;
var age1 bmi1 ob_grs MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 TEI1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 VitA1 VitD1 VitE1 Thia1 Ribo1 Niac1 VitB61 Folate1 VitB121 VitC1 Calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 wholegrain1 fish1 fruit1 vegetables1 redmeat1 desserts1 sugardrink1 friedpot1;
run;
proc freq data= sdiff;
tables T2Dincid*smoke1 T2Dincid*education1 T2Dincid*physact1/ chisq measures;
run;
proc npar1way data= sdiff wilcoxon;
class T2Dincid;
var age1 fglu1 twoglu1 d_grs MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 TEI1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 VitA1 VitD1 VitE1 Thia1 Ribo1 Niac1 VitB61 Folate1 VitB121 VitC1 Calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 wholegrain1 fish1 fruit1 vegetables1 redmeat1 desserts1 sugardrink1 friedpot1;
run;

/*******************************************************
CORRELATIONS FOR INDIVIDUAL LIFESTYLE VARIABLES ADJUSTED FOR AGE SEX TEI1 ENK
NO NEEDED RIGHT NOW
********************************************************/

proc corr data= stuff.a8_impute SPEARMAN outs= correlations NOSIMPLE;
var smoke1 education1 physact1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 VitA1 VitD1 VitE1 Thia1 Ribo1 Niac1 VitB61 Folate1 VitB121 VitC1 Calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 wholegrain1 fish1 fruit1 vegetables1 redmeat1 desserts1 sugardrink1 friedpot1;  
partial age1 sex tei1 enk1;
run;

PROC EXPORT DATA= correlations
OUTFILE= "N:\GLACIER\gene_vs_lifestyle\correlations.xls"
DBMS= xls 
REPLACE;
RUN;

/****************************************************************
     ASSOCIATIONS OF GRS WITH THEIR TRAITS AT BASELINE AND FOLLOW-UP
******************************************************************/
/*baseline*/
/*proc logistic data= stuff.A8_impute;
class sex (ref=last)/param=ref;
model obes1 (event= '1')= age1  AGESQ01 sex ob_grs/rsq stb;
run;
proc glmselect data= stuff.a8_impute;
class sex (ref=last)/param=reference;
model BMI1= age1 agesq01 sex ob_GRS/ selection=none stb showpvalues;
run;
proc logistic data= stuff.A8_impute;
class sex (ref=last)/param=ref;
model T2D1 (event= '1')= age1  AGESQ01 sex d_grs/rsq stb;
run;
proc glmselect data= stuff.a8_impute;
class sex (ref=last)/param=reference;
model fglu1= age1 agesq01 sex fglu_GRS/ selection=none stb showpvalues;
run;
proc glmselect data= stuff.a8_impute;
class sex (ref=last)/param=reference;
model twoglu1= age1 agesq01 sex tglu_GRS/ selection=none stb showpvalues;
run;

/*follow-up*/
/*proc logistic data= stuff.A8_impute;
class sex (ref=last)/param=ref;
model obes2 (event= '1')= age2  AGESQ02 sex ob_grs/rsq stb;
run;
proc glmselect data= stuff.a8_impute;
class sex (ref=last)/param=reference;
model BMI2= age2 agesq02 sex ob_GRS/ selection=none stb showpvalues;
run;
proc logistic data= stuff.A8_impute;
class sex (ref=last)/param=ref;
model T2D2 (event= '1')= age2  AGESQ02 sex d_grs/rsq stb;
run;
proc glmselect data= stuff.a8_impute;
class sex (ref=last)/param=reference;
model fglu2= age2 agesq02 sex fglu_GRS/ selection=none stb showpvalues;
run;
proc glmselect data= stuff.a8_impute;
class sex (ref=last)/param=reference;
model twoglu2= age2 agesq02 sex tglu_GRS/ selection=none stb showpvalues;
run;

/***************************************************************************
     ASSOCIATIONS OF INDIVIDUAL LIFESTYLE VARIABLES WITH INCIDENT OBESITY
*****************************************************************************/
/*proc logistic data= stuff.a8_impute;
class QT_alc1 (ref=first) sex (ref=last) enk1 (ref=first)/param=ref;
model obesincid (event= '1')= age1  AGESQ01 sex  lyear enk1 QT_alc1/rsq stb;
run;
proc logistic data= stuff.a8_impute;
class smoke1 (ref=first) sex (ref=last) enk1 (ref=first)/param=ref;
model obesincid (event= '1')= age1  AGESQ01 sex  lyear enk1 smoke1/rsq stb;
run;
proc logistic data= stuff.a8_impute;
class education1 (ref=first) sex (ref=last) enk1 (ref=first)/param=ref ;
model obesincid (event= '1')= age1 agesq01 sex lyear enk1 education1/rsq stb;
run;
proc logistic data= stuff.a8_impute;
class physact1 (ref=first) sex (ref=last) enk1 (ref=first)/param=ref ;
model obesincid (event= '1')= age1 agesq01 sex lyear enk1 physact1/rsq stb;
run;
proc logistic data= stuff.a8_impute;
class sex (ref=last)enk1 (ref=first)/param=ref;
model obesincid (event= '1')= age1 agesq01 sex lyear enk1 tei1/rsq stb;
run;
%macro basicassoc(risk);
proc logistic data=stuff.a8_impute;
class enk1 (ref=first) sex (ref=last)/ param= ref; 
model obesincid (event='1')= age1 agesq01 sex  lyear enk1 tei1 &risk/rsq stb ;
run; 

%mend;
%basicassoc(MUFA1)
%basicassoc(PUFA1)
%basicassoc(CARBOHYDRATES1)
%basicassoc(SUGARS1)
%basicassoc(PROTEIN1)
%basicassoc(SATFAT1)
%basicassoc(TOTFAT1)
%basicassoc(FIBER1)
%basicassoc(FA1)
%basicassoc(ALC1)
%basicassoc(SALT1)
%basicassoc(vitA1)
%basicassoc(vitD1)
%basicassoc(vitE1)
%basicassoc(Thia1)
%basicassoc(Ribo1)
%basicassoc(Niac1)
%basicassoc(vitB61)
%basicassoc(Folate1)
%basicassoc(vitB121)
%basicassoc(vitC1)
%basicassoc(calc1)
%basicassoc(Phosp1)
%basicassoc(Pota1)
%basicassoc(Magn1)
%basicassoc(Iron1)
%basicassoc(Zinc1)
%basicassoc(Iodine1)
%basicassoc(Selenium1)
%basicassoc(wholegrain1)
%basicassoc(fish1)
%basicassoc(fruit1)
%basicassoc(vegetables1)
%basicassoc(redmeat1)
%basicassoc(desserts1)
%basicassoc(sugardrink1)
%basicassoc(friedpot1)

/************************************************************************************
     ASSOCIATIONS OF INDIVIDUAL LIFESTYLE VARIABLES WITH INCIDENT T2D
*************************************************************************************/
/*proc logistic data= stuff.a8_impute;
class QT_alc1 (ref=first) sex (ref=last) enk1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first)fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model T2Dincid (event= '1')= age1 AGESQ01 sex lyear enk1 QT_alc1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/rsq stb;
run;
proc logistic data= stuff.a8_impute;
class smoke1 (ref=first) sex (ref=last) enk1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first)fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model T2Dincid (event= '1')= age1 AGESQ01 sex lyear enk1 smoke1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/rsq stb;
run;
proc logistic data= stuff.a8_impute;
class education1 (ref=first) sex (ref=last) enk1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first)fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref ;
model T2Dincid (event= '1')= age1 agesq01 sex lyear enk1 education1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/rsq stb;
run;
proc logistic data= stuff.a8_impute;
class physact1 (ref=first) sex (ref=last) enk1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first)fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref ;
model T2Dincid (event= '1')= age1 agesq01 sex lyear enk1 physact1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/rsq stb;
run;
proc logistic data= stuff.a8_impute;
class sex (ref=last)enk1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first)fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model T2Dincid (event= '1')= age1 agesq01 sex lyear enk1 tei1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/rsq stb;
run;

%macro basicassoc(risk);
proc logistic data=stuff.a8_impute;
class enk1 (ref=first) sex (ref=last) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first)fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/ param= ref; 
model T2Dincid (event='1')= age1 agesq01 sex lyear enk1 tei1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 &risk/rsq stb;
run; 

%mend;
%basicassoc(MUFA1)
%basicassoc(PUFA1)
%basicassoc(CARBOHYDRATES1)
%basicassoc(SUGARS1)
%basicassoc(PROTEIN1)
%basicassoc(SATFAT1)
%basicassoc(TOTFAT1)
%basicassoc(FIBER1)
%basicassoc(FA1)
%basicassoc(ALC1)
%basicassoc(SALT1)
%basicassoc(vitA1)
%basicassoc(vitD1)
%basicassoc(vitE1)
%basicassoc(Thia1)
%basicassoc(Ribo1)
%basicassoc(Niac1)
%basicassoc(vitB61)
%basicassoc(Folate1)
%basicassoc(vitB121)
%basicassoc(vitC1)
%basicassoc(calc1)
%basicassoc(Phosp1)
%basicassoc(Pota1)
%basicassoc(Magn1)
%basicassoc(Iron1)
%basicassoc(Zinc1)
%basicassoc(Iodine1)
%basicassoc(Selenium1)
%basicassoc(wholegrain1)
%basicassoc(fish1)
%basicassoc(fruit1)
%basicassoc(vegetables1)
%basicassoc(redmeat1)
%basicassoc(desserts1)
%basicassoc(sugardrink1)
%basicassoc(friedpot1)


/***MODEL DISCRIMINATION (AUC, Sensitivity, AIC and Hosmer-Lemeshow test) FOR INDIVIDUAL LIFESTYLE AND GENETIC VARIABLES***/
/*logistic models*/
/**obesity**/
data stuff.a9_impute; set stuff.a8_impute; if fiber1 ne .; if vitD1 ne .; if lyear ne .; if smoke1 ne .; if education1 ne .; if enk1 ne .; if physact1 ne .; run;
proc freq data= stuff.a9_impute; tables obesincid t2dincid IDFGincid IDGTincid; run;

ods html sge=on;
ods graphics on;
title1 'AUCs obesity';
proc logistic data= stuff.a9_impute;
class enk1 (ref=first)sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) /param=ref;
model obesincid (event='1')= age1 agesq01 sex lyear smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964/nofit;
roc 'Model 1 genetic' age1 agesq01 lyear sex rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964;
roc 'Model 2 lifestyle' age1 agesq01 lyear sex smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 ;
roc 'Model 3 combined' age1 agesq01 sex lyear smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964;
roccontrast adjacentpairs/ESTIMATE=ALLPAIRS;
run;
ods html sge=off;


proc logistic data= stuff.a9_impute;
class enk1 (ref=first)sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) /param=ref;
model obesincid (event='1')= age1 agesq01 lyear smoke1 education1 physact1 enk1 sex tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1/ rsq ctable lackfit outroc= rocoblif;
roc;
run;

proc logistic data= stuff.a9_impute;
class sex (ref=last)/param=ref;
model obesincid (event='1')= age1 agesq01 sex lyear rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964 / rsq ctable lackfit outroc= rocobgen;
roc;
run;
proc logistic data= stuff.a9_impute;
class enk1 (ref=first)sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) /param=ref;
model obesincid (event='1')= age1 agesq01 sex lyear smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964 / rsq ctable lackfit outroc= rocobcom;
roc;
run;

proc export data= rocobcom
DBMS= xls
outfile= rocobcom Replace;
run;

/**Diabetes**/

ods html sge=on;
ods graphics on;
title1 'ROCAUC t2d';
proc logistic data= stuff.a9_impute;
class enk1 (ref=first)sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model T2Dincid (event='1')= age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs340874d rs7515431 rs7903146d rs12571751 rs12242953 rs11257655 rs1111875 rs5215 rs2334499 rs163184 rs1552224 rs10830963d rs7955901 rs2261181 rs7965349 rs11063069 rs10842994 rs1359790 rs7177055 rs4502156d rs2007084
rs12899811 rs11634397 rs9923233 rs7202877 rs11651755 rs2447090 rs11663816 rs8182584 rs8108269 rs10401969 rs780094d rs7569522 rs2943640 rs243083 rs1128249 rs10203174 rs4812829 rs6795735 rs4402960 rs2197423
rs17301514 rs1496653 rs12497268 rs11717195d rs6819243 rs10012946 rs6878122 rs459193 rs7756992 rs4299828 rs3734621 rs849135 rs10229583 rs17168486 rs13233731 rs3824065 rs7845219 rs516946 rs3802177 rs2796441 rs17791513 rs16927668 rs10811661d rs10758593 /nofit;
roc 'Model 1 genetic' age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 rs340874d rs7515431 rs7903146d rs12571751 rs12242953 rs11257655 rs1111875 rs5215 rs2334499 rs163184 rs1552224 rs10830963d rs7955901 rs2261181 rs7965349 rs11063069 rs10842994 rs1359790 rs7177055 rs4502156d rs2007084
rs12899811 rs11634397 rs9923233 rs7202877 rs11651755 rs2447090 rs11663816 rs8182584 rs8108269 rs10401969 rs780094d rs7569522 rs2943640 rs243083 rs1128249 rs10203174 rs4812829 rs6795735 rs4402960 rs2197423
rs17301514 rs1496653 rs12497268 rs11717195d rs6819243 rs10012946 rs6878122 rs459193 rs7756992 rs4299828 rs3734621 rs849135 rs10229583 rs17168486 rs13233731 rs3824065 rs7845219 rs516946 rs3802177 rs2796441 rs17791513 rs16927668 rs10811661d rs10758593 ;
roc 'Model 2 lifestyle' age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1;
roc 'Model 3 combined' age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs340874d rs7515431 rs7903146d rs12571751 rs12242953 rs11257655 rs1111875 rs5215 rs2334499 rs163184 rs1552224 rs10830963d rs7955901 rs2261181 rs7965349 rs11063069 rs10842994 rs1359790 rs7177055 rs4502156d rs2007084
rs12899811 rs11634397 rs9923233 rs7202877 rs11651755 rs2447090 rs11663816 rs8182584 rs8108269 rs10401969 rs780094d rs7569522 rs2943640 rs243083 rs1128249 rs10203174 rs4812829 rs6795735 rs4402960 rs2197423
rs17301514 rs1496653 rs12497268 rs11717195d rs6819243 rs10012946 rs6878122 rs459193 rs7756992 rs4299828 rs3734621 rs849135 rs10229583 rs17168486 rs13233731 rs3824065 rs7845219 rs516946 rs3802177 rs2796441 rs17791513 rs16927668 rs10811661d rs10758593;
roccontrast adjacentpairs/ESTIMATE=ALLPAIRS;
run;
ods html sge=off;

proc logistic data= stuff.a9_impute;
class enk1 (ref=first)sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model T2Dincid (event='1')= age1 agesq01 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear smoke1 education1 physact1 enk1 sex tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1/ rsq ctable lackfit outroc= roct2dlif;
roc;
run;
proc logistic data= stuff.a9_impute;
class sex (ref=last) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model T2Dincid (event='1')= age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 rs340874d rs7515431 rs7903146d rs12571751 rs12242953 rs11257655 rs1111875 rs5215 rs2334499 rs163184 rs1552224 rs10830963d rs7955901 rs2261181 rs7965349 rs11063069 rs10842994 rs1359790 rs7177055 rs4502156d rs2007084
rs12899811 rs11634397 rs9923233 rs7202877 rs11651755 rs2447090 rs11663816 rs8182584 rs8108269 rs10401969 rs780094d rs7569522 rs2943640 rs243083 rs1128249 rs10203174 rs4812829 rs6795735 rs4402960 rs2197423
rs17301514 rs1496653 rs12497268 rs11717195d rs6819243 rs10012946 rs6878122 rs459193 rs7756992 rs4299828 rs3734621 rs849135 rs10229583 rs17168486 rs13233731 rs3824065 rs7845219 rs516946 rs3802177 rs2796441 rs17791513 rs16927668 rs10811661d rs10758593 / rsq ctable lackfit outroc= roct2dgen;
roc;
run;
proc logistic data= stuff.a9_impute;
class enk1 (ref=first)sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model T2Dincid (event='1')= age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs340874d rs7515431 rs7903146d rs12571751 rs12242953 rs11257655 rs1111875 rs5215 rs2334499 rs163184 rs1552224 rs10830963d rs7955901 rs2261181 rs7965349 rs11063069 rs10842994 rs1359790 rs7177055 rs4502156d rs2007084
rs12899811 rs11634397 rs9923233 rs7202877 rs11651755 rs2447090 rs11663816 rs8182584 rs8108269 rs10401969 rs780094d rs7569522 rs2943640 rs243083 rs1128249 rs10203174 rs4812829 rs6795735 rs4402960 rs2197423
rs17301514 rs1496653 rs12497268 rs11717195d rs6819243 rs10012946 rs6878122 rs459193 rs7756992 rs4299828 rs3734621 rs849135 rs10229583 rs17168486 rs13233731 rs3824065 rs7845219 rs516946 rs3802177 rs2796441 rs17791513 rs16927668 rs10811661d rs10758593 / rsq ctable lackfit outroc= roct2dcom;
roc;
run;

proc export data= roct2dcom
DBMS= xls
outfile= roct2dcom Replace;
run;


/**IDFGincid**/

ods html sge=on;
ods graphics on;
title1 'ROCAUCs incident IFG';
proc logistic data= stuff.a9_impute;
class enk1 (ref=first)sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first)fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first) /param=ref;
model IDFGincid (event='1')= age1 agesq01 sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs10811661f rs4869272 rs11619319 rs983309 rs6943153 rs11603334 rs6113722 rs16913693 rs3829109 rs3783347 rs2302593 rs9368222 rs10747083 rs6072275 rs7651090f
rs576674 rs11715915 rs17762454 rs7708285 rs2657879 rs340874f rs780094f rs560887 rs11708067 rs1280 rs2191349 rs2908289 rs11558471 rs10814916 rs11195502 rs7903146f rs11607883 rs11039182 rs174576 rs10830963f rs4502156f /nofit;
roc 'Model 1 genetic' age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 rs10811661f rs4869272 rs11619319 rs983309 rs6943153 rs11603334 rs6113722 rs16913693 rs3829109 rs3783347 rs2302593 rs9368222 rs10747083 rs6072275 rs7651090f
rs576674 rs11715915 rs17762454 rs7708285 rs2657879 rs340874f rs780094f rs560887 rs11708067 rs1280 rs2191349 rs2908289 rs11558471 rs10814916 rs11195502 rs7903146f rs11607883 rs11039182 rs174576 rs10830963f rs4502156f;
roc 'Model 2 lifestyle' age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1;
roc 'Model 3 combined' age1 agesq01 sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs10811661f rs4869272 rs11619319 rs983309 rs6943153 rs11603334 rs6113722 rs16913693 rs3829109 rs3783347 rs2302593 rs9368222 rs10747083 rs6072275 rs7651090f
rs576674 rs11715915 rs17762454 rs7708285 rs2657879 rs340874f rs780094f rs560887 rs11708067 rs1280 rs2191349 rs2908289 rs11558471 rs10814916 rs11195502 rs7903146f rs11607883 rs11039182 rs174576 rs10830963f rs4502156f;
roccontrast adjacentpairs/ESTIMATE=ALLPAIRS;
run;
ods html sge=off;

proc logistic data= stuff.a9_impute;
class enk1 (ref=first)sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first)fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDFGincid (event='1')= age1 agesq01 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear smoke1 education1 physact1 enk1 sex tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1/ rsq ctable lackfit outroc= rocifglif;
roc;
run;
proc logistic data= stuff.a9_impute;
class sex (ref=last) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDFGincid (event='1')= age1 agesq01 sex lyear fastamiss1 fasta4h1 fasta4to8h1  fastamiss2 fasta4h2 fasta4to8h2 rs10811661f rs4869272 rs11619319 rs983309 rs6943153 rs11603334 rs6113722 rs16913693 rs3829109 rs3783347 rs2302593 rs9368222 rs10747083 rs6072275 rs7651090f
rs576674 rs11715915 rs17762454 rs7708285 rs2657879 rs340874f rs780094f rs560887 rs11708067 rs1280 rs2191349 rs2908289 rs11558471 rs10814916 rs11195502 rs7903146f rs11607883 rs11039182 rs174576 rs10830963f rs4502156f/ rsq ctable lackfit outroc= rocifggen;
roc;
run;
proc logistic data= stuff.a9_impute;
class enk1 (ref=first)sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first)fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDFGincid (event='1')= age1 agesq01 sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs10811661f rs4869272 rs11619319 rs983309 rs6943153 rs11603334 rs6113722 rs16913693 rs3829109 rs3783347 rs2302593 rs9368222 rs10747083 rs6072275 rs7651090f
rs576674 rs11715915 rs17762454 rs7708285 rs2657879 rs340874f rs780094f rs560887 rs11708067 rs1280 rs2191349 rs2908289 rs11558471 rs10814916 rs11195502 rs7903146f rs11607883 rs11039182 rs174576 rs10830963f rs4502156f/ rsq ctable lackfit outroc= rocifgcom;
roc;
run;

proc export data= rocifgcom
DBMS= xls
outfile= rocifgcom Replace;
run;

/**IDGTincid**/

ods html sge=on;
ods graphics on;
title1 'ROCAUCs incident IGT';
proc logistic data= stuff.a9_impute;
class enk1 (ref=first)sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDGTincid (event='1')= age1 agesq01 sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1260326 rs11717195t rs12255372 rs1436958 rs11672660 rs6975024 rs11782386 rs1019503 rs7651090t /nofit;
roc 'Model 1 genetic' age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 rs1260326 rs11717195t rs12255372 rs1436958 rs11672660 rs6975024 rs11782386 rs1019503 rs7651090t;
roc 'Model 2 lifestyle' age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1;
roc 'Model 3 combined' age1 agesq01 sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1260326 rs11717195t rs12255372 rs1436958 rs11672660 rs6975024 rs11782386 rs1019503 rs7651090t;
roccontrast adjacentpairs/ESTIMATE=ALLPAIRS;
run;
ods html sge=off;

proc logistic data= stuff.a9_impute;
class enk1 (ref=first)sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDGTincid (event='1')= age1 agesq01 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear smoke1 education1 physact1 enk1 sex tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1/ rsq ctable lackfit outroc= rocigtlif;
roc;
run;
proc logistic data= stuff.a9_impute;
class sex (ref=last)fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDGTincid (event='1')= age1 agesq01 sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear rs1260326 rs11717195t rs12255372 rs1436958 rs11672660 rs6975024 rs11782386 rs1019503 rs7651090t/ rsq ctable lackfit outroc= rocigtgen;
roc;
run;
proc logistic data= stuff.a9_impute;
class enk1 (ref=first)sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDGTincid (event='1')= age1 agesq01 sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1260326 rs11717195t rs12255372 rs1436958 rs11672660 rs6975024 rs11782386 rs1019503 rs7651090t / rsq ctable lackfit outroc= rocigtcom;
roc;
run;

proc export data= rocigtcom
DBMS= xls
outfile= rocigtcom Replace;
run;


/*10% weight gain*/
/*create the variable of 10% of weight gain*/
data stuff.a9_impute; set stuff.a9_impute;
percweightch= .; percweightch= (100*deltaweight)/weight1;
wch10= .;
if percweightch < 10 then wch10= 0;
if percweightch >= 10 then wch10= 1;
if percweightch = . then wch10= .;
run;


ods html sge=on;
ods graphics on;
title1 'AUCs 10%weight gain';
proc logistic data= stuff.a9_impute;
class enk1 (ref=first)sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) /param=ref;
model wch10 (event='1')= age1 agesq01 sex lyear smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964/nofit;
roc 'Model 1 genetic' age1 agesq01 lyear sex rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964;
roc 'Model 2 lifestyle' age1 agesq01 lyear sex smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 ;
roc 'Model 3 combined' age1 agesq01 sex lyear smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964;
roccontrast adjacentpairs/ESTIMATE=ALLPAIRS;
run;
ods html sge=off;

proc logistic data= stuff.a9_impute;
class enk1 (ref=first)sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) /param=ref;
model wch10 (event='1')= age1 agesq01 lyear smoke1 education1 physact1 enk1 sex tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1/ rsq ctable lackfit outroc= rocwch10lif;
roc;
run;

proc logistic data= stuff.a9_impute;
class sex (ref=last)/param=ref;
model wch10 (event='1')= age1 agesq01 sex lyear rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964 / rsq ctable lackfit outroc= rocwch10gen;
roc;
run;

proc logistic data= stuff.a9_impute;
class enk1 (ref=first)sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) /param=ref;
model wch10 (event='1')= age1 agesq01 sex lyear smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964 / rsq ctable lackfit outroc= rocwch10com;
roc;
run;

proc export data= rocwch10com
DBMS= xls
outfile= rocwch10com Replace;
run;

/**FGworsen**/

/*proc logistic data= stuff.a8_impute;
class enk1 (ref=first)sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) QT_alc1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first)fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model FGworsen (event='1')= age1 agesq01 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear QT_alc1 smoke1 education1 physact1 enk1 sex tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1/ rsq ctable outroc= rocGRS11;
roc;
run;
proc logistic data= stuff.a8_impute;
class sex (ref=last) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model FGworsen (event='1')= age1 agesq01 sex lyear fastamiss1 fasta4h1 fasta4to8h1  fastamiss2 fasta4h2 fasta4to8h2 rs10811661f rs4869272 rs11619319 rs983309 rs6943153 rs11603334 rs6113722 rs16913693 rs3829109 rs3783347 rs2302593 rs9368222 rs10747083 rs6072275 rs7651090f
rs576674 rs11715915 rs17762454 rs7708285 rs2657879 rs340874f rs780094f rs560887 rs11708067 rs1280 rs2191349 rs2908289 rs11558471 rs10814916 rs11195502 rs7903146f rs11607883 rs11039182 rs174576 rs10830963f rs4502156f/ rsq ctable;
roc;
run;
proc logistic data= stuff.a8_impute;
class enk1 (ref=first)sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) QT_alc1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first)fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model FGworsen (event='1')= age1 agesq01 sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear QT_alc1 smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs10811661f rs4869272 rs11619319 rs983309 rs6943153 rs11603334 rs6113722 rs16913693 rs3829109 rs3783347 rs2302593 rs9368222 rs10747083 rs6072275 rs7651090f
rs576674 rs11715915 rs17762454 rs7708285 rs2657879 rs340874f rs780094f rs560887 rs11708067 rs1280 rs2191349 rs2908289 rs11558471 rs10814916 rs11195502 rs7903146f rs11607883 rs11039182 rs174576 rs10830963f rs4502156f/ rsq ctable;
roc;
run;



/**GTworsen**/
/*proc logistic data= stuff.a8_impute;
class enk1 (ref=first)sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) QT_alc1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model GTworsen (event='1')= age1 agesq01 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear QT_alc1 smoke1 education1 physact1 enk1 sex tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1/ rsq ctable outroc= rocGRS11;
roc;
run;
proc logistic data= stuff.a8_impute;
class sex (ref=last)fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model GTworsen (event='1')= age1 agesq01 sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear rs1260326 rs11717195t rs12255372 rs1436958 rs11672660 rs6975024 rs11782386 rs1019503 rs7651090t  / rsq ctable;
roc;
run;
proc logistic data= stuff.a8_impute;
class enk1 (ref=first)sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) QT_alc1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model GTworsen (event='1')= age1 agesq01 sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear QT_alc1 smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1260326 rs11717195t rs12255372 rs1436958 rs11672660 rs6975024 rs11782386 rs1019503 rs7651090t  / rsq ctable;
roc;
run;

/*linear models*/

/*change in bmi*/
/*proc glmselect data= stuff.a9_impute;
class sex (ref=last)/param=reference;
model deltabmi= age1 agesq01 sex lyear rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964/ selection=none stb showpvalues;
run;
proc glmselect data= stuff.a9_impute;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first)enk1 (ref=first) physact1 (ref=first)/param=reference;
model deltabmi= age1 agesq01 lyear smoke1 education1 physact1 enk1 sex tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1/selection=none stb showpvalues;
run;
proc glmselect data= stuff.a9_impute;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first)enk1 (ref=first) physact1 (ref=first)/param=reference;
model deltabmi= age1 agesq01 sex lyear smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964 /selection=none stb showpvalues;
run;

/*change in fasting glucose*/

/*proc glmselect data= stuff.a9_impute;
class sex (ref=last)fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=reference;
model deltafglu= age1 agesq01 sex lyear fastamiss1 fasta4h1 fasta4to8h1  fastamiss2 fasta4h2 fasta4to8h2 rs10811661f rs4869272 rs11619319 rs983309 rs6943153 rs11603334 rs6113722 rs16913693 rs3829109 rs3783347 rs2302593 rs9368222 rs10747083 rs6072275 rs7651090f
rs576674 rs11715915 rs17762454 rs7708285 rs2657879 rs340874f rs780094f rs560887 rs11708067 rs1280 rs2191349 rs2908289 rs11558471 rs10814916 rs11195502 rs7903146f rs11607883 rs11039182 rs174576 rs10830963f rs4502156f/ selection=none stb showpvalues;
run;
proc glmselect data= stuff.a9_impute;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first)enk1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=reference;
model deltafglu= age1 agesq01 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear smoke1 education1 physact1 enk1 sex tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1/selection=none stb showpvalues;
run;
proc glmselect data= stuff.a9_impute;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first)enk1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=reference;
model deltafglu= age1 agesq01 sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs10811661f rs4869272 rs11619319 rs983309 rs6943153 rs11603334 rs6113722 rs16913693 rs3829109 rs3783347 rs2302593 rs9368222 rs10747083 rs6072275 rs7651090f
rs576674 rs11715915 rs17762454 rs7708285 rs2657879 rs340874f rs780094f rs560887 rs11708067 rs1280 rs2191349 rs2908289 rs11558471 rs10814916 rs11195502 rs7903146f rs11607883 rs11039182 rs174576 rs10830963f rs4502156f/selection=none stb showpvalues;
run;


/*change in two hour glucose*/

/*proc glmselect data= stuff.a9_impute;
class sex (ref=last) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=reference;
model deltatwoglu= age1 agesq01 sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear rs1260326 rs11717195t rs12255372 rs1436958 rs11672660 rs6975024 rs11782386 rs1019503 rs7651090t/ selection=none stb showpvalues;
run;
proc glmselect data= stuff.a9_impute;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first)enk1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=reference;
model deltatwoglu= age1 agesq01 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear smoke1 education1 physact1 enk1 sex tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1/selection=none stb showpvalues;
run;
proc glmselect data= stuff.a9_impute;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first)enk1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=reference;
model deltatwoglu= age1 agesq01 sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1260326 rs11717195t rs12255372 rs1436958 rs11672660 rs6975024 rs11782386 rs1019503 rs7651090t/selection=none stb showpvalues;
run;


/***NET RECLASSIFICATION IMPROVEMENT**/

/*NRI and IDI*/

/*create datasets for each trait*/

data nri; set stuff.a9_impute;
cnt+1;
run; 

/*obesity*/
proc logistic data= nri;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first) physact1 (ref=first)/param=ref;
model obesincid (event='1')= age1 agesq01 lyear sex smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 ;
output out=m1 pred=p1;
run;
proc logistic data= nri;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first) physact1 (ref=first)/param=ref;
model obesincid (event='1')= age1 agesq01 sex lyear smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964;
output out=m2 pred=p2;
run;
proc sql; 
create table ob as select *
from m1 as a left join m2 as b on a.cnt=b.cnt;
quit;

/*weight gain*/
proc logistic data= nri;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first) physact1 (ref=first)/param=ref;
model wch10 (event='1')= age1 agesq01 lyear sex smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 ;
output out=m3 pred=p3;
run;
proc logistic data= nri;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first) physact1 (ref=first)/param=ref;
model wch10 (event='1')= age1 agesq01 sex lyear smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964;
output out=m4 pred=p4;
run;
proc sql; 
create table wch10 as select *
from m3 as a left join m4 as b on a.cnt=b.cnt;
quit;

/*T2D*/
proc logistic data= nri;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first) physact1 (ref=first)fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model T2Dincid (event='1')= age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1;
output out=m5 pred=p5;
run;
proc logistic data= nri;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first) physact1 (ref=first)fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model T2Dincid (event='1')= age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs340874d rs7515431 rs7903146d rs12571751 rs12242953 rs11257655 rs1111875 rs5215 rs2334499 rs163184 rs1552224 rs10830963d rs7955901 rs2261181 rs7965349 rs11063069 rs10842994 rs1359790 rs7177055 rs4502156d rs2007084
rs12899811 rs11634397 rs9923233 rs7202877 rs11651755 rs2447090 rs11663816 rs8182584 rs8108269 rs10401969 rs780094d rs7569522 rs2943640 rs243083 rs1128249 rs10203174 rs4812829 rs6795735 rs4402960 rs2197423
rs17301514 rs1496653 rs12497268 rs11717195d rs6819243 rs10012946 rs6878122 rs459193 rs7756992 rs4299828 rs3734621 rs849135 rs10229583 rs17168486 rs13233731 rs3824065 rs7845219 rs516946 rs3802177 rs2796441 rs17791513 rs16927668 rs10811661d rs10758593;
output out=m6 pred=p6;
run;
proc sql; 
create table t2d as select *
from m5 as a left join m6 as b on a.cnt=b.cnt;
quit;

/*IDFG*/
proc logistic data= nri;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first)  physact1 (ref=first)fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDFGincid (event='1')= age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1;
output out=m7 pred=p7;
run;
proc logistic data= nri;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first) physact1 (ref=first)fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDFGincid (event='1')= age1 agesq01 sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs10811661f rs4869272 rs11619319 rs983309 rs6943153 rs11603334 rs6113722 rs16913693 rs3829109 rs3783347 rs2302593 rs9368222 rs10747083 rs6072275 rs7651090f
rs576674 rs11715915 rs17762454 rs7708285 rs2657879 rs340874f rs780094f rs560887 rs11708067 rs1280 rs2191349 rs2908289 rs11558471 rs10814916 rs11195502 rs7903146f rs11607883 rs11039182 rs174576 rs10830963f rs4502156f;
output out=m8 pred=p8;
run;
proc sql; 
create table idfg as select *
from m7 as a left join m8 as b on a.cnt=b.cnt;
quit;


/*IDGT*/
proc logistic data= nri;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first) physact1 (ref=first)fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDGTincid (event='1')= age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1;
output out=m9 pred=p9;
run;
proc logistic data= nri;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first) physact1 (ref=first)fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDGTincid (event='1')= age1 agesq01 sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1260326 rs11717195t rs12255372 rs1436958 rs11672660 rs6975024 rs11782386 rs1019503 rs7651090t;
output out=m10 pred=p10;
run;
proc sql; 
create table idgt as select *
from m9 as a left join m10 as b on a.cnt=b.cnt;
quit;


/*run macro continous NRI*/

%macro CONTNRI(DSNAME,DETAIL,STATVAR,PROB1,PROB2);
* Macro to compute NRI with any up or down (no categories); 
* Control output of frequency tables and means below;
*  Variables:
*  DSNAME = dataset name;
*  DETAIL = 2 for detailed printout, 1 for limited, 0 for none;
*  STATVAR = outcome variable (coded 0,1);
*  PROB1 = probability for model 1;
*  PROB2 = probability for model 2;

title2 Test of Continuous NRI for &PROB2 vs &PROB1 ;

data nri1; set &dsname;
* compute diffs in probs;
stat=&STATVAR;
prob1=&PROB1;
prob2=&PROB2;
diffp=prob2-prob1;
if prob1>. and prob2>.;

if prob2>prob1 then disc=1;
  else if prob2=prob1 then disc=0;
  else if prob2<prob1 then disc=-1;
run;

*** Control output as needed ***;
%if &detail=2 %then %do;
proc means data=nri1 n sum mean median stddev min max;
var prob1 prob2 diffp disc stat;
run;
%end;

%if &detail=2 %then %do;
proc freq data=nri1;
tables stat*disc / chisq;
run;
%end;

ods listing exclude all;
ods output CrossTabFreqs=freqs;
ods output ChiSq=chisq;
run;
proc freq data=nri1;
tables stat*disc / chisq;
run;
ods listing exclude none;

data trend; set chisq;
* if df=1;
if Statistic="Mantel-Haenszel Chi-Square";
chitrend=Value;
ptrend=1-probchi(chitrend,1);
keep chitrend ptrend;
run;

%if &detail=2 %then %do;
proc print data=freqs;
run;
%end;

data nri; set freqs end=eof;
retain up_case down_case up_contr down_contr ncase ncontr 0;
if stat=1 and disc=1 then up_case=RowPercent/100;
   else if stat=1 and disc=-1 then down_case=RowPercent/100;
   else if stat=0 and disc=1 then up_contr=RowPercent/100;
   else if stat=0 and disc=-1 then down_contr=RowPercent/100;
* Number of cases and controls is the same for pd and pred;
if stat=1 and _type_='10' then ncase=Frequency;
   else if stat=0 and _type_='10' then ncontr=Frequency;

if eof then do;
  %if ncase=0 or ncontr=0 %then %do;
     nri=.; znri=.; p2nri=.;
     %goto exit;
  %end;
  ri_case=up_case-down_case;
  ri_contr=down_contr-up_contr;
  nri=ri_case+ri_contr;
  * corrected variance to reflect binomial;
  vri_case=4*up_case*(1-up_case)/ncase;
  vri_contr=4*up_contr*(1-up_contr)/ncontr;
  vnri=vri_case+vri_contr;
  senri=sqrt(vnri);
  nricil=nri-1.96*senri;
  nriciu=nri+1.96*senri;
  if vri_case ne 0 then zri_case=ri_case/sqrt(vri_case);
  if vri_contr ne 0 then zri_contr=ri_contr/sqrt(vri_contr);
  if vnri ne 0 then znri=nri/sqrt(vnri);
  p2ri_case=2*(1-probnorm(abs(zri_case)));
  p2ri_contr=2*(1-probnorm(abs(zri_contr)));
  p2nri=2*(1-probnorm(abs(znri)));
  output;
  keep ncase ncontr
       up_case down_case up_contr down_contr
       ri_case ri_contr nri vri_case vri_contr vnri senri nricil nriciu
       zri_case zri_contr znri p2ri_case p2ri_contr p2nri
       ;
end;

data _all_; merge nri trend ; 
run;

%if &detail>0 %then %do;
proc print data=_all_;
var    ncase ncontr 
       up_case down_case up_contr down_contr 
       ri_case ri_contr nri vri_case vri_contr vnri senri nricil nriciu
       zri_case zri_contr znri p2ri_case p2ri_contr p2nri
       chitrend ptrend
       ;
run;
%end;
title2; run;
* Usage:
* %contnri(probs,1,outxy,pdx,pdxy);
%exit: %mend CONTNRI;
%contnri(ob,1,obesincid,p1,p2);
%contnri(wch10,1,wch10,p3,p4);
%contnri(t2d,1,T2Dincid,p5,p6);
%contnri(idfg,1,IDFGincid,p7,p8);
%contnri(idgt,1,IDGTincid,p9,p10);

/*run macro continous IDI*/
%macro IDIMACRO(DSNAME,DETAIL,OUT01,PROB1,PROB2);
*  Macro to compute difference in Yates slopes or
     integrated discrimination improvement (IDI) from Pencina, 2007;

*  Variables:
*  DSNAME = dataset name;
*  DETAIL = 1 or 2 for limited printout, 0 for none;
*  OUT01 = outcome variable (coded 0,1) (if 1,2 alter signs);
*  PROB1 = probability for model 1;
*  PROB2 = probability for model 2;

data ididat; set &dsname;
diffprob=&prob2-&prob1;
if &prob1>. and &prob2>.;
run;

%if &detail<2 %then %do;
ods listing exclude all;
%end;
ods output TTests=tstats;
ods output Statistics=unistats;
proc ttest data=ididat;
class &out01;
var &prob1 &prob2 diffprob;
title2 "Test of Difference in Yates Slope (IDI)";
run;

data idi1; set unistats end=eof;
retain yates1 yse1 ycil1 yciu1 yates2 yse2 ycil2 yciu2
       diffcase dcasese dcasecil dcaseciu diffcont dcontse dcontcil dcontciu
       idi idicil idiciu;
* Note: Yates slopes are negative since outcome coded 0,1;
* And CIs are reversed;
if _n_=3 then do;
  yates1=-Mean;
  yse1=StdErr;
  ycil1=-UpperCLMean;
  yciu1=-LowerCLMean;
end;
else if _n_=6 then do;
  yates2=-Mean;
  yse2=StdErr;
  ycil2=-UpperCLMean;
  yciu2=-LowerCLMean;
end;
else if _n_=8 then do;
  diffcase=Mean;
  dcasese=StdErr;
  dcasecil=LowerCLMean;
  dcaseciu=UpperCLMean;
end;
else if _n_=7 then do;
  diffcont=Mean;
  dcontse=StdErr;
  dcontcil=LowerCLMean;
  dcontciu=UpperCLMean;
end;
else if _n_=9 then do;
  idi=-Mean;
  idise=StdErr;
  idicil=-UpperCLMean;
  idiciu=-LowerCLMean;
end;
if eof then do;
   rel_idi=yates2/yates1 - 1;
   * Compute normal zscore based on Pencina;
   normse=sqrt((dcasese)**2 +(dcontse)**2);
   if normse>0 then zidi=idi/normse;
   p2idi=2*(1-probnorm(abs(zidi)));
   output;
end;
keep yates1 yse1 ycil1 yciu1 yates2 yse2 ycil2 yciu2
     diffcase dcasese dcasecil dcaseciu diffcont dcontse dcontcil dcontciu
     idi idise idicil idiciu rel_idi normse zidi p2idi;
run;

data idi2; set tstats end=eof;
if _n_=6 then do;
  idit=-tValue;
  idip=Probt;
  output;
  keep idit idip;
end;

data _idi_; merge idi1 idi2;
run;
ods listing exclude none;
%if &detail>0 %then %do;
proc print data=_idi_;
run;
%end;
title2; run;

* Usage:
* %idimacro(probs,1,outxy,pdx,pdxy,outxy);
%mend IDIMACRO;
%idimacro(ob,1,obesincid,p1,p2);
%idimacro(wch10,1,wch10,p3,p4);
%idimacro(t2d,1,T2Dincid,p5,p6);
%idimacro(idfg,1,IDFGincid,p7,p8);
%idimacro(idgt,1,IDGTincid,p9,p10);


/*ADDITIONAL ANALYSES*/

/**MULTITRAIT genetic information*/

ods html sge=on;
ods graphics on;
title1 'ROCAUC t2d multitrait';
proc logistic data= stuff.a9_impute;
class enk1 (ref=first)sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model T2Dincid (event='1')= age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs340874d rs7515431 rs7903146d rs12571751 rs12242953 rs11257655 rs1111875 rs5215 rs2334499 rs163184 rs1552224 rs10830963d rs7955901 rs2261181 rs7965349 rs11063069 rs10842994 rs1359790 rs7177055 rs4502156d rs2007084
rs12899811 rs11634397 rs9923233 rs7202877 rs11651755 rs2447090 rs11663816 rs8182584 rs8108269 rs10401969 rs780094d rs7569522 rs2943640 rs243083 rs1128249 rs10203174 rs4812829 rs6795735 rs4402960 rs2197423
rs17301514 rs1496653 rs12497268 rs11717195d rs6819243 rs10012946 rs6878122 rs459193 rs7756992 rs4299828 rs3734621 rs849135 rs10229583 rs17168486 rs13233731 rs3824065 rs7845219 rs516946 rs3802177 rs2796441 rs17791513 rs16927668 rs10811661d rs10758593
rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411 rs7903146d
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2176598 rs734597 rs2245368 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964 rs4869272 rs11619319 rs983309 rs6943153 rs6113722 rs16913693 rs3829109 rs3783347 rs2302593 rs10747083 rs6072275
rs576674 rs11715915 rs17762454 rs2657879 rs560887 rs1280 rs2191349 rs2908289 rs10814916 rs11195502 rs11607883 rs11039182 rs174576 rs12255372 rs1436958 rs11672660 rs11782386 rs1019503/nofit;
roc 'Model 1 genetic' age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 rs340874d rs7515431 rs7903146d rs12571751 rs12242953 rs11257655 rs1111875 rs5215 rs2334499 rs163184 rs1552224 rs10830963d rs7955901 rs2261181 rs7965349 rs11063069 rs10842994 rs1359790 rs7177055 rs4502156d rs2007084
rs12899811 rs11634397 rs9923233 rs7202877 rs11651755 rs2447090 rs11663816 rs8182584 rs8108269 rs10401969 rs780094d rs7569522 rs2943640 rs243083 rs1128249 rs10203174 rs4812829 rs6795735 rs4402960 rs2197423
rs17301514 rs1496653 rs12497268 rs11717195d rs6819243 rs10012946 rs6878122 rs459193 rs7756992 rs4299828 rs3734621 rs849135 rs10229583 rs17168486 rs13233731 rs3824065 rs7845219 rs516946 rs3802177 rs2796441 rs17791513 rs16927668 rs10811661d rs10758593
rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411 rs7903146d
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2176598 rs734597 rs2245368 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964 rs4869272 rs11619319 rs983309 rs6943153 rs6113722 rs16913693 rs3829109 rs3783347 rs2302593 rs10747083 rs6072275
rs576674 rs11715915 rs17762454 rs2657879 rs560887 rs1280 rs2191349 rs2908289 rs10814916 rs11195502 rs11607883 rs11039182 rs174576 rs12255372 rs1436958 rs11672660 rs11782386 rs1019503;
roc 'Model 2 lifestyle' age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1;
roc 'Model 3 combined' age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs340874d rs7515431 rs7903146d rs12571751 rs12242953 rs11257655 rs1111875 rs5215 rs2334499 rs163184 rs1552224 rs10830963d rs7955901 rs2261181 rs7965349 rs11063069 rs10842994 rs1359790 rs7177055 rs4502156d rs2007084
rs12899811 rs11634397 rs9923233 rs7202877 rs11651755 rs2447090 rs11663816 rs8182584 rs8108269 rs10401969 rs780094d rs7569522 rs2943640 rs243083 rs1128249 rs10203174 rs4812829 rs6795735 rs4402960 rs2197423
rs17301514 rs1496653 rs12497268 rs11717195d rs6819243 rs10012946 rs6878122 rs459193 rs7756992 rs4299828 rs3734621 rs849135 rs10229583 rs17168486 rs13233731 rs3824065 rs7845219 rs516946 rs3802177 rs2796441 rs17791513 rs16927668 rs10811661d rs10758593
rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411 rs7903146d
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2176598 rs734597 rs2245368 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964 rs4869272 rs11619319 rs983309 rs6943153 rs6113722 rs16913693 rs3829109 rs3783347 rs2302593 rs10747083 rs6072275
rs576674 rs11715915 rs17762454 rs2657879 rs560887 rs1280 rs2191349 rs2908289 rs10814916 rs11195502 rs11607883 rs11039182 rs174576 rs12255372 rs1436958 rs11672660 rs11782386 rs1019503;
roccontrast adjacentpairs/ESTIMATE=ALLPAIRS;
run;
ods html sge=off;


proc logistic data= stuff.a9_impute;
class sex (ref=last)fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model T2Dincid (event='1')= age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 rs340874d rs7515431 rs7903146d rs12571751 rs12242953 rs11257655 rs1111875 rs5215 rs2334499 rs163184 rs1552224 rs10830963d rs7955901 rs2261181 rs7965349 rs11063069 rs10842994 rs1359790 rs7177055 rs4502156d rs2007084
rs12899811 rs11634397 rs9923233 rs7202877 rs11651755 rs2447090 rs11663816 rs8182584 rs8108269 rs10401969 rs780094d rs7569522 rs2943640 rs243083 rs1128249 rs10203174 rs4812829 rs6795735 rs4402960 rs2197423
rs17301514 rs1496653 rs12497268 rs11717195d rs6819243 rs10012946 rs6878122 rs459193 rs7756992 rs4299828 rs3734621 rs849135 rs10229583 rs17168486 rs13233731 rs3824065 rs7845219 rs516946 rs3802177 rs2796441 rs17791513 rs16927668 rs10811661d rs10758593
rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411 rs7903146d
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2176598 rs734597 rs2245368 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964 rs4869272 rs11619319 rs983309 rs6943153 rs6113722 rs16913693 rs3829109 rs3783347 rs2302593 rs10747083 rs6072275
rs576674 rs11715915 rs17762454 rs2657879 rs560887 rs1280 rs2191349 rs2908289 rs10814916 rs11195502 rs11607883 rs11039182 rs174576 rs12255372 rs1436958 rs11672660 rs11782386 rs1019503/ rsq ctable lackfit outroc= roct2dmgen;
roc;
run;
proc logistic data= stuff.a9_impute;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first) /param=ref;
model T2Dincid (event='1')= age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1/ rsq ctable lackfit outroc= roct2dmlif;
roc;
run;
proc logistic data= stuff.a9_impute;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first)fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first) /param=ref;
model T2Dincid (event='1')= age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs340874d rs7515431 rs7903146d rs12571751 rs12242953 rs11257655 rs1111875 rs5215 rs2334499 rs163184 rs1552224 rs10830963d rs7955901 rs2261181 rs7965349 rs11063069 rs10842994 rs1359790 rs7177055 rs4502156d rs2007084
rs12899811 rs11634397 rs9923233 rs7202877 rs11651755 rs2447090 rs11663816 rs8182584 rs8108269 rs10401969 rs780094d rs7569522 rs2943640 rs243083 rs1128249 rs10203174 rs4812829 rs6795735 rs4402960 rs2197423
rs17301514 rs1496653 rs12497268 rs11717195d rs6819243 rs10012946 rs6878122 rs459193 rs7756992 rs4299828 rs3734621 rs849135 rs10229583 rs17168486 rs13233731 rs3824065 rs7845219 rs516946 rs3802177 rs2796441 rs17791513 rs16927668 rs10811661d rs10758593
rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411 rs7903146d
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2176598 rs734597 rs2245368 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964 rs4869272 rs11619319 rs983309 rs6943153 rs6113722 rs16913693 rs3829109 rs3783347 rs2302593 rs10747083 rs6072275 
rs576674 rs11715915 rs17762454 rs2657879 rs560887 rs1280 rs2191349 rs2908289 rs10814916 rs11195502 rs11607883 rs11039182 rs174576 rs12255372 rs1436958 rs11672660 rs11782386 rs1019503/ rsq ctable lackfit outroc= roct2dmcom;
roc;
run;

proc export data= roct2dmcom
DBMS= xls
outfile= roct2dmcom Replace;
run;

/*NRI and IDI*/

/*create datasets for each trait*/

data nrit2dm; set stuff.a9_impute;
cnt+1;
run; 


/*T2D*/
proc logistic data= nrit2dm;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first) physact1 (ref=first)fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model T2Dincid (event='1')= age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1;
output out=m1 pred=p1;
run;
proc logistic data= nrit2dm;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first) physact1 (ref=first)fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model T2Dincid (event='1')= age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs340874d rs7515431 rs7903146d rs12571751 rs12242953 rs11257655 rs1111875 rs5215 rs2334499 rs163184 rs1552224 rs10830963d rs7955901 rs2261181 rs7965349 rs11063069 rs10842994 rs1359790 rs7177055 rs4502156d rs2007084
rs12899811 rs11634397 rs9923233 rs7202877 rs11651755 rs2447090 rs11663816 rs8182584 rs8108269 rs10401969 rs780094d rs7569522 rs2943640 rs243083 rs1128249 rs10203174 rs4812829 rs6795735 rs4402960 rs2197423
rs17301514 rs1496653 rs12497268 rs11717195d rs6819243 rs10012946 rs6878122 rs459193 rs7756992 rs4299828 rs3734621 rs849135 rs10229583 rs17168486 rs13233731 rs3824065 rs7845219 rs516946 rs3802177 rs2796441 rs17791513 rs16927668 rs10811661d rs10758593
rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411 rs7903146d
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2176598 rs734597 rs2245368 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964 rs4869272 rs11619319 rs983309 rs6943153 rs6113722 rs16913693 rs3829109 rs3783347 rs2302593 rs10747083 rs6072275
rs576674 rs11715915 rs17762454 rs2657879 rs560887 rs1280 rs2191349 rs2908289 rs10814916 rs11195502 rs11607883 rs11039182 rs174576 rs12255372 rs1436958 rs11672660 rs11782386 rs1019503;
output out=m2 pred=p2;
run;
proc sql; 
create table t2dm as select *
from m1 as a left join m2 as b on a.cnt=b.cnt;
quit;


/*run macro continous NRI*/

%macro CONTNRI(DSNAME,DETAIL,STATVAR,PROB1,PROB2);
* Macro to compute NRI with any up or down (no categories); 
* Control output of frequency tables and means below;
*  Variables:
*  DSNAME = dataset name;
*  DETAIL = 2 for detailed printout, 1 for limited, 0 for none;
*  STATVAR = outcome variable (coded 0,1);
*  PROB1 = probability for model 1;
*  PROB2 = probability for model 2;

title2 Test of Continuous NRI for &PROB2 vs &PROB1 ;

data nri1; set &dsname;
* compute diffs in probs;
stat=&STATVAR;
prob1=&PROB1;
prob2=&PROB2;
diffp=prob2-prob1;
if prob1>. and prob2>.;

if prob2>prob1 then disc=1;
  else if prob2=prob1 then disc=0;
  else if prob2<prob1 then disc=-1;
run;

*** Control output as needed ***;
%if &detail=2 %then %do;
proc means data=nri1 n sum mean median stddev min max;
var prob1 prob2 diffp disc stat;
run;
%end;

%if &detail=2 %then %do;
proc freq data=nri1;
tables stat*disc / chisq;
run;
%end;

ods listing exclude all;
ods output CrossTabFreqs=freqs;
ods output ChiSq=chisq;
run;
proc freq data=nri1;
tables stat*disc / chisq;
run;
ods listing exclude none;

data trend; set chisq;
* if df=1;
if Statistic="Mantel-Haenszel Chi-Square";
chitrend=Value;
ptrend=1-probchi(chitrend,1);
keep chitrend ptrend;
run;

%if &detail=2 %then %do;
proc print data=freqs;
run;
%end;

data nri; set freqs end=eof;
retain up_case down_case up_contr down_contr ncase ncontr 0;
if stat=1 and disc=1 then up_case=RowPercent/100;
   else if stat=1 and disc=-1 then down_case=RowPercent/100;
   else if stat=0 and disc=1 then up_contr=RowPercent/100;
   else if stat=0 and disc=-1 then down_contr=RowPercent/100;
* Number of cases and controls is the same for pd and pred;
if stat=1 and _type_='10' then ncase=Frequency;
   else if stat=0 and _type_='10' then ncontr=Frequency;

if eof then do;
  %if ncase=0 or ncontr=0 %then %do;
     nri=.; znri=.; p2nri=.;
     %goto exit;
  %end;
  ri_case=up_case-down_case;
  ri_contr=down_contr-up_contr;
  nri=ri_case+ri_contr;
  * corrected variance to reflect binomial;
  vri_case=4*up_case*(1-up_case)/ncase;
  vri_contr=4*up_contr*(1-up_contr)/ncontr;
  vnri=vri_case+vri_contr;
  senri=sqrt(vnri);
  nricil=nri-1.96*senri;
  nriciu=nri+1.96*senri;
  if vri_case ne 0 then zri_case=ri_case/sqrt(vri_case);
  if vri_contr ne 0 then zri_contr=ri_contr/sqrt(vri_contr);
  if vnri ne 0 then znri=nri/sqrt(vnri);
  p2ri_case=2*(1-probnorm(abs(zri_case)));
  p2ri_contr=2*(1-probnorm(abs(zri_contr)));
  p2nri=2*(1-probnorm(abs(znri)));
  output;
  keep ncase ncontr
       up_case down_case up_contr down_contr
       ri_case ri_contr nri vri_case vri_contr vnri senri nricil nriciu
       zri_case zri_contr znri p2ri_case p2ri_contr p2nri
       ;
end;

data _all_; merge nri trend ; 
run;

%if &detail>0 %then %do;
proc print data=_all_;
var    ncase ncontr 
       up_case down_case up_contr down_contr 
       ri_case ri_contr nri vri_case vri_contr vnri senri nricil nriciu
       zri_case zri_contr znri p2ri_case p2ri_contr p2nri
       chitrend ptrend
       ;
run;
%end;
title2; run;
* Usage:
* %contnri(probs,1,outxy,pdx,pdxy);
%exit: %mend CONTNRI;
%contnri(t2dm,1,T2Dincid,p1,p2);


/*run macro continous IDI*/
%macro IDIMACRO(DSNAME,DETAIL,OUT01,PROB1,PROB2);
*  Macro to compute difference in Yates slopes or
     integrated discrimination improvement (IDI) from Pencina, 2007;

*  Variables:
*  DSNAME = dataset name;
*  DETAIL = 1 or 2 for limited printout, 0 for none;
*  OUT01 = outcome variable (coded 0,1) (if 1,2 alter signs);
*  PROB1 = probability for model 1;
*  PROB2 = probability for model 2;

data ididat; set &dsname;
diffprob=&prob2-&prob1;
if &prob1>. and &prob2>.;
run;

%if &detail<2 %then %do;
ods listing exclude all;
%end;
ods output TTests=tstats;
ods output Statistics=unistats;
proc ttest data=ididat;
class &out01;
var &prob1 &prob2 diffprob;
title2 "Test of Difference in Yates Slope (IDI)";
run;

data idi1; set unistats end=eof;
retain yates1 yse1 ycil1 yciu1 yates2 yse2 ycil2 yciu2
       diffcase dcasese dcasecil dcaseciu diffcont dcontse dcontcil dcontciu
       idi idicil idiciu;
* Note: Yates slopes are negative since outcome coded 0,1;
* And CIs are reversed;
if _n_=3 then do;
  yates1=-Mean;
  yse1=StdErr;
  ycil1=-UpperCLMean;
  yciu1=-LowerCLMean;
end;
else if _n_=6 then do;
  yates2=-Mean;
  yse2=StdErr;
  ycil2=-UpperCLMean;
  yciu2=-LowerCLMean;
end;
else if _n_=8 then do;
  diffcase=Mean;
  dcasese=StdErr;
  dcasecil=LowerCLMean;
  dcaseciu=UpperCLMean;
end;
else if _n_=7 then do;
  diffcont=Mean;
  dcontse=StdErr;
  dcontcil=LowerCLMean;
  dcontciu=UpperCLMean;
end;
else if _n_=9 then do;
  idi=-Mean;
  idise=StdErr;
  idicil=-UpperCLMean;
  idiciu=-LowerCLMean;
end;
if eof then do;
   rel_idi=yates2/yates1 - 1;
   * Compute normal zscore based on Pencina;
   normse=sqrt((dcasese)**2 +(dcontse)**2);
   if normse>0 then zidi=idi/normse;
   p2idi=2*(1-probnorm(abs(zidi)));
   output;
end;
keep yates1 yse1 ycil1 yciu1 yates2 yse2 ycil2 yciu2
     diffcase dcasese dcasecil dcaseciu diffcont dcontse dcontcil dcontciu
     idi idise idicil idiciu rel_idi normse zidi p2idi;
run;

data idi2; set tstats end=eof;
if _n_=6 then do;
  idit=-tValue;
  idip=Probt;
  output;
  keep idit idip;
end;

data _idi_; merge idi1 idi2;
run;
ods listing exclude none;
%if &detail>0 %then %do;
proc print data=_idi_;
run;
%end;
title2; run;

* Usage:
* %idimacro(probs,1,outxy,pdx,pdxy,outxy);
%mend IDIMACRO;
%idimacro(t2dm,1,T2Dincid,p1,p2);


/**** ASSSOCIATION OF GENETIC AND LIFESTYLE FACTORS****/

/******************************
    CONSTRUCT DIET SCORES
******************************/

data stuff.rsscores;
set stuff.a8_impute;
/*nordic nutrition score (For vitamins range values are obtained form RI and UP)*/

/*baseline*/
PUFACal= .; 
PUFACal= 9*PUFA1;
PUFAE= .; 
PUFAE= (100*PUFACal)/TEI1;
ScPUFA=.;
if (PUFAE < 5) then ScPUFA= 0;
if (PUFAE >= 5) and (PUFAE <= 10) then ScPUFA= 1;
if (PUFAE > 10) then ScPUFA= 0;
if (PUFAE = .) then ScPUFA= .;
MUFACal= .; 
MUFACal= 9*MUFA1;
MUFAE= .; 
MUFAE= (100*MUFACal)/TEI1;
ScMUFA=.;
if (MUFAE < 10) then ScMUFA= 0;
if (MUFAE >= 10) and (MUFAE <= 20) then ScMUFA= 1;
if (MUFAE > 20) then ScMUFA= 0;
if (MUFAE = .) then ScMUFA= .;
SATFATCal= .; 
SATFATCal= 9*SATFAT1;
SATFATE= .; 
SATFATE= (100*SATFATCal)/TEI1;
ScSATFAT=.;
if (SATFATE <= 10) then ScSATFAT= 1;
if (SATFATE > 10) then ScSATFAT= 0;
if (SATFATE = .) then ScSATFAT= .;
TOTFATCal= .; 
TOTFATCal= 9*TOTFAT1;
TOTFATE= .; 
TOTFATE= (100*TOTFATCal)/TEI1;
ScTOTFAT=.;
if (TOTFATE < 25) then ScTOTFAT= 0;
if (TOTFATE >= 25) and (TOTFATE <= 40)then ScTOTFAT= 1;
if (TOTFATE > 40) then ScTOTFAT= 0;
if (TOTFATE = .) then ScTOTFAT= .;
ScFIBER=.;
if (FIBER1 < 25) then ScFIBER= 0;
if (FIBER1 >= 25) then ScFIBER= 1;
if (FIBER1 = .) then ScFIBER= .;
FACal= .; 
FACal= 9*FA1;
FAE= .; 
FAE= (100*FACal)/TEI1;
ScFA=.;
if (FAE < 3) then ScFA= 0;
if (FAE >= 3)then ScFA= 1;
if (FAE = .) then ScFA= .;
ScSALT= .;
if (SALT1 <= 6) then ScSALT= 1;
if (SALT1 > 6) then SCSALT= 0;
CARBOHYDRATESCal= .; 
CARBOHYDRATESCal= 4*CARBOHYDRATES1;
CARBOHYDRATESE= .; 
CARBOHYDRATESE= (100*CARBOHYDRATESCal)/TEI1;
ScCARBOHYDRATES=.;
if (CARBOHYDRATESE < 45) then ScCARBOHYDRATES= 0;
if (CARBOHYDRATESE >= 45)and (CARBOHYDRATESE <= 60) then ScCARBOHYDRATES= 1;
if (CARBOHYDRATESE > 60) then ScCARBOHYDRATES= 0;
if (CARBOHYDRATESE = .) then ScCARBOHYDRATES= .;
SUGARSCal= .; 
SUGARSCal= 4*SUGARS1;
SUGARSE= .; 
SUGARSE= (100*SUGARSCal)/TEI1;
ScSUGARS=.;
if (SUGARSE < 10) then ScSUGARS= 1;
if (SUGARSE >= 10) then ScSUGARS= 0;
if (SUGARSE = .) then ScSUGARS= .;
PROTEINCal= .; 
PROTEINCal= 4*PROTEIN1;
PROTEINE= .; 
PROTEINE= (100*PROTEINCal)/TEI1;
ScPROTEIN=.;
if (PROTEINE < 10) then ScPROTEIN= 0;
if (PROTEINE >= 10)and (PROTEINE <= 20) then ScPROTEIN= 1;
if (PROTEINE > 20) then ScPROTEIN= 0;
if (PROTEINE = .) then ScPROTEIN= .;
ScVitA=.;
if (SEX = 1) and (VitA1 < 0.900) then ScVitA= 0;
if (SEX = 1) and (VitA1 >= 0.900) and (VitA1 <= 3.000)then ScVitA= 1;
if (SEX = 1) and (VitA1 > 3.000) then ScVitA= 0;
if (SEX = 0) and (VitA1 < 0.700) then ScVitA= 0;
if (SEX = 0) and (VitA1 >= 0.700) and (VitA1 <= 3.000)then ScVitA= 1;
if (SEX = 0) and (VitA1 > 3.000) then ScVitA= 0;
if (VitA1= .) then ScVitA= .;
ScVitD=.;
if (VitD1 < 10) then ScVitD= 0;
if (VitD1 >= 10) and (VitD1 <= 100) then ScVitD= 1;
if (VitD1 > 100) then ScVitD= 0;
if (VitD1 = .) then ScVitD= .;
ScVitE=.;
if (SEX = 1) and (VitE1 < 10) then ScVitE= 0;
if (SEX = 1) and (VitE1 >= 10) and (VitE1 <= 300)then ScVitE= 1;
if (SEX = 1) and (VitE1 > 300) then ScVitE= 0;
if (SEX = 0) and (VitE1 < 8) then ScVitE= 0;
if (SEX = 0) and (VitE1 >= 8) and (VitE1 <= 300)then ScVitE= 1;
if (SEX = 0) and (VitE1 > 300) then ScVitE= 0;
if (VitE1= .) then ScVitE= .;
ScThia= .;
if (SEX = 1) and (Thia1 < 1.3) then ScThia= 0;
if (SEX = 1) and (Thia1 >= 1.3) then ScThia= 1;
if (SEX = 0) and (Thia1 < 1.1) then ScThia= 0;
if (SEX = 0) and (Thia1 >= 1.1) then ScThia= 1;
if (Thia1= .) then ScThia= .;
ScRibo= .;
if (SEX = 1) and (Ribo1 < 1.5) then ScRibo= 0;
if (SEX = 1) and (Ribo1 >= 1.5) then ScRibo= 1;
if (SEX = 0) and (Ribo1 < 1.2) then ScRibo= 0;
if (SEX = 0) and (Ribo1 >= 1.2) then ScRibo= 1;
if (Ribo1= .) then ScRibo= .;
ScNiac= .;
if (SEX = 1) and (Niac1 < 18) then ScNiac= 0;
if (SEX = 1) and (Niac1 >= 18) and (Niac1 <= 900)then ScNiac= 1;
if (SEX = 1) and (Niac1 > 900) then ScNiac= 0;
if (SEX = 0) and (Niac1 < 14) then ScNiac= 0;
if (SEX = 0) and (Niac1 >= 14) and (Niac1 <= 900)then ScNiac= 1;
if (SEX = 0) and (Niac1 > 900) then ScNiac= 0;
if (Niac1= .) then ScNiac= .;
ScVitB6= .;
if (SEX = 1) and (VitB61 < 1.5) then ScVitB6= 0;
if (SEX = 1) and (VitB61 >= 1.5) and (VitB61 <= 25)then ScVitB6= 1;
if (SEX = 1) and (VitB61 > 25) then ScVitB6= 0;
if (SEX = 0) and (VitB61 < 1.2) then ScVitB6= 0;
if (SEX = 0) and (VitB61 >= 1.2) and (VitB61 <= 25)then ScVitB6= 1;
if (SEX = 0) and (VitB61 > 25) then ScVitB6= 0;
if (VitB61= .) then ScVitB6= .;
ScFolate= .; 
if (Folate1 < 300) then ScFolate= 0;
if (Folate1 >= 300) and (Folate1 <= 1000) then ScFolate= 1;
if (Folate1 > 1000) then ScFolate= 0;
if (Folate1 = .) then ScFolate= .;
ScVitB12= .;
if (VitB121 < 2) then ScVitB12= 0;
if (VitB121 >= 2) then ScVitB12= 1;
if (VitB121 = .) then ScVitB12= .;
ScVitC= .;
if (VitC1 < 75) then ScVitC= 0;
if (VitC1 >= 75) and (VitC1 <= 1000) then ScVitC= 1;
if (VitC1 > 1000) then ScVitC= 0;
if (VitC1 = .) then ScVitC= .;
ScCalc= .;
if (Calc1 < 800) then ScCalc= 0;
if (Calc1 >= 800) and (Calc1 <= 2500) then ScCalc= 1;
if (Calc1 > 2500) then ScCalc= 0;
if (Calc1 = .) then ScCalc= .;
ScPhosp= .; 
if (Phosp1 < 600) then ScPhosp= 0;
if (Phosp1 >= 600) and (Phosp1 <= 3000) then ScPhosp= 1;
if (Phosp1 > 3000) then ScPhosp= 0;
if (Phosp1 = .) then ScPhosp= .;
ScPota= .;
if (SEX = 1) and (Pota1 < 3500) then ScPota= 0;
if (SEX = 1) and (Pota1 >= 3500) and (Pota1 <= 3700)then ScPota= 1;
if (SEX = 1) and (Pota1 > 3700) then ScPota= 0;
if (SEX = 0) and (Pota1 < 3100) then ScPota= 0;
if (SEX = 0) and (Pota1 >= 3100) and (Pota1 <= 3700)then ScPota= 1;
if (SEX = 0) and (Pota1 > 3700) then ScPota= 0;
if (Pota1= .) then ScPota= .;
ScMagn= .;
if (SEX = 1) and (Magn1 < 350) then ScMagn= 0;
if (SEX = 1) and (Magn1 >= 350) then ScMagn= 1;
if (SEX = 0) and (Magn1 < 280) then ScMagn= 0;
if (SEX = 0) and (Magn1 >= 280) then ScMagn= 1;
if (Magn1= .) then ScMagn= .;
ScIron= .;
if (Iron1 < 9) then ScIron= 0;
if (Iron1 >= 9) and (Iron1 <= 25) then ScIron= 1;
if (Iron1 > 25) then ScIron= 0;
if (Iron1 = .) then ScIron= .;
ScZinc= .;
if (SEX = 1) and (Zinc1 < 9) then ScZinc= 0;
if (SEX = 1) and (Zinc1 >= 9) and (Zinc1 <= 25)then ScZinc= 1;
if (SEX = 1) and (Zinc1 > 25) then ScZinc= 0;
if (SEX = 0) and (Zinc1 < 7) then ScZinc= 0;
if (SEX = 0) and (Zinc1 >= 7) and (Zinc1 <= 25)then ScZinc= 1;
if (SEX = 0) and (Zinc1 > 25) then ScZinc= 0;
if (Zinc1= .) then ScZinc= .;
ScIodine= .;
if (Iodine1 < 150) then ScIodine= 0;
if (Iodine1 >= 150) and (Iodine1 <= 600) then ScIodine= 1;
if (Iodine1 > 600) then ScIodine= 0;
if (Iodine1 = .) then ScIodine= .;
ScSelenium= .;
if (SEX = 1) and (Selenium1 < 60) then ScSelenium= 0;
if (SEX = 1) and (Selenium1 >= 60) and (Selenium1 <= 300)then ScSelenium= 1;
if (SEX = 1) and (Selenium1 > 300) then ScSelenium= 0;
if (SEX = 0) and (Selenium1 < 50) then ScSelenium= 0;
if (SEX = 0) and (Selenium1 >= 50) and (Selenium1 <= 300)then ScSelenium= 1;
if (SEX = 0) and (Selenium1 > 300) then ScSelenium= 0;
if (Selenium1= .) then ScSelenium= .;

/*follow-up*/
PUFACal2= .; 
PUFACal2= 9*PUFA2;
PUFAE2= .; 
PUFAE2= (100*PUFACal2)/TEI2;
ScPUFA2=.;
if (PUFAE2 < 5) then ScPUFA2= 0;
if (PUFAE2 >= 5) and (PUFAE2 <= 10) then ScPUFA2= 1;
if (PUFAE2 > 10) then ScPUFA2= 0;
if (PUFAE2 = .) then ScPUFA2= .;
MUFACal2= .; 
MUFACal2= 9*MUFA2;
MUFAE2= .; 
MUFAE2= (100*MUFACal2)/TEI2;
ScMUFA2=.;
if (MUFAE2 < 10) then ScMUFA2= 0;
if (MUFAE2 >= 10) and (MUFAE2 <= 20) then ScMUFA2= 1;
if (MUFAE2 > 20) then ScMUFA2= 0;
if (MUFAE2 = .) then ScMUFA2= .;
SATFATCal2= .; 
SATFATCal2= 9*SATFAT2;
SATFATE2= .; 
SATFATE2= (100*SATFATCal2)/TEI2;
ScSATFAT2=.;
if (SATFATE2 <= 10) then ScSATFAT2= 1;
if (SATFATE2 > 10) then ScSATFAT2= 0;
if (SATFATE2 = .) then ScSATFAT2= .;
TOTFATCal2= .; 
TOTFATCal2= 9*TOTFAT2;
TOTFATE2= .; 
TOTFATE2= (100*TOTFATCal2)/TEI2;
ScTOTFAT2=.;
if (TOTFATE2 < 25) then ScTOTFAT2= 0;
if (TOTFATE2 >= 25) and (TOTFATE2 <= 40)then ScTOTFAT2= 1;
if (TOTFATE2 > 40) then ScTOTFAT2= 0;
if (TOTFATE2 = .) then ScTOTFAT2= .;
ScFIBER2=.;
if (FIBER2 < 25) then ScFIBER2= 0;
if (FIBER2 >= 25) then ScFIBER2= 1;
if (FIBER2 = .) then ScFIBER2= .;
FACal2= .; 
FACal2= 9*FA2;
FAE2= .; 
FAE2= (100*FACal2)/TEI2;
ScFA2=.;
if (FAE2 < 3) then ScFA2= 0;
if (FAE2 >= 3)then ScFA2= 1;
if (FAE2 = .) then ScFA2= .;
ScSALT2= .;
if (SALT2 <= 6) then ScSALT2= 1;
if (SALT2 > 6) then SCSALT2= 0;
CARBOHYDRATESCal2= .; 
CARBOHYDRATESCal2= 4*CARBOHYDRATES2;
CARBOHYDRATESE2= .; 
CARBOHYDRATESE2= (100*CARBOHYDRATESCal2)/TEI2;
ScCARBOHYDRATES2=.;
if (CARBOHYDRATESE2 < 45) then ScCARBOHYDRATES2= 0;
if (CARBOHYDRATESE2 >= 45)and (CARBOHYDRATESE2 <= 60) then ScCARBOHYDRATES2= 1;
if (CARBOHYDRATESE2 > 60) then ScCARBOHYDRATES2= 0;
if (CARBOHYDRATESE2 = .) then ScCARBOHYDRATES2= .;
SUGARSCal2= .; 
SUGARSCal2= 4*SUGARS2;
SUGARSE2= .; 
SUGARSE2= (100*SUGARSCal2)/TEI2;
ScSUGARS2=.;
if (SUGARSE2 < 10) then ScSUGARS2= 1;
if (SUGARSE2 >= 10) then ScSUGARS2= 0;
if (SUGARSE2 = .) then ScSUGARS2= .;
PROTEINCal2= .; 
PROTEINCal2= 4*PROTEIN2;
PROTEINE2= .; 
PROTEINE2= (100*PROTEINCal)/TEI2;
ScPROTEIN2=.;
if (PROTEINE2 < 10) then ScPROTEIN2= 0;
if (PROTEINE2 >= 10)and (PROTEINE2 <= 20) then ScPROTEIN2= 1;
if (PROTEINE2 > 20) then ScPROTEIN2= 0;
if (PROTEINE2 = .) then ScPROTEIN2= .;
ScVitA2=.;
if (SEX = 1) and (VitA2 < 0.900) then ScVitA2= 0;
if (SEX = 1) and (VitA2 >= 0.900) and (VitA2 <= 3.000)then ScVitA2= 1;
if (SEX = 1) and (VitA2 > 3.000) then ScVitA2= 0;
if (SEX = 0) and (VitA2 < 0.700) then ScVitA2= 0;
if (SEX = 0) and (VitA2 >= 0.700) and (VitA2 <= 3.000)then ScVitA2= 1;
if (SEX = 0) and (VitA2 > 3.000) then ScVitA2= 0;
if (VitA2= .) then ScVitA2= .;
ScVitD2=.;
if (VitD2 < 10) then ScVitD2= 0;
if (VitD2 >= 10) and (VitD2 <= 100) then ScVitD2= 1;
if (VitD2 > 100) then ScVitD2= 0;
if (VitD2 = .) then ScVitD2= .;
ScVitE2=.;
if (SEX = 1) and (VitE2 < 10) then ScVitE2= 0;
if (SEX = 1) and (VitE2 >= 10) and (VitE2 <= 300)then ScVitE2= 1;
if (SEX = 1) and (VitE2 > 300) then ScVitE2= 0;
if (SEX = 0) and (VitE2 < 8) then ScVitE2= 0;
if (SEX = 0) and (VitE2 >= 8) and (VitE2 <= 300)then ScVitE2= 1;
if (SEX = 0) and (VitE2 > 300) then ScVitE2= 0;
if (VitE2= .) then ScVitE2= .;
ScThia2= .;
if (SEX = 1) and (Thia2 < 1.3) then ScThia2= 0;
if (SEX = 1) and (Thia2 >= 1.3) then ScThia2= 1;
if (SEX = 0) and (Thia2 < 1.1) then ScThia2= 0;
if (SEX = 0) and (Thia2 >= 1.1) then ScThia2= 1;
if (Thia2= .) then ScThia2= .;
ScRibo2= .;
if (SEX = 1) and (Ribo2 < 1.5) then ScRibo2= 0;
if (SEX = 1) and (Ribo2 >= 1.5) then ScRibo2= 1;
if (SEX = 0) and (Ribo2 < 1.2) then ScRibo2= 0;
if (SEX = 0) and (Ribo2 >= 1.2) then ScRibo2= 1;
if (Ribo2= .) then ScRibo2= .;
ScNiac2= .;
if (SEX = 1) and (Niac2 < 18) then ScNiac2= 0;
if (SEX = 1) and (Niac2 >= 18) and (Niac2 <= 900)then ScNiac2= 1;
if (SEX = 1) and (Niac2 > 900) then ScNiac2= 0;
if (SEX = 0) and (Niac2 < 14) then ScNiac2= 0;
if (SEX = 0) and (Niac2 >= 14) and (Niac2 <= 900)then ScNiac2= 1;
if (SEX = 0) and (Niac2 > 900) then ScNiac2= 0;
if (Niac2= .) then ScNiac2= .;
ScVitB62= .;
if (SEX = 1) and (VitB62 < 1.5) then ScVitB62= 0;
if (SEX = 1) and (VitB62 >= 1.5) and (VitB62 <= 25)then ScVitB62= 1;
if (SEX = 1) and (VitB62 > 25) then ScVitB62= 0;
if (SEX = 0) and (VitB62 < 1.2) then ScVitB62= 0;
if (SEX = 0) and (VitB62 >= 1.2) and (VitB62 <= 25)then ScVitB62= 1;
if (SEX = 0) and (VitB62 > 25) then ScVitB62= 0;
if (VitB62= .) then ScVitB62= .;
ScFolate2= .; 
if (Folate2 < 300) then ScFolate2= 0;
if (Folate2 >= 300) and (Folate2 <= 1000) then ScFolate2= 1;
if (Folate2 > 1000) then ScFolate2= 0;
if (Folate2 = .) then ScFolate2= .;
ScVitB122= .;
if (VitB122 < 2) then ScVitB122= 0;
if (VitB122 >= 2) then ScVitB122= 1;
if (VitB122 = .) then ScVitB122= .;
ScVitC2= .;
if (VitC2 < 75) then ScVitC2= 0;
if (VitC2 >= 75) and (VitC2 <= 1000) then ScVitC2= 1;
if (VitC2 > 1000) then ScVitC2= 0;
if (VitC2 = .) then ScVitC2= .;
ScCalc2= .;
if (Calc2 < 800) then ScCalc2= 0;
if (Calc2 >= 800) and (Calc2 <= 2500) then ScCalc2= 1;
if (Calc2 > 2500) then ScCalc2= 0;
if (Calc2 = .) then ScCalc2= .;
ScPhosp2= .; 
if (Phosp2 < 600) then ScPhosp2= 0;
if (Phosp2 >= 600) and (Phosp2 <= 3000) then ScPhosp2= 1;
if (Phosp2 > 3000) then ScPhosp2= 0;
if (Phosp2 = .) then ScPhosp2= .;
ScPota2= .;
if (SEX = 1) and (Pota2 < 3500) then ScPota2= 0;
if (SEX = 1) and (Pota2 >= 3500) and (Pota2 <= 3700)then ScPota2= 1;
if (SEX = 1) and (Pota2 > 3700) then ScPota2= 0;
if (SEX = 0) and (Pota2 < 3100) then ScPota2= 0;
if (SEX = 0) and (Pota2 >= 3100) and (Pota2 <= 3700)then ScPota2= 1;
if (SEX = 0) and (Pota2 > 3700) then ScPota2= 0;
if (Pota2= .) then ScPota2= .;
ScMagn2= .;
if (SEX = 1) and (Magn2 < 350) then ScMagn2= 0;
if (SEX = 1) and (Magn2 >= 350) then ScMagn2= 1;
if (SEX = 0) and (Magn2 < 280) then ScMagn2= 0;
if (SEX = 0) and (Magn2 >= 280) then ScMagn2= 1;
if (Magn2= .) then ScMagn2= .;
ScIron2= .;
if (Iron2 < 9) then ScIron2= 0;
if (Iron2 >= 9) and (Iron2 <= 25) then ScIron2= 1;
if (Iron2 > 25) then ScIron2= 0;
if (Iron2 = .) then ScIron2= .;
ScZinc2= .;
if (SEX = 1) and (Zinc2 < 9) then ScZinc2= 0;
if (SEX = 1) and (Zinc2 >= 9) and (Zinc2 <= 25)then ScZinc2= 1;
if (SEX = 1) and (Zinc2 > 25) then ScZinc2= 0;
if (SEX = 0) and (Zinc2 < 7) then ScZinc2= 0;
if (SEX = 0) and (Zinc2 >= 7) and (Zinc2 <= 25)then ScZinc2= 1;
if (SEX = 0) and (Zinc2 > 25) then ScZinc2= 0;
if (Zinc2= .) then ScZinc2= .;
ScIodine2= .;
if (Iodine2 < 150) then ScIodine2= 0;
if (Iodine2 >= 150) and (Iodine2 <= 600) then ScIodine2= 1;
if (Iodine2 > 600) then ScIodine2= 0;
if (Iodine2 = .) then ScIodine2= .;
ScSelenium2= .;
if (SEX = 1) and (Selenium2 < 60) then ScSelenium2= 0;
if (SEX = 1) and (Selenium2 >= 60) and (Selenium2 <= 300)then ScSelenium2= 1;
if (SEX = 1) and (Selenium2 > 300) then ScSelenium2= 0;
if (SEX = 0) and (Selenium2 < 50) then ScSelenium2= 0;
if (SEX = 0) and (Selenium2 >= 50) and (Selenium2 <= 300)then ScSelenium2= 1;
if (SEX = 0) and (Selenium2 > 300) then ScSelenium2= 0;
if (Selenium2= .) then ScSelenium2= .;
NNRdietscore=sum (of ScPUFA, ScMUFA, ScSATFAT, ScTOTFAT, ScFIBER, ScCARBOHYDRATES, ScFA, ScSALT, ScSUGARS, ScPROTEIN, ScVitA, ScVitD, ScVitE, ScThia, ScRibo, ScNiac, ScVitB6, ScFolate, ScVitB12, ScVitC, ScCalc, ScPhosp, ScPota, ScMagn, ScIron, ScZinc, ScIodine, ScSelenium) ;
NNRdietscoref=sum (of ScPUFA2, ScMUFA2, ScSATFAT2, ScTOTFAT2, ScFIBER2, ScCARBOHYDRATES2, ScFA2, ScSALT2, ScSUGARS2, ScPROTEIN2, ScVitA2, ScVitD2, ScVitE2, ScThia2, ScRibo2, ScNiac2, ScVitB62, ScFolate2, ScVitB122, ScVitC2, ScCalc2, ScPhosp2, ScPota2, ScMagn2, ScIron2, ScZinc2, ScIodine2, ScSelenium2) ;
run;

/*number of individuals following the NNR guidelines*/
proc freq data= stuff.rsscores;
table ScPUFA*sex ScMUFA*sex ScSATFAT*sex ScTOTFAT*sex ScFIBER*sex ScCARBOHYDRATES*sex ScFA*sex ScSALT*sex ScSUGARS*sex ScPROTEIN*sex ScVitA*sex ScVitD*sex ScVitE*sex ScThia*sex ScRibo*sex ScNiac*sex ScVitB6*sex ScFolate*sex ScVitB12*sex ScVitC*sex ScCalc*sex ScPhosp*sex ScPota*sex ScMagn*sex ScIron*sex ScZinc*sex ScIodine*sex ScSelenium*sex;
run;

/*delete individuals with no information in each variable*/
/*baseline*/
data rsscor1; set stuff.rsscores;
if ScPUFA eq . then delete; 
if ScMUFA eq . then delete;
if ScSATFAT eq . then delete;
if ScTOTFAT eq . then delete; 
if ScFIBER eq . then delete;
if ScCARBOHYDRATES eq . then delete; 
if ScFA eq . then delete; 
if ScSALT eq . then delete; 
if ScSUGARS eq . then delete; 
if ScPROTEIN eq . then delete; 
if ScVitA eq . then delete;
if ScVitD eq . then delete; 
if ScVitE eq . then delete;
if ScThia eq . then delete;
if ScRibo eq . then delete;
if ScNiac eq . then delete;
if ScVitB6 eq . then delete; 
if ScFolate eq . then delete;
if ScVitB12 eq . then delete; 
if ScVitC eq . then delete;
if ScCalc eq . then delete; 
if ScPhosp eq . then delete;
if ScPota eq . then delete; 
if ScMagn eq . then delete;
if ScIron eq . then delete;
if ScZinc eq . then delete;
if ScIodine eq . then delete;
if ScSelenium eq . then delete;
NNRdietscore1= sum (of ScPUFA, ScMUFA, ScSATFAT, ScTOTFAT, ScFIBER, ScCARBOHYDRATES, ScFA, ScSALT, ScSUGARS, ScPROTEIN, ScVitA, ScVitD, ScVitE, ScThia, ScRibo, ScNiac, ScVitB6, ScFolate, ScVitB12, ScVitC, ScCalc, ScPhosp, ScPota, ScMagn, ScIron, ScZinc, ScIodine, ScSelenium) ;
run;

/*follow-up*/
data rsscor2; set stuff.rsscores;
if ScPUFA2 eq . then delete; 
if ScMUFA2 eq . then delete;
if ScSATFAT2 eq . then delete;
if ScTOTFAT2 eq . then delete; 
if ScFIBER2 eq . then delete;
if ScCARBOHYDRATES2 eq . then delete; 
if ScFA2 eq . then delete; 
if ScSALT2 eq . then delete; 
if ScSUGARS2 eq . then delete; 
if ScPROTEIN2 eq . then delete; 
if ScVitA2 eq . then delete;
if ScVitD2 eq . then delete; 
if ScVitE2 eq . then delete;
if ScThia2 eq . then delete;
if ScRibo2 eq . then delete;
if ScNiac2 eq . then delete;
if ScVitB62 eq . then delete; 
if ScFolate2 eq . then delete;
if ScVitB122 eq . then delete; 
if ScVitC2 eq . then delete;
if ScCalc2 eq . then delete; 
if ScPhosp2 eq . then delete;
if ScPota2 eq . then delete; 
if ScMagn2 eq . then delete;
if ScIron2 eq . then delete;
if ScZinc2 eq . then delete;
if ScIodine2 eq . then delete;
if ScSelenium2 eq . then delete;
NNRdietscore2= sum (of ScPUFA2, ScMUFA2, ScSATFAT2, ScTOTFAT2, ScFIBER2, ScCARBOHYDRATES2, ScFA2, ScSALT2, ScSUGARS2, ScPROTEIN2, ScVitA2, ScVitD2, ScVitE2, ScThia2, ScRibo2, ScNiac2, ScVitB62, ScFolate2, ScVitB122, ScVitC2, ScCalc2, ScPhosp2, ScPota2, ScMagn2, ScIron2, ScZinc2, ScIodine2, ScSelenium2) ;
run;

/*merge score with the rest of the data*/
proc sort data=stuff.rsscores;
by ID;
run;
proc sort data= rsscor1;
by ID;
run;
proc sort data= rsscor2;
by ID;
run;
data stuff.rsscores;
merge stuff.rsscores rsscor1 rsscor2;
by id;
run;

/*healthy diet score*/
proc rank data= stuff.rsscores group = 4 out=stuff.rsscores;
VAR wholegrain1 fish1 fruit1 vegetables1 redmeat1 desserts1 sugardrink1 friedpot1 wholegrain2 fish2 fruit2 vegetables2 redmeat2 desserts2 sugardrink2 friedpot2;
ranks QT_wholegrain1 QT_fish1 QT_fruit1 QT_vegetables1 QT_redmeat1 QT_desserts1 QT_sugardrink1 QT_friedpot1 QT_wholegrain2 QT_fish2 QT_fruit2 QT_vegetables2 QT_redmeat2 QT_desserts2 QT_sugardrink2 QT_friedpot2;
run;

/* Recode 'bad' foods */
data stuff.rsscores; set stuff.rsscores;

if QT_redmeat1=0 then RQT_redmeat1=3;
if QT_redmeat1=1 then RQT_redmeat1=2;
if QT_redmeat1=2 then RQT_redmeat1=1;
if QT_redmeat1=3 then RQT_redmeat1=0;

if QT_redmeat2=0 then RQT_redmeat2=3;
if QT_redmeat2=1 then RQT_redmeat2=2;
if QT_redmeat2=2 then RQT_redmeat2=1;
if QT_redmeat2=3 then RQT_redmeat2=0;


if QT_desserts1=0 then RQT_desserts1=3;
if QT_desserts1=1 then RQT_desserts1=2;
if QT_desserts1=2 then RQT_desserts1=1;
if QT_desserts1=3 then RQT_desserts1=0;

if QT_desserts2=0 then RQT_desserts2=3;
if QT_desserts2=1 then RQT_desserts2=2;
if QT_desserts2=2 then RQT_desserts2=1;
if QT_desserts2=3 then RQT_desserts2=0;


if QT_sugardrink1=0 then RQT_sugardrink1=3;
if QT_sugardrink1=1 then RQT_sugardrink1=2;
if QT_sugardrink1=2 then RQT_sugardrink1=1;
if QT_sugardrink1=3 then RQT_sugardrink1=0;

if QT_sugardrink2=0 then RQT_sugardrink2=3;
if QT_sugardrink2=1 then RQT_sugardrink2=2;
if QT_sugardrink2=2 then RQT_sugardrink2=1;
if QT_sugardrink2=3 then RQT_sugardrink2=0;


if QT_friedpot1=0 then RQT_friedpot1=3;
if QT_friedpot1=1 then RQT_friedpot1=2;
if QT_friedpot1=2 then RQT_friedpot1=1;
if QT_friedpot1=3 then RQT_friedpot1=0;

if QT_friedpot2=0 then RQT_friedpot2=3;
if QT_friedpot2=1 then RQT_friedpot2=2;
if QT_friedpot2=2 then RQT_friedpot2=1;
if QT_friedpot2=3 then RQT_friedpot2=0;

run;

/* check sucessful recoding*/
proc freq data= stuff.rsscores; tables QT_redmeat1*RQT_redmeat1 QT_desserts1*RQT_desserts1 QT_sugardrink1*RQT_sugardrink1 QT_friedpot1*RQT_friedpot1 QT_redmeat2*RQT_redmeat2 QT_desserts2*RQT_desserts2 QT_sugardrink2*RQT_sugardrink2 QT_friedpot2*RQT_friedpot2; run;

data stuff.rsscores; set stuff.rsscores;
HDdietscore1=sum (of QT_wholegrain1, QT_fish1, QT_fruit1, QT_vegetables1, RQT_redmeat1, RQT_desserts1, RQT_sugardrink1, RQT_friedpot1);
HDdietscore2=sum (of QT_wholegrain2, QT_fish2, QT_fruit2, QT_vegetables2, RQT_redmeat2, RQT_desserts2, RQT_sugardrink2, RQT_friedpot2);
keep id fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 DATUM1 DATUM2 enk1 enk2 age1 age2 weight1 AGESQ01 AGESQ02 sex fglu1 twoglu1 fglu2 twoglu2 BMI1 QT_alc1 QT_alc2 SMOKE1 EDUCATION1 PHYSACT1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 TEI1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 VitA1 VitD1 VitE1 Thia1 Ribo1 Niac1 VitB61 Folate1 VitB121 VitC1 Calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 BMI2  visit1 visit2  SMOKE2 EDUCATION2 PHYSACT2 MUFA2 PUFA2 CARBOHYDRATES2 SUGARS2 PROTEIN2 TEI2 SATFAT2 TOTFAT2 FIBER2 FA2 ALC2 SALT2 VitA2 VitD2 VitE2 Thia2 Ribo2 Niac2 VitB62 Folate2 VitB122 VitC2 Calc2 Phosp2 Pota2 Magn2 Iron2 Zinc2 Iodine2 Selenium2  
deltaBMI deltaweight lyear ob_GRS tglu_GRS fglu_GRS d_GRS m_GRS FGworsen GTworsen T2Dincid T2D1 T2D2 FG1 GT1 FG2 GT2 IDFGincid IDGTincid deltafglu deltatwoglu obov1 obov2 obes1 obes2 obesincid c1 c2 c3 c4 wholegrain1 fish1 fruit1 vegetables1 redmeat1 desserts1 sugardrink1 friedpot1 wholegrain2 fish2 fruit2 vegetables2 redmeat2 desserts2 sugardrink2 friedpot2 NNRdietscore NNRdietscore1 NNRdietscore2 HDdietscore1 HDdietscore2;
run; 

/*PCA*/
PROC FACTOR data= stuff.rsscores method= prin mineigen= 1 nfactors=10 out= stuff.rsscores1 msa; 
var  CARBOHYDRATES1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 MUFA1 PUFA1 FA1;
partial tei1;
run;

/*create the variable of 10% of weight gain*/
proc means data= stuff.rsscores1; var deltaweight; run;
data stuff.rsscores1; set stuff.rsscores1;
percweightch= .; percweightch= (100*deltaweight)/weight1;
wch10= .;
if percweightch < 10 then wch10= 0;
if percweightch >= 10 then wch10= 1;
if percweightch = . then wch10= .;
run;
proc means data= stuff.rsscores1; var deltaweight percweightch; run;
proc freq data= stuff.rsscores1; table wch10 obesincid; run;
/************************************
correlations between the three scores
**************************************/
proc corr data= stuff.rsscores1 SPEARMAN NOSIMPLE;
var NNRdietscore1 HDdietscore1 Factor1;  
partial age1 agesq01 sex tei1 enk1;
run;

/******************************
basic descriptives for scores
******************************/

data followup_sc; set stuff.rsscores1;
if visit2 eq 2;
run;
proc means data= followup_sc; var NNRdietscore2 HDdietscore2; run;
proc means data= followup_sc; var NNRdietscore1 HDdietscore1; run;
proc sort data= followup_sc; by sex; run;
proc means data= followup_sc; var NNRdietscore1 HDdietscore1; by sex; run;
proc sort data= followup_sc; by id; run;

data diffbff; set followup_sc;
diffNNR= NNRdietscore1-NNRdietscore2;
diffHD= HDdietscore1-HDdietscore2;
run;

proc univariate data= diffbff;
var diffNNR diffHD;
run;

/*******************
create QUARTILES of scores
********************/
proc rank data= stuff.rsscores1 group = 4 out=stuff.rsscores1;
VAR ob_GRS tglu_GRS fglu_GRS d_GRS m_GRS NNRdietscore1 HDdietscore1 factor1;
ranks QT_ob_GRS QT_tglu_GRS QT_fglu_GRS QT_d_GRS QT_m_GRS QT_NNRdietscore1 QT_HDdietscore1 QT_factor1;
run;


/*************************************************
    FOLLOW-UP ASSOCIATIONS OF SCORES WITH OBESITY INCIDENCE
**************************************************/
data rsscores1NNR; set stuff.rsscores1; if QT_NNRdietscore1 ne .; if agesq01 ne .; if lyear ne .; if smoke1 ne .; if education1 ne .; if QT_alc1 ne .; if enk1 ne .; if physact1 ne .; run;
data rsscores1HD; set stuff.rsscores1; if QT_HDdietscore1 ne .; if agesq01 ne .; if lyear ne .; if smoke1 ne .; if education1 ne .; if QT_alc1 ne .; if enk1 ne .; if physact1 ne .; run;
data rsscores1PCA; set stuff.rsscores1; if QT_factor1 ne .; if agesq01 ne .; if lyear ne .; if smoke1 ne .; if education1 ne .; if QT_alc1 ne .; if enk1 ne .; if physact1 ne .; run;


proc logistic data= rsscores1NNR;
class sex (ref=last)QT_ob_GRS (ref=first)/param=ref;
model obesincid (event='1')= age1 agesq01 lyear sex QT_ob_GRS/ rsq ctable lackfit outroc= rocGRS12;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first) QT_alc1 (ref=first) QT_NNRdietscore1 (ref=first)/param=ref;
model obesincid (event='1')= age1 agesq01 lyear sex smoke1 education1 QT_alc1 physact1 enk1 QT_NNRdietscore1/ rsq ctable lackfit outroc= rocNNR22;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first) QT_alc1 (ref=first) QT_ob_GRS (ref=first) QT_NNRdietscore1 (ref=first)/param=ref;
model obesincid (event='1')= age1 agesq01 lyear sex smoke1 education1 QT_alc1 physact1 enk1 QT_ob_GRS QT_NNRdietscore1/ rsq ctable lackfit outroc= rocNNR32;
roc;
run;
proc logistic data= rsscores1HD;
class sex (ref=last)QT_ob_GRS (ref=first)/param=ref;
model obesincid (event='1')= age1 agesq01 lyear sex QT_ob_GRS/ rsq ctable lackfit outroc= rocGRS22;
roc;
run;
proc logistic data= stuff.rsscores1; 
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first) QT_alc1 (ref=first) QT_HDdietscore1 (ref=first)/param=ref;
model obesincid (event='1')= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_HDdietscore1/ rsq ctable lackfit outroc= rocHD22;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first) QT_alc1 (ref=first) QT_ob_GRS (ref=first) QT_HDdietscore1 (ref=first)/param=ref;
model obesincid (event='1')= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_ob_GRS QT_HDdietscore1/ rsq ctable lackfit outroc= rocHD32;
roc;
run;
proc logistic data= rsscores1PCA;
class sex (ref=last)QT_ob_GRS (ref=first)/param=ref;
model obesincid (event='1')= age1 agesq01 lyear sex QT_ob_GRS/ rsq ctable lackfit outroc= rocGRS32;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first) QT_alc1 (ref=first) QT_factor1 (ref=first)/param=ref;
model obesincid (event='1')= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_factor1/ rsq ctable lackfit outroc= rocPCA22;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first) QT_alc1 (ref=first) QT_ob_GRS (ref=first) QT_factor1(ref=first)/param=ref;
model obesincid (event='1')= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_ob_GRS QT_factor1/ rsq ctable lackfit outroc= rocPCA32;
roc;
run;
proc export data= rocGRS22
DBMS= xls
outfile= rocGRS22 Replace;
run;

/******************************************************
    FOLLOW-UP ASSOCIATIONS OF SCORES WITH CHANGE IN BMI
*******************************************************/

/*proc glmselect data= rsscores1NNR;
class sex (ref=last)QT_ob_GRS (ref= first)/param=reference;
model deltabmi= age1 agesq01 lyear sex QT_ob_GRS/ selection=none stb showpvalues;
run;
proc glmselect data= stuff.rsscores1;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first)enk1 (ref=first) QT_alc1 (ref=first) QT_NNRdietscore1 (ref=first)/param=reference;
model deltabmi= age1 agesq01 lyear sex smoke1 education1 QT_alc1 physact1 enk1 QT_NNRdietscore1/selection=none stb showpvalues;
run;
proc glmselect data= stuff.rsscores1;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first)enk1 (ref=first) QT_alc1 (ref=first) QT_ob_GRS (ref=first)QT_NNRdietscore1 (ref=first)/param=reference;
model deltabmi= age1 agesq01 lyear sex smoke1 education1 QT_alc1 physact1 enk1 QT_ob_GRS QT_NNRdietscore1/selection=none stb showpvalues;
run;
proc glmselect data= rsscores1HD;
class sex (ref=last)QT_ob_GRS (ref= first)/param=reference;
model deltabmi= age1 agesq01 lyear sex QT_ob_GRS/ selection=none stb showpvalues;
run;
proc glmselect data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first)enk1 (ref=first) QT_alc1 (ref=first) QT_HDdietscore1 (ref=first)/param=reference;
model deltabmi= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_HDdietscore1/selection=none stb showpvalues;
run;
proc glmselect data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first)enk1 (ref=first) QT_alc1 (ref=first) QT_ob_GRS (ref= first) QT_HDdietscore1 (ref=first)/param=reference;
model deltabmi= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_ob_GRS QT_HDdietscore1/selection=none stb showpvalues;
run;
proc glmselect data= rsscores1PCA;
class sex (ref=last)QT_ob_GRS (ref= first)/param=reference;
model deltabmi= age1 agesq01 lyear sex QT_ob_GRS/ selection=none stb showpvalues;
run;
proc glmselect data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first)enk1 (ref=first) QT_alc1 (ref=first) QT_factor1 (ref=first)/param=reference;
model deltabmi= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_factor1/selection=none stb showpvalues;
run;
proc glmselect data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first)enk1 (ref=first) QT_alc1 (ref=first) QT_ob_GRS (ref=first) QT_factor1 (ref=first)/param=reference;
model deltabmi= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_ob_GRS QT_factor1/selection=none stb showpvalues;
run;


/******************************************************
    FOLLOW-UP ASSOCIATIONS OF SCORES WITH CHANGE IN WEIGHT
*******************************************************/
/*
proc glmselect data= stuff.rsscores1;
class sex (ref=last)QT_ob_GRS (ref=first)/param=reference;
model deltaweight= age1 agesq01 lyear sex QT_ob_GRS/ selection=none stb showpvalues;
run;
proc glmselect data= stuff.rsscores1;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first)enk1 (ref=first) QT_alc1 (ref=first) QT_NNRdietscore1 (ref=first)/param=reference;
model deltaweight= age1 agesq01 lyear sex smoke1 education1 QT_alc1 physact1 enk1 QT_NNRdietscore1/selection=none stb showpvalues;
run;
proc glmselect data= stuff.rsscores1;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first)enk1 (ref=first) QT_alc1 (ref=first) QT_ob_GRS (ref=first) QT_NNRdietscore1 (ref=first)/param=reference;
model deltaweight= age1 agesq01 lyear sex smoke1 education1 QT_alc1 physact1 enk1 QT_ob_GRS QT_NNRdietscore1/selection=none stb showpvalues;
run;
proc glmselect data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first)enk1 (ref=first) QT_alc1 (ref=first) QT_HDdietscore1 (ref=first)/param=reference;
model deltaweight= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_HDdietscore1/selection=none stb showpvalues;
run;
proc glmselect data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first)enk1 (ref=first) QT_alc1 (ref=first) QT_ob_GRS (ref=first) QT_HDdietscore1 (ref=first)/param=reference;
model deltaweight= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_ob_GRS QT_HDdietscore1/selection=none stb showpvalues;
run;
proc glmselect data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first)enk1 (ref=first) QT_alc1 (ref=first) QT_factor1(ref=first)/param=reference;
model deltaweight= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_factor1/selection=none stb showpvalues;
run;
proc glmselect data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first)enk1 (ref=first) QT_alc1 (ref=first)QT_ob_GRS (ref=first) QT_factor1 (ref=first)/param=reference;
model deltaweight= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_ob_GRS QT_factor1/selection=none stb showpvalues;
run;

/************************************************************
    FOLLOW-UP ASSOCIATIONS OF SCORES WITH 10% OF WEIGHT GAIN
*************************************************************/

proc logistic data= rsscores1NNR;
class sex (ref=last)QT_ob_GRS (ref= first)/param=ref;
model wch10 (event='1')= age1 agesq01 lyear sex QT_ob_GRS/ rsq ctable lackfit outroc= rocGRS11;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first) QT_alc1 (ref=first) QT_NNRdietscore1 (ref=first)/param=ref;
model wch10 (event='1')= age1 agesq01 lyear sex smoke1 education1 QT_alc1 physact1 enk1 QT_NNRdietscore1/ rsq ctable lackfit outroc= rocNNR23;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first) QT_alc1 (ref=first) QT_ob_GRS (ref=first) QT_NNRdietscore1(ref=first)/param=ref;
model wch10 (event='1')= age1 agesq01 lyear sex smoke1 education1  QT_alc1 physact1 enk1 QT_ob_GRS QT_NNRdietscore1/ rsq ctable lackfit outroc= rocNNR33;
roc;
run;
proc logistic data= rsscores1HD;
class sex (ref=last)QT_ob_GRS (ref= first)/param=ref;
model wch10 (event='1')= age1 agesq01 lyear sex QT_ob_GRS/ rsq ctable lackfit outroc= rocGRS22;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first) QT_alc1 (ref=first) QT_HDdietscore1 (ref= first)/param=ref;
model wch10 (event='1')= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_HDdietscore1/ rsq ctable lackfit outroc= rocHD23;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first) QT_alc1 (ref=first) QT_ob_GRS (ref= first) QT_HDdietscore1 (ref=first)/param=ref;
model wch10 (event='1')= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_ob_GRS QT_HDdietscore1/ rsq ctable lackfit outroc= rocHD33;
roc;
run;
proc logistic data= rsscores1PCA;
class sex (ref=last)QT_ob_GRS (ref= first)/param=ref;
model wch10 (event='1')= age1 agesq01 lyear sex QT_ob_GRS/ rsq ctable lackfit outroc= rocGRS33;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first)  QT_alc1 (ref=first) QT_factor1 (ref=first)/param=ref;
model wch10 (event='1')= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_factor1/ rsq ctable lackfit outroc= rocPCA23;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first) QT_alc1 (ref=first) QT_ob_GRS (ref=first) QT_factor1 (ref=first)/param=ref;
model wch10 (event='1')= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_ob_GRS QT_factor1/ rsq ctable lackfit outroc= rocPCA33;
roc;
run;
proc export data= rocGRS22
DBMS= xls
outfile= rocGRS22 Replace;
run;



/******************************************************
    FOLLOW-UP ASSOCIATIONS OF SCORES WITH CHANGE IN GLYCAEMIC TRAITS
*******************************************************/

/*proc glmselect data= rsscores1NNR;
class sex (ref=last) QT_fglu_GRS (ref= first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=reference;
model deltafglu= age1 agesq01 lyear sex QT_fglu_GRS fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ selection=none stb showpvalues;
run;
proc means data= stuff.rsscores1; var deltafglu deltabmi deltatwoglu; run;
proc glmselect data= stuff.rsscores1;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first)enk1 (ref=first) QT_alc1 (ref=first) QT_NNRdietscore1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=reference;
model deltafglu= age1 agesq01 lyear sex smoke1 education1 QT_alc1 physact1 enk1 QT_NNRdietscore1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/selection=none stb showpvalues;
run;
proc glmselect data= stuff.rsscores1;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first)enk1 (ref=first) QT_alc1 (ref=first) QT_fglu_GRS (ref=first)QT_NNRdietscore1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=reference;
model deltafglu= age1 agesq01 lyear sex smoke1 QT_alc1 education1 physact1 enk1 QT_fglu_GRS QT_NNRdietscore1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/selection=none stb showpvalues;
run;
proc glmselect data= rsscores1HD;
class sex (ref=last) QT_fglu_GRS (ref= first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=reference;
model deltafglu= age1 agesq01 lyear sex QT_fglu_GRS fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ selection=none stb showpvalues;
run;
proc glmselect data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first)enk1 (ref=first) QT_alc1 (ref=first) QT_HDdietscore1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=reference;
model deltafglu= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_HDdietscore1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/selection=none stb showpvalues;
run;
proc glmselect data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first)enk1 (ref=first) QT_alc1 (ref=first) QT_fglu_GRS (ref= first) QT_HDdietscore1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=reference;
model deltafglu= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_fglu_GRS QT_HDdietscore1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/selection=none stb showpvalues;
run;
proc glmselect data= rsscores1PCA;
class sex (ref=last) QT_fglu_GRS (ref= first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=reference;
model deltafglu= age1 agesq01 lyear sex QT_fglu_GRS fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ selection=none stb showpvalues;
run;
proc glmselect data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first)enk1 (ref=first) QT_alc1 (ref=first) QT_factor1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=reference;
model deltafglu= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_factor1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/selection=none stb showpvalues;
run;
proc glmselect data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first)enk1 (ref=first) QT_alc1 (ref=first) QT_fglu_GRS (ref=first) QT_factor1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=reference;
model deltafglu= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_fglu_GRS QT_factor1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/selection=none stb showpvalues;
run;

proc glmselect data= rsscores1NNR;
class sex (ref=last)QT_tglu_GRS (ref= first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=reference;
model deltatwoglu= age1 agesq01 lyear sex QT_tglu_GRS fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ selection=none stb showpvalues;
run;
proc glmselect data= stuff.rsscores1;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first)enk1 (ref=first) QT_alc1 (ref=first) QT_NNRdietscore1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=reference;
model deltatwoglu= age1 agesq01 lyear sex smoke1 education1 QT_alc1 physact1 enk1 QT_NNRdietscore1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/selection=none stb showpvalues;
run;
proc glmselect data= stuff.rsscores1;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first)enk1 (ref=first) QT_alc1 (ref=first) QT_tglu_GRS (ref=first)QT_NNRdietscore1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=reference;
model deltatwoglu= age1 agesq01 lyear sex smoke1 education1 QT_alc1 physact1 enk1 QT_tglu_GRS QT_NNRdietscore1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/selection=none stb showpvalues;
run;
proc glmselect data= rsscores1HD;
class sex (ref=last)QT_tglu_GRS (ref= first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=reference;
model deltatwoglu= age1 agesq01 lyear sex QT_tglu_GRS fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ selection=none stb showpvalues;
run;
proc glmselect data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first)enk1 (ref=first) QT_alc1 (ref=first) QT_HDdietscore1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=reference;
model deltatwoglu= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_HDdietscore1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/selection=none stb showpvalues;
run;
proc glmselect data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first)enk1 (ref=first) QT_alc1 (ref=first) QT_tglu_GRS (ref= first) QT_HDdietscore1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=reference;
model deltatwoglu= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_tglu_GRS QT_HDdietscore1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/selection=none stb showpvalues;
run;
proc glmselect data= rsscores1PCA;
class sex (ref=last)QT_tglu_GRS (ref= first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=reference;
model deltatwoglu= age1 agesq01 lyear sex QT_tglu_GRS fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ selection=none stb showpvalues;
run;
proc glmselect data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first)enk1 (ref=first) QT_alc1 (ref=first) QT_factor1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=reference;
model deltatwoglu= age1 agesq01 lyear sex education1 smoke1  QT_alc1 physact1 enk1 QT_factor1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/selection=none stb showpvalues;
run;
proc glmselect data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first)enk1 (ref=first) QT_alc1 (ref=first) QT_tglu_GRS (ref=first) QT_factor1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=reference;
model deltatwoglu= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_tglu_GRS QT_factor1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/selection=none stb showpvalues;
run;

/**********************************************************************
    FOLLOW-UP ASSOCIATIONS OF SCORES WITH CATEGORICAL GLYCAEMIC TRAITS
***********************************************************************/
/*
proc logistic data= stuff.rsscores1;
class sex (ref=last)QT_fglu_GRS (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model FGworsen (event='1')= age1 agesq01 lyear sex QT_fglu_GRS fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable outroc= rocGRS12FGworsen;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first) QT_alc1 (ref=first) QT_NNRdietscore1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model FGworsen (event='1')= age1 agesq01 lyear sex smoke1 QT_alc1 education1 physact1 enk1 QT_NNRdietscore1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable outroc= rocNNR22FGworsen;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first)  QT_alc1 (ref=first) QT_fglu_GRS (ref=first) QT_NNRdietscore1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model FGworsen (event='1')= age1 agesq01 lyear sex smoke1 education1 QT_alc1 physact1 enk1 QT_fglu_GRS QT_NNRdietscore1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable outroc= rocNNR32FGworsen;
roc;
run;
proc logistic data= stuff.rsscores1; 
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first) QT_alc1 (ref=first) QT_HDdietscore1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model FGworsen (event='1')= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_HDdietscore1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable outroc= rocHD22FGworsen;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first) QT_alc1 (ref=first) QT_fglu_GRS (ref=first) QT_HDdietscore1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model FGworsen (event='1')= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_fglu_GRS QT_HDdietscore1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable outroc= rocHD32FGworsen;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first)  QT_alc1 (ref=first) QT_factor1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model FGworsen (event='1')= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_factor1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable outroc= rocPCA22FGworsen;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first) QT_alc1 (ref=first) QT_fglu_GRS (ref=first) QT_factor1(ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model FGworsen (event='1')= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_fglu_GRS QT_factor1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable outroc= rocPCA32FGworsen;
roc;
run;
proc export data= rocPCA32FGworsen
DBMS= xls
outfile= rocPCA32FGworsen Replace;
run;



proc logistic data= stuff.rsscores1;
class sex (ref=last)QT_tglu_GRS (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model GTworsen (event='1')= age1 agesq01 lyear sex QT_tglu_GRS fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable outroc= rocGRS12GTworsen;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first) QT_alc1 (ref=first) QT_NNRdietscore1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model GTworsen (event='1')= age1 agesq01 lyear sex smoke1 education1  QT_alc1 physact1 enk1 QT_NNRdietscore1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable outroc= rocNNR22GTworsen;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first) QT_alc1 (ref=first) QT_tglu_GRS (ref=first) QT_NNRdietscore1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model GTworsen (event='1')= age1 agesq01 lyear sex smoke1 education1 QT_alc1 physact1 enk1 QT_tglu_GRS QT_NNRdietscore1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable outroc= rocNNR32GTworsen;
roc;
run;
proc logistic data= stuff.rsscores1; 
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first) QT_alc1 (ref=first) QT_HDdietscore1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model GTworsen (event='1')= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_HDdietscore1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable outroc= rocHD22GTworsen;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first) QT_alc1 (ref=first) QT_tglu_GRS (ref=first) QT_HDdietscore1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model GTworsen (event='1')= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_tglu_GRS QT_HDdietscore1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable outroc= rocHD32GTworsen;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first)  QT_alc1 (ref=first) QT_factor1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model GTworsen (event='1')= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_factor1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable outroc= rocPCA22GTworsen;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first) QT_alc1 (ref=first) QT_tglu_GRS (ref=first) QT_factor1(ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model GTworsen (event='1')= age1 agesq01 lyear sex education1 smoke1  QT_alc1  physact1 enk1 QT_tglu_GRS QT_factor1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable outroc= rocPCA32GTworsen;
roc;
run;
proc export data= rocPCA32GTworsen
DBMS= xls
outfile= rocPCA32GTworsen Replace;
run;*/


proc logistic data= rsscores1NNR;
class sex (ref=last)QT_d_GRS (ref=first)fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model T2Dincid (event='1')= age1 agesq01 lyear sex QT_d_GRS fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable lackfit outroc= rocGRS12T2Dincid;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first) QT_alc1 (ref=first) QT_NNRdietscore1 (ref=first)fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first) /param=ref;
model T2Dincid (event='1')= age1 agesq01 lyear sex smoke1 education1 QT_alc1 physact1 enk1 QT_NNRdietscore1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable lackfit outroc= rocNNR22T2Dincid;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first) QT_alc1 (ref=first) QT_d_GRS (ref=first) QT_NNRdietscore1 (ref=first)fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first) /param=ref;
model T2Dincid (event='1')= age1 agesq01 lyear sex smoke1 education1 QT_alc1 physact1 enk1 QT_d_GRS QT_NNRdietscore1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable lackfit outroc= rocNNR32T2Dincid;
roc;
run;
proc logistic data= rsscores1HD;
class sex (ref=last)QT_d_GRS (ref=first)fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model T2Dincid (event='1')= age1 agesq01 lyear sex QT_d_GRS fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable lackfit outroc= rocGRS22T2Dincid;
roc;
run;
proc logistic data= stuff.rsscores1; 
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first) QT_alc1 (ref=first) QT_HDdietscore1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model T2Dincid (event='1')= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_HDdietscore1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable lackfit outroc= rocHD22T2Dincid;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first) QT_alc1 (ref=first) QT_d_GRS (ref=first) QT_HDdietscore1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model T2Dincid (event='1')= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_d_GRS QT_HDdietscore1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable lackfit outroc= rocHD32T2Dincid;
roc;
run;
proc logistic data= rsscores1PCA;
class sex (ref=last)QT_d_GRS (ref=first)fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model T2Dincid (event='1')= age1 agesq01 lyear sex QT_d_GRS fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable lackfit outroc= rocGRS32T2Dincid;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first) QT_alc1 (ref=first) QT_factor1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model T2Dincid (event='1')= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_factor1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable lackfit outroc= rocPCA22T2Dincid;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first) QT_alc1 (ref=first) QT_d_GRS (ref=first) QT_factor1(ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model T2Dincid (event='1')= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_d_GRS QT_factor1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable lackfit outroc= rocPCA32T2Dincid;
roc;
run;
proc export data= rocGRS32T2Dincid
DBMS= xls
outfile= rocGRS32T2Dincid Replace;
run;


/*IDFG*/

proc logistic data= rsscores1NNR;
class sex (ref=last)QT_fglu_GRS (ref=first)fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDFGincid (event='1')= age1 agesq01 lyear sex QT_fglu_GRS fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable lackfit outroc= rocGRS12IDFGincid;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first) QT_alc1 (ref=first) QT_NNRdietscore1 (ref=first)fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first) /param=ref;
model IDFGincid (event='1')= age1 agesq01 lyear sex smoke1 education1 QT_alc1 physact1 enk1 QT_NNRdietscore1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable lackfit outroc= rocNNR22IDFGincid;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first) QT_alc1 (ref=first) QT_fglu_GRS (ref=first) QT_NNRdietscore1 (ref=first)fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first) /param=ref;
model IDFGincid (event='1')= age1 agesq01 lyear sex smoke1 education1 QT_alc1 physact1 enk1 QT_fglu_GRS QT_NNRdietscore1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable lackfit outroc= rocNNR32IDFGincid;
roc;
run;
proc logistic data= rsscores1HD;
class sex (ref=last)QT_fglu_GRS (ref=first)fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDFGincid (event='1')= age1 agesq01 lyear sex QT_fglu_GRS fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable lackfit outroc= rocGRS22IDFGincid;
roc;
run;
proc logistic data= stuff.rsscores1; 
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first) QT_alc1 (ref=first) QT_HDdietscore1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDFGincid (event='1')= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_HDdietscore1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable lackfit outroc= rocHD22IDFGincid;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first) QT_alc1 (ref=first) QT_fglu_GRS (ref=first) QT_HDdietscore1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDFGincid (event='1')= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_fglu_GRS QT_HDdietscore1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable lackfit outroc= rocHD32IDFGincid;
roc;
run;
proc logistic data= rsscores1PCA;
class sex (ref=last)QT_fglu_GRS (ref=first)fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDFGincid (event='1')= age1 agesq01 lyear sex QT_fglu_GRS fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable lackfit outroc= rocGRS32IDFGincid;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first) QT_alc1 (ref=first) QT_factor1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDFGincid (event='1')= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_factor1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable lackfit outroc= rocPCA22IDFGincid;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first) QT_alc1 (ref=first) QT_fglu_GRS (ref=first) QT_factor1(ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDFGincid (event='1')= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_fglu_GRS QT_factor1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable lackfit outroc= rocPCA32IDFGincid;
roc;
run;
proc export data= rocGRS22IDFGincid
DBMS= xls
outfile= rocGRS22IDFGincid Replace;
run;

/*IDGT*/

proc logistic data= rsscores1NNR;
class sex (ref=last)QT_tglu_GRS (ref=first)fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDGTincid (event='1')= age1 agesq01 lyear sex QT_tglu_GRS fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable lackfit outroc= rocGRS12IDGTincid;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first) QT_alc1 (ref=first) QT_NNRdietscore1 (ref=first)fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first) /param=ref;
model IDGTincid (event='1')= age1 agesq01 lyear sex smoke1 education1 QT_alc1 physact1 enk1 QT_NNRdietscore1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable lackfit outroc= rocNNR22IDGTincid;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref=first) smoke1 (ref=first) QT_alc1 (ref=first) QT_tglu_GRS (ref=first) QT_NNRdietscore1 (ref=first)fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first) /param=ref;
model IDGTincid (event='1')= age1 agesq01 lyear sex smoke1 education1 QT_alc1 physact1 enk1 QT_tglu_GRS QT_NNRdietscore1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable lackfit outroc= rocNNR32IDGTincid;
roc;
run;
proc logistic data= rsscores1HD;
class sex (ref=last)QT_tglu_GRS (ref=first)fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDGTincid (event='1')= age1 agesq01 lyear sex QT_tglu_GRS fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable lackfit outroc= rocGRS22IDGTincid;
roc;
run;
proc logistic data= stuff.rsscores1; 
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first) QT_alc1 (ref=first) QT_HDdietscore1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDGTincid (event='1')= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_HDdietscore1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable lackfit outroc= rocHD22IDGTincid;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first)  QT_alc1 (ref=first) QT_tglu_GRS (ref=first) QT_HDdietscore1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDGTincid (event='1')= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_tglu_GRS QT_HDdietscore1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable lackfit outroc= rocHD32IDGTincid;
roc;
run;
proc logistic data= rsscores1PCA;
class sex (ref=last)QT_tglu_GRS (ref=first)fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDGTincid (event='1')= age1 agesq01 lyear sex QT_tglu_GRS fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable lackfit outroc= rocGRS32IDGTincid;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first) QT_alc1 (ref=first) QT_factor1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDGTincid (event='1')= age1 agesq01 lyear sex education1 smoke1 QT_alc1 physact1 enk1 QT_factor1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable lackfit outroc= rocPCA22IDGTincid;
roc;
run;
proc logistic data= stuff.rsscores1;
class sex (ref=last) education1 (ref= first) smoke1 (ref=first) physact1 (ref=first) QT_alc1 (ref=first) QT_tglu_GRS (ref=first) QT_factor1(ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDGTincid (event='1')= age1 agesq01 lyear sex education1 smoke1  QT_alc1 physact1 enk1 QT_tglu_GRS QT_factor1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2/ rsq ctable lackfit outroc= rocPCA32IDGTincid;
roc;
run;
proc export data= rocGRS22IDGTincid
DBMS= xls
outfile= rocGRS22IDGTincid Replace;
run;

/***SENSITIVITY ANALYSES***/

/*check that difference in questionnaire doesn't lead to a different result*/
data stuff.a10_impute; set stuff.a9_impute; if enk1 eq 1; run;
proc freq data= stuff.a10_impute; tables obesincid t2dincid IDFGincid IDGTincid; run;

ods html sge=on;
ods graphics on;
title1 'AUCs obesity';
proc logistic data= stuff.a10_impute;
class sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) /param=ref;
model obesincid (event='1')= age1 agesq01 sex lyear smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964/nofit;
roc 'Model 1 genetic' age1 agesq01 lyear sex rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964;
roc 'Model 2 lifestyle' age1 agesq01 lyear sex smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 ;
roc 'Model 3 combined' age1 agesq01 sex lyear smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964;
roccontrast adjacentpairs/ESTIMATE=ALLPAIRS;
run;
ods html sge=off;


proc logistic data= stuff.a10_impute;
class sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) /param=ref;
model obesincid (event='1')= age1 agesq01 lyear smoke1 education1 physact1 sex tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1/ rsq ctable lackfit outroc= rocoblif;
roc;
run;

proc logistic data= stuff.a10_impute;
class sex (ref=last)/param=ref;
model obesincid (event='1')= age1 agesq01 sex lyear rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964 / rsq ctable lackfit outroc= rocobgen;
roc;
run;
proc logistic data= stuff.a10_impute;
class sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) /param=ref;
model obesincid (event='1')= age1 agesq01 sex lyear smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964 / rsq ctable lackfit outroc= rocobcom;
roc;
run;

proc export data= rocobcom
DBMS= xls
outfile= rocobcom Replace;
run;

/**Diabetes**/

ods html sge=on;
ods graphics on;
title1 'ROCAUC t2d';
proc logistic data= stuff.a10_impute;
class sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model T2Dincid (event='1')= age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs340874d rs7515431 rs7903146d rs12571751 rs12242953 rs11257655 rs1111875 rs5215 rs2334499 rs163184 rs1552224 rs10830963d rs7955901 rs2261181 rs7965349 rs11063069 rs10842994 rs1359790 rs7177055 rs4502156d rs2007084
rs12899811 rs11634397 rs9923233 rs7202877 rs11651755 rs2447090 rs11663816 rs8182584 rs8108269 rs10401969 rs780094d rs7569522 rs2943640 rs243083 rs1128249 rs10203174 rs4812829 rs6795735 rs4402960 rs2197423
rs17301514 rs1496653 rs12497268 rs11717195d rs6819243 rs10012946 rs6878122 rs459193 rs7756992 rs4299828 rs3734621 rs849135 rs10229583 rs17168486 rs13233731 rs3824065 rs7845219 rs516946 rs3802177 rs2796441 rs17791513 rs16927668 rs10811661d rs10758593 /nofit;
roc 'Model 1 genetic' age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 rs340874d rs7515431 rs7903146d rs12571751 rs12242953 rs11257655 rs1111875 rs5215 rs2334499 rs163184 rs1552224 rs10830963d rs7955901 rs2261181 rs7965349 rs11063069 rs10842994 rs1359790 rs7177055 rs4502156d rs2007084
rs12899811 rs11634397 rs9923233 rs7202877 rs11651755 rs2447090 rs11663816 rs8182584 rs8108269 rs10401969 rs780094d rs7569522 rs2943640 rs243083 rs1128249 rs10203174 rs4812829 rs6795735 rs4402960 rs2197423
rs17301514 rs1496653 rs12497268 rs11717195d rs6819243 rs10012946 rs6878122 rs459193 rs7756992 rs4299828 rs3734621 rs849135 rs10229583 rs17168486 rs13233731 rs3824065 rs7845219 rs516946 rs3802177 rs2796441 rs17791513 rs16927668 rs10811661d rs10758593 ;
roc 'Model 2 lifestyle' age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1;
roc 'Model 3 combined' age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs340874d rs7515431 rs7903146d rs12571751 rs12242953 rs11257655 rs1111875 rs5215 rs2334499 rs163184 rs1552224 rs10830963d rs7955901 rs2261181 rs7965349 rs11063069 rs10842994 rs1359790 rs7177055 rs4502156d rs2007084
rs12899811 rs11634397 rs9923233 rs7202877 rs11651755 rs2447090 rs11663816 rs8182584 rs8108269 rs10401969 rs780094d rs7569522 rs2943640 rs243083 rs1128249 rs10203174 rs4812829 rs6795735 rs4402960 rs2197423
rs17301514 rs1496653 rs12497268 rs11717195d rs6819243 rs10012946 rs6878122 rs459193 rs7756992 rs4299828 rs3734621 rs849135 rs10229583 rs17168486 rs13233731 rs3824065 rs7845219 rs516946 rs3802177 rs2796441 rs17791513 rs16927668 rs10811661d rs10758593;
roccontrast adjacentpairs/ESTIMATE=ALLPAIRS;
run;
ods html sge=off;

proc logistic data= stuff.a10_impute;
class sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model T2Dincid (event='1')= age1 agesq01 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear smoke1 education1 physact1 sex tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1/ rsq ctable lackfit outroc= roct2dlif;
roc;
run;
proc logistic data= stuff.a10_impute;
class sex (ref=last) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model T2Dincid (event='1')= age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 rs340874d rs7515431 rs7903146d rs12571751 rs12242953 rs11257655 rs1111875 rs5215 rs2334499 rs163184 rs1552224 rs10830963d rs7955901 rs2261181 rs7965349 rs11063069 rs10842994 rs1359790 rs7177055 rs4502156d rs2007084
rs12899811 rs11634397 rs9923233 rs7202877 rs11651755 rs2447090 rs11663816 rs8182584 rs8108269 rs10401969 rs780094d rs7569522 rs2943640 rs243083 rs1128249 rs10203174 rs4812829 rs6795735 rs4402960 rs2197423
rs17301514 rs1496653 rs12497268 rs11717195d rs6819243 rs10012946 rs6878122 rs459193 rs7756992 rs4299828 rs3734621 rs849135 rs10229583 rs17168486 rs13233731 rs3824065 rs7845219 rs516946 rs3802177 rs2796441 rs17791513 rs16927668 rs10811661d rs10758593 / rsq ctable lackfit outroc= roct2dgen;
roc;
run;
proc logistic data= stuff.a10_impute;
class sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model T2Dincid (event='1')= age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs340874d rs7515431 rs7903146d rs12571751 rs12242953 rs11257655 rs1111875 rs5215 rs2334499 rs163184 rs1552224 rs10830963d rs7955901 rs2261181 rs7965349 rs11063069 rs10842994 rs1359790 rs7177055 rs4502156d rs2007084
rs12899811 rs11634397 rs9923233 rs7202877 rs11651755 rs2447090 rs11663816 rs8182584 rs8108269 rs10401969 rs780094d rs7569522 rs2943640 rs243083 rs1128249 rs10203174 rs4812829 rs6795735 rs4402960 rs2197423
rs17301514 rs1496653 rs12497268 rs11717195d rs6819243 rs10012946 rs6878122 rs459193 rs7756992 rs4299828 rs3734621 rs849135 rs10229583 rs17168486 rs13233731 rs3824065 rs7845219 rs516946 rs3802177 rs2796441 rs17791513 rs16927668 rs10811661d rs10758593 / rsq ctable lackfit outroc= roct2dcom;
roc;
run;

proc export data= roct2dcom
DBMS= xls
outfile= roct2dcom Replace;
run;


/**IDFGincid**/

ods html sge=on;
ods graphics on;
title1 'ROCAUCs incident IFG';
proc logistic data= stuff.a10_impute;
class sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first)fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first) /param=ref;
model IDFGincid (event='1')= age1 agesq01 sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs10811661f rs4869272 rs11619319 rs983309 rs6943153 rs11603334 rs6113722 rs16913693 rs3829109 rs3783347 rs2302593 rs9368222 rs10747083 rs6072275 rs7651090f
rs576674 rs11715915 rs17762454 rs7708285 rs2657879 rs340874f rs780094f rs560887 rs11708067 rs1280 rs2191349 rs2908289 rs11558471 rs10814916 rs11195502 rs7903146f rs11607883 rs11039182 rs174576 rs10830963f rs4502156f /nofit;
roc 'Model 1 genetic' age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 rs10811661f rs4869272 rs11619319 rs983309 rs6943153 rs11603334 rs6113722 rs16913693 rs3829109 rs3783347 rs2302593 rs9368222 rs10747083 rs6072275 rs7651090f
rs576674 rs11715915 rs17762454 rs7708285 rs2657879 rs340874f rs780094f rs560887 rs11708067 rs1280 rs2191349 rs2908289 rs11558471 rs10814916 rs11195502 rs7903146f rs11607883 rs11039182 rs174576 rs10830963f rs4502156f;
roc 'Model 2 lifestyle' age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1;
roc 'Model 3 combined' age1 agesq01 sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs10811661f rs4869272 rs11619319 rs983309 rs6943153 rs11603334 rs6113722 rs16913693 rs3829109 rs3783347 rs2302593 rs9368222 rs10747083 rs6072275 rs7651090f
rs576674 rs11715915 rs17762454 rs7708285 rs2657879 rs340874f rs780094f rs560887 rs11708067 rs1280 rs2191349 rs2908289 rs11558471 rs10814916 rs11195502 rs7903146f rs11607883 rs11039182 rs174576 rs10830963f rs4502156f;
roccontrast adjacentpairs/ESTIMATE=ALLPAIRS;
run;
ods html sge=off;

proc logistic data= stuff.a10_impute;
class sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first)fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDFGincid (event='1')= age1 agesq01 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear smoke1 education1 physact1 sex tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1/ rsq ctable lackfit outroc= rocifglif;
roc;
run;
proc logistic data= stuff.a10_impute;
class sex (ref=last) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDFGincid (event='1')= age1 agesq01 sex lyear fastamiss1 fasta4h1 fasta4to8h1  fastamiss2 fasta4h2 fasta4to8h2 rs10811661f rs4869272 rs11619319 rs983309 rs6943153 rs11603334 rs6113722 rs16913693 rs3829109 rs3783347 rs2302593 rs9368222 rs10747083 rs6072275 rs7651090f
rs576674 rs11715915 rs17762454 rs7708285 rs2657879 rs340874f rs780094f rs560887 rs11708067 rs1280 rs2191349 rs2908289 rs11558471 rs10814916 rs11195502 rs7903146f rs11607883 rs11039182 rs174576 rs10830963f rs4502156f/ rsq ctable lackfit outroc= rocifggen;
roc;
run;
proc logistic data= stuff.a10_impute;
class sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first)fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDFGincid (event='1')= age1 agesq01 sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs10811661f rs4869272 rs11619319 rs983309 rs6943153 rs11603334 rs6113722 rs16913693 rs3829109 rs3783347 rs2302593 rs9368222 rs10747083 rs6072275 rs7651090f
rs576674 rs11715915 rs17762454 rs7708285 rs2657879 rs340874f rs780094f rs560887 rs11708067 rs1280 rs2191349 rs2908289 rs11558471 rs10814916 rs11195502 rs7903146f rs11607883 rs11039182 rs174576 rs10830963f rs4502156f/ rsq ctable lackfit outroc= rocifgcom;
roc;
run;

proc export data= rocifgcom
DBMS= xls
outfile= rocifgcom Replace;
run;

/**IDGTincid**/

ods html sge=on;
ods graphics on;
title1 'ROCAUCs incident IGT';
proc logistic data= stuff.a10_impute;
class sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDGTincid (event='1')= age1 agesq01 sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1260326 rs11717195t rs12255372 rs1436958 rs11672660 rs6975024 rs11782386 rs1019503 rs7651090t /nofit;
roc 'Model 1 genetic' age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 rs1260326 rs11717195t rs12255372 rs1436958 rs11672660 rs6975024 rs11782386 rs1019503 rs7651090t;
roc 'Model 2 lifestyle' age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1;
roc 'Model 3 combined' age1 agesq01 sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1260326 rs11717195t rs12255372 rs1436958 rs11672660 rs6975024 rs11782386 rs1019503 rs7651090t;
roccontrast adjacentpairs/ESTIMATE=ALLPAIRS;
run;
ods html sge=off;

proc logistic data= stuff.a10_impute;
class sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDGTincid (event='1')= age1 agesq01 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear smoke1 education1 physact1 sex tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1/ rsq ctable lackfit outroc= rocigtlif;
roc;
run;
proc logistic data= stuff.a10_impute;
class sex (ref=last)fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDGTincid (event='1')= age1 agesq01 sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear rs1260326 rs11717195t rs12255372 rs1436958 rs11672660 rs6975024 rs11782386 rs1019503 rs7651090t/ rsq ctable lackfit outroc= rocigtgen;
roc;
run;
proc logistic data= stuff.a10_impute;
class sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDGTincid (event='1')= age1 agesq01 sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1260326 rs11717195t rs12255372 rs1436958 rs11672660 rs6975024 rs11782386 rs1019503 rs7651090t / rsq ctable lackfit outroc= rocigtcom;
roc;
run;

proc export data= rocigtcom
DBMS= xls
outfile= rocigtcom Replace;
run;


/*10% weight gain*/
/*create the variable of 10% of weight gain*/

ods html sge=on;
ods graphics on;
title1 'AUCs 10%weight gain';
proc logistic data= stuff.a10_impute;
class sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) /param=ref;
model wch10 (event='1')= age1 agesq01 sex lyear smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964/nofit;
roc 'Model 1 genetic' age1 agesq01 lyear sex rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964;
roc 'Model 2 lifestyle' age1 agesq01 lyear sex smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 ;
roc 'Model 3 combined' age1 agesq01 sex lyear smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964;
roccontrast adjacentpairs/ESTIMATE=ALLPAIRS;
run;
ods html sge=off;

proc logistic data= stuff.a10_impute;
class enk1 (ref=first)sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) /param=ref;
model wch10 (event='1')= age1 agesq01 lyear smoke1 education1 physact1 sex tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1/ rsq ctable lackfit outroc= rocwch10lif;
roc;
run;

proc logistic data= stuff.a10_impute;
class sex (ref=last)/param=ref;
model wch10 (event='1')= age1 agesq01 sex lyear rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964 / rsq ctable lackfit outroc= rocwch10gen;
roc;
run;

proc logistic data= stuff.a10_impute;
class sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) /param=ref;
model wch10 (event='1')= age1 agesq01 sex lyear smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964 / rsq ctable lackfit outroc= rocwch10com;
roc;
run;

proc export data= rocwch10com
DBMS= xls
outfile= rocwch10com Replace;
run;

/*do it by random*/
proc surveyselect data= stuff.a9_impute method= SRS rep= 1 sampsize= 2700 out= stuff.a11_impute;run;
proc freq data= stuff.a11_impute; tables obesincid t2dincid IDFGincid IDGTincid enk1; run;

ods html sge=on;
ods graphics on;
title1 'AUCs obesity';
proc logistic data= stuff.a11_impute;
class enk1 (ref= last) sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) /param=ref;
model obesincid (event='1')= age1 agesq01 sex lyear enk1 smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964/nofit;
roc 'Model 1 genetic' age1 agesq01 lyear sex rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964;
roc 'Model 2 lifestyle' age1 agesq01 lyear enk1 sex smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 ;
roc 'Model 3 combined' age1 agesq01 sex lyear enk1 smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964;
roccontrast adjacentpairs/ESTIMATE=ALLPAIRS;
run;
ods html sge=off;


/**Diabetes**/

ods html sge=on;
ods graphics on;
title1 'ROCAUC t2d';
proc logistic data= stuff.a11_impute;
class enk1(ref= first) sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model T2Dincid (event='1')= age1 agesq01 lyear enk1 sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 smoke1 education1 physact1 enk1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs340874d rs7515431 rs7903146d rs12571751 rs12242953 rs11257655 rs1111875 rs5215 rs2334499 rs163184 rs1552224 rs10830963d rs7955901 rs2261181 rs7965349 rs11063069 rs10842994 rs1359790 rs7177055 rs4502156d rs2007084
rs12899811 rs11634397 rs9923233 rs7202877 rs11651755 rs2447090 rs11663816 rs8182584 rs8108269 rs10401969 rs780094d rs7569522 rs2943640 rs243083 rs1128249 rs10203174 rs4812829 rs6795735 rs4402960 rs2197423
rs17301514 rs1496653 rs12497268 rs11717195d rs6819243 rs10012946 rs6878122 rs459193 rs7756992 rs4299828 rs3734621 rs849135 rs10229583 rs17168486 rs13233731 rs3824065 rs7845219 rs516946 rs3802177 rs2796441 rs17791513 rs16927668 rs10811661d rs10758593 /nofit;
roc 'Model 1 genetic' age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 rs340874d rs7515431 rs7903146d rs12571751 rs12242953 rs11257655 rs1111875 rs5215 rs2334499 rs163184 rs1552224 rs10830963d rs7955901 rs2261181 rs7965349 rs11063069 rs10842994 rs1359790 rs7177055 rs4502156d rs2007084
rs12899811 rs11634397 rs9923233 rs7202877 rs11651755 rs2447090 rs11663816 rs8182584 rs8108269 rs10401969 rs780094d rs7569522 rs2943640 rs243083 rs1128249 rs10203174 rs4812829 rs6795735 rs4402960 rs2197423
rs17301514 rs1496653 rs12497268 rs11717195d rs6819243 rs10012946 rs6878122 rs459193 rs7756992 rs4299828 rs3734621 rs849135 rs10229583 rs17168486 rs13233731 rs3824065 rs7845219 rs516946 rs3802177 rs2796441 rs17791513 rs16927668 rs10811661d rs10758593 ;
roc 'Model 2 lifestyle' age1 agesq01 lyear sex enk1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1;
roc 'Model 3 combined' age1 agesq01 lyear sex enk1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs340874d rs7515431 rs7903146d rs12571751 rs12242953 rs11257655 rs1111875 rs5215 rs2334499 rs163184 rs1552224 rs10830963d rs7955901 rs2261181 rs7965349 rs11063069 rs10842994 rs1359790 rs7177055 rs4502156d rs2007084
rs12899811 rs11634397 rs9923233 rs7202877 rs11651755 rs2447090 rs11663816 rs8182584 rs8108269 rs10401969 rs780094d rs7569522 rs2943640 rs243083 rs1128249 rs10203174 rs4812829 rs6795735 rs4402960 rs2197423
rs17301514 rs1496653 rs12497268 rs11717195d rs6819243 rs10012946 rs6878122 rs459193 rs7756992 rs4299828 rs3734621 rs849135 rs10229583 rs17168486 rs13233731 rs3824065 rs7845219 rs516946 rs3802177 rs2796441 rs17791513 rs16927668 rs10811661d rs10758593;
roccontrast adjacentpairs/ESTIMATE=ALLPAIRS;
run;
ods html sge=off;


/**IDFGincid**/

ods html sge=on;
ods graphics on;
title1 'ROCAUCs incident IFG';
proc logistic data= stuff.a11_impute;
class enk1 (ref=first) sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first)fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first) /param=ref;
model IDFGincid (event='1')= age1 agesq01 sex enk1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs10811661f rs4869272 rs11619319 rs983309 rs6943153 rs11603334 rs6113722 rs16913693 rs3829109 rs3783347 rs2302593 rs9368222 rs10747083 rs6072275 rs7651090f
rs576674 rs11715915 rs17762454 rs7708285 rs2657879 rs340874f rs780094f rs560887 rs11708067 rs1280 rs2191349 rs2908289 rs11558471 rs10814916 rs11195502 rs7903146f rs11607883 rs11039182 rs174576 rs10830963f rs4502156f /nofit;
roc 'Model 1 genetic' age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 rs10811661f rs4869272 rs11619319 rs983309 rs6943153 rs11603334 rs6113722 rs16913693 rs3829109 rs3783347 rs2302593 rs9368222 rs10747083 rs6072275 rs7651090f
rs576674 rs11715915 rs17762454 rs7708285 rs2657879 rs340874f rs780094f rs560887 rs11708067 rs1280 rs2191349 rs2908289 rs11558471 rs10814916 rs11195502 rs7903146f rs11607883 rs11039182 rs174576 rs10830963f rs4502156f;
roc 'Model 2 lifestyle' age1 agesq01 lyear enk1 sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1;
roc 'Model 3 combined' age1 agesq01 sex enk1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs10811661f rs4869272 rs11619319 rs983309 rs6943153 rs11603334 rs6113722 rs16913693 rs3829109 rs3783347 rs2302593 rs9368222 rs10747083 rs6072275 rs7651090f
rs576674 rs11715915 rs17762454 rs7708285 rs2657879 rs340874f rs780094f rs560887 rs11708067 rs1280 rs2191349 rs2908289 rs11558471 rs10814916 rs11195502 rs7903146f rs11607883 rs11039182 rs174576 rs10830963f rs4502156f;
roccontrast adjacentpairs/ESTIMATE=ALLPAIRS;
run;
ods html sge=off;

/**IDGTincid**/

ods html sge=on;
ods graphics on;
title1 'ROCAUCs incident IGT';
proc logistic data= stuff.a11_impute;
class enk1 (ref=first) sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDGTincid (event='1')= age1 agesq01 sex enk1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1260326 rs11717195t rs12255372 rs1436958 rs11672660 rs6975024 rs11782386 rs1019503 rs7651090t /nofit;
roc 'Model 1 genetic' age1 agesq01 lyear sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 rs1260326 rs11717195t rs12255372 rs1436958 rs11672660 rs6975024 rs11782386 rs1019503 rs7651090t;
roc 'Model 2 lifestyle' age1 agesq01 lyear sex enk1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1;
roc 'Model 3 combined' age1 agesq01 sex enk1 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1260326 rs11717195t rs12255372 rs1436958 rs11672660 rs6975024 rs11782386 rs1019503 rs7651090t;
roccontrast adjacentpairs/ESTIMATE=ALLPAIRS;
run;
ods html sge=off;

proc logistic data= stuff.a10_impute;
class sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDGTincid (event='1')= age1 agesq01 fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear smoke1 education1 physact1 sex tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1/ rsq ctable lackfit outroc= rocigtlif;
roc;
run;
proc logistic data= stuff.a10_impute;
class sex (ref=last)fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDGTincid (event='1')= age1 agesq01 sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear rs1260326 rs11717195t rs12255372 rs1436958 rs11672660 rs6975024 rs11782386 rs1019503 rs7651090t/ rsq ctable lackfit outroc= rocigtgen;
roc;
run;
proc logistic data= stuff.a10_impute;
class sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) fastamiss1(ref= first) fasta4h1(ref= first) fasta4to8h1(ref= first) fastamiss2(ref= first) fasta4h2(ref= first) fasta4to8h2(ref= first)/param=ref;
model IDGTincid (event='1')= age1 agesq01 sex fastamiss1 fasta4h1 fasta4to8h1 fastamiss2 fasta4h2 fasta4to8h2 lyear smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1260326 rs11717195t rs12255372 rs1436958 rs11672660 rs6975024 rs11782386 rs1019503 rs7651090t / rsq ctable lackfit outroc= rocigtcom;
roc;
run;

proc export data= rocigtcom
DBMS= xls
outfile= rocigtcom Replace;
run;


/*10% weight gain*/
/*create the variable of 10% of weight gain*/

ods html sge=on;
ods graphics on;
title1 'AUCs 10%weight gain';
proc logistic data= stuff.a10_impute;
class sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) /param=ref;
model wch10 (event='1')= age1 agesq01 sex lyear smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964/nofit;
roc 'Model 1 genetic' age1 agesq01 lyear sex rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964;
roc 'Model 2 lifestyle' age1 agesq01 lyear sex smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 ;
roc 'Model 3 combined' age1 agesq01 sex lyear smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964;
roccontrast adjacentpairs/ESTIMATE=ALLPAIRS;
run;
ods html sge=off;

proc logistic data= stuff.a10_impute;
class enk1 (ref=first)sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) /param=ref;
model wch10 (event='1')= age1 agesq01 lyear smoke1 education1 physact1 sex tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1/ rsq ctable lackfit outroc= rocwch10lif;
roc;
run;

proc logistic data= stuff.a10_impute;
class sex (ref=last)/param=ref;
model wch10 (event='1')= age1 agesq01 sex lyear rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964 / rsq ctable lackfit outroc= rocwch10gen;
roc;
run;

proc logistic data= stuff.a10_impute;
class sex (ref=last)smoke1 (ref=first)education1 (ref=first)physact1 (ref=first) /param=ref;
model wch10 (event='1')= age1 agesq01 sex lyear smoke1 education1 physact1 tei1 MUFA1 PUFA1 CARBOHYDRATES1 SUGARS1 PROTEIN1 SATFAT1 TOTFAT1 FIBER1 FA1 ALC1 SALT1 vitA1 vitD1 vitE1 Thia1 Ribo1 Niac1 vitB61 Folate1 vitB121 vitC1 calc1 Phosp1 Pota1 Magn1 Iron1 Zinc1 Iodine1 Selenium1 rs1000940 rs10132280 rs1016287 rs10182181 rs10733682 rs10938397 rs10968576 rs7103411
rs11057405 rs11126666 rs11165643 rs11191560 rs11583200 rs1167827 rs11688816
rs11727676 rs10134820 rs1885988 rs12286929 rs12401738 rs12429545 rs12446632 rs1514175 rs12885454 rs12940622 rs13021737 rs7622475 rs13107325 rs13191362 rs13201877 rs1441264 rs1460676
rs4234589 rs1528435 rs1421085 rs2035935 rs16907751 rs2241420 rs17001561 rs17024393 rs17094222 rs17203016 rs17405819 rs17724992 rs1808579 rs1928295 rs2033529 rs2033732 rs205262
rs2075650 rs2080454 rs2112347 rs2121279 rs2972143 rs2176598 rs734597 rs2245368 rs2287019 rs2365389 rs2650492 rs2820292 rs2836754 rs29941 rs3101336 rs3736485
rs3810291 rs3817334 rs3849570 rs3888190 rs7113874 rs4740619 rs4787491 rs492400 rs543874 rs6091540 rs6465468 rs6477694 rs6567160 rs657452 rs6804842 rs7138803
rs7141420 rs7164727 rs7239883 rs7243357 rs758747 rs7599312 rs7715256 rs7899106 rs7903146b rs9374842 rs9400239 rs9540493 rs5014937 rs977747 rs9914578 rs9925964 / rsq ctable lackfit outroc= rocwch10com;
roc;
run;

proc export data= rocwch10com
DBMS= xls
outfile= rocwch10com Replace;
run;
































































































