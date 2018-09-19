rm(list=ls())
library(doBy)
library(randtoolbox)
library(data.table)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition")

## Run
run = "2018-08-25"

#### 2015 Subsidy Percentage Function ####

subsPerc <- function(FPL){
  x = FPL[!is.na(FPL)]
  y = rep(100,length(x))
  y[x>=1&x<1.33]   = 2.01 + (x-1)[x>=1&x<1.33]/(1.33-1)*(3.02-2.01)
  y[x>=1.33&x<1.5] = 3.02 + (x-1.33)[x>=1.33&x<1.5]/(1.5-1.33)*(4.02-3.02)
  y[x>=1.5&x<2]    = 4.02 + (x-1.5)[x>=1.5&x<2]/(2-1.5)*(6.34-4.02)
  y[x>=2&x<2.5]    = 6.34 + (x-2)[x>=2&x<2.5]/(2.5-2)*(8.1-6.34)
  y[x>=2.5&x<3]    = 8.1 + (x-2.5)[x>=2.5&x<3]/(3-2.5)*(9.56-8.1)
  y[x>=3&x<=4]      = 9.56
  
  HHcont = rep(NA,length(FPL))
  HHcont[!is.na(FPL)] = y/100
  
  return(HHcont)
}


#### Read in  ACS Exchange Elligible Data ####
acs = read.csv("Data/2015_ACS/exchangePopulation2015.csv")
acs = as.data.table(acs)
setkey(acs,STATEFIP,PUMA)
#Uninsured Rate
with(acs,sum(uninsured*PERWT)/sum(PERWT))
acs$person = rownames(acs)

#### Match PUMA to Rating Area ####
areaMatch = read.csv("Intermediate_Output/Zip_RatingArea/PUMA_to_RatingArea.csv")
areaMatch = as.data.table(areaMatch)
acs = merge(acs,areaMatch[,c("PUMA","RatingArea","ST","STATEFIP","alloc")],by=c("STATEFIP","PUMA"),all.x=TRUE,allow.cartesian = TRUE)
# Distribute weight by population prob that observation is in a given Rating Area
acs[,PERWT:=PERWT*alloc]
acs[,household:=as.factor(paste(household,gsub("Rating Area ","",RatingArea),sep="-"))]

acs = acs[,c("household","HHincomeFPL","HH_income","AGE","SEX","PERWT","RatingArea","ST")]
names(acs) = c("household","HHincomeFPL","HH_income","AGE","SEX","PERWT","AREA","ST")




#### Household Characteristics ####
rating = read.csv("Data/AgeRating.csv")
rating = as.data.table(rating)
# Create truncated Age variable
acs$AgeMatch = acs$AGE
acs$AgeMatch[acs$AGE<14] = 14
acs$AgeMatch[acs$AGE>64] = 64

# Merge in Default and State-Specific Age Rating Curves
acs = merge(acs,rating[rating$State=="Default",c("Age","Rating")],by.x="AgeMatch",by.y="Age",all.x=TRUE)
acs = merge(acs,rating[rating$State!="Default",],by.x=c("ST","AgeMatch"),by.y=c("State","Age"),all.x=TRUE)
acs$ageRate = acs$Rating.x
acs$ageRate[!is.na(acs$Rating.y)] = acs$Rating.y[!is.na(acs$Rating.y)]
# Drop redundant rating variables
acs = acs[,c("Rating.x","Rating.y"):=NULL]
rm(rating)


# Merge in Age-specific HHS-HCC Risk Adjustment Factors
HCC = read.csv("Risk_Adjustment/2014_HHS_HCC_AgeRA_Coefficients.csv")
names(HCC) = c("Sex","Age","PlatHCC_Age","GoldHCC_Age","SilvHCC_Age","BronHCC_Age","CataHCC_Age")
acs[,AgeMatch:= pmax(floor(AGE/5)*5,21)]
acs = merge(acs,HCC,by.x=c("AgeMatch","SEX"),by.y=c("Age","Sex"))

#Count Members
setkey(acs,household)
acs$MEMBERS=1
#Age of HoH
acs[,MaxAge:=max(AGE),by="household"]
# Drop heads of household that are under 18 - 2,041
acs = acs[MaxAge>=18,]

#Count Children
acs[,childRank:=rank(AGE,ties.method="first"),by="household"]
acs$childRank[acs$AGE>18] = NA
acs$ageRate[!is.na(acs$childRank)&acs$childRank>3]=0

acs$catas_cnt = as.numeric(acs$AGE<=30)
acs$ageRate_avg = acs$ageRate*acs$PERWT

acs[,PlatHCC_Age:=PlatHCC_Age*PERWT]
acs[,GoldHCC_Age:=GoldHCC_Age*PERWT]
acs[,SilvHCC_Age:=SilvHCC_Age*PERWT]
acs[,BronHCC_Age:=BronHCC_Age*PERWT]
acs[,CataHCC_Age:=CataHCC_Age*PERWT]


acs = acs[,lapply(.SD,sum),by=c("household","HHincomeFPL","HH_income","MaxAge","AREA","ST"),
          .SDcols = c("MEMBERS","ageRate","ageRate_avg","PERWT","catas_cnt",
                      "PlatHCC_Age","GoldHCC_Age","SilvHCC_Age","BronHCC_Age","CataHCC_Age")]

names(acs) = c("household","HHincomeFPL","HH_income","AGE","AREA","ST","MEMBERS","ageRate","ageRate_avg","PERWT","catas_cnt",
               "PlatHCC_Age","GoldHCC_Age","SilvHCC_Age","BronHCC_Age","CataHCC_Age")
acs$ageRate_avg = with(acs,ageRate_avg/PERWT)
acs[,PlatHCC_Age:=PlatHCC_Age/PERWT]
acs[,GoldHCC_Age:=GoldHCC_Age/PERWT]
acs[,SilvHCC_Age:=SilvHCC_Age/PERWT]
acs[,BronHCC_Age:=BronHCC_Age/PERWT]
acs[,CataHCC_Age:=CataHCC_Age/PERWT]


acs$FAMILY_OR_INDIVIDUAL = "INDIVIDUAL"
acs$FAMILY_OR_INDIVIDUAL[acs$MEMBERS>1] = "FAMILY"
acs$catas_elig = acs$catas_cnt==acs$MEMBERS



#### Match to a Risk Draw ####
acs[,riskDraw:= runif(nrow(acs))*.99+.005]

## Risk Moments
r_mom = read.csv("Intermediate_Output/MEPS_Moments/R_Score_Moments.csv")
acs[,Age_Cat:= 0]
acs[AGE>45,Age_Cat:= 1]

acs[,Inc_Cat:= 0]
acs[HHincomeFPL>4,Inc_Cat:= 1]

#acs[,Rtype:= 1+Age_Cat+Inc_Cat*2]
r_mom$Rtype= with(r_mom,1+Age_Cat+Inc_Cat*2)

acs = merge(acs,r_mom,by=c("Age_Cat","Inc_Cat"),all.x=TRUE)

acs[,c("Age_Cat","Inc_Cat"):=NULL]

## Calculate HCC Score
acs[,draws_Any:=(riskDraw-(1-Any_HCC))/(Any_HCC)]
acs[draws_Any<0,draws_Any:=0]

acs[,HCC_Catastrophic:=exp(qnorm(draws_Any)*sqrt(var_HCC_Catastrophic) + mean_HCC_Catastrophic)]
acs[,HCC_Bronze:=exp(qnorm(draws_Any)*sqrt(var_HCC_Bronze) + mean_HCC_Bronze)]
acs[,HCC_Silver:=exp(qnorm(draws_Any)*sqrt(var_HCC_Silver) + mean_HCC_Silver)]
acs[,HCC_Gold:=exp(qnorm(draws_Any)*sqrt(var_HCC_Gold) + mean_HCC_Gold)]
acs[,HCC_Platinum:=exp(qnorm(draws_Any)*sqrt(var_HCC_Platinum) + mean_HCC_Platinum)]

acs[,names(acs)[grepl("(_HCC_|riskDraw|draws_Any)",names(acs))]:=NULL]


# acs$count = 1
# areas = summaryBy(MEMBERS+count~AREA+ST,data=acs,FUN=sum,keep.names=TRUE)

#### Match to Choice Set ####
choiceSets = read.csv("Intermediate_Output/Premiums/choiceSets2015.csv")
choiceSets = as.data.table(choiceSets)
setkey(choiceSets,ST,AREA)
setkey(acs,ST,AREA)
# Drop invalid states and Merge
acs = acs[acs$ST%in%choiceSets$ST,]
acs = merge(acs,choiceSets,by=c("ST","AREA"),all.x=TRUE,allow.cartesian=TRUE)

#### Set Correct Characteristics ####
acs = subset(acs,!(!catas_elig & METAL=="Catastrophic"))

# Remove non-valid areas
acs = acs[acs$valid,]


# Roll in Family Characteristcs
# acs$MedDeduct[acs$FAMILY_OR_INDIVIDUAL=="FAMILY"] = 
#   acs$MedDeductFam[acs$FAMILY_OR_INDIVIDUAL=="FAMILY"]
# acs$MedOOP[acs$FAMILY_OR_INDIVIDUAL=="FAMILY"] = 
#   acs$MedOOPFam[acs$FAMILY_OR_INDIVIDUAL=="FAMILY"]


# Keep only relevant variables
acs = acs[,c("ST","household","HHincomeFPL","HH_income","FAMILY_OR_INDIVIDUAL","MEMBERS","AGE",
             "AREA","Firm","METAL","hix","PREMI27",
             "PlatHCC_Age","GoldHCC_Age","SilvHCC_Age","BronHCC_Age","CataHCC_Age",
             "MedDeduct","MedOOP","ageRate","ageRate_avg","PERWT",
             "HCC_Catastrophic","HCC_Bronze","HCC_Silver","HCC_Gold","HCC_Platinum")]


# Make Premium for Age Rating = 1
acs$premBase = acs$PREMI27/1.048
acs$premBase[acs$ST=="DC"] = acs$PREMI27[acs$ST=="DC"]/.727
acs$premBase[acs$ST=="MA"] = acs$PREMI27[acs$ST=="MA"]/1.22
acs$premBase[acs$ST=="MN"] = acs$PREMI27[acs$ST=="MN"]/1.048
acs$premBase[acs$ST=="UT"] = acs$PREMI27[acs$ST=="UT"]/1.39


# Merge in Benchmark
benchmark = read.csv("Intermediate_Output/Premiums/benchmark2015.csv")
benchmark = as.data.table(benchmark)
acs = merge(acs,benchmark,by.x=c("ST","AREA"),by.y=c("ST","AREA"),all.x=TRUE)
rm(benchmark)

# Make bechmark for Age Rating = 1
acs$benchBase = acs$bench27/1.048
acs$benchBase[acs$ST=="DC"] = acs$bench27[acs$ST=="DC"]/.727
acs$benchBase[acs$ST=="MA"] = acs$bench27[acs$ST=="MA"]/1.22
acs$benchBase[acs$ST=="MN"] = acs$bench27[acs$ST=="MN"]/1.048
acs$benchBase[acs$ST=="UT"] = acs$bench27[acs$ST=="UT"]/1.39


# Calculate Rated Subsidy
acs[,Benchmark:=benchBase*ageRate]
acs[,HHcont:=subsPerc(HHincomeFPL)]
acs[,subsidy:=pmax(Benchmark-HHcont*HH_income/12,0)]


#### Choice Sets - Cost Sharing ####
acs[,METAL:=toupper(METAL)]
acs[,CSR:=gsub("[A-Z]+ ?","",METAL,perl=TRUE)]
acs[,SILVER:=grepl("SILVER",METAL)]
#acs[,METAL:=gsub(" [0-9]+","",METAL)]

## Create Character Difference Variables 
acs$High = 0 
acs$High[with(acs,METAL%in%c("GOLD","PLATINUM")|CSR%in%c("94","87"))] = 1

# acs[CSR=="",MedDeductStandard:=MedDeduct,by=c("ST","AREA","Firm","METAL","FAMILY_OR_INDIVIDUAL")]
# acs[,MedDeductStandard:=max(MedDeductStandard,na.rm=TRUE),by=c("ST","AREA","Firm","METAL","FAMILY_OR_INDIVIDUAL")]
# acs[CSR=="",MedOOPStandard:=MedOOP,by=c("ST","AREA","Firm","METAL","FAMILY_OR_INDIVIDUAL")]
# acs[,MedOOPStandard:=max(MedOOPStandard,na.rm=TRUE),by=c("ST","AREA","Firm","METAL","FAMILY_OR_INDIVIDUAL")]
# acs[CSR=="",HighStandard:=High,by=c("ST","AREA","Firm","METAL","FAMILY_OR_INDIVIDUAL")]
# acs[,HighStandard:=max(HighStandard,na.rm=TRUE),by=c("ST","AREA","Firm","METAL","FAMILY_OR_INDIVIDUAL")]
# 
# acs[,MedDeductDiff:=MedDeduct - MedDeductStandard]
# acs[,MedOOPDiff:=MedOOP - MedOOPStandard]
# acs[,HighDiff:=High - HighStandard]



# Set hix to be TRUE for all Silver, if for any
acs[SILVER==TRUE,hix:= any(hix),by=c("household","Firm")]

acs[,CSR_subs:= ""]
acs[SILVER==TRUE&hix&HHincomeFPL>=2 & HHincomeFPL<2.5,CSR_subs:= "73"]
acs[SILVER==TRUE&hix&HHincomeFPL>=1.5 & HHincomeFPL<2,CSR_subs:= "87"]
acs[SILVER==TRUE&hix&HHincomeFPL>=1 & HHincomeFPL<1.5,CSR_subs:= "94"]

# Keep only Silver plans for the appropriate income
acs = acs[with(acs,CSR==CSR_subs),]


##### Choice Set Stats ####
## Re-Calculate Premiums for Choice Set
acs[,Benchmark:=benchBase*ageRate]
acs[,HHcont:= subsPerc(HHincomeFPL)]
acs[,subsidy:= pmax(Benchmark - HHcont*HH_income/12,0)]
# Leave subsidies below 100 FPL
acs[is.na(HHincomeFPL)|HHincomeFPL>4,subsidy:=0]

acs[,Quote:= premBase*ageRate]
acs[,PremPaid:= pmax(premBase*ageRate - subsidy,0)]
acs$PremPaid[acs$METAL=="CATASTROPHIC"] = with(acs[acs$METAL=="CATASTROPHIC",],premBase*ageRate)


# Per Member Premium
acs[,PremPaid:=PremPaid/MEMBERS]
# Difference Out the Base Premium
acs[,PremPaidDiff:=PremPaid-premBase]


#### Calculate Mandate Penalty ####
acs[,filingThresh:= 10150]
acs$filingThresh[acs$FAMILY_OR_INDIVIDUAL=="FAMILY"] = 20300
# Calculate Mandate using average income for non-subsidized people.
acs[,Mandate:= pmin(pmax(325,.02*(HH_income-filingThresh)),2484)]
acs[MEMBERS>1,Mandate:=min(
  pmax(pmin(325*2+325*.5*(MEMBERS-2),975),
       .02*(HH_income-filingThresh)),
  2484*ageRate)]


#### Dummy Variables ####
acs[,Family:= 0] 
acs[FAMILY_OR_INDIVIDUAL=="FAMILY",Family:= 1]

acs[,LowIncome:= 1]
acs[is.na(HHincomeFPL)|HHincomeFPL>4,LowIncome:= 0]

#### Clean ####
acs[,Price:=(PremPaid*12-Mandate)/1000]
acs[,PriceDiff:=PremPaidDiff*12/1000]
acs$MedDeduct = acs$MedDeduct/1000
acs$MedDeductDiff = acs$MedDeductDiff/1000
acs$MedOOP = acs$MedOOP/1000
acs$MedOOPDiff = acs$MedOOPDiff/1000
acs[,ExcOOP:= (MedOOP - MedDeduct)]
acs[,ExcOOPDiff:= (MedOOPDiff - MedDeductDiff)]

acs[,Age := 0]
acs[AGE>=39,Age:= 1]

acs[,Big:=as.numeric(grepl("UNITED|BLUE|CIGNA|ASSURANT",Firm))]



# Product Variables
acs[,Market:= paste(ST,gsub("Rating Area ","",AREA),sep="_")]

acs[,Product_Name:= paste(Firm,METAL,Market,sep="_")]

acs[,Person:=as.factor(household)]
acs[,Person:=as.numeric(Person)]

acs[,prodCat:=":Low"]
acs[METAL%in%c("SILVER 87","SILVER 94","GOLD","PLATINUM"),prodCat:="High"]
acs[,Firm_Market_Cat:=paste(Firm,Market,prodCat,sep="_")]


#### Merge in Product Map #### 
prod_map = read.csv("Intermediate_Output/Estimation_Data/marketDataMap_discrete.csv")
prod_map = as.data.table(prod_map)

setkey(acs,Product_Name)
setkey(prod_map,Product_Name)
acs = merge(acs,prod_map[,c("Product_Name","Product")],by="Product_Name",all.x=TRUE)
# Drop 0 share products
acs = acs[!is.na(acs$Product),]


#### Fixed Effects ####
# Market Product Category Fixed Effects
firm_list = sort(unique(prod_map$Firm))
firm_list = firm_list[firm_list!="PREMERA_BLUE_CROSS_BLUE_SHIELD_OF_ALASKA"]
for (fe in firm_list){
  var = paste("FE",fe,sep="_")
  acs[,c(var):=0]
  acs[Firm==fe,c(var):=1]
}

acs[,AgeFE_18_30:=0]
acs[AGE<=30,AgeFE_18_30:=1]

acs[,AgeFE_31_39:=0]
acs[AGE>=31&AGE<=39,AgeFE_31_39:=1]

acs[,AgeFE_40_51:=0]
acs[AGE>=40&AGE<=51,AgeFE_40_51:=1]

acs[,AgeFE_52_64:=0]
acs[AGE>=52,AgeFE_52_64:=1]

#### Regulation Parameters ####
## Actuarial Value
acs[METAL=="CATASTROPHIC",AV:=.57]
acs[METAL=="BRONZE",AV:=.6]
acs[METAL=="SILVER",AV:=.7]
acs[METAL=="SILVER 73",AV:=.73]
acs[METAL=="SILVER 87",AV:=.87]
acs[METAL=="SILVER 94",AV:=.94]
acs[METAL=="GOLD",AV:=.8]
acs[METAL=="PLATINUM",AV:=.9]

## Merge in GCF
gcf = read.csv("Data/2015_MLR/2015_GCF.csv")
gcf$Market = with(gcf,paste(State,Rating.Area,sep="_"))
gcf=as.data.table(gcf)
setkey(gcf,Market)

acs = merge(acs,gcf[,c("Market","GCF")],by="Market")

## Induced Demand Factor
acs[METAL=="BRONZE",IDF:=1.0]
acs[METAL%in%c("SILVER","SILVER 73"),IDF:=1.03]
acs[METAL%in%c("SILVER 87","SILVER 94"),IDF:=1.08]
acs[METAL=="GOLD",IDF:=1.08]
acs[METAL=="PLATINUM",IDF:=1.15]

acs[,Gamma_j:=IDF*GCF]

## Consolidate Age-based Risk Scores
acs[METAL=="PLATINUM",HCC_age:=PlatHCC_Age]
acs[METAL=="GOLD",HCC_age:=GoldHCC_Age]
acs[METAL%in%c("SILVER","SILVER 73"),HCC_age:=SilvHCC_Age]
acs[METAL%in%c("SILVER 87","SILVER 94"),HCC_age:=GoldHCC_Age]
acs[METAL=="BRONZE",HCC_age:=BronHCC_Age]
acs[METAL=="CATASTROPHIC",HCC_age:=CataHCC_Age]


rm(gcf)

#### Density Weights ####
density = unique(acs[,c("Market","Person","PERWT")])
density[,mkt_size:=sum(PERWT),by="Market"]

density[,mkt_density:=PERWT/mkt_size]

acs = merge(acs,density[,c("Market","Person","mkt_density")],by=c("Market","Person"))

#### Read in Parameters ####


parFile = paste("Estimation_Output/estimationresults_",run,".csv",sep="")
pars = read.csv(parFile)

delFile = paste("Estimation_Output/deltaresults_",run,".csv",sep="")
deltas = read.csv(delFile)

beta_vec = pars$pars


gamma = beta_vec[1:5]
beta0 = beta_vec[6:8]
beta = matrix(0,nrow=3,ncol=5)
beta[1,1:ncol(beta)] = beta_vec[9:13]
sigma = beta_vec[14:15]
FE_pars = beta_vec[16:length(beta_vec)]


#### Predict ####
acs[,Person:= as.integer(Person)]


demos = as.matrix(acs[,c("AgeFE_31_39",
                         "AgeFE_40_51",
                         "AgeFE_52_64",
                         "Family",
                         "LowIncome")])
chars_risk = as.matrix(acs[,c("AV","Big")])
chars = as.matrix(acs[,c("Price","AV","Big")])


FE = as.matrix(acs[,.SD,.SDcols=names(acs)[grep("^FE_",names(acs))]])

intercept = demos%*%gamma +FE%*%FE_pars


# Only Price Coefficient
beta_z = (demos%*%t(beta))
beta_r = matrix(acs$HCC_Silver,nrow=nrow(acs),ncol=1)

chars_val = intercept + chars%*%beta0 +
  beta_z[,1]*chars[,1] + #Demographic Effect on Price
  beta_r*chars[,2]*sigma[1] + #Risk Effect on AV
  beta_r*chars[,3]*sigma[2] #Risk Effect on Big


acs[,util:=exp(chars_val)]

acs[,alpha:=beta0[1] + beta_z[,1]]
acs[,expsum:=sum(util),by="Person"]
acs[,s_pred:=util/(1+expsum)]


#####################################################################
############# SIMULATION STILL NEEDS TO BE VALIDATED ################
####################################################################



#### Clean and Save Data ####
acs_old = as.data.frame(acs)
acs = acs[,c("Person","Firm","ST","Market","Product_Name","Product",
             "METAL","Mandate","subsidy",
             "Price","PriceDiff","MedDeduct","MedOOP","High","premBase",
             "AV","Big","Gamma_j","mkt_density",
             "alpha","s_pred",
             "HCC_age",#"Rtype","Any_HCC",
             "AGE","HHincomeFPL","MEMBERS",
             "Family","Age","LowIncome","ageRate","ageRate_avg","PERWT",
             "HCC_Silver")]

## Standardize for Silver Plans
acs[,Metal_std:=gsub(" .*","",METAL)]
acs[,AV_std:=AV]
acs[Metal_std=="SILVER",AV_std:=.7]



#### Compact Prediction Data ####
full_predict = acs

## Adjust statistics
full_predict[,WTP:=-0.10*(beta0[2]+sigma[1]*HCC_Silver)/alpha]

## Save Data
simFile = paste("Simulation_Risk_Output/simData_Drawn_",run,".rData",sep="")
save(acs,full_predict,file=simFile)


### Predicted Product Shares ####
shares = acs[,list(e_pred=sum(s_pred*PERWT),pop_offered=sum(PERWT)),by=c("Product","Firm","Market")]
shares[,S_pred:= e_pred/pop_offered]

## Test against moments
share_moment = read.csv("Intermediate_Output/Estimation_Data/marketDataMap_discrete.csv")
share_test = merge(shares,share_moment,by=c("Product","Firm","Market"),all=TRUE)
share_test[,diff:=S_pred-Share]

share_test[,Catas:=0]
share_test[grepl("CATASTROPHIC",Product_Name),Catas:=1]

catas_test = share_test[,list(enroll = sum(e_pred)),by=c("Catas")]
catas_test[,share:=enroll/sum(enroll)]

insured = acs[,list(s_pred=sum(s_pred)),by=c("Person","PERWT","Market")]
insured[,ST:=gsub("_.*","",Market)]
insured = insured[,list(insured=sum(s_pred*PERWT),lives=sum(PERWT)),by="ST"]
insured[,urate:=1 - insured/lives]

unins_st = read.csv("Data/2015_ACS/uninsured_ST_acs2015.csv")
ins_test = merge(insured,unins_st,by.x="ST",by.y="state")

