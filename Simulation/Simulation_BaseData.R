rm(list=ls())
library(doBy)
library(randtoolbox)
library(data.table)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition")

## Run
# run = "2019-03-12"

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
             "AREA","Firm","METAL","hix","PREMI27","count_hix_prod",
             "PlatHCC_Age","GoldHCC_Age","SilvHCC_Age","BronHCC_Age","CataHCC_Age",
             "MedDeduct","MedOOP","ageRate","ageRate_avg","PERWT")]


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
acs$High[with(acs,METAL%in%c("GOLD","PLATINUM"))] = 1

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


##### Discretize the Data into Type Buckets #####
acs[,FPL_bucket:= "Less than 1"]
acs[HHincomeFPL>=1&HHincomeFPL<1.5,FPL_bucket:="1 - 1.5"]
acs[HHincomeFPL>=1.5&HHincomeFPL<2,FPL_bucket:="1.5 - 2"]
acs[HHincomeFPL>=2&HHincomeFPL<2.5,FPL_bucket:="2 - 2.5"]
acs[HHincomeFPL>=2.5&HHincomeFPL<4,FPL_bucket:="2.5 - 4"]
acs[is.na(HHincomeFPL)|HHincomeFPL>=4,FPL_bucket:="Greater than 4"]


acs[,AGE_bucket:= "26 or Under"]
acs[AGE>26&AGE<=30,AGE_bucket:= "26-30"]
acs[AGE>30&AGE<=38,AGE_bucket:= "31-38"]
acs[AGE>38&AGE<=46,AGE_bucket:= "39-46"]
acs[AGE>46&AGE<=54,AGE_bucket:= "47-54"]
acs[AGE>54,AGE_bucket:= "55-64"]

acs[,Mem_bucket:= "Single"]
acs[MEMBERS==2,Mem_bucket:= "Couple"]
acs[MEMBERS>=3,Mem_bucket:= "3+"]

#test = as.data.frame(acs)
acs = acs[,list(AGE = mean(AGE),
                ageRate = sum(ageRate*PERWT)/sum(PERWT),
                ageRate_avg = sum(ageRate_avg*PERWT)/sum(PERWT),
                PlatHCC_Age = sum(PlatHCC_Age*PERWT)/sum(PERWT),
                GoldHCC_Age = sum(GoldHCC_Age*PERWT)/sum(PERWT),
                SilvHCC_Age = sum(SilvHCC_Age*PERWT)/sum(PERWT),
                BronHCC_Age = sum(BronHCC_Age*PERWT)/sum(PERWT),
                CataHCC_Age = sum(CataHCC_Age*PERWT)/sum(PERWT),
                #SMOKER = mean(SMOKER),
                MEMBERS = sum(MEMBERS*PERWT)/sum(PERWT),
                HH_income = sum(HH_income*PERWT)/sum(PERWT),
                HHincomeFPL = sum(HHincomeFPL*PERWT)/sum(PERWT),
                PERWT = sum(PERWT),
                subsidy_mean= sum(subsidy*PERWT)/sum(PERWT)),
          by=c("ST","AREA","FPL_bucket","AGE_bucket","Mem_bucket","FAMILY_OR_INDIVIDUAL","Firm","METAL","hix","count_hix_prod","CSR",
               "MedDeduct","MedOOP","High",
               #"MedDeductDiff","MedOOPDiff","HighDiff", 
               "benchBase","premBase")]


## Re-Calculate Premiums for Choice Set
acs[,Benchmark:=benchBase*ageRate]
acs[,HHcont:= subsPerc(HHincomeFPL)]
acs[,subsidy:= pmax(Benchmark - HHcont*HH_income/12,0)]
# Leave subsidies below 100 FPL
acs[is.na(HHincomeFPL)|HHincomeFPL>4|HHcont==1,subsidy:=0]
#acs[HHincomeFPL<1,subsidy:=subsidy_mean]
#acs[,diff:=subsidy-subsidy_mean]

acs[,Quote:= premBase*ageRate]
acs[,subsidy:= pmin(premBase*ageRate,subsidy)]
acs[,PremPaid:= premBase*ageRate - subsidy]
acs$PremPaid[acs$METAL=="CATASTROPHIC"] = with(acs[acs$METAL=="CATASTROPHIC",],premBase*ageRate)


acs[,IncomeCont:=(HHcont*HH_income/1000)]
#acs[HHincomeFPL<1,IncomeCont:=(Benchmark-subsidy)*12/1000]
acs[subsidy==0,IncomeCont:=1e6]


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

acs[,Person:=as.factor(paste(Market,FPL_bucket,AGE_bucket,Mem_bucket))]
acs[,Person:=as.numeric(Person)]

acs[,prodCat:="Low"]
acs[METAL%in%c("GOLD","PLATINUM"),prodCat:="High"]
acs[,Firm_Market:=paste(Firm,Market,sep="_")]
acs[,Firm_Market_Cat:=paste(Firm,Market,prodCat,sep="_")]
acs[,Firm_ST:=paste(Firm,ST,sep="_")]


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
firm_list = sort(unique(acs$Firm_Market))
firm_list = firm_list[firm_list!="PREMERA_BLUE_CROSS_BLUE_SHIELD_OF_ALASKA_AK_1"]
for (fe in firm_list){
  var = paste("FE",fe,sep="_")
  acs[,c(var):=0]
  acs[Firm_Market==fe,c(var):=1]
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

## Standard Actuarial Value
acs[METAL=="CATASTROPHIC",AV_std:=.57]
acs[METAL=="BRONZE",AV_std:=.6]
acs[METAL=="SILVER",AV_std:=.7]
acs[METAL=="SILVER 73",AV_std:=.7]
acs[METAL=="SILVER 87",AV_std:=.7]
acs[METAL=="SILVER 94",AV_std:=.7]
acs[METAL=="GOLD",AV_std:=.8]
acs[METAL=="PLATINUM",AV_std:=.9]

## Cost Sharing Reduction
acs[,AV_diff:=AV-AV_std]


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
acs[is.na(Gamma_j),Gamma_j:=0]

## Consolidate Age-based Risk Scores
acs[METAL=="PLATINUM",HCC_age:=PlatHCC_Age]
acs[METAL=="GOLD",HCC_age:=GoldHCC_Age]
acs[METAL%in%c("SILVER","SILVER 73"),HCC_age:=SilvHCC_Age]
acs[METAL%in%c("SILVER 87","SILVER 94"),HCC_age:=GoldHCC_Age]
acs[METAL=="BRONZE",HCC_age:=BronHCC_Age]
acs[METAL=="CATASTROPHIC",HCC_age:=CataHCC_Age]


## Standardized Silver Plans
acs[,Metal_std:=gsub(" .*","",METAL)]
acs[,Product_std:=min(Product),by=c("Firm","Metal_std","Market")]
acs[,premBase_std:=median(premBase),by=c("Firm","Metal_std","Market")]

rm(gcf)

#### Density Weights ####
density = unique(acs[,c("Market","Person","PERWT")])
density[,mkt_size:=sum(PERWT),by="Market"]

density[,mkt_density:=PERWT/mkt_size]

acs = merge(acs,density[,c("Market","Person","mkt_density")],by=c("Market","Person"))


#### Random Draws ####
## Moments
r_mom = read.csv("Intermediate_Output/MEPS_Moments/R_Score_Moments.csv")
acs[,Age_Cat:= 0]
acs[AGE>45,Age_Cat:= 1]

acs[,Inc_Cat:= 0]
acs[HHincomeFPL>4,Inc_Cat:= 1]

#acs[,Rtype:= 1+Age_Cat+Inc_Cat*2]
r_mom$Rtype= with(r_mom,1+Age_Cat+Inc_Cat*2)

acs = merge(acs,r_mom,by=c("Age_Cat","Inc_Cat"),all.x=TRUE)

acs[,c("Age_Cat","Inc_Cat"):=NULL]


#### Output Analogous Data ####
choiceData = acs[,c("Person","Firm","ST","Firm_ST","Firm_Market","Firm_Market_Cat","Market","Product","PERWT","Price",
                    "MedDeduct","ExcOOP","High","AV","AV_std","AV_diff","Big","Gamma_j",
                    "Mandate","subsidy","benchBase","IncomeCont","mkt_density",
                    "Family","Age","LowIncome","AGE",
                    "METAL","premBase","count_hix_prod",
                    "Metal_std","Product_std","premBase_std",
                    "ageRate","ageRate_avg","HCC_age","SilvHCC_Age","MEMBERS",
                    "mean_HCC_Platinum","mean_HCC_Gold","mean_HCC_Silver","mean_HCC_Bronze","mean_HCC_Catastrophic",
                    "Rtype","Any_HCC",
                    "var_HCC_Platinum","var_HCC_Gold","var_HCC_Silver","var_HCC_Bronze","var_HCC_Catastrophic",
                    "AgeFE_18_30","AgeFE_31_39","AgeFE_40_51","AgeFE_52_64")]


choiceData[,S_ij:=0]
choiceData[,unins_rate:=0]
choiceData[,AGE:=AGE/10]
setkey(choiceData,Person,Product)


write.csv(choiceData,"Intermediate_Output/Simulated_BaseData/simchoiceData_discrete.csv",row.names=FALSE)

#### Output Market Size ####
people = unique(acs[,c("Person","Market","PERWT")])
marketSize = people[,list(size=sum(PERWT)),by=c("Market")]
save(marketSize,file="Intermediate_Output/Simulated_BaseData/simMarketSize.rData")
