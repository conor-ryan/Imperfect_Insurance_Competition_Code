rm(list=ls())
library(doBy)
library(randtoolbox)
library(data.table)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition")

## Run
run = "2018-09-19"

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
             "AREA","Firm","METAL","hix","PREMI27",
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
          by=c("ST","AREA","FPL_bucket","AGE_bucket","Mem_bucket","FAMILY_OR_INDIVIDUAL","Firm","METAL","hix","CSR",
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
acs[,PremPaid:= pmax(premBase*ageRate - subsidy,0)]
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


## Draws
n_draws = 1000

draws = halton(n_draws+100,dim=1,usetime=TRUE,normal=FALSE)[101:(100+n_draws)]

HCC_draws = matrix(NA,nrow=n_draws,ncol=max(acs$Rtype))

for (j in 1:max(r_mom$Rtype)){
  any = 1 - r_mom$Any_HCC[r_mom$Rtype==j]
  mu = r_mom$mean_HCC_Silver[r_mom$Rtype==j]
  sigma = sqrt(r_mom$var_HCC_Silver[r_mom$Rtype==j])
  draws_any = (draws-any)/(1-any)
  
  log_norm = exp(qnorm(draws_any)*sigma + mu)
  log_norm[is.nan(log_norm)] = 0
  
  HCC_draws[,j] = log_norm
}

HCC_draws_metal = matrix(NA,nrow=n_draws,ncol=max(acs$Rtype)*5)
metal_list = c("Catastrophic","Bronze","Silver","Gold","Platinum")

for (j in 1:max(r_mom$Rtype)){
  for (m in 1:5){
    any = 1 - r_mom$Any_HCC[r_mom$Rtype==j]
    mean_var = paste("mean_HCC",metal_list[m],sep="_")
    sigma_var = paste("var_HCC",metal_list[m],sep="_")
    
    mu = r_mom[[mean_var]][r_mom$Rtype==j]
    sigma = sqrt(r_mom[[sigma_var]][r_mom$Rtype==j])
    draws_any = (draws-any)/(1-any)
    
    log_norm = exp(qnorm(draws_any)*sigma + mu)
    log_norm[is.nan(log_norm)] = 0
    
    HCC_draws_metal[,(m-1)*max(r_mom$Rtype)+j] = log_norm
  }
}

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



randCoeffs = array(data=NA,dim=c(n_draws,ncol(HCC_draws),length(sigma)))
for (k in 1:length(sigma)){
  randCoeffs[,,k] = HCC_draws*sigma[k]
}

acs = merge(acs,deltas,by.x="Product",by.y="prods")

#### Convert Data Sets ####
acs[,Person:= as.integer(Person)]
setkey(acs,Person,Product)

people = sort(unique(acs$Person))

predict_data = acs[,c("Person","Product")]
predict_data[,s_pred:= vector("double",nrow(predict_data))]
predict_data[,non_price_util:= vector("double",nrow(predict_data))]
predict_data[,HCC_Silver:= vector("double",nrow(predict_data))]

## Replicate People in Order
sortedFirst = function(x,y){
  index = vector(mode="integer",length=length(x))
  j = 1
  for (i in 1:length(y)){
    if (x[j]<=y[i]){
      index[j] = i
      j = j+1
      if (j>length(x)){
        return(index)
      }
    }
  }
}

first_inds = sortedFirst(people,predict_data$Person)
first_inds = c(first_inds,nrow(predict_data)+1)

index = vector("list",length(people))

for (i in 1:length(people)){
  ind_temp = first_inds[i]:(first_inds[i+1]-1)
  index[[i]] = ind_temp
}

repl = unlist(lapply(index,FUN=function(x){rep(x,n_draws)}))
ind_draw = unlist(lapply(index,FUN=function(x){rep(1:n_draws,each=length(x))}))

predict_data = predict_data[repl, ]
predict_data[,d_ind:=ind_draw]
setkey(predict_data,Person)

#### Predict ####
cnt = 0
start = Sys.time()
acs[,s_pred_mean:=vector("double",nrow(acs))]
acs[,alpha:=vector("double",nrow(acs))]
gc()
for (p in people){
  cnt = cnt+1
  perData = acs[.(p),]
  
  demos = as.matrix(perData[1,c("AgeFE_31_39",
                                "AgeFE_40_51",
                                "AgeFE_52_64",
                                "Family",
                                "LowIncome")])
  chars = as.matrix(perData[,c("Price","AV","Big")])
  chars_0 = as.matrix(perData[,c("Price","AV","Big")])
  FE = as.matrix(perData[,.SD,.SDcols=names(perData)[grep("^FE_",names(perData))]])
  delta = perData$delta
  
  intercept = (demos%*%gamma)[1,1] #+ randCoeffs[,1]
  
  chars_int = chars_0%*%beta0 + FE%*%FE_pars
  chars_int = matrix(chars_int,nrow=nrow(chars),ncol=n_draws)
  
  beta_z = demos%*%t(beta)
  beta_zi = matrix(beta_z,nrow=n_draws,ncol=length(beta_z),byrow=TRUE)
  r_ind = unique(perData$Rtype)
  # for (n in 1:n_draws){
  #   beta_zi[n,2:ncol(beta_zi)] = beta_zi[n,2:ncol(beta_zi)] + randCoeffs[n,r_ind,]
  # }
  beta_zi[,2:ncol(beta_zi)] = beta_zi[,2:ncol(beta_zi)] + randCoeffs[,r_ind,]
  
  price_val = chars_0[,1]*(beta0[1] + beta_z[,1])
  price_val = matrix(price_val,nrow=nrow(chars),ncol=n_draws)
  
  
  # util = matrix(NA,nrow=nrow(chars),ncol=n_draws)
  # util_non_price = matrix(NA,nrow=nrow(chars),ncol=n_draws)
  shares = matrix(NA,nrow=nrow(chars),ncol=n_draws)
  util = exp(intercept + chars_int + chars%*%t(beta_zi))*delta
  util_non_price = exp(intercept + chars_int + chars%*%t(beta_zi)-price_val)*delta
  
  # for(n in 1:n_draws){
  #   util[,n] = exp(intercept + chars_int + chars%*%beta_zi[n,])*delta
  #   
  #   util_non_price[,n] = exp(intercept + chars_int + chars%*%beta_zi[n,]-price_val)*delta
  # }
  
  
  expsum = apply(util,MARGIN=2,sum)
  expsum = matrix(expsum,nrow=nrow(chars),ncol=n_draws,byrow=TRUE)
  shares = util/(1+expsum)
  # for(n in 1:n_draws){
  #   shares[,n] = util[,n]/(1+expsum[n])
  # }
  acs[.(p),s_pred_mean:=apply(shares,MARGIN=1,FUN=mean)]
  acs[.(p),alpha:=(beta0[1] + beta_z[,1])]
  predict_data[.(p),s_pred:=as.vector(shares)]
  predict_data[.(p),non_price_util:=as.vector(util_non_price)]
  predict_data[.(p),HCC_Silver:=rep(HCC_draws[,r_ind],each=nrow(chars))]
  if (cnt%%500==0){
    print(cnt)
  }
}
Sys.time() - start
#####################################################################
############# SIMULATION STILL NEEDS TO BE VALIDATED ################
####################################################################



#### Clean and Save Data ####
acs_old = as.data.frame(acs)
acs = acs[,c("Person","Firm","ST","Market","Product_Name","Product",
             "METAL","Mandate","subsidy","IncomeCont",
             "Price","PriceDiff","MedDeduct","MedOOP","High","premBase",
             "AV","Big","Gamma_j","mkt_density",
             "alpha","s_pred_mean",
             "HCC_age","Rtype","Any_HCC",
             "AGE","HHincomeFPL","MEMBERS",
             "Family","Age","LowIncome","ageRate","ageRate_avg","PERWT")]

## Standardize for Silver Plans
acs[,Metal_std:=gsub(" .*","",METAL)]
acs[,AV_std:=AV]
acs[Metal_std=="SILVER",AV_std:=.7]


## Integrate Draws and Prediction Data
draws = as.data.table(HCC_draws)
n_draws = nrow(draws)
draws[,d_ind:=as.integer(1:n_draws)]
setkey(draws,d_ind)
setkey(predict_data,d_ind,Person)
#nu_h_large = draws[predict_data$d_ind,2]*sign(sigma[2])
#nu_i_large = draws[predict_data$d_ind,1]*sign(sigma[1])

# predict_data[,alpha_draw:=0]
# predict_data[,nu_h:=nu_h_large]
# predict_data[,nu_i:=nu_i_large]
# rm(nu_h_large,nu_i_large)


#### Compact Prediction Data ####
predict_compact = predict_data[,list(d_ind=mean(d_ind)),
                               by=c("Person","Product","s_pred","non_price_util","HCC_Silver")]
d0_draws = which(apply(HCC_draws,MARGIN=1,FUN=sum)==0)[1]
predict_compact[HCC_Silver==0,d_ind:=d0_draws]

rm(predict_data)

full_predict = merge(acs,predict_compact,by=c("Product","Person"))

full_predict[,cnt:=1]
full_predict[HCC_Silver>0,pos_cnt:=sum(cnt),by=c("Person","Product")]
full_predict[,pos_cnt:=max(pos_cnt,na.rm=TRUE),by=c("Person","Product")]
full_predict[HCC_Silver>0,draw_wgt:=1]
full_predict[HCC_Silver==0,draw_wgt:=n_draws-pos_cnt]
full_predict[,c("cnt","pos_cnt"):=NULL]

## Adjust statistics
full_predict[,WTP:=-0.10*(beta0[2]+sigma[1]*HCC_Silver)/alpha]
full_predict[,mkt_density:=mkt_density/n_draws*draw_wgt]
full_predict[,PERWT:=PERWT/n_draws*draw_wgt]


## Save Data
simFile = paste("Simulation_Risk_Output/simData_old_",run,".rData",sep="")
save(acs,predict_compact,draws,file=simFile)

## Save Data
simFile = paste("Simulation_Risk_Output/simData_",run,".rData",sep="")
save(acs,full_predict,draws,file=simFile)



#### Firm-level Risk Data ####
## Fill in Appropriate HCC RisK Scores
full_predict[,HCC_Metal:=vector(mode="numeric",length=nrow(full_predict))]

HCC_long = as.vector(HCC_draws_metal)

full_predict[AV==.57,Rtype_m:=1]
full_predict[AV==.6,Rtype_m:=2]
full_predict[AV%in%c(.7,.73),Rtype_m:=3]
full_predict[AV%in%c(.87,.8,.94),Rtype_m:=4]
full_predict[AV==.9,Rtype_m:=5]

full_predict[,index:=((Rtype_m-1)*4 + (Rtype-1))*n_draws + d_ind]
full_predict[,HCC_Metal:=HCC_long[index]]
full_predict[,R:=HCC_Metal + HCC_age]

full_predict[,c("index","Rtype_m","HCC_Metal"):=NULL]


## Firm Data
full_predict[,sum(R*s_pred*PERWT)/sum(s_pred*PERWT)]
full_predict[Big==1,sum(R*s_pred*PERWT)/sum(s_pred*PERWT)]

firm_RA_Sim = full_predict[METAL!="CATASTROPHIC",list(enroll = sum(s_pred*PERWT),
                                                      R_Gamma_f=sum(R*Gamma_j*s_pred*PERWT)/sum(s_pred*PERWT),
                                                      R_f=sum(R*s_pred*PERWT)/sum(s_pred*PERWT),
                                 A_Gamma_f=sum(ageRate_avg*AV_std*Gamma_j*s_pred*PERWT)/sum(s_pred*PERWT),
                                 A_f=sum(ageRate_avg*s_pred*PERWT)/sum(s_pred*PERWT)),
                           by=c("Firm","ST")]
firm_RA_Sim[,st_share:=enroll/sum(enroll),by="ST"]
setkey(firm_RA_Sim,ST,Firm)

simFile = paste("Simulation_Risk_Output/firm_RA_Sim_",run,".rData",sep="")
save(firm_RA_Sim,file=simFile)


### Predicted Product Shares ####
shares = acs[,list(e_pred=sum(s_pred_mean*PERWT),pop_offered=sum(PERWT)),by=c("Product","Firm","Market")]
shares[,S_pred:= e_pred/pop_offered]

## Test against moments
share_moment = read.csv("Intermediate_Output/Estimation_Data/marketDataMap_discrete.csv")
share_test = merge(shares,share_moment,by=c("Product","Firm","Market"),all=TRUE)
share_test[,diff:=S_pred-Share]

share_test[,Catas:=0]
share_test[grepl("CATASTROPHIC",Product_Name),Catas:=1]

catas_test = share_test[,list(enroll = sum(e_pred)),by=c("Catas")]
catas_test[,share:=enroll/sum(enroll)]

insured = acs[,list(s_pred=sum(s_pred_mean)),by=c("Person","PERWT","Market")]
insured[,ST:=gsub("_.*","",Market)]
insured = insured[,list(insured=sum(s_pred*PERWT),lives=sum(PERWT)),by="ST"]
insured[,urate:=1 - insured/lives]

unins_st = read.csv("Data/2015_ACS/uninsured_ST_acs2015.csv")
ins_test = merge(insured,unins_st,by.x="ST",by.y="state")

