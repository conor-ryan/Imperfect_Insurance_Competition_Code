rm(list=ls())
library(ggplot2)
library(scales)
library(extrafont)
library(grid)
library(doBy)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition")

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
#Uninsured Rate
with(acs,sum(uninsured*PERWT)/sum(PERWT))


#### Match PUMA to Rating Area ####
areaMatch = read.csv("Intermediate_Output/Zip_RatingArea/PUMA_to_RatingArea.csv")
acs = merge(acs,areaMatch[,c("PUMA","RatingArea","ST","STATEFIP")],by=c("STATEFIP","PUMA"),all.x=TRUE)
acs = acs[,c("household","HHincomeFPL","HH_income","AGE","PERWT","RatingArea","ST")]
names(acs) = c("household","HHincomeFPL","HH_income","AGE","PERWT","AREA","ST")

#### Household Characteristics ####
rating = read.csv("Data/AgeRating.csv")

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
acs = acs[,which(!names(acs)%in%c("Rating.x","Rating.y"))]
rm(rating)

#Count Members
acs$MEMBERS=1
#Age of HoH
acs$MaxAge = ave(acs$AGE,acs$household,FUN=max)
#Count Children
acs$childRank = ave(acs$AGE,acs$household,FUN=function(x){rank(x,ties.method="first")})
acs$childRank[acs$AGE>18] = NA
acs$ageRate[!is.na(acs$childRank)&acs$childRank>3]=0

acs$catas_cnt = as.numeric(acs$AGE<30)

acs = summaryBy(MEMBERS+ageRate+PERWT+catas_cnt~household+HHincomeFPL+HH_income+MaxAge+AREA+ST,
               data=acs,FUN=sum,keep.names=TRUE)
names(acs) = c("household","HHincomeFPL","HH_income","AGE","AREA","ST","MEMBERS","ageRate","PERWT","catas_cnt")

acs$FAMILY_OR_INDIVIDUAL = "INDIVIDUAL"
acs$FAMILY_OR_INDIVIDUAL[acs$MEMBERS>1] = "FAMILY"
acs$catas_elig = acs$catas_cnt==acs$MEMBERS

#### Match to Choice Set ####
choiceSets = read.csv("Intermediate_Output/Premiums/choiceSets2015.csv")
acs = merge(acs,choiceSets,by=c("ST","AREA"),all.x=TRUE)


#### Set Correct Characteristics ####

acs = acs[!with(acs,catas_elig & METAL=="CATASTROPHIC"),]

# Remove non-valid areas
acs = acs[acs$valid,]


# Roll in Family Characteristcs
acs$MedDeduct[acs$FAMILY_OR_INDIVIDUAL=="FAMILY"] = 
  acs$MedDeductFam[acs$FAMILY_OR_INDIVIDUAL=="FAMILY"]
acs$MedOOP[acs$FAMILY_OR_INDIVIDUAL=="FAMILY"] = 
  acs$MedOOPFam[acs$FAMILY_OR_INDIVIDUAL=="FAMILY"]


# Keep only relevant variables
acs = acs[,c("ST","household","HHincomeFPL","HH_income","FAMILY_OR_INDIVIDUAL","MEMBERS","AGE","AREA","CARRIER","METAL","hix","PREMI27",
                     "MedDeduct","MedOOP","ageRate","PERWT")]


# Make Premium for Age Rating = 1
acs$premBase = acs$PREMI27/1.048
acs$premBase[acs$ST=="DC"] = acs$PREMI27[acs$ST=="DC"]/.727
acs$premBase[acs$ST=="MA"] = acs$PREMI27[acs$ST=="MA"]/1.22
acs$premBase[acs$ST=="MN"] = acs$PREMI27[acs$ST=="MN"]/1.048
acs$premBase[acs$ST=="UT"] = acs$PREMI27[acs$ST=="UT"]/1.39


# Merge in Benchmark
benchmark = read.csv("Intermediate_Output/Premiums/benchmark2015.csv")
acs = merge(acs,benchmark,by.x=c("ST","AREA"),by.y=c("ST","AREA"),all.x=TRUE)
rm(benchmark)

# Make bechmark for Age Rating = 1
acs$benchBase = acs$bench27/1.048
acs$benchBase[acs$ST=="DC"] = acs$bench27[acs$ST=="DC"]/.727
acs$benchBase[acs$ST=="MA"] = acs$bench27[acs$ST=="MA"]/1.22
acs$benchBase[acs$ST=="MN"] = acs$bench27[acs$ST=="MN"]/1.048
acs$benchBase[acs$ST=="UT"] = acs$bench27[acs$ST=="UT"]/1.39


# Calculate Rated Subsidy
acs$Benchmark = with(acs,benchBase*ageRate)
acs$HHcont = subsPerc(acs$HHincomeFPL)
acs$subsidy = pmax(with(acs,Benchmark - HHcont*HH_income/12),0)


# Calculate Premiums for Choice Set
acs$Quote = with(acs,premBase*ageRate)
acs$PremPaid = pmax(with(acs,premBase*ageRate - subsidy),0)
acs$PremPaid[acs$METAL=="CATASTROPHIC"] = with(acs[acs$METAL=="CATASTROPHIC",],premBase*ageRate)

# Keep Relevant Variables
acs = acs[,c("ST","household","HH_income","HHincomeFPL","FAMILY_OR_INDIVIDUAL","MEMBERS","AGE",
           "AREA","CARRIER","METAL","hix","MedDeduct","MedOOP","ageRate","Benchmark","HHcont","subsidy","Quote","PremPaid","PERWT")]


#### Choice Sets - Cost Sharing ####
acs$METAL = toupper(acs$METAL)
acs$CSR = gsub("[A-Z]+ ?","",acs$METAL,perl=TRUE)
acs$METAL = gsub(" [0-9]+","",acs$METAL)

# Set hix to be TRUE for all Silver, if for any
acs$hix = ave(acs$hix,with(acs,paste(household,CARRIER,METAL)),FUN=any)

acs$CSR_subs = ""
acs$CSR_subs[with(acs,METAL=="SILVER"&hix&HHincomeFPL>2 & HHincomeFPL<=2.5)] = "73"
acs$CSR_subs[with(acs,METAL=="SILVER"&hix&HHincomeFPL>1.5 & HHincomeFPL<=2)] = "87"
acs$CSR_subs[with(acs,METAL=="SILVER"&hix&HHincomeFPL>=1 & HHincomeFPL<=1.5)] = "94"

# Keep only Silver plans for the appropriate income
acs = acs[with(acs,CSR==CSR_subs),]


#### Calculate Mandate Penalty ####
acs$filingThresh = 10150
acs$filingThresh[acs$FAMILY_OR_INDIVIDUAL=="FAMILY"] = 20300
# Calculate Mandate using average income for non-subsidized people.
acs$Mandate = with(acs, pmin(pmax(325,.02*(HH_income-filingThresh)),2484))
acs$Mandate[acs$MEMBERS>1] = with(acs[acs$MEMBERS>1,], pmin(
  pmax(pmin(325*2+325*.5*(MEMBERS-2),975),
       .02*(HH_income-filingThresh)),
  2484*ageRate))


acs = acs[,c("ST","household","FAMILY_OR_INDIVIDUAL","MEMBERS","AGE","AREA","CARRIER","METAL","hix",
                     "MedDeduct","MedOOP","ageRate","HHincomeFPL","Benchmark","HHcont","subsidy","Quote",
                     "PremPaid","HH_income","CSR","Mandate","PERWT")]

#### Dummy Variables ####
acs$Family = 0 
acs$Family[acs$FAMILY_OR_INDIVIDUAL=="FAMILY"] = 1

acs$Youth = 1
acs$Youth[acs$AGE>35] = 0

acs$LowIncome = 1
acs$LowIncome[with(acs,is.na(HHincomeFPL)|HHincomeFPL>4)] = 0

acs$High = 0 
acs$High[with(acs,METAL%in%c("GOLD","PLATINUM")|CSR%in%c("94","87"))] = 1

#### Clean ####
acs$Price = (acs$PremPaid*12-acs$Mandate)/1000
acs$MedDeduct = acs$MedDeduct/1000
acs$MedOOP = acs$MedOOP/1000

acs$Age = 0
acs$Age[acs$AGE>=40] = 1
acs$Person = acs$household

# Product Variables
acs$Firm = gsub(" ","_",acs$CARRIER)
acs$Firm = gsub("[,.&'-:]","",acs$Firm)

acs$Market = with(acs,paste(ST,gsub("Rating Area ","",AREA),sep="_"))

acs$Product_Name = with(acs,paste(Firm,METAL,Market,sep="_"))


acs = acs[with(acs,order(Person)),]

acs = acs[,c("Person","Firm","Market","Product_Name","Price","MedDeduct","MedOOP","High","Family","Age","LowIncome","PERWT")]


#### Merge in Product Map #### 
prod_map = read.csv("Intermediate_Output/marketDataMap.csv")

acs = merge(acs,prod_map,by="Product_Name",all.x=TRUE)
# Drop 0 share products
acs = acs[!is.na(acs$Product),]


#### Read in Parameters ####
n_draws = 500
pars = read.csv("Estimation_Output/estimationresults_fullapprox2018-02-15.csv")

deltas = read.csv("Estimation_Output/deltaresults_fullapprox2018-02-15.csv")

gamma = pars$pars[0:3]
beta = matrix(pars$pars[4:15],nrow=4,ncol=3,byrow=FALSE)
sigma = pars$pars[16:19]

randCoeffs = halton(n_draws,dim=4,usetime=TRUE,normal=TRUE)
for (k in 1:4){
  randCoeffs[,k] = randCoeffs[,k]*sigma[k]
}

acs = merge(acs,deltas,by.x="Product",by.y="prods")
acs = acs[order(acs$Person),]

#### Predict ####
acs$s_pred = NA

people = unique(acs$Person)
cnt = 0

for (p in people){
  cnt = cnt+1
  perData = acs[acs$Person==p,]
  
  demos = as.matrix(perData[1,c("Age","Family","LowIncome")])
  chars = as.matrix(perData[,c("Price","MedDeduct","MedOOP","High")])
  delta = perData$delta
  
  beta_z = demos%*%t(beta)
  beta_zi = matrix(beta_z,nrow=n_draws,ncol=length(beta_z),byrow=TRUE)
  for (k in 1:n_draws){
    beta_zi[k,] = beta_zi[k,] + randCoeffs[k,]
  }
  
  intercept = (demos%*%gamma)[1,1]
  util = matrix(NA,nrow=nrow(chars),ncol=n_draws)
  shares = matrix(NA,nrow=nrow(chars),ncol=n_draws)
  for(k in 1:n_draws){
    util[,k] = exp(intercept + chars%*%beta_zi[k,] + delta)
  }
  expsum = apply(util,MARGIN=2,sum)
  for(k in 1:n_draws){
    shares[,k] = util[,k]/(1+expsum[k])
  }
  acs$s_pred[acs$Person==p] = apply(shares,MARGIN=1,mean)
  if (cnt%%50==0){
    print(cnt)
  }
}

save(acs,file="fullPrediction.rData")


shares = summaryBy(s_pred*PERWT+PERWT~Product+Share+Product_Name,data=acs,FUN=sum,keep.names=TRUE)
shares$s_pred = shares["s_pred * PERWT"]/shares$PERWT

acs$enroll = acs$PERWT*acs$s_pred
acs$ST = gsub("_.*","",acs$Market)
enrollment = summaryBy(enroll~ST+Firm,data=acs,FUN=sum,keep.names=TRUE)
enrollment$share = enrollment$enroll/ave(enrollment$enroll,enrollment$ST,FUN=sum)

#Uninsured Rate
insured = summaryBy(s_pred~Person+PERWT,data=acs,FUN=sum,keep.names=TRUE)
sum(insured$s_pred*insured$PERWT)/sum(insured$PERWT)
