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


#### Read in Data for Michigan ####
acs = read.csv("Data/2015_ACS/exchangePopulation2015.csv")
#Uninsured Rate
with(acs[acs$STATEFIP==26,],sum(uninsured*PERWT)/sum(PERWT))

MI = acs[acs$STATEFIP==26,]
MI$ST="MI"
rm(acs)

#### Match PUMA to Rating Area ####
areaMatch = read.csv("Intermediate_Output/Zip_RatingArea/PUMA_to_RatingArea.csv")
MI = merge(MI,areaMatch[areaMatch$STATEFIP==26,c("PUMA","RatingArea")],by="PUMA",all.x=TRUE)
MI = MI[,c("household","HHincomeFPL","HH_income","AGE","PERWT","RatingArea","ST")]
names(MI) = c("household","HHincomeFPL","HH_income","AGE","PERWT","AREA","ST")

#### Household Characteristics ####
rating = read.csv("Data/AgeRating.csv")

# Create truncated Age variable
MI$AgeMatch = MI$AGE
MI$AgeMatch[MI$AGE<14] = 14
MI$AgeMatch[MI$AGE>64] = 64

# Merge in Default and State-Specific Age Rating Curves
MI = merge(MI,rating[rating$State=="Default",c("Age","Rating")],by.x="AgeMatch",by.y="Age",all.x=TRUE)
MI = merge(MI,rating[rating$State!="Default",],by.x=c("ST","AgeMatch"),by.y=c("State","Age"),all.x=TRUE)
MI$ageRate = MI$Rating.x
MI$ageRate[!is.na(MI$Rating.y)] = MI$Rating.y[!is.na(MI$Rating.y)]
# Drop redundant rating variables
MI = MI[,which(!names(MI)%in%c("Rating.x","Rating.y"))]
rm(rating)

#Count Members
MI$MEMBERS=1
#Age of HoH
MI$MaxAge = ave(MI$AGE,MI$household,FUN=max)
#Count Children
MI$childRank = ave(MI$AGE,MI$household,FUN=function(x){rank(x,ties.method="first")})
MI$childRank[MI$AGE>18] = NA
MI$ageRate[!is.na(MI$childRank)&MI$childRank>3]=0

MI$catas_cnt = as.numeric(MI$AGE<30)

MI = summaryBy(MEMBERS+ageRate+PERWT+catas_cnt~household+HHincomeFPL+HH_income+MaxAge+AREA+ST,
               data=MI,FUN=sum,keep.names=TRUE)
names(MI) = c("household","HHincomeFPL","HH_income","AGE","AREA","ST","MEMBERS","ageRate","PERWT","catas_cnt")

MI$FAMILY_OR_INDIVIDUAL = "INDIVIDUAL"
MI$FAMILY_OR_INDIVIDUAL[MI$MEMBERS>1] = "FAMILY"
MI$catas_elig = MI$catas_cnt==MI$MEMBERS

#### Match to Choice Set ####
choiceSets = read.csv("Intermediate_Output/Premiums/choiceSets2015.csv")
MI = merge(MI,choiceSets,by=c("ST","AREA"),all.x=TRUE)


#### Set Correct Characteristics ####

MI = MI[!with(MI,catas_elig & METAL=="CATASTROPHIC"),]

# Remove non-valid areas
MI = MI[MI$valid,]


# Roll in Family Characteristcs
MI$MedDeduct[MI$FAMILY_OR_INDIVIDUAL=="FAMILY"] = 
  MI$MedDeductFam[MI$FAMILY_OR_INDIVIDUAL=="FAMILY"]
MI$MedOOP[MI$FAMILY_OR_INDIVIDUAL=="FAMILY"] = 
  MI$MedOOPFam[MI$FAMILY_OR_INDIVIDUAL=="FAMILY"]


# Keep only relevant variables
MI = MI[,c("ST","household","HHincomeFPL","HH_income","FAMILY_OR_INDIVIDUAL","MEMBERS","AGE","AREA","CARRIER","METAL","hix","PREMI27",
                     "MedDeduct","MedOOP","ageRate","PERWT")]


# Make Premium for Age Rating = 1
MI$premBase = MI$PREMI27/1.048
MI$premBase[MI$ST=="DC"] = MI$PREMI27[MI$ST=="DC"]/.727
MI$premBase[MI$ST=="MA"] = MI$PREMI27[MI$ST=="MA"]/1.22
MI$premBase[MI$ST=="MN"] = MI$PREMI27[MI$ST=="MN"]/1.048
MI$premBase[MI$ST=="UT"] = MI$PREMI27[MI$ST=="UT"]/1.39


# Merge in Benchmark
benchmark = read.csv("Intermediate_Output/Premiums/benchmark2015.csv")
MI = merge(MI,benchmark,by.x=c("ST","AREA"),by.y=c("ST","AREA"),all.x=TRUE)
rm(benchmark)

# Make bechmark for Age Rating = 1
MI$benchBase = MI$bench27/1.048
MI$benchBase[MI$ST=="DC"] = MI$bench27[MI$ST=="DC"]/.727
MI$benchBase[MI$ST=="MA"] = MI$bench27[MI$ST=="MA"]/1.22
MI$benchBase[MI$ST=="MN"] = MI$bench27[MI$ST=="MN"]/1.048
MI$benchBase[MI$ST=="UT"] = MI$bench27[MI$ST=="UT"]/1.39


# Calculate Rated Subsidy
MI$Benchmark = with(MI,benchBase*ageRate)
MI$HHcont = subsPerc(MI$HHincomeFPL)
MI$subsidy = pmax(with(MI,Benchmark - HHcont*HH_income/12),0)


# Calculate Premiums for Choice Set
MI$Quote = with(MI,premBase*ageRate)
MI$PremPaid = pmax(with(MI,premBase*ageRate - subsidy),0)
MI$PremPaid[MI$METAL=="CATASTROPHIC"] = with(MI[MI$METAL=="CATASTROPHIC",],premBase*ageRate)

# Keep Relevant Variables
MI = MI[,c("ST","household","HH_income","HHincomeFPL","FAMILY_OR_INDIVIDUAL","MEMBERS","AGE",
           "AREA","CARRIER","METAL","hix","MedDeduct","MedOOP","ageRate","Benchmark","HHcont","subsidy","Quote","PremPaid","PERWT")]


#### Choice Sets - Cost Sharing ####
MI$METAL = toupper(MI$METAL)
MI$CSR = gsub("[A-Z]+ ?","",MI$METAL,perl=TRUE)
MI$METAL = gsub(" [0-9]+","",MI$METAL)

# Set hix to be TRUE for all Silver, if for any
MI$hix = ave(MI$hix,with(MI,paste(household,CARRIER,METAL)),FUN=any)

MI$CSR_subs = ""
MI$CSR_subs[with(MI,METAL=="SILVER"&hix&HHincomeFPL>2 & HHincomeFPL<=2.5)] = "73"
MI$CSR_subs[with(MI,METAL=="SILVER"&hix&HHincomeFPL>1.5 & HHincomeFPL<=2)] = "87"
MI$CSR_subs[with(MI,METAL=="SILVER"&hix&HHincomeFPL>=1 & HHincomeFPL<=1.5)] = "94"

# Keep only Silver plans for the appropriate income
MI = MI[with(MI,CSR==CSR_subs),]


#### Calculate Mandate Penalty ####
MI$filingThresh = 10150
MI$filingThresh[MI$FAMILY_OR_INDIVIDUAL=="FAMILY"] = 20300
# Calculate Mandate using average income for non-subsidized people.
MI$Mandate = with(MI, pmin(pmax(325,.02*(HH_income-filingThresh)),2484))
MI$Mandate[MI$MEMBERS>1] = with(MI[MI$MEMBERS>1,], pmin(
  pmax(pmin(325*2+325*.5*(MEMBERS-2),975),
       .02*(HH_income-filingThresh)),
  2484*ageRate))


MI = MI[,c("ST","household","FAMILY_OR_INDIVIDUAL","MEMBERS","AGE","AREA","CARRIER","METAL","hix",
                     "MedDeduct","MedOOP","ageRate","HHincomeFPL","Benchmark","HHcont","subsidy","Quote",
                     "PremPaid","HH_income","CSR","Mandate","PERWT")]

#### Dummy Variables ####
MI$Family = 0 
MI$Family[MI$FAMILY_OR_INDIVIDUAL=="FAMILY"] = 1

MI$Youth = 1
MI$Youth[MI$AGE>35] = 0

MI$LowIncome = 1
MI$LowIncome[with(MI,is.na(HHincomeFPL)|HHincomeFPL>4)] = 0

MI$High = 0 
MI$High[with(MI,METAL%in%c("GOLD","PLATINUM")|CSR%in%c("94","87"))] = 1

#### Clean ####
MI$Price = (MI$PremPaid*12-MI$Mandate)/1000
MI$MedDeduct = MI$MedDeduct/1000
MI$MedOOP = MI$MedOOP/1000

MI$Age = 0
MI$Age[MI$AGE>=40] = 1
MI$Person = MI$household

# Product Variables
MI$Firm = gsub(" ","_",MI$CARRIER)
MI$Firm = gsub("[,.&'-:]","",MI$Firm)

MI$Market = with(MI,paste(ST,gsub("Rating Area ","",AREA),sep="_"))

MI$Product_Name = with(MI,paste(Firm,METAL,Market,sep="_"))


MI = MI[with(MI,order(Person)),]

MI = MI[,c("Person","Firm","Market","Product_Name","Price","MedDeduct","MedOOP","High","Family","Age","LowIncome","PERWT")]


#### Merge in Product Map #### 
prod_map = read.csv("Intermediate_Output/marketDataMap_MI.csv")

MI = merge(MI,prod_map,by="Product_Name",all.x=TRUE)
# Drop 0 share products
MI = MI[!is.na(MI$Product),]


#### Read in Parameters ####
n_draws = 500
pars = read.csv("Estimation_Output/estimationresults_2018-02-14.csv")

deltas = read.csv("Estimation_Output/deltaresults_2018-02-14.csv")

gamma = pars$pars[0:3]
beta = matrix(pars$pars[4:15],nrow=4,ncol=3,byrow=FALSE)
sigma = pars$pars[16:19]

randCoeffs = halton(n_draws,dim=4,usetime=TRUE,normal=TRUE)
for (k in 1:4){
  randCoeffs[,k] = randCoeffs[,k]*sigma[k]
}

MI = merge(MI,deltas,by.x="Product",by.y="prods")
MI = MI[order(MI$Person),]

#### Predict ####
MI$s_pred = NA

people = unique(MI$Person)
cnt = 0

for (p in people){
  cnt = cnt+1
  perData = MI[MI$Person==p,]
  
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
  MI$s_pred[MI$Person==p] = apply(shares,MARGIN=1,mean)
  if (cnt%%50==0){
    print(cnt)
  }
}


shares = summaryBy(s_pred*PERWT+PERWT~Product+Share+Product_Name,data=MI,FUN=sum,keep.names=TRUE)
shares$s_pred = shares["s_pred * PERWT"]/shares$PERWT

MI$enroll = MI$PERWT*MI$s_pred
enrollment = summaryBy(enroll~Firm,data=MI,FUN=sum,keep.names=TRUE)
enrollment$share = enrollment$enroll/sum(enrollment$enroll)

#Uninsured Rate
insured = summaryBy(s_pred~Person+PERWT,data=MI,FUN=sum,keep.names=TRUE)
sum(insured$s_pred*insured$PERWT)/sum(insured$PERWT)
