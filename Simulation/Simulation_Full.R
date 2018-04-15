rm(list=ls())
library(doBy)
library(randtoolbox)
library(data.table)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition")

## Run
run = "2018-04-12"

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

acs = acs[,c("household","HHincomeFPL","HH_income","AGE","PERWT","RatingArea","ST")]
names(acs) = c("household","HHincomeFPL","HH_income","AGE","PERWT","AREA","ST")




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

#Count Members
setkey(acs,household)
acs$MEMBERS=1
#Age of HoH
acs[,MaxAge:=max(AGE),by="household"]
#Count Children
acs[,childRank:=rank(AGE,ties.method="first"),by="household"]
acs$childRank[acs$AGE>18] = NA
acs$ageRate[!is.na(acs$childRank)&acs$childRank>3]=0

acs$catas_cnt = as.numeric(acs$AGE<=30)
acs$ageRate_avg = acs$ageRate*acs$PERWT
acs = acs[,lapply(.SD,sum),by=c("household","HHincomeFPL","HH_income","MaxAge","AREA","ST"),
          .SDcols = c("MEMBERS","ageRate","ageRate_avg","PERWT","catas_cnt")]

names(acs) = c("household","HHincomeFPL","HH_income","AGE","AREA","ST","MEMBERS","ageRate","ageRate_avg","PERWT","catas_cnt")
acs$ageRate_avg = with(acs,ageRate_avg/PERWT)


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
acs = acs[,c("ST","household","HHincomeFPL","HH_income","FAMILY_OR_INDIVIDUAL","MEMBERS","AGE","AREA","Firm","METAL","hix","PREMI27",
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
acs[,METAL:=gsub(" [0-9]+","",METAL)]

## Create Character Difference Variables 
acs$High = 0 
acs$High[with(acs,METAL%in%c("GOLD","PLATINUM")|CSR%in%c("94","87"))] = 1

acs[CSR=="",MedDeductStandard:=MedDeduct,by=c("ST","AREA","Firm","METAL","FAMILY_OR_INDIVIDUAL")]
acs[,MedDeductStandard:=max(MedDeductStandard,na.rm=TRUE),by=c("ST","AREA","Firm","METAL","FAMILY_OR_INDIVIDUAL")]
acs[CSR=="",MedOOPStandard:=MedOOP,by=c("ST","AREA","Firm","METAL","FAMILY_OR_INDIVIDUAL")]
acs[,MedOOPStandard:=max(MedOOPStandard,na.rm=TRUE),by=c("ST","AREA","Firm","METAL","FAMILY_OR_INDIVIDUAL")]
acs[CSR=="",HighStandard:=High,by=c("ST","AREA","Firm","METAL","FAMILY_OR_INDIVIDUAL")]
acs[,HighStandard:=max(HighStandard,na.rm=TRUE),by=c("ST","AREA","Firm","METAL","FAMILY_OR_INDIVIDUAL")]

acs[,MedDeductDiff:=MedDeduct - MedDeductStandard]
acs[,MedOOPDiff:=MedOOP - MedOOPStandard]
acs[,HighDiff:=High - HighStandard]



# Set hix to be TRUE for all Silver, if for any
acs[,hix:=any(hix),by=c("household","Firm","METAL")]

acs[,CSR_subs:= ""]
acs[METAL=="SILVER"&hix&HHincomeFPL>=2 & HHincomeFPL<2.5,CSR_subs:= "73"]
acs[METAL=="SILVER"&hix&HHincomeFPL>=1.5 & HHincomeFPL<2,CSR_subs:= "87"]
acs[METAL=="SILVER"&hix&HHincomeFPL>=1 & HHincomeFPL<1.5,CSR_subs:= "94"]

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
                        #SMOKER = mean(SMOKER),
                        MEMBERS = sum(MEMBERS*PERWT)/sum(PERWT),
                        HH_income = sum(HH_income*PERWT)/sum(PERWT),
                        HHincomeFPL = sum(HHincomeFPL*PERWT)/sum(PERWT),
                        PERWT = sum(PERWT),
                        subsidy_mean= sum(subsidy*PERWT)/sum(PERWT)),
                  by=c("ST","AREA","FPL_bucket","AGE_bucket","Mem_bucket","FAMILY_OR_INDIVIDUAL","Firm","METAL","hix","CSR",
                       "MedDeduct","MedOOP","High","MedDeductDiff","MedOOPDiff","HighDiff", "benchBase","premBase")]


## Re-Calculate Premiums for Choice Set
acs[,Benchmark:=benchBase*ageRate]
acs[,HHcont:= subsPerc(HHincomeFPL)]
acs[,subsidy:= pmax(Benchmark - HHcont*HH_income/12,0)]
# Leave subsidies below 100 FPL
acs[is.na(HHincomeFPL)|HHincomeFPL>4,subsidy:=0]
acs[HHincomeFPL<1,subsidy:=subsidy_mean]
acs[,diff:=subsidy-subsidy_mean]

acs[,Quote:= premBase*ageRate]
acs[,PremPaid:= pmax(premBase*ageRate - subsidy,0)]
acs$PremPaid[acs$METAL=="CATASTROPHIC"] = with(acs[acs$METAL=="CATASTROPHIC",],premBase*ageRate)


# Per Member Premium
acs[,PremPaid:=PremPaid/MEMBERS]



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
acs$MedDeduct = acs$MedDeduct/1000
acs$MedDeductDiff = acs$MedDeductDiff/1000
acs$MedOOP = acs$MedOOP/1000
acs$MedOOPDiff = acs$MedOOPDiff/1000
acs[,ExcOOP:= (MedOOP - MedDeduct)]
acs[,ExcOOPDiff:= (MedOOPDiff - MedDeductDiff)]

acs[,Age := 0]
acs[AGE>=39,Age:= 1]


# Product Variables
acs[,Market:= paste(ST,gsub("Rating Area ","",AREA),sep="_")]

acs[,Product_Name:= paste(Firm,METAL,Market,sep="_")]

acs[,Person:=as.factor(paste(Market,FPL_bucket,AGE_bucket,Mem_bucket))]
acs[,Person:=as.numeric(Person)]


acs = acs[,c("Person","Firm","Market","Product_Name","Price","MedDeduct","MedOOP","High",
             "MedDeductDiff","MedOOPDiff","HighDiff","Family","Age","LowIncome","ageRate","ageRate_avg","PERWT")]


#### Merge in Product Map #### 
prod_map = read.csv("Intermediate_Output/Estimation_Data/marketDataMap.csv")
prod_map = as.data.table(prod_map)

setkey(acs,Product_Name)
setkey(prod_map,Product_Name)
acs = merge(acs,prod_map[,c("Product_Name","Product")],by="Product_Name",all.x=TRUE)
# Drop 0 share products
acs = acs[!is.na(acs$Product),]




#### Read in Parameters ####
n_draws = 100

parFile = paste("Estimation_Output/estimationresults_",run,".csv",sep="")
pars = read.csv(parFile)

delFile = paste("Estimation_Output/deltaresults_",run,".csv",sep="")
deltas = read.csv(delFile)

# alpha = pars$pars[1]
# gamma = pars$pars[2:4]
# beta = matrix(pars$pars[5:16],nrow=4,ncol=3,byrow=FALSE)
# sigma = pars$pars[17:21]
gamma = pars$pars[1:3]
beta0 = pars$pars[4:6]
beta = matrix(0,nrow=3,ncol=3)
beta[2,1] = pars$pars[7]
beta[3,1] = pars$pars[8]
sigma = pars$pars[9:11]

draws = halton(n_draws,dim=3,usetime=TRUE,normal=TRUE)
randCoeffs = matrix(nrow=n_draws,ncol=length(sigma)+1)
randCoeffs[,1] = draws[,1]*sigma[1]
randCoeffs[,2] = 0
for (k in 3:4){
  randCoeffs[,k] = draws[,2]*sigma[k-1]
}

acs = merge(acs,deltas,by.x="Product",by.y="prods")

#### Convert Data Sets ####
acs[,Person:= as.integer(Person)]
setkey(acs,Person,Product)

people = sort(unique(acs$Person))

predict_data = acs[,c("Person","Product")]
predict_data$s_pred = vector("double",nrow(predict_data))


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
for (p in people){
  cnt = cnt+1
  perData = acs[.(p),]
  
  demos = as.matrix(perData[1,c("Age","Family","LowIncome")])
  #chars = as.matrix(perData[,c("Price","MedDeduct","MedOOP","High")])
  chars = as.matrix(perData[,c("Price","MedDeduct","High")])
  chars_0 = as.matrix(perData[,c("Price","MedDeductDiff","HighDiff")])
  delta = perData$delta
  
  intercept = (demos%*%gamma)[1,1] + randCoeffs[,1]
  
  chars_int = chars_0%*%beta0
  
  beta_z = demos%*%t(beta)
  beta_zi = matrix(beta_z,nrow=n_draws,ncol=length(beta_z),byrow=TRUE)
  for (k in 1:n_draws){
    beta_zi[k,] = beta_zi[k,] + randCoeffs[k,2:4]
  }
  
  
  util = matrix(NA,nrow=nrow(chars),ncol=n_draws)
  shares = matrix(NA,nrow=nrow(chars),ncol=n_draws)
  for(k in 1:n_draws){
    util[,k] = exp(intercept[k] + chars_int + chars%*%beta_zi[k,])*delta
  }
  expsum = apply(util,MARGIN=2,sum)
  for(k in 1:n_draws){
    shares[,k] = util[,k]/(1+expsum[k])
  }
  acs[.(p),s_pred_mean:=apply(shares,MARGIN=1,FUN=mean)]
  predict_data[.(p),s_pred:=as.vector(shares)]
  if (cnt%%500==0){
    print(cnt)
  }
}
Sys.time() - start

simFile = paste("Simulation_Risk_Output/simData_",run,".rData",sep="")
save(acs,predict_data,randCoeffs,file=simFile)


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

