rm(list=ls())
library(doBy)
library(randtoolbox)
library(data.table)
if (!grepl("Imperfect_Insurance_Competition",getwd())){
  setwd("C:/Users/cxr5626/Dropbox/Research/Imperfect_Insurance_Competition")
}

#### Load Rating Area Demographics ####
load("Intermediate_Output/Simulated_BaseData/rating_demo.rData")
rating_areas[,Market:=paste(ST,gsub("Rating Area ","", AREA),sep="_")]

#### Match to Choice Set ####
choiceSets = read.csv("Intermediate_Output/Premiums/choiceSets2015.csv")
choiceSets = as.data.table(choiceSets)
setkey(choiceSets,ST,AREA)
setkey(rating_areas,ST,AREA)
# Drop invalid states and Merge
rating_areas = rating_areas[rating_areas$ST%in%choiceSets$ST,]
cf = merge(rating_areas,choiceSets,by=c("ST","AREA"),all.x=TRUE,allow.cartesian=TRUE)
# Remove non-valid areas
cf = cf[valid==TRUE,]

# Make Premium for Age Rating = 1
cf[,premBase:=PREMI27/1.048]
cf[ST=="DC",premBase:=PREMI27/.727]
cf[ST=="MA",premBase:=PREMI27/1.22]
cf[ST=="MN",premBase:=PREMI27/1.048]
cf[ST=="UT",premBase:=PREMI27/1.39]
cf[,premBase:=premBase*12/1000]
cf[,premBase_disp:=premBase*1000]

res =cf[,lm(premBase~under35+METAL+Firm+ST)]

cf[,prem_predict:=predict(res)]
cf[,CF_res:=premBase-prem_predict]
cf[,METAL:=toupper(METAL)]
summary(res)

res =cf[,lm(premBase_disp~under35+METAL+Firm+ST)]
summary(res)

#### Data Products #### 
data_prod = fread("Intermediate_Output/Estimation_Data/marketDataMap_discrete.csv")
data_prod[,c("CF_res"):=NULL]
data_sample = merge(rating_areas,data_prod,by=c("Market"))
data_sample[,premBase:=premBase*12/1000]
res = data_sample[,lm(premBase~under35+Firm+STATE+METAL)]
print(summary(res))
## Residual computed on estimation sample, 0.891 R squared with full sample residual
# data_sample[,prem_predict:=predict(res)]
# data_sample[,CF_res:=premBase-prem_predict]

data_sample=merge(data_sample,cf[,c("Firm","METAL","Market","CF_res")],by=c("Firm","METAL","Market"),all=TRUE)

control_function = data_sample[,c("Product","CF_res","Firm","METAL","Market")]
save(control_function,file="Intermediate_Output/Estimation_Data/CF.rData")
