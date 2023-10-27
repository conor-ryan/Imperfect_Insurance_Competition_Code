rm(list=ls())
library(doBy)
library(randtoolbox)
library(data.table)
setwd("C:/Users/Conor/Dropbox/Research/Imperfect_Insurance_Competition")


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

res =cf[,summary(lm(PREMI27~under35+METAL+Firm+ST))]

#### Data Products #### 
data_prod = fread("Intermediate_Output/Estimation_Data/marketDataMap_discrete.csv")

data_sample = merge(rating_areas,data_prod,by=c("Market"))
data_sample[,premBase:=premBase*12/1000]
res = data_sample[,lm(premBase~under35+Firm+STATE+METAL)]
print(summary(res))
data_sample[,prem_predict:=predict(res)]
data_sample[,CF_res:=premBase-prem_predict]

control_function = data_sample[,c("Product","CF_res")]
save(control_function,file="Intermediate_Output/Estimation_Data/CF.rData")
