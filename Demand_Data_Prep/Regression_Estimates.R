rm(list=ls())
library(doBy)
library(randtoolbox)
library(data.table)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition")

## Run 
run = "2018-05-02"

#### Read in Data ####
estData = read.csv("Intermediate_Output/Estimation_Data/estimationData_discrete.csv")
estData = as.data.table(estData)
setkey(estData,Person,Product)

# Set AV Values
estData[METAL=="CATASTROPHIC",AV:=.57]
estData[METAL=="BRONZE",AV:=.6]
estData[METAL=="SILVER",AV:=.7]
estData[METAL=="SILVER 73",AV:=.73]
estData[METAL=="SILVER 87",AV:=.87]
estData[METAL=="SILVER 94",AV:=.94]
estData[METAL=="GOLD",AV:=.8]
estData[METAL=="PLATINUM",AV:=.9]

##### Prepare for Regression ####
estData[,regVar:= log(S_ij*(1-unins_rate)+1e-3) - log(unins_rate+1e-3)]

estData[,nestVar:= log(S_ij+1e-3)]

estData[,productFE:=as.factor(Product)]
estData[,Firm_Market:=paste(Firm,Market,sep="_")]

estData[,prodCat:="Low"]
estData[METAL%in%c("SILVER 87","SILVER 94","GOLD","PLATINUM"),prodCat:="High"]
estData[,Firm_Market_Cat:=paste(Firm,Market,prodCat,sep="_")]

estData[AGE<=30,AgeFE:="18 - 30"]
estData[AGE>=31&AGE<=40,AgeFE:="31 - 40"]
estData[AGE>=41&AGE<=50,AgeFE:="41 - 50"]
estData[AGE>=51,AgeFE:="51 - 64"]

estData[,ST:=gsub("_.*","",Market)]

#### First Stage Instrument ####
stage1 = lm(nestVar~log(nonexch_unins_rate+1e-3)+ST+METAL,data=estData)

estData[,nestVar_IV:=predict(stage1)]



#### Logit Regression Regression ####
ageRangeList = list(c(18,30),c(31,40),c(41,50),c(51,65))
subsList = c(0,1)
famList = c(0,1)




# regData = estData[AGE>=ageRange[1]&AGE<=ageRange[2]&LowIncome==subs&Family==fam,]
#regData = estData[AGE>=ageRange[1]&AGE<=ageRange[2],]

## Regular Logit
reg1 = lm(regVar~Price*AgeFE + Price*Family +Price*HighIncome + AV + Firm ,data=estData)
c1 = summary(reg1)$coefficients[grep("(Price|^AV$)",names(reg1$coefficients)),c("Estimate","t value")]

reg2 = lm(regVar~Price*AgeFE + Price*Family +Price*HighIncome +AV+Firm_Market,data=estData)
c2 = summary(reg2)$coefficients[grep("(Price|^AV$)",names(reg2$coefficients)),c("Estimate","t value")]

reg3 = lm(regVar~Price*AgeFE + Price*Family +Price*HighIncome+Firm_Market_Cat,data=estData)
c3 = summary(reg3)$coefficients[grep("(Price|^AV$)",names(reg3$coefficients)),c("Estimate","t value")]

reg4 = lm(regVar~Price*AgeFE + Price*Family +Price*HighIncome+productFE,data=estData)
c4 = summary(reg4)$coefficients[grep("(Price|^AV$)",names(reg4$coefficients)),c("Estimate","t value")]

## Nested Logit
reg5 = lm(regVar~Price*AgeFE + Price*Family +Price*HighIncome + AV + nestVar_IV+Firm ,data=estData)
c5 = summary(reg5)$coefficients[grep("(Price|^AV$|nestVar)",names(reg5$coefficients)),c("Estimate","t value")]

reg6 = lm(regVar~Price*AgeFE + Price*Family +Price*HighIncome +AV+nestVar_IV+Firm_Market,data=estData)
c6 = summary(reg6)$coefficients[grep("(Price|^AV$|nestVar)",names(reg6$coefficients)),c("Estimate","t value")]

reg7 = lm(regVar~Price*AgeFE + Price*Family +Price*HighIncome +nestVar_IV + Firm_Market_Cat,data=estData)
c7 = summary(reg7)$coefficients[grep("(Price|^AV$|nestVar)",names(reg7$coefficients)),c("Estimate","t value")]

reg8 = lm(regVar~Price*AgeFE + Price*Family +Price*HighIncome +nestVar_IV + productFE,data=estData)
c8 = summary(reg8)$coefficients[grep("(Price|^AV$|nestVar)",names(reg8$coefficients)),c("Estimate","t value")]

