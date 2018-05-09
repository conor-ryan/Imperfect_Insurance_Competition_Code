rm(list=ls())
library(doBy)
library(randtoolbox)
library(data.table)
library(stargazer)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

## Run 
run = "2018-05-02"

#### Read in Data ####
estData = read.csv("Intermediate_Output/Estimation_Data/descriptiveData_discrete.csv")
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

#estData[,prodCat:=paste(Market,High,sep="_H")]
estData[METAL%in%c("SILVER 87","SILVER 94","GOLD","PLATINUM"),prodCat:="High"]
estData[,Firm_Market_Cat:=paste(Firm,Market,prodCat,sep="_")]

estData[AGE<=30,AgeFE:="18 - 30"]
estData[AGE>30&AGE<=40,AgeFE:="31 - 40"]
estData[AGE>40&AGE<=50,AgeFE:="41 - 50"]
estData[AGE>50,AgeFE:="51 - 64"]


# estData[,IncomeFE:="LowIncome"]
# estData[Income_2==1,IncomeFE:="MiddleIncome"]
# estData[Income_3==1,IncomeFE:="HighIncome"]

#### Create Instrument - Product Market Share in Other States ####
instrument_Data = unique(estData[,c("STATE","AGE_bucket","FPL_bucket","Family","METAL")])
setkey(estData,Person,METAL)
setkey(instrument_Data,METAL)
instrument_Data[,share_instru:=vector(mode="numeric",length=nrow(instrument_Data))]

for (ST in unique(estData$STATE)){
  for (a in unique(estData$AGE_bucket)){
    print(ST)
    print(a)
    for (fpl in unique(estData$FPL_bucket)){
      for (mem in unique(estData$Family)){

        metal_list= instrument_Data$METAL[with(instrument_Data,
                                               AGE_bucket==a&FPL_bucket==fpl&Family==mem&STATE==ST)]
        if (length(metal_list)==0){next}
        inst = as.data.frame(estData[AGE_bucket==a&FPL_bucket==fpl&Family==mem&STATE!=ST,
                       list(s_ins=mean(S_ij)),by="METAL"])
        if(nrow(inst)==0){
          print("FLAG: Remove Family Status")
          print(fpl)
          print(mem)
          inst = as.data.frame(estData[AGE_bucket==a&FPL_bucket==fpl&STATE!=ST,
                         list(s_ins=mean(S_ij)),by="METAL"])
        }
        if(nrow(inst)==0){
          print("FLAG: Remove Income Status")
          print(fpl)
          print(mem)
          inst = as.data.frame(estData[AGE_bucket==a&STATE!=ST,
                         list(s_ins=sum(S_ij*N)/sum(N)),by="METAL"])
        }
        
        inst = inst[order(inst$METAL),]
        instrument_Data[AGE_bucket==a&FPL_bucket==fpl&Family==mem&STATE==ST,
                        share_instru:=inst$s_ins[inst$METAL%in%metal_list]]
      }
    }
  }
}

summary(instrument_Data[METAL=="PLATINUM",])
estData = merge(estData,instrument_Data,by=c("STATE","AGE_bucket","FPL_bucket","Family","METAL"))

#### First Stage Instrument ####
stage1 = lm(nestVar~log(share_instru + 1e-3),data=estData)
summary(stage1)
estData[,nestVar_IV:=predict(stage1)]
estData[,cor(nestVar_IV,nestVar)]


#### Logit Regression Regression ####
ageRangeList = list(c(18,30),c(31,40),c(41,50),c(51,65))
subsList = c(0,1)
famList = c(0,1)




# regData = estData[AGE>=ageRange[1]&AGE<=ageRange[2]&LowIncome==subs&Family==fam,]
#regData = estData[AGE>=ageRange[1]&AGE<=ageRange[2],]

## Regular Logit
# reg1 = lm(regVar~AgeFE + Family + LowIncome + Price*AgeFE + Price*Family +Price*LowIncome + AV + Firm,data=estData)
# c1 = summary(reg1)$coefficients[grep("(Price|MedDeduct|nestVar)",names(reg1$coefficients)),c("Estimate","t value")]
# 
# reg2 = lm(regVar~AgeFE + Family + LowIncome + Price*AgeFE + Price*Family +Price*LowIncome + AV + Firm +Market,data=estData)
# c2 = summary(reg2)$coefficients[grep("(Price|MedDeduct|nestVar)",names(reg2$coefficients)),c("Estimate","t value")]
# 
# reg3 = lm(regVar~AgeFE + Family + LowIncome + Price*AgeFE + Price*Family +Price*LowIncome + MedDeduct + Firm_Market_Cat,data=estData)
# c3 = summary(reg3)$coefficients[grep("(Price|MedDeduct|nestVar)",names(reg3$coefficients)),c("Estimate","t value")]

#reg4 = lm(regVar~AgeFE + Family + LowIncome + Price*AgeFE + Price*Family +Price*LowIncome + productFE,data=estData)
#c4 = summary(reg4)$coefficients[grep("(Price|MedDeduct|nestVar)",names(reg4$coefficients)),c("Estimate","t value")]


## Nested Logit
# reg5 = lm(regVar~AgeFE + Family + LowIncome + Price*AgeFE + Price*Family +Price*LowIncome + AV + nestVar_IV+ Firm,data=estData)
# c5 = summary(reg5)$coefficients[grep("(Price|MedDeduct|nestVar)",names(reg5$coefficients)),c("Estimate","t value")]
# 
# reg6 = lm(regVar~AgeFE + Family + LowIncome + Price*AgeFE + Price*Family +Price*LowIncome + AV + nestVar_IV+ Firm +Market,data=estData)
# c6 = summary(reg6)$coefficients[grep("(Price|MedDeduct|nestVar)",names(reg6$coefficients)),c("Estimate","t value")]
# 
# reg7 = lm(regVar~AgeFE + Family + LowIncome + Price*AgeFE + Price*Family +Price*LowIncome + MedDeduct+ nestVar_IV + Firm_Market_Cat,data=estData)
# c7 = summary(reg7)$coefficients[grep("(Price|MedDeduct|nestVar)",names(reg7$coefficients)),c("Estimate","t value")]

reg8 = lm(regVar~AgeFE + Family + LowIncome + Price*AgeFE + Price*Family +Price*LowIncome + nestVar_IV + productFE,data=estData)
c8 = summary(reg8)$coefficients[grep("(Price|MedDeduct|nestVar)",names(reg8$coefficients)),c("Estimate","t value")]


stargazer(reg8,omit=rep("(METAL|Market|productFE|prodCat|High|Firm_Market_Cat|Firm)",6),style="qje",
          report=("vc*"),single.row=TRUE,
          column.labels=c("Basic Logit","Nested Logit"),column.separate=c(3,3),
          covariate.labels=c("Age 31 - 40","Age 41 - 50","Age 51 - 64","Family",
                             "Subsidized",
                             "Price",#"AV","$\\sigma$","MedDeduct",
                             "$\\sigma$",
                             "Age 31 - 40","Age 41 - 50","Age 51 - 64","Family",
                             "Subsidized","Constant"),
          model.numbers=FALSE,omit.stat=c("adj.rsq","f","ser"),
          dep.var.labels.include=FALSE,dep.var.caption = "")

#save(estData,reg6,file="Estimation_Output/nestReg.rData")

