rm(list = ls())
library(data.table)
library(nleqslv)
library(ggplot2)
library(scales)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

run = "2019-11-04"
spec = "FMC"

#### Read in Base Data ####
baseDataFull = read.csv("Intermediate_Output/Simulated_BaseData/simchoiceData_discrete.csv")
baseData = as.data.table(baseDataFull[,c("Person","Firm","ST","Market","Product","PERWT","Price","Age","METAL","Family","LowIncome")])


#### Merge in Welfare Info ####
for (cw in c("Base","SP","SP_cp","SP_zp")){
  file = paste("Estimation_Output/consumerWelfare_",cw,"-",spec,"-",run,".csv",sep="")
  data_cw = read.csv(file)
  names(data_cw)[2:4] = paste(names(data_cw)[2:4],cw,sep="_")
  baseData = merge(baseData,data_cw,by.x="Person",by.y="pers")
}


baseData[,dCW_SP_Base:=CW_risk_Base-CW_risk_SP]
baseData[,dCW_SP_SPcp:=CW_risk_SP_cp-CW_risk_SP]
baseData[,dCW_SP_SPzp:=CW_risk_SP_zp-CW_risk_SP]
baseData[,dCW_SPcp_Base:=CW_risk_Base-CW_risk_SP_cp]

ggplot(baseData) + aes(x=dCW_SP_SPzp) + 
  geom_histogram(binwidth=0.1) + 
  coord_cartesian(xlim=c(-4,0.5))

ggplot(baseData) + aes(x=dCW_SP_SPcp) + 
  geom_histogram(binwidth=0.1) + 
  coord_cartesian(xlim=c(-4,0.5))

ggplot(baseData) + aes(x=dCW_SP_Base) + 
  geom_histogram(binwidth=0.1) + 
  coord_cartesian(xlim=c(-4,0.5))

ggplot(baseData) + aes(x=dCW_SPcp_Base) + 
  geom_histogram(binwidth=0.1) + 
  coord_cartesian(xlim=c(-4,0.5))



ggplot(baseData[LowIncome==1]) + aes(x=dCW_SP_SPzp) + 
  geom_histogram(binwidth=0.1) + 
  coord_cartesian(xlim=c(-4,0.5))

ggplot(baseData[LowIncome==1]) + aes(x=dCW_SP_SPcp) + 
  geom_histogram(binwidth=0.1) + 
  coord_cartesian(xlim=c(-4,0.5))

ggplot(baseData[LowIncome==1]) + aes(x=dCW_SP_Base) + 
  geom_histogram(binwidth=0.1) + 
  coord_cartesian(xlim=c(-4,0.5))

ggplot(baseData[LowIncome==1]) + aes(x=dCW_SPcp_Base) + 
  geom_histogram(binwidth=0.1) + 
  coord_cartesian(xlim=c(-4,0.5))



