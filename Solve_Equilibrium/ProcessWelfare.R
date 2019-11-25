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
baseData = as.data.table(unique(baseDataFull[,c("Person","ST","Market","PERWT","Age","Family","LowIncome")]))

#### Merge in Market Info ####
file = paste("Estimation_Output/MktHHI_",spec,"-",run,".rData",sep="")
load(file)

baseData = merge(baseData,Mkt,by="Market")

#### Merge in Welfare Info ####
for (cw in c("Base","SP_cp","SP_zp","SP")){
  file = paste("Estimation_Output/consumerWelfare_",cw,"-",spec,"-",run,".csv",sep="")
  data_cw = read.csv(file)
  names(data_cw)[2:4] = paste(names(data_cw)[2:4],cw,sep="_")
  baseData = merge(baseData,data_cw[1:2],by.x="Person",by.y="pers")
}


baseData[,dCW_SP_Base:=CW_Base-CW_SP]
baseData[,dCW_SP_SPcp:=CW_SP_cp-CW_SP]
baseData[,dCW_SP_SPzp:=CW_SP_zp-CW_SP]
baseData[,dCW_SPcp_Base:=CW_SP_cp-CW_Base]

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



ggplot(baseData) + aes(x=CW_SP_cp,y=CW_Base) + 
  geom_point()


ggplot(baseData[LowIncome==1]) + aes(x=dCW_SP_SPcp) + 
  geom_histogram(binwidth=0.1) + 
  coord_cartesian(xlim=c(-4,0.5))

ggplot(baseData[LowIncome==1]) + aes(x=dCW_SP_Base) + 
  geom_histogram(binwidth=0.1) + 
  coord_cartesian(xlim=c(-4,0.5))

ggplot(baseData[LowIncome==1]) + aes(x=dCW_SPcp_Base) + 
  geom_histogram(binwidth=0.1) + 
  coord_cartesian(xlim=c(-4,0.5))



