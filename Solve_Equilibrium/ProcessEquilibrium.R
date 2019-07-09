rm(list = ls())
library(data.table)
library(nleqslv)
library(ggplot2)
library(scales)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

run = "2019-06-25"

#### Read in Data ####
eqFile = paste("Estimation_Output/solvedEquilibrium_",run,".csv",sep="")
eqData = as.data.table(read.csv(eqFile))

prodData = as.data.table(read.csv("Intermediate_Output/Equilibrium_Data/estimated_prodData_full.csv"))
prodData = prodData[,c("ST","Firm","Product","Metal_std","Market")]

load("Intermediate_Output/Simulated_BaseData/simMarketSize.rData")
prodData = merge(prodData,marketSize,by="Market")

prod_pred = merge(prodData,eqData,by="Product")

#### Compute Shares ####
for (v in c("base","RA","man","RAman")){
  for (m in c("","_m")){
    var_name = paste("Share_",v,m,sep="")
    lives_name = paste("Lives_",v,m,sep="")
    prod_pred[,c(var_name):=.SD/size,.SDcols=c(lives_name)]
  }
}


#### HHI Breakdown ####
Firms = prod_pred[,lapply(.SD,sum),
                    .SDcols=c("Lives_base"),
                    by=c("ST","Market","Firm","size")]
Firms[,insured:=sum(Lives_base),by="Market"]
Firms[,share:=Lives_base/insured]

Mkt = Firms[,list(HHI=sum((share*100)^2)),by="Market"]
Mkt[,HHI_flag:=0]
Mkt[HHI>4000,HHI_flag:=1]
Mkt[HHI>5500,HHI_flag:=2]

prod_pred = merge(prod_pred,Mkt,by="Market")


#### Insured Results ####
Insured = prod_pred[,lapply(.SD,sum),
                    .SDcols=c("Lives_base","Lives_RA","Lives_man","Lives_RAman","Lives_base_m","Lives_RA_m","Lives_man_m","Lives_RAman_m"),
                    by=c("ST","Market","size","HHI_flag")]
Insured = Insured[,lapply(.SD,sum),
                    .SDcols=c("size","Lives_base","Lives_RA","Lives_man","Lives_RAman","Lives_base_m","Lives_RA_m","Lives_man_m","Lives_RAman_m"),
                    by=c("HHI_flag")]

for (v in c("base","RA","man","RAman")){
  for (m in c("","_m")){
    var_name = paste("Ins_",v,m,sep="")
    lives_name = paste("Lives_",v,m,sep="")
    Insured[,c(var_name):=.SD/size,.SDcols=c(lives_name)]
  }
}

#### Premium Results ####

prem = prod_pred[,list(Price_base = sum(Price_base*Lives_base)/sum(Lives_base),
                       Price_RA = sum(Price_RA*Lives_base)/sum(Lives_base),
                       Price_man = sum(Price_man*Lives_base)/sum(Lives_base),
                       Price_RAman = sum(Price_RAman*Lives_base)/sum(Lives_base)),
                 by=c("Metal_std","HHI_flag")]
setkey(prem,HHI_flag,Price_base)
