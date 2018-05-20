rm(list = ls())
library(data.table)
library(nleqslv)
library(ggplot2)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

run = "2018-05-12"

#### Read in Data ####
predFile = paste("Simulation_Risk_Output/predData_",run,".rData",sep="")
load(predFile)


#### Load Equilibrium Solutions ####
eqFiles = list.files("Estimation_Output")[grep("solvedEquil",list.files("Estimation_Output"))]

prod_data = prod_data[Firm!="OTHER",]
prod_data[,prem_pred:= vector(mode="double",nrow(prod_data))]
prod_data[,prem_pred_noT:= vector(mode="double",nrow(prod_data))]
setkey(prod_data,Product)
for (file in eqFiles){
  temp = read.csv(paste("Estimation_Output/",file,sep=""))
  temp = temp[order(temp$Products),]
  no_transfer = grepl("no_t",file)
  
  if(no_transfer){
    prod_data[Product%in%temp$Products,prem_pred_noT:= temp$Prices]
  }else{
    prod_data[Product%in%temp$Products,prem_pred:= temp$Prices]
  }
}



#### Calculate Market Shares / Equilibrium Distribution ####

## Monthly Alpha
full_predict[,alpha:=alpha*12/1000]

## Set Prices and Recalculate Shares
full_predict = merge(full_predict,prod_data[,c("Product","prem_pred","prem_pred_noT")],by="Product")
setkey(full_predict,Person,d_ind,Product)
full_predict[,s_Eq_pred:=vector(mode="double",nrow(full_predict))]
full_predict[,s_Eq_pred_noT:=vector(mode="double",nrow(full_predict))]

for (var in c("","_noT")){
  pvar = paste("prem_pred",var,sep="")
  svar = paste("s_Eq_pred",var,sep="")
  full_predict[,Price_new:=.SD*ageRate-subsidy,.SDcol=pvar]
  full_predict[,Price_new:=pmax(Price_new,0)]
  full_predict[,Price_new:= Price_new/MEMBERS]
  full_predict[METAL=="CATASTROPHIC",Price_new:= (.SD*ageRate)/MEMBERS,.SDcol=pvar]
  full_predict[,Price_new:= (Price_new-Mandate/12)]
  full_predict[,util:=non_price_util*exp(alpha*Price_new)]
  full_predict[,exp_sum:=sum(util),by=c("Person","d_ind")]
  full_predict[,c(svar):=util/(1+exp_sum)]
  full_predict[,c("Price_new","util","exp_sum"):=NULL]
}



prod_pred = full_predict[,list(S_Est = sum(s_pred*mkt_density),
                               S_Eq = sum(s_Eq_pred*mkt_density),
                               R_Eq = sum(s_Eq_pred*R*mkt_density)/sum(s_Eq_pred*mkt_density),
                               AV_Eq = sum(s_Eq_pred*AV*mkt_density)/sum(s_Eq_pred*mkt_density),
                               S_Eq_noT = sum(s_Eq_pred_noT*mkt_density)),
                         by=c("Product")]

prod_pred = merge(prod_pred,prod_data,by="Product")
prod_pred[,R_Eq:=R_Eq/R_bench]


#### Assess Results ####
prod_pred[,diff:=(prem_pred_noT-prem_pred)/prem_pred]
prod_pred[,S_f:=sum(S_Eq),by=c("Firm","Market")]

prod_pred[S_Eq>1e-5,plot(prem_pred,prem_pred_noT)]
prod_pred[S_Eq>1e-5,plot(prem_pred,diff)]
prod_pred[S_Eq>1e-5,plot(AV_Eq,diff)]

