rm(list = ls())
library(data.table)
library(nleqslv)
library(ggplot2)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

## Run
run = "2018-05-12"

#### Read in Data ####

## Simulation Data
simFile = paste("Simulation_Risk_Output/simData_",run,".rData",sep="")
load(simFile)

## Risk Function
riskFile = paste("Simulation_Risk_Output/riskParameters_",run,".rData",sep="")
load(riskFile)
f_list = list()
for (i in 1:length(res_list)){
  f_list[[i]] = res_list[[i]]$value
}
opt = which(unlist(f_list)==min(unlist(f_list)))
psi_final = res_list[[opt]]$par

## Demand Parameters
parFile = paste("Estimation_Output/estimationresults_",run,".csv",sep="")
pars = read.csv(parFile)
beta_vec = pars$pars


## Consolidate Silver Plans ##
full_predict[,Product:=min(Product),by=c("Firm","Metal_std","Market")]
full_predict[,premBase:=median(premBase),by=c("Firm","Metal_std","Market")]



## Predict Risk Scores
full_predict[,R:= HCC_age + AV*(psi_final[1]*nu_h +
                                  psi_final[2]*nu_i + 
                                  psi_final[3]*nu_i*nu_h+
                                  psi_final[4]*Age+
                                  psi_final[5]*Age*nu_h)]



#### Calibrate Risk Transfer Pool ####
## Load Risk Score Data
otherRiskFile = paste("Simulation_Risk_Output/otherRiskScores_",run,".rData",sep="")
load(otherRiskFile)
## Adjust RA Market Shares 
full_predict = merge(full_predict,other_RA[Firm_Ag=="Inside",c("ST","RA_share")],by="ST",all.x=TRUE)


#### Risk Adjustment Transfers ####
prod_data = unique(full_predict[,c("Product","Metal_std","ST","Market","Firm",
                          "premBase","AV_std","RA_share")])


## Normalize at the Firm Level
firmRiskFile = paste("Simulation_Risk_Output/firmRiskScores_Full_",run,".rData",sep="")
load(firmRiskFile)


firmData = full_predict[Metal_std!="CATASTROPHIC",list(lives_f=sum(s_pred*PERWT),
                                                    R_f_pred=sum(s_pred*PERWT*R*Gamma_j)/sum(s_pred*PERWT)),
                     by=c("ST","Firm")]

firmData = merge(firmData,firm_RA[,c("ST","Firm","R_f","RA_share")],by=c("ST","Firm"),all.y=TRUE)


firmData[R_f==1,R_bench:=R_f_pred]
firmData[,R_bench:=max(R_bench,na.rm=TRUE),by="ST"]
firmData[,R_f_pred:=R_f_pred/R_bench]
firmData[Firm=="OTHER",R_f_pred:=R_f]


firm_Data = firmData[,c("ST","Firm","R_bench","R_f")]


prod_data = merge(prod_data,firm_Data,by=c("ST","Firm"),all=TRUE)
prod_data[,RA_share:=max(RA_share,na.rm=TRUE),by="ST"]


#### Save Data ####
predFile = paste("Simulation_Risk_Output/predData_",run,".rData",sep="")
save(full_predict,prod_data,file=predFile)

write.csv(full_predict,file="Intermediate_Output/Equilibrium_Data/estimated_Data.csv",row.names=FALSE)
write.csv(prod_data,file="Intermediate_Output/Equilibrium_Data/estimated_prodData.csv",row.names=FALSE)