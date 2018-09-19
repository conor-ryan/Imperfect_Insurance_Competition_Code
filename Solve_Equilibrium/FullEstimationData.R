rm(list = ls())
library(data.table)
library(nleqslv)
library(ggplot2)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

## Run
run = "2018-08-25"

#### Read in Data ####

## Simulation Data
simFile = paste("Simulation_Risk_Output/simData_",run,".rData",sep="")
load(simFile)

# ## Risk Function
# riskFile = paste("Simulation_Risk_Output/riskParameters_",run,".rData",sep="")
# load(riskFile)
# f_list = list()
# for (i in 1:length(res_list)){
#   f_list[[i]] = res_list[[i]]$value
# }
# opt = which(unlist(f_list)==min(unlist(f_list)))
# psi_final = res_list[[opt]]$par

## Demand Parameters
parFile = paste("Estimation_Output/estimationresults_",run,".csv",sep="")
pars = read.csv(parFile)
beta_vec = pars$pars

## Cost Function 
costFile = paste("Simulation_Risk_Output/costParameters_MSM_Stage2_",run,".rData",sep="")
load(costFile)
phi = est_res$estimate

#cost_par = CostRes$coefficients[grep("(Age|WTP)",names(CostRes$coefficients))]

## Consolidate Silver Plans ##
full_predict[,Product:=min(Product),by=c("Firm","Metal_std","Market")]
full_predict[,premBase:=median(premBase),by=c("Firm","Metal_std","Market")]

#### Apply Cost Data ####
ST_list = sort(unique(full_predict$ST))
for (fe in ST_list){
  var = paste("FE",fe,sep="_")
  full_predict[,c(var):=0]
  full_predict[ST==fe,c(var):=1]
}


full_predict[,C:=exp(phi[1]*AGE/10 + 
                       phi[2]*HCC_Silver + 
                       phi[3]*AV_std + 
                       phi[4]*FE_AK +
                       phi[5]*FE_GA +
                       phi[6]*FE_IA +
                       phi[7]*FE_IL +
                       phi[8]*FE_MD +
                       phi[9]*FE_MI +
                       phi[10]*FE_MO +
                       phi[11]*FE_ND +
                       phi[12]*FE_NE +
                       phi[13]*FE_NM +
                       phi[14]*FE_OK +
                       phi[15]*FE_OR +
                       phi[16]*FE_TX +
                       phi[17]*FE_UT)]
full_predict[,C_nonAV:=exp(phi[1]*AGE/10 + 
                             phi[2]*HCC_Silver + 
                             #phi[3]*AV_std + 
                             phi[4]*FE_AK +
                             phi[5]*FE_GA +
                             phi[6]*FE_IA +
                             phi[7]*FE_IL +
                             phi[8]*FE_MD +
                             phi[9]*FE_MI +
                             phi[10]*FE_MO +
                             phi[11]*FE_ND +
                             phi[12]*FE_NE +
                             phi[13]*FE_NM +
                             phi[14]*FE_OK +
                             phi[15]*FE_OR +
                             phi[16]*FE_TX +
                             phi[17]*FE_UT)]

full_predict[,insured:=sum(s_pred),by=c("Person")]



## Predict Risk Scores
full_predict[,R:=HCC_age+HCC_Silver]
# full_predict[,R:= HCC_age + AV*(psi_final[1]*WTP+
#                                   psi_final[2]*Age+
#                                   psi_final[3]*Age*WTP)]
# full_predict[,R:= HCC_age + AV*(psi_final[1]*nu_h +
#                                   psi_final[2]*nu_i + 
#                                   psi_final[3]*nu_i*nu_h+
#                                   psi_final[4]*Age+
#                                   psi_final[5]*Age*nu_h)]



# #### Calibrate Risk Transfer Pool ####
# ## Load Risk Score Data
# otherRiskFile = paste("Simulation_Risk_Output/otherRiskScores_",run,".rData",sep="")
# load(otherRiskFile)
# ## Adjust RA Market Shares 
# full_predict = merge(full_predict,other_RA[Firm_Ag=="Inside",c("ST","RA_share")],by="ST",all.x=TRUE)


## Load Risk Score Data
load("Simulation_Risk_Output/FirmRiskScores_woSim.rData")
load(paste("Simulation_Risk_Output/firm_RA_Sim_",run,".rData",sep=""))

firm_RA = merge(firm_RA,firm_RA_Sim,by=c("ST","Firm"),all=TRUE)
setkey(firm_RA,ST,Firm)

firm_RA[,st_share_adj:=memberMonths/sum(memberMonths),by="ST"]
firm_RA[Firm=="OTHER",oth_share:=st_share_adj]
firm_RA[,RA_share:=1-max(oth_share,na.rm=TRUE),by="ST"]
firm_RA[Firm!="OTHER",st_share_adj:=st_share*RA_share]

firm_RA[Firm!="OTHER",R_Gamma_Avg:=sum(st_share_adj*R_Gamma_f),by="ST"]
firm_RA[,R_Gamma_Avg:=max(R_Gamma_Avg,na.rm=TRUE),by="ST"]
firm_RA[Firm=="OTHER",R_Gamma_f:=(T_norm_adj+1)*R_Gamma_Avg/(1-oth_share*(T_norm_adj+1))]

firm_RA[,ST_R:=sum(st_share_adj*R_Gamma_f),by="ST"]
firm_RA[Firm!="OTHER",ST_A:=sum(st_share_adj*A_Gamma_f)/sum(st_share_adj),by="ST"]
firm_RA[,ST_A:=max(ST_A,na.rm=TRUE),by="ST"]
firm_RA[Firm=="OTHER",A_Gamma_f:=ST_A]
firm_RA[,T_est:=R_Gamma_f/ST_R - A_Gamma_f/ST_A]


prod_data = unique(full_predict[,c("Product","Metal_std","ST","Market","Firm",
                                   "premBase","AV_std")])

prod_data = merge(prod_data,firm_RA[,c("ST","Firm","RA_share","R_Gamma_f")],by=c("ST","Firm"),all=TRUE)
names(prod_data) = c("ST","Firm","Product","Metal_std","Market","premBase","AV_std","RA_share","R_f")
prod_data[,C_AV:=exp(phi[3]*AV_std)]

#### Product Level Data ####
# prod_data = full_predict[,list(Age_j = sum(s_pred*AGE*mkt_density)/sum(s_pred*mkt_density),
#                                WTP_j = sum(s_pred*WTP*mkt_density)/sum(s_pred*mkt_density),
#                                Cost_j = sum(s_pred*C*mkt_density)/sum(s_pred*mkt_density),
#                                dsdp = sum(alpha*s_pred*(1-s_pred)*mkt_density)/sum(mkt_density),
#                                R_j = sum(s_pred*HCC_Silver*mkt_density)/sum(s_pred*mkt_density)),
#                          by=c("Product","Metal_std","ST","Market","Firm",
#                               "premBase","AV_std")]


# prod_data = unique(full_predict[,c("Product","Metal_std","ST","Market","Firm",
#                           "premBase","AV_std")])


# ## Normalize at the Firm Level
# firmRiskFile = paste("Simulation_Risk_Output/firmRiskScores_Full_",run,".rData",sep="")
# load(firmRiskFile)
# 
# 
# firmData = full_predict[Metal_std!="CATASTROPHIC",list(lives_f=sum(s_pred*PERWT),
#                                                     R_f_pred=sum(s_pred*PERWT*R*Gamma_j)/sum(s_pred*PERWT)),
#                      by=c("ST","Firm")]
# 
# firmData = merge(firmData,firm_RA[,c("ST","Firm","R_f","RA_share")],by=c("ST","Firm"),all.y=TRUE)
# 
# 
# firmData[R_f==1,R_bench:=R_f_pred]
# firmData[,R_bench:=max(R_bench,na.rm=TRUE),by="ST"]
# firmData[,R_f_pred:=R_f_pred/R_bench]
# firmData[Firm=="OTHER",R_f_pred:=R_f]
# 
# 
# firm_Data = firmData[,c("ST","Firm","R_bench","R_f")]
# 
# 
# prod_data = merge(prod_data,firm_Data,by=c("ST","Firm"),all=TRUE)
# prod_data[,RA_share:=max(RA_share,na.rm=TRUE),by="ST"]


#### Save Data ####


#write.csv(t(cost_par),file="Intermediate_Output/Equilibrium_Data/cost_pars.csv",row.names=FALSE)

predFile = paste("Simulation_Risk_Output/predData_",run,".rData",sep="")
save(full_predict,prod_data,file=predFile)



##### Consolidate Data for Simulation ####

## Eliminate Strings
full_predict[,Catastrophic:=0]
full_predict[METAL=="CATASTROPHIC",Catastrophic:=1]

## Unique Person Variables
full_predict[,Person:=Person*1000+d_ind]


setkey(full_predict,Product,Person)
setkey(prod_data,Product)

### Fill out OTHER Missings
prod_data[Firm=="OTHER",Product:=0]
prod_data[Firm=="OTHER",premBase:=0]
prod_data[Firm=="OTHER",AV_std:=0]
prod_data[Firm=="OTHER",C_AV:=0]


for (st in sort(unique(full_predict$ST))){
  
  write.csv(full_predict[ST==st,c("Person","Product","Catastrophic","AV","Gamma_j",
                                  "R","C","C_nonAV","alpha","WTP","AGE","mkt_density","ageRate","ageRate_avg",
                                  "Mandate","subsidy","IncomeCont","MEMBERS","non_price_util","PERWT")],
            file=paste("Intermediate_Output/Equilibrium_Data/estimated_Data_",st,".csv",sep=""),
            row.names=FALSE)
  
  write.csv(prod_data[ST==st,],
            file=paste("Intermediate_Output/Equilibrium_Data/estimated_prodData_",st,".csv",sep=""),
            row.names=FALSE)
  
}

