rm(list = ls())
library(data.table)
library(nleqslv)
library(ggplot2)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

## Run
run = "2018-05-12"


#### Read in Data ####
## Geographic Rating Factors
gcf = read.csv("Data/2015_MLR/2015_GCF.csv")
gcf$Market = with(gcf,paste(State,Rating.Area,sep="_"))
gcf=as.data.table(gcf)
setkey(gcf,Market)

## Simulation Data
simFile = paste("Simulation_Risk_Output/simData_",run,".rData",sep="")
load(simFile)


## Risk Function
riskFile = paste("Simulation_Risk_Output/riskParameters_exp_",run,".rData",sep="")
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

#### Merge in Regulation Parameters ####
# Merge in GCF
acs[,ST:=gsub("_.*","",Market)]
setkey(acs,Market)
acs = merge(acs,gcf[,c("Market","GCF")],by="Market")

# Set IDF Values
acs[METAL=="BRONZE",IDF:=1.0]
acs[grepl("SILVER",METAL),IDF:=1.03]
acs[METAL=="GOLD",IDF:=1.08]
acs[METAL=="PLATINUM",IDF:=1.15]


# Set Regulatory Factor
acs[,Gamma_j:=IDF*GCF]



#### Individual Values ####
gamma0 = beta_vec[1]
gamma = beta_vec[2:6]
beta0 = beta_vec[7:8]
beta = matrix(0,nrow=2,ncol=5)
beta[1,1:ncol(beta)] = beta_vec[9:13]
sigma = beta_vec[14:15]
FE_pars = beta_vec[16:length(beta_vec)]



