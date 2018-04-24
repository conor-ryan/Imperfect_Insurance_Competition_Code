rm(list = ls())
library(data.table)
library(nleqslv)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

## Run
run = "2018-04-12"

#### Read in GCF ####
gcf = read.csv("Data/2015_MLR/2015_GCF.csv")
gcf$Market = with(gcf,paste(State,Rating.Area,sep="_"))
gcf=as.data.table(gcf)
setkey(gcf,Market) 

#### Load Simulation Data and Merge in GCF/AV data ####
simFile = paste("Simulation_Risk_Output/simData_",run,".rData",sep="")
firmRiskFile = paste("Simulation_Risk_Output/FirmRiskScores_",run,".rData",sep="")
load(simFile)
load(firmRiskFile)

# Merge in GCF
acs[,Metal:=gsub("([A-Z_]*)(CATASTROPHIC|BRONZE|SILVER|GOLD|PLATINUM)([A-Z_0-9]*)","\\2",acs$Product_Name,perl=TRUE)]
acs[,ST:=gsub("_.*","",Market)]
setkey(acs,Market)
acs = merge(acs,gcf[,c("Market","GCF")],by="Market")

# Set IDF Values
acs[Metal=="BRONZE",IDF:=1.0]
acs[Metal=="SILVER",IDF:=1.03]
acs[Metal=="GOLD",IDF:=1.08]
acs[Metal=="PLATINUM",IDF:=1.15]

# Set AV Values
acs[Metal=="BRONZE",AV:=.6]
acs[Metal=="SILVER",AV:=.7]
acs[Metal=="GOLD",AV:=.8]
acs[Metal=="PLATINUM",AV:=.9]

# Set Regulatory Factor
acs[,Gamma_j:=IDF*GCF]

# Remove Catastrophic Plans for now. 
acs = acs[Metal!="CATASTROPHIC",]

setkey(acs,Product,Person)
setkey(predict_data,Product,Person)

#### Predict Plan Average Allowable Rating Factors ####
parFile = paste("Estimation_Output/estimationresults_",run,".csv",sep="")
pars = read.csv(parFile)

# 
# alpha = pars$pars[1]
# gamma = pars$pars[2:4]
# beta = matrix(pars$pars[5:16],nrow=4,ncol=3,byrow=FALSE)
# sigma = pars$pars[17:21]

gamma = pars$pars[1:3]
beta0 = pars$pars[4:6]
beta = matrix(0,nrow=3,ncol=3)
beta[2,1] = pars$pars[7]
beta[3,1] = pars$pars[8]
sigma = pars$pars[9:11]

alpha = beta0[1]

## Calculate alpha for each demographic ##
acs[,alpha:=alpha+beta[1,1]*Age+beta[1,2]*Family+beta[1,3]*LowIncome]


## Integrate Draws and Prediction Data
draws = as.data.table(draws)
n_draws = nrow(draws)
draws[,d_ind:=as.integer(1:n_draws)]
setkey(draws,d_ind)
setkey(predict_data,d_ind,Person)
nu_h_large = draws[predict_data$d_ind,2]*sign(sigma[3])
nu_i_large = draws[predict_data$d_ind,1]*sign(sigma[1])

predict_data[,alpha_draw:=0]
predict_data[,nu_h:=nu_h_large]
predict_data[,nu_i:=nu_i_large]
rm(nu_h_large,nu_i_large)

## indication
predict_data[,nu_h_ind:=as.numeric(nu_h>.6)]
predict_data[,nu_i_ind:=as.numeric(nu_i>.6)]

#### State Populations ####
states = unique(acs[,c("ST","Person","PERWT")])
states = states[,list(marketPop=sum(PERWT)),by=c("ST")]
acs = merge(acs,states,by="ST")

#### Risk By Product ####
setkey(acs,Product,Person)
setkey(predict_data,Product,Person,d_ind)

prodData = acs[,list(enroll=sum(s_pred_mean*PERWT)),by=c("Product","Metal","Firm","ST","marketPop","AV","Gamma_j")]
prodData[,R_wgt:=vector(mode="numeric",length=nrow(prodData))]
# Check insured rate...
prodData[,ST_lives:=sum(enroll),by="ST"]
products = prodData$Product


prodData = merge(prodData,firm_RA,by=c("Firm","ST"))
setkey(prodData,Product)

# Index through Products In Groups
# Saves Memory
predict_data[,rows:=1:nrow(predict_data)]
acs[,rows:=1:nrow(acs)]
index_j = list()
index_acs = list()
index_pred = list()
index_list = 1
len = 0
step = 700
while (len<length(products)){
  len_next = min(step*index_list,length(products))
  index_j[[index_list]] = products[(len+1):(len_next)]
  j = products[(len+1):(len_next)]
  index_acs[[index_list]] = rep(acs[.(j),rows],each=n_draws)
  index_pred[[index_list]] = predict_data[.(j),rows]
  len = len_next
  index_list = index_list + 1
}
predict_data[,rows:=NULL]
acs[,rows:=NULL]
#### Clear Memory ####
# clear = ls()[!grepl("(acs|predict_data|index|prodData|n_draws|run)",ls())]
# rm(list=clear)

#### Define HCC Age ####
acs[Metal=="PLATINUM",HCC_age:=PlatHCC_Age]
acs[Metal=="GOLD",HCC_age:=GoldHCC_Age]
acs[Metal=="SILVER",HCC_age:=SilvHCC_Age]
acs[Metal=="BRONZE",HCC_age:=BronHCC_Age]
acs[Metal=="CATASTROPHIC",HCC_age:=CataHCC_Age]

acs = acs[,c("Product","Age","Family","LowIncome","HCC_age",
             "Person","alpha","PERWT","AV","Gamma_j","Firm","ST","ageRate_avg")]
# gc()

#### Define Risk Function ####
psi_0 = 0
psi_age = .5
psi_Family = .1
psi_LowIncome = .8
psi_pref1 = 2
psi_pref2 = .2
psi = c(psi_age,psi_Family,psi_LowIncome,psi_pref1,psi_pref2)


risk_predict = merge(acs,predict_data,by=c("Product","Person"))
risk_predict[,pref_h:=nu_h/(alpha+alpha_draw)]
risk_predict[,alpha_i:=alpha+alpha_draw]

## Truncate Pref_H at tails
max_pref_h = quantile(risk_predict$pref_h,probs=.98)
min_pref_h = quantile(risk_predict$pref_h,probs=.02)
risk_predict[pref_h>max_pref_h,pref_h:=max_pref_h]
risk_predict[pref_h<min_pref_h,pref_h:=min_pref_h]
hist(risk_predict$pref_h)

#### Base Firm Risk Data ####
firmRiskFile = paste("Simulation_Risk_Output/FirmRiskScores_Full_",run,".rData",sep="")
load(firmRiskFile)

firm_RA_est = firm_RA[,c("Firm","ST","avg_prem","A_wtd","RA_share","pp_payments_adj","R_f")]
firm_RA_est[,A_sum:=sum(A_wtd),by="ST"]
firm_RA_est[,T_norm_data:=pp_payments_adj/avg_prem]
setkey(firm_RA,ST,Firm)

#### Risk Function 
risk_function_est<- function(psi){
  #for (i in 1:length(index_acs)){
  #cnt=cnt+1
  
  #Sys.time() - st
  
  # #st = Sys.time()
  # dem_data = acs[index_acs[[i]],c("alpha","PERWT","R_dem")]
  # #dem_data = dem_data[rep(1:nrow(dem_data),each=n_draws),]
  # risk_predict = predict_data[index_pred[[i]],]
  # risk_predict[,alpha:=dem_data$alpha]
  # risk_predict[,PERWT:=dem_data$PERWT]
  # risk_predict[,R_dem:=dem_data$R_dem]
  # #Sys.time()-st
  # 
  
  #risk_predict[,pref_ratio:=exp(psi[6]*nu_h)/(1+exp(psi[6]*nu_h))]
  ## Demographic Risk
  risk_predict[,R:= HCC_age + AV*(psi[1]*nu_h + psi[2]*nu_i + psi[3]*nu_i*nu_h+psi[4]*Age+psi[5]*Age*nu_h)]
  #R_wgt_temp = risk_predict[,list(R_wgt=sum(R*s_pred*PERWT/n_draws),enroll=sum(s_pred*PERWT/n_draws)),by="Product"]
  R_wgt_temp = risk_predict[,list(R_wgt=sum(R*Gamma_j*s_pred*PERWT/n_draws)/sum(s_pred*PERWT/n_draws)),by=c("ST","Firm")]
  setkey(R_wgt_temp,ST,Firm)
  

  firm_RA_est[Firm!="OTHER",R_f_pred:=R_wgt_temp$R_wgt]
  
  firm_RA_est[R_f==1,R_bench:=R_f_pred]
  firm_RA_est[,R_bench:=max(R_bench,na.rm=TRUE),by="ST"]
  firm_RA_est[,R_f_pred:=R_f_pred/R_bench]
  firm_RA_est[Firm=="OTHER",R_f_pred:=R_f]
  
  firm_RA_est[,R_sum:=sum(R_f_pred*RA_share),by="ST"]
  firm_RA_est[,T_norm_pred:=-(R_f_pred*RA_share/R_sum - A_wtd/A_sum)]
  
  
  error = firm_RA_est[!Firm%in%c("HEALTH_REPUBLIC_INSURANCE","ASSURANT_HEALTH")
                   ,sum((T_norm_data-T_norm_pred)^2)]
  firm_RA_est[,c("R_f_pred","R_bench","R_sum","T_norm_pred"):=NULL]
  print(error)
  print(psi)
  return(error)
}



# risk_function_est2<- function(psi){
#   prodData[,R_wgt:=vector(mode="numeric",length=nrow(prodData))]
#   ## Demographic Risk
#   acs[,R_dem:= psi[1]+psi[2]*Age+psi[3]*Family+psi[4]*LowIncome]
#   
# 
#   start = Sys.time()
#   for (i in 1:length(index_acs)){
#     #st = Sys.time()
#     #Sys.time() - st
#     
#     #st = Sys.time()
#     dem_data = acs[index_acs[[i]],c("alpha","PERWT","R_dem")]
#     #dem_data = dem_data[rep(1:nrow(dem_data),each=n_draws),]
#     risk_temp = predict_data[index_pred[[i]],]
#     risk_temp[,alpha:=dem_data$alpha]
#     risk_temp[,PERWT:=dem_data$PERWT]
#     risk_temp[,R_dem:=dem_data$R_dem]
#     #Sys.time()-st
#     
#     risk_temp[,pref_h:=nu_h/(alpha+alpha_draw)]
#     risk_temp[,pref_ratio:=exp(psi[6]*pref_h)/(1+exp(psi[6]*pref_h))]
#     
#     risk_temp[,R:=R_dem+psi[5]*pref_ratio]
#     #R_wgt_temp = risk_predict[,list(R_wgt=sum(R*s_pred*PERWT/n_draws),enroll=sum(s_pred*PERWT/n_draws)),by="Product"]
#     R_wgt_temp = risk_temp[,list(R_wgt=sum(R*s_pred*PERWT/n_draws)),by="Product"]
#     prodData[.(index_j[[i]]),R_wgt:=R_wgt_temp$R_wgt]
#     print(i)
#   }
#   
#   #print(Sys.time()-start)
#   
#   prodData[,R_j:=R_wgt/marketPop]
#   
#   #firmData = prodData[,list(R_wgt=sum(R_wgt*Gamma_j),Enroll=sum(S_j)),by=c("Firm","ST","marketPop","R_pred")]
#   firmData = prodData[,list(R_wgt=sum(R_wgt*Gamma_j)),by=c("Firm","ST","marketPop","R_pred")]
#   firmData[,R_f:=R_wgt/marketPop]
#   
#   error = firmData[,sum((R_pred-R_f)^2)]
#   rm(risk_temp,dem_data,firmData)
#   print(error)
#   print(psi)
#   return(error)
# }

psi = rep(0,6)

risk_function_est(psi)
#risk_function_est2(psi)

res_list = list()
f_list = list()
p_list = list()

for (i in 1:10){
  if (i==1){
    psi = c(0,0,0,0,0,0)
  }else{
    psi = c(runif(5)*4-2)
  }
  res = optim(par=psi,fn=risk_function_est,control=list(maxit=2000))
  res_list[[i]]=res
  f_list[[i]] = res$value
  p_list[[i]] = res$par
}



riskFile = paste("Simulation_Risk_Output/riskParameters_exp_",run,".rData",sep="")
save(res_list,file=riskFile)


#### Calibrate Other Firm Risk ####
firmRiskFile = paste("Simulation_Risk_Output/FirmRiskScores_Full_",run,".rData",sep="")
load(firmRiskFile)

#### Check ####
riskFile = paste("Simulation_Risk_Output/riskParameters_exp_",run,".rData",sep="")

load(riskFile)
f_list = list()
for (i in 1:length(res_list)){
  f_list[[i]] = res_list[[i]]$value
}
opt = which(unlist(f_list)==min(unlist(f_list)))
psi = res_list[[opt]]$par

risk_function_est(psi)
#risk_function_est(psi_final_2)

risk_predict[,R:= HCC_age + AV*(psi[1]*nu_h + psi[2]*nu_i+ psi[3]*nu_i*nu_h)]

R_temp = risk_predict[,list(R_wgt=sum(R*s_pred*PERWT/n_draws),
                            enroll = sum(s_pred*PERWT/n_draws),
                            enroll_Fam = sum(Family*s_pred*PERWT/n_draws)/sum(s_pred*PERWT/n_draws),
                            enroll_Age = sum(Age*s_pred*PERWT/n_draws)/sum(s_pred*PERWT/n_draws),
                            enroll_Inc = sum(LowIncome*s_pred*PERWT/n_draws)/sum(s_pred*PERWT/n_draws),
                            nu_h_avg = sum(nu_h*s_pred*PERWT/n_draws)/sum(s_pred*PERWT/n_draws),
                            nu_i_avg = sum(nu_i*s_pred*PERWT/n_draws)/sum(s_pred*PERWT/n_draws)),
                      by=c("Product")]


R_temp[,R_j:=R_wgt/enroll]
prodData[,R_j:=NULL]
prodData[,R_wgt:=NULL]
R_temp[,enroll:=NULL]
with(R_temp,plot(nu_h_avg,R_j))
prodData = merge(prodData,R_temp,by="Product")



firmData = prodData[,list(R_f_pred=sum(enroll*R_j*Gamma_j)/sum(enroll),
                          enroll_Fam=sum(enroll_Fam*enroll)/sum(enroll),
                          enroll_Age=sum(enroll_Age*enroll)/sum(enroll),
                          enroll_Inc=sum(enroll_Inc*enroll)/sum(enroll),
                          nu_h_avg=sum(nu_h_avg*enroll)/sum(enroll),
                          nu_i_avg=sum(nu_i_avg*enroll)/sum(enroll)),
                    by=c("Firm","ST","RA_share","R_f")]
firmData[R_f==1,R_bench:=R_f_pred]
firmData[,R_bench:=max(R_bench,na.rm=TRUE),by="ST"]
firmData[,R_f_pred:=R_f_pred/R_bench]
setkey(firmData,ST,Firm)

firm_RA = merge(firm_RA,firmData[,c("ST","Firm","R_f_pred")],by=c("ST","Firm"),all.x=TRUE)
setkey(firm_RA,ST,Firm)
firm_RA[Firm=="OTHER",R_f_pred:=R_f]

firm_RA[,A_sum:=sum(A_wtd),by="ST"]
firm_RA[,R_sum:=sum(R_f_pred*RA_share),by="ST"]
firm_RA[,transfer_pred:=-ST_MLR_lives*avg_prem*(R_f_pred*RA_share/R_sum - A_wtd/A_sum)]
firm_RA[,transfer_pp:=-avg_prem*(R_f_pred*RA_share/R_sum - A_wtd/A_sum)]

firm_RA[R_f>0,plot(R_f,R_f_pred)]
firm_RA[R_f>0,plot(transfer_pp/avg_prem,pp_payments_adj/avg_prem)]
firm_RA[R_f>0,plot(transfer_pred,payments_adj)]

