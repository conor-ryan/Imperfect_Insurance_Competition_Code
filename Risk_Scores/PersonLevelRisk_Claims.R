rm(list = ls())
library(data.table)
library(nleqslv)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

## Run
run = "2018-05-12"

#### Load Simulation Data and Merge in GCF/AV data ####
simFile = paste("Simulation_Risk_Output/simData_",run,".rData",sep="")
firmRiskFile = paste("Simulation_Risk_Output/FirmRiskScores_",run,".rData",sep="")
load(simFile)
load(firmRiskFile)

n_draws = nrow(draws)

# Remove Catastrophic Plans for now. 
acs = acs[METAL!="CATASTROPHIC",]
full_predict = full_predict[METAL!="CATASTROPHIC",]

setkey(acs,Product,Person)
setkey(full_predict,Product,Person)

#### Predict Plan Average Allowable Rating Factors ####
parFile = paste("Estimation_Output/estimationresults_",run,".csv",sep="")
pars = read.csv(parFile)
beta_vec = pars$pars

#### Risk By Product ####
# prodData = acs[,list(enroll=sum(s_pred_mean*PERWT)),by=c("Product","METAL","Firm","ST","marketPop","AV","Gamma_j")]
# prodData[,R_wgt:=vector(mode="numeric",length=nrow(prodData))]
# # Check insured rate...
# prodData[,ST_lives:=sum(enroll),by="ST"]
# products = prodData$Product
# 
# 
# prodData = merge(prodData,firm_RA,by=c("Firm","ST"))
# setkey(prodData,Product)

# # Index through Products In Groups
# # Saves Memory
# acs[,rows:=1:nrow(acs)]
# index_j = list()
# index_acs = list()
# index_pred = list()
# index_list = 1
# len = 0
# step = 700
# while (len<length(products)){
#   len_next = min(step*index_list,length(products))
#   index_j[[index_list]] = products[(len+1):(len_next)]
#   j = products[(len+1):(len_next)]
#   index_acs[[index_list]] = rep(acs[.(j),rows],each=n_draws)
#   index_pred[[index_list]] = predict_data[.(j),rows]
#   len = len_next
#   index_list = index_list + 1
# }
# predict_data[,rows:=NULL]
# acs[,rows:=NULL]


#### Base Firm Risk Data ####
firmRiskFile = paste("Simulation_Risk_Output/FirmRiskScores_Full_",run,".rData",sep="")
load(firmRiskFile)

firm_RA_est = firm_RA[,c("Firm","ST","avg_prem","A_wtd","RA_share","T_norm","T_norm_est","R_f")]
firm_RA_est[,A_sum:=sum(A_wtd),by="ST"]
firm_RA_est[,A_f:=A_wtd/RA_share]

setkey(firm_RA,ST,Firm)
setkey(firm_RA_est,ST,Firm)

#### Firm Metal level Risk Transfers ####
metalClaims = as.data.table(read.csv("Intermediate_Output/Average_Claims/firmClaims.csv"))
metalClaims[,T_avg:=EXP_RSK_ADJ/EXP_MM]

acs[,wgt:=PERWT*s_pred_mean]
metalEst = acs[,list(lives = sum(wgt),
                              Age_j = sum(ageRate_avg*AV*Gamma_j*wgt)/sum(wgt),
                              prod_prem_avg = sum(ageRate_avg*12*premBase*wgt)/sum(wgt)),
                        by=c("Firm","Metal_std","ST")]

ST_RA_Data = unique(firm_RA[,c("ST","avg_prem","R_bench","ST_R","ST_A")])

metalEst = merge(metalEst,ST_RA_Data,by="ST")
setkey(metalEst,ST,Firm,Metal_std)

metalClaims = merge(metalEst,metalClaims,by.x=c("ST","Firm","Metal_std"),by.y=c("STATE","Firm","METAL"),all.x=TRUE)

#metalClaims[,R_j:=(T_avg*12/avg_prem + Age_j/ST_A)*ST_R]
metalClaims[,T_norm_data:=T_avg*12/avg_prem]


# Drop firms with identical transfers across all metal levels
metalClaims[,mean:=mean(T_norm_data),by=c("ST","Firm")]
metalClaims[,noVar:=all(abs(T_norm_data-mean)<1e-3),by=c("ST","Firm")]

metal_moments = metalClaims[!is.na(T_norm_data)&!noVar,list(T_norm_data = sum(T_norm_data*EXP_MM)/sum(EXP_MM)),by=c("Metal_std")]
metal_moments[,Metal_std:=factor(Metal_std,levels=c("BRONZE","SILVER","GOLD","PLATINUM"))]
setkey(metal_moments,Metal_std)

# Wakely Numbers
metal_moments$T_norm_data = c(.814,1.503,1.889,2.675)


# 
# test = metalClaims[,list(T_norm = sum(T_norm_data*lives)/sum(lives),
#                          EXP_RSK_ADJ=sum(EXP_RSK_ADJ)),by=c("ST","Firm")]
# 
# ### Adjust to match firm average risk 
# test = merge(test,firm_RA[,c("ST","Firm","R_f","payments_adj","T_norm_est")],by=c("ST","Firm"),all=TRUE)
# test[,T_adj:= T_norm_est-T_norm]
# 
# metalClaims = merge(metalClaims,test[,c("ST","Firm","T_adj")],by=c("ST","Firm"))
# 
# metalClaims[,T_norm_data:=T_norm_data+T_adj]
# metalClaims[,R_j:=(T_norm_data + Age_j/ST_A)*ST_R]
# 
# test = metalClaims[,list(R_f_test=sum(R_j*lives)/sum(lives),
#                          T_norm = sum((T_norm_data)*lives)/sum(lives),
#                          EXP_RSK_ADJ=sum(EXP_RSK_ADJ)),by=c("ST","Firm")]
# 
# test = merge(test,firm_RA[,c("ST","Firm","R_f","payments_adj","T_norm_est")],by=c("ST","Firm"),all=TRUE)
# 
# 
# 
# metalClaims_Est = metalClaims[,c("ST","Firm","Metal_std","Age_j","ST_A","T_norm_data","R_j")]
# setkey(metalClaims_Est,ST,Firm,Metal_std)


##### Output Moments ####
mkt_data = read.csv("Intermediate_Output/Estimation_Data/marketDataMap_discrete.csv")
mkt_data = mkt_data[mkt_data$METAL!="CATASTROPHIC",]
mkt_data$METAL = gsub(" [0-9]+","",mkt_data$METAL)

## State Shares
population = unique(acs[,c("Person","ST","Market","PERWT")])
population = population[,list(pop = sum(PERWT)),by=c("ST","Market")]
population[,st_share:=pop/sum(pop),by="ST"]

mkt_data = merge(mkt_data,population[,c("ST","Market","st_share")],
                 by.x=c("STATE","Market"),by.y=c("ST","Market"))
## Risk Moments
# 
# # Drop firms with identical transfers across all metal levels
# metalClaims_Est[,mean:=mean(T_norm_data),by=c("ST","Firm")]
# metalClaims_Est[,noVar:=all(abs(T_norm_data-mean)<1e-3),by=c("ST","Firm")]
# 
# 
# metal_moments = metalClaims_Est[!is.na(T_norm_data)&!noVar,c("ST","Firm","Metal_std","T_norm_data")]
metal_moments[,momentID:=1:nrow(metal_moments)]

firm_moments = firm_RA[Firm!="OTHER",c("ST","Firm","T_norm_est")]
firm_moments[,momentID:=nrow(metal_moments)+1:nrow(firm_moments)]

metal_moments = merge(mkt_data[,c("STATE","Firm","METAL","Product","st_share")],metal_moments,
                      by.x=c("METAL"),by.y=c("Metal_std"))
firm_moments = merge(mkt_data[,c("STATE","Firm","Product","st_share")],firm_moments,
                      by.x=c("STATE","Firm"),by.y=c("ST","Firm"))

metal_moments = metal_moments[,c("Product","momentID","T_norm_data","st_share","STATE")]
names(metal_moments) = c("Product","momentID","T_moment","st_share","ST")
firm_moments = firm_moments[,c("Product","momentID","T_norm_est","st_share","STATE")]
names(firm_moments) = c("Product","momentID","T_moment","st_share","ST")

# Total Average Risk
total_moment = mkt_data[,c("STATE","Firm","Product","st_share")]
total_moment$T_moment = 1.448
#total_moment$momentID = max(firm_moments$momentID) + 1
total_moment$momentID = max(metal_moments$momentID) + 1
total_moment$ST = total_moment$STATE

total_moment = total_moment[,c("Product","momentID","T_moment","st_share","ST")]


# risk_moments = rbind(metal_moments,firm_moments)
# risk_moments = rbind(risk_moments,total_moment)
risk_moments = rbind(metal_moments,total_moment)
#risk_moments = total_moment
#risk_moments$momentID = 1
risk_moments$ST = as.numeric(risk_moments$ST)


write.csv(risk_moments,"Intermediate_Output/Estimation_Data/riskMoments.csv",row.names=FALSE)

#### Define Risk Function ####
psi_0 = 0
psi_age = .5
psi_Family = .1
psi_LowIncome = .8
psi_pref1 = 2
psi_pref2 = .2
psi = c(psi_age,psi_Family,psi_LowIncome,psi_pref1,psi_pref2)


## Truncate Pref_H at tails
# max_pref_h = quantile(risk_predict$pref_h,probs=.98)
# min_pref_h = quantile(risk_predict$pref_h,probs=.02)
# risk_predict[pref_h>max_pref_h,pref_h:=max_pref_h]
# risk_predict[pref_h<min_pref_h,pref_h:=min_pref_h]
# hist(risk_predict$pref_h)

#### Risk Function 
risk_function_est<- function(psi){
  ## Demographic Risk
  full_predict[,R:= HCC_age + AV*(psi[1]*WTP+psi[2]*Age+psi[3]*Age*WTP)]

  ## Firm Level Risk
  R_wgt_temp = full_predict[,list(R_wgt=sum(R*Gamma_j*s_pred*PERWT/n_draws)/sum(s_pred*PERWT/n_draws)),by=c("ST","Firm")]
  setkey(R_wgt_temp,ST,Firm)
  

  firm_RA_est[Firm!="OTHER",R_f_pred:=R_wgt_temp$R_wgt]
  
  firm_RA_est[R_f==1,R_bench:=R_f_pred]
  firm_RA_est[,R_bench:=max(R_bench,na.rm=TRUE),by="ST"]
  firm_RA_est[,R_f_pred:=R_f_pred/R_bench]
  firm_RA_est[Firm=="OTHER",R_f_pred:=R_f]
  
  firm_RA_est[,R_sum:=sum(R_f_pred*RA_share),by="ST"]
  firm_RA_est[,T_norm_pred:=-(R_f_pred/R_sum - A_f/A_sum)]
  
  ## Product Level Risk
  R_wgt_temp = full_predict[,list(R_wgt=sum(R*Gamma_j*s_pred*PERWT/n_draws)/sum(s_pred*PERWT/n_draws)),by=c("ST","Firm","Metal_std")]
  setkey(R_wgt_temp,ST,Firm,Metal_std)
  
  metalClaims_Est[,R_j_pred:=R_wgt_temp$R_wgt]
  ST_bench = unique(as.data.frame(firm_RA_est[,c("ST","R_sum","R_bench")]))
  
  prod_predict = merge(metalClaims_Est,ST_bench,by=c("ST"))
  prod_predict[,R_j_pred:=R_j_pred/R_bench]
  prod_predict[,T_norm_pred:=-(R_j_pred/R_sum - Age_j/ST_A)]
  
  
  error_firm = firm_RA_est[!Firm%in%c("HEALTH_REPUBLIC_INSURANCE","ASSURANT_HEALTH")
                   ,sum((T_norm_data-T_norm_pred)^2)]

  error_prod = prod_predict[,sum((T_norm_data-T_norm_pred)^2,na.rm=TRUE)]
  #error = error_firm + error_prod
  error = error_prod
    
  firm_RA_est[,c("R_f_pred","R_bench","R_sum","T_norm_pred"):=NULL]
  metalClaims_Est[,c("R_j_pred"):=NULL]
  rm(prod_predict)
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
    psi = c(0,0,0)
  }else{
    psi = c(runif(3)*4-2)
  }
  res = optim(par=psi,fn=risk_function_est,control=list(maxit=2000))
  res_list[[i]]=res
  f_list[[i]] = res$value
  p_list[[i]] = res$par
}



riskFile = paste("Simulation_Risk_Output/riskParameters_",run,".rData",sep="")
save(res_list,file=riskFile)








#### Calibrate Other Firm Risk ####
firmRiskFile = paste("Simulation_Risk_Output/FirmRiskScores_Full_",run,".rData",sep="")
load(firmRiskFile)

#### Check ####
riskFile = paste("Simulation_Risk_Output/riskParameters_",run,".rData",sep="")

load(riskFile)
f_list = list()
for (i in 1:length(res_list)){
  f_list[[i]] = res_list[[i]]$value
}
opt = which(unlist(f_list)==min(unlist(f_list)))
psi = res_list[[opt]]$par

risk_function_est(psi)
# 
# 
# 
# 
# 
# 
# #risk_function_est(psi_final_2)
# 
# #risk_predict[,R:= HCC_age + AV*(psi[1]*nu_h + psi[2]*nu_i+ psi[3]*nu_i*nu_h)]
# 
# R_temp = full_predict[,list(R_wgt=sum(R*s_pred*PERWT/n_draws),
#                             enroll = sum(s_pred*PERWT/n_draws),
#                             enroll_Fam = sum(Family*s_pred*PERWT/n_draws)/sum(s_pred*PERWT/n_draws),
#                             enroll_Age = sum(Age*s_pred*PERWT/n_draws)/sum(s_pred*PERWT/n_draws),
#                             enroll_Inc = sum(LowIncome*s_pred*PERWT/n_draws)/sum(s_pred*PERWT/n_draws),
#                             nu_h_avg = sum(nu_h*s_pred*PERWT/n_draws)/sum(s_pred*PERWT/n_draws),
#                             nu_i_avg = sum(nu_i*s_pred*PERWT/n_draws)/sum(s_pred*PERWT/n_draws)),
#                       by=c("Product")]
# 
# 
# R_temp[,R_j:=R_wgt/enroll]
# prodData[,R_j:=NULL]
# prodData[,R_wgt:=NULL]
# R_temp[,enroll:=NULL]
# with(R_temp,plot(nu_h_avg,R_j))
# prodData = merge(prodData,R_temp,by="Product")
# 
# 
# 
# 
# 
# firmData = prodData[,list(R_f_pred=sum(enroll*R_j*Gamma_j)/sum(enroll),
#                           enroll_Fam=sum(enroll_Fam*enroll)/sum(enroll),
#                           enroll_Age=sum(enroll_Age*enroll)/sum(enroll),
#                           enroll_Inc=sum(enroll_Inc*enroll)/sum(enroll),
#                           nu_h_avg=sum(nu_h_avg*enroll)/sum(enroll),
#                           nu_i_avg=sum(nu_i_avg*enroll)/sum(enroll)),
#                     by=c("Firm","ST","RA_share","R_f")]
# firmData[R_f==1,R_bench:=R_f_pred]
# firmData[,R_bench:=max(R_bench,na.rm=TRUE),by="ST"]
# firmData[,R_f_pred:=R_f_pred/R_bench]
# setkey(firmData,ST,Firm)
# 
# firm_RA = merge(firm_RA,firmData[,c("ST","Firm","R_f_pred")],by=c("ST","Firm"),all.x=TRUE)
# setkey(firm_RA,ST,Firm)
# firm_RA[Firm=="OTHER",R_f_pred:=R_f]
# 
# firm_RA[,A_sum:=sum(A_wtd),by="ST"]
# firm_RA[,R_sum:=sum(R_f_pred*RA_share),by="ST"]
# firm_RA[,transfer_pred:=-ST_MLR_lives*avg_prem*(R_f_pred*RA_share/R_sum - A_wtd/A_sum)]
# firm_RA[,transfer_pp:=-avg_prem*(R_f_pred*RA_share/R_sum - A_wtd/A_sum)]
# 
# firm_RA[R_f>0,plot(R_f,R_f_pred)]
# firm_RA[R_f>0,plot(transfer_pp/avg_prem,pp_payments_adj/avg_prem)]
# firm_RA[R_f>0,plot(transfer_pred,payments_adj)]
# 
