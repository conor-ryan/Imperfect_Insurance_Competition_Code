rm(list = ls())
library(data.table)
library(nleqslv)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

## Run
run = "2018-05-12"

#### Load Simulation Data and Merge in GCF/AV data ####
simFile = paste("Simulation_Risk_Output/simData_",run,".rData",sep="")
load(simFile)

#### Read in Total Claims Data ####
MLR_Data = read.csv("Data/2015_MLR/Part1_2_Summary_Data_Premium_Claims.csv")


claims = MLR_Data[MLR_Data$ROW_LOOKUP_CODE=="TOTAL_INCURRED_CLAIMS_PT2",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_Q1")]
names(claims) = c("ï..MR_SUBMISSION_TEMPLATE_ID","Claims")

enroll =MLR_Data[MLR_Data$ROW_LOOKUP_CODE=="NUMBER_OF_LIFE_YEARS",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_Q1")]
names(enroll) = c("ï..MR_SUBMISSION_TEMPLATE_ID","MLR_lives")

claims = merge(claims,enroll,by="ï..MR_SUBMISSION_TEMPLATE_ID")

# Remove non-Individual Market Insurers
claims$absent1 = is.na(claims$MLR_lives) | claims$MLR_lives==0
claims = claims[!claims$absent1,c("ï..MR_SUBMISSION_TEMPLATE_ID","Claims","MLR_lives")]

# Merge in and summarize by Firm Name
crosswalk = read.csv("Intermediate_Output/FirmCrosswalk.csv")
crosswalk = unique(crosswalk[,c("ï..MR_SUBMISSION_TEMPLATE_ID","Firm","STATE")])

claims = merge(claims,crosswalk[,c("ï..MR_SUBMISSION_TEMPLATE_ID","Firm","STATE")],by="ï..MR_SUBMISSION_TEMPLATE_ID")
claims = summaryBy(MLR_lives+Claims~Firm+STATE,data=claims,FUN=sum,na.rm=TRUE,keep.names=TRUE)

claims$AvgCost = with(claims,Claims/MLR_lives)

#### Filings Claims Data ####
metalClaims = read.csv("Intermediate_Output/Average_Claims/firmClaims.csv")

#### Define Cost Function ####
ST_list = sort(unique(full_predict$ST))[-1]
for (fe in ST_list){
  var = paste("FE",fe,sep="_")
  full_predict[,c(var):=0]
  full_predict[ST==fe,c(var):=1]
}


phi_age = 0
phi_age2 = 0
phi_WTP = 0
phi_AV = 0 
phi_ST = rep(0,length(ST_list))
phi = c(phi_age,phi_WTP,phi_AV,phi_ST)


#### Risk Function 
cost_function_est<- function(phi){
  ## Demographic Cost
  full_predict[,C:= exp()]
  
  estMat = as.matrix(full_predict[,.SD,.SDcols=c("AGE","WTP","AV_std",names(full_predict)[grep("^FE_",names(full_predict))])])
  full_predict[,C:=exp(estMat%*%phi)]

  # full_predict[,C:=exp(phi[1]*AGE + 
  #                        phi[2]*WTP + 
  #                        phi[3]*AV_std + 
  #                        phi[4]*FE_GA + 
  #                        phi[5]*FE_IA + 
  #                        phi[6]*FE_IL + 
  #                        phi[7]*FE_MD + 
  #                        phi[8]*FE_MI + 
  #                        phi[9]*FE_MO + 
  #                        phi[10]*FE_ND + 
  #                        phi[11]*FE_NE + 
  #                        phi[12]*FE_NM + 
  #                        phi[13]*FE_OK + 
  #                        phi[14]*FE_OR + 
  #                        phi[15]*FE_TX + 
  #                        phi[16]*FE_UT)]
  
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
