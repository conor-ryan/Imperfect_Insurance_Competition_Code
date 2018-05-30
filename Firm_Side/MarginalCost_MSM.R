rm(list = ls())
library(data.table)
library(nleqslv)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

## Run
run = "2018-05-12"

#### Load Simulation Data and Merge in GCF/AV data ####
simFile = paste("Simulation_Risk_Output/simData_",run,".rData",sep="")
load(simFile)
n_draws = nrow(draws)
#### Read in Total Claims Data ####
MLR_Data = read.csv("Data/2015_MLR/Part1_2_Summary_Data_Premium_Claims.csv")


claims = MLR_Data[MLR_Data$ROW_LOOKUP_CODE=="TOTAL_INCURRED_CLAIMS_PT2",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_Q1")]
names(claims) = c("ï..MR_SUBMISSION_TEMPLATE_ID","Claims")

enroll =MLR_Data[MLR_Data$ROW_LOOKUP_CODE=="MEMBER_MONTHS",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_Q1")]
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

firmData = unique(full_predict[,c("ST","Firm")])

firmClaims = merge(firmData,claims,by.x=c("ST","Firm"),by.y=c("STATE","Firm"),all.x=TRUE)
setkey(firmClaims,ST,Firm)

#### Filings Claims Data ####
metalClaims = as.data.table(read.csv("Intermediate_Output/Average_Claims/firmClaims.csv"))
metalData = unique(full_predict[,c("ST","Firm","Metal_std","AV_std")])
metalClaims = merge(metalData,metalClaims[,c("STATE","Firm","METAL","EXP_INC_CLM_PMPM")],by.x=c("ST","Firm","Metal_std"),
                    by.y=c("STATE","Firm","METAL"),all.x=TRUE)
setkey(metalClaims,ST,Firm,Metal_std)

#### MEPS Moments ####
ageMoments = as.data.table(read.csv("Data/2015_MEPS/ageMoments.csv"))
setkey(ageMoments,Age_Bin)

full_predict[,Age_Bin:=floor(AGE/5)*5]
full_predict[AGE>=18 & Age_Bin==15,Age_Bin:=20]
full_predict[,Age_1:=AGE/10]
full_predict[,Age_2:=Age_1^2]

#### Define Cost Function ####
ST_list = sort(unique(full_predict$ST))
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
phi = c(phi_age,phi_age2,phi_WTP,phi_AV,phi_ST)


#### Risk Function 
estMat = as.matrix(full_predict[,.SD,.SDcols=c("Age_1","Age_2","WTP","AV_std",names(full_predict)[grep("^FE_",names(full_predict))])])

cost_function_est<- function(phi){
  ## Demographic Cost
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
  Cost_prod = full_predict[,list(C_avg=sum(C*s_pred*PERWT/n_draws)/sum(s_pred*PERWT/n_draws)),by=c("ST","Firm","Metal_std")]
  setkey(Cost_prod,ST,Firm,Metal_std)
  

  Moments_Prod = Cost_prod$C_avg - metalClaims$EXP_INC_CLM_PMPM
  Moments_Prod = Moments_Prod[!is.na(Moments_Prod)]
  
  Cost_firm = full_predict[,list(C_avg=sum(C*s_pred*PERWT/n_draws)/sum(s_pred*PERWT/n_draws)),by=c("ST","Firm")]
  setkey(Cost_firm,ST,Firm)

  Moments_Firm = Cost_firm$C_avg - firmClaims$AvgCost

  Cost_age = full_predict[,list(C_avg=sum(C*s_pred*PERWT/n_draws)/sum(s_pred*PERWT/n_draws)),by=c("Age_Bin")]
  setkey(Cost_age,Age_Bin)
  Cost_age[,C_idx:=C_avg/min(C_avg)]

  Moments_Age = Cost_age$C_idx - ageMoments$costIndex

  moments = c(Moments_Prod/100,Moments_Firm/100,Moments_Age)
  #moments = c(Moments_Prod/100)
    
  error = sum((moments)^2)
  rm(Cost_prod,Cost_firm)
  #rm(Cost_prod)
  print(error)
  print(phi)
  full_predict[,c("C"):=NULL]
  return(error)
}



st = Sys.time()
cost_function_est(phi)
Sys.time()-st




#res = optim(par=phi,fn=cost_function_est,control=list(maxit=2000))
res = nlm(f=cost_function_est,p=phi,iterlim=2000,steptol=1e-10)



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



costFile = paste("Simulation_Risk_Output/costParameters_MSM_",run,".rData",sep="")
save(res_list,file=costFile)

