rm(list = ls())
library(data.table)
library(nleqslv)
library(Matrix)
library(doBy)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

## Run
run = "2018-08-25"
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
firmClaims[,logAvgCost:=log(AvgCost)]
setkey(firmClaims,ST,Firm)

#### Filings Claims Data ####
metalClaims = as.data.table(read.csv("Intermediate_Output/Average_Claims/firmClaims.csv"))
metalData = unique(full_predict[,c("ST","Firm","Metal_std")])
metalClaims = merge(metalData,metalClaims[,c("STATE","Firm","METAL","EXP_INC_CLM_PMPM")],by.x=c("ST","Firm","Metal_std"),
                    by.y=c("STATE","Firm","METAL"),all.x=TRUE)

metalClaims[EXP_INC_CLM_PMPM==0,EXP_INC_CLM_PMPM:=NA]
metalClaims[,logAvgCost:=log(EXP_INC_CLM_PMPM)]
setkey(metalClaims,ST,Firm,Metal_std)



#### MEPS Age Moments ####
ageMoments = as.data.table(read.csv("Intermediate_Output/MEPS_Moments/ageMoments.csv"))
setkey(ageMoments,Age_Bin)

full_predict[,Age_Bin:=floor(AGE/5)*5]
full_predict[AGE>=18 & Age_Bin==15,Age_Bin:=20]
full_predict[,Age_1:=AGE/10]
full_predict[,Age_2:=Age_1^2]

for (a in unique(full_predict$Age_Bin)){
  var = paste("Bin",a,sep="_")
  full_predict[,c(var):=0]
  full_predict[Age_Bin==a,c(var):=1]
}

#### MEPS Risk Score Moments ####
riskMoments = as.data.table(read.csv("Intermediate_Output/MEPS_Moments/riskMoments.csv"))
setkey(riskMoments,HCC_positive)

full_predict[,HCC_positive:=0]
full_predict[HCC_Silver>0,HCC_positive:=1]



#### All Moments ####
firmClaims$M_num = 1:nrow(firmClaims)
metalClaims$M_num[!is.na(metalClaims$logAvgCost)] = max(firmClaims$M_num) + 1:sum(!is.na(metalClaims$logAvgCost))
ageMoments$M_num = max(metalClaims$M_num,na.rm=TRUE) + 1:nrow(ageMoments)
riskMoments$M_num = max(ageMoments$M_num) + 1:nrow(riskMoments)

mom_Num = nrow(firmClaims) + sum(!is.na(metalClaims$logAvgCost)) + nrow(ageMoments) + nrow(riskMoments)

mkt_data = read.csv("Intermediate_Output/Estimation_Data/marketDataMap_discrete.csv")
mkt_data$METAL = gsub(" [0-9]+","",mkt_data$METAL)

## State Shares
population = unique(acs[,c("Person","ST","Market","PERWT")])
population = population[,list(pop = sum(PERWT)),by=c("ST","Market")]
population[,st_share:=pop/sum(pop),by="ST"]

mkt_data = merge(mkt_data,population[,c("ST","Market","st_share")],
                 by.x=c("STATE","Market"),by.y=c("ST","Market"))



metal_moments = merge(mkt_data[,c("STATE","Firm","METAL","Product","st_share")],metalClaims,
                      by.x=c("STATE","Firm","METAL"),by.y=c("ST","Firm","Metal_std"))
firm_moments = merge(mkt_data[,c("STATE","Firm","Product","st_share")],firmClaims,
                     by.x=c("STATE","Firm"),by.y=c("ST","Firm"))
prod_moments = rbind(metal_moments[,c("STATE","Firm","Product","logAvgCost","M_num")],
                     firm_moments[,c("STATE","Firm","Product","logAvgCost","M_num")])
prod_moments = prod_moments[!is.na(prod_moments$logAvgCost),]


rm(acs,MLR_Data)
#### Clear Memory ####
remove_Vars = ls()[!grepl("(full_predict|full_predict|Moments|Claims|est|moments|phi_start|aVar|cost_function|dAvar|run|n_draws|mom_Num)",ls())]

full_predict = full_predict[,c("ST","Firm","Person","Product","Metal_std",
                                                             "s_pred","draw_wgt",
                                                             "Age_Bin","HCC_positive","HCC_Silver",
                                                              "Age_1","Age_2","WTP","AV_std","Any_HCC","PERWT")]


rm(a,claims,crosswalk,draws,enroll,fe,firm_moments,firmData,metal_moments,metalData,mkt_data,population,var)
gc()

#### Define Fixed Effects and Starting Parameters ####
ST_list = sort(unique(full_predict$ST))
for (fe in ST_list){
  var = paste("FE",fe,sep="_")
  full_predict[,c(var):=0]
  full_predict[ST==fe,c(var):=1]
}


phi_age = .01
phi_HCC = .7
phi_AV = 4
phi_ST = rep(0,length(ST_list))
phi_start = c(phi_age,phi_HCC,phi_AV,phi_ST)


#### Assymptotic Variance GMM ####
aVar <- function(phi){
  #estMat_full = as.matrix(full_predict[,.SD,.SDcols=c("Age_1","HCC_Silver","AV_std",names(full_predict)[grep("^FE_",names(full_predict))])])
  
  
  full_predict[,C:=exp(estMat%*%phi)]
    # full_predict[,C:=exp(phi[1]*Age_1 + 
    #                      phi[2]*HCC_Silver + 
    #                      phi[3]*AV_std + 
    #                      phi[4]*FE_AK + 
    #                      phi[5]*FE_GA + 
    #                      phi[6]*FE_IA + 
    #                      phi[7]*FE_IL + 
    #                      phi[8]*FE_MD + 
    #                      phi[9]*FE_MI + 
    #                      phi[10]*FE_MO + 
    #                      phi[11]*FE_ND + 
    #                      phi[12]*FE_NE + 
    #                      phi[13]*FE_NM + 
    #                      phi[14]*FE_OK + 
    #                      phi[15]*FE_OR + 
    #                      phi[16]*FE_TX + 
    #                      phi[17]*FE_UT)]

  var_list = c("s_pred_mean","C_avg",
               "C_avg_HCC","C_avg_nonHCC",
               "S_HCC","S_nonHCC",
               "C_avg_20","C_avg_25","C_avg_30","C_avg_35","C_avg_40",
               "C_avg_45","C_avg_50","C_avg_55","C_avg_60",
               "S_mean_20","S_mean_25","S_mean_30","S_mean_35","S_mean_40",
               "S_mean_45","S_mean_50","S_mean_55","S_mean_60")
  M = length(var_list)
  J = length(unique(full_predict$Product))
  var = Matrix(0,nrow=J*M,ncol=J*M,sparse=TRUE)
  mean_moments = Matrix(0,nrow=J*M,ncol=1,sparse=TRUE)
  Pop = full_predict[,sum(PERWT)]

  ageBins = unique(full_predict$Age_Bin)
  cnt = 0 
  
  per_prod_Data = full_predict[,list(C_avg = sum(C*s_pred*draw_wgt)/n_draws,
                                     s_pred_mean = sum(s_pred*draw_wgt)/n_draws,
                                     PERWT = sum(PERWT),
                                C_avg_HCC = sum(C*HCC_positive*s_pred*draw_wgt/Any_HCC)/(n_draws),
                                C_avg_nonHCC = sum(C*(1-HCC_positive)*s_pred*draw_wgt/(1-Any_HCC))/(n_draws),
                               S_HCC = sum(HCC_positive*s_pred*draw_wgt/Any_HCC)/(n_draws),
                               S_nonHCC = sum((1-HCC_positive)*s_pred*draw_wgt/(1-Any_HCC))/(n_draws)),
                          by=c("Person","Product","Age_Bin","ST","Firm")]
  

  for (a in ageBins){
    C_var = paste("C_avg",a,sep="_")
    S_var = paste("S_mean",a,sep="_")
    per_prod_Data[,c(C_var):=0]
    per_prod_Data[,c(S_var):=0]
    per_prod_Data[Age_Bin==a,c(C_var):=C_avg]
    per_prod_Data[Age_Bin==a,c(S_var):=s_pred_mean]
  }
  
  
  
  setkey(per_prod_Data,Person)
  people = unique(per_prod_Data$Person)
  
  for (p in people){
    cnt = cnt+1
    if (cnt%%500==0){
      print(cnt)
    }
    C_mom = Matrix(0,nrow=J*M,ncol=1,sparse=TRUE)
    
    perData = per_prod_Data[.(p),]
    
    idx = perData$Product
    idx_long = c()
    
    prwgt = perData$PERWT
    for (m in 0:(M-1)){
      idx_long = c(idx_long,J*m+idx)
      C_mom[m*J+idx] = prwgt*perData[[var_list[m+1]]]
    }

    V_temp = C_mom%*%t(C_mom)
    var = var + V_temp
    mean_moments = mean_moments+C_mom
  }
  var= var/Pop
  mean_moments = mean_moments/Pop 
  
  return(list(moments=mean_moments,variance =var))
}

#### Derivative of Moments ####

dAvar <-function(moments){
  der = Matrix(0,ncol=length(moments),nrow=mom_Num,sparse=TRUE)
  J = length(unique(full_predict$Product))
  mom_est = vector(mode="numeric",length=mom_Num)
  for (m in 1:max(prod_moments$M_num,na.rm=TRUE)){
    idx = prod_moments$Product[prod_moments$M_num==m]
    sum_C = sum(moments[J+idx])
    sum_S = sum(moments[idx])
    der[m,idx] = log(sum_C) - 1/sum_S
    der[m,J+idx] = 1/sum_C - log(sum_S)
    mom_est[m] = log(sum_C/sum_S)
  }
  
  ### Age Moments
  ageM_idx = unique(ageMoments$M_num)
  sum_C_20 = sum(moments[J*6 + 1:J])
  sum_S_20 = sum(moments[J*15 + 1:J])
  mom_est[ageM_idx[1]] = 1
  
  for (m in 1:8){
    sum_C = sum(moments[J*(m+6) + 1:J])
    sum_S = sum(moments[J*(m+15) + 1:J])

    der[ageM_idx[1+m],J*6+ + 1:J] =  - (sum_C/sum_C_20) * (sum_S_20/sum_S) * (1/sum_C_20)
    der[ageM_idx[1+m],J*15 + 1:J] =  (sum_C/sum_C_20) * (1/sum_S) 
    
    der[ageM_idx[1+m],J*(m+6)+ + 1:J] =  (1/sum_C_20) * (sum_S_20/sum_S) 
    der[ageM_idx[1+m],J*(m+15) + 1:J] =  - (sum_C/sum_C_20) * (sum_S_20/sum_S) * (1/sum_S)
    mom_est[ageM_idx[1+m]] = (sum_C/sum_C_20) * (sum_S_20/sum_S)
  }
  
  ### Risk Moments
  riskM_idx = unique(riskMoments$M_num)
  sum_C_HCC = sum(moments[J*2 + 1:J])
  sum_S_HCC = sum(moments[J*4 + 1:J])
  
  sum_C_non = sum(moments[J*3 + 1:J])
  sum_S_non = sum(moments[J*5 + 1:J])
  
  mom_est[riskM_idx[1]] = 1
  mom_est[riskM_idx[2]] = (sum_C_HCC/sum_C_non) * (sum_S_non/sum_S_HCC)
  
  der[riskM_idx[2],J*2+ + 1:J] =  - (sum_C_HCC/sum_C_non) * (sum_S_non/sum_S_HCC) * (1/sum_C_HCC)
  der[riskM_idx[2],J*4 + 1:J] =  (sum_C_HCC/sum_C_non) * (1/sum_S_HCC) 
  
  der[riskM_idx[2],J*3+ + 1:J] =  (1/sum_C_non) * (sum_S_non/sum_S_HCC) 
  der[riskM_idx[2],J*5 + 1:J] =  - (sum_C_HCC/sum_C_non) * (sum_S_non/sum_S_HCC) * (1/sum_S_HCC)
  
  return(list(derivative=der,moments=mom_est))
}


# st = Sys.time()
# V = aVar(phi_start)
# Sys.time()-st
# 
# res = dAvar(V$moments)




#### GMM Function ####


full_predict[,C:=vector(mode="numeric",length=nrow(full_predict))]

# compact_predict = full_predict[,list(PERWT= sum(PERWT)),by=c("ST","Firm","Person","Product","Metal_std",
#                                                              "s_pred",
#                                                              "Age_Bin","HCC_positive","HCC_Silver",
#                                                               "Age_1","Age_2","WTP","AV_std","Any_HCC",
#                                                              names(full_predict)[grep("^FE_",names(full_predict))])]

estMat = as.matrix(full_predict[,.SD,.SDcols=c("Age_1","HCC_Silver","AV_std",names(full_predict)[grep("^FE_",names(full_predict))])])
#estMat = as.matrix(full_predict[,.SD,.SDcols=c("Age_1","Age_2","WTP","AV_std",names(full_predict)[grep("^FE_",names(full_predict))])])

# full_predict = full_predict[,c("ST","Firm","Person","Product","Metal_std",
#                                 "s_pred","C",
#                                 "Age_Bin","HCC_positive","HCC_Silver",
#                                  "Age_1","Age_2","WTP","AV_std","Any_HCC","PERWT")]
gc()

cost_function_est<- function(phi,W){
  ## Demographic Cost
  full_predict[,C:=exp(estMat%*%phi)]

  
  # ## Firm Level Risk
  Cost_prod = full_predict[,list(C_avg=sum(C*s_pred*PERWT)/sum(s_pred*PERWT)),
                              by=c("ST","Firm","Metal_std")]
  setkey(Cost_prod,ST,Firm,Metal_std)

  Moments_Prod = log(Cost_prod$C_avg) - metalClaims$logAvgCost
  Moments_Prod = Moments_Prod[!is.na(metalClaims$logAvgCost)]

  Cost_firm = full_predict[,list(C_avg=sum(C*s_pred*PERWT)/sum(s_pred*PERWT)),by=c("ST","Firm")]
  setkey(Cost_firm,ST,Firm)

  Moments_Firm = log(Cost_firm$C_avg) - firmClaims$logAvgCost

  Cost_age = full_predict[,list(C_avg=sum(C*s_pred*PERWT)/sum(s_pred*PERWT)),
                             by=c("Age_Bin")]
  setkey(Cost_age,Age_Bin)
  low_Cost = Cost_age$C_avg[Cost_age$Age_Bin==20]
  Cost_age[,C_idx:=C_avg/low_Cost]

  Moments_Age = Cost_age$C_idx- ageMoments$costIndex
  
  Cost_risk = full_predict[,sum(C*s_pred*HCC_positive*PERWT/(n_draws*Any_HCC))/sum(s_pred*HCC_positive*PERWT/(n_draws*Any_HCC))]
  Cost_non_risk = full_predict[,sum(C*s_pred*(1-HCC_positive)*PERWT/(n_draws*(1-Any_HCC)))/sum(s_pred*(1-HCC_positive)*PERWT/(n_draws*(1-Any_HCC)))]

  
  Moments_Risk = c(1,Cost_risk/Cost_non_risk)
  Moments_Risk = Moments_Risk - riskMoments$costIndex
  
  moments = c(Moments_Firm,Moments_Prod,Moments_Age,Moments_Risk)

  
  error = as.numeric(moments%*%W%*%moments)
  rm(Cost_prod,Cost_firm,Cost_age,Cost_risk,Cost_non_risk)
  
  
  print(error)
  print(phi)
  full_predict[,C:=0]
  #prodData[,c("logC"):=NULL]
  return(error)
}




#Wgt = diag(mom_Num)
# st = Sys.time()
# cost_function_est(phi_start,Wgt)
# Sys.time()-st

#### Regression Test ####
prod_Avgs = merge(full_predict[,c("ST","Firm","Product","Age_1",
                                  "s_pred","PERWT","HCC_Silver","AV_std")],prod_moments[,c("Product","logAvgCost","M_num")],by="Product",allow.cartesian = TRUE)

prod_Avgs = prod_Avgs[,list(Age = sum(10*Age_1*s_pred*PERWT)/sum(s_pred*PERWT),
                               HCC = sum(HCC_Silver*s_pred*PERWT)/sum(s_pred*PERWT),
                            AV = sum(AV_std*s_pred*PERWT)/sum(s_pred*PERWT)),
                         by=c("ST","Firm","M_num","logAvgCost")]

res = lm(logAvgCost~-1+Age+HCC+AV + ST ,data=prod_Avgs)
phi_start = res$coefficients
rm(prod_Avgs,res)
gc()


#### Estimate GMM ####
# Stage 1 
print("GMM Stage 1")

Wgt = diag(mom_Num)
est_res_1 = nlm(f=cost_function_est,p=phi_start,iterlim=2000,steptol=1e-10,W=Wgt)
costFile = paste("Simulation_Risk_Output/costParameters_MSM_Stage1_",run,".rData",sep="")
save(est_res_1,file=costFile)

# Optimal Weighting Matrix
print("Calculate Optimal Weighting Matrix")
res1 = aVar(est_res_1$estimate)
res2 = dAvar(res1$moments)

Sigma = res1$variance
H = res2$derivative

Wgt = H%*%Sigma%*%t(H)
costFile = paste("Simulation_Risk_Output/costParameters_Var",run,".rData",sep="")
save(res1,res2,file=costFile)

# Stage 2
print("GMM Stage 2")
est_res = nlm(f=cost_function_est,p=phi_start,iterlim=2000,steptol=1e-10,W=Wgt,stepmax=10)
costFile = paste("Simulation_Risk_Output/costParameters_MSM_Stage2_",run,".rData",sep="")
save(est_res,file=costFile)



