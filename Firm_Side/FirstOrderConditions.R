rm(list = ls())
library(data.table)
library(nleqslv)
library(ggplot2)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

## Run
run = "2018-05-12"
#1811770644

#### Read in Data ####
## Geographic Rating Factors
gcf = read.csv("Data/2015_MLR/2015_GCF.csv")
gcf$Market = with(gcf,paste(State,Rating.Area,sep="_"))
gcf=as.data.table(gcf)
setkey(gcf,Market) 

## Simulation Data
simFile = paste("Simulation_Risk_Output/simData_",run,".rData",sep="")
load(simFile)

# Drop FE
fe_names = names(acs)[grep("^FE_",names(acs))]
acs[,c(fe_names):=NULL]

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

## Cost Function 
costFile = paste("Simulation_Risk_Output/costParameters_",run,".rData",sep="")
load(costFile)

cost_par = CostRes$coefficients[grep("(Age|WTP)",names(CostRes$coefficients))]

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


#### Full Data Set ####
## Demographic Density 
acs[METAL!="CATASTROPHIC",mkt_insured:=sum(PERWT*s_pred_mean),by=c("Market")]
acs[METAL!="CATASTROPHIC",st_insured:=sum(PERWT*s_pred_mean),by=c("ST")]
acs[METAL!="CATASTROPHIC",S_m:=mkt_insured/st_insured,by=c("ST")]

density = unique(acs[,c("Market","Person","PERWT")])
density[,mkt_size:=sum(PERWT),by="Market"]

density[,mkt_density:=PERWT/mkt_size]
density[,PERWT:=NULL]

acs = merge(acs,density,by=c("Market","Person"))

## Merge Random Coefficients
full_predict = merge(acs,predict_data,by=c("Product","Person"))

n_draws = nrow(draws)
full_predict[,mkt_density:=mkt_density/n_draws]

rm(predict_data)

#### Consolidate Silver Plans ####
full_predict[,METAL:=gsub(" .*","",METAL)]
full_predict[,Product:=min(Product),by=c("Firm","METAL","Market")]
full_predict[,premBase:=median(premBase),by=c("Firm","METAL","Market")]

acs[,METAL:=gsub(" .*","",METAL)]
acs[,Product:=min(Product),by=c("Firm","METAL","Market")]
acs[,premBase:=median(premBase),by=c("Firm","METAL","Market")]

#### Incorporate Parameters ####
gamma0 = beta_vec[1]
gamma = beta_vec[2:6]
beta0 = beta_vec[7:8]
beta = matrix(0,nrow=2,ncol=5)
beta[1,1:ncol(beta)] = beta_vec[9:13]
sigma = beta_vec[14:15]
FE_pars = beta_vec[16:length(beta_vec)]


## Integrate Draws and Prediction Data
draws = as.data.table(draws)
n_draws = nrow(draws)
draws[,d_ind:=as.integer(1:n_draws)]
setkey(draws,d_ind)
setkey(full_predict,d_ind,Person)
nu_h_large = draws[full_predict$d_ind,2]*sign(sigma[2])
nu_i_large = draws[full_predict$d_ind,1]*sign(sigma[1])

full_predict[,alpha_draw:=0]
full_predict[,nu_h:=nu_h_large]
full_predict[,nu_i:=nu_i_large]
rm(nu_h_large,nu_i_large)

## indication
# full_predict[,nu_h_ind:=as.numeric(nu_h>.6)]
# full_predict[,nu_i_ind:=as.numeric(nu_i>.6)]

full_predict[,alpha:=(beta0[1]+
                        beta[1,1]*AgeFE_31_40+
                        beta[1,2]*AgeFE_41_50+
                        beta[1,3]*AgeFE_51_64+
                        beta[1,4]*Family+
                        beta[1,5]*LowIncome)*12/1000]

full_predict[,WTP:=-0.10*(beta0[2]+sigma[2]*nu_h)/(alpha*1000/12)]
# Test Larger Elasticty
#full_predict[,alpha:=(-.5)*12/1000]
# full_predict[,pref_h:=nu_h/alpha]
# full_predict[,elas:=alpha*(1-s_pred)*Price*1000/12*(ageRate_avg)]
# full_predict[,semi_elas:=alpha*(1-s_pred)*100*100/12]
# 




## Predict Risk Scores
#### Define HCC Age ####
full_predict[METAL=="PLATINUM",HCC_age:=PlatHCC_Age]
full_predict[METAL=="GOLD",HCC_age:=GoldHCC_Age]
full_predict[METAL=="SILVER",HCC_age:=SilvHCC_Age]
full_predict[METAL=="BRONZE",HCC_age:=BronHCC_Age]
full_predict[METAL=="CATASTROPHIC",HCC_age:=CataHCC_Age]


full_predict[,R:= HCC_age + AV*(psi_final[1]*nu_h +
                                  psi_final[2]*nu_i + 
                                  psi_final[3]*nu_i*nu_h+
                                  psi_final[4]*Age+
                                  psi_final[5]*Age*nu_h)]

## Standard Cost-Sharing Levels
# full_predict[,MedDeduct_std:= MedDeduct - MedDeductDiff]
# full_predict[,MedOOP_std:= MedOOP - MedOOPDiff]
# full_predict[,High_std:= High - HighDiff]


## Market Shares
full_predict[METAL!="CATASTROPHIC",s_pred_inside:=s_pred/sum(s_pred),by=c("Person","d_ind")]
full_predict[METAL!="CATASTROPHIC",s_pred_outside:=1-sum(s_pred),by=c("Person","d_ind")]


catas_prods = unique(acs$Product[acs$METAL=="CATASTROPHIC"])

# test = full_predict[METAL!="CATASTROPHIC",list(l1 = sum(s_pred*PERWT/n_draws),
#                           s_i = sum(s_pred_inside*mkt_density*(1-s_pred_outside))/sum(mkt_density*(1-s_pred_outside)),
#                           s = sum(s_pred*mkt_density),
#                           l4 = sum(s_pred*PERWT/n_draws),
#                           d=sum(mkt_density)),
#                     by=c("Product","S_m","st_insured","ST","mkt_size")]
# test[,l2:=s_i*S_m*st_insured]
# test[,l3:=s*mkt_size]
# test[,st_2:=sum(l1),by="ST"]

#### Calibrate Outside Risk Transfer ####
otherRiskFile = paste("Simulation_Risk_Output/otherRiskScores_",run,".rData",sep="")
load(otherRiskFile)

## Get Predicted Market Level Risk
inside_RA_pred = full_predict[METAL!="CATASTROPHIC",
                              list(R_tot_pred = sum(s_pred*Gamma_j*R*PERWT/n_draws)/
                                     sum(s_pred*PERWT/n_draws)),by="ST"]
## Compare to Predicted Outside Firm Risk
inside_Est = other_RA[Firm_Ag=="Inside",c("ST","avg_prem","ST_MLR_lives","payments_adj","RA_share","R_bench","R_tot")]
outside_Est = other_RA[Firm_Ag=="Other",c("ST","R_tot")]
names(outside_Est) = c("ST","R_tot_other")
## Merge firm-level estimates
inside_RA_pred = merge(inside_RA_pred,inside_Est,by="ST",all.x=TRUE)
inside_RA_pred = merge(inside_RA_pred,outside_Est,by="ST",all.x=TRUE)
# ## Calculate state-level risk, taking outside firm into account
# inside_RA_pred[,R_rel:= R_tot_other/R_tot]
# inside_RA_pred[,ST_R:=RA_share*R_tot_pred + (1-RA_share)*R_rel*R_tot_pred]
# inside_RA_pred[,t_test:=ST_MLR_lives*avg_prem*RA_share*((R_tot_pred/ST_R) - 1)]

inside_RA_pred = inside_RA_pred[,c("ST","RA_share","avg_prem","ST_MLR_lives","payments_adj")]

## Adjust RA Market Shares 
full_predict = merge(full_predict,inside_RA_pred[,c("ST","RA_share")],by="ST",all.x=TRUE)
full_predict[,S_m:=RA_share*S_m]

#### Risk Adjustment Transfers ####
RA_transfers = full_predict[,list(lives = sum(s_pred*PERWT/n_draws),
                                  S_j = sum(s_pred*mkt_density),
                                  S_0 = sum(mkt_density*(1-s_pred_outside)),
                                  R_j = sum(s_pred*R*mkt_density)/sum(s_pred*mkt_density),
                                  AV = sum(s_pred*AV*mkt_density)/sum(s_pred*mkt_density),
                                  A_j = sum(s_pred*ageRate_avg*mkt_density)/sum(s_pred*mkt_density),
                                  Age_j = sum(s_pred*AGE*mkt_density)/sum(s_pred*mkt_density),
                                  WTP_j = sum(s_pred*WTP*mkt_density)/sum(s_pred*mkt_density)),
                            by=c("Product","METAL","Market","ST","Firm","premBase","Gamma_j",
                                 "GCF","S_m","mkt_size","st_insured","RA_share")]
RA_transfers[METAL!="CATASTROPHIC",RA_lives:=sum(lives),by="ST"]

RA_transfers[,share_tilde:=RA_share*lives/RA_lives]
#RA_transfers = merge(RA_transfers,inside_RA_pred,by="ST",all.x=TRUE)

# What is wrong with MD, sum(share_tilde).... ?
#RA_transfers[,sum(share_tilde),by="ST"]


## Normalize at the Firm Level
firmRiskFile = paste("Simulation_Risk_Output/firmRiskScores_Full_",run,".rData",sep="")
load(firmRiskFile)

firmData = RA_transfers[METAL!="CATASTROPHIC",list(S_f=sum(share_tilde),
                                                   R_f_pred=sum(share_tilde*R_j*Gamma_j)/sum(share_tilde)),by=c("ST","Firm")]
firmData = merge(firmData,firm_RA[,c("ST","Firm","R_f","ST_R","RA_share")],by=c("ST","Firm"),all.y=TRUE)
firmData[R_f==1,R_bench:=R_f_pred]
firmData[,R_bench:=max(R_bench,na.rm=TRUE),by="ST"]
firmData[,R_f_pred:=R_f_pred/R_bench]
firmData[Firm=="OTHER",R_f_pred:=R_f]
firmData[,ST_R:=sum(RA_share*R_f_pred),by="ST"]


stData = unique(firmData[,c("ST","R_bench","ST_R")])

## Calculate Transfer Stats
RA_transfers = merge(RA_transfers,stData,by="ST")
RA_transfers[,R_j:=R_j/R_bench]

## Average State Premium
RA_transfers[,avg_prem:=sum(share_tilde*premBase*A_j,na.rm=TRUE)/sum(share_tilde,na.rm=TRUE),by="ST"]
#RA_transfers[,avg_prem:=avg_prem/12]
#RA_transfers[,ST_R:=sum(share_tilde*R_j*AV*Gamma_j,na.rm=TRUE)/sum(share_tilde),by="ST"]
RA_transfers[,ST_A:=sum(share_tilde*AV*A_j*Gamma_j,na.rm=TRUE)/sum(share_tilde,na.rm=TRUE),by="ST"]
RA_transfers[,T_norm_j:=(R_j*Gamma_j/ST_R - AV*A_j*Gamma_j/ST_A)]
RA_transfers[,R_norm_j:=(R_j*Gamma_j/ST_R)]
RA_transfers[,A_norm_j:=(A_j*AV*Gamma_j/ST_A)]
RA_transfers[,T_j:=T_norm_j*avg_prem]


RA_transfers[,C_j:=exp(predict(CostRes,newdata=RA_transfers))]

RA_transfers[METAL=="CATASTROPHIC",T_j:=0]
RA_transfers[,Profit:=A_j*premBase+T_j-C_j]


RA_transfers = RA_transfers[,c("ST","Product","METAL","Market","Firm","premBase",
                               "mkt_size","st_insured","AV","S_m","share_tilde","S_j","S_0",
                               "R_j","A_j","R_bench","ST_R","avg_prem","ST_A","T_norm_j",
                               "R_norm_j","A_norm_j","Gamma_j","T_j",
                               "Age_j","WTP_j","C_j")]

#### Save Data ####


## Check
# firmData = RA_transfers[METAL!="CATASTROPHIC",list(R_f_pred=sum(share_tilde*R_j*Gamma_j)/sum(share_tilde),
#                                                    T_sum_pm=-sum(share_tilde*12*T_j)/sum(share_tilde)),
#                     by=c("Firm","ST")]
# 
# firmData = merge(firm_RA,firmData,by=c("ST","Firm"),all.x=TRUE)
# setkey(firmData,ST,Firm)
# 
# firmData[,A_sum:=sum(A_wtd),by="ST"]
# firmData[,R_sum:=ST_R,by="ST"]
# firmData[,transfer_pred:=-ST_MLR_lives*avg_prem*(R_pred_wgt/R_sum - A_wtd/A_sum)]
# firmData[,transfer_pp:=-avg_prem*(R_f_pred/R_sum - A_f/A_sum)]
# 
# firmData[R_f>0,plot(R_f,R_f_pred)]
# firmData[R_f>0,plot(transfer_pp,pp_payments_adj)]
# firmData[R_f>0,plot(R_f,R_f_pred)]
#### Clear up Memory ####
rm(list=ls()[!grepl("acs|RA_transfers|full_predict|catas_prods|cost_par",ls())])

#### Calculate Person Level Demand Derivatives ####
markets = sort(unique(acs$Market))
states = unique(acs$ST)
setkey(full_predict,Market,Person,d_ind,Product)

per_derivs = list()
prod_derivs = list()
prod_rev_derivs = list()
prod_Age_derivs = list()
prod_WTP_derivs = list()

for (m in markets){
  print(m)
  m_data = full_predict[.(m),c("Person","d_ind","Market","Product","Firm",
                               "MedDeduct","MedOOP","High","GCF",
                               "Age","Family","LowIncome","WTP",
                               "alpha","ageRate","ageRate_avg","premBase",
                               "mkt_density","s_pred","s_pred_inside")]
  prods = sort(unique(m_data$Product))
  for (p in prods){
    dpvar = paste("dsdp",p,sep="_")
    #dpvar_insi = paste("ds_insi_dp",p,sep="_")
    evar = paste("elas",p,sep="_")
    dpvar_0 = paste("ds_0_dp",p,sep="_")
    prodtemp = m_data[Product==p,c("Person","d_ind","s_pred","s_pred_inside")]
    names(prodtemp) = c("Person","d_ind","s_prod","s_prod_inside")
    
    m_data = merge(m_data,prodtemp,by=c("Person","d_ind"),all.x=TRUE)
    # Set Market Share to 0 if missing
    m_data[is.na(s_prod),s_prod:=0]
    #m_data[,L_m:=max(L_m,na.rm=TRUE)]
    
    # Demand Derivatives
    m_data[,c(dpvar):=-alpha*ageRate_avg*s_pred*s_prod]
    m_data[Product==p,c(dpvar):=alpha*ageRate_avg*s_pred*(1-s_pred)]
    
    
    # State Mkt Share Derivs - Outside Good
    m_data[!Product%in%catas_prods,c(dpvar_0):=-sum(.SD*mkt_density,na.rm=TRUE),.SDcol=dpvar]
    #m_data[,dsMdp:=-(.SD*L_m/st_insured)*(1-S_m),.SDcol=dpvar_0]
    
    # Inside Market Share Derivatives 
    # m_data[,c(dpvar_insi):=-alpha*ageRate*s_pred_inside*s_prod_inside]
    # m_data[Product==p,c(dpvar_insi):=alpha*ageRate*s_pred_inside*(1-s_pred_inside)]
    # m_data[,c(dpvar_tilde):=dsdp_inside*S_m + dsMdp*s_pred_inside]
    # m_data[Market!=m,c(dpvar_tilde):=dsMdp*s_pred_inside]
    
    m_data[,c("s_prod","s_prod_inside"):=NULL]
    rm(prodtemp)
    
  }
  varlist = c("s_pred",names(m_data)[grepl("dsdp",names(m_data))])
  p_data = m_data[,lapply(.SD,function(x){sum(x*mkt_density)}),by=c("Product","premBase"),.SDcol=varlist]
  p_data_rev = m_data[,lapply(.SD,function(x){sum(x*ageRate_avg*mkt_density)}),by=c("Product","premBase"),.SDcol=varlist]
  p_data_Age = m_data[,lapply(.SD,function(x){sum(x*Age*mkt_density)}),by=c("Product","premBase"),.SDcol=varlist]
  p_data_WTP = m_data[,lapply(.SD,function(x){sum(x*WTP*mkt_density)}),by=c("Product","premBase"),.SDcol=varlist]
  
  setkey(p_data,Product)
  setkey(p_data_rev,Product)
  setkey(p_data_Age,Product)
  setkey(p_data_WTP,Product)
  
  
  ### Calculate Elasticities
  # dsdp = as.matrix(p_data[,-c(1,2,3)])
  # p_j = t(matrix(p_data$premBase,nrow=length(prods),ncol=length(prods)))
  # s_j = matrix(p_data$s_pred,nrow=length(prods),ncol=length(prods))
  # elas = dsdp*p_j/s_j
  # 
  # ### Calculate Diversion 
  # dsdp_own = matrix(diag(dsdp),nrow=length(prods),ncol=length(prods))
  # div = -dsdp/dsdp_own
  # 
  # o_share = matrix(1-p_data$s_pred,nrow=length(prods),ncol=length(prods))
  # div_share = t(s_j)/o_share
  # diag(div_share) = -1
  
  prod_derivs[[m]] = p_data[,-c(2,3)]
  prod_rev_derivs[[m]] = p_data_rev[,-c(2,3)]
  prod_Age_derivs[[m]] = p_data_Age[,-c(2,3)]
  prod_WTP_derivs[[m]] = p_data_WTP[,-c(2,3)]
  per_derivs[[m]] = m_data

  
  rm(m_data,p_data,p_data_rev)
}

## Save and Remove Lists
# save(prod_derivs,file="Intermediate_Output/FOC_Data/prod_derivs.rData")
# save(prod_rev_derivs,file="Intermediate_Output/FOC_Data/prod_rev_derivs.rData")
# rm(prod_derivs,prod_rev_derivs)
# 
# #### Save Person Derivative File to Memory ####
# states = sort(unique(acs$ST))
# for (st in states){
#   print(st)
#   mkts = unique(acs$Market[acs$ST==st])
#   st_per_derivs = per_derivs[mkts]
#   save(st_per_derivs,file=paste("Intermediate_Output/FOC_Data/per_derivs",st,".rData",sep=""))
# }
# 
# rm(per_derivs)

#### Transfer Derivatives ####
setkey(full_predict,Person,d_ind,Product)



T_derivs = list()
for (st in states){
  # file_deriv = paste("Intermediate_Output/FOC_Data/per_derivs",st,".rData",sep="")
  # load(file_deriv)
  # Initialize product data
  prod_temp = RA_transfers[ST==st,c("Product","share_tilde","S_j","S_0")]
  setkey(prod_temp,Product)
  
  
  prods = sort(unique(acs$Product[acs$ST==st]))
  for (p in prods){
    m = unique(full_predict$Market[full_predict$Product==p])
    print(st)
    print(p)
    # Full State Market
    st_data = full_predict[ST==st,c("Person","d_ind","Product","Market","premBase",
                                    "s_pred_inside","mkt_density",
                                    "R","ageRate_avg","ageRate",
                                    "S_m","mkt_size","st_insured",
                                    "s_pred","s_pred_outside")]
    
    dpvar = paste("dsdp",p,sep="_")
    dTvar = paste("dTdp",p,sep="_")
    dTvar_fixed = paste("dTdp_fix",p,sep="_")
    #dpvar_insi = paste("ds_insi_dp",p,sep="_")
    dpvar_0 = paste("ds_0_dp",p,sep="_")
    
    
    # Market Derivatives
    deriv = per_derivs[[m]]
    deriv = deriv[,.SD,.SDcol=c("Person","d_ind","Product",dpvar,dpvar_0)]
    
    # Match
    st_data = merge(st_data,deriv,by=c("Person","d_ind","Product"),all.x=TRUE)
    st_data = merge(st_data,RA_transfers[ST==st,c("Product","S_j","S_0")],by="Product",all.x=TRUE)
    setkey(st_data,Product,Market)
    
    # Calculate Adjusted State Derivatives
    st_data[,ds_0_dp:=max(.SD,na.rm=TRUE),.SDcol=dpvar_0]
    st_data[Market==m,L_m:=mkt_size]
    st_data[,L_m:=max(L_m,na.rm=TRUE)]
    
    st_data[,dsMdp:=-(ds_0_dp*L_m/st_insured)*(1-S_m)]
    st_data[Market!=m,dsMdp:=(ds_0_dp*L_m/st_insured)*(S_m)]
    
    
    if (!p%in%catas_prods){
      #Derivatives will be negative in sum, bc of OTHER firm
      st_data[,ds_tilde_dp:=dsMdp*S_j/S_0 + S_m*.SD/S_0 + S_m*ds_0_dp*S_j/(S_0^2),
              .SDcol=dpvar]
      st_data[Market!=m,ds_tilde_dp:=dsMdp*S_j/S_0]
      # Wrong
      # st_data[,ds_tilde_dp2:=.SD*S_m + dsMdp*s_pred_inside,.SDcol=dpvar_insi]
      # st_data[Market!=m,ds_tilde_dp:=dsMdp*s_pred_inside]
    }else{
      st_data[,ds_tilde_dp:=dsMdp*S_j/S_0]
    }
    st_data[Market!=m,c(dpvar):=0]
    
    st_data[Product%in%catas_prods,ds_tilde_dp:=NA]
    
    # Risk and Average Age Derivatives
    st_data[,dR_ij:=(.SD*R) ,.SDcol=dpvar]
    st_data[,dA_ij:=(.SD*ageRate_avg) ,.SDcol=dpvar]
    st_data[,elas:=.SD/s_pred*premBase,.SDcol=dpvar]
    
    # Aggregate to Product Level Data
    prod_T_deriv = st_data[,list(dR_j=sum(dR_ij*mkt_density),
                                 dA_j=sum(dA_ij*mkt_density),
                                 dsdp = sum(.SD*mkt_density),
                                 ds_tilde_dp = sum(ds_tilde_dp*mkt_density)),
                           by=c("Product"),.SDcol=dpvar]
    
    # Merge with product level RA info
    prod_T_deriv = merge(prod_T_deriv,RA_transfers,by="Product",all.x=TRUE)
    
    #prod_T_deriv[,elas:=dsdp*A_j*premBase/S_j]
    
    # Finish R and A derivatives 
    prod_T_deriv[,dR_j:=dR_j/R_bench]
    prod_T_deriv[,dR_j:=dR_j/S_j - dsdp*R_j/(S_j)]
    prod_T_deriv[,dA_j:=dA_j/S_j - dsdp*A_j/(S_j)]
    
    # Average Price Derivative
    prod_T_deriv[,dP_s:=(ds_tilde_dp*A_j*premBase + share_tilde*dA_j*premBase)]
    prod_T_deriv[Product==p,dP_s:=dP_s + share_tilde*A_j]
    prod_T_deriv[,dP_s:=sum(dP_s,na.rm=TRUE)]
    
    
    #Transfer Derivatives
    prod_T_deriv[,dR_norm_j:=(dR_j*Gamma_j/ST_R - 
                                R_norm_j*sum(Gamma_j*(ds_tilde_dp*R_j + share_tilde*dR_j),na.rm=TRUE)/ST_R)]
    
    prod_T_deriv[,dA_norm_j:=(dA_j*AV*Gamma_j/ST_A - 
                                A_norm_j*sum(Gamma_j*AV*(ds_tilde_dp*A_j + share_tilde*dA_j),na.rm=TRUE)/ST_A)]
    
    prod_T_deriv[,dT_norm_j:=dR_norm_j - dA_norm_j]
    
    #Fixed Transfer Derivative
    prod_T_deriv[,c(dTvar_fixed):=(dR_j*AV*Gamma_j/ST_R - dA_j*AV*Gamma_j/ST_A)*avg_prem]
    
    # Total Transfer Derivative
    prod_T_deriv[,c(dTvar):=dT_norm_j*avg_prem+T_norm_j*dP_s]
    
    prod_T_deriv[Product%in%catas_prods,c(dTvar):=0]
    prod_T_deriv[Product%in%catas_prods,c(dTvar_fixed):=0]
    
    prod_temp = merge(prod_temp,prod_T_deriv[,.SD,.SDcol=c("Product",dTvar,dTvar_fixed)],by="Product",all=TRUE)
    #Error won't be 0 because of outside RA firm
    # err = prod_T_deriv[,sum(share_tilde*.SD + ds_tilde_dp*T_j,na.rm=TRUE),.SDcol=dTvar]
    # print(err)
    rm(prod_T_deriv,st_data,deriv)
    
    #foc_data[Product%in%prods,foc_t:=solve(L_m*dsdp)%*%(t(dTdp)%*%(L_s*S_st))]
  }
  T_derivs[[st]] = prod_temp
  rm(prod_temp)
}



#### Apply Ownership Matrix to Demand Derivatives ####
for (m in names(per_derivs)){
  deriv = per_derivs[[m]]
  prods = sort(unique(deriv$Product))
  for (p in prods){
    f = unique(deriv$Firm[deriv$Product==p])
    dpvar = paste("dsdp",p,sep="_")
    deriv[Firm!=f,c(dpvar):=NA]
  }
  per_derivs[[m]] = deriv
}

#### FOC ####
MktConc = RA_transfers[,list(S_f = sum(S_j),
                             S_f_tilde = sum(share_tilde)),by=c("ST","Firm","Market")]
setkey(MktConc,S_f)

markets = sort(unique(acs$Market))

setkey(full_predict,Market,Person,d_ind,Product)
setkey(RA_transfers,Product,Market)
setkey(acs,Product)

foc_data = RA_transfers[,c("Product","Firm","ST","Market","premBase","T_j","S_j")]
for (m in markets){
  print(m)
  st = unique(acs$ST[acs$Market==m])
  
  ## Create Ownership Matrix
  ownMat = unique(acs[Market==m,c("Firm","Product")])
  setkey(ownMat,Product)
  firm_levels = unique(as.character(ownMat$Firm))
  firms = factor(ownMat$Firm,levels=firm_levels)
  if (length(firm_levels)>1){
    ownMat = model.matrix(1:nrow(ownMat)~-1+firms)
    ownMat = tcrossprod(ownMat)
  }else{
    ownMat = matrix(1,nrow=length(firms),ncol=length(firms))
  }
  
  ownMat_st = unique(acs[ST==st,c("Firm","Product")])
  firms = factor(ownMat_st$Firm,levels=unique(as.character(ownMat_st$Firm)))
  ownMat_st = model.matrix(1:nrow(ownMat_st)~-1+firms)
  ownMat_st = tcrossprod(ownMat_st)
  
  # deriv = per_derivs[[m]]
  # dpvar_list = names(deriv)[grepl("dsdp",names(deriv))]
  # deriv = deriv[,.SD,.SDcol=c("Person","d_ind","Market","Firm","Product","ageRate_avg","premBase","mkt_density","s_pred",
  #                             dpvar_list)]
  dsdp = as.matrix(prod_derivs[[m]][,-1])*ownMat
  dsdp_rev = as.matrix(prod_rev_derivs[[m]][,-1])*ownMat
  dsdp_age = as.matrix(prod_Age_derivs[[m]][,-1])*ownMat
  dsdp_wtp = as.matrix(prod_WTP_derivs[[m]][,-1])*ownMat
  
  
  prods = sort(unique(RA_transfers$Product[RA_transfers$Market==m]))
  A_j = as.matrix(RA_transfers[Market==m,"A_j"])
  P_j = as.matrix(RA_transfers[Market==m,"premBase"])
  T_j = as.matrix(RA_transfers[Market==m,"T_j"])
  
  
  vars = paste("dTdp",prods,sep="_")
  vars_fix = paste("dTdp_fix",prods,sep="_")
  
  dTdp = as.data.frame(T_derivs[[st]])
  dTdp = as.matrix(dTdp[,-c(1:4,grep("fix",names(dTdp)))])*ownMat_st
  dTdp = dTdp[,vars]
  
  dTdp_fix = as.data.frame(T_derivs[[st]])
  dTdp_fix = as.matrix(dTdp_fix[,grep("fix",names(dTdp_fix))])*ownMat_st
  dTdp_fix = dTdp_fix[,vars_fix]
  
  S_mkt = as.matrix(RA_transfers[Market==m,"S_j"])
  S_st = as.matrix(RA_transfers[ST==st,"S_j"])
  
  ## Costs
  C_j = as.matrix(RA_transfers[Market==m,"C_j"])
  Age_j = as.matrix(RA_transfers[Market==m,"Age_j"])
  Age_j = t(matrix(Age_j,nrow=length(S_mkt),ncol=length(S_mkt)))
  WTP_j = as.matrix(RA_transfers[Market==m,"WTP_j"])
  WTP_j = t(matrix(WTP_j,nrow=length(S_mkt),ncol=length(S_mkt)))
  S_mat = t(matrix(S_mkt,nrow=length(S_mkt),ncol=length(S_mkt)))
  dAge_j = dsdp_age/S_mat - dsdp/S_mat*Age_j
  dWTP_j = dsdp_wtp/S_mat - dsdp/S_mat*WTP_j
  
  dC_j = t((dAge_j*cost_par[1] + dWTP_j*cost_par[2])*matrix(C_j,nrow=length(S_mkt),ncol=length(S_mkt),byrow=TRUE))
  
  
  L_m = as.matrix(RA_transfers[Market==m,"mkt_size"])[1,1]
  L_s = as.matrix(RA_transfers[ST==st,"mkt_size"])
  
  foc1 =L_m*(dsdp_rev%*%P_j  + S_mkt*A_j) + L_m*dsdp%*%T_j + t(dTdp)%*%(L_s*S_st)
  
  rev = solve(dsdp)%*%(dsdp_rev%*%P_j + S_mkt*A_j)
  tran = solve(L_m*dsdp)%*% (L_m*dsdp%*%T_j + t(dTdp)%*%(L_s*S_st) ) 
  cost = solve(dsdp)%*%(dsdp%*%C_j + dC_j%*%S_mkt)
  cost_FOC = solve(L_m*dsdp)%*%(foc1)
  
  P_pred_0 =  - solve(L_m*dsdp_rev)%*%(
    L_m*(S_mkt*A_j - dsdp%*%C_j - dC_j%*%S_mkt) +L_m*dsdp%*%T_j + t(dTdp)%*%(L_s*S_st)
  )
  
  P_pred_1 =  - solve(L_m*dsdp_rev)%*%(
    L_m*(S_mkt*A_j - dsdp%*%C_j - dC_j%*%S_mkt) + .5*(L_m*dsdp%*%T_j + t(dTdp)%*%(L_s*S_st))
  )
  
  P_pred_2 =  - solve(L_m*dsdp_rev)%*%(
    L_m*(S_mkt*A_j - dsdp%*%C_j - dC_j%*%S_mkt)
  )
  
  P_pred_3 =  - solve(L_m*dsdp_rev)%*%(
    L_m*(S_mkt*A_j - dsdp%*%C_j - dC_j%*%S_mkt) +L_m*dsdp%*%T_j + t(dTdp_fix)%*%(L_s*S_st)
  )

  
    
  foc_data[Product%in%prods,margRev:=rev]
  foc_data[Product%in%prods,margTran:=tran]
  foc_data[Product%in%prods,margCost:=cost]
  foc_data[Product%in%prods,margCost_FOC:=cost_FOC]
  foc_data[Product%in%prods,premBase_sim0:=P_pred_0]
}
setkey(foc_data,Product)


#### Simulate ####