rm(list = ls())
library(data.table)
library(nleqslv)
library(ggplot2)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

## Run
run = "2018-05-12"

#### Load Data #### 
predFile = paste("Simulation_Risk_Output/predData_",run,".rData",sep="")
load(predFile)

## Cost Function 
costFile = paste("Simulation_Risk_Output/costParameters_",run,".rData",sep="")
load(costFile)

cost_par = CostRes$coefficients[grep("(Age|WTP)",names(CostRes$coefficients))]

## Alaska only...
full_predict = full_predict[ST=="AK",]
prod_data = prod_data[ST=="AK",]


prod_data[,premBase_pred:=premBase]

## Monthly Alpha
full_predict[,alpha:=alpha*12/1000]

full_predict[,elas:=alpha*(1-s_pred)*Price*1000/12*(ageRate_avg)]
full_predict[,semi_elas:=alpha*(1-s_pred)*100*100/12]

#### Calculate Market Shares / Equilibrium Distribution ####
## Set Prices and Recalculate Shares
err = 10
#while (err>.5){
st = Sys.time()

full_predict = merge(full_predict,prod_data[,c("Product","premBase_pred")],by="Product")
setkey(full_predict,Person,d_ind,Product)

full_predict[,Price_new:= pmax(premBase_pred*ageRate - subsidy,0)/MEMBERS]
full_predict[METAL=="CATASTROPHIC",Price_new:= (premBase_pred*ageRate)/MEMBERS]
full_predict[,Price_new:= (Price_new-Mandate/12)]


full_predict[,util:=non_price_util*exp(alpha*Price_new)]

full_predict[,exp_sum:=sum(util),by=c("Person","d_ind")]
full_predict[,s_pred:=util/(1+exp_sum)]


full_predict[METAL!="CATASTROPHIC",s_pred_outside:=1-sum(s_pred),by=c("Person","d_ind")]

prod_pred = full_predict[,list(lives = sum(s_pred*PERWT),
                               S_j = sum(s_pred*mkt_density),
                               S_0 = sum(mkt_density*(1-s_pred_outside)),
                               R_j = sum(s_pred*R*mkt_density)/sum(s_pred*mkt_density),
                               AV = sum(s_pred*AV*mkt_density)/sum(s_pred*mkt_density),
                               A_j = sum(s_pred*ageRate_avg*mkt_density)/sum(s_pred*mkt_density),
                               R_Gamma_j = sum(s_pred*R*Gamma_j*mkt_density)/sum(s_pred*mkt_density),
                               A_Gamma_j = sum(s_pred*AV*ageRate_avg*Gamma_j*mkt_density)/sum(s_pred*mkt_density),
                               Age_j = sum(s_pred*AGE*mkt_density)/sum(s_pred*mkt_density),
                               WTP_j = sum(s_pred*WTP*mkt_density)/sum(s_pred*mkt_density)),
                         by=c("Product")]

prod_pred = merge(prod_pred,prod_data,by="Product",all=TRUE)

## State Market Share
prod_pred[Metal_std!="CATASTROPHIC",RA_lives:=sum(lives),by="ST"]
prod_pred[,share_tilde:=RA_share*lives/RA_lives]
prod_pred[Metal_std!="CATASTROPHIC",S_m:=sum(lives)/RA_lives,by="Market"]
prod_pred[,S_m:=max(S_m,na.rm=TRUE),by="Market"]

## Normalize Risk Scores
prod_pred[,R_j:=R_j/R_bench]
prod_pred[,R_Gamma_j:=R_Gamma_j/R_bench]

## Other Firm
prod_pred[Firm=="OTHER",R_Gamma_j:=R_f]
prod_pred[Firm=="OTHER",share_tilde:=1-RA_share]

## State Wide Risk and Age
prod_pred[,ST_R:=sum(share_tilde*R_Gamma_j,na.rm=TRUE)]
prod_pred[Metal_std!="CATASTROPHIC",ST_A:=sum(lives*A_Gamma_j,na.rm=TRUE)/sum(lives,na.rm=TRUE)]


firmData = prod_pred[Metal_std!="CATASTROPHIC",list(S_f=sum(share_tilde),
                                                    R_f_pred=sum(share_tilde*R_Gamma_j)/sum(share_tilde)),
                     by=c("ST","Firm","R_f","R_bench")]


## Average State Premium
prod_pred[Firm!="OTHER",avg_prem:=sum(share_tilde*premBase_pred*A_j,na.rm=TRUE)/sum(share_tilde,na.rm=TRUE),by="ST"]
prod_pred[,T_norm_j:=(R_Gamma_j/ST_R - A_Gamma_j/ST_A)]
prod_pred[Firm=="OTHER",T_norm_j:=(R_Gamma_j/ST_R - 1)]
prod_pred[,R_norm_j:=(R_Gamma_j/ST_R)]
prod_pred[,A_norm_j:=(A_Gamma_j/ST_A)]
prod_pred[,T_j:=T_norm_j*avg_prem]
prod_pred[Metal_std=="CATASTROPHIC",T_j:=0]
prod_pred[Metal_std=="CATASTROPHIC",S_m:=0]
prod_pred[Metal_std=="CATASTROPHIC",RA_lives:=0]


prod_pred[,C_j:=exp(predict(CostRes,newdata=prod_pred))]

prod_pred = prod_pred[Firm!="OTHER",]

##### Demand Derivatives ####
print("Calculate Demand Derivatives")
markets = unique(prod_pred$Market)
catas_prods = prod_data$Product[prod_data$Metal_std=="CATASTROPHIC"]

per_derivs = list()
prod_derivs = list()
prod_rev_derivs = list()
prod_Age_derivs = list()
prod_WTP_derivs = list()

setkey(full_predict,Market)

for (m in markets){
  m_data = full_predict[.(m),c("Person","d_ind","Market","Product","Firm",
                               "MedDeduct","MedOOP","High",
                               "AGE","Family","LowIncome","WTP",
                               "alpha","ageRate","ageRate_avg","premBase",
                               "mkt_density","s_pred")]
  prods = sort(unique(m_data$Product))
  for (p in prods){
    dpvar = paste("dsdp",p,sep="_")
    
    evar = paste("elas",p,sep="_")
    dpvar_0 = paste("ds_0_dp",p,sep="_")
    prodtemp = m_data[Product==p,c("Person","d_ind","s_pred")]
    names(prodtemp) = c("Person","d_ind","s_prod")
    
    m_data = merge(m_data,prodtemp,by=c("Person","d_ind"),all.x=TRUE)
    
    # Set Market Share to 0 if missing
    m_data[is.na(s_prod),s_prod:=0]
    
    # Demand Derivatives
    m_data[,c(dpvar):=-alpha*ageRate_avg*s_pred*s_prod]
    m_data[Product==p,c(dpvar):=alpha*ageRate_avg*s_pred*(1-s_pred)]
    
    # State Mkt Share Derivs - Outside Good
    m_data[!Product%in%catas_prods,c(dpvar_0):=-sum(.SD*mkt_density,na.rm=TRUE),.SDcol=dpvar]
    
    m_data[,c("s_prod"):=NULL]
    rm(prodtemp)
    
  }
  varlist = c("s_pred",names(m_data)[grepl("dsdp",names(m_data))])
  p_data = m_data[,lapply(.SD,function(x){sum(x*mkt_density)}),by=c("Product","premBase"),.SDcol=varlist]
  p_data_rev = m_data[,lapply(.SD,function(x){sum(x*ageRate_avg*mkt_density)}),by=c("Product","premBase"),.SDcol=varlist]
  p_data_Age = m_data[,lapply(.SD,function(x){sum(x*AGE*mkt_density)}),by=c("Product","premBase"),.SDcol=varlist]
  p_data_WTP = m_data[,lapply(.SD,function(x){sum(x*WTP*mkt_density)}),by=c("Product","premBase"),.SDcol=varlist]
  
  setkey(p_data,Product)
  setkey(p_data_rev,Product)
  setkey(p_data_Age,Product)
  setkey(p_data_WTP,Product)
  
  prod_derivs[[m]] = p_data[,-c(2,3)]
  prod_rev_derivs[[m]] = p_data_rev[,-c(2,3)]
  prod_Age_derivs[[m]] = p_data_Age[,-c(2,3)]
  prod_WTP_derivs[[m]] = p_data_WTP[,-c(2,3)]
  per_derivs[[m]] = m_data
  
  rm(m_data,p_data,p_data_rev)
}


#### Transfer Derivatives ####
print("Calculate Transfer Derivatives")
setkey(full_predict,Person,d_ind,Product)
setkey(prod_pred,Product)

T_derivs = prod_pred[,c("Product")]

prods = sort(unique(prod_pred$Product))
for (p in prods){
  m = unique(full_predict$Market[full_predict$Product==p])
  # Full State Market
  st_data = full_predict[,c("Person","d_ind","Product","Market","premBase",
                            "mkt_density",
                            "Gamma_j","AV",
                            "R","ageRate_avg","ageRate",
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
  st_data = merge(st_data,prod_pred[,c("Product","S_j","S_0","S_m","RA_lives")],by="Product",all.x=TRUE)
  setkey(st_data,Product,Market)
  
  # Calculate Adjusted State Derivatives
  st_data[,ds_0_dp:=max(.SD,na.rm=TRUE),.SDcol=dpvar_0]
  st_data[Market==m,L_m:=S_m*RA_lives]
  st_data[,L_m:=max(L_m,na.rm=TRUE)]
  
  st_data[,dsMdp:=-(ds_0_dp*L_m/RA_lives)*(1-S_m)]
  st_data[Market!=m,dsMdp:=(ds_0_dp*L_m/RA_lives)*(S_m)]
  
  
  if (!p%in%catas_prods){
    #Derivatives will be negative in sum, bc of OTHER firm
    st_data[,ds_tilde_dp:=dsMdp*S_j/S_0 + S_m*.SD/S_0 + S_m*ds_0_dp*S_j/(S_0^2),
            .SDcol=dpvar]
    st_data[Market!=m,ds_tilde_dp:=dsMdp*S_j/S_0]
  }else{
    st_data[,ds_tilde_dp:=dsMdp*S_j/S_0]
  }
  
  st_data[Market!=m,c(dpvar):=0]
  
  st_data[Product%in%catas_prods,ds_tilde_dp:=NA]
  
  # Risk and Average Age Derivatives
  st_data[,dR_Gamma_ij:=(.SD*R*Gamma_j) ,.SDcol=dpvar]
  st_data[,dA_Gamma_ij:=(.SD*ageRate_avg*AV*Gamma_j) ,.SDcol=dpvar]
  st_data[,dA_ij:=(.SD*ageRate_avg) ,.SDcol=dpvar]

  # Aggregate to Product Level Data
  prod_T_deriv = st_data[,list(dR_Gamma_j=sum(dR_Gamma_ij*mkt_density),
                               dA_Gamma_j=sum(dA_Gamma_ij*mkt_density),
                               dA_j=sum(dA_ij*mkt_density),
                               dsdp = sum(.SD*mkt_density),
                               ds_tilde_dp = sum(ds_tilde_dp*mkt_density)),
                         by=c("Product"),.SDcol=dpvar]
  
  # Merge with product level RA info
  prod_T_deriv = merge(prod_T_deriv,prod_pred,by="Product",all.x=TRUE)
  

  # Finish R and A derivatives 
  prod_T_deriv[,dR_Gamma_j:=dR_Gamma_j/R_bench]
  prod_T_deriv[,dR_Gamma_j:=dR_Gamma_j/S_j - dsdp*R_Gamma_j/(S_j)]
  prod_T_deriv[,dA_Gamma_j:=dA_Gamma_j/S_j - dsdp*A_Gamma_j/(S_j)]
  
  # Average Price Derivative
  prod_T_deriv[,dP_s:=(ds_tilde_dp*A_j*premBase_pred + share_tilde*dA_j*premBase_pred)]
  prod_T_deriv[Product==p,dP_s:=dP_s + share_tilde*A_j]
  prod_T_deriv[,dP_s:=sum(dP_s,na.rm=TRUE)]
  
  
  #Transfer Derivatives
  prod_T_deriv[,dR_norm_j:=(dR_Gamma_j/ST_R - 
                              R_norm_j*sum((ds_tilde_dp*R_Gamma_j + share_tilde*dR_Gamma_j),na.rm=TRUE)/ST_R)]
  
  prod_T_deriv[,dA_norm_j:=(dA_Gamma_j/ST_A - 
                              A_norm_j*sum((ds_tilde_dp*A_Gamma_j + share_tilde*dA_Gamma_j),na.rm=TRUE)/ST_A)]
  
  prod_T_deriv[,dT_norm_j:=dR_norm_j - dA_norm_j]
  
  #Fixed Transfer Derivative
  prod_T_deriv[,c(dTvar_fixed):=(dR_Gamma_j/ST_R - dA_Gamma_j/ST_A)*avg_prem]
  
  # Total Transfer Derivative
  prod_T_deriv[,c(dTvar):=dT_norm_j*avg_prem+T_norm_j*dP_s]
  
  prod_T_deriv[Product%in%catas_prods,c(dTvar):=0]
  prod_T_deriv[Product%in%catas_prods,c(dTvar_fixed):=0]
  
  T_derivs = merge(T_derivs,prod_T_deriv[,.SD,.SDcol=c("Product",dTvar,dTvar_fixed)],by="Product",all=TRUE)
  #Error won't be 0 because of outside RA firm
  err = prod_T_deriv[,sum(share_tilde*.SD + ds_tilde_dp*T_j,na.rm=TRUE),.SDcol=dTvar]
  # print(err)
  rm(prod_T_deriv,st_data,deriv)
  
  #foc_data[Product%in%prods,foc_t:=solve(L_m*dsdp)%*%(t(dTdp)%*%(L_s*S_st))]
}


#### Apply Ownership Matrix to Demand Derivatives ####
# for (m in names(per_derivs)){
#   deriv = per_derivs[[m]]
#   prods = sort(unique(deriv$Product))
#   for (p in prods){
#     f = unique(deriv$Firm[deriv$Product==p])
#     dpvar = paste("dsdp",p,sep="_")
#     deriv[Firm!=f,c(dpvar):=NA]
#   }
#   per_derivs[[m]] = deriv
# }

#### FOC ####
print("Calculate First Order Conditions")
setkey(full_predict,Market,Person,d_ind,Product)
setkey(prod_pred,Product,Market)

err=0.0
foc_data = prod_pred[,c("Product","Firm","ST","Market","premBase","T_j","S_j","C_j")]
for (m in markets){
  print(m)
  
  ## Create Ownership Matrix
  ownMat = unique(prod_pred[Market==m,c("Firm","Product")])
  setkey(ownMat,Product)
  firm_levels = unique(as.character(ownMat$Firm))
  firms = factor(ownMat$Firm,levels=firm_levels)
  if (length(firm_levels)>1){
    ownMat = model.matrix(1:nrow(ownMat)~-1+firms)
    ownMat = tcrossprod(ownMat)
  }else{
    ownMat = matrix(1,nrow=length(firms),ncol=length(firms))
  }
  
  ownMat_st = unique(prod_pred[,c("Firm","Product")])
  firms = factor(ownMat_st$Firm,levels=unique(as.character(ownMat_st$Firm)))
  ownMat_st = model.matrix(1:nrow(ownMat_st)~-1+firms)
  ownMat_st = tcrossprod(ownMat_st)

  dsdp = as.matrix(prod_derivs[[m]][,-1])*ownMat
  dsdp_rev = as.matrix(prod_rev_derivs[[m]][,-1])*ownMat
  dsdp_age = as.matrix(prod_Age_derivs[[m]][,-1])*ownMat
  dsdp_wtp = as.matrix(prod_WTP_derivs[[m]][,-1])*ownMat
  
  
  prods = sort(unique(prod_pred$Product[prod_pred$Market==m]))
  A_j = as.matrix(prod_pred[Market==m,"A_j"])
  P_j = as.matrix(prod_pred[Market==m,"premBase_pred"])
  T_j = as.matrix(prod_pred[Market==m,"T_j"])
  
  
  vars = paste("dTdp",prods,sep="_")
  vars_fix = paste("dTdp_fix",prods,sep="_")
  
  dTdp = as.data.frame(T_derivs)
  dTdp = as.matrix(dTdp[,-c(1,grep("fix",names(dTdp)))])*ownMat_st
  dTdp = dTdp[,vars]
  
  dTdp_fix = as.data.frame(T_derivs)
  dTdp_fix = as.matrix(dTdp_fix[,grep("fix",names(dTdp_fix))])*ownMat_st
  dTdp_fix = dTdp_fix[,vars_fix]
  
  S_mkt = as.matrix(prod_pred[Market==m,"S_j"])
  S_st = as.matrix(prod_pred[,"S_j"])
  
  ## Costs
  C_j = as.matrix(prod_pred[Market==m,"C_j"])
  Age_j = as.matrix(prod_pred[Market==m,"Age_j"])
  Age_j = t(matrix(Age_j,nrow=length(S_mkt),ncol=length(S_mkt)))
  WTP_j = as.matrix(prod_pred[Market==m,"WTP_j"])
  WTP_j = t(matrix(WTP_j,nrow=length(S_mkt),ncol=length(S_mkt)))
  S_mat = t(matrix(S_mkt,nrow=length(S_mkt),ncol=length(S_mkt)))
  dAge_j = dsdp_age/S_mat - dsdp/S_mat*Age_j
  dWTP_j = dsdp_wtp/S_mat - dsdp/S_mat*WTP_j
  
  dC_j = t((dAge_j*cost_par[1] + dWTP_j*cost_par[2])*matrix(C_j,nrow=length(S_mkt),ncol=length(S_mkt),byrow=TRUE))
  
  
  L_m = as.matrix(prod_pred[Market==m,S_m*RA_lives])[1,1]
  L_s = as.matrix(prod_pred[,S_m*RA_lives])
  
  foc1 =L_m*(dsdp_rev%*%P_j  + S_mkt*A_j) + L_m*dsdp%*%T_j + t(dTdp)%*%(L_s*S_st)
  
  rev = solve(dsdp)%*%(dsdp_rev%*%P_j + S_mkt*A_j)
  tran = solve(L_m*dsdp)%*% (L_m*dsdp%*%T_j + t(dTdp)%*%(L_s*S_st) ) 
  cost = solve(dsdp)%*%(dsdp%*%C_j + dC_j%*%S_mkt)
  cost_FOC = solve(L_m*dsdp)%*%(foc1)
  
  P_pred_0 =  - solve(L_m*dsdp_rev)%*%(
    L_m*(S_mkt*A_j - dsdp%*%C_j - dC_j%*%S_mkt) +L_m*dsdp%*%T_j + t(dTdp)%*%(L_s*S_st)
  )
  foc_err = (rev+tran-cost)/100
  
  #MLR Constraint
  P_pred_0 = pmin(P_pred_0,C_j/.7)
  
  #Remove 0 Market Share
  P_pred_0[S_mkt<1e-5] = 0
  foc_err[S_mkt<1e-5] = 0
  
  print(m)
  print(P_pred_0)
  
  # # Calculate step
  # P_0 = as.matrix(prod_data[Market==m,"premBase_pred"])
  # diff = abs(P_pred_0 - P_0)
  #step = 1-sum((foc_err/100)^2)*5
  step = .05
  # step = min(max(step,0.1),.9)
  # 
  # print(step)
  
  foc_data[Product%in%prods,margRev:=rev]
  foc_data[Product%in%prods,margTran:=tran]
  foc_data[Product%in%prods,margCost:=cost]
  foc_data[Product%in%prods,margCost_FOC:=cost_FOC]
  foc_data[Product%in%prods,premBase_pred:=P_pred_0]
  err = err + sum((foc_err)^2)
  
  prod_data[Product%in%prods,premBase_pred:=P_pred_0*step + premBase_pred*(1-step)]
}
print(err)
print(Sys.time()-st)
full_predict[,premBase_pred:=NULL]



# setkey(foc_data,Product)
#}