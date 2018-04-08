rm(list = ls())
library(data.table)
library(nleqslv)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

## Run
run = "2018-03-18"

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
riskFile = paste("Simulation_Risk_Output/riskParameters_",run,".rData",sep="")
load(riskFile)
f_list = list()
for (i in 1:length(res_list)){
  f_list[[i]] = res_list[[i]]$value
}
opt = which(unlist(f_list)==min(unlist(f_list)))
psi_final = res_list[[opt]]$par[1:4]

## Demand Parameters
parFile = paste("Estimation_Output/estimationresults_",run,".csv",sep="")
pars = read.csv(parFile)


#### Merge in Regulation Parameters ####
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

#### Merge in Base Premium Information ####
choiceSet = read.csv("Intermediate_Output/Premiums/choiceSets2015.csv")
# Get Product Name
choiceSet$Market = with(choiceSet,paste(ST,gsub("Rating Area ","",AREA),sep="_"))
choiceSet$Product_Name = with(choiceSet,paste(Firm,toupper(METAL),Market,sep="_"))
# Normalize Base Premium
choiceSet$premBase = choiceSet$PREMI27/1.048
choiceSet$premBase[choiceSet$ST=="DC"] = choiceSet$PREMI27[choiceSet$ST=="DC"]/.727
choiceSet$premBase[choiceSet$ST=="MA"] = choiceSet$PREMI27[choiceSet$ST=="MA"]/1.22
choiceSet$premBase[choiceSet$ST=="MN"] = choiceSet$PREMI27[choiceSet$ST=="MN"]/1.048
choiceSet$premBase[choiceSet$ST=="UT"] = choiceSet$PREMI27[choiceSet$ST=="UT"]/1.39

# Create Data Table
choiceSet = as.data.table(choiceSet[,c("Product_Name","premBase")])

# Merge with prediction data 
setkey(acs,Product_Name)
acs = merge(acs,choiceSet,by="Product_Name",all.x=TRUE)


#### Full Data Set ####
## Demographic Density 
acs[Metal!="CATASTROPHIC",mkt_insured:=sum(PERWT*s_pred_mean),by=c("Market")]
acs[Metal!="CATASTROPHIC",st_insured:=sum(PERWT*s_pred_mean),by=c("ST")]
acs[Metal!="CATASTROPHIC",S_m:=mkt_insured/st_insured,by=c("ST")]

density = unique(acs[,c("Market","Person","PERWT")])
density[,mkt_size:=sum(PERWT),by="Market"]

density[,mkt_density:=PERWT/mkt_size]
density[,PERWT:=NULL]

acs = merge(acs,density,by=c("Market","Person"))

## Merge Random Coefficients
full_predict = merge(acs,predict_data,by=c("Product","Person"))

n_draws = nrow(randCoeffs)
full_predict[,mkt_density:=mkt_density/n_draws]

rm(predict_data)


#### Incorporate Parameters ####
alpha = pars$pars[1]
gamma = pars$pars[2:4]
beta = matrix(pars$pars[5:16],nrow=4,ncol=3,byrow=FALSE)
sigma = pars$pars[17:21]

## Calculate alpha/health pref for each demographic ##
randCoeffs = as.data.table(randCoeffs)
n_draws = nrow(randCoeffs)
randCoeffs[,nu_h:=V5/abs(sigma[5])]
randCoeffs[,alpha_draw:=V2]
randCoeffs[,d_ind:=as.integer(1:n_draws)]
randCoeffs = randCoeffs[,c("d_ind","alpha_draw","nu_h")]
setkey(randCoeffs,d_ind)
setkey(full_predict,d_ind,Person)
alpha_large = randCoeffs[full_predict$d_ind,c("alpha_draw")]
nu_large = randCoeffs[full_predict$d_ind,c("nu_h")]

full_predict[,alpha_draw:=alpha_large]
full_predict[,nu_h:=nu_large]

full_predict[,alpha:=(alpha+beta[1,1]*Age+beta[1,2]*Family+beta[1,3]*LowIncome + alpha_draw)*12/1000]
# Test Larger Elasticty
full_predict[,alpha:=alpha*4]
full_predict[,pref_h:=nu_h/alpha]
full_predict[,elas:=alpha*(1-s_pred)*Price*1000/12]


rm(alpha_large,nu_large)

## Predict Risk Scores

full_predict[,R_dem:= 1+psi_final[1]*Age+psi_final[2]*Family+psi_final[3]*LowIncome]
#full_predict[,pref_ratio:=exp(psi_final[6]*pref_h)/(1+exp(psi_final[6]*pref_h))]
full_predict[,R:=R_dem+psi_final[4]*nu_h]


## Market Shares
full_predict[Metal!="CATASTROPHIC",s_pred_inside:=s_pred/sum(s_pred),by=c("Person","d_ind")]
full_predict[Metal!="CATASTROPHIC",s_pred_outside:=1-sum(s_pred),by=c("Person","d_ind")]


catas_prods = unique(acs$Product[acs$Metal=="CATASTROPHIC"])

# test = full_predict[Metal!="CATASTROPHIC",list(l1 = sum(s_pred*PERWT/n_draws),
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
inside_RA_pred = full_predict[Metal!="CATASTROPHIC",
                              list(R_tot_pred = sum(s_pred*AV*Gamma_j*R*PERWT/n_draws)/
                                     sum(s_pred*PERWT/n_draws)),by="ST"]
## Compare to Predicted Outside Firm Risk
inside_Est = other_RA[Firm_Ag=="Inside",c("ST","avg_prem","ST_MLR_lives","payments_adj","RA_share","R_tot")]
outside_Est = other_RA[Firm_Ag=="Other",c("ST","R_tot")]
names(outside_Est) = c("ST","R_tot_other")
## Merge firm-level estimates
inside_RA_pred = merge(inside_RA_pred,inside_Est,by="ST",all.x=TRUE)
inside_RA_pred = merge(inside_RA_pred,outside_Est,by="ST",all.x=TRUE)
## Calculate state-level risk, taking outside firm into account
inside_RA_pred[,R_rel:= R_tot_other/R_tot]
inside_RA_pred[,ST_R:=RA_share*R_tot_pred + (1-RA_share)*R_rel*R_tot_pred]
inside_RA_pred[,t_test:=ST_MLR_lives*avg_prem*RA_share*((R_tot_pred/ST_R) - 1)]

inside_RA_pred = inside_RA_pred[,c("ST","ST_R","RA_share","avg_prem","ST_MLR_lives","payments_adj")]

## Adjust RA Market Shares 
full_predict = merge(full_predict,inside_RA_pred[,c("ST","RA_share")],by="ST",all.x=TRUE)
full_predict[,S_m:=RA_share*S_m]


#### Risk Adjustment Transfers ####
RA_transfers = full_predict[,list(share_tilde=sum(S_m*s_pred*mkt_density)/sum(mkt_density*(1-s_pred_outside)),
                                  S_j = sum(s_pred*mkt_density),
                                  S_0 = sum(mkt_density*(1-s_pred_outside)),
                                  R_j = sum(s_pred*R*mkt_density)/sum(s_pred*mkt_density),
                                  Age_j = sum(s_pred*ageRate_avg*mkt_density)/sum(s_pred*mkt_density)),
                            by=c("Product","Metal","Market","ST","Firm","premBase","Gamma_j","AV","S_m","mkt_size","st_insured")]
RA_transfers[Metal=="CATASTROPHIC",share_tilde:=0]
RA_transfers = merge(RA_transfers,inside_RA_pred,by="ST",all.x=TRUE)


## Average State Premium
#RA_transfers[,avg_prem_check:=sum(12*share_tilde*premBase*Age_j,na.rm=TRUE)/sum(share_tilde),by="ST"]
RA_transfers[,avg_prem:=avg_prem/12]
#RA_transfers[,ST_R_check:=sum(share_tilde*R_j*AV*Gamma_j,na.rm=TRUE)/sum(share_tilde),by="ST"]
RA_transfers[,ST_A:=sum(share_tilde*AV*Age_j*Gamma_j,na.rm=TRUE)/sum(share_tilde),by="ST"]
RA_transfers[,T_norm_j:=(R_j*AV*Gamma_j/ST_R - AV*Age_j*Gamma_j/ST_A)]
RA_transfers[,R_norm_j:=(R_j*AV*Gamma_j/ST_R)]
RA_transfers[,A_norm_j:=(Age_j*AV*Gamma_j/ST_A)]
RA_transfers[,T_j:=T_norm_j*avg_prem]
RA_transfers[Metal=="CATASTROPHIC",T_j:=0]
RA_transfers[,Revenue:=Age_j*premBase+T_j]

# ## Check
# firmRiskFile = paste("Simulation_Risk_Output/firmRiskScores_",run,".rData",sep="")
# load(firmRiskFile)
# firm_pred = RA_transfers[,list(tran_pred=sum(T_j*12*S_j*mkt_size)),by=c("Firm","ST")]
# firm_RA = merge(firm_RA,firm_pred,by= c("ST","Firm"),all.x=TRUE)
# firm_RA[,check:=(payments_adj+tran_pred)/payments_adj]
# 
# 
# RA_transfers = RA_transfers[,c("Product","Market","ST","Firm","Gamma_j","AV","premBase",
#                                "share_tilde","S_j","R_j","Age_j",
#                                "avg_prem","ST_R","ST_A","R_norm_j","A_norm_j","T_norm_j","T_j")]



#### Calculate Person Level Demand Derivatives ####
markets = unique(acs$Market)
states = unique(acs$ST)
setkey(full_predict,Market,Person,d_ind,Product)

per_derivs = list()
prod_derivs = list()

for (m in markets){
  print(m)
  m_data = full_predict[.(m),c("Person","d_ind","Market","Product","Firm",
                               "MedDeduct","MedOOP","High","GCF",
                               "Age","Family","LowIncome","nu_h",
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
    m_data[,c(dpvar):=-alpha*ageRate*s_pred*s_prod]
    m_data[Product==p,c(dpvar):=alpha*ageRate*s_pred*(1-s_pred)]
  
    
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
  ### Calculate Elasticities
  dsdp = as.matrix(p_data[,-c(1,2,3)])
  p_j = t(matrix(p_data$premBase,nrow=length(prods),ncol=length(prods)))
  s_j = matrix(p_data$s_pred,nrow=length(prods),ncol=length(prods))
  elas = dsdp*p_j/s_j
  
  ### Calculate Diversion 
  dsdp_own = matrix(diag(dsdp),nrow=length(prods),ncol=length(prods))
  div = -dsdp/dsdp_own
  
  o_share = matrix(1-p_data$s_pred,nrow=length(prods),ncol=length(prods))
  div_share = t(s_j)/o_share
  diag(div_share) = -1
  
  prod_derivs[[m]] = p_data[,-c(2,3)]
  per_derivs[[m]] = m_data
  rm(m_data)
}


#### Transfer Derivatives ####
setkey(full_predict,Person,d_ind,Product)

states = sort(unique(acs$ST))

T_derivs = list()
for (st in states){
  
  # Initialize product data
  prod_temp = RA_transfers[ST==st,c("Product","share_tilde","S_j","S_0","T_j")]
  setkey(prod_temp,Product)
  
  
  prods = sort(unique(acs$Product[acs$ST==st]))
  for (p in prods){
    m = unique(acs$Market[acs$Product==p])
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
    
    #prod_T_deriv[,elas:=dsdp*Age_j*premBase/S_j]
    
    # Finish R and A derivatives 
    prod_T_deriv[,dR_j:=dR_j/S_j - dsdp*R_j/S_j]
    prod_T_deriv[,dA_j:=dA_j/S_j - dsdp*Age_j/S_j]
    
    # Average Price Derivative
    prod_T_deriv[,dP_s:=(ds_tilde_dp*Age_j*premBase + share_tilde*dA_j*premBase)]
    prod_T_deriv[Product==p,dP_s:=dP_s + share_tilde*Age_j]
    prod_T_deriv[,dP_s:=sum(dP_s,na.rm=TRUE)]
    
    
    #Transfer Derivatives
    prod_T_deriv[,dR_norm_j:=(dR_j*AV*Gamma_j/ST_R - 
                                R_norm_j*sum(Gamma_j*AV*(ds_tilde_dp*R_j + share_tilde*dR_j),na.rm=TRUE)/ST_R)]

    prod_T_deriv[,dA_norm_j:=(dA_j*AV*Gamma_j/ST_A - 
                                A_norm_j*sum(Gamma_j*AV*(ds_tilde_dp*Age_j + share_tilde*dA_j),na.rm=TRUE)/ST_A)]
    
    prod_T_deriv[,dT_norm_j:=dR_norm_j - dA_norm_j]
    
    # Total Transfer Derivative
    prod_T_deriv[,c(dTvar):=dT_norm_j*avg_prem+T_norm_j*dP_s]
    prod_T_deriv[Product%in%catas_prods,c(dTvar):=0]
    prod_temp = merge(prod_temp,prod_T_deriv[,.SD,.SDcol=c("Product",dTvar)],by="Product",all=TRUE)
    #Error won't be 0 because of outside RA firm
    # err = prod_T_deriv[,sum(share_tilde*.SD + ds_tilde_dp*T_j,na.rm=TRUE),.SDcol=dTvar]
    # print(err)
    rm(prod_T_deriv,st_data,deriv)
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

#### Prepare First Order Conditions ####
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
  
  deriv = per_derivs[[m]]
  dpvar_list = names(deriv)[grepl("dsdp",names(deriv))]
  deriv = deriv[,.SD,.SDcol=c("Person","d_ind","Market","Firm","Product","ageRate_avg","premBase","mkt_density","s_pred",
                              dpvar_list)]
  dsdp = prod_derivs[[m]]
  prods = sort(unique(deriv$Product))
  
  prod_dRev = deriv[,lapply(.SD,function(x){sum(x*(ageRate_avg*premBase)*mkt_density,na.rm=TRUE)}),
                    by="Product",.SDcol=dpvar_list]
  
  prod_dRev = as.matrix(prod_dRev[,-1])
  prod_dRev = colSums(prod_dRev)
  
  dsdp = as.matrix(dsdp[,-1]) *ownMat
  age_j = as.matrix(RA_transfers[Market==m,"Age_j"])
  T_j = as.matrix(RA_transfers[Market==m,"T_j"])
  vars = paste("dTdp",prods,sep="_")
  dTdp = T_derivs[[st]]
  dTdp = as.matrix(dTdp[,-c(1:5)])*ownMat_st
  dTdp = dTdp[,vars]
  
  S_mkt = as.matrix(RA_transfers[Market==m,"S_j"])
  S_st = as.matrix(RA_transfers[ST==st,"S_j"])
  
  foc1 = prod_dRev  + S_mkt*age_j 
  foc2 =  dsdp%*%T_j 
  foc3 = t(dTdp)%*%S_st
  c_j = solve(dsdp)%*%(foc1+foc2+foc3)
  
  # 
  df = deriv[,sum(s_pred*(ageRate_avg*premBase)*mkt_density,na.rm=TRUE),by=c("Product","premBase")]#/S_mkt# + S_mkt*T_j
  avg_price = df$V1/S_mkt
  # foc_data[Product%in%prods,foc:=prod_dRev + S_mkt*age_j + dsdp%*%T_j +  t(dTdp)%*%S_st]
  foc_data[Product%in%prods,foc:=prod_dRev + S_mkt*age_j + dsdp%*%T_j +  t(dTdp)%*%S_st]
  foc_data[Product%in%prods,c_j:=solve(dsdp)%*%(foc1+foc2+foc3)]
}

setkey(foc_data,Product)
#### Dummy Marginal Cost Estimate ####


#### Full Marginal Cost Estimate ####
s_length = length(unique(foc_data$ST))
f_length = length(unique(foc_data$Firm))
phi_start = rep(0,10)
phi_start[1] = 1

marginalCostEst <- function(phi){
  for (m in markets){
    deriv = per_derivs[[m]]
    #setkey(deriv,Firm)
    dpvar_list = names(deriv)[grepl("dsdp",names(deriv))]
    # deriv = deriv[,.SD,.SDcol=c("Person","d_ind","Product","Firm","mkt_density",
    #                             "MedDeduct","MedOOP","High","GCF",
    #                             "Age","Family","LowIncome","nu_h",
    #                             dpvar_list)]
    
    deriv[,cost:=exp(phi[1] + phi[2]*GCF + phi[3]*MedDeduct + phi[4]*MedOOP + phi[5]*High + 
                       phi[6]*nu_h + phi[7]*Age + phi[8]*Family + phi[9]*LowIncome + 
                       phi[10]*nu_h*High)]
    prods = deriv$Product
    prod_dCost = t(as.matrix(deriv[,lapply(.SD,function(x){sum(x*cost*mkt_density,na.rm=TRUE)}),.SDcol=dpvar_list]))
    foc_data[Product%in%prods,MC:=prod_dCost]
  }
  err = foc_data[,sum((foc - MC)^2)]
  print(err)
  print(phi)
  return(err)
}

marginalCostEst(phi_start)
res = optim(par=phi_start,fn=marginalCostEst,control=list(maxit=5000))

costFile = paste("Simulation_Risk_Output/costParameters_",run,".rData",sep="")
save(res,file=costFile)
load(costFile)
phi=res$par


#### Test ####

full_predict[,cost:=exp(phi[1] + phi[2]*GCF + phi[3]*MedDeduct + phi[4]*MedOOP + phi[5]*High + 
                   phi[6]*nu_h + phi[7]*Age + phi[8]*Family + phi[9]*LowIncome + 
                   phi[10]*nu_h*High)]


cost_test = full_predict[,list(avgPrem = sum(ageRate_avg*premBase*s_pred*mkt_density)/sum(s_pred*mkt_density),
                               avgCost = sum(cost*s_pred*mkt_density)/sum(s_pred*mkt_density)),
             by=c("Product")]
cost_test = merge(cost_test,RA_transfers[,c("Product","Firm","Market","S_j","T_j")],by="Product")
cost_test[,post_transfer_rev:=avgPrem + T_j]
cost_test[,per_profit:=avgPrem -avgCost + T_j]

#### Simulate Prices ####
simPrices <- function(p,m){
  ## Create Ownership Matrix
  st = unique(acs$ST[acs$Market==m])
  Firms = unique(acs[.(m),c("Firm","Product")])
  firm_levels = unique(as.character(Firms$Firm))
  firms = factor(Firms$Firm,levels=firm_levels)
  if (length(firm_levels)>1){
    ownMat = model.matrix(1:nrow(ownMat)~-1+firms)
    ownMat = tcrossprod(ownMat)
  }else{
    ownMat = matrix(1,nrow=length(firms),ncol=length(firms))
  }
  Firms[,p_est:=p]
  
  ownMat_st = unique(acs[ST==st,c("Firm","Product")])
  firms = factor(ownMat_st$Firm,levels=unique(as.character(ownMat_st$Firm)))
  ownMat_st = model.matrix(1:nrow(ownMat_st)~-1+firms)
  ownMat_st = tcrossprod(ownMat_st)
  
  deriv = per_derivs[[m]]
  dpvar_list = names(deriv)[grepl("dsdp",names(deriv))]
  # deriv = deriv[,.SD,.SDcol=c("Person","d_ind","Market","Firm","Product","ageRate_avg","premBase","mkt_density","s_pred",
  #                             dpvar_list)]
  dsdp = prod_derivs[[m]]
  prods = sort(unique(deriv$Product))
  
  
  deriv[,cost:=exp(phi[1] + phi[2]*GCF + phi[3]*MedDeduct + phi[4]*MedOOP + phi[5]*High + 
                     phi[6]*nu_h + phi[7]*Age + phi[8]*Family + phi[9]*LowIncome + 
                     phi[10]*nu_h*High)]
  deriv = merge(deriv,Firms,by=c("Firm","Product"))
  
  prod_dRev = deriv[,lapply(.SD,function(x){sum(x*(ageRate_avg*p_est-cost)*mkt_density,na.rm=TRUE)}),
                    by="Product",.SDcol=dpvar_list]
  
  prod_dRev = as.matrix(prod_dRev[,-1])
  prod_dRev = colSums(prod_dRev)
  
  prod_dCost = deriv[,lapply(.SD,function(x){sum(x*(cost)*mkt_density,na.rm=TRUE)}),
                    by="Product",.SDcol=dpvar_list]
  
  prod_dCost = as.matrix(prod_dCost[,-1])
  prod_dCost = colSums(prod_dCost)
  
  dsdp = as.matrix(dsdp[,-1]) *ownMat
  age_j = as.matrix(RA_transfers[.(m),"Age_j"])
  T_j = as.matrix(RA_transfers[.(m),"T_j"])
  vars = paste("dTdp",prods,sep="_")
  dTdp = T_derivs[[st]]
  dTdp = as.matrix(dTdp[,-c(1,2,3)])*ownMat_st
  dTdp = dTdp[,vars]
  
  S_mkt = as.matrix(RA_transfers[.(m),"S_j"])
  S_st = as.matrix(RA_transfers[ST==st,"S_j"])
  
  foc_err = prod_dRev + S_mkt*age_j + dsdp%*%T_j  +  t(dTdp)%*%S_st
  return(foc_err)
}

phi = res$par
m = "AK_1"
p = foc_data$premBase[foc_data$Market==m]
simPrices(p,m)
res = nleqslv(p,simPrices,m=m)
