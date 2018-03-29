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

## Marginal Cost Function
costFile = paste("Simulation_Risk_Output/costParameters_",run,".rData",sep="")
load(costFile)
phi_final = res$par


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
full_predict[,nu_h:=alpha_large]

full_predict[,alpha:=alpha+beta[1,1]*Age+beta[1,2]*Family+beta[1,3]*LowIncome + alpha_draw]
full_predict[,pref_h:=nu_h/alpha]

rm(alpha_large,nu_large)

## Predict Risk Scores

full_predict[,R_dem:= 1+psi_final[1]*Age+psi_final[2]*Family+psi_final[3]*LowIncome]
#full_predict[,pref_ratio:=exp(psi_final[6]*pref_h)/(1+exp(psi_final[6]*pref_h))]
full_predict[,R:=R_dem+psi_final[4]*nu_h]


## Market Shares
full_predict[Metal!="CATASTROPHIC",s_pred_inside:=s_pred/sum(s_pred),by=c("Person","d_ind")]
catas_prods = unique(acs$Product[acs$Metal=="CATASTROPHIC"])

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
    dpvar_insi = paste("ds_insi_dp",p,sep="_")
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
    m_data[,c(dpvar_insi):=-alpha*ageRate*s_pred_inside*s_prod_inside]
    m_data[Product==p,c(dpvar_insi):=alpha*ageRate*s_pred_inside*(1-s_pred_inside)]
    # m_data[,c(dpvar_tilde):=dsdp_inside*S_m + dsMdp*s_pred_inside]
    # m_data[Market!=m,c(dpvar_tilde):=dsMdp*s_pred_inside]
    
    m_data[,c("s_prod","s_prod_inside"):=NULL]
    rm(prodtemp)
    
  }
  varlist = names(m_data)[grepl("dsdp",names(m_data))]
  prod_derivs[[m]] = m_data[,lapply(.SD,function(x){sum(x*mkt_density)}),by="Product",.SDcol=varlist]
  per_derivs[[m]] = m_data
  rm(m_data)
}

#### Risk Adjustment Transfers ####
RA_transfers = full_predict[,list(share_tilde=sum(S_m*s_pred_inside*mkt_density),
                                  S_j = sum(s_pred*mkt_density),
                                  R_j = sum(s_pred*R*mkt_density)/sum(s_pred*mkt_density),
                                  Age_j = sum(s_pred*ageRate_avg*mkt_density)/sum(s_pred*mkt_density)),
                            by=c("Product","Metal","Market","ST","Firm","premBase","Gamma_j","AV","S_m","mkt_size","st_insured")]
RA_transfers[Metal=="CATASTROPHIC",share_tilde:=0]
## Average State Premium
RA_transfers[,avg_prem:=sum(share_tilde*premBase*Age_j,na.rm=TRUE),by="ST"]
RA_transfers[,ST_R:=sum(share_tilde*R_j*Gamma_j,na.rm=TRUE),by="ST"]
RA_transfers[,ST_A:=sum(share_tilde*AV*Age_j*Gamma_j,na.rm=TRUE),by="ST"]
RA_transfers[,T_norm_j:=(R_j*Gamma_j/ST_R - AV*Age_j*Gamma_j/ST_A)]
RA_transfers[,R_norm_j:=(R_j*Gamma_j/ST_R)]
RA_transfers[,A_norm_j:=(Age_j*AV*Gamma_j/ST_A)]
RA_transfers[,T_j:=T_norm_j*avg_prem]
RA_transfers[Metal=="CATASTROPHIC",T_j:=0]

RA_transfers = RA_transfers[,c("Product","Market","ST","Firm","Gamma_j","AV","premBase",
                               "share_tilde","S_j","R_j","Age_j",
                               "avg_prem","ST_R","ST_A","R_norm_j","A_norm_j","T_norm_j","T_j")]

#### Transfer Derivatives ####
setkey(full_predict,Person,d_ind,Product)

states = sort(unique(acs$ST))

T_derivs = list()
for (st in states){
  
  # Initialize product data
  prod_temp = RA_transfers[ST==st,c("Product","share_tilde","T_j")]
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
                                    "S_m","mkt_size","st_insured")]
    
    dpvar = paste("dsdp",p,sep="_")
    dTvar = paste("dTdp",p,sep="_")
    dpvar_insi = paste("ds_insi_dp",p,sep="_")
    dpvar_0 = paste("ds_0_dp",p,sep="_")
    
    
    # Market Derivatives
    deriv = per_derivs[[m]]
    deriv = deriv[,.SD,.SDcol=c("Person","d_ind","Product",dpvar,dpvar_0,dpvar_insi)]
    
    # Match
    st_data = merge(st_data,deriv,by=c("Person","d_ind","Product"),all.x=TRUE)
    setkey(st_data,Product,Market)
    
    # Calculate Adjusted State Derivatives
    st_data[,c(dpvar_0):=max(.SD,na.rm=TRUE),.SDcol=dpvar_0]
    st_data[Market==m,L_m:=mkt_size]
    st_data[,L_m:=max(L_m,na.rm=TRUE)]
    
    st_data[,dsMdp:=-(.SD*L_m/st_insured)*(1-S_m),.SDcol=dpvar_0]
    st_data[Market!=m,dsMdp:=(.SD*L_m/st_insured)*(S_m),.SDcol=dpvar_0]
    
    
    if (!p%in%catas_prods){
      st_data[,ds_tilde_dp:=.SD*S_m + dsMdp*s_pred_inside,.SDcol=dpvar_insi]
      st_data[Market!=m,ds_tilde_dp:=dsMdp*s_pred_inside]
      
      
    }else{
      st_data[,ds_tilde_dp:=dsMdp*s_pred_inside]
    }
    st_data[Market!=m,c(dpvar):=0]
    
    st_data[Product%in%catas_prods,ds_tilde_dp:=NA]
    
    # Risk and Average Age Derivatives
    st_data[,dR_ij:=(.SD*R) ,.SDcol=dpvar]
    st_data[,dA_ij:=(.SD*ageRate_avg) ,.SDcol=dpvar]
    
    # Average Price Derivative
    st_data[,dP_s:=(ds_tilde_dp*ageRate_avg*premBase)]
    st_data[Product==p,dP_s:=dP_s + s_pred_inside*S_m*ageRate_avg]
    st_data[,dP_s:=sum(dP_s*mkt_density,na.rm=TRUE)]
    
    # Aggregate to Product Level Data
    prod_T_deriv = st_data[,list(dR_j=sum(dR_ij*mkt_density),
                                 dA_j=sum(dA_ij*mkt_density),
                                 dsdp = sum(.SD*mkt_density),
                                 ds_tilde_dp = sum(ds_tilde_dp*mkt_density)),
                           by=c("Product","dP_s"),.SDcol=dpvar]
    # Merge with product level RA info
    prod_T_deriv = merge(prod_T_deriv,RA_transfers,by="Product",all.x=TRUE)
    
    # Transfer Index Derivative
    prod_T_deriv[,dR_j:=dR_j/S_j - R_j*dsdp/S_j]
    prod_T_deriv[,dA_j:=dA_j/S_j - Age_j*dsdp/S_j]
    
    prod_T_deriv[,dR_norm_j:=(dR_j*Gamma_j/ST_R - 
                                R_norm_j*sum(Gamma_j*(ds_tilde_dp*R_j + share_tilde*dR_j),na.rm=TRUE)/ST_R)]
    prod_T_deriv[,dA_norm_j:=(dA_j*AV*Gamma_j/ST_A - 
                                A_norm_j*sum(Gamma_j*AV*(ds_tilde_dp*Age_j + share_tilde*dA_j),na.rm=TRUE)/ST_A)]
    
    prod_T_deriv[,dT_norm_j:=dR_norm_j - dA_norm_j]
    
    # Total Transfer Derivative
    prod_T_deriv[,c(dTvar):=dT_norm_j*avg_prem+T_norm_j*dP_s]
    prod_T_deriv[Product%in%catas_prods,c(dTvar):=0]
    prod_temp = merge(prod_temp,prod_T_deriv[,.SD,.SDcol=c("Product",dTvar)],by="Product",all=TRUE)
    err = prod_T_deriv[,sum(share_tilde*.SD + ds_tilde_dp*T_j,na.rm=TRUE),.SDcol=dTvar]
    print(err)
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
markets = unique(acs$Market)

setkey(full_predict,Market,Person,d_ind,Product)
setkey(RA_transfers,Market,Product)

foc_data = RA_transfers[,c("Product","Firm","Market","premBase","T_j","S_j")]
for (m in markets){
  print(m)
  st = unique(acs$ST[acs$Market==m])
  
  ## Create Ownership Matrix
  ownMat = unique(acs[.(m),c("Firm","Product")])
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

  deriv[,cost:=exp(phi_final[1] + phi_final[2]*GCF + phi_final[3]*MedDeduct + phi_final[4]*MedOOP + phi_final[5]*High + 
                     phi_final[6]*nu_h + phi_final[7]*Age + phi_final[7]*Family + phi_final[9]*LowIncome + 
                     phi_final[10]*nu_h*High)]
  dsdp = prod_derivs[[m]]
  prods = sort(unique(deriv$Product))
  
  prod_dRev = deriv[,lapply(.SD,function(x){sum(x*(ageRate_avg*premBase-cost)*mkt_density,na.rm=TRUE)}),
                    by="Product",.SDcol=dpvar_list]
  
  prod_dRev = as.matrix(prod_dRev[,-1])
  prod_dRev = colSums(prod_dRev)
  
  dsdp = as.matrix(dsdp[,-1]) *ownMat
  age_j = as.matrix(RA_transfers[.(m),"Age_j"])
  T_j = as.matrix(RA_transfers[.(m),"T_j"])
  vars = paste("dTdp",prods,sep="_")
  dTdp = T_derivs[[st]]
  dTdp = as.matrix(dTdp[,-c(1,2,3)])*ownMat_st
  dTdp = dTdp[,vars]
  
  S_mkt = as.matrix(RA_transfers[.(m),"S_j"])
  S_st = as.matrix(RA_transfers[ST==st,"S_j"])
  
  foc_data[Product%in%prods,foc:=prod_dRev + dsdp%*%T_j + S_mkt*age_j +  t(dTdp)%*%S_st]
}

setkey(foc_data,Product)
