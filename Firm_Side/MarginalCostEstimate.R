rm(list = ls())
library(data.table)
library(nleqslv)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

## Run
run = "2018-03-17"

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
psi_final = res$par

## Demand Parameters
parFile = paste("Estimation_Output/estimationresults_",run,".csv",sep="")
pars = read.csv(parFile)


#### Full Data Set ####
## Demographic Density 
density = unique(acs[,c("Market","Person","PERWT")])
density[,ST:=gsub("_.*","",Market)]
density[,mkt_size:=sum(PERWT),by="Market"]
density[,st_size:=sum(PERWT),by="ST"]

density[,mkt_density:=PERWT/mkt_size]
density[,st_density:=PERWT/st_size]
density[,PERWT:=NULL]
acs = merge(acs,density,by=c("Market","Person"))

## Merge Random Coefficients
full_predict = merge(acs,predict_data,by=c("Product","Person"))

n_draws = nrow(randCoeffs)
full_predict[,mkt_density:=mkt_density/n_draws]
full_predict[,st_density:=st_density/n_draws]

## Product Data
prod_data = acs[,list(S_j=sum(s_pred_mean*PERWT)/sum(PERWT)),by=c("Product","Firm","Market")]

rm(predict_data)


#### Calculate Alpha ####
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

full_predict[,R_dem:= psi_final[1]+psi_final[2]*Age+psi_final[3]*Family+psi_final[4]*LowIncome]
full_predict[,pref_ratio:=exp(psi_final[6]*pref_h)/(1+exp(psi_final[6]*pref_h))]
full_predict[,R:=R_dem+psi_final[5]*pref_ratio]


#### Calculate Demand Derivatives ####
markets = sort(unique(acs$Market))
setkey(full_predict,Market,Person,d_ind,Product)

per_derivs = list()
prod_derivs = list()

for (m in markets){
  print(m)
  m_data = full_predict[.(m),c("Person","Product","d_ind","alpha","ageRate","R","mkt_density","s_pred")]
  m_data[,s_pred_wgt:=s_pred*mkt_density]
  prods = sort(unique(m_data$Product))
  for (p in prods){
    dpvar = paste("dsdp",p,sep="_")
    prodtemp = m_data[Product==p,c("Person","d_ind","s_pred")]
    names(prodtemp) = c("Person","d_ind","s_prod")
    
    m_data = merge(m_data,prodtemp,by=c("Person","d_ind"))
    
  
    m_data[,c(dpvar):=-alpha*ageRate*s_pred*s_prod]
    m_data[Product==p,c(dpvar):=alpha*ageRate*s_pred*(1-s_pred)]
    
    m_data[,c(paste(dpvar,"wgt",sep="_")):=-alpha*ageRate*s_pred*s_prod*mkt_density]
    m_data[Product==p,c(paste(dpvar,"wgt",sep="_")):=alpha*ageRate*s_pred*(1-s_pred)*mkt_density]
    
    
    m_data[,s_prod:=NULL]
    rm(prodtemp)
  }
  varlist = c("s_pred_wgt",paste("dsdp",prods,"wgt",sep="_"))
  
  prod_deriv_temp = m_data[,lapply(.SD,sum),by="Product",
                           .SDcols=varlist]
  prod_data[Product%in%prods,s_check:=prod_deriv_temp$s_pred_wgt]
  
  m_data[,c(varlist):=NULL]
  
  per_derivs[[m]] = m_data
  prod_derivs[[m]] = prod_deriv_temp
  
  rm(m_data,prod_deriv_temp)
}


#### Calculate product Level Demand Derivatives ####





#### Risk Adjustment Transfer Derivatives ####


