rm(list = ls())
library(data.table)
library(nleqslv)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

#### Read in GCF ####
gcf = read.csv("Data/2015_MLR/2015_GCF.csv")
gcf$Market = with(gcf,paste(State,Rating.Area,sep="_"))
gcf=as.data.table(gcf)
setkey(gcf,Market) 
#### Load Simulation Data and Merge in GCF/AV data ####
load("Simulation_Risk_Output/simData.rData")
load("Simulation_Risk_Output/FirmRiskScores.rData")

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

# Set Regulatory Factor
acs[,Gamma_j:=IDF*GCF]

# Remove Catastrophic Plans for now. 
acs = acs[Metal!="CATASTROPHIC",]

setkey(acs,Product,Person)
setkey(predict_data,Product,Person)

#### Predict Plan Average Allowable Rating Factors
pars = read.csv("Estimation_Output/estimationresults_fullapprox2018-03-07.csv")


alpha = pars$pars[1]
gamma = pars$pars[2:4]
beta = matrix(pars$pars[5:16],nrow=4,ncol=3,byrow=FALSE)
sigma = pars$pars[17:21]

## Calculate alpha for each demographic ##
acs[,alpha:=alpha+beta[1,1]*Age+beta[1,2]*Family+beta[1,3]*LowIncome]


## Integrate Draws and Prediction Data
randCoeffs = as.data.table(randCoeffs)
n_draws = nrow(randCoeffs)
randCoeffs[,nu_h:=V3/sigma[3]]
randCoeffs[,alpha_draw:=V1]
randCoeffs[,d_ind:=as.integer(1:n_draws)]
randCoeffs = randCoeffs[,c("d_ind","alpha_draw","nu_h")]
setkey(randCoeffs,d_ind)
setkey(predict_data,d_ind,Person)
alpha_large = randCoeffs[predict_data$d_ind,c("alpha_draw")]
nu_large = randCoeffs[predict_data$d_ind,c("nu_h")]

predict_data[,alpha_draw:=alpha_large]
predict_data[,nu_h:=nu_large]
rm(alpha_large,nu_large)

#### State Populations ####
states = unique(acs[,c("ST","Person","PERWT")])
states = states[,list(marketPop=sum(PERWT)),by=c("ST")]
acs = merge(acs,states,by="ST")

#### Risk By Product ####
setkey(acs,Product,Person)
setkey(predict_data,Product,Person,d_ind)

prodData = unique(acs[,c("Product","Firm","ST","marketPop","Gamma_j")])
prodData[,R_wgt:=vector(mode="numeric",length=nrow(prodData))]
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
step = 100
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
clear = ls()[!grepl("(acs|predict_data|index|prodData|n_draws)",ls())]
rm(list=clear)

acs = acs[,c("Product","Age","Family","LowIncome","Person","alpha","PERWT")]
gc()

#### Define Risk Function ####
psi_0 = 0
psi_age = .5
psi_Family = .1
psi_LowIncome = .8
psi_pref1 = .01
psi_pref2 = .5
psi = c(psi_0,psi_age,psi_Family,psi_LowIncome,psi_pref1,psi_pref2)

risk_function_est<- function(psi){
  prodData[,R_wgt:=vector(mode="numeric",length=nrow(prodData))]
  ## Demographic Risk
  acs[,R_dem:= psi[1]+psi[2]*Age+psi[3]*Family+psi[4]*LowIncome]
  
  cnt=0
  start = Sys.time()
  for (i in 1:length(index_acs)){
    cnt=cnt+1

    # st = Sys.time()
    # risk_predict = merge(acs[.(j),],predict_data[.(j),],by=c("Product","Person"))
    # Sys.time() - st
    
    #st = Sys.time()
    dem_data = acs[index_acs[[i]],c("alpha","PERWT","R_dem")]
    #dem_data = dem_data[rep(1:nrow(dem_data),each=n_draws),]
    risk_predict = predict_data[index_pred[[i]],]
    risk_predict[,alpha:=dem_data$alpha]
    risk_predict[,PERWT:=dem_data$PERWT]
    risk_predict[,R_dem:=dem_data$R_dem]
    #Sys.time()-st
    
    risk_predict[,pref_ratio:=exp(psi[6]*nu_h/(alpha+alpha_draw))/(1+exp(psi[6]*nu_h/(alpha+alpha_draw)))]
    
    risk_predict[,R:=R_dem+psi[5]*pref_ratio]
    #R_wgt_temp = risk_predict[,list(R_wgt=sum(R*s_pred*PERWT/n_draws),enroll=sum(s_pred*PERWT/n_draws)),by="Product"]
    R_wgt_temp = risk_predict[,list(R_wgt=sum(R*s_pred*PERWT/n_draws)),by="Product"]
    prodData[.(index_j[[i]]),R_wgt:=R_wgt_temp$R_wgt]
    print(cnt)
  }
  
  print(Sys.time()-start)
  
  prodData[,R_j:=R_wgt/marketPop]
  
  #firmData = prodData[,list(R_wgt=sum(R_wgt*Gamma_j),Enroll=sum(S_j)),by=c("Firm","ST","marketPop","R_pred")]
  firmData = prodData[,list(R_wgt=sum(R_wgt*Gamma_j)),by=c("Firm","ST","marketPop","R_pred")]
  firmData[,R_f:=R_wgt/marketPop]
  
  error = firmData[,sum((R_pred-R_f)^2)]
  rm(risk_predict,dem_data,firmData)
  print(error)
  print(psi)
  return(error)
}

risk_function_est(psi)

res = optim(par=psi,fn=risk_function_est)


# 
# 
# firmData[,S_f:=Enroll/marketPop]
# setkey(firmData,ST,Firm)
# 
# firmData[,list(lives=sum(Enroll),ins_rate=sum(Enroll)/marketPop),by=c("ST","marketPop")]
# 
# 
# Person_Predict = predict_data[,list(s_pred=mean(s_pred)),by=c("Person","Product")]
# 
# acs_predict = merge(Person_Predict,acs,by=c("Person","Product"))
# acs_predict = merge(acs_predict,prodData[,c("ST","Product")],by="Product")
# ###############
# insured = acs_predict[,list(s_pred=sum(s_pred)),by=c("Person","PERWT","ST")]
# insured[,list(ins_rate=sum(s_pred*PERWT)/sum(PERWT),ins_lives=sum(s_pred*PERWT),ins_total=sum(PERWT)),by="ST"]
# sum(insured$s_pred*insured$PERWT)/sum(insured$PERWT)
