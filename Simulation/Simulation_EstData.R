rm(list=ls())
library(doBy)
library(randtoolbox)
library(data.table)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition")

## Run 
run = "2018-08-25"

#### Read in Data ####
estData = read.csv("Intermediate_Output/Estimation_Data/estimationData_discrete.csv")
estData = as.data.table(estData)
setkey(estData,Person,Product)

#### Draws ####
n_draws = 1000
r_mom = read.csv("Intermediate_Output/MEPS_Moments/R_Score_Moments.csv")
r_mom$Rtype= with(r_mom,1+Age_Cat+Inc_Cat*2)

draws = halton(n_draws+500,dim=1,usetime=TRUE,normal=FALSE)[501:(500+n_draws)]

HCC_draws = matrix(NA,nrow=n_draws,ncol=max(estData$Rtype))
RtypeMax = max(estData$Rtype)

for (j in 1:RtypeMax){
  any = 1 - r_mom$Any_HCC[r_mom$Rtype==j]
  mu = r_mom$mean_HCC_Silver[r_mom$Rtype==j]
  sigma = sqrt(r_mom$var_HCC_Silver[r_mom$Rtype==j])
  draws_any = (draws-any)/(1-any)
  
  log_norm = exp(qnorm(draws_any)*sigma + mu)
  log_norm[is.nan(log_norm)] = 0
  
  HCC_draws[,j] = log_norm
}

HCC_draws_metal = matrix(NA,nrow=n_draws,ncol=RtypeMax*5)
metal_list = c("Catastrophic","Bronze","Silver","Gold","Platinum")

for (j in 1:RtypeMax){
  for (m in 1:5){
    any = 1 - r_mom$Any_HCC[r_mom$Rtype==j]
    mean_var = paste("mean_HCC",metal_list[m],sep="_")
    sigma_var = paste("var_HCC",metal_list[m],sep="_")
    
    mu = r_mom[[mean_var]][r_mom$Rtype==j]
    sigma = sqrt(r_mom[[sigma_var]][r_mom$Rtype==j])
    draws_any = (draws-any)/(1-any)
    
    log_norm = exp(qnorm(draws_any)*sigma + mu)
    log_norm[is.nan(log_norm)] = 0
    
    HCC_draws_metal[,(m-1)*RtypeMax+j] = log_norm
  }
}

#### Read in Parameters ####
parFile = paste("Estimation_Output/estimationresults_",run,".csv",sep="")
pars = read.csv(parFile)

delFile = paste("Estimation_Output/deltaresults_",run,".csv",sep="")
deltas = read.csv(delFile)

beta_vec = pars$pars


gamma = beta_vec[1:5]
beta0 = beta_vec[6:8]
beta = matrix(0,nrow=3,ncol=5)
beta[1,1:ncol(beta)] = beta_vec[9:13]
sigma = beta_vec[14:15]
FE_pars = beta_vec[16:length(beta_vec)]



randCoeffs = array(data=NA,dim=c(n_draws,ncol(HCC_draws),length(sigma)))
for (k in 1:length(sigma)){
  randCoeffs[,,k] = HCC_draws*sigma[k]
}

#estData = merge(estData,deltas,by.x="Product",by.y="prods")
estData[,delta:=1]


#### Create Fixed Effects ####
# Market Product Category Fixed Effects
firm_list = sort(unique(estData$Firm))
firm_list = firm_list[firm_list!="PREMERA_BLUE_CROSS_BLUE_SHIELD_OF_ALASKA"]
for (fe in firm_list){
  var = paste("FE",fe,sep="_")
  estData[,c(var):=0]
  estData[Firm==fe,c(var):=1]
}
# 
# firm_list = sort(unique(estData$Firm_Market_Cat))[-1]
# for (fe in firm_list){
#   var = paste("FE",fe,sep="_")
#   estData[,c(var):=0]
#   estData[Firm_Market_Cat==fe,c(var):=1]
# }


#### Convert Data Sets ####
estData[,Person:= as.integer(Person)]
setkey(estData,Person,Product)

people = sort(unique(estData$Person))

predict_data = estData[,c("Person","Product")]
predict_data$s_pred = vector("double",nrow(predict_data))


## Replicate People in Order
sortedFirst = function(x,y){
  index = vector(mode="integer",length=length(x))
  j = 1
  for (i in 1:length(y)){
    if (x[j]<=y[i]){
      index[j] = i
      j = j+1
      if (j>length(x)){
        return(index)
      }
    }
  }
}

first_inds = sortedFirst(people,predict_data$Person)
first_inds = c(first_inds,nrow(predict_data)+1)

index = vector("list",length(people))

for (i in 1:length(people)){
  ind_temp = first_inds[i]:(first_inds[i+1]-1)
  index[[i]] = ind_temp
}

repl = unlist(lapply(index,FUN=function(x){rep(x,n_draws)}))
ind_draw = unlist(lapply(index,FUN=function(x){rep(1:n_draws,each=length(x))}))

predict_data = predict_data[repl, ]
predict_data[,d_ind:=ind_draw]
setkey(predict_data,Person)

#### Predict ####
cnt = 0
start = Sys.time()
estData[,s_pred_mean:=vector("double",nrow(estData))]
estData[,alpha:=vector("double",nrow(estData))]

#### Predict ####
cnt = 0
start = Sys.time()
for (p in people){
  cnt = cnt+1
  perData = estData[.(p),]
  
  demos = as.matrix(perData[1,c("AgeFE_31_39",
                                "AgeFE_40_51",
                                "AgeFE_52_64",
                                "Family",
                                "LowIncome")])
  chars = as.matrix(perData[,c("Price","AV","Big")])
  chars_0 = as.matrix(perData[,c("Price","AV","Big")])
  FE = as.matrix(perData[,.SD,.SDcols=names(perData)[grep("^FE_",names(perData))]])
  delta = perData$delta
  
  intercept = (demos%*%gamma)[1,1] #+ randCoeffs[,1]
  
  chars_int = chars_0%*%beta0 + FE%*%FE_pars
  chars_int = matrix(chars_int,nrow=nrow(chars),ncol=n_draws)
  
  beta_z = demos%*%t(beta)
  beta_zi = matrix(beta_z,nrow=n_draws,ncol=length(beta_z),byrow=TRUE)
  r_ind = unique(perData$Rtype)
  # for (n in 1:n_draws){
  #   beta_zi[n,2:ncol(beta_zi)] = beta_zi[n,2:ncol(beta_zi)] + randCoeffs[n,r_ind,]
  # }
  beta_zi[,2:ncol(beta_zi)] = beta_zi[,2:ncol(beta_zi)] + randCoeffs[,r_ind,]
  
  price_val = chars_0[,1]*(beta0[1] + beta_z[,1])
  price_val = matrix(price_val,nrow=nrow(chars),ncol=n_draws)
  
  
  # util = matrix(NA,nrow=nrow(chars),ncol=n_draws)
  # util_non_price = matrix(NA,nrow=nrow(chars),ncol=n_draws)
  shares = matrix(NA,nrow=nrow(chars),ncol=n_draws)
  util = exp(intercept + chars_int + chars%*%t(beta_zi))*delta
  util_non_price = exp(intercept + chars_int + chars%*%t(beta_zi)-price_val)*delta
  
  # for(n in 1:n_draws){
  #   util[,n] = exp(intercept + chars_int + chars%*%beta_zi[n,])*delta
  #   
  #   util_non_price[,n] = exp(intercept + chars_int + chars%*%beta_zi[n,]-price_val)*delta
  # }
  
  
  expsum = apply(util,MARGIN=2,sum)
  expsum = matrix(expsum,nrow=nrow(chars),ncol=n_draws,byrow=TRUE)
  shares = util/(1+expsum)
  # for(n in 1:n_draws){
  #   shares[,n] = util[,n]/(1+expsum[n])
  # }
  estData[.(p),s_pred_mean:=apply(shares,MARGIN=1,FUN=mean)]
  estData[.(p),alpha:=(beta0[1] + beta_z[,1])]
  predict_data[.(p),s_pred:=as.vector(shares)]
  predict_data[.(p),non_price_util:=as.vector(util_non_price)]
  predict_data[.(p),HCC_Silver:=rep(HCC_draws[,r_ind],each=nrow(chars))]
  if (cnt%%500==0){
    print(cnt)
  }
}
Sys.time() - start






rm(beta_zi,chars_int,deltas,demos,expsum,FE,pars,perData,price_val,r_mom,shares,util,util_non_price,
   beta_vec,FE_pars,firm_list,first_inds,ind_draw,ind_temp,index,log_norm,people,repl,randCoeffs)
gc()

predict_data[,"non_price_util":=NULL]
predict_data[,"draw_wgt":=NULL]
predict_compact = predict_data[,list(d_ind=mean(d_ind)),
                               by=c("Person","Product","s_pred","HCC_Silver")]
d0_draws = which(apply(HCC_draws,MARGIN=1,FUN=sum)==0)[1]
predict_compact[HCC_Silver==0,d_ind:=d0_draws]

rm(predict_data)

full_predict = merge(estData[,c("Person","Product","AV","N","METAL",
                                "HCC_age","Rtype","Any_HCC")],predict_compact,by=c("Product","Person"))

full_predict[,cnt:=1]
full_predict[HCC_Silver>0,pos_cnt:=sum(cnt),by=c("Person","Product")]
full_predict[,pos_cnt:=max(pos_cnt,na.rm=TRUE),by=c("Person","Product")]
full_predict[HCC_Silver>0,draw_wgt:=1]
full_predict[HCC_Silver==0,draw_wgt:=n_draws-pos_cnt]
full_predict[,c("cnt","pos_cnt"):=NULL]

#### Firm-level Risk Data ####
## Fill in Appropriate HCC RisK Scores
full_predict[,HCC_Metal:=vector(mode="numeric",length=nrow(full_predict))]

HCC_long = as.vector(HCC_draws_metal)

full_predict[AV==.57,Rtype_m:=1]
full_predict[AV==.6,Rtype_m:=2]
full_predict[AV%in%c(.7,.73),Rtype_m:=3]
full_predict[AV%in%c(.87,.8,.94),Rtype_m:=4]
full_predict[AV==.9,Rtype_m:=5]

full_predict[,index:=((Rtype_m-1)*4 + (Rtype-1))*n_draws + d_ind]
full_predict[,HCC_Metal:=HCC_long[index]]
full_predict[,R:=HCC_Metal + HCC_age]

full_predict[,c("index","Rtype_m","HCC_Metal"):=NULL]

## Firm Data
test = full_predict[Person==1,]
test[,sum(R*s_pred*draw_wgt)/sum(s_pred*draw_wgt),by="Product"]

full_predict[METAL!="CATASTROPHIC",sum(R*s_pred*draw_wgt*N)/sum(s_pred*draw_wgt*N)]

# firm_RA_Sim = full_predict[METAL!="CATASTROPHIC",list(R_f=sum(HCC_Metal*Gamma_j*s_pred*PERWT/n_draws)/sum(s_pred*PERWT/n_draws),
#                                                       A_f=sum(ageRate_avg*AV_std*Gamma_j*s_pred*PERWT/n_draws)/sum(s_pred*PERWT/n_draws)),
#                            by=c("Firm","ST")]


estData[,s_pred:=s_pred_mean]

### Predicted Product Shares ####
shares = estData[,list(e_pred=sum(s_pred*N),e_data=sum(S_ij*N),pop_offered=sum(N)),by=c("Product","Firm","Market")]
shares[,S_pred:= e_pred/pop_offered]
shares[,S_data_inside:= e_data/pop_offered]
shares[,mkt_ins:=sum(S_pred),]

## Test against moments
share_moment = read.csv("Intermediate_Output/Estimation_Data/marketDataMap_discrete.csv")
share_test = merge(shares,share_moment,by=c("Product","Firm","Market"),all=TRUE)
share_test[,diff:=S_pred-Share]

insured = estData[,list(s_pred=sum(s_pred)),by=c("Person","N","Market")]
insured[,ST:=gsub("_.*","",Market)]
insured = insured[,list(insured=sum(s_pred*N),lives=sum(N)),by="ST"]
insured[,urate:=1 - insured/lives]

unins_st = read.csv("Data/2015_ACS/uninsured_ST_acs2015.csv")
ins_test = merge(insured,unins_st,by.x="ST",by.y="state")

### Delta Analysis ####
prodData = as.data.table(share_moment)
prodData = merge(prodData,deltas,by.x="Product",by.y="prods")
prodData[,delta:=log(delta)]
prodData[,premBase:=premBase*12/1000]
prodData[,delta_adj:=delta - beta0*premBase]

summary(lm(delta~-1 + premBase + MedDeduct+METAL,data=prodData))
