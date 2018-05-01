rm(list=ls())
library(doBy)
library(randtoolbox)
library(data.table)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition")

## Run 
run = "2018-04-12"

#### Read in Data ####
estData = read.csv("Intermediate_Output/Estimation_Data/estimationData_MI_discrete.csv")
estData = as.data.table(estData)
setkey(estData,Person,Product)

#### Read in Parameters ####
n_draws = 1

parFile = paste("Estimation_Output/estimationresults_",run,".csv",sep="")
pars = read.csv(parFile)

delFile = paste("Estimation_Output/deltaresults_",run,".csv",sep="")
deltas = read.csv(delFile)

# alpha = pars$pars[1]
# gamma = pars$pars[2:4]
# beta = matrix(pars$pars[5:16],nrow=4,ncol=3,byrow=FALSE)
# sigma = pars$pars[17:21]

# gamma = pars$pars[1:3]
# beta0 = pars$pars[4:6]
# beta = matrix(0,nrow=3,ncol=3)
# beta[2,1] = pars$pars[7]
# beta[3,1] = pars$pars[8]
# sigma = pars$pars[9:11]

gamma = pars$pars[1:3]
beta0 = pars$pars[4:6]
beta = matrix(0,nrow=3,ncol=3)
beta[2,1] = pars$pars[7]
beta[3,1] = pars$pars[8]
sigma = pars$pars[9:11]

draws = halton(n_draws,dim=2,usetime=TRUE,normal=TRUE)
randCoeffs = matrix(nrow=n_draws,ncol=length(sigma)+1)
randCoeffs[,1] = draws[,1]*sigma[1]
randCoeffs[,2] = 0
for (k in 3:4){
  randCoeffs[,k] = draws[,2]*sigma[k-1]
}

estData = merge(estData,deltas,by.x="Product",by.y="prods")

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
for (p in people){
  cnt = cnt+1
  perData = estData[.(p),]
  
  demos = as.matrix(perData[1,c("Age","Family","LowIncome")])
  #chars = as.matrix(perData[,c("Price","MedDeduct","MedOOP","High")])
  chars = as.matrix(perData[,c("Price","MedDeduct","High")])
  chars_0 = as.matrix(perData[,c("PriceDiff","MedDeductDiff","HighDiff")])
  delta = perData$delta
  
  intercept = (demos%*%gamma)[1,1] + randCoeffs[,1]
  
  chars_int = chars_0%*%beta0
  
  beta_z = demos%*%t(beta)
  beta_zi = matrix(beta_z,nrow=n_draws,ncol=length(beta_z),byrow=TRUE)
  for (k in 1:n_draws){
    beta_zi[k,] = beta_zi[k,] + randCoeffs[k,2:4]
  }
  
  
  util = matrix(NA,nrow=nrow(chars),ncol=n_draws)
  shares = matrix(NA,nrow=nrow(chars),ncol=n_draws)
  for(k in 1:n_draws){
    util[,k] = exp(intercept[k] + chars_int + chars%*%beta_zi[k,])*delta
  }
  expsum = apply(util,MARGIN=2,sum)
  for(k in 1:n_draws){
    shares[,k] = util[,k]/(1+expsum[k])
  }
  estData[.(p),s_pred_mean:=apply(shares,MARGIN=1,FUN=mean)]
  predict_data[.(p),s_pred:=as.vector(shares)]
  if (cnt%%500==0){
    print(cnt)
  }
}
Sys.time() - start

### Predicted Product Shares ####
shares = estData[,list(e_pred=sum(s_pred*N),e_data=sum(S_ij*N),pop_offered=sum(N)),by=c("Product","Firm","Market")]
shares[,S_pred:= e_pred/pop_offered]
shares[,S_data_inside:= e_data/pop_offered]
shares[,mkt_ins:=sum(S_pred),]

## Test against moments
share_moment = read.csv("Intermediate_Output/Estimation_Data/marketDataMap_MI_discrete.csv")
share_test = merge(shares,share_moment,by=c("Product","Firm","Market"),all=TRUE)
share_test[,diff:=S_pred-Share]

insured = estData[,list(s_pred=sum(s_pred)),by=c("Person","N","Market")]
insured[,ST:=gsub("_.*","",Market)]
insured = insured[,list(insured=sum(s_pred*N),lives=sum(N)),by="ST"]
insured[,urate:=1 - insured/lives]

unins_st = read.csv("Data/2015_ACS/uninsured_ST_acs2015.csv")
ins_test = merge(insured,unins_st,by.x="ST",by.y="state")

