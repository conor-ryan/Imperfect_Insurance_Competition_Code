rm(list=ls())
library(doBy)
library(randtoolbox)
library(data.table)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition")

#### Read in Data ####
estData = read.csv("Intermediate_Output/Estimation_Data/estimationData_discrete.csv")
estData = as.data.table(estData)
setkey(estData,Person,Product)


#### Read in Parameters ####
n_draws = 500
pars = read.csv("Estimation_Output/estimationresults_2018-03-17.csv")

deltas = read.csv("Estimation_Output/deltaresults_2018-03-17.csv")

alpha = pars$pars[1]
gamma = pars$pars[2:4]
beta = matrix(pars$pars[5:16],nrow=4,ncol=3,byrow=FALSE)
sigma = pars$pars[17:21]

draws = halton(n_draws,dim=3,usetime=TRUE,normal=TRUE)
randCoeffs = matrix(nrow=n_draws,ncol=length(sigma))
j = 1
for (k in 1:5){
  if (k<3){j=j+1}
  randCoeffs[,k] = draws[,j]*sigma[k]
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
estData[,s_pred:=vector("double",nrow(estData))]
for (p in people){
  cnt = cnt+1
  perData = estData[.(p),]
  
  demos = as.matrix(perData[1,c("Age","Family","LowIncome")])
  chars = as.matrix(perData[,c("Price","MedDeduct","MedOOP","High")])
  delta = perData$delta
  
  intercept = (demos%*%gamma)[1,1] + randCoeffs[,1]
  
  price_int = alpha*chars[,1]
  
  beta_z = demos%*%t(beta)
  beta_zi = matrix(beta_z,nrow=n_draws,ncol=length(beta_z),byrow=TRUE)
  for (k in 1:n_draws){
    beta_zi[k,] = beta_zi[k,] + randCoeffs[k,2:5]
  }
  
  
  util = matrix(NA,nrow=nrow(chars),ncol=n_draws)
  shares = matrix(NA,nrow=nrow(chars),ncol=n_draws)
  for(k in 1:n_draws){
    util[,k] = exp(intercept[k] + price_int + chars%*%beta_zi[k,])*delta
  }
  expsum = apply(util,MARGIN=2,sum)
  for(k in 1:n_draws){
    shares[,k] = util[,k]/(1+expsum[k])
  }
  estData[.(p),s_pred:=apply(shares,MARGIN=1,FUN=mean)]
  #predict_data[.(p),s_pred:=as.vector(shares)]
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
share_moment = read.csv("Intermediate_Output/Estimation_Data/marketDataMap_discrete.csv")
share_test = merge(shares,share_moment,by=c("Product","Firm","Market"),all=TRUE)
share_test[,diff:=S_pred-Share]

insured = estData[,list(s_pred=sum(s_pred)),by=c("Person","N","Market")]
insured[,ST:=gsub("_.*","",Market)]
insured = insured[,list(insured=sum(s_pred*N),lives=sum(N)),by="ST"]
insured[,urate:=1 - insured/lives]

unins_st = read.csv("Data/2015_ACS/uninsured_ST_acs2015.csv")
ins_test = merge(insured,unins_st,by.x="ST",by.y="state")

