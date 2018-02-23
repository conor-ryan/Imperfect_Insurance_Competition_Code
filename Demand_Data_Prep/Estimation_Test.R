rm(list=ls())
library(doBy)
library(data.table)
library(randtoolbox)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition")

#### Read in Data ####
choices = read.csv("Intermediate_Output/estimationData_MI.csv")
mkt = read.csv("Intermediate_Output/marketData_MI.csv")
#### Construct Parameters ####
n_draws = 500
pars = read.csv("Estimation_Output/estimationresults_2018-02-14.csv")

deltas = read.csv("Estimation_Output/deltaresults_2018-02-14.csv")

gamma = pars$pars[0:3]
beta = matrix(pars$pars[4:15],nrow=4,ncol=3,byrow=FALSE)
sigma = pars$pars[16:19]

randCoeffs = halton(n_draws,dim=4,usetime=TRUE,normal=TRUE)
for (k in 1:4){
  randCoeffs[,k] = randCoeffs[,k]*sigma[k]
}

choices = merge(choices,deltas,by.x="Product",by.y="prods")
choices = choices[order(choices$Person),]

#### Predict ####
choices$s_pred = NA

people = unique(choices$Person)
cnt = 0
start.time <- Sys.time()
for (p in people){
  cnt = cnt+1
  perData = choices[choices$Person==p,]
  
  demos = as.matrix(perData[1,c("Age","Family","LowIncome")])
  chars = as.matrix(perData[,c("Price","MedDeduct","MedOOP","High")])
  delta = perData$delta
  
  beta_z = demos%*%t(beta)
  beta_zi = matrix(beta_z,nrow=n_draws,ncol=length(beta_z),byrow=TRUE)
  for (k in 1:n_draws){
    beta_zi[k,] = beta_zi[k,] + randCoeffs[k,]
  }
  
  intercept = (demos%*%gamma)[1,1]
  util = matrix(NA,nrow=nrow(chars),ncol=n_draws)
  shares = matrix(NA,nrow=nrow(chars),ncol=n_draws)
  for(k in 1:n_draws){
    util[,k] = exp(intercept + chars%*%beta_zi[k,] + delta)
  }
  expsum = apply(util,MARGIN=2,sum)
  for(k in 1:n_draws){
    shares[,k] = util[,k]/(1+expsum[k])
  }
  choices$s_pred[choices$Person==p] = apply(shares,MARGIN=1,mean)
  if (cnt%%50==0){
    print(cnt)
  }
}


shares = summaryBy(s_pred~Product,data=choices,FUN=mean,keep.names=TRUE)
shares = merge(shares,mkt,by="Product")
