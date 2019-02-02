rm(list = ls())
library(data.table)
library(nleqslv)
library(scales)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

run = "2018-08-25"

#### Read in Data ####
predFile = paste("Simulation_Risk_Output/predData_",run,".rData",sep="")
load(predFile)

full_predict[,names(full_predict)[grepl("(FE_|PriceDiff|MedDeduct|MedOOP|Product_Name|Big|HCC_age|Any_HCC|HHincomeFPL|Family|Age|LowIncome|AGE|Rtype|WTP)",
                                        names(full_predict))]:=NULL]
gc()
## Cost Function 
costFile = paste("Simulation_Risk_Output/costParameters_MSM_Stage2_",run,".rData",sep="")
load(costFile)

#cost_par = CostRes$coefficients[grep("(Age|WTP)",names(CostRes$coefficients))]

#### Load Equilibrium Solutions ####
eqFiles = list.files("Estimation_Output")[grep("solvedEquil.*_RA_Man2_.*csv",list.files("Estimation_Output"))]

prod_data = prod_data[Firm!="OTHER",]
prod_data = prod_data[ST%in%c("MI","MO","GA","IL"),]
full_predict = full_predict[ST%in%c("MI","MO","GA","IL"),]

setkey(prod_data,Product)
setkey(full_predict,Product)
for (st in c("MI","MO","GA","IL")){
  file = paste("Estimation_Output/solvedEquilibrium_",st,".csv",sep="")
  print(file)
  temp = read.csv(file)
  temp = temp[order(temp$Products),]
  #no_transfer = grepl("no_t",file)
  
  prod_data[Product%in%temp$Products,prem_base:= temp$Price_base]
  prod_data[Product%in%temp$Products,prem_RA:= temp$Price_RA]
  prod_data[Product%in%temp$Products,prem_man:= temp$Price_man]
  
  n = 0
  for (j in unique(temp$Products)){
    n = n+1
    full_predict[.(j),prem_base:= temp$Price_base[n]]
    full_predict[.(j),prem_RA:= temp$Price_RA[n]]
    full_predict[.(j),prem_man:= temp$Price_man[n]]
  }
  
  file = paste("Estimation_Output/solvedEquilibrium_merger_",st,".csv",sep="")
  print(file)
  temp = read.csv(file)
  temp = temp[order(temp$Products),]
  #no_transfer = grepl("no_t",file)
  
  prod_data[Product%in%temp$Products,prem_base_m:= temp$Price_base]
  prod_data[Product%in%temp$Products,prem_RA_m:= temp$Price_RA]
  prod_data[Product%in%temp$Products,prem_man_m:= temp$Price_man]
  
  n = 0
  for (j in unique(temp$Products)){
    n = n+1
    full_predict[.(j),prem_base_m:= temp$Price_base[n]]
    full_predict[.(j),prem_RA_m:= temp$Price_RA[n]]
    full_predict[.(j),prem_man_m:= temp$Price_man[n]]
  }
}


#### Calculate Base Model Market Shares ####
## Monthly Alpha
full_predict[,alpha:=alpha*12/1000]
full_predict[,Mandate:=Mandate/12]


## Set Prices and Recalculate Shares

full_predict[,s_base:=vector(mode="double",nrow(full_predict))]
full_predict[,s_man:=vector(mode="double",nrow(full_predict))]
full_predict[,s_RA:=vector(mode="double",nrow(full_predict))]
full_predict[,s_base_m:=vector(mode="double",nrow(full_predict))]
full_predict[,s_man_m:=vector(mode="double",nrow(full_predict))]
full_predict[,s_RA_m:=vector(mode="double",nrow(full_predict))]

full_predict[,CW_base:=vector(mode="double",nrow(full_predict))]
full_predict[,CW_man:=vector(mode="double",nrow(full_predict))]
full_predict[,CW_RA:=vector(mode="double",nrow(full_predict))]
full_predict[,CW_base_m:=vector(mode="double",nrow(full_predict))]
full_predict[,CW_man_m:=vector(mode="double",nrow(full_predict))]
full_predict[,CW_RA_m:=vector(mode="double",nrow(full_predict))]

setkey(full_predict,Person,d_ind,Product)
for (var in c("base","man","RA","base_m","man_m","RA_m")){
  print(var)
  pvar = paste("prem",var,sep="_")
  svar = paste("s",var,sep="_")
  CWvar = paste("CW",var,sep="_")
  Insvar = paste("ins",var,sep="_")
  
  ## Re-Calculate Benchmark and Subsidies
  print("Benchmark Calculation")
  #setkey(full_predict,Product)
  prod_data$rank=ave(prod_data[[pvar]],with(prod_data,paste(Metal_std,Market)),FUN=rank)
  benchmark = prod_data[Metal_std=="SILVER",]
  benchmark[,maxrank:=max(rank),by="Market"]
  benchmark = benchmark[rank==2 | (rank==1&maxrank==1),]
  benchmark = benchmark[,c(.SD),.SDcol=c("Product",pvar)]
  names(benchmark) = c("Product","Benchmark")
  print(nrow(benchmark))
  
  for (j in unique(benchmark$Product)){
    full_predict[Product==j,Benchmark:=benchmark$Benchmark[benchmark$Product==j]]
  }
  full_predict[,Benchmark:=max(Benchmark,na.rm=TRUE),by="Market"]
  full_predict[,subsidy:=pmax(Benchmark*ageRate - IncomeCont,0)]
  
  
  print("Share Calculation")
  full_predict[,Price_new:=.SD*ageRate-subsidy,.SDcol=pvar]
  full_predict[,Price_new:=pmax(Price_new,0)]
  full_predict[,Price_new:= Price_new/MEMBERS]
  full_predict[METAL=="CATASTROPHIC",Price_new:= (.SD*ageRate)/MEMBERS,.SDcol=pvar]
  full_predict[,util:=non_price_util*exp(alpha*Price_new)]
  full_predict[,exp_sum:=sum(util),by=c("Person","d_ind")]
  full_predict[,c(svar):=util/(exp(alpha*Mandate)+exp_sum)]
  full_predict[,c(Insvar):=exp_sum/(exp(alpha*Mandate)+exp_sum)]
  if(var=="man"){
    full_predict[,c(CWvar):=-log(exp_sum + 1)/alpha]
  }else{
    full_predict[,c(CWvar):=-log(exp_sum+ exp(alpha*(Mandate)))/alpha]
  }
  full_predict[,c("Price_new","util","exp_sum","Benchmark"):=NULL]
}



#### Preliminary Results ####
prod_pred = full_predict[,list(lives = sum(s_base*PERWT),
                               s_base = sum(s_base*PERWT),
                               s_RA = sum(s_RA*PERWT),
                               s_man = sum(s_man*PERWT),
                               s_base_m = sum(s_base_m*PERWT),
                               s_RA_m = sum(s_RA_m*PERWT),
                               s_man_m = sum(s_man_m*PERWT),
                               Age_Avg = sum(s_base*ageRate_avg*mkt_density)/sum(s_base*mkt_density),
                               C_j = sum(C*s_base*PERWT)/sum(s_base*PERWT)),
                         by=c("Product")]

prod_pred = merge(prod_pred,prod_data,by="Product")

### HHI Table
prod_pred[,s_base:=s_base/sum(s_base),by="Market"]
prod_pred[,s_RA:=s_RA/sum(s_RA),by="Market"]
prod_pred[,s_man:=s_man/sum(s_man),by="Market"]

prod_pred[,s_base_m:=s_base_m/sum(s_base_m),by="Market"]
prod_pred[,s_RA_m:=s_RA_m/sum(s_RA_m),by="Market"]
prod_pred[,s_man_m:=s_man_m/sum(s_man_m),by="Market"]


firms = prod_pred[,list(s_base = sum(s_base),
                        s_RA   = sum(s_RA),
                        s_man  = sum(s_man)),by=c("Market","Firm")]

firms[,merger:="None"]
firms[Firm%in%c("AETNA","HUMANA"),merger:= "Aetna-Humana"]
firms[Firm%in%c("ANTHEM_BLUE_CROSS_AND_BLUE_SHIELD","BLUE_CROSS_BLUE_SHIELD_OF_GEORGIA","CIGNA_HEALTH_AND_LIFE_INSURANCE_COMPANY"),merger:= "Anthem-Cigna"]


firms[,mergeMarket:= 0]
firms[Firm%in%c("AETNA","HUMANA"),mergeMarket:= 1]
firms[,mergeMarket:=sum(mergeMarket),by="Market"]
firms[mergeMarket<2,mergeMarket:=0]
firms[mergeMarket==0&Firm%in%c("AETNA","HUMANA"),merger:="None"]

firms[,mergeMarket_2:= 0]
firms[Firm%in%c("ANTHEM_BLUE_CROSS_AND_BLUE_SHIELD","BLUE_CROSS_BLUE_SHIELD_OF_GEORGIA","CIGNA_HEALTH_AND_LIFE_INSURANCE_COMPANY"),mergeMarket_2:= 1]
firms[,mergeMarket_2:=sum(mergeMarket_2),by="Market"]
firms[mergeMarket_2<2,mergeMarket_2:=0]
firms[mergeMarket_2==0&Firm%in%c("ANTHEM_BLUE_CROSS_AND_BLUE_SHIELD","BLUE_CROSS_BLUE_SHIELD_OF_GEORGIA","CIGNA_HEALTH_AND_LIFE_INSURANCE_COMPANY"),merger:="None"]

firms[,mergeMarket:=pmax(mergeMarket,mergeMarket_2)]
firms[,mergeMarket_2:=NULL]

firms[,dHHI:=2*prod(s_base*100,na.rm=TRUE),by=c("Market","merger")]
firms[merger=="None"|mergeMarket==0,dHHI:=0]
# firms[dHHI==0,merger:="None"]




hhi = firms[,list(hhi_base = sum((s_base*100)^2),
                  hhi_RA   = sum((s_RA*100)^2),
                  hhi_man  = sum((s_man*100)^2),
                  dhhi_pred  = sum(dHHI)/2), by=c("Market")]

prod_pred[Firm%in%c("AETNA","HUMANA"),Firm:= "AETNA"]
prod_pred[Firm%in%c("ANTHEM_BLUE_CROSS_AND_BLUE_SHIELD","BLUE_CROSS_BLUE_SHIELD_OF_GEORGIA","CIGNA_HEALTH_AND_LIFE_INSURANCE_COMPANY"),Firm:="ANTHEM"]
firms_m = prod_pred[,list(s_base_m = sum(s_base_m),
                          s_RA_m   = sum(s_RA_m),
                          s_man_m  = sum(s_man_m)),by=c("Market","Firm")]
hhi_m = firms_m[,list(hhi_base_m = sum((s_base_m*100)^2),
                    hhi_RA_m   = sum((s_RA_m*100)^2),
                    hhi_man_m  = sum((s_man_m*100)^2)), by=c("Market")]

hhi = merge(hhi,hhi_m,by="Market")
hhi[,dhhi_actual:=hhi_base_m-hhi_base]

hhi[,mergerLabel:="No Merger"]
hhi[dhhi_pred>0,mergerLabel:="Weak"]
hhi[dhhi_pred>200,mergerLabel:="Strong"]

## Label Categories
prod_pred = merge(prod_pred,hhi[,c("Market","dhhi_pred","mergerLabel")],by="Market")
prod_pred = merge(prod_pred,firms[,c("Market","Firm","merger")],by=c("Market","Firm"))





### Premium Table
table_prem = prod_pred[merger!="None",list(prem_base = 12/1000*sum(s_base*Age_Avg*prem_base)/sum(s_base),
                             prem_RA = 12/1000*sum(s_RA*Age_Avg*prem_RA)/sum(s_RA),
                             prem_man = 12/1000*sum(s_man*Age_Avg*prem_man)/sum(s_man),
                             prem_base_m = 12/1000*sum(s_base*Age_Avg*prem_base_m)/sum(s_base),
                             prem_RA_m = 12/1000*sum(s_RA*Age_Avg*prem_RA_m)/sum(s_RA),
                             prem_man_m = 12/1000*sum(s_man*Age_Avg*prem_man_m)/sum(s_man)),by=c("Metal_std")]


table_prem[,Metal_std:=factor(Metal_std,levels=c("CATASTROPHIC","BRONZE","SILVER","GOLD","PLATINUM"))]
setkey(table_prem,Metal_std)


table_prem[,base_effect:=round(100*(prem_base_m-prem_base)/prem_base,1)]
table_prem[,RA_effect:=  round(100*(prem_RA_m-prem_RA)/prem_RA,1)]
table_prem[,man_effect:= round(100*(prem_man_m-prem_man)/prem_man,1)]

table_prem[,Group:="Merging Parties"]

table_prem_all = prod_pred[,list(prem_base = 12/1000*sum(s_base*Age_Avg*prem_base)/sum(s_base),
                                           prem_RA = 12/1000*sum(s_RA*Age_Avg*prem_RA)/sum(s_RA),
                                           prem_man = 12/1000*sum(s_man*Age_Avg*prem_man)/sum(s_man),
                                           prem_base_m = 12/1000*sum(s_base*Age_Avg*prem_base_m)/sum(s_base),
                                           prem_RA_m = 12/1000*sum(s_RA*Age_Avg*prem_RA_m)/sum(s_RA),
                                           prem_man_m = 12/1000*sum(s_man*Age_Avg*prem_man_m)/sum(s_man)),by=c("Metal_std")]


table_prem_all[,Metal_std:=factor(Metal_std,levels=c("CATASTROPHIC","BRONZE","SILVER","GOLD","PLATINUM"))]
setkey(table_prem_all,Metal_std)


table_prem_all[,base_effect:=round(100*(prem_base_m-prem_base)/prem_base,1)]
table_prem_all[,RA_effect:=  round(100*(prem_RA_m-prem_RA)/prem_RA,1)]
table_prem_all[,man_effect:= round(100*(prem_man_m-prem_man)/prem_man,1)]
table_prem_all[,Group:="All Firms"]

table_prem = rbind(table_prem[,c("Metal_std","Group","base_effect","RA_effect","man_effect")],
                   table_prem_all[,c("Metal_std","Group","base_effect","RA_effect","man_effect")])




