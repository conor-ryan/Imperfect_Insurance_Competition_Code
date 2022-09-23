rm(list = ls())
library(data.table)
library(ggplot2)
library(scales)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

run = "2022-03-18"
spec = "FMC"
policy = "Base"

filestub = paste("Estimation_Output/AllMergers_",spec,"-",run,"_",policy,"_",sep="")

prodData = as.data.table(read.csv("Intermediate_Output/Equilibrium_Data/estimated_prodData_full.csv"))
prodData = prodData[,c("ST","Firm","Product_std","Metal_std","Market")]
names(prodData) = c("ST","Firm","Product","Metal_std","Market")

load("Intermediate_Output/Simulated_BaseData/simMarketSize.rData")
prodData = merge(prodData,marketSize,by="Market")



### Baseline Market Data ####
baseline = fread(paste(filestub,"baseline.csv",sep=""))
baseline = merge(baseline,prodData,by="Product")
baseline[,insideShare:=Lives/sum(Lives),by="Market"]

## Create HHI baseline data
firm_share = baseline[,list(share=sum(insideShare*100)),by=c("Market","ST","Firm")]
firm_share[,count:=1]
hhi = firm_share[,list(hhi=sum((share)^2)),by=c("Market","ST")]
hhi[,markets:=as.numeric(as.factor(Market))]

## Baseline welfare data
base_welfare = fread(paste("Estimation_Output/totalWelfare_bymkt_AllMergers_",spec,"-",run,"_",policy,"_baseline-",spec,"-",run,".csv",sep=""))

#### Iterate Through Mergers ####
merger_files = list.files("Estimation_Output",pattern=paste("totalWelfare_bymkt_AllMergers_",spec,"-",run,"_",policy,sep=""))
unique_firms = sort(firm_share[,unique(Firm)])

merger_welfare = NULL
for (i in 1:length(unique_firms)){
  for (j in 1:(i-1)){
    if (j==0){next}
    m1 = unique_firms[j]
    m2 = unique_firms[i]
    ## Read in Welfare File
    merging_party_string = paste(policy,"_",m1,"_",m2,"-",spec,sep="")
    file = merger_files[grepl(merging_party_string,merger_files)]
    if (length(file)==0){next}
    welfare = fread(paste("Estimation_Output/",file,sep=""))
    names(welfare)[2:length(names(welfare))] = paste(names(welfare)[2:length(names(welfare))],"merge",sep="_")
    
    # Merge in HHI and Baseline Data
    welfare = merge(hhi,welfare,by="markets")
    welfare = merge(base_welfare,welfare,by="markets")
    
    #Compute Change variables
    welfare[,chg_CW:=CW_merge-CW]
    welfare[,chg_Profit:=Profit_merge-Profit]
    welfare[,chg_Spending:=Spending_merge-Spending]
    welfare[,chg_RA_transfers:=RA_transfers_merge-RA_transfers]
    welfare[,chg_Insured:=Insured_merge-Insured]
    
    merge_vars = names(welfare)[grepl("merge",names(welfare))]
    welfare[,c(merge_vars):=NULL]
    
    welfare[,merging_parties:= paste(unique_firms[j],unique_firms[i],sep="-")]
    
    #
    dHHI = firm_share[Firm%in%c(m1,m2),list(dHHI=2*prod(share),merger=sum(count)),by="Market"]
    dHHI[merger<2,dHHI:=0]
    
    welfare = merge(welfare,dHHI[,c("Market","dHHI")],by="Market",all.x=TRUE)
    welfare[is.na(dHHI),dHHI:=0]
    welfare = welfare[dHHI>0]
    merger_welfare = rbind(merger_welfare,welfare)
  }
}



