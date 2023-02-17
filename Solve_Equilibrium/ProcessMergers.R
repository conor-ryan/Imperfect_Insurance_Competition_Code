rm(list = ls())
library(data.table)
library(ggplot2)
library(scales)
setwd("C:/Users/cxr5626/Dropbox/Research/Imperfect_Insurance_Competition/")

run = "2022-12-26"
spec = "FMC"

### Base Data 
prodData = as.data.table(read.csv("Intermediate_Output/Equilibrium_Data/estimated_prodData_full.csv"))
prodData = prodData[,c("ST","Firm","Product_std","Metal_std","Market")]
names(prodData) = c("ST","Firm","Product","Metal_std","Market")

load("Intermediate_Output/Simulated_BaseData/simMarketSize.rData")
prodData = merge(prodData,marketSize,by="Market")


# policy = "RAMan"
# filestub = paste("Estimation_Output/AllMergers_",spec,"-",run,"_",policy,"_",sep="")
# 
# ### Baseline Market Data ####
# test2 = fread(paste(filestub,"baseline.csv",sep=""))
# test2 = merge(test2,prodData,by="Product")
# 

margins = fread("Estimation_Output/AllMergers_FMC-2022-12-26_Base_Marigins.csv")
ggplot(margins) + aes(x=MR,y=MC_RA) + geom_point() + geom_abline() + geom_smooth(method="lm")
margins[,omega:=MR-MC_RA]
margins[,error_ratio:=omega/MC_RA]

#### Testing ####
#### Baseline Data
basedata = NULL
for (policy in c("Base","RA","Man","RAMan")){
  print(policy)
  if (policy=="PL"){
    spec_temp = paste("PL","FMC",sep="_")
    policy_temp="Base"
  }else{
    spec_temp = spec
    policy_temp = policy
  }
  filestub = paste("Estimation_Output/AllMergers_",spec_temp,"-",run,"_",policy_temp,"_",sep="")
  
  ### Baseline Market Data ####
  baseline = fread(paste(filestub,"baseline.csv",sep=""))
  baseline = merge(baseline,prodData,by="Product")
  baseline[,insideShare:=Lives/sum(Lives),by="Market"]
  

  
  ### Baseline Market Data ####
  filestub = paste("Estimation_Output/AllMergers_",spec_temp,"-",run,"_",policy_temp,"_SP_cp_",sep="")
  baseline_cp = fread(paste(filestub,"baseline.csv",sep=""))
  names(baseline_cp)[2:length(baseline_cp)] = paste(names(baseline_cp)[2:length(baseline_cp)],"CP",sep="_")
  baseline = merge(baseline,baseline_cp,by="Product")
  
  ### Baseline Market Data ####
  filestub = paste("Estimation_Output/AllMergers_",spec_temp,"-",run,"_",policy_temp,"_SP_",sep="")
  baseline_sp = fread(paste(filestub,"baseline.csv",sep=""))
  names(baseline_sp)[2:length(baseline_sp)] = paste(names(baseline_sp)[2:length(baseline_sp)],"SP",sep="_")
  baseline = merge(baseline,baseline_sp,by="Product")
  
  ## Create HHI baseline data
  firm_share = baseline[,list(share=sum(insideShare*100)),by=c("Market","ST","Firm")]
  firm_share[,count:=1]
  hhi = firm_share[,list(hhi=sum((share)^2),firm_num=sum(count)),by=c("Market","ST")]
  hhi[,markets:=as.numeric(as.factor(Market))]
  
  hhi[,hhi_category:=0]
  hhi[hhi>=3500,hhi_category:=1]
  # hhi[hhi>=4000,hhi_category:=2] "2850 < HHI < 4000",
  hhi[,hhi_category:=factor(hhi_category,levels=c(0,1),
                            labels=c("HHI < 3500","HHI > 3500"))]
  baseline = merge(baseline,hhi,by=c("Market"))
  baseline = baseline[grepl("GA",Market)]
  baseline[,policy:=policy]
  print(baseline[,mean(Price),by="Metal_std"])
  basedata = rbind(basedata,baseline)
  rm(baseline,hhi)
}


ggplot(basedata[policy=="Base"])+ 
  geom_point(aes(x=Price,y=Price_CP),color="blue") +
  geom_point(aes(x=Price,y=Price_SP)) + geom_abline()

ggplot(basedata[policy=="RA"])+ 
  geom_point(aes(x=Price,y=Price_CP),color="blue") +
  geom_point(aes(x=Price,y=Price_SP)) + geom_abline()

ggplot(basedata[policy=="Man"])+ 
  geom_point(aes(x=Price,y=Price_CP),color="blue") +
  geom_point(aes(x=Price,y=Price_SP)) + geom_abline()

ggplot(basedata[policy=="RAMan"])+ 
  geom_point(aes(x=Price,y=Price_CP),color="blue") +
  geom_point(aes(x=Price,y=Price_SP)) + geom_abline()



base_welfare = NULL
for (policy in c("Base","RA","Man","RAMan")){
  print(policy)
  if (policy=="PL"){
    spec_temp = paste("PL","FMC",sep="_")
    policy_temp="Base"
  }else{
    spec_temp = spec
    policy_temp = policy
  }
  baseline_CP = fread(paste("Estimation_Output/totalWelfare_bymkt_AllMergers_",spec_temp,"-",run,"_",policy_temp,"_SP_cp_baseline-",spec,"-",run,".csv",sep=""))
  baseline_Comp = fread(paste("Estimation_Output/totalWelfare_bymkt_AllMergers_",spec_temp,"-",run,"_",policy_temp,"_baseline-",spec,"-",run,".csv",sep=""))
  baseline_SP = fread(paste("Estimation_Output/totalWelfare_bymkt_AllMergers_",spec_temp,"-",run,"_",policy_temp,"_SP_baseline-",spec,"-",run,".csv",sep=""))
  
  
  
  baseline_CP[,Welfare_baseline_CP:=CW+Profit]
  baseline_Comp[,Welfare_baseline_Comp:=CW+Profit]
  baseline_SP[,Welfare_baseline_SP:=CW+Profit]
  
  welfare = merge(baseline_CP[,c("markets","Welfare_baseline_CP")],baseline_Comp[,c("markets","Welfare_baseline_Comp","Profit","RA_transfers","Population","Insured")],by="markets")
  welfare = merge(welfare,baseline_SP[,c("markets","Welfare_baseline_SP")],by="markets")
  welfare[,sorting_cost:=Welfare_baseline_CP-Welfare_baseline_Comp]
  welfare[,policy:=policy]
  base_welfare = rbind(base_welfare,welfare)
}
base_welfare = base_welfare[markets%in%basedata$markets]
setkey(base_welfare,markets,policy)

test = base_welfare[,list(SP=sum(Welfare_baseline_SP*Population)/sum(Population),
                   CP=sum(Welfare_baseline_CP*Population)/sum(Population),
                   Comp = sum(Welfare_baseline_Comp*Population)/sum(Population),
                   Transfers = sum(RA_transfers),
                   Profit = sum(Profit*Population)/sum(Population)),by="policy"]
test[,markup:=SP-CP]
test[,sorting:=CP-Comp]

### Merger Price-Effect Data 
merger_effects = NULL
for (policy in c("Base","RA","Man","RAMan")){
  print(policy)
  if (policy=="PL"){
    spec_temp = paste("PL","FMC",sep="_")
    policy_temp="Base"
  }else{
    spec_temp = spec
    policy_temp = policy
  }
  filestub = paste("Estimation_Output/AllMergers_",spec_temp,"-",run,"_",policy_temp,"_",sep="")
  
  ### Baseline Market Data ####
  baseline = fread(paste(filestub,"baseline.csv",sep=""))
  baseline = merge(baseline,prodData,by="Product")
  baseline[,insideShare:=Lives/sum(Lives),by="Market"]
  
  print(baseline[,mean(Price),by="Metal_std"])
  
  ## Create HHI baseline data
  firm_share = baseline[,list(share=sum(insideShare*100)),by=c("Market","ST","Firm")]
  firm_share[,count:=1]
  hhi = firm_share[,list(hhi=sum((share)^2),firm_num=sum(count)),by=c("Market","ST")]
  hhi[,markets:=as.numeric(as.factor(Market))]
  
  hhi[,hhi_category:=0]
  hhi[hhi>=3500,hhi_category:=1]
  # hhi[hhi>=4000,hhi_category:=2] "2850 < HHI < 4000",
  hhi[,hhi_category:=factor(hhi_category,levels=c(0,1),
                            labels=c("HHI < 3500","HHI > 3500"))]
  
  #### Iterate Through Mergers ####
  merger_files = list.files("Estimation_Output",pattern=paste("^AllMergers_",spec_temp,"-",run,"_",policy_temp,sep=""))
  merger_files_pre_eq = list.files("Estimation_Output",pattern=paste("^AllMergers_",spec_temp,"-",run,"_",policy_temp,sep=""))
  unique_firms = sort(firm_share[,unique(Firm)])
  
  for (i in 1:length(unique_firms)){
    for (j in 1:(i-1)){
      if (j==0){next}
      m1 = unique_firms[j]
      m2 = unique_firms[i]
      ## Read in File
      merging_party_string = paste(policy_temp,"_",m1,"_",m2,".csv",sep="")
      file = merger_files[grepl(merging_party_string,merger_files)]
      if (length(file)==0){next}
      postmerge = fread(paste("Estimation_Output/",file,sep=""))
      names(postmerge)[2:length(names(postmerge))] = paste(names(postmerge)[2:length(names(postmerge))],"merge",sep="_")
    
      # merging_party_pre_string = paste(policy_temp,"_PRE_",m1,"_",m2,".csv",sep="")
      # file = merger_files[grepl(merging_party_pre_string,merger_files)]
      # premerge = fread(paste("Estimation_Output/",file,sep=""))
      # names(premerge)[2:length(names(premerge))] = paste(names(premerge)[2:length(names(premerge))],"premerge",sep="_")
      #   
      # Merge in HHI and Baseline Data
      postmerge = merge(baseline,postmerge,by="Product")
      # postmerge = merge(postmerge,premerge,by="Product")
      postmerge = merge(hhi,postmerge,by=c("Market","ST"))
      
      #Compute Change variables
      postmerge[,Price_Effect:=Price_merge-Price]
      postmerge[,Price_Effect_percent:=(Price_merge-Price)/Price]
      
      postmerge[,merging_parties:= paste(unique_firms[j],unique_firms[i],sep="-")]
      postmerge[,policy:=policy]
      
      #
      dHHI = firm_share[Firm%in%c(m1,m2),list(dHHI=2*prod(share),merger=sum(count)),by="Market"]
      dHHI[merger<2,dHHI:=0]
      
      postmerge = merge(postmerge,dHHI[,c("Market","dHHI")],by="Market",all.x=TRUE)
      postmerge[is.na(dHHI),dHHI:=0]
      postmerge = postmerge[dHHI>0]
      
      postmerge[Firm%in%c(m1,m2),Firm:=merging_parties]
      postmerge[,parties_indicator:=as.numeric(Firm==merging_parties)]
      postmerge[,pre_profit_firm:=sum(Profit),by=c("Firm","ST")]
      postmerge[,post_profit_firm:=sum(Profit_merge),by=c("Firm","ST")]
      
      merger_effects = rbind(merger_effects,postmerge)
    }
  }
  rm(postmerge,hhi,baseline,firm_share,dHHI,merger_files)
}

## Equilibrium Stability
merger_effects = merger_effects[grepl("GA",Market)]
ggplot(merger_effects) + aes(x=Price,y=Price_premerge) + geom_point(alpha=0.5) + geom_smooth(method="lm",se=FALSE) + geom_abline(intercept=0,slope=1)

merger_effects[,profit_effect:=post_profit_firm-pre_profit_firm]

## Merger Welfare Effects
merger_welfare = NULL
for (policy in c("Base","Man","RA","RAMan")){
  print(policy)
  if (policy=="PL"){
    spec_temp = paste("PL","FMC",sep="_")
    policy_temp="Base"
  }else{
    spec_temp = spec
    policy_temp = policy
  }
  filestub = paste("Estimation_Output/AllMergers_",spec_temp,"-",run,"_",policy_temp,"_",sep="")
  
  ### Baseline Market Data ####
  baseline = fread(paste(filestub,"baseline.csv",sep=""))
  baseline = merge(baseline,prodData,by="Product")
  baseline[,insideShare:=Lives/sum(Lives),by="Market"]
  
  ## Create HHI baseline data
  firm_share = baseline[,list(share=sum(insideShare*100)),by=c("Market","ST","Firm")]
  firm_share[,count:=1]
  hhi = firm_share[,list(hhi=sum((share)^2),firm_num=sum(count)),by=c("Market","ST")]
  hhi[,markets:=as.numeric(as.factor(Market))]
  
  hhi[,hhi_category:=0]
  hhi[hhi>=3500,hhi_category:=1]
  # hhi[hhi>=4000,hhi_category:=2] "2850 < HHI < 4000",
  hhi[,hhi_category:=factor(hhi_category,levels=c(0,1),
                            labels=c("HHI < 3500","HHI > 3500"))]
  
  ## Baseline welfare data
  base_welfare = fread(paste("Estimation_Output/totalWelfare_bymkt_AllMergers_",spec_temp,"-",run,"_",policy_temp,"_baseline-",spec,"-",run,".csv",sep=""))
  
  #### Iterate Through Mergers ####
  merger_files = list.files("Estimation_Output",pattern=paste("totalWelfare_bymkt_AllMergers_",spec_temp,"-",run,"_",policy_temp,sep=""))
  unique_firms = sort(firm_share[,unique(Firm)])
  
  
  for (i in 1:length(unique_firms)){
    for (j in 1:(i-1)){
      if (j==0){next}
      m1 = unique_firms[j]
      m2 = unique_firms[i]
      ## Read in Welfare File
      merging_party_string = paste(policy_temp,"_",m1,"_",m2,"-",spec,sep="")
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
      welfare[,policy:=policy]
      
      #
      dHHI = firm_share[Firm%in%c(m1,m2),list(dHHI=2*prod(share),merger=sum(count)),by="Market"]
      dHHI[merger<2,dHHI:=0]
      
      welfare = merge(welfare,dHHI[,c("Market","dHHI")],by="Market",all.x=TRUE)
      welfare[is.na(dHHI),dHHI:=0]
      # welfare = welfare[dHHI>100]
      merger_welfare = rbind(merger_welfare,welfare)
    }
  }
  rm(welfare,hhi,base_welfare,baseline,firm_share,dHHI)
}
merger_welfare = merger_welfare[grepl("GA",Market)]

test = merger_welfare[,list(chg_CW=sum(chg_CW*Population)/sum(Population),
                          chg_Profit=sum(chg_Profit*Population)/sum(Population),
                          chg_Spending = sum(chg_Spending*Population)/sum(Population),
                          chg_Insured = sum(chg_Insured),
                          chg_RA = sum(chg_RA_transfers)),by="policy"]

test[,chg_totWelfare:=chg_CW+chg_Profit]

ggplot(merger_effects[policy=="RAMan"&parties_indicator==1]) + aes(x=Price,y=Price_merge) + geom_abline() + geom_point()







#### Welfare By Market Concentration ####
conc_welfare = NULL
for (policy in c("Base","RAMan","Man","RA")){
  if (policy=="PL"){
    spec_temp = paste("PL","FMC",sep="_")
    policy_temp="Base"
  }else{
    spec_temp = spec
    policy_temp = policy
  }
  print(policy)
  filestub = paste("Estimation_Output/AllMergers_",spec_temp,"-",run,"_",policy_temp,"_",sep="")
  
  ### Baseline Market Data ####
  baseline = fread(paste(filestub,"baseline.csv",sep=""))
  baseline = merge(baseline,prodData,by="Product")
  baseline[,insideShare:=Lives/sum(Lives),by="Market"]
  
  ## Create HHI baseline data
  firm_share = baseline[,list(share=sum(insideShare*100)),by=c("Market","ST","Firm")]
  firm_share[,count:=1]
  firm_share[,markets:=as.numeric(as.factor(Market))]
  hhi = firm_share[,list(hhi=sum((share)^2),firm_num=sum(count)),by=c("markets","Market","ST")]

  
  ## Baseline welfare data
  base_welfare = fread(paste("Estimation_Output/totalWelfare_bymkt_AllMergers_",spec_temp,"-",run,"_",policy_temp,"_baseline-",spec,"-",run,".csv",sep=""))
  base_welfare[,tot_Welfare:=CW+Profit]
  base_welfare[,tot_Welfare_gov:=CW+Profit+Spending]
  
  conc_base = merge(base_welfare[,c("markets","tot_Welfare","tot_Welfare_gov")],hhi[,c("Market","markets","hhi","firm_num")],by="markets")
  conc_base[,dHHI:=0]
  conc_base[,merging_parties:="baseline"]
  conc_base[,policy:=policy]
  conc_welfare = rbind(conc_welfare,conc_base)
  
  #### Iterate Through Mergers ####
  merger_welfare_files = list.files("Estimation_Output",pattern=paste("totalWelfare_bymkt_AllMergers_",spec_temp,"-",run,"_",policy_temp,sep=""))
  merger_price_files = list.files("Estimation_Output",pattern=paste("^AllMergers_",spec_temp,"-",run,"_",policy_temp,sep=""))
  unique_firms = sort(firm_share[,unique(Firm)])
  
  
  for (i in 1:length(unique_firms)){
    for (j in 1:(i-1)){
      if (j==0){next}
      m1 = unique_firms[j]
      m2 = unique_firms[i]
      ## Read in Welfare File
      merging_party_string = paste(policy_temp,"_",m1,"_",m2,"-",spec,sep="")
      welfare_file = merger_welfare_files[grepl(merging_party_string,merger_welfare_files)]
      merging_party_string = paste(policy_temp,"_",m1,"_",m2,".csv",sep="")
      price_file = merger_price_files[grepl(merging_party_string,merger_price_files)]
      if (length(welfare_file)==0){next}
      ## Welfare
      welfare = fread(paste("Estimation_Output/",welfare_file,sep=""))
      welfare[,tot_Welfare:=CW+Profit]
      welfare[,tot_Welfare_gov:=CW+Profit+Spending]
      
      dHHI = firm_share[Firm%in%c(m1,m2),list(dHHI=2*prod(share),merger=sum(count)),by="markets"]
      dHHI[merger<2,dHHI:=0]
      
      welfare = merge(welfare,dHHI[,c("markets","dHHI")],by="markets",all.x=TRUE)
      welfare[is.na(dHHI),dHHI:=0]
      welfare = welfare[dHHI>100]
      
      
      ## Post-Merger HHI
      equi = fread(paste("Estimation_Output/",price_file,sep=""))
      equi = merge(equi,prodData,by="Product")
      equi[,insideShare:=Lives/sum(Lives),by="Market"]
      equi[Firm%in%c(m1,m2),Firm:=m1]
      
      ## Create HHI baseline data
      temp_share = equi[,list(share=sum(insideShare*100)),by=c("Market","ST","Firm")]
      temp_share[,count:=1]
      temp_hhi = temp_share[,list(hhi=sum((share)^2),firm_num=sum(count)),by=c("Market","ST")]
      temp_hhi[,markets:=as.numeric(as.factor(Market))]
      
      temp= merge(welfare[,c("markets","tot_Welfare","tot_Welfare_gov","dHHI")],temp_hhi[,c("Market","markets","hhi","firm_num")],by="markets",all.x=TRUE)
      temp[,merging_parties:=paste(unique_firms[j],unique_firms[i],sep="-")]
      temp[,policy:=policy]
      
      
      conc_welfare = rbind(conc_welfare,temp)
      rm(temp,temp_hhi,temp_share,equi,welfare,dHHI)
    }
  }
  rm(welfare,hhi,base_welfare,baseline,firm_share,dHHI)
}


### Regression Analysis

conc_welfare[,hhi_bucket:=floor(hhi/500)*500]
conc_welfare[hhi>9000,hhi_bucket:=9000]
conc_welfare[,hhi_bucket:=as.factor(hhi_bucket)]
conc_welfare[,hhi_2:=hhi^2]
conc_welfare[,hhi_3:=hhi^3]
conc_welfare[,firmFactor:=as.factor(firm_num)]

conc_welfare[policy=="RAMan"&(dHHI>100|merging_parties=="baseline"),summary(lm(tot_Welfare~firmFactor+Market))]
conc_welfare[policy=="Base"&(dHHI>100|merging_parties=="baseline"),summary(lm(tot_Welfare~firmFactor+Market))]
conc_welfare[policy=="RA"&(dHHI>100|merging_parties=="baseline"),summary(lm(tot_Welfare~firmFactor+Market))]
conc_welfare[policy=="Man"&(dHHI>100|merging_parties=="baseline"),summary(lm(tot_Welfare~firmFactor+Market))]
# conc_welfare[policy=="PL"&(dHHI>100|merging_parties=="baseline"),summary(lm(tot_Welfare~firmFactor+Market))]

#### Merger Welfare Data ####
merger_welfare = NULL
for (policy in c("Base","RA","RAMan")){
  print(policy)
  if (policy=="PL"){
    spec_temp = paste("PL","FMC",sep="_")
    policy_temp="Base"
  }else{
    spec_temp = spec
    policy_temp = policy
  }
  filestub = paste("Estimation_Output/AllMergers_",spec_temp,"-",run,"_",policy_temp,"_",sep="")
  
  ### Baseline Market Data ####
  baseline = fread(paste(filestub,"baseline.csv",sep=""))
  baseline = merge(baseline,prodData,by="Product")
  baseline[,insideShare:=Lives/sum(Lives),by="Market"]
  
  ## Create HHI baseline data
  firm_share = baseline[,list(share=sum(insideShare*100)),by=c("Market","ST","Firm")]
  firm_share[,count:=1]
  hhi = firm_share[,list(hhi=sum((share)^2),firm_num=sum(count)),by=c("Market","ST")]
  hhi[,markets:=as.numeric(as.factor(Market))]
  
  hhi[,hhi_category:=0]
  hhi[hhi>=3500,hhi_category:=1]
  # hhi[hhi>=4000,hhi_category:=2] "2850 < HHI < 4000",
  hhi[,hhi_category:=factor(hhi_category,levels=c(0,1),
                            labels=c("HHI < 3500","HHI > 3500"))]
  
  ## Baseline welfare data
  base_welfare = fread(paste("Estimation_Output/totalWelfare_bymkt_AllMergers_",spec_temp,"-",run,"_",policy_temp,"_baseline-",spec,"-",run,".csv",sep=""))
  
  #### Iterate Through Mergers ####
  merger_files = list.files("Estimation_Output",pattern=paste("totalWelfare_bymkt_AllMergers_",spec_temp,"-",run,"_",policy_temp,sep=""))
  unique_firms = sort(firm_share[,unique(Firm)])
  
  
  for (i in 1:length(unique_firms)){
    for (j in 1:(i-1)){
      if (j==0){next}
      m1 = unique_firms[j]
      m2 = unique_firms[i]
      ## Read in Welfare File
      merging_party_string = paste(policy_temp,"_",m1,"_",m2,"-",spec,sep="")
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
      welfare[,policy:=policy]
      
      #
      dHHI = firm_share[Firm%in%c(m1,m2),list(dHHI=2*prod(share),merger=sum(count)),by="Market"]
      dHHI[merger<2,dHHI:=0]
      
      welfare = merge(welfare,dHHI[,c("Market","dHHI")],by="Market",all.x=TRUE)
      welfare[is.na(dHHI),dHHI:=0]
      # welfare = welfare[dHHI>100]
      merger_welfare = rbind(merger_welfare,welfare)
    }
  }
  rm(welfare,hhi,base_welfare,baseline,firm_share,dHHI)
}

mw_state = merger_welfare[,list(chg_CW=sum(Population*chg_CW)/sum(Population),
                                chg_Profit=sum(Population*chg_Profit)/sum(Population),
                                chg_Spending=sum(Population*chg_Spending)/sum(Population),
                                chg_RA_transfers = sum(chg_RA_transfers),
                                chg_Insured = sum(chg_Insured),
                                dHHI = sum(dHHI*Population)/sum(Population)),
                          by=c("ST","merging_parties","policy")]
mw_state = mw_state[dHHI>0]

#### Market Structure Description #####
## Firms
# merger_welfare[policy=="Base",quantile(firm_num,probs=c(.1,.25,.5,.75,.9))]
## Pre-HHI
# merger_welfare[policy=="Man",quantile(hhi,probs=c(.1,.25,.5,.75,.9))]
## Post-HHI
# merger_welfare[policy=="Base",quantiles(firm_num,probs=c(.1,.25,.5,.75,.9))]
## Delta HHI
# merger_welfare[policy=="Base",quantile(dHHI,probs=c(.1,.25,.5,.75,.9,.99))]


#### Welfare Effect Merger Plot #####
merger_welfare[,chg_Tot_Welfare:=chg_CW+chg_Profit]
mw_state[,chg_Tot_Welfare:=chg_CW+chg_Profit]
merger_welfare[,chg_Tot_Welfaregov:=chg_CW+chg_Profit+chg_Spending]


plotdf1 = merger_welfare[,c("dHHI","chg_CW","policy")]
names(plotdf1)[2] = "value"
plotdf1[,label:="Consumer Welfare"]
plotdf1[,chg_CW:=NA]
plotdf2 = merger_welfare[,c("dHHI","chg_Tot_Welfare","chg_CW","policy")]
names(plotdf2)[2] = "value"
plotdf2[,label:="Total Welfare"]
plotdf = rbind(plotdf1,plotdf2)


ggplot(merger_welfare[policy=="RA"]) + aes(x=dHHI,y=chg_Tot_Welfare) + 
  geom_point()


png("Writing/Images/Base_WelfareEffect.png",width=2500,height=1500,res=275)
ggplot(plotdf[policy=="Base"])+
  geom_point(aes(x=dHHI,y=value,color=label,shape=label),size=2.5)+
  # geom_point(aes(x=dHHI,y=chg_Tot_Welfare,color="Total Welfare",shape ="Total Welfare"),size=2) +
  scale_shape_manual(values=c(16,17)) +
  geom_errorbar(aes(x=dHHI,ymin=chg_CW,ymax=value)) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  # facet_wrap(~hhi_category,ncol=1) +
  geom_abline(slope=0,intercept=0) +
  xlab("Predicted Change in HHI")+
  ylab("Dollars Per-Person Per-Month")+
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    legend.background = element_rect(color=grey(.5)),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "bottom",
    axis.title=element_text(size=14),
    axis.text = element_text(size=16))
dev.off()


png("Writing/Images/RAMan_WelfareEffect.png",width=2500,height=1500,res=275)
ggplot(plotdf[policy=="RAMan"])+ 
  geom_point(aes(x=dHHI,y=value,color=label,shape=label),size=2.5)+
  scale_shape_manual(values=c(16,17)) +
  geom_errorbar(aes(x=dHHI,ymin=chg_CW,ymax=value)) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  geom_abline(slope=0,intercept=0) +
  xlab("Predicted Change in HHI")+
  ylab("Dollars Per-Person Per-Month")+
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    legend.background = element_rect(color=grey(.5)),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "bottom",
    axis.title=element_text(size=14),
    axis.text = element_text(size=16))
dev.off()

ggplot(plotdf[policy=="Man"])+ 
  geom_point(aes(x=dHHI,y=value,color=label,shape=label),size=2.5)+
  scale_shape_manual(values=c(16,17)) +
  geom_errorbar(aes(x=dHHI,ymin=chg_CW,ymax=value)) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  geom_abline(slope=0,intercept=0) +
  xlab("Predicted Change in HHI")+
  ylab("Dollars Per-Person Per-Month")+
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    legend.background = element_rect(color=grey(.5)),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "bottom",
    axis.title=element_text(size=14),
    axis.text = element_text(size=16))

ggplot(plotdf[policy=="RA"])+ 
  geom_point(aes(x=dHHI,y=value,color=label,shape=label),size=2.5)+
  scale_shape_manual(values=c(16,17)) +
  geom_errorbar(aes(x=dHHI,ymin=chg_CW,ymax=value)) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  geom_abline(slope=0,intercept=0) +
  xlab("Predicted Change in HHI")+
  ylab("Dollars Per-Person Per-Month")+
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    legend.background = element_rect(color=grey(.5)),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "bottom",
    axis.title=element_text(size=14),
    axis.text = element_text(size=16))

ggplot(plotdf[policy=="PL"])+ 
  geom_point(aes(x=dHHI,y=value,color=label,shape=label),size=2.5)+
  scale_shape_manual(values=c(16,17)) +
  geom_errorbar(aes(x=dHHI,ymin=chg_CW,ymax=value)) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  geom_abline(slope=0,intercept=0) +
  xlab("Predicted Change in HHI")+
  ylab("Dollars Per-Person Per-Month")+
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    legend.background = element_rect(color=grey(.5)),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "bottom",
    axis.title=element_text(size=14),
    axis.text = element_text(size=16))

#### Merger Price-Effect Data ####
merger_effects = NULL
for (policy in c("Man","RAMan")){
  print(policy)
  if (policy=="PL"){
    spec_temp = paste("PL","FMC",sep="_")
    policy_temp="Base"
  }else{
    spec_temp = spec
    policy_temp = policy
  }
  filestub = paste("Estimation_Output/AllMergers_",spec_temp,"-",run,"_",policy_temp,"_",sep="")
  
  ### Baseline Market Data ####
  baseline = fread(paste(filestub,"baseline.csv",sep=""))
  baseline = merge(baseline,prodData,by="Product")
  baseline[,insideShare:=Lives/sum(Lives),by="Market"]
  
  print(baseline[,mean(Price),by="Metal_std"])
  
  ## Create HHI baseline data
  firm_share = baseline[,list(share=sum(insideShare*100)),by=c("Market","ST","Firm")]
  firm_share[,count:=1]
  hhi = firm_share[,list(hhi=sum((share)^2),firm_num=sum(count)),by=c("Market","ST")]
  hhi[,markets:=as.numeric(as.factor(Market))]
  
  hhi[,hhi_category:=0]
  hhi[hhi>=3500,hhi_category:=1]
  # hhi[hhi>=4000,hhi_category:=2] "2850 < HHI < 4000",
  hhi[,hhi_category:=factor(hhi_category,levels=c(0,1),
                            labels=c("HHI < 3500","HHI > 3500"))]
  
  #### Iterate Through Mergers ####
  merger_files = list.files("Estimation_Output",pattern=paste("^AllMergers_",spec_temp,"-",run,"_",policy_temp,sep=""))
  unique_firms = sort(firm_share[,unique(Firm)])
  
  
  for (i in 1:length(unique_firms)){
    for (j in 1:(i-1)){
      if (j==0){next}
      m1 = unique_firms[j]
      m2 = unique_firms[i]
      ## Read in Welfare File
      merging_party_string = paste(policy_temp,"_",m1,"_",m2,".csv",sep="")
      file = merger_files[grepl(merging_party_string,merger_files)]
      if (length(file)==0){next}
      postmerge = fread(paste("Estimation_Output/",file,sep=""))
      names(postmerge)[2:length(names(postmerge))] = paste(names(postmerge)[2:length(names(postmerge))],"merge",sep="_")
      
      # Merge in HHI and Baseline Data
      postmerge = merge(baseline,postmerge,by="Product")
      postmerge = merge(hhi,postmerge,by=c("Market","ST"))
      
      #Compute Change variables
      postmerge[,Price_Effect:=Price_merge-Price]
      postmerge[,Price_Effect_percent:=(Price_merge-Price)/Price]
      
      postmerge[,merging_parties:= paste(unique_firms[j],unique_firms[i],sep="-")]
      postmerge[,policy:=policy]
      
      #
      dHHI = firm_share[Firm%in%c(m1,m2),list(dHHI=2*prod(share),merger=sum(count)),by="Market"]
      dHHI[merger<2,dHHI:=0]
      
      postmerge = merge(postmerge,dHHI[,c("Market","dHHI")],by="Market",all.x=TRUE)
      postmerge[is.na(dHHI),dHHI:=0]
      postmerge = postmerge[dHHI>0]
      
      postmerge[Firm%in%c(m1,m2),Firm:=merging_parties]
      postmerge[,parties_indicator:=as.numeric(Firm==merging_parties)]
      postmerge[,pre_profit_firm:=sum(Profit),by=c("Firm","Market")]
      postmerge[,post_profit_firm:=sum(Profit_merge),by=c("Firm","Market")]
      
      merger_effects = rbind(merger_effects,postmerge)
    }
  }
  rm(postmerge,hhi,baseline,firm_share,dHHI,merger_files)
}

merger_effects[,Metal_std:=factor(Metal_std,levels=c("CATASTROPHIC","BRONZE","SILVER","GOLD","PLATINUM"))]

ggplot(merger_effects[policy=="Base"]) + 
  aes(x=Metal_std,y=Price_Effect_percent) + 
  geom_boxplot(outlier.shape=NA) + 
  coord_cartesian(ylim=c(-.025,.125))  +
  ylab("Change in Monthly Premium")+
  xlab("")+
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    title=element_text(hjust=0,size=18),
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    legend.background = element_rect(color=grey(.5)),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "bottom",
    axis.title=element_text(size=14),
    axis.text = element_text(size=16))

png("Writing/Images/BasePriceEffect_HHHI1000.png",width=2500,height=1500,res=275)
ggplot(merger_effects[policy=="Base"&dHHI>1000]) + 
  aes(x=Metal_std,y=Price_Effect_percent) + 
  geom_abline(intercept=0,slope=0) + 
  geom_boxplot(outlier.shape=NA) + 
  coord_cartesian(ylim=c(-.25,.25))  +
  scale_y_continuous(label=percent) +
  ylab("Percent Change in Monthly Premium")+
  xlab("")+
  ggtitle("Price Effect With Selection Regulations") + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    title=element_text(hjust=0,size=16),
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    legend.background = element_rect(color=grey(.5)),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "bottom",
    axis.title=element_text(size=16),
    axis.text = element_text(size=16))
dev.off()

png("Writing/Images/RAManPriceEffect_HHHI1000.png",width=2500,height=1500,res=275)
ggplot(merger_effects[policy=="RAMan"&dHHI>1000]) + 
  aes(x=Metal_std,y=Price_Effect_percent) + 
  geom_abline(intercept=0,slope=0) + 
  geom_boxplot(outlier.shape=NA) + 
  coord_cartesian(ylim=c(-.25,.25))  +
  scale_y_continuous(label=percent) +
  ylab("Percent Change in Monthly Premium")+
  xlab("")+
  ggtitle("Price Effect Without Selection Regulations") + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    title=element_text(hjust=0,size=16),
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    legend.background = element_rect(color=grey(.5)),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "bottom",
    axis.title=element_text(size=16),
    axis.text = element_text(size=16))
dev.off()

ggplot(merger_effects[policy=="RA"&dHHI>1000]) + 
  aes(x=Metal_std,y=Price_Effect_percent) + 
  geom_abline(intercept=0,slope=0) + 
  geom_boxplot(outlier.shape=NA) + 
  coord_cartesian(ylim=c(-.25,.25))  +
  scale_y_continuous(label=percent) +
  ylab("Percent Change in Monthly Premium")+
  xlab("")+
  ggtitle("Price Effect Without Selection Regulations") + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    title=element_text(hjust=0,size=16),
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    legend.background = element_rect(color=grey(.5)),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "bottom",
    axis.title=element_text(size=16),
    axis.text = element_text(size=16))

ggplot(merger_effects[policy=="Man"&dHHI>1000]) + 
  aes(x=Metal_std,y=Price_Effect_percent) + 
  geom_abline(intercept=0,slope=0) + 
  geom_boxplot(outlier.shape=NA) + 
  coord_cartesian(ylim=c(-.25,.25))  +
  scale_y_continuous(label=percent) +
  ylab("Percent Change in Monthly Premium")+
  xlab("")+
  ggtitle("Price Effect Without Selection Regulations") + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    title=element_text(hjust=0,size=16),
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    legend.background = element_rect(color=grey(.5)),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "bottom",
    axis.title=element_text(size=16),
    axis.text = element_text(size=16))

ggplot(merger_effects[policy=="PL"&dHHI>1000]) + 
  aes(x=Metal_std,y=Price_Effect_percent) + 
  geom_abline(intercept=0,slope=0) + 
  geom_boxplot(outlier.shape=NA) + 
  coord_cartesian(ylim=c(-.25,.25))  +
  scale_y_continuous(label=percent) +
  ylab("Percent Change in Monthly Premium")+
  xlab("")+
  ggtitle("Price Effect Without Selection Regulations") + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    title=element_text(hjust=0,size=16),
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    legend.background = element_rect(color=grey(.5)),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "bottom",
    axis.title=element_text(size=16),
    axis.text = element_text(size=16))

##### Decomposition  Data ####
base_welfare = NULL
for (policy in c("Base","RAMan","Man","RA","PL")){
  print(policy)
  if (policy=="PL"){
    spec_temp = paste("PL","FMC",sep="_")
    policy_temp="Base"
  }else{
    spec_temp = spec
    policy_temp = policy
  }
  baseline_CP = fread(paste("Estimation_Output/totalWelfare_bymkt_AllMergers_",spec_temp,"-",run,"_",policy_temp,"_SP_cp_baseline-",spec,"-",run,".csv",sep=""))
  baseline_Comp = fread(paste("Estimation_Output/totalWelfare_bymkt_AllMergers_",spec_temp,"-",run,"_",policy_temp,"_baseline-",spec,"-",run,".csv",sep=""))
  baseline_SP = fread(paste("Estimation_Output/totalWelfare_bymkt_AllMergers_",spec_temp,"-",run,"_",policy_temp,"_SP_baseline-",spec,"-",run,".csv",sep=""))
  
  
  
  baseline_CP[,Welfare_baseline_CP:=CW+Profit]
  baseline_Comp[,Welfare_baseline_Comp:=CW+Profit]
  baseline_SP[,Welfare_baseline_SP:=CW+Profit]
  
  welfare = merge(baseline_CP[,c("markets","Welfare_baseline_CP")],baseline_Comp[,c("markets","Welfare_baseline_Comp")],by="markets")
  welfare = merge(welfare,baseline_SP[,c("markets","Welfare_baseline_SP")],by="markets")
  welfare[,sorting_cost:=Welfare_baseline_CP-Welfare_baseline_Comp]
  welfare[,policy:=policy]
  base_welfare = rbind(base_welfare,welfare)
}
merger_welfare = merge(merger_welfare,base_welfare,by=c("markets","policy"),all.x=TRUE)


merger_welfare_SP = NULL
for (policy in c("Base","RAMan","Man","RA","PL")){
  print(policy)
  if (policy=="PL"){
    spec_temp = paste("PL",spec,sep="_")
    policy_temp="Base"
  }else{
    spec_temp = spec
    policy_temp = policy
  }
  #### Iterate Through Mergers ####
  merger_files = list.files("Estimation_Output",pattern=paste("totalWelfare_bymkt_AllMergers_",spec_temp,"-",run,"_",policy_temp,"_",sep=""))
  merger_SP_CP_files = merger_files[grepl("_SP_",merger_files)]
  merger_base_files = merger_files[!grepl("_SP_",merger_files)]
  
  
  for (i in 1:length(unique_firms)){
    for (j in 1:(i-1)){
      if (j==0){next}
      m1 = unique_firms[j]
      m2 = unique_firms[i]
      ## Read in Welfare File
      merging_party_string = paste("_",m1,"_",m2,"-",spec,sep="")
      file_SP_CP = merger_SP_CP_files[grepl(merging_party_string,merger_SP_CP_files)]
      if (length(file_SP_CP)==0){next}
      welfare_SP_CP = fread(paste("Estimation_Output/",file_SP_CP,sep=""))
      welfare_SP_CP[,Tot_Welfare_merge_cp:=CW+Profit]
      welfare_SP_CP[,merging_parties:=paste(unique_firms[j],unique_firms[i],sep="-")]
      welfare_SP_CP[,policy:=policy]
      merger_welfare_SP = rbind(merger_welfare_SP,welfare_SP_CP[,c("markets","Tot_Welfare_merge_cp","merging_parties","policy")])
    }
  }
  rm(welfare_SP_CP)
}

merger_welfare = merge(merger_welfare,merger_welfare_SP,by=c("markets","merging_parties","policy"),all.x=TRUE)


##### Welfare Decomposition #####
big_mergers = merger_welfare[dHHI>500]
big_mergers[,chg_rank:= rank(chg_Tot_Welfare,ties.method="first"),by=policy]
big_mergers[,sorting_rank:= rank(sorting_cost,ties.method="first"),by=policy]
big_mergers[,dMarkup:=Tot_Welfare_merge_cp - Welfare_baseline_CP]
big_mergers[,dSorting:=chg_Tot_Welfare - dMarkup]

## Smooth out non-ordered variables
big_mergers[,chg_rank_bucket:=floor(chg_rank/10)]
big_mergers[,sorting_rank_bucket:=floor(sorting_rank/10)]
big_mergers[sorting_rank==max(sorting_rank),sorting_rank_bucket:=max(sorting_rank_bucket)-1]# Group max into previous group

big_mergers[,sorting_cost_smooth:=mean(sorting_cost,na.rm=TRUE),by=c("sorting_rank_bucket","policy")]
big_mergers[,dSorting_smooth:=mean(dSorting,na.rm=TRUE),by=c("sorting_rank_bucket","policy")]
big_mergers[,dMarkup_smooth:=mean(dMarkup,na.rm=TRUE),by=c("sorting_rank_bucket","policy")]
big_mergers[,dWelfare_smooth:=mean(chg_Tot_Welfare,na.rm=TRUE),by=c("sorting_rank_bucket","policy")]
big_mergers[,hhi_smooth:=mean(hhi,na.rm=TRUE),by=c("sorting_rank_bucket","policy")]


plot_df1 = big_mergers[,c("sorting_cost_smooth","policy","dWelfare_smooth")]
names(plot_df1)[3] = "value"
plot_df1[,label:="Total Welfare Effect"]
plot_df2 = big_mergers[,c("sorting_cost_smooth","policy","hhi_smooth")]
names(plot_df2)[3] = "value"
plot_df2[,label:="Pre-Merger HHI"]
plot_df3 = big_mergers[,c("sorting_cost_smooth","policy","dMarkup_smooth")]
names(plot_df3)[3] = "value"
plot_df3[,label:="Change due to Markups"]
plot_df4 = big_mergers[,c("sorting_cost_smooth","policy","dSorting_smooth")]
names(plot_df4)[3] = "value"
plot_df4[,label:="Change due to Sorting"]
plotdf = rbind(plot_df1,plot_df2,plot_df3,plot_df4)

plotdf[,label:= factor(label,levels=c("Total Welfare Effect","Pre-Merger HHI","Change due to Sorting","Change due to Markups"))]
plotdf[,facet:=label]
plotdf[label%in%c("Change due to Sorting","Change due to Markups"),facet:="Decomposition"]
plotdf[,color:=label]
plotdf[!label%in%c("Change due to Sorting","Change due to Markups"),color:=NA]

ggplot(plotdf[policy=="RAMan"&label!="Pre-Merger HHI"]) + 
  aes(x=sorting_cost_smooth,y=value,color=label) + geom_point(alpha = 0.5) + geom_smooth(se=FALSE)

ggplot(plotdf[policy=="Base"&label!="Pre-Merger HHI"]) + 
  aes(x=sorting_cost_smooth,y=value,color=label) + geom_point(alpha = 0.5) + geom_smooth(se=FALSE)

ggplot(plotdf[policy=="RA"&label!="Pre-Merger HHI"]) + 
  geom_point(aes(x=sorting_cost_smooth,y=value,color=label),alpha = 0.5) + geom_histogram(aes(x=sorting_cost_smooth)) + geom_smooth(aes(x=sorting_cost_smooth,y=value,color=label),se=FALSE) +ggtitle("Mandate Only")

ggplot(plotdf[policy=="Man"&label!="Pre-Merger HHI"]) + 
   geom_point(aes(x=sorting_cost_smooth,y=value,color=label),alpha = 0.5) + geom_histogram(aes(x=sorting_cost_smooth)) + geom_smooth(aes(x=sorting_cost_smooth,y=value,color=label),se=FALSE) +ggtitle("Risk Adj Only")

ggplot(plotdf[policy=="PL"&label!="Pre-Merger HHI"]) + 
  aes(x=sorting_cost_smooth,y=value,color=label) + geom_point(alpha = 0.5) + geom_smooth(se=FALSE)


png("Writing/Images/RAManWelfareDist.png",width=2500,height=1500,res=275)
ggplot(plotdf[policy=="RAMan"&label%in%c("Total Welfare Effect")]) + 
  aes(x=chg_rank,y=value) + geom_bar(stat="identity",color=grey(0.5),fill=grey(0.5)) + 
  ylab("Dollars Per-Person Per-Month")+
  xlab("")+
  theme(strip.background = element_blank(),
    strip.text = element_text(size=14),
    legend.background = element_rect(color=grey(.5)),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "bottom",
    axis.title=element_text(size=14),
    axis.text.y = element_text(size=16),
    axis.text.x = element_blank())
dev.off()

png("Writing/Images/RAManWelfareSorting.png",width=2500,height=1500,res=275)
ggplot(plotdf[policy=="RAMan"&label%in%c("Total Welfare Effect","Pre-Merger Cost of Inefficient Sorting")]) +
  aes(x=chg_rank,y=value) + geom_bar(stat="identity",color=grey(0.5),fill=grey(0.5)) + 
  facet_wrap(~label,ncol=1,scales="free")  + 
  ylab("Dollars Per-Person Per-Month")+
  xlab("")+
  theme(strip.background = element_blank(),
        strip.text = element_text(size=14),
        legend.background = element_rect(color=grey(.5)),
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        legend.key.width = unit(.05,units="npc"),
        legend.key = element_rect(color="transparent",fill="transparent"),
        legend.position = "bottom",
        axis.title=element_text(size=14),
        axis.text.y = element_text(size=16),
        axis.text.x = element_blank())
dev.off()


png("Writing/Images/RAManWelfareDecomp.png",width=2500,height=1500,res=275)
ggplot(plotdf[policy=="RAMan"&label%in%c("Total Welfare Effect","Change due to Sorting","Change due to Markups")]) + 
  aes(x=chg_rank,y=value,fill=color,color=color) + geom_bar(stat="identity",position="dodge") + 
  facet_wrap(~facet,ncol=1)  +  
  scale_fill_discrete(limits = c("Change due to Sorting","Change due to Markups")) +
  scale_color_discrete(limits = c("Change due to Sorting","Change due to Markups")) +
  ylab("Dollars Per-Person Per-Month")+
  xlab("")+
  theme(strip.background = element_blank(),
        strip.text = element_text(size=14),
        legend.background = element_rect(color=grey(.5)),
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        legend.key.width = unit(.05,units="npc"),
        legend.key = element_rect(color="transparent",fill="transparent"),
        legend.position = "bottom",
        axis.title=element_text(size=14),
        axis.text.y = element_text(size=16),
        axis.text.x = element_blank())
dev.off()


png("Writing/Images/BaseWelfareDist.png",width=2500,height=1500,res=275)
ggplot(plotdf[policy=="Base"&label%in%c("Total Welfare Effect")]) + 
  aes(x=chg_rank,y=value) + geom_bar(stat="identity",color=grey(0.5),fill=grey(0.5)) + 
  ylab("Dollars Per-Person Per-Month")+
  xlab("")+
  theme(strip.background = element_blank(),
        strip.text = element_text(size=14),
        legend.background = element_rect(color=grey(.5)),
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        legend.key.width = unit(.05,units="npc"),
        legend.key = element_rect(color="transparent",fill="transparent"),
        legend.position = "bottom",
        axis.title=element_text(size=14),
        axis.text.y = element_text(size=16),
        axis.text.x = element_blank())
dev.off()

png("Writing/Images/BaseWelfareSorting.png",width=2500,height=1500,res=275)
ggplot(plotdf[policy=="Base"&label%in%c("Total Welfare Effect","Pre-Merger Cost of Inefficient Sorting")]) +
  aes(x=chg_rank,y=value) + geom_bar(stat="identity",color=grey(0.5),fill=grey(0.5)) + 
  facet_wrap(~label,ncol=1,scales="free")  + 
  ylab("Dollars Per-Person Per-Month")+
  xlab("")+
  theme(strip.background = element_blank(),
        strip.text = element_text(size=14),
        legend.background = element_rect(color=grey(.5)),
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        legend.key.width = unit(.05,units="npc"),
        legend.key = element_rect(color="transparent",fill="transparent"),
        legend.position = "bottom",
        axis.title=element_text(size=14),
        axis.text.y = element_text(size=16),
        axis.text.x = element_blank())
dev.off()


png("Writing/Images/BaseWelfareDecomp.png",width=2500,height=1500,res=275)
ggplot(plotdf[policy=="Base"&label%in%c("Total Welfare Effect","Change due to Sorting","Change due to Markups")]) + 
  aes(x=chg_rank,y=value,fill=color,color=color) + geom_bar(stat="identity",position="dodge") + 
  facet_wrap(~facet,ncol=1)  +  
  scale_fill_discrete(limits = c("Change due to Sorting","Change due to Markups")) +
  scale_color_discrete(limits = c("Change due to Sorting","Change due to Markups")) +
  ylab("Dollars Per-Person Per-Month")+
  xlab("")+
  theme(strip.background = element_blank(),
        strip.text = element_text(size=14),
        legend.background = element_rect(color=grey(.5)),
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        legend.key.width = unit(.05,units="npc"),
        legend.key = element_rect(color="transparent",fill="transparent"),
        legend.position = "bottom",
        axis.title=element_text(size=14),
        axis.text.y = element_text(size=16),
        axis.text.x = element_blank())
dev.off()

