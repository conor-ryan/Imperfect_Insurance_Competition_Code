rm(list = ls())
library(data.table)
library(ggplot2)
library(scales)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

run = "2022-03-18"
spec = "FMC"

### Base Data 
prodData = as.data.table(read.csv("Intermediate_Output/Equilibrium_Data/estimated_prodData_full.csv"))
prodData = prodData[,c("ST","Firm","Product_std","Metal_std","Market")]
names(prodData) = c("ST","Firm","Product","Metal_std","Market")

load("Intermediate_Output/Simulated_BaseData/simMarketSize.rData")
prodData = merge(prodData,marketSize,by="Market")


#### Merger Welfare Data ####
merger_welfare = NULL
for (policy in c("Base","RAMan")){
  print(policy)
  filestub = paste("Estimation_Output/AllMergers_",spec,"-",run,"_",policy,"_",sep="")
  
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
  base_welfare = fread(paste("Estimation_Output/totalWelfare_bymkt_AllMergers_",spec,"-",run,"_",policy,"_baseline-",spec,"-",run,".csv",sep=""))
  
  #### Iterate Through Mergers ####
  merger_files = list.files("Estimation_Output",pattern=paste("totalWelfare_bymkt_AllMergers_",spec,"-",run,"_",policy,sep=""))
  unique_firms = sort(firm_share[,unique(Firm)])
  
  
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
      welfare[,policy:=policy]
      
      #
      dHHI = firm_share[Firm%in%c(m1,m2),list(dHHI=2*prod(share),merger=sum(count)),by="Market"]
      dHHI[merger<2,dHHI:=0]
      
      welfare = merge(welfare,dHHI[,c("Market","dHHI")],by="Market",all.x=TRUE)
      welfare[is.na(dHHI),dHHI:=0]
      welfare = welfare[dHHI>100]
      merger_welfare = rbind(merger_welfare,welfare)
    }
  }
  rm(welfare,hhi,base_welfare,baseline,firm_share,dHHI)
}


#### Market Structure Description #####
## Firms
merger_welfare[policy=="Base",quantile(firm_num,probs=c(.1,.25,.5,.75,.9))]
## Pre-HHI
merger_welfare[policy=="Base",quantile(hhi,probs=c(.1,.25,.5,.75,.9))]
## Post-HHI
# merger_welfare[policy=="Base",quantiles(firm_num,probs=c(.1,.25,.5,.75,.9))]
## Delta HHI
merger_welfare[policy=="Base",quantile(dHHI,probs=c(.1,.25,.5,.75,.9,.99))]

#### Welfare Effect Merger Plot #####
merger_welfare[,chg_Tot_Welfare:=chg_CW+chg_Profit]
merger_welfare[,chg_Tot_Welfaregov:=chg_CW+chg_Profit+chg_Spending]


# ggplot(merger_welfare[policy=="Base"])+ 
#   geom_point(aes(x=dHHI,y=chg_CW,color="Consumer Welfare"),shape=16,size=2)+ 
#   geom_point(aes(x=dHHI,y=chg_Tot_Welfare,color="Total Welfare"),shape=17,size=2) + 
#   geom_errorbar(aes(x=dHHI,ymin=chg_CW,ymax=chg_Tot_Welfare)) +
#   facet_wrap(~hhi_category,ncol=1) + 
#   geom_abline(slope=0,intercept=0) + 
#   xlab("Predicted Change in HHI")+
#   ylab("Dollars Per-Person Per-Month")+
#   theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
#     strip.background = element_blank(),
#     strip.text = element_text(size=14),
#     legend.background = element_rect(color=grey(.5)),
#     legend.title = element_blank(),
#     legend.text = element_text(size=14),
#     legend.key.width = unit(.05,units="npc"),
#     legend.key = element_rect(color="transparent",fill="transparent"),
#     legend.position = "bottom",
#     axis.title=element_text(size=14),
#     axis.text = element_text(size=16))

# ggplot(merger_welfare[policy=="Base"])+ 
#   geom_point(aes(x=dHHI,y=chg_CW,color="Consumer Welfare"),shape=16,size=2)+ 
#   geom_point(aes(x=dHHI,y=chg_Tot_Welfare,color="Total Welfare"),shape=17,size=2) + 
#   geom_errorbar(aes(x=dHHI,ymin=chg_CW,ymax=chg_Tot_Welfare)) +
#   facet_wrap(~hhi_category,ncol=1) + 
#   geom_abline(slope=0,intercept=0) + 
#   xlab("Predicted Change in HHI")+
#   ylab("Dollars Per-Person Per-Month")+
#   coord_cartesian(xlim=c(0,1000),ylim=c(-10,10))+
#   theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
#     strip.background = element_blank(),
#     strip.text = element_text(size=14),
#     legend.background = element_rect(color=grey(.5)),
#     legend.title = element_blank(),
#     legend.text = element_text(size=14),
#     legend.key.width = unit(.05,units="npc"),
#     legend.key = element_rect(color="transparent",fill="transparent"),
#     legend.position = "bottom",
#     axis.title=element_text(size=14),
#     axis.text = element_text(size=16))

ggplot(merger_welfare[policy=="Base"])+
  geom_point(aes(x=dHHI,y=chg_CW,color="Consumer Welfare"),shape=16,size=2)+
  geom_point(aes(x=dHHI,y=chg_Tot_Welfare,color="Total Welfare"),shape=17,size=2) +
  geom_errorbar(aes(x=dHHI,ymin=chg_CW,ymax=chg_Tot_Welfare)) +
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
# 
# ggplot(merger_welfare[policy=="Base"])+ 
#   geom_point(aes(x=dHHI,y=chg_CW,color="Consumer Welfare"),shape=16,size=2)+ 
#   geom_point(aes(x=dHHI,y=chg_Tot_Welfare,color="Total Welfare"),shape=17,size=2) + 
#   geom_errorbar(aes(x=dHHI,ymin=chg_CW,ymax=chg_Tot_Welfare)) +
#   # facet_wrap(~hhi_category,ncol=1) + 
#   geom_abline(slope=0,intercept=0) + 
#   xlab("Predicted Change in HHI")+
#   ylab("Dollars Per-Person Per-Month")+
#   coord_cartesian(xlim=c(0,1000),ylim=c(-10,10))+
#   theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
#     strip.background = element_blank(),
#     strip.text = element_text(size=14),
#     legend.background = element_rect(color=grey(.5)),
#     legend.title = element_blank(),
#     legend.text = element_text(size=14),
#     legend.key.width = unit(.05,units="npc"),
#     legend.key = element_rect(color="transparent",fill="transparent"),
#     legend.position = "bottom",
#     axis.title=element_text(size=14),
#     axis.text = element_text(size=16))

# ggplot(merger_welfare[policy=="RAMan"])+ 
#   geom_point(aes(x=dHHI,y=chg_CW,color="Consumer Welfare"),shape=16,size=2)+ 
#   geom_point(aes(x=dHHI,y=chg_Tot_Welfare,color="Total Welfare"),shape=17,size=2) + 
#   geom_errorbar(aes(x=dHHI,ymin=chg_CW,ymax=chg_Tot_Welfare)) +
#   facet_wrap(~hhi_category,ncol=1) +
#   geom_abline(slope=0,intercept=0) +
#   xlab("Predicted Change in HHI")+
#   ylab("Dollars Per-Person Per-Month")+
#   theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
#     strip.background = element_blank(),
#     strip.text = element_text(size=14),
#     legend.background = element_rect(color=grey(.5)),
#     legend.title = element_blank(),
#     legend.text = element_text(size=14),
#     legend.key.width = unit(.05,units="npc"),
#     legend.key = element_rect(color="transparent",fill="transparent"),
#     legend.position = "bottom",
#     axis.title=element_text(size=14),
#     axis.text = element_text(size=16))

# ggplot(merger_welfare[policy=="RAMan"])+ 
#   geom_point(aes(x=dHHI,y=chg_CW,color="Consumer Welfare"),shape=16,size=2)+ 
#   geom_point(aes(x=dHHI,y=chg_Tot_Welfare,color="Total Welfare"),shape=17,size=2) + 
#   geom_errorbar(aes(x=dHHI,ymin=chg_CW,ymax=chg_Tot_Welfare)) +
#   facet_wrap(~hhi_category,ncol=1) +
#   geom_abline(slope=0,intercept=0) + 
#   xlab("Predicted Change in HHI")+
#   ylab("Dollars Per-Person Per-Month")+
#   coord_cartesian(xlim=c(0,1000),ylim=c(-10,35))+
#   theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
#     strip.background = element_blank(),
#     strip.text = element_text(size=14),
#     legend.background = element_rect(color=grey(.5)),
#     legend.title = element_blank(),
#     legend.text = element_text(size=14),
#     legend.key.width = unit(.05,units="npc"),
#     legend.key = element_rect(color="transparent",fill="transparent"),
#     legend.position = "bottom",
#     axis.title=element_text(size=14),
#     axis.text = element_text(size=16))

ggplot(merger_welfare[policy=="RAMan"])+ 
  geom_point(aes(x=dHHI,y=chg_CW,color="Consumer Welfare"),shape=16,size=2)+ 
  geom_point(aes(x=dHHI,y=chg_Tot_Welfare,color="Total Welfare"),shape=17,size=2) + 
  geom_errorbar(aes(x=dHHI,ymin=chg_CW,ymax=chg_Tot_Welfare)) +
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

# ggplot(merger_welfare[policy=="RAMan"])+ 
#   geom_point(aes(x=dHHI,y=chg_CW,color="Consumer Welfare"),shape=16,size=2)+ 
#   geom_point(aes(x=dHHI,y=chg_Tot_Welfare,color="Total Welfare"),shape=17,size=2) + 
#   geom_errorbar(aes(x=dHHI,ymin=chg_CW,ymax=chg_Tot_Welfare)) +
#   # facet_wrap(~hhi_category,ncol=1) + 
#   geom_abline(slope=0,intercept=0) + 
#   xlab("Predicted Change in HHI")+
#   ylab("Dollars Per-Person Per-Month")+
#   coord_cartesian(xlim=c(0,1000),ylim=c(-10,35))+
#   theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
#     strip.background = element_blank(),
#     strip.text = element_text(size=14),
#     legend.background = element_rect(color=grey(.5)),
#     legend.title = element_blank(),
#     legend.text = element_text(size=14),
#     legend.key.width = unit(.05,units="npc"),
#     legend.key = element_rect(color="transparent",fill="transparent"),
#     legend.position = "bottom",
#     axis.title=element_text(size=14),
#     axis.text = element_text(size=16))



#### Merger Price-Effect Data ####
merger_effects = NULL
for (policy in c("Base","RAMan")){
  print(policy)
  filestub = paste("Estimation_Output/AllMergers_",spec,"-",run,"_",policy,"_",sep="")
  
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
  
  #### Iterate Through Mergers ####
  merger_files = list.files("Estimation_Output",pattern=paste("^AllMergers_",spec,"-",run,"_",policy,sep=""))
  unique_firms = sort(firm_share[,unique(Firm)])
  
  
  for (i in 1:length(unique_firms)){
    for (j in 1:(i-1)){
      if (j==0){next}
      m1 = unique_firms[j]
      m2 = unique_firms[i]
      ## Read in Welfare File
      merging_party_string = paste(policy,"_",m1,"_",m2,".csv",sep="")
      file = merger_files[grepl(merging_party_string,merger_files)]
      if (length(file)==0){next}
      postmerge = fread(paste("Estimation_Output/",file,sep=""))
      names(postmerge)[2:length(names(postmerge))] = paste(names(postmerge)[2:length(names(postmerge))],"merge",sep="_")
      
      # Merge in HHI and Baseline Data
      postmerge = merge(baseline,postmerge,by="Product")
      postmerge = merge(hhi,postmerge,by=c("Market","ST"))
      
      #Compute Change variables
      postmerge[,Price_Effect:=Price_merge-Price]
      
      postmerge[,merging_parties:= paste(unique_firms[j],unique_firms[i],sep="-")]
      postmerge[,policy:=policy]
      
      #
      dHHI = firm_share[Firm%in%c(m1,m2),list(dHHI=2*prod(share),merger=sum(count)),by="Market"]
      dHHI[merger<2,dHHI:=0]
      
      postmerge = merge(postmerge,dHHI[,c("Market","dHHI")],by="Market",all.x=TRUE)
      postmerge[is.na(dHHI),dHHI:=0]
      postmerge = postmerge[dHHI>100]
      merger_effects = rbind(merger_effects,postmerge)
    }
  }
  rm(postmerge,hhi,baseline,firm_share,dHHI,merger_files)
}





