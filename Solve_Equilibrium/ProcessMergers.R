rm(list = ls())
library(data.table)
library(ggplot2)
library(scales)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

run = "2022-12-10"
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


#### Welfare By Market Concentration ####
conc_welfare = NULL
for (policy in c("Base")){
  print(policy)
  filestub = paste("Estimation_Output/AllMergers_",spec,"-",run,"_",policy,"_",sep="")
  
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
  base_welfare = fread(paste("Estimation_Output/totalWelfare_bymkt_AllMergers_",spec,"-",run,"_",policy,"_baseline-",spec,"-",run,".csv",sep=""))
  base_welfare[,tot_Welfare:=CW+Profit]
  base_welfare[,tot_Welfare_gov:=CW+Profit+Spending]
  
  conc_base = merge(base_welfare[,c("markets","tot_Welfare","tot_Welfare_gov")],hhi[,c("Market","markets","hhi","firm_num")],by="markets")
  conc_base[,dHHI:=0]
  conc_base[,merging_parties:="baseline"]
  conc_base[,policy:=policy]
  conc_welfare = rbind(conc_welfare,conc_base)
  
  #### Iterate Through Mergers ####
  merger_welfare_files = list.files("Estimation_Output",pattern=paste("totalWelfare_bymkt_AllMergers_",spec,"-",run,"_",policy,sep=""))
  merger_price_files = list.files("Estimation_Output",pattern=paste("^AllMergers_",spec,"-",run,"_",policy,sep=""))
  unique_firms = sort(firm_share[,unique(Firm)])
  
  
  for (i in 1:length(unique_firms)){
    for (j in 1:(i-1)){
      if (j==0){next}
      m1 = unique_firms[j]
      m2 = unique_firms[i]
      ## Read in Welfare File
      merging_party_string = paste(policy,"_",m1,"_",m2,"-",spec,sep="")
      welfare_file = merger_welfare_files[grepl(merging_party_string,merger_welfare_files)]
      merging_party_string = paste(policy,"_",m1,"_",m2,".csv",sep="")
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

conc_welfare[policy=="RAMan",summary(lm(tot_Welfare~Market+firmFactor))]
conc_welfare[policy=="Base",summary(lm(tot_Welfare~Market+firmFactor))]
conc_welfare[policy=="RA",summary(lm(tot_Welfare~Market+firmFactor))]
conc_welfare[policy=="Man",summary(lm(tot_Welfare~Market+firmFactor))]

#### Merger Welfare Data ####
merger_welfare = NULL
for (policy in c("Base")){
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
merger_welfare[policy=="Man",quantile(hhi,probs=c(.1,.25,.5,.75,.9))]
## Post-HHI
# merger_welfare[policy=="Base",quantiles(firm_num,probs=c(.1,.25,.5,.75,.9))]
## Delta HHI
merger_welfare[policy=="Base",quantile(dHHI,probs=c(.1,.25,.5,.75,.9,.99))]


#### Welfare Effect Merger Plot #####
merger_welfare[,chg_Tot_Welfare:=chg_CW+chg_Profit]
merger_welfare[,chg_Tot_Welfaregov:=chg_CW+chg_Profit+chg_Spending]


plotdf1 = merger_welfare[,c("dHHI","chg_CW","policy")]
names(plotdf1)[2] = "value"
plotdf1[,label:="Consumer Welfare"]
plotdf1[,chg_CW:=NA]
plotdf2 = merger_welfare[,c("dHHI","chg_Tot_Welfare","chg_CW","policy")]
names(plotdf2)[2] = "value"
plotdf2[,label:="Total Welfare"]
plotdf = rbind(plotdf1,plotdf2)



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



#### Merger Price-Effect Data ####
merger_effects = NULL
for (policy in c("RAMan")){
  print(policy)
  filestub = paste("Estimation_Output/AllMergers_",spec,"-",run,"_",policy,"_",sep="")
  
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
      postmerge[,Price_Effect_percent:=(Price_merge-Price)/Price]
      
      postmerge[,merging_parties:= paste(unique_firms[j],unique_firms[i],sep="-")]
      postmerge[,policy:=policy]
      
      #
      dHHI = firm_share[Firm%in%c(m1,m2),list(dHHI=2*prod(share),merger=sum(count)),by="Market"]
      dHHI[merger<2,dHHI:=0]
      
      postmerge = merge(postmerge,dHHI[,c("Market","dHHI")],by="Market",all.x=TRUE)
      postmerge[is.na(dHHI),dHHI:=0]
      postmerge = postmerge[dHHI>0]
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
  coord_cartesian(ylim=c(-.1,.25))  +
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
  coord_cartesian(ylim=c(-.7,.25))  +
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

##### Decomposition  Data ####

base_welfare = NULL
for (policy in c("Base","RA","Man","RAMan")){
  print(policy)
  baseline_CP = fread(paste("Estimation_Output/totalWelfare_bymkt_AllMergers_",spec,"-",run,"_",policy,"_SP_cp_baseline-",spec,"-",run,".csv",sep=""))
  baseline_Comp = fread(paste("Estimation_Output/totalWelfare_bymkt_AllMergers_",spec,"-",run,"_",policy,"_baseline-",spec,"-",run,".csv",sep=""))
  baseline_SP = fread(paste("Estimation_Output/totalWelfare_bymkt_AllMergers_",spec,"-",run,"_",policy,"_SP_baseline-",spec,"-",run,".csv",sep=""))
  
  
  
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
for (policy in c("Base","RA","Man","RAMan")){
  print(policy)
  #### Iterate Through Mergers ####
  merger_files = list.files("Estimation_Output",pattern=paste("totalWelfare_bymkt_AllMergers_",spec,"-",run,"_",policy,"_",sep=""))
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

merger_welfare[,chg_rank:= rank(chg_Tot_Welfare,ties.method="first"),by=policy]
merger_welfare[,dMarkup:=Tot_Welfare_merge_cp - Welfare_baseline_CP]
merger_welfare[,dSorting:=chg_Tot_Welfare - dMarkup]

## Smooth out non-ordered variables
merger_welfare[,chg_rank_bucket:=floor(chg_rank/10)]
merger_welfare[,sorting_cost_smooth:=-mean(sorting_cost,na.rm=TRUE),by=c("chg_rank_bucket","policy")]
merger_welfare[,dSorting_smooth:=mean(dSorting,na.rm=TRUE),by=c("chg_rank_bucket","policy")]
merger_welfare[,dMarkup_smooth:=mean(dMarkup,na.rm=TRUE),by=c("chg_rank_bucket","policy")]


plot_df1 = merger_welfare[,c("chg_rank","policy","chg_Tot_Welfare")]
names(plot_df1)[3] = "value"
plot_df1[,label:="Total Welfare Effect"]
plot_df2 = merger_welfare[,c("chg_rank","policy","sorting_cost_smooth")]
names(plot_df2)[3] = "value"
plot_df2[,label:="Pre-Merger Cost of Inefficient Sorting"]
plot_df3 = merger_welfare[,c("chg_rank","policy","dMarkup_smooth")]
names(plot_df3)[3] = "value"
plot_df3[,label:="Change due to Markups"]
plot_df4 = merger_welfare[,c("chg_rank","policy","dSorting_smooth")]
names(plot_df4)[3] = "value"
plot_df4[,label:="Change due to Sorting"]
plotdf = rbind(plot_df1,plot_df2,plot_df3,plot_df4)

plotdf[,label:= factor(label,levels=c("Total Welfare Effect","Pre-Merger Cost of Inefficient Sorting","Change due to Sorting","Change due to Markups"))]
plotdf[,facet:=label]
plotdf[label%in%c("Change due to Sorting","Change due to Markups"),facet:="Decomposition"]
plotdf[,color:=label]
plotdf[!label%in%c("Change due to Sorting","Change due to Markups"),color:=NA]




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
ggplot(plotdf[policy=="Man"&label%in%c("Total Welfare Effect","Pre-Merger Cost of Inefficient Sorting")]) +
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
ggplot(plotdf[policy=="RA"&label%in%c("Total Welfare Effect","Change due to Sorting","Change due to Markups")]) + 
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

