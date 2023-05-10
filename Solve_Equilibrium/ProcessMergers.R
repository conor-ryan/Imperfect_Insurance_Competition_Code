rm(list = ls())
library(data.table)
library(ggplot2)
library(scales)
library(tidyr)
setwd("C:/Users/cxr5626/Dropbox/Research/Imperfect_Insurance_Competition/")
# setwd("C:/Users/Conor/Dropbox/Research/Imperfect_Insurance_Competition/")

run = "2022-12-26"
spec = "FMC"

### Base Data 
prodData = as.data.table(read.csv("Intermediate_Output/Equilibrium_Data/estimated_prodData_full.csv"))
prodData = prodData[,c("ST","Firm","Product_std","Metal_std","Market")]
names(prodData) = c("ST","Firm","Product","Metal_std","Market")

load("Intermediate_Output/Simulated_BaseData/simMarketSize.rData")
prodData = merge(prodData,marketSize,by="Market")

#### Merger Price-Effect Data ####
merger_effects = NULL
for (policy in c("Base","RA")){
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
      if (!"UPP_avg_merge"%in%names(postmerge)){
        postmerge[,UPP_avg_merge:=NA]
        postmerge[,UPP_sel_merge:=NA]
      }
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
      
      postmerge[,Firm_merge:=Firm]
      postmerge[Firm%in%c(m1,m2),Firm_merge:=merging_parties]
      postmerge[,parties_indicator:=as.numeric(Firm_merge==merging_parties)]
      postmerge[,pre_profit_firm:=sum(Profit),by=c("Firm_merge","Market")]
      postmerge[,post_profit_firm:=sum(Profit_merge),by=c("Firm_merge","Market")]
      
      merger_effects = rbind(merger_effects,postmerge)
    }
  }
  rm(postmerge,hhi,baseline,firm_share,dHHI,merger_files)
}

merger_effects[,Metal_std:=factor(Metal_std,levels=c("CATASTROPHIC","BRONZE","SILVER","GOLD","PLATINUM"),
                                  labels=c("Catas","Bronze","Silver","Gold","Plat"))]
merger_effects[,missing_gold_plat:=as.numeric(Metal_std%in%c("Gold","Plat")&Lives<1)]
merger_effects[,missing_gold_plat_post:=as.numeric(Metal_std%in%c("Gold","Plat")&Lives_merge<1)]
merger_effects[,UPP:=UPP_avg_merge+UPP_sel_merge]

merger_firms = merger_effects[parties_indicator==1,
                              list(avg_risk=sum(Risk*Lives)/sum(Lives),
                                   avg_price=sum(Price*Lives)/sum(Lives),
                                   missing_gold_plat=sum(missing_gold_plat),
                                   missing_gold_plat_post=sum(missing_gold_plat_post)),
                              by=c("merging_parties","markets","policy","Firm")]
merger_properties = merger_firms[,list(risk_diff=max(avg_risk)-min(avg_risk),
                                       price_diff=max(avg_price)-min(avg_price),
                                       missing_gold_plat=sum(missing_gold_plat),
                                       missing_gold_plat_post=sum(missing_gold_plat_post)),
                                 by=c("merging_parties","markets","policy")]

#### Merger Welfare Data ####
merger_welfare = NULL
for (policy in c("Base","RA")){
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
      welfare = welfare[dHHI>0]
      merger_welfare = rbind(merger_welfare,welfare)
    }
  }
  rm(welfare,hhi,base_welfare,baseline,firm_share,dHHI)
}



#### Welfare Effect Merger Plot #####
merger_welfare[,chg_Tot_Welfare:=chg_CW+chg_Profit]
merger_welfare[,chg_Tot_Welfaregov:=chg_CW+chg_Profit+chg_Spending]

##### Decomposition  Data ####
base_welfare = NULL
for (policy in c("Base","RA")){
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


# merger_welfare_SP = NULL
# for (policy in c("Base","RAMan","Man","RA")){
#   print(policy)
#   if (policy=="PL"){
#     spec_temp = paste("PL",spec,sep="_")
#     policy_temp="Base"
#   }else{
#     spec_temp = spec
#     policy_temp = policy
#   }
#   #### Iterate Through Mergers ####
#   merger_files = list.files("Estimation_Output",pattern=paste("totalWelfare_bymkt_AllMergers_",spec_temp,"-",run,"_",policy_temp,"_",sep=""))
#   merger_SP_CP_files = merger_files[grepl("_SP_",merger_files)]
#   merger_base_files = merger_files[!grepl("_SP_",merger_files)]
# 
# 
#   for (i in 1:length(unique_firms)){
#     for (j in 1:(i-1)){
#       if (j==0){next}
#       m1 = unique_firms[j]
#       m2 = unique_firms[i]
#       ## Read in Welfare File
#       merging_party_string = paste("_",m1,"_",m2,"-",spec,sep="")
#       file_SP_CP = merger_SP_CP_files[grepl(merging_party_string,merger_SP_CP_files)]
#       if (length(file_SP_CP)==0){next}
#       welfare_SP_CP = fread(paste("Estimation_Output/",file_SP_CP,sep=""))
#       welfare_SP_CP[,Tot_Welfare_merge_cp:=CW+Profit]
#       welfare_SP_CP[,merging_parties:=paste(unique_firms[j],unique_firms[i],sep="-")]
#       welfare_SP_CP[,policy:=policy]
#       merger_welfare_SP = rbind(merger_welfare_SP,welfare_SP_CP[,c("markets","Tot_Welfare_merge_cp","merging_parties","policy")])
#       rm(welfare_SP_CP)
#     }
#   }
# }
# 
# merger_welfare = merge(merger_welfare,merger_welfare_SP,by=c("markets","merging_parties","policy"),all.x=TRUE)
merger_welfare[,policy_label:=factor(policy,levels=c("Base","RA"),labels=c("Baseline","No Risk Adjustment"))]
merger_welfare = merge(merger_welfare,merger_properties,by=c("merging_parties","markets","policy"))


##### Main Results Table #####
merger_welfare[,count:=1]

merger_welfare[,dHHI_Label:="<200"]
# merger_welfare[dHHI>50,dHHI_Label:="50 - 100"]
# merger_welfare[dHHI>100,dHHI_Label:="100 - 200"]
merger_welfare[dHHI>200,dHHI_Label:="200 - 500"]
merger_welfare[dHHI>500,dHHI_Label:="500 - 1000"]
merger_welfare[dHHI>1000,dHHI_Label:=">1000"]
merger_welfare[,dHHI_Label:=factor(dHHI_Label,levels=c("<200","200 - 500","500 - 1000",">1000"))]

merger_welfare[,sorting_Label:="<\\$5"]
merger_welfare[sorting_cost>5,sorting_Label:="\\$5-\\$7.5"]
merger_welfare[sorting_cost>7.5,sorting_Label:="\\$7.5-\\$10"]
merger_welfare[sorting_cost>10,sorting_Label:=">\\$10"]
merger_welfare[,sorting_Label:=factor(sorting_Label,levels=c("<\\$5","\\$5-\\$7.5","\\$7.5-\\$10",">\\$10"))]

Main_HHI = merger_welfare[,list(avgdWelfare=round(mean(chg_Tot_Welfare),2),#avgdCW=round(mean(chg_CW),2),
                                posdWelf=round(100*mean(chg_Tot_Welfare>0),1),posdCW=round(100*mean(chg_CW>0),2),N=sum(count)),by=c("dHHI_Label","policy")]

setkey(Main_HHI,policy,dHHI_Label)

Main_sort = merger_welfare[,list(avgdWelfare=round(mean(chg_Tot_Welfare),2),#avgdCW=round(mean(chg_CW),2),
                                posdWelf=round(100*mean(chg_Tot_Welfare>0),1),posdCW=round(100*mean(chg_CW>0),2),N=sum(count)),by=c("sorting_Label","policy")]

setkey(Main_sort,policy,sorting_Label)

Main_HHI = as.data.table(Main_HHI %>% gather(label,value,avgdWelfare:N))
Main_HHI[,label:=paste(policy,label,sep="_")]
Main_HHI[,policy:=NULL]
Main_HHI %>% spread(label,value)

Main_sort = as.data.table(Main_sort %>% gather(label,value,avgdWelfare:N))
Main_sort[,label:=paste(policy,label,sep="_")]
Main_sort[,policy:=NULL]
Main_sort %>% spread(label,value)

##### Identifying Positive Mergers ####
df = merger_welfare[policy=="RA"&dHHI>200]
sort = df[,list(avgdWelfare=round(mean(chg_Tot_Welfare),2),#avgdCW=round(mean(chg_CW),2),
                                 posdWelf=round(100*mean(chg_Tot_Welfare>0),1),posdCW=round(100*mean(chg_CW>0),2),N=sum(count)),by=c("sorting_Label","policy")]

setkey(sort,sorting_Label)

df[,sorting_bucket:=ceiling(sorting_cost/2.5)*2.5]
df[,risk_label:="1"]
df[risk_diff>0.064,risk_label:="2"]
df[risk_diff>0.123,risk_label:="3"]
df[risk_diff>0.19,risk_label:="4"]

plot = df[,list(pos_welfare=mean(chg_Tot_Welfare>0),N=sum(count)),by=c("sorting_bucket","risk_label")]

ggplot(plot) + aes(x=sorting_bucket,y=pos_welfare,group=risk_label,color=risk_label,shape=risk_label,size=N) + geom_point()

plotdf1 = merger_welfare[,c("dHHI","chg_CW","policy","sorting_cost","risk_diff","price_diff","policy_label")]
names(plotdf1)[2] = "value"
plotdf1[,label:="Consumer Welfare"]
plotdf1[,chg_CW:=NA]
plotdf2 = merger_welfare[,c("dHHI","chg_Tot_Welfare","chg_CW","policy","sorting_cost","risk_diff","price_diff","policy_label")]
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

png("Writing/Images/Base_WelfareEffect_bySort.png",width=2500,height=1500,res=275)
ggplot(plotdf[policy=="Base"])+
  geom_point(aes(x=sorting_cost,y=value,color=label,shape=label),size=2.5)+
  # geom_point(aes(x=dHHI,y=chg_Tot_Welfare,color="Total Welfare",shape ="Total Welfare"),size=2) +
  scale_shape_manual(values=c(16,17)) +
  geom_errorbar(aes(x=sorting_cost,ymin=chg_CW,ymax=value)) +
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

png("Writing/Images/RA_WelfareEffect.png",width=2500,height=1500,res=275)
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
dev.off()

png("Writing/Images/RA_WelfareEffect_bySort.png",width=2500,height=1500,res=275)
ggplot(plotdf[policy=="RA"])+ 
  geom_point(aes(x=sorting_cost,y=value,color=label,shape=label),size=3.0,alpha=0.2)+
  scale_shape_manual(values=c(16,17)) +
  # geom_errorbar(aes(x=sorting_cost,ymin=chg_CW,ymax=value)) +
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

ggplot(plotdf[policy=="RA"&dHHI>200&sorting_cost>6&sorting_cost<12])+ 
  geom_point(aes(x=sorting_cost,y=value,color=label,shape=label),size=2.5)+
  scale_shape_manual(values=c(16,17)) +
  geom_errorbar(aes(x=sorting_cost,ymin=chg_CW,ymax=value)) +
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

##### Welfare Decomposition #####
big_mergers = merger_welfare[dHHI>0]
big_mergers[,chg_rank:= rank(chg_Tot_Welfare,ties.method="first"),by=policy]
# big_mergers[,dMarkup:=Tot_Welfare_merge_cp - Welfare_baseline_CP]
# big_mergers[,dSorting:=chg_Tot_Welfare - dMarkup]

## Smooth out non-ordered variables
big_mergers[,chg_rank_bucket:=floor(chg_rank/10)]
big_mergers[,chg_rank_dist:=chg_rank/max(chg_rank),by="policy"]

big_mergers[,sorting_cost_smooth:=mean(sorting_cost,na.rm=TRUE),by=c("chg_rank_bucket","policy")]
big_mergers[,dWelfare_smooth:=mean(chg_Tot_Welfare,na.rm=TRUE),by=c("chg_rank_bucket","policy")]
big_mergers[,hhi_smooth:=mean(hhi,na.rm=TRUE),by=c("chg_rank_bucket","policy")]


plot_df1 = big_mergers[,c("chg_rank_bucket","policy","chg_Tot_Welfare","chg_rank_dist","policy_label")]
names(plot_df1)[3] = "value"
plot_df1[,label:="Total Welfare Effect"]
plot_df2 = big_mergers[,c("chg_rank_bucket","policy","hhi_smooth","chg_rank_dist","policy_label")]
names(plot_df2)[3] = "value"
plot_df2[,label:="Pre-Merger HHI"]
plot_df3 = big_mergers[,c("chg_rank_bucket","policy","sorting_cost","chg_rank_dist","policy_label")]
names(plot_df3)[3] = "value"
plot_df3[,label:="Sorting Cost"]
# plot_df4 = big_mergers[,c("sorting_cost_smooth","policy","dSorting_smooth")]
# names(plot_df4)[3] = "value"
# plot_df4[,label:="Change due to Sorting"]
plotdf = rbind(plot_df1,plot_df2,plot_df3)#,plot_df3,plot_df4)

plotdf[,label:= factor(label,levels=c("Total Welfare Effect","Pre-Merger HHI","Sorting Cost"))]#,"Change due to Sorting","Change due to Markups"))]
plotdf[,facet:=label]
plotdf[label%in%c("Change due to Sorting","Change due to Markups"),facet:="Decomposition"]
plotdf[,color:=label]
plotdf[!label%in%c("Change due to Sorting","Change due to Markups"),color:=NA]


plot_df1[,cross:=min(abs(value)),by="policy"]
thresh = plot_df1[abs(value)==cross]
setkey(plotdf,chg_rank_dist)

png("Writing/Images/WelfareDist_all.png",width=2500,height=1500,res=275)
ggplot(plotdf[label%in%c("Total Welfare Effect")]) + 
  facet_wrap(~policy_label,ncol=1,scales="free") + 
  aes(x=chg_rank_dist,y=value) + geom_bar(stat="identity",width=0.001,color=grey(0.5),fill=grey(0.5)) + 
  ylab("Dollars Per-Person Per-Month")+
  xlab("")+
  geom_vline(data=thresh,aes(xintercept=chg_rank_dist)) + 
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
    axis.text.x = element_text(size=16))
dev.off()

big_mergers = merger_welfare[dHHI<200&dHHI>0]
big_mergers[,chg_rank:= rank(chg_Tot_Welfare,ties.method="first"),by=policy]
# big_mergers[,dMarkup:=Tot_Welfare_merge_cp - Welfare_baseline_CP]
# big_mergers[,dSorting:=chg_Tot_Welfare - dMarkup]

## Smooth out non-ordered variables
big_mergers[,chg_rank_bucket:=floor(chg_rank/10)]
big_mergers[,chg_rank_dist:=chg_rank/max(chg_rank),by="policy"]

big_mergers[,sorting_cost_smooth:=mean(sorting_cost,na.rm=TRUE),by=c("chg_rank_bucket","policy")]
big_mergers[,dWelfare_smooth:=mean(chg_Tot_Welfare,na.rm=TRUE),by=c("chg_rank_bucket","policy")]
big_mergers[,hhi_smooth:=mean(hhi,na.rm=TRUE),by=c("chg_rank_bucket","policy")]


plot_df1 = big_mergers[,c("chg_rank_bucket","policy","chg_Tot_Welfare","chg_rank_dist","policy_label")]
names(plot_df1)[3] = "value"
plot_df1[,label:="Total Welfare Effect"]
plot_df2 = big_mergers[,c("chg_rank_bucket","policy","hhi_smooth","chg_rank_dist","policy_label")]
names(plot_df2)[3] = "value"
plot_df2[,label:="Pre-Merger HHI"]
plot_df3 = big_mergers[,c("chg_rank_bucket","policy","sorting_cost","chg_rank_dist","policy_label")]
names(plot_df3)[3] = "value"
plot_df3[,label:="Sorting Cost"]
# plot_df4 = big_mergers[,c("sorting_cost_smooth","policy","dSorting_smooth")]
# names(plot_df4)[3] = "value"
# plot_df4[,label:="Change due to Sorting"]
plotdf = rbind(plot_df1,plot_df2,plot_df3)#,plot_df3,plot_df4)

plotdf[,label:= factor(label,levels=c("Total Welfare Effect","Pre-Merger HHI","Sorting Cost"))]#,"Change due to Sorting","Change due to Markups"))]
plotdf[,facet:=label]
plotdf[label%in%c("Change due to Sorting","Change due to Markups"),facet:="Decomposition"]
plotdf[,color:=label]
plotdf[!label%in%c("Change due to Sorting","Change due to Markups"),color:=NA]


plot_df1[,cross:=min(abs(value)),by="policy"]
thresh = plot_df1[abs(value)==cross]
setkey(plotdf,chg_rank_dist)

png("Writing/Images/WelfareDist_dHHI_LT200.png",width=2500,height=1500,res=275)
ggplot(plotdf[label%in%c("Total Welfare Effect")]) + 
  facet_wrap(~policy_label,ncol=1,scales="free") + 
  aes(x=chg_rank_dist,y=value) + geom_bar(stat="identity",width=0.001,color=grey(0.5),fill=grey(0.5)) + 
  ylab("Dollars Per-Person Per-Month")+
  xlab("")+
  geom_vline(data=thresh,aes(xintercept=chg_rank_dist)) + 
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
        axis.text.x = element_text(size=16))
dev.off()

big_mergers = merger_welfare[dHHI>200]
big_mergers[,chg_rank:= rank(chg_Tot_Welfare,ties.method="first"),by=policy]
# big_mergers[,dMarkup:=Tot_Welfare_merge_cp - Welfare_baseline_CP]
# big_mergers[,dSorting:=chg_Tot_Welfare - dMarkup]

## Smooth out non-ordered variables
big_mergers[,chg_rank_bucket:=floor(chg_rank/10)]
big_mergers[,chg_rank_dist:=chg_rank/max(chg_rank),by="policy"]

big_mergers[,sorting_cost_smooth:=mean(sorting_cost,na.rm=TRUE),by=c("chg_rank_bucket","policy")]
big_mergers[,dWelfare_smooth:=mean(chg_Tot_Welfare,na.rm=TRUE),by=c("chg_rank_bucket","policy")]
big_mergers[,hhi_smooth:=mean(hhi,na.rm=TRUE),by=c("chg_rank_bucket","policy")]


plot_df1 = big_mergers[,c("chg_rank_bucket","policy","chg_Tot_Welfare","chg_rank_dist","policy_label")]
names(plot_df1)[3] = "value"
plot_df1[,label:="Total Welfare Effect"]
plot_df2 = big_mergers[,c("chg_rank_bucket","policy","hhi_smooth","chg_rank_dist","policy_label")]
names(plot_df2)[3] = "value"
plot_df2[,label:="Pre-Merger HHI"]
plot_df3 = big_mergers[,c("chg_rank_bucket","policy","sorting_cost","chg_rank_dist","policy_label")]
names(plot_df3)[3] = "value"
plot_df3[,label:="Sorting Cost"]
# plot_df4 = big_mergers[,c("sorting_cost_smooth","policy","dSorting_smooth")]
# names(plot_df4)[3] = "value"
# plot_df4[,label:="Change due to Sorting"]
plotdf = rbind(plot_df1,plot_df2,plot_df3)#,plot_df3,plot_df4)

plotdf[,label:= factor(label,levels=c("Total Welfare Effect","Pre-Merger HHI","Sorting Cost"))]#,"Change due to Sorting","Change due to Markups"))]
plotdf[,facet:=label]
plotdf[label%in%c("Change due to Sorting","Change due to Markups"),facet:="Decomposition"]
plotdf[,color:=label]
plotdf[!label%in%c("Change due to Sorting","Change due to Markups"),color:=NA]


plot_df1[,cross:=min(abs(value)),by="policy"]
thresh = plot_df1[abs(value)==cross]
setkey(plotdf,chg_rank_dist)

png("Writing/Images/WelfareDist_dHHI_GT200.png",width=2500,height=1500,res=275)
ggplot(plotdf[label%in%c("Total Welfare Effect")]) + 
  facet_wrap(~policy_label,ncol=1,scales="free") + 
  aes(x=chg_rank_dist,y=value) + geom_bar(stat="identity",width=0.001,color=grey(0.5),fill=grey(0.5)) + 
  ylab("Dollars Per-Person Per-Month")+
  xlab("")+
  geom_vline(data=thresh,aes(xintercept=chg_rank_dist)) + 
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
        axis.text.x = element_text(size=16))
dev.off()

big_mergers = merger_welfare[dHHI>500]
big_mergers[,chg_rank:= rank(chg_Tot_Welfare,ties.method="first"),by=policy]
# big_mergers[,dMarkup:=Tot_Welfare_merge_cp - Welfare_baseline_CP]
# big_mergers[,dSorting:=chg_Tot_Welfare - dMarkup]

## Smooth out non-ordered variables
big_mergers[,chg_rank_bucket:=floor(chg_rank/10)]
big_mergers[,chg_rank_dist:=chg_rank/max(chg_rank),by="policy"]

big_mergers[,sorting_cost_smooth:=mean(sorting_cost,na.rm=TRUE),by=c("chg_rank_bucket","policy")]
big_mergers[,dWelfare_smooth:=mean(chg_Tot_Welfare,na.rm=TRUE),by=c("chg_rank_bucket","policy")]
big_mergers[,hhi_smooth:=mean(hhi,na.rm=TRUE),by=c("chg_rank_bucket","policy")]


plot_df1 = big_mergers[,c("chg_rank_bucket","policy","chg_Tot_Welfare","chg_rank_dist","policy_label")]
names(plot_df1)[3] = "value"
plot_df1[,label:="Total Welfare Effect"]
plot_df2 = big_mergers[,c("chg_rank_bucket","policy","hhi_smooth","chg_rank_dist","policy_label")]
names(plot_df2)[3] = "value"
plot_df2[,label:="Pre-Merger HHI"]
plot_df3 = big_mergers[,c("chg_rank_bucket","policy","sorting_cost","chg_rank_dist","policy_label")]
names(plot_df3)[3] = "value"
plot_df3[,label:="Sorting Cost"]
# plot_df4 = big_mergers[,c("sorting_cost_smooth","policy","dSorting_smooth")]
# names(plot_df4)[3] = "value"
# plot_df4[,label:="Change due to Sorting"]
plotdf = rbind(plot_df1,plot_df2,plot_df3)#,plot_df3,plot_df4)

plotdf[,label:= factor(label,levels=c("Total Welfare Effect","Pre-Merger HHI","Sorting Cost"))]#,"Change due to Sorting","Change due to Markups"))]
plotdf[,facet:=label]
plotdf[label%in%c("Change due to Sorting","Change due to Markups"),facet:="Decomposition"]
plotdf[,color:=label]
plotdf[!label%in%c("Change due to Sorting","Change due to Markups"),color:=NA]


plot_df1[,cross:=min(abs(value)),by="policy"]
thresh = plot_df1[abs(value)==cross]
setkey(plotdf,chg_rank_dist)

png("Writing/Images/WelfareDist_dHHI_GT500.png",width=2500,height=1500,res=275)
ggplot(plotdf[label%in%c("Total Welfare Effect")]) + 
  facet_wrap(~policy_label,ncol=1,scales="free") + 
  aes(x=chg_rank_dist,y=value) + geom_bar(stat="identity",width=0.001,color=grey(0.5),fill=grey(0.5)) + 
  ylab("Dollars Per-Person Per-Month")+
  xlab("")+
  geom_vline(data=thresh,aes(xintercept=chg_rank_dist)) + 
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
        axis.text.x = element_text(size=16))
dev.off()

# png("Writing/Images/RAManWelfareSorting.png",width=2500,height=1500,res=275)
# ggplot(plotdf[policy=="RA"&label%in%c("Total Welfare Effect","Sorting Cost")]) +
#   aes(x=sorting_rank,y=value) + geom_bar(stat="identity",color=grey(0.5),fill=grey(0.5)) + 
#   facet_wrap(~label,ncol=1,scales="free")  + 
#   ylab("Dollars Per-Person Per-Month")+
#   xlab("")+
#   theme(strip.background = element_blank(),
#         strip.text = element_text(size=14),
#         legend.background = element_rect(color=grey(.5)),
#         legend.title = element_blank(),
#         legend.text = element_text(size=14),
#         legend.key.width = unit(.05,units="npc"),
#         legend.key = element_rect(color="transparent",fill="transparent"),
#         legend.position = "bottom",
#         axis.title=element_text(size=14),
#         axis.text.y = element_text(size=16),
#         axis.text.x = element_blank())
# dev.off()


# png("Writing/Images/RAManWelfareDecomp.png",width=2500,height=1500,res=275)
# ggplot(plotdf[policy=="RAMan"&label%in%c("Total Welfare Effect","Change due to Sorting","Change due to Markups")]) + 
#   aes(x=chg_rank,y=value,fill=color,color=color) + geom_bar(stat="identity",position="dodge") + 
#   facet_wrap(~facet,ncol=1)  +  
#   scale_fill_discrete(limits = c("Change due to Sorting","Change due to Markups")) +
#   scale_color_discrete(limits = c("Change due to Sorting","Change due to Markups")) +
#   ylab("Dollars Per-Person Per-Month")+
#   xlab("")+
#   theme(strip.background = element_blank(),
#         strip.text = element_text(size=14),
#         legend.background = element_rect(color=grey(.5)),
#         legend.title = element_blank(),
#         legend.text = element_text(size=14),
#         legend.key.width = unit(.05,units="npc"),
#         legend.key = element_rect(color="transparent",fill="transparent"),
#         legend.position = "bottom",
#         axis.title=element_text(size=14),
#         axis.text.y = element_text(size=16),
#         axis.text.x = element_blank())
# dev.off()


# png("Writing/Images/BaseWelfareDist.png",width=2500,height=1500,res=275)
# ggplot(plotdf[policy=="Base"&label%in%c("Total Welfare Effect")]) + 
#   aes(x=chg_rank,y=value) + geom_bar(stat="identity",color=grey(0.5),fill=grey(0.5)) + 
#   ylab("Dollars Per-Person Per-Month")+
#   xlab("")+
#   theme(strip.background = element_blank(),
#         strip.text = element_text(size=14),
#         legend.background = element_rect(color=grey(.5)),
#         legend.title = element_blank(),
#         legend.text = element_text(size=14),
#         legend.key.width = unit(.05,units="npc"),
#         legend.key = element_rect(color="transparent",fill="transparent"),
#         legend.position = "bottom",
#         axis.title=element_text(size=14),
#         axis.text.y = element_text(size=16),
#         axis.text.x = element_blank())
# dev.off()

# png("Writing/Images/BaseWelfareSorting.png",width=2500,height=1500,res=275)
# ggplot(plotdf[policy=="Base"&label%in%c("Total Welfare Effect","Pre-Merger Cost of Inefficient Sorting")]) +
#   aes(x=chg_rank,y=value) + geom_bar(stat="identity",color=grey(0.5),fill=grey(0.5)) + 
#   facet_wrap(~label,ncol=1,scales="free")  + 
#   ylab("Dollars Per-Person Per-Month")+
#   xlab("")+
#   theme(strip.background = element_blank(),
#         strip.text = element_text(size=14),
#         legend.background = element_rect(color=grey(.5)),
#         legend.title = element_blank(),
#         legend.text = element_text(size=14),
#         legend.key.width = unit(.05,units="npc"),
#         legend.key = element_rect(color="transparent",fill="transparent"),
#         legend.position = "bottom",
#         axis.title=element_text(size=14),
#         axis.text.y = element_text(size=16),
#         axis.text.x = element_blank())
# dev.off()


# png("Writing/Images/BaseWelfareDecomp.png",width=2500,height=1500,res=275)
# ggplot(plotdf[policy=="Base"&label%in%c("Total Welfare Effect","Change due to Sorting","Change due to Markups")]) + 
#   aes(x=chg_rank,y=value,fill=color,color=color) + geom_bar(stat="identity",position="dodge") + 
#   facet_wrap(~facet,ncol=1)  +  
#   scale_fill_discrete(limits = c("Change due to Sorting","Change due to Markups")) +
#   scale_color_discrete(limits = c("Change due to Sorting","Change due to Markups")) +
#   ylab("Dollars Per-Person Per-Month")+
#   xlab("")+
#   theme(strip.background = element_blank(),
#         strip.text = element_text(size=14),
#         legend.background = element_rect(color=grey(.5)),
#         legend.title = element_blank(),
#         legend.text = element_text(size=14),
#         legend.key.width = unit(.05,units="npc"),
#         legend.key = element_rect(color="transparent",fill="transparent"),
#         legend.position = "bottom",
#         axis.title=element_text(size=14),
#         axis.text.y = element_text(size=16),
#         axis.text.x = element_blank())
# dev.off()









merger_plot = merge(merger_effects,merger_welfare[,c("markets","policy","merging_parties","chg_Tot_Welfare")],by=c("markets","policy","merging_parties"))

merger_plot[,welf_effect:="Negative Welfare Effect"]
merger_plot[chg_Tot_Welfare>0,welf_effect:="Positive Welfare Effect"]

png("Writing/Images/BasePriceEffect.png",width=2500,height=1500,res=275)
ggplot(merger_plot[policy=="Base"&parties_indicator==1]) + 
  facet_wrap(~welf_effect,ncol=1,scales="free") +
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
    axis.text = element_text(size=12))
dev.off()

png("Writing/Images/BasePriceEffect_Positive.png",width=2500,height=1500,res=275)
ggplot(merger_plot[policy=="Base"&parties_indicator==1&chg_Tot_Welfare>0]) + 
  # facet_wrap(~welf_effect,ncol=1,scales="free") +
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
    axis.text = element_text(size=12))
dev.off()

png("Writing/Images/BasePriceEffect_Negative.png",width=2500,height=1500,res=275)
ggplot(merger_plot[policy=="Base"&parties_indicator==1&chg_Tot_Welfare<0]) + 
  facet_wrap(~welf_effect,ncol=1,scales="free") +
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
    axis.text = element_text(size=12))
dev.off()


png("Writing/Images/RAPriceEffect.png",width=2500,height=1500,res=275)
ggplot(merger_plot[policy=="RA"&parties_indicator==1]) + 
  facet_wrap(~welf_effect,ncol=1,scales="free") +
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
    axis.text = element_text(size=12))
dev.off()

png("Writing/Images/RAPriceEffect_Positive.png",width=2500,height=1500,res=275)
ggplot(merger_plot[policy=="RA"&parties_indicator==1&chg_Tot_Welfare>0]) + 
  # facet_wrap(~welf_effect,ncol=1,scales="free") +
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
    axis.text = element_text(size=12))
dev.off()

png("Writing/Images/RAPriceEffect_Negative.png",width=2500,height=1500,res=275)
ggplot(merger_plot[policy=="RA"&parties_indicator==1&chg_Tot_Welfare<0]) + 
  facet_wrap(~welf_effect,ncol=1,scales="free") +
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
    axis.text = element_text(size=12))
dev.off()
