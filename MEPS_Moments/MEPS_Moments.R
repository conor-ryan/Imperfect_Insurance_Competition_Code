rm(list=ls())
library(doBy)
library(data.table)
library(ggplot2)
library(Hmisc)
if (!grepl("Imperfect_Insurance_Competition",getwd())){
  setwd("C:/Users/cxr5626/Dropbox/Research/Imperfect_Insurance_Competition")
}
#### Read in MEPS Data ####
mepsFull = read.csv("Data/2015_MEPS/MEPS_Full_2015.csv")


#### Distribution of Risk Scores  ####
meps = mepsFull[,c("HIEUIDX","DUPERSID","PID","PANEL","AGELAST","AGE15X","TTLP15X","POVLEV15","OFFER31X","OFFER42X","OFFER53X",
                   "TRIEV15","MCREV15","MCDEV15","OPAEV15","OPBEV15","ADSMOK42","REGION15",
                  "UNINS15","INSCOV15","INSURC15","PERWT15F")]

meps_risk = read.csv("Intermediate_Output/MEPS_Moments/meps_risk_scores.csv")

meps = merge(meps,meps_risk,by=c("DUPERSID","PANEL"))#,all.x=TRUE)
# Subset on Region
# meps = meps[meps$REGION15!=1,]
# meps = as.data.table(meps)


# 
mepsPers = read.csv("Data/2015_MEPS/MEPS_Person_2015.csv")
## Non Group Coverage
mepsPers = mepsPers[mepsPers$PRIVCAT%in%c(2,3,5,6,99),]
# #Not Through Employer or Association
mepsPers = mepsPers[mepsPers$CMJINS!=1,]
mepsPers = mepsPers[mepsPers$TYPEFLAG%in%c(5,6,7,11,12,13,21),]

mepsPers = mepsPers[mepsPers$STEXCH!=-1,]

mepsPers = summaryBy(STEXCH~DUPERSID+PANEL,data=mepsPers,FUN=min,keep.names=TRUE)

meps = merge(meps,mepsPers,by=c("DUPERSID","PANEL"),all.x=TRUE)

# 
# ## Keep Non-Group Only
meps = meps[meps$AGE15X<65,] # Under 65
meps=meps[meps$AGE15X>=0,]
# meps = meps[!is.na(meps$STEXCH)|meps$INSCOV15==3,] #Non Group or Uninsured
# meps = meps[meps$INSCOV15!=3,]
# 
# ## Age of HoH
meps$HoH_Age = ave(meps$AGE15X,meps$HIEUIDX,FUN=max)
meps$Count = 1
meps$HoH_Count = ave(meps$Count,meps$HIEUIDX,FUN=sum)
# 
# 
meps$Age_Cat = 0
meps$Age_Cat[meps$HoH_Age>45] = 1

meps$Inc_Cat = 0
meps$Inc_Cat[meps$POVLEV15>=400] = 1



score_vars = names(meps)[grep("HCC_Score",names(meps))]
for (var in score_vars){
  wtd_var = paste("wtd",var,sep="_")
  meps[[wtd_var]] = meps[[var]]*meps$PERWT15F
}

f = as.formula(paste(paste(names(meps)[grep("wtd_HCC_Score|PERWT15F",names(meps))],collapse="+"),"Age_Cat+Inc_Cat",sep="~"))

meps_dist = meps[with(meps,HCC_Score_Silver>0),]

moments = summaryBy(f,data=meps_dist,FUN=sum,keep.names=TRUE)


score_vars = names(moments)[grep("wtd_HCC_Score",names(moments))]
for (var in score_vars){
  moments[[var]] = moments[[var]]/moments$PERWT15F
}

names(moments) = gsub("wtd_","mean_",names(moments))

for (m in c("Platinum","Gold","Silver","Bronze","Catastrophic")){
  var = paste("HCC_Score",m,sep="_")
  v_var = paste("var_HCC_Score",m,sep="_")
  moments[[v_var]] = wtd.var(meps_dist[[var]],weights=meps_dist$PERWT15F)
}

for (m in c("Platinum","Gold","Silver","Bronze","Catastrophic")){
  nm_var = paste("norm_mean_HCC_Score",m,sep="_")
  nv_var = paste("norm_var_HCC_Score",m,sep="_")
  
  m_var = paste("mean_HCC_Score",m,sep="_")
  v_var = paste("var_HCC_Score",m,sep="_")
  
  mean_X2 = moments[[m_var]]^2
  var_X = moments[[v_var]]

  
  moments[[nm_var]] = log(mean_X2/(sqrt(mean_X2+var_X)))
  moments[[nv_var]] = log(1+var_X/mean_X2)
}

# ### 95th Percentile Truncated Mean
# for (m in c("Platinum","Gold","Silver","Bronze","Catastrophic")){
#   tm_var = paste("trunc_mean_HCC_Score",m,sep="_")
#   
#   nm_var = paste("norm_mean_HCC_Score",m,sep="_")
#   nv_var = paste("norm_var_HHC_Score",m,sep="_")
#   
#   norm_mean = moments[[nm_var]]
#   norm_std = sqrt(moments[[nv_var]])
#   
#   a = exp(qnorm(0.999,mean=norm_mean,sd=norm_std))
#   
#   exp(norm_mean + norm_std^2/2)*pnorm(norm_std-(log(a)-norm_mean)/norm_std)/(1-pnorm((log(a)-norm_mean)/norm_std))
# }


meps$Any_HCC = as.numeric(meps$HCC_Score_Silver>0)*meps$PERWT15F
means_any = summaryBy(Any_HCC+PERWT15F~Age_Cat+Inc_Cat,data=meps,FUN=sum,keep.names=TRUE)
means_any$Any_HCC = with(means_any,Any_HCC/PERWT15F)

moments = merge(moments,means_any,by=c("Age_Cat","Inc_Cat"))
moments = moments[,grep("Cat$|norm|Any",names(moments))]
names(moments) = gsub("Score_","",names(moments))
names(moments) = gsub("norm_","",names(moments))



# ##############
# score_vars = names(meps)[grep("R_Score|HCC_Score",names(meps))]
# for (var in score_vars){
#   wtd_log_var = paste("wtd_log",var,sep="_")
#   log_var = paste("log",var,sep="_")
#   meps[[log_var]] = log(meps[[var]])
#   meps[[wtd_log_var]] = log(meps[[var]])*meps$PERWT15F
# }
# 
# 
# f = as.formula(paste(paste(names(meps)[grep("wtd_log_HCC_Score|PERWT15F",names(meps))],collapse="+"),"Age_Cat+Inc_Cat",sep="~"))
# 
# meps_dist = meps[with(meps,HCC_Score_Silver>0),]
# #means = summaryBy(f,data=meps_dist,FUN=mean)
# 
# means = summaryBy(f,data=meps_dist,FUN=sum,keep.names=TRUE)
# 
# 
# score_vars = names(means)[grep("wtd_log_R_Score|wtd_log_HCC_Score",names(means))]
# for (var in score_vars){
#   means[[var]] = means[[var]]/means$PERWT15F
# }
# 
# # meps$any_HCC = as.numeric(meps$HCC_Score_Silver>0)*meps$PERWT15F
# # means_any = summaryBy(any_HCC+PERWT15F~Age_Cat+Inc_Cat,data=meps,FUN=sum,keep.names=TRUE)
# # means_any$any_HCC = with(means_any,any_HCC/PERWT15F)
# 
# moments2 = merge(means,means_any,by=c("Age_Cat","Inc_Cat"))
# 
# 
# for (m in c("Platinum","Gold","Silver","Bronze","Catastrophic")){
#   var = paste("log_HCC_Score",m,sep="_")
#   v_var = paste("var_HHC_Score",m,sep="_")
#   moments2[[v_var]] = wtd.var(meps_dist[[var]],weights=meps_dist$PERWT15F)
# }
# 
# moments = moments[,-grep("PERWT15F",names(moments))]
# 
# names(moments) = c("Age_Cat","Inc_Cat","mean_HCC_Platinum","mean_HCC_Gold","mean_HCC_Silver","mean_HCC_Bronze","mean_HCC_Catastrophic",
#                   "Any_HCC","var_HCC_Platinum","var_HCC_Gold","var_HCC_Silver","var_HCC_Bronze","var_HCC_Catastrophic")
write.csv(moments,"Intermediate_Output/MEPS_Moments/R_Score_Moments.csv",row.names=FALSE)


#### Illustrate Fit####
meps = as.data.table(meps)
meps[,Any_HCC:=NULL]
meps = merge(meps,moments,by=c("Age_Cat","Inc_Cat"),all.x=TRUE)
meps[,riskDraw:= runif(nrow(meps))]
meps[,draws_Any:=(riskDraw-(1-Any_HCC))/(Any_HCC)]
meps[draws_Any<0,draws_Any:=0]

meps[,HCC_pred_Silver:=exp(qnorm(draws_Any)*sqrt(var_HCC_Silver) + mean_HCC_Silver)]

## Positive Plot (Unncessary)
# meps[,pos_label:="Zero"]
# meps[draws_Any>0,pos_label:="Positive"]
# meps[,pos_label:=factor(pos_label,levels=c("Zero","Positive"))]
# pos_plot1 = meps[,list(population=sum(PERWT15F),type="Model"),by=pos_label]
# 
# meps[,pos_label:="Zero"]
# meps[HCC_Score_Silver>0,pos_label:="Positive"]
# meps[,pos_label:=factor(pos_label,levels=c("Zero","Positive"))]
# pos_plot2 = meps[,list(population=sum(PERWT15F),type="Data"),by=pos_label]
# 
# pos_plot = rbind(pos_plot1,pos_plot2)
# pos_plot[,prob:=population/sum(population),by="type"]

## Distribution of Positive Scores
meps[,risk_score_bucket:=ceiling(HCC_pred_Silver/0.5)*0.5]
meps[risk_score_bucket>20,risk_score_bucket:=20]
dist1 = meps[HCC_pred_Silver>0,list(population=sum(PERWT15F),type="Model"),by=risk_score_bucket]
meps[,risk_score_bucket:=ceiling(HCC_Score_Silver/0.5)*0.5]
meps[risk_score_bucket>20,risk_score_bucket:=20]
dist2 = meps[HCC_Score_Silver>0,list(population=sum(PERWT15F),type="Data"),by=risk_score_bucket]
dist_plot = rbind(dist1,dist2)

dist_plot[,density:=population/sum(population),by="type"]
save(dist_plot,file="Intermediate_Output/MEPS_Moments/risk_dist.rData")

png("Writing/Images/RiskDist_Fit_Positive.png",width=2500,height=1500,res=275)
ggplot(dist_plot) + aes(x=risk_score_bucket,y=density) + 
  geom_bar(stat="identity",position="identity",alpha=0.75) + 
  ylab("Density") +
  xlab("Risk Score") +
  facet_wrap(~type,ncol=1) + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "right",
    axis.title=element_text(size=14),
    axis.text = element_text(size=14))
dev.off()

#### Risk - Cost Moments ####
meps = mepsFull[mepsFull$UNINS15==2,c("HIEUIDX","DUPERSID","PID","PANEL","AGELAST","AGE15X","TTLP15X","POVLEV15","OFFER31X","OFFER42X","OFFER53X",
                   "TRIEV15","MCREV15","MCDEV15","OPAEV15","OPBEV15","ADSMOK42","REGION15",
                   "UNINS15","INSCOV15","INSURC15","TOTEXP15","TOTPRV15","PERWT15F")]

meps_risk = read.csv("Intermediate_Output/MEPS_Moments/meps_risk_scores.csv")

meps = merge(meps,meps_risk,by=c("DUPERSID","PANEL"))#,all.x=TRUE)

# mepsPers = read.csv("Data/2015_MEPS/MEPS_Person_2015.csv")
# ## Non Group Coverage
# mepsPers = mepsPers[mepsPers$PRIVCAT%in%c(2,3,5,6,99),]
# # #Not Through Employer or Association
# mepsPers = mepsPers[mepsPers$CMJINS!=1,]
# mepsPers = mepsPers[mepsPers$TYPEFLAG%in%c(5,6,7,11,12,13,21),]
# #
# # mepsPers = mepsPers[mepsPers$STEXCH!=-1,]
# #
# # mepsPers = summaryBy(STEXCH~DUPERSID+PANEL,data=mepsPers,FUN=min,keep.names=TRUE)
# #
# meps = merge(meps,mepsPers,by=c("DUPERSID","PANEL"))#,all.x=TRUE)
# meps = merge(meps,meps_risk,by=c("DUPERSID","PANEL"))#,all.x=TRUE)
# 
# ## Keep Non-Group Only
# meps = meps[!is.na(meps$STEXCH),]


# Subset on 0 - 65, Insured
meps = meps[meps$AGE15X>=0,]
meps = meps[meps$AGE15X<66,]
# meps = meps[meps$REGION15!=1,]
meps = as.data.table(meps)

meps$HoH_Age = ave(meps$AGE15X,meps$HIEUIDX,FUN=max)
meps$HoH_AvgAge = ave(meps$AGE15X,meps$HIEUIDX,FUN=mean)
meps$Age_Cat = 0
meps$Age_Cat[meps$HoH_Age>45] = 1

## Non-Zero Average Cost
meps[,HCC_positive:=0]
meps[HCC_Score_Silver>0,HCC_positive:=1]
meps[,sample_count:=1]

riskMoments = meps[,list(avgCost=sum(PERWT15F*TOTPRV15)/sum(PERWT15F)/12),by="HCC_positive"]
riskMoments[,costIndex:=avgCost/min(avgCost)]
setkey(riskMoments,costIndex)


write.csv(riskMoments,file="Intermediate_Output/MEPS_Moments/riskMoments.csv",row.names=FALSE)

#### Age - Cost Moments

meps[,Age_Bin:= floor(HoH_AvgAge/5)*5]
meps[HoH_AvgAge<20,Age_Bin:=20]
ageMoments = meps[HoH_Age>=18&Age_Bin<65,list(avgCost=sum(PERWT15F*TOTPRV15)/sum(PERWT15F),
                                               sample_size=sum(sample_count)),
                   by=c("Age_Bin")]
ageMoments[,costIndex:=avgCost/min(avgCost)]
setkey(ageMoments,Age_Bin)

ageMoments_noHCC = meps[HCC_positive==0&HoH_Age>=18&Age_Bin<65,list(avgCost=sum(PERWT15F*TOTPRV15)/sum(PERWT15F)),by=c("Age_Bin")]
ageMoments_noHCC[,costIndex:=avgCost/min(avgCost)]
setkey(ageMoments_noHCC,Age_Bin)

write.csv(ageMoments,file="Intermediate_Output/MEPS_Moments/ageMoments.csv",row.names=FALSE)
write.csv(ageMoments_noHCC,file="Intermediate_Output/MEPS_Moments/ageMoments_noHCC.csv",row.names=FALSE)



