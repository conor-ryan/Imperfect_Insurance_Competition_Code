rm(list=ls())
library(doBy)
library(data.table)
library(ggplot2)
library(Hmisc)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

#### Read in MEPS Data ####
mepsFull = read.csv("Data/2015_MEPS/MEPS_Full_2015.csv")

# #### Age - Cost Moments ####
# ins = as.data.table(mepsFull[mepsFull$UNINS15==2,c("HIEUIDX","PID","AGELAST","SEX","TTLP15X","OFFER31X","OFFER42X","OFFER53X",
#                                           "TRIEV15","MCREV15","MCDEV15","OPAEV15","OPBEV15","ADSMOK42",
#                                           "INSCOV15","INSURC15","TOTEXP15","PERWT15F")])
# 
# ins[,Age_Bin:= floor(AGELAST/5)*5]
# ins[AGELAST>=18&Age_Bin==15,Age_Bin:=20]
# ageMoments = ins[Age_Bin>=20&Age_Bin<65,list(avgCost=sum(PERWT15F*TOTEXP15)/sum(PERWT15F)),by=c("Age_Bin")]
# ageMoments[,costIndex:=avgCost/min(avgCost)]
# setkey(ageMoments,Age_Bin)
# write.csv(ageMoments,file="Intermediate_Output/MEPS_Moments/ageMoments.csv",row.names=FALSE)



#### Distribution of Risk Scores  ####
meps = mepsFull[,c("HIEUIDX","DUPERSID","PID","PANEL","AGELAST","AGE15X","TTLP15X","POVLEV15","OFFER31X","OFFER42X","OFFER53X",
                   "TRIEV15","MCREV15","MCDEV15","OPAEV15","OPBEV15","ADSMOK42",
                   "INSCOV15","INSURC15","PERWT15F")]

meps_risk = read.csv("Intermediate_Output/MEPS_Moments/meps_risk_scores.csv")
mepsPers = read.csv("Data/2015_MEPS/MEPS_Person_2015.csv")
## Non Group Coverage
mepsPers = mepsPers[mepsPers$PRIVCAT%in%c(2,3,5,6,99),]
# #Not Through Employer or Association 
mepsPers = mepsPers[mepsPers$CMJINS!=1,]
mepsPers = mepsPers[mepsPers$TYPEFLAG%in%c(5,6,7,11,12,13,21),]

mepsPers = mepsPers[mepsPers$STEXCH!=-1,]

mepsPers = summaryBy(STEXCH~DUPERSID+PANEL,data=mepsPers,FUN=min,keep.names=TRUE)

meps = merge(meps,mepsPers,by=c("DUPERSID","PANEL"),all.x=TRUE)
meps = merge(meps,meps_risk,by=c("DUPERSID","PANEL"),all.x=TRUE)

## Keep Non-Group Only
meps = meps[meps$AGE15X<66,]
meps = meps[!is.na(meps$STEXCH),]

## Age of HoH
meps$HoH_Age = ave(meps$AGE15X,meps$HIEUIDX,FUN=max)
meps$Count = 1
meps$HoH_Count = ave(meps$Count,meps$HIEUIDX,FUN=sum)
# 
# 
# meps$EmpOffer = with(meps,OFFER31X==1|OFFER42X==1|OFFER53X==1)
# meps$PrivateCov = with(meps,INSCOV15==1&TRIEV15==2&MCREV15==2&MCDEV15==2&OPAEV15==2&OPBEV15==2)
# meps$Uninsured = with(meps,INSCOV15==3)
# meps$InMarket = ((meps$PrivateCov&!meps$EmpOffer)|meps$Uninsured)&meps$AGELAST<65
# meps$Smoking = with(meps,as.numeric(ADSMOK42==1))
# #meps = meps[meps$InMarket,]
# meps = meps[!meps$Uninsured&meps$AGE15X<66,]
meps=meps[meps$AGE15X>=0,]

meps$Age_Cat = 0
meps$Age_Cat[meps$HoH_Age>45] = 1

meps$Inc_Cat = 0
meps$Inc_Cat[meps$POVLEV15>=400] = 1


ggplot(meps) + 
  aes(x=HCC_Score_Silver,weights=PERWT15F,y=..density..) + 
  geom_histogram()


ggplot(meps) + 
  aes(x=log(HCC_Score_Silver+.001),weights=PERWT15F,y=..density..) + 
  geom_histogram() + 
  facet_grid(Inc_Cat~Age_Cat) 

score_vars = names(meps)[grep("R_Score|HCC_Score",names(meps))]
for (var in score_vars){
  wtd_log_var = paste("wtd_log",var,sep="_")
  log_var = paste("log",var,sep="_")
  meps[[log_var]] = log(meps[[var]])
  meps[[wtd_log_var]] = log(meps[[var]])*meps$PERWT15F
}


f = as.formula(paste(paste(names(meps)[grep("wtd_log_HCC_Score|PERWT15F",names(meps))],collapse="+"),"Age_Cat+Inc_Cat",sep="~"))

meps_dist = meps[with(meps,HCC_Score_Silver>0),]
#means = summaryBy(f,data=meps_dist,FUN=mean)

means = summaryBy(f,data=meps_dist,FUN=sum,keep.names=TRUE)


score_vars = names(means)[grep("wtd_log_R_Score|wtd_log_HCC_Score",names(means))]
for (var in score_vars){
  means[[var]] = means[[var]]/means$PERWT15F
}

meps$any_HCC = as.numeric(meps$HCC_Score_Silver>0)*meps$PERWT15F
means_any = summaryBy(any_HCC+PERWT15F~Age_Cat+Inc_Cat,data=meps,FUN=sum,keep.names=TRUE)
means_any$any_HCC = with(means_any,any_HCC/PERWT15F)

moments = merge(means,means_any,by=c("Age_Cat","Inc_Cat"))


for (m in c("Platinum","Gold","Silver","Bronze","Catastrophic")){
  var = paste("log_HCC_Score",m,sep="_")
  v_var = paste("var_HHC_Score",m,sep="_")
  moments[[v_var]] = wtd.var(meps_dist[[var]],weights=meps_dist$PERWT15F)
}

moments = moments[,-grep("PERWT15F",names(moments))]
names(moments) = c("Age_Cat","Inc_Cat","mean_HCC_Platinum","mean_HCC_Gold","mean_HCC_Silver","mean_HCC_Bronze","mean_HCC_Catastrophic",
                  "Any_HCC","var_HCC_Platinum","var_HCC_Gold","var_HCC_Silver","var_HCC_Bronze","var_HCC_Catastrophic")
write.csv(moments,"Intermediate_Output/MEPS_Moments/R_Score_Moments.csv",row.names=FALSE)


#### Risk - Cost Moments ####
meps = mepsFull[mepsFull$UNINS15==2,c("HIEUIDX","DUPERSID","PID","PANEL","AGELAST","AGE15X","TTLP15X","POVLEV15","OFFER31X","OFFER42X","OFFER53X",
                   "TRIEV15","MCREV15","MCDEV15","OPAEV15","OPBEV15","ADSMOK42",
                   "UNINS15","INSCOV15","INSURC15","TOTEXP15","PERWT15F")]

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
meps = meps[meps$UNINS15==2,]
meps = as.data.table(meps)

meps$HoH_Age = ave(meps$AGE15X,meps$HIEUIDX,FUN=max)
meps$Age_Cat = 0
meps$Age_Cat[meps$HoH_Age>45] = 1

## Non-Zero Average Cost
meps[,HCC_positive:=0]
meps[HCC_Score_Silver>0,HCC_positive:=1]
meps[,sample_count:=1]

riskMoments = meps[,list(avgCost=sum(PERWT15F*TOTEXP15)/sum(PERWT15F)),by="HCC_positive"]
riskMoments[,costIndex:=avgCost/min(avgCost)]
setkey(riskMoments,costIndex)


write.csv(riskMoments,file="Intermediate_Output/MEPS_Moments/riskMoments.csv",row.names=FALSE)

#### Age - Cost Moments

meps[,Age_Bin:= floor(AGELAST/5)*5]
meps[AGELAST>=18&Age_Bin==15,Age_Bin:=20]
ageMoments = meps[Age_Bin>=20&Age_Bin<65,list(avgCost=sum(PERWT15F*TOTEXP15)/sum(PERWT15F),
                                               sample_size=sum(sample_count)),
                   by=c("Age_Bin")]
ageMoments[,costIndex:=avgCost/min(avgCost)]
setkey(ageMoments,Age_Bin)

meps[,Age_Bin:= floor(AGELAST/5)*5]
meps[AGELAST>=18&Age_Bin==15,Age_Bin:=20]
ageMoments_noHCC = meps[HCC_positive==0&Age_Bin>=20&Age_Bin<65,list(avgCost=sum(PERWT15F*TOTEXP15)/sum(PERWT15F)),by=c("Age_Bin")]
ageMoments_noHCC[,costIndex:=avgCost/min(avgCost)]
setkey(ageMoments_noHCC,Age_Bin)

write.csv(ageMoments,file="Intermediate_Output/MEPS_Moments/ageMoments.csv",row.names=FALSE)
write.csv(ageMoments_noHCC,file="Intermediate_Output/MEPS_Moments/ageMoments_noHCC.csv",row.names=FALSE)



