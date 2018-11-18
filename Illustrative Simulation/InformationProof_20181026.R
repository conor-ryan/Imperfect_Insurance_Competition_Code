rm(list=ls())
library(doBy)
library(randtoolbox)
library(data.table)
library(scales)
library(ggplot2)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition")

## Run
run = "2018-08-25"

#### 2015 Subsidy Percentage Function ####

subsPerc <- function(FPL){
  x = FPL[!is.na(FPL)]
  y = rep(100,length(x))
  y[x>=1&x<1.33]   = 2.01 + (x-1)[x>=1&x<1.33]/(1.33-1)*(3.02-2.01)
  y[x>=1.33&x<1.5] = 3.02 + (x-1.33)[x>=1.33&x<1.5]/(1.5-1.33)*(4.02-3.02)
  y[x>=1.5&x<2]    = 4.02 + (x-1.5)[x>=1.5&x<2]/(2-1.5)*(6.34-4.02)
  y[x>=2&x<2.5]    = 6.34 + (x-2)[x>=2&x<2.5]/(2.5-2)*(8.1-6.34)
  y[x>=2.5&x<3]    = 8.1 + (x-2.5)[x>=2.5&x<3]/(3-2.5)*(9.56-8.1)
  y[x>=3&x<=4]      = 9.56
  
  HHcont = rep(NA,length(FPL))
  HHcont[!is.na(FPL)] = y/100
  
  return(HHcont)
}


#### Read in  ACS Exchange Elligible Data ####
acs = read.csv("Data/2015_ACS/exchangePopulation2015.csv")
acs = as.data.table(acs)
setkey(acs,STATEFIP,PUMA)
#Uninsured Rate
with(acs,sum(uninsured*PERWT)/sum(PERWT))
acs$person = rownames(acs)

#### Match PUMA to Rating Area ####
acs = acs[,c("household","HHincomeFPL","HH_income","AGE","SEX","PERWT")]
names(acs) = c("household","HHincomeFPL","HH_income","AGE","SEX","PERWT")

#### Rating Curves ####
rating = read.csv("Data/AgeRating.csv")
rating = as.data.table(rating)
# Create truncated Age variable
acs$AgeMatch = acs$AGE
acs$AgeMatch[acs$AGE<14] = 14
acs$AgeMatch[acs$AGE>64] = 64

# Merge in Default Age Rating Curves - Ignore State Specific Effects For Now
acs = merge(acs,rating[rating$State=="Default",c("Age","Rating")],by.x="AgeMatch",by.y="Age",all.x=TRUE)
#acs = merge(acs,rating[rating$State!="Default",],by.x=c("ST","AgeMatch"),by.y=c("State","Age"),all.x=TRUE)
acs$ageRate = acs$Rating
#acs$ageRate[!is.na(acs$Rating.y)] = acs$Rating.y[!is.na(acs$Rating.y)]
# Drop redundant rating variables
acs = acs[,c("Rating"):=NULL]
rm(rating)

# Merge in Age-specific HHS-HCC Risk Adjustment Factors
HCC = read.csv("Risk_Adjustment/2014_HHS_HCC_AgeRA_Coefficients.csv")
names(HCC) = c("Sex","Age","PlatHCC_Age","GoldHCC_Age","SilvHCC_Age","BronHCC_Age","CataHCC_Age")
acs[,AgeMatch:= pmax(floor(AGE/5)*5,21)]
acs = merge(acs,HCC,by.x=c("AgeMatch","SEX"),by.y=c("Age","Sex"))

#### Household Characteristics ####

#Count Members
setkey(acs,household)
acs$MEMBERS=1
#Age of HoH
acs[,MaxAge:=max(AGE),by="household"]
# Drop heads of household that are under 18 - 2,041
acs = acs[MaxAge>=18,]

#Count Children
acs[,childRank:=rank(AGE,ties.method="first"),by="household"]
acs$childRank[acs$AGE>18] = NA
acs$ageRate[!is.na(acs$childRank)&acs$childRank>3]=0

acs$catas_cnt = as.numeric(acs$AGE<=30)
acs$ageRate_avg = acs$ageRate*acs$PERWT

acs[,PlatHCC_Age:=PlatHCC_Age*PERWT]
acs[,GoldHCC_Age:=GoldHCC_Age*PERWT]
acs[,SilvHCC_Age:=SilvHCC_Age*PERWT]
acs[,BronHCC_Age:=BronHCC_Age*PERWT]
acs[,CataHCC_Age:=CataHCC_Age*PERWT]


acs = acs[,lapply(.SD,sum),by=c("household","HHincomeFPL","HH_income","MaxAge"),
          .SDcols = c("MEMBERS","ageRate","ageRate_avg","PERWT","catas_cnt",
                      "PlatHCC_Age","GoldHCC_Age","SilvHCC_Age","BronHCC_Age","CataHCC_Age")]

names(acs) = c("household","HHincomeFPL","HH_income","AGE","MEMBERS","ageRate","ageRate_avg","PERWT","catas_cnt",
               "PlatHCC_Age","GoldHCC_Age","SilvHCC_Age","BronHCC_Age","CataHCC_Age")
acs$ageRate_avg = with(acs,ageRate_avg/PERWT)
acs[,PlatHCC_Age:=PlatHCC_Age/PERWT]
acs[,GoldHCC_Age:=GoldHCC_Age/PERWT]
acs[,SilvHCC_Age:=SilvHCC_Age/PERWT]
acs[,BronHCC_Age:=BronHCC_Age/PERWT]
acs[,CataHCC_Age:=CataHCC_Age/PERWT]


acs$FAMILY_OR_INDIVIDUAL = "INDIVIDUAL"
acs$FAMILY_OR_INDIVIDUAL[acs$MEMBERS>1] = "FAMILY"
acs$catas_elig = acs$catas_cnt==acs$MEMBERS


#### Match to a Risk Draw ####
acs[,riskDraw:= runif(nrow(acs))*.99+.005]

## Risk Moments
r_mom = read.csv("Intermediate_Output/MEPS_Moments/R_Score_Moments.csv")
acs[,Age_Cat:= 0]
acs[AGE>45,Age_Cat:= 1]

acs[,Inc_Cat:= 0]
acs[HHincomeFPL>4,Inc_Cat:= 1]

#acs[,Rtype:= 1+Age_Cat+Inc_Cat*2]
r_mom$Rtype= with(r_mom,1+Age_Cat+Inc_Cat*2)

acs = merge(acs,r_mom,by=c("Age_Cat","Inc_Cat"),all.x=TRUE)

acs[,c("Age_Cat","Inc_Cat"):=NULL]

## Calculate HCC Score
acs[,draws_Any:=(riskDraw-(1-Any_HCC))/(Any_HCC)]
acs[draws_Any<0,draws_Any:=0]

acs[,HCC_Silver:=exp(qnorm(draws_Any)*sqrt(var_HCC_Silver) + mean_HCC_Silver)]

acs[,names(acs)[grepl("(_HCC_|Any_HCC|riskDraw|Rtype|draws_Any)",names(acs))]:=NULL]


#### Apply Cost ####
## Cost Function 
costFile = paste("Simulation_Risk_Output/costParameters_MSM_Stage2_",run,".rData",sep="")
load(costFile)
phi = est_res$estimate

# Use Georgia Fixed Effect
phi_st_avg = phi[5]


# #### MEPS COMPARE ####
# mepsFull = read.csv("Data/2015_MEPS/MEPS_Full_2015.csv")
# meps = mepsFull[,c("HIEUIDX","DUPERSID","PID","PANEL","AGELAST","AGE15X","TTLP15X","POVLEV15","OFFER31X","OFFER42X","OFFER53X",
#                    "TRIEV15","MCREV15","MCDEV15","OPAEV15","OPBEV15","ADSMOK42",
#                    "INSCOV15","INSURC15","PERWT15F","TOTEXP15","UNINS15")]
# ins = as.data.table(mepsFull[mepsFull$UNINS15==2,c("HIEUIDX","PID","AGELAST","SEX","TTLP15X","OFFER31X","OFFER42X","OFFER53X",
#                                                    "TRIEV15","MCREV15","MCDEV15","OPAEV15","OPBEV15","ADSMOK42",
#                                                    "INSCOV15","INSURC15","TOTEXP15","PERWT15F")])
# 
# meps_risk = read.csv("Intermediate_Output/MEPS_Moments/meps_risk_scores.csv")
# mepsPers = read.csv("Data/2015_MEPS/MEPS_Person_2015.csv")
# ## Non Group Coverage
# mepsPers = mepsPers[mepsPers$PRIVCAT%in%c(2,3,5,6,99),]
# # #Not Through Employer or Association
# mepsPers = mepsPers[mepsPers$CMJINS!=1,]
# mepsPers = mepsPers[mepsPers$TYPEFLAG%in%c(5,6,7,11,12,13,21),]
# 
# mepsPers = mepsPers[mepsPers$STEXCH!=-1,]
# 
# mepsPers = summaryBy(STEXCH~DUPERSID+PANEL,data=mepsPers,FUN=min,keep.names=TRUE)
# 
# meps = merge(meps,mepsPers,by=c("DUPERSID","PANEL"),all.x=TRUE)
# meps = merge(meps,meps_risk,by=c("DUPERSID","PANEL"),all.x=TRUE)
# 
# ## Keep Non-Group Only
# meps = meps[meps$AGE15X<66,]
# meps = meps[!is.na(meps$STEXCH),]
# 
# meps$risk_positive="Zero"
# meps$risk_positive[meps$HCC_Score_Silver>0]="Non-Zero"
# 
# acs[,risk_positive:="Zero"]
# acs[HCC_Silver>0,risk_positive:="Non-Zero"]
# 
# 
# bar_plot1 = summaryBy(PERWT15F~risk_positive,data=meps,FUN=sum,keep.names=TRUE)
# bar_plot1$model="Data"
# bar_plot1$dist = with(bar_plot1,PERWT15F/sum(PERWT15F))
# 
# 
# bar_plot2 = summaryBy(PERWT~risk_positive,data=acs,FUN=sum,keep.names=TRUE)
# bar_plot2$model="Model"
# bar_plot2[,dist:=PERWT/sum(PERWT)]
# barplot = rbind(bar_plot1[,c("risk_positive","model","dist")],
#                 bar_plot2[,c("risk_positive","model","dist")])
# barplot$risk_positive = factor(barplot$risk_positive,levels=c("Zero","Non-Zero"))
# ### Risk Distribution
# png("Writing/Images/RiskDist_Fit_Log.png",width=2500,height=1500,res=275)
# ggplot() +
#   geom_histogram(data=acs,aes(x=log(HCC_Silver+.001),weights=PERWT,y=..density..,fill="Data"),binwidth=.3,alpha=0.6)+
#   geom_histogram(data=meps,aes(x=log(HCC_Score_Silver+.001),weights=PERWT15F,y=..density..,fill="Model"),binwidth=.3,alpha=0.6)+
#   ylab("Density") +
#   xlab("Log Risk Score") +
#   theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
#     strip.background = element_blank(),
#     #panel.grid.major = element_line(color=grey(.8)),
#     legend.background = element_blank(),
#     legend.title=element_blank(),
#     legend.text = element_text(size=14),
#     legend.key.width = unit(.05,units="npc"),
#     legend.key = element_rect(color="transparent",fill="transparent"),
#     legend.position = "right",
#     axis.title=element_text(size=14),
#     axis.text = element_text(size=14))
# dev.off()
# 
# 
# png("Writing/Images/RiskDist_Fit_Any.png",width=2500,height=1500,res=275)
# ggplot(barplot) + aes(x=risk_positive,y=dist,fill=model) +
#   geom_bar(position="dodge",stat="identity") +
#   ylab("Probability") +
#   xlab("Risk Score") +
#   theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
#     strip.background = element_blank(),
#     #panel.grid.major = element_line(color=grey(.8)),
#     legend.background = element_blank(),
#     legend.title=element_blank(),
#     legend.text = element_text(size=14),
#     legend.key.width = unit(.05,units="npc"),
#     legend.key = element_rect(color="transparent",fill="transparent"),
#     legend.position = "right",
#     axis.title=element_text(size=16),
#     axis.text = element_text(size=14))
# dev.off()
# 
# 
# 
# png("Writing/Images/RiskDist_Fit_Positive.png",width=2500,height=1500,res=275)
# ggplot() +
#   geom_histogram(data=acs[HCC_Silver>0,],aes(x=HCC_Silver,weights=PERWT,y=..density..,fill="Data"),binwidth=.3,alpha=0.6)+
#   geom_histogram(data=meps[meps$HCC_Score_Silver>0,],aes(x=HCC_Score_Silver,weights=PERWT15F,y=..density..,fill="Model"),binwidth=.3,alpha=0.6)+
#   ylab("Density") +
#   xlab("Risk Score") +
#   coord_cartesian(xlim=c(0,20)) + 
#   theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
#     strip.background = element_blank(),
#     #panel.grid.major = element_line(color=grey(.8)),
#     legend.background = element_blank(),
#     legend.title=element_blank(),
#     legend.text = element_text(size=14),
#     legend.key.width = unit(.05,units="npc"),
#     legend.key = element_rect(color="transparent",fill="transparent"),
#     legend.position = "none",
#     axis.title=element_text(size=16),
#     axis.text = element_text(size=14))
# dev.off()
# 
# ### Cost by Age
# acs[,AV:=0.7]
# ## Apply Cost Function
# acs[,C:=exp(phi[1]*AGE/10 +
#               phi[2]*HCC_Silver +
#               phi[3]*AV +
#               # phi[4]*FE_AK +
#               # phi[5]*FE_GA +
#               # phi[6]*FE_IA +
#               # phi[7]*FE_IL +
#               # phi[8]*FE_MD +
#               # phi[9]*FE_MI +
#               # phi[10]*FE_MO +
#               # phi[11]*FE_ND +
#               # phi[12]*FE_NE +
#               # phi[13]*FE_NM +
#               # phi[14]*FE_OK +
#             # phi[15]*FE_OR +
#             # phi[16]*FE_TX +
#             # phi[17]*FE_UT +
#             phi_st_avg)]
# 
# ageMeans = acs[,list(avgCost=sum(PERWT*C)/sum(PERWT),Pop=sum(PERWT)),by=c("AGE")]
# ageMeans[AGE<=22,yngAvg:=sum(avgCost*Pop)/sum(Pop)]
# ageMeans[,yngAvg:=max(yngAvg,na.rm=TRUE)]
# ageMeans[,c_index:=avgCost/yngAvg]
# setkey(ageMeans,AGE)
# 
# 
# ageMeans_MEPS = ins[AGELAST>=18&AGELAST<65,list(avgCost=sum(PERWT15F*TOTEXP15)/sum(PERWT15F),Pop=sum(PERWT15F)),by=c("AGELAST")]
# ageMeans_MEPS[AGELAST<=22,yngAvg:=sum(avgCost*Pop)/sum(Pop)]
# ageMeans_MEPS[,yngAvg:=max(yngAvg,na.rm=TRUE)]
# ageMeans_MEPS[,c_index:=avgCost/yngAvg]
# setkey(ageMeans_MEPS,AGELAST)
# 
# ### Age- Cost Curve
# png("Writing/Images/AgeCost_Fit.png",width=2500,height=1500,res=275)
# ggplot() +
#   geom_line(data=ageMeans,aes(x=AGE,y=c_index,color="Model"),size=1.5) +
#   geom_point(data=ageMeans_MEPS,aes(x=AGELAST,y=c_index,color="Data",size=Pop/1e6))+
#   xlab("Age") +
#   ylab("Cost Index") +
#   scale_size_continuous(guide="none") + 
#   theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
#     strip.background = element_blank(),
#     #panel.grid.major = element_line(color=grey(.8)),
#     legend.background = element_blank(),
#     legend.title=element_blank(),
#     legend.text = element_text(size=14),
#     legend.key.width = unit(.05,units="npc"),
#     legend.key = element_rect(color="transparent",fill="transparent"),
#     legend.position = "right",
#     axis.title=element_text(size=14),
#     axis.text = element_text(size=14))
# dev.off()
# 
# meps = as.data.table(meps)
# meps[,HCC_rnd:=ceiling(HCC_Score_Silver)]
# 
# hccMeans_MEPS = meps[!is.na(HCC_rnd),list(avgCost=sum(PERWT15F*TOTEXP15)/sum(PERWT15F),
#                                           avgHCC=sum(PERWT15F*HCC_Score_Silver)/sum(PERWT15F),
#                                           Pop = sum(PERWT15F)),by=c("HCC_rnd")]
# setkey(hccMeans_MEPS,HCC_rnd)
# hccMeans_MEPS[HCC_rnd==0,LowAvg:=avgCost]
# hccMeans_MEPS[,LowAvg:=max(LowAvg,na.rm=TRUE)]
# hccMeans_MEPS[,c_index:=avgCost/LowAvg]
# 
# acs[,HCC_rnd:=ceiling(HCC_Silver)]
# hccMeans = acs[,list(avgCost=sum(PERWT*C)/sum(PERWT),
#                                           avgHCC=sum(PERWT*HCC_Silver)/sum(PERWT),
#                                           Pop = sum(PERWT)),by=c("HCC_rnd")]
# setkey(hccMeans,HCC_rnd)
# hccMeans[HCC_rnd==0,LowAvg:=avgCost]
# hccMeans[,LowAvg:=max(LowAvg,na.rm=TRUE)]
# hccMeans[,c_index:=avgCost/LowAvg]
# 
# ### Risk - Cost Curve
# png("Writing/Images/RiskCost_Fit.png",width=2500,height=1500,res=275)
# ggplot() +
#   geom_line(data=hccMeans[HCC_rnd>0,],aes(x=HCC_rnd,y=c_index,color="Model"),size=1.2) +
#   geom_point(data=hccMeans_MEPS[HCC_rnd>0&HCC_rnd<40,],aes(x=HCC_rnd,y=c_index,color="Data",size=Pop/1e6))+
#   ylab("Cost Index") +
#   xlab("Risk Score") +
#   scale_size_continuous(guide="none") + 
#   theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
#     strip.background = element_blank(),
#     #panel.grid.major = element_line(color=grey(.8)),
#     legend.background = element_blank(),
#     legend.title=element_blank(),
#     legend.text = element_text(size=14),
#     legend.key.width = unit(.05,units="npc"),
#     legend.key = element_rect(color="transparent",fill="transparent"),
#     legend.position = "right",
#     axis.title=element_text(size=14),
#     axis.text = element_text(size=14))
# dev.off()
# 
# 
# 
# 
# acs[,c("HCC_rnd"):=NULL]
# acs[,c("risk_positive"):=NULL]
# 
# 
# rm(ageMeans,ageMeans_MEPS,HCC,hccMeans,hccMeans_MEPS,ins,meps,meps_risk,mepsFull,mepsPers,r_mom)
# 
# 
# 
# 
# 

#### Read in Parameters ####
parFile = paste("Estimation_Output/estimationresults_",run,".csv",sep="")
pars = read.csv(parFile)

beta_vec = pars$pars

gamma = beta_vec[1:5]
beta0 = beta_vec[6:8]
beta = matrix(0,nrow=3,ncol=5)
beta[1,1:ncol(beta)] = beta_vec[9:13]
sigma = beta_vec[14:15]
FE_pars = beta_vec[16:length(beta_vec)]
FE_avg = FE_pars[1] # Let Aetna be the representative firm



#### Demand Variables ####
## Dummy Variables
acs[,Family:= 0] 
acs[FAMILY_OR_INDIVIDUAL=="FAMILY",Family:= 1]

acs[,LowIncome:= 1]
acs[is.na(HHincomeFPL)|HHincomeFPL>4,LowIncome:= 0]

acs[,Big:=0]

acs[,AgeFE_18_30:=0]
acs[AGE<=30,AgeFE_18_30:=1]

acs[,AgeFE_31_39:=0]
acs[AGE>=31&AGE<=39,AgeFE_31_39:=1]

acs[,AgeFE_40_51:=0]
acs[AGE>=40&AGE<=51,AgeFE_40_51:=1]

acs[,AgeFE_52_64:=0]
acs[AGE>=52,AgeFE_52_64:=1]

# Person Identifiers Variables
acs[,Person:=as.factor(household)]
acs[,Person:=as.numeric(Person)]
setkey(acs,Person)

## Subsidy Variables
#acs[,Benchmark:=benchBase*ageRate]
acs[,HHcont:=subsPerc(HHincomeFPL)]
acs[,subsidy:=0]


#### Willingness to Pay ####
demos = as.matrix(acs[,c("AgeFE_31_39",
                         "AgeFE_40_51",
                         "AgeFE_52_64",
                         "Family",
                         "LowIncome")])
alpha_z = (demos%*%t(beta))
acs[,alpha:=beta0[1] + alpha_z[,1]]
acs[,WTP:=-.1*(beta0[2]+HCC_Silver*sigma[1])/(alpha/1000*12)]

acs[,addC:=exp(phi[1]*AGE/10 + 
                 phi[2]*HCC_Silver + 
                 phi[3]*.7 + phi_st_avg)-
      exp(phi[1]*AGE/10 + 
            phi[2]*HCC_Silver + 
            phi[3]*.6 + phi_st_avg)]



#### Two Products ####
acs_High = as.data.frame(acs)
acs_High$AV = 0.8
acs_Low = as.data.frame(acs)
acs_Low$AV = 0.6
acs = as.data.table(rbind(acs_High,acs_Low))
rm(acs_High,acs_Low)

## Apply Cost Function
acs[,C:=exp(phi[1]*AGE/10 + 
              phi[2]*HCC_Silver + 
              phi[3]*AV + 
              # phi[4]*FE_AK + 
              # phi[5]*FE_GA + 
              # phi[6]*FE_IA + 
              # phi[7]*FE_IL + 
              # phi[8]*FE_MD + 
              # phi[9]*FE_MI + 
              # phi[10]*FE_MO + 
              # phi[11]*FE_ND + 
              # phi[12]*FE_NE + 
              # phi[13]*FE_NM + 
              # phi[14]*FE_OK + 
            # phi[15]*FE_OR + 
            # phi[16]*FE_TX + 
            # phi[17]*FE_UT + 
            phi_st_avg)]

acs[AV==0.6,C_low:=C]
acs[,C_low:=max(C_low,na.rm=TRUE),by="Person"]
acs[AV==0.8,C_high:=C]
acs[,C_high:=max(C_high,na.rm=TRUE),by="Person"]



##### Uniform Price Optimum #####
uniform = NULL

pref_int = 5

p_1_l = 3
p_1_h = p_1_l + 3
demos = as.matrix(acs[,c("AgeFE_31_39",
                         "AgeFE_40_51",
                         "AgeFE_52_64",
                         "Family",
                         "LowIncome")])
chars_risk = as.matrix(acs[,c("AV","Big")])

intercept = demos%*%gamma

# Only Price Coefficient
beta_z = (demos%*%t(beta))
beta_r = matrix(acs$HCC_Silver,nrow=nrow(acs),ncol=1)

for (comp in c("PC","SW","M","RA","O")){
  err_l = 10 
  M = 0
  mandate = 0
  while(abs(sum(err_l))>.001){
    err_h = 10
    acs[AV==.6,P_base:=p_1_l]
    while (sum(abs(err_h))>.001){
      acs[AV==.8,P_base:=p_1_h]
      
      acs[,Price:=P_base]
      chars = as.matrix(acs[,c("Price","AV","Big")])
      
      chars_val = intercept + chars%*%beta0 +
        beta_z[,1]*chars[,1] + #Demographic Effect on Price
        beta_r*chars[,2]*sigma[1] + #Risk Effect on AV
        beta_r*chars[,3]*sigma[2] + pref_int #Risk Effect on Big
      # mean(FE_pars)
      # alpha = beta0[1] + beta_z[,1]
      # price_val = chars[,1]*alpha
      
      
      acs[,util:=exp(chars_val)]
      
      
      acs[,alpha:=beta0[1] + beta_z[,1]]
      acs[,expsum:=util]
      
      acs[,expsum:=sum(expsum),by=c("Person")]
      acs[,outside:=exp(alpha*mandate)]
      acs[,shares:=util/(exp(alpha*mandate)+expsum)]
      acs[,insured:=expsum/(exp(alpha*mandate)+expsum)]
      
      acs[AV==0.6,share_own_low:=shares]
      acs[,share_own_low:=max(share_own_low,na.rm=TRUE),by=c("Person")]
      acs[AV==0.8,share_own_high:=shares]
      acs[,share_own_high:=max(share_own_high,na.rm=TRUE),by=c("Person")]
      
      
      acs[,dsdp:=alpha*shares*(1-shares)]
      acs[AV==0.6,dsdp_low:=alpha*share_own_low*(1-shares)]
      acs[AV==0.8,dsdp_low:=-alpha*share_own_low*(shares)]
      acs[,dsdp_low_m:=-alpha*share_own_low*shares]
      
      acs[AV==0.6,dsdp_high:=-alpha*share_own_high*(shares)]
      acs[AV==0.8,dsdp_high:=alpha*share_own_high*(1-shares)]
      acs[,dsdp_high_m:=-alpha*share_own_high*shares]
      
      
      acs[,CW:=log(exp(alpha*mandate)+expsum)]
      acs[,CW:=CW/(-alpha)]
      
      
      res_temp = acs[,list(total_insured=sum(insured*PERWT)/sum(PERWT),
                           share = sum(shares*PERWT)/sum(PERWT),
                           AC = 12*sum(C*shares*PERWT)/sum(shares*PERWT),
                           MC = 12*sum(C*dsdp*PERWT)/sum(dsdp*PERWT),
                           PC = 12*sum(C*(share_own_low+share_own_high)*PERWT)/sum((share_own_low+share_own_high)*PERWT),
                           MC_low = 12*sum(C_low*dsdp_low*PERWT)/sum(PERWT),
                           MC_high = 12*sum(C_high*dsdp_high*PERWT)/sum(PERWT),
                           dSdp_low = sum(dsdp_low*PERWT)/sum(PERWT),
                           dSdp_high = sum(dsdp_high*PERWT)/sum(PERWT),
                           Markup = -1000*sum(shares*PERWT)/sum(dsdp*PERWT),
                           ConsWelfare = sum(CW*PERWT),
                           Pop = sum(PERWT)
      ),by=c("AV","P_base")]
      setkey(res_temp,AV)
      
      dSdp_m = as.matrix(res_temp[,c("dSdp_low","dSdp_high")])
      S_m = as.matrix(res_temp$share)
      MC_m = as.matrix(res_temp[,(MC_low+MC_high)/1000])
      
      
      if(comp=="PC"){
        res_temp[,P_new:=AC/1000]
      }      
      if(comp=="RA"){
        res_temp[,P_new:=PC/1000]
      }
      if(comp=="SW"){
        res_temp[,P_new:=(solve(dSdp_m)%*%(MC_m))]
      }
      
      if(comp=="M"){
        res_temp[,P_new:=(solve(dSdp_m)%*%(-S_m + MC_m))]
        
      }
      if(comp=="O"){
        res_temp[,P_new:=(Markup+MC)/1000]
        
      }
      rm(dSdp_m,S_m,MC_m)
      
      err_h = with(res_temp[res_temp$AV==.8,],P_new-P_base)  
      p_1_h = p_1_h + err_h
      print(err_h)
      print(c(p_1_h))
      acs[,c("share_own_low","share_own_high","dsdp_low","dsdp_high"):=NULL]#"share_2_low","share_2_high","dsdp_low_m","dsdp_high_m"):=NULL]
    }
    print("UPDATE P_l")
    err_l = with(res_temp[res_temp$AV==.6,],P_new-P_base)
    p_1_l = p_1_l + err_l[1]
    
    print(c(p_1_l))
  }
  res_temp$comp=comp
  uniform = rbind(uniform,res_temp)
}

uniform[,Profit:=share*(P_new-AC/1000)*Pop]
uniform[,Profit:=sum(Profit),by="comp"]
uniform[,TotalSurplus:=ConsWelfare+Profit]
uniform[,TotalAvgCost:=sum(share*AC)/sum(share),by="comp"]

acs[,c("dsdp","shares","insured","dsdp_low","dsdp_high","dsdp_low_m","dsdp_high_m","CW","util","expsum","P_base","Price"):=NULL]




#### Perfect Information Optimum ####
setkey(acs,household,AV)
# acs_old = acs
# mini = acs[household=="1-2-4",]
# acs = acs_old
demos = as.matrix(acs[,c("AgeFE_31_39",
                         "AgeFE_40_51",
                         "AgeFE_52_64",
                         "Family",
                         "LowIncome")])
chars_risk = as.matrix(acs[,c("AV","Big")])

intercept = demos%*%gamma

# Only Price Coefficient
beta_z = (demos%*%t(beta))
beta_r = matrix(acs$HCC_Silver,nrow=nrow(acs),ncol=1)



perfect = NULL
for (comp in c("PC","SW","M")){
  acs[,P_old:=12*C/1000]
  err = 10
  while(err>.001){
    acs[,Price:=P_old]
    chars = as.matrix(acs[,c("Price","AV","Big")])
    
    chars_val = intercept + chars%*%beta0 +
      beta_z[,1]*chars[,1] + #Demographic Effect on Price
      beta_r*chars[,2]*sigma[1] + #Risk Effect on AV
      beta_r*chars[,3]*sigma[2] + pref_int #Risk Effect on Big
    # mean(FE_pars)
    # alpha = beta0[1] + beta_z[,1]
    # price_val = chars[,1]*alpha
    
    
    acs[,util:=exp(chars_val)]
    
    
    acs[,alpha:=beta0[1] + beta_z[,1]]
    acs[,expsum:=util]
    
    acs[,expsum:=sum(expsum),by=c("Person")]
    acs[,outside:=exp(alpha*mandate)]
    acs[,shares:=util/(exp(alpha*mandate)+expsum)]
    acs[,insured:=expsum/(exp(alpha*mandate)+expsum)]
    
    acs[AV==0.6,share_own_low:=shares]
    acs[,share_own_low:=max(share_own_low,na.rm=TRUE),by=c("Person")]
    acs[AV==0.8,share_own_high:=shares]
    acs[,share_own_high:=max(share_own_high,na.rm=TRUE),by=c("Person")]
    
    
    acs[,dsdp:=alpha*shares*(1-shares)]
    acs[AV==0.6,dsdp_low:=alpha*share_own_low*(1-shares)]
    acs[AV==0.8,dsdp_low:=-alpha*share_own_low*(shares)]
    
    acs[AV==0.6,dsdp_high:=-alpha*share_own_high*(shares)]
    acs[AV==0.8,dsdp_high:=alpha*share_own_high*(1-shares)]
    
    acs[AV==0.6,dsdp_other:=alpha*share_own_high*(1-share_own_high)]
    acs[AV==0.8,dsdp_other:=alpha*share_own_low*(1-share_own_low)]
    
    acs[AV==0.6,share_other:=share_own_high]
    acs[AV==0.8,share_other:=share_own_low]
    
    acs[AV==0.6,dsdp_cross:=-alpha*share_own_high*(shares)]
    acs[AV==0.8,dsdp_cross:=-alpha*share_own_low*(shares)]
    
    
    
    # dSdp_m = as.matrix(acs[household=="1-2-4",c("dsdp_low","dsdp_high")])
    # S_m = as.matrix(acs$shares[acs$household=="1-2-4"])
    # test = solve(dSdp_m)%*%S_m
    
    acs[,CW:=log(exp(alpha*mandate)+expsum)]
    acs[,CW:=CW/(-alpha)]
    
    # Monopolist Price
    
    
    acs[,markup:=1/(dsdp*dsdp_other - dsdp_cross^2)*(-dsdp_other*shares + dsdp_cross*share_other)]
    if (comp=="M"){
      acs[,P_new:=markup+12*C/1000]
      acs[shares<1e-30,P_new:=100]
    }else{
      acs[,P_new:=12*C/1000]
    }
    acs[,err:=P_new - P_old]
    
    err = acs[, mean(err^2)]
    #acs[shares<1e-5,P_old:=P_new]
    
    acs[abs(err)>10,P_old:= P_old + .5*(P_new-P_old)]
    acs[abs(err)<=10&abs(err)>1,P_old:= P_old + .5*(P_new-P_old)]
    acs[abs(err)<=1,P_old:= P_old + .5*(P_new-P_old)]
    
    
    
    print(err)
    acs[,c("share_own_low","share_own_high","dsdp_low","dsdp_high"):=NULL]
    
  }
  res_temp = acs[,list(total_insured=sum(insured*PERWT)/sum(PERWT),
                       share = sum(shares*PERWT)/sum(PERWT),
                       AC = 12*sum(C*shares*PERWT)/sum(shares*PERWT),
                       MC = 12*sum(C*dsdp*PERWT)/sum(dsdp*PERWT),
                       Markup = -1000*sum(shares*PERWT)/sum(dsdp*PERWT),
                       ConsWelfare = sum(CW*PERWT),
                       Profit = sum(PERWT*shares*(P_new - 12*C/1000)),
                       Pop = sum(PERWT)
  ),by=c("AV")]
  setkey(res_temp,AV)
  res_temp$comp=comp
  perfect = rbind(perfect,res_temp)
}
perfect[,Profit:=sum(Profit),by="comp"]
perfect[,TotalSurplus:=ConsWelfare+Profit]



##### Uniform Price and Cost Optimum #####
meanCost = NULL

acs[,meanC:=sum(PERWT*C)/sum(PERWT),by="AV"]
acs[,meanC_low:=sum(PERWT*C_low)/sum(PERWT)]
acs[,meanC_high:=sum(PERWT*C_high)/sum(PERWT)]

p_1_l = 3
p_1_h = p_1_l + 3
demos = as.matrix(acs[,c("AgeFE_31_39",
                         "AgeFE_40_51",
                         "AgeFE_52_64",
                         "Family",
                         "LowIncome")])
chars_risk = as.matrix(acs[,c("AV","Big")])

intercept = demos%*%gamma


# Only Price Coefficient
beta_z = (demos%*%t(beta))
beta_r = matrix(acs$HCC_Silver,nrow=nrow(acs),ncol=1)

for (comp in c("PC","SW","M")){
  err_l = 10 
  M = 0
  mandate = 0
  while(abs(sum(err_l))>.001){
    err_h = 10
    acs[AV==.6,P_base:=p_1_l]
    while (sum(abs(err_h))>.001){
      acs[AV==.8,P_base:=p_1_h]
      
      acs[,Price:=P_base]
      chars = as.matrix(acs[,c("Price","AV","Big")])
      
      chars_val = intercept + chars%*%beta0 +
        beta_z[,1]*chars[,1] + #Demographic Effect on Price
        beta_r*chars[,2]*sigma[1] + #Risk Effect on AV
        beta_r*chars[,3]*sigma[2] + pref_int #Risk Effect on Big
      # mean(FE_pars)
      # alpha = beta0[1] + beta_z[,1]
      # price_val = chars[,1]*alpha
      
      
      acs[,util:=exp(chars_val)]
      
      
      acs[,alpha:=beta0[1] + beta_z[,1]]
      acs[,expsum:=util]
      
      acs[,expsum:=sum(expsum),by=c("Person")]
      acs[,outside:=exp(alpha*mandate)]
      acs[,shares:=util/(exp(alpha*mandate)+expsum)]
      acs[,insured:=expsum/(exp(alpha*mandate)+expsum)]
      
      acs[AV==0.6,share_own_low:=shares]
      acs[,share_own_low:=max(share_own_low,na.rm=TRUE),by=c("Person")]
      acs[AV==0.8,share_own_high:=shares]
      acs[,share_own_high:=max(share_own_high,na.rm=TRUE),by=c("Person")]
      
      
      acs[,dsdp:=alpha*shares*(1-shares)]
      acs[AV==0.6,dsdp_low:=alpha*share_own_low*(1-shares)]
      acs[AV==0.8,dsdp_low:=-alpha*share_own_low*(shares)]
      acs[,dsdp_low_m:=-alpha*share_own_low*shares]
      
      acs[AV==0.6,dsdp_high:=-alpha*share_own_high*(shares)]
      acs[AV==0.8,dsdp_high:=alpha*share_own_high*(1-shares)]
      acs[,dsdp_high_m:=-alpha*share_own_high*shares]
      
      
      acs[,CW:=log(exp(alpha*mandate)+expsum)]
      acs[,CW:=CW/(-alpha)]
      
      
      res_temp = acs[,list(total_insured=sum(insured*PERWT)/sum(PERWT),
                           share = sum(shares*PERWT)/sum(PERWT),
                           AC = 12*sum(meanC*shares*PERWT)/sum(shares*PERWT),
                           MC = 12*sum(meanC*dsdp*PERWT)/sum(dsdp*PERWT),
                           MC_low = 12*sum(meanC_low*dsdp_low*PERWT)/sum(PERWT),
                           MC_high = 12*sum(meanC_high*dsdp_high*PERWT)/sum(PERWT),
                           dSdp_low = sum(dsdp_low*PERWT)/sum(PERWT),
                           dSdp_high = sum(dsdp_high*PERWT)/sum(PERWT),
                           Markup = -1000*sum(shares*PERWT)/sum(dsdp*PERWT),
                           ConsWelfare = sum(CW*PERWT),
                           Pop = sum(PERWT)
      ),by=c("AV","P_base")]
      setkey(res_temp,AV)
      
      dSdp_m = as.matrix(res_temp[,c("dSdp_low","dSdp_high")])
      S_m = as.matrix(res_temp$share)
      MC_m = as.matrix(res_temp[,(MC_low+MC_high)/1000])
      
      
      if(comp=="PC"){
        res_temp[,P_new:=AC/1000]
      }
      if(comp=="SW"){
        res_temp[,P_new:=(solve(dSdp_m)%*%(MC_m))]
      }
      
      if(comp=="M"){
        res_temp[,P_new:=(solve(dSdp_m)%*%(-S_m + MC_m))]
        
      }
      rm(dSdp_m,S_m,MC_m)
      
      err_h = with(res_temp[res_temp$AV==.8,],P_new-P_base)  
      p_1_h = p_1_h + err_h
      print(err_h)
      print(c(p_1_h))
      acs[,c("share_own_low","share_own_high","dsdp_low","dsdp_high"):=NULL]#"share_2_low","share_2_high","dsdp_low_m","dsdp_high_m"):=NULL]
    }
    print("UPDATE P_l")
    err_l = with(res_temp[res_temp$AV==.6,],P_new-P_base)
    p_1_l = p_1_l + err_l[1]
    
    print(c(p_1_l))
  }
  res_temp$comp=comp
  meanCost = rbind(meanCost,res_temp)
}

meanCost[,Profit:=share*(P_new-AC/1000)*Pop]
meanCost[,Profit:=sum(Profit),by="comp"]
meanCost[,TotalSurplus:=ConsWelfare+Profit]

acs[,c("dsdp","shares","insured","dsdp_low","dsdp_high","dsdp_low_m","dsdp_high_m","CW","util","expsum","P_base","Price"):=NULL]




##### Constrained Optimum #####
constrain = NULL

pref_int = 5


lambda = 0
demos = as.matrix(acs[,c("AgeFE_31_39",
                         "AgeFE_40_51",
                         "AgeFE_52_64",
                         "Family",
                         "LowIncome")])
chars_risk = as.matrix(acs[,c("AV","Big")])

intercept = demos%*%gamma

# Only Price Coefficient
beta_z = (demos%*%t(beta))
beta_r = matrix(acs$HCC_Silver,nrow=nrow(acs),ncol=1)

CS_target = uniform$ConsWelfare[1]
CS_max = uniform$ConsWelfare[3]
CS_min = uniform$ConsWelfare[5]


p_1_l = 3.905567
p_1_h = 6.333139
lambda = .5
CS_err = 1
while(abs(CS_err)>.00001){
  err_l = 10 
  M = 0
  mandate = 0
  while(abs(sum(err_l))>1e-3){
    err_h = 10
    acs[AV==.6,P_base:=p_1_l]
    while (sum(abs(err_h))>1e-3){
      acs[AV==.8,P_base:=p_1_h]
      
      acs[,Price:=P_base]
      chars = as.matrix(acs[,c("Price","AV","Big")])
      
      chars_val = intercept + chars%*%beta0 +
        beta_z[,1]*chars[,1] + #Demographic Effect on Price
        beta_r*chars[,2]*sigma[1] + #Risk Effect on AV
        beta_r*chars[,3]*sigma[2] + pref_int #Risk Effect on Big
      # mean(FE_pars)
      # alpha = beta0[1] + beta_z[,1]
      # price_val = chars[,1]*alpha
      
      
      acs[,util:=exp(chars_val)]
      
      
      acs[,alpha:=beta0[1] + beta_z[,1]]
      acs[,expsum:=util]
      
      acs[,expsum:=sum(expsum),by=c("Person")]
      acs[,outside:=exp(alpha*mandate)]
      acs[,shares:=util/(exp(alpha*mandate)+expsum)]
      acs[,insured:=expsum/(exp(alpha*mandate)+expsum)]
      
      acs[AV==0.6,share_own_low:=shares]
      acs[,share_own_low:=max(share_own_low,na.rm=TRUE),by=c("Person")]
      acs[AV==0.8,share_own_high:=shares]
      acs[,share_own_high:=max(share_own_high,na.rm=TRUE),by=c("Person")]
      
      
      acs[,dsdp:=alpha*shares*(1-shares)]
      acs[AV==0.6,dsdp_low:=alpha*share_own_low*(1-shares)]
      acs[AV==0.8,dsdp_low:=-alpha*share_own_low*(shares)]
      acs[,dsdp_low_m:=-alpha*share_own_low*shares]
      
      acs[AV==0.6,dsdp_high:=-alpha*share_own_high*(shares)]
      acs[AV==0.8,dsdp_high:=alpha*share_own_high*(1-shares)]
      acs[,dsdp_high_m:=-alpha*share_own_high*shares]
      
      
      acs[,CW:=log(exp(alpha*mandate)+expsum)]
      acs[,CW:=CW/(-alpha)]
      
      
      res_temp = acs[,list(total_insured=sum(insured*PERWT)/sum(PERWT),
                           share = sum(shares*PERWT)/sum(PERWT),
                           AC = 12*sum(C*shares*PERWT)/sum(shares*PERWT),
                           MC = 12*sum(C*dsdp*PERWT)/sum(dsdp*PERWT),
                           PC = 12*sum(C*(share_own_low+share_own_high)*PERWT)/sum((share_own_low+share_own_high)*PERWT),
                           MC_low = 12*sum(C_low*dsdp_low*PERWT)/sum(PERWT),
                           MC_high = 12*sum(C_high*dsdp_high*PERWT)/sum(PERWT),
                           dSdp_low = sum(dsdp_low*PERWT)/sum(PERWT),
                           dSdp_high = sum(dsdp_high*PERWT)/sum(PERWT),
                           Markup = -1000*sum(shares*PERWT)/sum(dsdp*PERWT),
                           ConsWelfare = sum(CW*PERWT),
                           Pop = sum(PERWT)
      ),by=c("AV","P_base")]
      setkey(res_temp,AV)
      
      dSdp_m = as.matrix(res_temp[,c("dSdp_low","dSdp_high")])
      S_m = as.matrix(res_temp$share)
      MC_m = as.matrix(res_temp[,(MC_low+MC_high)/1000])
      
      res_temp[,P_new:=AC/1000]
      
      # res_temp[,P_new:=(solve(dSdp_m)%*%(MC_m))]
      
      
      res_temp[,P_new:=(solve(dSdp_m)%*%((lambda-1)*S_m + MC_m))]
      
      rm(dSdp_m,S_m,MC_m)
      
      err_h = with(res_temp[res_temp$AV==.8,],P_new-P_base)  
      p_1_h = p_1_h + err_h
      print(err_h)
      print(c(p_1_h))
      acs[,c("share_own_low","share_own_high","dsdp_low","dsdp_high"):=NULL]#"share_2_low","share_2_high","dsdp_low_m","dsdp_high_m"):=NULL]
    }
    print("UPDATE P_l")
    err_l = with(res_temp[res_temp$AV==.6,],P_new-P_base)
    p_1_l = p_1_l + err_l[1]
    
    print(c(p_1_l))
  }
  res_temp[,Profit:=share*(P_new-AC/1000)*Pop]
  res_temp[,Profit:=sum(Profit)]
  CS_err = (res_temp$Profit[1])/1e6
  CS_adj = (res_temp$Profit[1])/(9876223+23653758)
  
  # CS_err = (CS_target-res_temp$ConsWelfare[1])/CS_target
  # CS_adj = (CS_target-res_temp$ConsWelfare[1])/(CS_max-CS_min)
  lambda = lambda + CS_adj
  print(lambda)
  print(res_temp$Profit[1])
}
constrain = res_temp
constrain[,lambda:=lambda]
constrain[,Profit:=share*(P_new-AC/1000)*Pop]
constrain[,Profit:=sum(Profit)]
constrain[,TotalSurplus:=ConsWelfare+Profit]

acs[,c("dsdp","shares","insured","dsdp_low","dsdp_high","dsdp_low_m","dsdp_high_m","CW","util","expsum","P_base","Price"):=NULL]

#### Constraint Plot ####
Profit_Target = 0
Profit_Target = 2366.5964767
P_low = 3.714432
#dP = 10.210809 - 6.856660
lambda = 0.7257728
sorting = NULL
for (dP in seq(1,5,.5)){
  print("dP")
  print(dP)
  err_prof = 100 
  M = 0
  mandate = 0
  while(abs(err_prof)>10){
    acs[AV==.6,P_base:=P_low]
    acs[AV==.8,P_base:=P_low+dP]
    
    acs[,Price:=P_base]
    chars = as.matrix(acs[,c("Price","AV","Big")])
    
    chars_val = intercept + chars%*%beta0 +
      beta_z[,1]*chars[,1] + #Demographic Effect on Price
      beta_r*chars[,2]*sigma[1] + #Risk Effect on AV
      beta_r*chars[,3]*sigma[2] + pref_int #Risk Effect on Big
    # mean(FE_pars)
    # alpha = beta0[1] + beta_z[,1]
    # price_val = chars[,1]*alpha
    
    
    acs[,util:=exp(chars_val)]
    
    
    acs[,alpha:=beta0[1] + beta_z[,1]]
    acs[,expsum:=util]
    
    acs[,expsum:=sum(expsum),by=c("Person")]
    acs[,outside:=exp(alpha*mandate)]
    acs[,shares:=util/(exp(alpha*mandate)+expsum)]
    acs[,insured:=expsum/(exp(alpha*mandate)+expsum)]
    
    acs[AV==0.6,share_own_low:=shares]
    acs[,share_own_low:=max(share_own_low,na.rm=TRUE),by=c("Person")]
    acs[AV==0.8,share_own_high:=shares]
    acs[,share_own_high:=max(share_own_high,na.rm=TRUE),by=c("Person")]
    
    
    acs[,dsdp:=alpha*shares*(1-shares)]
    acs[AV==0.6,dsdp_low:=alpha*share_own_low*(1-shares)]
    acs[AV==0.8,dsdp_low:=-alpha*share_own_low*(shares)]
    
    acs[AV==0.6,dsdp_high:=-alpha*share_own_high*(shares)]
    acs[AV==0.8,dsdp_high:=alpha*share_own_high*(1-shares)]
    
    acs[,CW:=log(exp(alpha*mandate)+expsum)]
    acs[,CW:=CW/(-alpha)]
    
    
    res_temp = acs[,list(share = sum(shares*PERWT)/sum(PERWT),
                         AC = 12*sum(C*shares*PERWT)/sum(shares*PERWT),
                         MC_low = 12*sum(C_low*dsdp_low*PERWT)/sum(PERWT),
                         MC_high = 12*sum(C_high*dsdp_high*PERWT)/sum(PERWT),
                         dSdp_low = sum(dsdp_low*PERWT)/sum(PERWT),
                         dSdp_high = sum(dsdp_high*PERWT)/sum(PERWT),
                         ConsWelfare = sum(CW*PERWT)
    ),by=c("AV","P_base")]
    setkey(res_temp,AV)
    dSdp_m = as.matrix(res_temp[,c("dSdp_low","dSdp_high")])
    S_m = as.matrix(res_temp$share)
    MC_m = as.matrix(res_temp[,(MC_low+MC_high)/1000])
    
    res_temp[,MC_SW:=(solve(dSdp_m)%*%(MC_m))]
    
    
    res_temp[,Mkup_SW:=(solve(dSdp_m)%*%((lambda-1)*S_m))]
    res_temp[,Mkup_M:=(solve(dSdp_m)%*%(-S_m))]
    res_temp[,P_m:=MC_SW+Mkup_M]
    
    res_temp[,Profit:=share*(P_base-AC/1000)]
    prof = res_temp[,sum(Profit*1000)/sum(share)]
    err_prof = prof - Profit_Target 
    print("Profit Error")
    print(err_prof)
    P_low = P_low - err_prof/2000
    print("P_Low")
    print(P_low)
  }
  
  
  dAC = with(res_temp,AC[2]-AC[1])/1000
  dMC = with(res_temp,MC_SW[2]-MC_SW[1])
  dMkup = with(res_temp,Mkup_M[2]-Mkup_M[1])
  dMkup_SW = with(res_temp,Mkup_SW[2]-Mkup_SW[1])
  
  sorting = rbind(sorting,data.frame(dP=dP,P_low=P_low,Q=res_temp$share[2],CS = res_temp$ConsWelfare[1],
                                     dAC=dAC,dMC=dMC,dMkup=dMkup,dMkup_SW=dMkup_SW))
  acs[,c("share_own_low","share_own_high","dsdp_low","dsdp_high"):=NULL]
}

ggplot(sorting) + 
  geom_line(aes(x=Q,y=dP)) + 
  geom_line(aes(x=Q,y=dAC),color="red") + 
  geom_line(aes(x=Q,y=dMC),color="blue")  + 
  #geom_line(aes(x=Q,y=dMC+dMkup_SW),color="green")  + 
  geom_line(aes(x=Q,y=dP - dMkup),color="purple")  

sorting$eq = with(sorting,dMC+dMkup - dP)


##### Continuum of Constraints - Optimum #####
constrain = NULL
setkey(acs,household,AV)

pref_int = 5


lambda = 0
demos = as.matrix(acs[,c("AgeFE_31_39",
                         "AgeFE_40_51",
                         "AgeFE_52_64",
                         "Family",
                         "LowIncome")])
chars_risk = as.matrix(acs[,c("AV","Big")])

intercept = demos%*%gamma

# Only Price Coefficient
beta_z = (demos%*%t(beta))
beta_r = matrix(acs$HCC_Silver,nrow=nrow(acs),ncol=1)

## Set PC Target
for (itr in c("PC","SW","M")){
  p_1_l = uniform$P_base[with(uniform,AV==0.6&comp==itr)]
  p_1_h = uniform$P_base[with(uniform,AV==0.8&comp==itr)]
  acs[AV==.6,P_base:=p_1_l]
  acs[AV==.8,P_base:=p_1_h]
  
  acs[,Price:=P_base]
  chars = as.matrix(acs[,c("Price","AV","Big")])
  
  chars_val = intercept + chars%*%beta0 +
    beta_z[,1]*chars[,1] + #Demographic Effect on Price
    beta_r*chars[,2]*sigma[1] + #Risk Effect on AV
    beta_r*chars[,3]*sigma[2] + pref_int #Risk Effect on Big
  # mean(FE_pars)
  # alpha = beta0[1] + beta_z[,1]
  # price_val = chars[,1]*alpha
  
  
  acs[,util:=exp(chars_val)]
  
  
  acs[,alpha:=beta0[1] + beta_z[,1]]
  acs[,expsum:=util]
  
  acs[,expsum:=sum(expsum),by=c("Person")]
  acs[,outside:=exp(alpha*mandate)]
  acs[,shares:=util/(exp(alpha*mandate)+expsum)]
  acs[,insured:=expsum/(exp(alpha*mandate)+expsum)]
  
  acs[,CW:=log(exp(alpha*mandate)+expsum)]
  acs[,CW:=CW/(-alpha)]
  if (itr=="PC"){
    acs[,CS_target:=CW]
  }  
  if (itr=="SW"){
    acs[,CS_max:=CW]
  }  
  if (itr=="M"){
    acs[,CS_min:=CW]
  }
}

acs[,lambda:=.5]
while(abs(CS_err)>.001){
  err_l = 10 
  M = 0
  mandate = 0
  while(abs(sum(err_l))>.001){
    err_h = 10
    acs[AV==.6,P_base:=p_1_l]
    while (sum(abs(err_h))>.001){
      acs[AV==.8,P_base:=p_1_h]
      
      acs[,Price:=P_base]
      chars = as.matrix(acs[,c("Price","AV","Big")])
      
      chars_val = intercept + chars%*%beta0 +
        beta_z[,1]*chars[,1] + #Demographic Effect on Price
        beta_r*chars[,2]*sigma[1] + #Risk Effect on AV
        beta_r*chars[,3]*sigma[2] + pref_int #Risk Effect on Big
      # mean(FE_pars)
      # alpha = beta0[1] + beta_z[,1]
      # price_val = chars[,1]*alpha
      
      
      acs[,util:=exp(chars_val)]
      
      
      acs[,alpha:=beta0[1] + beta_z[,1]]
      acs[,expsum:=util]
      
      acs[,expsum:=sum(expsum),by=c("Person")]
      acs[,outside:=exp(alpha*mandate)]
      acs[,shares:=util/(exp(alpha*mandate)+expsum)]
      acs[,insured:=expsum/(exp(alpha*mandate)+expsum)]
      
      acs[AV==0.6,share_own_low:=shares]
      acs[,share_own_low:=max(share_own_low,na.rm=TRUE),by=c("Person")]
      acs[AV==0.8,share_own_high:=shares]
      acs[,share_own_high:=max(share_own_high,na.rm=TRUE),by=c("Person")]
      
      
      acs[,dsdp:=alpha*shares*(1-shares)]
      acs[AV==0.6,dsdp_low:=alpha*share_own_low*(1-shares)]
      acs[AV==0.8,dsdp_low:=-alpha*share_own_low*(shares)]
      acs[,dsdp_low_m:=-alpha*share_own_low*shares]
      
      acs[AV==0.6,dsdp_high:=-alpha*share_own_high*(shares)]
      acs[AV==0.8,dsdp_high:=alpha*share_own_high*(1-shares)]
      acs[,dsdp_high_m:=-alpha*share_own_high*shares]
      
      
      acs[,CW:=log(exp(alpha*mandate)+expsum)]
      acs[,CW:=CW/(-alpha)]
      
      
      res_temp = acs[,list(total_insured=sum(insured*PERWT)/sum(PERWT),
                           share = sum(shares*PERWT)/sum(PERWT) - sum(shares*PERWT*lambda)/sum(PERWT),
                           AC = 12*sum(C*shares*PERWT)/sum(shares*PERWT),
                           MC = 12*sum(C*dsdp*PERWT)/sum(dsdp*PERWT),
                           PC = 12*sum(C*(share_own_low+share_own_high)*PERWT)/sum((share_own_low+share_own_high)*PERWT),
                           MC_low = 12*sum(C_low*dsdp_low*PERWT)/sum(PERWT),
                           MC_high = 12*sum(C_high*dsdp_high*PERWT)/sum(PERWT),
                           dSdp_low = sum(dsdp_low*PERWT)/sum(PERWT),
                           dSdp_high = sum(dsdp_high*PERWT)/sum(PERWT),
                           Markup = -1000*sum(shares*PERWT)/sum(dsdp*PERWT),
                           ConsWelfare = sum(CW*PERWT),
                           Pop = sum(PERWT)
      ),by=c("AV","P_base")]
      setkey(res_temp,AV)
      
      dSdp_m = as.matrix(res_temp[,c("dSdp_low","dSdp_high")])
      S_m = as.matrix(res_temp$share)
      MC_m = as.matrix(res_temp[,(MC_low+MC_high)/1000])
      
      
      
      res_temp[,P_new:=(solve(dSdp_m)%*%(-S_m + MC_m))]
      
      rm(dSdp_m,S_m,MC_m)
      
      err_h = with(res_temp[res_temp$AV==.8,],P_new-P_base)  
      p_1_h = p_1_h + err_h
      print(err_h)
      print(c(p_1_h))
      acs[,c("share_own_low","share_own_high","dsdp_low","dsdp_high"):=NULL]#"share_2_low","share_2_high","dsdp_low_m","dsdp_high_m"):=NULL]
    }
    print("UPDATE P_l")
    err_l = with(res_temp[res_temp$AV==.6,],P_new-P_base)
    p_1_l = p_1_l + err_l[1]
    
    print(c(p_1_l))
  }
  acs[,CS_err:=(CW-CS_target)/CS_target]
  acs[,lambda:=lambda - (CW-CS_target)/(CS_max-CS_min)]
  CS_err = acs[,mean(CS_err^2)]
  print(CS_err)
}


constrain = res_temp
constrain[,lambda:=lambda]
constrain[,Profit:=share*(P_new-AC/1000)*Pop]
constrain[,Profit:=sum(Profit)]
constrain[,TotalSurplus:=ConsWelfare+Profit]
constrain[,TotalAvgCost:=sum(share*AC)/sum(share)]

acs[,c("dsdp","shares","insured","dsdp_low","dsdp_high","dsdp_low_m","dsdp_high_m","CW","util","expsum","P_base","Price"):=NULL]


##### Optimal Risk Adjustment #####
optrisk = NULL

pref_int = 4

p_1_l = 3
p_1_h = p_1_l + 3
demos = as.matrix(acs[,c("AgeFE_31_39",
                         "AgeFE_40_51",
                         "AgeFE_52_64",
                         "Family",
                         "LowIncome")])
chars_risk = as.matrix(acs[,c("AV","Big")])

intercept = demos%*%gamma

# Only Price Coefficient
beta_z = (demos%*%t(beta))
beta_r = matrix(acs$HCC_Silver,nrow=nrow(acs),ncol=1)
J = 10
for (comp in c("PC","IP")){
  for (lambda in seq(0,2.5,.1)){
    err_l = 10 
    M = 0
    mandate = 0
    while(abs(sum(err_l))>.001){
      err_h = 10
      acs[AV==.6,P_base:=p_1_l]
      while (sum(abs(err_h))>.001){
        acs[AV==.8,P_base:=p_1_h]
        
        acs[,Price:=P_base]
        chars = as.matrix(acs[,c("Price","AV","Big")])
        
        chars_val = intercept + chars%*%beta0 +
          beta_z[,1]*chars[,1] + #Demographic Effect on Price
          beta_r*chars[,2]*sigma[1] + #Risk Effect on AV
          beta_r*chars[,3]*sigma[2] + pref_int #Risk Effect on Big
        # mean(FE_pars)
        # alpha = beta0[1] + beta_z[,1]
        # price_val = chars[,1]*alpha
        
        
        acs[,util:=exp(chars_val)]
        
        
        acs[,alpha:=beta0[1] + beta_z[,1]]
        acs[,expsum:=J*util]
        
        acs[,expsum:=sum(expsum),by=c("Person")]
        acs[,outside:=exp(alpha*mandate)]
        acs[,shares:=util/(exp(alpha*mandate)+expsum)]
        acs[,insured:=expsum/(exp(alpha*mandate)+expsum)]
        
        acs[AV==0.6,share_own_low:=shares]
        acs[,share_own_low:=max(share_own_low,na.rm=TRUE),by=c("Person")]
        acs[AV==0.8,share_own_high:=shares]
        acs[,share_own_high:=max(share_own_high,na.rm=TRUE),by=c("Person")]
        
        
        acs[,dsdp:=alpha*shares*(1-shares)]
        acs[AV==0.6,dsdp_low:=alpha*share_own_low*(1-shares)]
        acs[AV==0.8,dsdp_low:=-alpha*share_own_low*(shares)]
        acs[,dsdp_low_m:=-alpha*share_own_low*shares]
        
        acs[AV==0.6,dsdp_high:=-alpha*share_own_high*(shares)]
        acs[AV==0.8,dsdp_high:=alpha*share_own_high*(1-shares)]
        acs[,dsdp_high_m:=-alpha*share_own_high*shares]
        
        
        acs[,CW:=log(exp(alpha*mandate)+expsum)]
        acs[,CW:=CW/(-alpha)]
        
        
        res_temp = acs[,list(total_insured=sum(insured*PERWT)/sum(PERWT),
                             share = sum(shares*PERWT)/sum(PERWT),
                             AC = 12*sum(C*shares*PERWT)/sum(shares*PERWT),
                             # MC = 12*sum(C*dsdp*PERWT)/sum(dsdp*PERWT),
                             PC = 12*sum(C*(share_own_low+share_own_high)*PERWT)/sum((share_own_low+share_own_high)*PERWT),
                             MC_low = 12*sum(C_low*dsdp_low*PERWT)/sum(PERWT),
                             MC_high = 12*sum(C_high*dsdp_high*PERWT)/sum(PERWT),
                             MC_low_m = 12*sum(C_low*dsdp_low_m*PERWT)/sum(PERWT),
                             MC_high_m = 12*sum(C_high*dsdp_high_m*PERWT)/sum(PERWT),
                             dSdp_low = sum(dsdp_low*PERWT)/sum(PERWT),
                             dSdp_high = sum(dsdp_high*PERWT)/sum(PERWT),
                             dSdp_low_m = sum(dsdp_low_m*PERWT)/sum(PERWT),
                             dSdp_high_m = sum(dsdp_high_m*PERWT)/sum(PERWT),
                             # Markup = -1000*sum(shares*PERWT)/sum(dsdp*PERWT),
                             ConsWelfare = sum(CW*PERWT),
                             Pop = sum(PERWT)
        ),by=c("AV","P_base")]
        setkey(res_temp,AV)
        
        dSdp_m = as.matrix(res_temp[,c("dSdp_low","dSdp_high")])
        S_m = as.matrix(res_temp$share)
        MC_m = as.matrix(res_temp[,(MC_low+MC_high)/1000])
        res_temp[,fullPC:=sum(share*AC)/sum(share)]
        res_temp[,RA_PC:=sum(share*PC)/sum(share)]
        res_temp[,RA_adj:=fullPC/RA_PC]
        #res_temp[,avgRev:=sum(share*P_new*1000)/sum(share)]
        
        if(comp=="PC"){
          res_temp[,P_new:=(AC + lambda*(PC*RA_adj-AC))/1000]  
        }
        if(comp=="IP"){
          dSdp_m = as.matrix(res_temp[,c("dSdp_low","dSdp_high")])
          S_m = as.matrix(res_temp$share)
          MC_m = as.matrix(res_temp[,(MC_low+MC_high)/1000])
          
          M = J-1
          dSdp_r = as.matrix(res_temp[,c("dSdp_low_m","dSdp_high_m")])
          S_r = as.matrix(res_temp$share)
          MC_r = as.matrix(res_temp[,(MC_low+MC_high+M*(MC_low_m+MC_high_m))/1000])
          
          dSdp_c = dSdp_r
          S_merge = S_r
          MC_merge = MC_r
          for (i in 1:M){
            dSdp_c = cbind(dSdp_c,dSdp_r)
          }
          dSdp_merge = dSdp_c
          for (i in 1:M){
            dSdp_merge = rbind(dSdp_merge,dSdp_c)
            S_merge = rbind(S_merge,S_r)
            MC_merge = rbind(MC_merge,MC_r)
          }
          for (i in 0:M){
            dSdp_merge[i*2+1:2,i*2+1:2] = as.matrix(res_temp[,c("dSdp_low","dSdp_high")])
          }
          
          rm(dSdp_r,dSdp_c,S_r,MC_r)
          
         MC_all=MC_merge[1:2]
          
          
          
          
          
          res_temp[,P_new:=(solve(dSdp_m)%*%(-S_m + (1-lambda)*MC_m + lambda*(0.1*MC_all)) + lambda*0.9*PC*RA_adj/1000)]
          
        }
        # rm(dSdp_m,S_m,MC_m)
        
        err_h = with(res_temp[res_temp$AV==.8,],P_new-P_base)  
        p_1_h = p_1_h + err_h
        print(err_h)
        print(c(p_1_h))
        acs[,c("share_own_low","share_own_high","dsdp_low","dsdp_high","dsdp_low_m","dsdp_high_m"):=NULL]#"share_2_low","share_2_high","dsdp_low_m","dsdp_high_m"):=NULL]
      }
      print("UPDATE P_l")
      err_l = with(res_temp[res_temp$AV==.6,],P_new-P_base)
      p_1_l = p_1_l + err_l[1]
      
      print(c(p_1_l))
    }
    print("Lambda")
    print(lambda)
    res_temp$lambda=lambda
    res_temp$comp=comp
    optrisk = rbind(optrisk,res_temp)
  }
}

optrisk[,Profit:=share*(P_new-AC/1000)*Pop]
optrisk[,Profit_Total:=sum(Profit),by=c("lambda","comp")]
optrisk[,TotalSurplus:=ConsWelfare+10*Profit_Total]
optrisk[,TotalAvgCost:=sum(share*AC)/sum(share),by=c("lambda","comp")]

acs[,c("dsdp","shares","insured","dsdp_low","dsdp_high","dsdp_low_m","dsdp_high_m","CW","util","expsum","P_base","Price"):=NULL]


