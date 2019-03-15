rm(list=ls())
library(doBy)
library(randtoolbox)
library(data.table)
library(scales)
library(ggplot2)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition")

## Run
run = "2019-03-07"

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
costFile = paste("Intermediate_Output/Estimation_Parameters/MCestimation_",run,".csv",sep="")
mc_res = read.csv(costFile)
phi = mc_res$pars

# Use Georgia Fixed Effect
phi_st_avg = phi[5]


#### MEPS COMPARE ####
mepsFull = read.csv("Data/2015_MEPS/MEPS_Full_2015.csv")
meps = mepsFull[,c("HIEUIDX","DUPERSID","PID","PANEL","AGELAST","AGE15X","TTLP15X","POVLEV15","OFFER31X","OFFER42X","OFFER53X",
                   "TRIEV15","MCREV15","MCDEV15","OPAEV15","OPBEV15","ADSMOK42",
                   "INSCOV15","INSURC15","PERWT15F","TOTEXP15","UNINS15")]
ins = as.data.table(mepsFull[mepsFull$UNINS15==2,c("HIEUIDX","PID","AGELAST","SEX","TTLP15X","OFFER31X","OFFER42X","OFFER53X",
                                                   "TRIEV15","MCREV15","MCDEV15","OPAEV15","OPBEV15","ADSMOK42",
                                                   "INSCOV15","INSURC15","TOTEXP15","PERWT15F")])

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

meps$risk_positive="Zero"
meps$risk_positive[meps$HCC_Score_Silver>0]="Non-Zero"

acs[,risk_positive:="Zero"]
acs[HCC_Silver>0,risk_positive:="Non-Zero"]


bar_plot1 = summaryBy(PERWT15F~risk_positive,data=meps,FUN=sum,keep.names=TRUE)
bar_plot1$model="Data"
bar_plot1$dist = with(bar_plot1,PERWT15F/sum(PERWT15F))


bar_plot2 = summaryBy(PERWT~risk_positive,data=acs,FUN=sum,keep.names=TRUE)
bar_plot2$model="Model"
bar_plot2[,dist:=PERWT/sum(PERWT)]
barplot = rbind(bar_plot1[,c("risk_positive","model","dist")],
                bar_plot2[,c("risk_positive","model","dist")])
barplot$risk_positive = factor(barplot$risk_positive,levels=c("Zero","Non-Zero"))
### Risk Distribution
png("Writing/Images/RiskDist_Fit_Log.png",width=2500,height=1500,res=275)
ggplot() +
  geom_histogram(data=acs,aes(x=log(HCC_Silver+.001),weights=PERWT,y=..density..,fill="Model"),binwidth=.3,alpha=0.6)+
  geom_histogram(data=meps,aes(x=log(HCC_Score_Silver+.001),weights=PERWT15F,y=..density..,fill="Data"),binwidth=.3,alpha=0.6)+
  ylab("Density") +
  xlab("Log Risk Score") +
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
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


png("Writing/Images/RiskDist_Fit_Any.png",width=2500,height=1500,res=275)
ggplot(barplot) + aes(x=risk_positive,y=dist,fill=model) +
  geom_bar(position="dodge",stat="identity") +
  ylab("Probability") +
  xlab("Risk Score") +
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "right",
    axis.title=element_text(size=16),
    axis.text = element_text(size=14))
dev.off()



png("Writing/Images/RiskDist_Fit_Positive.png",width=2500,height=1500,res=275)
ggplot() +
  geom_histogram(data=acs[HCC_Silver>0,],aes(x=HCC_Silver,weights=PERWT,y=..density..,fill="Model"),binwidth=.3,alpha=0.6)+
  geom_histogram(data=meps[meps$HCC_Score_Silver>0,],aes(x=HCC_Score_Silver,weights=PERWT15F,y=..density..,fill="Data"),binwidth=.3,alpha=0.6)+
  ylab("Density") +
  xlab("Risk Score") +
  coord_cartesian(xlim=c(0,20)) +
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "none",
    axis.title=element_text(size=16),
    axis.text = element_text(size=14))
dev.off()

### Cost by Age
acs[,AV:=0.7]
## Apply Cost Function
acs[,C:=exp(phi[1]*AGE +
              phi[2]*AV +
              phi[3]*HCC_Silver +
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
            phi_st_avg)/AV]

ageMeans = acs[,list(avgCost=sum(PERWT*C)/sum(PERWT),Pop=sum(PERWT)),by=c("AGE")]
ageMeans[AGE<=22,yngAvg:=sum(avgCost*Pop)/sum(Pop)]
ageMeans[,yngAvg:=max(yngAvg,na.rm=TRUE)]
ageMeans[,c_index:=avgCost/yngAvg]
setkey(ageMeans,AGE)


ageMeans_MEPS = ins[AGELAST>=18&AGELAST<65,list(avgCost=sum(PERWT15F*TOTEXP15)/sum(PERWT15F),Pop=sum(PERWT15F)),by=c("AGELAST")]
ageMeans_MEPS[AGELAST<=22,yngAvg:=sum(avgCost*Pop)/sum(Pop)]
ageMeans_MEPS[,yngAvg:=max(yngAvg,na.rm=TRUE)]
ageMeans_MEPS[,c_index:=avgCost/yngAvg]
setkey(ageMeans_MEPS,AGELAST)

### Age- Cost Curve
png("Writing/Images/AgeCost_Fit.png",width=2500,height=1500,res=275)
ggplot() +
  geom_line(data=ageMeans,aes(x=AGE,y=c_index,color="Model"),size=1.5) +
  geom_point(data=ageMeans_MEPS,aes(x=AGELAST,y=c_index,color="Data",size=Pop/1e6))+
  xlab("Age") +
  ylab("Cost Index") +
  scale_size_continuous(guide="none") +
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
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

meps = as.data.table(meps)
meps[,HCC_rnd:=ceiling(HCC_Score_Silver)]

hccMeans_MEPS = meps[!is.na(HCC_rnd),list(avgCost=sum(PERWT15F*TOTEXP15)/sum(PERWT15F),
                                          avgHCC=sum(PERWT15F*HCC_Score_Silver)/sum(PERWT15F),
                                          Pop = sum(PERWT15F)),by=c("HCC_rnd")]
setkey(hccMeans_MEPS,HCC_rnd)
hccMeans_MEPS[HCC_rnd==0,LowAvg:=avgCost]
hccMeans_MEPS[,LowAvg:=max(LowAvg,na.rm=TRUE)]
hccMeans_MEPS[,c_index:=avgCost/LowAvg]

acs[,HCC_rnd:=ceiling(HCC_Silver)]
hccMeans = acs[,list(avgCost=sum(PERWT*C)/sum(PERWT),
                                          avgHCC=sum(PERWT*HCC_Silver)/sum(PERWT),
                                          Pop = sum(PERWT)),by=c("HCC_rnd")]
setkey(hccMeans,HCC_rnd)
hccMeans[HCC_rnd==0,LowAvg:=avgCost]
hccMeans[,LowAvg:=max(LowAvg,na.rm=TRUE)]
hccMeans[,c_index:=avgCost/LowAvg]

### Risk - Cost Curve
png("Writing/Images/RiskCost_Fit.png",width=2500,height=1500,res=275)
ggplot() +
  geom_line(data=hccMeans[HCC_rnd>0,],aes(x=HCC_rnd,y=c_index,color="Model"),size=1.2) +
  geom_point(data=hccMeans_MEPS[HCC_rnd>0&HCC_rnd<40,],aes(x=HCC_rnd,y=c_index,color="Data",size=Pop/1e6))+
  ylab("Cost Index") +
  xlab("Risk Score") +
  scale_size_continuous(guide="none") +
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
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




acs[,c("HCC_rnd"):=NULL]
acs[,c("risk_positive"):=NULL]


rm(ageMeans,ageMeans_MEPS,HCC,hccMeans,hccMeans_MEPS,ins,meps,meps_risk,mepsFull,mepsPers,r_mom)






#### Read in Parameters ####
parFile = paste("Intermediate_Output/Estimation_Parameters/estimationresults_GMM_",run,".csv",sep="")
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

acs[,addC:=exp(phi[1]*AGE + 
                 phi[2]*.7 + 
                    phi[3]*HCC_Silver + phi_st_avg)-
      exp(phi[1]*AGE/10 +  
            phi[2]*.6 +
            phi[3]*HCC_Silver + phi_st_avg)]



#### Two Products ####
acs_High = as.data.frame(acs)
acs_High$AV = 0.8
acs_Low = as.data.frame(acs)
acs_Low$AV = 0.6
acs = as.data.table(rbind(acs_High,acs_Low))
rm(acs_High,acs_Low)

## Apply Cost Function
acs[,C:=exp(phi[1]*AGE + 
              phi[2]*AV + 
              phi[3]*HCC_Silver + 
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





#### Add Competitor ####
acs_firm1 = as.data.frame(acs)
acs_firm2 = as.data.frame(acs)
acs_firm3 = as.data.frame(acs)
acs_firm1$Firm = "L"
acs_firm2$Firm = "S"
acs = as.data.table(rbind(acs_firm1,acs_firm2))#,acs_firm3))
setkey(acs,Person)
#rm(acs_firm1,acs_firm2,acs_firm3)

# 
# ##### Social Welfare Optimum #####
# output = NULL
# 
# J = 10
# pref_int = 5
# 
# p_1_l = 3
# p_2_l = 3
# p_1_h = p_1_l + 3
# p_2_h = p_2_l + 3
# acs_L = acs[Firm=="L",]
# demos = as.matrix(acs_L[,c("AgeFE_31_39",
#                            "AgeFE_40_51",
#                            "AgeFE_52_64",
#                            "Family",
#                            "LowIncome")])
# chars_risk = as.matrix(acs_L[,c("AV","Big")])
# err_l = 10 
# M = 0
# mandate = 0
# while(abs(sum(err_l))>.001){
#   err_h = 10
#   acs_L[AV==.6&Firm=="L",P_base:=p_1_l]
#   # acs_L[AV==.6&Firm==3,P_base:=p_3_l]
#   while (sum(abs(err_h))>.001){
#     acs_L[AV==.8&Firm=="L",P_base:=p_1_h]
#     # acs_L[AV==.8&Firm==3,P_base:=p_3_h]
#     
#     #acs_L[,Price:= pmax(P_base*ageRate-subsidy,0)]
#     acs_L[,Price:=P_base]
#     chars = as.matrix(acs_L[,c("Price","AV","Big")])
#     
#     intercept = demos%*%gamma
#     
#     # Only Price Coefficient
#     beta_z = (demos%*%t(beta))
#     beta_r = matrix(acs_L$HCC_Silver,nrow=nrow(acs_L),ncol=1)
#     
#     chars_val = intercept + chars%*%beta0 +
#       beta_z[,1]*chars[,1] + #Demographic Effect on Price
#       beta_r*chars[,2]*sigma[1] + #Risk Effect on AV
#       beta_r*chars[,3]*sigma[2] + pref_int #Risk Effect on Big
#     # mean(FE_pars)
#     # alpha = beta0[1] + beta_z[,1]
#     # price_val = chars[,1]*alpha
#     
#     
#     acs_L[,util:=exp(chars_val)]
#     acs_L[Firm=="L",util:=util]
#     
#     
#     acs_L[,alpha:=beta0[1] + beta_z[,1]]
#     acs_L[,expsum:=util]
#     
#     acs_L[Firm=="L",expsum:=J*expsum]
#     
#     acs_L[,expsum:=sum(expsum),by=c("Person")]
#     acs_L[,outside:=exp(alpha*mandate)]
#     acs_L[,shares:=util/(exp(alpha*mandate)+expsum)]
#     acs_L[,insured:=expsum/(exp(alpha*mandate)+expsum)]
#     
#     acs_L[AV==0.6,share_own_low:=shares]
#     acs_L[,share_own_low:=max(share_own_low,na.rm=TRUE),by=c("Person","Firm")]
#     acs_L[AV==0.8,share_own_high:=shares]
#     acs_L[,share_own_high:=max(share_own_high,na.rm=TRUE),by=c("Person","Firm")]
#     
#     
#     acs_L[,dsdp:=alpha*shares*(1-shares)]
#     acs_L[AV==0.6,dsdp_low:=alpha*share_own_low*(1-shares)]
#     acs_L[AV==0.8,dsdp_low:=-alpha*share_own_low*(shares)]
#     acs_L[,dsdp_low_m:=-alpha*share_own_low*shares]
#     
#     acs_L[AV==0.6,dsdp_high:=-alpha*share_own_high*(shares)]
#     acs_L[AV==0.8,dsdp_high:=alpha*share_own_high*(1-shares)]
#     acs_L[,dsdp_high_m:=-alpha*share_own_high*shares]
#     
#     
#     acs_L[,CW:=log(exp(alpha*mandate)+expsum)]
#     acs_L[,CW:=CW/(-alpha)]
#     acs_L[,lowrisk:=as.numeric(HCC_Silver==0)]
#     acs_L[,midrisk:=as.numeric(HCC_Silver>0&HCC_Silver<2.18)]
#     acs_L[,highrisk:=as.numeric(HCC_Silver>2.18)]
#     acs_L[,highestrisk:=as.numeric(HCC_Silver>10)]
#     
#     
#     res_temp = acs_L[,list(total_insured=sum(insured*PERWT)/sum(PERWT),
#                          share = sum(shares*PERWT)/sum(PERWT),
#                          AC = 12*sum(C*shares*PERWT)/sum(shares*PERWT),
#                          MC = 12*sum(C*dsdp*PERWT)/sum(dsdp*PERWT),
#                          PC = 12*sum(C*(share_own_low+share_own_high)*PERWT)/sum((share_own_low+share_own_high)*PERWT),
#                          MC_low = 12*sum(C_low*dsdp_low*PERWT)/sum(PERWT),
#                          MC_high = 12*sum(C_high*dsdp_high*PERWT)/sum(PERWT),
#                          MC_low_m = 12*sum(C_low*dsdp_low_m*PERWT)/sum(PERWT),
#                          MC_high_m = 12*sum(C_high*dsdp_high_m*PERWT)/sum(PERWT),
#                          dSdp_low = sum(dsdp_low*PERWT)/sum(PERWT),
#                          dSdp_high = sum(dsdp_high*PERWT)/sum(PERWT),
#                          dSdp_low_m = sum(dsdp_low_m*PERWT)/sum(PERWT),
#                          dSdp_high_m = sum(dsdp_high_m*PERWT)/sum(PERWT),
#                          Markup = -1000*sum(shares*PERWT)/sum(dsdp*PERWT),
#                          ConsWelfare = sum(CW*PERWT),
#                          CW_low = sum(CW*PERWT*lowrisk)/sum(lowrisk*PERWT),
#                          CW_mid = sum(CW*PERWT*midrisk)/sum(midrisk*PERWT),
#                          CW_high = sum(CW*PERWT*highrisk)/sum(highrisk*PERWT),
#                          CW_highest = sum(CW*PERWT*highestrisk)/sum(highestrisk*PERWT)
#     ),by=c("AV","P_base","Firm")]
#     setkey(res_temp,Firm,AV)
#     
#     if(M>0){
#       dSdp_r = as.matrix(res_temp[Firm=="L",c("dSdp_low_m","dSdp_high_m")])
#       S_r = as.matrix(res_temp$share[res_temp$Firm=="L"])
#       MC_r = as.matrix(res_temp[Firm=="L",(MC_low+MC_high+M*(MC_low_m+MC_high_m))/1000])
#       
#       dSdp_c = dSdp_r
#       S_merge = S_r
#       MC_merge = MC_r
#       for (i in 1:M){
#         dSdp_c = cbind(dSdp_c,dSdp_r)
#       }
#       dSdp_merge = dSdp_c
#       for (i in 1:M){
#         dSdp_merge = rbind(dSdp_merge,dSdp_c)
#         S_merge = rbind(S_merge,S_r)
#         MC_merge = rbind(MC_merge,MC_r)
#       }
#       for (i in 0:M){
#         dSdp_merge[i*2+1:2,i*2+1:2] = as.matrix(res_temp[Firm=="L",c("dSdp_low","dSdp_high")])
#       }
#       P_m_SW = (solve(dSdp_merge)%*%(MC_merge))[1:2]
#       rm(dSdp_r,dSdp_c,S_r,MC_r)
#       # # if(M==1){
#       # dSdp_merge = rbind(as.matrix(res_temp[Firm==1,c("dSdp_low","dSdp_high","dSdp_low_m","dSdp_high_m")]),
#       #                    as.matrix(res_temp[Firm==2,c("dSdp_low_m","dSdp_high_m","dSdp_low","dSdp_high")]))
#       # dSdp_merge[3:4,1:2] = dSdp_merge[1:2,3:4]
#       # 
#       # S_merge = as.matrix(res_temp$share[res_temp$Firm<3])
#       # MC_merge = as.matrix(res_temp[Firm<3,(MC_low+MC_high+MC_low_m+MC_high_m)/1000])
#       # P_m = (solve(dSdp_merge)%*%(-S_merge+MC_merge))
#       # # }
#       # 
#       res_temp[Firm=="L",P_new:=P_m_SW]
#       res_temp[Firm=="L",MC_all:=(solve(dSdp_merge)%*%(MC_merge))[1:2]]
#       res_temp[Firm=="L",Mkup_all:=(solve(dSdp_merge)%*%(-S_merge))[1:2]]
#       
#     }else{
#       
#       res_temp[Firm=="L",P_new:=(AC)/1000]
#     }
#     
#     err_h = with(res_temp[res_temp$AV==.8,],P_new-P_base)  
#     p_1_h = p_1_h + .75*err_h[1]
#     print(err_h)
#     #print(res_temp$P_new[res_temp$AV==.8])
#     print(c(p_1_h))
#     acs_L[,c("share_own_low","share_own_high","dsdp_low","dsdp_high"):=NULL]#"share_2_low","share_2_high","dsdp_low_m","dsdp_high_m"):=NULL]
#   }
#   print("UPDATE P_l")
#   err_l = with(res_temp[res_temp$AV==.6,],P_new-P_base)
#   p_1_l = p_1_l + .5*err_l[1]
#   #p_l = p_l +.5*err_l
#   print(c(p_1_l,p_2_l))
# }
# 
# PC_max = res_temp
# 
# SW_max = res_temp
# 
# 
# 
# 
##### Market Structure Comparison ####
output = NULL

J = 10
pref_int = 2

p_1_l = 3
p_2_l = 3
p_1_h = p_1_l + 3
p_2_h = p_2_l + 3

p_1_l = 3.573859
p_2_l = 3.070763
p_1_h = 8.237890
p_2_h = 8.412547
demos = as.matrix(acs[,c("AgeFE_31_39",
                         "AgeFE_40_51",
                         "AgeFE_52_64",
                         "Family",
                         "LowIncome")])
chars_risk = as.matrix(acs[,c("AV","Big")])
for (M in seq(0,8,1)){
  for (mandate in c(0,1.5)){
    print("Number of Merged Firms")
    print(M)
    print("Mandate")
    print(mandate)
    err_l = 10 
    while(abs(sum(err_l))>.001){
      err_h = 10
      acs[AV==.6&Firm=="L",P_base:=p_1_l]
      acs[AV==.6&Firm=="S",P_base:=p_2_l]
      # acs[AV==.6&Firm==3,P_base:=p_3_l]
      while (sum(abs(err_h))>.001){
        acs[AV==.8&Firm=="L",P_base:=p_1_h]
        acs[AV==.8&Firm=="S",P_base:=p_2_h]
        # acs[AV==.8&Firm==3,P_base:=p_3_h]
        
        #acs[,Price:= pmax(P_base*ageRate-subsidy,0)]
        acs[,Price:=P_base]
        chars = as.matrix(acs[,c("Price","AV","Big")])
        
        intercept = demos%*%gamma
        
        # Only Price Coefficient
        beta_z = (demos%*%t(beta))
        beta_r = matrix(acs$HCC_Silver,nrow=nrow(acs),ncol=1)
        
        chars_val = intercept + chars%*%beta0 +
          beta_z[,1]*chars[,1] + #Demographic Effect on Price
          beta_r*chars[,2]*sigma[1] + #Risk Effect on AV
          beta_r*chars[,3]*sigma[2] + pref_int #Risk Effect on Big
        # mean(FE_pars)
        # alpha = beta0[1] + beta_z[,1]
        # price_val = chars[,1]*alpha
        
        
        acs[,util:=exp(chars_val)]
        acs[Firm=="L",util:=util]
        acs[Firm=="S",util:=util]
        
        
        acs[,alpha:=beta0[1] + beta_z[,1]]
        acs[,expsum:=util]
        
        acs[Firm=="L",expsum:=(M+1)*expsum]
        acs[Firm=="S",expsum:=(J-M-1)*expsum]
        #acs[Firm==2,expsum:=expsum]
        #acs[Firm==3,expsum:=(J-2)*expsum]
        #acs[,expsum:=J*expsum]
        
        acs[,expsum:=sum(expsum),by=c("Person")]
        acs[,outside:=exp(alpha*mandate)]
        acs[,shares:=util/(exp(alpha*mandate)+expsum)]
        acs[,insured:=expsum/(exp(alpha*mandate)+expsum)]
        
        acs[AV==0.6,share_own_low:=shares]
        acs[,share_own_low:=max(share_own_low,na.rm=TRUE),by=c("Person","Firm")]
        acs[AV==0.8,share_own_high:=shares]
        acs[,share_own_high:=max(share_own_high,na.rm=TRUE),by=c("Person","Firm")]
        
        
        acs[,dsdp:=alpha*shares*(1-shares)]
        acs[AV==0.6,dsdp_low:=alpha*share_own_low*(1-shares)]
        acs[AV==0.8,dsdp_low:=-alpha*share_own_low*(shares)]
        acs[,dsdp_low_m:=-alpha*share_own_low*shares]
        acs[Firm=="S",dsdp_low_m:=NA]
        
        acs[AV==0.6,dsdp_high:=-alpha*share_own_high*(shares)]
        acs[AV==0.8,dsdp_high:=alpha*share_own_high*(1-shares)]
        acs[,dsdp_high_m:=-alpha*share_own_high*shares]
        acs[Firm=="S",dsdp_high_m:=NA]
        
        
        acs[,CW:=log(exp(alpha*mandate)+expsum)]
        acs[,CW:=CW/(-alpha)]
        acs[,lowrisk:=as.numeric(HCC_Silver==0)]
        acs[,midrisk:=as.numeric(HCC_Silver>0&HCC_Silver<2.18)]
        acs[,highrisk:=as.numeric(HCC_Silver>2.18)]
        acs[,highestrisk:=as.numeric(HCC_Silver>10)]
        
        
        res_temp = acs[,list(total_insured=sum(insured*PERWT)/sum(PERWT),
                             share = sum(shares*PERWT)/sum(PERWT),
                             AC = 12*sum(C*shares*PERWT)/sum(shares*PERWT),
                             MC = 12*sum(C*dsdp*PERWT)/sum(dsdp*PERWT),
                             PC = 12*sum(C*(share_own_low+share_own_high)*PERWT)/sum((share_own_low+share_own_high)*PERWT),
                             MC_low = 12*sum(C_low*dsdp_low*PERWT)/sum(PERWT),
                             MC_high = 12*sum(C_high*dsdp_high*PERWT)/sum(PERWT),
                             MC_low_m = 12*sum(C_low*dsdp_low_m*PERWT)/sum(PERWT),
                             MC_high_m = 12*sum(C_high*dsdp_high_m*PERWT)/sum(PERWT),
                             dSdp_low = sum(dsdp_low*PERWT)/sum(PERWT),
                             dSdp_high = sum(dsdp_high*PERWT)/sum(PERWT),
                             dSdp_low_m = sum(dsdp_low_m*PERWT)/sum(PERWT),
                             dSdp_high_m = sum(dsdp_high_m*PERWT)/sum(PERWT),
                             Markup = -1000*sum(shares*PERWT)/sum(dsdp*PERWT),
                             ConsWelfare = sum(CW*PERWT),
                             CW_low = sum(CW*PERWT*lowrisk)/sum(lowrisk*PERWT),
                             CW_mid = sum(CW*PERWT*midrisk)/sum(midrisk*PERWT),
                             CW_high = sum(CW*PERWT*highrisk)/sum(highrisk*PERWT),
                             CW_highest = sum(CW*PERWT*highestrisk)/sum(highestrisk*PERWT)
        ),by=c("AV","P_base","Firm")]
        setkey(res_temp,Firm,AV)
        res_temp[Firm=="S",P_new:=(Markup + MC)/1000]
        # res_temp[Firm=="S",P_new:=solve(as.matrix(res_temp[Firm=="S",c("dSdp_low","dSdp_high")]))%*%(-as.matrix(res_temp$share[res_temp$Firm=="S"]) + as.matrix(res_temp[Firm=="S",(MC_low+MC_high)/1000]))]
        res_temp[Firm=="S",MC_all:=solve(as.matrix(res_temp[Firm=="S",c("dSdp_low","dSdp_high")]))%*%(as.matrix(res_temp[Firm=="S",(MC_low+MC_high)/1000]))]
        res_temp[Firm=="S",Mkup_all:=solve(as.matrix(res_temp[Firm=="S",c("dSdp_low","dSdp_high")]))%*%(-as.matrix(res_temp$share[res_temp$Firm=="S"]))]
        
        if(M>0){
          dSdp_r = as.matrix(res_temp[Firm=="L",c("dSdp_low_m","dSdp_high_m")])
          S_r = as.matrix(res_temp$share[res_temp$Firm=="L"])
          MC_r = as.matrix(res_temp[Firm=="L",(MC_low+MC_high+M*(MC_low_m+MC_high_m))/1000])
          
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
            dSdp_merge[i*2+1:2,i*2+1:2] = as.matrix(res_temp[Firm=="L",c("dSdp_low","dSdp_high")])
          }
          P_m = (solve(dSdp_merge)%*%(-S_merge+MC_merge))[1:2]
          rm(dSdp_r,dSdp_c,S_r,MC_r)
          # # if(M==1){
          # dSdp_merge = rbind(as.matrix(res_temp[Firm==1,c("dSdp_low","dSdp_high","dSdp_low_m","dSdp_high_m")]),
          #                    as.matrix(res_temp[Firm==2,c("dSdp_low_m","dSdp_high_m","dSdp_low","dSdp_high")]))
          # dSdp_merge[3:4,1:2] = dSdp_merge[1:2,3:4]
          # 
          # S_merge = as.matrix(res_temp$share[res_temp$Firm<3])
          # MC_merge = as.matrix(res_temp[Firm<3,(MC_low+MC_high+MC_low_m+MC_high_m)/1000])
          # P_m = (solve(dSdp_merge)%*%(-S_merge+MC_merge))
          # # }
          # 
          res_temp[Firm=="L",P_new:=P_m]
          res_temp[Firm=="L",MC_all:=(solve(dSdp_merge)%*%(MC_merge))[1:2]]
          res_temp[Firm=="L",Mkup_all:=(solve(dSdp_merge)%*%(-S_merge))[1:2]]
          
        }else{
          
          res_temp[Firm=="L",P_new:=(Markup + MC)/1000]
          # res_temp[Firm=="L",P_new:=solve(as.matrix(res_temp[Firm=="L",c("dSdp_low","dSdp_high")]))%*%(-as.matrix(res_temp$share[res_temp$Firm=="L"]) + as.matrix(res_temp[Firm=="L",(MC_low+MC_high)/1000]))]
          res_temp[Firm=="L",MC_all:=solve(as.matrix(res_temp[Firm=="L",c("dSdp_low","dSdp_high")]))%*%(as.matrix(res_temp[Firm=="L",(MC_low+MC_high)/1000]))]
          res_temp[Firm=="L",Mkup_all:=solve(as.matrix(res_temp[Firm=="L",c("dSdp_low","dSdp_high")]))%*%(-as.matrix(res_temp$share[res_temp$Firm=="L"]))]
          
        }
        
        err_h = with(res_temp[res_temp$AV==.8,],P_new-P_base)  
        p_1_h = p_1_h + .75*err_h[1]
        p_2_h = p_2_h + .75*err_h[2]
        print(err_h)
        #print(res_temp$P_new[res_temp$AV==.8])
        print(c(p_1_h,p_2_h))
        acs[,c("share_own_low","share_own_high","dsdp_low","dsdp_high"):=NULL]#"share_2_low","share_2_high","dsdp_low_m","dsdp_high_m"):=NULL]
      }
      print("UPDATE P_l")
      err_l = with(res_temp[res_temp$AV==.6,],P_new-P_base)
      p_1_l = p_1_l + .5*err_l[1]
      p_2_l = p_2_l + .5*err_l[2]
      #p_l = p_l +.5*err_l
      print(c(p_1_l,p_2_l))
    }
    print("Total Insured")
    print(max(res_temp$total_insured))
    res_temp[,MergedFirms:=M]
    res_temp[,tau:=mandate]
    output = rbind(output,res_temp)
  }
}

Pop = sum(acs$PERWT)/4
#output[,total_insured:=sum(insured),by="FirmSize"]
#output[,total_AV:=sum(share),by=c("FirmSize","AV")]

save(output,file=paste("Estimation_Output/mergedFirmSimulation_",run,".rData"))
load(paste("Estimation_Output/mergedFirmSimulation_",run,".rData"))
output[,firmShare:=sum(share),by=c("MergedFirms","tau","Firm")]
output[Firm=="L",firmShare:=(1+MergedFirms)*firmShare/total_insured]
output[Firm=="S",firmShare:=NA]
output[,firmShare:=max(firmShare,na.rm=TRUE),by=c("MergedFirms","tau")]

output[Firm=="L",AV_share:=(MergedFirms+1)*share]
output[Firm=="S",AV_share:=(J-MergedFirms-1)*share]
output[,AV_share:=sum(AV_share),by=c("AV","MergedFirms","tau")]
output[,AV_share:=AV_share]


output[Firm=="L",Profit:=(MergedFirms+1)*Pop*share*(P_base-AC/1000)]
output[Firm=="S",Profit:=(J-MergedFirms-1)*Pop*share*(P_base-AC/1000)]
output[,Profit:=sum(Profit),by=c("MergedFirms","tau")]
output[,totalSurplus:=ConsWelfare+Profit]

output_wide_J = reshape(output[,],timevar=c("AV"),
                        idvar=c("MergedFirms","total_insured","firmShare","tau","Firm"),
                        direction="wide")
output_wide_J[,ConsWelfare.0.6:=ConsWelfare.0.8/Pop]
output_wide_J[,CW_low.0.8:=CW_low.0.8-output_wide_J$CW_low.0.8[nrow(output_wide_J)]]
output_wide_J[,CW_mid.0.8:=CW_mid.0.8-output_wide_J$CW_mid.0.8[nrow(output_wide_J)]]
output_wide_J[,CW_high.0.8:=CW_high.0.8-output_wide_J$CW_high.0.8[nrow(output_wide_J)]]
output_wide_J[,ConsWelfare.0.6:=ConsWelfare.0.6-output_wide_J$ConsWelfare.0.6[nrow(output_wide_J)]]


png("Writing/Images/SimIllus_NoMandateL.png",width=2000,height=1500,res=275)
ggplot(output_wide_J[tau==0&Firm=="L",]) +
  geom_line(aes(x=firmShare,y=MC_all.0.6,color="Marginal Cost"))+
  geom_point(aes(x=firmShare,y=MC_all.0.6,color="Marginal Cost",shape="Low Plan"),size=3)+
  geom_line(aes(x=firmShare,y=MC_all.0.8,color="Marginal Cost"))+
  geom_point(aes(x=firmShare,y=MC_all.0.8,color="Marginal Cost",shape="High Plan"),size=3)+
  geom_line(aes(x=firmShare,y=P_new.0.6,color="Price")) +
  geom_point(aes(x=firmShare,y=P_new.0.6,color="Price",shape="Low Plan"),size=3) +
  geom_line(aes(x=firmShare,y=P_new.0.8,color="Price"))  +
  geom_point(aes(x=firmShare,y=P_new.0.8,color="Price",shape="High Plan"),size=3)  +
  scale_y_continuous(label=dollar) +
  coord_cartesian(ylim=c(3,9)) + 
  scale_x_continuous(label=percent) +
  ylab("000s") +
  xlab("Market Share of Large Firm")+
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
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

png("Writing/Images/SimIllus_NoMandateS.png",width=2000,height=1500,res=275)
ggplot(output_wide_J[tau==0&Firm=="S",]) +
  geom_line(aes(x=firmShare,y=MC.0.6/1000,color="Marginal Cost"))+
  geom_point(aes(x=firmShare,y=MC.0.6/1000,color="Marginal Cost",shape="Low Plan"),size=3)+
  geom_line(aes(x=firmShare,y=MC.0.8/1000,color="Marginal Cost"))+
  geom_point(aes(x=firmShare,y=MC.0.8/1000,color="Marginal Cost",shape="High Plan"),size=3)+
  geom_line(aes(x=firmShare,y=P_new.0.6,color="Price")) +
  geom_point(aes(x=firmShare,y=P_new.0.6,color="Price",shape="Low Plan"),size=3) +
  geom_line(aes(x=firmShare,y=P_new.0.8,color="Price"))  +
  geom_point(aes(x=firmShare,y=P_new.0.8,color="Price",shape="High Plan"),size=3)  +
  scale_y_continuous(label=dollar) +
  scale_x_continuous(label=percent) +
  coord_cartesian(ylim=c(3,9)) + 
  ylab("000s") +
  xlab("Market Share of Large Firm")+
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
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



png("Writing/Images/SimIllus_MandateL.png",width=2000,height=1500,res=275)
ggplot(output_wide_J[tau==1.5&Firm=="L",]) +
  geom_line(aes(x=firmShare,y=MC_all.0.6,color="Marginal Cost"))+
  geom_point(aes(x=firmShare,y=MC_all.0.6,color="Marginal Cost",shape="Low Plan"),size=3)+
  geom_line(aes(x=firmShare,y=MC_all.0.8,color="Marginal Cost"))+
  geom_point(aes(x=firmShare,y=MC_all.0.8,color="Marginal Cost",shape="High Plan"),size=3)+
  geom_line(aes(x=firmShare,y=P_new.0.6,color="Price")) +
  geom_point(aes(x=firmShare,y=P_new.0.6,color="Price",shape="Low Plan"),size=3) +
  geom_line(aes(x=firmShare,y=P_new.0.8,color="Price"))  +
  geom_point(aes(x=firmShare,y=P_new.0.8,color="Price",shape="High Plan"),size=3)  +
  scale_y_continuous(label=dollar) +
  scale_x_continuous(label=percent) +
  coord_cartesian(ylim=c(2,8.5)) + 
  ylab("000s") +
  xlab("Market Share of Large Firm")+
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
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

png("Writing/Images/SimIllus_MandateS.png",width=2000,height=1500,res=275)
ggplot(output_wide_J[tau==1.5&Firm=="S",]) +
  geom_line(aes(x=firmShare,y=MC.0.6/1000,color="Marginal Cost"))+
  geom_point(aes(x=firmShare,y=MC.0.6/1000,color="Marginal Cost",shape="Low Plan"),size=3)+
  geom_line(aes(x=firmShare,y=MC.0.8/1000,color="Marginal Cost"))+
  geom_point(aes(x=firmShare,y=MC.0.8/1000,color="Marginal Cost",shape="High Plan"),size=3)+
  geom_line(aes(x=firmShare,y=P_new.0.6,color="Price")) +
  geom_point(aes(x=firmShare,y=P_new.0.6,color="Price",shape="Low Plan"),size=3) +
  geom_line(aes(x=firmShare,y=P_new.0.8,color="Price"))  +
  geom_point(aes(x=firmShare,y=P_new.0.8,color="Price",shape="High Plan"),size=3)  +
  scale_y_continuous(label=dollar) +
  scale_x_continuous(label=percent) +
  coord_cartesian(ylim=c(2,8.5)) + 
  ylab("000s") +
  xlab("Market Share of Large Firm")+
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
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

png("Writing/Images/SimIllus_Composition.png",width=2000,height=1500,res=275)
ggplot(output_wide_J[tau==0.0&Firm=="L",]) + 
  geom_line(aes(x=firmShare,y=AV_share.0.8,color="Market Share of 80% AV")) +
  geom_point(aes(x=firmShare,y=AV_share.0.8,color="Market Share of 80% AV"),size=3) +
  geom_line(aes(x=firmShare,y=total_insured,color="Total Insurance Rate"))  +
  geom_point(aes(x=firmShare,y=total_insured,color="Total Insurance Rate"),size=3)  + 
  scale_y_continuous(label=percent) + 
  scale_x_continuous(label=percent) + 
  ylab("") +
  xlab("Market Share of Large Firm")+
  coord_cartesian(ylim=c(0,.5)) +
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "bottom",
    axis.title=element_text(size=14),
    axis.text = element_text(size=14))
dev.off()


ggplot(output_wide_J[tau<2&MergedFirms%in%c(0,2,4,6,8),]) + 
 # geom_line(aes(x=MergedFirms,y=1000*P_new.0.6,color=as.factor(tau))) +
  geom_point(aes(x=MergedFirms,y=1000*P_new.0.6,color=as.factor(tau),shape=Firm),size=3) +
 # geom_line(aes(x=MergedFirms,y=1000*P_new.0.8,color=as.factor(tau)))  +
  geom_point(aes(x=MergedFirms,y=1000*P_new.0.8,color=as.factor(tau),shape=Firm),size=3)  + 
  scale_y_continuous(label=dollar) + 
  #scale_x_continuous(label=percent) + 
  ylab("000s") +
  xlab("Market Share of Large Firm")+
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "right",
    axis.title=element_text(size=14),
    axis.text = element_text(size=14))


output_wide_J[,NumProds:= factor(MergedFirms,levels=0:8,labels=c(1,4,6,8,10,12,14,16,18))]

png("Writing/Images/SimIllus_WelfareMandate.png",width=2500,height=1500,res=275)
ggplot() + 
  geom_line(data=output_wide_J,aes(x=tau,y=ConsWelfare.0.8/1e6,color=NumProds)) +
  scale_y_continuous(label=dollar) + 
  scale_x_continuous(label=dollar) + 
  scale_color_discrete("Number of Products\nOwned By Large Firm") + 
  ylab("Total Consumer Surplus (millions)") +
  xlab("Value of Mandate Penalty \n(000s)")+
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_blank(),
    legend.title=element_text(size=14),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "right",
    axis.title=element_text(size=14),
    axis.text = element_text(size=14))
dev.off()


##### Risk Adjustment and Market Structure ####
output = NULL

J = 10
pref_int = 5

p_1_l = 3
p_2_l = 3
p_1_h = p_1_l + 3
p_2_h = p_2_l + 3
demos = as.matrix(acs[,c("AgeFE_31_39",
                         "AgeFE_40_51",
                         "AgeFE_52_64",
                         "Family",
                         "LowIncome")])
chars_risk = as.matrix(acs[,c("AV","Big")])
mandate=0
for (M in seq(0,8,1)){
  for (risk_adj in c(0,1)){
    print("Number of Merged Firms")
    print(M)
    print("Risk Adj")
    print(risk_adj)
    err_l = 10 
    while(abs(sum(err_l))>.001){
      err_h = 10
      acs[AV==.6&Firm=="L",P_base:=p_1_l]
      acs[AV==.6&Firm=="S",P_base:=p_2_l]
      # acs[AV==.6&Firm==3,P_base:=p_3_l]
      while (sum(abs(err_h))>.001){
        acs[AV==.8&Firm=="L",P_base:=p_1_h]
        acs[AV==.8&Firm=="S",P_base:=p_2_h]
        # acs[AV==.8&Firm==3,P_base:=p_3_h]
        
        #acs[,Price:= pmax(P_base*ageRate-subsidy,0)]
        acs[,Price:=P_base]
        chars = as.matrix(acs[,c("Price","AV","Big")])
        
        intercept = demos%*%gamma
        
        # Only Price Coefficient
        beta_z = (demos%*%t(beta))
        beta_r = matrix(acs$HCC_Silver,nrow=nrow(acs),ncol=1)
        
        chars_val = intercept + chars%*%beta0 +
          beta_z[,1]*chars[,1] + #Demographic Effect on Price
          beta_r*chars[,2]*sigma[1] + #Risk Effect on AV
          beta_r*chars[,3]*sigma[2] + pref_int #Risk Effect on Big
        # mean(FE_pars)
        # alpha = beta0[1] + beta_z[,1]
        # price_val = chars[,1]*alpha
        
        
        acs[,util:=exp(chars_val)]
        acs[Firm=="L",util:=util]
        acs[Firm=="S",util:=util]
        
        
        acs[,alpha:=beta0[1] + beta_z[,1]]
        acs[,expsum:=util]
        
        acs[Firm=="L",expsum:=(M+1)*expsum]
        acs[Firm=="S",expsum:=(J-M-1)*expsum]
        #acs[Firm==2,expsum:=expsum]
        #acs[Firm==3,expsum:=(J-2)*expsum]
        #acs[,expsum:=J*expsum]
        
        acs[,expsum:=sum(expsum),by=c("Person")]
        acs[,outside:=exp(alpha*mandate)]
        acs[,shares:=util/(exp(alpha*mandate)+expsum)]
        acs[,insured:=expsum/(exp(alpha*mandate)+expsum)]
        
        acs[AV==0.6,share_own_low:=shares]
        acs[,share_own_low:=max(share_own_low,na.rm=TRUE),by=c("Person","Firm")]
        acs[AV==0.8,share_own_high:=shares]
        acs[,share_own_high:=max(share_own_high,na.rm=TRUE),by=c("Person","Firm")]
        
        
        acs[,dsdp:=alpha*shares*(1-shares)]
        acs[AV==0.6,dsdp_low:=alpha*share_own_low*(1-shares)]
        acs[AV==0.8,dsdp_low:=-alpha*share_own_low*(shares)]
        acs[,dsdp_low_m:=-alpha*share_own_low*shares]
        acs[Firm=="S",dsdp_low_m:=NA]
        
        acs[AV==0.6,dsdp_high:=-alpha*share_own_high*(shares)]
        acs[AV==0.8,dsdp_high:=alpha*share_own_high*(1-shares)]
        acs[,dsdp_high_m:=-alpha*share_own_high*shares]
        acs[Firm=="S",dsdp_high_m:=NA]
        
        
        acs[,CW:=log(exp(alpha*mandate)+expsum)]
        acs[,CW:=CW/(-alpha)]
        acs[,lowrisk:=as.numeric(HCC_Silver==0)]
        acs[,midrisk:=as.numeric(HCC_Silver>0&HCC_Silver<2.18)]
        acs[,highrisk:=as.numeric(HCC_Silver>2.18)]
        acs[,highestrisk:=as.numeric(HCC_Silver>10)]
        
        
        res_temp = acs[,list(total_insured=sum(insured*PERWT)/sum(PERWT),
                             share = sum(shares*PERWT)/sum(PERWT),
                             AC = 12*sum(C*shares*PERWT)/sum(shares*PERWT),
                             MC = 12*sum(C*dsdp*PERWT)/sum(dsdp*PERWT),
                             PC = 12*sum(C*(share_own_low+share_own_high)*PERWT)/sum((share_own_low+share_own_high)*PERWT),
                             MC_low = 12*sum(C_low*dsdp_low*PERWT)/sum(PERWT),
                             MC_high = 12*sum(C_high*dsdp_high*PERWT)/sum(PERWT),
                             MC_low_m = 12*sum(C_low*dsdp_low_m*PERWT)/sum(PERWT),
                             MC_high_m = 12*sum(C_high*dsdp_high_m*PERWT)/sum(PERWT),
                             dSdp_low = sum(dsdp_low*PERWT)/sum(PERWT),
                             dSdp_high = sum(dsdp_high*PERWT)/sum(PERWT),
                             dSdp_low_m = sum(dsdp_low_m*PERWT)/sum(PERWT),
                             dSdp_high_m = sum(dsdp_high_m*PERWT)/sum(PERWT),
                             Markup = -1000*sum(shares*PERWT)/sum(dsdp*PERWT),
                             ConsWelfare = sum(CW*PERWT),
                             CW_low = sum(CW*PERWT*lowrisk)/sum(lowrisk*PERWT),
                             CW_mid = sum(CW*PERWT*midrisk)/sum(midrisk*PERWT),
                             CW_high = sum(CW*PERWT*highrisk)/sum(highrisk*PERWT),
                             CW_highest = sum(CW*PERWT*highestrisk)/sum(highestrisk*PERWT)
        ),by=c("AV","P_base","Firm")]
        setkey(res_temp,Firm,AV)
        res_temp[,firmShare:=sum(share),by="Firm"]
        res_temp[Firm=="L",firmShare:=firmShare*(1+M)]
        res_temp[,firmShare:=firmShare/total_insured]
        res_temp[Firm=="S",P_new:=(Markup + (1-risk_adj)*MC+risk_adj*((1-firmShare)*PC+firmShare*MC))/1000]
        # res_temp[Firm=="S",P_new:=solve(as.matrix(res_temp[Firm=="S",c("dSdp_low","dSdp_high")]))%*%(-as.matrix(res_temp$share[res_temp$Firm=="S"]) + as.matrix(res_temp[Firm=="S",(MC_low+MC_high)/1000]))]
        res_temp[Firm=="S",MC_all:=MC]
        res_temp[Firm=="S",Mkup_all:=Markup]
        
        if(M>0){
          dSdp_r = as.matrix(res_temp[Firm=="L",c("dSdp_low_m","dSdp_high_m")])
          S_r = as.matrix(res_temp$share[res_temp$Firm=="L"])
          MC_r = as.matrix(res_temp[Firm=="L",(MC_low+MC_high+M*(MC_low_m+MC_high_m))/1000])
          
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
            dSdp_merge[i*2+1:2,i*2+1:2] = as.matrix(res_temp[Firm=="L",c("dSdp_low","dSdp_high")])
          }
          P_m = (solve(dSdp_merge)%*%(-S_merge+MC_merge))[1:2]
          rm(dSdp_r,dSdp_c,S_r,MC_r)
          # # if(M==1){
          # dSdp_merge = rbind(as.matrix(res_temp[Firm==1,c("dSdp_low","dSdp_high","dSdp_low_m","dSdp_high_m")]),
          #                    as.matrix(res_temp[Firm==2,c("dSdp_low_m","dSdp_high_m","dSdp_low","dSdp_high")]))
          # dSdp_merge[3:4,1:2] = dSdp_merge[1:2,3:4]
          # 
          # S_merge = as.matrix(res_temp$share[res_temp$Firm<3])
          # MC_merge = as.matrix(res_temp[Firm<3,(MC_low+MC_high+MC_low_m+MC_high_m)/1000])
          # P_m = (solve(dSdp_merge)%*%(-S_merge+MC_merge))
          # # }
          # 
          
          res_temp[Firm=="L",MC_all:=(solve(dSdp_merge)%*%(MC_merge))[1:2]]
          res_temp[Firm=="L",Mkup_all:=(solve(dSdp_merge)%*%(-S_merge))[1:2]]
          res_temp[Firm=="L",P_new:= Mkup_all + MC_all*(1-risk_adj) + risk_adj*((1-firmShare)*PC/1000+firmShare*MC_all)]
          
        }else{
          
          res_temp[Firm=="L",P_new:=(Markup + (1-risk_adj)*MC+risk_adj*((1-firmShare)*PC+firmShare*MC))/1000]
          # res_temp[Firm=="S",P_new:=solve(as.matrix(res_temp[Firm=="S",c("dSdp_low","dSdp_high")]))%*%(-as.matrix(res_temp$share[res_temp$Firm=="S"]) + as.matrix(res_temp[Firm=="S",(MC_low+MC_high)/1000]))]
          res_temp[Firm=="L",MC_all:=MC]
          res_temp[Firm=="L",Mkup_all:=Markup]
        }
        
        err_h = with(res_temp[res_temp$AV==.8,],P_new-P_base)  
        p_1_h = p_1_h + .75*err_h[1]
        p_2_h = p_2_h + .75*err_h[2]
        print(err_h)
        #print(res_temp$P_new[res_temp$AV==.8])
        print(c(p_1_h,p_2_h))
        acs[,c("share_own_low","share_own_high","dsdp_low","dsdp_high"):=NULL]#"share_2_low","share_2_high","dsdp_low_m","dsdp_high_m"):=NULL]
      }
      print("UPDATE P_l")
      err_l = with(res_temp[res_temp$AV==.6,],P_new-P_base)
      p_1_l = p_1_l + .5*err_l[1]
      p_2_l = p_2_l + .5*err_l[2]
      #p_l = p_l +.5*err_l
      print(c(p_1_l,p_2_l))
    }
    print("Total Insured")
    print(max(res_temp$total_insured))
    res_temp[,MergedFirms:=M]
    res_temp[,tau:=risk_adj]
    output = rbind(output,res_temp)
  }
}

save(output,file=paste("Estimation_Output/RiskAdjSimulation_",run,".rData"))

Pop = sum(acs$PERWT)/4

output[,firmShare:=sum(share),by=c("MergedFirms","tau","Firm")]
output[Firm=="L",firmShare:=(1+MergedFirms)*firmShare/total_insured]
output[Firm=="S",firmShare:=NA]
output[,firmShare:=max(firmShare,na.rm=TRUE),by=c("MergedFirms","tau")]

output[Firm=="L",Profit:=(MergedFirms+1)*Pop*share*(P_base-AC/1000)]
output[Firm=="S",Profit:=(J-MergedFirms-1)*Pop*share*(P_base-AC/1000)]
output[,Profit:=sum(Profit),by=c("MergedFirms","tau")]
output[,totalSurplus:=ConsWelfare+Profit]

output_wide_J = reshape(output[,],timevar=c("AV"),
                        idvar=c("MergedFirms","total_insured","firmShare","tau","Firm"),
                        direction="wide")
output_wide_J[,ConsWelfare.0.6:=ConsWelfare.0.8/Pop]
output_wide_J[tau==0,CW_low_bench:=CW_low.0.6]
output_wide_J[,CW_low_bench:=max(CW_low_bench,na.rm=TRUE),by="MergedFirms"]
output_wide_J[,CW_low_effect:=(CW_low.0.6-CW_low_bench)*1000]

output_wide_J[tau==0,CW_high_bench:=CW_highest.0.6]
output_wide_J[,CW_high_bench:=max(CW_high_bench,na.rm=TRUE),by="MergedFirms"]
output_wide_J[,CW_high_effect:=(CW_highest.0.6-CW_high_bench)*1000]

output_wide_J[,ConsWelfare.0.6:=ConsWelfare.0.6-output_wide_J$ConsWelfare.0.6[nrow(output_wide_J)]]

output_wide_J[,RA:=factor(tau,levels=c(0,1),labels=c("Without Risk Adj","With Risk Adj"))]


png("Writing/Images/SimIllus_RAL.png",width=2000,height=1500,res=275)
ggplot(output_wide_J[Firm=="L",]) +
  geom_line(aes(x=firmShare,y=P_new.0.6,color=RA)) +
  geom_point(aes(x=firmShare,y=P_new.0.6,color=RA,shape="Low Plan"),size=3) +
  geom_line(aes(x=firmShare,y=P_new.0.8,color=RA))  +
  geom_point(aes(x=firmShare,y=P_new.0.8,color=RA,shape="High Plan"),size=3)  +
  scale_y_continuous(label=dollar) +
  scale_x_continuous(label=percent) +
  ylab("Premium (000s)") +
  xlab("Market Share of Large Firm")+
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
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

png("Writing/Images/SimIllus_RAS.png",width=2000,height=1500,res=275)
ggplot(output_wide_J[Firm=="S",]) +
  geom_line(aes(x=firmShare,y=P_new.0.6,color=RA)) +
  geom_point(aes(x=firmShare,y=P_new.0.6,color=RA,shape="Low Plan"),size=3) +
  geom_line(aes(x=firmShare,y=P_new.0.8,color=RA))  +
  geom_point(aes(x=firmShare,y=P_new.0.8,color=RA,shape="High Plan"),size=3)  +
  scale_y_continuous(label=dollar) +
  scale_x_continuous(label=percent) +
  ylab("Premium (000s)") +
  xlab("Market Share of Large Firm")+
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
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

png("Writing/Images/SimIllus_RA_CW.png",width=2000,height=1500,res=275)
ggplot(output_wide_J[Firm=="L"&tau==1,]) +
  geom_line(aes(x=firmShare,y=CW_high_effect,color="High Risks")) +
  geom_point(aes(x=firmShare,y=CW_high_effect,color="High Risks",shape="High Risks"),size=3) +
  geom_line(aes(x=firmShare,y=CW_low_effect,color="Low Risks"))  +
  geom_point(aes(x=firmShare,y=CW_low_effect,color="Low Risks",shape="Low Risks"),size=3)  +
  geom_hline(yintercept = 0) + 
  scale_y_continuous(label=dollar) +
  scale_x_continuous(label=percent) +
  ylab("Change in Average Consumer Surplus") +
  xlab("Market Share of Large Firm")+
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
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

