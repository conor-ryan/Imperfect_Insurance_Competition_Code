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
# 
# ### Risk Distribution
# png("Writing/Images/RiskDist_Fit.png",width=2500,height=1500,res=275)
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
# 
# 
# 
# rm(ageMeans,ageMeans_MEPS,HCC,hccMeans,hccMeans_MEPS,ins,meps,meps_risk,mepsFull,mepsPers,r_mom)
# 
# 




#### Read in Parameters ####
parFile = paste("Estimation_Output/estimationresults_",run,".csv",sep="")
pars = read.csv(parFile)

beta_vec = pars$pars
beta_vec[1:15] = c(0.514611, 0.545914, 0.165789, 0.0431966, -3.14873, -1.29452, 3.88508, -0.129367, 0.11211, 0.342127, 0.726742, 0.0248219, -0.147222, 0.711456, 0.0103048)


gamma = beta_vec[1:5]
beta0 = beta_vec[6:8]
beta = matrix(0,nrow=3,ncol=5)
beta[1,1:ncol(beta)] = beta_vec[9:13]
sigma = beta_vec[14:15]
FE_pars = beta_vec[16:length(beta_vec)]
FE_avg = FE_pars[1] # Let Aetna be the representative firm

# # Make Consumers More Price Elastic, holding WTP constant
# beta0[1:2] = beta0[1:2]*2
# beta[1,] = beta[1,]*2
# sigma[1] = sigma[1]*8

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

# 
# # #### Single Product ####
# #pref_int = FE_avg + 1#+ mean(FE_pars)
# pref_int = 2
# ## Monopolist
# J = 1
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
# output = NULL
# for (p_l in seq(-.25,12,.1)){
#   print(p_l)
#   acs[,P_base:=p_l]
#   #acs[AV==.6,P_base:=p_l]
#   #acs[,Benchmark:=p_l*(.7/.6)]
#   #acs[,subsidy:=pmax(Benchmark*ageRate-HHcont*HH_income/1000,0)]
#   
#   err = 10
#   p_h = p_l + 3
#   cnt = 0
#   while (abs(err)>.005){
#     cnt = cnt+1
#     #acs[AV==.8,P_base:=p_h]
#     
#     #acs[,Price:= pmax(P_base*ageRate-subsidy,0)]
#     acs[,Price:=P_base]
#     
#     demos = as.matrix(acs[,c("AgeFE_31_39",
#                              "AgeFE_40_51",
#                              "AgeFE_52_64",
#                              "Family",
#                              "LowIncome")])
#     chars_risk = as.matrix(acs[,c("AV","Big")])
#     chars = as.matrix(acs[,c("Price","AV","Big")])
#     
#     intercept = demos%*%gamma
#     
#     # Only Price Coefficient
#     beta_z = (demos%*%t(beta))
#     beta_r = matrix(acs$HCC_Silver,nrow=nrow(acs),ncol=length(sigma),byrow=FALSE)*sigma
#     
#     chars_val = chars%*%beta0 +
#       beta_z[,1]*chars[,1] + #Demographic Effect on Price
#       beta_r[,1]*chars[,2] + #Risk Effect on AV
#       beta_r[,2]*chars[,3] + #Risk Effect on Big
#       pref_int #+mean(FE_pars)
#     # alpha = beta0[1] + beta_z[,1]
#     # price_val = chars[,1]*alpha
#     
#     acs[,util:=exp(chars_val)]
#     acs[,alpha:=beta0[1] + beta_z[,1]]
#     acs[,expsum:=J*sum(util),by="Person"]
#     acs[,shares:=util/(1+expsum)]
#     
#     # acs[AV==0.6,share_low:=shares]
#     # acs[,share_low:=max(share_low,na.rm=TRUE),by="Person"]
#     # acs[AV==0.8,share_high:=shares]
#     # acs[,share_high:=max(share_high,na.rm=TRUE),by="Person"]
#     
#     acs[,dsdp:=alpha*shares*(1-shares)]
#     # acs[AV==0.6,dsdp_low:=alpha*share_low*(1-shares)]
#     # acs[AV==0.8,dsdp_low:=-alpha*share_low*(shares)]
#     #
#     # acs[AV==0.6,dsdp_high:=-alpha*share_high*(shares)]
#     # acs[AV==0.8,dsdp_high:=alpha*share_high*(1-shares)]
#     #
#     #
#     res_temp = acs[,list(insured=sum(J*shares*PERWT)/sum(PERWT),
#                          SA = sum(shares*ageRate*PERWT)/sum(PERWT),
#                          AC = 12*sum(C*shares*PERWT)/sum(shares*PERWT),
#                          #MC_low = 12*sum(C*dsdp_low*PERWT)/sum(PERWT),
#                          #MC_high = 12*sum(C*dsdp_high*PERWT)/sum(PERWT),
#                          #dSAdp_low = sum(dsdp_low*ageRate*PERWT)/sum(PERWT),
#                          #dSAdp_high = sum(dsdp_high*ageRate*PERWT)/sum(PERWT),
#                          # MC_A = 12*sum(C*dsdp*PERWT)/sum(dsdp*ageRate*PERWT),
#                          MC = 12*sum(C*dsdp*PERWT)/sum(dsdp*PERWT),
#                          #Markup_A = -1000*sum(shares*ageRate*PERWT)/sum(dsdp*ageRate*PERWT),
#                          Markup = -1000*sum(shares*PERWT)/sum(dsdp*PERWT)
#     ),by=c("AV","P_base")]
#     setkey(res_temp,AV)
#     # res_temp[,P_new:=solve(as.matrix(res_temp[,c("dSAdp_low","dSAdp_high")]))%*%(-as.matrix(res_temp$SA) + as.matrix(res_temp[,(MC_low+MC_high)/1000]))]
#     #  res_temp[,MC:=1000*solve(as.matrix(res_temp[,c("dSAdp_low","dSAdp_high")]))%*%(as.matrix(res_temp[,(MC_low+MC_high)/1000]))]
#     #  res_temp[,Markup:=1000*solve(as.matrix(res_temp[,c("dSAdp_low","dSAdp_high")]))%*%(-as.matrix(res_temp$SA))]
#     
#     #res_temp[,P_new:=(MC_A+Markup_A)/1000]
#     res_temp[,P_new:=(MC+Markup)/1000]
#     res_temp[,P_low:=p_l]
#     #err = with(res_temp[res_temp$AV==.8,],P_new-P_base)
#     #p_h = p_h + err
#     print(err)
#     err = 0
#     
#     #print(res_temp$P_new[res_temp$AV==.8])
#     #print(p_h)
#   }
#   res_temp[,total_insured:=sum(insured)]
#   # print(p_h)
#   
#   # res_temp = acs[,list(insured=sum(shares*PERWT)/sum(PERWT),
#   #                      AC = 12*sum(C*shares*PERWT)/sum(shares*PERWT),
#   #                      MC = 12*sum(C*dsdp*PERWT)/sum(dsdp*PERWT),
#   #                      Markup = -12*sum(shares*PERWT)/sum(dsdp*PERWT)),by=c("AV","Price")]
#   #res_temp[,c("P_base","MC_low","MC_high","dSAdp_low","dSAdp_high"):=NULL]
#   output = rbind(output,res_temp)
# }
# 
# output[,total_insured:=sum(insured),by="P_low"]
# output_wide = reshape(output,timevar=c("AV"),
#                       idvar=c("P_low","total_insured"),
#                       direction="wide")
# #output_wide[,Eq:=abs(P_low-P_new.0.6)]
# #output_wide[,P_diff:=1000*(P_new.0.8-P_low)]
# 
# output_wide[Eq<.5,]
# 
# png("Writing/Images/SelectionExampleBasic.png",width=2500,height=1500,res=275)
# ggplot() +
#   geom_line(data=output_wide,aes(x=total_insured,y=AC.0.7,color="red"))+
#   geom_line(data=output_wide,aes(x=total_insured,y=MC.0.7,color="blue"))+
#   geom_line(data=output_wide,aes(x=total_insured,y=1000*P_low,color="black")) +
#   geom_line(data=output_wide,aes(x=total_insured,y=1000*P_new.0.7,color="green")) +
#   geom_point(aes(x=.94,y=2059),size=2) + 
#   geom_text(aes(x=.94,y=2059),label="p^w",parse=TRUE,vjust=0,size=6) + 
#   geom_point(aes(x=.625,y=4350),size=2) + 
#   geom_text(aes(x=.625,y=4350),label="p^c",parse=TRUE,vjust=0,size=6) + 
#   geom_point(aes(x=.245,y=8075),size=2) + 
#   geom_text(aes(x=.245,y=8075),label="p^m",parse=TRUE,vjust=0,size=6) + 
#   xlab("Q(P)") + 
#   scale_y_continuous(label=dollar) + 
#   scale_colour_manual(values = c('red' = 'red','blue' = 'blue','green'='green','black'='black'),
#                       name = '',
#                       labels = expression(AC,MC,MC+Mkup,P)) + 
#   ylab("") +
#   coord_cartesian(xlim=c(.15,.95),ylim=c(1000,15000))+ 
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
# 




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

# #### Two Product Example Simulations ####
# 
# #pref_int = FE_avg + 1#+ mean(FE_pars)
# pref_int = 2
# ## Oligopoly
# J = 1
# output = NULL
# for (p_l in seq(-.25,12,.5)){
#   print(p_l)
#   acs[AV==.6,P_base:=p_l]
#   #acs[,Benchmark:=p_l*(.7/.6)]
#   #acs[,subsidy:=pmax(Benchmark*ageRate-HHcont*HH_income/1000,0)]
#   
#   err = 10
#   p_h = min(p_l + 3,5)
#   cnt = 0
#   while (abs(sum(err))>.005){
#     #acs[AV==.6,P_base:=p_l]
#     acs[AV==.8,P_base:=p_h]
#     
#     #acs[,Price:= pmax(P_base*ageRate-subsidy,0)]
#     acs[,Price:=P_base]
#     
#     demos = as.matrix(acs[,c("AgeFE_31_39",
#                              "AgeFE_40_51",
#                              "AgeFE_52_64",
#                              "Family",
#                              "LowIncome")])
#     chars_risk = as.matrix(acs[,c("AV","Big")])
#     chars = as.matrix(acs[,c("Price","AV","Big")])
#     
#     intercept = demos%*%gamma
#     
#     # Only Price Coefficient
#     beta_z = (demos%*%t(beta))
#     beta_r = matrix(acs$HCC_Silver,nrow=nrow(acs),ncol=length(sigma),byrow=FALSE)*sigma
#     
#     chars_val = chars%*%beta0 +
#       beta_z[,1]*chars[,1] + #Demographic Effect on Price
#       beta_r[,1]*chars[,2] + #Risk Effect on AV
#       beta_r[,2]*chars[,3] + #Risk Effect on Big
#       pref_int #+mean(FE_pars)
#     # alpha = beta0[1] + beta_z[,1]
#     # price_val = chars[,1]*alpha
#     
#     acs[,util:=exp(chars_val)]
#     acs[,alpha:=beta0[1] + beta_z[,1]]
#     acs[,expsum:=J*sum(util),by="Person"]
#     acs[,shares:=util/(1+expsum)]
#     
#     # acs[AV==0.6,share_low:=shares]
#     # acs[,share_low:=max(share_low,na.rm=TRUE),by="Person"]
#     # acs[AV==0.8,share_high:=shares]
#     # acs[,share_high:=max(share_high,na.rm=TRUE),by="Person"]
#     # 
#     acs[,dsdp:=alpha*shares*(1-shares)]
#     # acs[AV==0.6,dsdp_low:=alpha*share_low*(1-shares)]
#     # acs[AV==0.8,dsdp_low:=-alpha*share_low*(shares)]
#     # 
#     # acs[AV==0.6,dsdp_high:=-alpha*share_high*(shares)]
#     # acs[AV==0.8,dsdp_high:=alpha*share_high*(1-shares)]
#     #
#     #
#     res_temp = acs[,list(insured=sum(J*shares*PERWT)/sum(PERWT),
#                          SA = sum(shares*PERWT)/sum(PERWT),
#                          AC = 12*sum(C*shares*PERWT)/sum(shares*PERWT),
#                          # MC_low = 12*sum(C*dsdp_low*PERWT)/sum(PERWT),
#                          # MC_high = 12*sum(C*dsdp_high*PERWT)/sum(PERWT),
#                          # dSAdp_low = sum(dsdp_low*PERWT)/sum(PERWT),
#                          # dSAdp_high = sum(dsdp_high*PERWT)/sum(PERWT),
#                          # MC_A = 12*sum(C*dsdp*PERWT)/sum(dsdp*ageRate*PERWT),
#                          MC = 12*sum(C*dsdp*PERWT)/sum(dsdp*PERWT),
#                          #Markup_A = -1000*sum(shares*ageRate*PERWT)/sum(dsdp*ageRate*PERWT),
#                          Markup = -1000*sum(shares*PERWT)/sum(dsdp*PERWT)
#     ),by=c("AV","P_base")]
#     setkey(res_temp,AV)
#     # res_temp[,P_new:=solve(as.matrix(res_temp[,c("dSAdp_low","dSAdp_high")]))%*%(-as.matrix(res_temp$SA) + as.matrix(res_temp[,(MC_low+MC_high)/1000]))]
#     # res_temp[,MC_2:=1000*solve(as.matrix(res_temp[,c("dSAdp_low","dSAdp_high")]))%*%(as.matrix(res_temp[,(MC_low+MC_high)/1000]))]
#     # res_temp[,Markup_2:=1000*solve(as.matrix(res_temp[,c("dSAdp_low","dSAdp_high")]))%*%(-as.matrix(res_temp$SA))]
#     
#     #res_temp[,P_new:=(MC_A+Markup_A)/1000]
#     res_temp[,P_new:=(MC+Markup)/1000]
#     res_temp[,P_low:=p_l]
#     #err = with(res_temp[res_temp$AV==.8,],P_new-P_base)
#     err = with(res_temp,P_new-P_base)
#     # p_l = max(min(p_l + .25*err[1],40),0)
#     err[1] = 0
#     p_h = max(min(p_h + .5*err[2],40),0)
#     print(err)
#     if (p_h==0|p_h==40){
#       cnt = cnt+1
#     }
#     if(cnt>2){
#       err = 0
#     }
#     #print(res_temp$P_new[res_temp$AV==.8])
#     # acs[,c("share_low","share_high","dsdp_low","dsdp_high"):=NULL]
#     print(c(p_l,p_h))
#   }
#   res_temp[,total_insured:=sum(insured)]
#   print(p_h)
#   
#   # res_temp = acs[,list(insured=sum(shares*PERWT)/sum(PERWT),
#   #                      AC = 12*sum(C*shares*PERWT)/sum(shares*PERWT),
#   #                      MC = 12*sum(C*dsdp*PERWT)/sum(dsdp*PERWT),
#   #                      Markup = -12*sum(shares*PERWT)/sum(dsdp*PERWT)),by=c("AV","Price")]
#   #res_temp[,c("P_base","MC_low","MC_high","dSAdp_low","dSAdp_high"):=NULL]
#   output = rbind(output,res_temp)
# }
# 
# output[,total_insured:=sum(insured),by="P_low"]
# output_wide = reshape(output,timevar=c("AV"),
#                       idvar=c("P_low","total_insured"),
#                       direction="wide")
# output_wide[,Eq:=abs(P_low-P_new.0.6)]
# #output_wide[,P_diff:=1000*(P_new.0.8-P_low)]
# 
# output_wide[Eq<.5,]
# 
# png("Writing/Images/SelectionExampleOligopoly.png",width=2500,height=1500,res=275)
# ggplot() +
#   geom_line(data=output_wide,aes(x=total_insured,y=AC.0.6,color="red"))+
#   geom_line(data=output_wide,aes(x=total_insured,y=AC.0.8,color="red"))+
#   geom_line(data=output_wide,aes(x=total_insured,y=MC.0.6,color="blue"))+
#   geom_line(data=output_wide,aes(x=total_insured,y=MC.0.8,color="blue"))+
#   geom_line(data=output_wide,aes(x=total_insured,y=1000*P_low,color="black")) +
#   geom_line(data=output_wide,aes(x=total_insured,y=1000*P_new.0.6,color="green")) +
#   geom_line(data=output_wide,aes(x=total_insured,y=1000*P_new.0.8,color="green")) +
#   geom_point(aes(x=.485,y=5150),size=2) + 
#   geom_text(aes(x=.485,y=5150),label="p[l]^o",parse=TRUE,vjust=1.2,size=6) + 
#   geom_point(aes(x=.485,y=7500),size=2) + 
#   geom_text(aes(x=.485,y=7500),label="p[h]^o",parse=TRUE,vjust=-0.2,size=6) + 
#   geom_segment(aes(x=.485,y=5150,xend=.485,yend=7500),linetype=2) + 
#   xlab(expression(Q(P[l],P[h](P[l])))) + 
#   scale_y_continuous(label=dollar) + 
#   scale_colour_manual(values = c('red' = 'red','blue' = 'blue','green'='green','black'='black'),
#                       name = '',
#                       labels = expression(AC,MC,MC+Mkup,P[l])) + 
#   ylab("") +
#   coord_cartesian(xlim=c(.2,.75),ylim=c(1000,12000))+ 
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
# ## Merger to Monopoly
# J = 1
# pref_int = 0
# output = NULL
# for (p_l in seq(-.25,12,.5)){
#   print(p_l)
#   acs[AV==.6,P_base:=p_l]
#   #acs[,Benchmark:=p_l*(.7/.6)]
#   #acs[,subsidy:=pmax(Benchmark*ageRate-HHcont*HH_income/1000,0)]
# 
#   err = 10
#   p_h = min(p_l + 3,5)
#   cnt = 0
#   while (abs(sum(err))>.005){
#     #acs[AV==.6,P_base:=p_l]
#     acs[AV==.8,P_base:=p_h]
# 
#     #acs[,Price:= pmax(P_base*ageRate-subsidy,0)]
#     acs[,Price:=P_base]
# 
#     demos = as.matrix(acs[,c("AgeFE_31_39",
#                              "AgeFE_40_51",
#                              "AgeFE_52_64",
#                              "Family",
#                              "LowIncome")])
#     chars_risk = as.matrix(acs[,c("AV","Big")])
#     chars = as.matrix(acs[,c("Price","AV","Big")])
# 
#     intercept = demos%*%gamma
# 
#     # Only Price Coefficient
#     beta_z = (demos%*%t(beta))
#     beta_r = matrix(acs$HCC_Silver,nrow=nrow(acs),ncol=length(sigma),byrow=FALSE)*sigma
# 
#     chars_val = chars%*%beta0 +
#       beta_z[,1]*chars[,1] + #Demographic Effect on Price
#       beta_r[,1]*chars[,2] + #Risk Effect on AV
#       beta_r[,2]*chars[,3] + #Risk Effect on Big
#       pref_int #+mean(FE_pars)
#     # alpha = beta0[1] + beta_z[,1]
#     # price_val = chars[,1]*alpha
# 
#     acs[,util:=exp(chars_val)]
#     acs[,alpha:=beta0[1] + beta_z[,1]]
#     acs[,expsum:=J*sum(util),by="Person"]
#     acs[,shares:=util/(1+expsum)]
# 
#     acs[AV==0.6,share_low:=shares]
#     acs[,share_low:=max(share_low,na.rm=TRUE),by="Person"]
#     acs[AV==0.8,share_high:=shares]
#     acs[,share_high:=max(share_high,na.rm=TRUE),by="Person"]
# 
#     acs[,dsdp:=alpha*shares*(1-shares)]
#     acs[AV==0.6,dsdp_low:=alpha*share_low*(1-shares)]
#     acs[AV==0.8,dsdp_low:=-alpha*share_low*(shares)]
# 
#     acs[AV==0.6,dsdp_high:=-alpha*share_high*(shares)]
#     acs[AV==0.8,dsdp_high:=alpha*share_high*(1-shares)]
#     #
#     #
#     res_temp = acs[,list(insured=sum(J*shares*PERWT)/sum(PERWT),
#                          SA = sum(shares*PERWT)/sum(PERWT),
#                          AC = 12*sum(C*shares*PERWT)/sum(shares*PERWT),
#                          MC_low = 12*sum(C*dsdp_low*PERWT)/sum(PERWT),
#                          MC_high = 12*sum(C*dsdp_high*PERWT)/sum(PERWT),
#                          dSAdp_low = sum(dsdp_low*PERWT)/sum(PERWT),
#                          dSAdp_high = sum(dsdp_high*PERWT)/sum(PERWT),
#                          # MC_A = 12*sum(C*dsdp*PERWT)/sum(dsdp*ageRate*PERWT),
#                          MC = 12*sum(C*dsdp*PERWT)/sum(dsdp*PERWT),
#                          #Markup_A = -1000*sum(shares*ageRate*PERWT)/sum(dsdp*ageRate*PERWT),
#                          Markup = -1000*sum(shares*PERWT)/sum(dsdp*PERWT)
#     ),by=c("AV","P_base")]
#     setkey(res_temp,AV)
#     res_temp[,P_new:=solve(as.matrix(res_temp[,c("dSAdp_low","dSAdp_high")]))%*%(-as.matrix(res_temp$SA) + as.matrix(res_temp[,(MC_low+MC_high)/1000]))]
#     # res_temp[,MC_2:=1000*solve(as.matrix(res_temp[,c("dSAdp_low","dSAdp_high")]))%*%(as.matrix(res_temp[,(MC_low+MC_high)/1000]))]
#     # res_temp[,Markup_2:=1000*solve(as.matrix(res_temp[,c("dSAdp_low","dSAdp_high")]))%*%(-as.matrix(res_temp$SA))]
# 
#     #res_temp[,P_new:=(MC_A+Markup_A)/1000]
#     #res_temp[,P_new:=(MC+Markup)/1000]
#     res_temp[,P_low:=p_l]
#     #err = with(res_temp[res_temp$AV==.8,],P_new-P_base)
#     err = with(res_temp,P_new-P_base)
#     # p_l = max(min(p_l + .25*err[1],40),0)
#     err[1] = 0
#     p_h = max(min(p_h + .5*err[2],40),0)
#     print(err)
#     if (p_h==0|p_h==40){
#       cnt = cnt+1
#     }
#     if(cnt>2){
#       err = 0
#     }
#     #print(res_temp$P_new[res_temp$AV==.8])
#     acs[,c("share_low","share_high","dsdp_low","dsdp_high"):=NULL]
#     print(c(p_l,p_h))
#   }
#   res_temp[,total_insured:=sum(insured)]
#   print(p_h)
# 
#   # res_temp = acs[,list(insured=sum(shares*PERWT)/sum(PERWT),
#   #                      AC = 12*sum(C*shares*PERWT)/sum(shares*PERWT),
#   #                      MC = 12*sum(C*dsdp*PERWT)/sum(dsdp*PERWT),
#   #                      Markup = -12*sum(shares*PERWT)/sum(dsdp*PERWT)),by=c("AV","Price")]
#   #res_temp[,c("P_base","MC_low","MC_high","dSAdp_low","dSAdp_high"):=NULL]
#   output = rbind(output,res_temp)
# }
# 
# output[,total_insured:=sum(insured),by="P_low"]
# output_wide_M = reshape(output,timevar=c("AV"),
#                       idvar=c("P_low","total_insured"),
#                       direction="wide")
# output_wide_M[,Eq:=abs(P_low-P_new.0.6)]
# #output_wide[,P_diff:=1000*(P_new.0.8-P_low)]
# 
# output_wide_M[Eq<.5,]
# 
# ggplot() +
#   geom_line(data=output_wide_M,aes(x=total_insured,y=AC.0.6,color="red"))+
#   geom_line(data=output_wide_M,aes(x=total_insured,y=AC.0.8,color="red"))+
#   geom_line(data=output_wide_M,aes(x=total_insured,y=MC.0.6,color="blue"))+
#   geom_line(data=output_wide_M,aes(x=total_insured,y=MC.0.8,color="blue"))+
#   geom_line(data=output_wide_M,aes(x=total_insured,y=1000*P_low,color="black")) +
#   geom_line(data=output_wide_M,aes(x=total_insured,y=1000*P_new.0.6,color="green")) +
#   geom_line(data=output_wide_M,aes(x=total_insured,y=1000*P_new.0.8,color="green")) +
#   scale_y_continuous(label=dollar) +
#   scale_colour_manual(values = c('red' = 'red','blue' = 'blue','green'='green','black'='black'),
#                       name = '',
#                       labels = expression(P[l],MC,MC+Mkup,AC)) +
#   ylab("") +
#   coord_cartesian(xlim=c(.2,.75),ylim=c(1000,12000))+
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



# 
# #### Social Welfare Optimum ####
# J = 1
# pref_int = 2
# output = NULL
# for (p_l in seq(-.25,12,.5)){
#   print(p_l)
#   acs[AV==.6,P_base:=p_l]
#   #acs[,Benchmark:=p_l*(.7/.6)]
#   #acs[,subsidy:=pmax(Benchmark*ageRate-HHcont*HH_income/1000,0)]
#   
#   err = 10
#   p_h = min(p_l + 3,5)
#   cnt = 0
#   while (abs(sum(err))>.005){
#     #acs[AV==.6,P_base:=p_l]
#     acs[AV==.8,P_base:=p_h]
#     
#     #acs[,Price:= pmax(P_base*ageRate-subsidy,0)]
#     acs[,Price:=P_base]
#     
#     demos = as.matrix(acs[,c("AgeFE_31_39",
#                              "AgeFE_40_51",
#                              "AgeFE_52_64",
#                              "Family",
#                              "LowIncome")])
#     chars_risk = as.matrix(acs[,c("AV","Big")])
#     chars = as.matrix(acs[,c("Price","AV","Big")])
#     
#     intercept = demos%*%gamma
#     
#     # Only Price Coefficient
#     beta_z = (demos%*%t(beta))
#     beta_r = matrix(acs$HCC_Silver,nrow=nrow(acs),ncol=length(sigma),byrow=FALSE)*sigma
#     
#     chars_val = chars%*%beta0 +
#       beta_z[,1]*chars[,1] + #Demographic Effect on Price
#       beta_r[,1]*chars[,2] + #Risk Effect on AV
#       beta_r[,2]*chars[,3] + #Risk Effect on Big
#       pref_int #+mean(FE_pars)
#     # alpha = beta0[1] + beta_z[,1]
#     # price_val = chars[,1]*alpha
#     
#     acs[,util:=exp(chars_val)]
#     acs[,alpha:=beta0[1] + beta_z[,1]]
#     acs[,expsum:=J*sum(util),by="Person"]
#     acs[,shares:=util/(1+expsum)]
#     
#     acs[AV==0.6,share_low:=shares]
#     acs[,share_low:=max(share_low,na.rm=TRUE),by="Person"]
#     acs[AV==0.8,share_high:=shares]
#     acs[,share_high:=max(share_high,na.rm=TRUE),by="Person"]
#     
#     acs[,dsdp:=alpha*shares*(1-shares)]
#     acs[AV==0.6,dsdp_low:=alpha*share_low*(1-shares) - (J-1)*alpha*share_low*shares]
#     acs[AV==0.8,dsdp_low:=-alpha*J*share_low*(shares)]
#     
#     acs[AV==0.6,dsdp_high:=-alpha*J*share_high*(shares)]
#     acs[AV==0.8,dsdp_high:=alpha*share_high*(1-shares)- (J-1)*alpha*share_high*shares]
#     #
#     #
#     res_temp = acs[,list(insured=sum(J*shares*PERWT)/sum(PERWT),
#                          SA = sum(shares*PERWT)/sum(PERWT),
#                          AC = 12*sum(C*shares*PERWT)/sum(shares*PERWT),
#                          MC_low = 12*sum(C*dsdp_low*PERWT)/sum(PERWT),
#                          MC_high = 12*sum(C*dsdp_high*PERWT)/sum(PERWT),
#                          dSAdp_low = sum(dsdp_low*PERWT)/sum(PERWT),
#                          dSAdp_high = sum(dsdp_high*PERWT)/sum(PERWT),
#                          # MC_A = 12*sum(C*dsdp*PERWT)/sum(dsdp*ageRate*PERWT),
#                          MC = 12*sum(C*dsdp*PERWT)/sum(dsdp*PERWT),
#                          #Markup_A = -1000*sum(shares*ageRate*PERWT)/sum(dsdp*ageRate*PERWT),
#                          Markup = -1000*sum(shares*PERWT)/sum(dsdp*PERWT)
#     ),by=c("AV","P_base")]
#     setkey(res_temp,AV)
#     res_temp[,P_new:=solve(as.matrix(res_temp[,c("dSAdp_low","dSAdp_high")]))%*%(as.matrix(res_temp[,(MC_low+MC_high)/1000]))]
#     # res_temp[,MC_2:=1000*solve(as.matrix(res_temp[,c("dSAdp_low","dSAdp_high")]))%*%(as.matrix(res_temp[,(MC_low+MC_high)/1000]))]
#     # res_temp[,Markup_2:=1000*solve(as.matrix(res_temp[,c("dSAdp_low","dSAdp_high")]))%*%(-as.matrix(res_temp$SA))]
#     
#     #res_temp[,P_new:=(MC_A+Markup_A)/1000]
#     #res_temp[,P_new:=(MC+Markup)/1000]
#     res_temp[,P_low:=p_l]
#     #err = with(res_temp[res_temp$AV==.8,],P_new-P_base)
#     err = with(res_temp,P_new-P_base)
#     # p_l = max(min(p_l + .25*err[1],40),0)
#     err[1] = 0
#     p_h = max(min(p_h + .5*err[2],40),0)
#     print(err)
#     if (p_h==0|p_h==40){
#       cnt = cnt+1
#     }
#     if(cnt>2){
#       err = 0
#     }
#     #print(res_temp$P_new[res_temp$AV==.8])
#     acs[,c("share_low","share_high","dsdp_low","dsdp_high"):=NULL]
#     print(c(p_l,p_h))
#   }
#   res_temp[,total_insured:=sum(insured)]
#   print(p_h)
#   
#   # res_temp = acs[,list(insured=sum(shares*PERWT)/sum(PERWT),
#   #                      AC = 12*sum(C*shares*PERWT)/sum(shares*PERWT),
#   #                      MC = 12*sum(C*dsdp*PERWT)/sum(dsdp*PERWT),
#   #                      Markup = -12*sum(shares*PERWT)/sum(dsdp*PERWT)),by=c("AV","Price")]
#   #res_temp[,c("P_base","MC_low","MC_high","dSAdp_low","dSAdp_high"):=NULL]
#   output = rbind(output,res_temp)
# }
# 
# output[,total_insured:=sum(insured),by="P_low"]
# output_wide_SW = reshape(output,timevar=c("AV"),
#                         idvar=c("P_low","total_insured"),
#                         direction="wide")
# output_wide_SW[,Eq:=abs(P_low-P_new.0.6)]
# #output_wide[,P_diff:=1000*(P_new.0.8-P_low)]
# 
# output_wide_SW[Eq<.5,]
# 
# ggplot() +
#   geom_line(data=output_wide_SW,aes(x=total_insured,y=AC.0.6,color="red"))+
#   geom_line(data=output_wide_SW,aes(x=total_insured,y=AC.0.8,color="red"))+
#   geom_line(data=output_wide_SW,aes(x=total_insured,y=MC.0.6,color="blue"))+
#   geom_line(data=output_wide_SW,aes(x=total_insured,y=MC.0.8,color="blue"))+
#   geom_line(data=output_wide_SW,aes(x=total_insured,y=1000*P_low,color="black")) +
#   geom_line(data=output_wide_SW,aes(x=total_insured,y=1000*P_new.0.6,color="green")) +
#   geom_line(data=output_wide_SW,aes(x=total_insured,y=1000*P_new.0.8,color="green")) +
#   coord_cartesian(xlim=c(.9,1),ylim=c(1000,12000))+ 
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
# 
# 




#### Add Competitor ####
acs_firm1 = as.data.frame(acs)
acs_firm2 = as.data.frame(acs)
acs_firm3 = as.data.frame(acs)
acs_firm1$Firm = "L"
acs_firm2$Firm = "S"
acs = as.data.table(rbind(acs_firm1,acs_firm2))#,acs_firm3))
setkey(acs,Person)
#rm(acs_firm1,acs_firm2,acs_firm3)

#### Risk Adjustment and Welfare ####
J = 3
output = NULL
pref_int = 2
inside_pref = 3*exp(pref_int)
p_l = 3
p_sm_l = 3
p_lg_l = 3
p_sm_h = p_sm_l + 3
p_lg_h = p_lg_l + 3
for  (large_share in c(1,.9999,.99,.9,.8,.6,.4,1/J)) {
  risk_tau = 1
  for (mandate in c(0,.5,1,1.5,2)){
    print("Mandate Value")
    print(mandate)
    print("Firm Size")
    print(large_share)
    if(large_share<1){
      small_share = (1-large_share)/(J-1)
    }else{
      small_share = 0
    }
    cnt = 0
    err_l = 10 
    while(abs(sum(err_l))>.005){
      err_h = 10
      acs[AV==.6&Firm=="S",P_base:=p_sm_l]
      acs[AV==.6&Firm=="L",P_base:=p_lg_l]
      while (sum(abs(err_h))>.005){
        cnt = cnt+1
        acs[AV==.8&Firm=="S",P_base:=p_sm_h]
        acs[AV==.8&Firm=="L",P_base:=p_lg_h]
        
        #acs[,Price:= pmax(P_base*ageRate-subsidy,0)]
        acs[,Price:=P_base]
        
        demos = as.matrix(acs[,c("AgeFE_31_39",
                                 "AgeFE_40_51",
                                 "AgeFE_52_64",
                                 "Family",
                                 "LowIncome")])
        chars_risk = as.matrix(acs[,c("AV","Big")])
        chars = as.matrix(acs[,c("Price","AV","Big")])
        
        intercept = demos%*%gamma
        
        # Only Price Coefficient
        beta_z = (demos%*%t(beta))
        beta_r = matrix(acs$HCC_Silver,nrow=nrow(acs),ncol=1)
        
        chars_val = intercept + chars%*%beta0 +
          beta_z[,1]*chars[,1] + #Demographic Effect on Price
          beta_r*chars[,2]*sigma[1] + #Risk Effect on AV
          beta_r*chars[,3]*sigma[2] #Risk Effect on Big
        #pref_int mean(FE_pars)
        # alpha = beta0[1] + beta_z[,1]
        # price_val = chars[,1]*alpha
        acs[,util:=exp(chars_val)]
        acs[Firm=="L",util:=util*large_share*inside_pref]
        acs[Firm=="S",util:=util*small_share*inside_pref]
        
        acs[,alpha:=beta0[1] + beta_z[,1]]
        acs[,expsum:=util]
        acs[Firm=="S",expsum:=(J-1)*util]
        acs[,expsum:=sum(expsum),by="Person"]
        # acs[,shares:=util/(1+expsum)]
        acs[,shares:=util/(exp(alpha*mandate)+expsum)]
        
        acs[,insured:=expsum/(exp(alpha*mandate)+expsum)]
        
        acs[AV==0.6,share_low:=shares]
        acs[,share_low:=max(share_low,na.rm=TRUE),by=c("Person","Firm")]
        acs[AV==0.8,share_high:=shares]
        acs[,share_high:=max(share_high,na.rm=TRUE),by=c("Person","Firm")]
        
        acs[,dsdp:=alpha*shares*(1-shares)]
        acs[AV==0.6,dsdp_low:=alpha*share_low*(1-shares)]
        acs[AV==0.8,dsdp_low:=-alpha*share_low*(shares)]
        
        acs[AV==0.6,dsdp_high:=-alpha*share_high*(shares)]
        acs[AV==0.8,dsdp_high:=alpha*share_high*(1-shares)]
        
        
        acs[,CW:=log(exp(alpha*mandate)+expsum)]
        acs[,CW:=CW/(-alpha)]
        acs[,lowrisk:=as.numeric(HCC_Silver==0)]
        acs[,midrisk:=as.numeric(HCC_Silver>0&HCC_Silver<2.18)]
        acs[,highrisk:=as.numeric(HCC_Silver>2.18)]
        
        res_temp = acs[,list(insured=sum(shares*PERWT)/sum(PERWT),
                             SA = sum(shares*PERWT)/sum(PERWT),
                             AC = 12*sum(C*shares*PERWT)/sum(shares*PERWT),
                             PC = 12*sum(C*insured*PERWT)/sum(insured*PERWT),
                             MC = 12*sum(C*dsdp*PERWT)/sum(dsdp*PERWT),
                             MC_low = 12*sum(C_low*dsdp_low*PERWT)/sum(PERWT),
                             MC_high = 12*sum(C_high*dsdp_high*PERWT)/sum(PERWT),
                             dSAdp_low = sum(dsdp_low*PERWT)/sum(PERWT),
                             dSAdp_high = sum(dsdp_high*PERWT)/sum(PERWT),
                             Markup = -1000*sum(shares*PERWT)/sum(dsdp*PERWT),
                             ConsWelfare = sum(CW*PERWT),
                             CW_low = sum(CW*PERWT*lowrisk)/sum(lowrisk*PERWT),
                             CW_mid = sum(CW*PERWT*midrisk)/sum(midrisk*PERWT),
                             CW_high = sum(CW*PERWT*highrisk)/sum(highrisk*PERWT),
                             PlanWelfare = sum(shares*(AV*C*12/1000-Price))
                             
        ),by=c("AV","P_base","Firm")]
        # res_temp[Firm=="S",insured:=(J-1)*insured]
        # res_temp[Firm=="S",PlanWelfare:=(J-1)*PlanWelfare]
        res_temp[,PlanWelfare:=sum(PlanWelfare)]
        setkey(res_temp,Firm,AV)
        
        res_temp[Firm=="L",MC_RA:=(1-risk_tau)*PC+ risk_tau*1000*solve(as.matrix(res_temp[Firm=="L",c("dSAdp_low","dSAdp_high")]))%*%(as.matrix(res_temp[Firm=="L",(MC_low+MC_high)/1000]))]
        res_temp[Firm=="L",Markup_full:=1000*solve(as.matrix(res_temp[Firm=="L",c("dSAdp_low","dSAdp_high")]))%*%(-as.matrix(res_temp$SA[res_temp$Firm=="L"]))]
        if(large_share<1){
        res_temp[Firm=="S",MC_RA:=(1-risk_tau)*PC+ risk_tau*1000*solve(as.matrix(res_temp[Firm=="S",c("dSAdp_low","dSAdp_high")]))%*%(as.matrix(res_temp[Firm=="S",(MC_low+MC_high)/1000]))]
        res_temp[Firm=="S",Markup_full:=1000*solve(as.matrix(res_temp[Firm=="S",c("dSAdp_low","dSAdp_high")]))%*%(-as.matrix(res_temp$SA[res_temp$Firm=="S"]))]
        }
        #res_temp[,P_new:=(MC+Markup)/1000]
        res_temp[,P_new:=(MC_RA+Markup_full)/1000]
        
        err_h = with(res_temp[res_temp$AV==.8,],P_new-P_base)  
        if(large_share==1){
          err_h[2] = 0
        }
        p_lg_h = p_lg_h + .5*err_h[1]
        p_sm_h = p_sm_h + .5*err_h[2]
        print(err_h)
        #print(res_temp$P_new[res_temp$AV==.8])
        print(c(p_lg_h,p_sm_h))
        
        acs[,c("share_low","share_high","dsdp_low","dsdp_high"):=NULL]
      }
      print(c(p_lg_h,p_sm_h))
      print("UPDATE P_l")
      err_l = with(res_temp[res_temp$AV==.6,],P_new-P_base)
      if(large_share==1){
        err_l[2] = 0
      }
      p_lg_l = p_lg_l + .5*err_l[1]
      p_sm_l = p_sm_l + .5*err_l[2]
      #p_l = p_l +.5*err_l
      print(c(p_lg_l,p_sm_l))
    }
    
    res_temp[,FirmSize:=large_share]
    res_temp[,mandate:=mandate]
    res_temp[,c("P_base"):=NULL]
    output = rbind(output,res_temp)
  }
}



output[,total_insured:=FirmNum*sum(insured),by=c("risk_tau","FirmSize")]
output[,firmShare:=sum(insured)/total_insured,by=c("risk_tau","FirmSize","Firm")]
output_wide_RA = reshape(output,timevar=c("AV"),
                         idvar=c("risk_tau","firmShare","FirmNum","Firm"),
                         direction="wide")

output_wide_RA[,RA:=factor(risk_tau,levels=sort(unique(output_wide_RA$risk_tau)),labels=c("100% Adjustment","66%","33% Adjustment","0%"))]

output_MC = output_wide_RA[risk_tau==1,c("FirmSize","ConsWelfare.0.8","CW_high.0.8","CW_mid.0.8","CW_low.0.8")]
names(output_MC) = c("FirmSize","CW_MC","CW_High_MC","CW_Mid_MC","CW_Low_MC")
output_wide_RA = merge(output_wide_RA,output_MC,by="FirmSize")

output_wide_RA[,CW_diff:=1000*(ConsWelfare.0.8-CW_MC)/sum(acs$PERWT/4)]
output_wide_RA[,CW_High_diff:=1000*(CW_high.0.8-CW_High_MC)]
output_wide_RA[,CW_Low_diff:=1000*(CW_low.0.8-CW_Low_MC)]

#png("Writing/Images/RiskAdj_Welfare.png",width=2500,height=1500,res=275)
ggplot(output_wide_RA[!risk_tau%in%c(1,1/3),]) + 
  # geom_line(data=output_wide_RA,aes(x=firmShare,y=P_new.0.6,color=RA))+ 
  # geom_line(data=output_wide_RA,aes(x=firmShare,y=P_new.0.8,color=RA))+ 
  geom_line(aes(x=firmShare,y=CW_High_diff,color=RA))+
  geom_point(aes(x=firmShare,y=CW_High_diff,color=RA,shape="High Risk"),size=3)+
  geom_line(aes(x=firmShare,y=CW_diff,color=RA))+
  geom_point(aes(x=firmShare,y=CW_diff,color=RA,shape="Average"),size=3)+
  geom_abline(intercept=0,slope=0) + 
  scale_y_continuous(label=dollar) + 
  scale_x_continuous(label=percent) + 
  ylab("CW Per Person") +
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
#dev.off()

#png("Writing/Images/RiskAdj_Prices.png",width=2500,height=1500,res=275)
ggplot(output_wide_RA[risk_tau!=1/3,]) + 
  geom_line(aes(x=firmShare,y=1000*P_new.0.6,color=RA))+
  geom_point(aes(x=firmShare,y=1000*P_new.0.6,color=RA,shape="Low Plan"),size=3)+
  geom_line(aes(x=firmShare,y=1000*P_new.0.8,color=RA))+
  geom_point(aes(x=firmShare,y=1000*P_new.0.8,color=RA,shape="High Plan"),size=3)+
  scale_y_continuous(label=dollar) + 
  scale_x_continuous(label=percent) + 
  ylab("") +
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
#dev.off()

# 
# 
# #### Risk Adjustment and Welfare, and Mandate ####
# J = 3
# output = NULL
# pref_int = 2
# inside_pref = J*exp(pref_int)
# 
# p_sm_l = 3
# p_lg_l = 3
# p_sm_h = p_sm_l + 3
# p_lg_h = p_lg_l + 3
# for  (large_share in c(.99,.95,seq(.9,1/J,-.1),1/J)) { 
#   
#   for (risk_tau in c(0,1/3,2/3,1)){
#     print(risk_tau)
#     #for (large_share in c(seq(.7,1/J,-.1),1/J)){
#     #large_share = 1/J
#     print("Size of Large Firm")
#     print(large_share)
#     
#     small_share = (1-large_share)/(J-1)
#     
#     err_l = 10 
#     while(abs(sum(err_l))>.01){
#       err_h = 10
#       acs[AV==.6&Firm=="S",P_base:=p_sm_l]
#       acs[AV==.6&Firm=="L",P_base:=p_lg_l]
#       cnt = 0
#       while (sum(abs(err_h))>.01){
#         acs[AV==.8&Firm=="S",P_base:=p_sm_h]
#         acs[AV==.8&Firm=="L",P_base:=p_lg_h]
#         
#         #acs[,Price:= pmax(P_base*ageRate-subsidy,0)]
#         acs[,Price:=P_base]
#         
#         demos = as.matrix(acs[,c("AgeFE_31_39",
#                                  "AgeFE_40_51",
#                                  "AgeFE_52_64",
#                                  "Family",
#                                  "LowIncome")])
#         chars_risk = as.matrix(acs[,c("AV","Big")])
#         chars = as.matrix(acs[,c("Price","AV","Big")])
#         
#         intercept = demos%*%gamma
#         
#         # Only Price Coefficient
#         beta_z = (demos%*%t(beta))
#         beta_r = matrix(acs$HCC_Silver,nrow=nrow(acs),ncol=length(sigma),byrow=FALSE)*sigma
#         
#         chars_val = chars%*%beta0 + 
#           beta_z[,1]*chars[,1] + #Demographic Effect on Price
#           beta_r[,1]*chars[,2] + #Risk Effect on AV
#           beta_r[,2]*chars[,3]   #Risk Effect on Big
#         #pref_int mean(FE_pars)
#         # alpha = beta0[1] + beta_z[,1]
#         # price_val = chars[,1]*alpha
#         acs[,util:=exp(chars_val)]
#         acs[Firm=="L",util:=util*large_share*inside_pref]
#         acs[Firm=="S",util:=util*small_share*inside_pref]
#         
#         acs[,alpha:=beta0[1] + beta_z[,1]]
#         acs[,expsum:=util]
#         acs[Firm=="S",expsum:=(J-1)*util]
#         acs[,expsum:=sum(expsum),by="Person"]
#         acs[,shares:=util/(expsum)]
#         
#         acs[,insured:=expsum/(expsum)]
#         
#         acs[AV==0.6,share_low:=shares]
#         acs[,share_low:=max(share_low,na.rm=TRUE),by="Person"]
#         acs[AV==0.8,share_high:=shares]
#         acs[,share_high:=max(share_high,na.rm=TRUE),by="Person"]
#         
#         acs[,dsdp:=alpha*shares*(1-shares)]
#         acs[AV==0.6,dsdp_low:=alpha*share_low*(1-shares)]
#         acs[AV==0.8,dsdp_low:=-alpha*share_low*(shares)]
#         
#         acs[AV==0.6,dsdp_high:=-alpha*share_high*(shares)]
#         acs[AV==0.8,dsdp_high:=alpha*share_high*(1-shares)]
#         
#         
#         acs[,CW:=log(1+expsum)]
#         acs[,CW:=CW/(-alpha)]
#         acs[,lowrisk:=as.numeric(HCC_Silver==0)]
#         acs[,midrisk:=as.numeric(HCC_Silver>0&HCC_Silver<2.18)]
#         acs[,highrisk:=as.numeric(HCC_Silver>2.18)]
#         
#         res_temp = acs[,list(insured=sum(shares*PERWT)/sum(PERWT),
#                              SA = sum(shares*PERWT)/sum(PERWT),
#                              AC = 12*sum(C*shares*PERWT)/sum(shares*PERWT),
#                              PC = 12*sum(C*insured*PERWT)/sum(insured*PERWT),
#                              MC = 12*sum(C*dsdp*PERWT)/sum(dsdp*PERWT),
#                              MC_low = 12*sum(C*dsdp_low*PERWT)/sum(PERWT),
#                              MC_high = 12*sum(C*dsdp_high*PERWT)/sum(PERWT),
#                              dSAdp_low = sum(dsdp_low*PERWT)/sum(PERWT),
#                              dSAdp_high = sum(dsdp_high*PERWT)/sum(PERWT),
#                              Markup = -1000*sum(shares*PERWT)/sum(dsdp*PERWT),
#                              ConsWelfare = sum(CW*PERWT),
#                              CW_low = sum(CW*PERWT*lowrisk)/sum(lowrisk*PERWT),
#                              CW_mid = sum(CW*PERWT*midrisk)/sum(midrisk*PERWT),
#                              CW_high = sum(CW*PERWT*highrisk)/sum(highrisk*PERWT),
#                              PlanWelfare = sum(shares*(AV*C*12/1000-Price))
#                              
#         ),by=c("AV","P_base","Firm")]
#         res_temp[Firm=="S",insured:=(J-1)*insured]
#         res_temp[Firm=="S",PlanWelfare:=(J-1)*PlanWelfare]
#         res_temp[,PlanWelfare:=sum(PlanWelfare)]
#         res_temp[,P_low:=p_l]
#         setkey(res_temp,Firm,AV)
#         
#         res_temp[Firm=="S",MC_RA:=(1-risk_tau)*PC+ risk_tau*1000*solve(as.matrix(res_temp[Firm=="S",c("dSAdp_low","dSAdp_high")]))%*%(as.matrix(res_temp[Firm=="S",(MC_low+MC_high)/1000]))]
#         res_temp[Firm=="L",MC_RA:=(1-risk_tau)*PC+ risk_tau*1000*solve(as.matrix(res_temp[Firm=="L",c("dSAdp_low","dSAdp_high")]))%*%(as.matrix(res_temp[Firm=="L",(MC_low+MC_high)/1000]))]
#         
#         
#         res_temp[Firm=="S",Markup_full:=1000*solve(as.matrix(res_temp[Firm=="S",c("dSAdp_low","dSAdp_high")]))%*%(-as.matrix(res_temp$SA[res_temp$Firm=="S"]))]
#         res_temp[Firm=="L",Markup_full:=1000*solve(as.matrix(res_temp[Firm=="L",c("dSAdp_low","dSAdp_high")]))%*%(-as.matrix(res_temp$SA[res_temp$Firm=="L"]))]
#         
#         
#         #res_temp[,P_new:=(MC+Markup)/1000]
#         res_temp[,P_new:=(MC_RA+Markup_full)/1000]
#         
#         err_h = with(res_temp[res_temp$AV==.8,],P_new-P_base)  
#         p_lg_h = p_lg_h + .25*err_h[1]
#         p_sm_h = p_sm_h + .25*err_h[2]
#         if(p_lg_h>40|p_lg_h<0){
#           p_lg_h = min(max(p_lg_h,0),40)
#           cnt = cnt+1
#         }
#         if(cnt>3){
#           err_h[1] = 0
#         }
#         print(err_h)
#         #print(res_temp$P_new[res_temp$AV==.8])
#         print(c(p_lg_h,p_sm_h))
#         
#         acs[,c("share_low","share_high","dsdp_low","dsdp_high"):=NULL]
#       }
#       print(c(p_lg_h,p_sm_h))
#       print("UPDATE P_l")
#       err_l = with(res_temp[res_temp$AV==.6,],P_new-P_base)
#       p_lg_l = p_lg_l + .25*err_l[1]
#       if((p_lg_l>40&err_l[1]>0)|(p_lg_l<0&err_l[1]<0)){
#         p_lg_l = min(max(p_lg_l,0),40)
#         err_l[1] = 0
#       }
#       p_sm_l = p_sm_l + .25*err_l[2]
#       #p_l = p_l +.5*err_l
#       print(c(p_lg_l,p_sm_l))
#     }
#     res_temp[,FirmSize:=large_share]
#     res_temp[,risk_tau:=risk_tau]
#     res_temp[,c("P_base"):=NULL]
#     output = rbind(output,res_temp)
#   }
# }
# 
# 
# 
# output[,total_insured:=sum(insured),by=c("risk_tau","FirmSize")]
# output[,firmShare:=sum(insured)/total_insured,by=c("risk_tau","FirmSize","Firm")]
# output_wide_MRA = reshape(output[Firm=="L",],timevar=c("AV"),
#                           idvar=c("risk_tau","total_insured","firmShare","FirmSize"),
#                           direction="wide")
# 
# output_wide_MRA[,RA:=factor(risk_tau,levels=sort(unique(output_wide_MRA$risk_tau)),labels=c("100% Adjustment","66%","33% Adjustment","0%"))]
# 
# output_MC = output_wide_MRA[risk_tau==1,c("FirmSize","ConsWelfare.0.8","CW_high.0.8","CW_mid.0.8","CW_low.0.8")]
# names(output_MC) = c("FirmSize","CW_MC","CW_High_MC","CW_Mid_MC","CW_Low_MC")
# output_wide_MRA = merge(output_wide_MRA,output_MC,by="FirmSize")
# 
# output_wide_MRA[,CW_diff:=1000*(ConsWelfare.0.8-CW_MC)/sum(acs$PERWT/4)]
# output_wide_MRA[,CW_High_diff:=1000*(CW_high.0.8-CW_High_MC)]
# output_wide_MRA[,CW_Low_diff:=1000*(CW_low.0.8-CW_Low_MC)]
# 
# png("Writing/Images/RA_Mandate_Welfare.png",width=2500,height=1500,res=275)
# ggplot(output_wide_MRA[!risk_tau%in%c(1,1/3),]) + 
#   # geom_line(data=output_wide_MRA,aes(x=firmShare,y=P_new.0.6,color=RA))+ 
#   # geom_line(data=output_wide_MRA,aes(x=firmShare,y=P_new.0.8,color=RA))+ 
#   geom_line(aes(x=firmShare,y=CW_High_diff,color=RA))+
#   geom_point(aes(x=firmShare,y=CW_High_diff,color=RA,shape="High Risk"),size=3)+
#   geom_line(aes(x=firmShare,y=CW_diff,color=RA))+
#   geom_point(aes(x=firmShare,y=CW_diff,color=RA,shape="Average"),size=3)+
#   geom_abline(intercept=0,slope=0) + 
#   scale_y_continuous(label=dollar) + 
#   scale_x_continuous(label=percent) + 
#   ylab("CW Per Person") +
#   xlab("Market Share of Large Firm")+
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
# png("Writing/Images/RA_Mandate_Prices.png",width=2500,height=1500,res=275)
# ggplot(output_wide_MRA[risk_tau!=1/3,]) + 
#   geom_line(aes(x=firmShare,y=1000*P_new.0.6,color=RA))+
#   geom_point(aes(x=firmShare,y=1000*P_new.0.6,color=RA,shape="Low Plan"),size=3)+
#   geom_line(aes(x=firmShare,y=1000*P_new.0.8,color=RA))+
#   geom_point(aes(x=firmShare,y=1000*P_new.0.8,color=RA,shape="High Plan"),size=3)+
#   scale_y_continuous(label=dollar) + 
#   scale_x_continuous(label=percent) + 
#   ylab("") +
#   xlab("Market Share of Large Firm")+
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
