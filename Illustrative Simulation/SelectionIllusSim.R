rm(list=ls())
library(doBy)
library(randtoolbox)
library(data.table)
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


#### MEPS COMPARE ####
# mepsFull = read.csv("Data/2015_MEPS/MEPS_Full_2015.csv")
# meps = mepsFull[,c("HIEUIDX","DUPERSID","PID","PANEL","AGELAST","AGE15X","TTLP15X","POVLEV15","OFFER31X","OFFER42X","OFFER53X",
#                    "TRIEV15","MCREV15","MCDEV15","OPAEV15","OPBEV15","ADSMOK42",
#                    "INSCOV15","INSURC15","PERWT15F")]
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
# ggplot(meps) + 
#   aes(x=log(HCC_Score_Silver+.001),weights=PERWT15F,y=..density..) + 
#   geom_histogram(binwidth=.25) 
#   
# 
# ggplot(acs) + 
#   aes(x=log(HCC_Silver+.001),weights=PERWT,y=..density..) + 
#   geom_histogram(binwidth=.25)
# 
# ggplot() +
#   geom_histogram(data=acs,aes(x=log(HCC_Silver+.001),weights=PERWT,y=..density..),binwidth=.3,fill="red")+
#   geom_histogram(data=meps,aes(x=log(HCC_Score_Silver+.001),weights=PERWT15F,y=..density..),binwidth=.3,fill="blue")

#### Two Products ####
acs_High = as.data.frame(acs)
acs_High$AV = 0.8
acs_Low = as.data.frame(acs)
acs_Low$AV = 0.6
acs = as.data.table(rbind(acs_High,acs_Low))


#### Apply Cost ####
## Cost Function 
costFile = paste("Simulation_Risk_Output/costParameters_MSM_Stage2_",run,".rData",sep="")
load(costFile)
phi = est_res$estimate

phi_st_avg = mean(phi[4:17])
## Apply Function
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


# #### Predict - Monopolist/PC Comparison ####
pref_int = 2 #+ mean(FE_pars)
## Monopolist
J = 1
output = NULL
for (p_l in seq(-.25,10,.75)){
  print(p_l)
  acs[AV==.6,P_base:=p_l]
  #acs[,Benchmark:=p_l*(.7/.6)]
  #acs[,subsidy:=pmax(Benchmark*ageRate-HHcont*HH_income/1000,0)]

  err = 10
  p_h = p_l + 3
  cnt = 0
  while (abs(err)>.005){
    cnt = cnt+1
    acs[AV==.8,P_base:=p_h]

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
    beta_r = matrix(acs$HCC_Silver,nrow=nrow(acs),ncol=length(sigma),byrow=FALSE)*sigma

    chars_val = chars%*%beta0 +
      beta_z[,1]*chars[,1] + #Demographic Effect on Price
      beta_r[,1]*chars[,2] + #Risk Effect on AV
      beta_r[,2]*chars[,3]  #Risk Effect on Big
      pref_int #+mean(FE_pars)
    # alpha = beta0[1] + beta_z[,1]
    # price_val = chars[,1]*alpha

    acs[,util:=exp(chars_val)]
    acs[,alpha:=beta0[1] + beta_z[,1]]
    acs[,expsum:=J*sum(util),by="Person"]
    acs[,shares:=util/(1+expsum)]

    # acs[AV==0.6,share_low:=shares]
    # acs[,share_low:=max(share_low,na.rm=TRUE),by="Person"]
    # acs[AV==0.8,share_high:=shares]
    # acs[,share_high:=max(share_high,na.rm=TRUE),by="Person"]

    acs[,dsdp:=alpha*shares*(1-shares)]
    # acs[AV==0.6,dsdp_low:=alpha*share_low*(1-shares)]
    # acs[AV==0.8,dsdp_low:=-alpha*share_low*(shares)]
    #
    # acs[AV==0.6,dsdp_high:=-alpha*share_high*(shares)]
    # acs[AV==0.8,dsdp_high:=alpha*share_high*(1-shares)]
    #
    #
    res_temp = acs[,list(insured=sum(J*shares*PERWT)/sum(PERWT),
                         SA = sum(shares*ageRate*PERWT)/sum(PERWT),
                         AC = 12*sum(C*shares*PERWT)/sum(shares*PERWT),
                         # MC_low = 12*sum(C*dsdp_low*PERWT)/sum(PERWT),
                         # MC_high = 12*sum(C*dsdp_high*PERWT)/sum(PERWT),
                         # dSAdp_low = sum(dsdp_low*ageRate*PERWT)/sum(PERWT),
                         # dSAdp_high = sum(dsdp_high*ageRate*PERWT)/sum(PERWT)
                         #MC_A = 12*sum(C*dsdp*PERWT)/sum(dsdp*ageRate*PERWT),
                         MC = 12*sum(C*dsdp*PERWT)/sum(dsdp*PERWT),
                         #Markup_A = -1000*sum(shares*ageRate*PERWT)/sum(dsdp*ageRate*PERWT),
                         Markup = -1000*sum(shares*PERWT)/sum(dsdp*PERWT)
    ),by=c("AV","P_base")]
    setkey(res_temp,AV)
    # res_temp[,P_new:=solve(as.matrix(res_temp[,c("dSAdp_low","dSAdp_high")]))%*%(-as.matrix(res_temp$SA) + as.matrix(res_temp[,(MC_low+MC_high)/1000]))]
    # res_temp[,MC:=1000*solve(as.matrix(res_temp[,c("dSAdp_low","dSAdp_high")]))%*%(as.matrix(res_temp[,(MC_low+MC_high)/1000]))]
    # res_temp[,Markup:=1000*solve(as.matrix(res_temp[,c("dSAdp_low","dSAdp_high")]))%*%(-as.matrix(res_temp$SA))]

    #res_temp[,P_new:=(MC_A+Markup_A)/1000]
    res_temp[,P_new:=(MC+Markup)/1000]
    res_temp[,P_low:=p_l]
    err = with(res_temp[res_temp$AV==.8,],P_new-P_base)
    p_h = p_h + err
    print(err)

    print(res_temp$P_new[res_temp$AV==.8])
    print(p_h)
  }
  res_temp[,total_insured:=sum(insured)]
  print(p_h)

  # res_temp = acs[,list(insured=sum(shares*PERWT)/sum(PERWT),
  #                      AC = 12*sum(C*shares*PERWT)/sum(shares*PERWT),
  #                      MC = 12*sum(C*dsdp*PERWT)/sum(dsdp*PERWT),
  #                      Markup = -12*sum(shares*PERWT)/sum(dsdp*PERWT)),by=c("AV","Price")]
  #res_temp[,c("P_base","MC_low","MC_high","dSAdp_low","dSAdp_high"):=NULL]
  output = rbind(output,res_temp)
}

output[,total_insured:=sum(insured),by="P_low"]
output_wide = reshape(output,timevar=c("AV"),
                      idvar=c("P_low","total_insured"),
                      direction="wide")
output_wide[,Eq:=abs(P_low-P_new.0.6)]
output_wide[,P_diff:=1000*(P_new.0.8-P_low)]

output_wide[Eq<.1,]

# ggplot() +
#   geom_line(data=output_wide,aes(x=insured.0.6,y=AC.0.6),color="red")+
#   geom_line(data=output_wide,aes(x=insured.0.6,y=AC.0.8),color="red")+
#   geom_line(data=output_wide,aes(x=insured.0.6,y=MC.0.6),color="blue")+
#   geom_line(data=output_wide,aes(x=insured.0.6,y=MC.0.8),color="blue")+
#   geom_line(data=output_wide,aes(x=insured.0.6,y=1000*P_low)) +
#   geom_line(data=output_wide,aes(x=insured.0.6,y=1000*P_new.0.6),color="green") +
#   geom_line(data=output_wide,aes(x=insured.0.6,y=1000*P_new.0.8),color="green") +
#   geom_line(data=output_wide,aes(x=insured.0.6,y=P_diff),color="green") +
#   coord_cartesian(xlim=c(.1,.95),ylim=c(0,15000))



## Perfect Competition
output = NULL
for (p_l in seq(-.25,12,.25)){
  print(p_l)
  acs[AV==.6,Price:=p_l]

  err = 10
  p_h = p_l + 3
  cnt = 0
  while (abs(err)>.01){
    cnt = cnt+1
    acs[AV==.8,Price:=p_h]
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
    beta_r = matrix(acs$HCC_Silver,nrow=nrow(acs),ncol=length(sigma),byrow=FALSE)*sigma

    chars_val = chars%*%beta0 +
      beta_z[,1]*chars[,1] + #Demographic Effect on Price
      beta_r[,1]*chars[,2] + #Risk Effect on AV
      beta_r[,2]*chars[,3] + #Risk Effect on Big
      pref_int #mean(FE_pars)
    #alpha = beta0[1] + beta_z[,1]
    #price_val = chars[,1]*alpha

    # util_val=exp(chars_val)
    #
    # ## Sum Across Products
    # ind_low = (1:(length(util_val)/2))*2 - 1
    # ind_high = (1:(length(util_val)/2))*2
    # util_mat = matrix(NA,nrow=length(ind_low),ncol=2)
    # util_mat[,1] = util_val[ind_low]
    # util_mat[,2] = util_val[ind_high]
    # util_mat = util_mat[rep(1:nrow(util_mat),each=2),]
    # exp_sum = apply(util_mat,FUN=sum,MARGIN=1)
    #

    acs[,util:=exp(chars_val)]
    acs[,alpha:=beta0[1] + beta_z[,1]]
    acs[,expsum:=sum(util),by="Person"]
    acs[,shares:=util/(1+expsum)]
    acs[,dsdp:=alpha*shares*(1-shares)]

    res_temp = acs[,list(insured=sum(shares*PERWT)/sum(PERWT),
                         SA = sum(shares*ageRate*PERWT)/sum(PERWT),
                         AC = 12*sum(C*shares*PERWT)/sum(shares*PERWT),
                         MC = 12*sum(C*dsdp*PERWT)/sum(dsdp*PERWT)),by=c("AV","Price")]
    res_temp[,P_new:=(AC)/1000]
    res_temp[,P_low:=p_l]
    err = with(res_temp[res_temp$AV==.8,],P_new-Price)
    p_h = p_h + err
    print(err)
    print(res_temp$P_new[res_temp$AV==.8])
  }
  print(p_h)

  # res_temp = acs[,list(insured=sum(shares*PERWT)/sum(PERWT),
  #                      AC = 12*sum(C*shares*PERWT)/sum(shares*PERWT),
  #                      MC = 12*sum(C*dsdp*PERWT)/sum(dsdp*PERWT),
  #                      Markup = -12*sum(shares*PERWT)/sum(dsdp*PERWT)),by=c("AV","Price")]
  res_temp[,c("Price"):=NULL]
  output = rbind(output,res_temp)
}
output[,total_insured:=sum(insured),by="P_low"]
output_wide_PC = reshape(output,timevar=c("AV"),
                         idvar=c("P_low","total_insured"),
                         direction="wide")

output_wide_PC[,P_diff:=(AC.0.8-1000*P_low)]
output_wide_PC[,Eq:=abs(P_low-P_new.0.6)]

ggplot() +
  geom_line(data=output_wide,aes(x=insured.0.6,y=AC.0.6),color="red")+
  geom_line(data=output_wide,aes(x=insured.0.6,y=AC.0.8),color="red")+
  geom_line(data=output_wide,aes(x=insured.0.6,y=MC.0.6),color="blue")+
  geom_line(data=output_wide,aes(x=insured.0.6,y=MC.0.8),color="blue")+
  geom_line(data=output_wide,aes(x=insured.0.6,y=1000*P_low)) +
  geom_line(data=output_wide,aes(x=insured.0.6,y=1000*P_new.0.6),color="green") +
  geom_line(data=output_wide,aes(x=insured.0.6,y=1000*P_new.0.8),color="green") +
  geom_line(data=output_wide,aes(x=insured.0.6,y=P_diff),color="green") +
  coord_cartesian(xlim=c(.1,.95),ylim=c(0,15000))


ggplot() +
  geom_line(data=output_wide_PC,aes(x=insured.0.6,y=AC.0.6),color="red")+
  geom_line(data=output_wide_PC,aes(x=insured.0.6,y=AC.0.8),color="red")+
  geom_line(data=output_wide_PC,aes(x=insured.0.6,y=MC.0.6),color="blue")+
  geom_line(data=output_wide_PC,aes(x=insured.0.6,y=MC.0.8),color="blue")+
  geom_line(data=output_wide_PC,aes(x=insured.0.6,y=1000*P_low))+
  geom_line(data=output_wide_PC,aes(x=insured.0.6,y=P_diff),color="green") +
  coord_cartesian(xlim=c(.1,.95),ylim=c(0,15000))

output_wide[P_low>=3&P_low<=5,]
output_wide_PC[P_low>=3&P_low<=5,]




#### Comparative Statics on Competition ####

acs_small = as.data.frame(acs)
acs_large = as.data.frame(acs)
acs_small$Firm = "S"
acs_large$Firm = "L"
acs = as.data.table(rbind(acs_small,acs_large))


pref_int = 2 #+ mean(FE_pars)
## Monopolist
pref_int = 2
J = 2
inside_pref = J*exp(pref_int)
large_share = .9
output = NULL
p_sm_l = 3
p_lg_l = 3
p_sm_h = p_l + 3
p_lg_h = p_l + 3
for (large_share in seq(.9,.5,-.05)){
  print("Size of Large Firm")
  print(large_share)

  cnt = 0
  err_l = 10 
  while(abs(sum(err_l))>.01){
    err_h = 10
    acs[AV==.6&Firm=="S",P_base:=p_sm_l]
    acs[AV==.6&Firm=="L",P_base:=p_lg_l]
    while (sum(abs(err_h))>.01){
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
      beta_r = matrix(acs$HCC_Silver,nrow=nrow(acs),ncol=length(sigma),byrow=FALSE)*sigma
      
      chars_val = chars%*%beta0 + 
        beta_z[,1]*chars[,1] + #Demographic Effect on Price
        beta_r[,1]*chars[,2] + #Risk Effect on AV
        beta_r[,2]*chars[,3]   #Risk Effect on Big
        #pref_int mean(FE_pars)
      # alpha = beta0[1] + beta_z[,1]
      # price_val = chars[,1]*alpha
      acs[,util:=exp(chars_val)]
      acs[Firm=="L",util:=util*large_share*inside_pref]
      acs[Firm=="S",util:=util*(1-large_share)*inside_pref]
      
      acs[,alpha:=beta0[1] + beta_z[,1]]
      acs[,expsum:=sum(util),by="Person"]
      acs[,shares:=util/(1+expsum)]
      
      # acs[AV==0.6,share_low:=shares]
      # acs[,share_low:=max(share_low,na.rm=TRUE),by="Person"]
      # acs[AV==0.8,share_high:=shares]
      # acs[,share_high:=max(share_high,na.rm=TRUE),by="Person"]
      
      acs[,dsdp:=alpha*shares*(1-shares)]
      # acs[AV==0.6,dsdp_low:=alpha*share_low*(1-shares)]
      # acs[AV==0.8,dsdp_low:=-alpha*share_low*(shares)]
      # 
      # acs[AV==0.6,dsdp_high:=-alpha*share_high*(shares)]
      # acs[AV==0.8,dsdp_high:=alpha*share_high*(1-shares)]
      # 
      # 
      acs[,CW:=log(sum(util)),by="Person"]
      acs[,lowrisk:=as.numeric(HCC_Silver==0)]
      acs[,midrisk:=as.numeric(HCC_Silver>0&HCC_Silver<2.18)]
      acs[,highrisk:=as.numeric(HCC_Silver>2.18)]
      
      res_temp = acs[,list(insured=sum(shares*PERWT)/sum(PERWT),
                           SA = sum(shares*ageRate*PERWT)/sum(PERWT),
                           AC = 12*sum(C*shares*PERWT)/sum(shares*PERWT),
                           MC = 12*sum(C*dsdp*PERWT)/sum(dsdp*PERWT),
                           #Markup_A = -1000*sum(shares*ageRate*PERWT)/sum(dsdp*ageRate*PERWT),
                           Markup = -1000*sum(shares*PERWT)/sum(dsdp*PERWT),
                           ConsWelfare = sum(CW*PERWT),
                           CW_low = sum(CW*PERWT*lowrisk)/sum(lowrisk*PERWT),
                           CW_mid = sum(CW*PERWT*midrisk)/sum(midrisk*PERWT),
                           CW_high = sum(CW*PERWT*highrisk)/sum(highrisk*PERWT),
                           PlanWelfare = sum(J*shares*(AV*C*12/1000-Price))
                           
      ),by=c("AV","P_base","Firm")]
      res_temp[,PlanWelfare:=sum(PlanWelfare)]
      
      setkey(res_temp,Firm,AV)
      # res_temp[,P_new:=solve(as.matrix(res_temp[,c("dSAdp_low","dSAdp_high")]))%*%(-as.matrix(res_temp$SA) + as.matrix(res_temp[,(MC_low+MC_high)/1000]))]
      # res_temp[,MC:=1000*solve(as.matrix(res_temp[,c("dSAdp_low","dSAdp_high")]))%*%(as.matrix(res_temp[,(MC_low+MC_high)/1000]))]
      # res_temp[,Markup:=1000*solve(as.matrix(res_temp[,c("dSAdp_low","dSAdp_high")]))%*%(-as.matrix(res_temp$SA))]
      
      #res_temp[,P_new:=(MC_A+Markup_A)/1000]
      res_temp[,P_new:=(MC+Markup)/1000]
      err_h = with(res_temp[res_temp$AV==.8,],P_new-P_base)  
      p_lg_h = p_lg_h + .75*err_h[1]
      p_sm_h = p_sm_h + .75*err_h[2]
      print(err_h)
      #print(res_temp$P_new[res_temp$AV==.8])
      print(c(p_lg_h,p_sm_h))
    }
    print(c(p_lg_h,p_sm_h))
    print("UPDATE P_l")
    err_l = with(res_temp[res_temp$AV==.6,],P_new-P_base)
    p_lg_l = p_lg_l + .75*err_l[1]
    p_sm_l = p_sm_l + .75*err_l[2]
    #p_l = p_l +.5*err_l
    print(c(p_lg_l,p_sm_l))
  }
  res_temp[,FirmSize:=large_share]
  res_temp[,c("P_base"):=NULL]
  output = rbind(output,res_temp)
}

output[,total_insured:=sum(insured),by="FirmSize"]
output_wide_J = reshape(output,timevar=c("AV"),
                        idvar=c("numFirm","total_insured"),
                        direction="wide")

ggplot() + 
  geom_line(data=output_wide_J,aes(x=numFirm,y=AC.0.6),color="red")+ 
  geom_line(data=output_wide_J,aes(x=numFirm,y=AC.0.8),color="red")+ 
  geom_line(data=output_wide_J,aes(x=numFirm,y=MC.0.6),color="blue")+ 
  geom_line(data=output_wide_J,aes(x=numFirm,y=MC.0.8),color="blue")+
  geom_line(data=output_wide_J,aes(x=numFirm,y=10000*insured.0.6)) +
  geom_line(data=output_wide_J,aes(x=numFirm,y=10000*insured.0.8)) +
  geom_line(data=output_wide_J,aes(x=numFirm,y=1000*P_new.0.6),color="green") +
  geom_line(data=output_wide_J,aes(x=numFirm,y=1000*P_new.0.8),color="green") 


#### Comparative Statics on Competition With Risk Adjustment ####

output = NULL
p_l = 5
p_h = p_l + 3
for (J in c(2:10)){
  print("Number of Firms")
  print(J)
  err_l = 10 
  cnt = 0
  while(abs(err_l)>.005){
    err_h = 10
    acs[AV==.6,P_base:=p_l]
    while (abs(err_h)>.005){
      cnt = cnt+1
      acs[AV==.8,P_base:=p_h]
      
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
      beta_r = matrix(acs$HCC_Silver,nrow=nrow(acs),ncol=length(sigma),byrow=FALSE)*sigma
      
      chars_val = chars%*%beta0 + 
        beta_z[,1]*chars[,1] + #Demographic Effect on Price
        beta_r[,1]*chars[,2] + #Risk Effect on AV
        beta_r[,2]*chars[,3] +  #Risk Effect on Big
        pref_int #mean(FE_pars)
      # alpha = beta0[1] + beta_z[,1]
      # price_val = chars[,1]*alpha
      
      acs[,util:=exp(chars_val)]
      acs[,alpha:=beta0[1] + beta_z[,1]]
      acs[,expsum:=sum(J*util),by="Person"]
      acs[,shares:=util/(1+expsum)]
      
      acs[AV==0.6,share_low:=shares]
      acs[,share_low:=max(share_low,na.rm=TRUE),by="Person"]
      acs[AV==0.8,share_high:=shares]
      acs[,share_high:=max(share_high,na.rm=TRUE),by="Person"]

      acs[,dsdp:=alpha*shares*(1-shares)]
      acs[AV==0.6,dsdp_low:=alpha*share_low*(1-shares)]
      acs[AV==0.8,dsdp_low:=-alpha*share_low*(shares)]

      acs[AV==0.6,dsdp_high:=-alpha*share_high*(shares)]
      acs[AV==0.8,dsdp_high:=alpha*share_high*(1-shares)]
      # 
      # 
      acs[,CW:=log(J*sum(util)),by="Person"]
      
      res_temp = acs[,list(insured=sum(J*shares*PERWT)/sum(PERWT),
                           dsdp = sum(dsdp*PERWT)/sum(PERWT),
                           SA = sum(shares*ageRate*PERWT)/sum(PERWT),
                           AC = 12*sum(C*shares*PERWT)/sum(shares*PERWT),
                           MC = 12*sum(C*dsdp*PERWT)/sum(dsdp*PERWT),
                           dSdp_low = sum(dsdp_low*PERWT)/sum(PERWT),
                           dSdp_high = sum(dsdp_high*PERWT)/sum(PERWT),
                           PC_all = 12*sum(C*PERWT)/sum(PERWT),
                           PMC = 12*sum(C*(dsdp_low+dsdp_high)*PERWT)/sum((dsdp_high+dsdp_low)*PERWT),
                           MC_low = 12*sum(C*dsdp_low*PERWT)/sum(dsdp_low*PERWT),
                           MC_high = 12*sum(C*dsdp_high*PERWT)/sum(dsdp_high*PERWT),
                           #Markup_A = -1000*sum(shares*ageRate*PERWT)/sum(dsdp*ageRate*PERWT),
                           Markup = -1000*sum(shares*PERWT)/sum(dsdp*PERWT),
                           #Pmkup = -1000*sum((share_low+share_high)*PERWT)/sum((dsdp_high+dsdp_low)*PERWT),
                           ConsWelfare = sum(CW*PERWT),
                           PlanWelfare = sum(J*shares*(AV*C*12/1000-Price))
      ),by=c("AV","P_base")]
      res_temp[,PlanWelfare:=sum(PlanWelfare)]
      res_temp[,PC:=sum(AC*insured)/sum(insured)]
      res_temp[,PMC2:=sum(MC*dsdp)/sum(dsdp)]
      
      setkey(res_temp,AV)
      # res_temp[,P_new:=solve(as.matrix(res_temp[,c("dSAdp_low","dSAdp_high")]))%*%(-as.matrix(res_temp$SA) + as.matrix(res_temp[,(MC_low+MC_high)/1000]))]
      # res_temp[,MC:=1000*solve(as.matrix(res_temp[,c("dSAdp_low","dSAdp_high")]))%*%(as.matrix(res_temp[,(MC_low+MC_high)/1000]))]
      # res_temp[,Markup:=1000*solve(as.matrix(res_temp[,c("dSAdp_low","dSAdp_high")]))%*%(-as.matrix(res_temp$SA))]
      
      #res_temp[,P_new:=(MC_A+Markup_A)/1000]
      res_temp[,P_new:=(PMC+Pmkup)/1000]
      err_h = with(res_temp[res_temp$AV==.8,],P_new-P_base)  
      p_h = p_h + .2*err_h
      print(err_h)
      print(res_temp$P_new[res_temp$AV==.8])
      print(p_h)
    }
    print(p_h)
    print("UPDATE P_l")
    err_l = with(res_temp[res_temp$AV==.6,],P_new-P_base) 
    p_l = p_l +.5*err_l
    print(p_l)
  }
  res_temp[,numFirm:=J]
  res_temp[,c("P_base"):=NULL]
  output = rbind(output,res_temp)
}

output[,total_insured:=sum(insured),by="numFirm"]
output_wide_RA = reshape(output,timevar=c("AV"),
                        idvar=c("numFirm","total_insured"),
                        direction="wide")

ggplot() + 
  geom_line(data=output_wide_J,aes(x=numFirm,y=AC.0.6),color="red")+ 
  geom_line(data=output_wide_J,aes(x=numFirm,y=AC.0.8),color="red")+ 
  geom_line(data=output_wide_J,aes(x=numFirm,y=MC.0.6),color="blue")+ 
  geom_line(data=output_wide_J,aes(x=numFirm,y=MC.0.8),color="blue")+
  geom_line(data=output_wide_J,aes(x=numFirm,y=10000*insured.0.6)) +
  geom_line(data=output_wide_J,aes(x=numFirm,y=10000*insured.0.8)) +
  geom_line(data=output_wide_J,aes(x=numFirm,y=1000*P_new.0.6),color="green") +
  geom_line(data=output_wide_J,aes(x=numFirm,y=1000*P_new.0.8),color="green") +
  coord_cartesian(ylim=c(0,10000))

ggplot() + 
  geom_line(data=output_wide_RA,aes(x=numFirm,y=AC.0.6),color="red")+ 
  geom_line(data=output_wide_RA,aes(x=numFirm,y=AC.0.8),color="red")+ 
  geom_line(data=output_wide_RA,aes(x=numFirm,y=MC.0.6),color="blue")+ 
  geom_line(data=output_wide_RA,aes(x=numFirm,y=MC.0.8),color="blue")+
  geom_line(data=output_wide_RA,aes(x=numFirm,y=10000*insured.0.6)) +
  geom_line(data=output_wide_RA,aes(x=numFirm,y=10000*insured.0.8)) +
  geom_line(data=output_wide_RA,aes(x=numFirm,y=1000*P_new.0.6),color="green") +
  geom_line(data=output_wide_RA,aes(x=numFirm,y=1000*P_new.0.8),color="green") +
  coord_cartesian(ylim=c(0,10000))
