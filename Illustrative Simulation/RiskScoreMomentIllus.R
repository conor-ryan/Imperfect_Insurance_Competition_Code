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
acs[,riskDraw:= runif(nrow(acs))]

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
  geom_histogram(data=acs,aes(x=log(HCC_Silver),weights=PERWT,y=..density..,fill="Model"),binwidth=.3,alpha=0.6)+
  geom_histogram(data=meps,aes(x=log(HCC_Score_Silver),weights=PERWT15F,y=..density..,fill="Data"),binwidth=.3,alpha=0.6)+
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
  # coord_cartesian(xlim=c(0,20)) +
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
