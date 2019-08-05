rm(list = ls())
library(doBy)
library(data.table)
library(nleqslv)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

#### Read in Risk Adjustment Data ####
claims = read.csv("Data/2015_MLR/Part1_2_Summary_Data_Premium_Claims.csv")


payments = claims[claims$ROW_LOOKUP_CODE=="FED_RISK_ADJ_NET_PAYMENTS",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_Q1")]
names(payments) = c("ï..MR_SUBMISSION_TEMPLATE_ID","Payments")

payments_RC = claims[claims$ROW_LOOKUP_CODE=="FED_RISK_ADJ_NET_PAYMENTS",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_RC")]
names(payments_RC) = c("ï..MR_SUBMISSION_TEMPLATE_ID","Payments_RC")

enroll =claims[claims$ROW_LOOKUP_CODE=="NUMBER_OF_LIFE_YEARS",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_Q1")]
names(enroll) = c("ï..MR_SUBMISSION_TEMPLATE_ID","MLR_lives")

months =claims[claims$ROW_LOOKUP_CODE=="MEMBER_MONTHS",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_Q1")]
names(months) = c("ï..MR_SUBMISSION_TEMPLATE_ID","memberMonths")

months_RC =claims[claims$ROW_LOOKUP_CODE=="MEMBER_MONTHS",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_RC")]
names(months_RC) = c("ï..MR_SUBMISSION_TEMPLATE_ID","memberMonths_RC")

revenue =claims[claims$ROW_LOOKUP_CODE=="TOTAL_DIRECT_PREMIUM_EARNED",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_Q1")]
names(revenue) = c("ï..MR_SUBMISSION_TEMPLATE_ID","Revenue")


RA_claims = merge(payments,enroll,by="ï..MR_SUBMISSION_TEMPLATE_ID")
RA_claims = merge(RA_claims,payments_RC,by="ï..MR_SUBMISSION_TEMPLATE_ID")
RA_claims = merge(RA_claims,months,by="ï..MR_SUBMISSION_TEMPLATE_ID")
RA_claims = merge(RA_claims,months_RC,by="ï..MR_SUBMISSION_TEMPLATE_ID")
RA_claims = merge(RA_claims,revenue,by="ï..MR_SUBMISSION_TEMPLATE_ID")


# Remove non-Individual Market Insurers
RA_claims$absent1 = is.na(RA_claims$MLR_lives) | RA_claims$MLR_lives==0
RA_claims = RA_claims[!RA_claims$absent1,]

# Merge in and summarize by Firm Name
crosswalk = read.csv("Intermediate_Output/FirmCrosswalk.csv")
crosswalk = unique(crosswalk[,c("ï..MR_SUBMISSION_TEMPLATE_ID","Firm","STATE")])

RA_claims = merge(RA_claims,crosswalk[,c("ï..MR_SUBMISSION_TEMPLATE_ID","Firm","STATE")],by="ï..MR_SUBMISSION_TEMPLATE_ID")
RA_claims = summaryBy(MLR_lives+Payments+Payments_RC+Revenue+memberMonths+memberMonths_RC~Firm+STATE,data=RA_claims,FUN=sum,na.rm=TRUE,keep.names=TRUE)


#### Read in Product Data ####
mkt_data = read.csv("Intermediate_Output/Estimation_Data/marketDataMap_discrete.csv")
firm_RA = summaryBy(enroll~STATE+Firm,data=mkt_data,FUN=sum)
names(firm_RA) = c("ST","Firm","lives")
firm_RA = as.data.table(firm_RA)
firm_RA[,share:=lives/sum(lives),by="ST"]
## Merge with Risk Adjustment Data ##

RA_claims = RA_claims[RA_claims$STATE%in%firm_RA$ST,]
RA_claims = as.data.table(RA_claims)
setkey(RA_claims,STATE,Firm)


firm_RA = merge(firm_RA,RA_claims,by.x=c("ST","Firm"),by.y=c("STATE","Firm"),all=TRUE)

## MLR Average Premium
firm_RA[,MLR_avg_prem:=sum(Revenue)/sum(memberMonths),by="ST"]


#### Approximate ACA-Compliant Months ####
firm_RA[abs(Payments)>0,RA_diff:= abs(Payments - Payments_RC)/abs(Payments)]
firm_RA[Payments==0,RA_diff:=0]
firm_RA[,MM_diff:= abs(memberMonths-memberMonths_RC)/memberMonths]

firm_RA[,MM_compliant:=memberMonths_RC]
# This is a reporting error...
firm_RA[Firm=="HEALTH_REPUBLIC_INSURANCE",MM_compliant:=memberMonths]
firm_RA[,MM_noncompliant:=memberMonths-memberMonths_RC]
firm_RA[RA_diff<1e-3,MM_noncompliant:=0]
firm_RA[,RA_flag:=0]
firm_RA[RA_diff>=1e-3,RA_flag:=1]
firm_RA[,noncompliant_share:=MM_noncompliant/sum(MM_noncompliant),by="ST"]

#### Compile un-matched firms into Other ####
firm_RA[is.na(share),Firm:="OTHER"]
firm_RA = firm_RA[,lapply(.SD,sum),by=c("Firm","ST","MLR_avg_prem","share"),
                  .SDcols=c("memberMonths","MM_noncompliant","MM_compliant","Payments","Payments_RC","noncompliant_share")]


#### Merge with Interim Data ####
interim = read.csv("Data/RA_Interim_2015.csv")
names(interim) = c("ST","RA_prem","AvgPLRS","AvgARF","AvgAV","AvgIDF","RA_MM")

firm_RA = merge(firm_RA,interim[,c("ST","RA_prem","AvgPLRS","AvgARF","RA_MM")],
                by="ST")

## Correct when Risk Adjustment Member Months are below reported QHP MM
## This a small correction in 3 states
firm_RA[,QHP_diff:=(RA_MM-sum(MM_compliant))/RA_MM,by="ST"]
firm_RA[QHP_diff<0&MM_compliant>0,MM_noncompliant:=0]
firm_RA[QHP_diff<0,RA_MM_new:=sum(MM_compliant)+sum(MM_noncompliant),by="ST"]
firm_RA[QHP_diff<0,noncompliant_share:=0]
firm_RA[QHP_diff<0,MM_compliant_est:=MM_compliant+MM_noncompliant]



firm_RA[QHP_diff>=0,MM_compliant_est:=(RA_MM-sum(MM_compliant))*noncompliant_share+MM_compliant,by="ST"]


firm_RA[,T_norm:=-Payments/(MM_compliant_est*RA_prem)]
firm_RA[Payments==0,T_norm:=0]

### Average Risk Assuming Balanced Age Distribution
firm_RA[,R_est:=(T_norm+1)*AvgPLRS]



firm_RA[,ST_MLR_months:=sum(memberMonths),by="ST"]
firm_RA[,MLR_share:=memberMonths/ST_MLR_months]
setkey(firm_RA,ST,Firm)

# Adjust Payments to sum to 0
firm_RA[,payments_adj:=Payments]
firm_RA[Firm=="OTHER",payments_adj:=0]
firm_RA[,payments_adj_net:=sum(payments_adj),by="ST"]
firm_RA[Firm=="OTHER",payments_adj:=-payments_adj_net]

# Set avg_prem everywhere
firm_RA[,MLR_avg_prem:=max(MLR_avg_prem,na.rm=TRUE),by="ST"]



### Average Transfer - Assuming Balanced Ages
firm_RA[,T_norm_adj:=-payments_adj/(memberMonths*RA_prem)]
firm_RA[,R_adj:=(T_norm_adj+1)*AvgPLRS]
firm_RA[,R_adj_natl:=(T_norm_adj+1)*1.448]
firm_RA[,ST_R:=sum(memberMonths*R_adj_natl)/sum(memberMonths),by="ST"]



firm_RA[,names(firm_RA)[!names(firm_RA)%in%c("Firm","ST","payments_adj",
                                             "ST_MLR_months","memberMonths","MM_compliant","T_norm","T_norm_adj","R_adj","R_adj_natl",
                                             "RA_prem","AvgPLRS","AvgARF")]:=NULL]

firm_RA[,HighRisk:=0]
firm_RA[T_norm_adj>0.08,HighRisk:=1] # 75th percentile of most risky
firm_RA[,share:=memberMonths/(sum(memberMonths)),by="ST"]



#### Save Files ####
firmRiskFile = "Simulation_Risk_Output/FirmRiskScores_woSim.rData"
save(firm_RA,file=firmRiskFile)
rm(list=ls())
