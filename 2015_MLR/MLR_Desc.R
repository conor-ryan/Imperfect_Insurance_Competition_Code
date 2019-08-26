rm(list = ls())
library(doBy)
library(noncensus)
library(data.table)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")
##### Firm IDs  ####
firms = read.csv("Data/2015_MLR/MR_Submission_Template_Header.csv",stringsAsFactors=FALSE)
firms = firms[,c("ï..MR_SUBMISSION_TEMPLATE_ID","BUSINESS_STATE","GROUP_AFFILIATION","COMPANY_NAME","DBA_MARKETING_NAME")]

##### Individual Market Shares #####
claims = read.csv("Data/2015_MLR/Part1_2_Summary_Data_Premium_Claims.csv")
rebates = read.csv("Data/2015_MLR/Part3_MLR_Rebate_Calculation.csv")


payments = claims[claims$ROW_LOOKUP_CODE=="FED_RISK_ADJ_NET_PAYMENTS",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_Q1","CMM_INDIVIDUAL_RC")]
names(payments) = c("ï..MR_SUBMISSION_TEMPLATE_ID","Payments1","Payments2")

enroll =claims[claims$ROW_LOOKUP_CODE=="MEMBER_MONTHS",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_Q1","CMM_INDIVIDUAL_RC")]
names(enroll) = c("ï..MR_SUBMISSION_TEMPLATE_ID","Enrollment","EnrollmentQHP")

revenue =claims[claims$ROW_LOOKUP_CODE=="TOTAL_DIRECT_PREMIUM_EARNED",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_Q1","CMM_INDIVIDUAL_RC")]
names(revenue) = c("ï..MR_SUBMISSION_TEMPLATE_ID","Revenue","RevenueQHP")

premiums =claims[claims$ROW_LOOKUP_CODE=="DIRECT_PREMIUM_WRITTEN",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_Q1","CMM_INDIVIDUAL_RC")]
names(premiums) = c("ï..MR_SUBMISSION_TEMPLATE_ID","Premiums","PremiumsQHP")

costs =claims[claims$ROW_LOOKUP_CODE=="TOTAL_INCURRED_CLAIMS_PT2",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_Q1","CMM_INDIVIDUAL_RC")]
names(costs) = c("ï..MR_SUBMISSION_TEMPLATE_ID","Cost","CostQHP")

constrained =rebates[rebates$ROW_LOOKUP_CODE=="REBATE_AMT_CREDIBILITY_ADJ_MLR",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_TOTAL")]
names(constrained) = c("ï..MR_SUBMISSION_TEMPLATE_ID","Const2")

indMarket = merge(payments,enroll,by="ï..MR_SUBMISSION_TEMPLATE_ID")
indMarket = merge(indMarket,revenue,by="ï..MR_SUBMISSION_TEMPLATE_ID")
indMarket = merge(indMarket,premiums,by="ï..MR_SUBMISSION_TEMPLATE_ID")
indMarket = merge(indMarket,costs,by="ï..MR_SUBMISSION_TEMPLATE_ID")
indMarket = merge(indMarket,constrained,by="ï..MR_SUBMISSION_TEMPLATE_ID")

# Remove non-Individual Market Insurers
indMarket$absent1 = is.na(indMarket$Enrollment) | indMarket$Enrollment==0
indMarket$absent2 = is.na(indMarket$EnrollmentQHP) | indMarket$EnrollmentQHP==0
indMarket = indMarket[!(indMarket$absent1&indMarket$absent2),]


#### Merge-in Firm Info ####

indMarket = merge(indMarket,firms,by="ï..MR_SUBMISSION_TEMPLATE_ID",all.x=TRUE)
indMarket = as.data.table(indMarket)

crosswalk = read.csv("Intermediate_Output/FirmCrosswalk.csv")
crosswalk = unique(crosswalk[,c("ï..MR_SUBMISSION_TEMPLATE_ID","Firm","STATE")])
indMarket = merge(indMarket,crosswalk,by="ï..MR_SUBMISSION_TEMPLATE_ID")

choiceData = read.csv("Intermediate_Output/Simulated_BaseData/simchoiceData_discrete.csv")
choiceData = as.data.table(choiceData)
choiceData[,Metal_std:=gsub(" [0-9]+","",METAL)]

firmData = unique(choiceData[,c("ST","Firm")])

indMarket = merge(indMarket,firmData,by.x=c("STATE","Firm"),by.y=c("ST","Firm"))

#### Constrained ####
indMarket[,MLR:=Cost/Revenue]


#### Evidence for Across Firm Selection ####
indMarket[,risk_pmpm:=Payments1/Enrollment]
indMarket[,rev_pmpm:=Revenue/Enrollment]
indMarket[,prem_pmpm:=Premiums/Enrollment]
indMarket[,claims_pmpm:=Cost/Enrollment]
indMarket[prem_pmpm<=0,prem_pmpm:=NA]
indMarket[claims_pmpm<=0,claims_pmpm:=NA]


indMarket[!is.na(claims_pmpm)&!is.na(risk_pmpm),claims_pred:=predict(lm(claims_pmpm~risk_pmpm+BUSINESS_STATE))]


summary(indMarket[,lm(prem_pmpm~claims_pred+GROUP_AFFILIATION+BUSINESS_STATE)])
summary(indMarket[,lm(prem_pmpm~risk_pmpm+GROUP_AFFILIATION+BUSINESS_STATE)])
summary(indMarket[,lm(prem_pmpm~claims_pmpm+risk_pmpm+GROUP_AFFILIATION+BUSINESS_STATE)])

#### Share Analysis ####
stateShares = summaryBy(MARKET_SHARE1~BUSINESS_STATE+GROUP_AFFILIATION,data=indMarket,FUN=sum,keep.names=TRUE)
maxShare = summaryBy(MARKET_SHARE1~BUSINESS_STATE,data=stateShares,FUN=max,keep.names=TRUE)
maxShare = maxShare[!maxShare$BUSINESS_STATE%in%c("United States Virgin Islands","Puerto Rico","Northern Mariana Islands","Guam","Grand Total","American Samoa"),]
maxShare = maxShare[order(maxShare$MARKET_SHARE1),]

indMarket$rank = ave(-indMarket$MARKET_SHARE1,indMarket$BUSINESS_STATE,FUN=rank,na.rm=TRUE)
big2 = indMarket[indMarket$rank<=2&
                   !indMarket$BUSINESS_STATE%in%c("United States Virgin Islands","Puerto Rico","Northern Mariana Islands","Guam","Grand Total","American Samoa"),]

bigShare = summaryBy(MARKET_SHARE1~BUSINESS_STATE,data=big2,FUN=sum,keep.names=TRUE)
bigShare = bigShare[order(bigShare$MARKET_SHARE1),]

##HHI
stateShares$HHI = (stateShares$MARKET_SHARE1*100)^2
HHI = summaryBy(HHI~BUSINESS_STATE,data=stateShares,FUN=sum,keep.names=TRUE)
HHI = HHI[!HHI$BUSINESS_STATE%in%c("United States Virgin Islands","Puerto Rico","Northern Mariana Islands","Guam","Grand Total","American Samoa"),]
HHI = HHI[order(HHI$HHI),]

##### Risk Adjustment Analysis #####
claims = read.csv("Data/2015_MLR/Part1_2_Summary_Data_Premium_Claims.csv")

claims = claims[claims$ROW_LOOKUP_CODE%in%c("FED_RISK_ADJ_NET_PAYMENTS","TOTAL_INCURRED_CLAIMS_PT2"),
                c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_YEARLY","CMM_INDIVIDUAL_Q1","ROW_LOOKUP_CODE")]
claims = claims[!is.na(claims$CMM_INDIVIDUAL_Q1),]

indMarket = merge(claims,firms,by="ï..MR_SUBMISSION_TEMPLATE_ID")
payments = summaryBy(abs(CMM_INDIVIDUAL_Q1)~BUSINESS_STATE,
                     data=indMarket[indMarket$ROW_LOOKUP_CODE=="FED_RISK_ADJ_NET_PAYMENTS",],
                     FUN=sum,na.rm=TRUE)
names(payments) = c("State","Payments")
claims = summaryBy(abs(CMM_INDIVIDUAL_Q1)~BUSINESS_STATE,
                     data=indMarket[indMarket$ROW_LOOKUP_CODE=="TOTAL_INCURRED_CLAIMS_PT2",],
                     FUN=sum,na.rm=TRUE)
names(claims) = c("State","Claims")

riskadj = merge(payments,claims,by="State",all=TRUE)
riskadj = riskadj[!riskadj$State%in%c("United States Virgin Islands","Puerto Rico","Northern Mariana Islands","Guam","Grand Total","American Samoa"),]
riskadj$RAshare = with(riskadj,Payments/Claims)
sum(riskadj$Payments)/sum(riskadj$Claims)
