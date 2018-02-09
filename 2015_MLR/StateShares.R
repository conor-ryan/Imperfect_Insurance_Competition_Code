rm(list = ls())
library(doBy)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")
##### Firm IDs  ####
firms = read.csv("Data/2015_MLR/MR_Submission_Template_Header.csv")
firms = firms[,c("ï..MR_SUBMISSION_TEMPLATE_ID","BUSINESS_STATE","GROUP_AFFILIATION","COMPANY_NAME","DBA_MARKETING_NAME")]


##### Individual Market Shares #####
claims = read.csv("Data/2015_MLR/Part1_2_Summary_Data_Premium_Claims.csv")

claims = claims[claims$ROW_LOOKUP_CODE=="NUMBER_OF_LIFE_YEARS",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_YEARLY","CMM_INDIVIDUAL_Q1")]
claims = claims[!is.na(claims$CMM_INDIVIDUAL_Q1),]

indMarket = merge(claims,firms,by="ï..MR_SUBMISSION_TEMPLATE_ID")
indMarket$TOTAL_LIVES = ave(indMarket$CMM_INDIVIDUAL_Q1,indMarket$BUSINESS_STATE,FUN=sum)
indMarket$MARKET_SHARE = with(indMarket,CMM_INDIVIDUAL_Q1/TOTAL_LIVES)
indMarket = indMarket[with(indMarket,order(BUSINESS_STATE,-MARKET_SHARE)),]
write.csv(indMarket,"Intermediate_Output/MLR_Shares/MLR_Claims_Shares.csv",row.names=FALSE)

#### Share Analysis ####
maxShare = summaryBy(MARKET_SHARE~BUSINESS_STATE,data=indMarket,FUN=max,keep.names=TRUE)
maxShare = maxShare[!maxShare$BUSINESS_STATE%in%c("United States Virgin Islands","Puerto Rico","Northern Mariana Islands","Guam","Grand Total","American Samoa"),]

indMarket$rank = ave(-indMarket$MARKET_SHARE,indMarket$BUSINESS_STATE,FUN=rank,na.rm=TRUE)
big3 = indMarket[indMarket$rank<=2&
                   !indMarket$BUSINESS_STATE%in%c("United States Virgin Islands","Puerto Rico","Northern Mariana Islands","Guam","Grand Total","American Samoa"),]

bigShare = summaryBy(MARKET_SHARE~BUSINESS_STATE,data=big3,FUN=sum,keep.names=TRUE)


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
