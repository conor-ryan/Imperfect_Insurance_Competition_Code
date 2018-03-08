rm(list = ls())
library(doBy)
library(noncensus)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")
##### Firm IDs  ####
firms = read.csv("Data/2015_MLR/MR_Submission_Template_Header.csv",stringsAsFactors=FALSE)
firms = firms[,c("ï..MR_SUBMISSION_TEMPLATE_ID","BUSINESS_STATE","GROUP_AFFILIATION","COMPANY_NAME","DBA_MARKETING_NAME")]
 
# #### eHealth Market Shares ####

eHealth = read.csv("C:/Users/Conor/Documents/Research/eHealth Data/eHealth_2015.csv",stringsAsFactors = FALSE)
# Drop "referential integrity" rows
eHealth = eHealth[eHealth$PLAN_NAME!="UNKNOWN PLAN - this row is used for referential integrity - DSTOCK",]

# Drop eHealth observations with NA or 0 zip code
eHealth = eHealth[with(eHealth,!is.na(TRUNCATED_ZIP)&TRUNCATED_ZIP!=0 ),]
eHealth = eHealth[with(eHealth,PLAN_METAL_LEVEL!="N/A"),]

# State Level Market Shares
eHealth$count = 1
shares = summaryBy(count~STATE+CARRIER_NAME,data=eHealth,FUN=sum,keep.names=TRUE)
shares$share = shares$count/ave(shares$count,shares$STATE,FUN=sum)


#### Plan Counts ####
planData = read.csv("Data/2015_Premiums/2015_RWJF.csv")


planData$planCount = 1
planData = summaryBy(planCount~CARRIER+ST,data=planData,FUN=sum,keep.names = TRUE)


##### Individual Market Shares #####
claims = read.csv("Data/2015_MLR/Part1_2_Summary_Data_Premium_Claims.csv")


payments = claims[claims$ROW_LOOKUP_CODE=="FED_RISK_ADJ_NET_PAYMENTS",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_Q1","CMM_INDIVIDUAL_RC")]
names(payments) = c("ï..MR_SUBMISSION_TEMPLATE_ID","Payments1","Payments2")

enroll =claims[claims$ROW_LOOKUP_CODE=="NUMBER_OF_LIFE_YEARS",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_Q1","CMM_INDIVIDUAL_RC")]
names(enroll) = c("ï..MR_SUBMISSION_TEMPLATE_ID","Enrollment","EnrollmentQHP")

indMarket = merge(payments,enroll,by="ï..MR_SUBMISSION_TEMPLATE_ID")

# Remove non-Individual Market Insurers
indMarket$absent1 = is.na(indMarket$Enrollment) | indMarket$Enrollment==0
indMarket$absent2 = is.na(indMarket$EnrollmentQHP) | indMarket$EnrollmentQHP==0
indMarket = indMarket[!(indMarket$absent1&indMarket$absent2),c("ï..MR_SUBMISSION_TEMPLATE_ID","Payments1","Payments2","Enrollment","EnrollmentQHP")]

#### Merge-in Firm Info ####
firmCrosswalk = read.csv("Intermediate_Output/FirmCrosswalk.csv")

indMarket = merge(indMarket,firmCrosswalk,by="ï..MR_SUBMISSION_TEMPLATE_ID",all.y=TRUE)
indMarket = merge(indMarket,shares[shares$STATE%in%firmCrosswalk$STATE,],by.y=c("STATE","CARRIER_NAME"),by.x=c("STATE","eHealth_CARRIER_NAME"),all=TRUE)
indMarket = merge(indMarket,planData[planData$ST%in%firmCrosswalk$STATE,],by.y=c("ST","CARRIER"),by.x=c("STATE","RWJF_CARRIER"),all=TRUE)
#Drop un-matched plan choices (These firms are not present in eHealth Data)
indMarket = indMarket[!(is.na(indMarket$DBA_MARKETING_NAME)&is.na(indMarket$eHealth_CARRIER_NAME)),]



indMarket$TOTAL_LIVES = ave(indMarket$Enrollment,indMarket$BUSINESS_STATE,FUN=function(x){sum(x,na.rm=TRUE)})
indMarket$MARKET_SHARE1 = with(indMarket,Enrollment/TOTAL_LIVES)
indMarket$TOTAL_LIVES = ave(indMarket$EnrollmentQHP,indMarket$BUSINESS_STATE,FUN=function(x){sum(x,na.rm=TRUE)})
indMarket$MARKET_SHARE2 = with(indMarket,EnrollmentQHP/TOTAL_LIVES)
indMarket = indMarket[with(indMarket,order(BUSINESS_STATE,-MARKET_SHARE2)),]


indMarket$shareRank = indMarket$MARKET_SHARE1
indMarket$shareRank[is.na(indMarket$shareRank)] = indMarket$share[is.na(indMarket$shareRank)]
indMarket = indMarket[order(indMarket$BUSINESS_STATE,-indMarket$shareRank),]


write.csv(indMarket[,c("STATE","Firm","GROUP_AFFILIATION","COMPANY_NAME","DBA_MARKETING_NAME","eHealth_CARRIER_NAME","RWJF_CARRIER","Payments1","Payments2",
                                           "MARKET_SHARE1","MARKET_SHARE2","count","share","planCount")],
          "firmNamesTest.csv",row.names=FALSE)


