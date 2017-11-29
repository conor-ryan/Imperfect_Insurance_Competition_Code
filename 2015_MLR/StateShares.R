rm(list = ls())
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
write.csv(indMarket,"Intermediate_Output/MLR_Shares/MLR_Claims_Shares.csv")