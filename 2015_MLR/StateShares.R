rm(list = ls())
library(doBy)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")
##### Firm IDs  ####
firms = read.csv("Data/2015_MLR/MR_Submission_Template_Header.csv",stringsAsFactors=FALSE)
firms = firms[,c("ï..MR_SUBMISSION_TEMPLATE_ID","BUSINESS_STATE","GROUP_AFFILIATION","COMPANY_NAME","DBA_MARKETING_NAME")]

firms$COMPANY_NAME[grep("Cigna",firms$COMPANY_NAME)] = "Cigna"
firms$COMPANY_NAME[grep("Health Net",firms$COMPANY_NAME)] = "Health Net"
firms$COMPANY_NAME[grep("Aetna",firms$COMPANY_NAME)] = "Aetna"
firms$COMPANY_NAME[grep("Anthem",firms$COMPANY_NAME)] = "Anthem"
firms$COMPANY_NAME[grep("Humana",firms$COMPANY_NAME)] = "Humana"
firms$COMPANY_NAME[grep("HUMANA",firms$GROUP_AFFILIATION)] = "Humana"
firms$COMPANY_NAME[grep("Coventry",firms$COMPANY_NAME)] = "Aetna"
firms$COMPANY_NAME[grep("Ambetter",firms$COMPANY_NAME)] = "Ambetter"
firms$COMPANY_NAME[grep("Kaiser",firms$COMPANY_NAME)] = "Kaiser"
firms$COMPANY_NAME[grep("Moda",firms$COMPANY_NAME)] = "Moda"
firms$COMPANY_NAME[grep("Molina",firms$COMPANY_NAME)] = "Molina"
firms$COMPANY_NAME[grep("Health Republic",firms$COMPANY_NAME)] = "Health Republic"
firms$COMPANY_NAME[grep("Oscar",firms$COMPANY_NAME)] = "Oscar"
firms$COMPANY_NAME[grep("UnitedHealth",firms$COMPANY_NAME)] = "UnitedHealthcare"
firms$COMPANY_NAME[grep("UNITEDHEALTH",firms$GROUP_AFFILIATION)] = "UnitedHealthcare"
firms$COMPANY_NAME[grep("Capital BlueCross",firms$COMPANY_NAME)] = "Capital BlueCross"
firms$COMPANY_NAME[grep("AmeriHealth",firms$COMPANY_NAME)] = "AmeriHealth"

firms$COMPANY_NAME[firms$GROUP_AFFILIATION=="Assurant Inc Grp"] = "Assurant Health"
firms$COMPANY_NAME[firms$COMPANY_NAME=="Allegian Insurance Company"] = "Allegian"
firms$COMPANY_NAME[firms$COMPANY_NAME=="Alliant Health Plans, Inc."] = "Alliant"
firms$COMPANY_NAME[firms$COMPANY_NAME=="Arches Mutual Insurance Company"] = "Arches Health Plan"
firms$COMPANY_NAME[firms$DBA_MARKETING_NAME=="Arkansas Blue Cross Blue Shield"] = "Arkansas Blue Cross and Blue Shield"
firms$COMPANY_NAME[firms$COMPANY_NAME=="Avera Health Plans, Inc."] = "Avera Health Plans"
firms$COMPANY_NAME[firms$COMPANY_NAME=="Premera Blue Cross"] = "Premera Blue Cross Blue Shield of Alaska"
firms$COMPANY_NAME[firms$COMPANY_NAME=="Celtic Insurance Company"] = "Ambetter"
firms$COMPANY_NAME[firms$GROUP_AFFILIATION=="QualChoice Holdings Inc."] = "QualChoice"
firms$COMPANY_NAME[firms$GROUP_AFFILIATION=="Blue Shield of California Group"] = "Blue Shield of California"
firms$COMPANY_NAME[firms$GROUP_AFFILIATION=="Wellpoint Inc Grp"&
                     firms$BUSINESS_STATE=="Georgia"] = "Anthem"
firms$COMPANY_NAME[firms$GROUP_AFFILIATION=="Wellpoint Inc Grp"&
                     firms$BUSINESS_STATE=="Missouri"] = "Anthem"
firms$COMPANY_NAME[firms$GROUP_AFFILIATION=="Peach State Health Plan"] = "Ambetter"
firms$COMPANY_NAME[firms$GROUP_AFFILIATION=="HCSC GRP"&
                     firms$BUSINESS_STATE=="Illinois"] = "Blue Cross and Blue Shield of Illinois"
firms$COMPANY_NAME[firms$GROUP_AFFILIATION=="HCSC GRP"&
                     firms$BUSINESS_STATE=="New Mexico"] = "Blue Cross and Blue Shield of New Mexico"
firms$COMPANY_NAME[firms$GROUP_AFFILIATION=="HCSC GRP"&
                     firms$BUSINESS_STATE=="Oklahoma"] = "Blue_Cross and Blue Shield of Oklahoma"
firms$COMPANY_NAME[firms$GROUP_AFFILIATION=="HCSC GRP"&
                     firms$BUSINESS_STATE=="Texas"] = "Blue_Cross and Blue Shield of Texas"

firms$COMPANY_NAME[firms$GROUP_AFFILIATION=="Wellmark Group"&
                     firms$BUSINESS_STATE=="Iowa"] = "Wellmark Blue Cross Blue Shield of Iowa"
firms$COMPANY_NAME[firms$GROUP_AFFILIATION=="Wellmark Group"&
                     firms$BUSINESS_STATE=="Iowa"] = "Wellmark Blue Cross Blue Shield of Iowa"
firms$COMPANY_NAME[firms$GROUP_AFFILIATION=="BCBS OF KC GRP"] = "Blue Cross and Blue Shield of Kansas City"
firms$COMPANY_NAME[firms$GROUP_AFFILIATION=="CAREFIRST INC GRP"&
                     firms$BUSINESS_STATE=="Maryland"] = "CareFirst BlueCross BlueShield"
firms$COMPANY_NAME[firms$GROUP_AFFILIATION=="BCBS OF MI GRP"&
                     firms$BUSINESS_STATE=="Michigan"] = "Blue Cross Blue Shield of Michigan"
firms$COMPANY_NAME[firms$GROUP_AFFILIATION=="HIGHMARK GRP"&
                     firms$BUSINESS_STATE=="Pennsylvania"] = "Blue Cross of Northeastern Pennsylvania"
firms$COMPANY_NAME[firms$COMPANY_NAME=="Health Alliance Medical Plans, Inc."] = "Health Alliance Medical Plans"
firms$COMPANY_NAME[firms$COMPANY_NAME=="IlliniCare Health Plan, Inc."] = "IlliniCare Health"
firms$COMPANY_NAME[firms$COMPANY_NAME=="Health Alliance Plan of Michigan"] = "HAP"
firms$COMPANY_NAME[firms$GROUP_AFFILIATION=="Healthplus of MI Grp"] = "HealthPlus Insurance Company"
firms$COMPANY_NAME[firms$GROUP_AFFILIATION=="Spectrum Hlth GRP"] = "Priority Health"
firms$COMPANY_NAME[firms$COMPANY_NAME=="Presbyterian Health Plan, Inc."] = "Presbyterian Health Plan Inc"
firms$COMPANY_NAME[firms$GROUP_AFFILIATION=="Independence Health Group, Inc."] = "Independence Blue Cross"
firms$COMPANY_NAME[firms$GROUP_AFFILIATION=="GEISINGER INS GRP"] = "Geisinger Choice"
firms$COMPANY_NAME[firms$GROUP_AFFILIATION=="Firstcare Grp"] = "Firstcare Health Plans"
firms$COMPANY_NAME[firms$GROUP_AFFILIATION=="Scott & White GRP"] = "Insurance Company of Scott & White"
firms$COMPANY_NAME[firms$GROUP_AFFILIATION=="AETNA GRP"&
                     firms$BUSINESS_STATE=="Utah"] = "Altius"
firms$COMPANY_NAME[firms$COMPANY_NAME=="SelectHealth, Inc."] = "SelectHealth"


#### Choice Sets ####
shares = read.csv("Intermediate_Output/marketDataMap.csv")
data(states)
shares = merge(shares,states[,c("state","name")],by.x="STATE",by.y="state")
shares = summaryBy(Share~name+Firm+Market,data=shares,FUN=sum,keep.names=TRUE)
shares = summaryBy(Share~name+Firm,data=shares,FUN=mean,keep.names=TRUE)
shares$Share = shares$Share/ave(shares$Share,shares$name,FUN=sum)

##### Individual Market Shares #####
claims = read.csv("Data/2015_MLR/Part1_2_Summary_Data_Premium_Claims.csv")

claims = claims[claims$ROW_LOOKUP_CODE=="NUMBER_OF_LIFE_YEARS",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_YEARLY","CMM_INDIVIDUAL_Q1")]
claims = claims[!is.na(claims$CMM_INDIVIDUAL_Q1),]

indMarket = merge(claims,firms,by="ï..MR_SUBMISSION_TEMPLATE_ID")
# indMarket = summaryBy(CMM_INDIVIDUAL_YEARLY+CMM_INDIVIDUAL_Q1~BUSINESS_STATE+GROUP_AFFILIATION+COMPANY_NAME,
#                       data=indMarket,FUN=sum,keep.names=TRUE)
indMarket = indMarket[indMarket$CMM_INDIVIDUAL_Q1>0,]
indMarket$TOTAL_LIVES = ave(indMarket$CMM_INDIVIDUAL_Q1,indMarket$BUSINESS_STATE,FUN=sum)
indMarket$MARKET_SHARE = with(indMarket,CMM_INDIVIDUAL_Q1/TOTAL_LIVES)
indMarket = indMarket[with(indMarket,order(BUSINESS_STATE,-MARKET_SHARE)),]
write.csv(indMarket,"Intermediate_Output/MLR_Shares/MLR_Claims_Shares.csv",row.names=FALSE)

test = indMarket[indMarket$BUSINESS_STATE%in%shares$name,]
test$COMPANY_NAME = gsub(" ","_",test$COMPANY_NAME)
choices$COMPANY_NAME = gsub("[,.&'-:]","",choices$COMPANY_NAME)
test = merge(test,shares,by.x=c("BUSINESS_STATE","COMPANY_NAME"),
             by.y=c("name","Firm"),all=TRUE)

write.csv(test[,c("BUSINESS_STATE","COMPANY_NAME","GROUP_AFFILIATION","DBA_MARKETING_NAME","MARKET_SHARE","Share")],
          "firmNamesTest.csv",row.names=FALSE)



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
