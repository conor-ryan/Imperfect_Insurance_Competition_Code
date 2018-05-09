rm(list = ls())
library(doBy)
library(noncensus)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")
##### Firm IDs  ####
firms = read.csv("Data/2015_MLR/MR_Submission_Template_Header.csv")
firms = firms[,c("ï..MR_SUBMISSION_TEMPLATE_ID","BUSINESS_STATE","GROUP_AFFILIATION","COMPANY_NAME","DBA_MARKETING_NAME")]

firms$Firm = as.character(firms$DBA_MARKETING_NAME)
firms$Firm[grepl("N/A",firms$DBA_MARKETING_NAME)] = paste(firms$COMPANY_NAME[grepl("N/A",firms$DBA_MARKETING_NAME)],"No DBA Name")

firms$Firm[firms$DBA_MARKETING_NAME=="United Healthcare Life Insurance Company"]="UnitedHealthcare Life Ins. Co."
firms$Firm[firms$DBA_MARKETING_NAME=="Premera Blue Cross"&firms$BUSINESS_STATE=="Alaska"]="Premera Blue Cross Blue Shield of Alaska"
firms$Firm[grepl("Aetna",firms$DBA_MARKETING_NAME)&firms$DBA_MARKETING_NAME!="Aetna Life Insurance Company"] = "Aetna"
firms$Firm[grepl("Aetna",firms$DBA_MARKETING_NAME)&firms$BUSINESS_STATE=="Connecticut"] = "Aetna"
firms$Firm[grepl("Aetna",firms$DBA_MARKETING_NAME)&firms$BUSINESS_STATE=="Michigan"] = "Aetna"
firms$Firm[grepl("Aetna",firms$DBA_MARKETING_NAME)&firms$BUSINESS_STATE=="Virginia"] = "Aetna"
firms$Firm[grepl("Aetna",firms$DBA_MARKETING_NAME)&firms$BUSINESS_STATE=="Deleware"] = "Aetna"
firms$Firm[grepl("Aetna",firms$DBA_MARKETING_NAME)&firms$BUSINESS_STATE=="Iowa"] = "Coventry Health Care of Iowa, Inc."
firms$Firm[firms$DBA_MARKETING_NAME=="Aetna Health of Utah Inc."&firms$BUSINESS_STATE=="Utah"] = "Altius Health Plans"
firms$Firm[firms$GROUP_AFFILIATION=="Assurant Inc Grp"] = "Assurant Health"
firms$Firm[firms$DBA_MARKETING_NAME=="Blue Cross and Blue Shield of Arizona, Inc."]="Blue Cross Blue Shield of Arizona"
firms$Firm[grepl("COMPASS",firms$DBA_MARKETING_NAME)]="Meritus"
firms$Firm[firms$DBA_MARKETING_NAME=="Health Net of Arizona, Inc"]="Health Net of Arizona"
firms$Firm[firms$DBA_MARKETING_NAME=="Health Net of California, Inc"]="Health Net"
firms$Firm[firms$DBA_MARKETING_NAME=="Arkansas Blue Cross Blue Shield"]="Arkansas Blue Cross and Blue Shield"
firms$Firm[firms$GROUP_AFFILIATION=="CENTENE CORP GRP"&firms$BUSINESS_STATE=="Arkansas"]="Arkansas Health and Wellness"
firms$Firm[firms$GROUP_AFFILIATION=="CENTENE CORP GRP"&firms$BUSINESS_STATE=="Florida"]="Ambetter from Sunshine Health"
firms$Firm[firms$DBA_MARKETING_NAME=="Peach State Health Plan"]="Ambetter from Peach State Health Plan"
firms$Firm[firms$DBA_MARKETING_NAME=="IlliniCare Health Plan, Inc."&firms$BUSINESS_STATE=="Illinois"]="Ambetter Insured by Celtic"
firms$Firm[firms$GROUP_AFFILIATION=="CENTENE CORP GRP"&firms$BUSINESS_STATE=="Indiana"]="Ambetter from MHS"
firms$Firm[firms$GROUP_AFFILIATION=="CENTENE CORP GRP"&firms$BUSINESS_STATE=="Mississippi"]="Ambetter from Magnolia Health"
firms$Firm[firms$GROUP_AFFILIATION=="CENTENE CORP GRP"&firms$BUSINESS_STATE=="Ohio"]="Ambetter from Buckeye Health"
firms$Firm[firms$GROUP_AFFILIATION=="CENTENE CORP GRP"&firms$BUSINESS_STATE=="Oregon"]="Health Net of Oregon"
firms$Firm[firms$GROUP_AFFILIATION=="CENTENE CORP GRP"&firms$BUSINESS_STATE=="Texas"]="Ambetter from Superior HealthPlan"
firms$Firm[firms$GROUP_AFFILIATION=="CENTENE CORP GRP"&firms$BUSINESS_STATE=="Wisconsin"]="Ambetter from MHS Health WI"
firms$Firm[firms$COMPANY_NAME=="QCA Health Plan, Inc."]="QCA Health Plan Inc"
firms$Firm[firms$DBA_MARKETING_NAME=="Kaiser Permanente"&firms$BUSINESS_STATE=="California"]="Kaiser Permanente of CA"
firms$Firm[firms$DBA_MARKETING_NAME=="Kaiser Permanente"&firms$BUSINESS_STATE=="Colorado"]="Kaiser Permanente CO"
firms$Firm[firms$DBA_MARKETING_NAME=="Kaiser Foundation Health Plan of Georgia, Inc."]="Kaiser Permanente GA"
firms$Firm[firms$DBA_MARKETING_NAME=="Kaiser Permanente"&firms$BUSINESS_STATE=="Hawaii"]="Kaiser Permanente of HI"
firms$Firm[firms$DBA_MARKETING_NAME=="Kaiser Permanente"&firms$BUSINESS_STATE=="Oregon"]="Kaiser Foundation Health Plan of the NW"
firms$Firm[firms$DBA_MARKETING_NAME=="Kaiser Foundation Health Plan of Mid-Atlantic States, Inc."]="Kaiser Mid-Atlantic"
firms$Firm[firms$DBA_MARKETING_NAME=="Anthem Health Plans, Inc."&firms$BUSINESS_STATE=="Connecticut"]="Anthem Blue Cross and Blue Shield of CT"
firms$Firm[firms$DBA_MARKETING_NAME=="ConnectiCare Benefits, Inc."]="ConnectiCare Inc."
firms$Firm[firms$DBA_MARKETING_NAME=="HealthyCT, Inc."]="HealthYCT"
firms$Firm[firms$DBA_MARKETING_NAME=="Highmark BCBSD Inc."]="Highmark Blue Cross Blue Shield Delaware"
firms$Firm[firms$DBA_MARKETING_NAME=="Coventry Health Plan of Fl., Inc."]="Coventry Health Care of Florida, Inc."
firms$Firm[grepl("Molina",firms$DBA_MARKETING_NAME)]="Molina Health Care"
firms$Firm[firms$DBA_MARKETING_NAME=="Blue Cross of Idaho Health Service, Inc."]="Blue Cross of Idaho"
firms$Firm[firms$DBA_MARKETING_NAME=="PacificSource Health Plans"&firms$BUSINESS_STATE=="Idaho"]="PacificSource Health Plans of Idaho"
firms$Firm[firms$DBA_MARKETING_NAME=="Regence BlueShield of Idaho, Inc."]="Regence BlueShield of Idaho"
firms$Firm[firms$COMPANY_NAME=="SelectHealth, Inc."]="SelectHealth"
firms$Firm[firms$GROUP_AFFILIATION=="HCSC GRP"&firms$BUSINESS_STATE=="Illinois"]="Blue Cross and Blue Shield of Illinois"
firms$Firm[firms$GROUP_AFFILIATION=="HCSC GRP"&firms$BUSINESS_STATE=="New Mexico"]="Blue Cross and Blue Shield of New Mexico"
firms$Firm[firms$GROUP_AFFILIATION=="HCSC GRP"&firms$BUSINESS_STATE=="Oklahoma"]="Blue Cross and Blue Shield of Oklahoma"
firms$Firm[firms$GROUP_AFFILIATION=="HCSC GRP"&firms$BUSINESS_STATE=="Texas"]="Blue Cross and Blue Shield of Texas"
firms$Firm[firms$GROUP_AFFILIATION=="HCSC GRP"&firms$BUSINESS_STATE=="Montana"]="BlueCross BlueShield of Montana"
firms$Firm[firms$DBA_MARKETING_NAME=="Land of Lincoln Mutual Health Insurance Company"]="Land of Lincoln Health"
firms$Firm[firms$DBA_MARKETING_NAME=="Health Alliance Medical Plans, Inc."&firms$BUSINESS_STATE=="Illinois"]="My Health Alliance"
firms$Firm[firms$DBA_MARKETING_NAME=="Health Alliance Plan of Michigan"]="Health Alliance Plan"
firms$Firm[firms$DBA_MARKETING_NAME=="Anthem Insurance Companies, Inc."&firms$BUSINESS_STATE=="Indiana"]="Anthem Blue Cross and Blue Shield"
firms$Firm[firms$DBA_MARKETING_NAME=="Anthem Health Plans of Kentucky, Inc."&firms$BUSINESS_STATE=="Kentucky"]="Anthem Blue Cross and Blue Shield"
firms$Firm[firms$DBA_MARKETING_NAME=="Anthem Health Plans of Maine, Inc."]="Anthem BlueCross BlueShield"
firms$Firm[firms$DBA_MARKETING_NAME=="Avera Health Plans, Inc."]="Avera Health Plans"
firms$Firm[grepl("Wellmark Blue Cross and Blue Shield of Iowa",firms$DBA_MARKETING_NAME)] = "Wellmark Blue Cross and Blue Shield of Iowa"
firms$Firm[firms$DBA_MARKETING_NAME=="Blue Cross Blue Shield of Louisiana"]="BlueCross BlueShield of Louisiana"
firms$Firm[firms$DBA_MARKETING_NAME=="Harvard Pilgrim Health Care, Inc."]="Harvard Pilgrim Health Care"
firms$Firm[firms$DBA_MARKETING_NAME=="Harvard Pilgrim Health Care of New England, Inc."]="Harvard Pilgrim Health Care"
firms$Firm[firms$GROUP_AFFILIATION=="CAREFIRST INC GRP"]="CareFirst BlueCross BlueShield"
firms$Firm[grepl("Humana",firms$DBA_MARKETING_NAME)]="Humana"
firms$Firm[firms$DBA_MARKETING_NAME=="Blue Cross Blue Shield of Minnesota"]="Blue Cross and Blue Shield of Minnesota"
firms$Firm[firms$DBA_MARKETING_NAME=="HealthPartners, Inc."]="HealthPartners"
firms$Firm[firms$DBA_MARKETING_NAME=="HealthPartners Insurance Company"]="HealthPartners"
firms$Firm[firms$DBA_MARKETING_NAME=="Coventry Health & Life Insurance Company"]="Coventry Health and Life Insurance Company"
firms$Firm[firms$DBA_MARKETING_NAME=="Healthy Alliance Life Insurance Company"&firms$BUSINESS_STATE=="Missouri"]="Anthem Blue Cross and Blue Shield"
firms$Firm[firms$DBA_MARKETING_NAME=="Blue Cross and Blue Shield of Nebraska"]="BlueCross BlueShield of Nebraska"
firms$Firm[firms$DBA_MARKETING_NAME=="Health Plan of Nevada, Inc."]="Health Plan of Nevada"
firms$Firm[firms$DBA_MARKETING_NAME=="HMO Colorado, Inc., dba HMO Nevada"]="Anthem BlueCross BlueShield"
firms$Firm[firms$DBA_MARKETING_NAME=="Anthem Health Plans of New Hampshire, Inc."]="Anthem Blue Cross and Blue Shield of NH"
firms$Firm[firms$DBA_MARKETING_NAME=="Sierra Health and Life Insurance Company, Inc."]="Sierra Health and Life"
firms$Firm[grepl("AmeriHealth",firms$DBA_MARKETING_NAME)]="AmeriHealth - New Jersey"
firms$Firm[firms$DBA_MARKETING_NAME=="Freelancers CO-OP of New Jersey"]="Health Republic Insurance of New Jersey"
firms$Firm[firms$DBA_MARKETING_NAME=="Horizon Blue Cross Blue Shield of NJ"]="Horizon Blue Cross Blue Shield of New Jersey"
firms$Firm[firms$DBA_MARKETING_NAME=="Horizon HMO NJ"]="Horizon Blue Cross Blue Shield of New Jersey"
firms$Firm[grepl("Oscar",firms$DBA_MARKETING_NAME)]="Oscar"
firms$Firm[grepl("Oxford",firms$DBA_MARKETING_NAME)]="Oxford NJ"
firms$Firm[grepl("Presbyterian",firms$DBA_MARKETING_NAME)]="Presbyterian"
firms$Firm[firms$GROUP_AFFILIATION=="Wellpoint Inc Grp"&firms$BUSINESS_STATE=="New York"]="Empire"
firms$Firm[firms$GROUP_AFFILIATION=="Wellpoint Inc Grp"&firms$BUSINESS_STATE=="Wisconsin"]="Anthem Blue Cross and Blue Shield"
firms$Firm[firms$DBA_MARKETING_NAME=="Freelancers Health Service Corporation"]="Health Republic Insurance of New York"
firms$Firm[grepl("MVP",firms$DBA_MARKETING_NAME)]="MVP"
firms$Firm[firms$DBA_MARKETING_NAME=="Medica Health Plans"&firms$BUSINESS_STATE=="North Dakota"]="Medica"
firms$Firm[firms$DBA_MARKETING_NAME=="BridgeSpan Health Company"] = "BridgeSpan"
firms$Firm[firms$DBA_MARKETING_NAME=="Health Republic Insurance Company"&firms$BUSINESS_STATE=="Oregon"] = "Health Republic Insurance"
firms$Firm[firms$DBA_MARKETING_NAME=="Keystone Health Plan East"] = "Independence Blue Cross"
firms$Firm[firms$DBA_MARKETING_NAME=="Keystone Health Plan Central"] = "Capital BlueCross"
firms$Firm[firms$DBA_MARKETING_NAME=="HealthAmerica"] = "HealthAmerica Pennsylvania"
firms$Firm[firms$DBA_MARKETING_NAME=="BlueCross and BlueShield of South Carolina"] = "BlueCross BlueShield of South Carolina"
firms$Firm[firms$DBA_MARKETING_NAME=="BlueChoice HealthPlan of South Carolina, Inc."] = "BlueChoice HealthPlan"
firms$Firm[firms$DBA_MARKETING_NAME=="BlueCross BlueShield of Tennessee, Inc."] = "BlueCross BlueShield of Tennessee"
firms$Firm[firms$DBA_MARKETING_NAME=="FirstCare Health Plans"] = "FirstCare"
firms$Firm[firms$DBA_MARKETING_NAME=="Scott and White Health Plan"] = "Scott & White Health Plan"
firms$Firm[firms$DBA_MARKETING_NAME=="Arches Mutual Insurance Company"] = "Arches Health Plan"
firms$Firm[firms$DBA_MARKETING_NAME=="Anthem Health Plans of Virginia, Inc."] = "Anthem Blue Cross and Blue Shield of VA"
firms$Firm[firms$DBA_MARKETING_NAME=="Coventry Health Care"&firms$BUSINESS_STATE=="Virginia"] = "Coventry Health Care of Virginia"
firms$Firm[firms$DBA_MARKETING_NAME=="Innovation Health Insurance Company"] = "Innovation Health"
firms$Firm[firms$DBA_MARKETING_NAME=="Medica Health Plans of Wisconsin"&firms$BUSINESS_STATE=="Wisconsin"] = "Medica"
firms$Firm[firms$DBA_MARKETING_NAME=="Physicians Plus Insurance Corporation"] = "Physicians Plus"
firms$Firm[firms$DBA_MARKETING_NAME=="Security Health Plan of Wisconsin, Inc."] = "Security Health Plan"
firms$Firm[firms$DBA_MARKETING_NAME=="Blue Cross Blue Shield Healthcare Plan of Georgia, Inc."] = "Blue Cross Blue Shield of Georgia"
firms$Firm[firms$GROUP_AFFILIATION=="BCBS OF MI GRP"] = "Blue Cross Blue Shield of Michigan"
firms$Firm[firms$DBA_MARKETING_NAME=="ConnectiCare"] = "ConnectiCare Inc."
firms$Firm[firms$GROUP_AFFILIATION=="Cigna Hlth Grp"] = "Cigna Health and Life Insurance Company"
firms$Firm[firms$DBA_MARKETING_NAME=="HealthKeepers, Inc."]="Anthem Blue Cross and Blue Shield of VA"
firms$Firm[firms$GROUP_AFFILIATION=="Healthplus of MI Grp"]="HealthPlus"

#Merge in some ambiguous firms
firms$Firm[firms$DBA_MARKETING_NAME=="Health Net Life Insurance Company"]="Health Net"
firms$Firm[firms$DBA_MARKETING_NAME=="UnitedHealthcare Insurance Company"&firms$BUSINESS_STATE=="Connecticut"]="UnitedHealthcare Life Ins. Co."
firms$Firm[grepl("Coventry",firms$DBA_MARKETING_NAME)&firms$BUSINESS_STATE=="Illinois"]="Coventry"
firms$Firm[grepl("Coventry",firms$DBA_MARKETING_NAME)&firms$BUSINESS_STATE=="Missouri"]="Coventry"

data(states)
firms = merge(firms,states[,c("state","name")],by.x="BUSINESS_STATE",by.y="name")

firms$Firm = gsub(" ","_",firms$Firm)
firms$Firm = gsub("[,.&'-:]","",firms$Firm)
firms$Firm = toupper(firms$Firm)

# #### eHealth Firms ####
eHealth = read.csv("C:/Users/Conor/Documents/Research/eHealth Data/eHealth_2015.csv")
# Drop "referential integrity" rows
eHealth = eHealth[eHealth$PLAN_NAME!="UNKNOWN PLAN - this row is used for referential integrity - DSTOCK",]

eHealth$Firm = as.character(eHealth$CARRIER_NAME)

#Merge Ambiguous Firms
eHealth$Firm[eHealth$CARRIER_NAME=="Coventry Health Care of Georgia, Inc."] = "Aetna"
eHealth$Firm[grepl("Coventry",eHealth$CARRIER_NAME)&eHealth$STATE=="IL"] = "Coventry"
eHealth$Firm[grepl("Coventry",eHealth$CARRIER_NAME)&eHealth$STATE=="MO"] = "Coventry"


# Drop eHealth observations with NA or 0 zip code
eHealth = eHealth[with(eHealth,!is.na(TRUNCATED_ZIP)&TRUNCATED_ZIP!=0 ),]
eHealth = eHealth[with(eHealth,PLAN_METAL_LEVEL!="N/A"),]

# Unique Firms
eHealth = unique(eHealth[,c("STATE","CARRIER_NAME","Firm")])


eHealth$Firm = gsub(" ","_",eHealth$Firm)
eHealth$Firm = gsub("[,.&'-:]","",eHealth$Firm)
eHealth$Firm = toupper(eHealth$Firm)


#### RWJF - Plan Choice Firms ####
planData = read.csv("Data/2015_Premiums/2015_RWJF.csv")
planData$Firm = as.character(planData$CARRIER)

planData$Firm[grepl("Moda Health",planData$CARRIER)] = "Moda Health Plan, Inc."
planData$Firm[grepl("Cigna",planData$CARRIER)] = "Cigna Health and Life Insurance Company"
planData$Firm[grepl("Humana",planData$CARRIER)] = "Humana"
planData$Firm[grepl("Molina",planData$CARRIER)] = "Molina Health Care"
planData$Firm[planData$CARRIER=="Health Net of California, Inc"] = "Health Net"
planData$Firm[planData$CARRIER=="Kaiser Permanente"&planData$ST=="CA"] = "Kaiser Permanente of CA"
planData$Firm[planData$CARRIER=="Kaiser Permanente"&planData$ST=="OR"] = "Kaiser Foundation Health Plan of the NW"
planData$Firm[planData$CARRIER=="Kaiser Permanente"&planData$ST=="VA"] = "Kaiser Mid-Atlantic"
planData$Firm[planData$CARRIER=="Kaiser Foundation Health Plan of Georgia"] = "Kaiser Permanente GA"
planData$Firm[planData$CARRIER=="Kaiser Foundation Health Plan of the Mid-Atlantic States, Inc."] = "Kaiser Mid-Atlantic"
planData$Firm[planData$CARRIER=="Anthem Health Plans"&planData$ST=="CT"] = "Anthem Blue Cross and Blue Shield of CT"
planData$Firm[planData$CARRIER=="ConnectiCare"] = "ConnectiCare Inc."
planData$Firm[planData$CARRIER=="UnitedHealthcare"&planData$ST=="CT"] = "UnitedHealthcare Life Ins. Co."
planData$Firm[planData$CARRIER=="Anthem Blue Cross and Blue Shield"&planData$ST=="GA"] = "Blue Cross Blue Shield of Georgia"
planData$Firm[planData$CARRIER=="UnitedHealthcare"&planData$ST=="GA"&grepl("UnitedHealthcare",planData$PLANNAME)] = "UnitedHealthcare of Georgia, Inc"
planData$Firm[planData$CARRIER=="UnitedHealthcare"&planData$ST=="GA"&!grepl("UnitedHealthcare",planData$PLANNAME)] = "UnitedHealthcare Life Ins. Co."
planData$Firm[planData$CARRIER=="UnitedHealthcare"&planData$ST=="IL"&grepl("UnitedHealthcare",planData$PLANNAME)] = "UnitedHealthcare of the Midwest, Inc"
planData$Firm[planData$CARRIER=="UnitedHealthcare"&planData$ST=="IL"&!grepl("UnitedHealthcare",planData$PLANNAME)] = "UnitedHealthcare Life Ins. Co."
planData$Firm[planData$CARRIER=="UnitedHealthcare"&planData$ST=="MI"&grepl("UnitedHealthcare",planData$PLANNAME)] = "UnitedHealthcare Community Plan Inc"
planData$Firm[planData$CARRIER=="UnitedHealthcare"&planData$ST=="MI"&!grepl("UnitedHealthcare",planData$PLANNAME)] = "UnitedHealthcare Life Ins. Co."
planData$Firm[planData$CARRIER=="UnitedHealthcare"&planData$ST=="MO"&grepl("UnitedHealthcare",planData$PLANNAME)] = "All Savers Insurance Company"
planData$Firm[planData$CARRIER=="UnitedHealthcare"&planData$ST=="MO"&!grepl("UnitedHealthcare",planData$PLANNAME)] = "UnitedHealthcare Life Ins. Co."
planData$Firm[planData$CARRIER=="UnitedHealthcare"&planData$ST=="TX"&grepl("UnitedHealthcare",planData$PLANNAME)] = "All Savers Insurance Company"
planData$Firm[planData$CARRIER=="UnitedHealthcare"&planData$ST=="TX"&!grepl("UnitedHealthcare",planData$PLANNAME)] = "UnitedHealthcare Life Ins. Co."
planData$Firm[planData$CARRIER=="UnitedHealthcare"&planData$ST=="NJ"] = "Oxford NJ"
planData$Firm[planData$CARRIER=="UnitedHealthcare"&planData$ST=="OK"] = "UnitedHealthcare Life Ins. Co."
planData$Firm[planData$CARRIER=="UnitedHealthcare"&planData$ST=="VA"] = "UnitedHealthcare Life Ins. Co."
planData$Firm[planData$CARRIER=="UnitedHealthcare Life Insurance Company"&planData$ST=="NE"] = "UnitedHealthcare Life Ins. Co."
planData$Firm[planData$CARRIER=="UnitedHealthcare Life Insurance Company"&planData$ST=="UT"] = "UnitedHealthcare Life Ins. Co."
planData$Firm[planData$CARRIER=="Wellmark Blue Cross Blue Shield of Iowa"] = "Wellmark Blue Cross and Blue Shield of Iowa"
planData$Firm[planData$CARRIER=="IlliniCare Health"&planData$ST=="IL"] = "Ambetter Insured by Celtic"
planData$Firm[planData$CARRIER=="Coventry Health Care"&planData$ST=="IL"] = "Coventry Health Care of Illinois, Inc."
planData$Firm[planData$CARRIER=="Land of Lincoln Mutual Health Insurance Company"] = "Land of Lincoln Health"
planData$Firm[planData$CARRIER=="HealthPlus Insurance Company"] = "HealthPlus"
planData$Firm[planData$CARRIER=="HAP"] = "Health Alliance Plan"
planData$Firm[planData$CARRIER=="Coventry Health Care"] = "Coventry Health and Life Insurance Company"
planData$Firm[planData$CARRIER=="Blue Cross and Blue Shield of Nebraska"] = "BlueCross BlueShield of Nebraska"
planData$Firm[planData$CARRIER=="AmeriHealth"] = "AmeriHealth - New Jersey"
planData$Firm[planData$CARRIER=="Horizon BCBS New Jersey"] = "Horizon Blue Cross Blue Shield of New Jersey"
planData$Firm[planData$CARRIER=="Presbyterian Health Plan, Inc."] = "Presbyterian"
planData$Firm[planData$CARRIER=="BridgeSpan Health Company"] = "BridgeSpan"
planData$Firm[planData$CARRIER=="Health Net Health Plan of Oregon, Inc."] = "Health Net of Oregon"
planData$Firm[planData$CARRIER=="Health Republic"] = "Health Republic Insurance"
planData$Firm[planData$CARRIER=="Aetna"&planData$ST=="TX"]="Aetna Life Insurance Company"
planData$Firm[planData$CARRIER=="Aetna Life Insurance Company"&planData$ST=="VA"]="Aetna"
planData$Firm[planData$CARRIER=="Ambetter from Superior Health Plan"]="Ambetter from Superior HealthPlan"
planData$Firm[planData$CARRIER=="Firstcare Health Plans"]="FirstCare"
planData$Firm[planData$CARRIER=="Insurance Company of Scott & White"]="Scott & White Health Plan"
planData$Firm[planData$CARRIER=="Coventry Health Care of Virginia, Inc"]="Coventry Health Care of Virginia"
planData$Firm[planData$CARRIER=="Innovation Health Insurance Company"]="Innovation Health"
planData$Firm[planData$CARRIER=="Optima Health"]="Optima Health Plan"
planData$Firm[planData$CARRIER=="Health Alliance Medical Plans"]="My Health Alliance"
planData$Firm[planData$CARRIER=="HealthKeepers, Inc."]="Anthem Blue Cross and Blue Shield of VA"
planData$Firm[planData$CARRIER=="Altius"]="Altius Health Plans"

#Merge Ambiguous Firms
planData$Firm[planData$CARRIER=="Coventry Health Care of Georgia, Inc."] = "Aetna"
planData$Firm[grepl("Coventry",planData$CARRIER)&planData$ST=="IL"] = "Coventry"
planData$Firm[grepl("Coventry",planData$CARRIER)&planData$ST=="MO"] = "Coventry"

planData$Firm = gsub(" ","_",planData$Firm)
planData$Firm = gsub("[,.&'-:]","",planData$Firm)
planData$Firm = toupper(planData$Firm)

planDataFirms = unique(planData[,c("ST","CARRIER","Firm")])
planDataPlans = unique(planData[,c("ST","CARRIER","PLANID","Firm")])

planData$ISSUER_ID = gsub("[A-Z]+.*","",planData$PLANID)
filingMatch = unique(planData[,c("Firm","ISSUER_ID")])

#### Rate Filing Firms ####
filings = read.csv("Data/2016_Rate_Filings/WKSH2_PUF_2016_20161103.csv")

filings = filings[filings$MARKET!="Small Group",c("STATE","COMPANY","ISSUER_ID","EXP_MM")]
filings = summaryBy(EXP_MM~STATE+COMPANY+ISSUER_ID,data=filings,keep.names=TRUE)


filings = merge(filings,filingMatch,by="ISSUER_ID",all.x=TRUE)
filings$Firm=as.character(filings$Firm)
filings$Firm[is.na(filings$Firm)] = as.character(filings$COMPANY[is.na(filings$Firm)])

filings$Firm = gsub(" ","_",filings$Firm)
filings$Firm = gsub("[,.&'-:]","",filings$Firm)
filings$Firm = toupper(filings$Firm)

filings$Firm[with(filings,STATE=="NJ"&grepl("Oxford",COMPANY))] = "OXFORD_NJ"
filings$Firm[with(filings,grepl("Aetna Health Inc.",COMPANY))] = "AETNA"
filings$Firm[with(filings,STATE=="GA"&grepl("Peach ",COMPANY))] = "AMBETTER_FROM_PEACH_STATE_HEALTH_PLAN"
filings$Firm[with(filings,STATE=="IA"&grepl("UnitedHealthcare",COMPANY))] = "UNITEDHEALTHCARE_INSURANCE_COMPANY"
filings$Firm[with(filings,STATE=="TX"&grepl("Celtic Insurance",COMPANY))] = "AMBETTER_FROM_SUPERIOR_HEALTHPLAN"
filings$Firm[with(filings,STATE=="VA"&grepl("Piedmont",COMPANY))] = "PIEDMONT_COMMUNITY_HEALTHCARE_INC"


#filings = unique()


#### Create Firm Crosswalk for Relevant States ####
crosswalk = merge(eHealth,firms,by.x=c("STATE","Firm"),by.y=c("state","Firm"),all=TRUE)
crosswalk = merge(crosswalk,planDataFirms,by.x=c("STATE","Firm"),by.y=c("ST","Firm"),all=TRUE)
crosswalk = merge(crosswalk,filings,by.x=c("STATE","Firm"),by.y=c("STATE","Firm"),all=TRUE)

validStates = c("AK","CA","CT","DE","GA","IL","IA","MD","MI","MO","NE","NJ","NM","ND","OK","OR","TX","UT","VA","WV")

crosswalk = crosswalk[crosswalk$STATE%in%validStates,
                      c("BUSINESS_STATE","STATE","Firm","ï..MR_SUBMISSION_TEMPLATE_ID","ISSUER_ID","GROUP_AFFILIATION","COMPANY_NAME","DBA_MARKETING_NAME","CARRIER_NAME","CARRIER","COMPANY")]
names(crosswalk) = c("BUSINESS_STATE","STATE","Firm","ï..MR_SUBMISSION_TEMPLATE_ID","ISSUER_ID","GROUP_AFFILIATION","COMPANY_NAME","DBA_MARKETING_NAME","eHealth_CARRIER_NAME","RWJF_CARRIER","RF_COMPANY")

write.csv(crosswalk,"Intermediate_Output/FirmCrosswalk.csv",row.names=FALSE)
write.csv(planDataPlans,"Intermediate_Output/RWJF_PlanCrosswalk.csv",row.names=FALSE)





