rm(list=ls())
library(ggplot2)
library(scales)
library(extrafont)
library(grid)
library(doBy)
setwd("C:/Users/Conor/Documents/Research/Imperfect Insurance Competition")

#Hix Plans
#Read in plan description data
hixData = read.csv("Data/2015_Premiums/2015_QHP_Landscape_Individual_Market_Medical.csv")

NM = read.csv("Data/2015_Premiums/2015_QHP_Landscape_NM_Individual_Market_Medical.csv")
NV = read.csv("Data/2015_Premiums/2015_QHP_Landscape_NV_Individual_Market_Medical.csv")
OR= read.csv("Data/2015_Premiums/2015_QHP_Landscape_OR_Individual_Market_Medical.csv")

hixData = rbind(hixData,NM)
hixData = rbind(hixData,NV)
hixData = rbind(hixData,OR)

rm(NM,NV,OR)

#Drop Child-Only Plans
hixData = hixData[hixData$Child.Only.Offering=="Allows Adult and Child-Only",]

#Fix Issuer Names
hixData$Issuer.Name = as.character(hixData$Issuer.Name)

hixData$Issuer.Name = as.character(hixData$Issuer.Name)
hixData$Issuer.Name[grep("Aetna",hixData$Issuer.Name)] = "Aetna"
hixData$Issuer.Name[grep("Humana",hixData$Issuer.Name)] = "Humana"
hixData$Issuer.Name[grep("Anthem",hixData$Issuer.Name)] = "Anthem"
hixData$Issuer.Name[grep("Highmark",hixData$Issuer.Name)] = "Highmark"
hixData$Issuer.Name[grep("Coventry",hixData$Issuer.Name)] = "Coventry"
hixData$Issuer.Name[grep("Ambetter",hixData$Issuer.Name)] = "Ambetter"
hixData$Issuer.Name[grep("Cigna",hixData$Issuer.Name)] = "Cigna"
hixData$Issuer.Name[grep("Kaiser",hixData$Issuer.Name)] = "Kaiser"
hixData$Issuer.Name[grep("Moda",hixData$Issuer.Name)] = "Moda"
hixData$Issuer.Name[grep("Molina",hixData$Issuer.Name)] = "Molina"
hixData$Issuer.Name[grep("Health Net",hixData$Issuer.Name)] = "Health Net"
hixData$Issuer.Name[grep("QualChoice",hixData$Issuer.Name)] = "QualChoice"
hixData$Issuer.Name[grep("Rocky Mountain",hixData$Issuer.Name)] = "Rocky Mountain"
hixData$Issuer.Name[grep("Land of Lincoln",hixData$Issuer.Name)] = "Land of Lincoln"
hixData$Issuer.Name[grep("HealthPlus",hixData$Issuer.Name)] = "HealthPlus"
hixData$Issuer.Name[grep("HealthSpan",hixData$Issuer.Name)] = "HealthSpan"
hixData$Issuer.Name[grep("Health Republic",hixData$Issuer.Name)] = "Health Republic"
hixData$Issuer.Name[grep("BridgeSpan",hixData$Issuer.Name)] = "BridgeSpan"
hixData$Issuer.Name[grep("Geisinger",hixData$Issuer.Name)] = "Geisinger"
hixData$Issuer.Name[grep("Oscar",hixData$Issuer.Name)] = "Oscar"
hixData$Issuer.Name[grep("(UnitedHealth)|(United Health)",hixData$Issuer.Name)] = "UnitedHealthcare"
hixData$Issuer.Name[grep("Blue",hixData$Issuer.Name)] = "Blue Cross Blue Shield"

hixData = unique(hixData[,c("State","Issuer.Name")])


#Read in plan description data
planData = read.csv("Data/2015_Premiums/2015_RWJF.csv")
planData$CARRIER = as.character(planData$CARRIER)

# Standardize Plan Names
planData$CARRIER[grep("Cigna",planData$CARRIER)] = "Cigna"
planData$CARRIER[grep("Health Net",planData$CARRIER)] = "Health Net"
planData$CARRIER[grep("Aetna",planData$CARRIER)] = "Aetna"
planData$CARRIER[grep("Anthem",planData$CARRIER)] = "Anthem"
planData$CARRIER[grep("Humana",planData$CARRIER)] = "Humana"
planData$CARRIER[grep("Coventry",planData$CARRIER)] = "Coventry"
planData$CARRIER[grep("Ambetter",planData$CARRIER)] = "Ambetter"
planData$CARRIER[grep("Kaiser",planData$CARRIER)] = "Kaiser"
planData$CARRIER[grep("Moda",planData$CARRIER)] = "Moda"
planData$CARRIER[grep("Molina",planData$CARRIER)] = "Molina"
planData$CARRIER[grep("Health Republic",planData$CARRIER)] = "Health Republic"
planData$CARRIER[grep("Oscar",planData$CARRIER)] = "Oscar"
planData$CARRIER[grep("UnitedHealth",planData$CARRIER)] = "UnitedHealthcare"
planData$CARRIER[grep("Capital BlueCross",planData$CARRIER)] = "Capital BlueCross"

# Match to Truncated Zipcode
planData$AREA = gsub("([A-Z]+)([0-9]+)","\\2",planData$AREA,perl=TRUE)
planData$AREA = paste("Rating Area",as.numeric(planData$AREA))
mapping = read.csv("Zip_RatingArea/Zip3_to_RatingArea.csv")



planData = merge(planData,mapping,by.x=c("ST","AREA"),by.y=c("ST","RatingArea"),all.x=TRUE)
planData = planData[!is.na(planData$Zip3),] # Drop NJ Small Group Areas

planData = unique(planData[,c("ST","AREA","CARRIER","Zip3","alloc")])


eHealth = read.csv("C:/Users/Conor/Documents/Research/eHealth Data/eHealth_2015.csv",stringsAsFactors = FALSE)

# Merge in Default and State-Specific Age Rating Curves

# Include Family only....
#eHealth = eHealth[eHealth$FAMILY_OR_INDIVIDUAL=="FAMILY",]
#eHealth = eHealth[eHealth$PRODUCTLINE_TYPE=="QHP",]
eHealth = eHealth[with(eHealth,!is.na(TRUNCATED_ZIP)&TRUNCATED_ZIP!=0 ),]
eHealth = eHealth[eHealth$PLAN_NAME!="UNKNOWN PLAN - this row is used for referential integrity - DSTOCK",]

#eHealth = unique(eHealth[,c("STATE","CARRIER_NAME","PLAN_NAME","PLAN_TYPE","PRODUCTLINE_TYPE","PLAN_METAL_LEVEL","ANNUAL_OOP_LIMIT","DEDUCTIBLE")])
#eHealth = eHealth[eHealth$STATE%in%FFM,]
#planData$Metal = toupper(planData$Metal)

eHealth$eHealthID = 1:nrow(eHealth)


# Standardize Names
eHealth$CARRIER_NAME[grep("Cigna",eHealth$CARRIER_NAME)] = "Cigna"
eHealth$CARRIER_NAME[grep("Health Net",eHealth$CARRIER_NAME)] = "Health Net"
eHealth$CARRIER_NAME[grep("Aetna",eHealth$CARRIER_NAME)] = "Aetna"
eHealth$CARRIER_NAME[grep("Anthem",eHealth$CARRIER_NAME)] = "Anthem"
eHealth$CARRIER_NAME[grep("Humana",eHealth$CARRIER_NAME)] = "Humana"
eHealth$CARRIER_NAME[grep("Coventry",eHealth$CARRIER_NAME)] = "Coventry"
eHealth$CARRIER_NAME[grep("Ambetter",eHealth$CARRIER_NAME)] = "Ambetter"
eHealth$CARRIER_NAME[grep("Kaiser",eHealth$CARRIER_NAME)] = "Kaiser"
eHealth$CARRIER_NAME[grep("Moda",eHealth$CARRIER_NAME)] = "Moda"
eHealth$CARRIER_NAME[grep("Molina",eHealth$CARRIER_NAME)] = "Molina"
eHealth$CARRIER_NAME[grep("Health Republic",eHealth$CARRIER_NAME)] = "Health Republic"
eHealth$CARRIER_NAME[grep("Oscar",eHealth$CARRIER_NAME)] = "Oscar"
eHealth$CARRIER_NAME[grep("UnitedHealth",eHealth$CARRIER_NAME)] = "UnitedHealthcare"


eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Blue Cross Blue Shield of Georgia"] = "Anthem"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Wellmark Blue Cross and Blue Shield of Iowa"] = "Wellmark Blue Cross Blue Shield of Iowa"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Blue Cross and Blue Shield of Minnesota"] = "Blue Plus"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Blue Cross and Blue Shield of MN"] = "Blue Plus"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="BlueCross BlueShield of Nebraska"] = "Blue Cross and Blue Shield of Nebraska"
eHealth$CARRIER_NAME[grep("Blue Cross of Northeastern Penn",eHealth$CARRIER_NAME)] = "Blue Cross of Northeastern Pennsylvania"


eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Land of Lincoln Health"] = "Land of Lincoln Mutual Health Insurance Company"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="HealthPlus"] = "HealthPlus Insurance Company"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="BridgeSpan"] = "BridgeSpan Health Company"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Geisinger Health Plan"] = "Geisinger Choice"



eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Arkansas Health and Wellness"] = "Ambetter"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="QCA Health Plan Inc"] = "QualChoice"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="ConnectiCare Inc."] = "ConnectiCare"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="PacificSource Health Plans of Idaho"] = "PacificSource Health Plans"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Ambetter" & eHealth$STATE=="IL"] = "IlliniCare Health"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="My Health Alliance"] = "Health Alliance Medical Plans"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="All Savers Insurance Company"] = "UnitedHealthcare"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Physicians Health Plan of Northern Indiana, Inc."] = "PHP"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Health Alliance Plan" & eHealth$STATE == "MI"] = "HAP"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Medica of Minnesota"] = "UnitedHealthcare"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="AmeriHealth - New Jersey"] = "AmeriHealth"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Oxford NJ"] = "UnitedHealthcare"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Presbyterian"] = "Presbyterian Health Plan, Inc."
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Health Plan of Nevada"] = "UnitedHealthcare"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Sierra Health and Life"] = "UnitedHealthcare"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Health Republic Insurance of New York"] = "Health Republic"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="MVP"] = "MVP Health Plans"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="SummaCare Inc of Ohio"] = "SummaCare"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="HealthAmerica Pennsylvania"] = "Coventry"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="FirstCare"] = "Firstcare Health Plans"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Scott & White Health Plan"] = "Insurance Company of Scott & White"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Altius Health Plans"] = "Altius"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Anthem" & eHealth$STATE=="VA"] = "HealthKeepers, Inc."
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Innovation Health"] = "Innovation Health Insurance Company"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Optima Health Plan"] = "Optima Health"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="LifeWise Health Plan of Washington"] = "LifeWise Health Plan of WA"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Dean Health Plan, Inc."] = "Dean Health Plan"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Medica" & eHealth$STATE == "WI"] = "UnitedHealthcare"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Physicians Plus"] = "Physicians Plus Insurance Corporation"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Security Health Plan"] = "Security Health Plan of Wisconsin, Inc."
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Prevea360 Health Plan"] = "Dean Health Plan"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="WPS Health Insurance"] = "Wisconsin Physicians Svc Insurance Corp"


# Only match metal level and issuer
eHealth$count = 1
eHealth = summaryBy(count~STATE+CARRIER_NAME + TRUNCATED_ZIP,FUN=sum,data=eHealth,na.rm=TRUE,keep.names=TRUE)



planData$check1 = 1
eHealth$check2 = 1
hixData$check3 = 1

# Keep States w/ relatively complete markets
STselection  = c("AK","AR","CA","CT","DE","GA","IL","IA","KS","MD","MI","MN","MO","NE","NJ","NM","OK","OR","PA","TX","UT","VA","WV")
sum(eHealth$count[eHealth$STATE%in%STselection])
sum(eHealth$count[eHealth$STATE%in%STselection])/sum(eHealth$count)

# Drop certain counties in CA and PA. 



# First Stage
test= merge(eHealth[eHealth$STATE%in%STselection,],planData[planData$ST%in%STselection,],
            by.x=c("STATE","CARRIER_NAME","TRUNCATED_ZIP"),by.y=c("ST","CARRIER","Zip3"),all.x=TRUE)

# Check in HIX data
# test= merge(test,hixData,by.x=c("STATE","CARRIER_NAME"),by.y=c("State","Issuer.Name"),all=TRUE)

# AL - no blue cross ~ 86%
# AK - No Assurant Health (Not in HIX) ~ 6%
# AZ - Compass DBA Meritus, no UHC ~ 13%
# AR - Missings are small
# CA - No Assurant ~2%
# CO - No CO Helath Coop ~ 23%
# CT - No Assurant ~ 2%
# DE - No UHC (Not in HIX) ~ 1%
# DC - Absent
# FL - No blue cross ~ 28%
# GA - Missings are small
# HI - No HMSA ~ 57%
# ID - No Montana COOP ~ 13%
# IL - Missings are small
# IN - No Caresourse, MDwise ~ 14%
# IA - No UHC, Assurant (Not in HIX) ~ 5%
# KS - No Assurant  (Not in HIX) ~ 1%
# KY - No KY Health COOP ~ 32%
# LA - No Assurant (Not in HIX), LA COOP (Not in HIX), HMO...? >10%
# ME - No Main COOP ~ 71%
# MD - No UHC (Not in HIX), Evergreen (Not in HIX) ~ 4%
# MA - No everything basically
# MI - A few missings ~ 4%
# MN - No UCare, Assurant ~ 5%
# MS - No Blue Cross (Not in HIX) ~ 44%
# MO - No Cox (Not in HIX), Assurant (Not in HIX) ~ 5%
# MT - No MT COOP ~ 23%
# NE - No COOP (Not in HIX) ~ 3%
# NV - No NV COOP ~ 13%
# NH - No Minuteman ~ 12%
# NJ - No Amer Intl Group (Not in HIX) ~ 2%
# NM - Missings are small
# NY - Confusing...
# NC - No Blue Cross ~ 75%
# ND - No Assurant (Not in HIX), Sanford ~ 13%
# OH - No Caresourse, AultCare, Paramount ~ 14%
# OK - No CommunityCare, Global Health ~ 2%
# OR - No Assurant (Not in HIX) ~2%, or COOP ~ 4% 
# PA - No UPMC (Not in HIX) ~ 8%, Missing Highmark in some areas
# RI - Absent
# SC - No Consumers Choice ~ 27%
# SD - No Dakota Care ~ 17%
# TN - No TRH, Community Health (Neither in HIX) ~ 24%
# TX - No Community Health Choice, First Care ~ 4%
# UT - Missings are small
# VT - Absent
# VA - No Assurant (Not in HIX) ~ 1%
# WA - No Group Health Cooperative ~ 11%
# WV - No UHC, Assurant (Neither in HIX) ~ 7% 
# WI - No Common Ground ~13%
# WY - No Assurant, UHC (Neither in HIX) ~ 13%


