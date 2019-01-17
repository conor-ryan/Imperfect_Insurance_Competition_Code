rm(list = ls())
library(data.table)
library(Matrix)
library(doBy)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

#### Load Data####
choiceData = read.csv("Intermediate_Output/Simulated_BaseData/simchoiceData_discrete.csv")
choiceData = as.data.table(choiceData)
choiceData[,Metal_std:=gsub(" [0-9]+","",METAL)]
# n_draws = nrow(draws)
#### Read in Total Claims Data ####
MLR_Data = read.csv("Data/2015_MLR/Part1_2_Summary_Data_Premium_Claims.csv")


claims = MLR_Data[MLR_Data$ROW_LOOKUP_CODE=="TOTAL_INCURRED_CLAIMS_PT2",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_Q1")]
names(claims) = c("ï..MR_SUBMISSION_TEMPLATE_ID","Claims")

enroll =MLR_Data[MLR_Data$ROW_LOOKUP_CODE=="MEMBER_MONTHS",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_Q1")]
names(enroll) = c("ï..MR_SUBMISSION_TEMPLATE_ID","MLR_lives")

claims = merge(claims,enroll,by="ï..MR_SUBMISSION_TEMPLATE_ID")

# Remove non-Individual Market Insurers
claims$absent1 = is.na(claims$MLR_lives) | claims$MLR_lives==0
claims = claims[!claims$absent1,c("ï..MR_SUBMISSION_TEMPLATE_ID","Claims","MLR_lives")]

# Merge in and summarize by Firm Name
crosswalk = read.csv("Intermediate_Output/FirmCrosswalk.csv")
crosswalk = unique(crosswalk[,c("ï..MR_SUBMISSION_TEMPLATE_ID","Firm","STATE")])

claims = merge(claims,crosswalk[,c("ï..MR_SUBMISSION_TEMPLATE_ID","Firm","STATE")],by="ï..MR_SUBMISSION_TEMPLATE_ID")
claims = summaryBy(MLR_lives+Claims~Firm+STATE,data=claims,FUN=sum,na.rm=TRUE,keep.names=TRUE)

claims$AvgCost = with(claims,Claims/MLR_lives)

firmData = unique(choiceData[,c("ST","Firm")])

firmClaims = merge(firmData,claims,by.x=c("ST","Firm"),by.y=c("STATE","Firm"),all.x=TRUE)
firmClaims[,logAvgCost:=log(AvgCost)]
setkey(firmClaims,ST,Firm)

#### Filings Claims Data ####
metalClaims = as.data.table(read.csv("Intermediate_Output/Average_Claims/firmClaims.csv"))
metalData = unique(choiceData[,c("ST","Firm","Metal_std")])
metalClaims = merge(metalData,metalClaims[,c("STATE","Firm","METAL","EXP_INC_CLM_PMPM")],by.x=c("ST","Firm","Metal_std"),
                    by.y=c("STATE","Firm","METAL"),all.x=TRUE)

metalClaims[EXP_INC_CLM_PMPM==0,EXP_INC_CLM_PMPM:=NA]
metalClaims[,logAvgCost:=log(EXP_INC_CLM_PMPM)]
setkey(metalClaims,ST,Firm,Metal_std)

## Drop Claims for firms that only have one purchased product in the state
metalClaims[ST=="IL"&Firm=="ASSURANT_HEALTH",logAvgCost:=NA]
metalClaims[ST=="NE"&Firm=="ASSURANT_HEALTH",logAvgCost:=NA]
metalClaims[ST=="IA"&Firm=="AVERA_HEALTH_PLANS",logAvgCost:=NA]


#### MEPS Age Moments ####
ageMoments = as.data.table(read.csv("Intermediate_Output/MEPS_Moments/ageMoments.csv"))
setkey(ageMoments,Age_Bin)

choiceData[,Age_Bin:=floor(AGE/5)*5]
choiceData[AGE>=18 & Age_Bin==15,Age_Bin:=20]
choiceData[,Age_1:=AGE/10]
choiceData[,Age_2:=Age_1^2]

for (a in unique(choiceData$Age_Bin)){
  var = paste("Bin",a,sep="_")
  choiceData[,c(var):=0]
  choiceData[Age_Bin==a,c(var):=1]
}

#### MEPS Risk Score Moments ####
riskMoments = as.data.table(read.csv("Intermediate_Output/MEPS_Moments/riskMoments.csv"))
setkey(riskMoments,HCC_positive)



#### All Moments ####
firmClaims$M_num = 1:nrow(firmClaims)
metalClaims$M_num[!is.na(metalClaims$logAvgCost)] = max(firmClaims$M_num) + 1:sum(!is.na(metalClaims$logAvgCost))
ageMoments$M_num = 1:nrow(ageMoments)
riskMoments$M_num = 1:nrow(riskMoments)

#### Create Moment Index DF ####
setkey(choiceData,Person,Product)
choiceData[,index:=1:nrow(choiceData)]
firmDict = merge(firmClaims,choiceData,by=c("ST","Firm"))
metalDict = merge(metalClaims,choiceData,by=c("ST","Firm","Metal_std"))
avgMoments = rbind(firmDict[,c("logAvgCost","M_num","Product","index")],metalDict[,c("logAvgCost","M_num","Product","index")])
avgMoments = avgMoments[!is.na(M_num),]

ageMoments = merge(ageMoments,choiceData,by=c("Age_Bin"))
ageMoments = ageMoments[,c("costIndex","M_num","index")]

write.csv(ageMoments,file="Intermediate_Output/MC_Moments/ageMoments.csv",row.names=FALSE)
write.csv(avgMoments,file="Intermediate_Output/MC_Moments/avgMoments.csv",row.names=FALSE)
write.csv(riskMoments,file="Intermediate_Output/MC_Moments/riskMoments.csv",row.names=FALSE)
write.csv(choiceData,"Simulation_Risk_Output/simchoiceData_discrete.csv",row.names=FALSE)

remove_Vars = ls()[!grepl("(full_predict|metalDict)",ls())]

rm(list = remove_Vars)
gc()

#### Starting Vector ####
run = "2018-08-25"
simFile = paste("Simulation_Risk_Output/simData_",run,".rData",sep="")
load(simFile)
full_predict[,Age_1:=AGE/10]

full_predict = full_predict[,c("ST","Firm","Product","Age_1","s_pred","PERWT","HCC_Silver","AV_std")]
metalDict = unique(metalDict[,c("Product","logAvgCost","M_num")])
rm(acs,draws)
gc()

prod_Avgs = merge(full_predict,metalDict[,c("Product","logAvgCost","M_num")],by="Product",allow.cartesian = TRUE)
rm(full_predict,metalDict)
gc()

prod_Avgs = prod_Avgs[,list(Age = sum(10*Age_1*s_pred*PERWT)/sum(s_pred*PERWT),
                            HCC = sum(HCC_Silver*s_pred*PERWT)/sum(s_pred*PERWT),
                            AV = sum(AV_std*s_pred*PERWT)/sum(s_pred*PERWT)),
                      by=c("ST","Firm","M_num","logAvgCost")]

res = lm(logAvgCost~-1+Age+AV+HCC+ ST ,data=prod_Avgs)
phi_start = res$coefficients
write.csv(data.frame(par_start=phi_start),"Intermediate_Output/MC_Moments/linregpars.csv",row.names=FALSE)

