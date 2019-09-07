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

#### Firm-Share Adjustments ####
## Merge in Sample Market Shares
prod_data = as.data.table(read.csv("Intermediate_Output/Estimation_Data/marketDataMap_discrete.csv"))
prod_data[,Metal_std:=gsub(" .*","",METAL)]
prod_adj = prod_data[,list(lives=sum(lives),
                           AV=sum(lives*AV)/sum(lives)),by=c("STATE","Firm","Metal_std")]
# firm_adj = prod_data[,list(lives=sum(lives),
#                             AV=sum(lives*AV)/sum(lives)),by=c("STATE","Firm")]

firmFilings = as.data.table(read.csv("Intermediate_Output/Average_Claims/firmFilings.csv"))
firmFilings=firmFilings[Year==2015,]
# metalClaims[PRJ_INC_CLM_PMPM<=0,PRJ_INC_CLM_PMPM:=NA]
# metalClaims[EXP_INC_CLM_PMPM<=0,EXP_INC_CLM_PMPM:=NA]
# 
# firm_adj1 = metalClaims[EXP_MM>0,list(expFirmCost=sum(EXP_MM*expAvgCost)/sum(EXP_MM),EXP_MM=sum(EXP_MM)),by=c("STATE","Firm","Year")]
# firm_adj2 = metalClaims[PRJ_MM>0,list(prjFirmCost=sum(PRJ_MM*prjAvgCost)/sum(PRJ_MM),PRJ_MM=sum(PRJ_MM)),by=c("STATE","Firm","Year")]

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
firmClaims = merge(firmClaims,firmFilings,by.x=c("ST","Firm"),by.y=c("STATE","Firm"),all.x=TRUE)

# test = firmClaims[,list(c1=sum(MLR_lives*AvgCost)/sum(MLR_lives),c2=sum(EXP_MM_WK1*expAvgCost)/sum(EXP_MM_WK1),c3=sum(PRJ_MM_WK1*prjAvgCost)/sum(PRJ_MM_WK1)),by="Year"]



# Expecatation Adjustment
# firmClaims[!is.na(expAvgCost)&expAvgCost>5,sum(EXP_MM_WK1*expAvgCost)/sum(EXP_MM_WK1)/(sum(MLR_lives*AvgCost)/sum(MLR_lives))]
prj_adj=firmClaims[!is.na(prjAvgCost),sum(PRJ_MM_WK1*prjAvgCost)/sum(PRJ_MM_WK1)/(sum(MLR_lives*AvgCost)/sum(MLR_lives))]
firmClaims[is.na(prjAvgCost),prjAvgCost:=AvgCost*prj_adj]


firmClaims[,logAvgCost:=log(prjAvgCost)]
firmClaims[,Firm_ST:=paste(Firm,ST,sep="_")]
setkey(firmClaims,Firm_ST)
firmClaims[,Firm_ST:=NULL]
firmClaims$M_num = 1:nrow(firmClaims)


#### Filings Claims Data ####


### Bronze Cost Ratio
load("Intermediate_Output/Average_Claims/allMetalFilings.rData")
metalAvg[METAL=="BRONZE",bronzeCost:=prjAvgCost]
metalAvg[,bronzeCost:=max(bronzeCost,na.rm=TRUE),by=c("STATE","MARKET","COMPANY","Year")]
metalAvg[bronzeCost<=0,bronzeCost:=NA]
metalAvg[!is.na(bronzeCost),costRatio:=prjAvgCost/bronzeCost]
metalAvg[,max_ratio:=max(costRatio),by=c("STATE","MARKET","COMPANY","Year")]
metalAvg = metalAvg[max_ratio>1.05,]

metalAvg[,firmSize:=sum(PRJ_MM),by=c("STATE","MARKET","COMPANY","Year")]
metalAvg = metalAvg[!is.na(costRatio),list(costIndex=sum(costRatio*firmSize)/sum(firmSize)),by=c("METAL","Year")]
metalAvg = metalAvg[METAL!="CATASTROPHIC"&Year=="2016",c("METAL","costIndex")]

# metalAvg[METAL=="BRONZE",bronzeCost:=expAvgCost]
# metalAvg[,bronzeCost:=max(bronzeCost,na.rm=TRUE)]#,by=c("ST","Firm")]
# metalAvg[,costIndex:=expAvgCost/bronzeCost]
# metalAvg[,c("bronzeCost","expAvgCost"):=NULL]
# metalAvg = metalAvg[,list(costIndex=sum(costIndex*PRJ_MM)/sum(PRJ_MM)),by="Metal_std"]
setkey(metalAvg,costIndex)

metalAvg[,M_num:=1:nrow(metalAvg)]

prod_data[,Metal_merge:=Metal_std]
# prod_data[Metal_std=="PLATINUM",Metal_merge:="GOLD"]

metalMoments = merge(metalAvg,prod_data[,c("Product","Metal_merge")],by.x="METAL",by.y="Metal_merge")
metalMoments[,c("METAL"):=NULL]

save(firmClaims,metalAvg,file="Intermediate_Output/Average_Claims/AvgCostMoments.rData")

#### MEPS Age Moments ####
ageMoments = as.data.table(read.csv("Intermediate_Output/MEPS_Moments/ageMoments.csv"))
setkey(ageMoments,Age_Bin)

ageMoments_noHCC = as.data.table(read.csv("Intermediate_Output/MEPS_Moments/ageMoments_noHCC.csv"))
setkey(ageMoments_noHCC,Age_Bin)

choiceData[,Age_Bin:=floor(AvgAge*10/5)*5]
choiceData[AvgAge<2.0,Age_Bin:=20]
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

#### Risk Transfer Moments ####
firmRiskFile = "Simulation_Risk_Output/FirmRiskScores_woSim.rData"
load(firmRiskFile)
firms = unique(prod_data[,c("Firm","STATE","Small")])
firm_RA = merge(firm_RA,firms,by.x=c("Firm","ST"),by.y=c("Firm","STATE"))

RAmom = firm_RA[,list(memberMonths=sum(memberMonths),payments=sum(payments_adj)),by=c("HighRisk")]
RAmom[,avgTransfer:=payments/memberMonths]

RAmom = merge(firm_RA[,c("Firm","ST","HighRisk")],RAmom[,c("HighRisk","avgTransfer")],by="HighRisk")
RAmom = RAmom[HighRisk==1,]
RAmom[,avgTransfer:=avgTransfer/10]
RAmom[,M_num:=1]


#### All Moments ####
# metalClaims$M_num[!is.na(metalClaims$logAvgCost)] = max(firmClaims$M_num) + 1:sum(!is.na(metalClaims$logAvgCost))
ageMoments$M_num = 1:nrow(ageMoments)
ageMoments_noHCC$M_num = 1:nrow(ageMoments_noHCC)
riskMoments$M_num = 1:nrow(riskMoments)

#### Create Moment Index DF ####
setkey(choiceData,Person,Product)
choiceData[,index:=1:nrow(choiceData)]
firmMoments = merge(firmClaims,choiceData[Metal_std!="PLATINUM",],by=c("ST","Firm"))
firmMoments = firmMoments[,c("logAvgCost","M_num","Product","index")]

raMoments = merge(choiceData[Metal_std!="PLATINUM",],RAmom,by=c("ST","Firm"))
raMoments = raMoments[,c("avgTransfer","M_num","Product","index")]


metalMoments = merge(metalMoments,choiceData[,c("Product","index","Firm","ST")],by="Product")
f_merge = unique(choiceData[,c("Firm","ST")])
setkey(f_merge,ST,Firm)
f_merge[,F_M_num:=1:nrow(f_merge)]

metalMoments = merge(metalMoments,f_merge,by=c("Firm","ST"))
metalMoments = metalMoments[,c("M_num","Product","index","F_M_num","costIndex")]
# metalDict = merge(metalClaims,choiceData,by=c("ST","Firm","Metal_std"))
# avgMoments = rbind(firmDict[,c("logAvgCost","M_num","Product","index")],metalDict[,c("logAvgCost","M_num","Product","index")])
# avgMoments = avgMoments[!is.na(M_num),]

ageMoments = merge(ageMoments,choiceData,by=c("Age_Bin"))
ageMoments = ageMoments[,c("costIndex","M_num","index")]

ageMoments_noHCC = merge(ageMoments_noHCC,choiceData,by=c("Age_Bin"))
ageMoments_noHCC = ageMoments_noHCC[,c("costIndex","M_num","index")]

setkey(firmMoments,index)
setkey(metalMoments,index)
setkey(ageMoments,index)
setkey(ageMoments_noHCC,index)
setkey(raMoments,index)

write.csv(ageMoments,file="Intermediate_Output/MC_Moments/ageMoments.csv",row.names=FALSE)
write.csv(ageMoments_noHCC,file="Intermediate_Output/MC_Moments/ageMoments_noHCC.csv",row.names=FALSE)
write.csv(firmMoments,file="Intermediate_Output/MC_Moments/firmMoments.csv",row.names=FALSE)
write.csv(metalMoments,file="Intermediate_Output/MC_Moments/metalMoments.csv",row.names=FALSE)
write.csv(riskMoments,file="Intermediate_Output/MC_Moments/riskMoments.csv",row.names=FALSE)
write.csv(raMoments,file="Intermediate_Output/MC_Moments/raMoments.csv",row.names=FALSE)
# write.csv(choiceData,"Simulation_Risk_Output/simchoiceData_discrete.csv",row.names=FALSE)

# remove_Vars = ls()[!grepl("(full_predict|metalDict)",ls())]
# 
# rm(list = remove_Vars)
# gc()
# 
# 
# #### Starting Vector ####
# run = "2019-03-12"
# simFile = paste("Simulation_Risk_Output/simData_",run,".rData",sep="")
# load(simFile)
# full_predict[,Age_1:=AGE]
# full_predict[,AV_diff:=AV-AV_std]
# 
# full_predict = full_predict[,c("ST","Firm","Product","Metal_std","Age_1","s_pred","PERWT","HCC_Silver","AV_std","AV_diff")]
# metalDict = unique(metalDict[,c("Product","logAvgCost","M_num")])
# rm(acs,draws)
# gc()
# 
# prod_Avgs = merge(full_predict,metalDict[,c("Product","logAvgCost","M_num")],by="Product",allow.cartesian = TRUE)
# rm(full_predict,metalDict)
# gc()
# 
# prod_Avgs = prod_Avgs[,list(Age = sum(10*Age_1*s_pred*PERWT)/sum(s_pred*PERWT),
#                             HCC = sum(HCC_Silver*s_pred*PERWT)/sum(s_pred*PERWT),
#                             AV_std = sum(AV_std*s_pred*PERWT)/sum(s_pred*PERWT),
#                             AV_diff = sum(AV_diff*s_pred*PERWT)/sum(s_pred*PERWT)),
#                       by=c("ST","Firm","M_num","logAvgCost","Metal_std")]
# 
# res = lm(logAvgCost~-1+Age+AV_std+AV_diff+HCC+ ST ,data=prod_Avgs)
# phi_start = res$coefficients
# linregfile = paste("Intermediate_Output/MC_Moments/linregpars_",run,".csv",sep="")
# write.csv(data.frame(par_start=phi_start),linregfile,row.names=FALSE)
# 