rm(list = ls())
library(doBy)
library(data.table)
library(nleqslv)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

## Run
run = "2018-05-12"

#### Load Simulation Data and Merge in GCF/AV data ####
simFile = paste("Simulation_Risk_Output/simData_",run,".rData",sep="")
load(simFile)

#### Read in Total Claims Data ####
MLR_Data = read.csv("Data/2015_MLR/Part1_2_Summary_Data_Premium_Claims.csv")


claims = MLR_Data[MLR_Data$ROW_LOOKUP_CODE=="TOTAL_INCURRED_CLAIMS_PT2",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_Q1")]
names(claims) = c("ï..MR_SUBMISSION_TEMPLATE_ID","Claims")

enroll =MLR_Data[MLR_Data$ROW_LOOKUP_CODE=="NUMBER_OF_LIFE_YEARS",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_Q1")]
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

#### Filings Claims Data ####
metalClaims = read.csv("Intermediate_Output/Average_Claims/firmClaims.csv")

#### Firm Data ####
n_draws = nrow(draws)
full_predict[,wgt:=PERWT*s_pred/n_draws]

firms = full_predict[,list(sim_lives=sum(wgt),
                           Age_j = sum(AGE*wgt)/sum(wgt),
                           mem = sum(MEMBERS*wgt)/sum(wgt),
                           inc = sum(LowIncome*wgt)/sum(wgt),
                           fpl = sum(HHincomeFPL*wgt)/sum(wgt),
                           high = sum(High*wgt)/sum(wgt),
                           nu_h = sum(nu_h*wgt)/sum(wgt),
                           nu_i = sum(nu_i*wgt)/sum(wgt),
                           WTP_j = sum(WTP*wgt)/sum(wgt),
                           AV = sum(AV*wgt)/sum(wgt)), by=c("ST","Firm","Metal_std","AV_std")]

firms = merge(firms,metalClaims,by.x=c("Firm","ST","Metal_std"),
              by.y=c("Firm","STATE","METAL"),all.x=TRUE)
firms[,regVar:=log(EXP_INC_CLM_PMPM)]
firms[regVar==-Inf,regVar:=NA]

spec1 = lm(regVar~AV_std+Age_j+WTP_j,data=firms)
spec2 = lm(regVar~ST+AV_std+Age_j+WTP_j,data=firms)
spec3 = lm(regVar~ST+Firm+AV_std+Age_j+WTP_j,data=firms)


CostRes = lm(regVar~ST+AV_std+Age_j+WTP_j,data=firms)

firms[,pred_cost:=exp(predict(CostRes,newdata=firms))]

costFile = paste("Simulation_Risk_Output/costParameters_",run,".rData",sep="")
save(CostRes,file=costFile)
