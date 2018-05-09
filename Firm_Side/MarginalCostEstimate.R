rm(list = ls())
library(doBy)
library(data.table)
library(nleqslv)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

## Run
run = "2018-04-12"

#### Load Simulation Data and Merge in GCF/AV data ####
simFile = paste("Simulation_Risk_Output/simData_",run,".rData",sep="")
load(simFile)

setkey(acs,Product,Person)
setkey(predict_data,Product,Person)


#### Preference Parameters ####
parFile = paste("Estimation_Output/estimationresults_",run,".csv",sep="")
pars = read.csv(parFile)

# 
# alpha = pars$pars[1]
# gamma = pars$pars[2:4]
# beta = matrix(pars$pars[5:16],nrow=4,ncol=3,byrow=FALSE)
# sigma = pars$pars[17:21]

gamma = pars$pars[1:3]
beta0 = pars$pars[4:6]
beta = matrix(0,nrow=3,ncol=3)
beta[2,1] = pars$pars[7]
beta[3,1] = pars$pars[8]
sigma = pars$pars[9:11]

alpha = beta0[1]

## Calculate alpha for each demographic ##
acs[,alpha:=alpha+beta[1,1]*Age+beta[1,2]*Family+beta[1,3]*LowIncome]


## Integrate Draws and Prediction Data
draws = as.data.table(draws)
n_draws = nrow(draws)
draws[,d_ind:=as.integer(1:n_draws)]
setkey(draws,d_ind)
setkey(predict_data,d_ind,Person)
nu_h_large = draws[predict_data$d_ind,2]*sign(sigma[3])
nu_i_large = draws[predict_data$d_ind,1]*sign(sigma[1])

predict_data[,alpha_draw:=0]
predict_data[,nu_h:=nu_h_large]
predict_data[,nu_i:=nu_i_large]
rm(nu_h_large,nu_i_large)

## indication
predict_data[,nu_h_ind:=as.numeric(nu_h>.6)]
predict_data[,nu_i_ind:=as.numeric(nu_i>.6)]

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
acs[,STATE:=gsub("_.*","",Market)]
full_predict = merge(acs,predict_data,by=c("Product","Person"))
full_predict[,wgt:=PERWT*s_pred/n_draws]
#full_predict[,METAL:=gsub(" .*","",METAL)]

firms = full_predict[,list(sim_lives=sum(wgt),
                           age = sum(AGE*wgt)/sum(wgt),
                           mem = sum(MEMBERS*wgt)/sum(wgt),
                           inc = sum(LowIncome*wgt)/sum(wgt),
                           fpl = sum(HHincomeFPL*wgt)/sum(wgt),
                           high = sum(High*wgt)/sum(wgt),
                           nu_i_ind = sum(nu_i_ind*wgt)/sum(wgt),
                           nu_h_ind = sum(nu_h_ind*wgt)/sum(wgt),
                           nu_h = sum(nu_h*wgt)/sum(wgt),
                           nu_i = sum(nu_i*wgt)/sum(wgt)), by=c("Firm","STATE")]

firms = merge(firms,metalClaims,by=c("Firm","STATE","METAL"),all.x=TRUE)
#firms[,regVar:=log(AvgCost)]

reg = lm(regVar~age+mem+fpl+high+nu_i_ind+nu_h_ind,data=firms)


