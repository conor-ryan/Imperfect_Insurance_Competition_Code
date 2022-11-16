rm(list = ls())
library(doBy)
library(data.table)
library(nleqslv)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

#### Read in Rate Filings ####
filings=NULL
for (year in 2015:2017){
  print(year)
  if (year==2015){
    temp_file = read.csv("Data/2016_Rate_Filings/WKSH2_PUF_2015_20161103.csv")
  }
  if (year==2016){
    temp_file = read.csv("Data/2016_Rate_Filings/WKSH2_PUF_2016_20161103.csv")
  }
  if (year==2017){
    temp_file = read.csv("Data/2016_Rate_Filings/WKSH2_PUF_2017_20171024.csv")
  }
  temp_file = as.data.table(temp_file)
  
  if (year==2015){
    temp_file[,EXP_PLN_ADJ_INDX:=EXP_PRM_PMPM]
  }
  temp_file = unique(temp_file[MARKET!="Small Group",])
  
  temp_file[,STATUS_DT:=as.Date(STATUS_DT,format="%d%b%y:%H:%M:%S")]
  temp_file[,latest:=max(STATUS_DT),by=c("PLAN_ID","METAL","MARKET")]
  temp_file[,latest_ind:=STATUS_DT==latest]
  temp_file = temp_file[latest_ind==TRUE,]
  
  ## Calculate Insurer pre-transfer costs
  # temp_file[EXP_MM>0,expAvgCost:=(EXP_TAC-EXP_TAC_NOT+EXP_REIN+EXP_RSK_ADJ)/EXP_MM]
  # temp_file[PRJ_MM>0,prjAvgCost:=(PRJ_TAC-PRJ_TAC_NOT+PRJ_REIN+PRJ_RSK_ADJ)/PRJ_MM]
  # temp_file[EXP_MM==0,expAvgCost:=0]
  # temp_file[PRJ_MM==0,prjAvgCost:=0]
  
  # Include Reinsurance Payments
  temp_file[EXP_MM>0,expAvgCost:=(EXP_TAC-EXP_TAC_NOT+EXP_RSK_ADJ)/EXP_MM]
  temp_file[PRJ_MM>0,prjAvgCost:=(PRJ_TAC-PRJ_TAC_NOT+PRJ_RSK_ADJ)/PRJ_MM]
  temp_file[EXP_MM==0,expAvgCost:=0]
  temp_file[PRJ_MM==0,prjAvgCost:=0]
  
  ## Mean of remaining duplicates
  
  # filings = filings[,list(
  #   expAvgCost = sum(expAvgCost*EXP_MM)/sum(EXP_MM),
  #   prjAvgCost = sum(prjAvgCost*PRJ_MM)/sum(PRJ_MM),
  #   EXP_PLN_ADJ_INDX=sum(EXP_MM*EXP_PLN_ADJ_INDX)/sum(EXP_MM),
  #   EXP_RSK_ADJ = sum(EXP_RSK_ADJ*EXP_MM)/sum(EXP_MM),
  #   EXP_INC_CLM_PMPM = sum(EXP_INC_CLM_PMPM*EXP_MM)/sum(EXP_MM),
  #   EXP_ALWD_CLM_PMPM = sum(EXP_ALWD_CLM_PMPM*EXP_MM)/sum(EXP_MM),
  #   EXP_MM = sum(EXP_MM),
  #   PRJ_PLN_ADJ_INDX=sum(PRJ_MM*PRJ_PLN_ADJ_INDX)/sum(PRJ_MM),
  #   PRJ_RSK_ADJ = sum(PRJ_RSK_ADJ*PRJ_MM)/sum(PRJ_MM),
  #   PRJ_INC_CLM_PMPM = sum(PRJ_INC_CLM_PMPM*PRJ_MM)/sum(PRJ_MM),
  #   PRJ_ALWD_CLM_PMPM = sum(PRJ_ALWD_CLM_PMPM*PRJ_MM)/sum(PRJ_MM),
  #   PRJ_MM = sum(PRJ_MM)),
  #   by=c("STATE","MARKET","COMPANY","ISSUER_ID","PLAN_ID","METAL")]
  
  temp_file[,METAL:=toupper(METAL)]
  temp_file = temp_file[,c("STATE","MARKET","COMPANY","ISSUER_ID","PLAN_ID","METAL",
                           "expAvgCost","prjAvgCost",
                           "EXP_PLN_ADJ_INDX","EXP_RSK_ADJ","EXP_INC_CLM_PMPM","EXP_ALWD_CLM_PMPM","EXP_MM",
                           "PRJ_PLN_ADJ_INDX","PRJ_RSK_ADJ","PRJ_INC_CLM_PMPM","PRJ_ALWD_CLM_PMPM","PRJ_MM")]
  temp_file[,Year:=year]
  filings = rbind(filings,temp_file)
  
}

### Risk and Claims Correlation ###
risk_test = filings[Year==2016&!is.na(EXP_RSK_ADJ)&EXP_RSK_ADJ!=0.0&EXP_MM>0,]
risk_test[,avgRisk:=EXP_RSK_ADJ/EXP_MM]
risk_test[,costDev:=expAvgCost - sum(expAvgCost*EXP_MM)/sum(EXP_MM),by=c("STATE")]

### Save Metal Avg Costs ###
metalAvg = filings[EXP_MM>0,list(expAvgCost=sum(expAvgCost*EXP_MM)/sum(EXP_MM),EXP_MM=sum(EXP_MM)),by=c("METAL","STATE","MARKET","COMPANY","Year")]
metalAvg2 = filings[PRJ_MM>0,list(prjAvgCost=sum(prjAvgCost*PRJ_MM)/sum(PRJ_MM),PRJ_MM=sum(PRJ_MM)),by=c("METAL","STATE","MARKET","COMPANY","Year")]
metalAvg = merge(metalAvg,metalAvg2,by=c("METAL","STATE","MARKET","COMPANY","Year"))
save(metalAvg,file="Intermediate_Output/Average_Claims/allMetalFilings.rData")



#### Cost by metal level... ####
crosswalk = read.csv("Intermediate_Output/FirmCrosswalk.csv",check.names=F)
crosswalk = unique(crosswalk[,c("ISSUER_ID","Firm")])

filings = merge(filings,crosswalk,by="ISSUER_ID",all.x=TRUE)

filings = filings[!is.na(Firm)&(EXP_MM>0|PRJ_MM>0),
                  list(
                    expAvgCost = sum(expAvgCost*EXP_MM)/sum(EXP_MM),
                    prjAvgCost = sum(prjAvgCost*PRJ_MM)/sum(PRJ_MM),
                    EXP_PLN_ADJ_INDX=sum(EXP_MM*EXP_PLN_ADJ_INDX)/sum(EXP_MM),
                    EXP_RSK_ADJ = sum(EXP_RSK_ADJ*EXP_MM)/sum(EXP_MM),
                    EXP_INC_CLM_PMPM = sum(EXP_INC_CLM_PMPM*EXP_MM)/sum(EXP_MM),
                    EXP_ALWD_CLM_PMPM = sum(EXP_ALWD_CLM_PMPM*EXP_MM)/sum(EXP_MM),
                    EXP_MM = sum(EXP_MM),
                    PRJ_PLN_ADJ_INDX=sum(PRJ_MM*PRJ_PLN_ADJ_INDX)/sum(PRJ_MM),
                    PRJ_RSK_ADJ = sum(PRJ_RSK_ADJ*PRJ_MM)/sum(PRJ_MM),
                    PRJ_INC_CLM_PMPM = sum(PRJ_INC_CLM_PMPM*PRJ_MM)/sum(PRJ_MM),
                    PRJ_ALWD_CLM_PMPM = sum(PRJ_ALWD_CLM_PMPM*PRJ_MM)/sum(PRJ_MM),
                    PRJ_MM = sum(PRJ_MM)),
                  by=c("STATE","MARKET","Firm","METAL","Year")]

filings[EXP_MM==0,expAvgCost:=0]
filings[PRJ_MM==0,prjAvgCost:=0]


#### Merge into Product Categories ####
shares = as.data.table(read.csv("Intermediate_Output/Estimation_Data/marketDataMap_discrete.csv"))
shares[,STATE:=gsub("_.*","",Market)]
shares[,METAL:=gsub(" .*","",METAL)]
shares = unique(shares[,c("Firm","STATE","METAL")])

firm_claims = merge(shares,filings,by=c("STATE","Firm","METAL"),all.x=TRUE)
write.csv(firm_claims,"Intermediate_Output/Average_Claims/firmMetalFilings.csv",row.names=FALSE)


##### Firm Level Filings ####
rm(list = ls())
filings=NULL
for (year in 2015:2017){
  print(year)
  if (year==2015){
    temp_file = read.csv("Data/2016_Rate_Filings/WKSH1_PUF_2015_20161103.csv")
  }
  if (year==2016){
    temp_file = read.csv("Data/2016_Rate_Filings/WKSH1_PUF_2016_20161103.csv")
  }
  if (year==2017){
    temp_file = read.csv("Data/2016_Rate_Filings/WKSH1_PUF_2017_20171024.csv")
  }
  temp_file = as.data.table(temp_file)
  
  if (year==2015){
    temp_file[,EXP_PLN_ADJ_INDX:=EXP_PRM_PMPM]
  }
  temp_file = unique(temp_file[MARKET!="Small Group",])
  
  temp_file[,STATUS_DT:=as.Date(STATUS_DT,format="%d%b%y:%H:%M:%S")]
  temp_file[,latest:=max(STATUS_DT),by=c("ISSUER_ID","MARKET")]
  temp_file[,latest_ind:=STATUS_DT==latest]
  temp_file = temp_file[latest_ind==TRUE,]
  
  ## Calculate Insurer pre-transfer costs
  # temp_file[EXP_MM>0,expAvgCost:=(EXP_TAC-EXP_TAC_NOT+EXP_REIN+EXP_RSK_ADJ)/EXP_MM]
  # temp_file[PRJ_MM>0,prjAvgCost:=(PRJ_TAC-PRJ_TAC_NOT+PRJ_REIN+PRJ_RSK_ADJ)/PRJ_MM]
  # temp_file[EXP_MM==0,expAvgCost:=0]
  # temp_file[PRJ_MM==0,prjAvgCost:=0]
  
  # Include Reinsurance Payments
  temp_file[,expAvgCost:=EXP_CLM_IN_PMPM]
  temp_file[,prjAvgCost:=PRJ_CLM_IN_BEF_ACA_PMPM - PRJ_REINS_PMPM]
  # temp_file[EXP_MM==0,expAvgCost:=0]
  # temp_file[PRJ_MM==0,prjAvgCost:=0]
  
  temp_file = temp_file[,c("STATE","MARKET","COMPANY","ISSUER_ID",
                           "expAvgCost","prjAvgCost","PRJ_RSK_ADJ_PMPM",
                           "EXP_MM_WK1","PRJ_MM_WK1")]
  temp_file[,Year:=year]
  filings = rbind(filings,temp_file)
  
}




#### Merge into Existing Firms ####
crosswalk = read.csv("Intermediate_Output/FirmCrosswalk.csv",check.names=F)
crosswalk = unique(crosswalk[,c("ISSUER_ID","Firm")])

filings = merge(filings,crosswalk,by="ISSUER_ID",all.x=TRUE)


shares = as.data.table(read.csv("Intermediate_Output/Estimation_Data/marketDataMap_discrete.csv"))
shares[,STATE:=gsub("_.*","",Market)]
shares = unique(shares[,c("Firm","STATE")])

firm_claims = merge(shares,filings,by=c("STATE","Firm"),all.x=TRUE)

firm_claims = firm_claims[,list(expAvgCost=sum(expAvgCost*EXP_MM_WK1)/sum(EXP_MM_WK1),
                                prjAvgCost=sum(prjAvgCost*PRJ_MM_WK1)/sum(PRJ_MM_WK1),
                                prjRiskAdj=sum(PRJ_RSK_ADJ_PMPM*PRJ_MM_WK1)/sum(PRJ_MM_WK1),
                                EXP_MM_WK1=sum(EXP_MM_WK1),
                                PRJ_MM_WK1=sum(PRJ_MM_WK1)),
                          by=c("STATE","Firm","MARKET","Year")]



write.csv(firm_claims,"Intermediate_Output/Average_Claims/firmFilings.csv",row.names=FALSE)