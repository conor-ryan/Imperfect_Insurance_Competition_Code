rm(list = ls())
library(data.table)
library(nleqslv)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

#### Base Firm Risk Data ####
firmRiskFile ="Simulation_Risk_Output/FirmRiskScores_woSim.rData"
load(firmRiskFile)
setkey(firm_RA,ST,Firm)

#### Market Data ####
mkt_data = read.csv("Intermediate_Output/Estimation_Data/marketDataMap_discrete.csv")
mkt_data$Metal_std = gsub(" [0-9]+","",mkt_data$METAL)
metalEst = summaryBy(enroll~STATE+Firm+Metal_std,data=mkt_data[mkt_data$METAL!="CATASTROPHIC",],FUN=sum,keep.names=TRUE)

#### Firm Metal level Risk Transfers ####
metalClaims = as.data.table(read.csv("Intermediate_Output/Average_Claims/firmClaims.csv"))
metalClaims[,T_avg:=EXP_RSK_ADJ/EXP_MM]

ST_RA_Data = unique(firm_RA[,c("ST","RA_prem","AvgPLRS","AvgARF")])

metalEst = merge(ST_RA_Data,metalEst,by.x="ST",by.y="STATE")
setkey(metalEst,ST,Firm,Metal_std)

metalClaims = merge(metalEst,metalClaims,by.x=c("ST","Firm","Metal_std"),by.y=c("STATE","Firm","METAL"),all.x=TRUE)

metalClaims[,T_norm_data:=T_avg*12/RA_prem]


# Drop firms with identical transfers across all metal levels
metalClaims[,mean:=mean(T_norm_data),by=c("ST","Firm")]
metalClaims[,noVar:=all(abs(T_norm_data-mean)<1e-3),by=c("ST","Firm")]

metal_moments = metalClaims[!is.na(T_norm_data)&!noVar,list(T_norm_data = sum(T_norm_data*EXP_MM)/sum(EXP_MM)),by=c("Metal_std")]
metal_moments[,Metal_std:=factor(Metal_std,levels=c("BRONZE","SILVER","GOLD","PLATINUM"))]
setkey(metal_moments,Metal_std)


#### Metal Moments ####
# Wakely Numbers
metal_moments[,R_adj:= c(.814,1.503,1.889,2.675)]
metal_moments[,R_adj_est:=(T_norm_data+1)*1.448]

metal_moments[,momentID:=1:nrow(metal_moments)]

#### Firm Moments ####
firm_moments = firm_RA[Firm!="OTHER",c("ST","Firm","T_norm_adj","R_adj_natl","memberMonths")]
firm_moments[,Big:=as.numeric(grepl("UNITED|BLUE|CIGNA|ASSURANT",Firm))]
firm_moments[,R_avg:=sum(R_adj_natl*memberMonths)/sum(memberMonths),by="Big"]
firm_moments = firm_moments[Big==1,]
firm_moments[,momentID:=nrow(metal_moments)+2]

#### Total Enrolled Moment ####
total_moment = mkt_data[mkt_data$METAL!="CATASTROPHIC",c("STATE","Firm","Product")]
total_moment$T_moment = 1.448
total_moment$momentID = max(metal_moments$momentID) + 1
total_moment$ST = total_moment$STATE

total_moment = total_moment[,c("Product","momentID","T_moment","ST")]


##### Output Moments ####
mkt_data = read.csv("Intermediate_Output/Estimation_Data/marketDataMap_discrete.csv")
mkt_data = mkt_data[mkt_data$METAL!="CATASTROPHIC",]
mkt_data$METAL = gsub(" [0-9]+","",mkt_data$METAL)



metal_moments = merge(mkt_data[,c("STATE","Firm","METAL","Product")],metal_moments,
                      by.x=c("METAL"),by.y=c("Metal_std"))
firm_moments = merge(mkt_data[,c("STATE","Firm","Product")],firm_moments,
                      by.x=c("STATE","Firm"),by.y=c("ST","Firm"))

metal_moments = metal_moments[,c("Product","momentID","R_adj","STATE")]
names(metal_moments) = c("Product","momentID","T_moment","ST")
firm_moments = firm_moments[,c("Product","momentID","R_avg","STATE")]
names(firm_moments) = c("Product","momentID","T_moment","ST")


risk_moments = rbind(metal_moments,firm_moments)
risk_moments = rbind(risk_moments,total_moment)
risk_moments$ST = as.numeric(risk_moments$ST)

## Place Holder
risk_moments$st_share = 0 

unique(risk_moments[,c("T_moment","momentID")])


# risk_moment_test = read.csv("Intermediate_Output/Estimation_Data/riskMoments.csv")
# 
# test = merge(risk_moments,risk_moment_test,by=c("Product","momentID","ST"),all=TRUE)

write.csv(risk_moments,"Intermediate_Output/Estimation_Data/riskMoments.csv",row.names=FALSE)
