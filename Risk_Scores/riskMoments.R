rm(list = ls())
library(data.table)
# library(nleqslv)
library(doBy)
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
metalClaims = as.data.table(read.csv("Intermediate_Output/Average_Claims/firmMetalFilings.csv"))
metalClaims = metalClaims[(EXP_MM>0)&(Year==2017),]
metalClaims[,T_avg:=EXP_RSK_ADJ/EXP_MM]

ST_RA_Data = unique(firm_RA[,c("ST","Firm","RA_prem","AvgPLRS","AvgARF","T_norm","payments_adj","MM_compliant","memberMonths")])

metalEst = merge(ST_RA_Data,metalEst,by.x=c("ST","Firm"),by.y=c("STATE","Firm"))
setkey(metalEst,ST,Firm,Metal_std)

metalClaims = merge(metalEst,metalClaims,by.x=c("ST","Firm","Metal_std"),by.y=c("STATE","Firm","METAL"),all.x=TRUE)

metalClaims[,T_norm_data:=T_avg*12/RA_prem]
metalClaims[,R_est:=(T_norm_data+1)*AvgPLRS]

metalClaims[,avgRA_1:=payments_adj/memberMonths]
metalClaims[,avgRA_2:=payments_adj/MM_compliant]

# test = metalClaims[,list(PRJ_RSK_ADJ=sum(PRJ_RSK_ADJ)/sum(PRJ_MM),
#                          EXP_RSK_ADJ=sum(EXP_RSK_ADJ)/sum(EXP_MM),
#                          PRJ_MM = sum(PRJ_MM),
#                          EXP_MM = sum(EXP_MM)),
#                    by=c("ST","Firm","Year","avgRA_1","avgRA_2")]



# Drop firms with identical transfers across all metal levels
metalClaims[,mean:=mean(T_norm_data),by=c("ST","Firm")]
metalClaims[,noVar:=all(abs(T_norm_data-mean)<1e-3),by=c("ST","Firm")]

metal_moments = metalClaims[!is.na(T_norm_data)&!noVar,list(T_norm_data = sum(T_norm_data*EXP_MM)/sum(EXP_MM),
                                                            R_est       = sum(R_est*EXP_MM)/sum(EXP_MM)),by=c("Metal_std")]
metal_moments[,Metal_std:=factor(Metal_std,levels=c("BRONZE","SILVER","GOLD","PLATINUM"))]
setkey(metal_moments,Metal_std)


#### Metal Moments ####
# Wakely Numbers
# metal_moments[,R_adj:= c(0.8434997,1.445666,2.178068,2.973488)]
metal_moments[,R_adj:= c(.814,1.503,1.889,2.675)]
metal_moments[,R_adj_est:=(T_norm_data+1)*1.448]

metal_moments[,momentID:=1:nrow(metal_moments)]

#### Total Enrolled Moment ####
total_moment = mkt_data[mkt_data$METAL!="CATASTROPHIC",c("ST","Firm","Product")]
total_moment$R_moment = 1.448
total_moment$momentID = max(metal_moments$momentID) + total_moment$ST

total_moment = total_moment[,c("Product","momentID","R_moment","ST")]

#### Firm Moments ####
firm_moments = firm_RA[,c("ST","Firm","T_norm_adj","memberMonths","payments_adj","HighRisk","relative_risk_age_const","relative_risk_unadj","relative_risk_adj_term")]
firm_data = unique(mkt_data[,c("STATE","Firm","Small")])
firm_moments = merge(firm_moments,firm_data,by.x=c("ST","Firm"),by.y=c("STATE","Firm"))


# Drop At least one firm per state, prioritze those without full Product Menu
menu_check = merge(firm_moments,mkt_data[,c("STATE","Firm","Product","Market","METAL")],
                   by.y=c("STATE","Firm"),by.x=c("ST","Firm"))
menu_check[,Bronze:=METAL=="BRONZE"]
menu_check[,Silver:=METAL=="SILVER"]
menu_check[,Gold:=METAL=="GOLD"]

full_menu = menu_check[,list(Bronze=max(Bronze),Silver=max(Silver),Gold=max(Gold)),by=c("Firm","ST","memberMonths")]
# Dropped Firms
full_menu[,drop:=0]
# full_menu[Bronze==0|Silver==0|Gold==0,drop:=1]

#If all firms have complete menu, drop smallest
full_menu[,any_drop:=max(drop),by="ST"]
full_menu[,smallest:=min(memberMonths),by="ST"]
full_menu[,smallest:=memberMonths==smallest]
full_menu[any_drop==0&smallest==TRUE,drop:=1]

#Match String
full_menu[,Firm_ST:=paste(ST,Firm,sep="_")]
firm_moments[,Firm_ST:=paste(ST,Firm,sep="_")]

# Drop two more very small firms that are a pain to match
full_menu = full_menu[ST%in%c("AK","IA","GA")]

## Drop some weird plans


# full_menu = full_menu[ST!="IL"]


# firm_moments = firm_moments[Firm_ST%in%full_menu$Firm_ST[full_menu$drop==0]]
firm_moments = firm_moments[Firm_ST%in%full_menu$Firm_ST]

firm_moments[,paste(paste(":",Firm_ST,sep=""),collapse=",")]

# firm_moments = firm_moments[HighRisk==1,]
firm_moments[,momentID:=max(total_moment$momentID)+(1:nrow(firm_moments))]
# firm_moments[,momentID:=(1:nrow(firm_moments))]
setkey(firm_moments,momentID)


##### Output Moments ####
mkt_data = read.csv("Intermediate_Output/Estimation_Data/marketDataMap_discrete.csv")
mkt_data = mkt_data[mkt_data$METAL!="CATASTROPHIC",]
mkt_data$METAL = gsub(" [0-9]+","",mkt_data$METAL)



metal_moments = merge(mkt_data[,c("STATE","Firm","METAL","Product","ST")],metal_moments,
                      by.x=c("METAL"),by.y=c("Metal_std"))
firm_moments = merge(firm_moments,mkt_data[,c("STATE","Firm","Product","Market","METAL","ST")],
                     by.y=c("STATE","Firm"),by.x=c("ST","Firm"))





metal_moments = metal_moments[,c("Product","momentID","R_adj","ST")]
names(metal_moments) = c("Product","momentID","R_moment","ST")
firm_moments = firm_moments[,c("Product","momentID","relative_risk_age_const","ST.y")]
names(firm_moments) = c("Product","momentID","T_moment","ST")

# firm_moments = firm_moments[firm_moments$ST%in%c("AK","GA")]


# risk_moments = rbind(metal_moments,firm_moments)
risk_moments = rbind(metal_moments,total_moment)
# risk_moments$ST = as.numeric(as.factor(risk_moments$ST))
# firm_moments$ST = as.numeric(as.factor(firm_moments$ST))
## Place Holder
risk_moments$st_share = 0 
firm_moments$st_share = 0 
unique(risk_moments[,c("R_moment","momentID")])
unique(firm_moments[,c("T_moment","momentID","ST")])

# risk_moment_test = read.csv("Intermediate_Output/Estimation_Data/riskMoments.csv")
# 
# test = merge(risk_moments,risk_moment_test,by=c("Product","momentID","ST"),all=TRUE)
risk_moments = as.data.table(risk_moments)
setkey(risk_moments,momentID,Product)
write.csv(risk_moments,"Intermediate_Output/Estimation_Data/riskMoments.csv",row.names=FALSE)

setkey(firm_moments,momentID,Product)
write.csv(firm_moments,"Intermediate_Output/Estimation_Data/firmMoments.csv",row.names=FALSE)