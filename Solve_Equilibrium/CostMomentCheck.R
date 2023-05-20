rm(list = ls())
library(data.table)
library(Matrix)
library(doBy)
setwd("C:/Users/cxr5626/Dropbox/Research/Imperfect_Insurance_Competition/")

run = "2022-12-26"
#### Load Estimated Data ####
simdataFiles = list.files("Intermediate_Output/Equilibrium_Data")[grep(paste("estimated_Data.*",run,sep=""),list.files("Intermediate_Output/Equilibrium_Data"))]

simData = NULL
for (f in simdataFiles[1]){
  print(f)
  temp = as.data.table(read.csv(paste("Intermediate_Output/Equilibrium_Data",f,sep="/")))
  # temp[R_HCC==0,C_noHCC:=C]
  # temp[R_HCC>0,C_HCC:=C]
  # temp[,AGE:=AGE*10]
  # temp[,Person:=floor(Person/1000)]
  # temp = temp[,list(C=sum(C*PERWT)/sum(PERWT),
  #                   C_noHCC=sum(C_noHCC*PERWT,na.rm=TRUE)/sum(PERWT*(!is.na(C_noHCC))),
  #                   C_HCC=sum(C_HCC*PERWT,na.rm=TRUE)/sum(PERWT*(!is.na(C_HCC))),
  #                   PERWT = sum(PERWT),
  #                   mkt_density=sum(mkt_density)),
  #             by=c("Person","Product","AGE")]
  temp = temp[,c("Person","Product","AGE","C","PERWT","util","R")]
  simData = rbind(simData,temp)
  rm(temp)
  gc()
}

## Calculate Base Shares
# setkey(simData,Person)
simData[,exp_sum:=sum(util),by="Person"]
simData[,s_hat:=util/(1+exp_sum),by="Person"]
simData[,s_ins:=exp_sum/(1+exp_sum)]

simData[,dsdp_own:=alpha*s_hat*(1-s_hat)]
simData[,avg_wgt:= s_hat*PERWT/sum(s_hat*PERWT),by="Product"]
simData[,marg_wgt:= dsdp_own*PERWT/sum(dsdp_own*PERWT),by="Product"]

simData[R_HCC>0,plot(C,avg_wgt-marg_wgt)]

#### Load Product Data ####
load("Simulation_Risk_Output/prodData.rData")

# simData[,c("goldCost","bronzeCost"):=NULL]
# simData[Product==1597,goldCost:=C]
# simData[,goldCost:=max(goldCost,na.rm=TRUE),by="Person"]
# 
# simData[Product==1586,bronzeCost:=C]
# simData[,bronzeCost:=max(bronzeCost,na.rm=TRUE),by="Person"]
# simData[,diff_test:= goldCost-bronzeCost]

simData = merge(simData,prod_data[,c("ST","Product","Firm","Market","Metal_std")],by="Product")

prod_test = simData[,list(avgCost=sum(C*s_hat*PERWT)/sum(s_hat*PERWT),
                          avgAllCost=sum(C*s_ins*PERWT)/sum(s_ins*PERWT),
                          share = sum(s_hat*PERWT)/sum(PERWT),
                          c1 = mean(C),
                          s1 = mean(s_ins),
                          c2 =sum(C*PERWT)/sum(PERWT)),
                    by=c("ST","Product","Firm","Metal_std","Market")]


metalClaims = as.data.table(read.csv("Intermediate_Output/Average_Claims/firmClaims.csv"))
prod_test = merge(prod_test,metalClaims,by.x=c("ST","Firm","Metal_std"),by.y=c("STATE","Firm","METAL"))


#### Firm Moment Data ####
load("Intermediate_Output/Average_Claims/FirmAvgCost.rData")
firm_test = simData[,list(avgCost=sum(C*s_hat*PERWT)/sum(s_hat*PERWT),
                          avgAllCost=sum(C*s_ins*PERWT)/sum(s_ins*PERWT)),
                    by=c("ST","Firm")]
firm_test = merge(firm_test,firmClaims,by=c("ST","Firm"))







## Drop Claims for firms that only have one purchased product in the state
metalClaims[ST=="IL"&Firm=="ASSURANT_HEALTH",EXP_INC_CLM_PMPM:=NA]
metalClaims[ST=="NE"&Firm=="ASSURANT_HEALTH",EXP_INC_CLM_PMPM:=NA]
metalClaims[ST=="IA"&Firm=="AVERA_HEALTH_PLANS",EXP_INC_CLM_PMPM:=NA]

## Drop Claims for firms that have no variation in reported cost
metalClaims[!is.na(EXP_INC_CLM_PMPM),cost_var:=(max(EXP_INC_CLM_PMPM)-min(EXP_INC_CLM_PMPM))/mean(EXP_INC_CLM_PMPM),by=c("Firm","ST")]
metalClaims[cost_var<.05,EXP_INC_CLM_PMPM:=NA]

prod_data = merge(prod_data,metalClaims[,c("ST","Firm","Metal_std","EXP_INC_CLM_PMPM","EXP_MM")],by=c("ST","Firm","Metal_std"))
prod_data = merge(prod_data,firmClaims[,c("ST","Firm","AvgCost")],by=c("ST","Firm"))

names(prod_data) = c("ST","Firm","Metal_std","Product","Market","premBase","AV_std","RA_share","MetalAvgCost","EXP_MM","FirmAvgCost")



#### Estimated Product Data ####
setkey(simData,Product)
net_insured = simData[,list(insured=sum(s_hat)),by=c("Person","PERWT")]

prod_est = simData[,list(avgCost_est=sum(C*PERWT*s_hat)/sum(PERWT*s_hat),
                         MM_est=12*sum(PERWT*s_hat)),
                   by="Product"]

prod_data = merge(prod_data,prod_est,by="Product")

#### Tests ####
metal_avg = prod_data[,list(avgCost_est=sum(MM_est*avgCost_est)/sum(MM_est),
                            MM_est = sum(MM_est)),
                      by=c("ST","Firm","Metal_std","MetalAvgCost","EXP_MM")]


# metal_avg = metal_avg[!is.na(MetalAvgCost)&!is.na(EXP_MM),list(filingAvg=sum(EXP_MM*MetalAvgCost)/sum(EXP_MM),
#                 estAvg = sum(EXP_MM*avgCost_est)/sum(EXP_MM)),by="Metal_std"]

metal_avg = metal_avg[!is.na(MetalAvgCost)&!is.na(EXP_MM),list(filingAvg=sum(MM_est*MetalAvgCost)/sum(MM_est),
                                                               estAvg = sum(MM_est*avgCost_est)/sum(MM_est)),by="Metal_std"]

metal_avg[Metal_std=="BRONZE",filingBrz:=filingAvg]
metal_avg[,filingBrz:=max(filingBrz,na.rm=TRUE)]
metal_avg[,filingAvg:=filingAvg/filingBrz]

metal_avg[Metal_std=="BRONZE",estBrz:=estAvg]
metal_avg[,estBrz:=max(estBrz,na.rm=TRUE)]
metal_avg[,estAvg:=estAvg/estBrz]

