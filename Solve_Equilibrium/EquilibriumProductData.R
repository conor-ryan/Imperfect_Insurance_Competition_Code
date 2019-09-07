rm(list = ls())
library(data.table)
library(nleqslv)
library(ggplot2)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")


## Load Simulation Data
simData = as.data.table(read.csv("Intermediate_Output/Simulated_BaseData/simchoiceData_discrete.csv"))

## Load Risk Score Data
load("Simulation_Risk_Output/FirmRiskScores_woSim.rData")

firm_RA = as.data.table(firm_RA)
firm_RA[,st_share_adj:=memberMonths/sum(memberMonths),by="ST"]
firm_RA[Firm=="OTHER",oth_share:=st_share_adj]
firm_RA[,RA_share:=1-max(oth_share,na.rm=TRUE),by="ST"]



prod_data = simData[,list(count_hix_prod= max(count_hix_prod)),
                    by=c("Product_std","Metal_std","ST","Market","Firm",
                         "premBase_std","AV_std","benchBase","HighRisk")]

names(prod_data) = c("Product","Metal_std","ST","Market","Firm",
                     "premBase","AV_std","benchBase","HighRisk","count_hix_prod")

prod_data = merge(prod_data,firm_RA[,c("ST","Firm","RA_share")],by=c("ST","Firm"),all=TRUE)
# names(prod_data) = c("ST","Firm","Product","Metal_std","Market","premBase","AV_std","benchBase","count_hix_prod","HighRisk","RA_share","HighRisk")



##### Consolidate Data for Simulation ####
setkey(prod_data,Product)

### Fill out OTHER Missings
prod_data[Firm=="OTHER",Product:=0]
prod_data[Firm=="OTHER",premBase:=0]
prod_data[Firm=="OTHER",AV_std:=0]

predFile = paste("Simulation_Risk_Output/prodData.rData",sep="")
save(prod_data,file=predFile)


# for (st in sort(unique(prod_data$ST))){
#   
#   write.csv(prod_data[ST==st,],
#             file=paste("Intermediate_Output/Equilibrium_Data/estimated_prodData_",st,".csv",sep=""),
#             row.names=FALSE)
#   
# }

write.csv(prod_data[Firm!="OTHER",],
          file=paste("Intermediate_Output/Equilibrium_Data/estimated_prodData_full.csv",sep=""),
          row.names=FALSE)
