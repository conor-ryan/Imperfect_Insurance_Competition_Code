rm(list = ls())
library(data.table)
library(nleqslv)
library(ggplot2)
library(scales)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")


## Estimation Run 
run = "2019-06-25"

#Load Product Data
predFile = paste("Simulation_Risk_Output/prodData.rData",sep="")
load(predFile)


eqFile = paste("Estimation_Output/checkMargins_",run,".csv",sep="")
eqData = as.data.table(read.csv(eqFile))

prod_data = merge(prod_data,eqData,by="Product")

# firms = prod_data[,list(R_avg=sum(share_base*size*R_avg)/sum(share_base*size),
#                         Revenue=sum(share_base*size*premBase*ageRate)/sum(share_base*size)),
#                   by=c("ST","Firm")]



#### Market Size ####
load("Intermediate_Output/Simulated_BaseData/simMarketSize.rData")
prod_data = merge(prod_data,marketSize,by="Market")
prod_data[,share_base:=lives/size]

## Test against moments
share_moment = read.csv("Intermediate_Output/Estimation_Data/marketDataMap_discrete.csv")
prod_data = merge(prod_data,share_moment[,c("Product","unins_rate","Share")],by=c("Product"),all=TRUE)

prod_data[,plot(share_base,Share)]
prod_data[,unins_base:=1-sum(share_base),by="Market"]
unins_test = prod_data[,list(unins_base=1-sum(share_base)),by=c("Market","size","unins_rate")]

prod_data[,plot(unins_base,unins_rate)]

#### Merge Firm Costs ####
load("Intermediate_Output/Average_Claims/AvgCostMoments.rData")
firmClaims = firmClaims[,c("ST","Firm","AvgCost","prjFirmCost")]
names(firmClaims) = c("ST","Firm","FirmAvgCost","prjFirmCost")
prod_data = merge(prod_data,firmClaims,by=c("ST","Firm"))


metalClaims = as.data.table(read.csv("Intermediate_Output/Average_Claims/avgFirmCosts.csv"))
metalClaims[,expFirmCost:=sum(EXP_MM*expAvgCost)/sum(EXP_MM),by=c("STATE","Firm")]


prod_test = merge(prod_data,metalClaims,by.x=c("ST","Firm","Metal_std"),by.y=c("STATE","Firm","METAL"))

prod_test[,estFirmCost:=sum(lives*avgCost)/sum(lives),by=c("ST","Firm")]
prod_test[,estMetalCost:=sum(lives*avgCost)/sum(lives),by=c("ST","Firm","Metal_std")]
prod_test[,adjMetalCost:=expAvgCost*prjFirmCost/expFirmCost]

prod_test[,plot(prjFirmCost,estFirmCost)]


prod_test[,plot(adjMetalCost,estMetalCost)]
prod_test[,lm(estMetalCost~adjMetalCost)]

prod_test[,firmAvgPrem:=sum(lives*premBase*ageRate)/sum(lives),by=c("ST","Firm")]

prod_test[,sum(lives*avgCost)/sum(lives),by="Metal_std"]


# #### Share Estimates from Data ####
# shares = read.csv("Intermediate_Output/Estimation_Data/marketData_discrete.csv")
# prod_data = merge(prod_data,shares[,c("Product","Share")],by="Product")
# 
# #### Test Cost Moments ####
# firmMoments = prod_data[,list(estFirmCost=sum(share_base*avgCost)/sum(share_base),
#                               share=sum(share_base)),by=c("ST","Firm","Market","FirmAvgCost","M_num")]
# 
# metalMoments = prod_data[,list(estAvgCost=sum(share_base*avgCost)/sum(share_base)),by=c("Metal_std")]
# metalMoments = merge(metalMoments,metalAvg,by="Metal_std")
# 
# # metalMoments[Metal_std=="BRONZE",bronzeCost:=avgCost]
# # metalMoments[,bronzeCost:=max(bronzeCost,na.rm=TRUE)]
# # metalMoments[,costIndex:=avgCost/bronzeCost]
# # metalMoments[,c("bronzeCost"):=NULL]
# 
# metalMoments[Metal_std=="BRONZE",bronzeCost:=estAvgCost]
# metalMoments[,bronzeCost:=max(bronzeCost,na.rm=TRUE)]
# metalMoments[,estCostIndex:=estAvgCost/bronzeCost]
# metalMoments[,c("bronzeCost"):=NULL]

#### Plot Margin Check ####
png("Writing/Images/marginCheckBase.png",width=2000,height=1500,res=275)
plot = ggplot(prod_data[Firm!="OTHER",]) + aes(y=12/1000*prem_FOC_base,x=12/1000*premBase) +
  geom_point() + 
  geom_abline(intercept=0,slope=1) + 
  geom_smooth(color="red",method="lm",se=FALSE) + 
  # scale_x_continuous(labels = dollar,breaks = c(2,4,6,8,10)) +
  # scale_y_continuous(labels = dollar,breaks = c(2,4,6,8,10)) +
  # coord_cartesian(ylim=c(0,6)) +
  xlab("Observed Base Premium (000s)") + 
  ylab("Optimal Base Premium (000s)") + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_rect(color=grey(.5)),
    legend.title=element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "none",
    axis.title=element_text(size=14),
    axis.text = element_text(size=16))
print(plot)
dev.off()

png("Writing/Images/marginCheckRA.png",width=2000,height=1500,res=275)
ggplot(prod_data[Firm!="OTHER",]) + aes(x=12/1000*premBase,y=12/1000*prem_FOC_RA) +
  geom_point() + 
  geom_abline(intercept=0,slope=1) + 
  geom_smooth(color="red",method="lm",se=FALSE) + 
  # scale_x_continuous(labels = dollar,breaks = c(2,4,6,8,10)) +
  # scale_y_continuous(labels = dollar,breaks = c(2,4,6,8,10)) +
  # coord_cartesian(ylim=c(0,8)) +
  xlab("Observed Base Premium (000s)") + 
  ylab("Optimal Base Premium (000s)") + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_rect(color=grey(.5)),
    legend.title=element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "none",
    axis.title=element_text(size=14),
    axis.text = element_text(size=16))
dev.off()

ggplot(prod_test[Firm!="OTHER",]) + aes(premBase*ageRate,y=EXP_INC_CLM_PMPM) +
  geom_point() + 
  geom_abline(intercept=0,slope=1) + 
  geom_smooth(color="red",method="lm",se=FALSE) + 
  # scale_x_continuous(labels = dollar,breaks = c(2,4,6,8,10)) +
  # scale_y_continuous(labels = dollar,breaks = c(2,4,6,8,10)) +
  # coord_cartesian(ylim=c(0,8)) +
  xlab("Observed Base Premium (000s)") + 
  ylab("Optimal Base Premium (000s)") + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_rect(color=grey(.5)),
    legend.title=element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "none",
    axis.title=element_text(size=14),
    axis.text = element_text(size=16))


ggplot(prod_test[,]) + aes(y=estMetalCost,x=adjMetalCost) +
  geom_point() + 
  geom_abline(intercept=0,slope=1) + 
  geom_smooth(color="red",method="lm",se=FALSE) + 
  # scale_x_continuous(labels = dollar,breaks = c(2,4,6,8,10)) +
  # scale_y_continuous(labels = dollar,breaks = c(2,4,6,8,10)) +
  # coord_cartesian(ylim=c(0,8)) +
  # xlab("Observed Base Premium (000s)") + 
  # ylab("Optimal Base Premium (000s)") + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_rect(color=grey(.5)),
    legend.title=element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "none",
    axis.title=element_text(size=14),
    axis.text = element_text(size=16))
