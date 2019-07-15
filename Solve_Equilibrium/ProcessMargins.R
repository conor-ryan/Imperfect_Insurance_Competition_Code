rm(list = ls())
library(data.table)
library(nleqslv)
library(ggplot2)
library(scales)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")


## Estimation Run 
run = "2019-07-14"

#Load Product Data
predFile = paste("Simulation_Risk_Output/prodData.rData",sep="")
load(predFile)


eqFile = paste("Estimation_Output/checkMargins_",run,".csv",sep="")
eqData = as.data.table(read.csv(eqFile))

prod_data = merge(prod_data,eqData,by="Product")

# firms = prod_data[,list(R_avg=sum(share_base*size*R_avg)/sum(share_base*size),
#                         Revenue=sum(share_base*size*premBase*ageRate)/sum(share_base*size)),
#                   by=c("ST","Firm")]
baseData = read.csv("Intermediate_Output/Simulated_BaseData/simchoiceData_discrete.csv")



#### Market Size ####
load("Intermediate_Output/Simulated_BaseData/simMarketSize.rData")
prod_data = merge(prod_data,marketSize,by="Market")
prod_data[,share_base:=lives/size]

## Test against moments
share_moment = as.data.table(read.csv("Intermediate_Output/Estimation_Data/marketDataMap_discrete.csv"))
share_moment[,Metal_std:=gsub(" .*","",METAL)]
share_moment[,Product_std:=min(Product),by=c("Firm","Metal_std","Market")]
share_moment = share_moment[,list(Share=sum(Share)),by=c("Product_std","Firm","Market","unins_rate")]



prod_data = merge(prod_data,share_moment[,c("Product_std","unins_rate","Share")],by.x=c("Product"),by.y="Product_std",all=TRUE)

prod_data[,plot(share_base,Share)]
prod_data[,unins_base:=1-sum(share_base),by="Market"]
unins_test = prod_data[,list(unins_base=1-sum(share_base)),by=c("Market","size","unins_rate")]

prod_data[,plot(unins_base,unins_rate)]




#### Merge Firm Costs ####
load("Intermediate_Output/Average_Claims/AvgCostMoments.rData")
firmClaims = firmClaims[,c("ST","Firm","AvgCost","prjAvgCost","expAvgCost")]
names(firmClaims) = c("ST","Firm","FirmAvgCost","prjFirmCost","expFirmCost")

fMom = read.csv("Intermediate_Output/MC_Moments/firmMoments.csv")
fMom = unique(fMom[,c("logAvgCost","M_num","Product")])

prod_data = merge(prod_data,firmClaims,by=c("ST","Firm"))
prod_data = merge(prod_data,fMom,by="Product")
prod_data[,MR:=premBase-Mkup]
setkey(prod_data,Market,Firm,AV_std)


firm_test = prod_data[,list(avgCost=sum(avgCost*lives)/sum(lives),
                            pooledCost=sum(pooledCost*lives)/sum(lives),
                            avgRev=sum(P_obs*ageRate*lives)/sum(lives),
                            avgR = sum(avgR*lives)/sum(lives),
                            lives=sum(lives)),
                      by=c("Market","Firm","FirmAvgCost","prjFirmCost","HighRisk")]
firm_test[,share:=lives/sum(lives),by="Market"]


#### Plot Margin Check ####
# png("Writing/Images/marginCheckBase.png",width=2000,height=1500,res=275)
plot = ggplot(prod_data) + aes(y=MR,x=MC_std) +
  geom_point() + 
  geom_abline(intercept=0,slope=1) + 
  geom_smooth(color="red",method="lm",se=FALSE) + 
  # scale_x_continuous(labels = dollar,breaks = c(2,4,6,8,10)) +
  # scale_y_continuous(labels = dollar,breaks = c(2,4,6,8,10)) +
  # coord_cartesian(ylim=c(0,6)) +
  ylab("Marginal Revenue") + 
  xlab("Marginal Cost") + 
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
# dev.off()

# png("Writing/Images/marginCheckRA.png",width=2000,height=1500,res=275)
ggplot(prod_data) + aes(x=MR,y=MC_RA) +
  geom_point() + 
  geom_abline(intercept=0,slope=1) + 
  geom_smooth(color="red",method="lm",se=FALSE) + 
  # scale_x_continuous(labels = dollar,breaks = c(2,4,6,8,10)) +
  # scale_y_continuous(labels = dollar,breaks = c(2,4,6,8,10)) +
  # coord_cartesian(ylim=c(0,8)) +
  xlab("Marginal Cost") + 
  ylab("Marginal Revenue") + 
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
# dev.off()
