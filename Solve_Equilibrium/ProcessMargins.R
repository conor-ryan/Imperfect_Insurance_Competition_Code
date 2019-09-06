rm(list = ls())
library(data.table)
library(nleqslv)
library(ggplot2)
library(scales)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")


## Estimation Run 
run = "2019-09-05"
spec = "FOC_TEST_RUN"

#Load Product Data
predFile = paste("Simulation_Risk_Output/prodData.rData",sep="")
load(predFile)


eqFile = paste("Estimation_Output/checkMargins_",spec,"-",run,".csv",sep="")
eqData = as.data.table(read.csv(eqFile))

# eqFile = paste("Estimation_Output/checkMargins_",run,".csv",sep="")
# eqData = as.data.table(read.csv(eqFile))
# eqData[,MR:=P_obs-Mkup]

prod_data = merge(prod_data,eqData,by="Product")

ggplot(prod_data) + aes(y=MC_RA,x=MR) +
  geom_point(size=1.5,alpha=0.5) +
  geom_abline(intercept=0,slope=1) +
  geom_smooth(color="red",method="lm",se=FALSE)

# firms = prod_data[,list(R_avg=sum(share_base*size*R_avg)/sum(share_base*size),
#                         Revenue=sum(share_base*size*premBase*ageRate)/sum(share_base*size)),
#                   by=c("ST","Firm")]
# baseData = as.data.table(read.csv("Intermediate_Output/Simulated_BaseData/simchoiceData_discrete.csv"))



#### Market Size ####
load("Intermediate_Output/Simulated_BaseData/simMarketSize.rData")
prod_data = merge(prod_data,marketSize,by="Market")
prod_data[,share_base:=lives/size]

## Test against moments
share_moment = as.data.table(read.csv("Intermediate_Output/Estimation_Data/marketDataMap_discrete.csv"))
share_moment[,Metal_std:=gsub(" .*","",METAL)]
share_moment[,Product_std:=min(Product),by=c("Firm","Metal_std","Market")]
share_moment = share_moment[,list(Share=sum(Share)),by=c("Product_std","Firm","Market")]
share_moment[,unins_rate:=1-sum(Share),by="Market"]



prod_data = merge(prod_data,share_moment[,c("Product_std","unins_rate","Share")],by.x=c("Product"),by.y="Product_std",all=TRUE)

prod_data[,unins_base:=1-sum(share_base),by="Market"]
unins_test = prod_data[,list(unins_base=1-sum(share_base)),by=c("Market","size","unins_rate")]

ggplot(unins_test) + aes(y=unins_rate,x=unins_base) +
  geom_point(size=1.5,alpha=0.5) + 
  geom_abline(intercept=0,slope=1) + 
  geom_smooth(color="red",method="lm",se=FALSE) + 
  ylab("Data") + 
  xlab("Prediction") + 
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

ggplot(prod_data) + aes(y=Share,x=share_base) +
  geom_point(size=1.5,alpha=0.5) + 
  geom_abline(intercept=0,slope=1) + 
  geom_smooth(color="red",method="lm",se=FALSE) + 
  ylab("Data") + 
  xlab("Prediction") + 
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



#### Merge Firm Costs ####
load("Intermediate_Output/Average_Claims/AvgCostMoments.rData")
firmClaims = firmClaims[,c("ST","Firm","AvgCost","prjAvgCost","expAvgCost")]
names(firmClaims) = c("ST","Firm","FirmAvgCost","prjFirmCost","expFirmCost")

fMom = read.csv("Intermediate_Output/MC_Moments/firmMoments.csv")
fMom = unique(fMom[,c("logAvgCost","M_num","Product")])

prod_data = merge(prod_data,firmClaims,by=c("ST","Firm"),all.x=TRUE)
prod_data = merge(prod_data,fMom,by="Product",all.x=TRUE)
setkey(prod_data,Market,Firm,AV_std)


firm_test = prod_data[Metal_std!="PLATINUM",list(avgCost=sum(avgCost*lives)/sum(lives),
                            pooledCost=sum(pooledCost*lives)/sum(lives),
                            avgRev=sum(P_obs*ageRate*lives)/sum(lives),
                            # avgR = sum(avgR*lives)/sum(lives),
                            lives=sum(lives)),
                      by=c("ST","Firm","FirmAvgCost","prjFirmCost","HighRisk","logAvgCost")]
firm_test[,share:=lives/sum(lives),by="ST"]
firm_test[,avgTransfer:=avgCost-pooledCost]
firm_test[,target:=exp(logAvgCost)]
prod_data[,MR:=P_obs-Mkup]
## Best Fit MR
# prod_data[Metal_std!="PLATINUM",firmAvgEst:=sum(avgCost*lives)/sum(lives),by=c("Firm","ST")]
# prod_data[,adj:=prjFirmCost/firmAvgEst]

res_std = prod_data[,glm(MC_std~MR,weights=lives)]
res_RA = prod_data[,glm(MC_RA~MR,weights=lives)]
summary(res_std)
summary(res_RA)
prod_data[,MC_fit_std:=predict(res_std)]
prod_data[,MC_fit_RA:=predict(res_RA)]


#### Plot Margin Check ####
# png("Writing/Images/marginCheckBase.png",width=2000,height=1500,res=275)
plot = ggplot(prod_data[,]) + aes(x=MR,y=MC_std) +
  geom_point(size=1.5,alpha=0.5) + 
  geom_abline(intercept=0,slope=1) + 
  geom_smooth(color="red",method="lm",se=FALSE) + 
  geom_line(aes(y=MC_fit_std),color="blue") +
  # scale_x_continuous(labels = dollar,breaks = c(2,4,6,8,10)) +
  # scale_y_continuous(labels = dollar,breaks = c(2,4,6,8,10)) +
  # coord_cartesian(ylim=c(-250,500),xlim=c(0,1500)) +
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
ggplot(prod_data[,]) + aes(x=MR,y=MC_RA) +
  geom_point(size=1.5,alpha=0.5) + 
  geom_abline(intercept=0,slope=1) + 
  geom_smooth(color="red",method="lm",se=FALSE) + 
  geom_line(aes(y=MC_fit_RA),color="blue") +
  # scale_x_continuous(labels = dollar,breaks = c(2,4,6,8,10)) +
  # scale_y_continuous(labels = dollar,breaks = c(2,4,6,8,10)) +
  # coord_cartesian(ylim=c(-250,500),xlim=c(0,1500)) +
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

ggplot(prod_data[,]) + aes(y=MC_std,x=MC_RA) +
  geom_point(size=1.5,alpha=0.5) + 
  geom_abline(intercept=0,slope=1) + 
  geom_smooth(color="red",method="lm",se=FALSE) + 
  # scale_x_continuous(labels = dollar,breaks = c(2,4,6,8,10)) +
  # scale_y_continuous(labels = dollar,breaks = c(2,4,6,8,10)) +
  # coord_cartesian(ylim=c(-250,500),xlim=c(0,1500)) +
  xlab("Risk Adjusted Marginal Cost") + 
  ylab("Incurred Marginal Cost") + 
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

ggplot(prod_data[,]) + aes(y=MC_std,x=avgCost) +
  geom_point(size=1.5,alpha=0.5) + 
  geom_abline(intercept=0,slope=1) + 
  geom_smooth(color="red",method="lm",se=FALSE) + 
  # scale_x_continuous(labels = dollar,breaks = c(2,4,6,8,10)) +
  # scale_y_continuous(labels = dollar,breaks = c(2,4,6,8,10)) +
  # coord_cartesian(ylim=c(-250,500),xlim=c(0,1500)) +
  xlab("Average Cost") + 
  ylab("Incurred Marginal Cost") + 
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