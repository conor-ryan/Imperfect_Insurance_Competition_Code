rm(list = ls())
library(data.table)
library(nleqslv)
library(ggplot2)
library(scales)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")


## Estimation Run 
run = "2019-03-12"

#Load Product Data
predFile = paste("Simulation_Risk_Output/prodData.rData",sep="")
load(predFile)



focFiles = list.files("Estimation_Output")[grep(paste("focMargin.*",run,sep=""),list.files("Estimation_Output"))]

prod_data[,prem_FOC:= vector(mode="double",nrow(prod_data))]
setkey(prod_data,Product)
for (file in focFiles){
  temp = read.csv(paste("Estimation_Output/",file,sep=""))
  temp = temp[order(temp$Products),]
  prod_data[Product%in%temp$Products,prem_FOC_base:= temp$Price_Std]
  prod_data[Product%in%temp$Products,prem_FOC_RA:= temp$Price_RA]
}



#### Plot Margin Check ####


png("Writing/Images/marginCheckBase.png",width=2000,height=1500,res=275)
plot = ggplot(prod_data[Firm!="OTHER"&Metal_std=="GOLD",]) + aes(y=12/1000*prem_FOC_base,x=12/1000*premBase) +
  geom_point() + 
  geom_abline(intercept=0,slope=1) + 
  geom_smooth(color="red",method="lm",se=FALSE) + 
  scale_x_continuous(labels = dollar,breaks = c(2,4,6,8,10)) +
  scale_y_continuous(labels = dollar,breaks = c(2,4,6,8,10)) +
  # coord_cartesian(ylim=c(0,10)) + 
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
ggplot(prod_data[,]) + aes(x=12/1000*premBase,y=12/1000*prem_FOC_RA) +
  geom_point() + 
  geom_abline(intercept=0,slope=1) + 
  geom_smooth(color="red",method="lm",se=FALSE) + 
  scale_x_continuous(labels = dollar,breaks = c(2,4,6,8,10)) +
  scale_y_continuous(labels = dollar,breaks = c(2,4,6,8,10)) +
  coord_cartesian(ylim=c(0,10)) + 
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

