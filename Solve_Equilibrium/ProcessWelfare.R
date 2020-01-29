rm(list = ls())
library(data.table)
library(nleqslv)
library(ggplot2)
library(scales)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

run = "2019-12-07"
spec = "FMC"

#### Read in Base Data ####
baseDataFull = read.csv("Intermediate_Output/Simulated_BaseData/simchoiceData_discrete.csv")
baseData = as.data.table(unique(baseDataFull[,c("Person","ST","Market","PERWT","Age","AGE","ageRate","MEMBERS","Family","LowIncome")]))

#### Merge in Market Info ####
file = paste("Estimation_Output/MktHHI_",spec,"-",run,".rData",sep="")
load(file)

baseData = merge(baseData,Mkt,by="Market")
baseData[,Market_num:=as.numeric(as.factor(Market))]

#### Merge in Welfare Info ####
for (cw in c("Base","RA","RA_m","SP_cp","SP_cpm","SP_zp","SP")){
  file = paste("Estimation_Output/consumerWelfare_",cw,"-",spec,"-",run,".csv",sep="")
  data_cw = read.csv(file)
  names(data_cw)[2:4] = paste(names(data_cw)[2:4],cw,sep="_")
  baseData = merge(baseData,data_cw[1:2],by.x="Person",by.y="pers")
}


baseData[,dCW_SP_Base:=CW_Base-CW_SP]
baseData[,dCW_SP_SPzp:=CW_SP-CW_SP_zp]
baseData[,dCW_SP_SPcp:=CW_SP_zp-CW_SP_cp]
baseData[,dCW_SPcp_Base:=CW_SP_cp-CW_RA]
baseData[,dCW_RA_Base:=CW_Base-CW_RA]

baseData[,dCW_RA:=(CW_RA_m-CW_RA)]
# CW_RA = CW_SP_cp + (CW_RA-CW_SP_cp)
baseData[,dSort_merger:=(CW_RA_m-CW_SP_cpm) - (CW_RA-CW_SP_cp)]
baseData[,dProf_merger:=(CW_SP_cpm-CW_SP_cp)]
baseData[,dCW_merger:=dSort_merger + dProf_merger]
# 
ggplot(baseData[mergerLabel=="Strong"]) + aes(y=dSort_merger,x=CW_RA) +
  geom_point() + geom_smooth(method="lm") + facet_grid(as.factor(HHI)~as.factor(dhhi_actual))

merger.plot = as.data.table(reshape(baseData[ST=="GA",c("Market","Person","PERWT","HHI","ageRate","MEMBERS","CW_RA","dProf_merger","dSort_merger","dCW_merger")],
                      varying = c("dProf_merger","dSort_merger","dCW_merger"),
                      v.names="Welfare",
                      timevar="Source",
                      times=c("dProf_merger","dSort_merger","dCW_merger"),
                      idvar=c("Market","Person","PERWT","HHI","ageRate","MEMBERS","CW_RA"),
                      direction="long"))

merger.plot[,CW_RA_2:=CW_RA^2]
merger.plot[,CW_RA_3:=CW_RA^3]

merger.plot[,HHI_flag:=0]

merger.plot[HHI>3900,HHI_flag:=1]

merger.plot[Source=="dSort_merger",Source:="Improved Sorting"]
merger.plot[Source=="dProf_merger",Source:="Higher Markups"]
merger.plot[Source=="dCW_merger",Source:="Net Welfare Effect"]

# merger.plot[HHI_flag==0&Source=="dCW_merger",Welf_pred:=predict(lm(Welfare~CW_RA + CW_RA_2 + CW_RA_3))]
# merger.plot[HHI_flag==1&Source=="dCW_merger",Welf_pred:=predict(lm(Welfare~CW_RA + CW_RA_2 + CW_RA_3))]

merger.plot[,HHI_factor:=factor(HHI_flag,levels=c(0,1),
                              labels=c("Pre-merger HHI Less than 3900",
                             "Pre-merger HHI Greater than 3900"))]

png("Writing/Images/mergerWelfare.png",width=2500,height=1500,res=275)
ggplot(merger.plot[Source%in%c("Higher Markups","Improved Sorting")]) + 
  aes(x=CW_RA,y=Welfare,color=Source,shape=Source) + #,linetype=Source,group=Person) + 
  geom_point(size=3,alpha=0.5) +
  geom_smooth(data=merger.plot[Source=="Net Welfare Effect"],method="lm",se=FALSE) + #+ geom_smooth(method="lm",se=FALSE)  + 
  facet_wrap(~HHI_factor) +
  ylab("Change in Consumer Welfare") + 
  xlab("Pre-merger Consumer Welfare ($000s per year per person)") + 
  # labs(color = "Source of Welfare Change",shape = "Source of Welfare Change",linetype = "Source of Welfare Change") +
  # guides(shape=guide_legend(override.aes=list(size=3)),linetype=guide_legend(override.aes=list(size=1))) + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_rect(color=grey(.5)),
    legend.title = element_blank(),
    strip.text = element_text(size=14),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "bottom",
    axis.title=element_text(size=14),
    axis.text = element_text(size=16))
dev.off()


Markets = baseData[,list(dCW_RA_Base = sum(dCW_RA_Base*PERWT)/sum(PERWT),
                         dCW_SPcp_Base = sum(dCW_SPcp_Base*PERWT)/sum(PERWT),
                         dCW_SP_SPcp = sum(dCW_SP_SPcp*PERWT)/sum(PERWT),
                         dCW_SP_SPzp = sum(dCW_SP_SPzp*PERWT)/sum(PERWT)#,
                         # dSort_merger = sum(dSort_merger*PERWT)/sum(PERWT),
                         # dProf_merger = sum(dProf_merger*PERWT)/sum(PERWT)
                         ), by=c("ST","Market","HHI","HHI_flag","Market_num")]

market.plot = reshape(Markets,varying = c("dCW_RA_Base","dCW_SPcp_Base","dCW_SP_SPcp","dCW_SP_SPzp"),
                      v.names="Welfare",
                      timevar="Source",
                      times=c("Risk Adjustment","Inefficient Sorting","Equilibrium Profits","Zero Profit"),
                      idvar=c("ST","Market","HHI","Market_num"),
                      direction="long")

png("Writing/Images/crossWelfare.png",width=2500,height=1500,res=275)
ggplot(market.plot[Source%in%c("Inefficient Sorting","Equilibrium Profits","Zero Profit")]) + 
  aes(x=HHI,y=Welfare,color=Source,shape=Source,linetype=Source,group=Source) + 
  geom_point(size=3,alpha=0.8) + geom_smooth(method="loess",se=FALSE)  + 
  ylab("Welfare ($000s per year per person)") + 
  labs(color = "Source of Welfare Loss",shape = "Source of Welfare Loss",linetype = "Source of Welfare Loss") +
  # guides(shape=guide_legend(override.aes=list(size=3)),linetype=guide_legend(override.aes=list(size=1))) + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_rect(color=grey(.5)),
    legend.title = element_text(size=14),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "bottom",
    axis.title=element_text(size=14),
    axis.text = element_text(size=16))
dev.off()
