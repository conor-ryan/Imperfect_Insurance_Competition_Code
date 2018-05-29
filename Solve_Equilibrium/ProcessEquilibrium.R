rm(list = ls())
library(data.table)
library(nleqslv)
library(ggplot2)
library(scales)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

run = "2018-05-12"

#### Read in Data ####
predFile = paste("Simulation_Risk_Output/predData_",run,".rData",sep="")
load(predFile)


#### Load Equilibrium Solutions ####
eqFiles = list.files("Estimation_Output")[grep("solvedEquil",list.files("Estimation_Output"))]

prod_data = prod_data[Firm!="OTHER",]
prod_data[,prem_pred:= vector(mode="double",nrow(prod_data))]
prod_data[,prem_pred_noT:= vector(mode="double",nrow(prod_data))]
setkey(prod_data,Product)
for (file in eqFiles){
  temp = read.csv(paste("Estimation_Output/",file,sep=""))
  temp = temp[order(temp$Products),]
  no_transfer = grepl("no_t",file)
  
  if(no_transfer){
    prod_data[Product%in%temp$Products,prem_pred_noT:= temp$Prices]
  }else{
    prod_data[Product%in%temp$Products,prem_pred:= temp$Prices]
  }
}


### Margin Check

focFiles = list.files("Estimation_Output")[grep("focMargin",list.files("Estimation_Output"))]

prod_data[,prem_FOC:= vector(mode="double",nrow(prod_data))]
setkey(prod_data,Product)
for (file in focFiles){
  temp = read.csv(paste("Estimation_Output/",file,sep=""))
  temp = temp[order(temp$Products),]
  prod_data[Product%in%temp$Products,prem_FOC:= temp$Price_FOC]
}


#### Calculate Market Shares / Equilibrium Distribution ####

## Monthly Alpha
full_predict[,alpha:=alpha*12/1000]

## Set Prices and Recalculate Shares
full_predict = merge(full_predict,prod_data[,c("Product","prem_pred","prem_pred_noT")],by="Product")
setkey(full_predict,Person,d_ind,Product)
full_predict[,s_Eq_pred:=vector(mode="double",nrow(full_predict))]
full_predict[,s_Eq_pred_noT:=vector(mode="double",nrow(full_predict))]

for (var in c("","_noT")){
  pvar = paste("prem_pred",var,sep="")
  paidvar = paste("prem_paid",var,sep="")
  svar = paste("s_Eq_pred",var,sep="")
  full_predict[,Price_new:=.SD*ageRate-subsidy,.SDcol=pvar]
  full_predict[,Price_new:=pmax(Price_new,0)]
  full_predict[,Price_new:= Price_new/MEMBERS]
  full_predict[METAL=="CATASTROPHIC",Price_new:= (.SD*ageRate)/MEMBERS,.SDcol=pvar]
  full_predict[,c(paidvar):=Price_new]
  full_predict[,Price_new:= (Price_new-Mandate/12)]
  full_predict[,util:=non_price_util*exp(alpha*Price_new)]
  full_predict[,exp_sum:=sum(util),by=c("Person","d_ind")]
  full_predict[,c(svar):=util/(1+exp_sum)]
  full_predict[,c("Price_new","util","exp_sum"):=NULL]
}



prod_pred = full_predict[,list(S_Est = sum(s_pred*mkt_density),
                               S_Eq = sum(s_Eq_pred*mkt_density),
                               R_Eq = sum(s_Eq_pred*R*mkt_density)/sum(s_Eq_pred*mkt_density),
                               AV_Eq = sum(s_Eq_pred*AV*mkt_density)/sum(s_Eq_pred*mkt_density),
                               S_Eq_noT = sum(s_Eq_pred_noT*mkt_density)),
                         by=c("Product")]

prod_pred = merge(prod_pred,prod_data,by="Product")
prod_pred[,R_Eq:=R_Eq/R_bench]

# Age Fixed Effects
full_predict[,AgeFE:="18 to 30"]
full_predict[AGE>30&AGE<=40,AgeFE:="31 to 40"]
full_predict[AGE>40&AGE<=50,AgeFE:="41 to 50"]
full_predict[AGE>50,AgeFE:="51 to 64"]

full_predict[,WTP_qrt:=1]
full_predict[WTP>=(-0.1955)&WTP<(0.1568),WTP_qrt:=2]
full_predict[WTP>=(0.1568)&WTP<(0.5225),WTP_qrt:=3]
full_predict[WTP>=(0.5225),WTP_qrt:=4]

by_age = full_predict[,list(P_base = sum(s_Eq_pred*prem_paid*mkt_density)/sum(s_Eq_pred*mkt_density),
                            P_base_2 = sum(s_Eq_pred_noT*prem_paid*mkt_density)/sum(s_Eq_pred_noT*mkt_density),
                            P_noT_1 = sum(s_Eq_pred*prem_paid_noT*mkt_density)/sum(s_Eq_pred*mkt_density),
                            P_noT_2  = sum(s_Eq_pred_noT*prem_paid_noT*mkt_density)/sum(s_Eq_pred_noT*mkt_density)),
                      by=c("AgeFE")]

by_wtp = full_predict[,list(P_base = sum(s_Eq_pred*prem_paid*mkt_density)/sum(s_Eq_pred*mkt_density),
                            P_base_2 = sum(s_Eq_pred_noT*prem_paid*mkt_density)/sum(s_Eq_pred_noT*mkt_density),
                            P_noT_1 = sum(s_Eq_pred*prem_paid_noT*mkt_density)/sum(s_Eq_pred*mkt_density),
                            P_noT_2  = sum(s_Eq_pred_noT*prem_paid_noT*mkt_density)/sum(s_Eq_pred_noT*mkt_density)),
                      by=c("WTP_qrt")]
setkey(by_wtp,WTP_qrt)

by_wtp_90 = full_predict[WTP>0.876,list(P_base = sum(s_Eq_pred*prem_paid*mkt_density)/sum(s_Eq_pred*mkt_density),
                            P_noT_1 = sum(s_Eq_pred*prem_paid_noT*mkt_density)/sum(s_Eq_pred*mkt_density),
                            P_noT_2  = sum(s_Eq_pred_noT*prem_paid_noT*mkt_density)/sum(s_Eq_pred_noT*mkt_density))]
by_wtp_95 = full_predict[WTP>1.124,list(P_base = sum(s_Eq_pred*prem_paid*mkt_density)/sum(s_Eq_pred*mkt_density),
                                     P_noT_1 = sum(s_Eq_pred*prem_paid_noT*mkt_density)/sum(s_Eq_pred*mkt_density),
                                     P_noT_2  = sum(s_Eq_pred_noT*prem_paid_noT*mkt_density)/sum(s_Eq_pred_noT*mkt_density))]
by_wtp_99 = full_predict[WTP>1.64,list(P_base = sum(s_Eq_pred*prem_paid*mkt_density)/sum(s_Eq_pred*mkt_density),
                                     P_noT_1 = sum(s_Eq_pred*prem_paid_noT*mkt_density)/sum(s_Eq_pred*mkt_density),
                                     P_noT_2  = sum(s_Eq_pred_noT*prem_paid_noT*mkt_density)/sum(s_Eq_pred_noT*mkt_density))]






#### Assess Results ####
prod_pred[,diff:=(prem_pred_noT-prem_pred)/prem_pred]
prod_pred[,diff_abs:=(prem_pred_noT-prem_pred)]
prod_pred[,S_f:=sum(S_Eq),by=c("Firm","Market")]

prod_pred[S_Eq>1e-5,plot(prem_pred,prem_pred_noT)]
prod_pred[S_Eq>1e-5,plot(prem_pred,diff)]
prod_pred[S_Eq>1e-5,plot(AV_Eq,diff)]

prod_pred[,Metal_std:=factor(prod_pred$Metal_std,levels=c("CATASTROPHIC",
                                                          "BRONZE",
                                                          "SILVER",
                                                          "GOLD",
                                                          "PLATINUM"))]

prod_pred[S_Eq>1e-5,median(diff),by="Metal_std"]

prod_pred[,S_Eq_Inside:=S_Eq/sum(S_Eq),by="Market"]
prod_pred[,S_high:=0]
prod_pred[S_Eq>.075,S_high:=1]
prod_pred[S_Eq>1e-5,median(abs(diff)),by="S_high"]


png("Writing/Images/NoTransMetalEff.png",width=2000,height=1500,res=275)
ggplot(prod_pred[S_Eq>1e-5,]) + aes(x=Metal_std,y=-diff_abs) + 
  geom_boxplot(outlier.shape=NA) + 
  scale_y_continuous(label=dollar) + 
  coord_cartesian(ylim=c(100,-250)) + 
  xlab("") + 
  ylab("Effect on Base Premiums") + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_rect(color=grey(.5)),
    legend.title=element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "bottom",
    axis.title=element_text(size=12),
    axis.text = element_text(size=12))
dev.off()

png("Writing/Images/NoTransShareEff.png",width=2000,height=1500,res=275)
ggplot(prod_pred[S_Eq>1e-5,]) + aes(x=S_Eq_Inside,y=-diff_abs) +
  geom_point(alpha=.6) + 
  #geom_smooth(method="loess") + 
  scale_x_continuous(label=percent) + 
  scale_y_continuous(label=dollar) + 
  xlab("Product Market Share") + 
  ylab("Effect on Base Premium") + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_rect(color=grey(.5)),
    legend.title=element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "none",
    axis.title=element_text(size=12),
    axis.text = element_text(size=12))
dev.off()


#### Plot Margin Check ####


png("Writing/Images/marginCheck.png",width=2000,height=1500,res=275)
ggplot(prod_pred[,]) + aes(x=premBase,y=prem_FOC) +
  geom_point() + 
  geom_abline(intercept=0,slope=1) + 
  geom_smooth(color="red",method="lm",se=FALSE) + 
  # scale_x_continuous(limits=c(50,600)) + 
  # scale_y_continuous(limits=c(50,600)) + 
  xlab("Observed Base Premium") + 
  ylab("FOC Implied Base Premium") + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_rect(color=grey(.5)),
    legend.title=element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "none",
    axis.title=element_text(size=12),
    axis.text = element_text(size=12))
dev.off()

prod_pred[S_Est>.01,lm(prem_FOC~premBase)]
