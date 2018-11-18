rm(list = ls())
library(data.table)
library(nleqslv)
library(ggplot2)
library(scales)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

run = "2018-08-25"

#### Read in Data ####
predFile = paste("Simulation_Risk_Output/predData_",run,".rData",sep="")
load(predFile)

full_predict[,names(full_predict)[grepl("(FE_|PriceDiff|MedDeduct|MedOOP|Product_Name|Big|HCC_age|Any_HCC|HHincomeFPL|Family|Age|LowIncome|AGE|Rtype|WTP)",
                                        names(full_predict))]:=NULL]
gc()
## Cost Function 
costFile = paste("Simulation_Risk_Output/costParameters_MSM_Stage2_",run,".rData",sep="")
load(costFile)

#cost_par = CostRes$coefficients[grep("(Age|WTP)",names(CostRes$coefficients))]

#### Load Equilibrium Solutions ####
eqFiles = list.files("Estimation_Output")[grep("solvedEquil.*_RA_Man2_.*csv",list.files("Estimation_Output"))]

prod_data = prod_data[Firm!="OTHER",]
setkey(prod_data,Product)
setkey(full_predict,Product)
for (file in eqFiles){
  print(file)
  temp = read.csv(paste("Estimation_Output/",file,sep=""))
  temp = temp[order(temp$Products),]
  #no_transfer = grepl("no_t",file)

  prod_data[Product%in%temp$Products,prem_base:= temp$Price_base]
  prod_data[Product%in%temp$Products,prem_RA:= temp$Price_RA]
  prod_data[Product%in%temp$Products,prem_RAtau:= temp$Price_RA_p]
  prod_data[Product%in%temp$Products,prem_man:= temp$Price_base_man]
  prod_data[Product%in%temp$Products,prem_RA_man:= temp$Price_RA_man]
  prod_data[Product%in%temp$Products,prem_RAtau_man:= temp$Price_RA_p_man]
  
  n = 0
  for (j in unique(temp$Products)){
    n = n+1
    full_predict[.(j),prem_base:= temp$Price_base[n]]
    full_predict[.(j),prem_RA:= temp$Price_RA[n]]
    full_predict[.(j),prem_RAtau:= temp$Price_RA_p[n]]
    full_predict[.(j),prem_man:= temp$Price_base_man[n]]
    full_predict[.(j),prem_RA_man:= temp$Price_RA_man[n]]
    full_predict[.(j),prem_RAtau_man:= temp$Price_RA_p_man[n]]
  }
  # 
  # if(no_transfer){
  #   prod_data[Product%in%temp$Products,prem_pred_noT:= temp$Prices]
  # }else{
  #   prod_data[Product%in%temp$Products,prem_pred:= temp$Prices]
  # }
}


### Margin Check

focFiles = list.files("Estimation_Output")[grep("focMargin",list.files("Estimation_Output"))]

prod_data[,prem_FOC:= vector(mode="double",nrow(prod_data))]
setkey(prod_data,Product)
for (file in focFiles){
  temp = read.csv(paste("Estimation_Output/",file,sep=""))
  temp = temp[order(temp$Products),]
  prod_data[Product%in%temp$Products,prem_FOC_base:= temp$Price_Std]
  prod_data[Product%in%temp$Products,prem_FOC_RA:= temp$Price_RA]
}

#### Calculate Base Model Market Shares ####
## Monthly Alpha
full_predict[,alpha:=alpha*12/1000]
full_predict[,Mandate:=Mandate/12]


## Set Prices and Recalculate Shares

full_predict[,s_base:=vector(mode="double",nrow(full_predict))]
full_predict[,s_man:=vector(mode="double",nrow(full_predict))]
full_predict[,s_RA:=vector(mode="double",nrow(full_predict))]
full_predict[,s_RAtau:=vector(mode="double",nrow(full_predict))]
full_predict[,CW_base:=vector(mode="double",nrow(full_predict))]
full_predict[,CW_man:=vector(mode="double",nrow(full_predict))]
full_predict[,CW_RA:=vector(mode="double",nrow(full_predict))]
full_predict[,CW_RAtau:=vector(mode="double",nrow(full_predict))]

setkey(full_predict,Person,d_ind,Product)
for (var in c("base","man","RA","RAtau")){
  print(var)
  pvar = paste("prem",var,sep="_")
  svar = paste("s",var,sep="_")
  CWvar = paste("CW",var,sep="_")
  Insvar = paste("ins",var,sep="_")

  ## Re-Calculate Benchmark and Subsidies
  print("Benchmark Calculation")
  #setkey(full_predict,Product)
  prod_data$rank=ave(prod_data[[pvar]],with(prod_data,paste(Metal_std,Market)),FUN=rank)
  benchmark = prod_data[Metal_std=="SILVER",]
  benchmark[,maxrank:=max(rank),by="Market"]
  benchmark = benchmark[rank==2 | (rank==1&maxrank==1),]
  benchmark = benchmark[,c(.SD),.SDcol=c("Product",pvar)]
  names(benchmark) = c("Product","Benchmark")
  print(nrow(benchmark))

  for (j in unique(benchmark$Product)){
    full_predict[Product==j,Benchmark:=benchmark$Benchmark[benchmark$Product==j]]
  }
  full_predict[,Benchmark:=max(Benchmark,na.rm=TRUE),by="Market"]
  full_predict[,subsidy:=pmax(Benchmark*ageRate - IncomeCont,0)]


  print("Share Calculation")
  full_predict[,Price_new:=.SD*ageRate-subsidy,.SDcol=pvar]
  full_predict[,Price_new:=pmax(Price_new,0)]
  full_predict[,Price_new:= Price_new/MEMBERS]
  full_predict[METAL=="CATASTROPHIC",Price_new:= (.SD*ageRate)/MEMBERS,.SDcol=pvar]
  full_predict[,util:=non_price_util*exp(alpha*Price_new)]
  full_predict[,exp_sum:=sum(util),by=c("Person","d_ind")]
  full_predict[,c(svar):=util/(exp(alpha*Mandate)+exp_sum)]
  full_predict[,c(Insvar):=exp_sum/(exp(alpha*Mandate)+exp_sum)]
  if(var=="man"){
    full_predict[,c(CWvar):=-log(exp_sum + 1)/alpha]
  }else{
    full_predict[,c(CWvar):=-log(exp_sum+ exp(alpha*(Mandate)))/alpha]
  }
  full_predict[,c("Price_new","util","exp_sum","Benchmark"):=NULL]
}



#### Preliminary Results ####
Welfare = unique(full_predict[,c("Person","d_ind","PERWT","HCC_Silver","ST","CW_base","CW_man","CW_RA","CW_RAtau",
                                 "ins_base","ins_man","ins_RA","ins_RAtau")])


prod_pred = full_predict[,list(lives = sum(s_base*PERWT),
                               S_est = sum(s_base*mkt_density),
                               Age_Avg = sum(s_base*ageRate_avg*mkt_density)/sum(s_base*mkt_density),
                               C_j = sum(C*s_base*PERWT)/sum(s_base*PERWT)),
                         by=c("Product")]

prod_pred = merge(prod_pred,prod_data,by="Product")

prod_pred[,ST_insured:=sum(lives),by="ST"]
prod_pred[,ST_share:=lives/ST_insured,by="ST"]
prod_pred[,Mkt_insured:=sum(lives),by="Market"]
prod_pred[,Mkt_share:=lives/Mkt_insured,by="Market"]

prod_pred[,FirmShare:=sum(ST_share),by=c("ST","Firm")]
firms = unique(prod_pred[,c("ST","Firm","FirmShare")])
firms[,HHI:= sum((FirmShare*100)^2),by="ST"]
prod_pred = merge(prod_pred,firms,by=c("ST","Firm"))

### HHI Table
prod_pred[,HHI_flag:=0]
prod_pred[HHI>3000,HHI_flag:=1]
prod_pred[HHI>6000,HHI_flag:=2]

table_prem = prod_pred[,list(prem_base = 12/1000*sum(lives*Age_Avg*prem_base)/sum(lives),
                        prem_RA = 12/1000*sum(lives*Age_Avg*prem_RA)/sum(lives),
                        prem_man = 12/1000*sum(lives*Age_Avg*prem_man)/sum(lives),
                        prem_RAtau = 12/1000*sum(lives*Age_Avg*prem_RAtau)/sum(lives)),by=c("Metal_std","HHI_flag")]
table_prem[,Metal_std:=factor(Metal_std,levels=c("CATASTROPHIC","BRONZE","SILVER","GOLD","PLATINUM"))]
setkey(table_prem,HHI_flag,Metal_std)
table_prem = table_prem[!Metal_std%in%c("CATASTROPHIC","PLATINUM"),]

Welfare = merge(Welfare,unique(firms[,c("ST","HHI")]),by="ST")
Welfare[,HHI_flag:=0]
Welfare[HHI>3000,HHI_flag:=1]
Welfare[HHI>6000,HHI_flag:=2]

Welfare[,HighRisk:=0]
Welfare[HCC_Silver>10,HighRisk:=1]

Welfare[,LowRisk:=0]
Welfare[HCC_Silver==0,LowRisk:=1]

table_CW = Welfare[,list(CW_base=12/1000*sum(CW_base*PERWT)/sum(PERWT),
                           CW_man=sum(CW_man*PERWT)/sum(PERWT),
                           CW_RA=sum(CW_RA*PERWT)/sum(PERWT),
                           CW_RAtau=sum(CW_RAtau*PERWT)/sum(PERWT),
                         ins_base=sum(ins_base*PERWT)/sum(PERWT),
                         ins_man=sum(ins_man*PERWT)/sum(PERWT),
                         ins_RA=sum(ins_RA*PERWT)/sum(PERWT),
                         ins_RAtau=sum(ins_RAtau*PERWT)/sum(PERWT)),by=c("HHI_flag","HighRisk","LowRisk")]

table_CW_avg = Welfare[,list(CW_base=12/1000*sum(CW_base*PERWT)/sum(PERWT),
                         CW_RA=12/1000*sum(CW_RA*PERWT)/sum(PERWT),
                         CW_man=12/1000*sum(CW_man*PERWT)/sum(PERWT),
                         CW_RAtau=sum(CW_RAtau*PERWT)/sum(PERWT),
                         ins_base=sum(ins_base*PERWT)/sum(PERWT),
                         ins_RA=sum(ins_RA*PERWT)/sum(PERWT),
                         ins_man=sum(ins_man*PERWT)/sum(PERWT),
                         ins_RAtau=sum(ins_RAtau*PERWT)/sum(PERWT)),by=c("HHI_flag")]

#### Calculate Market Shares / Equilibrium Distribution ####







prod_pred = full_predict[,list(S_Est = sum(s_pred*mkt_density),
                               dSdp = sum(alpha*ageRate_avg*s_pred*(1-s_pred)*mkt_density),
                               C_j = sum(C*s_pred*mkt_density)/sum(s_pred*mkt_density),
                               AMC_j = sum(C*alpha*ageRate_avg*s_pred*(1-s_pred)*mkt_density)/
                                 sum(alpha*ageRate_avg*s_pred*(1-s_pred)*mkt_density),
                               # Age_j = sum(AGE*s_pred*mkt_density)/sum(s_pred*mkt_density),
                               # dAge_j = sum(AGE*alpha*ageRate_avg*s_pred*(1-s_pred)*mkt_density),
                               # WTP_j = sum(WTP*s_pred*mkt_density)/sum(s_pred*mkt_density),
                               # dWTP_j = sum(WTP*alpha*ageRate_avg*s_pred*(1-s_pred)*mkt_density),
                               S_Eq = sum(s_Eq_pred*mkt_density),
                               C_Eq_j = sum(C*s_Eq_pred*mkt_density)/sum(s_Eq_pred*mkt_density),
                               AMC_Eq_j = sum(C*alpha*ageRate_avg*s_Eq_pred*(1-s_Eq_pred)*mkt_density)/
                                 sum(alpha*ageRate_avg*s_Eq_pred*(1-s_Eq_pred)*mkt_density),
                               R_Eq = sum(s_Eq_pred*R*mkt_density)/sum(s_Eq_pred*mkt_density),
                               AV_Eq = sum(s_Eq_pred*AV*mkt_density)/sum(s_Eq_pred*mkt_density),
                               S_Eq_noT = sum(s_Eq_pred_noT*mkt_density),
                               S_Eq_fixedT = sum(s_Eq_pred_fixedT*mkt_density)),
                         by=c("Product")]

prod_pred = merge(prod_pred,prod_data,by="Product")
prod_pred[,R_Eq:=R_Eq/R_bench]


#### Illustrative Points ####
## Product Costs
# prod_pred[,C_j:=exp(predict(CostRes,newdata=prod_pred))]
# 
# prod_pred[,dAge_j:=  dAge_j/S_Est - dSdp/S_Est*Age_j]
# prod_pred[,dWTP_j:=  dWTP_j/S_Est - dSdp/S_Est*WTP_j]
# 
# prod_pred[,dC_j:= (dAge_j*cost_par[1] + dWTP_j*cost_par[2])*C_j]
#
# prod_pred[,own_marg_cost:=(dSdp*C_j + dC_j*S_Est)/dSdp]


## Markup 
prod_pred[,markup:= - S_Est/dSdp]

## Pooled Cost
prod_pred[,pooled_cost:=sum(S_Est*C_j/AV_std)/sum(S_Est),by="ST"]

## Average at the state level. Markets given equal weighting within states.  
state_avg = prod_pred[,list(markup=sum(markup*S_Est)/sum(S_Est),
                            own_marg_cost=sum(own_marg_cost*S_Est)/sum(S_Est),
                            share = sum(S_Est),
                            pooled_cost=sum(pooled_cost*S_Est)/sum(S_Est)),by=c("ST","Metal_std","AV_std")]
state_avg[,share:=share/sum(share),by="ST"]


setkey(state_avg,ST,Metal_std)
all_avg = state_avg[,list(markup=mean(markup),
                          own_marg_cost=mean(own_marg_cost),
                          pooled_cost=mean(pooled_cost)),by=c("Metal_std","AV_std")]
all_avg[,pooled_cost:=pooled_cost*AV_std]


#### RA Results Tables ####

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
                            P_noT_1 = sum(s_Eq_pred*prem_paid_noT*mkt_density)/sum(s_Eq_pred*mkt_density),
                            P_noT_2  = sum(s_Eq_pred_noT*prem_paid_noT*mkt_density)/sum(s_Eq_pred_noT*mkt_density),
                            P_fixedT_1 = sum(s_Eq_pred*prem_paid_fixedT*mkt_density)/sum(s_Eq_pred*mkt_density),
                            P_fixedT_2  = sum(s_Eq_pred_fixedT*prem_paid_fixedT*mkt_density)/sum(s_Eq_pred_fixedT*mkt_density)),
                      by=c("AgeFE")]

by_wtp = full_predict[,list(P_base = sum(s_Eq_pred*prem_paid*mkt_density)/sum(s_Eq_pred*mkt_density),
                            P_noT_1 = sum(s_Eq_pred*prem_paid_noT*mkt_density)/sum(s_Eq_pred*mkt_density),
                            P_noT_2  = sum(s_Eq_pred_noT*prem_paid_noT*mkt_density)/sum(s_Eq_pred_noT*mkt_density),
                            P_fixedT_1 = sum(s_Eq_pred*prem_paid_fixedT*mkt_density)/sum(s_Eq_pred*mkt_density),
                            P_fixedT_2  = sum(s_Eq_pred_fixedT*prem_paid_fixedT*mkt_density)/sum(s_Eq_pred_fixedT*mkt_density)),
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
prod_pred[,diff:=(prem_pred_fixedT-prem_pred)/prem_pred]
prod_pred[,diff_abs:=(prem_pred_fixedT-prem_pred)]
prod_pred[,S_f:=sum(S_Eq),by=c("Firm","Market")]

prod_pred[S_Eq>1e-5,plot(prem_pred,prem_pred_fixedT)]
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


png("Writing/Images/fixedTransMetalEff.png",width=2000,height=1500,res=275)
ggplot(prod_pred[S_Eq>1e-5,]) + aes(x=Metal_std,y=-diff_abs) + 
  geom_boxplot(outlier.shape=NA) + 
  scale_y_continuous(label=dollar) + 
  #coord_cartesian(ylim=c(100,-250)) + 
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

png("Writing/Images/fixedTransShareEff.png",width=2000,height=1500,res=275)
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


png("Writing/Images/marginCheckBase.png",width=2000,height=1500,res=275)
ggplot(prod_data[,]) + aes(y=12/1000*prem_base,x=12/1000*premBase) +
  geom_point() + 
  geom_abline(intercept=0,slope=1) + 
  geom_smooth(color="red",method="lm",se=FALSE) + 
  scale_x_continuous(labels = dollar,breaks = c(2,4,6,8,10)) +
  scale_y_continuous(labels = dollar,breaks = c(2,4,6,8,10)) +
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

png("Writing/Images/marginCheckRA.png",width=2000,height=1500,res=275)
ggplot(prod_data[,]) + aes(x=12/1000*premBase,y=12/1000*prem_RA) +
  geom_point() + 
  geom_abline(intercept=0,slope=1) + 
  geom_smooth(color="red",method="lm",se=FALSE) + 
  scale_x_continuous(labels = dollar,breaks = c(2,4,6,8,10)) +
  scale_y_continuous(labels = dollar,breaks = c(2,4,6,8,10)) +
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

ggplot(prod_data[,]) + aes(y=prem_pred_RA,x=premBase) +
  geom_point() + 
  geom_abline(intercept=0,slope=1) + 
  geom_smooth(color="red",method="lm",se=FALSE)



prod_pdre[S_Est>.01,lm(prem_FOC~premBase)]
