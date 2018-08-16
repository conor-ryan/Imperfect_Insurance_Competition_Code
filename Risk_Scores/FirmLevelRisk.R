rm(list = ls())
library(doBy)
library(data.table)
library(nleqslv)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

## Run
run = "2018-05-12"

#### Load Simulation Data ####
simFile = paste("Simulation_Risk_Output/simData_",run,".rData",sep="")
load(simFile)


# Get Mean Firm Shares
#per_predict = predict_data[,list(s_pred_mean=mean(s_pred)),by=c("Product","Person")]
# Remove Catastrophic Plans for now. 
predict_full = acs[METAL!="CATASTROPHIC",]

# Calculate Rating Factors
predict_full[,lives:=s_pred_mean*PERWT]
predict_full[,ageR_wt:=s_pred_mean*PERWT*ageRate_avg]

pred_prods = predict_full[,lapply(.SD,sum),
                          by=c("Product","Product_Name","premBase","Firm","ST","AV","Gamma_j"),
                          .SDcols=c("lives","ageR_wt")]
pred_prods[,ARF:=ageR_wt/lives]
pred_prods[,mkt_lives:=sum(lives),by="ST"]
pred_prods[,share:=lives/mkt_lives]

##### Merge in Base Premium Information #####

# Calculate State Premiums
pred_prods[,charged_prem_avg:=ARF*premBase*12]
pred_prods[,avg_prem:=sum(charged_prem_avg*share),by="ST"]


#### Calculate Firm Level ARF ####
pred_prods[,A:=AV*ARF*Gamma_j]
pred_prods[,A_wtd:=A*share]

firm_RA = pred_prods[,lapply(.SD,sum),by=c("Firm","ST","avg_prem","mkt_lives"),.SDcols=c("A_wtd","share")]
firm_RA[,A_f:=A_wtd/share]
setkey(firm_RA,ST,Firm)

#### Read in Risk Adjustment Data ####
claims = read.csv("Data/2015_MLR/Part1_2_Summary_Data_Premium_Claims.csv")


payments = claims[claims$ROW_LOOKUP_CODE=="FED_RISK_ADJ_NET_PAYMENTS",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_Q1")]
names(payments) = c("ï..MR_SUBMISSION_TEMPLATE_ID","Payments")

payments_RC = claims[claims$ROW_LOOKUP_CODE=="FED_RISK_ADJ_NET_PAYMENTS",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_RC")]
names(payments_RC) = c("ï..MR_SUBMISSION_TEMPLATE_ID","Payments_RC")

enroll =claims[claims$ROW_LOOKUP_CODE=="NUMBER_OF_LIFE_YEARS",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_Q1")]
names(enroll) = c("ï..MR_SUBMISSION_TEMPLATE_ID","MLR_lives")

months =claims[claims$ROW_LOOKUP_CODE=="MEMBER_MONTHS",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_Q1")]
names(months) = c("ï..MR_SUBMISSION_TEMPLATE_ID","memberMonths")

months_RC =claims[claims$ROW_LOOKUP_CODE=="MEMBER_MONTHS",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_RC")]
names(months_RC) = c("ï..MR_SUBMISSION_TEMPLATE_ID","memberMonths_RC")

revenue =claims[claims$ROW_LOOKUP_CODE=="TOTAL_DIRECT_PREMIUM_EARNED",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_Q1")]
names(revenue) = c("ï..MR_SUBMISSION_TEMPLATE_ID","Revenue")


RA_claims = merge(payments,enroll,by="ï..MR_SUBMISSION_TEMPLATE_ID")
RA_claims = merge(RA_claims,payments_RC,by="ï..MR_SUBMISSION_TEMPLATE_ID")
RA_claims = merge(RA_claims,months,by="ï..MR_SUBMISSION_TEMPLATE_ID")
RA_claims = merge(RA_claims,months_RC,by="ï..MR_SUBMISSION_TEMPLATE_ID")
RA_claims = merge(RA_claims,revenue,by="ï..MR_SUBMISSION_TEMPLATE_ID")


# Remove non-Individual Market Insurers
RA_claims$absent1 = is.na(RA_claims$MLR_lives) | RA_claims$MLR_lives==0
RA_claims = RA_claims[!RA_claims$absent1,]

# Merge in and summarize by Firm Name
crosswalk = read.csv("Intermediate_Output/FirmCrosswalk.csv")
crosswalk = unique(crosswalk[,c("ï..MR_SUBMISSION_TEMPLATE_ID","Firm","STATE")])

RA_claims = merge(RA_claims,crosswalk[,c("ï..MR_SUBMISSION_TEMPLATE_ID","Firm","STATE")],by="ï..MR_SUBMISSION_TEMPLATE_ID")
RA_claims = summaryBy(MLR_lives+Payments+Payments_RC+Revenue+memberMonths+memberMonths_RC~Firm+STATE,data=RA_claims,FUN=sum,na.rm=TRUE,keep.names=TRUE)

RA_claims = RA_claims[RA_claims$STATE%in%firm_RA$ST,]
RA_claims = as.data.table(RA_claims)
setkey(RA_claims,STATE,Firm)
## Merge with Risk Adjustment Data ##
firm_RA = merge(firm_RA,RA_claims,by.x=c("ST","Firm"),by.y=c("STATE","Firm"),all=TRUE)

## MLR Average Premium
firm_RA[,MLR_avg_prem:=sum(Revenue)/sum(memberMonths),by="ST"]


#### Approximate ACA-Compliant Months ####
firm_RA[abs(Payments)>0,RA_diff:= abs(Payments - Payments_RC)/abs(Payments)]
firm_RA[Payments==0,RA_diff:=0]
firm_RA[,MM_diff:= abs(memberMonths-memberMonths_RC)/memberMonths]

firm_RA[,MM_compliant:=memberMonths_RC]
# This is a reporting error...
firm_RA[Firm=="HEALTH_REPUBLIC_INSURANCE",MM_compliant:=memberMonths]
firm_RA[,MM_noncompliant:=memberMonths-memberMonths_RC]
firm_RA[RA_diff<1e-3,MM_noncompliant:=0]
firm_RA[,RA_flag:=0]
firm_RA[RA_diff>=1e-3,RA_flag:=1]
firm_RA[,noncompliant_share:=MM_noncompliant/sum(MM_noncompliant),by="ST"]

#### Compile un-matched firms into Other ####
firm_RA[is.na(share),Firm:="OTHER"]
firm_RA = firm_RA[,lapply(.SD,sum),by=c("Firm","ST","MLR_avg_prem","mkt_lives","A_wtd","A_f","share"),
                  .SDcols=c("memberMonths","MM_noncompliant","MM_compliant","Payments","Payments_RC","noncompliant_share")]


#### Merge with Interim Data ####
interim = read.csv("Data/RA_Interim_2015.csv")
names(interim) = c("ST","RA_prem","AvgPLRS","AvgARF","AvgAV","AvgIDF","RA_MM")

firm_RA = merge(firm_RA,interim[,c("ST","RA_prem","AvgPLRS","AvgARF","RA_MM")],
                by="ST")

## Correct when Risk Adjustment Member Months are below reported QHP MM
## This a small correction in 3 states
firm_RA[,QHP_diff:=(RA_MM-sum(MM_compliant))/RA_MM,by="ST"]
firm_RA[QHP_diff<0&MM_compliant>0,MM_noncompliant:=0]
firm_RA[QHP_diff<0,RA_MM_new:=sum(MM_compliant)+sum(MM_noncompliant),by="ST"]
firm_RA[QHP_diff<0,noncompliant_share:=0]
firm_RA[QHP_diff<0,MM_compliant_est:=MM_compliant+MM_noncompliant]



firm_RA[QHP_diff>=0,MM_compliant_est:=(RA_MM-sum(MM_compliant))*noncompliant_share+MM_compliant,by="ST"]


firm_RA[,T_norm:=-Payments/(MM_compliant_est*RA_prem)]
firm_RA[Payments==0,T_norm:=0]

firm_RA[,R_est:=(T_norm+1)*AvgPLRS]



firm_RA[,ST_MLR_months:=sum(memberMonths),by="ST"]
firm_RA[,MLR_share:=memberMonths/ST_MLR_months]
setkey(firm_RA,ST,Firm)

# Adjust Payments to sum to 0
firm_RA[,payments_adj:=Payments]
firm_RA[Firm=="OTHER",payments_adj:=0]
firm_RA[,payments_adj_net:=sum(payments_adj),by="ST"]
firm_RA[Firm=="OTHER",payments_adj:=-payments_adj_net]
# Adjust Market Shares
firm_RA[Firm!="OTHER",MLR_share:=0]
firm_RA[,s_adj:=1-sum(MLR_share),by="ST"]
firm_RA[Firm!="OTHER",RA_share:=s_adj*share]
firm_RA[Firm=="OTHER",RA_share:=1-s_adj]

# Set Average Allowable Rating Factor to OTHER Firm
firm_RA[,A_avg:=sum(A_wtd,na.rm=TRUE),by="ST"]
firm_RA[Firm=="OTHER",A_wtd:=A_avg*RA_share]
firm_RA[Firm=="OTHER",A_f:=A_avg]
# Normalize ARF to RA share 
firm_RA[Firm!="OTHER",A_wtd:=A_wtd*RA_share/share]
# Set avg_prem everywhere
firm_RA[,MLR_avg_prem:=max(MLR_avg_prem,na.rm=TRUE),by="ST"]



### Average Transfer 
firm_RA[,T_norm_adj:=-payments_adj/(memberMonths*RA_prem)]
firm_RA[,R_adj:=(T_norm_adj+1)*AvgPLRS]
firm_RA[,R_adj_natl:=(T_norm_adj+1)*1.448]



firm_RA[,names(firm_RA)[!names(firm_RA)%in%c("Firm","ST","MLR_avg_prem","A_wtd","A_f","payments_adj",
                                             "ST_MLR_months","RA_share","memberMonths","MM_compliant","T_norm","T_norm_adj","R_adj","R_adj_natl")]:=NULL]

#firm_RA[,c("A_wtd","ST_MLR_lives"):=NULL]



#### Predict Firm Level Risk Scores ####

firm_level_risk_orig = function(R,df){
  df$R_wtd[df$Firm!="OTHER"] = R
  df$R_wtd[df$Firm=="OTHER"] = df$A_wtd[df$Firm=="OTHER"]

  df$A_sum = ave(df$A_wtd,FUN=sum)
  df$R_sum = ave(df$R_wtd,FUN=sum)
  df$transfer_pred=with(df,-ST_MLR_months*MLR_avg_prem*(R_wtd/R_sum - A_wtd/A_sum))

  error = with(df[df$Firm!="OTHER",],(payments_adj-transfer_pred)/(ST_MLR_months*MLR_avg_prem))
  return(error)
}

firm_level_risk = function(R,df){
  df$R_f[df$Firm!="OTHER"] = R
  df$R_f[df$Firm=="OTHER"] = 1

  df$A_avg = with(df,sum(A_f*memberMonths/ST_MLR_months))
  df$R_avg = with(df,sum(R_f*memberMonths/ST_MLR_months))
  df$transfer_pred=with(df,(R_f/R_avg - A_f/A_avg))

  error = with(df[df$Firm!="OTHER",],(T_norm_adj-transfer_pred))
  return(error)
}


firm_RA[,R_pred:=vector("double",nrow(firm_RA))]
firm_RA[,R_pred_wgt:=vector("double",nrow(firm_RA))]
firm_RA[Firm=="OTHER",R_pred:=1]
firm_RA[Firm=="OTHER",R_pred_wgt:=A_wtd]
#firm_RA[,pp_payments_adj:=payments_adj/lives_f]

for (state in unique(firm_RA$ST)){
  st_RA = as.data.frame(firm_RA[ST==state,])
  R_start = rep(1,sum(st_RA$Firm!="OTHER"))
  res1 = nleqslv(x=R_start,fn=firm_level_risk,df=st_RA,
                control = list(xtol=1e-14,ftol=1e-12,maxit=2000,allowSingular=TRUE))
  R_start = st_RA$A_wtd[st_RA$Firm!="OTHER"]
  res2 = nleqslv(x=R_start,fn=firm_level_risk_orig,df=st_RA,
                control = list(xtol=1e-14,ftol=1e-12,maxit=2000,allowSingular=TRUE))
  print(state)
  print(res1$message)
  print(res2$message)
  # if(res$termcd==7){
  #   res = optim(par=R_start,fn=firm_level_risk_optim,df=st_RA,method="SANN",
  #                 control = list(maxit=2000))
  #   print(res$message)
  # }
  firm_RA[ST==state&Firm!="OTHER",R_pred:=res1$x]
  firm_RA[ST==state&Firm!="OTHER",R_pred_wgt:=res2$x]
}



# ## Check
# R_pred_wgt is found under the wrong assumptions
firm_RA[,R_f:=R_pred]

firm_RA[,ST_A:=sum(A_f*memberMonths/ST_MLR_months),by="ST"]
firm_RA[,ST_R:=sum(R_pred*memberMonths/ST_MLR_months),by="ST"]
firm_RA[,T_norm_est:=(R_pred/ST_R - A_f/ST_A)]

# firm_RA[,R_avg:=sum(R_pred_wgt),by="ST"]
# firm_RA[,R_f:=R_f/R_avg]

#firm_RA = firm_RA[Firm!="OTHER",]
firm_RA[Firm!="OTHER",R_bench:=max(R_f),by="ST"]
firm_RA[,R_bench:=max(R_bench,na.rm=TRUE),by="ST"]
firm_RA[,R_f:=R_f/R_bench]

firm_RA[,R_pred_wgt:=R_f*RA_share]


#### Other Calibrate Data Set ####
firm_RA[,Firm_Ag:="Inside"]
firm_RA[Firm=="OTHER",Firm_Ag:="Other"]
firm_RA[,ST_R:=sum(R_pred_wgt),by="ST"]

#firm_RA[,T_norm_est2:=R_f/ST_R - A_f/ST_A]

firm_RA[,avg_prem:=MLR_avg_prem*12]
firm_RA[,ST_MLR_lives:=ST_MLR_months/12]


other_RA = firm_RA[,list(payments_adj=sum(payments_adj),
                         RA_share = sum(RA_share),
                         R_tot = sum(RA_share*R_f)/sum(RA_share),
                         A_tot = sum(RA_share*A_f)/sum(RA_share)),
                   by=c("Firm_Ag","ST","avg_prem","ST_MLR_lives","R_bench","ST_R")]

#### Save Files ####
firmRiskFile = paste("Simulation_Risk_Output/FirmRiskScores_Full_",run,".rData",sep="")
save(firm_RA,file=firmRiskFile)

firm_RA = firm_RA[Firm!="OTHER",c("Firm","ST","RA_share","R_f","R_bench","ST_R","payments_adj")]
firmRiskFile = paste("Simulation_Risk_Output/FirmRiskScores_",run,".rData",sep="")
save(firm_RA,file=firmRiskFile)

otherRiskFile = paste("Simulation_Risk_Output/otherRiskScores_",run,".rData",sep="")
save(other_RA,file=otherRiskFile)


#### Check ####
# firm_RA[,R_sum:=sum(R_pred_wgt),by="ST"]
# firm_RA[,transfer_pred:=-ST_MLR_lives*avg_prem*(R_pred_wgt/R_sum - A_wtd/A_sum)]
# firm_RA[,transfer_pp:=-avg_prem*(R_f/R_sum - A_f/A_sum)]
# 
# firm_RA[,error:=payments_adj-transfer_pred]
# plot(firm_RA$R_f[firm_RA$R_f>0],firm_RA$pp_payments_adj[firm_RA$R_f>0])
# 
# 
# other_RA[,R_sum:=sum(RA_share*R_tot),by="ST"]
# other_RA[,t_pred:=-ST_MLR_lives*avg_prem*RA_share*(R_tot/R_sum -1)]
