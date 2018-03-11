rm(list = ls())
library(doBy)
library(data.table)
library(nleqslv)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

#### Read in GCF ####
gcf = read.csv("Data/2015_MLR/2015_GCF.csv")
gcf$Market = with(gcf,paste(State,Rating.Area,sep="_"))
gcf=as.data.table(gcf)
setkey(gcf,Market) 
#### Load Simulation Data and Merge in GCF/AV data ####
load("Simulation_Risk_Output/simData.rData")

# Merge in GCF
acs[,Metal:=gsub("([A-Z_]*)(CATASTROPHIC|BRONZE|SILVER|GOLD|PLATINUM)([A-Z_0-9]*)","\\2",acs$Product_Name,perl=TRUE)]
acs[,ST:=gsub("_.*","",Market)]
setkey(acs,Market)
acs = merge(acs,gcf[,c("Market","GCF")],by="Market")

# Set AV Values
acs[Metal=="BRONZE",AV:=.6]
acs[Metal=="SILVER",AV:=.7]
acs[Metal=="GOLD",AV:=.8]
acs[Metal=="PLATINUM",AV:=.9]

# Set IDF Values
acs[Metal=="BRONZE",IDF:=1.0]
acs[Metal=="SILVER",IDF:=1.03]
acs[Metal=="GOLD",IDF:=1.08]
acs[Metal=="PLATINUM",IDF:=1.15]

# Remove Catastrophic Plans for now. 
acs = acs[Metal!="CATASTROPHIC",]

#### Predict Plan Average Allowable Rating Factors
setkey(acs,Product,Person)
setkey(predict_data,Product,Person)



# Get Mean Firm Shares
per_predict = predict_data[,mean(s_pred),by=c("Product","Person")]
predict_full = merge(acs,per_predict,by=c("Product","Person"))

# Calculate Rating Factors
predict_full[,lives:=V1*PERWT]
predict_full[,ageR_wt:=V1*PERWT*ageRate_avg]

pred_prods = predict_full[,lapply(.SD,sum),by=c("Product","Product_Name","Firm","ST","AV","GCF","IDF"),.SDcols=c("lives","ageR_wt")]
pred_prods[,ARF:=ageR_wt/lives]
pred_prods[,mkt_lives:=sum(lives),by="ST"]
pred_prods[,share:=lives/mkt_lives]

##### Merge in Base Premium Information #####
choiceSet = read.csv("Intermediate_Output/Premiums/choiceSets2015.csv")
# Get Product Name
choiceSet$Market = with(choiceSet,paste(ST,gsub("Rating Area ","",AREA),sep="_"))
choiceSet$Product_Name = with(choiceSet,paste(Firm,toupper(METAL),Market,sep="_"))
# Normalize Base Premium
choiceSet$premBase = choiceSet$PREMI27/1.048
choiceSet$premBase[choiceSet$ST=="DC"] = choiceSet$PREMI27[choiceSet$ST=="DC"]/.727
choiceSet$premBase[choiceSet$ST=="MA"] = choiceSet$PREMI27[choiceSet$ST=="MA"]/1.22
choiceSet$premBase[choiceSet$ST=="MN"] = choiceSet$PREMI27[choiceSet$ST=="MN"]/1.048
choiceSet$premBase[choiceSet$ST=="UT"] = choiceSet$PREMI27[choiceSet$ST=="UT"]/1.39

# Create Data Table
choiceSet = as.data.table(choiceSet[,c("Product_Name","premBase")])

# Merge with prediction data 
setkey(choiceSet,Product_Name)
setkey(pred_prods,Product_Name)
pred_prods = merge(pred_prods,choiceSet,by="Product_Name",all.x=TRUE)

# Calculate State Premiums
pred_prods[,charged_prem_avg:=ARF*premBase*12]
pred_prods[,avg_prem:=sum(charged_prem_avg*share),by="ST"]


#### Calculate Firm Level ARF ####
pred_prods[,A:=AV*ARF*IDF*GCF]
pred_prods[,A_wtd:=A*share]

firm_RA = pred_prods[,lapply(.SD,sum),by=c("Firm","ST","avg_prem","mkt_lives"),.SDcols=c("A_wtd","share")]
setkey(firm_RA,ST,Firm)

#### Read in Risk Adjustment Data ####
claims = read.csv("Data/2015_MLR/Part1_2_Summary_Data_Premium_Claims.csv")


payments = claims[claims$ROW_LOOKUP_CODE=="FED_RISK_ADJ_NET_PAYMENTS",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_Q1")]
names(payments) = c("ï..MR_SUBMISSION_TEMPLATE_ID","Payments")

enroll =claims[claims$ROW_LOOKUP_CODE=="NUMBER_OF_LIFE_YEARS",c("ï..MR_SUBMISSION_TEMPLATE_ID","CMM_INDIVIDUAL_Q1")]
names(enroll) = c("ï..MR_SUBMISSION_TEMPLATE_ID","MLR_lives")

RA_claims = merge(payments,enroll,by="ï..MR_SUBMISSION_TEMPLATE_ID")

# Remove non-Individual Market Insurers
RA_claims$absent1 = is.na(RA_claims$MLR_lives) | RA_claims$MLR_lives==0
RA_claims = RA_claims[!RA_claims$absent1,c("ï..MR_SUBMISSION_TEMPLATE_ID","Payments","MLR_lives")]

# Merge in and summarize by Firm Name
crosswalk = read.csv("Intermediate_Output/FirmCrosswalk.csv")
crosswalk = unique(crosswalk[,c("ï..MR_SUBMISSION_TEMPLATE_ID","Firm","STATE")])

RA_claims = merge(RA_claims,crosswalk[,c("ï..MR_SUBMISSION_TEMPLATE_ID","Firm","STATE")],by="ï..MR_SUBMISSION_TEMPLATE_ID")
RA_claims = summaryBy(MLR_lives+Payments~Firm+STATE,data=RA_claims,FUN=sum,na.rm=TRUE,keep.names=TRUE)

RA_claims = RA_claims[RA_claims$STATE%in%firm_RA$ST,]
RA_claims = as.data.table(RA_claims)
setkey(RA_claims,STATE,Firm)
## Merge with Risk Adjustment Data ##
firm_RA = merge(firm_RA,RA_claims,by.x=c("ST","Firm"),by.y=c("STATE","Firm"),all=TRUE)

#### Compile un-matched firms into Other ####
firm_RA[is.na(share),Firm:="OTHER"]
firm_RA = firm_RA[,lapply(.SD,sum),by=c("Firm","ST","avg_prem","mkt_lives","A_wtd","share"),.SDcols=c("MLR_lives","Payments")]

firm_RA[,ST_MLR_lives:=sum(MLR_lives),by="ST"]
firm_RA[,MLR_share:=MLR_lives/ST_MLR_lives]
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
# Normalize ARF to RA share 
firm_RA[Firm!="OTHER",A_wtd:=A_wtd*RA_share/share]
# Set avg_prem everywhere
firm_RA[,avg_prem:=max(avg_prem,na.rm=TRUE),by="ST"]


firm_RA[,names(firm_RA)[!names(firm_RA)%in%c("Firm","ST","avg_prem","A_wtd","payments_adj","ST_MLR_lives","RA_share")]:=NULL]


#### Predict Firm Level Risk Scores ####

firm_level_risk = function(R,df){
  df$R_wtd[df$Firm!="OTHER"] = R
  df$R_wtd[df$Firm=="OTHER"] = df$A_wtd[df$Firm=="OTHER"]
  
  df$A_sum = ave(df$A_wtd,FUN=sum)
  df$R_sum = ave(df$R_wtd,FUN=sum)
  df$transfer_pred=with(df,-ST_MLR_lives*avg_prem*(R_wtd/R_sum - A_wtd/A_sum))
  
  error = with(df[df$Firm!="OTHER",],(payments_adj-transfer_pred)/(ST_MLR_lives*avg_prem))
  return(error)
}


firm_RA[,R_pred:=vector("double",nrow(firm_RA))]
firm_RA[Firm=="OTHER",R_pred:=A_wtd]
for (state in unique(firm_RA$ST)){
  st_RA = as.data.frame(firm_RA[ST==state,])
  R_start = st_RA$A_wtd[st_RA$Firm!="OTHER"]
  res = nleqslv(x=R_start,fn=firm_level_risk,df=st_RA,
                control = list(xtol=1e-14,ftol=1e-12,maxit=2000,allowSingular=TRUE))
  print(state)
  print(res$message)
  # if(res$termcd==7){
  #   res = optim(par=R_start,fn=firm_level_risk_optim,df=st_RA,method="SANN",
  #                 control = list(maxit=2000))
  #   print(res$message)
  # }
  firm_RA[ST==state&Firm!="OTHER",R_pred:=res$x]
}


firm_RA = firm_RA[,c("Firm","ST","R_pred")]
save(firm_RA,file="Simulation_Risk_Output/FirmRiskScores.rData")


# ## Check
# firm_RA[,A_sum:=sum(A_wtd),by="ST"]
# firm_RA[,R_sum:=sum(R_pred),by="ST"]
# firm_RA[,transfer_pred:=-ST_MLR_lives*avg_prem*(R_pred/R_sum - A_wtd/A_sum)]
# firm_RA[,error:=payments_adj-transfer_pred]
