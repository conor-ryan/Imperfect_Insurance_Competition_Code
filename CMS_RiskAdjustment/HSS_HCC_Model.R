rm(list=ls())
library(data.table)
library(doBy)
library(Hmisc)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition")


#### ICD09 to 3-Digit Mapping ####
mapdig = as.data.table(read.csv("Code/CMS_RiskAdjustment/HHS-HCC/ICD09_Codes.csv",stringsAsFactors=FALSE,colClasses = "character"))
mapdig[,trunc_ICD9:=substr(ICD9,0,3)]
mapdig[,ICD9_num:=as.numeric(gsub("V|E","",ICD9))]
mapdig[,ICD9_min:=min(ICD9_num),by="trunc_ICD9"]
mapdig = mapdig[ICD9_num==ICD9_min,]

#### CC code map (manual)
CC_HCC = as.data.table(read.csv("Code/CMS_RiskAdjustment/HHS-HCC/MEPS_to_HCC_CCMap.csv"))


#### Classify ICD-9 Codes into HCCs ####

crosswalk <- as.data.table(read.csv("Code/CMS_RiskAdjustment/HHS-HCC/ICD09_HCC.csv",stringsAsFactors=FALSE,colClasses = "character"))

## Truncate to first three digits
crosswalk[,trunc_ICD9:=substr(ICD9,0,3)]

## Keep relevant variables
crosswalk = crosswalk[Valid_2015=="Y",]
## Set Age Brackets
crosswalk[,Age_Diag_Min:=0]
crosswalk[grepl("12 <=",AGE_AT_DIAG),Age_Diag_Min:=12]
crosswalk[grepl(">=15",AGE_AT_DIAG),Age_Diag_Min:=15]

crosswalk[,Age_Diag_Max:=85]
crosswalk[grepl("= 0",AGE_AT_DIAG),Age_Diag_Max:=0]
crosswalk[grepl("<= 17",AGE_AT_DIAG),Age_Diag_Max:=17]
crosswalk[grepl("<= 55",AGE_AT_DIAG),Age_Diag_Max:=55]

crosswalk[,Age_Min:=0]
crosswalk[grepl(">= 1",CC_AGE_SPLIT),Age_Min:=1]
crosswalk[grepl(">= 2",CC_AGE_SPLIT),Age_Min:=2]
crosswalk[grepl(">= 18",CC_AGE_SPLIT),Age_Min:=18]
crosswalk[grepl(">= 50",CC_AGE_SPLIT),Age_Min:=50]

crosswalk[,Age_Max:=85]
crosswalk[grepl(" = 0",CC_AGE_SPLIT),Age_Max:=0]
crosswalk[grepl(" < 2",CC_AGE_SPLIT),Age_Max:=1]
crosswalk[grepl(" < 18",CC_AGE_SPLIT),Age_Max:=17]
crosswalk[grepl(" < 50",CC_AGE_SPLIT),Age_Max:=49]

# #crosswalk = crosswalk[,list(HCC=min(HCC)),by="trunc_ICD9"]
# crosswalk = unique(crosswalk[,c("trunc_ICD9","CC","Additional_CC","CC_SEX_SPLIT","Age_Min","Age_Max","Age_Diag_Min","Age_Diag_Max")])
CC_cats = as.numeric(unique(crosswalk$CC))
# ## Minimum CC per truncated code. Imperfect adjustment... 
# crosswalk = crosswalk[,list(CC=min(CC)),
#                       by=c("trunc_ICD9","CC_SEX_SPLIT","Age_Min","Age_Max","Age_Diag_Min","Age_Diag_Max")]





#### Classify MEPS into HCCs ####
meps_med = as.data.table(read.csv("Data/2015_MEPS/MEPS_Med_2015.csv"))
meps_full = read.csv("Data/2015_MEPS/MEPS_Full_2015.csv")
meps_full = meps_full[,c("DUID","PID","DUPERSID","PANEL","AGE15X","SEX")]

meps_med = merge(meps_med,meps_full,by=c("DUID","PID","DUPERSID","PANEL"),all.x=TRUE)




## Merge into crosswalk
meps_med = merge(meps_med,mapdig[,c("trunc_ICD9","ICD9")],by.x="ICD9CODX",by.y="trunc_ICD9",all.x=TRUE)

meps_med = merge(meps_med,crosswalk,by.x="ICD9",by.y="ICD9",all.x=TRUE)

meps_med = merge(meps_med,CC_HCC[,c("MEPS_CC","HCC")],by.x="CCCODEX",by.y="MEPS_CC",all.x=TRUE)

# # Code Mycoses conservatively 
# meps_med[CCCODEX==4&ICD9CODX==112&is.na(CC),CC:=3]
# meps_med[CCCODEX==4&ICD9CODX!=112,HCC:=NA]

meps_med[is.na(CC),CC:=HCC]

meps_med[,keep_sex:=TRUE]
meps_med[,keep_age:=(is.na(Age_Min)) | (AGE15X>=Age_Min & AGE15X<=Age_Max)]

## In this sample, no sex distinctions or Additional CCs to worry about.
meps_med = meps_med[keep_sex==TRUE & keep_age==TRUE,]



meps_med = unique(meps_med[,c("DUID","PID","DUPERSID","PANEL","AGE15X","CC")])
meps_med$CC = as.numeric(meps_med$CC)


# Create Wide Dataset
for (cat in CC_cats){
  cc = as.character(cat)
  if (cat<100) {cc = paste("0",cat,sep="")}
  if (cat<10) {cc = paste("00",cat,sep="")}
  var = paste("HHS_HCC",cc,sep="")
  print(var)
  meps_med[[var]] = 0
  meps_med[[var]][meps_med$CC==cat] = 1
}

cc_vars = names(meps_med)[grep("HHS_HCC",names(meps_med))]

formula = as.formula(paste(paste(cc_vars,collapse="+"),"~DUID+PID+DUPERSID+PANEL+AGE15X"))
meps_CC = summaryBy(formula,data=meps_med,FUN=sum,keep.names=TRUE)

## Process the Hierarchy 
# Create hierarchy lists
hier = read.csv("Code/CMS_RiskAdjustment/HHS-HCC/HCC_Hierarchy.csv",stringsAsFactors = FALSE)
HCC_Hier = list()
for (n in 1:nrow(hier)){
  cc = hier$HCC[n]
  cc_list = as.numeric(unlist(strsplit(hier$Hierarhcy[n],",")))
  HCC_Hier[[cc]] = cc_list
}

# Zero out superceded CCs
for (hcc_var in cc_vars){
  #print(hcc_var)
  hcc = as.numeric(gsub("HHS_HCC","",hcc_var))
  if(length(HCC_Hier[[hcc]])>0){
    #print("Hier")
    for (cat in HCC_Hier[[hcc]]){
      cc = as.character(cat)
      if (cat<100) {cc = paste("0",cat,sep="")}
      if (cat<10) {cc = paste("00",cat,sep="")}
      var_CC = paste("HHS_HCC",cc,sep="")
      #print(var_CC)
      meps_CC[[var_CC]][meps_CC[[hcc_var]]==1] = 0
    }
  }
}


#### Create Demogrophic Base for Risk Adjustment ####
meps_full = read.csv("Data/2015_MEPS/MEPS_Full_2015.csv")
meps_full = as.data.table(meps_full[,c("DUID","PID","DUPERSID","PANEL","AGE15X","SEX","PERWT15F")])
meps_full = meps_full[AGE15X>(-1),]
## Age Categories
meps_full[,Age_Cat_Min:= floor(AGE15X/5)*5]
meps_full[,Age_Cat_Max:=Age_Cat_Min+4]


## Youth Age Categories
meps_full[AGE15X==1,Age_Cat_Min:= 1]
meps_full[AGE15X>1&AGE15X<=4,Age_Cat_Min:= 2]
meps_full[AGE15X>4&AGE15X<=0,Age_Cat_Min:= 5]
meps_full[AGE15X>9&AGE15X<=14,Age_Cat_Min:= 10]
meps_full[AGE15X>14&AGE15X<=20,Age_Cat_Min:= 15]
meps_full[AGE15X>20&AGE15X<=24,Age_Cat_Min:= 21]

meps_full[AGE15X==0,Age_Cat_Max:= 0]
meps_full[AGE15X==1,Age_Cat_Max:= 1]
meps_full[AGE15X>1&AGE15X<=4,Age_Cat_Max:= 4]
meps_full[AGE15X>4&AGE15X<=0,Age_Cat_Max:= 9]
meps_full[AGE15X>9&AGE15X<=14,Age_Cat_Max:= 14]
meps_full[AGE15X>14&AGE15X<=20,Age_Cat_Max:= 20]
meps_full[AGE15X>20&AGE15X<=24,Age_Cat_Max:= 24]


## Top Code
meps_full[,Age_Cat_Max:=as.character(Age_Cat_Max)]

meps_full[AGE15X>=60,Age_Cat_Min:=60]
meps_full[AGE15X>=60,Age_Cat_Max:="GT"]

meps_full[,Age_Cat:=paste("AGE_LAST",Age_Cat_Min,Age_Cat_Max,sep="_")]

## Create Age Variables
meps_full[SEX==1,Age_Var:=paste("M",Age_Cat,sep="")]
meps_full[SEX==2,Age_Var:=paste("F",Age_Cat,sep="")]

meps_full[Age_Var=="MAGE_LAST_0_0",Age_Var:="AGE0_MALE"]
meps_full[Age_Var=="MAGE_LAST_1_1",Age_Var:="AGE1_MALE"]

meps_full[Age_Var=="FAGE_LAST_0_0",Age_Var:="AGE0_FEMALE"]
meps_full[Age_Var=="FAGE_LAST_1_1",Age_Var:="AGE1_FEMALE"]

## Reshape to Wide Format 
age_Vars = sort(unique(meps_full$Age_Var))
for (var in age_Vars){
  meps_full[,c(var):= 0]
  meps_full[Age_Var==var,c(var):= 1]
  
  #meps_full[[var]][meps_full$Age_Var==var] = 1
}

## Keep necessary Variables
drop_vars = c("SEX","Age_Var","Age_Cat","Age_Cat_Min","Age_Cat_Max")
meps_full[,c(drop_vars):=NULL]

# formula = as.formula(paste(paste(age_Vars,collapse="+"),"~DUID+PID+DUPERSID+PANEL+AGE15X"))
# meps_full = summaryBy(formula,data=meps_full,FUN=sum,keep.names=TRUE)


#### Group and Severity Variables ####
group_def_adult = read.csv("Code/CMS_RiskAdjustment/HHS-HCC/HCC_Groups_Adult.csv",stringsAsFactors = FALSE)
group_def_child = read.csv("Code/CMS_RiskAdjustment/HHS-HCC/HCC_Groups_Child.csv",stringsAsFactors = FALSE)

## Severity
severe = group_def_adult[group_def_adult$Variable=="SEVERE_V3",]
severe_HCC = gsub("(.*)(HHS_HCC[0-9]{3})(.*)","\\2",severe$Definition,perl=TRUE)

meps_CC$SEVERE_V3=0
for (sev_var in severe_HCC){
  meps_CC$SEVERE_V3[meps_CC$AGE15X>=21&meps_CC[[sev_var]]==1] = 1
}

## Groups
groups = rbind(group_def_adult[grepl("^G",group_def_adult$Variable),],
               group_def_child)
groups$HCC = gsub("(.*)(HHS_HCC[0-9]{3})(.*)","\\2",groups$Definition,perl=TRUE)

# Initialize Variables
for (g_var in sort(unique(groups$Variable))){
  meps_CC[[g_var]] = 0
}

# Set Groups
for (n in 1:nrow(groups)){
  g_var = groups$Variable[n]
  hcc = groups$HCC[n]
  if (groups$Model[n]=="Adult"){
    meps_CC[[g_var]][meps_CC$AGE15X>=21 & meps_CC[[hcc]]==1] = 1
    meps_CC[[hcc]][meps_CC$AGE15X>=21 & meps_CC[[hcc]]==1] = 0
  }
  if (groups$Model[n]=="Child"){
    meps_CC[[g_var]][meps_CC$AGE15X<21 & meps_CC$AGE15X>1 &  meps_CC[[hcc]]==1] = 1
    meps_CC[[hcc]][meps_CC$AGE15X<21 & meps_CC$AGE15X>1 & meps_CC[[hcc]]==1] = 0
  }
}

## Interactions 
interact = group_def_adult[grep("_X_",group_def_adult$Variable),]

for (var in interact$Variable){
  var = gsub(" ","",var,perl=TRUE)
  v1 = gsub("(.*)(_X_)(.*)","\\1",var,perl=TRUE)
  v2 = gsub("(.*)(_X_)(.*)","\\3",var,perl=TRUE)
  
  meps_CC[[var]] = meps_CC[[v1]]*meps_CC[[v2]]
}

## Interaction Groups
intgrp = group_def_adult[grep("^INT",group_def_adult$Variable),]

intgrp$int_var = gsub("(.+?)([A-Z0-9_]+_X_[A-Z0-9_]+)(.+)","\\2",intgrp$Definition,perl=TRUE)

meps_CC$INT_GROUP_H = 0
for (int_var in intgrp$int_var[intgrp$Variable=="INT_GROUP_H"]){
  meps_CC$INT_GROUP_H[meps_CC[[int_var]]==1] = 1
}

meps_CC$INT_GROUP_M = 0
for (int_var in intgrp$int_var[intgrp$Variable=="INT_GROUP_M"]){
  meps_CC$INT_GROUP_M[meps_CC$INT_GROUP_H==0 & meps_CC[[int_var]]==1] = 1
}

#### Infant Severity Variables ####
group_def_infant = read.csv("Code/CMS_RiskAdjustment/HHS-HCC/HCC_Groups_Infant.csv",stringsAsFactors = FALSE)

## Severity
severe = group_def_infant[grepl("^IHCC_SEVERITY[0-9]$",group_def_infant$Variable),]
severe$HCC = gsub("(.*)(HHS_HCC[0-9]{3})(.*)","\\2",severe$Definition,perl=TRUE)

# Initialize Variables
for (s_var in sort(unique(severe$Variable))){
  meps_CC[[s_var]] = 0
}

# Set Severity Variables
for (n in 1:nrow(severe)){
  s_var = severe$Variable[n]
  hcc = severe$HCC[n]
  meps_CC[[s_var]][meps_CC$AGE15X<=1 & meps_CC[[hcc]]==1] = 1
}

# Enforce Higherarchy
for (i in 5:2){
  hier_sev = paste("IHCC_SEVERITY",i,sep="")
  for (j in (i-1):1){
    low_sev = paste("IHCC_SEVERITY",j,sep="")
    meps_CC[[low_sev]][meps_CC[[hier_sev]]==1] = 0
  }
}

# Other Variables
meps_CC = as.data.frame(meps_CC)
meps_CC$IHCC_AGE1= with(meps_CC,as.numeric(AGE15X==1))
meps_CC$IHCC_EXTREMELY_IMMATURE=with(meps_CC,as.numeric(AGE15X==0 & (HHS_HCC242==1 | HHS_HCC243==1 | HHS_HCC244==1)))
meps_CC$IHCC_IMMATURE = with(meps_CC,as.numeric(AGE15X==0 & (HHS_HCC245==1 | HHS_HCC246==1) & IHCC_EXTREMELY_IMMATURE==0))
meps_CC$IHCC_PREMATURE_MULTIPLES = with(meps_CC,as.numeric(AGE15X==0 & (HHS_HCC247==1 | HHS_HCC248==1) & IHCC_EXTREMELY_IMMATURE==0 & IHCC_IMMATURE==0))
meps_CC$IHCC_TERM = with(meps_CC,as.numeric(AGE15X==0 & IHCC_EXTREMELY_IMMATURE==0 & IHCC_IMMATURE==0 & IHCC_PREMATURE_MULTIPLES==0))

## Interaction Terms
interact = group_def_infant[grep("_X_",group_def_infant$Variable),]

for (var in interact$Variable){
  var = gsub(" ","",var,perl=TRUE)
  v1 = paste("IHCC",gsub("(.*)(_X_)(.*)","\\1",var,perl=TRUE),sep="_")
  v2 = paste("IHCC",gsub("(.*)(_X_)(.*)","\\3",var,perl=TRUE),sep="_")
  meps_CC[[var]] = meps_CC[[v1]]*meps_CC[[v2]]
}

#### Combine Demographic and HCC Variables ####
meps_CC = meps_CC[meps_CC$AGE15X>=0,]
meps_CC = meps_CC[,-grep("AGE15X",names(meps_CC))]
model_full = merge(meps_full,meps_CC,by=c("DUID","PID","DUPERSID","PANEL"),all=TRUE)
model_full = as.data.frame(model_full)
## Set Missing Variables to 0
for (var in names(model_full)){
  model_full[[var]][is.na(model_full[[var]])] = 0
}

model_full$model="Adult"
model_full$model[model_full$AGE15X<=20]="Child"
model_full$model[model_full$AGE15X<=1]="Infant"


#### Calculate Unadjusted Risk Scores ####
coeffs = read.csv("Code/CMS_RiskAdjustment/HHS-HCC/HHS_HCC_Coefficients.csv",stringsAsFactors = FALSE)

## Full Risk Scores
model_full$R_Score_Platinum = 0
model_full$R_Score_Gold = 0
model_full$R_Score_Silver = 0
model_full$R_Score_Bronze = 0
model_full$R_Score_Catastrophic = 0

old_mean = 0
for (n in 1:nrow(coeffs)){
  for (m in c("Platinum","Gold","Silver","Bronze","Catastrophic")){
    var = coeffs$Variable[n]
    beta = coeffs[[m]][n]
    mod = coeffs$Model[n]
    score_var = paste("R_Score",m,sep="_")
    model_full[[score_var]][model_full$model==mod] = 
      model_full[[score_var]][model_full$model==mod] + 
      model_full[[var]][model_full$model==mod]*beta
  }
  # new_mean = mean(model_full$R_Score_Bronze)
  # mean_diff = new_mean - old_mean
  # old_mean = new_mean
  # if (mean_diff>.05){
  #   print(var)
  #   print(new_mean)
  #   print(mean_diff)
  # }
}

## Non-Age Risk Scores
nonAge_coeffs = coeffs[!grepl("AGE",coeffs$Variable),]

model_full$HCC_Score_Platinum = 0
model_full$HCC_Score_Gold = 0
model_full$HCC_Score_Silver = 0
model_full$HCC_Score_Bronze = 0
model_full$HCC_Score_Catastrophic = 0

for (n in 1:nrow(nonAge_coeffs)){
  var = nonAge_coeffs$Variable[n]
  for (m in c("Platinum","Gold","Silver","Bronze","Catastrophic")){
    beta = nonAge_coeffs[[m]][n]
    mod = nonAge_coeffs$Model[n]
    score_var = paste("HCC_Score",m,sep="_")
    model_full[[score_var]][model_full$model==mod] = 
      model_full[[score_var]][model_full$model==mod] + 
      model_full[[var]][model_full$model==mod]*beta
  }
}

##### Match Mean of Actual Risk Score ####
## Non Group Coverage
mepsPers = read.csv("Data/2015_MEPS/MEPS_Person_2015.csv")

mepsPers = mepsPers[mepsPers$PRIVCAT%in%c(2,3,5,6,99),]
mepsPers = mepsPers[mepsPers$CMJINS!=1,]
mepsPers = mepsPers[mepsPers$TYPEFLAG%in%c(5,6,7,11,12,13,21),]

mepsPers = mepsPers[mepsPers$STEXCH!=-1,]

mepsPers = summaryBy(STEXCH~DUPERSID+PANEL,data=mepsPers,FUN=min,keep.names=TRUE)

model_full = merge(model_full,mepsPers,all.x=TRUE,by=c("DUPERSID","PANEL"))

mean_R_Silver = wtd.mean(model_full$R_Score_Silver[!is.na(model_full$STEXCH)&model_full$AGE15X<66],weights=model_full$PERWT15F[!is.na(model_full$STEXCH)&model_full$AGE15X<66])
mean_HCC_Silver = wtd.mean(model_full$HCC_Score_Silver[!is.na(model_full$STEXCH)&model_full$AGE15X<66],weights=model_full$PERWT15F[!is.na(model_full$STEXCH)&model_full$AGE15X<66])
mean_Age = mean_R_Silver - mean_HCC_Silver
## Adjust to match actual mean
HCC_Adj = (1.615 - mean_Age)/mean_HCC_Silver
# Close enoough... 


##### Re- Calculate Risk Scores ####
# 
# ## Full Risk Scores
# model_full$R_Score_Platinum = 0
# model_full$R_Score_Gold = 0
# model_full$R_Score_Silver = 0
# model_full$R_Score_Bronze = 0
# model_full$R_Score_Catastrophic = 0
# 
# old_mean = 0
# for (n in 1:nrow(coeffs)){
#   for (m in c("Platinum","Gold","Silver","Bronze","Catastrophic")){
#     var = coeffs$Variable[n]
#     beta = coeffs[[m]][n]
#     mod = coeffs$Model[n]
#     score_var = paste("R_Score",m,sep="_")
#     if(!grepl("AGE",var)){
#       beta = HCC_Adj*beta
#     }
#     
#     model_full[[score_var]][model_full$model==mod] = 
#       model_full[[score_var]][model_full$model==mod] + 
#       model_full[[var]][model_full$model==mod]*beta
#   }
#   
#   # new_mean = mean(model_full$R_Score_Bronze)
#   # mean_diff = new_mean - old_mean
#   # old_mean = new_mean
#   # if (mean_diff>.1){
#   #   print(var)
#   #   print(new_mean)
#   #   print(mean_diff)
#   # }
# }
# 
# mean_R_Silver = wtd.mean(model_full$R_Score_Silver[!is.na(model_full$STEXCH)],weights=model_full$PERWT15F[!is.na(model_full$STEXCH)])
# print(mean_R_Silver)
# 
# ## Non-Age Risk Scores
# nonAge_coeffs = coeffs[!grepl("AGE",coeffs$Variable),]
# 
# model_full$HCC_Score_Platinum = 0
# model_full$HCC_Score_Gold = 0
# model_full$HCC_Score_Silver = 0
# model_full$HCC_Score_Bronze = 0
# model_full$HCC_Score_Catastrophic = 0
# 
# for (n in 1:nrow(nonAge_coeffs)){
#   var = nonAge_coeffs$Variable[n]
#   for (m in c("Platinum","Gold","Silver","Bronze","Catastrophic")){
#     beta = HCC_Adj*nonAge_coeffs[[m]][n]
#     mod = nonAge_coeffs$Model[n]
#     score_var = paste("HCC_Score",m,sep="_")
#     model_full[[score_var]][model_full$model==mod] = 
#       model_full[[score_var]][model_full$model==mod] + 
#       model_full[[var]][model_full$model==mod]*beta
#   }
# }

#### Output Data ####
model_full = model_full[,c("DUPERSID","PANEL",names(model_full)[grep("(R_Score|HCC_Score)",names(model_full))])]

write.csv(model_full,"Intermediate_Output/MEPS_Moments/meps_risk_scores.csv",row.names=FALSE)
