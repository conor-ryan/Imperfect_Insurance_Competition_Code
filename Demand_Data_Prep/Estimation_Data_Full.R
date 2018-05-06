rm(list=ls())
library(doBy)
library(data.table)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition")

#### 2015 Subsidy Percentage Function ####

subsPerc <- function(FPL){
  x = FPL[!is.na(FPL)]
  y = rep(100,length=length(x))
  y[x>=1&x<1.33]   = 2.01 + (x-1)[x>=1&x<1.33]/(1.33-1)*(3.02-2.01)
  y[x>=1.33&x<1.5] = 3.02 + (x-1.33)[x>=1.33&x<1.5]/(1.5-1.33)*(4.02-3.02)
  y[x>=1.5&x<2]    = 4.02 + (x-1.5)[x>=1.5&x<2]/(2-1.5)*(6.34-4.02)
  y[x>=2&x<2.5]    = 6.34 + (x-2)[x>=2&x<2.5]/(2.5-2)*(8.1-6.34)
  y[x>=2.5&x<3]    = 8.1 + (x-2.5)[x>=2.5&x<3]/(3-2.5)*(9.56-8.1)
  y[x>=3&x<=4]      = 9.56
  
  HHcont = rep(NA,length(FPL))
  HHcont[!is.na(FPL)] = y/100
  
  return(HHcont)
}

# Based on Individual Income
subsInv <- function(cont,pov_line = 11770,FPL_flag=FALSE){
  
  y = cont[!is.na(cont)]
  x = rep(NA,length(y))
  
  # y[x>=1&x<1.33]   = (2.01*pov_line + (x-1)[x>=1&x<1.33]/(1.33-1)*(3.02*pov_line*1.33-2.01*pov_line))/100/12
  # y[x>=1.33&x<1.5] = (3.02*pov_line*1.33 + (x-1.33)[x>=1.33&x<1.5]/(1.5-1.33)*(4.02*pov_line*1.5-3.02*pov_line*1.33))/100/12
  # y[x>=1.5&x<2]    = (4.02*pov_line*1.5 + (x-1.5)[x>=1.5&x<2]/(2-1.5)*(6.34*pov_line*2-4.02*pov_line*1.5))/100/12
  # y[x>=2&x<2.5]    = (6.34*pov_line*2 + (x-2)[x>=2&x<2.5]/(2.5-2)*(8.1*pov_line*2.5-6.34*pov_line*2))/100/12
  # y[x>=2.5&x<3]    = (8.1*pov_line*2.5 + (x-2.5)[x>=2.5&x<3]/(3-2.5)*(9.56*pov_line*3-8.1*pov_line*2.5))/100/12
  # y[x>=3&x<=4]      = (9.56*pov_line*3 + (x-3)[x>=3&x<=4]/(4-3)*(9.56*pov_line*4-9.56*pov_line*3))/100/12
  
  br1 = .0201*pov_line*1.00/12
  br2 = .0302*pov_line*1.33/12
  br3 = .0402*pov_line*1.50/12
  br4 = .0643*pov_line*2.00/12
  br5 = .0810*pov_line*2.50/12
  br6 = .0956*pov_line*3.00/12
  br7 = .0956*pov_line*4.00/12
  
  x[y>=br1&y<br2] = 1 + (y-br1)[y>=br1&y<br2]/(br2-br1)*(1.33-1)
  x[y>=br2&y<br3] = 1.33 + (y-br2)[y>=br2&y<br3]/(br3-br2)*(1.5-1.33)
  x[y>=br3&y<br4] = 1.5 + (y-br3)[y>=br3&y<br4]/(br4-br3)*(2-1.5)
  x[y>=br4&y<br5] = 2 +  (y-br4)[y>=br4&y<br5]/(br5-br4)*(2.5-2)
  x[y>=br5&y<br6] = 2.5 + (y-br5)[y>=br5&y<br6]/(br6-br5)*(3-2.5)
  x[y>=br6&y<br7] = 3 +   (y-br6)[y>=br6&y<br7]/(br7-br6)*(4-3)
  
  
  Income = rep(NA,length(cont))
  FPL = rep(NA,length(cont))
  Income[!is.na(cont)] = x*pov_line
  FPL[!is.na(cont)] = x
  if(FPL_flag){
    return(FPL)
  }else{
    return(Income)
  }
  
}


#### Read in eHealth Data and clean Premium Information ####
eHealth = read.csv("C:/Users/Conor/Documents/Research/eHealth Data/eHealth_2015.csv",stringsAsFactors = FALSE)

# Firm Crosswalk
firmCrosswalk = read.csv("Intermediate_Output/FirmCrosswalk.csv")
firmCrosswalk = unique(firmCrosswalk[,c("STATE","eHealth_CARRIER_NAME","Firm")])
eHealth = merge(eHealth,firmCrosswalk,by.x=c("STATE","CARRIER_NAME"),by.y=c("STATE","eHealth_CARRIER_NAME"),all.x=TRUE)


# Drop "referential integrity" rows - 2,248
eHealth = eHealth[eHealth$PLAN_NAME!="UNKNOWN PLAN - this row is used for referential integrity - DSTOCK",]

# Drop eHealth observations with NA or 0 zip code - 2,813
eHealth = eHealth[with(eHealth,!is.na(TRUNCATED_ZIP)&TRUNCATED_ZIP!=0 ),]
eHealth = eHealth[with(eHealth,PLAN_METAL_LEVEL!="N/A"),]

# Drop kids that are heads of houeshold - 6,035
eHealth = eHealth[with(eHealth,AGE>18),]

# Subset eHealth for Valid Markets
STselection = unique(firmCrosswalk$STATE)


# Reconstruct from subsidy information
# # Unclear what QUOTED_RATE means is the presence of subsidies
# # We appear unable to reconstructed base premium if post-subsidiy premium is 0
# eHealth$premium = eHealth$QUOTED_RATE
# eHealth$premium[!is.na(eHealth$FFM_APTC_AMOUNT)] = with(eHealth[!is.na(eHealth$FFM_APTC_AMOUNT),],FFM_APTC_AMOUNT+FFM_PREMIUM_DUE)
# eHealth$premium[eHealth$FFM_PREMIUM_DUE==0] = NA
# #Sometimes subsidies are implied by income, but not listed in the data
# eHealth$premium[eHealth$HOUSEHOLD_INCOME<50000&is.na(eHealth$FFM_PREMIUM_DUE)] = NA
# #Some plans still have 0 premiums for no apparent reason. 
# eHealth$premium[eHealth$premium==0] = NA


#### Merge eHealth and Plan Data ####
# Default Choice Set By Zip3
choiceSets = read.csv("Intermediate_Output/Premiums/choiceSets2015.csv")
mapping = read.csv("Intermediate_Output/Zip_RatingArea/Zip3_to_RatingArea.csv")

choiceSets = as.data.table(choiceSets)
setkey(choiceSets,ST,AREA)

mapping = as.data.table(mapping)
setkey(mapping,ST,RatingArea)

choices = merge(choiceSets,mapping[,c("ST","Zip3","RatingArea","alloc")],by.x=c("ST","AREA"),by.y=c("ST","RatingArea"),
                all.x=TRUE,allow.cartesian=TRUE)

# Merge in eHealth data
eHealth = as.data.table(eHealth[eHealth$TRUNCATED_ZIP%in%choices$Zip3,])
setkey(eHealth,STATE,TRUNCATED_ZIP)
setkey(choices,ST,Zip3)

choices = merge(eHealth,choices,by.x=c("STATE","TRUNCATED_ZIP"),by.y=c("ST","Zip3"),
                all.x=TRUE,allow.cartesian=TRUE)
setkey(choices,APP_RECORD_NUM,METAL)

names(choices)[names(choices)=="Firm.x"] = "Firm_Choice"
choices[,Firm_Choice:= as.character(choices$Firm_Choice)]
names(choices)[names(choices)=="Firm.y"] = "Firm"
choices[,Firm:= as.character(choices$Firm)]


#rm(mapping,eHealth,choiceSets)

#### Create Valid Choice Sets ####

# Set Choice Indicator
choices[,METAL:= toupper(METAL)]
choices[,Y:= 0]
choices[METAL==PLAN_METAL_LEVEL&Firm_Choice==Firm,Y:=1]

ids = unique(choices$APP_RECORD_NUM[choices$Y==1])
missing = eHealth$APP_RECORD_NUM[!eHealth$APP_RECORD_NUM%in%ids]
test = choices[choices$APP_RECORD_NUM%in%missing,]
test = unique(test[!test$Firm_Choice%in%c("HEALTHYCT","SHARP_HEALTH_PLAN"),c("APP_RECORD_NUM","Firm_Choice","AREA","STATE")])


#test = eHealth[eHealth$APP_RECORD_NUM%in%missing,]


# Drop Catastrophic for over 30. 
# Lose two observations that chose catas over 30
choices = choices[!(AGE>30 & METAL=="CATASTROPHIC"),]


# If choice is not available, then that individual is not in the right rating area
# This step drops CA/CT (data collection needed), 3 obs in IL that don't have right rating area
choices[,flag:= sum(Y),by=c("APP_RECORD_NUM","AREA")]
choices = choices[flag!=0,]

# Remove non-valid areas
choices = choices[choices$valid,]

# Keep only most likely rating area
choices[,maxAlloc:=max(alloc),by="APP_RECORD_NUM"]
choices = choices[alloc==maxAlloc,]



# Roll in Family Characteristcs
# choices$MedDeduct[choices$FAMILY_OR_INDIVIDUAL=="FAMILY"] = 
#   choices$MedDeductFam[choices$FAMILY_OR_INDIVIDUAL=="FAMILY"]
# choices$MedOOP[choices$FAMILY_OR_INDIVIDUAL=="FAMILY"] = 
#   choices$MedOOPFam[choices$FAMILY_OR_INDIVIDUAL=="FAMILY"]


# Keep only relevant variables
choices = choices[,c("STATE","APP_RECORD_NUM","QUOTED_RATE","HOUSEHOLD_INCOME","FFM_APTC_AMOUNT","FFM_PREMIUM_DUE",
                     "FAMILY_OR_INDIVIDUAL","MEMBERS","AGE","SMOKER","AREA","Firm","METAL","hix","PREMI27",
                     "MedDeduct","MedOOP","Y")]


#### Set Rating & Subsidy Values ####
rating = read.csv("Data/AgeRating.csv")

# Create truncated Age variable
choices$AgeMatch = choices$AGE
choices$AgeMatch[choices$AGE<14] = 14
choices$AgeMatch[choices$AGE>64] = 64

# Merge in Default and State-Specific Age Rating Curves

choices = merge(choices,rating[rating$State=="Default",c("Age","Rating")],by.x="AgeMatch",by.y="Age",all.x=TRUE)
choices = merge(choices,rating[rating$State!="Default",],by.x=c("STATE","AgeMatch"),by.y=c("State","Age"),all.x=TRUE)
choices$ageRate = choices$Rating.x
choices$ageRate[!is.na(choices$Rating.y)] = choices$Rating.y[!is.na(choices$Rating.y)]
# Drop redundant rating variables
choices = choices[,c("Rating.x","Rating.y"):=NULL]
rm(rating)


# Modify Age Rate for Family and Smoker
# Are the agents actually paying the extra smoking cost?? Seems unclear at the moment. 
# Assume only one smoker per family
choices$ageRate[choices$MEMBERS==1] = with(choices[choices$MEMBERS==1,],ageRate) #+.5*ageRate*(SMOKER=="Y"))
choices$ageRate[choices$MEMBERS==2] = with(choices[choices$MEMBERS==2,],ageRate*1.9) #+.5*ageRate*(SMOKER=="Y"))
choices$ageRate[choices$MEMBERS==3] = with(choices[choices$MEMBERS==3,],ageRate*2+.5) #+.5*ageRate*(SMOKER=="Y"))
choices$ageRate[choices$MEMBERS>3] = with(choices[choices$MEMBERS>3,],ageRate*2+.6*(MEMBERS-2)) #+.5*ageRate*(SMOKER=="Y"))
choices$ageRate[choices$MEMBERS>5] = with(choices[choices$MEMBERS>5,],ageRate*2+.5*3) #+.5*ageRate*(SMOKER=="Y"))

# Make Premium for Age Rating = 1
choices$premBase = choices$PREMI27/1.048
choices$premBase[choices$STATE=="DC"] = choices$PREMI27[choices$STATE=="DC"]/.727
choices$premBase[choices$STATE=="MA"] = choices$PREMI27[choices$STATE=="MA"]/1.22
choices$premBase[choices$STATE=="MN"] = choices$PREMI27[choices$STATE=="MN"]/1.048
choices$premBase[choices$STATE=="UT"] = choices$PREMI27[choices$STATE=="UT"]/1.39


# Merge in Benchmark
benchmark = read.csv("Intermediate_Output/Premiums/benchmark2015.csv")
choices = merge(choices,benchmark,by.x=c("STATE","AREA"),by.y=c("ST","AREA"),all.x=TRUE)
rm(benchmark)

# Make bechmark for Age Rating = 1
choices$benchBase = choices$bench27/1.048
choices$benchBase[choices$STATE=="DC"] = choices$bench27[choices$STATE=="DC"]/.727
choices$benchBase[choices$STATE=="MA"] = choices$bench27[choices$STATE=="MA"]/1.22
choices$benchBase[choices$STATE=="MN"] = choices$bench27[choices$STATE=="MN"]/1.048
choices$benchBase[choices$STATE=="UT"] = choices$bench27[choices$STATE=="UT"]/1.39

# 2015 FPL Calculation - Individual Only
choices$FPL = with(choices,HOUSEHOLD_INCOME/(11770 + (MEMBERS-1)*4160))

# Calculate Rated Subsidy
choices$Benchmark = with(choices,benchBase*ageRate)
choices$HHcont = subsPerc(choices$FPL)
choices$subsidy = pmax(with(choices,Benchmark - HHcont*HOUSEHOLD_INCOME/12),0)

# If HOUSEHOLD_INCOME is missing, but there is a subsidy observation, take it as true subsidy. 
choices$subsidy[is.na(choices$subsidy)] = choices$FFM_APTC_AMOUNT[is.na(choices$subsidy)]
choices$subsidy[is.na(choices$subsidy)] = 0


#### Impute Income ####
choices$IncCont = with(choices,Benchmark-subsidy)
choices$IncCont[choices$subsidy==0] = NA
choices$IncCont[choices$IncCont<.0201*(11770+(choices$MEMBERS-1)*4160)/12] = NA

# This depends on Family Size to calculate the appropriate poverty line.
# Potentially use reported income and subsidy to impute FPL
# This function could probably be written better...
choices$ImputedIncome[choices$MEMBERS==1] = subsInv(choices$IncCont[choices$MEMBERS==1],pov_line=11770)
choices$ImputedIncome[choices$MEMBERS==2] = subsInv(choices$IncCont[choices$MEMBERS==2],pov_line=11770+4160)
choices$ImputedIncome[choices$MEMBERS==3] = subsInv(choices$IncCont[choices$MEMBERS==3],pov_line=11770+4160*2)
choices$ImputedIncome[choices$MEMBERS==4] = subsInv(choices$IncCont[choices$MEMBERS==4],pov_line=11770+4160*3)
choices$ImputedIncome[choices$MEMBERS==5] = subsInv(choices$IncCont[choices$MEMBERS==5],pov_line=11770+4160*4)
choices$ImputedIncome[choices$MEMBERS==6] = subsInv(choices$IncCont[choices$MEMBERS==6],pov_line=11770+4160*5)
choices$ImputedIncome[choices$MEMBERS==7] = subsInv(choices$IncCont[choices$MEMBERS==7],pov_line=11770+4160*6)
choices$ImputedIncome[choices$MEMBERS==8] = subsInv(choices$IncCont[choices$MEMBERS==8],pov_line=11770+4160*7)
choices$ImputedIncome[choices$MEMBERS==9] = subsInv(choices$IncCont[choices$MEMBERS==9],pov_line=11770+4160*8)
choices$ImputedIncome[choices$MEMBERS==10] = subsInv(choices$IncCont[choices$MEMBERS==10],pov_line=11770+4160*9)


choices$Income = choices$HOUSEHOLD_INCOME
choices$Income[is.na(choices$HOUSEHOLD_INCOME)] = choices$ImputedIncome[is.na(choices$HOUSEHOLD_INCOME)]


# For now, drop missing income observations that recieve subsidies ~ 240 obs roughly divided btw fam and ind
choices = choices[!(is.na(choices$Income)&choices$subsidy>0),]
choices$Income[choices$Income<1000] = 1000

# With Imputed Income, we can get the base premium w/subsidy
choices$FPL_imp = with(choices,Income/(11770 + (MEMBERS-1)*4160))

# Drop incomes less than 100% FPL - 151
choices = choices[FPL_imp>1 | is.na(FPL_imp),]


#### Set Income Specific Choice Sets ####
choices[,CSR:= gsub("[A-Z]+ ?","",METAL)]
choices[,SILVER:=grepl("SILVER",METAL)]

# Set Y = 1 for all Silver, if for any
choices[SILVER==TRUE,Y:= sum(Y),by=c("APP_RECORD_NUM","Firm")]
# Set hix to be TRUE for all Silver, if for any
choices[SILVER==TRUE,hix:= any(hix),by=c("APP_RECORD_NUM","Firm")]
# Set Premium to be the Same for all Silver plans
#choices[,premBase:=median(premBase),by=c("APP_RECORD_NUM","Firm","METAL")]

## Create Character Difference Variables 
choices$High = 0 
choices$High[with(choices,METAL%in%c("GOLD","PLATINUM")|CSR%in%c("94","87"))] = 1

# choices[CSR=="",MedDeductStandard:=MedDeduct,by=c("STATE","AREA","Firm","METAL","FAMILY_OR_INDIVIDUAL")]
# choices[,MedDeductStandard:=max(MedDeductStandard,na.rm=TRUE),by=c("STATE","AREA","Firm","METAL","FAMILY_OR_INDIVIDUAL")]
# choices[CSR=="",MedOOPStandard:=MedOOP,by=c("STATE","AREA","Firm","METAL","FAMILY_OR_INDIVIDUAL")]
# choices[,MedOOPStandard:=max(MedOOPStandard,na.rm=TRUE),by=c("STATE","AREA","Firm","METAL","FAMILY_OR_INDIVIDUAL")]
# choices[CSR=="",HighStandard:=High,by=c("STATE","AREA","Firm","METAL","FAMILY_OR_INDIVIDUAL")]
# choices[,HighStandard:=max(HighStandard,na.rm=TRUE),by=c("STATE","AREA","Firm","METAL","FAMILY_OR_INDIVIDUAL")]
# 
# choices[,MedDeductDiff:=MedDeduct - MedDeductStandard]
# choices[,MedOOPDiff:=MedOOP - MedOOPStandard]
# choices[,HighDiff:=High - HighStandard]

## Set Income Specific Choice Sets
choices$CSR_subs = ""
choices[SILVER&hix&FPL_imp>=2 & FPL_imp<2.5,CSR_subs:="73"]
choices[SILVER&hix&FPL_imp>=1.5 & FPL_imp<2,CSR_subs:="87"]
choices[SILVER&hix&FPL_imp>=1 & FPL_imp<1.5,CSR_subs:="94"]

# Keep only Silver plans for the appropriate income
choices = choices[with(choices,CSR==CSR_subs),]

# Set CSR at the Person Level
choices$CSR = ""
choices[FPL_imp>=2 & FPL_imp<2.5,CSR:="73"]
choices[FPL_imp>=1.5 & FPL_imp<2,CSR:="87"]
choices[FPL_imp>=1 & FPL_imp<1.5,CSR:="94"]


##### Discretize the Data into Type Buckets #####
choices[,FPL_bucket:= "Less than 1"]
choices[FPL_imp>=1&FPL_imp<1.25,FPL_bucket:="1 - 1.25"]
choices[FPL_imp>=1.25&FPL_imp<1.5,FPL_bucket:="1.25 - 1.5"]
choices[FPL_imp>=1.5&FPL_imp<1.75,FPL_bucket:="1.5 - 1.75"]
choices[FPL_imp>=1.75&FPL_imp<2,FPL_bucket:="1.75 - 2"]
choices[FPL_imp>=2&FPL_imp<2.25,FPL_bucket:="2 - 2.25"]
choices[FPL_imp>=2.25&FPL_imp<2.5,FPL_bucket:="2.25 - 2.5"]
choices[FPL_imp>=2.5&FPL_imp<3,FPL_bucket:="2.5 - 3"]
choices[FPL_imp>=3&FPL_imp<3.5,FPL_bucket:="3 - 3.5"]
choices[FPL_imp>=3.5&FPL_imp<4,FPL_bucket:="3.5 - 4"]
choices[is.na(FPL_imp)|FPL_imp>=4,FPL_bucket:="Greater than 4"]


choices[,AGE_bucket:= "Under 18"]
choices[AGE>=18&AGE<=20,AGE_bucket:= "18-20"]
choices[AGE>=21&AGE<=23,AGE_bucket:= "21-23"]
choices[AGE>=24&AGE<=26,AGE_bucket:= "24-26"]
choices[AGE>=27&AGE<=30,AGE_bucket:= "27-30"]
choices[AGE>=31&AGE<=33,AGE_bucket:= "31-33"]
choices[AGE>=34&AGE<=36,AGE_bucket:= "34-36"]
choices[AGE>=37&AGE<=39,AGE_bucket:= "37-39"]
choices[AGE>=40&AGE<=42,AGE_bucket:= "40-42"]
choices[AGE>=43&AGE<=45,AGE_bucket:= "43-45"]
choices[AGE>=46&AGE<=48,AGE_bucket:= "46-48"]
choices[AGE>=49&AGE<=51,AGE_bucket:= "49-51"]
choices[AGE>=52&AGE<=54,AGE_bucket:= "52-54"]
choices[AGE>=55&AGE<=57,AGE_bucket:= "55-57"]
choices[AGE>=58&AGE<=60,AGE_bucket:= "58-60"]
choices[AGE>=61&AGE<=63,AGE_bucket:= "61-63"]
choices[AGE>=64, AGE_bucket:= "64 and up"]

# choices[,AGE_bucket:=vector(mode="integer",nrow(choices))]
# choices[AGE>=26,AGE_bucket:=AGE]
# choices[AGE<26,AGE_bucket:=0]


choices[,Mem_bucket:= "Single"]
choices[MEMBERS==2,Mem_bucket:= "Couple"]
choices[MEMBERS>=3,Mem_bucket:= "3+"]

#test = as.data.frame(choices)
choices = choices[,list(AGE = mean(AGE),
                        ageRate = mean(ageRate),
                        #SMOKER = mean(SMOKER),
                        MEMBERS = mean(MEMBERS),
                        Income = mean(Income,na.rm=TRUE),
                        FPL_imp = mean(FPL_imp,na.rm=TRUE),
                        Y = sum(Y*MEMBERS),
                        N = sum(MEMBERS),
                        subsidy_mean= mean(subsidy)),
                  by=c("STATE","AREA","FPL_bucket","AGE_bucket","Mem_bucket","FAMILY_OR_INDIVIDUAL","Firm","METAL","hix","CSR",
                       "MedDeduct","MedOOP","High",
                       #"MedDeductStandard","MedOOPStandard","HighStandard",
                       #"MedDeductDiff","MedOOPDiff","HighDiff",
                       "benchBase","premBase")]

choices[,S_ij:= Y/N]


## Re-Calculate Premiums for Choice Set
choices[,Benchmark:=benchBase*ageRate]
choices[,HHcont:= subsPerc(FPL_imp)]
choices[,subsidy:= pmax(Benchmark - HHcont*Income/12,0)]
choices[is.na(FPL_imp)|FPL_imp>4,subsidy:=0]
choices[,diff:=subsidy-subsidy_mean]

choices[,Quote:= premBase*ageRate]
choices[,PremPaid:= pmax(premBase*ageRate - subsidy,0)]
choices$PremPaid[choices$METAL=="CATASTROPHIC"] = with(choices[choices$METAL=="CATASTROPHIC",],premBase*ageRate)


# choices[,premMean:=mean(PremPaid),by="APP_RECORD_NUM"]
# test1 = choices[Y==1,list(avg1=mean(PremPaid/premMean),N=sum(Y)),by=c("FPL_bucket","Mem_bucket")]
# setkey(test1,FPL_bucket,Mem_bucket)
# 
# test2 = choices[,list(avg2=mean(PremPaid)),by=c("FPL_bucket","Mem_bucket")]
# test = merge(test1,test2,by=c("FPL_bucket","Mem_bucket"))
# test[,diff:=avg1/avg2]

# choices[,lowest:=(PremPaid==min(PremPaid)),by="APP_RECORD_NUM"]
# choices[,median:=(PremPaid==median(PremPaid)),by="APP_RECORD_NUM"]
# choices[lowest==1,list(shareL=mean(Y),premL=mean(PremPaid))]
# choices[median==1,list(shareL=mean(Y),premL=mean(PremPaid))]

# Per Member Premium
choices[,PremPaid:=PremPaid/MEMBERS]
# Difference Out the Base Premium
choices[,PremPaidDiff:=PremPaid-premBase]

#### Calculate Mandate Penalty ####
# Based on 2015 Tax Thresholds 
choices$Income_Filled = choices$Income
choices$Income_Filled[is.na(choices$Income)] = 70e3 # Check average income given greater than 400 FPL
choices$filingThresh = 10150
choices$filingThresh[choices$FAMILY_OR_INDIVIDUAL=="FAMILY"] = 20300
# Calculate Mandate using average income for non-subsidized people.
choices$Mandate = with(choices, pmin(pmax(325,.02*(Income_Filled-filingThresh)),2484))
choices$Mandate[choices$MEMBERS>1] = with(choices[choices$MEMBERS>1,], pmin(pmax(pmin(325*2+325*.5*(MEMBERS-2),975),
                                                                                 .02*(Income_Filled-filingThresh)),
                                                                            2484*2+2484*.5*(MEMBERS-2)))


#### Merge in Uninsured Rate ####
choices$inc_cat = 1
choices$inc_cat[with(choices,is.na(FPL_imp)|FPL_imp>4)] = 2

choices$AGE_cat = 1
choices$AGE_cat[choices$AGE>35] = 2

choices$mem_cat = 1
choices$mem_cat[choices$MEMBERS==2] = 2
choices$mem_cat[choices$MEMBERS>2] = 3

unins = read.csv("Data/2015_ACS/uninsured_acs2015.csv")

choices = merge(choices,unins,by.x=c("STATE","inc_cat","AGE_cat","mem_cat"),
                    by.y=c("state","inc_cat","AGE_cat","mem_cat"),all.x=TRUE)


choices = choices[,c("STATE","AREA","FPL_bucket","AGE_bucket","Mem_bucket",
                     "FAMILY_OR_INDIVIDUAL","MEMBERS","AGE","Firm","METAL","hix","CSR",
                     "MedDeduct","MedOOP","High",
                     #"MedDeductDiff","MedOOPDiff","HighDiff",
                     #"MedDeductStandard","MedOOPStandard","HighStandard",
                     "ageRate","FPL_imp","Benchmark","HHcont","subsidy","Quote","premBase",
                     "PremPaid","PremPaidDiff","S_ij","N","Income","Mandate","unins_rate")]


# 
# 
# #### Type-Specific Choice Set ####
# # Create app specific metal segment
# choices$sel_metal = NA
# choices$sel_metal = NA
# choices$sel_metal[choices$Y==1] = choices$METAL[choices$Y==1]
# choices$sel_metal = ave(choices$sel_metal,choices$APP_RECORD_NUM,FUN=function(x){max(x,na.rm=TRUE)})
# 
# # Restrict choices to only the metal level inevitably chosen
# # Need to think more on how to deal with CSR Silver and Catastrophic
# choices = choices[choices$METAL==choices$sel_metal,]
# choices = choices[with(choices,order(APP_RECORD_NUM,METAL)),]
# 
# choices = choices[,c("STATE","APP_RECORD_NUM","QUOTED_RATE","FFM_APTC_AMOUNT","FFM_PREMIUM_DUE",
#                      "FAMILY_OR_INDIVIDUAL","MEMBERS","AGE","SMOKER","AREA","Firm","METAL","hix",
#                      "MedDeduct","MedOOP","ageRate","FPL_imp","Benchmark","HHcont","subsidy","Quote",
#                      "PremPaid","Y","Income","logIncome","CSR","Mandate","unins_rate")]
# 

#### Create Dummy Variables ####
choices$Family = 0 
choices$Family[choices$FAMILY_OR_INDIVIDUAL=="FAMILY"] = 1

choices$Age = 0
choices$Age[choices$AGE>=31] = 1

choices$LowIncome = 1
choices$LowIncome[with(choices,is.na(FPL_imp)|FPL_imp>4)] = 0

choices$HighIncome = 0
choices$HighIncome[with(choices,is.na(FPL_imp)|FPL_imp>4)] = 1

choices[,IncomeCts:=FPL_imp]
choices[is.na(FPL_imp)|FPL_imp>4,IncomeCts:=0]

# Create Group Fixed Effects
for (fam in 0:1){
  for (you in 0:1){
    for (inc in 0:1){
      var = paste("F",fam,"_Y",you,"_LI",inc,sep="")
      choices[[var]]=0
      choices[[var]][with(choices,Family==fam&Age==you&LowIncome==inc)]=1
    }
  }
}




#### Break Down to Smallest Estimatable Data
# Product Variables
choices$Market = with(choices,paste(STATE,gsub("Rating Area ","",AREA),CSR,Age,sep="_"))

# Test products by different ages 
choices$Product = with(choices,paste(Firm,METAL,Market,sep="_"))

choices[,Person:=as.factor(paste(Market,FPL_bucket,AGE_bucket,Mem_bucket))]
choices[,Person:=as.numeric(Person)]

## Only Singles: Experiment
choices = choices[Family==0,]

#### Summary Stats for Tables ####
choices[,premMin:=min(PremPaid),by=c("Person")]
choices[,lowestPrem:=as.numeric(premMin==PremPaid)]

t1 = choices[,list(enroll=sum(S_ij*N)),by=c("METAL","lowestPrem")]
t1[,share:=enroll/sum(enroll)]
setkey(t1,METAL,lowestPrem)

t1 = choices[Family==1,list(enroll=sum(S_ij*N)),by=c("METAL","lowestPrem")]
t1[,share:=enroll/sum(enroll)]
setkey(t1,METAL,lowestPrem)

t1 = choices[,list(enroll=sum(S_ij*N)),by=c("METAL","LowIncome")]
t1[,share:=enroll/sum(enroll)]
setkey(t1,METAL,LowIncome)

#### Calculate Product Market Share ####
#unins_st = read.csv("Data/2015_ACS/uninsured_ST_acs2015.csv")
# Take Uninsurance Rates implied in the sample
insured = unique(choices[,c("Person","STATE","unins_rate","N")])
insured = insured[,list(unins_rate=sum(unins_rate*N)/sum(N)),by="STATE"]

shares = choices[,list(enroll=sum(S_ij*N),pop_offered=sum(N)),by=c("Product","Firm","Market","STATE","METAL",
                                                                   "MedDeduct","MedOOP","High","premBase")]
shares = merge(shares,insured,by="STATE")
shares[,lives:=sum(enroll),by="Market"]
shares[,s_inside:= enroll/pop_offered]
shares$Share = shares$s_inside*(1-shares$unins_rate)



firmShares = choices[,list(enroll=sum(S_ij*N*MEMBERS)),by=c("Firm","Market")]
firmShares[,lives:=sum(enroll),by="Market"]
firmShares[,share:= enroll/lives]


absent = firmShares[firmShares$share==0,]

#Drop all firms that are absent in that market
shares = shares[!with(shares,paste(Firm,Market))%in%with(absent,paste(Firm,Market)),]
#Drop markets with less than 10 observations
# One market in Illinois. and older ages in MI. May need to re-add
shares = shares[shares$lives>10,]
#Drop Products with 0 market share
shares = shares[shares$s_inside>0,]

#Drop TX market with only one plan pruchased
shares = shares[Market!="TX_1_94_1",]

# Eliminate the 0 share products from the choice set
choices = choices[choices$Product%in%shares$Product,]

##### Dummy Logit Test ####
# shares[,regvar:=log(Share) - log(1-s_inside)]
# 
# reg = lm(regvar~premBase+MedDeductStandard+MedOOPStandard+HighStandard,data=shares)
# 
# premSummary = shares[,list(premMean=mean(premBase),premMedian=median(premBase),premSD=sd(premBase)),by="Market"]
# mkts = premSummary$Market[with(premSummary,premMean>211&premMean<250)]
# 
# 
# summary(lm(regvar~premBase+METAL,data=shares[Market%in%mkts,]))
# summary(lm(regvar~premBase+METAL,data=shares[!Market%in%c("AK_1","AK_2","AK_3"),]))
# summary(lm(regvar~premBase+METAL,data=shares[,]))
# 
# reg = lm(regvar~premBase+MedDeductStandard+HighStandard,data=shares[!Market%in%c("AK_1","AK_2","AK_3"),])
# shares[!Market%in%c("AK_1","AK_2","AK_3"),reg_pred:=predict(reg)]
# shares[!Market%in%c("AK_1","AK_2","AK_3"),resid:=exp(Share)-exp(reg_pred)]



#### Clean and Print ####
#choices$Price = (choices$PremPaid*12-choices$Mandate)/1000
choices$Price = (choices$PremPaid*12)/1000
choices$PriceDiff = (choices$PremPaidDiff*12)/1000
choices$MedDeduct = choices$MedDeduct/1000
choices$MedDeductDiff = choices$MedDeductDiff/1000
choices$MedOOP = choices$MedOOP/1000
choices$MedOOPDiff = choices$MedOOPDiff/1000
choices[,ExcOOP:= (MedOOP - MedDeduct)]
choices[,ExcOOPDiff:= (MedOOPDiff - MedDeductDiff)]

choices$Product = as.factor(choices$Product)
shares$Product_Name = factor(shares$Product,levels=levels(choices$Product))

choices$Product = as.numeric(choices$Product)
shares$Product = as.numeric(shares$Product_Name)

choices = choices[with(choices,order(Person,Product)),]
setkey(choices,Person,Product)
setkey(shares,Product)

write.csv(choices[,c("Person","Firm","Market","Product","S_ij","N","Price",
                     "PriceDiff",#"MedDeductDiff","ExcOOPDiff","HighDiff",
                     "MedDeduct","ExcOOP","High",
                     "Family","Age","LowIncome",
                     "F0_Y0_LI0","F0_Y0_LI1","F0_Y1_LI0","F0_Y1_LI1",
                     "F1_Y0_LI0","F1_Y0_LI1","F1_Y1_LI0","F1_Y1_LI1","unins_rate")],
          "Intermediate_Output/Estimation_Data/estimationData_discrete.csv",row.names=FALSE)
write.csv(choices,"Intermediate_Output/Estimation_Data/descriptiveData_discrete.csv",row.names=FALSE)
write.csv(shares[,c("Product","Share")],
          "Intermediate_Output/Estimation_Data/marketData_discrete.csv",row.names=FALSE)
write.csv(shares,
          "Intermediate_Output/Estimation_Data/marketDataMap_discrete.csv",row.names=FALSE)

# Create mini Michigan Dataset and Renumber Products
# MI = choices[STATE=="MI"&Market%in%c("MI_1_0","MI_1_1"),]
# MI_mkt = shares[STATE=="MI"&Market%in%c("MI_1_0","MI_1_1"),]
MI = choices[STATE=="MI",]
MI_mkt = shares[STATE=="MI",]

MI$Product = as.factor(MI$Product)
MI_mkt$Product = factor(MI_mkt$Product,levels=levels(MI$Product))

MI$Product = as.numeric(MI$Product)
MI_mkt$Product = as.numeric(MI_mkt$Product)

setkey(MI,Person,Product)
setkey(MI_mkt,Product)


vars = c("Person","Firm","Market","Product","S_ij","N","Price",
         "MedDeduct","ExcOOP","High","MedDeductDiff","ExcOOPDiff","HighDiff",
         "Family","Age","LowIncome",names(choices)[grepl("F[0-9]_Y.*",names(choices))],"unins_rate")

write.csv(MI[,c("Person","Firm","Market","Product","S_ij","N","Price",
                "PriceDiff",#"MedDeductDiff","ExcOOPDiff","HighDiff",
                "MedDeduct","ExcOOP","High",
                "Family","Age","LowIncome","AGE","HighIncome","IncomeCts",
                "F0_Y0_LI0","F0_Y0_LI1","F0_Y1_LI0","F0_Y1_LI1",
                "F1_Y0_LI0","F1_Y0_LI1","F1_Y1_LI0","F1_Y1_LI1","unins_rate")],
          "Intermediate_Output/Estimation_Data/estimationData_MI_discrete.csv",row.names=FALSE)
write.csv(MI_mkt[,c("Product","Share")],
          "Intermediate_Output/Estimation_Data/marketData_MI_discrete.csv",row.names=FALSE)
write.csv(MI_mkt,
          "Intermediate_Output/Estimation_Data/marketDataMap_MI_discrete.csv",row.names=FALSE)



#### Tests

# shares = choices[,list(enroll=sum(S_ij*N),pop_offered=sum(N)),by=c("Product","Firm","Market","STATE")]
# shares = merge(shares,unins_st,by.x="STATE",by.y="state")
# shares[,lives:=sum(enroll),by="Market"]
# shares[,s_inside:= enroll/pop_offered]
# shares$Share = shares$s_inside*(1-shares$unins_rate)
