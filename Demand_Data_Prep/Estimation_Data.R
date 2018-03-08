rm(list=ls())
library(doBy)
library(data.table)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition")

#### 2015 Subsidy Percentage Function ####

subsPerc <- function(FPL){
  x = FPL[!is.na(FPL)]
  y = vector(mode="numeric",length=length(x))
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
subsInv <- function(cont,pov_line = 11770){
  
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
  Income[!is.na(cont)] = x*pov_line
  
  return(Income)
}


#### Read in eHealth Data and clean Premium Information ####
eHealth = read.csv("C:/Users/Conor/Documents/Research/eHealth Data/eHealth_2015.csv",stringsAsFactors = FALSE)

# Firm Crosswalk
firmCrosswalk = read.csv("Intermediate_Output/FirmCrosswalk.csv")
firmCrosswalk = unique(firmCrosswalk[,c("STATE","eHealth_CARRIER_NAME","Firm")])
eHealth = merge(eHealth,firmCrosswalk,by.x=c("STATE","CARRIER_NAME"),by.y=c("STATE","eHealth_CARRIER_NAME"),all.x=TRUE)


# Drop "referential integrity" rows
eHealth = eHealth[eHealth$PLAN_NAME!="UNKNOWN PLAN - this row is used for referential integrity - DSTOCK",]

# Drop eHealth observations with NA or 0 zip code
eHealth = eHealth[with(eHealth,!is.na(TRUNCATED_ZIP)&TRUNCATED_ZIP!=0 ),]
eHealth = eHealth[with(eHealth,PLAN_METAL_LEVEL!="N/A"),]


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
choices = merge(choiceSets,mapping[,c("ST","Zip3","RatingArea","alloc")],by.x=c("ST","AREA"),by.y=c("ST","RatingArea"),all.x=TRUE)

# Merge in eHealth data
eHealth = eHealth[eHealth$TRUNCATED_ZIP%in%choices$Zip3,]
choices = merge(eHealth,choices,by.x=c("STATE","TRUNCATED_ZIP"),by.y=c("ST","Zip3"),all.x=TRUE)
choices = choices[with(choices,order(APP_RECORD_NUM,METAL)),]

names(choices)[names(choices)=="Firm.x"] = "Firm_Choice"
choices$Firm_Choice = as.character(choices$Firm_Choice)
names(choices)[names(choices)=="Firm.y"] = "Firm"
choices$Firm = as.character(choices$Firm)
# Not Dropping kids for now
#choices = choices[with(choices,AGE>18),]

#rm(mapping,eHealth,choiceSets)

#### Create Valid Choice Sets ####

# Set Choice Indicator
choices$METAL = toupper(choices$METAL)
choices$Y = 0
choices$Y[with(choices,METAL==PLAN_METAL_LEVEL&Firm_Choice==Firm)]=1

ids = unique(choices$APP_RECORD_NUM[choices$Y==1])
missing = eHealth$APP_RECORD_NUM[!eHealth$APP_RECORD_NUM%in%ids]
test = choices[choices$APP_RECORD_NUM%in%missing,]
test = unique(test[!test$Firm_Choice%in%c("HEALTHYCT","SHARP_HEALTH_PLAN"),c("APP_RECORD_NUM","Firm_Choice","AREA","STATE")])


#test = eHealth[eHealth$APP_RECORD_NUM%in%missing,]


# Drop Catastrophic for over 30. 
# Lose two observations that chose catas over 30
choices = choices[!with(choices,AGE>30 & METAL=="CATASTROPHIC"),]


# If choice is not available, then that individual is not in the right rating area
# This step drops CA/CT (data collection needed), 3 obs in IL that don't have right rating area
choices$flag = ave(choices$Y,with(choices,paste(APP_RECORD_NUM,AREA)),FUN=sum)
choices = choices[choices$flag!=0,]

# Remove non-valid areas
choices = choices[choices$valid,]

# Keep only most likely rating area
choices$maxAlloc = ave(choices$alloc,choices$APP_RECORD_NUM,FUN=max)
choices = choices[with(choices,alloc==maxAlloc),]



# Roll in Family Characteristcs
choices$MedDeduct[choices$FAMILY_OR_INDIVIDUAL=="FAMILY"] = 
  choices$MedDeductFam[choices$FAMILY_OR_INDIVIDUAL=="FAMILY"]
choices$MedOOP[choices$FAMILY_OR_INDIVIDUAL=="FAMILY"] = 
  choices$MedOOPFam[choices$FAMILY_OR_INDIVIDUAL=="FAMILY"]


# Keep only relevant variables
choices = choices[,c("STATE","APP_RECORD_NUM","QUOTED_RATE","HOUSEHOLD_INCOME","FFM_APTC_AMOUNT","FFM_PREMIUM_DUE",
                     "FAMILY_OR_INDIVIDUAL","MEMBERS","AGE","SMOKER","AREA","Firm","METAL","hix","PREMI27",
                     "MedDeduct","MedOOP","Y")]


#### Set Correct Premiums ####
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
choices = choices[,which(!names(choices)%in%c("Rating.x","Rating.y"))]
rm(rating)


# Modify Age Rate for Family and Smoker
# Are the agents actually paying the extra smoking cost?? Seems unclear at the moment. 
# Assume only one smoker per family
choices$ageRate[choices$MEMBERS==1] = with(choices[choices$MEMBERS==1,],ageRate+.5*ageRate*(SMOKER=="Y"))
choices$ageRate[choices$MEMBERS==2] = with(choices[choices$MEMBERS==2,],ageRate*1.9+.5*ageRate*(SMOKER=="Y"))
choices$ageRate[choices$MEMBERS==3] = with(choices[choices$MEMBERS==3,],ageRate*2+.5+.5*ageRate*(SMOKER=="Y"))
choices$ageRate[choices$MEMBERS>3] = with(choices[choices$MEMBERS>3,],ageRate*2+.6*(MEMBERS-2)+.5*ageRate*(SMOKER=="Y"))
choices$ageRate[choices$MEMBERS>5] = with(choices[choices$MEMBERS>5,],ageRate*2+.5*(MEMBERS-2)+.5*ageRate*(SMOKER=="Y"))

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


# Calculate Premiums for Choice Set
choices$Quote = with(choices,premBase*ageRate)
choices$PremPaid = pmax(with(choices,premBase*ageRate - subsidy),0)
choices$PremPaid[choices$METAL=="CATASTROPHIC"] = with(choices[choices$METAL=="CATASTROPHIC",],premBase*ageRate)

# Keep Relevant Variables
choices = choices[,c("STATE","APP_RECORD_NUM","QUOTED_RATE","HOUSEHOLD_INCOME","FFM_APTC_AMOUNT","FFM_PREMIUM_DUE",
                     "FAMILY_OR_INDIVIDUAL","MEMBERS","AGE","SMOKER","AREA","Firm","METAL","hix",
                     "MedDeduct","MedOOP","ageRate","FPL","Benchmark","HHcont","subsidy","Quote","PremPaid","Y")]

#### Impute Income ####


choices$IncCont = with(choices,Benchmark-subsidy)
choices$IncCont[choices$subsidy==0] = NA
choices$IncCont[choices$IncCont<.0201*(11770+(choices$MEMBERS-1)*4160)/12] = NA

# This depends on Family Size to calculate the appropriate poverty line.
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

# Use Log Income
choices$logIncome = log(choices$Income)
choices$logIncome[choices$logIncome<0] = 0

# With Imputed Income, we can get the base premium w/subsidy
choices$FPL_imp = with(choices,Income/(11770 + (MEMBERS-1)*4160))

#### Set Income Specific Choice Sets ####
choices$CSR = gsub("[A-Z]+ ?","",choices$METAL)
choices$METAL = gsub(" [0-9]+","",choices$METAL)

# Set Y = 1 for all Silver, if for any
choices$Y = ave(choices$Y,with(choices,paste(APP_RECORD_NUM,Firm,METAL)),FUN=sum)
# Set hix to be TRUE for all Silver, if for any
choices$hix = ave(choices$hix,with(choices,paste(APP_RECORD_NUM,Firm,METAL)),FUN=any)

choices$CSR_subs = ""
choices$CSR_subs[with(choices,METAL=="SILVER"&hix&FPL_imp>2 & FPL_imp<=2.5)] = "73"
choices$CSR_subs[with(choices,METAL=="SILVER"&hix&FPL_imp>1.5 & FPL_imp<=2)] = "87"
choices$CSR_subs[with(choices,METAL=="SILVER"&hix&FPL_imp>=1 & FPL_imp<=1.5)] = "94"

# Keep only Silver plans for the appropriate income
choices = choices[with(choices,CSR==CSR_subs),]


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


choices = choices[,c("STATE","APP_RECORD_NUM","QUOTED_RATE","FFM_APTC_AMOUNT","FFM_PREMIUM_DUE",
                     "FAMILY_OR_INDIVIDUAL","MEMBERS","AGE","SMOKER","AREA","Firm","METAL","hix",
                     "MedDeduct","MedOOP","ageRate","FPL_imp","Benchmark","HHcont","subsidy","Quote",
                     "PremPaid","Y","Income","logIncome","CSR","Mandate")]


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

choices$Youth = 1
choices$Youth[choices$AGE>35] = 0

choices$LowIncome = 1
choices$LowIncome[with(choices,is.na(FPL_imp)|FPL_imp>4)] = 0

choices$High = 0 
choices$High[with(choices,METAL%in%c("GOLD","PLATINUM")|CSR%in%c("94","87"))] = 1


# Create Group Fixed Effects
for (fam in 0:1){
  for (you in 0:1){
    for (inc in 0:1){
      var = paste("F",fam,"_Y",you,"_LI",inc,sep="")
      choices[[var]]=0
      choices[[var]][with(choices,Family==fam&Youth==you&LowIncome==inc)]=1
    }
  }
}


#### Summary Stats for Tables ####
sumData = choices[choices$Y==1,]
t1 = summaryBy(MEMBERS~METAL,data=sumData,FUN=sum,keep.names=TRUE)
t1$MEMBERS = t1$MEMBERS/sum(t1$MEMBERS)

t2 = summaryBy(MEMBERS~METAL,data=sumData[sumData$LowIncome==0,],FUN=sum,keep.names=TRUE)
t2$MEMBERS = t2$MEMBERS/sum(t2$MEMBERS)




#### Break Down to Smallest Estimatable Data
# Product Variables
choices$Market = with(choices,paste(STATE,gsub("Rating Area ","",AREA),sep="_"))

choices$Product = with(choices,paste(Firm,METAL,Market,sep="_"))

#### Calculate Product Market Share ####
unins_st = read.csv("Data/2015_ACS/uninsured_ST_acs2015.csv")

choices$count = 1
shares = summaryBy(count+Y~Product+Firm+Market+STATE,data=choices,FUN=sum,keep.names=TRUE)
shares = merge(shares,unins_st,by.x="STATE",by.y="state")
shares$s_inside = with(shares,Y/count)
shares$Share = shares$s_inside*(1-shares$unins_rate)
shares$marketTotal = ave(shares$Y,shares$Market,FUN=sum)

firmShares = summaryBy(Y~Firm+Market,data=choices,FUN=sum,keep.names=TRUE)
firmShares$marketTotal = ave(firmShares$Y,firmShares$Market,FUN=sum)
firmShares$share = firmShares$Y/firmShares$marketTotal


absent = firmShares[firmShares$share==0,]

#Drop all firms that are absent in that market
shares = shares[!with(shares,paste(Firm,Market))%in%with(absent,paste(Firm,Market)),]
#Drop markets with less than 10 observations
shares = shares[shares$marketTotal>10,]
#Drop Products with 0 market share
shares = shares[shares$s_inside>0,]

# Eliminate the 0 share products from the choice set
choices = choices[choices$Product%in%shares$Product,]

#### Clean and Print ####
choices$Price = (choices$PremPaid*12-choices$Mandate)/1000
choices$MedDeduct = choices$MedDeduct/1000
choices$MedOOP = choices$MedOOP/1000

choices$Age = 0
choices$Age[choices$AGE>=40] = 1
choices$Person = as.character(choices$APP_RECORD_NUM)

choices$Product = as.factor(choices$Product)
shares$Product_Name = factor(shares$Product,levels=levels(choices$Product))

choices$Product = as.numeric(choices$Product)
shares$Product = as.numeric(shares$Product_Name)

choices = choices[with(choices,order(Person,Product)),]
shares = shares[order(shares$Product),]

write.csv(choices[,c("Person","Firm","Market","Product","Y","Price","MedDeduct","MedOOP","High","Family","Age","LowIncome",names(choices)[grepl("F[0-9]_Y.*",names(choices))],"unins_rate")],
          "Intermediate_Output/Estimation_Data/estimationData.csv",row.names=FALSE)
write.csv(shares[,c("Product","Share")],
          "Intermediate_Output/Estimation_Data/marketData.csv",row.names=FALSE)
write.csv(shares[,c("Product_Name","Product","Share","Firm","Market","STATE")],
          "Intermediate_Output/Estimation_Data/marketDataMap.csv",row.names=FALSE)

# Create mini Michigan Dataset and Renumber Products
MI = choices[choices$STATE=="MI",]
MI_mkt = shares[shares$STATE=="MI",]

MI$Product = as.factor(MI$Product)
MI_mkt$Product = factor(MI_mkt$Product,levels=levels(MI$Product))

MI$Product = as.numeric(MI$Product)
MI_mkt$Product = as.numeric(MI_mkt$Product)

MI = MI[with(MI,order(Person,Product)),]
MI_mkt = MI_mkt[order(MI_mkt$Product),]

write.csv(MI[,c("Person","Firm","Market","Product","Y","Price","MedDeduct","MedOOP","High","Family","Age","LowIncome",names(choices)[grepl("F[0-9]_Y.*",names(choices))],"unins_rate")],
          "Intermediate_Output/Estimation_Data/estimationData_MI.csv",row.names=FALSE)
write.csv(MI_mkt[,c("Product","Share")],
          "Intermediate_Output/Estimation_Data/marketData_MI.csv",row.names=FALSE)
write.csv(MI_mkt[,c("Product_Name","Product","Share","Firm","Market","STATE")],
          "Intermediate_Output/Estimation_Data/marketDataMap_MI.csv",row.names=FALSE)


#### Product Analysis #### 
products = unique(choices[,c("STATE","Market","Firm")])
nrow(products)
products$count = 1
products = summaryBy(count~Firm+METAL,data=products,FUN=sum,keep.names=TRUE)
products = products[order(products$count),]


#### Benchmark ####
alpha = seq(0,1,length.out=9)/1000
beta = c(1,1)/1000
phi  = seq(0,1,length.out=9)/1000
gamma = c(1,1,1,1)/1000

par = c(alpha,beta,phi,gamma)

#### Likelihood Function #####

choices.Est = as.data.table(choices[,
                                    c("Person","Firm","Market","Product","Y","Price","MedDeduct",
                                      "MedOOP","Family","Age","LowIncome",
                                      names(choices)[grepl("F[0-9]_Y.*",names(choices))],
                                      "unins_rate")]
)


delta = data.table(delta = 0, Product = unique(choices.Est$Product))
setkey(delta,Product)
setorder(delta,Product)

merge_helper = choices.Est[,"Product"]

setkey(choices.Est,Person)
setkey(choices.Est,Product)
setkey(merge_helper,Product)

choices.Est$count = 1
marketshares = choices.Est[,.(sum(Y), sum(count)),keyby=Product]
marketshares$share = with(marketshares,V1/V2)
# Get rid of 0s
marketshares$share[marketshares$share==0] = NA


logsumExp <- function(x){log(1+sum(exp(x)))}
sumExp <- function(x){sum(exp(x))}

insideLL <- function(par){
  alpha = par[1:9]
  beta = par[10:11]
  phi  = par[12:20]
  gamma = par[21:length(par)]
  
  dataEst = choices.Est
  
  dataEst$withinMarket = with(dataEst,
                          Price*(alpha[1]+
                                   alpha[2]*F0_Y0_LI0 + 
                                   alpha[3]*F0_Y0_LI1 + 
                                   alpha[4]*F0_Y1_LI0 + 
                                   alpha[5]*F0_Y1_LI1 + 
                                   alpha[6]*F1_Y0_LI0 + 
                                   alpha[7]*F1_Y0_LI1 + 
                                   alpha[8]*F1_Y1_LI0 + 
                                   alpha[9]*F1_Y1_LI1) +
                            (MedDeduct*beta[1] + MedOOP*beta[2])*(phi[1] +
                                                                    phi[2]*F0_Y0_LI0 + 
                                                                    phi[3]*F0_Y0_LI1 + 
                                                                    phi[4]*F0_Y1_LI0 + 
                                                                    phi[5]*F0_Y1_LI1 + 
                                                                    phi[6]*F1_Y0_LI0 + 
                                                                    phi[7]*F1_Y0_LI1 + 
                                                                    phi[8]*F1_Y1_LI0 + 
                                                                    phi[9]*F1_Y1_LI1) +
                            gamma[1] +
                            Family*gamma[2] + 
                            Age*gamma[3] + 
                            LowIncome*gamma[4]
  )

  
  # Contraction Mapping
  err = 1
  tol = 1e-10
  cnt = 0
  while (err>tol&cnt<1000){
    delta_long = merge(merge_helper,delta)
    dataEst[,delta:=delta_long$delta]
    dataEst[,delta_ij:= delta + withinMarket]
    dataEst[,exp_ij:= exp(delta_ij)]
    dataEst[,exp_sum:=lapply(.SD,sum,na.rm=TRUE),by = Person,.SDcols = "exp_ij"]
    dataEst[,s_ij:=exp_ij/(exp_sum)]
    s_j = dataEst[,mean(s_ij),keyby=Product]
    #delta[,delta:= delta + (log(marketshares$share) - log(s_j$V1))]
    delta$delta = delta$delta + (log(marketshares$share) - log(s_j$V1))
    err = max(abs(log(marketshares$share) - log(s_j$V1)),na.rm=TRUE)
    # if (err>1 & cnt>5){
    #   delta$delta=1
    #   break
    # }
    cnt = cnt + 1
    #print(err)
  }
  print(cnt)
  delta$delta[is.na(delta$delta)] = -10
  delta_long = merge(merge_helper,delta)
  dataEst[,delta:=delta_long$delta]
  dataEst[,delta_ij:= delta + withinMarket]

  dataEst[,totexp:=sumExp(delta_ij), by=Person]
  dataEst[,logtotexp:=logsumExp(delta_ij), by=Person ]
  
  LL_Inside = dataEst[,sum((1-unins_rate)*Y*(delta_ij-log(totexp)))]
  LL_Outside = dataEst[,sum(Y*((1-unins_rate)*log(totexp)-logtotexp))]
  LL = LL_Inside+LL_Outside
  print(LL)
  # print(beta)
  return(-LL)
}

start.time <- Sys.time()
insideLL(par)
Sys.time()-start.time

x = rep(5.5,55e6)
start.time <- Sys.time()
y=exp(x)
Sys.time()-start.time
