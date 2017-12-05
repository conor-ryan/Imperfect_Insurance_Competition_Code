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

# Standardize eHealth Names
eHealth$CARRIER_NAME[grep("Cigna",eHealth$CARRIER_NAME)] = "Cigna"
eHealth$CARRIER_NAME[grep("Health Net",eHealth$CARRIER_NAME)] = "Health Net"
eHealth$CARRIER_NAME[grep("Aetna",eHealth$CARRIER_NAME)] = "Aetna"
eHealth$CARRIER_NAME[grep("Anthem",eHealth$CARRIER_NAME)] = "Anthem"
eHealth$CARRIER_NAME[grep("Humana",eHealth$CARRIER_NAME)] = "Humana"
eHealth$CARRIER_NAME[grep("Coventry",eHealth$CARRIER_NAME)] = "Coventry"
eHealth$CARRIER_NAME[grep("Ambetter",eHealth$CARRIER_NAME)] = "Ambetter"
eHealth$CARRIER_NAME[grep("Kaiser",eHealth$CARRIER_NAME)] = "Kaiser"
eHealth$CARRIER_NAME[grep("Moda",eHealth$CARRIER_NAME)] = "Moda"
eHealth$CARRIER_NAME[grep("Molina",eHealth$CARRIER_NAME)] = "Molina"
eHealth$CARRIER_NAME[grep("Health Republic",eHealth$CARRIER_NAME)] = "Health Republic"
eHealth$CARRIER_NAME[grep("Oscar",eHealth$CARRIER_NAME)] = "Oscar"
eHealth$CARRIER_NAME[grep("UnitedHealth",eHealth$CARRIER_NAME)] = "UnitedHealthcare"


eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Blue Cross Blue Shield of Georgia"] = "Anthem"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Wellmark Blue Cross and Blue Shield of Iowa"] = "Wellmark Blue Cross Blue Shield of Iowa"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Blue Cross and Blue Shield of Minnesota"] = "Blue Plus"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Blue Cross and Blue Shield of MN"] = "Blue Plus"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="BlueCross BlueShield of Nebraska"] = "Blue Cross and Blue Shield of Nebraska"
eHealth$CARRIER_NAME[grep("Blue Cross of Northeastern Penn",eHealth$CARRIER_NAME)] = "Blue Cross of Northeastern Pennsylvania"


eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Land of Lincoln Health"] = "Land of Lincoln Mutual Health Insurance Company"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="HealthPlus"] = "HealthPlus Insurance Company"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="BridgeSpan"] = "BridgeSpan Health Company"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Geisinger Health Plan"] = "Geisinger Choice"


eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Arkansas Health and Wellness"] = "Ambetter"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="QCA Health Plan Inc"] = "QualChoice"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="ConnectiCare Inc."] = "ConnectiCare"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="PacificSource Health Plans of Idaho"] = "PacificSource Health Plans"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Ambetter" & eHealth$STATE=="IL"] = "IlliniCare Health"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="My Health Alliance"] = "Health Alliance Medical Plans"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="All Savers Insurance Company"] = "UnitedHealthcare"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Physicians Health Plan of Northern Indiana, Inc."] = "PHP"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Health Alliance Plan" & eHealth$STATE == "MI"] = "HAP"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Medica of Minnesota"] = "UnitedHealthcare"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="AmeriHealth - New Jersey"] = "AmeriHealth"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Oxford NJ"] = "UnitedHealthcare"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Presbyterian"] = "Presbyterian Health Plan, Inc."
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Health Plan of Nevada"] = "UnitedHealthcare"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Sierra Health and Life"] = "UnitedHealthcare"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Health Republic Insurance of New York"] = "Health Republic"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="MVP"] = "MVP Health Plans"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="SummaCare Inc of Ohio"] = "SummaCare"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="HealthAmerica Pennsylvania"] = "Coventry"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="FirstCare"] = "Firstcare Health Plans"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Scott & White Health Plan"] = "Insurance Company of Scott & White"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Altius Health Plans"] = "Altius"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Anthem" & eHealth$STATE=="VA"] = "HealthKeepers, Inc."
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Innovation Health"] = "Innovation Health Insurance Company"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Optima Health Plan"] = "Optima Health"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="LifeWise Health Plan of Washington"] = "LifeWise Health Plan of WA"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Dean Health Plan, Inc."] = "Dean Health Plan"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Medica" & eHealth$STATE == "WI"] = "UnitedHealthcare"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Physicians Plus"] = "Physicians Plus Insurance Corporation"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Security Health Plan"] = "Security Health Plan of Wisconsin, Inc."
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="Prevea360 Health Plan"] = "Dean Health Plan"
eHealth$CARRIER_NAME[eHealth$CARRIER_NAME=="WPS Health Insurance"] = "Wisconsin Physicians Svc Insurance Corp"


# Drop "referential integrity" rows
eHealth = eHealth[eHealth$PLAN_NAME!="UNKNOWN PLAN - this row is used for referential integrity - DSTOCK",]

# Drop eHealth observations with NA or 0 zip code
eHealth = eHealth[with(eHealth,!is.na(TRUNCATED_ZIP)&TRUNCATED_ZIP!=0 ),]
eHealth = eHealth[with(eHealth,PLAN_METAL_LEVEL!="N/A"),]

# Subset eHealth for Valid Markets
STselection = c("AK","AR","CA","CT","DE","GA","IL","IA","KS","MD","MI","MN","MO","NE","NJ","NM","OK","OR","PA","TX","UT","VA","WV")


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
choices = read.csv("Intermediate_Output/Premiums/choiceSets2015.csv")
mapping = read.csv("Intermediate_Output/Zip_RatingArea/Zip3_to_RatingArea.csv")
choices = merge(choices,mapping[,c("ST","Zip3","RatingArea","alloc")],by.x=c("ST","AREA"),by.y=c("ST","RatingArea"),all.x=TRUE)

# Merge in eHealth data
eHealth = eHealth[eHealth$TRUNCATED_ZIP%in%choices$Zip3,]
choices = merge(eHealth,choices,by.x=c("STATE","TRUNCATED_ZIP"),by.y=c("ST","Zip3"),all.x=TRUE)
choices = choices[with(choices,order(APP_RECORD_NUM,METAL)),]

# Not Dropping kids for now
#choices = choices[with(choices,AGE>18),]

#rm(mapping,eHealth)

#### Create Valid Choice Sets ####

# Set Choice Indicator
choices$METAL = toupper(choices$METAL)
choices$Y = 0
choices$Y[with(choices,METAL==PLAN_METAL_LEVEL&CARRIER_NAME==CARRIER)]=1

ids = unique(choices$APP_RECORD_NUM[choices$Y==1])
missing = eHealth$APP_RECORD_NUM[!eHealth$APP_RECORD_NUM%in%ids]
test = choices[choices$APP_RECORD_NUM%in%missing,]

# Drop Catastrophic for over 30. 
# Lose two observations that chose catas over 30
choices = choices[!with(choices,AGE>30 & METAL=="CATASTROPHIC"),]


# If choice is not available, then that individual is not in the right rating area
# This step drops PA, CA/CT (data collection needed), 3 obs in IL that don't have right rating area
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
                     "FAMILY_OR_INDIVIDUAL","MEMBERS","AGE","SMOKER","AREA","CARRIER","METAL","hix","PREMI27",
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
                     "FAMILY_OR_INDIVIDUAL","MEMBERS","AGE","SMOKER","AREA","CARRIER","METAL","hix",
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
choices$Y = ave(choices$Y,with(choices,paste(APP_RECORD_NUM,CARRIER,METAL)),FUN=sum)
# Set hix to be TRUE for all Silver, if for any
choices$hix = ave(choices$hix,with(choices,paste(APP_RECORD_NUM,CARRIER,METAL)),FUN=any)

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
                     "FAMILY_OR_INDIVIDUAL","MEMBERS","AGE","SMOKER","AREA","CARRIER","METAL","hix",
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

unins = read.csv("Data/uninsured_acs2015.csv")

choices = merge(choices,unins,by.x=c("STATE","inc_cat","AGE_cat","mem_cat"),
                    by.y=c("state","inc_cat","AGE_cat","mem_cat"),all.x=TRUE)



#### Type-Specific Choice Set ####
# Create app specific metal segment
choices$sel_metal = NA
choices$sel_metal = NA
choices$sel_metal[choices$Y==1] = choices$METAL[choices$Y==1]
choices$sel_metal = ave(choices$sel_metal,choices$APP_RECORD_NUM,FUN=function(x){max(x,na.rm=TRUE)})

# Restrict choices to only the metal level inevitably chosen
# Need to think more on how to deal with CSR Silver and Catastrophic
choices = choices[choices$METAL==choices$sel_metal,]
choices = choices[with(choices,order(APP_RECORD_NUM,METAL)),]

choices = choices[,c("STATE","APP_RECORD_NUM","QUOTED_RATE","FFM_APTC_AMOUNT","FFM_PREMIUM_DUE",
                     "FAMILY_OR_INDIVIDUAL","MEMBERS","AGE","SMOKER","AREA","CARRIER","METAL","hix",
                     "MedDeduct","MedOOP","ageRate","FPL_imp","Benchmark","HHcont","subsidy","Quote",
                     "PremPaid","Y","Income","logIncome","CSR","Mandate","unins_rate")]

#### Create Dummy Variables ####
choices$Family = 0 
choices$Family[choices$FAMILY_OR_INDIVIDUAL=="FAMILY"] = 1

choices$Youth = 1
choices$Youth[choices$AGE>35] = 0

choices$LowIncome = 1
choices$LowIncome[with(choices,is.na(FPL_imp)|FPL_imp>4)] = 0

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




#### Break Down to Smallest Estimatable Data
# Product Variables
choices$Firm = gsub(" ","_",choices$CARRIER)
choices$Firm = gsub("[,.&'-:]","",choices$Firm)


choices$Product = with(choices,paste(Firm,METAL,sep="_"))

choices$Market = with(choices,paste(STATE,gsub("Rating Area ","",AREA),sep="_"))

# A little cleaning
choices$Price = choices$PremPaid
choices$Price = choices$PremPaid

choices$Age = choices$AGE
choices$Person = choices$APP_RECORD_NUM

write.csv(choices[,c("Person","Firm","Market","Product","Y","Price","MedDeduct","MedOOP","Family","Age","LowIncome",names(choices)[grepl("F[0-9]_Y.*",names(choices))],"unins_rate")],
          "Intermediate_Output/estimationData.csv",row.names=FALSE)

#### Product Analysis #### 
products = unique(choices[,c("STATE","Market","Firm")])
nrow(products)
products$count = 1
products = summaryBy(count~CARRIER+METAL,data=products,FUN=sum,keep.names=TRUE)
products = products[order(products$count),]

