rm(list=ls())
library(doBy)
library(randtoolbox)
library(data.table)
library(stargazer)
library(ggplot2)
library(reldist)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")


#### Read in estimation Sample ####

load("Intermediate_Output/Estimation_Data/estimationData.rData")

est_areas = choices[,list(wgt=sum(N)),by=c("STATE","AREA")]
est_areas[,inSample:=TRUE]
rm(choices)

#### Process ASPE County-level Data #####

### Age Demographics 
demos = read.csv("Data/ASPE Data/CountyDemos.csv")
demos = demos[,c(1,4:12)]
names(demos) = c("FIPS.code","Total","Age.lt.18",names(demos)[4:10])
## Impute average value of * 
for (v in names(demos)[2:10]){
  demos[[v]]=as.numeric(gsub("[*,]","",demos[[v]]))
}

demos$nonMissing = 0
demos$numMissing = 0
for (v in names(demos)[3:10]){
  var_vector = demos[[v]]
  missing_vector = as.numeric(is.na(var_vector))
  var_vector[is.na(var_vector)] = 0
  demos$nonMissing = demos$nonMissing + var_vector
  demos$numMissing = demos$numMissing + missing_vector
  rm(missing_vector,var_vector)
}

demos$imputed_values = with(demos,(Total-nonMissing)/numMissing)
for (v in names(demos)[2:10]){
  demos[[v]][is.na(demos[[v]])] = demos$imputed_values[is.na(demos[[v]])]
}

demos = as.data.table(demos[,1:10])


### Income 
income = read.csv("Data/ASPE Data/CountyIncome.csv")
income = income[,c(1,4:12)]
names(income) = c("FIPS.code","Total","FPL.lt.100","FPL.100.150","FPL.150.200","FPL.200.250","FPL.250.300","FPL.300.400","FPL.gt.400","FPL.Unknown")
## Impute average value of * 
for (v in names(income)[2:10]){
  income[[v]]=as.numeric(gsub("[*,]","",income[[v]]))
}

income$nonMissing = 0
income$numMissing = 0
for (v in names(income)[3:10]){
  var_vector = income[[v]]
  missing_vector = as.numeric(is.na(var_vector))
  var_vector[is.na(var_vector)] = 0
  income$nonMissing = income$nonMissing + var_vector
  income$numMissing = income$numMissing + missing_vector
  rm(missing_vector,var_vector)
}

income$imputed_values = with(income,(Total-nonMissing)/numMissing)
for (v in names(income)[3:10]){
  income[[v]][is.na(income[[v]])] = income$imputed_values[is.na(income[[v]])]
}

income = as.data.table(income[,1:10])


### Metal Level
metal = as.data.table(read.csv("Data/ASPE Data/CountyMetal.csv"))
metal = metal[,c(1,4:9)]
names(metal)[1:2] = c("FIPS.code","Total")
## Impute average value of * 
for (v in names(metal)[2:7]){
  metal[[v]]=as.numeric(gsub("[*,]","",metal[[v]]))
}

metal$nonMissing = 0
metal$numMissing = 0
for (v in names(metal)[3:7]){
  var_vector = metal[[v]]
  missing_vector = as.numeric(is.na(var_vector))
  var_vector[is.na(var_vector)] = 0
  metal$nonMissing = metal$nonMissing + var_vector
  metal$numMissing = metal$numMissing + missing_vector
  rm(missing_vector,var_vector)
}

metal$imputed_values = with(metal,(Total-nonMissing)/numMissing)
for (v in names(metal)[3:7]){
  metal[[v]][is.na(metal[[v]])] = metal$imputed_values[is.na(metal[[v]])]
}

metal = as.data.table(metal[,1:7])




aspe = merge(demos,income,by="FIPS.code")
aspe = merge(aspe,metal,by="FIPS.code")
aspe[,c("Total.x","Total.y"):=NULL]
aspe = aspe[!is.na(Total)] # Drop where the total is less than 11

aspe = aspe[!FIPS.code%in%c("","Unsuppressed Total")]
aspe[,FIPS.code:=as.numeric(as.character(FIPS.code))]


#### Match to Rating Area ####
area = read.csv("Data/Zip_RatingArea/County_to_RatingArea.csv")

aspe = merge(aspe,area,by.x="FIPS.code",by.y="FIPS",all.x=TRUE)

aspe = merge(aspe,est_areas,by.x=c("ST","RatingArea"),by.y=c("STATE","AREA"),all=TRUE)

aspe = aspe[inSample==TRUE]
## Drop Maryland
aspe = aspe[ST!="MD"]
# ## Reweight
# reweight = aspe[,list(ST_wgt=sum(Total)),by=c("ST","RatingArea","wgt")]
# reweight[,adj_factor:=(wgt/sum(wgt)) / (ST_wgt/sum(ST_wgt))]
# aspe = merge(aspe,reweight,by=c("ST","RatingArea"))


vars = names(aspe)[grepl("Age|FPL|Catas|Bronze|Silver|Gold|Platinum|Total",names(aspe))]

# for (v in vars){
#   aspe[,c(v):=.SD *adj_factor,.SDcol=v]
# }

aspe = aspe[,lapply(.SD,FUN=sum),.SDcols=vars,by=c("ST","RatingArea")]

#### ASPE Distributions ####
aspe[,FPL.gt.400:=FPL.gt.400+FPL.Unknown]
aspe[,FPL.Unknown:=0]
aspe[,FPL.100.150:=FPL.100.150+FPL.lt.100] # COmbine less than 100 with 100-150
aspe[,FPL.lt.100:=0]
## Total Distribution 
aspeDist = aspe[,lapply(.SD,FUN=sum),.SDcols=vars]
vars = names(aspeDist)[grepl("Age|FPL|Catas|Bronze|Silver|Gold|Platinum",names(aspeDist))]

for (v in vars){
  aspeDist[,c(v):=.SD/Total,.SDcol=v]
}

## By Rating Area
vars = names(aspe)[grepl("Age|FPL|Catas|Bronze|Silver|Gold|Platinum",names(aspe))]
aspe = reshape(aspe,varying = vars,
               v.names="weight",
               timevar="label",
               times = vars,
               idvar = c("ST","RatingArea"),
               direction="long")

aspe[,label:= factor(label,levels=c("Age.lt.18","Age.18.25","Age.26.34","Age.35.44","Age.45.54","Age.55.64","Age.65.","Age.Unknown",
                              "FPL.lt.100","FPL.100.150" ,"FPL.150.200","FPL.200.250","FPL.250.300","FPL.300.400","FPL.gt.400","FPL.Unknown",
                              "Catastrophic","Bronze","Silver","Gold","Platinum"),
                     labels = c("Under 18","18-25","26-34","35-44","45-54","55-65","65 and up","Age Unknown",
                                "Less than 1","1 - 1.5","1.5 - 2","2 - 2.5","2.5 - 3","3 - 4","Greater than 4","FPL Unknown",
                                "CATASTROPHIC","BRONZE","SILVER","GOLD","PLATINUM"))]

                                
                     

#### Estimation Sample ####
load("Intermediate_Output/Estimation_Data/estimationSampleData.rData")

households = merge(households,est_areas,by.x=c("STATE","AREA"),by.y=c("STATE","AREA"),all.x=TRUE)
households = households[inSample==TRUE]

households[,FPL_bucket:= "Less than 2.5"]
households[FPL>=2.5&FPL<4,FPL_bucket:="2.5 - 4"]
households[is.na(FPL)|FPL>=4,FPL_bucket:="Greater than 4"]


households[,AGE_bucket:= "Under 18"]
households[AGE>=18&AGE<=25,AGE_bucket:= "18-25"]
households[AGE>=26&AGE<=34,AGE_bucket:= "26-34"]
households[AGE>=35&AGE<=44,AGE_bucket:= "35-44"]
households[AGE>=45&AGE<=54,AGE_bucket:= "45-54"]
households[AGE>=55&AGE<=64,AGE_bucket:= "55-64"]
households[AGE>=65, AGE_bucket:= "65 and up"]


### Age Distribution ###

age = households[,list(rawN=sum(rawN),N=sum(N)),by="AGE_bucket"]
age[,portion_N:=round(N/sum(N),3)]
age[,portion_raw:=round(rawN/sum(rawN),3)]
setkey(age,AGE_bucket)
# age_highincome = households[FPL_bucket=="Greater than 4",list(N=sum(Y),Members=sum(MEMBERS)),by="AGE_bucket"]
# age_highincome[,portion_N:=round(N/sum(N),2)]
# age_highincome[,portion_Mem:=round(Members/sum(Members),2)]

### FPL Distribution ###
income = households[,list(N=sum(N),rawN=sum(rawN)),by="FPL_bucket"]
income[,portion_N:=round(N/sum(N),3)]
income[,portion_raw:=round(rawN/sum(rawN),3)]

### Metal Level Distribution
households[grepl("SILVER",METAL),METAL:="SILVER"]
metal = households[,list(N=sum(N),rawN=sum(rawN)),by="METAL"]
metal[,portion_N:=round(N/sum(N),3)]
metal[,portion_raw:=round(rawN/sum(rawN),3)]


#### ACS Data ####
load("Intermediate_Output/Simulated_BaseData/acs_prepped.rData")
acs = acs[HHincomeFPL>1&insured==TRUE,]

### Merge in Market Info 
### Keep only rating areas in original data
acs = merge(acs,est_areas,by.x=c("ST","AREA"),by.y=c("STATE","AREA"),all.x=TRUE)
acs = acs[inSample==TRUE]
#acs = acs[ST!="MD"] # Drop Maryland
## Reweight
# reweight = acs[,list(ST_wgt=sum(PERWT)),by=c("ST","AREA","wgt")]
# reweight[,adj_factor:=(wgt/sum(wgt)) / (ST_wgt/sum(ST_wgt))]
# acs = merge(acs,reweight,by=c("ST","AREA"))
# acs[,PERWTadj:=PERWT*adj_factor]




#### ACS Distribution ####

acs[,FPL_bucket:= "Less than 2.5"]
acs[HHincomeFPL>=2.5&HHincomeFPL<=4,FPL_bucket:="2.5 - 4"]
acs[HHincomeFPL>4,FPL_bucket:="Greater than 4"]
acs[ST%in%c("MD","CT"),FPL_bucket:="Greater than 4"]

acs[,Income_flag:=0]
acs[HHincomeFPL>4,Income_flag:=1]
acs[ST%in%c("MD","CT"),Income_flag:=1]
test = acs[,list(totalWeight=sum(PERWT)),by=c("ST","AREA","insured","Income_flag")]

acs[,AGE_bucket:= "Under 18"]
acs[AGE>=18&AGE<=25,AGE_bucket:= "18-25"]
acs[AGE>=26&AGE<=34,AGE_bucket:= "26-34"]
acs[AGE>=35&AGE<=44,AGE_bucket:= "35-44"]
acs[AGE>=45&AGE<=54,AGE_bucket:= "45-54"]
acs[AGE>=55&AGE<=64,AGE_bucket:= "55-64"]
acs[AGE>=65, AGE_bucket:= "65 and up"]


### Age Distribution ###

age_acs = acs[,list(N=sum(PERWT)),by="AGE_bucket"]
age_acs[,portion_N:=round(N/sum(N),3)]
# age_acs[,portion_Nadj:=round(Nadj/sum(Nadj),3)]
setkey(age_acs,AGE_bucket)

# age_acs_highincome = acs[FPL_bucket=="Greater than 4",list(N=sum(PERWT),Members=sum(totalWT)),by="AGE_bucket"]
# age_acs_highincome[,portion_N:=round(N/sum(N),2)]
# age_acs_highincome[,portion_Mem:=round(Members/sum(Members),2)]


### FPL Distribution ###
income_acs = acs[,list(N=sum(PERWT)),by="FPL_bucket"]
income_acs[,portion_N:=round(N/sum(N),3)]
# income_acs[,portion_Nadj:=round(Nadj/sum(Nadj),2)]
