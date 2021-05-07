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
eHealth = as.data.table(eHealth)

## FPL Calculation 

eHealth[,FPL:=HOUSEHOLD_INCOME/(11770 + (MEMBERS-1)*4160)]
eHealth[STATE=="AK",FPL:=HOUSEHOLD_INCOME/(14720 + (MEMBERS-1)*5200)]
eHealth[STATE=="HI",FPL:=HOUSEHOLD_INCOME/(18330 + (MEMBERS-1)*4780)]


#### Set Age Rating ####
rating = read.csv("Data/AgeRating.csv")

# Create truncated Age variable
eHealth$AgeMatch = eHealth$AGE
eHealth$AgeMatch[eHealth$AGE<14] = 14
eHealth$AgeMatch[eHealth$AGE>64] = 64

# Merge in Default and State-Specific Age Rating Curves

eHealth = merge(eHealth,rating[rating$State=="Default",c("Age","Rating")],by.x="AgeMatch",by.y="Age",all.x=TRUE)
eHealth = merge(eHealth,rating[rating$State!="Default",],by.x=c("STATE","AgeMatch"),by.y=c("State","Age"),all.x=TRUE)
eHealth$ageRate = eHealth$Rating.x
eHealth$ageRate[!is.na(eHealth$Rating.y)] = eHealth$Rating.y[!is.na(eHealth$Rating.y)]
# Drop redundant rating variables
eHealth = eHealth[,c("Rating.x","Rating.y"):=NULL]
rm(rating)


# Modify Age Rate for Family and Smoker
# Straight Forward rating assumption. 
eHealth$ageRate_base = eHealth$ageRate
eHealth$ageRate[eHealth$MEMBERS==1] = with(eHealth[eHealth$MEMBERS==1,],ageRate)
eHealth$ageRate[eHealth$MEMBERS==2] = with(eHealth[eHealth$MEMBERS==2,],ageRate*2)
eHealth$ageRate[eHealth$MEMBERS==3] = with(eHealth[eHealth$MEMBERS==3,],ageRate*2+.635)
eHealth$ageRate[eHealth$MEMBERS>3] = with(eHealth[eHealth$MEMBERS>3,],ageRate*2+.635*(MEMBERS-2))
eHealth$ageRate[eHealth$MEMBERS>5] = with(eHealth[eHealth$MEMBERS>5,],ageRate*2+.635*3)

eHealth$ageRate[eHealth$STATE%in%c("AK")] = with(eHealth[eHealth$STATE%in%c("AK"),],ageRate+.2*ageRate_base*(SMOKER=="Y"))
eHealth$ageRate[!eHealth$STATE%in%c("AK","NJ","NY","RI","VT","CA","CT")] = with(eHealth[!eHealth$STATE%in%c("AK","NJ","NY","RI","VT","CA","CT"),],ageRate+.5*ageRate_base*(SMOKER=="Y"))

# eHealth$ageRate[eHealth$MEMBERS==1] = with(eHealth[eHealth$MEMBERS==1,],ageRate+.5*ageRate*(SMOKER=="Y"))
# eHealth$ageRate[eHealth$MEMBERS==2] = with(eHealth[eHealth$MEMBERS==2,],ageRate*2+.5*ageRate*(SMOKER=="Y"))
# eHealth$ageRate[eHealth$MEMBERS==3] = with(eHealth[eHealth$MEMBERS==3,],ageRate*2+.5+.5*ageRate*(SMOKER=="Y"))
# eHealth$ageRate[eHealth$MEMBERS>3] = with(eHealth[eHealth$MEMBERS>3,],ageRate*2+.5*(MEMBERS-2)+.5*ageRate*(SMOKER=="Y"))
# eHealth$ageRate[eHealth$MEMBERS>5] = with(eHealth[eHealth$MEMBERS>5,],ageRate*2+.5*3+.5*ageRate*(SMOKER=="Y"))
# 

# Merge in Age-specific HHS-HCC Risk Adjustment Factors
HCC = read.csv("Risk_Adjustment/2014_HHS_HCC_AgeRA_Coefficients.csv")
names(HCC) = c("Sex","Age","PlatHCC_Age","GoldHCC_Age","SilvHCC_Age","BronHCC_Age","CataHCC_Age")
HCC = HCC[HCC$Sex==0,]

eHealth[,AgeMatch:= pmax(floor(AGE/5)*5,21)]
eHealth = merge(eHealth,HCC,by.x=c("AgeMatch"),by.y=c("Age"))



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


#### Read in Choice Sets ####
choiceSets = read.csv("Intermediate_Output/Premiums/choiceSets2015.csv")
mapping = read.csv("Intermediate_Output/Zip_RatingArea/Zip3_to_RatingArea.csv")

choiceSets = as.data.table(choiceSets)
setkey(choiceSets,ST,AREA)

mapping = as.data.table(mapping)
setkey(mapping,ST,RatingArea)

choices = merge(choiceSets,mapping[,c("ST","Zip3","RatingArea","alloc")],by.x=c("ST","AREA"),by.y=c("ST","RatingArea"),
                all.x=TRUE,allow.cartesian=TRUE)

#### Merge eHealth Data into Rating Areas ####
eHealth = eHealth[eHealth$TRUNCATED_ZIP%in%choices$Zip3,]

eHealth = merge(eHealth,mapping[,c("ST","Zip3","RatingArea","alloc")],by.x=c("STATE","TRUNCATED_ZIP"),by.y=c("ST","Zip3"),
                all.x=TRUE,allow.cartesian=TRUE)
eHealth[,AREA:=RatingArea]

eHealth[,N:=alloc]
eHealth[,catas_elig:=AGE<=30]

## Keep Necessary Variables
eHealth = eHealth[,c("STATE","APP_RECORD_NUM","QUOTED_RATE","HOUSEHOLD_INCOME","FFM_APTC_AMOUNT","FFM_PREMIUM_DUE",
                     "FAMILY_OR_INDIVIDUAL","MEMBERS","AGE","ageRate_base","ageRate","SMOKER","AREA","alloc", "Firm","PLAN_METAL_LEVEL","FPL","N",
                     "PlatHCC_Age","GoldHCC_Age","SilvHCC_Age","BronHCC_Age","CataHCC_Age","catas_elig")]

### Un Weighted Code
fulldf = eHealth
fulldf[,insured:=TRUE]
fulldf[,Income_flag:=0]
fulldf[is.na(FPL)|FPL>4,Income_flag:=1]

#### Merge eHealth and Plan Data ####
# Merge in eHealth data
setkey(fulldf,STATE,AREA)
setkey(choiceSets,ST,AREA)

choices = merge(fulldf,choiceSets,by.x=c("STATE","AREA"),by.y=c("ST","AREA"),
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
choices = choices[!(catas_elig==FALSE & METAL=="CATASTROPHIC"),]


### Re-weight up if choices rule out certain areas
# If choice is not available, then that individual is not in the right rating area
# This step drops CA/CT (data collection needed), 3 obs in IL that don't have right rating area
choices[,flag:= sum(Y),by=c("APP_RECORD_NUM","AREA")]
choices = choices[!(flag==0&insured),]

Alloc = unique(choices[!is.na(alloc),c("APP_RECORD_NUM","STATE","AREA","alloc")])
Alloc = Alloc[,list(totalalloc = max(alloc)),by=c("APP_RECORD_NUM")]
choices = merge(choices,Alloc,by=c("APP_RECORD_NUM"),all.x=TRUE)

# Keep only most likely rating area
choices = choices[is.na(alloc)|(alloc==totalalloc),]
choices[!is.na(alloc),N:=N/totalalloc]

# Remove non-valid areas
choices = choices[choices$valid,]


# Keep only relevant variables
choices = choices[,c("STATE","APP_RECORD_NUM","QUOTED_RATE","HOUSEHOLD_INCOME","FFM_APTC_AMOUNT","FFM_PREMIUM_DUE",
                     "FAMILY_OR_INDIVIDUAL","MEMBERS","AGE","ageRate","ageRate_base","SMOKER","AREA","Firm","METAL","hix","PREMI27",
                     "MedDeduct","MedOOP","Y","N","FPL","insured","Income_flag",
                     "PlatHCC_Age","GoldHCC_Age","SilvHCC_Age","BronHCC_Age","CataHCC_Age")]



# Make Premium for Age Rating = 1
choices[,premBase:=PREMI27/1.048]
choices[STATE=="DC",premBase:=PREMI27/.727]
choices[STATE=="MA",premBase:=PREMI27/1.22]
choices[STATE=="MN",premBase:=PREMI27/1.048]
choices[STATE=="UT",premBase:=PREMI27/1.39]


#### Imputed Age Rating
choices = choices[Y==1,]
choices[,ageRate_imp1:=QUOTED_RATE/premBase]
choices[,QUOTED_imp:=FFM_APTC_AMOUNT+FFM_PREMIUM_DUE]
choices[,ageRate_imp2:=QUOTED_imp/premBase]


### Member Category
choices[,mem_cat:="1"]
choices[MEMBERS==2,mem_cat:="2"]
choices[MEMBERS==3,mem_cat:="3"]
choices[MEMBERS>3,mem_cat:="4-5"]
choices[MEMBERS>5,mem_cat:="6+"]


### Imputation Regression
choices[,regvar:=ageRate_imp2-ageRate_base]
choices[,ageRate_mem2:=as.numeric(MEMBERS==2)*ageRate_base]
choices[,ageRate_mem3:=as.numeric(MEMBERS==3)*ageRate_base]
choices[,ageRate_mem4_5:=as.numeric(MEMBERS>3&MEMBERS<=5)*ageRate_base]
choices[,ageRate_mem6:=as.numeric(MEMBERS>5)*ageRate_base]

choices[,mem6:=as.numeric(MEMBERS>5)*(MEMBERS-2)]
choices[,mem_plus:=(MEMBERS-2)*as.numeric(MEMBERS<6&MEMBERS>1)]

res = choices[,lm(regvar~-1+ageRate_mem2+ageRate_mem3+ageRate_mem4_5+ageRate_mem6+mem_plus+mem6)]
summary(res)

save(res,file="Intermediate_Output/Premiums/ageRate_Imputed_Regression.rData")

choices[,ageRate2:=predict(res,newdata=choices)+ageRate_base]

png("Writing/Images/ageRating_simpleRule.png",width=2000,height=1500,res=275)
plot = ggplot(choices[!is.na(ageRate_imp2),]) + aes(y=ageRate,x=ageRate_imp2) +
  geom_point(size=1,alpha=0.05) +
  geom_abline(intercept=0,slope=1) +
  geom_smooth(color="black",method="lm",se=FALSE,size=1.25,linetype=2) +
  xlab("Imputed Household-Level Rating") + 
  ylab("Simple Rule Age Rating") + 
  coord_cartesian(xlim=c(0.5,6.0),y=c(0.5,6.0)) + 
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
print(plot)
dev.off()
print(plot)

choices[!is.na(ageRate_imp2),cor(ageRate_imp2,ageRate)]
choices[!is.na(ageRate_imp1),cor(ageRate_imp1,ageRate)]
