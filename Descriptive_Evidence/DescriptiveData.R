rm(list=ls())
library(ggplot2)
library(scales)
library(extrafont)
library(grid)
library(doBy)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition")

#### Combine Data ####
plansAttr = read.csv("Data/2015_Premiums/Plan_Attributes_PUF.csv")
plansAttr = plansAttr[,c("State.Code","Benefit.Package.ID","Market.Coverage",
                 "Dental.Only.Plan.Indicator","Plan.ID","Plan.Marketing.Name",
                 "Network.ID", "Service.Area.ID","Formulary.ID",
                 "Plan.Type","Metal.Level","CSR.Variation.Type","Issuer.Actuarial.Value",
                 "AV.Calculator.Output.Number",
                 "SBC.Scenario..Having.Diabetes..Coinsurance",
                 "SBC.Scenario..Having.Diabetes..Copayment")]

for (v in c("SBC.Scenario..Having.Diabetes..Coinsurance","SBC.Scenario..Having.Diabetes..Copayment")){
  #Remove dollar signs from the data
  plansAttr[[v]]=gsub("[$,]","",plansAttr[[v]])
  #Convert premiums to a numeric variable
  plansAttr[[v]] = as.numeric(plansAttr[[v]])
}

plansAttr$diabetes = with(plansAttr,SBC.Scenario..Having.Diabetes..Coinsurance+SBC.Scenario..Having.Diabetes..Copayment)





plansAttr = plansAttr[plansAttr$Dental.Only.Plan.Indicator=="No",]
plansAttr = plansAttr[plansAttr$Market.Coverage=="Individual",]
plansAttr = plansAttr[!grepl("%",plansAttr$CSR.Variation.Type),]
# What is zero cost-sharing and limited cost-sharing...?
plansAttr = plansAttr[!grepl("%",plansAttr$CSR.Variation.Type),]
plansAttr = plansAttr[!plansAttr$CSR.Variation.Type%in%c("Zero Cost Sharing Plan Variation","Limited Cost Sharing Plan Variation"),]
plansAttr = plansAttr[!grepl("Off Exchange",plansAttr$CSR.Variation.Type),]

plansAttr$AV.Calculator.Output.Number[abs(plansAttr$AV.Calculator.Output.Number)==0] = NA
plansAttr$AV.Calculator.Output.Number[is.na(plansAttr$AV.Calculator.Output.Number)] =
  as.numeric(gsub("%","",plansAttr$Issuer.Actuarial.Value[is.na(plansAttr$AV.Calculator.Output.Number)]))/100

plansAttr = unique(plansAttr[,c("Plan.ID", "AV.Calculator.Output.Number","diabetes")])

#### Compile Plan Description Data ####
planData = read.csv("Data/2015_Premiums/2015_RWJF.csv")
planData$CARRIER = as.character(planData$CARRIER)

# Standardize Plan Names
planData$CARRIER[grep("Cigna",planData$CARRIER)] = "Cigna"
planData$CARRIER[grep("Health Net",planData$CARRIER)] = "Health Net"
planData$CARRIER[grep("Aetna",planData$CARRIER)] = "Aetna"
planData$CARRIER[grep("Anthem",planData$CARRIER)] = "Anthem"
planData$CARRIER[grep("Humana",planData$CARRIER)] = "Humana"
planData$CARRIER[grep("Coventry",planData$CARRIER)] = "Coventry"
planData$CARRIER[grep("Ambetter",planData$CARRIER)] = "Ambetter"
planData$CARRIER[grep("Kaiser",planData$CARRIER)] = "Kaiser"
planData$CARRIER[grep("Moda",planData$CARRIER)] = "Moda"
planData$CARRIER[grep("Molina",planData$CARRIER)] = "Molina"
planData$CARRIER[grep("Health Republic",planData$CARRIER)] = "Health Republic"
planData$CARRIER[grep("Oscar",planData$CARRIER)] = "Oscar"
planData$CARRIER[grep("UnitedHealth",planData$CARRIER)] = "UnitedHealthcare"
planData$CARRIER[grep("Capital BlueCross",planData$CARRIER)] = "Capital BlueCross"

#  Standardize Rating Area
planData$AREA = gsub("^([A-Z]+)([0]{0,})([1-9]+[0]?)$","Rating Area \\3",planData$AREA,perl=TRUE)
# NJ is actually only one rating area... 
planData$AREA[planData$ST=="NJ"] = "Rating Area 1"


planData = planData[,c("ST","METAL","AREA","PLANID","PLANNAME","CARRIER",
                       "PREMI27","PLANMARKET",
                       "MEHBDedInnTier1IndividualA","MEHBDedInnTier2IndividualA",
                       "TEHBDedInnTier1IndividualA","TEHBDedInnTier2IndividualA",
                       "MEHBDedInnTier1FamilyA","MEHBDedInnTier2FamilyA",
                       "TEHBDedInnTier1FamilyA","TEHBDedInnTier2FamilyA",
                       "MEHBDedOutOfNetIndividualA","TEHBDedOutOfNetIndividualA",
                       "MEHBDedOutOfNetFamilyA","TEHBDedOutOfNetFamilyA",
                       "MEHBInnTier1IndividualMOOPA","MEHBInnTier2IndividualMOOPA",
                       "TEHBInnTier1IndividualMOOPA","TEHBInnTier2IndividualMOOPA",
                       "MEHBInnTier1FamilyMOOPA","MEHBInnTier2FamilyMOOPA",
                       "TEHBInnTier1FamilyMOOPA","TEHBInnTier2FamilyMOOPA")]
# Unique observations in NJ
planData = unique(planData)

# Convert NA to 0. This method is only valid because there are no true 0s in the data. 
for (var in c("MEHBDedInnTier1IndividualA","MEHBDedInnTier2IndividualA",
              "TEHBDedInnTier1IndividualA","TEHBDedInnTier2IndividualA",
              "MEHBDedInnTier1FamilyA","MEHBDedInnTier2FamilyA",
              "TEHBDedInnTier1FamilyA","TEHBDedInnTier2FamilyA",
              "MEHBDedOutOfNetIndividualA","TEHBDedOutOfNetIndividualA",
              "MEHBDedOutOfNetFamilyA","TEHBDedOutOfNetFamilyA",
              "MEHBInnTier1IndividualMOOPA","MEHBInnTier2IndividualMOOPA",
              "TEHBInnTier1IndividualMOOPA","TEHBInnTier2IndividualMOOPA",
              "MEHBInnTier1FamilyMOOPA","MEHBInnTier2FamilyMOOPA",
              "TEHBInnTier1FamilyMOOPA","TEHBInnTier2FamilyMOOPA")){
  # print(var)
  # print(any(planData[[var]]==0,na.rm=TRUE))
  planData[[var]][is.na(planData[[var]])] = 0
}

planData$MedDeduct = with(planData,pmax(MEHBDedInnTier1IndividualA,MEHBDedInnTier2IndividualA)+
                            pmax(TEHBDedInnTier1IndividualA,TEHBDedInnTier2IndividualA))

planData$MedDeduct_OON = with(planData,MEHBDedOutOfNetIndividualA + TEHBDedOutOfNetIndividualA)

planData$MedDeductFam = with(planData,pmax(MEHBDedInnTier1FamilyA,MEHBDedInnTier2FamilyA)+
                               pmax(TEHBDedInnTier1FamilyA,TEHBDedInnTier2FamilyA))

planData$MedDeductFam_OON = with(planData,MEHBDedOutOfNetFamilyA + TEHBDedOutOfNetFamilyA)

planData$MedOOP = with(planData,pmax(MEHBInnTier1IndividualMOOPA,MEHBInnTier2IndividualMOOPA)+
                         pmax(TEHBInnTier1IndividualMOOPA,TEHBInnTier2IndividualMOOPA))
planData$MedOOPFam = with(planData,pmax(MEHBInnTier1FamilyMOOPA,MEHBInnTier2FamilyMOOPA)+
                            pmax(TEHBInnTier1FamilyMOOPA,TEHBInnTier2FamilyMOOPA))

planData$MedDeduct[planData$MedDeduct==0] = planData$MedDeduct_OON[planData$MedDeduct==0]
planData$MedDeduct[planData$MedDeduct==0] = NA
planData$MedOOP[planData$MedOOP==0] = NA

planData$MedDeductFam[planData$MedDeductFam==0] = planData$MedDeductFam_OON[planData$MedDeductFam==0]
planData$MedDeductFam[planData$MedDeductFam==0] = NA
planData$MedOOPFam[planData$MedOOPFam==0] = NA


#Read in HIX plan description data 
hixData = read.csv("Data/2015_Premiums/2015_QHP_Landscape_Individual_Market_Medical.csv")

NM = read.csv("Data/2015_Premiums/2015_QHP_Landscape_NM_Individual_Market_Medical.csv")
NV = read.csv("Data/2015_Premiums/2015_QHP_Landscape_NV_Individual_Market_Medical.csv")
OR= read.csv("Data/2015_Premiums/2015_QHP_Landscape_OR_Individual_Market_Medical.csv")

hixData = rbind(hixData,NM)
hixData = rbind(hixData,NV)
hixData = rbind(hixData,OR)

rm(NM,NV,OR)

#Drop Child-Only Plans
hixData = hixData[hixData$Child.Only.Offering=="Allows Adult and Child-Only",]

#Isolate important characteristic variables
hixData = hixData[,c("State","Metal.Level","Rating.Area","Plan.ID..standard.component.",
                     "Plan.Marketing.Name",
                     "Premium.Adult.Individual.Age.27",
                     "Medical.Deductible.individual.standard",
                     "Drug.Deductible.individual.standard",
                     "Medical.Maximum.Out.Of.Pocket...individual...standard",  
                     "Drug.Maximum.Out.of.Pocket...individual...standard",
                     "Medical.Deductible...individual...73.percent",
                     "Drug.Deductible...individual...73.percent",
                     "Medical.Maximum.Out.Of.Pocket...individual...73.percent",
                     "Drug.Maximum.Out.of.Pocket...individual...73.percent",
                     "Medical.Deductible...individual...87.percent",
                     "Drug.Deductible...individual...87.percent",
                     "Medical.Maximum.Out.Of.Pocket...individual...87.percent",
                     "Drug.Maximum.Out.of.Pocket...individual...87.percent",
                     "Medical.Deductible...individual...94.percent",
                     "Drug.Deductible...individual...94.percent",
                     "Medical.Maximum.Out.Of.Pocket..individual...94.percent",
                     "Drug.Maximum.Out.of.Pocket...individual...94.percent",
                     "Medical.Deductible.family.standard",
                     "Drug.Deductible...family...standard",
                     "Medical.Maximum.Out.of.Pocket...family...standard",  
                     "Drug.Maximum.Out.of.Pocket...Family....standard",
                     "Medical.Deductible...family...73.percent",
                     "Drug.Deductible...family...73.percent",
                     "Medical.Maximum.Out.of.Pocket...family...73.percent",
                     "Drug.Maximum.Out.of.Pocket...Family...73.percent",
                     "Medical.Deductible...family...87.percent",
                     "Drug.Deductible...family...87.percent",
                     "Medical.Maximum.Out.of.Pocket...family...87.percent",
                     "Drug.Maximum.Out.of.Pocket...Family...87.percent",
                     "Medical.Deductible...family...94.percent",
                     "Drug.Deductible...family...94.percent",
                     "Medical.Maximum.Out.of.Pocket...family...94.percent",
                     "Drug.Maximum.Out.of.Pocket...Family....94.percent"
)]

names(hixData) = c("State","Metal","RatingArea","PlanID","PlanName",
                   "Prem27","MedDeduct","DrugDeduct",
                   "MedOOP","DrugOOP",
                   "MedDeduct73","DrugDeduct73",
                   "MedOOP73","DrugOOP73",
                   "MedDeduct87","DrugDeduct87",
                   "MedOOP87","DrugOOP87",
                   "MedDeduct94","DrugDeduct94",
                   "MedOOP94","DrugOOP94",
                   "MedDeductFam","DrugDeductFam",
                   "MedOOPFam","DrugOOPFam",
                   "MedDeduct73Fam","DrugDeduct73Fam",
                   "MedOOP73Fam","DrugOOP73Fam",
                   "MedDeduct87Fam","DrugDeduct87Fam",
                   "MedOOP87Fam","DrugOOP87Fam",
                   "MedDeduct94Fam","DrugDeduct94Fam",
                   "MedOOP94Fam","DrugOOP94Fam")

allVars = c("Prem27","MedDeduct","DrugDeduct",
            "MedOOP","DrugOOP",
            "MedDeduct73","DrugDeduct73",
            "MedOOP73","DrugOOP73",
            "MedDeduct87","DrugDeduct87",
            "MedOOP87","DrugOOP87",
            "MedDeduct94","DrugDeduct94",
            "MedOOP94","DrugOOP94",
            "MedDeductFam","DrugDeductFam",
            "MedOOPFam","DrugOOPFam",
            "MedDeduct73Fam","DrugDeduct73Fam",
            "MedOOP73Fam","DrugOOP73Fam",
            "MedDeduct87Fam","DrugDeduct87Fam",
            "MedOOP87Fam","DrugOOP87Fam",
            "MedDeduct94Fam","DrugDeduct94Fam",
            "MedOOP94Fam","DrugOOP94Fam")

# For the moment, ignore drug variables
# Clean Variables
for (var in allVars[!grepl("Drug",allVars)]){
  #Remove dollar signs from the data
  hixData[[var]]=gsub("[$,]","",hixData[[var]])
  #Convert premiums to a numeric variable
  hixData[[var]] = as.numeric(hixData[[var]])
}

# Drop 26 plans without premium info
hixData = hixData[!is.na(hixData$Prem27),]

# Keep unique plan options
hixData = unique(hixData)
hixData = hixData[,!grepl("Drug",names(hixData))]

## Merge RWJF and HIX plan data 
planData = planData[,c("ST","METAL","AREA","PLANID","CARRIER","PLANNAME","PREMI27","PLANMARKET","MedDeduct","MedDeductFam","MedOOP","MedOOPFam")]
planData$CSR = gsub("([0-9]+[A-Z]+[0-9]+)(-)?(04|05|06)?","\\3",planData$PLANID,perl=TRUE)
planData$PLANID = gsub("([0-9]+[A-Z]+[0-9]+)(-)?(04|05|06)?","\\1",planData$PLANID,perl=TRUE)
planData$CSR = factor(planData$CSR,levels=c("","04","05","06"),labels=c("","73","87","94"))

STselection  = c("AK","AR","CA","CT","DE","GA","IL","IA","KS","MD","MI","MN","MO","NE","NJ","NM","OK","OR","PA","TX","UT","VA","WV")

comb = merge(planData[planData$ST%in%STselection,],hixData[hixData$State%in%STselection,],
             by.x=c("ST","AREA","PLANID"),by.y=c("State","RatingArea","PlanID"),all=TRUE)
comb = comb[order(comb$PLANID),]


# Best of Both Datasets
comb$MedDeduct = comb$MedDeduct.y
comb$MedDeduct[comb$CSR=="94"] = comb$MedDeduct94[comb$CSR=="94"]
comb$MedDeduct[comb$CSR=="87"] = comb$MedDeduct87[comb$CSR=="87"]
comb$MedDeduct[comb$CSR=="73"] = comb$MedDeduct73[comb$CSR=="73"]
comb$MedDeduct[is.na(comb$MedDeduct)] = comb$MedDeduct.x[is.na(comb$MedDeduct)]

comb$MedOOP = comb$MedOOP.y
comb$MedOOP[comb$CSR=="94"] = comb$MedOOP94[comb$CSR=="94"]
comb$MedOOP[comb$CSR=="87"] = comb$MedOOP87[comb$CSR=="87"]
comb$MedOOP[comb$CSR=="73"] = comb$MedOOP73[comb$CSR=="73"]
comb$MedOOP[is.na(comb$MedOOP)] = comb$MedOOP.x[is.na(comb$MedOOP)]

comb$MedDeductFam = comb$MedDeductFam.y
comb$MedDeductFam[comb$CSR=="94"] = comb$MedDeduct94Fam[comb$CSR=="94"]
comb$MedDeductFam[comb$CSR=="87"] = comb$MedDeduct87Fam[comb$CSR=="87"]
comb$MedDeductFam[comb$CSR=="73"] = comb$MedDeduct73Fam[comb$CSR=="73"]
comb$MedDeductFam[is.na(comb$MedDeductFam)] = comb$MedDeductFam.x[is.na(comb$MedDeductFam)]
comb$MedDeductFam[is.na(comb$MedDeductFam)] = 2*comb$MedDeduct[is.na(comb$MedDeductFam)]

comb$MedOOPFam = comb$MedOOPFam.y
comb$MedOOPFam[comb$CSR=="94"] = comb$MedOOP94Fam[comb$CSR=="94"]
comb$MedOOPFam[comb$CSR=="87"] = comb$MedOOP87Fam[comb$CSR=="87"]
comb$MedOOPFam[comb$CSR=="73"] = comb$MedOOP73Fam[comb$CSR=="73"]
comb$MedOOPFam[is.na(comb$MedOOPFam)] = comb$MedOOPFam.x[is.na(comb$MedOOPFam)]
comb$MedOOPFam[is.na(comb$MedOOPFam)] = 2*comb$MedOOP[is.na(comb$MedOOPFam)]

# missingIDs_in = comb$PLANID[comb$PLANMARKET!=2&(is.na(comb$MedDeduct)|is.na(comb$MedOOP))]
# missingIDs_out = unique(comb$PLANID[comb$PLANMARKET==2&(is.na(comb$MedDeduct)|is.na(comb$MedOOP))])
# 
# plans = unique(planData[planData$PLANID%in%missingIDs_in&is.na(planData$MedOOP),c("ST","CARRIER","PLANNAME")])

planDesc = comb[with(comb,order(ST,AREA,METAL,CARRIER,PLANID)),
            c("ST","AREA","PLANID","METAL","CARRIER","PLANNAME","PREMI27","PLANMARKET","CSR","MedDeduct","MedOOP","MedDeductFam","MedOOPFam")]
names(planDesc) = c("State","RatingArea","Plan.ID","Metal","CARRIER","PLANNAME","Prem27","PLANMARKET","CSR","MedDeduct","MedOOP","MedDeductFam","MedOOPFam")

plans = merge(planDesc,plansAttr,by="Plan.ID")



#### Actuarial Value Plot ####
plans$AV.target = round(plans$AV.Calculator.Output.Number,1)
plans$distance = with(plans,AV.Calculator.Output.Number - AV.target)

AVplot = unique(plans[,c("Plan.ID","Metal","AV.Calculator.Output.Number")])
AVplot$AV.target = round(AVplot$AV.Calculator.Output.Number,1)
AVplot$distance = with(AVplot,AV.Calculator.Output.Number - AV.target)
AVplot$Metal = factor(AVplot$Metal,levels=c("Catastrophic","Bronze","Silver","Gold","Platinum"))

png("Writing/Images/AVtarget.png",width=2000,height=1500,res=275)
ggplot(AVplot[AVplot$Metal!="Catastrophic",]) + 
  aes(x=distance) + 
  facet_wrap(~Metal,ncol=1,scales="free_y") + 
  geom_histogram(binwidth=.002) + 
  xlab("Distance from AV Target") + 
  ylab("") + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_rect(color=grey(.5)),
    legend.title=element_blank(),
    legend.text = element_text(size=18),
    legend.key.width = unit(.075,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "none",
    axis.title=element_text(size=12),
    axis.text = element_text(size=12))
dev.off()


#### Screening Regression ####
for (v in c("Prem27","MedDeduct","DrugDeduct","MedOOP","DrugOOP")){
  #Remove dollar signs from the data
  plans[[v]]=gsub("[$,]","",plans[[v]])
  #Convert premiums to a numeric variable
  plans[[v]] = as.numeric(plans[[v]])
}
#Check on Premium NAs


plans$Area = paste(plans$State,plans$RatingArea)
res = lm(Prem27~AV.Calculator.Output.Number*Area,data=plans)

plans$pHat = NA
plans$pHat[!is.na(plans$Prem27)&!is.na(plans$AV.Calculator.Output.Number)] = predict(res)
plans$resid = with(plans,Prem27-pHat)

spread = summaryBy(resid~State+RatingArea+CARRIER,
                   data=plans,
                   FUN=var,keep.names=TRUE,na.rm=TRUE)
spread$Firm = gsub(" ","_",spread$CARRIER)
spread$Firm = gsub("[,.&'-:]","",spread$Firm)


#### Regress on Market Share ####
choices = read.csv("Intermediate_Output/estimationData.csv")

choices$count = 1
firmShares = summaryBy(Y~Firm+Market,data=choices,FUN=sum,keep.names=TRUE)
firmShares$marketTotal = ave(firmShares$Y,firmShares$Market,FUN=sum)
firmShares$share = firmShares$Y/firmShares$marketTotal
firmShares = firmShares[with(firmShares,order(Market,Firm)),]


spread = merge(spread,firmShares,by=c("Firm","Market"))


