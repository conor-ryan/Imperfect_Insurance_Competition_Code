rm(list=ls())
library(ggplot2)
library(scales)
library(extrafont)
library(grid)
library(doBy)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition")

#### Plan Attribute Summaries ####
plans = read.csv("Data/2015_Premiums/Plan_Attributes_PUF.csv")
plans = plans[,c("State.Code","Benefit.Package.ID","Market.Coverage",
                 "Dental.Only.Plan.Indicator","Plan.ID","Plan.Marketing.Name",
                 "Network.ID", "Service.Area.ID","Formulary.ID",
                 "Plan.Type","Metal.Level","CSR.Variation.Type","Issuer.Actuarial.Value",
                 "AV.Calculator.Output.Number")]

plans$AV.Calculator.Output.Number[abs(plans$AV.Calculator.Output.Number-1)<.001] = NA
plans$AV.Calculator.Output.Number[is.na(plans$AV.Calculator.Output.Number)] =
  as.numeric(gsub("%","",plans$Issuer.Actuarial.Value[is.na(plans$AV.Calculator.Output.Number)]))/100
plans$AV.Calculator.Output.Number[abs(plans$AV.Calculator.Output.Number-1)<.001] = NA
plans$AV.Calculator.Output.Number[abs(plans$AV.Calculator.Output.Number-0)<.001] = NA

plans = plans[plans$Dental.Only.Plan.Indicator=="No",]
plans = plans[plans$Market.Coverage=="Individual",]
plans = plans[!grepl("%",plans$CSR.Variation.Type),]
plans = unique(plans[,c("Plan.ID","Metal.Level","CSR.Variation.Type", "AV.Calculator.Output.Number")])
plans$AV.target = round(plans$AV.Calculator.Output.Number,1)
plans$distance = with(plans,AV.Calculator.Output.Number - AV.target)
plans$Metal.Level = factor(plans$Metal.Level,levels=c("Catastrophic","Bronze","Silver","Gold","Platinum"))

png("Writing/AVtarget.png",width=2000,height=1500,res=275)
ggplot(plans[grepl("Standard",plans$CSR.Variation.Type)&plans$Metal.Level!="Catastrophic",]) + 
  aes(x=distance) + 
  facet_wrap(~Metal.Level,ncol=1,scales="free_y") + 
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


#### Read in Data ####
#Read in plan description data 
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

#### Subset Data on Salient Characteristics ####
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


#### Read in HIX Data for Missing Data ####
#Read in plan description data 
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

#### Merge RWJF and HIX plan data ####
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
 
comb = comb[with(comb,order(ST,AREA,METAL,CARRIER,PLANID)),
            c("ST","AREA","PLANID","METAL","CARRIER","PLANNAME","PREMI27","PLANMARKET","CSR","MedDeduct","MedOOP","MedDeductFam","MedOOPFam")]


##### Cost Sharing Plot ####
comb$METAL = factor(comb$METAL,levels=c("Catastrophic","Bronze","Silver","Gold","Platinum"))
plotData = comb[with(comb,!is.na(MedDeduct)&CSR==""&MedDeduct<MedOOP),]
png("Writing/DeductOOP.png",width=2000,height=1500,res=275)
ggplot(plotData) + 
  aes(x=MedDeduct,y=MedOOP) + 
  geom_point()  + 
  facet_wrap(~METAL,ncol=1) + 
  xlab("Deductible") + 
  ylab("Maximum Out-of-Pocket") + 
  scale_y_continuous(labels=dollar) + 
  scale_x_continuous(labels=dollar) + 
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

##### Create Rating Area Choice Sets #####
comb$METAL = gsub(" $","",paste(comb$METAL,comb$CSR))

comb$count = 1
comb$count_prod = ave(comb$count,with(comb,paste(ST,AREA,CARRIER,METAL)),FUN=sum)
comb$count_all = ave(comb$count,with(comb,paste(ST,AREA)),FUN=sum)

comb$premRank = ave(comb$PREMI27,with(comb,paste(ST,AREA,CARRIER,METAL)),
                    FUN=function(x){return(rank(x,ties.method="first"))})
comb$medRank = floor(ave(comb$premRank,with(comb,paste(ST,AREA,CARRIER,METAL)),FUN=median))

choiceSet = comb[comb$premRank==comb$medRank,]


#### HIX Silver Benchmark ####
hixSilver = comb[with(comb,PLANMARKET%in%c(1,3)&METAL=="Silver"),]
hixSilver$premRank = ave(hixSilver$PREMI27,with(hixSilver,paste(ST,AREA)),
                    FUN=function(x){return(rank(x,ties.method="first"))})
hixSilver$maxRank = floor(ave(hixSilver$premRank,with(hixSilver,paste(ST,AREA)),FUN=max))
benchmark = hixSilver[with(hixSilver,premRank==2|maxRank==1),c("ST","AREA","PREMI27")]
names(benchmark) = c("ST","AREA","bench27")
write.csv(benchmark,"Intermediate_Output/Premiums/benchmark2015.csv",row.names=FALSE)

#### Check Valid Choice Sets ####
counts_all = unique(comb[,c("ST","AREA","count_all")])
counts_prod = unique(comb[,c("ST","AREA","CARRIER","METAL","count_prod")])

choiceSet$count = 1 
choiceSet$count = ave(choiceSet$count,with(choiceSet,paste(ST,AREA,METAL)),FUN=sum)
choiceSet$valid = ave(!is.na(choiceSet$MedOOP),with(choiceSet,paste(ST,AREA,METAL)),FUN=all)

areas = summaryBy(count+valid~ST+AREA,data=choiceSet,FUN=mean)
validAreas = areas[areas$valid==1,c("ST","AREA")]

areas = summaryBy(count+valid~ST+AREA,data=choiceSet,FUN=median)
areas = areas[with(areas,paste(ST,AREA))%in%with(validAreas,paste(ST,AREA)),]

#### Subset by markets verified through MLR ####
# Drop Counties where Highmark operates in PA
validAreas = validAreas[!with(validAreas,ST=="PA"&AREA%in%c("Rating Area 2", "Rating Area 5", "Rating Area 7")),]
# Drop CT and San Diego, but can recover with some data entry
validAreas = validAreas[!with(validAreas,ST=="CT"),]
validAreas = validAreas[!with(validAreas,ST=="CA"&AREA%in%c("Rating Area 19")),]



choiceSet$valid = with(choiceSet,paste(ST,AREA))%in%with(validAreas,paste(ST,AREA))
choiceSet$hix = choiceSet$PLANMARKET%in%c(1,3)


write.csv(choiceSet[,c("ST","AREA","CARRIER","METAL","PREMI27","MedDeduct","MedOOP","MedDeductFam","MedOOPFam","hix","valid")],
          "Intermediate_Output/Premiums/choiceSets2015.csv",row.names=FALSE)


# ##### Check to what we've got left #####
# mapping = read.csv("Data/Zip_RatingArea/Zip3_to_RatingArea.csv")
# 
# validAreas = merge(validAreas,mapping,by.x=c("ST","AREA"),by.y=c("ST","RatingArea"),all.x=TRUE)
# 
# 
# eHealth = read.csv("C:/Users/Conor/Documents/Research/eHealth Data/eHealth_2015.csv",stringsAsFactors = FALSE)
# eHealth = eHealth[with(eHealth,!is.na(TRUNCATED_ZIP)&TRUNCATED_ZIP!=0 ),]
# eHealth = eHealth[eHealth$PLAN_NAME!="UNKNOWN PLAN - this row is used for referential integrity - DSTOCK",]
# 
# match = merge(eHealth,validAreas,
#               by.x=c("STATE","TRUNCATED_ZIP"),by.y=c("ST","Zip3"),all.x=TRUE)
# 
# match$valid = !is.na(match$AREA)
# 
# match = match[!is.na(match$alloc) & match$alloc == ave(match$alloc,match$APP_RECORD_NUM,FUN=max),]
# match = match[match$valid,]
# match$count = 1 
# obs_market = summaryBy(count~STATE+AREA,data=match,FUN=sum)

