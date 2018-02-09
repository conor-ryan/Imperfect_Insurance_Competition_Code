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

for (var in c("SBC.Scenario..Having.Diabetes..Coinsurance","SBC.Scenario..Having.Diabetes..Copayment")){
  #Remove dollar signs from the data
  plansAttr[[var]]=gsub("[$,]","",plansAttr[[var]])
  #Convert premiums to a numeric variable
  plansAttr[[var]] = as.numeric(plansAttr[[var]])
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

#Read in plan description data 
plansDesc = read.csv("Data/2015_Premiums/2015_QHP_Landscape_Individual_Market_Medical.csv")

NM = read.csv("Data/2015_Premiums/2015_QHP_Landscape_NM_Individual_Market_Medical.csv")
NV = read.csv("Data/2015_Premiums/2015_QHP_Landscape_NV_Individual_Market_Medical.csv")
OR= read.csv("Data/2015_Premiums/2015_QHP_Landscape_OR_Individual_Market_Medical.csv")

plansDesc = rbind(plansDesc,NM)
plansDesc = rbind(plansDesc,NV)
plansDesc = rbind(plansDesc,OR)

rm(NM,NV,OR)

#Drop Child-Only Plans
plansDesc = plansDesc[plansDesc$Child.Only.Offering=="Allows Adult and Child-Only",]

#Isolate important characteristic variables
plansDesc = plansDesc[,c("State","Metal.Level","Rating.Area","Plan.ID..standard.component.",
                     "Issuer.Name","Plan.Marketing.Name",
                     "Premium.Adult.Individual.Age.27",
                     "Medical.Deductible.individual.standard",
                     "Drug.Deductible.individual.standard",
                     "Medical.Maximum.Out.Of.Pocket...individual...standard",  
                     "Drug.Maximum.Out.of.Pocket...individual...standard"
)]

names(plansDesc) = c("State","Metal","RatingArea","Plan.ID","Issuer","PlanName",
                   "Prem27","MedDeduct","DrugDeduct",
                   "MedOOP","DrugOOP")
plansDesc = unique(plansDesc)

plans = merge(plansDesc,plansAttr,by="Plan.ID")



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
for (var in c("Prem27","MedDeduct","DrugDeduct","MedOOP","DrugOOP")){
  #Remove dollar signs from the data
  plans[[var]]=gsub("[$,]","",plans[[var]])
  #Convert premiums to a numeric variable
  plans[[var]] = as.numeric(plans[[var]])
}
#Check on Premium NAs


plans$Area = paste(plans$State,plans$RatingArea)
res = lm(Prem27~AV.Calculator.Output.Number*Area,data=plans)

plans$pHat = NA
plans$pHat[!is.na(plans$Prem27)&!is.na(plans$AV.Calculator.Output.Number)] = predict(res)
plans$resid = with(plans,Prem27-pHat)

test = plans[plans$State=="IL"&plans$RatingArea=="Rating Area 1",]
write.csv(test,"selectiontest.csv",row.names=FALSE)

ggplot(plans) + 
  aes(x=resid) + 
  geom_histogram()


ggplot(plans) + 
  aes(y=resid,x=MedDeduct) + 
  geom_point()

res = lm(resid~MedDeduct,data=plans[plans$Metal=="Bronze",])
summary(res)

ggplot(plans) + 
  aes(y=resid,x=distance) + 
  geom_point()

res = lm(resid~distance,data=plans)
summary(res)


ggplot(plans) + 
  aes(y=resid,x=diabetes) + 
  geom_point()

res = lm(resid~diabetes,data=plans[plans$Metal=="Platinum",])
summary(res)
