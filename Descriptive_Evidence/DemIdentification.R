rm(list=ls())
library(doBy)
library(randtoolbox)
library(data.table)
library(ggplot2)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition")


#### Read in Estimating Data ####
estData = read.csv("Intermediate_Output/Estimation_Data/descriptiveData_discrete.csv")
estData = as.data.table(estData)
setkey(estData,Person,Product)

#### Create Density Bars ####
age_density = estData[,list(dens = sum(N)),by="AGE_bucket"]
age_density[,Age:= c(26,22,35,39,43,47,51,55,59,31)]
age_density[,dens:=dens/max(dens)*3]
inc_density = estData[,list(dens = sum(N)),by="FPL_bucket"]
inc_density[,fpl:=c(1,1.5,2,2.5,3,3.5,4,.5)]
inc_density[,dens:=dens/max(dens)*6600]

#### Read in Age Rating Curve ####
rating = read.csv("Data/AgeRating.csv")
rating = rating[rating$State=="Default",]

breaks = sort(c(26,22,35,39,43,47,51,55,59,31))

png("Writing/Images/AgeIdent.png",width=2000,height=1500,res=275)
ggplot() + 
  geom_bar(data=age_density,aes(x=Age,y=dens),stat = "identity",color=grey(.5)) +  
  geom_line(data=rating,aes(x=Age,y=Rating,color="Age Rating Curve"),size=1.2) + 
  scale_x_continuous(breaks=breaks,
                     labels = c("<26",as.character(breaks[-1]))) + 
  xlab("Age Buckets") + 
  ylab("") + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_rect(color=grey(.5)),
    legend.title=element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "bottom",
    axis.title=element_text(size=12),
    axis.text = element_text(size=12))
dev.off()




#### Read in choice Sets ####
choiceSets = read.csv("Intermediate_Output/Premiums/choiceSets2015.csv")
choiceSets = as.data.table(choiceSets)
choiceSets[,MedDeduct:=pmin(MedDeduct,MedOOP)]

deduct = choiceSets[ST=="IL"&AREA=="Rating Area 3",list(Deduct= mean(MedOOP,na.rm=TRUE)),by="METAL"]
deduct[,merge:=1]
FPL = data.table(fpl=seq(0,4,.1),merge=1)
deduct = merge(deduct,FPL,by="merge",allow.cartesian=TRUE)

deduct = deduct[!(METAL=="Silver"& (fpl<=2.5)),]
deduct = deduct[!(METAL=="Silver 73"& (fpl>=2.5|fpl<=2)),]
deduct = deduct[!(METAL=="Silver 87"& (fpl>=2|fpl<=1.5)),]
deduct = deduct[!(METAL=="Silver 94"& (fpl>=1.5|fpl<=1)),]
deduct[,METAL:=gsub("Silver.*","Silver",METAL)]


png("Writing/Images/IncomeIdent.png",width=2000,height=1500,res=275)
ggplot() + 
  geom_bar(data=inc_density,aes(x=fpl,y=dens),stat = "identity",color=grey(.5)) + 
  geom_line(data=deduct,aes(x=fpl,y=Deduct,color=METAL),size=1.2)+
  scale_x_continuous(breaks=c(1,2,3,4),
                      labels = c("100%","200%","300%",">400%")) + 
  xlab("Income Buckets") + 
  ylab("") + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_rect(color=grey(.5)),
    legend.title=element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "bottom",
    axis.title=element_text(size=12),
    axis.text = element_text(size=12))
dev.off()