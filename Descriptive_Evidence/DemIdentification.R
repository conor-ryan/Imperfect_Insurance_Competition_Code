rm(list=ls())
library(doBy)
library(randtoolbox)
library(data.table)
library(ggplot2)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition")

#### 2015 Subsidy Percentage Function ####

subsPerc <- function(FPL){
  x = FPL[!is.na(FPL)]
  y = rep(100,length(x))
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

#### Read in Estimating Data ####
estData = read.csv("Intermediate_Output/Estimation_Data/descriptiveData_discrete.csv")
estData = as.data.table(estData)
setkey(estData,Person,Product)

#### Create Density Bars ####
age_density = estData[,list(dens = sum(N)),by="AGE_bucket"]
setkey(age_density,AGE_bucket)
age_density[,Age:= c(18,21,24,27,31,34,37,40,43,46,49,52,55,58,61,64)]
age_density[,dens:=dens/max(dens)*3]
inc_density = estData[,list(dens = sum(N)),by="FPL_bucket"]
setkey(inc_density,FPL_bucket)
inc_density[,fpl:=c(1,1.25,1.5,1.75,2,2.25,2.5,3,3.5,4)]
inc_density[,dens:=dens/max(dens)*4600]

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


png("Writing/Images/IncomeIdent_deduct.png",width=2000,height=1500,res=275)
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



inc_density$price = 11770*inc_density$fpl*subsPerc(inc_density$fpl)

png("Writing/Images/IncomeIdent_price.png",width=2000,height=1500,res=275)
ggplot() + 
  geom_bar(data=inc_density,aes(x=fpl,y=dens),stat = "identity",color=grey(.5)) + 
  geom_line(data=inc_density,aes(x=fpl,y=price,color="Maximum Annual Premium of\nSecond Lowest Cost Silver Plan"),size=1.2)+
  scale_x_continuous(breaks=c(1,2,3,4),
                     labels = c("100%","200%","300%",">400%")) + 
  scale_y_continuous(label=dollar) + 
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