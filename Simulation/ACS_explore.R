rm(list=ls())
library(doBy)
library(randtoolbox)
library(data.table)
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

#### Load Prepped ACS Data ####

load("Intermediate_Output/Simulated_BaseData/acs_unrest.rData")
acs = acs[insured==TRUE,]

acs[,ageBracket:="Under 18"]
acs[AGE>=18,ageBracket:="18 to 26"]
acs[AGE>=26,ageBracket:="26 to 34"]
acs[AGE>=35,ageBracket:="35 to 44"]
acs[AGE>=45,ageBracket:="45 to 54"]
acs[AGE>=55,ageBracket:="55 to 64"]

ageDist = acs[,list(Pop = sum(PERWT)),by="ageBracket"]
ageDist[,Pop:=Pop/sum(Pop)]
setkey(ageDist,ageBracket)
ageDist

acs[,incBracket:="a - Under 200%"]
acs[HHincomeFPL>=2,incBracket:="b - 200% to 400%"]
acs[HHincomeFPL>=4,incBracket:="c - Over 400%"]

incDist = acs[,list(Pop = sum(PERWT)),by="incBracket"]
incDist[,Pop:=Pop/sum(Pop)]
setkey(incDist,incBracket)
