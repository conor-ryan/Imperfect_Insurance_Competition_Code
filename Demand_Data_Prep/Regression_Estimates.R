rm(list=ls())
library(doBy)
library(randtoolbox)
library(data.table)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition")

## Run 
run = "2018-05-02"

#### Read in Data ####
estData = read.csv("Intermediate_Output/Estimation_Data/estimationData_MI_discrete.csv")
estData = as.data.table(estData)
setkey(estData,Person,Product)