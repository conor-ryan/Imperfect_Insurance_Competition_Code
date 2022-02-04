setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition")

###### Choice Set ######
rm(list=ls())
source("Code/2015_Premiums/ChoiceSets.R")
  
###### Base Demand Data ######
# rm(list=ls())
source("Code/Simulation/ACS_Prep.R")

rm(list=ls())
source("Code/MEPS_Moments/MEPS_Moments.R")

rm(list=ls())
source("Code/Demand_Data_Prep/Estimation_Data_Discrete.R")
# source("Code/Risk_Scores/FirmLevelRisk_woSim.R") # This file is now run inside Estimation_Data_Discrete.R

###### Data Preperation ######
rm(list=ls())
source("Code/Rate_Filings/Rate_Process.R")

###### Simulation Data ######
rm(list=ls())
source("Code/Simulation/Simulation_BaseData.R")

rm(list=ls())
source("Code/Solve_Equilibrium/EquilibriumProductData.R")

##### Moment Data #####
rm(list=ls())
source("Code/Risk_Scores/riskMoments.R")
rm(list=ls())
source("Code/Firm_Side/MarginalCost_momentData.R")