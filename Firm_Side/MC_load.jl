using DataFrames
using CSV
########################################################################
#################### Loading and Cleaning Data #########################
########################################################################
# Load the data
df = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Simulated_BaseData/simchoiceData_discrete.csv",
types=Dict("unins_rate"=>Float64,"S_ij"=>Float64,"var_HCC_Silver"=>Float64))

df_mkt = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Data/marketData_discrete.csv")

df_risk = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Data/riskMoments.csv")
# May need to change column types
# for key in [:Firm, :Product]
#     df[key] = String.(df[key])
# end
df[:Firm] = String.(df[:Firm])
# No constant
df[:constant] = ones(size(df, 1))


#### Load Moments
mom_firm = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/MC_Moments/firmMoments.csv")
mom_metal = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/MC_Moments/metalMoments.csv")
mom_age = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/MC_Moments/ageMoments.csv")
mom_risk = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/MC_Moments/riskMoments.csv")
