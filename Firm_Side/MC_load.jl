using DataFrames
using CSV
########################################################################
#################### Loading and Cleaning Data #########################
########################################################################
# Load the data
df = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Simulated_BaseData/simchoiceData_discrete.csv",
DataFrame,types=Dict("unins_rate"=>Float64,"S_ij"=>Float64,"var_HCC_Silver"=>Float64))

df_mkt = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Data/marketData_discrete.csv",DataFrame)

df_risk = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Data/riskMoments.csv",DataFrame)
df_transfer = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Data/firmMoments.csv",DataFrame)
# May need to change column types
# for key in [:Firm, :Product]
#     df[key] = String.(df[key])
# end
df[!,:Firm] = String.(df[!,:Firm])
# No constant
df[!,:constant] = ones(size(df, 1))


#### Load Moments
mom_firm = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/MC_Moments/firmMoments.csv",DataFrame)
mom_metal = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/MC_Moments/metalMoments.csv",DataFrame)
mom_age = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/MC_Moments/ageMoments.csv",DataFrame)
mom_age_no = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/MC_Moments/ageMoments_noHCC.csv",DataFrame)
mom_risk = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/MC_Moments/riskMoments.csv",DataFrame)
mom_ra = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/MC_Moments/raMoments.csv",DataFrame)


#### Original Demand Data
df_dem = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Data/estimationData_discrete.csv",DataFrame,
types=Dict("unins_rate"=>Float64,"S_ij"=>Float64,"var_HCC_Silver"=>Float64))

df_dem[!,:Firm] = String.(df_dem[!,:Firm])
# No constant
df_dem[!,:constant] = ones(size(df_dem, 1))
