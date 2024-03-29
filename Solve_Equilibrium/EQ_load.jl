using DataFrames
using CSV
########################################################################
#################### Loading and Cleaning Data #########################
########################################################################
# Load the data
df = CSV.read("$(home_directory)/Research/Imperfect_Insurance_Competition/Intermediate_Output/Simulated_BaseData/simchoiceData_discrete.csv",DataFrame,
types=Dict("unins_rate"=>Float64,"S_ij"=>Float64,"var_HCC_Silver"=>Float64,"Gamma_j"=>Float64))

# df = CSV.read("$(homedir())/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Data/estimationData_discrete.csv",
# types=Dict("unins_rate"=>Float64,"S_ij"=>Float64,"var_HCC_Silver"=>Float64))
df_mkt = CSV.read("$(home_directory)/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Data/marketDataMap_discrete.csv",DataFrame)

df_risk = CSV.read("$(home_directory)/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Data/riskMoments.csv",DataFrame)
df_transfer = CSV.read("$(home_directory)/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Data/firmMoments.csv",DataFrame)
# May need to change column types
# for key in [:Firm, :Product]
#     df[key] = String.(df[key])
# end
df[!,:Firm] = String.(df[!,:Firm])
# No constant
df[!,:constant] = ones(size(df, 1))


mom_firm = CSV.read("$(home_directory)/Research/Imperfect_Insurance_Competition/Intermediate_Output/MC_Moments/firmMoments.csv",DataFrame)
mom_metal = CSV.read("$(home_directory)/Research/Imperfect_Insurance_Competition/Intermediate_Output/MC_Moments/metalMoments.csv",DataFrame)
mom_age = CSV.read("$(home_directory)/Research/Imperfect_Insurance_Competition/Intermediate_Output/MC_Moments/ageMoments.csv",DataFrame)
mom_age_no = CSV.read("$(home_directory)/Research/Imperfect_Insurance_Competition/Intermediate_Output/MC_Moments/ageMoments_noHCC.csv",DataFrame)
mom_risk = CSV.read("$(home_directory)/Research/Imperfect_Insurance_Competition/Intermediate_Output/MC_Moments/riskMoments.csv",DataFrame)
mom_ra = CSV.read("$(home_directory)/Research/Imperfect_Insurance_Competition/Intermediate_Output/MC_Moments/raMoments.csv",DataFrame)

eq_mkt = CSV.read("$(home_directory)/Research/Imperfect_Insurance_Competition/Intermediate_Output/Equilibrium_Data/estimated_prodData_full.csv",DataFrame, missingstring="NA")
