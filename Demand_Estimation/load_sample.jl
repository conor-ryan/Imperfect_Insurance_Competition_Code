using DataFrames
using CSV
########################################################################
#################### Loading and Cleaning Data #########################
########################################################################
# Load the data
df = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Data/estimationData_MI_discrete.csv")
df_mkt = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Data/marketData_MI_discrete.csv")
# May need to change column types
# for key in [:Firm, :Product]
#     df[key] = String.(df[key])
# end
df[:Firm] = String.(df[:Firm])
# No constant
#df[:constant] = ones(size(df, 1))
