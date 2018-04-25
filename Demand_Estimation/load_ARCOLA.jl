using DataFrames
using CSV
########################################################################
#################### Loading and Cleaning Data #########################
########################################################################
# Load the data
df = CSV.read("$(homedir())/Documents/Research/ARCOLA/Detailed Demand Estimation/estimationData_discrete.csv")
df_mkt = CSV.read("$(homedir())/Documents/Research/ARCOLA/Detailed Demand Estimation/marketData_discrete.csv")
# May need to change column types
# for key in [:Firm, :Product]
#     df[key] = String.(df[key])
# end
# No constant
#df[:constant] = ones(size(df, 1))
