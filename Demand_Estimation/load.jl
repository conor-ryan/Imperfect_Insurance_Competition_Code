using DataFrames
using CSV
########################################################################
#################### Loading and Cleaning Data #########################
########################################################################
cd("$(homedir())/Documents/Research/Imperfect_Insurance_Competition")

# Load the data
df = CSV.read("Intermediate_Output/estimationData_MI.csv")
df_mkt = CSV.read("Intermediate_Output/marketData_MI.csv")
# May need to change column types
# for key in [:Firm, :Product]
#     df[key] = String.(df[key])
# end
df[:Firm] = String.(df[:Firm])
# No constant
#df[:constant] = ones(size(df, 1))
