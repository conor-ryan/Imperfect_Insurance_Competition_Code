using DataFrames
using CSV
########################################################################
#################### Loading and Cleaning Data #########################
########################################################################
cd("$(homedir())/Documents/Research/Imperfect_Insurance_Competition")


# Load the data
df = CSV.read("Intermediate_Output/estimationData.csv")

# May need to change column types
for key in [:Firm, :Product]
    df[key] = String.(df[key])
end

# Add a constant
df[:constant] = ones(size(df, 1))
