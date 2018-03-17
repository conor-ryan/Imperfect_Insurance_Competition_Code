using DataFrames
using CSV
using JLD

# Data Structure
include("InsChoiceData.jl")

#Halton Draws
include("Halton.jl")

# Random Coefficients MLE
include("MLE_RC.jl")
println("Code Loaded")

# Predict on Full Data
cd("$(homedir())/Documents/Research/Imperfect_Insurance_Competition")
df = CSV.read("Intermediate_Output/Estimation_Data/estimationData_discrete.csv")
df_mkt = CSV.read("Intermediate_Output/Estimation_Data/marketData_discrete.csv")
df[:Firm] = String.(df[:Firm])


c = ChoiceData(df,df_mkt)
# Fit into model
m = InsuranceLogit(c,500)

file = "Estimation_Output/estimationresults_2018-03-17.jld"
flag, fval, p_est = load(file)["p_est"]
paramFinal = parDict(m,p_est)

contraction!(m,paramFinal)

run = Dates.today()
out1 = DataFrame(pars=p_est)
file1 = "Estimation_Output/estimationresults_$run.csv"
CSV.write(file1,out1)

out2 = DataFrame(delta=m.deltas,prods=m.prods)
file2 = "Estimation_Output/deltaresults_$run.csv"
CSV.write(file2,out2)
