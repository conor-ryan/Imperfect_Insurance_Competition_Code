using DataFrames
using CSV
using BenchmarkTools
########################################################################
#################### Loading and Cleaning Data #########################
########################################################################
# Set Directory

# Load Cost
include("predictionData.jl")

states = ["AK","NE","ND","OK","MD","IA","NM","UT",
"OR","MO","IL","MI","GA","TX"]

for st in states
    run_st_equil(st)
end


#
#
#
#
#
#
#
#
#
#
#
#
# # Load the data
# df = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Equilibrium_Data/estimated_Data_GA.csv",
# types=Dict("AGE"=>Float64,"Mandate"=>Float64,"MEMBERS"=>Float64,"Gamma_j"=>Union{Missing,Float64}),null="NA")
# df_mkt = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Equilibrium_Data/estimated_prodData_GA.csv",
# null="NA")
# cost_pars = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Equilibrium_Data/cost_pars.csv",
# null="NA")
#
# include("predictionData.jl")
#
#
# c = ChoiceData(df,df_mkt)
#
# model = EqData(c,df_mkt,cost_pars)
#
# solve_model!(model,0.5)
#
#
#
# @benchmark prod_data(model)
#
# println("Profiler")
# Profile.init()
# Profile.clear()
# Juno.@profile eval_FOC(model)
# Juno.profiletree()
# Juno.profiler()
