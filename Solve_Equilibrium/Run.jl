using DataFrames
using CSV
using BenchmarkTools
using JLD2
########################################################################
#################### Loading and Cleaning Data #########################
########################################################################
# Set Directory
cd("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/")

# Load Cost
include("predictionData.jl")
include("SolveModel.jl")
include("EquilibriumFunctions.jl")
include("MLR_Functions.jl")
# run_st_equil("NE")
# Check_Margin("NE")
rundate = "2019-06-25"
#
# states = ["AK","NE","ND","OK","MD","IA","NM","UT",
# "OR","MO","IL","MI","GA","TX"]
states = ["TX"]
for st in states
    # println("Solve Baseline Market Structure")
    # run_st_equil(st,rundate)
    # if st in ["MO","IL","MI","GA"]
    #     println("Solve Merger Market Structure")
    #     run_st_equil(st,rundate,merger=true)
    # end
    println("Check Margin")
    Check_Margin(st,rundate)
end
