using DataFrames
using CSV
using BenchmarkTools
########################################################################
#################### Loading and Cleaning Data #########################
########################################################################
# Set Directory

# Load Cost
include("predictionData.jl")
include("SolveModel.jl")
include("EquilibriumFunctions.jl")
# run_st_equil("NE")
# Check_Margin("NE")

# states = ["AK","NE","ND","OK","MD","IA","NM","UT",
# "OR","MO","IL","MI","GA","TX"]
# for st in states
    st ="MI"
    run_st_equil(st)
    if st in ["IL","MI","GA"]
        run_st_equil(st,merger=true)
    end
    println("Check Margin")
    Check_Margin(st)
# end


# # Load the data
# df = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Equilibrium_Data/estimated_Data_AK.csv",
# types=Dict("AGE"=>Float64,"Mandate"=>Float64,"MEMBERS"=>Float64,"Gamma_j"=>Union{Missing,Float64}),null="NA")
# df_mkt = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Equilibrium_Data/estimated_prodData_AK.csv",
# null="NA")
# cost_pars = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Equilibrium_Data/cost_pars.csv",
# null="NA")
#
#
#
# c = ChoiceData(df,df_mkt)
#
# model = EqData(c,df_mkt,cost_pars)
#
# evaluate_model!(model)
# foc_Std, foc_RA, foc_RA_fix, S_m, dsdp_rev = eval_FOC(model)
#
# P_new = predict_price(foc_Std,foc_RA,foc_RA_fix,S_m,dsdp_rev,model)
#
# solve_model!(model,0.02,sim="Fixed Transfer")
# solve_model!(model,0.02,sim="No Transfer")
# solve_model!(model,0.02,sim="Half Transfer")
# #
# #
# #
# # @benchmark prod_data(model)
# #
# println("Profiler")
# Profile.init()
# Profile.clear()
# Juno.@profile evaluate_model!(model)
# Juno.profiletree()
# Juno.profiler()
