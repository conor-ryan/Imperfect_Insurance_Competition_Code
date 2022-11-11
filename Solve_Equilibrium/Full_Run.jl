using Distributed
println("Add Workers")
addprocs(4)

@everywhere using BenchmarkTools
@everywhere using JLD2
@everywhere using CSV
@everywhere using Random
@everywhere using Dates
@everywhere using LinearAlgebra
@everywhere using Statistics
@everywhere using DataFrames
@everywhere using StatsFuns
@everywhere using ForwardDiff
@everywhere using NLopt
@everywhere using FiniteDiff
@everywhere using SharedArrays


## Check OS Environment
if occursin(r"cxr5626",homedir())
        @everywhere home_directory = "$(homedir())/work"
else
        @everywhere home_directory = "$(homedir())/Documents"
end

@everywhere codeDir = "$(home_directory)/Research/Imperfect_Insurance_Competition/Code/"

##### Set Specification ####
halton_draws = 500
spec_demoRaw = [:AgeFE_31_39,
        :AgeFE_40_51,
        :AgeFE_52_64,
        :Family,
        :LowIncome]
spec_prodchars=[:Price,:AV]
spec_prodchars_σ=[:AV,
:AK_MODA_HEALTH_PLAN_INC,:AK_PREMERA_BLUE_CROSS_BLUE_SHIELD_OF_ALASKA,
:GA_AETNA,:GA_AMBETTER_FROM_PEACH_STATE_HEALTH_PLAN,:GA_ASSURANT_HEALTH,:GA_BLUE_CROSS_BLUE_SHIELD_OF_GEORGIA,:GA_CIGNA_HEALTH_AND_LIFE_INSURANCE_COMPANY,:GA_HUMANA,:GA_KAISER_PERMANENTE_GA,:GA_UNITEDHEALTHCARE_LIFE_INS_CO,:GA_UNITEDHEALTHCARE_OF_GEORGIA_INC,
:IA_AVERA_HEALTH_PLANS,:IA_COVENTRY_HEALTH_CARE_OF_IOWA_INC,:IA_WELLMARK_BLUE_CROSS_AND_BLUE_SHIELD_OF_IOWA]
cost_spec = [:AGE,:AV]

rundate = Dates.today()
# rundate = "2022-03-18"
println("Running on $rundate")
spec = "FMC"
spec_fixedEffects=[:Firm_ST]

println("##### Estimate Demand #####")
println("Load Demand Estimation Code...")
# Data Structure
@everywhere include("$codeDir/Demand_Estimation/InsChoiceData.jl")
#Halton Draws
@everywhere include("$codeDir/Demand_Estimation/Halton.jl")
# Random Coefficients MLE
@everywhere include("$codeDir/Demand_Estimation/RandomCoefficients.jl")
@everywhere include("$codeDir/Demand_Estimation/RandomCoefficients_der.jl")
@everywhere include("$codeDir/Demand_Estimation/DerivFunctions.jl")
@everywhere include("$codeDir/Demand_Estimation/Log_Likehood.jl")
@everywhere include("$codeDir/Demand_Estimation/Log_Likehood_Penalty.jl")

# GMM Functions
@everywhere include("$codeDir/Demand_Estimation/Contraction.jl")
@everywhere include("$codeDir/Demand_Estimation/RiskMoments.jl")
@everywhere include("$codeDir/Demand_Estimation/GMM_Var.jl")
# Estimation Functions
@everywhere include("$codeDir/Demand_Estimation/Estimate_Basic.jl")
@everywhere include("$codeDir/Demand_Estimation/Estimate_GMM.jl")
@everywhere include("$codeDir/Demand_Estimation/Estimate_TwoStage.jl")
@everywhere include("$codeDir/Demand_Estimation/utility.jl")
@everywhere include("$codeDir/Demand_Estimation/Specification_Run.jl")

filename = "PLL_Estimate_$spec"
estimate_demand(filename,rundate,home_directory,
                    halton_draws,
                    spec_demoRaw,
                    spec_prodchars,
                    spec_prodchars_σ,
                    spec_fixedEffects)


println("##### Estimation Marginal Cost #####")
println("Load Marginal Cost Estimation Code...")
@everywhere include("$codeDir/Firm_Side/MC_parameters.jl")
@everywhere include("$codeDir/Firm_Side/MC_GMM.jl")
@everywhere include("$codeDir/Firm_Side/MC_var.jl")
@everywhere include("$codeDir/Firm_Side/MC_derivatives.jl")
@everywhere include("$codeDir/Firm_Side/MC_optimization.jl")
@everywhere include("$codeDir/Firm_Side/Firm_Inner_Loop.jl")
@everywhere include("$codeDir/Firm_Side/SpecRunMC.jl")
# estimate_marginal_cost(rundate,spec,cost_spec)

@everywhere include("ProcessDemResults.jl")
# process_demand(rundate,spec)


println("##### Solve Equilibrium #####")
println("Load Equilibrium Code...")
@everywhere include("predictionData.jl")
@everywhere include("FirmFunctions.jl")
@everywhere include("SolveModel.jl")
@everywhere include("EvaluateModel.jl")
@everywhere include("PriceUpdate.jl")
@everywhere include("ConsumerWelfare.jl")
@everywhere include("PlannerProblem.jl")
@everywhere include("SolveMergers.jl")

# MergersMain(rundate,spec,home_directory)


# solve_equilibrium(rundate,spec)
