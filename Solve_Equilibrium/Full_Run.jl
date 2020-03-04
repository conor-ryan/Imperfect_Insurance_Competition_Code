using BenchmarkTools
using JLD2
using CSV
using Random
using Dates
using LinearAlgebra
using Statistics
using DataFrames

codeDir = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Code/"

##### Set Specification ####
halton_draws = 1000
spec_demoRaw = [:AgeFE_31_39,
        :AgeFE_40_51,
        :AgeFE_52_64,
        :Family,
        :LowIncome]
spec_prodchars=[:Price,:AV,:constant,:HighRisk,:Small,:High_small]
spec_prodchars_0=[:AV,:constant,:HighRisk,:Small,:High_small]


cost_spec = [:AGE,:AV]

rundate = Dates.today()
# rundate = "2019-12-07"
println("Running on $rundate")
spec = "FMC"
spec_fixedEffects=[:Market_Firm,:Market_Cat]

println("##### Estimate Demand #####")
println("Load Demand Estimation Code...")
# Data Structure
include("$codeDir/Demand_Estimation/InsChoiceData.jl")
#Halton Draws
include("$codeDir/Demand_Estimation/Halton.jl")
# Random Coefficients MLE
include("$codeDir/Demand_Estimation/RandomCoefficients.jl")
include("$codeDir/Demand_Estimation/RandomCoefficients_der.jl")
include("$codeDir/Demand_Estimation/DerivFunctions.jl")
include("$codeDir/Demand_Estimation/Log_Likehood.jl")
# GMM Functions
include("$codeDir/Demand_Estimation/Contraction.jl")
include("$codeDir/Demand_Estimation/RiskMoments.jl")
include("$codeDir/Demand_Estimation/GMM_Var.jl")
# Estimation Functions
include("$codeDir/Demand_Estimation/Estimate_Basic.jl")
include("$codeDir/Demand_Estimation/Estimate_GMM.jl")
include("$codeDir/Demand_Estimation/Estimate_TwoStage.jl")
include("$codeDir/Demand_Estimation/utility.jl")
include("$codeDir/Demand_Estimation/Specification_Run.jl")

filename = "GMM_Estimate_$spec"
estimate_demand(filename,rundate,
                    halton_draws,
                    spec_demoRaw,
                    spec_prodchars,
                    spec_prodchars_0,
                    spec_fixedEffects)


println("##### Estimation Marginal Cost #####")
println("Load Marginal Cost Estimation Code...")
include("$codeDir/Firm_Side/MC_parameters.jl")
include("$codeDir/Firm_Side/MC_GMM.jl")
include("$codeDir/Firm_Side/MC_var.jl")
include("$codeDir/Firm_Side/MC_derivatives.jl")
include("$codeDir/Firm_Side/MC_optimization.jl")
include("$codeDir/Firm_Side/Firm_Inner_Loop.jl")
include("$codeDir/Firm_Side/SpecRunMC.jl")
estimate_marginal_cost(rundate,spec,cost_spec)


println("##### Solve Equilibrium #####")
println("Load Equilibrium Code...")
include("predictionData.jl")
include("FirmFunctions.jl")
include("SolveModel.jl")
include("EvaluateModel.jl")
include("PriceUpdate.jl")
include("ConsumerWelfare.jl")
include("PlannerProblem.jl")
solve_equilibrium(rundate,spec)
