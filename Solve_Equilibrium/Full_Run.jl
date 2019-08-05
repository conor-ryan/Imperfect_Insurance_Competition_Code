using BenchmarkTools
using JLD2
using CSV
using Random
using Dates
using LinearAlgebra
using Statistics
using DataFrames


load_path = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Code/"
include("$load_path/Demand_Estimation/InsChoiceData.jl")
include("$load_path/Demand_Estimation/Specification_Run.jl")
include("$load_path/Firm_Side/SpecRunMC.jl")
include("predictionData.jl")
include("FirmFunctions.jl")


##### Set Specification ####
halton_draws = 500
spec_demoRaw = [:AgeFE_31_39,
        :AgeFE_40_51,
        :AgeFE_52_64,
        :Family,
        :LowIncome]
spec_prodchars=[:Price,:constant,:AV,:HighRisk,:Small,:High_small]
spec_prodchars_0=[:constant,:AV,:HighRisk,:Small,:High_small]
spec_fixedEffects=[:Firm]

cost_spec = [:AvgAge,:AV]

rundate = Dates.today()
println("Running on $rundate")
spec = "Firm"


println("##### Estimate Demand #####")
filename = "GMM_Estimate_$spec"
estimate_demand(filename,rundate,
                    halton_draws,
                    spec_demoRaw,
                    spec_prodchars,
                    spec_prodchars_0,
                    spec_fixedEffects)

println("##### Estimation Marginal Cost #####")
rundate = "2019-08-03"
estimate_marginal_cost(rundate,spec,cost_spec)


println("##### Solve Equilibrium #####")
rundate = "2019-08-03"
solve_equilibrium(rundate,spec)
