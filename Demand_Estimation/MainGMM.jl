using BenchmarkTools
using JLD2
using CSV
using Random
using Dates
using LinearAlgebra
using Statistics


# Data Structure
include("InsChoiceData.jl")

#Halton Draws
include("Halton.jl")

# Random Coefficients MLE
include("RandomCoefficients.jl")
include("Specification_Run.jl")
include("RandomCoefficients_der.jl")
include("Contraction.jl")
include("Log_Likehood.jl")
include("RiskMoments.jl")
include("Estimate_Basic.jl")
include("Estimate_GMM.jl")
include("Estimate_TwoStage.jl")
include("GMM_Var.jl")
include("utility.jl")
include("DerivFunctions.jl")
println("Code Loaded")

# Load the Data
include("load.jl")


#### General Specification ####

halton_draws = 500
spec_demoRaw = [:AgeFE_31_39,
        :AgeFE_40_51,
        :AgeFE_52_64,
        :Family,
        :LowIncome]
spec_prodchars=[:Price,:constant,:AV,:Big]
spec_prodchars_0=[:constant,:AV,:Big]

rundate = Dates.today()
println("Running on $rundate")
#### Run Specification 1 ####
# println("#### Run Specification 1  - Firm Fixed Effects ####")
# filename = "GMM_Estimate_Firm"
# spec1 = run_specification_GMM(filename,rundate,
#                     df,df_mkt,df_risk,
#                     haltonDim = halton_draws,
#                     spec_demoRaw=spec_demoRaw,
#                     spec_prodchars=spec_prodchars,
#                     spec_prodchars_0=spec_prodchars_0,
#                     spec_fixedEffects=[:Firm])


# #### Run Specification 1 ####
println("#### Run Specification 2  - Firm-Market-Category Fixed Effects ####")
filename = "GMM_Estimate_FMC"
spec1 = run_specification_GMM(filename,rundate,
                df,df_mkt,df_risk,
                haltonDim = halton_draws,
                spec_demoRaw=spec_demoRaw,
                spec_prodchars=spec_prodchars,
                spec_prodchars_0=spec_prodchars_0,
                spec_fixedEffects=[:Firm_Market_Cat])


println("#### Run Specification 3  - Firm-Market-Category-Age Fixed Effects ####")
filename = "GMM_Estimate_FMCA"
spec1 = run_specification_GMM(filename,rundate,
                df,df_mkt,df_risk,
                haltonDim = halton_draws,
                spec_demoRaw=spec_demoRaw,
                spec_prodchars=spec_prodchars,
                spec_prodchars_0=spec_prodchars_0,
                spec_fixedEffects=[:Firm_Market_Cat_Age])
