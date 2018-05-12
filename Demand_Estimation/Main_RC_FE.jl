using BenchmarkTools
using JLD
using CSV

# Data Structure
include("InsChoiceData.jl")

#Halton Draws
include("Halton.jl")

# Random Coefficients MLE
include("RandomCoefficients.jl")
include("Contraction.jl")
include("Log_Likehood.jl")
include("Estimate_Basic.jl")
include("Specification_Run.jl")
# Load the Data
include("load.jl")

println("Code Loaded")


#### General Specification ####

halton_draws = 50
spec_demoRaw = [:AgeFE_31_40,
        :AgeFE_41_50,
        :AgeFE_51_64,
        :Family,
        :LowIncome]
spec_prodchars=[:Price,:AV]
spec_prodchars_0=[]

rundate = Dates.today()

#### Run Specification 1 ####
println("Run Specification 1")
spec1 = run_specification(df,df_mkt,
                    haltonDim = halton_draws,
                    spec_demoRaw=spec_demoRaw,
                    spec_prodchars=spec_prodchars,
                    spec_prodchars_0=spec_prodchars_0,
                    spec_fixedEffects=[:Firm])


file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estresults_fe_rc_spec1_$rundate.jld"
save(file,"spec1",spec1)

#### Run Specification 2 ####
println("Run Specification 2")
spec2 = run_specification(df,df_mkt,
                    haltonDim = halton_draws,
                    spec_demoRaw=spec_demoRaw,
                    spec_prodchars=spec_prodchars,
                    spec_prodchars_0=spec_prodchars_0,
                    spec_fixedEffects=[:Firm,:Market])

file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estresults_fe_rc_spec2_$rundate.jld"
save(file,"spec2",spec2)

#### Run Specification 2 ####
println("Run Specification 3")
spec3 = run_specification(df,df_mkt,
                    haltonDim = halton_draws,
                    spec_demoRaw=spec_demoRaw,
                    spec_prodchars=spec_prodchars,
                    spec_prodchars_0=spec_prodchars_0,
                    spec_fixedEffects=[:Firm_Market_Cat])

file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estresults_fe_rc_spec3_$rundate.jld"
save(file,"spec3",spec3)
