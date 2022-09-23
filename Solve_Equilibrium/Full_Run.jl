using BenchmarkTools
using JLD2
using CSV
using Random
using Dates
using LinearAlgebra
using Statistics
using DataFrames

## Check OS Environment
if occursin(r"cxr5626",homedir())
        home_directory = "$(homedir())/work"
else
        home_directory = "$(homedir())/Documents"
end

codeDir = "$(home_directory)/Research/Imperfect_Insurance_Competition/Code/"

##### Set Specification ####
halton_draws = 500
spec_demoRaw = [:AgeFE_31_39,
        :AgeFE_40_51,
        :AgeFE_52_64,
        :Family,
        :LowIncome]
spec_prodchars=[:Price,:constant,:AV]
spec_prodchars_σ=[:constant,:AV,
# :AK_MODA_HEALTH_PLAN_INC,
:AK_PREMERA_BLUE_CROSS_BLUE_SHIELD_OF_ALASKA,
# :GA_AETNA,
:GA_BLUE_CROSS_BLUE_SHIELD_OF_GEORGIA,
:GA_UNITEDHEALTHCARE_LIFE_INS_CO,
:GA_UNITEDHEALTHCARE_OF_GEORGIA_INC,
:GA_ASSURANT_HEALTH,
:GA_CIGNA_HEALTH_AND_LIFE_INSURANCE_COMPANY,
:GA_HUMANA,
:GA_KAISER_PERMANENTE_GA,
:GA_AMBETTER_FROM_PEACH_STATE_HEALTH_PLAN,
# :IA_COVENTRY_HEALTH_CARE_OF_IOWA_INC,
:IA_WELLMARK_BLUE_CROSS_AND_BLUE_SHIELD_OF_IOWA,
:IA_AVERA_HEALTH_PLANS,
# :IL_AETNA,
:IL_AMBETTER_INSURED_BY_CELTIC,
:IL_BLUE_CROSS_AND_BLUE_SHIELD_OF_ILLINOIS,
:IL_COVENTRY,
:IL_HUMANA,
:IL_LAND_OF_LINCOLN_HEALTH,
:IL_UNITEDHEALTHCARE_LIFE_INS_CO,
:IL_UNITEDHEALTHCARE_OF_THE_MIDWEST_INC,
:IL_MY_HEALTH_ALLIANCE,
:IL_ASSURANT_HEALTH,
# :MD_CAREFIRST_BLUECROSS_BLUESHIELD,
:MD_KAISER_MIDATLANTIC,
# :MI_AETNA,
:MI_ASSURANT_HEALTH,
:MI_BLUE_CROSS_BLUE_SHIELD_OF_MICHIGAN,
:MI_HEALTH_ALLIANCE_PLAN,
:MI_HEALTHPLUS,
:MI_HUMANA,
:MI_PRIORITY_HEALTH,
:MI_UNITEDHEALTHCARE_COMMUNITY_PLAN_INC,
:MI_UNITEDHEALTHCARE_LIFE_INS_CO,
:MI_MOLINA_HEALTH_CARE,
# :MO_BLUE_CROSS_AND_BLUE_SHIELD_OF_KANSAS_CITY,
:MO_COVENTRY,
:MO_HUMANA,
:MO_UNITEDHEALTHCARE_LIFE_INS_CO,
:MO_ALL_SAVERS_INSURANCE_COMPANY,
:MO_ANTHEM_BLUE_CROSS_AND_BLUE_SHIELD,
:MO_CIGNA_HEALTH_AND_LIFE_INSURANCE_COMPANY,
# :ND_BLUE_CROSS_BLUE_SHIELD_OF_NORTH_DAKOTA,
:ND_MEDICA,
# :NE_ASSURANT_HEALTH,
:NE_BLUECROSS_BLUESHIELD_OF_NEBRASKA,
:NE_COVENTRY_HEALTH_CARE_OF_NEBRASKA_INC,
:NE_UNITEDHEALTHCARE_LIFE_INS_CO,
# :NM_BLUE_CROSS_AND_BLUE_SHIELD_OF_NEW_MEXICO,
:NM_MOLINA_HEALTH_CARE,
:NM_NEW_MEXICO_HEALTH_CONNECTIONS,
:NM_PRESBYTERIAN,
# :OK_BLUE_CROSS_AND_BLUE_SHIELD_OF_OKLAHOMA,
:OK_UNITEDHEALTHCARE_LIFE_INS_CO,
:OK_ASSURANT_HEALTH,
:OK_HUMANA,
# :OR_BRIDGESPAN,
:OR_HEALTH_NET_OF_OREGON,
:OR_HEALTH_REPUBLIC_INSURANCE,
:OR_KAISER_FOUNDATION_HEALTH_PLAN_OF_THE_NW,
:OR_LIFEWISE_HEALTH_PLAN_OF_OREGON,
:OR_MODA_HEALTH_PLAN_INC,
:OR_PACIFICSOURCE_HEALTH_PLANS,
:OR_PROVIDENCE_HEALTH_PLAN,
:OR_REGENCE_BLUECROSS_BLUESHIELD_OF_OREGON,
# :TX_BLUE_CROSS_AND_BLUE_SHIELD_OF_TEXAS,
:TX_FIRSTCARE,
:TX_UNITEDHEALTHCARE_LIFE_INS_CO,
:TX_AETNA_LIFE_INSURANCE_COMPANY,
:TX_ALL_SAVERS_INSURANCE_COMPANY,
:TX_CIGNA_HEALTH_AND_LIFE_INSURANCE_COMPANY,
:TX_HUMANA,
:TX_MOLINA_HEALTH_CARE,
:TX_SCOTT__WHITE_HEALTH_PLAN,
:TX_ASSURANT_HEALTH,
:TX_AMBETTER_FROM_SUPERIOR_HEALTHPLAN,
# :UT_ALTIUS_HEALTH_PLANS,
:UT_ARCHES_HEALTH_PLAN,
:UT_BRIDGESPAN,
:UT_HUMANA,
:UT_REGENCE_BLUECROSS_BLUESHIELD_OF_UTAH,
:UT_SELECTHEALTH,
:UT_UNITEDHEALTHCARE_LIFE_INS_CO,
:UT_MOLINA_HEALTH_CARE]


cost_spec = [:AGE,:AV]

# rundate = Dates.today()
rundate = "2022-03-18"
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
include("$codeDir/Demand_Estimation/Log_Likehood_Penalty.jl")

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

filename = "PLL_Estimate_$spec"
# estimate_demand(filename,rundate,
#                     halton_draws,
#                     spec_demoRaw,
#                     spec_prodchars,
#                     spec_prodchars_σ,
#                     spec_fixedEffects)


println("##### Estimation Marginal Cost #####")
println("Load Marginal Cost Estimation Code...")
include("$codeDir/Firm_Side/MC_parameters.jl")
include("$codeDir/Firm_Side/MC_GMM.jl")
include("$codeDir/Firm_Side/MC_var.jl")
include("$codeDir/Firm_Side/MC_derivatives.jl")
include("$codeDir/Firm_Side/MC_optimization.jl")
include("$codeDir/Firm_Side/Firm_Inner_Loop.jl")
include("$codeDir/Firm_Side/SpecRunMC.jl")
# estimate_marginal_cost(rundate,spec,cost_spec)

include("ProcessDemResults.jl")
# process_demand(rundate,spec)


println("##### Solve Equilibrium #####")
println("Load Equilibrium Code...")
include("predictionData.jl")
include("FirmFunctions.jl")
include("SolveModel.jl")
include("EvaluateModel.jl")
include("PriceUpdate.jl")
include("ConsumerWelfare.jl")
include("PlannerProblem.jl")
include("SolveMergers.jl")

MergersMain(rundate,spec,home_directory)


# solve_equilibrium(rundate,spec)
