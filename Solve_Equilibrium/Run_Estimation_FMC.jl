using Distributed
using BenchmarkTools
using JLD2
using CSV
using Random
using Dates
using LinearAlgebra
using Statistics
using DataFrames
using StatsFuns
using ForwardDiff
using NLopt
using FiniteDiff
using SharedArrays
using Primes
using StatsBase

# ## Check OS Environment
if occursin(r"Users",homedir())
        home_directory = "$(homedir())/Dropbox"
else
        home_directory = "$(homedir())/work/"
end

codeDir = "$(home_directory)/Research/Imperfect_Insurance_Competition/Code/"

##### Set Specification ####
halton_draws = 500
spec_demoRaw = [:AgeFE_31_39,
        :AgeFE_40_51,
        :AgeFE_52_64,
        :Family,
        :LowIncome]
# spec_prodchars=[:Price,:AV,:constant]
spec_prodchars=[:Price,:Price_31_39,:Price_40_51,:Price_52_64,:Price_Family,:Price_LowIncome, 
                :AV,:constant,:AgeFE_31_39,:AgeFE_40_51,:AgeFE_52_64,:Family,:LowIncome,
                :CF_res,:CF_res_31_39,:CF_res_40_51,:CF_res_52_64,
                :CF_res_2,:CF_res_2_31_39,:CF_res_2_40_51,:CF_res_2_52_64,
                :CF_res_3,:CF_res_3_31_39,:CF_res_3_40_51,:CF_res_3_52_64]
spec_prodchars_σ=[:AV,:constant,
:AK_MODA_HEALTH_PLAN_INC,#:AK_PREMERA_BLUE_CROSS_BLUE_SHIELD_OF_ALASKA,
:GA_AETNA,:GA_AMBETTER_FROM_PEACH_STATE_HEALTH_PLAN,:GA_ASSURANT_HEALTH,:GA_BLUE_CROSS_BLUE_SHIELD_OF_GEORGIA,:GA_CIGNA_HEALTH_AND_LIFE_INSURANCE_COMPANY,:GA_HUMANA,:GA_KAISER_PERMANENTE_GA,:GA_UNITEDHEALTHCARE_LIFE_INS_CO,#:GA_UNITEDHEALTHCARE_OF_GEORGIA_INC,
:IA_COVENTRY_HEALTH_CARE_OF_IOWA_INC,#:IA_WELLMARK_BLUE_CROSS_AND_BLUE_SHIELD_OF_IOWA,
:IL_AETNA,:IL_AMBETTER_INSURED_BY_CELTIC,:IL_ASSURANT_HEALTH,:IL_BLUE_CROSS_AND_BLUE_SHIELD_OF_ILLINOIS,:IL_COVENTRY,:IL_HUMANA,:IL_LAND_OF_LINCOLN_HEALTH,:IL_MY_HEALTH_ALLIANCE,:IL_UNITEDHEALTHCARE_LIFE_INS_CO,#:IL_UNITEDHEALTHCARE_OF_THE_MIDWEST_INC,
:MD_CAREFIRST_BLUECROSS_BLUESHIELD,#:MD_KAISER_MIDATLANTIC,
:MI_AETNA,:MI_ASSURANT_HEALTH,:MI_BLUE_CROSS_BLUE_SHIELD_OF_MICHIGAN,:MI_HEALTHPLUS,:MI_HEALTH_ALLIANCE_PLAN,:MI_HUMANA,:MI_MOLINA_HEALTH_CARE,:MI_PRIORITY_HEALTH,:MI_UNITEDHEALTHCARE_COMMUNITY_PLAN_INC,#:MI_UNITEDHEALTHCARE_LIFE_INS_CO,
:MO_ALL_SAVERS_INSURANCE_COMPANY,:MO_ANTHEM_BLUE_CROSS_AND_BLUE_SHIELD,:MO_BLUE_CROSS_AND_BLUE_SHIELD_OF_KANSAS_CITY,:MO_CIGNA_HEALTH_AND_LIFE_INSURANCE_COMPANY,:MO_COVENTRY,:MO_HUMANA,#:MO_UNITEDHEALTHCARE_LIFE_INS_CO,
:ND_BLUE_CROSS_BLUE_SHIELD_OF_NORTH_DAKOTA,#:ND_MEDICA,
:NE_ASSURANT_HEALTH,:NE_BLUECROSS_BLUESHIELD_OF_NEBRASKA,:NE_COVENTRY_HEALTH_CARE_OF_NEBRASKA_INC,#:NE_UNITEDHEALTHCARE_LIFE_INS_CO,
:NM_BLUE_CROSS_AND_BLUE_SHIELD_OF_NEW_MEXICO,:NM_MOLINA_HEALTH_CARE,:NM_NEW_MEXICO_HEALTH_CONNECTIONS,#:NM_PRESBYTERIAN,
:OK_ASSURANT_HEALTH,:OK_BLUE_CROSS_AND_BLUE_SHIELD_OF_OKLAHOMA,:OK_HUMANA,#:OK_UNITEDHEALTHCARE_LIFE_INS_CO,
:OR_BRIDGESPAN,:OR_HEALTH_REPUBLIC_INSURANCE,:OR_KAISER_FOUNDATION_HEALTH_PLAN_OF_THE_NW,:OR_LIFEWISE_HEALTH_PLAN_OF_OREGON,:OR_MODA_HEALTH_PLAN_INC,:OR_PACIFICSOURCE_HEALTH_PLANS,:OR_PROVIDENCE_HEALTH_PLAN,#:OR_REGENCE_BLUECROSS_BLUESHIELD_OF_OREGON, :OR_HEALTH_NET_OF_OREGON,
:TX_AETNA_LIFE_INSURANCE_COMPANY,:TX_ALL_SAVERS_INSURANCE_COMPANY,:TX_AMBETTER_FROM_SUPERIOR_HEALTHPLAN,:TX_ASSURANT_HEALTH,:TX_BLUE_CROSS_AND_BLUE_SHIELD_OF_TEXAS,:TX_CIGNA_HEALTH_AND_LIFE_INSURANCE_COMPANY,:TX_FIRSTCARE,:TX_HUMANA,:TX_MOLINA_HEALTH_CARE,:TX_SCOTT__WHITE_HEALTH_PLAN,#:TX_UNITEDHEALTHCARE_LIFE_INS_CO,
:UT_ALTIUS_HEALTH_PLANS,:UT_ARCHES_HEALTH_PLAN,:UT_BRIDGESPAN,:UT_HUMANA,:UT_MOLINA_HEALTH_CARE,:UT_REGENCE_BLUECROSS_BLUESHIELD_OF_UTAH,:UT_SELECTHEALTH]#.:UT_UNITEDHEALTHCARE_LIFE_INS_CO]
cost_spec = [:AGE,:AV_std]

# rundate = Dates.today()
rundate = "2023-11-08"
spec = "FMC"
spec_fixedEffects=[:Market_Firm,:Market_Cat]
println("Running $spec on $rundate")


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
# estimate_demand(filename,rundate,home_directory,
#                     halton_draws,
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
estimate_marginal_cost(rundate,spec,cost_spec,home_directory)

include("ProcessDemResults.jl")
process_demand(rundate,spec,home_directory)
