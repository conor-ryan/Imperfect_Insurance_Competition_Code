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
include("Log_Likehood_Penalty.jl")
println("Code Loaded")

# Load the Data
include("load.jl")


#### General Specification ####

halton_draws = 50
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

rundate = Dates.today()
println("Running on $rundate")

### Test Specification ####
println("####  ####")
filename = "Test"
spec1 = run_specification_penalizedlikelihood(filename,rundate,
                    df,df_mkt,df_risk,df_transfer,
                    haltonDim = halton_draws,
                    spec_demoRaw=spec_demoRaw,
                    spec_prodchars=spec_prodchars,
                    spec_prodchars_σ=spec_prodchars_σ,
                    spec_fixedEffects=[:Firm_ST])

# ### Run Specification 1 ####
# println("#### Run Specification 1  - Firm Fixed Effects ####")
# filename = "GMM_Estimate_Firm"
# spec1 = run_specification_GMM(filename,rundate,
#                     df,df_mkt,df_risk,
#                     haltonDim = halton_draws,
#                     spec_demoRaw=spec_demoRaw,
#                     spec_prodchars=spec_prodchars,
#                     spec_prodchars_σ=spec_prodchars_σ,
#                     spec_fixedEffects=[:Firm])
#
# # #### Run Specification 1 ####
# println("#### Run Specification 2  - Firm-Market-Category Fixed Effects ####")
# filename = "GMM_Estimate_FMC"
# spec1 = run_specification_GMM(filename,rundate,
#                 df,df_mkt,df_risk,
#                 haltonDim = halton_draws,
#                 spec_demoRaw=spec_demoRaw,
#                 spec_prodchars=spec_prodchars,
#                 spec_prodchars_σ=spec_prodchars_σ,
#                 spec_fixedEffects=[:Firm_Market_Cat])
#
#
#
# # println("#### Run Specification 3  - Firm-Market-Category-Age Fixed Effects ####")
# # filename = "GMM_Estimate_FMCA"
# # spec1 = run_specification_GMM(filename,rundate,
# #                 df,df_mkt,df_risk,
# #                 haltonDim = halton_draws,
# #                 spec_demoRaw=spec_demoRaw,
# #                 spec_prodchars=spec_prodchars,
# #                 spec_prodchars_σ=spec_prodchars_σ,
# #                 spec_fixedEffects=[:Firm_Market_Cat_Age])
