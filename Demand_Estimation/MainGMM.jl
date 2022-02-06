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
spec_prodchars_σ=[:AV, :AETNA_GA,:AETNA_IL,:AETNA_MI,:AETNA_LIFE_INSURANCE_COMPANY_TX,
:ALTIUS_HEALTH_PLANS_UT,:AMBETTER_FROM_PEACH_STATE_HEALTH_PLAN_GA,:AMBETTER_FROM_SUPERIOR_HEALTHPLAN_TX,
:AMBETTER_INSURED_BY_CELTIC_IL                    ,
:ANTHEM_BLUE_CROSS_AND_BLUE_SHIELD_MO             ,
:ARCHES_HEALTH_PLAN_UT                         ,
:ASSURANT_HEALTH_GA,
:ASSURANT_HEALTH_IL,
:ASSURANT_HEALTH_MI,
:ASSURANT_HEALTH_NE,
:ASSURANT_HEALTH_TX ,
:ASSURANT_HEALTH_OK,
:AVERA_HEALTH_PLANS_IA,
:BLUE_CROSS_AND_BLUE_SHIELD_OF_ILLINOIS_IL,
:BLUE_CROSS_AND_BLUE_SHIELD_OF_KANSAS_CITY_MO,
:BLUE_CROSS_AND_BLUE_SHIELD_OF_NEW_MEXICO_NM,
:BLUE_CROSS_AND_BLUE_SHIELD_OF_OKLAHOMA_OK,
:BLUE_CROSS_BLUE_SHIELD_OF_GEORGIA_GA,
:BLUE_CROSS_AND_BLUE_SHIELD_OF_TEXAS_TX         ,
:BLUE_CROSS_BLUE_SHIELD_OF_MICHIGAN_MI,
:BLUE_CROSS_BLUE_SHIELD_OF_NORTH_DAKOTA_ND,
:BLUECROSS_BLUESHIELD_OF_NEBRASKA_NE,
:BRIDGESPAN_OR,
:BRIDGESPAN_UT ,
:CAREFIRST_BLUECROSS_BLUESHIELD_MD,
:CIGNA_HEALTH_AND_LIFE_INSURANCE_COMPANY_GA,
:CIGNA_HEALTH_AND_LIFE_INSURANCE_COMPANY_MO ,
:CIGNA_HEALTH_AND_LIFE_INSURANCE_COMPANY_TX,
:COVENTRY_IL,
:COVENTRY_MO ,
:COVENTRY_HEALTH_CARE_OF_IOWA_INC_IA,
:COVENTRY_HEALTH_CARE_OF_NEBRASKA_INC_NE          ,
:FIRSTCARE_TX                                   ,
:HEALTH_ALLIANCE_PLAN_MI,
:HEALTH_NET_OF_OREGON_OR,
:HEALTH_REPUBLIC_INSURANCE_OR                   ,
:HEALTHPLUS_MI                                   ,
:HUMANA_GA                                        ,
:HUMANA_IL                                      ,
:HUMANA_MI                                       ,
:HUMANA_MO                                        ,
:HUMANA_OK                                      ,
:HUMANA_TX                                       ,
:HUMANA_UT                                        ,
:KAISER_FOUNDATION_HEALTH_PLAN_OF_THE_NW_OR     ,
:KAISER_MIDATLANTIC_MD                           ,
:KAISER_PERMANENTE_GA_GA                          ,
:LAND_OF_LINCOLN_HEALTH_IL                      ,
:LIFEWISE_HEALTH_PLAN_OF_OREGON_OR               ,
:MEDICA_ND                                        ,
:MODA_HEALTH_PLAN_INC_AK                        ,
:MODA_HEALTH_PLAN_INC_OR                         ,
:MOLINA_HEALTH_CARE_MI                            ,
:MOLINA_HEALTH_CARE_NM                          ,
:MOLINA_HEALTH_CARE_TX                           ,
:MOLINA_HEALTH_CARE_UT                            ,
:MY_HEALTH_ALLIANCE_IL                          ,
:NEW_MEXICO_HEALTH_CONNECTIONS_NM                ,
:PACIFICSOURCE_HEALTH_PLANS_OR                    ,
:PREMERA_BLUE_CROSS_BLUE_SHIELD_OF_ALASKA_AK    ,
:PRESBYTERIAN_NM                                 ,
:PRIORITY_HEALTH_MI                               ,
:PROVIDENCE_HEALTH_PLAN_OR                      ,
:REGENCE_BLUECROSS_BLUESHIELD_OF_OREGON_OR       ,
:REGENCE_BLUECROSS_BLUESHIELD_OF_UTAH_UT          ,
:SCOTT__WHITE_HEALTH_PLAN_TX                    ,
:SELECTHEALTH_UT                                 ,
:UNITEDHEALTHCARE_COMMUNITY_PLAN_INC_MI           ,
:UNITEDHEALTHCARE_LIFE_INS_CO_GA                ,
:UNITEDHEALTHCARE_LIFE_INS_CO_IL                 ,
:UNITEDHEALTHCARE_LIFE_INS_CO_MI                  ,
:UNITEDHEALTHCARE_LIFE_INS_CO_MO                ,
:UNITEDHEALTHCARE_LIFE_INS_CO_NE                 ,
:UNITEDHEALTHCARE_LIFE_INS_CO_OK                  ,
:UNITEDHEALTHCARE_LIFE_INS_CO_TX                ,
:UNITEDHEALTHCARE_LIFE_INS_CO_UT                 ,
:UNITEDHEALTHCARE_OF_GEORGIA_INC_GA               ,
:UNITEDHEALTHCARE_OF_THE_MIDWEST_INC_IL         ,
:WELLMARK_BLUE_CROSS_AND_BLUE_SHIELD_OF_IOWA_IA ]

rundate = Dates.today()
println("Running on $rundate")

### Test Specification ####
println("####  ####")
filename = "Test"
spec1 = run_specification_penalizedlikelihood(filename,rundate,
                    df,df_mkt,df_risk,
                    haltonDim = halton_draws,
                    spec_demoRaw=spec_demoRaw,
                    spec_prodchars=spec_prodchars,
                    spec_prodchars_σ=spec_prodchars_σ,
                    spec_fixedEffects=[:Firm_ST])

### Run Specification 1 ####
println("#### Run Specification 1  - Firm Fixed Effects ####")
filename = "GMM_Estimate_Firm"
spec1 = run_specification_GMM(filename,rundate,
                    df,df_mkt,df_risk,
                    haltonDim = halton_draws,
                    spec_demoRaw=spec_demoRaw,
                    spec_prodchars=spec_prodchars,
                    spec_prodchars_σ=spec_prodchars_σ,
                    spec_fixedEffects=[:Firm])

# #### Run Specification 1 ####
println("#### Run Specification 2  - Firm-Market-Category Fixed Effects ####")
filename = "GMM_Estimate_FMC"
spec1 = run_specification_GMM(filename,rundate,
                df,df_mkt,df_risk,
                haltonDim = halton_draws,
                spec_demoRaw=spec_demoRaw,
                spec_prodchars=spec_prodchars,
                spec_prodchars_σ=spec_prodchars_σ,
                spec_fixedEffects=[:Firm_Market_Cat])



# println("#### Run Specification 3  - Firm-Market-Category-Age Fixed Effects ####")
# filename = "GMM_Estimate_FMCA"
# spec1 = run_specification_GMM(filename,rundate,
#                 df,df_mkt,df_risk,
#                 haltonDim = halton_draws,
#                 spec_demoRaw=spec_demoRaw,
#                 spec_prodchars=spec_prodchars,
#                 spec_prodchars_σ=spec_prodchars_σ,
#                 spec_fixedEffects=[:Firm_Market_Cat_Age])
