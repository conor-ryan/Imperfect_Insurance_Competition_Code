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
# include("RandomCoefficients_2der_nonzero.jl")
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
#Structure the data
c = ChoiceData(df,df_mkt,df_risk;
    demoRaw=spec_demoRaw,
    prodchars=spec_prodchars,
    prodchars_σ=spec_prodchars_σ,
    fixedEffects=[:Firm_ST])


param_labels = vcat(String.(spec_demoRaw),String.(spec_prodchars),"Price:" .* String.(spec_demoRaw),"Variance:".*String.(spec_prodchars_σ),c.feNames)

# Fit into model
m = InsuranceLogit(c,5)
println("Data Loaded")


γstart = rand(m.parLength[:γ])/10 .-.05
β0start = rand(m.parLength[:β])/10 .-.05
βstart = rand(m.parLength[:γ])/10 .- .05
σstart = rand(m.parLength[:σ])/10 .- .05

FEstart = rand(m.parLength[:FE])/100 .-.005

p0 = vcat(γstart,β0start,βstart,σstart,FEstart)


W = -Matrix(1.0I,length(m.data.tMoments),length(m.data.tMoments))

# p_est, fval = newton_raphson_ll(m,p0,W)
# out1 = DataFrame(labels=param_labels,pars=p_est)
# file1 = "test.csv"
# CSV.write(file1,out1)




grad = Vector{Float64}(undef,length(p0))
grad_test = Vector{Float64}(undef,length(p0))
hess = Matrix{Float64}(undef,length(p0),length(p0))

ll = log_likelihood(m,p0)
println(ll)


# ll = log_likelihood_penalty!(hess,grad,m,p0,W)
# ll = log_likelihood_penalty!(grad_test,m,p0,W)
println(ll)
ll = log_likelihood_penalty!(grad,m,p0,W)
println(ll)


d = m
p = parDict(d,p0,no2Der=true)
individual_values!(d,p)
individual_shares(d,p)
app = iterate(eachperson(d.data),3171)[1]
grad[:].=0.0
ll, ind = ll_obs_gradient!(grad,app,d,p)

# f_obj(x) = log_likelihood_penalty(m,x,W)
f_obj(x) = ll_obs_test(app,m,x)

grad_1 = Vector{Float64}(undef,length(p0))
hess_1 = Matrix{Float64}(undef,length(p0),length(p0))
fval = f_obj(p0)
println(fval-ll)

println("Grad")
ForwardDiff.gradient!(grad_1,f_obj, p0)#, cfg)
println(maximum(abs.(grad_1-grad)))

println("Hessian")
cfg = ForwardDiff.HessianConfig(f_obj, p0, ForwardDiff.Chunk{8}())
ForwardDiff.hessian!(hess_1,f_obj, p0)#,cfg)
println(maximum(abs.(hess_1-hess)))


f_obj_ll(x) = log_likelihood(m,x)
grad_ll = Vector{Float64}(undef,length(p0))
hess_ll = Matrix{Float64}(undef,length(p0),length(p0))
println(f_obj_ll(p0))
println("Hessian")
cfg = ForwardDiff.HessianConfig(f_obj_ll, p0, ForwardDiff.Chunk{8}())
ForwardDiff.hessian!(hess_ll,f_obj_ll, p0)#,cfg)


f_obj_p(x) = calc_penalty(m,x,W)
grad_pen = Vector{Float64}(undef,length(p0))
hess_pen = Matrix{Float64}(undef,length(p0),length(p0))
println(f_obj_p(p0))
println("Grad")
ForwardDiff.gradient!(grad_pen,f_obj_p, p0)#, cfg)
println("Hessian")
cfg = ForwardDiff.HessianConfig(f_obj_p, p0, ForwardDiff.Chunk{8}())
ForwardDiff.hessian!(hess_pen,f_obj_p, p0)#,cfg)
