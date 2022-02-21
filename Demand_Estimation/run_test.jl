println(p_est)
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
#Structure the data
c = ChoiceData(df,df_mkt,df_risk,df_transfer;
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


p_insert = [-0.16153771486428595, -0.28182331291391044, -0.11858018372946014, -0.8170267902302796, -3.8722343175380476, -1.5398921437542288, -0.6057290873580762, 6.456950984835993, 0.17119682683133233, 0.2734357641938088, 0.6846844012571253, -0.05620193071362155, -0.10685589729462884, 38.42781520392144, 1.2473076294254815]
p0[1:15] = p_insert[:]

W = -Matrix(1.0I,length(m.data.rMoments),length(m.data.rMoments))

p_est, fval = newton_raphson_ll(m,p0,W)

# p0 = copy(p_est)

grad = Vector{Float64}(undef,length(p0))
grad_test = Vector{Float64}(undef,length(p0))
hess = Matrix{Float64}(undef,length(p0),length(p0))

ll = log_likelihood_penalty(m,p0,W)
println(ll)


ll = log_likelihood_penalty!(hess,grad,m,p0,W)
println(ll)
ll = log_likelihood_penalty!(grad_test,m,p0,W)
println(ll)


f_obj(x) = log_likelihood_penalty(m,x,W)


grad_1 = Vector{Float64}(undef,length(p0))
hess_1 = Matrix{Float64}(undef,length(p0),length(p0))
fval = f_obj(p0)
println(fval-ll)

println("Grad")
ForwardDiff.gradient!(grad_1,f_obj, p0)#, cfg)
println(maximum(abs.(grad_1-grad)))
#
println("Hessian")
cfg = ForwardDiff.HessianConfig(f_obj, p0, ForwardDiff.Chunk{8}())
ForwardDiff.hessian!(hess_1,f_obj, p0)#,cfg)
println(maximum(abs.(hess_1-hess)))



d = m
p = parDict(d,p_est,no2Der=false)

grad = Vector{Float64}(undef,length(p0))
hess = Matrix{Float64}(undef,length(p0),length(p0))
ll = log_likelihood!(hess,grad,m,p)

mom_grad = Matrix{Float64}(undef,length(p0),length(d.data.rMoments))
mom_hess = Array{Float64,3}(undef,length(p0),length(p0),length(d.data.rMoments))

grad = Vector{Float64}(undef,length(p0))
hess = Matrix{Float64}(undef,length(p0),length(p0))

mom = calc_risk_moments!(mom_grad,d,p)
mom = calc_risk_moments!(mom_hess,mom_grad,d,p)

obj = calc_GMM_Obj(mom,W)

grad[:].=0.0
hess[:].=0.0
calc_GMM_Grad!(grad,mom,mom_grad,W)
calc_GMM_Hess!(hess,mom,mom_grad,mom_hess,W)

# app = iterate(eachperson(d.data),11)[1]
# grad[:].=0.0
# hess[:].=0.0
# ll, ind = ll_obs_hessian!(hess,grad,app,d,p)
#
#
#
# f_obj_ll(x) = log_likelihood(m,x)
# grad_ll = Vector{Float64}(undef,length(p0))
# hess_ll = Matrix{Float64}(undef,length(p0),length(p0))
# println(f_obj_ll(p0))
# println("Hessian")
# cfg = ForwardDiff.HessianConfig(f_obj_ll, p0, ForwardDiff.Chunk{8}())
# ForwardDiff.hessian!(hess_ll,f_obj_ll, p0)#,cfg)


f_obj_p(x) = calc_penalty(m,x,W)
grad_pen = Vector{Float64}(undef,length(p0))
hess_pen = Matrix{Float64}(undef,length(p0),length(p0))
println(f_obj_p(p0))
println("Grad")
ForwardDiff.gradient!(grad_pen,f_obj_p, p0)#, cfg)



println("Hessian")
cfg = ForwardDiff.HessianConfig(f_obj_p, p0, ForwardDiff.Chunk{8}())
ForwardDiff.hessian!(hess_pen,f_obj_p, p0)#,cfg)
