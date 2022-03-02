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
m = InsuranceLogit(c,10)

sample = bootstrapSample(c)
m_sample = InsuranceLogit(sample,100)
println("Data Loaded")


γstart = rand(m.parLength[:γ])/10 .-.05
β0start = rand(m.parLength[:β])/10 .-.05
βstart = rand(m.parLength[:γ])/10 .- .05
σstart = rand(m.parLength[:σ])/10 .- .05

FEstart = rand(m.parLength[:FE])/100 .-.005

p0 = vcat(γstart,β0start,βstart,σstart,FEstart)
p0[14:15] = [0.0,1.0]

W = -Matrix(1.0I,length(m.data.rMoments),length(m.data.rMoments))./10
W[1,1] = -5.0
W[2,2] = -5.0
W[3,3] = -5.0
W[4,4] = -5.0
W[5,5] = -5.0

# p_est, fval = newton_raphson_ll(m,p0,W)


p_est = [-0.12087037193457834, -0.2859315484370647, -0.15464420933104453, -0.9125794817429929, -3.7864540751083475, -1.8678607315395601, 0.04864749140611499, 6.449986185602322, 0.2571903095437419, 0.4831788245621049, 0.9416715634221721, -0.0055747293354561295, 0.23730236812228306, 0.023495500169090584, 0.7883882073273308, -0.04210686916556791, -0.011927011692508021, -0.11365894049351451, -0.15510333207811716, -0.07024747232304158, -0.01868143410786222, -0.08175209176532117,
-0.005615947713735438, 0.019779529495094216, 0.023781885839797134, -518.4893001227666, -0.0013971757695659599, -0.11290509991956217, -0.05559187258562644, -0.08310248711874295, -0.08963150496441102, -0.11706169529830109, -0.09480343378366195, -0.0814973897576135, -0.14587286275031341, -0.00015618109298013284, -0.1175278820098078, -0.11008360188681056, -0.17865393418834222, -0.07868489017475915, -0.1341529643871978, -0.05834972807185646, -0.13864513031196346, -0.18712557544623018,
 -0.13171236056593524, 0.016066082012065202, -0.021982839923647135, -0.0816120087756141, 0.019278212459926974, 0.005057816885675334, -0.0035759597568561906, 0.013284104209921762, -0.10179621898970363, -0.10339342386447971, -0.20585833283905272, 0.023000765016547495, 0.05658665025029507, 0.0010204343187465875, -0.10222514046442795, 0.045325503049851244, -0.00566745363978069, -70.30938605212398, -0.10161923226783891, -0.09874826065314994, -0.044342880401305, -0.05715976850428066,
 -0.1319216136477661, -0.06432709495556255, -0.046531097953203274, -0.0201777799178037, -0.08234297219341712, 0.031990201770361666, -0.03430859426677291, -0.06259534668067336, -0.027111818983270423, -0.016569609160310896, 0.01896385848906828, -0.00044645604766863707, -0.3257263846417471, -0.005528668534005947, 0.00227408646957924, -0.07795414536429845, -0.007921992591699654, -0.06477165052666359, -0.12608542009289525, -0.015105459233522299, 1.6313686171174135, -2.864695517719415,
 -3.3942813865052206, -3.379948537282905, -1.5976814518621778, -3.001168574791616, -1.6755101787801208, -3.0956808875272004, -4.128964046065889, -3.4707642254827573, 5.613487904982874, -1.9757295604454845, -2.372220914628171, -5.309412365637976, -5.883670366253426, 1.2513949856798892, -1.2083678650585734, -2.705965485531149, -3.8340515812987412, -3.440835749599644, -1.67410253275775, -3.7014810591346365, -5.412110586543294, -1.7220918405401724, -4.041024692268246, -3.879622803711256,
 -2.9258406095797533, 0.4342573320173246, -2.526956688591098, -2.6707038679428745, -2.328281708266794, -3.82891931098764, -1.365443861610989, -3.101975416653687, -3.3222170306679697, -1.7395647082150616, -1.2256148856773112, -0.06768944246495487, -2.040347591316351, -1.0586653389651204, -2.474903329845662, -2.8493360669114796, 1.0557418446290985, -0.24683945633260423, 1.8871595292595218, 0.10358523384988019, -0.035396173815625036, -1.342246607477718, -0.2587027762612584, -2.91302229732365,
 -1.8370293743519595, -1.3324681050961453, -3.715550086338394, -0.34862663365189983, -4.1854905258025035, -3.5464328560566223, -5.202960864571008, -3.6106335037304116, -2.4917043737904985, -1.228817657283356, -1.1995524851284747, -1.16443165928802, -1.5309768860431496, -0.8980587343841236, -1.8833839743722327, -3.2751811909718995, -2.9012533763088504, -3.118426621319595, -4.428797833432115, -0.3435849189584427, -3.270465173997951, -4.0512179956862076, -2.46388741547597, -6.0993722088688935,
 -3.8236839530297155, -3.9705504972401937, -2.5525965428116058, -1.6416271158539235, -2.649563472228927, -2.3427172328234205, -3.3411224093759335, -1.5087536196180449, -0.3214796633230698, -3.2829426322432442]

mom, V = risk_moment_bootstrap(m,p_est)

# p0 = copy(p_est)

grad = Vector{Float64}(undef,length(p0))
grad_test = Vector{Float64}(undef,length(p0))
hess = Matrix{Float64}(undef,length(p0),length(p0))

ll = log_likelihood_penalty(m,p0,W)
println(ll)


ll = log_likelihood_penalty!(hess,grad,m,p_est,W)
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



@benchmark calc_risk_moments(m_sample,p_est)

using Profile
Profile.init()
Profile.clear()
Juno.@profile sample = calc_risk_moments(m_sample,p_est)
Juno.profiler()



d = m
p = parDict(m_sample,p_est,no2Der=false)

grad = Vector{Float64}(undef,length(p0))
hess = Matrix{Float64}(undef,length(p0),length(p0))
ll = log_likelihood!(hess,grad,m,p)

mom_grad = Matrix{Float64}(undef,length(p0),length(d.data.rMoments))
mom_hess = Array{Float64,3}(undef,length(p0),length(p0),length(d.data.rMoments))

grad = Vector{Float64}(undef,length(p0))
hess = Matrix{Float64}(undef,length(p0),length(p0))

# mom = calc_risk_moments!(mom_grad,d,p)
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
