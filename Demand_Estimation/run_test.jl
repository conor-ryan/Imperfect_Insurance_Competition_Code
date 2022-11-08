using Distributed

using BenchmarkTools
using JLD2
using CSV
using Random
using Dates
using LinearAlgebra
using Statistics
using SharedArrays
using FiniteDiff
using DataFrames
using StatsFuns
using ForwardDiff
using NLopt

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



#
# println("Rebuild Demand Model...")
# file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/PLL_Estimate_FMC-2022-03-18-stg1.jld2"
# @load file p_stg1 spec_Dict


home_directory = "$(homedir())/Documents"
# Load the Data
include("load.jl")
halton_draws = 500
spec_demoRaw = [:AgeFE_31_39,
        :AgeFE_40_51,
        :AgeFE_52_64,
        :Family,
        :LowIncome]
spec_prodchars=[:Price,:AV]
spec_prodchars_σ=[:constant,:AV,
:AK_MODA_HEALTH_PLAN_INC,
:GA_AETNA,:GA_AMBETTER_FROM_PEACH_STATE_HEALTH_PLAN,:GA_ASSURANT_HEALTH,:GA_BLUE_CROSS_BLUE_SHIELD_OF_GEORGIA,:GA_HUMANA,:GA_KAISER_PERMANENTE_GA,:GA_UNITEDHEALTHCARE_LIFE_INS_CO,:GA_UNITEDHEALTHCARE_OF_GEORGIA_INC,
:IA_COVENTRY_HEALTH_CARE_OF_IOWA_INC,:IA_WELLMARK_BLUE_CROSS_AND_BLUE_SHIELD_OF_IOWA,
:MD_CAREFIRST_BLUECROSS_BLUESHIELD,
:MI_BLUE_CROSS_BLUE_SHIELD_OF_MICHIGAN,:MI_HEALTHPLUS,:MI_HEALTH_ALLIANCE_PLAN,:MI_HUMANA,:MI_PRIORITY_HEALTH,:MI_UNITEDHEALTHCARE_COMMUNITY_PLAN_INC,:MI_UNITEDHEALTHCARE_LIFE_INS_CO,
:MO_ALL_SAVERS_INSURANCE_COMPANY,:MO_ANTHEM_BLUE_CROSS_AND_BLUE_SHIELD,:MO_BLUE_CROSS_AND_BLUE_SHIELD_OF_KANSAS_CITY,:MO_COVENTRY,:MO_HUMANA,:MO_UNITEDHEALTHCARE_LIFE_INS_CO,
:ND_BLUE_CROSS_BLUE_SHIELD_OF_NORTH_DAKOTA,
:NE_BLUECROSS_BLUESHIELD_OF_NEBRASKA,:NE_COVENTRY_HEALTH_CARE_OF_NEBRASKA_INC,:NE_UNITEDHEALTHCARE_LIFE_INS_CO,
:NM_BLUE_CROSS_AND_BLUE_SHIELD_OF_NEW_MEXICO,:NM_NEW_MEXICO_HEALTH_CONNECTIONS,:NM_PRESBYTERIAN,
:OK_BLUE_CROSS_AND_BLUE_SHIELD_OF_OKLAHOMA,:OK_UNITEDHEALTHCARE_LIFE_INS_CO,
:OR_KAISER_FOUNDATION_HEALTH_PLAN_OF_THE_NW,:OR_LIFEWISE_HEALTH_PLAN_OF_OREGON,:OR_MODA_HEALTH_PLAN_INC,:OR_PACIFICSOURCE_HEALTH_PLANS,:OR_PROVIDENCE_HEALTH_PLAN,:OR_REGENCE_BLUECROSS_BLUESHIELD_OF_OREGON,
:TX_AETNA_LIFE_INSURANCE_COMPANY,:TX_ALL_SAVERS_INSURANCE_COMPANY,:TX_AMBETTER_FROM_SUPERIOR_HEALTHPLAN,:TX_ASSURANT_HEALTH,:TX_BLUE_CROSS_AND_BLUE_SHIELD_OF_TEXAS,:TX_FIRSTCARE,:TX_HUMANA,:TX_SCOTT__WHITE_HEALTH_PLAN,:TX_UNITEDHEALTHCARE_LIFE_INS_CO,
:UT_ALTIUS_HEALTH_PLANS,:UT_ARCHES_HEALTH_PLAN,:UT_BRIDGESPAN,:UT_HUMANA,:UT_MOLINA_HEALTH_CARE,:UT_REGENCE_BLUECROSS_BLUESHIELD_OF_UTAH,:UT_SELECTHEALTH]


#Structure the data
# c = ChoiceData(df,df_mkt,df_risk,df_transfer;
# demoRaw=spec_Dict["demoRaw"],
# prodchars=spec_Dict["prodchars"],
# prodchars_σ=spec_Dict["prodchars_σ"],
# fixedEffects=spec_Dict["fixedEffects"])
c = ChoiceData(df,df_mkt,df_risk,df_transfer;
demoRaw=spec_demoRaw,
prodchars=spec_prodchars,
prodchars_σ=spec_prodchars_σ,
fixedEffects=[:Firm_ST])

param_labels = vcat(String.(spec_demoRaw),String.(spec_prodchars),"Price:" .* String.(spec_demoRaw),"Variance:".*String.(spec_prodchars_σ),c.feNames)
#
# out1 = DataFrame(labels=param_labels,pars_stg1=p_stg1)#,se=stdErr,ts=t_stat,sig=stars)
# file1 = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/PLL_Estimate_FMC_stg1-2022-03-18.csv"
# CSV.write(file1,out1)





# Fit into model
m = InsuranceLogit(c,500)


W = -Matrix(1.0I,length(m.data.rMoments),length(m.data.rMoments))
W[1,1] = -5.0
W[2,2] = -5.0
W[3,3] = -5.0
W[4,4] = -5.0
W[5,5] = -5.0
W = W.*10

# p_est, fval = newton_raphson_ll(m,p0,W)


p_est =[-0.1255077797990561, -0.2323689791414759, 0.24763065132109494, -0.8789330232286875, -3.6991862459367644, -1.8452984797114897, 8.268043514782724, 0.23155908056687488, 0.37271381340536874, 0.7080686067669316, -0.026755612192740162, 0.11708749060465114, -0.09228180327008906, 0.827029691916879, 0.06180442698497526, 0.336211058625174, 0.35557813061403415, 0.2844939750307887, 0.334174198460619, 0.25621953635858, 0.3336813106326467, 0.22569080865523078, 0.18988465416955927, 10.632049430268253,
 10.676247901007255, 0.016259396157811228, 1.2528586557695864, 1.2436721373175463, 1.1700005884847344, 1.2014475529912367, 1.300197930623565, 1.1967932912161794, 1.1573474902836192, 0.23374266622661735, 0.2305732179557567, 0.23466968083722117, 0.23596584663610345, 0.19557562652472427, 0.11851653350523392, 0.0030214646256773, 0.48404984622845193, 0.470879799527185, 0.3566763800931707, 0.021485853534747672, 0.06510680885284074, 0.0284422228267825, 37.134287627466044, 37.03244095018531,
 -0.07000510662309621, -0.034159092832827566, -0.0432988041384919, -0.0950269836344364, -0.048380568765792706, -0.01512303054169914, 0.010495454002638502, -0.07150558805723371, -0.42994343567979726, -0.011398790955663365, -0.010154403620262938, -0.045350858734510724, -0.054103456611378324, -0.008218894404284558, -0.10404831136893442, -0.023822048382325156, -0.03019053572388684, -0.021896645582275758, -0.09658172152910756, -0.038557762218537656, -0.021552201097978535, -0.06727342661866316,
 -0.8841299960015306, 0.34421444776197296, -3.2035356981018808, -3.639839726737582, -4.2209707550928846, -2.0257960133110737, -1.8375387019993656, -2.4586450590281226, -3.543126509880589, -5.024872628219213, -4.64678080449167, 4.747712019173753, -1.6746039274348314, -0.5353247821566813, -1.5799150120628278, -5.024188850402892, 1.8901467323179646, -1.7979611385954413, -2.556611775795136, -4.115307347765241, -3.8508491301390944, -1.7350154609353974, -3.6528150574655927, -5.8860138660735055,
 -3.404620237296417, -5.788217793004729, -3.654306124782962, -3.366068401293087, -0.2075500758909395, -3.075374697060908, -3.5659140218621443, -2.9315999145406013, -2.225597960937475, -1.7932350893698925, -3.7690771057798873, -4.280857434223523, -2.568965800349616, -2.1513953856081627, -1.0747053399267958, -2.902305125357849, -1.9658576300830466, -3.376328547476633, -4.1164636208673375, -1.2326927413303568, -2.235338601119564, 0.5197248744179288, -0.20023431567069847, -0.5083858876374898,
 -1.9071920780290996, -0.21265715123787335, -2.7992463751393215, -1.78930044448321, -1.31847169211608, -5.029365710364063, -1.2119230067126465, -0.874571599691025, -4.598537053677796, -5.443188837008065, -4.260535433708316, -3.1394920034968306, -2.267274636031151, -2.8463630043809682, -2.70339647459394, -1.953232990581946, -2.426034635053264, -3.213036420227775, -3.9936351698967663, -3.8553412598806838, -3.278398161367888, -4.945986015413857, -1.0247711734190228, -4.1820656931235485,
 -4.878990536603722, -3.2353734129392038, -7.376640434875093, -4.701294320780662, -4.842787922189433, -5.05512486598048, -4.240778747573996, -5.2224840682371605, -4.457041744404288, -6.209348252539522, -3.7124850142960986, -2.29225461794077, -4.549935310599595]

# p = parDict(m,p_est,no2Der=false)
tx_index = findall(p_est.>10)
# p_est = rand(m.parLength[:All]).*0.1 .- 0.5
#
# p_est[[13,14]] = [0.6,-0.03]
# p_est[13] = 10
# p_est[tx_index].=0.0
p = parDict(m,p_est,no2Der=false)
individual_values!(m,p)
individual_shares(m,p)
moms = calc_risk_moments(m,p)
println(moms)

test = []
for (st, st_idx) in d.data._stDict
        test = vcat(test,st_idx)
end


grad = Vector{Float64}(undef,length(p_est))
grad_test = Vector{Float64}(undef,length(p_est))
hess = Matrix{Float64}(undef,length(p_est),length(p_est))

ll = log_likelihood_penalty(m,p_est,W)
println(ll)


ll = log_likelihood_penalty!(hess,grad,m,p_est,W)
println(ll)
ll = log_likelihood_penalty!(grad_test,m,p_est,W)
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
