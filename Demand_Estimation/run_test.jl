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
:GA_AETNA,
:GA_AMBETTER_FROM_PEACH_STATE_HEALTH_PLAN,
:GA_ASSURANT_HEALTH,
:GA_BLUE_CROSS_BLUE_SHIELD_OF_GEORGIA,
:GA_HUMANA,
:GA_KAISER_PERMANENTE_GA,
:GA_UNITEDHEALTHCARE_LIFE_INS_CO,
:GA_UNITEDHEALTHCARE_OF_GEORGIA_INC,
:IA_COVENTRY_HEALTH_CARE_OF_IOWA_INC,
:IA_WELLMARK_BLUE_CROSS_AND_BLUE_SHIELD_OF_IOWA,
# :IL_BLUE_CROSS_AND_BLUE_SHIELD_OF_ILLINOIS,
# :IL_COVENTRY,
# :IL_HUMANA,
# :IL_LAND_OF_LINCOLN_HEALTH,
# :IL_MY_HEALTH_ALLIANCE,
# :IL_UNITEDHEALTHCARE_LIFE_INS_CO,
# :IL_UNITEDHEALTHCARE_OF_THE_MIDWEST_INC,
# :MI_BLUE_CROSS_BLUE_SHIELD_OF_MICHIGAN,
:MI_HEALTHPLUS,
:MD_CAREFIRST_BLUECROSS_BLUESHIELD,
:MI_HEALTH_ALLIANCE_PLAN,
:MI_HUMANA,
:MI_PRIORITY_HEALTH,
:MI_UNITEDHEALTHCARE_COMMUNITY_PLAN_INC,
:MI_UNITEDHEALTHCARE_LIFE_INS_CO,
:MO_ALL_SAVERS_INSURANCE_COMPANY,
:MO_ANTHEM_BLUE_CROSS_AND_BLUE_SHIELD,
:MO_BLUE_CROSS_AND_BLUE_SHIELD_OF_KANSAS_CITY,
:MO_COVENTRY,
:MO_HUMANA,
:MO_UNITEDHEALTHCARE_LIFE_INS_CO,
:ND_BLUE_CROSS_BLUE_SHIELD_OF_NORTH_DAKOTA,
:NE_BLUECROSS_BLUESHIELD_OF_NEBRASKA,
:NE_COVENTRY_HEALTH_CARE_OF_NEBRASKA_INC,
:NE_UNITEDHEALTHCARE_LIFE_INS_CO,
:NM_BLUE_CROSS_AND_BLUE_SHIELD_OF_NEW_MEXICO,
:NM_NEW_MEXICO_HEALTH_CONNECTIONS,
:NM_PRESBYTERIAN,
:OK_BLUE_CROSS_AND_BLUE_SHIELD_OF_OKLAHOMA,
:OK_UNITEDHEALTHCARE_LIFE_INS_CO,
:OR_KAISER_FOUNDATION_HEALTH_PLAN_OF_THE_NW,
:OR_LIFEWISE_HEALTH_PLAN_OF_OREGON,
:OR_MODA_HEALTH_PLAN_INC,
:OR_PACIFICSOURCE_HEALTH_PLANS,
:OR_PROVIDENCE_HEALTH_PLAN,
:OR_REGENCE_BLUECROSS_BLUESHIELD_OF_OREGON,
:TX_AETNA_LIFE_INSURANCE_COMPANY,
:TX_ALL_SAVERS_INSURANCE_COMPANY,
:TX_AMBETTER_FROM_SUPERIOR_HEALTHPLAN,
:TX_ASSURANT_HEALTH,
:TX_BLUE_CROSS_AND_BLUE_SHIELD_OF_TEXAS,
:TX_FIRSTCARE,
:TX_SCOTT__WHITE_HEALTH_PLAN,
:TX_UNITEDHEALTHCARE_LIFE_INS_CO,
:TX_HUMANA,
:UT_ALTIUS_HEALTH_PLANS,
:UT_ARCHES_HEALTH_PLAN,
:UT_BRIDGESPAN,
:UT_HUMANA,
:UT_MOLINA_HEALTH_CARE,
:UT_REGENCE_BLUECROSS_BLUESHIELD_OF_UTAH,
:UT_SELECTHEALTH]

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


p_est = [-0.0022005571027650765, -0.1790336016606025, -0.027825365568647616, -0.7135839638314401, -4.096425949683802, -1.7843675181884182, 7.288419078466846, 0.163692618383599, 0.2312909952552012, 0.5841787298009176, -0.05723983600056303, 0.4173913111623381, 1.9983785967371437, 0.954747417732374, 0.03216787097038604, 0.26057703047489567, 0.27946627809907504, 0.19105184326281538, 0.25420868758987486, 0.16387266573013365, 0.2552026478364803, 0.12714657330980897, 0.07669638970889688,
 0.22268720563996286, 0.26622593482349993, 0.015831493124806115, 0.23694613322678207, -0.08455942275668296, -0.04789688518151366, 0.05172968324572426, -0.05187977234246166, -0.09796097308151709, -0.07502060897042963, -0.08849051892053392, -0.09817179641170068, -0.08112926038314047, -0.14158551126997188, -0.22354888411869175, -0.015089597109786985, -0.36000800887263235, -0.3655423044966407, -0.5049736023882574, -0.08001453779784964, -0.010153831032152645, -0.0728147427275564,
 -0.12760383423085525, -0.24044997038336766, 0.01339177452477284, 0.05704435210027259, 0.04711324333221644, -0.01966852585112983, 0.040998827793459154, 0.06466446183573048, 11.459492531646614, 11.364995054909711, 10.986478748358007, 11.432865011198528, 11.438802796581644, 11.40792112424, 11.44457638626922, 11.32323359266981, 11.385059339501199, 0.150209971270163, 0.145036274211265, 0.15264231748597393, 0.06267331370662711, 0.13756015691738058, 0.14720272486068023, 0.10419909549709662,
 -0.05407428022140919, 1.1071583422966451, -2.42630531684522, -2.9403370359507077, -3.7424283767529585, -1.451052736019602, -2.4076584594587866, -1.877908212915832, -3.0413713520645898, -4.500801032693441, -4.060496571540966, 0.16272505613276106, -0.04728037697955808, 1.5281938057315951, -4.524340433744038, -4.802177676172607, -0.3636762294542256, -1.7452715036975706, -2.950347900720552, -4.42752051125408, -3.728891164628932, -2.3034298043305794, -3.5883929039472755, -5.654355467785297,
 -2.8048208949385045, -4.458604578638267, -3.3066660925156612, -2.5106359671736005, 0.48010250959651873, -2.22813774845748, -2.727283537651285, -2.3160998961094186, -3.3128698891804627, -1.2203160654793843, -2.9731957232970805, -3.2690565742082476, -1.431420258774018, -1.1064052592759535, -0.1930817724168079, -2.517592325593988, -0.6651312410546463, -2.499507219057863, -3.090176921173159, 0.04939113879729754, -1.331050420111277, -0.9402874992972177, 0.9390597529368048, 1.5472044369332305,
 -1.2625291014736284, -0.007095912788314829, -0.6877927953612213, -0.8041288458950494, -0.9950471953698616, -4.188644641316774, -0.5527921714957373, -4.183082553499457, -3.7675433424892386, -4.503627428989627, -3.682535498720082, -3.4293860296039185, -1.7073888692187706, -1.8250236845219214, -1.7680702667139256, -2.7430846374493374, -1.3684893056690004, -2.367740640834514, -3.096678454783749, -2.9924096728933414, -3.030436021177844, -4.225334205885481, -0.16703804525281155, -2.626779378562597,
 -3.9467683839391126, -2.4414695704968232, -5.133005410877387, -3.6312919661738525, -4.258380872108515, -3.9829323465233983, -2.8591012323165375, -4.108601101913506, -3.2640172332281985, -4.916916481785186, -2.6958678561596963, -1.333486229758428, -3.240282471741261]
# p = parDict(m,p_est,no2Der=false)
tx_index = findall(p_est.>10)
# p_est = rand(m.parLength[:All]).*0.1 .- 0.5
#
# p_est[[13,14]] = [0.6,-0.03]
p_est[13] = 7
p_est[tx_index].=0.0
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
