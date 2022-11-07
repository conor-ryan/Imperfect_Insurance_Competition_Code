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


p_est = [-0.1708975955783925, -0.4654625051471914, -0.5166233429367835, -1.0061889748916564, -4.225702059206194, -2.123076011363915, 8.013961923464983, 0.27819714565306236, 0.5805296980595994, 1.1549328363734264, -0.0048354237037540335, 0.29703595507444513, 0.04775843869729644, 0.6159193402641485, 0.08709775928292164, 38.09571289299118, 38.11132008400057, 38.016267044917186,
38.08231207249777, 38.02038714977805, 38.079579358377906, 37.955716353517104, 37.93528541088972, 11.781679611732333, 11.800210759153513, 0.0065819994228914465, 15.306581074298112, 15.323420359164407, 15.241843432075848, 15.286468503522707, 15.351282468975572, 15.282054605159686, 15.23198997680591, 8.102562559654366, 8.087868318888775, 8.089325173393162, 8.102985569672212, 8.066146607756632,
7.998994944470808, -0.004742740364330119, 13.267446401902566, 13.270478374203492, 13.153213449714752, 0.07037231307321662, 0.1260028490644459, 0.06965655027607232, -1.1524551483220067, -3.553700044401992, -1.2598885131509938, -0.788477827144668, -0.8564435484007706, -1.950694203531583, -0.870143188590254, -0.8933410688026395, 0.10793602114979622, 0.04817357803015928, -0.14463429822292523,
0.07138658956051165, 0.0833766355207659, 0.06963587254977364, 0.05598493701761859, 0.10032853920744766, 0.0007528264640411207, -1.2940658496353543, -1.2437613609423819, -1.276844255700175, -1.5523087350946234, -1.334860961717502, -1.4864161628819592, -2.4503185088907475, -0.15130644679481386, 1.0350082463785595, -3.1018562758789994, -3.6080188077215976, -4.4242030970548845, -2.1044983408127576,
-1.9761979075103668, -2.4514261931841066, -3.6006445229735, -4.787712591372114, -4.361438948758952, 4.884148351835865, -1.7696775319054672, -0.9720850654439199, -4.435389870713342, -4.479775290626782, 0.06162565053085704, -1.2881235633789814, -2.3980535087563597, -4.00149588311021, -3.224450171616672, -1.7171113813662922, -3.769593464402056, -5.207613256557861, -2.14077135697933,
-4.502443131988662, -3.181933538622382, -3.0740808803287263, -0.2938859116420454, -2.8505325494833795, -3.1606062245036433, -3.1008555108322495, -4.078301915798125, -1.8932316326053173, -3.7597142958092387,-3.882322660094752, -2.454289882790262, -2.1887204565937144, -1.1858717967264945, -3.081003789104929, -1.9235980653469298, -3.5070918778198186, -3.73280714301194, 0.1316430210681585,
 -0.8128503452073906, 0.3812362215786322, -0.7333412166052082, -0.6775501992584996, -2.285912137947989, -1.1574496275601485, -3.43409683645246, -2.318002047902352, -2.259826705158452, -11.681423525068208, -0.18332063730519702, -16.449822393932692, -3.3497490085160746, -14.954183487133106, -16.21770828978569, -15.389208601843043, -0.9758356844809415, -1.395700448548415, -1.2100951950483363,
 -0.9691576467450744,-1.0336191776581711, -1.7832777582388109, -3.5968066509063337, -3.5516274733440936, -3.3532055749766165, -4.729059558936022, -0.7071625090089438, -3.4809066107499005, -4.519064681307985, -3.0091171963287007, -6.471312813774486, -4.252352587263855, -4.289272784463722, -2.502027620491811, -1.6814367652737616, -2.4939254955743295, -2.3075807538530095, -3.0766034658577155,
 -1.3513762583342006, 0.398188867078369, -12.301065849041299]

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
