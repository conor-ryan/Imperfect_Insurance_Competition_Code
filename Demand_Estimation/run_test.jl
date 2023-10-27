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


home_directory = "$(homedir())/Dropbox"
# Load the Data
include("load.jl")
halton_draws = 500
spec_demoRaw = [:AgeFE_31_39,
        :AgeFE_40_51,
        :AgeFE_52_64,
        :Family,
        :LowIncome]
spec_prodchars=[:Price,:AV]
spec_prodchars_σ=[:AV,
:AK_MODA_HEALTH_PLAN_INC,:AK_PREMERA_BLUE_CROSS_BLUE_SHIELD_OF_ALASKA,
:GA_AETNA,:GA_AMBETTER_FROM_PEACH_STATE_HEALTH_PLAN,:GA_ASSURANT_HEALTH,:GA_BLUE_CROSS_BLUE_SHIELD_OF_GEORGIA,:GA_CIGNA_HEALTH_AND_LIFE_INSURANCE_COMPANY,:GA_HUMANA,:GA_KAISER_PERMANENTE_GA,:GA_UNITEDHEALTHCARE_LIFE_INS_CO,:GA_UNITEDHEALTHCARE_OF_GEORGIA_INC,
:IA_COVENTRY_HEALTH_CARE_OF_IOWA_INC,:IA_WELLMARK_BLUE_CROSS_AND_BLUE_SHIELD_OF_IOWA]

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


p_est =[-0.21137510167988036, -0.32501298923514255, -0.07888167627153007, -0.8821767856736046, -3.57764792936605, -1.5121413749739168, 4.789464982172973, 0.292640652607038, 0.4589443360776047, 0.7911236976045463, -0.008721931740667448, 0.18220713468571356, 0.5387951256982593, -0.06759124777019626, -0.10622973926928896, -0.2444918408963374, -0.22885408361073423, -0.31202274101574223, -0.2621966484879078, -0.2681334613521046, -0.3161015724362939, -0.2550620972167974, -0.32416734400476227,
-0.37103979510675344, 37.795909551875084, 37.811886287149335, 0.3680529923023766, 1.2911565770125526, -1.8326409386052043, -2.396421316616979, -3.7051370219764648, -0.9750984286544947, -2.8236669508849794, -1.2060533852938888, -2.6036010940064536, -3.8260563904153027, -3.179231319717235, 0.6860144672435651, -0.0325211863680741, -4.058362313337969, -3.3074092239676878, 0.03869036294574895, -0.0608790498030508, -1.3844820724690838, -3.2328519416680916, -1.9694309143064705, -0.6307082247701036,
-3.2815816804257554, -3.992718606253883, -1.2244377997766742, -3.3876994430970644, -3.0610073317015902, -2.828014642019718, 0.7555635414602871, -1.8013268125046211, -2.007624090248878, -1.7500306394195084, -3.447528017298003, -0.6053283626289123, -2.396641094445314, -2.4670826321158175, -1.0383654917188556, -0.9137620490031799, 0.06354281717417723, -1.8087382810304715, -0.5208413835987841, -2.1290341458419424, -2.0726895334387105, 0.3924265843045748, -0.46241548828556545, -0.7502607710698992,
 0.4901626224103025, 0.9180428563161934, -0.8524757302871954, 0.3247952292034852, -1.7629528381115669, -0.35822122310200954, -0.782109290584449, -3.5879262076957765, 0.044593901575053815, -4.4339344289687075, -3.1543484131279493, -4.5772043691194435, -3.1267216267708235, -4.14858880986651, -1.5848459655041471, -0.5323538282030792, -0.8144447403613255, -2.626637233397961, -0.567684748343466, -2.008502624917879, -2.759624994924222, -2.4637563978284374, -3.1969536679123904, -4.284620216159756,
 -0.01766709954376433, -3.139277556145024, -3.5411687721224556, -2.135003149117337, -5.6316432939703, -3.188108774148905, -3.516521880542274, -2.373997815456707, -1.5089124107756533, -2.4776965221231557, -1.918898863461526, -3.2603889959747754, -1.641207394775812, -0.5270690650396431, -2.7458079234692088]
# p = parDict(m,p_est,no2Der=false)
tx_index = findall(p_est.>10)
# p_est = rand(m.parLength[:All]).*0.1 .- 0.5
#
# p_est[[13,14]] = [0.6,-0.03]
# p_est[13] = 10
p_est[tx_index]=p_est[tx_index]./10
p = parDict(m,p_est,no2Der=false)
individual_values!(m,p)
individual_shares(m,p)
println("########")
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
