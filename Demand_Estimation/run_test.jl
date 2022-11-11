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


p_est =[-0.203147038997239, -0.3148338945485697, -0.0402843195707744, -0.9108282664855358, -3.548845159026222, -1.4761762203214788, 4.8208598857417275, 0.26646309600635276, 0.42577231745633726, 0.7388022663705692, 0.0051242140181436005, 0.16265112028303313, 0.550681083707923, -0.06187189942069825, -0.09740431429823458, -0.25226553686118414, -0.23632026310721338, -0.3167930020957156, -0.2690446526843062, -0.2743014538683707, -0.323872671772887, -0.26154823678512845, -0.3293017781531471,
-0.3768343348298264, 5.969838181054081, 35.20091727444314, 35.218635082694284, 0.33067001271124097, 1.2684998094816164, -1.8728850901799032, -2.434806589865307, -3.736834803451909, -1.0126191154950572, -2.85779181761712, -1.2437567388633128, -2.639521196473884, -3.8568595807095103, -3.21255877570167, 0.6160985234795678, -0.06540094898860796, -0.8142784186058849, -4.082713413603031, -3.3419410541236005, 0.056416999572031654, -0.09118452868134444, -1.415854834060434, -3.2575424323137545,
-2.00141554279498, -0.6634902205448985, -3.307779753902469, -4.021131512755587, -1.2560197557139663, -3.421621683059842, -3.096785574213396, -2.856376678588723, 0.7210012707517968, -1.8373050682932657, -2.0412697219028892, -1.7888171366779249, -3.4878333968001614, -0.6412953107199276, -2.4343070881067743, -2.5017406252735634, -1.0846599180994807, -0.9547322842770959, 0.026811023641847044, -1.8521324479615024, -0.5645977839699142, -2.170466144392621, -2.1145769822401874, 0.3455896644377465,
-0.5127522491114334, -0.7858540711240545, 0.45783249889507965, 0.8749974736269409, -0.8894162501180202, 0.2953976165894996, -1.8005941206917415, -0.3975796561019738, -0.8127685377079946, -3.6225150057660684, 0.00439583672222079, -4.465831662859558, -3.195443930797592, -4.614147421341501, -3.166322713280579, -4.184944716181309, -1.61982002830029, -0.5703781807026191, -0.8515586428177824, -2.6605444360800004, -0.6038396872422245, -2.039817298147312, -2.796419048674204, -2.5052970956286655,
-3.2368005120006305, -4.315954558654696, -0.05316962682042418, -3.173026620299399, -3.582910053497143, -2.172717686618515, -5.675268417023385, -3.229566481825675, -3.555233111934317, -2.4160732490673986, -1.5511493954931495, -2.5186383495852143, -1.9633407868313164, -3.3038390012446373, -1.681716851476474, -0.5693323097935498, -2.7917443213713193]
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
