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
spec_prodchars_σ=[:AV,:AK_MODA_HEALTH_PLAN_INC]

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
m = InsuranceLogit(c,halton_draws)


W = -Matrix(1.0I,length(m.data.rMoments),length(m.data.rMoments))
W[1,1] = -5.0
W[2,2] = -5.0
W[3,3] = -5.0
W[4,4] = -5.0
W[5,5] = -5.0
W = W.*10

# p_est, fval = newton_raphson_ll(m,p0,W)


p_est = [-1.7401897128079984, -3.3927478612399304, -1.2919577851128827, 2.7454205906644975, 0.8893427802389863, -2.785277802222062, 5.419843653048754, 0.3696348716168584, 0.9977196399139419, 1.189508522003214, -0.5630162376529283, -0.7429989721260255, 1.0640565063644385, 0.4331785081637945, 3.444847569887851, 7.427852921476602, -6.569755440394035, -6.277969604313245, -1.8191062285425097, -6.229615802536679, -3.2574872780199757, -2.6162904902194293, -7.880688986969396, -2.603161753311076,
 -0.5010998089534655, 2.582377114634089, -3.621447138080569, -5.40368850894528, 3.434818839203763, -2.292948712477177, -435.7745487839687, 0.5306992959744715, 4.065870140290551, 4.1815062716670575, 0.04014885307743049, 4.012044427192235, -3.317537516988593, -0.0671840484631946, -5.4425867031269215, -7.010022211227096, -6.194038507336201, -11.860729372373362, -3.096366606712311, -2.183372780145842, -0.6034590154529714, -2.96075795739867, -0.10859492704665132, -2.292357734560848,
 -5.144945518951758, 1.097425630866006, -2.423955477159677, -0.8108339806376644, -2.0373577980267297, -5.553626072998672, -1.8841904743928146, -2.0191273569122603, -1.0051096441709928, 7.422447161003717, 4.2962380405274985, 3.031673486709798, 0.17116927707973514, 5.152316872414642, 8.43861616333463, 1.4571679114982359, 1.003776727964798, -9.510235604648036, -1.1850165281338212, 0.5233680358594056, 2.9848519049097484, 3.2654975158409645, -6.502344996748469, -3.2954343646116966,
  2.8980288280559714, -3.4616784102709772, -3.4250507886458568, -1.0027699577944176, -0.6754555711672697, 0.44847571281803716, -0.5832209646977365, -2.792244415067467, -6.787808854917277, -4.932909617832589, 0.3019660138485742, -3.181056355394229, -2.7017581498208627, -5.488874318231958, -1.9638858890699864, -3.7911163532862684, -2.018083533105919, -6.637805300305692, -1.479863660531848, 5.667522341508582, -0.84655471290925, 1.4488107103366896, -0.46963325393729227, 4.993900849124116,
   3.697685193796644, 5.050179162833674, -3.4899580395178256]
p = parDict(m,p_est,no2Der=false)



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
