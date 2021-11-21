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
spec_prodchars = [:Price,:constant,:AV,:HighRisk,:Small,:High_small]
spec_prodchars_0 = [:AV,:HighRisk,:Small,:High_small]
#Structure the data
c = ChoiceData(df,df_mkt,df_risk;
    demoRaw=spec_demoRaw,
    prodchars=spec_prodchars,
    prodchars_0=spec_prodchars_0,
    fixedEffects=[:Firm])


param_labels = vcat(String.(spec_demoRaw),String.(spec_prodchars),"Price:" .* String.(spec_demoRaw),"Variance:".*String.(spec_prodchars_0),c.feNames)

# Fit into model
m = InsuranceLogit(c,10)
println("Data Loaded")


γstart = rand(m.parLength[:γ])/10 .-.05
β0start = rand(m.parLength[:β])/10 .-.05
βstart = rand(m.parLength[:γ])/10 .- .05
σstart = rand(m.parLength[:σ])/10 .- .05

FEstart = rand(m.parLength[:FE])/100 .-.005

p0 = vcat(γstart,β0start,βstart,σstart,FEstart)


W = -Matrix(1.0I,length(m.data.tMoments),length(m.data.tMoments))

p_est, fval = newton_raphson_ll(m,p0,W)
out1 = DataFrame(labels=param_labels,pars=p_est)
file1 = "test.csv"
CSV.write(file1,out1)




grad = Vector{Float64}(undef,length(p0))
grad_test = Vector{Float64}(undef,length(p0))
hess = Matrix{Float64}(undef,length(p0),length(p0))

ll = log_likelihood(m,p0)
println(ll)
ll = log_likelihood_penalty!(hess,grad,m,p0,W)
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
