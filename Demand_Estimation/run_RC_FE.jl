using BenchmarkTools
using JLD
using CSV

# Data Structure
include("InsChoiceData.jl")

#Halton Draws
include("Halton.jl")

# Random Coefficients MLE
include("RandomCoefficients.jl")
include("RandomCoefficients_2der.jl")
include("Contraction.jl")
include("Log_Likehood.jl")
include("RiskMoments.jl")
include("Estimate_Basic.jl")
include("Estimate_GMM.jl")
println("Code Loaded")

# Load the Data
include("load.jl")
# Structre the data
c = ChoiceData(df,df_mkt,df_risk;
    demoRaw=[:AgeFE_31_39,
            :AgeFE_40_51,
            :AgeFE_52_64,
            :Family,
            :LowIncome],
    prodchars=[:Price,:AV,],
    prodchars_0=[:Price,:AV,],
    fixedEffects=[:constant])

# Fit into model
m = InsuranceLogit(c,100)
println("Data Loaded")

#γ0start = rand(1)-.5
γstart = rand(m.parLength[:γ])/10 -.05
β0start = rand(m.parLength[:β])/10-.05
βstart = rand(m.parLength[:γ])/10 - .05
σstart = rand(m.parLength[:σ])/10 - .05
FEstart = rand(m.parLength[:FE])/100-.005

p0 = vcat(γstart,β0start,βstart,σstart,FEstart)
par0 = parDict(m,p0)
## Estimate
flag, val, p_ll = estimate!(m, p0)
est_res = estimate_GMM!(m,p_ll)

#
# s_hess(x) = test_hess(app,m,x)
# grad_s = Vector{Float64}(length(p0))
# hess_s = Matrix{Float64}(length(p0),length(p0))
# ForwardDiff.gradient!(grad_s,s_hess, p0)
# ForwardDiff.hessian!(hess_s,s_hess, p0)
#
#
# ll_hess(x) = log_likelihood(app,m,x)
# hess_ll = Matrix{Float64}(length(p0),length(p0))
# ForwardDiff.hessian!(hess_ll,ll_hess, p0)

# app = next(eachperson(c),100)[1]
# hess = Matrix{Float64}(length(p0),length(p0))
# individual_values!(m,par0)
# individual_shares(m,par0)
#
#
# grad_2 = Vector{Float64}(length(p0))
# grad_3 = Vector{Float64}(length(p0))
# hess_2 = Matrix{Float64}(length(p0),length(p0))
# hess_3 = Matrix{Float64}(length(p0),length(p0))
# llg =  log_likelihood!(grad_2,m,p0)
# llh =  log_likelihood!(hess_3,grad_3,m,par0)
#
# # llh3 =  log_likelihood!(hess_3,m,p0)
# #
# #
# # @benchmark log_likelihood!(grad_2,m,p0)
# # #
# #@benchmark log_likelihood!(hess_3,grad_3,m,p0)
# #
# # @time log_likelihood!(grad_2,m,p0)
# # @time log_likelihood!(hess_2,m,p0)
# #
# println("Gradient Test")
# # f_ll(x) = log_likelihood(m,x)
# # #f_ll(x) = calc_risk_moments(m,x)
# # grad_1 = Vector{Float64}(length(p0))
# # hess_1 = Matrix{Float64}(length(p0),length(p0))
# # fval_old = f_ll(p0)
# # ForwardDiff.gradient!(grad_1,f_ll, p0)
# # ForwardDiff.hessian!(hess_1,f_ll, p0)
# #
# #
# # println(fval_old-llh)
# # println(maximum(abs.(grad_1-grad_3)))
# # println(maximum(abs.(hess_1-hess_3)))
#
# grad = Matrix{Float64}(length(p0),length(m.data.tMoments))
# @benchmark res = calc_risk_moments!(grad,m,par0)
#
# res,grad_2 = calc_risk_moments!(grad,m,par0)
# f_ll(x) = calc_risk_moments(m,x)
# grad_1 = Vector{Float64}(length(p0))
# fval_old = f_ll(p0)
# ForwardDiff.gradient!(grad_1,f_ll, p0)
# println(fval_old-res)
# println(maximum(abs.(grad_1-grad_2)))
#
# res = GMM_objective!(grad_2,m,p0)
# f_obj(x) = GMM_objective(m,x)
# grad_1 = Vector{Float64}(length(p0))
# fval_old = f_obj(p0)
# ForwardDiff.gradient!(grad_1,f_obj, p0)
# println(fval_old-res)
# println(maximum(abs.(grad_1-grad_2)))
#
#
#
# #
# #
# Profile.init(n=10^8,delay=.001)
# Profile.clear()
# Juno.@profile GMM_objective!(grad_2,m,p0)
# #Juno.@profile calc_risk_moments!(grad,m,par0)
# Juno.profiletree()
# Juno.profiler()
#
# for (x,i) in enumerate([1,2,5])
#     print(x)
# end

rundate = Dates.today()
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estimationresults_fe_rc_$rundate.jld"
save(file,"est_res",est_res)

# flag, fval, p_est = est_res
#
#
#
# # #parStart1 = parDict(m,p1)
#
# #
#
#
# #ll_gradient!(grad_2,m,p0)
# #
#
# #
# #
#  individual_values!(m,parStart0)
# individual_shares(m,parStart0)
# app = next(eachperson(m.data),200)[1]
# # @benchmark util_gradient(m,app,parStart0)
# #
# # grad = util_gradient(m,app,parStart0)
# #
# println("Profiler")
#
# #
