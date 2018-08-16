using BenchmarkTools
using JLD
using CSV

# Data Structure
include("InsChoiceData.jl")

#Halton Draws
include("Halton.jl")

# Random Coefficients MLE
include("RandomCoefficients.jl")
include("RandomCoefficients_2der_nonzero.jl")
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
    prodchars=[:Price,:AV,:Big],
    prodchars_0=[:Price,:AV,:Big],
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

# println("Gradient Test")
# grad_2 = Vector{Float64}(length(p0))
# W = eye(length(p0)+length(m.data.tMoments))
# res = GMM_objective!(grad_2,m,p0,W)
# f_obj(x) = GMM_objective(m,x,W)
# grad_1 = Vector{Float64}(length(p0))
# fval_old = f_obj(p0)
# println("ForwardDiff")
# ForwardDiff.gradient!(grad_1,f_obj, p0)
# println(fval_old-res)
# println(maximum(abs.(grad_1-grad_2)))
#

## Estimate
flag, val, p_ll = estimate!(m, p0)
println("#################")
println("#################")
println("###### Estimation 2 #######")
println("#################")
println("#################")
#W = calc_gmm_Avar(m,p_ll)
W = eye(length(p0)+length(m.data.tMoments))
est_res = estimate_GMM!(m,p_ll,W)
p_est = est_res[3]
W2 = calc_gmm_Avar(m,p_est)
println("#################")
println("#################")
println("###### Estimation 3 #######")
println("#################")
println("#################")
est_res = estimate_GMM!(m,p_est,W2)


# individual_values!(m,par0)
# individual_shares(m,par0)
#

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
# grad = Vector{Float64}(length(p0))
# hess = Matrix{Float64}(length(p0),length(p0))
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

#
#
#
# #
# #
# Profile.init(n=10^8,delay=.001)
# Profile.clear()
# Juno.@profile log_likelihood!(hess,grad,m,par0)
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
