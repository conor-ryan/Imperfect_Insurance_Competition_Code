using BenchmarkTools
using JLD
using CSV

# Data Structure
include("InsChoiceData.jl")

#Halton Draws
include("Halton.jl")

# Random Coefficients MLE
include("RandomCoefficients_nonzero.jl")
# include("RandomCoefficients_2der_nonzero.jl")
include("RandomCoefficients_3der.jl")
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
    fixedEffects=[:Firm])

# Fit into model
m = InsuranceLogit(c,11)
println("Data Loaded")

#γ0start = rand(1)-.5
γstart = rand(m.parLength[:γ])/10 -.05
β0start = rand(m.parLength[:β])/10-.05
βstart = rand(m.parLength[:γ])/10 - .05
σstart = rand(m.parLength[:σ])/10 - .05
FEstart = rand(m.parLength[:FE])/100-.005

p0 = vcat(γstart,β0start,βstart,σstart,FEstart)
par0 = parDict(m,p0)
#
# W = eye(length(p0)+length(m.data.tMoments))
# est = newton_raphson(m,p0,W)

grad = Vector{Float64}(length(p0))
hess = Matrix{Float64}(length(p0),length(p0))
grad[:] = 0.0
hess[:] = 0.0
#
# println("Gradient Test")
grad_2 = Vector{Float64}(length(p0))
hess_2 = Matrix{Float64}(length(p0),length(p0))
res = log_likelihood!(hess_2,grad_2,m,p0)

grad_3 = Vector{Float64}(length(p0))
hess_3 = Matrix{Float64}(length(p0),length(p0))
res = log_likelihood!(hess_3,grad_3,m,p0)
#
f_ll(x) = log_likelihood(m,x)
grad_1 = Vector{Float64}(length(p0))
hess_1 = Matrix{Float64}(length(p0),length(p0))
fval_old = f_ll(p0)
ForwardDiff.gradient!(grad_1,f_ll, p0)


ForwardDiff.hessian!(hess_1,f_ll, p0)
#
# println(fval_old-res)
println(maximum(abs.(grad_3-grad_2)))
println(maximum(abs.(hess_3-hess_2)))
# #
#
W = eye(length(p0)+length(m.data.tMoments))
res = GMM_objective!(grad_2,m,p0,W)
# f_obj(x) = GMM_objective(m,x,W)
# grad_1 = Vector{Float64}(length(p0))
# fval_old = f_obj(p0)
# println("ForwardDiff")
# # cfg = ForwardDiff.GradientConfig(f_obj, p0, ForwardDiff.Chunk{4}());
# #
# ForwardDiff.gradient!(grad_1,f_obj, p0)#, cfg)
# println(fval_old-res)
# println(maximum(abs.(grad_1-grad_2)))
#
#p_ll = newton_raphson(m,p0)

rundate = Dates.today()

## Estimate
# flag, val, p_ll = estimate!(m, p0)



println("#################")
println("#################")
println("###### Estimation 2 #######")
println("#################")
println("#################")

# file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estimationresults_stage1_$rundate.jld"
# save(file,"p_ll",p_ll)
# #W = calc_gmm_Avar(m,p_ll)
# W = eye(length(p0)+length(m.data.tMoments))
# est_res = estimate_GMM!(m,p_est,W)

# rundate = "2018-08-25"
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estimationresults_stage3_2018-08-25_NEWEIGHT.jld"
#save(file,"est_res",est_res)
est_res = load(file)["est_res"]

p_est = est_res[3]

# p_est = [-3.13731, -3.66356, -4.17707, 1.28711, -4.07659, -2.71348, 6.95234, 1.62123, 1.38525, 1.76242, 2.19671, -0.262142, -0.0726661, 0.436289, -0.00440429, -1.05899, -1.26554, -2.03655, -1.29205, -2.45016, -4.87626, -2.82076, -1.98936, -1.08366, -7.1579, 1.47115, -1.50611, 0.45754, -1.24011, -2.79319, -0.826092, -0.365338, -1.93863, -0.208294, -0.889068, -2.43939, -0.482924, -3.27541, -0.0832024, 5.04283, 1.18866, -5.64587, -1.4914, -4.14378, 0.0746764, -4.63658, -1.09026, -0.0150545, -0.959422, -2.73396, 0.244816, 1.08502, -0.997056, 0.850759, -7.69697, 1.36272, -2.83583, -2.97174, -7.16544, -0.510894, 1.07375, -2.01001, -1.86915, -2.39802, -0.105112, -2.45296, -3.23003, -4.05812, -1.39944, 3.05908]

println("#################")
println("#################")
println("###### Estimation 3 #######")
println("#################")
println("#################")
m = InsuranceLogit(c,1000)
S = calc_gmm_Avar(m,p_est)
W2 = inv(S)
#est_pre = newton_raphson(m,p_est,W2)

est_res = estimate_GMM!(m,p_est,W2)
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estimationresults_stage3_$rundate.jld"
save(file,"est_res",est_res)


##### TEST ######
# rundate = "2018-08-25"
# file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estimationresults_fe_rc_$rundate.jld"
# p_est = load(file)["est_res"][3]
#
# par0 = parDict(m,p_est)
#
# individual_values!(m,par0)
# individual_shares(m,par0)
#
# grad = Matrix{Float64}(length(p_est),length(m.data.tMoments))
# r = calc_risk_moments!(grad,m,par0)

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
#grad = Matrix{Float64}(length(p0),length(m.data.tMoments))
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
# p0 = p_est
# W = eye(length(p0)+6)
# grad_2 = Vector{Float64}(length(p0)+6)
# hess = Matrix{Float64}(length(p0),length(p0))
Profile.init(n=10^8,delay=.001)
Profile.clear()
#Juno.@profile add_obs_mat!(hess,grad,hess_obs,grad_obs,Pop)
#Juno.@profile log_likelihood!(hess,grad_2,m,par0)
Juno.@profile log_likelihood!(hess_2,grad_2,m,p0)
#Juno.@profile calc_risk_moments!(grad,m,par0)
Juno.profiletree()
Juno.profiler()
#
# for (x,i) in enumerate([1,2,5])
#     print(x)
# end



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
