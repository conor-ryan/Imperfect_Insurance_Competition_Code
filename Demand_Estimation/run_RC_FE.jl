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
include("RandomCoefficients_nonzero.jl")
# include("RandomCoefficients_2der_nonzero.jl")
include("RandomCoefficients_3der.jl")
include("Contraction.jl")
include("Log_Likehood.jl")
include("RiskMoments.jl")
include("Estimate_Basic.jl")
include("Estimate_GMM.jl")
include("utility.jl")
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
    prodchars_0=[:AV,:Big],
    fixedEffects=[:Firm])

#2018 - 12 - 24 : Firm Specification
#2018 - 03 - 4 : Firm Specification
#2019 - 02 - 16: Firm - Market Specification

# Fit into model
m = InsuranceLogit(c,1000)
println("Data Loaded")

#γ0start = rand(1)-.5
γstart = rand(m.parLength[:γ])/10 .-.05
β0start = rand(m.parLength[:β])/10 .-.05
βstart = rand(m.parLength[:γ])/10 .- .05
σstart = rand(m.parLength[:σ])/10 .- .05
FEstart = rand(m.parLength[:FE])/100 .-.005

p0 = vcat(γstart,β0start,βstart,σstart,FEstart)
par0 = parDict(m,p0)
#

# ll = log_likelihood(m,par0)

# file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estimationresults_ll_2018-12-17.jld2"
# @load file p_ll
# # #
# # #
# W = Matrix{Float64}(I,length(p0)+length(m.data.tMoments),length(p0)+length(m.data.tMoments))
# # # # W = eye(length(p0))
# grad_2 = Vector{Float64}(undef,length(p0))
# grad_3 = Vector{Float64}(undef,length(p0))
# hess_2 = Matrix{Float64}(undef,length(p0),length(p0))
#
#
# obj_grad = Vector{Float64}(undef,length(p0))
# grad = Vector{Float64}(undef,length(p0))
# hess = Matrix{Float64}(undef,length(p0),length(p0))
# par0 = parDict(m,p_ll)
# ll = log_likelihood!(hess_2,grad_2,m,par0)
#
# mom_grad = Matrix{Float64}(undef,length(p0),length(m.data.tMoments))
# # mom_hess = Array{Float64,3}(undef,length(p0),length(p0),length(m.data.tMoments))
# # mom = calc_risk_moments!(mom_hess,mom_grad,m,par0)
# mom = calc_risk_moments!(mom_grad,m,par0)
#
# moments = vcat(mom,grad)
# moments_grad = hcat(mom_grad,hess)
#
# calc_GMM_Grad!(obj_grad,moments,moments_grad,W)
#
# # W = Matrix{Float64}(I,length(m.prods),length(m.prods))
# # res = GMM_objective!(hess_2,grad_2,m,p_stg1,W)
# res = GMM_objective!(grad_3,m,p_ll,W)
# f_obj(x) = log_likelihood(m,x)
# # p_test = p0[1:20]
# grad_1 = Vector{Float64}(undef,length(p0))
# hess_1 = Matrix{Float64}(undef,length(p0),length(p0))
# fval_old = f_obj(p0)
# # # #
# println("Grad")
# ForwardDiff.gradient!(grad_1,f_obj, p0)#, cfg)
# println("Hessian")
# cfg = ForwardDiff.HessianConfig(f_obj, p0, ForwardDiff.Chunk{8}())
# ForwardDiff.hessian!(hess_1,f_obj, p0)#,cfg)
#
# println(fval_old-ll)
# println(maximum(abs.(grad_1-grad_2)))
# println(maximum(abs.(hess_1-hess_2)))

# println(fval_old-res)
# println(maximum(abs.(grad_1-grad_2)))
#
#p_ll = newton_raphson(m,p0)

rundate = Dates.today()
rundate = "2019-03-04"
println("#################")
println("#################")
println("###### Estimation 1 #######")
println("#################")
println("#################")
# Estimate
# p_ll,ll = newton_raphson_ll(m,p0)
# rundate = "2018-12-24"
println("Load MLE")
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estimationresults_ll_$rundate.jld2"
# @save file p_ll
@load file p_ll

println("#################")
println("#################")
println("###### Estimation 2 #######")
println("#################")
println("#################")

#
# S = calc_gmm_Avar(m,p_ll)
# W = inv(S)
W = Matrix(1.0I,length(p0)+length(m.data.tMoments),length(p0)+length(m.data.tMoments))
# println("Baby Test")
# par0 = parDict(m,p_ll)
# grad = Vector{Float64}(undef,length(p_ll))
# ll = log_likelihood!(grad,m,par0)
# println(ll)
# par0 = 0.0
# grad = 0.0
# p_stg1, obj_1 = newton_raphson_GMM(m,p_ll,W)
# p_ll = [1.23823, -0.169297, -1.69873, -0.282764, -3.6772, -1.51087, 4.74558, 0.112289, -0.195032,
# 0.440518, 1.00573, 0.0810735, 0.0145202, 0.408243, 0.00430049, -1.98779, -2.47102, -2.87605, -2.38402,
#  -3.32094, -3.83025, -3.94337, -1.83788, -1.87167, -4.43274, 0.575503, -0.136325, 0.610831, -0.599432,
#  -1.49977, -0.497044, -0.189045, -1.43361, 0.227016, 0.355058, -3.31365, -0.399787, -2.95859, -1.36806,
#  0.353456, -0.111014, -3.95334, -1.77059, -2.24438, -2.8546, -5.01624, -2.05154, -1.82248, -2.66176,
#   -3.15346, -1.6959, -0.719817, -0.493211, -0.934829, -4.92727, -1.1841, -1.87416, -2.68204, -2.42847,
#   -1.13669, -0.736661, -1.72, -1.26253, -3.16234, -0.963663, -3.0988, -3.00063, -3.9023, -4.54545, -0.0770832]

# p_stg1, obj_1 = estimate_GMM(m,p_ll,W)
# p_stg1 = [1.69666, -0.624074, 7.70532, -3.40489, -6.69419, -2.48158, 7.60393, -14.0308, -0.181931, 0.717822, 0.340304, 0.469169, -0.507845, 0.788595, 0.123226, -1.79194, -0.333285, -0.723316, -3.17282, -0.426228, -0.293929, -2.64561, -0.0784879, -1.35754, 3.21791, -1.88876,
# -1.58925, -2.53687, -0.370789, -2.18622, -14.7182, 1.80582, -1.0009, -2.17884, 0.200387, 16.1311, 0.288741, 3.14, -0.860258, 4.10627, -1.70509, 1.49477, -3.3637, -2.42231, 24.7657, -20.5494, 2.2456, 19.4633, -2.92765,
# -3.89891, -2.2637, -14.4735, -11.431, 0.533514, -2.2294, -2.6287, -26.1137, -1.4943, -2.44579, 8.59815, 0.969846, -1.72829, -5.566, -13.7933, -1.97754, -3.8871, 4.10815, -0.718699, -3.23624, -1.27109, 0.117634, -4.85136, 13.4222, 20.4344, 2.75925, 14.4956, 16.0905,
# 14.712, -2.29047, -20.4553, -1.69311, 0.636583, -7.28982, -3.25287, 14.0069, 13.4575, 13.4976, 14.7885, 9.97631, 13.0686, 23.616, 16.3882, 15.7613, 6.55768, 16.6312, 30.5015, 12.0083, 5.75734, 9.50593, 11.4955, 15.0992, 31.9034,
#  10.7399, 12.6273, 129.168, 11.746, 12.0794, 15.4479, -7.72893, 15.1765, 17.0606, 16.4796, 13.7675, 54.9088, 12.84, 14.6236, 16.8992, 13.4441, 13.8746, 17.7088, 19.6572, 14.8618, 15.1487, 18.6795, 11.8008, 14.5714, 5.94802, 12.632, 14.7182, 14.0199, 20.299, 12.1547, 11.0481, 10.7667, 11.3731,
#  4.77459, 2.98929, 15.5077, 6.08346, 15.4254, 13.3497, 12.7366, 30.754, 14.7638, 17.0217, 13.7683, 15.0922, 14.4767, 18.6033, 16.4093, 6.69374, 15.0097, 16.3746, 18.4878, 13.0072, 5.28099, 17.3688, 18.3902,
#  16.3492, 14.8055, 17.3371, 14.8072, 8.86309, 14.5357, 14.7041, 15.7328, 14.5812, 27.203, 15.0537, 18.6303, 16.811, 18.345, 16.0117, 11.4185, 14.5457, 21.1681, 18.3544, 14.6987, 16.3102, 71.7111, 12.6081, 27.9114, 22.9514, -25.2762, -2.81921, 1.28416, -1.97289, -33.6452, -3.761, 3.72481,
#  3.30046, 16.1146, 17.5299, 19.0534, 15.6671, 11.3279, -1.81948, 12.2638, 7.04861, 16.9456, 2.72151, 10.6088, 11.1932, 13.7991, 10.9142, 12.5361, -0.262637, 3.34155, 2.91767, 2.71639, 1.29024, -5.34426,
#  -0.507517, 4.85633, 2.7115, -1.79548, -0.554815, -17.8983, -1.93679, 4.7072, -1.32493, -2.73103, -1.4692, -5.76774, 2.27507, 0.529312, -4.88166, 6.62343, -21.7512, -0.584631, 0.37651, -7.29668, 2.21585, 7.07035, -1.51304, -12.8521, 0.648917, -10.4851, -22.8192,
#  -2.80049, -414.855, -0.555476, -3.57521, -1.45637, 2.34392, 3.14224, 11.9275, 10.8854, -4.36675, -0.0396752, 1.14311, 10.8104, -2.73342, 0.680304, -0.0130464, 0.754695, -77.3134, 3.16377, -3.06166, -0.58513, -3.77809, -1.83466,
#  -3.67042, -0.592141, -0.149081, 1.02719, -1.28883, 22.0755, -6.05469, -1.53998, -3.48026, -3.10138, 0.262141, -0.563401, 2.94712, 2.87669, -3.33325, -7.31953, -2.43986, 2.65374, 8.75737, -3.99743, 3.86736, 24.5931, 2.63096, -0.244969, 2.3693, -3.43872, 2.2077, -0.791152,
#  -1.28057, -1.84943, -1.34083, -4.98461, -3.28109, -4.19035, -0.5804, 0.979155, -0.765953, -1.06087, -17.8531, -11.2749, -1.24275, -19.9131, -0.133796, -2.68833, -6.51201, 1.17825, 0.764222, -1.40733, -1.33038, -1.55132,
#   1.78223, -2.90678, -2.55741, -3.42324, -2.13216, -0.477896, -1.64307, -32.1129, -3.27814, 91.2748, -0.242606, -2.19838, -1.88803, 0.858556, 2.42138, -1.15503, -9.84862, 2.82709, 4.10514, 1.99151, -0.253482, 1.35517, 1.55452, -1.63472, 8.29906, 4.9549, -22.4457,
#   -11.6423, -16.9304, -16.958, 4.07072, 1.79674, 0.215514, 1.90029, 0.74697, 4.35856, 1.26694, -6.02297, -5.43165, -2.5835, -1.31203, -3.60975, -8.32222, -3.90463, -6.18311, -3.38345, -2.17206, 25.3508, -27.347, -4.50076, -14.1678,
#   -2.34542, -1.85345, -0.0227093, -1.48743, -0.571708, 0.0129129, 1.8656, -5.29908, -2.1017, 0.460516, 1.63457, 1.90922, -9.33969, -0.490458, -0.788265, -1.41574, -1.14433, -0.748084, 1.25216, -5.22158, -1.61105, -6.49611, -1.34354, 0.372064, 0.361508, -2.13102, 2.02306,
#   1.68706, 1.38314, -4.38445, -4.23085, 4.52342, 8.36033, -0.181798, -0.0297677, 21.3174, 2.54351, 1.06834, -0.953299, -1.07374, -0.558054, 0.314748, 17.157, 12.2153, 8.24038, 10.8553, 11.201, 17.1641, 12.3958, 8.7091, 14.2849,
#    13.0159, 10.8737, 30.0388, -7.68786, -21.3508, 2.18235, 0.398621, -1.1672, -3.19972, -3.28416, -0.223679, -1.77073, -18.0203, -0.591349, -1.22773, -3.47961, 108.768, 12.5799, 18.0579, 13.3641, 12.4269, 14.7246, 17.2879, 11.7199, 13.2392, 14.0214, 17.8036, 23.2208, 16.7952, 13.1126,
#    16.3177, 45.6328, 9.92202, 13.1725, 16.3553, 13.5437, 16.4826, 15.9537, 21.168, 13.3771, 10.7396, 8.03692, 10.034, 16.3079, 79.3114, 14.2101, -7.49809, 90.0325, 13.1136, -14.6901, 9.3159, 16.4484, 15.8154, 17.246,
#     -5.06572, 11.7315, 18.1173, 10.5411, 12.0244, 15.8235, 14.736, 9.60201, 4.14706, 8.93215, 0.988055, 8.08833, 11.9478, -14.9794, 23.1496, 10.239, 7.26027, 28.488, 15.2253, 10.6566, 14.3801, 14.0296, 20.8127, 13.2942, 12.1854, 9.24998, 14.8631, 10.1576, -0.0425643, 13.184, 16.2932, 14.2816,
#     7.25704, 11.0117, 15.7999, 13.8628, 168.437, 12.9696, 12.7651, 14.7568, 0.564273, 10.6377, 5.83764, 18.6474, 11.5926, 56.8914, 5.08712, 8.68608, 13.3077, 14.4718, 13.3431, 12.1455, 18.5251, 11.6841,17.7341]

rundate = "2019-03-07"
# println("Load GMM - Stage 1")
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estimationresults_stage1_$rundate.jld2"
# @save file p_stg1
@load file p_stg1
# est_res = load(file)["est_res"]
#
# p_est = est_res[3]
#
# # p_est = [-3.13731, -3.66356, -4.17707, 1.28711, -4.07659, -2.71348, 6.95234, 1.62123, 1.38525, 1.76242, 2.19671, -0.262142, -0.0726661, 0.436289, -0.00440429, -1.05899, -1.26554, -2.03655, -1.29205, -2.45016, -4.87626, -2.82076, -1.98936, -1.08366, -7.1579, 1.47115, -1.50611, 0.45754, -1.24011, -2.79319, -0.826092, -0.365338, -1.93863, -0.208294, -0.889068, -2.43939, -0.482924, -3.27541, -0.0832024, 5.04283, 1.18866, -5.64587, -1.4914, -4.14378, 0.0746764, -4.63658, -1.09026, -0.0150545, -0.959422, -2.73396, 0.244816, 1.08502, -0.997056, 0.850759, -7.69697, 1.36272, -2.83583, -2.97174, -7.16544, -0.510894, 1.07375, -2.01001, -1.86915, -2.39802, -0.105112, -2.45296, -3.23003, -4.05812, -1.39944, 3.05908]
#
println("#################")
println("#################")
println("###### Estimation 3 #######")
println("#################")
println("#################")

m = InsuranceLogit(c,1000)
S = calc_gmm_Avar(m,p_stg1)
W2 = inv(S)
p_stg2, obj_2 = estimate_GMM(m,p_ll,W2)

# #est_pre = newton_raphson(m,p_est,W2)
#
# est_res = estimate_GMM!(m,p_est,W2)
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estimationresults_stage2_$rundate.jld2"
@save file p_stg2
#
#
# ##### TEST ######
# rundate = "2018-12-29"
# file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estimationresults_stage2_$rundate.jld2"
# @load file p_stg2
# # file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estimationresults_fe_rc_$rundate.jld"
# # p_est = load(file)["est_res"][3]
# #
# p_est = p_stg1
# par0 = parDict(m,p_est)
#
# individual_values!(m,par0)
# individual_shares(m,par0)
#
# grad = Matrix{Float64}(length(p_est),length(m.data.tMoments))
# r = calc_risk_moments!(grad,m,par0)
#
# #
#
# #
# #
# # ll_hess(x) = log_likelihood(app,m,x)
# # hess_ll = Matrix{Float64}(length(p0),length(p0))
# # ForwardDiff.hessian!(hess_ll,ll_hess, p0)
#
# # app = next(eachperson(c),100)[1]
# # hess = Matrix{Float64}(length(p0),length(p0))
# # individual_values!(m,par0)
# # individual_shares(m,par0)
# #
# #
# # grad_2 = Vector{Float64}(length(p0))
# # grad = Vector{Float64}(length(p0))
# # hess = Matrix{Float64}(length(p0),length(p0))
# # hess_3 = Matrix{Float64}(length(p0),length(p0))
# # llg =  log_likelihood!(grad_2,m,p0)
# # llh =  log_likelihood!(hess_3,grad_3,m,par0)
# #
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
# grad_2 = Vector{Float64}(undef,length(p0))
# hess_2 = Matrix{Float64}(undef,length(p0),length(p0))
# using Profile
# Profile.init(n=10^8,delay=.001)
# Profile.clear()
# #Juno.@profile add_obs_mat!(hess,grad,hess_obs,grad_obs,Pop)
# # Juno.@profile log_likelihood!(thD_2,hess_2,grad_2,m,par0)
# Juno.@profile res = GMM_objective(m,p0,W)
# Juno.profiletree()
# Juno.profiler()
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



# ### Test for collinearity...
# all_data = vcat(c.data,c.fixedEffects)
# all_data = all_data[6:size(all_data,1),:]
# rsq_vec = Vector{Float64}(undef,size(all_data,1))
# ind_all = 1:size(all_data,1)
# remove_vec = zeros(length(rsq_vec))
# for i in reverse(ind_all)
#     println(i)
#     Y = all_data[i,:]
#     ind = ind_all[findall( (ind_all.<i) .& (remove_vec.==0) )]
#     # ind = ind_all[findall( (ind_all.!=i))]
#     X = all_data[ind,:]
#     β = (inv(X*X')*X*Y)
#     y_hat = X'*β
#     y_mean = mean(Y)
#     err = Y.-y_hat
#     t1 = dot(Y,Y)
#     t2 = dot(y_hat,y_hat)
#     t3 = dot(err,err)
#
#     # rsq = sum( (y_hat .- y_mean).^2)/sum( (Y .- y_mean).^2)
#     rsq = 1 - dot(err,err)/sum( (Y .- y_mean).^2)
#     # rsq = dot(y_hat,y_hat)/dot(Y,Y)
#     println(rsq)
#     rsq_vec[i] = rsq
#     if rsq>(1-1e-8)
#         remove_vec[i] = 1
#     end
# end
#
# A = cor(all_data,dims=2)
# J = size(A,1)
# for i in 1:J
#     A[i,i] = 0.0
# end
#
#
# A = cov(c.fixedEffects')
# (J1,J2) = size(A)
# for i in 1:J1
#     for j in 1:J2
#         A[i,j] = A[i,j]/(A[i,i]*A[j,j])
#     end
# end
# for i in 1:J1
#     A[i,i] = 0.0
# end
