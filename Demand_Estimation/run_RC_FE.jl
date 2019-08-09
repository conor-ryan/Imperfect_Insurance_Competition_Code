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
println("Code Loaded")

# Load the Data
include("load.jl")


test = unique(df[[:Person,:N,:Rtype]])
total = sum(test[:N])
for i in 1:4
    perc = round(sum(test[:N][test[:Rtype].==i])/total,digits=3)
    println("Risk type $i: $perc")
end


#Structure the data
c = ChoiceData(df,df_mkt,df_risk;
    demoRaw=[:AgeFE_31_39,
            :AgeFE_40_51,
            :AgeFE_52_64,
            :Family,
            :LowIncome],
    # demoRaw=Vector{Symbol}(undef,0),
    prodchars=[:Price,:constant,:AV,:HighRisk,:Small,:High_small],
    prodchars_0=[:constant,:AV,:HighRisk,:Small,:High_small],
    # prodchars_0=Vector{Symbol}(undef,0),
    fixedEffects=[:Firm])

#2018 - 12 - 24 : Firm Specification
#2019 - 03 - 7 : Firm Specification
#2019 - 03 - 12: Firm - Market Specification
#2019 - 03 - 24: Firm - Market - Cat Specification


# Fit into model
m = InsuranceLogit(c,50)
println("Data Loaded")


γstart = rand(m.parLength[:γ])/10 .-.05
β0start = rand(m.parLength[:β])/10 .-.05
βstart = rand(m.parLength[:γ])/10 .- .05
σstart = rand(m.parLength[:σ])/10 .- .05
# σstart = zeros(m.parLength[:σ])
FEstart = rand(m.parLength[:FE])/100 #.-.005
#
p0 = vcat(γstart,β0start,βstart,σstart,FEstart)



two_stage_est(m,p0)
# # ll = log_likelihood(m,par0)
rundate = "2019-08-08"
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/GMM_Estimate_Firm-$rundate-stg2.jld2"
@load file p_stg2
p0 = copy(p_stg2)
par0 = parDict(m,p0,no2Der=true)


p0[17] = 6.0
r,t = calc_risk_moments(m,p0)
println("Risk Moments are $r,\n $t")

grad = Vector{Float64}(undef,length(p0))
W = Matrix(1.0I,length(p0)+length(m.data.tMoments),length(p0)+length(m.data.tMoments))
GMM_objective!(grad,m,p0,W)
res = GMM_objective(m,p0,W)

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
grad = Vector{Float64}(undef,length(p0))
hess = Matrix{Float64}(undef,length(p0),length(p0))
# par0 = parDict(m,p0)
ll = log_likelihood(m,p0)
# println(maximum(abs.(grad_2)))
# println(ll)
# println(p0)
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
# # # p_test = p0[1:20]
# grad_1 = Vector{Float64}(undef,length(p0))
# hess_1 = Matrix{Float64}(undef,length(p0),length(p0))
# fval_old = f_obj(p0)
# # # # #
# println("Grad")
# ForwardDiff.gradient!(grad_1,f_obj, p0)#, cfg)
# println(fval_old-ll)
# println(maximum(abs.(grad_1-grad_2)))
# println("Hessian")
# cfg = ForwardDiff.HessianConfig(f_obj, p0, ForwardDiff.Chunk{8}())
# ForwardDiff.hessian!(hess_1,f_obj, p0)#,cfg)
# println(maximum(abs.(hess_1-hess_2)))

# println(fval_old-res)
# println(maximum(abs.(grad_1-grad_2)))
#
#p_ll = newton_raphson(m,p0)

# rundate = Dates.today()
rundate = "2019-05-20"
println("#################")
println("#################")
println("###### Estimation 1 #######")
println("#################")
println("#################")
# # Estimate
# p_ll,ll = newton_raphson_ll(m,p0)
# file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estimationresults_ll_$rundate.jld2"
# @save file p_ll

println("Load MLE")
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/GMM_Estimate_Firm-2019-06-03-ll.jld2"
@load file p_ll
# p_ll = copy(p_est)


println("#################")
println("#################")
println("###### Estimation 2 #######")
println("#################")
println("#################")
c = ChoiceData(df,df_mkt,df_risk;
    demoRaw=[:AgeFE_31_39,
            :AgeFE_40_51,
            :AgeFE_52_64,
            :Family,
            :LowIncome],
    prodchars=[:Price,:constant,:AV,:Big],
    prodchars_0=[:AV,:Big],
    fixedEffects=[:Firm])
# Fit into model
m_GMM = InsuranceLogit(c,50)
ind1 = 1:(m_GMM.parLength[:γ]*2+m_GMM.parLength[:β])
ind2 = (1 + maximum(ind1) + m_GMM.parLength[:σ]):m_GMM.parLength[:All]

p0 = zeros(m_GMM.parLength[:All])
p0[ind1] = p_ll[ind1]
p0[ind2] = p_ll[ind2.-m_GMM.parLength[:σ]]

#
# S = calc_mom_Avar(m,p_ll)
# W = inv(S)
W = Matrix(1.0I,length(p0)+length(m_GMM.data.tMoments),length(p0)+length(m_GMM.data.tMoments))

# p_stg1, obj_1 = two_stage_est(m_GMM,p0,W)


# rundate = "2019-03-07"
# println("Load GMM - Stage 1")
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/GMM_Estimate_FMC-2019-06-22-stg1.jld2"
# @save file p_stg1
@load file p_stg1 obj_1
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

m = InsuranceLogit(c,50)

S = calc_mom_Avar(m,p_stg1)
W2 = inv(S[mom_pars,mom_pars])
W[mom_pars]
p_stg2, obj_2 = estimate_GMM(m,p_stg1,W2)

# #est_pre = newton_raphson(m,p_est,W2)
#
# est_res = estimate_GMM!(m,p_est,W2)
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estimationresults_stage2_$rundate.jld2"
@save file p_stg2


println("#################")
println("#################")
println("###### Save Results #######")
println("#################")
println("#################")
# rundate = "2019-03-12"
# file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estimationresults_stage2_$rundate.jld2"
# @load file p_stg2
Avar, se, t_stat, stars = GMM_var(m,p_stg2)

out1 = DataFrame(pars=p_stg2,se=se,ts=t_stat,sig=stars)
file1 = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estimationresults_GMM_$rundate.csv"
CSV.write(file1,out1)

out2 = DataFrame(delta=m.deltas,prods=m.prods)
file2 = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/deltaresults_GMM_$rundate.csv"
CSV.write(file2,out2)


#
#
# ##### TEST ######
f_old = [100.0]
for check in 5:5:255
    file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/checkin_$check.jld2"
    @load file grad_size p_vec f_test no_progress
    println("###### Test Results: $check #######")
    improvement = (f_test-f_old[1])/f_old[1]
    println("Percent Improvement: $improvement")
    f_old[1] = copy(f_test)
    println("Gradient Size: $grad_size")
    println(p_vec[1:20])
    println(maximum(abs.(p_vec)))
    println("Function Value: $f_test")
    println("No Progress: $no_progress")
end


rundate = "2019-03-12"
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/GMM_Estimate_Firm-2019-05-31-stg1.jld2"
@load file p_stg1
m = InsuranceLogit(c,500)
W = Matrix(1.0I,length(p_stg1)+length(m.data.tMoments),length(p_stg1)+length(m.data.tMoments))

res =  GMM_objective(m,p_stg1,W)



p_est = p_stg2
par0 = parDict(m,p_est)

individual_values!(m,par0)
individual_shares(m,par0)
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
p0 = p_est
W = Matrix(1.0I,length(p0)+length(m.data.tMoments),length(p0)+length(m.data.tMoments))
grad_1 = Vector{Float64}(undef,length(p0))
grad_2 = Vector{Float64}(undef,length(p0))
hess_2 = Matrix{Float64}(undef,length(p0),length(p0))
using Profile
Profile.init(n=10^8,delay=.001)
Profile.clear()
#Juno.@profile add_obs_mat!(hess,grad,hess_obs,grad_obs,Pop)
# Juno.@profile log_likelihood!(thD_2,hess_2,grad_2,m,par0)
Juno.@profile res = GMM_objective!(hess_2,grad_2,m,p0,W)
Juno.profiletree()
Juno.profiler()


m = InsuranceLogit(c,25)
res = GMM_objective!(hess_2,grad_2,m,p0,W)

@time GMM_objective!(hess_2,grad_2,m,p0,W)
@time GMM_objective!(hess_2,grad_2,m,p0,W)

m = InsuranceLogit(c,50)
res = GMM_objective!(hess_2,grad_2,m,p0,W)

@time GMM_objective!(hess_2,grad_2,m,p0,W)
@time GMM_objective!(hess_2,grad_2,m,p0,W)

m = InsuranceLogit(c,100)
res = GMM_objective!(hess_2,grad_2,m,p0,W)

@time GMM_objective!(hess_2,grad_2,m,p0,W)
@time GMM_objective!(hess_2,grad_2,m,p0,W)
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


#
# W = Matrix(1.0I,length(p0)+length(m.data.tMoments),length(p0)+length(m.data.tMoments))
# grad_2 = Vector{Float64}(undef,length(p0))
# grad_3 = Vector{Float64}(undef,length(p0))
# hess_2 = Matrix{Float64}(undef,length(p0),length(p0))
# grad_4 = Vector{Float64}(undef,length(p0))
# hess_4 = Matrix{Float64}(undef,length(p0),length(p0))
# ThD_4 = Array{Float64,3}(undef,length(p0),length(p0),length(p0))
# llg =  log_likelihood!(grad_2,m,p0)
# llh =  log_likelihood!(hess_3,grad_3,m,p0)
# llt =  log_likelihood!(ThD_4,hess_4,grad_4,m,p0)
#
# println(maximum(abs.(grad_2-grad_3)))
# println(maximum(abs.(grad_2-grad_4)))
# println(maximum(abs.(hess_3-hess_4)))
#
#
# @time  GMM_objective!(hess_2,grad_2,m,p0,W)
# # #
# # # llh3 =  log_likelihood!(hess_3,m,p0)
# # #
# # #
# # # @benchmark log_likelihood!(grad_2,m,p0)
# # # #
# # #@benchmark log_likelihood!(hess_3,grad_3,m,p0)
# # #
# # # @time log_likelihood!(grad_2,m,p0)
# # # @time log_likelihood!(hess_2,m,p0)
# # #
# println("Gradient Test")
# f_ll(x) = GMM_objective(m,x,p0,W)
# p_test = p0[1:5]
# #f_ll(x) = calc_risk_moments(m,x)
# grad_1 = Vector{Float64}(undef,length(p_test))
# hess_1 = Matrix{Float64}(undef,length(p_test),length(p_test))
# fval_old = f_ll(p_test)
# ForwardDiff.gradient!(grad_1,f_ll, p_test)
# ForwardDiff.hessian!(hess_1,f_ll, p_test)
# # #
# # #
# println(fval_old-llg)
# println(maximum(abs.(grad_1-grad_2[1:5])))
# println(maximum(abs.(hess_1-hess_2[1:5,1:5])))
#
#
#
#
#
#
#
#
# p = parDict(m,p0)
# individual_values!(m,p)
# individual_shares(m,p)
# grad = Vector{Float64}(undef,length(p0))
# grad[:].=0.0
# hess = Matrix{Float64}(undef,length(p0),length(p0))
# thD = Array{Float64,3}(undef,length(p0),length(p0),length(p0))
# app = iterate(eachperson(c))[1]
# ll_obs,pars_relevant = ll_obs_gradient!(grad,app,m,p)
