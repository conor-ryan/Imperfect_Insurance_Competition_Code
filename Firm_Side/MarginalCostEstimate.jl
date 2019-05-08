using BenchmarkTools
using CSV
using JLD2
using LinearAlgebra
using Statistics


load_path = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Code/Demand_Estimation"
# Data Structure
include("$load_path/InsChoiceData.jl")
include("$load_path/Halton.jl")
include("$load_path/RandomCoefficients.jl")
include("$load_path/utility.jl")
include("$load_path/Contraction.jl")

# MC Parameters
include("MC_parameters.jl")
include("MC_GMM.jl")
include("MC_var.jl")
include("MC_derivatives.jl")
include("MC_optimization.jl")
include("Firm_Inner_Loop.jl")
# Load the Data
include("MC_load.jl")

#### Build Model ####
# Structre the data
chdf = ChoiceData(df,df_mkt,df_risk;
    demoRaw=[:AgeFE_31_39,
            :AgeFE_40_51,
            :AgeFE_52_64,
            :Family,
            :LowIncome],
    prodchars=[:Price,:AV,:Big],
    prodchars_0=[:AV,:Big],
    fixedEffects=[:Firm],
    wgt=[:PERWT])

# Fit into model
m = InsuranceLogit(chdf,1000)

# Cost Data
costdf = MC_Data(df,mom_firm,mom_metal,mom_age,mom_risk;
                baseSpec=[:AGE,:AV_std,:AV_diff],
                fixedEffects=[:Firm_ST])



# ### State-Level Sampling Dictionary
# data_choice = df
# states = sort(unique(data_choice[:ST]))
# st_vec = Vector{Int}(undef,length(data_choice[:ST]))
# for (k,s) in enumerate(states)
#     st_vec[data_choice[:ST].==s] .= k
# end
#
# _stDict_temp = build_ProdDict(st_vec)
# _stDict = Dict{Int,Array{Int,1}}()
# person_vec = data_choice[:Person]
# x = [0]
# for (st,ind) in _stDict_temp
#     _stDict[st] = unique(person_vec[ind])
#     x[1] += length(unique(person_vec[ind]))
# end
#
# draw = bootstrap_sample(m,costdf)
# st_vec_bs = st_vec[draw]
#
# _stDict_temp = build_ProdDict(st_vec_bs)
# _stDict_bs = Dict{Int,Array{Int,1}}()
# person_vec_bs = person_vec[draw]
# x_bs = [0]
# for (st,ind) in _stDict_temp
#     _stDict_bs[st] = unique(person_vec_bs[ind])
#     x_bs[1] += length(unique(person_vec_bs[ind]))
# end

println("Data Loaded")

#### Load Demand Estimation ####
rundate = "2019-03-07"
# resDF = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estimationresults_$rundate.csv")
# p_est = Float64.(resDF[:pars])
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estimationresults_stage2_$rundate.jld2"
@load file p_stg2
p_est = copy(p_stg2)

#### Compute Demand Estimation
par_est = parDict(m,p_est)
individual_values!(m,par_est)

#### Load Starting Parameter
# parStart = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/MC_Moments/linregpars_$rundate.csv")
# p0 = Float64.(parStart[:par_start])


println("#################")
println("#################")
println("###### Estimation 1 #######")
println("#################")
println("#################")

# W = Matrix(1.0I,costdf.mom_length,costdf.mom_length)
# p0 = vcat(rand(1)*.2,rand(2).*4,rand(1)*.2,rand(length(costdf._feIndex)).*3 .+1)
# est_stg1 = estimate_GMM(p0,par_est,m,costdf,W,fit=true)
# incase = est_stg1
# #
# file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg1_$rundate.jld2"
# @save file est_stg1

# p0 = [.1,4,.1,.1]
# p_firm = fit_firm_moments(p0,par_est,m,costdf)
#
#
# p0 = vcat([0,1,1,0],rand(length(costdf._feIndex)).+2)
# p0 = [0.197683, 3.19544, -0.389545, 0.142835]
# est = estimate_NLOpt(p0,par_est,m,costdf,W)
# flag, fval, pinit = est
# p_full = fit_firm_moments(pinit,par_est,m,costdf)
# est_init = estimate_GMM(p_full,par_est,m,costdf,W)
#
# est = twopart_NR_GMM(p0,par_est,m,costdf,W)
# p0 = vcat(rand(1)*.3,rand(2).*4,rand(1)*.3)
#
#
#
# par = parMC(p2,par_est,m,costdf)
# individual_costs(m,par)
# m2 = costMoments(costdf,m,par)
#
#
# p0 = vcat(rand(1)*.3,rand(2).*4,rand(1)*.3,rand(length(costdf._feIndex)).*3 .+1)
# est_init = estimate_GMM(p0,par_est,m,costdf,W)
#
#
# est = twopart_GA_GMM(p0,par_est,m,costdf,W)
#
# GMM_outer_loop(p0,par_est,m,costdf,W)
# est = estimate_NLOpt(p0,par_est,m,costdf,W)
#
# p_test,f_test = newton_raphson_GMM(p0,par_est,m,costdf,W)
#
# est_init = estimate_GMM(p_firm,par_est,m,costdf,W)
# p_init, fval = est_init
# est_stg1 = estimate_GMM(p_init,par_est,m,costdf,W;squared=true)
# incase = est_stg1
# #
# file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg1_$rundate.jld2"
# @save file est_stg1
#
#
#
#
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg1_$rundate.jld2"
@load file est_stg1
p_stg1, fval = est_stg1
#
# p_test = fit_firm_moments(p_stg1[1:4],par_est,m,costdf)

println("#################")
println("#################")
println("###### Estimation 2 #######")
println("#################")
println("#################")


S,Σ,Δ,mom_long = aVar(costdf,m,p_stg1,par_est)
W = inv(S)
# S,S_unwt,mom_est = var_bootstrap(costdf,m,p_stg1,par_est,draw_num=5000)
# W = inv(S)

p0 = vcat(rand(1)*.2,rand(2).*4,rand(1)*.2)
# p0 = [0.0152152, 2.42283, -0.21084, 0.154506]
flag,fval,p_init = estimate_NLOpt(p0,par_est,m,costdf,W)
p_full0 = fit_firm_moments(p_init,par_est,m,costdf)

est_stg2 = estimate_GMM(p_full0,par_est,m,costdf,W)
p_stg2, fval = est_stg2

# GMM_objective(p_fit,par_est,m,costdf,W,squared=true)
#
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg2_$rundate.jld2"
@save file est_stg2
# @load file est_stg2
p_stg2 ,fval = est_stg2



println("#################")
println("#################")
println("###### Estimation 3 #######")
println("#################")
println("#################")


S2,Σ,Δ,mom_long = aVar(costdf,m,p_stg2,par_est)
W = inv(S2)
# S,S_unwt,mom_est = var_bootstrap(costdf,m,p_stg1,par_est,draw_num=5000)
# W = inv(S)

p0 = vcat(rand(1)*.2,rand(2).*4,rand(1)*.2,rand(length(costdf._feIndex)).*3 .+1)

est_stg3 = estimate_GMM(p0,par_est,m,costdf,W)
p_stg3, fval = est_stg3

file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg3_$rundate.jld2"
@save file est_stg3

println("#################")
println("#################")
println("###### Estimation 4 #######")
println("#################")
println("#################")


S3,Σ,Δ,mom_long = aVar(costdf,m,p_stg3,par_est)
W = inv(S3)
# S,S_unwt,mom_est = var_bootstrap(costdf,m,p_stg1,par_est,draw_num=5000)
# W = inv(S)

p0 = vcat(rand(1)*.2,rand(2).*4,rand(1)*.2)
# p0 = [0.0152152, 2.42283, -0.21084, 0.154506]
flag,fval,p_init = estimate_NLOpt(p0,par_est,m,costdf,W)
p_full0 = fit_firm_moments(p_init,par_est,m,costdf)

est_stg4 = estimate_GMM(p_full0,par_est,m,costdf,W)
p_stg4, fval = est_stg4

file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg4_$rundate.jld2"
@save file est_stg4

println("#################")
println("#################")
println("###### Estimation 5 #######")
println("#################")
println("#################")


S4,Σ,Δ,mom_long = aVar(costdf,m,p_stg4,par_est)
W = inv(S4)
# S,S_unwt,mom_est = var_bootstrap(costdf,m,p_stg1,par_est,draw_num=5000)
# W = inv(S)

p0 = vcat(rand(1)*.2,rand(2).*4,rand(1)*.2)
# p0 = [0.0152152, 2.42283, -0.21084, 0.154506]
flag,fval,p_init = estimate_NLOpt(p0,par_est,m,costdf,W)
p_full0 = fit_firm_moments(p_init,par_est,m,costdf)

est_stg5 = estimate_GMM(p_full0,par_est,m,costdf,W)
p_stg5, fval = est_stg5

file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg5_$rundate.jld2"
@save file est_stg5


println("#################")
println("#################")
println("###### Save Results #######")
println("#################")
println("#################")

file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg5_$rundate.jld2"
@load file est_stg5
p_stg5 ,fval = est_stg5
Avar, se, t_stat, stars = GMM_var(costdf,m,p_stg5,par_est)

out1 = DataFrame(pars=p_stg5,se=se,ts=t_stat,sig=stars)
file1 = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_$rundate.csv"
CSV.write(file1,out1)



## Test Estimate Outcome

# f_obj(x) = GMM_objective(x,par_est,m,costdf,W)
f_obj(x) = GMM_test(x,p0,par_est,m,costdf,W)
p_test = p0[1:4]
grad = Vector{Float64}(undef,length(p_test))
hess = Matrix{Float64}(undef,length(p_test),length(p_test))
println("Gradient")
ForwardDiff.gradient!(grad, f_obj, p_test)
println("Hessian")
ForwardDiff.hessian!(hess, f_obj, p_test)


# hess_FD = copy(hess)
obj_grad = Vector{Float64}(undef,length(p0))
obj_hess = Matrix{Float64}(undef,length(p0),length(p0))


grad2 = Vector{Float64}(undef,length(p0))
hess2 = Matrix{Float64}(undef,length(p0),length(p0))
# GMM_objective!(grad2,p0,par_est,m,costdf,W)
GMM_objective!(hess2,grad2,p0,par_est,m,costdf,W)

# grad1 = copy(grad2)
# hess1 = copy(hess2)



mom_grad = Matrix{Float64}(undef,costdf.par_length,costdf.mom_length)
mom_hess = Array{Float64,3}(undef,costdf.par_length,costdf.par_length,costdf.mom_length)

par = parMC(p_stg2,par_est,m,costdf)
individual_costs(m,par)
moments = costMoments(costdf,m,par)
# moments = costMoments!(mom_hess,mom_grad,costdf,m,par)
println(sum(moments.^2))
println(sum(moments.^4))

file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg2_$rundate.jld2"
@load file est_stg2
p_stg2, fval = est_stg2
#
# GMM_objective(p_stg3,par_est,m,costdf,W)
# GMM_objective(p_stg2,par_est,m,costdf,W)
# GMM_objective(p_stg1,par_est,m,costdf,W)
# GMM_objective(p_test,par_est,m,costdf,W)
#
# S,Σ,Δ,mom_long = aVar(costdf,m,p_stg2,par_est)
# W = inv(S)./1000
# est_stg3 = estimate_GMM(p_stg1,par_est,m,costdf,W)
#
#
par = parMC(p_stg2,par_est,m,costdf)
individual_costs(m,par)
moments = costMoments(costdf,m,par)

# par1 = parMC(p_stg1,par_est,m,costdf)
# individual_costs(m,par1)
# moments1 = costMoments(costdf,m,par1)


grad = Matrix{Float64}(undef,costdf.par_length,costdf.mom_length)
hess = Array{Float64,3}(undef,costdf.par_length,costdf.par_length,costdf.mom_length)
mom_test = costMoments!(hess,grad,costdf,m,par)


grad2 = mom_gradient(p_stg2,par_est,m,costdf)

test = grad - grad2'
test[costdf._riskIndex,:] .= 0.0
println(maximum(abs.(test)))

#
# m1 = costMoments_bootstrap(costdf,m,par)
#
# Σ,mom_est = var_bootstrap(costdf,m,par,draw_num=1000)


## Test Delta Gradient
par = parMC(p_stg1,par_est,m,costdf)
individual_costs(m,par)
moments = costMoments(costdf,m,par)

S2,Σ,Δ,mom_long = aVar(costdf,m,p_stg1,par_est)
test = test_Avar(costdf,m,mom_long)



f_obj(x) = test_Avar(costdf,m,x)
grad = Matrix{Float64}(undef,length(moments),length(mom_long))
ForwardDiff.jacobian!(grad, f_obj, mom_long)
#
println(findall(abs.(Δ[297,:]).>1e-11) == findall(abs.(grad).>1e-11))
println(maximum(abs.(Δ[297,:] - grad)))
#
# all_mom = costMoments(costdf,m,p0,par_est)
#
#
# p0 = rand(length(1:maximum(costdf._feIndex))).* 0.5
# gmm(x) = GMM_objective(x,par_est,m,costdf)
# grad = Vector{Float64}(undef,length(p0))
# ForwardDiff.gradient!(grad, gmm, p0)
# # GMM_objective(p0,par_est,m,costdf)
#
#
#
# par0 = parMC(p0,p_est,m,costdf)
#
# individual_costs(m,par0)

using Profile
Profile.init(n=10^8,delay=.001)
Profile.clear()
Juno.@profile GMM_objective!(hess2,grad2,p0,par_est,m,costdf,W)
# Juno.@profile costMoments!(mom_hess,mom_grad,costdf,m,par)
Juno.profiletree()
Juno.profiler()
