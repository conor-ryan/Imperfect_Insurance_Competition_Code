using BenchmarkTools
using CSV
using JLD2
using LinearAlgebra
using Statistics


load_path = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Code/Demand_Estimation"
# Data Structure
include("$load_path/InsChoiceData.jl")
include("$load_path/Halton.jl")
include("$load_path/RandomCoefficients_nonzero.jl")
include("$load_path/utility.jl")
include("$load_path/Contraction.jl")

# MC Parameters
include("MC_parameters.jl")
include("MC_GMM.jl")
include("MC_var.jl")
include("MC_derivatives.jl")
include("MC_optimization.jl")
# Load the Data
include("MC_load.jl")

#### Build Model ####
# Structre the data
c = ChoiceData(df,df_mkt,df_risk;
    demoRaw=[:AgeFE_31_39,
            :AgeFE_40_51,
            :AgeFE_52_64,
            :Family,
            :LowIncome],
    prodchars=[:Price,:AV,:Big],
    prodchars_0=[:AV,:Big],
    fixedEffects=[:Firm_Market],
    wgt=[:PERWT])

# Fit into model
m = InsuranceLogit(c,1000)

# Cost Data
costdf = MC_Data(df,mom_avg,mom_age,mom_risk;
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
rundate = "2019-03-12"
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

mom_length = length(costdf.avgMoments) + (length(costdf.ageMoments)-1) + 1
W = Matrix(1.0I,mom_length,mom_length)
# p0 = [0.0142467, 2.38318, 0.118645, 3.71279, 2.82461, 3.05562, 3.13485, 2.9542, 2.57828, 3.15453, 3.11387, 3.04592, 2.28916, 3.28014, 3.22481, 2.87773, 2.82558]
p0 = vcat([0,1,1,0],rand(length(costdf._feIndex)).+2)
est_init = estimate_GMM(p0,par_est,m,costdf,W)
p_init, fval = est_init
est_stg1 = estimate_GMM(p_init,par_est,m,costdf,W;squared=true)
incase = est_stg1
#
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg1_$rundate.jld2"
@save file est_stg1




file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg1_$rundate.jld2"
@load file est_stg1
p_stg1, fval = est_stg1

println("#################")
println("#################")
println("###### Estimation 2 #######")
println("#################")
println("#################")
# 0.013256879768224933
# 2.216022853728525
# 0.1379307879498515
# 3.7043473610912434
# 3.0742318364102337
# 2.5971482342483383
# 2.9299329664027836
# 3.0627843999066524
# 2.2569960479161213
# 4.0509420797356706
# 2.5501373583954208
# 3.4911435432186857
# 2.7044854042466335
# 3.140991172783581
# 3.01763297597877
# 3.0255579091830334
# 3.44939149700815

# S2,Σ,Δ,mom_long = aVar(costdf,m,p_stg1,par_est)
# W = inv(S2)
S,mom_est = var_bootstrap(costdf,m,p_stg1,par_est,draw_num=1000)
W = inv(S)

p0 = vcat([0.1,2,2,0.1],rand(length(costdf._feIndex)).+2)
est_stg2 = estimate_GMM(p0,par_est,m,costdf,W;squared=true)
p_stg2, fval = est_stg2


#
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg2_$rundate.jld2"
@save file est_stg2
# @load file est_stg2
p_stg2 ,fval = est_stg2

println("#################")
println("#################")
println("###### Save Results #######")
println("#################")
println("#################")

file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg2_$rundate.jld2"
@load file est_stg2
p_stg2 ,fval = est_stg2
Avar, se, t_stat, stars = GMM_var(costdf,m,p_stg2,par_est)

out1 = DataFrame(pars=p_stg2,se=se,ts=t_stat,sig=stars)
file1 = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_$rundate.csv"
CSV.write(file1,out1)


# println("#################")
# println("#################")
# println("###### Estimation 3 #######")
# println("#################")
# println("#################")
# # S,Σ,Δ,mom_long = aVar(costdf,m,p_stg2,par_est)
# # W = inv(S)
# S,mom_est = var_bootstrap(costdf,m,p_stg1,par_est,draw_num=1000)
# W = inv(S)
#
# est_stg3 = estimate_GMM(p_stg1,par_est,m,costdf,W)
# flag, fval, p_stg3 = est_stg3
#
# file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg3_$rundate.jld2"
# @save file est_stg3
#








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

par = parMC(p_stg1,par_est,m,costdf)
individual_costs(m,par)
moments = costMoments(costdf,m,par)
# moments = costMoments!(mom_hess,mom_grad,costdf,m,par)
println(sum(moments.^2))
println(sum(moments.^4))

# file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg2_$rundate.jld2"
# @load file est_stg2
# flag, fval, p_stg2 = est_stg2
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

par1 = parMC(p_stg1,par_est,m,costdf)
individual_costs(m,par1)
moments1 = costMoments(costdf,m,par1)


grad = Matrix{Float64}(undef,costdf.par_length,costdf.mom_length)
hess = Array{Float64,3}(undef,costdf.par_length,costdf.par_length,costdf.mom_length)
costMoments!(hess,grad,costdf,m,par)


grad2 = mom_gradient(p0,par_est,m,costdf)

test = grad - grad2'
test[costdf._riskIndex,:] .= 0.0
println(maximum(abs.(test)))

#
# m1 = costMoments_bootstrap(costdf,m,par)
#
# Σ,mom_est = var_bootstrap(costdf,m,par,draw_num=1000)

## Test Delta Gradient
f_obj(x) = test_Avar(costdf,m,x)
grad = Vector{Float64}(undef,length(mom_long))
ForwardDiff.gradient!(grad, f_obj, mom_long)
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
