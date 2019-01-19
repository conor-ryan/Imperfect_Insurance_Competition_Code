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
    fixedEffects=[:Firm],
    wgt=[:PERWT])

# Fit into model
m = InsuranceLogit(c,1000)

# Cost Data
costdf = MC_Data(df,mom_avg,mom_age,mom_risk;
                baseSpec=[:AGE,:AV],
                fixedEffects=[:ST])

println("Data Loaded")

#### Load Demand Estimation ####
rundate = "2018-08-25"
resDF = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estimationresults_$rundate.csv")
p_est = Float64.(resDF[:pars])

#### Compute Demand Estimation
par_est = parDict(m,p_est)
individual_values!(m,par_est)

#### Estimate Cost Parameters
p0 = Float64.(parStart[:par_start])


println("#################")
println("#################")
println("###### Estimation 1 #######")
println("#################")
println("#################")

S,Σ,Δ,mom_long = aVar(costdf,m,p0,par_est)
(P,Q) = size(S)
W = Matrix(1.0I,P,Q)

est_stg1 = estimate_GMM(p0,par_est,m,costdf,W)
# flag, fval, p_stg1 = est_stg1

file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg1_$rundate.jld2"
# @save file est_stg1
@load file est_stg1
flag, fval, p_stg1 = est_stg1


println("#################")
println("#################")
println("###### Estimation 2 #######")
println("#################")
println("#################")

# S,Σ,Δ,mom_long = aVar(costdf,m,p_stg1,par_est)
# W = inv(S)
S,mom_est = var_bootstrap(costdf,m,p_stg1,par_est,draw_num=1000)
W = inv(S)

est_stg2 = estimate_GMM(p_stg1,par_est,m,costdf,W)
flag, fval, p_stg2 = est_stg2
#
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg2_$rundate.jld2"
@save file est_stg2
# @load file est_stg2
flag, fval, p_stg2 = est_stg2

println("#################")
println("#################")
println("###### Estimation 3 #######")
println("#################")
println("#################")
# S,Σ,Δ,mom_long = aVar(costdf,m,p_stg2,par_est)
# W = inv(S)
S,mom_est = var_bootstrap(costdf,m,p_stg1,par_est,draw_num=1000)
W = inv(S)

est_stg3 = estimate_GMM(p_stg1,par_est,m,costdf,W)
flag, fval, p_stg3 = est_stg3

file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg3_$rundate.jld2"
@save file est_stg3



## Test Estimate Outcome
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
# par = parMC(p_stg1,par_est,m,costdf)
# individual_costs(m,par)
# moments = costMoments(costdf,m,par)
#
# m1 = costMoments_bootstrap(costdf,m,par)
#
# Σ,mom_est = var_bootstrap(costdf,m,par,draw_num=1000)

## Test Delta Gradient
# f_obj(x) = test_Avar(costdf,m,x)
# grad = Vector{Float64}(undef,length(mom_long))
# ForwardDiff.gradient!(grad, f_obj, mom_long)
#
# println(findall(abs.(Δ[315,:]).>1e-11) == findall(abs.(grad).>1e-11))
# println(maximum(abs.(Δ[315,:] - grad)))
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

# using Profile
# Profile.init(n=10^8,delay=.001)
# Profile.clear()
# Juno.@profile m1 = costMoments_bootstrap(costdf,m,par)
# Juno.profiletree()
# Juno.profiler()
