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
    fixedEffects=[:Firm_Market],
    wgt=[:PERWT])

# Fit into model
m = InsuranceLogit(chdf,1000)

# Cost Data
costdf = MC_Data(df,mom_firm,mom_metal,mom_age,mom_age_no,mom_risk;
                baseSpec=[:AGE,:AV_std,:AV_diff],
                fixedEffects=[:Firm_ST])


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

W = Matrix(1.0I,costdf.mom_length,costdf.mom_length)
p0 = vcat(rand(1)*.2,rand(2).*4,rand(1)*.2,rand(length(costdf._feIndex)).*3 .+1)
est_stg1 = estimate_GMM(p0,par_est,m,costdf,W,fit=true)
incase = est_stg1

# p0 = vcat(rand(1)*.2,rand(2).*4,rand(1)*.2)
# est_stg1 = estimate_NLOpt(p0,par_est,m,costdf,W)
# incase = est_stg1
# #
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg1_$rundate.jld2"
@save file est_stg1

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
# flag,fval,p_stg1 = est_stg1
#
# p_test = fit_firm_moments(p_stg1[1:4],par_est,m,costdf)

println("#################")
println("#################")
println("###### Estimation 2 #######")
println("#################")
println("#################")

# p_full1 = fit_firm_moments(p_stg1,par_est,m,costdf)
# S,Σ,Δ,mom_long = aVar(costdf,m,p_full1,par_est)
S,Σ,Δ,mom_long = aVar(costdf,m,p_stg1,par_est)
# W = zeros(costdf.mom_length,costdf.mom_length)
# for i in 1:costdf.mom_length
#     W[i,i] = 1/S[i,i]
# end
W = inv(S)
# S,S_unwt,mom_est = var_bootstrap(costdf,m,p_stg1,par_est,draw_num=5000)
# W = inv(S)

# p0 = vcat(rand(1)*.2,rand(2).*4,rand(1)*.2)
# # p0 = [0.0152152, 2.42283, -0.21084, 0.154506]
# est_stg2 = estimate_NLOpt(p0,par_est,m,costdf,W)
# x1,x2,p_init = est_stg2
# p_full = fit_firm_moments(p_init,par_est,m,costdf)

p0 = vcat(rand(1)*.2,rand(2).*4,rand(1)*.2,rand(length(costdf._feIndex)).*3 .+1)
est_stg2 = estimate_GMM(p0,par_est,m,costdf,W,max_ga_itr=45,fit=true)
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

#
# p0 = vcat(rand(1)*.2,rand(2).*4,rand(1)*.2)
# flag,fval,p_init = estimate_NLOpt(p0,par_est,m,costdf,W)
# p_full0 = fit_firm_moments(p_init,par_est,m,costdf)
p_full0 = vcat(rand(1)*.2,rand(2).*4,rand(1)*.2,rand(length(costdf._feIndex)).*3 .+1)
est_stg3 = estimate_GMM(p_full0,par_est,m,costdf,W)
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

# p0 = vcat(rand(1)*.2,rand(2).*4,rand(1)*.2)
# # p0 = [0.0152152, 2.42283, -0.21084, 0.154506]
# flag,fval,p_init = estimate_NLOpt(p0,par_est,m,costdf,W)
# p_full0 = fit_firm_moments(p_init,par_est,m,costdf)
p_full0 = vcat(rand(1)*.2,rand(2).*4,rand(1)*.2,rand(length(costdf._feIndex)).*3 .+1)
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

# p0 = vcat(rand(1)*.2,rand(2).*4,rand(1)*.2)
# # p0 = [0.0152152, 2.42283, -0.21084, 0.154506]
# flag,fval,p_init = estimate_NLOpt(p0,par_est,m,costdf,W)
# p_full0 = fit_firm_moments(p_init,par_est,m,costdf)

p_full0 = vcat(rand(1)*.2,rand(2).*4,rand(1)*.2,rand(length(costdf._feIndex)).*3 .+1)
est_stg5 = estimate_GMM(p_full0,par_est,m,costdf,W)
p_stg5, fval = est_stg5

file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg5_$rundate.jld2"
@save file est_stg5


println("#################")
println("#################")
println("###### Save Results #######")
println("#################")
println("#################")

file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg2_$rundate.jld2"
@load file est_stg2
p_stg5 ,fval = est_stg2
Avar, se, t_stat, stars = GMM_var(costdf,m,p_stg5,par_est)

out1 = DataFrame(pars=p_stg5,se=se,ts=t_stat,sig=stars)
file1 = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_$rundate.csv"
CSV.write(file1,out1)


#### TEST OUTCOMES ####
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg2_$rundate.jld2"
@load file est_stg2
p ,fval = est_stg2
# file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg1_$rundate.jld2"
# @load file est_stg1
# p_stg1, fval = est_stg1
# S,Σ,Δ,mom_long = aVar(costdf,m,p_stg1,par_est)
# W = inv(S)
#
# p=p_full
# p = p_full[1:4]
# p = fit_firm_moments(p,par_est,m,costdf)
par = parMC(p,par_est,m,costdf)
individual_costs(m,par)
moments = costMoments(costdf,m,par)

GMM_objective(p,par_est,m,costdf,W)

f_adj = firmParameters(costdf,m,p,par_est,28)
p_test = similar(p)
p_test[1:4] = p[1:4]
p_test[5:length(p_test)] = p[5:length(p_test)] + f_adj

par = parMC(p_test,par_est,m,costdf)
individual_costs(m,par)
moments_test = costMoments(costdf,m,par)

GMM_objective(p_test,par_est,m,costdf,W)




#### TEST FUNCTIONS ####
p =  vcat(rand(1)*.2,rand(2).*4,rand(1)*.2,rand(length(costdf._feIndex)).*3 .+1)
W = Matrix(1.0I,costdf.mom_length,costdf.mom_length)
# W = zeros(costdf.mom_length,costdf.mom_length)
# M2 = 1+ length(costdf.firmMoments) #+ length(costdf.metalMoments)-1 #+ length(costdf.ageMoments)-1
# M3 = length(costdf.firmMoments) + length(costdf.metalMoments)-1 #+ length(costdf.ageMoments)-1
# for i in M2:M3
#         W[i,i]=1.0
# end
par = parMC(p,par_est,m,costdf)
individual_costs(m,par)
moments = costMoments(costdf,m,par)

S,Σ,Δ,mom_long = aVar(costdf,m,p,par_est)
moments_test = moments_Avar(costdf,m,mom_long)
println("Test Moment Gradient Function")
println(maximum(abs.(moments_test-moments)))


grad2 = Vector{Float64}(undef,length(p))
hess2 = Matrix{Float64}(undef,length(p),length(p))
obj2 = GMM_objective!(hess2,grad2,p,par_est,m,costdf,W)

grad3 = Vector{Float64}(undef,length(p))
obj3 = GMM_objective!(grad3,p,par_est,m,costdf,W)
println("Matching Derivative Functions")
println(obj2-obj3)
println(maximum(abs.(grad2-grad3)))

# f_obj(x) = GMM_objective(x,par_est,m,costdf,W)
f_obj(x) = GMM_test(x,p,par_est,m,costdf,W)
p_test = p[1:5]
L = length(p_test)
fval = f_obj(p_test)
println(fval-obj2)


grad = Vector{Float64}(undef,length(p_test))
hess = Matrix{Float64}(undef,length(p_test),length(p_test))
println("Gradient")
ForwardDiff.gradient!(grad, f_obj, p_test)
println(maximum(abs.(grad-grad2[1:L])))
println("Hessian")
ForwardDiff.hessian!(hess, f_obj, p_test)
println(maximum(abs.(hess-hess2[1:L,1:L])))



mom_grad1 = Matrix{Float64}(undef,costdf.par_length,costdf.mom_length)
mom_grad2 = Matrix{Float64}(undef,costdf.par_length,costdf.mom_length)
mom_hess = Array{Float64,3}(undef,costdf.par_length,costdf.par_length,costdf.mom_length)

moments1 = costMoments!(mom_hess,mom_grad1,costdf,m,par)
moments2 = costMoments!(mom_grad2,costdf,m,par)
println("Matching Moment Derivatives")
println(maximum(abs.(moments1-moments2)))

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
