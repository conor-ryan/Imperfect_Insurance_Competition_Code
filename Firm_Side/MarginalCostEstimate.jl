using BenchmarkTools
using CSV
using LinearAlgebra
using Statistics


load_path = "C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/Code/Demand_Estimation"
# Data Structure
include("$load_path/InsChoiceData.jl")
include("$load_path/Halton.jl")
include("$load_path/RandomCoefficients_nonzero.jl")
include("$load_path/utility.jl")
include("$load_path/Contraction.jl")

# MC Parameters
include("MC_parameters.jl")
include("MC_GMM.jl")
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
runDate = "2018-08-25"
resDF = CSV.read("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estimationresults_2018-08-25.csv")
p_est = Float64.(resDF[:pars])

#### Compute Demand Estimation
par_est = parDict(m,p_est)
individual_values!(m,par_est)

p0 = rand(length(1:maximum(costdf._feIndex))).* 0.5
gmm(x) = GMM_objective(x,par_est,m,costdf)
grad = Vector{Float64}(undef,length(p0))
ForwardDiff.gradient!(grad, gmm, p0)
# GMM_objective(p0,par_est,m,costdf)

estimate_GMM(p0,par_est,m,costdf)

par0 = parMC(p0,p_est,m,costdf)

individual_costs(m,par0)



using Profile
Profile.init(n=10^8,delay=.001)
Profile.clear()
#Juno.@profile add_obs_mat!(hess,grad,hess_obs,grad_obs,Pop)
# Juno.@profile log_likelihood!(thD_2,hess_2,grad_2,m,par0)
Juno.@profile GMM_objective(p0,par_est,m,costdf)
Juno.profiletree()
Juno.profiler()
