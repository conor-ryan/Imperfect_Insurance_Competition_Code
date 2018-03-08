addprocs()
using BenchmarkTools
@everywhere using JLD
@everywhere using CSV

@everywhere cd("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Code/Demand_Estimation")
# Data Structure
@everywhere include("InsChoiceData.jl")

#Halton Draws
@everywhere include("Halton.jl")

# Random Coefficients MLE
@everywhere include("MLE_RC_untyped.jl")
println("Code Loaded")


@everywhere include("load_parallel_sample.jl")

## Function to Execute Estimation
# Takes a starting point as an argument
@everywhere function estimate_parallel(p0::Vector{Float64})
    parStart0 = parDict(m,p0)
    # Estimate the Model
    est_ret,est_f,est_x = estimate!(m, p0)
    return est_ret,est_f,est_x
end

# Initial Parameters
p_list = []
for i in 1:20
    γstart = Array{Float64}([0,0,0])/100
    αstart = -rand()
    βstart = -rand(4*3)/5
    σstart = rand(5)/5
    p0 = vcat(αstart,γstart,βstart,σstart)
    p_list = vcat(p_list,[p0])
end

res = pmap(estimate_parallel,p_list)

run = Dates.today()
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estimationresults_p_$run.jld"
save(file,"p_est",p_est)
