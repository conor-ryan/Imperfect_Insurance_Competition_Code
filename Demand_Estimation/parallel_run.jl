addprocs()
using BenchmarkTools
@everywhere using JLD
@everywhere using CSV

# Data Structure
@everywhere include("InsChoiceData.jl")

#Halton Draws
@everywhere include("Halton.jl")

# Random Coefficients MLE
@everywhere include("MLE_RC_untyped.jl")
println("Code Loaded")


## Function to Execute Estimation
# Takes a starting point as an argument
@everywhere estimate_parallel(p0::Vector{Float64})
    # Load the Data
    include("load_sample.jl")
    # Structre the data
    c = ChoiceData(df,df_mkt)

    # Fit into model
    m = InsuranceLogit(c,500)

    parStart0 = parDict(m,p0)

    # Estimate the Model
    est_ret,est_f,est_x = estimate!(m, p0)

    return est_ret,est_f,est_x
end

# Initial Parameters
p_list = []
for i in 1:3
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
