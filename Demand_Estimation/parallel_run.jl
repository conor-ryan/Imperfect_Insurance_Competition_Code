addprocs()
using BenchmarkTools
@everywhere using JLD
@everywhere using CSV

@everywhere cd("$(homedir())/Documents/Research/Imperfect_Insurance_Competition")
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
γstart = Array{Float64}([0,0,0])/100
αstart = -.4
βstart = -ones(4*3)/10
σstart = [1,1,.5,1,1.5]/10
p0 = vcat(αstart,γstart,βstart,σstart)
