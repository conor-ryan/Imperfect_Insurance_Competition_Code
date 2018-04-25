using BenchmarkTools
using JLD
using CSV

# Data Structure
include("InsChoiceData.jl")

#Halton Draws
include("Halton.jl")

# Random Coefficients MLE
include("NestedLogit.jl")
include("Contraction.jl")
include("Estimate_MLE.jl")
println("Code Loaded")

# Load the Data
include("load_ARCOLA.jl")
# Structre the data
c = ChoiceData(df,df_mkt;
        )

# Fit into model
m = InsuranceLogit(c,1)

# -2.6967 in 1727 evaluations
# Initial Parameters
γstart = Array{Float64}([2,2,2])/100
#γstart = Array{Float64}([0,0,0,0,0,0,0])/100
β0start = -ones(3)
βstart = -ones(1)/5
σstart = .7
p0 = vcat(γstart,β0start,βstart,σstart)
parStart0 = parDict(m,p0)

#parStart1 = parDict(m,p1)
println("Data Loaded")

# Estimate the Model
p_est = estimate!(m, p0)
#p_est = gradient_ascent(m,p0,max_step=1,grad_tol=1e-5)

run = Dates.today()
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estimationresults_$run.jld"
save(file,"p_est",p_est)
