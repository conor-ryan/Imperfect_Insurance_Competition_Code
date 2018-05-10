using BenchmarkTools
using JLD
using CSV

# Data Structure
include("InsChoiceData.jl")

#Halton Draws
include("Halton.jl")

# Random Coefficients MLE
include("RandomCoefficients.jl")
include("Contraction.jl")
include("Estimate_MLE.jl")
println("Code Loaded")

# Load the Data
include("load.jl")
# Structre the data
c = ChoiceData(df,df_mkt;
prodchars_0 = [:Price],
demoRaw = [:Age,:Family,:HighIncome])

# Fit into model
m = InsuranceLogit(c,1000)

# -2.6967 in 1727 evaluations
# Initial Parameters
γstart = Array{Float64}([0.1,0.1,0.1])/100
#γstart = Array{Float64}([0,0,0,0,0,0,0])/100
β0start = [-1.0] #,-1,1]
βstart = [0,0,0.0]
σstart = [0,0,0.0]
#p0 = vcat(γstart,β0start,βstart,σstart)
p0 = vcat(γstart,β0start,βstart,σstart)
parStart0 = parDict(m,p0)

#parStart1 = parDict(m,p1)
println("Data Loaded")
#-1.238070945516972
# Estimate the Model

# Need to control better for value of insurance.
# Put Age and Income in the utility equation


p_est = estimate!(m, p0)
#p_est = gradient_ascent(m,p0,max_step=1,grad_tol=1e-5)
#
# p0 = [-0.235116, -1.38568, -1.29449, 0.320543, -0.00184672]
# p1 = [-0.235928, -1.38765, -1.29439, 0.320148, -0.00334064]
#
# parStart0 = parDict(m,p0)
# contraction!(m,parStart0)
# log_likelihood(m,parStart0)
#
# parStart1 = parDict(m,p1)
# contraction!(m,parStart1)
# log_likelihood(m,parStart1)


rundate = Dates.today()
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estimationresults_$rundate.jld"
save(file,"p_est",p_est)
