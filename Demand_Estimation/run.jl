using BenchmarkTools
using JLD
using CSV

# Data Structure
include("InsChoiceData.jl")

#Halton Draws
include("Halton.jl")

# Random Coefficients MLE
include("MLE_RC.jl")
println("Code Loaded")

# Load the Data
include("load.jl")
# Structre the data
c = ChoiceData(df,df_mkt)

# Fit into model
m = InsuranceLogit(c,1500)


# Initial Parameters
γstart = Array{Float64}([0,0,0,0])/100
#αstart = -.4
βstart = -ones(4*4)/10
σstart = [1,1,.5,1,1.5]/1000
p0 = vcat(γstart,βstart,σstart)

# p0 = [-0.0162396, 0.314508, 2.12447, -0.0773287, -0.000348166,
# 0.0202988, 0.0263916, 0.175525, 0.0034852, -0.0185302, -0.0593699,
# 0.072032, -0.0608892, -0.129298, -0.09745, -1.66462, -0.135903,
# -0.00324694, -0.0314416, -0.145326, -3.97585]

p0 = [-0.197159, 0.253734, 0.51662, -0.166343, -0.0547134,
 -0.53111, -0.39775, -0.152077, 0.00889907, 0.0552391, 0.0313981,
  -0.142324, 0.0656932, 0.281034, 0.216577, 0.105576, -0.0155838,
   -0.258925, -0.0887626, -1.21513, 0.232384, -0.0155108, 0.159628,
    0.0843623, 0.0722192]

p0 = [-0.195938, 0.253235, 0.516015, -0.168222, -0.0508759, -0.527343, -0.396065, 
    -0.149112, 0.00705938, 0.0588234, 0.0315465, -0.141726, 0.0651431, 0.279406, 0.217439,
    0.106517, -0.016283, -0.258406, -0.0900659, -1.21492, 0.233004, -0.0147776, 0.1635,
     0.0783282, 0.074761]

#p1 = p0/2
# unpack!(m,parStart)
parStart0 = parDict(m,p0)
#parStart1 = parDict(m,p1)
println("Data Loaded")

# Estimate the Model
#p_est = estimate!(m, p0)
p_est = gradient_ascent(m,p0,max_step=1,grad_tol=1e-5)

run = Dates.today()
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estimationresults_$run.jld"
save(file,"p_est",p_est)
