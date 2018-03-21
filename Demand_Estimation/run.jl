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
m = InsuranceLogit(c,500)


# Initial Parameters
γstart = Array{Float64}([0,0,0])/100
αstart = -.4
βstart = -ones(4*3)/10
σstart = [1,1,.5,1,1.5]/1000
p0 = vcat(αstart,γstart,βstart,σstart)
 p0 =[-0.544871, -0.933327, 1.82717, 1.84324, 0.292994,
 0.0913905, 0.0801583, 0.509676, 0.320517, -0.0402489,
 0.0543395, -0.79319, -0.452117, -0.410022, -0.313142,
 -0.941509, 6.84528, 0.0249277, 0.439089, 0.0444243, -0.861078]

 p0 = [-0.339757, 0.574995, 2.13052, -1.98083, 0.0216381,
  0.0578002, 0.0289437, 0.955081, 0.228506, -0.0202136,
  -0.0571023, -0.490412, -0.0738414, -0.115849, -0.136463,
   -1.37222, 3.69372, -0.0660455, -0.0162958, -0.167433, -3.92012]

p0 = [-0.0162396, 0.314508, 2.12447, -0.0773287, -0.000348166,
0.0202988, 0.0263916, 0.175525, 0.0034852, -0.0185302, -0.0593699,
0.072032, -0.0608892, -0.129298, -0.09745, -1.66462, -0.135903,
-0.00324694, -0.0314416, -0.145326, -3.97585]

#p1 = p0/2
# unpack!(m,parStart)
parStart0 = parDict(m,p0)
#parStart1 = parDict(m,p1)
println("Data Loaded")

# Estimate the Model
#p_est = estimate!(m, p0)
p_est = gradient_ascent(m,p0,max_step=1e-7,grad_tol=500)

run = Dates.today()
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estimationresults_$run.jld"
save(file,"p_est",p_est)
