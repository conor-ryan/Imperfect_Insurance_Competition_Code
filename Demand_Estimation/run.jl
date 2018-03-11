using BenchmarkTools
using JLD
using CSV

# Data Structure
include("InsChoiceData.jl")

#Halton Draws
include("Halton.jl")

# Random Coefficients MLE
include("MLE_RC_untyped.jl")
println("Code Loaded")

# Load the Data
include("load_sample.jl")
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
p0 = [-0.215655, 0.270784, 0.240974, 0.0825205,
0.0909263, 0.0641491, 0.0234843, 0.142832,
0.201448, -0.0810201, 0.0978135, -0.183395,
-0.31014, -0.321971, -0.0673609, 0.0736619,
0.255953, 0.100079, 0.2204, 0.0847462, 0.129146]
#p1 = p0/2
# unpack!(m,parStart)
parStart0 = parDict(m,p0)
#parStart1 = parDict(m,p1)
println("Data Loaded")

# Estimate the Model
p_est = estimate!(m, p0)
#p_est = gradient_ascent(m,p0,init_step=1e-5)

run = Dates.today()
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estimationresults_$run.jld"
save(file,"p_est",p_est)
