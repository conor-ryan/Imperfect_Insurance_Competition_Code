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
p0 = [-0.215655, 0.270784, 0.240974, 0.0825205,
0.0909263, 0.0641491, 0.0234843, 0.142832,
0.201448, -0.0810201, 0.0978135, -0.183395,
-0.31014, -0.321971, -0.0673609, 0.0736619,
0.255953, 0.100079, 0.2204, 0.0847462, 0.129146]
p0 = [-0.260622, 0.269384, 0.255973, 0.116504,
0.0797932, 0.114953, -0.0241331, 0.153463,
 0.229592, -0.0489806, 0.0809528, -0.167998,
 -0.219146, -0.354901, -0.0752608, 0.0787359,
 0.264838, 0.0651117, 0.352503, 0.129392, 0.123992]

 p0 =[-0.544871, -0.933327, 1.82717, 1.84324, 0.292994,
 0.0913905, 0.0801583, 0.509676, 0.320517, -0.0402489,
 0.0543395, -0.79319, -0.452117, -0.410022, -0.313142,
 -0.941509, 6.84528, 0.0249277, 0.439089, 0.0444243, -0.861078]

#p1 = p0/2
# unpack!(m,parStart)
parStart0 = parDict(m,p0)
#parStart1 = parDict(m,p1)
println("Data Loaded")

# Estimate the Model
p_est = estimate!(m, p0)
#p_est = gradient_ascent(m,p0,max_step=1e-5)

run = Dates.today()
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estimationresults_$run.jld"
save(file,"p_est",p_est)
