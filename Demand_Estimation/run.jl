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
m = InsuranceLogit(c,2500)


# Initial Parameters
γstart = Array{Float64}([0,0,0])/100
β0start = -ones(4)/5
βstart = -ones(4*3)/10
σstart = [1,.5,1,-1.5]/100
p0 = vcat(γstart,β0start,βstart,σstart)

# p0 = [-0.195938, 0.253235, 0.516015, -0.168222, -0.0508759, -0.527343, -0.396065,
#     -0.149112, 0.00705938, 0.0588234, 0.0315465, -0.141726, 0.0651431, 0.279406, 0.217439,
#     0.106517, -0.016283, -0.258406, -0.0900659, -1.21492, 0.233004, -0.0147776, 0.1635,
#      0.0783282, 0.074761]

# p0 = [0.0576722, 0.0616882, 0.0685012, -0.00806572, -0.0151571, -0.0193941,
#  -0.00620399, 0.0122666, 0.0253093, 0.0262103, 0.0259141, 0.0360304, 0.0542378,
#   0.0282865, 0.00276404, -0.0553111, -0.0776253, -0.141009, -0.0959306,
#   -0.0867197, 0.035496, 0.0306258, 0.0347868, 0.0254983, 0.0120267]

# p0 = [0.0482056, 0.0556701, 0.0616089,
# 0.0572539, -0.0358625, 0.0377119, -0.064001,
# -0.335578, -0.264522, -0.0173447,
# 0.0566126,  0.0501532, -0.069754,
#  0.199378, 0.181476, 0.0116419,
# -0.213134, -0.120886, -0.401103,
#  0.143654,0.0847654, 0.0335804, 0.00734248]

#p1 = p0/2
# unpack!(m,parStart)
parStart0 = parDict(m,p0)
#parStart1 = parDict(m,p1)
println("Data Loaded")

# Estimate the Model
p_est = estimate!(m, p0)
#p_est = gradient_ascent(m,p0,max_step=1,grad_tol=1e-5)

run = Dates.today()
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estimationresults_$run.jld"
save(file,"p_est",p_est)
