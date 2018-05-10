using BenchmarkTools
using JLD
using CSV

# Data Structure
include("InsChoiceData.jl")

#Halton Draws
include("Halton.jl")

# Random Coefficients MLE
include("BasicLogit.jl")
include("Contraction.jl")
include("Log_Likehood.jl")
include("Estimate_Basic.jl")
println("Code Loaded")

# Load the Data
include("load.jl")
# Structre the data
c = ChoiceData(df,df_mkt;
    demoRaw=[:AgeFE_31_40,
            :AgeFE_41_50,
            :AgeFE_51_64,
            :Family,
            :LowIncome],
    prodchars=[:Price,:AV],
    prodchars_0=[],
    fixedEffects=[:Firm,:Market])

# Fit into model
m = InsuranceLogit(c,1)


γ0start = 0.0
γstart = Array{Float64}([0.1,0.1,0.1,0.1,0.1])/100
#γstart = Array{Float64}([0,0,0,0,0,0,0])/100
β0start = [-1.0,1.0]
βstart = [0.01,0,0,0,0.01]
FEstart = zeros(length(c._fixedEffects))


#p0 = vcat(γstart,β0start,βstart,σstart)
p0 = vcat(γ0start,γstart,β0start,βstart,FEstart)
parStart0 = parDict(m,p0)

#parStart1 = parDict(m,p1)
println("Data Loaded")

println("Gradient Test")
# f_ll(x) = log_likelihood(m,x)
# grad_1 = Vector{Float64}(length(p0))
# grad_2 = Vector{Float64}(length(p0))
#
# fval = log_likelihood(m,p0)
# ForwardDiff.gradient!(grad_1,f_ll, p0)
# ll_gradient!(grad_2,m,p0)
#
# println(maximum(abs.(grad_1-grad_2)))

## Estimate
est_res = estimate!(m, p0)


rundate = Dates.today()
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estimationresults_fe_$rundate.jld"
save(file,"est_res",est_res)

flag, fval, p_est = est_res
p_est = [-0.241498, -0.0800202, -0.0409392, -0.0178879, -0.113074, -0.28628, -1.02488, 0.836155, -0.109041, 0.001718, 0.251737, -0.0200249, -0.316483, -0.04894, -0.105178, -0.00795242, -0.0312864, -0.0319401, -0.0125555, -0.002333, -0.00379423, -0.0330827, 7.94628e-6, 0.00206126, 0.0962461,0.00658533, 0.00490887, 0.0169301, 0.252643, 0.0303712, 0.05453, 0.00322099, -0.0147726, 0.108975, -0.0469248, -0.00294666, 0.00889079, 0.00285551, -0.0210117, -0.00549378, -0.00745438, -0.00419523, -0.00766899, -0.0523586, -0.0043873, -0.0686898, -0.0227611, -0.0256671, 0.0116931, -0.00238287, 0.00127014, -0.0669821, -0.00383782, 0.00151651, -0.00880966, 0.00133921, -0.00351299, -0.00527229, 0.0112729, -0.0033942, 0.00456307, -0.0953334, 0.0134069, -0.0108429, -0.154123, -0.03074, -0.0112929, -0.00140755]

#p_est = rand(length(p0))-.5
println("Gradient Test")
f_ll(x) = log_likelihood(m,x)
grad_1 = Vector{Float64}(length(p0))
grad_2 = Vector{Float64}(length(p0))

fval = log_likelihood(m,p_est)
ForwardDiff.gradient!(grad_1,f_ll, p_est)
ll_gradient!(grad_2,m,p_est)

maximum(abs.(grad_1-grad_2))


println("Calculate Hessian")
Pop = sum(weight(m.data).*choice(m.data))
hess_exp = Matrix{Float64}(length(p_est),length(p_est))
#cfg2 = ForwardDiff.HessianConfig(ll, p_est, ForwardDiff.Chunk{3}());
hess_exp = ForwardDiff.hessian!(hess_exp,ll, p_est) #,cfg2)


Var = -inv(hess_exp)
stderr = sqrt.(diag(Var))
