using DataFrames
using CSV
using JLD
using FiniteDiff
# Data Structure
include("InsChoiceData.jl")

#Halton Draws
include("Halton.jl")

# Random Coefficients MLE
include("MLE_RC.jl")
println("Code Loaded")

# Predict on Full Data
cd("$(homedir())/Documents/Research/Imperfect_Insurance_Competition")
df = CSV.read("Intermediate_Output/Estimation_Data/estimationData_discrete.csv")
df_mkt = CSV.read("Intermediate_Output/Estimation_Data/marketData_discrete.csv")
df[:Firm] = String.(df[:Firm])


c = ChoiceData(df,df_mkt)
# Fit into model
m = InsuranceLogit(c,500)
rundate = "2018-03-20"
file = "Estimation_Output/estimationresults_2018-03-20.jld"
#flag, fval, p_est = load(file)["p_est"]
p_est = load(file)["p_est"]
paramFinal = parDict(m,p_est)


# #### Debug
# delta_df = CSV.read("Estimation_Output/deltaresults_2018-03-17.csv")
# m.deltas
# unpack_δ!(parStart0.δ,m)
# individual_values!(m,parStart0)
# individual_shares_RC(m,parStart0)

contraction!(m,paramFinal)


out1 = DataFrame(pars=p_est)
file1 = "Estimation_Output/estimationresults_$rundate.csv"
CSV.write(file1,out1)

out2 = DataFrame(delta=m.deltas,prods=m.prods)
file2 = "Estimation_Output/deltaresults_$rundate.csv"
CSV.write(file2,out2)

## Calculate the Hessian
# Total Weight
N = 0
wgts = weight(m.data)
for idxitr in values(m.data._personDict)
    N+=wgts[idxitr[1]]
end



function exp_ll(x)
    ll = evaluate_iteration!(m,x,update=false)
    return ll/N
end

ll(x) = evaluate_iteration!(m,x,update=false)

# hess = Matrix{Float64}(length(p_est),length(p_est))
# cfg1 = ForwardDiff.HessianConfig(ll, p_est, ForwardDiff.Chunk{1}());
# hess = ForwardDiff.hessian!(hess,ll, p_est,cfg1)
# InfMat = hess./N
# Var = inv(InfMat)
# stderr = sqrt.(diag(Var))

println("Calculate Hessian")
hess_exp = Matrix{Float64}(length(p_est),length(p_est))
cfg2 = ForwardDiff.HessianConfig(exp_ll, p_est, ForwardDiff.Chunk{3}());
hess_exp = ForwardDiff.hessian!(hess_exp,exp_ll, p_est,cfg2)


InfMat = -hess_exp
Var = inv(InfMat)
stderr = sqrt.(diag(-Var))

hess_num = Matrix{Float64}(length(p_est),length(p_est))
buff = similar(p_est)
FiniteDiff.hessian!(hess_num,exp_ll, p_est,buff)

# estTable = Matrix{Float64}(length(p_est),2)
# estTable[:,1] = p_est
# estTable[:,2] = stderr

out2 = DataFrame(pars=p_est,stderr=stderr)
file2 = "Estimation_Output/estTable_$rundate.csv"
CSV.write(file2,out2)

grad = similar(p_est)
@benchmark FiniteDiff.gradient!(grad,exp_ll,p_est,buff)
