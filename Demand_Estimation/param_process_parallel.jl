using DataFrames
using CSV
using JLD
using FiniteDiff
# Data Structure
include("InsChoiceData.jl")

#Halton Draws
include("Halton.jl")

# Random Coefficients MLE
include("NestedLogit.jl")
include("Contraction.jl")
include("Estimate_MLE.jl")
println("Code Loaded")

# Predict on Full Data
include("load_ARCOLA.jl")


c = ChoiceData(df,df_mkt;
        prodchars=[:Price,:MedDeduct,:Silver,:Gold,:Platinum,:Catas],
        prodchars_0=[:PriceDiff,:MedDeductDiff],
        demoRaw=[:AGE,:Family,:Income_2,:Income_3])
# Fit into model
m = InsuranceLogit(c,1)

cd("$(homedir())/Documents/Research/ARCOLA/Detailed Demand Estimation")
rundate = "2018-05-02"
file = "Estimation_Output/estimationresults_p_$rundate.jld"

resList = load(file)["res"]

fvalmax = -100
flag = 0
fval = 0
p_est = 0
for i in 1:length(resList)
    flag_temp,fval_temp,p_temp = resList[i]
    if fval_temp>fvalmax
        flag,fval,p_est = resList[i]
        fvalmax = fval
        println(fval)
    end
end

paramFinal = parDict(m,p_est)
contraction!(m,paramFinal)


# #### Debug
# delta_df = CSV.read("Estimation_Output/deltaresults_2018-03-17.csv")
# m.deltas
# unpack_δ!(parStart0.δ,m)
# individual_values!(m,parStart0)
# individual_shares_RC(m,parStart0)

#contraction!(m,paramFinal)


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



function ll(x)
    ll = log_likelihood(m,x)
    return ll
end

# hess = Matrix{Float64}(length(p_est),length(p_est))
# cfg1 = ForwardDiff.HessianConfig(ll, p_est, ForwardDiff.Chunk{1}());
# hess = ForwardDiff.hessian!(hess,ll, p_est,cfg1)
# InfMat = hess./N
# Var = inv(InfMat)
# stderr = sqrt.(diag(Var))
println("Calculate Gradient")
grad_exp = Vector{Float64}(length(p_est))
cfg1 = ForwardDiff.GradientConfig(ll, p_est, ForwardDiff.Chunk{3}());
grad_exp = ForwardDiff.gradient!(grad_exp,ll, p_est,cfg1)


println("Calculate Hessian")
hess_exp = Matrix{Float64}(length(p_est),length(p_est))
cfg2 = ForwardDiff.HessianConfig(ll, p_est, ForwardDiff.Chunk{3}());
hess_exp = ForwardDiff.hessian!(hess_exp,ll, p_est,cfg2)


InfMat = -hess_exp
Var = inv(InfMat)
stderr = sqrt.(diag(Var))

hess_num = Matrix{Float64}(length(p_est),length(p_est))
buff = similar(p_est)
FiniteDiff.hessian!(hess_num,ll, p_est,buff)

# estTable = Matrix{Float64}(length(p_est),2)
# estTable[:,1] = p_est
# estTable[:,2] = stderr

out2 = DataFrame(pars=p_est,stderr=stderr)
file2 = "Estimation_Output/estTable_$rundate.csv"
CSV.write(file2,out2)

grad = similar(p_est)
@benchmark FiniteDiff.gradient!(grad,exp_ll,p_est,buff)

grad2 = similar(p_est)
@benchmark ForwardDiff.gradient!(grad2,exp_ll,p_est)
