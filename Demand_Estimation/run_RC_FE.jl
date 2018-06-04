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
    prodchars_0=[:Price,:AV],
    fixedEffects=[:constant,:Firm])

# Fit into model
m = InsuranceLogit(c,25)
println("Data Loaded")

#γ0start = rand(1)-.5
γstart = rand(m.parLength[:γ])/10 -.05
β0start = rand(m.parLength[:β])/10-.05
βstart = rand(m.parLength[:γ])/10 - .05
σstart = rand(m.parLength[:σ])/10 - .05
FEstart = rand(m.parLength[:FE])/100-.005

p0 = vcat(γstart,β0start,βstart,σstart,FEstart)
par0 = parDict(m,p0)

grad_2 = Vector{Float64}(length(p0))
ll =  log_likelihood!(grad_2,m,par0)


c2 = ChoiceData(df,df_mkt;
    demoRaw=[:AgeFE_31_40,
            :AgeFE_41_50,
            :AgeFE_51_64,
            :Family,
            :LowIncome],
    prodchars=[:Price,:AV],
    prodchars_0=[:Price,:AV],
    fixedEffects=[:Firm])

# Fit into model
m2 = InsuranceLogit(c2,25)

FE_2 = vcat(0,FEstart[2:length(FEstart)]).+FEstart[1]
p2 = vcat(γstart,β0start,βstart,σstart,FE_2)
par2 = parDict(m2,)
test= Vector{Float64}(length(p2))
ll_2 =  log_likelihood!(test,m2,p2)




Profile.init(n=10^7,delay=.001)
Profile.clear()
Juno.@profile log_likelihood!(grad_2,m,p0)
Juno.profiletree()
Juno.profiler()










## Estimate
est_res = estimate!(m, p0)

println("Gradient Test")
f_ll(x) = log_likelihood(m,x)
grad_1 = Vector{Float64}(length(p0))
fval = log_likelihood!(grad_2,m,parStart0)
#
# @benchmark log_likelihood!(grad_2,m,parStart0)
# @time log_likelihood!(grad_2,m,parStart0)

fval_old = log_likelihood(m,p0)
ForwardDiff.gradient!(grad_1,f_ll, p0)

println(fval-fval_old)
println(maximum(abs.(grad_1-grad_2)))




rundate = Dates.today()
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estimationresults_fe_rc_$rundate.jld"
save(file,"est_res",est_res)

flag, fval, p_est = est_res



# #parStart1 = parDict(m,p1)

#


#ll_gradient!(grad_2,m,p0)
#

#
#
 individual_values!(m,parStart0)
individual_shares(m,parStart0)
app = next(eachperson(m.data),200)[1]
# @benchmark util_gradient(m,app,parStart0)
#
# grad = util_gradient(m,app,parStart0)
#
println("Profiler")

#
