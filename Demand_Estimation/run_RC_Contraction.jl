using BenchmarkTools
using JLD
using CSV

# Data Structure
include("InsChoiceData.jl")

#Halton Draws
include("Halton.jl")

# Random Coefficients MLE
include("RandomCoefficients.jl")
include("RandomCoefficients_2der.jl")
include("Contraction.jl")
include("Log_Likehood.jl")
include("RiskMoments.jl")
include("Estimate_Contraction.jl")
include("Estimate_GMM_Contraction.jl")
println("Code Loaded")

# Load the Data
include("load.jl")
# Structre the data
c = ChoiceData(df,df_mkt,df_risk;
    demoRaw=[:AgeFE_31_39,
            :AgeFE_40_51,
            :AgeFE_52_64,
            :Family,
            :LowIncome],
    prodchars=[:Price,:AV,],
    prodchars_0=[:Price,:AV,],
    fixedEffects=[])

# Fit into model
m = InsuranceLogit(c,100)
println("Data Loaded")

#γ0start = rand(1)-.5
γstart = rand(m.parLength[:γ])/10 -.05
β0start = rand(m.parLength[:β])/10-.05
βstart = rand(m.parLength[:γ])/10 - .05
σstart = rand(m.parLength[:σ])/10 - .05
FEstart = rand(m.parLength[:FE])/100-.005

p0 = vcat(γstart,β0start,βstart,σstart,FEstart)
par0 = parDict(m,p0)

# println("Gradient Test")
# grad_2 = Vector{Float64}(length(p0))
#res = GMM_objective(m,p0)
# f_obj(x) = GMM_objective(m,x)
# grad_1 = Vector{Float64}(length(p0))
# fval_old = f_obj(p0)
# ForwardDiff.gradient!(grad_1,f_obj, p0)
# println(fval_old-res)
# println(maximum(abs.(grad_1-grad_2)))


## Estimate
flag, val, p_ll = estimate!(m, p0)


est_res = estimate_GMM!(m,p0)




rundate = Dates.today()
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estimationresults_fe_rc_$rundate.jld"
save(file,"est_res",est_res)


Profile.init(n=10^8,delay=.001)
Profile.clear()
Juno.@profile GMM_objective(m,p0)
#Juno.@profile calc_risk_moments!(grad,m,par0)
Juno.profiletree()
Juno.profiler()
