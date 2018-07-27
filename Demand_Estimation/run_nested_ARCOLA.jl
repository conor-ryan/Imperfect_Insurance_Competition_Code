using BenchmarkTools
using JLD
using CSV

# Data Structure
include("InsChoiceData.jl")

#Halton Draws
include("Halton.jl")

# Random Coefficients MLE
include("BasicNestedLogit.jl")
include("Contraction.jl")
include("Log_Likehood.jl")
include("Estimate_Basic.jl")
println("Code Loaded")

# Load the Data
include("load_ARCOLA.jl")
# Structre the data
c = ChoiceData(df,df_mkt;
    demoRaw=[:AgeFE_31_39,
            :AgeFE_40_51,
            :AgeFE_52_64,
            :Family,
            :LowIncome],
    prodchars=[:Price,:MedDeduct,:MedOOP],
    prodchars_0=[:Price,:MedDeduct,:MedOOP],
    fixedEffects=[:prodCat],
    riskscores=false)


# Get Starting point for NelderMead solution
# rundate = "2018-05-02"
# file = "$(homedir())/Documents/Research/ARCOLA/Detailed Demand Estimation/Estimation_Output/estimationresults_p_$rundate.jld"
#
# resList = load(file)["res"]
#
# fvalmax = -100
# flag = 0
# fval = 0
# p_start = 0
# for i in 1:length(resList)
#     flag_temp,fval_temp,p_temp = resList[i]
#     if fval_temp>fvalmax
#         flag,fval,p_start = resList[i]
#         fvalmax = fval
#         println(fval)
#     end
# end


println("Data Loaded")

# Fit into model
m = InsuranceLogit(c,1,nested=true,riskscores=false)

γstart = rand(m.parLength[:γ])/10 -.05
β0start = rand(m.parLength[:β])/10-.05
βstart = rand(m.parLength[:γ])/10 - .05
σstart = rand(m.parLength[:σ])/2 + .5
FEstart = rand(m.parLength[:FE])/100-.005

p0 = vcat(γstart,β0start,βstart,σstart,FEstart)

grad_2 = Vector{Float64}(length(p0))
ll =  log_likelihood!(grad_2,m,p0)
@benchmark log_likelihood!(grad_2,m,p0)

println("Gradient Test")
f_ll(x) = log_likelihood(m,x)
grad_1 = Vector{Float64}(length(p0))
fval_old = log_likelihood(m,p0)
ForwardDiff.gradient!(grad_1,f_ll, p0)

println(fval_old-ll)
println(maximum(abs.(grad_1-grad_2)))


est_res = estimate!(m, p0)

flag, val, p_est = est_res

out1 = DataFrame(pars=p_est)
file1 = "$(homedir())/Documents/Research/ARCOLA/Detailed Demand Estimation/Estimation_Output/estimationresults.csv"
CSV.write(file1,out1)



rundate = Dates.today()
file = "$(homedir())/Documents/Research/ARCOLA/Detailed Demand Estimation/Estimation_Output/estimationresults_$rundate.jld"
save(file,"p_est",p_est)
