using BenchmarkTools
using JLD
using CSV

# Data Structure
include("InsChoiceData.jl")

#Halton Draws
include("Halton.jl")

# Random Coefficients MLE
include("NestedLogit.jl")
include("Contraction.jl")
include("Log_Likehood.jl")
include("Estimate_Contraction.jl")
println("Code Loaded")

# Load the Data
include("load_ARCOLA.jl")
# Structre the data
c = ChoiceData(df,df_mkt;
        prodchars=[:Price,:MedDeduct,:Silver,:Gold,:Platinum,:Catas],
        prodchars_0=[:PriceDiff,:MedDeductDiff],
        demoRaw=[:AGE,:Family,:Income_2,:Income_3])

# Get Starting point for NelderMead solution
rundate = "2018-05-02"
file = "$(homedir())/Documents/Research/ARCOLA/Detailed Demand Estimation/Estimation_Output/estimationresults_p_$rundate.jld"

resList = load(file)["res"]

fvalmax = -100
flag = 0
fval = 0
p_start = 0
for i in 1:length(resList)
    flag_temp,fval_temp,p_temp = resList[i]
    if fval_temp>fvalmax
        flag,fval,p_start = resList[i]
        fvalmax = fval
        println(fval)
    end
end


println("Data Loaded")

# Fit into model
m = InsuranceLogit(c,1)
p_est = estimate!(m, p_start;method=:LN_NELDERMEAD)
p_est = gradient_ascent(m,p_start;grad_tol=.5)


rundate = Dates.today()
file = "$(homedir())/Documents/Research/ARCOLA/Detailed Demand Estimation/Estimation_Output/estimationresults_$rundate.jld"
save(file,"p_est",p_est)
