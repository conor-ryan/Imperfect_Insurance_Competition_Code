addprocs(20)
using JLD
using CSV

@everywhere cd("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Code/Demand_Estimation")
# Data Structure
@everywhere include("InsChoiceData.jl")

#Halton Draws
@everywhere include("Halton.jl")

# Random Coefficients MLE
@everywhere include("NestedLogit.jl")
@everywhere include("Contraction.jl")
@everywhere include("Estimate_MLE.jl")
println("Code Loaded")


@everywhere include("load_ARCOLA_parallel.jl")
println("Data Loaded")
## Function to Execute Estimation
# Takes a starting point as an argument
@everywhere function estimate_parallel(p0::Vector{Float64})
    parStart0 = parDict(m,p0)
    # Estimate the Model
    est_ret,est_f,est_x = estimate!(m, p0)
    return est_ret,est_f,est_x
end

# # Initial Parameters
# p_list = []
# for i in 1:20
#     γstart = (rand(4)*2 - 1)/100
#     β0start = rand(2)*2 - 1
#     βstart = (rand(6*3+4)*2 - 1)/2
#     σstart = rand()
#     p0 = vcat(γstart,β0start,βstart,σstart)
#     p_list = vcat(p_list,[p0])
# end

cd("$(homedir())/Documents/Research/ARCOLA/Detailed Demand Estimation")
rundate = "2018-05-02"
file = "Estimation_Output/estimationresults_p_$rundate.jld"

resList = load(file)["res"]

fvalmax = -100
flag = 0
fval = 0
p_init = 0
for i in 1:length(resList)
    flag_temp,fval_temp,p_temp = resList[i]
    if fval_temp>fvalmax
        flag,fval,p_init = resList[i]
        fvalmax = fval
        println(fval)
    end
end

p_list = []
for i in 1:20
    dev = rand(length(p_init))*0.01 - 0.005
    p0 = p_init.+dev
    p_list = vcat(p_list,[p0])
end



res = pmap(estimate_parallel,p_list)

run = Dates.today()
file = "$(homedir())/Documents/Research/ARCOLA/Detailed Demand Estimation/estimationresults_p2_$run.jld"
save(file,"p_est",p_est)
