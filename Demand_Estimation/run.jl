using BenchmarkTools
using JLD
using CSV
# Data Structure
include("InsChoiceData.jl")

#Halton Draws
include("Halton.jl")

# Random Coefficients MLE
include("MLE_RC_untyped.jl")

# Load the Data
include("load_sample.jl")

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
#p1 = p0/2
# unpack!(m,parStart)
parStart0 = parDict(m,p0)
#parStart1 = parDict(m,p1)

# tic()
# ll = evaluate_iteration(m,parStart0)
# toc()

# Estimate the Model
p_est = estimate!(m, p0)

run = Dates.today()
file = "estimationresults_$run.jld"
save(file,"p_est",p_est)
paramFinal = parDict(m,p_est)
contraction!(m,paramFinal)
out1 = DataFrame(pars=p_est)

file1 = "estimationresults_$run.csv"
CSV.write(file1,out1)

out2 = DataFrame(delta=m.deltas,prods=m.prods)
file2 = "deltaresults_$run.csv"
CSV.write(file2,out2)



# Predict on Full Data
df = CSV.read("Intermediate_Output/estimationData.csv")
df_mkt = CSV.read("Intermediate_Output/marketData.csv")
df[:Firm] = String.(df[:Firm])


c = ChoiceData(df,df_mkt)
# Fit into model
m = InsuranceLogit(c,500)

file = "Estimation_Output/estimationresults_2018-02-14.jld"
p_est = load(file)["p_est"]
paramFinal = parDict(m,p_est)

contraction!(m,paramFinal)

run = Dates.today()
out1 = DataFrame(pars=p_est)
file1 = "Estimation_Output/estimationresults_fullapprox$run.csv"
CSV.write(file1,out1)

out2 = DataFrame(delta=m.deltas,prods=m.prods)
file2 = "Estimation_Output/deltaresults_fullapprox$run.csv"
CSV.write(file2,out2)















m1 = InsuranceLogit(c,500)
tic()
ll = evaluate_iteration(m1,parStart1)
toc()

ll_test0(x) = evaluate_iteration!(m,x)
ll_test1(x) = evaluate_iteration!(m1,x)

grad0 = Array{Float64,1}(length(p0))
ForwardDiff.gradient!(grad0, ll_test0, p0)

grad1 = Array{Float64,1}(length(p1))
ForwardDiff.gradient!(grad1, ll_test1, p1)





calc_RC!(m,parStart0)
reset_δ!(m)

@benchmark unpack_δ!(m,parStart0)

individual_values!(m,parStart0;init=true)
@benchmark individual_values!(m,parStart0)

@benchmark δ_update!(m,parStart)






tic()
evaluate_iteration(m)
toc()

@benchmark calc_RC!(m)

calc_RC!(m)
@benchmark individual_values!(m)

@benchmark unpack_δ!(m)

@benchmark individual_shares_RC!(m)

@benchmark δ_update!(m)

# individual_values!(m)
# unpack_δ!(m)
# utility_val!(m)
# individual_shares!(m)
# δ_update!(m)

@time log_likelihood(m)

tic()
log_likelihood(m)
toc()

@benchmark individual_values!(m)
@benchmark unpack_δ!(m)
@benchmark utility_val!(m)
@benchmark individual_shares!(m)
@benchmark δ_update!(m)


@benchmark individual_values!(m,parStart0)

Profile.clear()
Profile.init(n=10^7,delay=.01)
#@profile estimate!(m,parStart)
@profile individual_values!(m,parStart0)
#@profile individual_shares_RC(μ_ij,δ;inside=true)
Juno.profiletree()
Juno.profiler()

@benchmark individual_shares!(m)
@benchmark individual_values!(m)
# Estimate the Model
estimate!(m, parStart)



unpack!(m,parStart)
@benchmark estimate!(m,parStart)
@time log_likelihood(m)
