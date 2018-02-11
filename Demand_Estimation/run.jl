using BenchmarkTools
# Data Structure
include("InsChoiceData.jl")

#Halton Draws
include("Halton.jl")

# Random Coefficients MLE
include("MLE_RC_untyped.jl")

# Load the Data
include("load.jl")

# Structre the data
c = ChoiceData(df,df_mkt)

# Fit into model
m = InsuranceLogit(c,500)


# Initial Parameters
γstart = Array{Float64}([1,1,1])/100
βstart = ones(4*3)/100
σstart = ones(4)/100
p0 = vcat(γstart,βstart,σstart)
#p1 = p0/2
# unpack!(m,parStart)
parStart0 = parDict(m,p0)
#parStart1 = parDict(m,p1)

tic()
ll = evaluate_iteration(m,parStart0)
toc()

# Estimate the Model
estimate!(m, p0)




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



calc_RC!(m,parStart)
reset_δ!(m)

@benchmark unpack_δ!(m)

@benchmark individual_values!(m,parStart,1)

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


@benchmark individual_values!(m,parStart)

Profile.clear()
Profile.init(n=10^7,delay=.001)
#@profile estimate!(m,parStart)
@profile individual_values!(m,parStart,1)
Juno.profiletree()
Juno.profiler()

@benchmark individual_shares!(m)
@benchmark individual_values!(m)
# Estimate the Model
estimate!(m, parStart)



unpack!(m,parStart)
@benchmark estimate!(m,parStart)
@time log_likelihood(m)
