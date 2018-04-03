using BenchmarkTools
using JLD
using CSV

# Data Structure
include("InsChoiceData.jl")

#Halton Draws
include("Halton.jl")

# Random Coefficients MLE
include("MLE_RC.jl")
println("Code Loaded")

# Load the Data
include("load.jl")
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
p0 = [-0.197159, 0.253734, 0.51662, -0.166343, -0.0547134,
 -0.53111, -0.39775, -0.152077, 0.00889907, 0.0552391, 0.0313981,
  -0.142324, 0.0656932, 0.281034, 0.216577, 0.105576, -0.0155838,
   -0.258925, -0.0887626, -1.21513, 0.232384, -0.0155108, 0.159628,
    0.0843623, 0.0722192]
#p1 = p0/2
# unpack!(m,parStart)
parStart0 = parDict(m,p0)
#parStart1 = parDict(m,p1)
println("Data Loaded")

Profile.clear()
Profile.init()
#@profile estimate!(m,parStart)
@profile contraction_SQM!(m,parStart0)
#@profile individual_shares_RC(μ_ij,δ;inside=true)
Juno.profiletree()
Juno.profiler()





@time contraction!(m,parStart0)

m.deltas = m.deltas./m.deltas
unpack_δ!(parStart0.δ,m)
contraction!(m,parStart0)


m1 = InsuranceLogit(c,100)
par1 = parDict(m1,p0)
m2 = InsuranceLogit(c,50)
par2 = parDict(m2,p0)

contraction!(m1,par1)
contraction!(m2,par2)

log_likelihood(m1,par1)
log_likelihood(m2,par2)

individual_values!(m,parStart0)
individual_shares_RC(m,parStart0)



δ_update!(m,parStart0)
unpack_δ!(parStart0.δ,m)



ll = log_likelihood(m,p0)

@benchmark individual_values!(m,parStart0)
@benchmark individual_shares_RC(m,parStart0)
@benchmark δ_update!(m,parStart0)
@benchmark log_likelihood(m,p0)


@benchmark unpack_δ!(parStart0.δ,m)

@benchmark individual_values!(m,parStart0)












# Estimate the Model
#p_est = estimate!(m, p0)

p_est = [-0.326573, -0.861207, 1.56964, 1.49424, 0.00298468, 0.0711555, 0.0437188, 0.577937, 0.337729, 0.00181465, 0.0197669, -0.487562, -0.272428, -0.335795, -0.312408, -1.01044, 6.84523, -0.0410552, 0.245715, 0.0262984, -0.776266]

run = Dates.today()
# file = "estimationresults_$run.jld"
# save(file,"p_est",p_est)
# paramFinal = parDict(m,p_est)
# contraction!(m,paramFinal)
# out1 = DataFrame(pars=p_est)
#
# file1 = "estimationresults_$run.csv"
# CSV.write(file1,out1)
#
# out2 = DataFrame(delta=m.deltas,prods=m.prods)
# file2 = "deltaresults_$run.csv"
# CSV.write(file2,out2)

# Predict on Full Data
df = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Data/estimationData_discrete.csv")
df_mkt = CSV.read("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Data/marketData_discrete.csv")
df[:Firm] = String.(df[:Firm])


c = ChoiceData(df,df_mkt)
# Fit into model
m = InsuranceLogit(c,500)

# file = "Estimation_Output/estimationresults_2018-02-14.jld"
# p_est = load(file)["p_est"]
paramFinal = parDict(m,p_est)

contraction!(m,paramFinal)

run = Dates.today()
out1 = DataFrame(pars=p_est)
file1 = "estimationresults_$run.csv"
CSV.write(file1,out1)

out2 = DataFrame(delta=m.deltas,prods=m.prods)
file2 = "deltaresults_$run.csv"
CSV.write(file2,out2)








tupList = []
dataList = []
idxList = []
for app in eachperson(m.data)
    ind = person(app)[1]
    idxitr = app._personDict[ind]
    δ = parStart0.δ[idxitr]
    tupList = vcat(tupList,[(app.data,parStart0.α[1],
                    parStart0.γ,parStart0.β,δ,
                    parStart0.randCoeffs)])
    dataList = vcat(dataList,[app.data])
    idxList  = vcat(idxList,[idxitr])
end
#idList = Set(eachperson(m.data))
#Collection of Parameters
# parList =[parStart0]
# while length(parList)<length(idList)
#     parList = vcat(parList,[parStart0])
# end
@everywhere pGlo = $parStart0
@everywhere cdata = $c
@everywhere itrCalc(i) = per_val_calc(i,pGlo,cdata)


#Function for parallel caculations
res = map(itrCalc,idxList)
@benchmark map(itrCalc,idxList)

res = pmap(itrCalc,idxList)

@benchmark pmap(itrCalc,idxList)

@everywhere itrCalc(x,i) = pmap(x,i,p)



b1 = @benchmark individual_values!(m,parStart0)
println(b1)
b2 = @benchmark parallel_values!(m,parStart0)
println(b2)

@benchmark map(per_val_calc,idList,parList)


@everywhere function slow(x::Float64)
    a = 1.0
    for i in 1:1000
        for j in 1:25
            a+=asinh(i+j)
        end
    end
    return a
end


map(slow,linspace(1,1000,10))
pmap(slow,linspace(1,1000,10))

@benchmark map(slow,linspace(1,1000,30))
@benchmark pmap(slow,linspace(1,1000,30))


Profile.clear()
Profile.init(n=10^7,delay=.001)
#@profile estimate!(m,parStart)
@profile pmap(itrCalc,idxList)
#@profile individual_shares_RC(μ_ij,δ;inside=true)
Juno.profiletree()
Juno.profiler()


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


@benchmark individual_shares!(m)
@benchmark individual_values!(m)
# Estimate the Model
estimate!(m, parStart)



unpack!(m,parStart)
@benchmark estimate!(m,parStart)
@time log_likelihood(m)
