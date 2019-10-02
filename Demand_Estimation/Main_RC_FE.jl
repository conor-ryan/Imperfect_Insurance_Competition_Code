using JLD2
using CSV
using Random
using Dates
using LinearAlgebra
using Statistics

# Data Structure
include("InsChoiceData.jl")

#Halton Draws
include("Halton.jl")

# Random Coefficients MLE
include("RandomCoefficients.jl")
include("RandomCoefficients_der.jl")
include("Contraction.jl")
include("Log_Likehood.jl")
include("RiskMoments.jl")
include("Estimate_Basic.jl")
include("utility.jl")
include("DerivFunctions.jl")
# include("Estimate_GMM.jl")
include("Specification_Run.jl")
# Load the Data
include("load.jl")

println("Code Loaded")

P_mat = Matrix{Float64}(undef,20,4)
#### General Specification ####

halton_draws = 100
spec_demoRaw = [:AgeFE_31_39,
        :AgeFE_40_51,
        :AgeFE_52_64,
        :Family,
        :LowIncome]
spec_prodchars=[:Price,:constant,:AV,:HighRisk,:Small,:High_small]
spec_prodchars_0=[:AV]

rundate = Dates.today()
# rundate = "2018-12-23"

# #### Run Specification 1 ####
println("Run Specification 1")
spec = "Firm"
spec1 = run_specification(df,df_mkt,df_risk,
                    haltonDim = halton_draws,
                    spec_demoRaw=spec_demoRaw,
                    spec_prodchars=spec_prodchars,
                    spec_prodchars_0=spec_prodchars_0,
                    spec_fixedEffects=[:Firm])
p_est, model, fval = spec1
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estresults_fe_rc_$spec-$rundate.jld2"
@save file p_est
# @load file spec1
println("Save Results")

p_est, m, fval = spec1
println(p_est[1:20])
P_mat[:,1] = p_est[1:20]

# Fit into model
par0 = parDict(m,p_est)
individual_values!(m,par0)
individual_shares(m,par0)
#
AsVar, stdErr,t_stat, stars = res_process_ll(m,p_est)


out1 = DataFrame(pars=p_est,se=stdErr,ts=t_stat,sig=stars)
file1 = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estimationresults_$spec-$rundate.csv"
CSV.write(file1,out1)

out2 = DataFrame(delta=m.deltas,prods=m.prods)
file2 = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/deltaresults_$spec-$rundate.csv"
CSV.write(file2,out2)

#### Run Specification 2 ####
println("Run Specification 2")
spec = "FMC"
spec2 = run_specification(df,df_mkt,df_risk,
                    haltonDim = halton_draws,
                    spec_demoRaw=spec_demoRaw,
                    spec_prodchars=spec_prodchars,
                    spec_prodchars_0=spec_prodchars_0,
                    spec_fixedEffects=[:Firm_Market_Cat])
p_est, model, fval = spec2
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estresults_fe_rc_$spec-$rundate.jld2"
@save file p_est
# @load file spec2
println("Save Results")

p_est, m, fval = spec2
println(p_est[1:20])
P_mat[:,2] = p_est[1:20]

# Fit into model
par0 = parDict(m,p_est)
individual_values!(m,par0)
individual_shares(m,par0)
#
AsVar, stdErr,t_stat, stars = res_process_ll(m,p_est)


out1 = DataFrame(pars=p_est,se=stdErr,ts=t_stat,sig=stars)
file1 = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estimationresults_$spec-$rundate.csv"
CSV.write(file1,out1)

out2 = DataFrame(delta=m.deltas,prods=m.prods)
file2 = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/deltaresults_$spec-$rundate.csv"
CSV.write(file2,out2)


#### Run Specification 3 ####
println("Run Specification 3")
spec = "FMA"
spec3 = run_specification(df,df_mkt,df_risk,
                    haltonDim = halton_draws,
                    spec_demoRaw=spec_demoRaw,
                    spec_prodchars=spec_prodchars,
                    spec_prodchars_0=spec_prodchars_0,
                    spec_fixedEffects=[:Firm_Market_Age])
p_est, model, fval = spec3
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estresults_fe_rc_$spec-$rundate.jld2"
@save file p_est
# @load file spec3
println("Save Results")

p_est, m, fval = spec3
println(p_est[1:20])
P_mat[:,3] = p_est[1:20]

# Fit into model
par0 = parDict(m,p_est)
individual_values!(m,par0)
individual_shares(m,par0)
#
AsVar, stdErr,t_stat, stars = res_process_ll(m,p_est)


out1 = DataFrame(pars=p_est,se=stdErr,ts=t_stat,sig=stars)
file1 = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estimationresults_$spec-$rundate.csv"
CSV.write(file1,out1)

out2 = DataFrame(delta=m.deltas,prods=m.prods)
file2 = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/deltaresults_$spec-$rundate.csv"
CSV.write(file2,out2)


#### Run Specification 4 ####
println("Run Specification 4")
spec = "FNCA"
spec4 = run_specification(df,df_mkt,df_risk,
                    haltonDim = halton_draws,
                    spec_demoRaw=spec_demoRaw,
                    spec_prodchars=spec_prodchars,
                    spec_prodchars_0=spec_prodchars_0,
                    spec_fixedEffects=[:Firm_Market_Cat_Age])
p_est, model, fval = spec4
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estresults_fe_rc_$spec-$rundate.jld2"
@save file p_est
# @load file spec4
println("Save Results")

p_est, m, fval = spec4
println(p_est[1:20])
P_mat[:,4] = p_est[1:20]

# Fit into model
par0 = parDict(m,p_est)
individual_values!(m,par0)
individual_shares(m,par0)
#
AsVar, stdErr,t_stat, stars = res_process_ll(m,p_est)

out1 = DataFrame(pars=p_est,se=stdErr,ts=t_stat,sig=stars)
file1 = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estimationresults_$spec-$rundate.csv"
CSV.write(file1,out1)

out2 = DataFrame(delta=m.deltas,prods=m.prods)
file2 = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/deltaresults_$spec-$rundate.csv"
CSV.write(file2,out2)

#### Full Parameter Set
file_all = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estimationresults_spec_comp_$rundate.csv"
CSV.write(file_all,DataFrame(P_mat))
#
#### Load Results ####
# rundate = "2018-08-25"
# file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estimationresults_fe_rc_$rundate.jld"
# flag, fval, p_est = load(file)["est_res"]
#
# rundate = "_2018-09-01"
# spec = "spec4"
# file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estresults_fe_rc_$spec$rundate.jld"
# p_est, m, fval = load(file)["$spec"]
# println(p_est[1:15])


# p_est = [-3.13731, -3.66356, -4.17707, 1.28711, -4.07659, -2.71348, 6.95234, 1.62123, 1.38525, 1.76242, 2.19671, -0.262142, -0.0726661, 0.436289, -0.00440429, -1.05899, -1.26554, -2.03655, -1.29205, -2.45016, -4.87626, -2.82076, -1.98936, -1.08366, -7.1579, 1.47115, -1.50611, 0.45754, -1.24011, -2.79319, -0.826092, -0.365338, -1.93863, -0.208294, -0.889068, -2.43939, -0.482924, -3.27541, -0.0832024, 5.04283, 1.18866, -5.64587, -1.4914, -4.14378, 0.0746764, -4.63658, -1.09026, -0.0150545, -0.959422, -2.73396, 0.244816, 1.08502, -0.997056, 0.850759, -7.69697, 1.36272, -2.83583, -2.97174, -7.16544, -0.510894, 1.07375, -2.01001, -1.86915, -2.39802, -0.105112, -2.45296, -3.23003, -4.05812, -1.39944, 3.05908]

# # Structre the data
# c = ChoiceData(df,df_mkt,df_risk,
#                     demoRaw=spec_demoRaw,
#                     prodchars=spec_prodchars,
#                     prodchars_0=spec_prodchars_0,
#                     fixedEffects=[:Firm])
# #
# # # Fit into model
# m = InsuranceLogit(c,1000)
# par0 = parDict(m,p_est)
# individual_values!(m,par0)
# individual_shares(m,par0)

#
# AsVar, stdErr,t_stat, stars = res_process(m,p_est)
#
# out1 = DataFrame(pars=p_est,se=stdErr,ts=t_stat,sig=stars)
# file1 = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estimationresults_$rundate.csv"
# CSV.write(file1,out1)
#
# out2 = DataFrame(delta=m.deltas,prods=m.prods)
# file2 = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/deltaresults_$rundate.csv"
# CSV.write(file2,out2)
