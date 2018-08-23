using JLD
using CSV

# Data Structure
include("InsChoiceData.jl")

#Halton Draws
include("Halton.jl")

# Random Coefficients MLE
include("RandomCoefficients.jl")
include("RandomCoefficients_2der_nonzero.jl")
include("Contraction.jl")
include("Log_Likehood.jl")
include("RiskMoments.jl")
include("Estimate_Basic.jl")
include("Estimate_GMM.jl")
include("Specification_Run.jl")
# Load the Data
include("load.jl")

println("Code Loaded")


#### General Specification ####

halton_draws = 1000
spec_demoRaw = [:AgeFE_31_39,
        :AgeFE_40_51,
        :AgeFE_52_64,
        :Family,
        :LowIncome]
spec_prodchars=[:Price,:AV,:Big]
spec_prodchars_0=[:Price,:AV,:Big]

rundate = Dates.today()

# # #### Run Specification 1 ####
# # println("Run Specification 1")
# spec1 = run_specification(df,df_mkt,
#                     haltonDim = halton_draws,
#                     spec_demoRaw=spec_demoRaw,
#                     spec_prodchars=spec_prodchars,
#                     spec_prodchars_0=spec_prodchars_0,
#                     spec_fixedEffects=[:Firm])
#
# file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estresults_fe_rc_spec1_$rundate.jld"
# save(file,"spec1",spec1)
#
# #### Run Specification 2 ####
# println("Run Specification 2")
# spec2 = run_specification(df,df_mkt,
#                     haltonDim = halton_draws,
#                     spec_demoRaw=spec_demoRaw,
#                     spec_prodchars=spec_prodchars,
#                     spec_prodchars_0=spec_prodchars_0,
#                     spec_fixedEffects=[:Firm_Market_Cat])
#
# file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estresults_fe_rc_spec2_$rundate.jld"
# save(file,"spec2",spec2)
#
# #### Run Specification 3 ####
# println("Run Specification 3")
# spec3 = run_specification(df,df_mkt,
#                     haltonDim = halton_draws,
#                     spec_demoRaw=spec_demoRaw,
#                     spec_prodchars=spec_prodchars,
#                     spec_prodchars_0=spec_prodchars_0,
#                     spec_fixedEffects=[:Firm_Market_Age])
#
# file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estresults_fe_rc_spec3_$rundate.jld"
# save(file,"spec3",spec3)


#### Run Specification 3 ####
println("Run Specification 4")
spec4 = run_specification(df,df_mkt,
                    haltonDim = halton_draws,
                    spec_demoRaw=spec_demoRaw,
                    spec_prodchars=spec_prodchars,
                    spec_prodchars_0=spec_prodchars_0,
                    spec_fixedEffects=[:Firm_Market_Cat_Age])

file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estresults_fe_rc_spec4_$rundate.jld"
save(file,"spec4",spec4)


# #### Load Results ####
rundate = "2018-08-17"
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estimationresults_fe_rc_$rundate.jld"
flag, fval, p_est = load(file)["est_res"]

p_est = [-3.13731, -3.66356, -4.17707, 1.28711, -4.07659, -2.71348, 6.95234, 1.62123, 1.38525, 1.76242, 2.19671, -0.262142, -0.0726661, 0.436289, -0.00440429, -1.05899, -1.26554, -2.03655, -1.29205, -2.45016, -4.87626, -2.82076, -1.98936, -1.08366, -7.1579, 1.47115, -1.50611, 0.45754, -1.24011, -2.79319, -0.826092, -0.365338, -1.93863, -0.208294, -0.889068, -2.43939, -0.482924, -3.27541, -0.0832024, 5.04283, 1.18866, -5.64587, -1.4914, -4.14378, 0.0746764, -4.63658, -1.09026, -0.0150545, -0.959422, -2.73396, 0.244816, 1.08502, -0.997056, 0.850759, -7.69697, 1.36272, -2.83583, -2.97174, -7.16544, -0.510894, 1.07375, -2.01001, -1.86915, -2.39802, -0.105112, -2.45296, -3.23003, -4.05812, -1.39944, 3.05908]

# # Structre the data
c = ChoiceData(df,df_mkt,df_risk,
                    demoRaw=spec_demoRaw,
                    prodchars=spec_prodchars,
                    prodchars_0=spec_prodchars_0,
                    fixedEffects=[:Firm])
#
# # Fit into model
m = InsuranceLogit(c,1000)

AsVar, stdErr,t_stat, stars = res_process(m,p_est)

out1 = DataFrame(pars=p_est,se=stdErr,ts=t_stat,sig=stars)
file1 = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estimationresults_$rundate.csv"
CSV.write(file1,out1)

out2 = DataFrame(delta=m.deltas,prods=m.prods)
file2 = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/deltaresults_$rundate.csv"
CSV.write(file2,out2)
