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
spec_prodchars=[:Price,:AV,:AV_old]
spec_prodchars_0=[:Price,:AV,:AV_old]

rundate = Dates.today()

# #### Run Specification 1 ####
# println("Run Specification 1")
spec1 = run_specification(df,df_mkt,
                    haltonDim = halton_draws,
                    spec_demoRaw=spec_demoRaw,
                    spec_prodchars=spec_prodchars,
                    spec_prodchars_0=spec_prodchars_0,
                    spec_fixedEffects=[:Firm])

file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estresults_fe_rc_spec1_$rundate.jld"
save(file,"spec1",spec1)

#### Run Specification 2 ####
println("Run Specification 2")
spec2 = run_specification(df,df_mkt,
                    haltonDim = halton_draws,
                    spec_demoRaw=spec_demoRaw,
                    spec_prodchars=spec_prodchars,
                    spec_prodchars_0=spec_prodchars_0,
                    spec_fixedEffects=[:Firm_Market_Cat])

file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estresults_fe_rc_spec2_$rundate.jld"
save(file,"spec2",spec2)

#### Run Specification 3 ####
println("Run Specification 3")
spec3 = run_specification(df,df_mkt,
                    haltonDim = halton_draws,
                    spec_demoRaw=spec_demoRaw,
                    spec_prodchars=spec_prodchars,
                    spec_prodchars_0=spec_prodchars_0,
                    spec_fixedEffects=[:Firm_Market_Age])

file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estresults_fe_rc_spec3_$rundate.jld"
save(file,"spec3",spec3)


#### Run Specification 3 ####
println("Run Specification 4")
spec4 = run_specification(df,df_mkt,
                    haltonDim = halton_draws,
                    spec_demoRaw=spec_demoRaw,
                    spec_prodchars=[:Price],
                    spec_prodchars_0=spec_prodchars_0,
                    spec_fixedEffects=[:Firm_Market_Cat_Age])

file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estresults_fe_rc_spec4_$rundate.jld"
save(file,"spec4",spec4)


# #### Load Results ####
# # rundate = "2018-05-12"
# # file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estresults_fe_rc_spec3_small_$rundate.jld"
# # flag, fval, p_est = load(file)["spec3"]
#
# # # Structre the data
# c = ChoiceData(df,df_mkt,
#                     demoRaw=spec_demoRaw,
#                     prodchars=spec_prodchars,
#                     prodchars_0=spec_prodchars_0,
#                     fixedEffects=[:Firm,:Market])
# #
# # # Fit into model
# # m = InsuranceLogit(c,250)
#
# AsVar, stdErr,t_stat, stars = res_process(model3,p_est3)
#
# out1 = DataFrame(pars=p_est3,se=stdErr,ts=t_stat,sig=stars)
# file1 = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estimationresults_$rundate.csv"
# CSV.write(file1,out1)
#
# out2 = DataFrame(delta=m.deltas,prods=m.prods)
# file2 = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/deltaresults_$rundate.csv"
# CSV.write(file2,out2)
