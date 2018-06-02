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

halton_draws = 250
spec_demoRaw = [:AgeFE_31_40,
        :AgeFE_41_50,
        :AgeFE_51_64,
        :Family,
        :LowIncome]
spec_prodchars=[:Price,:AV]
spec_prodchars_0=[:Price,:AV]

rundate = Dates.today()

# #### Run Specification 1 ####
# println("Run Specification 1")
# p_est1, model1, flags1 = run_specification(df,df_mkt,
#                     haltonDim = halton_draws,
#                     spec_demoRaw=spec_demoRaw,
#                     spec_prodchars=spec_prodchars,
#                     spec_prodchars_0=spec_prodchars_0,
#                     spec_fixedEffects=[:Firm])
#
#
# file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estresults_fe_rc_spec1_$rundate.jld"
# save(file,"spec1",p_est1)
#
# # #### Run Specification 2 ####
# println("Run Specification 2")
# p_est2, model2, flags2 = run_specification(df,df_mkt,
#                     haltonDim = halton_draws,
#                     spec_demoRaw=spec_demoRaw,
#                     spec_prodchars=spec_prodchars,
#                     spec_prodchars_0=spec_prodchars_0,
#                     spec_fixedEffects=[:Firm,:Market])
#
# file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estresults_fe_rc_spec2_$rundate.jld"
# save(file,"spec2",p_est2)

#### Run Specification 3 ####
println("Run Specification 3")
p_est3, model3, flags3 = run_specification(df,df_mkt,
                    haltonDim = halton_draws,
                    spec_demoRaw=spec_demoRaw,
                    spec_prodchars=spec_prodchars,
                    spec_prodchars_0=spec_prodchars_0,
                    spec_fixedEffects=[:Firm_Market_Cat])

file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estresults_fe_rc_spec3_small_$rundate.jld"
save(file,"spec3",p_est3)


#### Run Specification 3 ####
# println("Run Specification 4")
# p_est4, model4, flags4 = run_specification(df,df_mkt,
#                     haltonDim = halton_draws,
#                     spec_demoRaw=spec_demoRaw,
#                     spec_prodchars=[:Price],
#                     spec_prodchars_0=spec_prodchars_0,
#                     spec_fixedEffects=[:Product])
#
# file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estresults_fe_rc_spec4_small_$rundate.jld"
# save(file,"spec4",p_est4)


#### Load Results ####
# rundate = "2018-05-12"
# file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estresults_fe_rc_spec3_small_$rundate.jld"
# flag, fval, p_est = load(file)["spec3"]

# # Structre the data
c = ChoiceData(df,df_mkt,
                    demoRaw=spec_demoRaw,
                    prodchars=spec_prodchars,
                    prodchars_0=spec_prodchars_0,
                    fixedEffects=[:Firm,:Market])
#
# # Fit into model
# m = InsuranceLogit(c,250)

AsVar, stdErr,t_stat, stars = res_process(model3,p_est3)

out1 = DataFrame(pars=p_est3,se=stdErr,ts=t_stat,sig=stars)
file1 = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estimationresults_$rundate.csv"
CSV.write(file1,out1)

out2 = DataFrame(delta=m.deltas,prods=m.prods)
file2 = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/deltaresults_$rundate.csv"
CSV.write(file2,out2)
