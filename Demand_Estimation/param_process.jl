using DataFrames
using CSV
using JLD2
# Data Structure
include("InsChoiceData.jl")
#Halton Draws
include("Halton.jl")
include("RandomCoefficients_nonzero.jl")
include("RandomCoefficients_3der.jl")
include("Contraction.jl")
include("Log_Likehood.jl")
include("RiskMoments.jl")
include("utility.jl")
include("Estimate_Basic.jl")
include("Specification_Run.jl")


rundate = "_2018-12-23"

P_mat = Matrix{Float64}(undef,15,4)

for (s,spec) in enumerate(["spec1","spec2","spec3","spec4"])
    println(spec)
    file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estresults_fe_rc_$spec$rundate.jld2"
    #est = load(file)
    @load file "$spec"
    p_est, m, fval = spec
    # p_est, m, fval = load(file)["$spec"]
    println(p_est[1:16])
    P_mat[:,s] = p_est[1:16]


    # Fit into model
    par0 = parDict(m,p_est)
    individual_values!(m,par0)
    individual_shares(m,par0)

    # hess = Matrix{Float64}(length(p_est),length(p_est))
    # grad = Vector{Float64}(length(p_est))
    # ll = log_likelihood!(hess,grad,m,p_est)

    #
    AsVar, stdErr,t_stat, stars = res_process(m,p_est)

    out1 = DataFrame(pars=p_est,se=stdErr,ts=t_stat,sig=stars)
    file1 = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estimationresults_$spec$rundate.csv"
    CSV.write(file1,out1)

    out2 = DataFrame(delta=m.deltas,prods=m.prods)
    file2 = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/deltaresults_$spec$rundate.csv"
    CSV.write(file2,out2)
end
file_all = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estimationresults_spec_comp$rundate.csv"
CSV.write(file_all,DataFrame(P_mat))

#
#
#
# rundate = "2018-09-19"
# file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estimationresults_stage3_$rundate.jld"
# flag,fval,p_est = load(file)["est_res"]
#
#
# include("load.jl")
# c = ChoiceData(df,df_mkt,df_risk,
#                 demoRaw=[:AgeFE_31_39,
#                         :AgeFE_40_51,
#                         :AgeFE_52_64,
#                         :Family,
#                         :LowIncome],
#                 prodchars=[:Price,:AV,:Big],
#                 prodchars_0=[:Price,:AV,:Big],
#                 fixedEffects=[:Firm])
# #
# # # Fit into model
# m = InsuranceLogit(c,100)
#
# AsVar, stdErr,t_stat, stars = res_process(m,p_est)
#
# # out1 = DataFrame(pars=p_est,se=stdErr,ts=t_stat,sig=stars)
# # file1 = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/estimationresults_$rundate.csv"
# # CSV.write(file1,out1)
# #
# # out2 = DataFrame(delta=m.deltas,prods=m.prods)
# # file2 = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/deltaresults_$rundate.csv"
# # CSV.write(file2,out2)
#
# out1 = DataFrame(pars=p_est,se=stdErr,ts=t_stat,sig=stars)
# file1 = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estimationresults_gmm_spec2_$rundate.csv"
# CSV.write(file1,out1)
#
# out2 = DataFrame(delta=m.deltas,prods=m.prods)
# file2 = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/deltaresults_gmm_spec2_$rundate.csv"
# CSV.write(file2,out2)
