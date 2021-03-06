using DataFrames
using CSV
using LinearAlgebra
using Statistics
using BenchmarkTools
using JLD2
using Dates
########################################################################
#################### Loading and Cleaning Data #########################
########################################################################
load_path = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Code/"
# Data Structure
include("$load_path/Demand_Estimation/InsChoiceData.jl")
include("$load_path/Demand_Estimation/Halton.jl")
include("$load_path/Demand_Estimation/RandomCoefficients.jl")
include("$load_path/Demand_Estimation/RiskMoments.jl")
include("$load_path/Demand_Estimation/utility.jl")
include("$load_path/Demand_Estimation/Contraction.jl")
include("$load_path/Firm_Side/MC_parameters.jl")
include("$load_path/Firm_Side/Firm_Inner_Loop.jl")

#Equilibrium Functions
include("predictionData.jl")
include("EvaluateModel.jl")
include("PriceUpdate.jl")
include("FirmFunctions.jl")
#Load Data
include("EQ_load.jl")


rundate = "2019-10-19"
spec = "FMC"
mark_the_output_date = Dates.today()
println("Running spec $rundate on $mark_the_output_date")

df[:High_small] = df[:HighRisk].*df[:Small]
#### Build Model ####
# Structre the data
chdf = ChoiceData(df,df_mkt,df_risk;
    demoRaw=[:AgeFE_31_39,
            :AgeFE_40_51,
            :AgeFE_52_64,
            :Family,
            :LowIncome],
    prodchars=[:Price,:constant,:AV,:HighRisk,:Small,:High_small],
    prodchars_0=[:constant,:AV,:HighRisk,:Small,:High_small],
    fixedEffects=[:Firm_Market_Cat],
    wgt=[:PERWT])

model = InsuranceLogit(chdf,500)

costdf = MC_Data(df,mom_firm,mom_metal,mom_age,mom_age_no,mom_risk,mom_ra;
                baseSpec=[:AvgAge,:AV_std],
                fixedEffects=[:Firm_ST],
                constMoments=true)



## Load Demand Estimation
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/GMM_Estimate_$spec-$rundate-stg2.jld2"
@load file p_stg2
p_est = copy(p_stg2)


if length(p_stg2)!=model.parLength[:All]
    println(length(p_stg2))
    println(model.parLength[:All])
    error("Parameter Vector Not Quite Right")
end


# r = calc_risk_moments(model,p_est)
# println("Risk Moments are $r")

#### Compute Demand Estimation
par_dem = parDict(model,p_est,no2Der=true)
individual_values!(model,par_dem)
individual_shares(model,par_dem)


# r = calc_risk_moments(model,par_dem)
# println("Risk Moments are $r")

### Load Marginal Cost Estimation
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg2_$spec-$rundate.jld2"
@load file p_stg2
# p_stg2 = est_stg1[3]
# p_stg2[3]/=2
p_stg3 = fit_firm_moments(p_stg2[1:3],par_dem,model,costdf,itrFirms=true)
mc_est = copy(p_stg2)


# par = parMC(mc_est,par_dem,model,costdf)
# individual_costs(model,par)
# moments = costMoments(costdf,model,par)
# W = Matrix(1.0I,costdf.mom_length,costdf.mom_length)
# GMM_objective(mc_est,par_dem,model,costdf,W)

#### Compute Marginal Costs
par_cost = parMC(mc_est,par_dem,model,costdf)


# eq_mkt[:Firm_ST] = eq_mkt[:Firm].*"_".*eq_mkt[:ST]
# df[:Firm_ST] = df[:Firm].*"_".*df[:ST]
# firms = unique(eq_mkt[:Firm_ST])
# _firmProdDict = Dict{String,Array{Int64,1}}()
# _firmPerDict = Dict{String,Array{Int64,1}}()
# for f in firms
#     _firmProdDict[f] = eq_mkt[:Product][eq_mkt[:Firm_ST].==f]
#     _firmPerDict[f] = findall(df[:Firm_ST].==f)
# end


#### Solve Equilibrium ####
firm = firmData(model,df,eq_mkt,par_dem,par_cost)
evaluate_model!(model,firm,"All",foc_check=true)

#
# r,t = calc_risk_moments(model,firm.par_dem)
# println("Risk Moments are $r,\n $t")
# f = firm
# m = model



moments = costMoments(costdf,model,firm.par_cost)

println("Check Margins")
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/checkMargins_$rundate.csv"
checkMargin(model,firm,file)

println("Solve Equilibrium")
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/solvedEquilibrium_$rundate.csv"
solveMain(model,firm,file)


# m = model
# f = firm
#
# solve_model!(m,f,sim="RA")



# evaluate_model!(m,f,"All")
#
# P,P_RA = evaluate_FOC(f)






####### TESTING GROUND #####
age_long = firm[:ageRate]
mem_long = firm[:MEMBERS]
alpha_long = par_dem.β_0[1] .+ par_dem.β[1,:]'*demoRaw(model.data)
price_long = prodchars(model.data)[1,:]
elas = Vector{Float64}(undef,length(alpha_long))
for i in 1:length(elas)
    elas[i] = age_long[i]/mem_long[i]*alpha_long[i]*price_long[i]*(1 - par_dem.s_hat[i])
end


# using PyPlot
#
firm = firmData(model,df,eq_mkt,par_dem,par_cost)
evaluate_model!(model,firm,"All",foc_check=false)


# solve_model!(model,firm,sim="RA")
# P_Base = copy(firm.P_j[:])
# evaluate_model!(model,firm,"All")
# S_Base = copy(firm.S_j[:])


evaluate_model!(model,firm,"All")
ind_GA = firm._prodSTDict["GA"]
ind_GA = ind_GA[inlist(ind_GA,firm.prods)]
# ind_GA = ind_GA[.!(inlist(ind_GA,firm.catas_prods))]
R, C, S, PC, Adj = compute_profit(model,firm)
MR, MC, MC_pl = prof_margin_raw(firm,ind_GA)
P_std, P_RA, MR2, MC2 = evaluate_FOC(firm,"GA")




dR, dC,dS,dPC,dAdj,test = test_MR(model,firm)
ind = ind_GA
ind = findall(dPC.!=0.0)
println(maximum(abs.(firm.dRdp_j[1,ind] - dR[ind])))
println(maximum(abs.(firm.dCdp_j[1,ind] - dC[ind])))
println(maximum(abs.(firm.dSdp_j[1,ind] - dS[ind])))
println(maximum(abs.(firm.dCdp_pl_j[1,ind] - dPC[ind])))
#
#
#
# P_std, P_RA = evaluate_FOC(firm)
#
# MR, MC, MC_pl = prof_margin(firm)
#
# # MR = MR./firm.Mkt_j
# # MC = MC./firm.Mkt_j
# # MC_pl = MC_pl./firm.Mkt_j
#
# figure()
# plot(firm.P_j[firm.prods]-MR,MC_pl,linestyle="",marker="o")
# plot([0,500],[0,500],linestyle="-")
# gcf()
#
# figure()
# plot(MR,MC,linestyle="",marker="o")
# # plot([0,0.6],[0,0.6],linestyle="-")
# gcf()
#
# figure()
# plot(MR,MC_pl,linestyle="",marker="o")
# plot([0,3e5],[0,3e5],linestyle="-")
# gcf()
#
# figure()
# plot(MC,firm.C_j[firm.prods],linestyle="",marker="o")
# plot([0,5e3],[0,5e3],linestyle="-")
# gcf()
#
# figure()
# plot(MC_pl,firm.PC_j[firm.prods],linestyle="",marker="o")
# plot([0,5e3],[0,5e3],linestyle="-")
# gcf()
#
#
#
#
# figure()
# plot(firm.P_j[firm.prods],P_RA[firm.prods],linestyle="",marker="o")
# gcf()
#
#
#
using Profile
Profile.init(n=10^8,delay=.001)
Profile.clear()
Juno.@profile evaluate_model!(model,firm)
Juno.profiletree()
Juno.profiler()
