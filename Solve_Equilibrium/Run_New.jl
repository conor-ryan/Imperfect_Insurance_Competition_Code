using DataFrames
using CSV
using LinearAlgebra
using Statistics
using BenchmarkTools
using JLD2
using PyPlot
########################################################################
#################### Loading and Cleaning Data #########################
########################################################################
load_path = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Code/"
# Data Structure
include("$load_path/Demand_Estimation/InsChoiceData.jl")
include("$load_path/Demand_Estimation/Halton.jl")
include("$load_path/Demand_Estimation/RandomCoefficients.jl")
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


rundate = "2019-06-25"

chdf = ChoiceData(df,df_mkt,df_risk;
    demoRaw=[:AgeFE_31_39,
            :AgeFE_40_51,
            :AgeFE_52_64,
            :Family,
            :LowIncome],
    prodchars=[:Price,:constant,:AV,:Big],
    prodchars_0=[:constant,:AV,:Big],
    fixedEffects=[:Firm_Market_Cat],
    wgt=[:PERWT],
    constMoments=false)

model = InsuranceLogit(chdf,500)

costdf = MC_Data(df,mom_firm,mom_metal,mom_age,mom_age_no,mom_risk;
                baseSpec=[:AGE,:AV_std],
                fixedEffects=[:Firm_ST],
                constMoments=true)



## Load Demand Estimation
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/GMM_Estimate_FMC-$rundate-stg2.jld2"
@load file p_stg2
p_est = copy(p_stg2)

#### Compute Demand Estimation
par_dem = parDict(model,p_est,no2Der=true)
individual_values!(model,par_dem)
individual_shares(model,par_dem)

### Load Marginal Cost Estimation
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg2_$rundate.jld2"
@load file p_stg2
# p_stg2 = est_stg1[3]
# p_stg2 = fit_firm_moments(p_stg2,par_dem,m,costdf)
mc_est = copy(p_stg2)

#### Compute Marginal Costs
par_cost = parMC(mc_est,par_dem,model,costdf)



#### Solve Equilibrium ####
firm = firmData(model,df,eq_mkt,par_dem,par_cost)
m = model
f = firm

P_Obs = Vector{Float64}(undef,length(m.prods))
P_Base = Vector{Float64}(undef,length(m.prods))
P_RA = Vector{Float64}(undef,length(m.prods))
P_man = Vector{Float64}(undef,length(m.prods))
P_RAman = Vector{Float64}(undef,length(m.prods))
P_Base_m = Vector{Float64}(undef,length(m.prods))
P_RA_m = Vector{Float64}(undef,length(m.prods))
P_man_m = Vector{Float64}(undef,length(m.prods))
P_RAman_m = Vector{Float64}(undef,length(m.prods))

P_Obs[:] = f.P_j[:]

#### Solve Baseline - With Risk Adjustment and Mandate ####
solve_model!(m,f,sim="RA")
P_Base[:] = f.P_j[:]
f.ownMat = f.ownMat_merge
solve_model!(m,f,sim="RA")
P_Base_m[:] = f.P_j[:]

#### Solve without Risk Adjustment ####
f.P_j[:] = P_Obs[:]
solve_model!(m,f,sim="Base")
P_RA[:] = f.P_j[:]
f.ownMat = f.ownMat_merge
solve_model!(m,f,sim="Base")
P_RA_m[:] = f.P_j[:]

#### Solve without mandate ####
f.P_j[:] = P_Obs[:]
f[:Mandate].=0.0
solve_model!(m,f,sim="RA")
P_man[:] = f.P_j[:]
f.ownMat = f.ownMat_merge
solve_model!(m,f,sim="RA")
P_man_m[:] = f.P_j[:]

#### Solve without mandate NOR risk adjustment  ####
f.P_j[:] = P_Obs[:]
f[:Mandate].=0.0
solve_model!(m,f,sim="Base")
P_man[:] = f.P_j[:]
f.ownMat = f.ownMat_merge
solve_model!(m,f,sim="Base")
P_man_m[:] = f.P_j[:]




output =  DataFrame(Products=model.prods,
                    Price_base=P_Base,
                    Price_RA =P_RA,
                    Price_man=P_man,
                    Price_RAman=P_RAman,
                    Price_base_m=P_Base_m,
                    Price_RA_m =P_RA_m,
                    Price_man_m=P_man_m,
                    Price_RAman_m=P_RAman_m)

file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/solvedEquilibrium_$rundate.csv"
CSV.write(file,output)






####### TESTING GROUND #####
#
# # evaluate_model!(model,firm,foc_check=true)
#
# solve_model!(model,firm)
#
#
# TotalCosts = firm.poolMat*firm.C_j
# PooledCosts = firm.poolMat*(firm.PC_j.*firm.S_j)
# dAdj_dp = sum(firm.dCdp_j,dims=2)./PooledCosts - (sum(firm.dCdp_pl_j,dims=2)./PooledCosts).*firm.Adj_j
#
#
# R, C, S, PC = compute_profit(model,firm)
#
# dR, dC,dS,dPC,dAdj = test_MR(model,firm)
# ind = findall(dS.!=0.0)
# println(maximum(abs.(firm.dRdp_j[57,ind] - dR[ind])))
# println(maximum(abs.(firm.dCdp_j[57,ind] - dC[ind])))
# println(maximum(abs.(firm.dSdp_j[57,ind] - dS[ind])))
# println(maximum(abs.(firm.dCdp_pl_j[57,ind] - dPC[ind])))
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
# using Profile
# Profile.init(n=10^8,delay=.001)
# Profile.clear()
# Juno.@profile evaluate_model!(model,firm)
# Juno.profiletree()
# Juno.profiler()
