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




####### TESTING GROUND #####
firm = firmData(model,df,eq_mkt,par_dem,par_cost)

evaluate_model!(model,firm)

R, C, S, PC = compute_profit(model,firm)

dR, dC,dS,dPC = test_MR(model,firm)

ind = findall(dPC.!=0.0)


P_std, P_RA = evaluate_FOC(firm)

MR, MC, MC_pl = prof_margin(firm)


figure()
plot(-MR,-MC,linestyle="",marker="o")
plot([0,3e5],[0,3e5],linestyle="-")
gcf()

figure()
plot(-MR,-MC_pl,linestyle="",marker="o")
plot([0,3e5],[0,3e5],linestyle="-")
gcf()


figure()
plot(firm.P_j[firm.prods],P_RA[firm.prods],linestyle="",marker="o")
gcf()



using Profile
Profile.init(n=10^8,delay=.001)
Profile.clear()
Juno.@profile evaluate_model!(model,firm)
Juno.profiletree()
Juno.profiler()
