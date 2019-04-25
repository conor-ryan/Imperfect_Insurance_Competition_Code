using DataFrames
using CSV
using LinearAlgebra
using Statistics
using BenchmarkTools
using JLD2
########################################################################
#################### Loading and Cleaning Data #########################
########################################################################
load_path = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Code/"
# Data Structure
include("$load_path/Demand_Estimation/InsChoiceData.jl")
include("$load_path/Demand_Estimation/Halton.jl")
include("EQ_RandomCoefficients.jl")
include("$load_path/Demand_Estimation/utility.jl")
include("$load_path/Demand_Estimation/Contraction.jl")
include("$load_path/Firm_Side/MC_parameters.jl")

#Equilibrium Functions
# include("predictionData_New.jl")
include("EQ_RandomCoefficients.jl")

#Load Data
include("EQ_load.jl")


rundate = "2019-03-12"


# AK_df = df[(df[:ST].=="AK"),:]
# AK_df_mkt = df_mkt[df_mkt[:STATE].=="AK",:]
# cd("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/")
# st = "AK"
# file2 = "Intermediate_Output/Equilibrium_Data/estimated_prodData_$st.csv"
# AK_eq_df_mkt = CSV.read(file2)

# chdf = ChoiceData(df,df_mkt,df_risk;
#     demoRaw=[:AgeFE_31_39,
#             :AgeFE_40_51,
#             :AgeFE_52_64,
#             :Family,
#             :LowIncome],
#     prodchars=[:Price,:AV,:Big],
#     prodchars_0=[:AV,:Big],
#     fixedEffects=[:Firm_Market],
#     wgt=[:PERWT])

chdf = ChoiceData(df,df_mkt,df_risk;
demoRaw=[:AgeFE_31_39,
    :AgeFE_40_51,
    :AgeFE_52_64,
    :Family,
    :LowIncome],
prodchars=[:Price,:AV,:Big],
prodchars_0=[:AV,:Big],
fixedEffects=[:Firm_Market])

m = InsuranceLogit(chdf,1000)

costdf = MC_Data(df,mom_avg,mom_age,mom_risk;
                baseSpec=[:AGE,:AV_std,:AV_diff],
                fixedEffects=[:Firm_ST],
                constMoments=false)



## Load Demand Estimation
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estimationresults_stage2_$rundate.jld2"
@load file p_stg2
p_est = copy(p_stg2)

#### Compute Demand Estimation
par_est_dem = parDict(m,p_est)
individual_values_nonprice!(m,par_est_dem)
individual_values_price!(m,par_est_dem)

### Load Marginal Cost Estimation
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg2_$rundate.jld2"
@load file est_stg2
p_stg2 ,fval = est_stg2
mc_est = copy(p_stg2)

#### Compute Marginal Costs
par_est_mc = parMC(mc_est,par_est_dem,m,costdf)
individual_costs(m,par_est_mc)



####### TESTING GROUND #####
individual_values_price!(m,par_est_dem)
individual_shares(m,par_est_dem)


app = iterate(eachperson(m.data),1)[1]
