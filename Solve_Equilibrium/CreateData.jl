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
include("$load_path/Firm_Side/Firm_Inner_Loop.jl")

#Equilibrium Functions
# include("predictionData_New.jl")
include("EQ_RandomCoefficients.jl")

#Load Data
include("EQ_load.jl")


rundate = "2019-06-23"

chdf = ChoiceData(df,df_mkt,df_risk;
    demoRaw=[:AgeFE_31_39,
            :AgeFE_40_51,
            :AgeFE_52_64,
            :Family,
            :LowIncome],
    prodchars=[:Price,:constant,:AV,:Big],
    prodchars_0=[:constant,:AV,:Big],
    fixedEffects=[:Firm_Market_Cat],
    wgt=[:PERWT])

m = InsuranceLogit(chdf,500)

costdf = MC_Data(df,mom_firm,mom_metal,mom_age,mom_age_no,mom_risk;
                baseSpec=[:AGE,:AV_std,:AV_diff],
                fixedEffects=[:Firm_ST])
chdf = 0.0


## Load Demand Estimation
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/estimationresults_stage2_$rundate.jld2"
@load file p_stg2
p_est = copy(p_stg2)

#### Compute Demand Estimation
par_est_dem = parDict(m,p_est,no2Der=true)
individual_values_nonprice!(m,par_est_dem)
individual_values_price!(m,par_est_dem)
individual_shares(m,par_est_dem)

### Load Marginal Cost Estimation
file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg2_$rundate.jld2"
# @load file est_stg2
# p_stg2 ,fval = est_stg2
# mc_est = copy(p_stg2)
@load file est_stg2
flag,fval,p_stg2 = est_stg2
mc_est = fit_firm_moments(p_stg2,par_est_dem,m,costdf)


#### Compute Marginal Costs
par_est_mc = parMC(mc_est,par_est_dem,m,costdf)
individual_costs(m,par_est_mc)

## Size of Data
(K,M) = size(m.data.data)
(N,Q) = size(m.draws)

output_data = Matrix{Float64}(undef,Int(M*N/2),23)

### Count 0 HCC Individuals ####
pos_HCC = sum(m.draws.>0,dims=1)


person_long = person(m.data)
# product_long = product(m.data)
product_long = Float64.(df[:Product_std])
price_long = m.data[:Price]
riskInd_long = Int.(rInd(m.data))
riskIndS_long = Int.(rIndS(m.data))
ageHCC_long = ageHCC(m.data)
otherData = convert(Matrix{Float64},df[[:AGE,:ageRate,:ageRate_avg,:AV,:Gamma_j,:Mandate,:subsidy,:IncomeCont,:MEMBERS]])
density_long = df[:mkt_density]
wgts_long = df[:PERWT]
Catas_long = Float64.(df[:METAL].=="CATASTROPHIC")
C_AV_long = exp.(mc_est[2].*df[:AV_std])

state_index = Vector{String}(undef,Int(M*N/2))
state_long = df[:ST]
ind = [0]
for i in 1:M
    if i%1000==0
        println(i)
    end
    zero_draw_flag = 0
    r_ind = riskInd_long[i]
    r_indS = riskIndS_long[i]
    per_raw = person_long[i]
    prd = product_long[i]
    oth = otherData[i,:]
    catas = Catas_long[i]
    ageHCC = ageHCC_long[i]
    s_pred = par_est_dem.s_hat[i]
    p_base = price_long[i]


    dens_raw = density_long[i]/N
    wgt_raw = wgts_long[i]/N

    C_nonrisk = par_est_mc.C_nonrisk[i]
    C_AV = C_AV_long[i]
    for n in 1:N
        risk_draw = m.draws[n,r_ind]
        risk_HCC = m.draws[n,r_indS]
        if risk_HCC>0
            mkt_dens = dens_raw
            wgt = wgt_raw
        elseif zero_draw_flag==0
            mkt_dens = dens_raw*(N-pos_HCC[r_ind])
            wgt = wgt_raw*(N-pos_HCC[r_ind])
            zero_draw_flag = 1
        else
            continue
        end
        ind[1]+=1

        per = per_raw*N + n
        R = risk_draw + ageHCC
        C = C_nonrisk*par_est_mc.risks[n,r_indS]
        C_nonAV = C/C_AV
        α = par_est_dem.α[n,i]
        μ_ij_nonprice = par_est_dem.μ_ij_nonprice[n,i]
        μ_ij = par_est_dem.μ_ij[n,i]

        row = vcat([per,prd,catas,mkt_dens,wgt],oth,[α,μ_ij_nonprice,μ_ij,R,risk_HCC,C,C_nonAV,s_pred,p_base])
        output_data[ind[1],:] = row
        state_index[ind[1]] = state_long[i]
    end
end
output_data = output_data[1:ind[1],:]
state_index = state_index[1:ind[1]]

states = unique(df[:ST])
header = Symbol.(["Person","Product","Catastrophic","mkt_density","PERWT",
"AGE","ageRate","ageRate_avg","AV","Gamma_j","Mandate","subsidy","IncomeCont","MEMBERS",
"alpha","non_price_util","util","R","R_HCC","C","C_nonAV","s_base","p_base"])
for st in states
    println(st)
    file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Equilibrium_Data/estimated_Data_$st$rundate.csv"
    st_subset = state_index.==st
    df_st = convert(DataFrame,output_data[st_subset,:])
    names!(df_st,header)
    # sort!(df_st,(:Person,:Product))
    sort!(df_st,(:Product,:Person))
    CSV.write(file,df_st)
end
