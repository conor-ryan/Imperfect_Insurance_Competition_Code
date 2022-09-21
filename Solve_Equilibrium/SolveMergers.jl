#Load Data
println("Loading Data...")
include("EQ_load.jl")

# df[:High_small] = df[:HighRisk].*df[:Small]

mark_the_output_date = Dates.today()
println("Running spec $rundate on $mark_the_output_date")

file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg2_$spec-$rundate.jld2"
@load file p_stg2 p_dem_est cost_spec spec_Dict
mc_est = copy(p_stg2)
#### Load Estimation Results ####
# file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/PLL_Estimate_$spec-$rundate-stg1.jld2"
# @load file p_stg1 spec_Dict

#### Build Model ####
println("Rebuild Demand Model...")
# Structre the data
chdf = ChoiceData(df,df_mkt,df_risk,df_transfer;
    product =[:Product_std],
    demoRaw=spec_Dict["demoRaw"],
    prodchars=spec_Dict["prodchars"],
    prodchars_σ=spec_Dict["prodchars_σ"],
    fixedEffects=spec_Dict["fixedEffects"],
    wgt=[:PERWT])

# Fit into model
model = InsuranceLogit(chdf,spec_Dict["haltonDim"])


if length(p_dem_est)!=model.parLength[:All]
    println(length(p_dem_est))
    println(model.parLength[:All])
    error("Parameter Vector Not Quite Right")
end

println("Rebuild Cost Data...")

costdf = MC_Data(df,mom_firm,mom_metal,mom_age,mom_age_no,mom_risk,mom_ra;
                baseSpec=cost_spec,
                fixedEffects=[:Firm_ST],
                constMoments=true)


#### Compute Parameter Objects ####
println("Compute Parameters...")
par_dem = parDict(model,p_dem_est,no2Der=true)
individual_values!(model,par_dem)
individual_shares(model,par_dem)

par_cost = parMC(mc_est,par_dem,model,costdf)


firm = firmData(model,df,eq_mkt,par_dem,par_cost)





function ownerMatrix!(fdata::firmData)
    fdata.ownMat[:].=0.0
    prod_std = fdata.prods
    firm_list = fdata.firm_vector
    for j in prod_std
        f = firm_list[j]
        for i in prod_std
            if (f=="") | (firm_list[i]=="")
                continue
            elseif (f == firm_list[i])
                fdata.ownMat[j,i]=1
            end
        end
    end
    return nothing
end




function ownerMatrix!(fdata::firmData,merging_firms)
    fdata.ownMat[:].=0.0
    prod_std = fdata.prods
    firm_list = fdata.firm_vector
    for j in prod_std
        f = firm_list[j]
        for i in prod_std
            if (f=="") | (firm_list[i]=="")
                continue
            elseif (f == firm_list[i]) | ((f in merging_firms) & (firm_list[i] in merging_firms))
                fdata.ownMat[j,i]=1
            end
        end
    end
    return nothing
end

f = firm
m = model



println("####################################")
println("#### Solve Baseline - With Risk Adjustment and Mandate ####")
println("####################################")
J = maximum(m.prods)
P_Base = zeros(J)
S_Base = zeros(J)
solve_model!(m,f,sim="Base",voucher=true)
P_Base[:] = f.P_j[:]
evaluate_model!(m,f,"All",voucher=true)
S_Base[:] = f.S_j[:]


unique_firms = sort(unique(f.firm_vector[f.firm_vector.!=""]))
for (f_index,merge_party_2) in enumerate(unique_firms)
    for merge_party_1 in unique_firms[1:(f_index-1)]
        println([merge_party_1,merge_party_2])

    end
end
