function estimate_marginal_cost(rundate,spec,cost_spec)
    # Load the Data
    println("Loading Data...")
    codeDir = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Code/Firm_Side"
    include("$codeDir/MC_load.jl")


    # df[:High_small] = df[:HighRisk].*df[:Small]


    #### Load Demand Estimation Results ####
    println("Rebuild Demand Model...")
    file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/GMM_Estimate_$spec-$rundate-stg2.jld2"
    @load file p_stg2 spec_Dict
    p_dem_est = copy(p_stg2)


    #### Build Model ####
    # Structre the data
    chdf = ChoiceData(df,df_mkt,df_risk;
        demoRaw=spec_Dict["demoRaw"],
        prodchars=spec_Dict["prodchars"],
        prodchars_0=spec_Dict["prodchars_0"],
        fixedEffects=spec_Dict["fixedEffects"],
        wgt=[:PERWT])

    # Fit into model
    m = InsuranceLogit(chdf,spec_Dict["haltonDim"])


    if length(p_stg2)!=m.parLength[:All]
        println(length(p_stg2))
        println(m.parLength[:All])
        error("Parameter Vector Not Quite Right")
    end

    #### Compute Demand Estimation
    par_est = parDict(m,p_dem_est,no2Der=true)
    individual_values!(m,par_est)
    individual_shares(m,par_est)

    #### Cost Data ####
    println("Build Cost Data...")
    costdf = MC_Data(df,mom_firm,mom_metal,mom_age,mom_age_no,mom_risk,mom_ra;
                    baseSpec=cost_spec,
                    fixedEffects=[:Firm_ST],
                    constMoments=true)


    println("#################")
    println("#################")
    println("###### Estimation 1 #######")
    println("#################")
    println("#################")

    W = Matrix(1.0I,costdf.mom_length,costdf.mom_length)

    p0 = vcat(rand(length(cost_spec)+1)*.2)
    est_init = estimate_NLOpt(p0,par_est,m,costdf,W,itrFirms=false,tol=1e-4,max_itr=100)
    est_stg1 = estimate_NLOpt(est_init[3],par_est,m,costdf,W,itrFirms=true)

    file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg1_$spec-$rundate.jld2"
    @save file est_stg1

    flag,fval,p_stg1 = est_stg1


    println("#################")
    println("#################")
    println("###### Estimation 2 #######")
    println("#################")
    println("#################")

    p_full1 = fit_firm_moments(p_stg1,par_est,m,costdf)
    S,Σ,Δ,mom_long = aVar(costdf,m,p_full1,par_est)
    W = inv(S)

    p0 = vcat(rand(length(cost_spec)+1)*.2)

    est_stg2 = estimate_NLOpt(p0,par_est,m,costdf,W,itrFirms=false,tol=1e-4,max_itr=100)
    est_stg2 = estimate_NLOpt(est_stg2[3],par_est,m,costdf,W,itrFirms=true)


    p_stg2 = fit_firm_moments(est_stg2[3],par_est,m,costdf,itrFirms=true)
    file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg2_$spec-$rundate.jld2"
    @save file p_stg2 p_dem_est cost_spec spec_Dict



    println("#################")
    println("#################")
    println("###### Save Results #######")
    println("#################")
    println("#################")

    Avar, se, t_stat, stars = GMM_var(costdf,m,p_stg2,par_est)

    out1 = DataFrame(pars=p_stg2,se=se,ts=t_stat,sig=stars)
    file1 = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_$spec-$rundate.csv"
    CSV.write(file1,out1)

    return nothing
end
