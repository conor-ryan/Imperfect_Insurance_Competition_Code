function estimate_marginal_cost(rundate,spec,cost_spec,home_directory)
    # Load the Data
    println("Loading Data...")
    codeDir = "$home_directory/Research/Imperfect_Insurance_Competition/Code/Firm_Side"
    include("$codeDir/MC_load.jl")


    # df[:High_small] = df[:HighRisk].*df[:Small]


    #### Load Demand Estimation Results ####
    println("Rebuild Demand Model...")
    file = "$home_directory/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/PLL_Estimate_$spec-$rundate-stg2.jld2"
    @load file p_stg2 spec_Dict
    p_dem_est = copy(p_stg2)
    # p_stg2 = copy(p_stg1)

    #### Compute Hessian of Likelihood from Demand for Std Errors ####
    df_demand = ChoiceData(df_dem,df_mkt,df_risk,df_transfer;
        demoRaw=spec_Dict["demoRaw"],
        prodchars=spec_Dict["prodchars"],
        prodchars_σ=spec_Dict["prodchars_σ"],
        fixedEffects=spec_Dict["fixedEffects"])

    # Fit into model
    m_demand = InsuranceLogit(df_demand,spec_Dict["haltonDim"])

    if length(p_stg2)!=m_demand.parLength[:All]
        println(length(p_stg2))
        println(m.parLength[:All])
        error("Parameter Vector Not Quite Right")
    end

    println("Re-solve Demand")
    par_dem = parDict(m_demand,p_dem_est)
    individual_values!(m_demand,par_dem)
    individual_shares(m_demand,par_dem)

    println("Construct Hessians for Standard Errors")
    ll_grad = Vector{Float64}(undef,length(p_dem_est))
    ll_hess = Matrix{Float64}(undef,length(p_dem_est),length(p_dem_est))
    ll = log_likelihood!(ll_hess,ll_grad,m_demand,par_dem)

    mom_grad = Matrix{Float64}(undef,length(p_dem_est),length(m_demand.data.rMoments))
    mom = calc_risk_moments!(mom_grad,m_demand,par_dem)
    G_θ = hcat(mom_grad,ll_hess)

    m_demand = 0.0
    df_demand = 0.0
    par_dem = 0.0
    ll_grad = 0.0
    ll_hess = 0.0
    mom_grad = 0.0

    #### Build Model ####
    # Structre the data
    chdf = ChoiceData(df,df_mkt,df_risk,df_transfer;
        product = [:Product_std],
        demoRaw=spec_Dict["demoRaw"],
        prodchars=spec_Dict["prodchars"],
        prodchars_σ=spec_Dict["prodchars_σ"],
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
    p0[2] = rand()*3+1
    est_init = estimate_NLOpt(p0,par_est,m,costdf,W,itrFirms=false,tol=1e-4,max_itr=100)
    est_stg1 = estimate_NLOpt(est_init[3],par_est,m,costdf,W,itrFirms=true)
    p_stg1 = fit_firm_moments(est_stg1[3],par_est,m,costdf,itrFirms=true)
    file = "$home_directory/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg1_$spec-$rundate.jld2"
    @save file p_stg1 p_dem_est cost_spec spec_Dict
    # @load file p_stg1 p_dem_est cost_spec spec_Dict

    println("#################")
    println("#################")
    println("###### Estimation 2 #######")
    println("#################")
    println("#################")
    #### Use Predicted Shares as Observed Shares for computing two-stage Standard Errors
    m.data.data[m.data._choice,:] = par_est.s_hat


    S_m,Σ,Δ,mom_long = aVar(costdf,m,p_stg1,par_est)
    S_diag = Matrix(Diagonal(diag(S_m)))
    W = Matrix(Diagonal(diag(inv(S_diag))))

    p0 = vcat(rand(length(cost_spec)+1)*.2)
    p0[2] = rand()*3+1

    est_stg2 = estimate_NLOpt(p0,par_est,m,costdf,W,itrFirms=false,tol=1e-4,max_itr=100)
    est_stg2 = estimate_NLOpt(est_stg2[3],par_est,m,costdf,W,itrFirms=true)


    p_stg2 = fit_firm_moments(est_stg2[3],par_est,m,costdf,itrFirms=true)

    file = "$home_directory/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg2_$spec-$rundate.jld2"
    @save file p_stg2 p_dem_est cost_spec spec_Dict
    # @load file p_stg2 p_dem_est cost_spec spec_Dict



    println("#################")
    println("#################")
    println("###### Save Results #######")
    println("#################")
    println("#################")



    Avar, se, t_stat, stars = GMM_var(costdf,m,p_stg2,par_est,p_dem_est,W,G_θ)

    out1 = DataFrame(pars=p_stg2,se=se,ts=t_stat,sig=stars)
    file1 = "$home_directory/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_$spec-$rundate.csv"
    CSV.write(file1,out1)

    return nothing
end
