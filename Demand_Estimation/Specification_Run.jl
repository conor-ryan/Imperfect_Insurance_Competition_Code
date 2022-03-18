function run_specification(df::DataFrame,
                            df_mkt::DataFrame,
                            df_risk::DataFrame;
                            haltonDim = 1,
                            spec_prodchars=[:Price,:MedDeduct,:High],
                            spec_prodchars_σ=[:PriceDiff],
                            spec_demoRaw=[:Age,:Family,:LowIncome],
                            spec_fixedEffects=[],
                            nested = false,
                            pre_calc=true)

    ## Build Model
    c_data = ChoiceData(df,df_mkt,df_risk;
        demoRaw=spec_demoRaw,
        prodchars=spec_prodchars,
        prodchars_σ=spec_prodchars_σ,
        fixedEffects=spec_fixedEffects)

    param_labels = vcat(String.(spec_demoRaw),String.(spec_prodchars),"Price:" .* String.(spec_demoRaw),"Variance:".*String.(spec_prodchars_σ),c_data.feNames)

    m = InsuranceLogit(c_data,haltonDim,nested=nested)

    println("Data Loaded")

    ## Initialize Starting Parameters
    #γ0start = rand(1)-.5
    γstart = rand(m.parLength[:γ])/10 .-.05
    β0start = rand(m.parLength[:β])/10 .-.05
    βstart = rand(m.parLength[:γ])/10 .- .05
    σstart = rand(m.parLength[:σ])/10 .- .05
    FEstart = rand(m.parLength[:FE])/100 .-.005

    #p0 = vcat(γ0start,γstart,β0start,βstart,σstart,FEstart)
    p0 = vcat(γstart,β0start,βstart,σstart,FEstart)
    p0 = zeros(length(p0))

    if pre_calc
        println("Compute No Heterogeneity Parameters")
        ## Compute Non_heterogeneity Starting Point
        ll_res = run_specification(df,df_mkt,df_risk,
                        haltonDim = 1,
                            spec_prodchars = spec_prodchars,
                            spec_prodchars_σ= Vector{Symbol}(undef,0),
                            spec_demoRaw=spec_demoRaw,
                            spec_fixedEffects=spec_fixedEffects,
                            nested = nested,
                            pre_calc=false)

        x_est = ll_res[1]
        ind1 = 1:(length(p0) - m.parLength[:FE] - m.parLength[:σ])
        ind2 = (length(p0) - m.parLength[:FE] + 1):m.parLength[:All]
        p0[ind1] = x_est[ind1]
        p0[ind2] = x_est[ind2 .- m.parLength[:σ]]
        # indσ = (maximum(ind1) + 1):(minimum(ind2) - 1)
        # p0[indσ].= rand(m.parLength[:σ])/10 .+ 1e-4
    end

    println("Begin Estimation")

    ## Estimate
    p_est, fval = newton_raphson_ll(m,p0)
    return p_est, m, fval,param_labels
end

function res_process_ll(model::InsuranceLogit,p_est::Vector{Float64})
    ## Create Param Dictionary

    paramFinal = parDict(model,p_est,no2Der=true)
    Pop =sum(weight(model.data).*choice(model.data))
    #### Likelihood Errors ####
    # AsVar_ll = calc_Avar(model,paramFinal)
    hess = Matrix{Float64}(undef,length(p_est),length(p_est))
    grad = Vector{Float64}(undef,length(p_est))
    res = log_likelihood!(hess,grad,model,p_est)
    AsVar_ll = -inv(hess)./Pop
    if any(diag(AsVar_ll.<0))
        println("Some negative variances")
        stdErr = sqrt.(abs.(diag(AsVar_ll)))
    else
        stdErr = sqrt.(diag(AsVar_ll))
    end


    t_stat = p_est./stdErr

    stars = Vector{String}(undef,length(t_stat))
    for i in 1:length(stars)
        if abs(t_stat[i])>2.326
            stars[i] = "***"
        elseif abs(t_stat[i])>1.654
            stars[i] = "**"
        elseif abs(t_stat[i])>1.282
            stars[i] = "*"
        else
            stars[i] = ""
        end
    end

    return AsVar_ll, stdErr, t_stat, stars
end



function res_process_GMM(model::InsuranceLogit,p_est::Vector{Float64})
    ## Create Param Dictionary

    paramFinal = parDict(model,p_est,no2Der=true)

    #### GMM Errors ####
    V1 = calc_mom_Avar(model,p_est)
    V2 = integration_var_bootstrap(model,p_est;n=500)

    ## Derivative of Moments wrt Parameters
    grad = Vector{Float64}(undef,length(p_est))
    hess = Matrix{Float64}(undef,length(p_est),length(p_est))
    ll = log_likelihood!(hess,grad,model,paramFinal)
    mom_grad = Matrix{Float64}(undef,length(p_est),length(model.data.tMoments))
    mom = calc_risk_moments!(mom_grad,model,paramFinal)
    G = hcat(mom_grad,hess)

    ## Calculate Variance
    AsVar = inv(G*inv(V1 + V2)*G')
    N = length(d.data._personIDs)
    V = Avar./N

    if any(diag(AsVar.<0))
        println("Some negative variances")
        stdErr = sqrt.(abs.(diag(AsVar)))
    else
        stdErr = sqrt.(diag(AsVar))
    end


    t_stat = p_est./stdErr

    stars = Vector{String}(undef,length(t_stat))
    for i in 1:length(stars)
        if abs(t_stat[i])>2.326
            stars[i] = "***"
        elseif abs(t_stat[i])>1.654
            stars[i] = "**"
        elseif abs(t_stat[i])>1.282
            stars[i] = "*"
        else
            stars[i] = ""
        end
    end

    return S,G,AsVar, stdErr, t_stat, stars
end


function run_specification_GMM(filename::String,
                            rundate,
                            df::DataFrame,
                            df_mkt::DataFrame,
                            df_risk::DataFrame;
                            haltonDim = 1,
                            spec_prodchars=[:Price,:MedDeduct,:High],
                            spec_prodchars_σ=Vector{Symbol}(undef,0),
                            spec_demoRaw=[:Age,:Family,:LowIncome],
                            spec_fixedEffects=Vector{Symbol}(undef,0),
                            nested = false)

    spec_Dict = Dict("prodchars" => spec_prodchars,
    "prodchars_σ"=> spec_prodchars_σ,
    "demoRaw"=>spec_demoRaw,
    "fixedEffects"=>spec_fixedEffects,
    "nested"=>nested,
    "haltonDim"=>haltonDim)

    cd("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/")
    ## Build Log_Likehood Model
    println("Build LL Model")
    # c_ll = ChoiceData(df,df_mkt,df_risk;
    #     demoRaw=spec_demoRaw,
    #     prodchars=spec_prodchars,
    #     prodchars_σ=Vector{Symbol}(undef,0),
    #     fixedEffects=spec_fixedEffects)
    #
    # m_ll = InsuranceLogit(c_ll,1,nested=nested)
    #
    # ## Initialize Starting Parameters
    # γstart = rand(m_ll.parLength[:γ])/10 .-.05
    # β0start = rand(m_ll.parLength[:β])/10 .-.05
    # βstart = rand(m_ll.parLength[:γ])/10 .- .05
    # σstart = rand(m_ll.parLength[:σ])/10 .- .05
    # FEstart = rand(m_ll.parLength[:FE])/100 .-.005
    #
    # p0 = vcat(γstart,β0start,βstart,σstart,FEstart)
    # println("#### Estimate LL Starting Point ####")
    #
    # ## Estimate
    # p_ll, fval = newton_raphson_ll(m_ll,p0)
    #
    # println("Save LL Result")
    # file = "$filename-$rundate-ll.jld2"
    # @save file p_ll spec_Dict

    # println("Load LL Result")
    # file = "GMM_Estimate_FMC-2019-08-11-ll.jld2"
    # @load file p_ll spec_Dict



    ## Build GMM Model
    println("Build GMM Model")
    c_data = ChoiceData(df,df_mkt,df_risk;
        demoRaw=spec_demoRaw,
        prodchars=spec_prodchars,
        prodchars_σ=spec_prodchars_σ,
        fixedEffects=spec_fixedEffects)

    param_labels = vcat(String.(spec_demoRaw),String.(spec_prodchars),"Price:" .* String.(spec_demoRaw),"Variance:".*String.(spec_prodchars_σ),c_data.feNames)

    m_GMM = InsuranceLogit(c_data,haltonDim,nested=nested)

    # Initialize Starting Parameters
    ind1 = 1:(m_GMM.parLength[:γ]*2+m_GMM.parLength[:β])
    ind2 = (1 + maximum(ind1) + m_GMM.parLength[:σ]):m_GMM.parLength[:All]
    σ_ind = (1 + maximum(ind1)):(minimum(ind2)-1)


    p0 = zeros(m_GMM.parLength[:All])
    println(length(p0))
    println(length(p_ll))
    println(m_GMM.parLength[:All])
    println(m_GMM.parLength[:σ])

    p0[ind1] = p_ll[ind1]
    p0[ind2] = p_ll[ind2.-m_GMM.parLength[:σ]]

    println("#### Estimate GMM First Stage ####")
    # if spec_fixedEffects== [:Firm_Market_Cat]
    #     p0[σ_ind] = [0.534812, 0.497257, 0.0252315, -0.0605508, 0.144096]
    # end
    # file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/checkin_265.jld2"
    # @load file p_vec
    # p0 = copy(p_vec)
    W = Matrix(1.0I,m_GMM.parLength[:All]+length(m_GMM.data.tMoments),m_GMM.parLength[:All]+length(m_GMM.data.tMoments))
    flag = ""
    p_stg1 = similar(p0)
    obj_1 = 0.0
    while flag!="converged"
        p0[σ_ind]=rand(length(σ_ind)).*0.1 .- .05
        p_stg1, obj_1, flag = two_stage_est(m_GMM,p0,W)
    end

    println("Save First Stage Result")
    file = "$filename-$rundate-stg1.jld2"
    @save file p_stg1 obj_1 spec_Dict

    println("Load First Stage Result")
    file = "$filename-$rundate-stg1.jld2"
    @load file p_stg1 obj_1 spec_Dict


    println("#### Estimate GMM Second Stage ####")
    mom_pars = vcat(1:length(m_GMM.data.tMoments),(length(m_GMM.data.tMoments)).+σ_ind)
    mom_ind = 1:length(m_GMM.data.tMoments)
    S = calc_mom_Avar(m_GMM,p_stg1)
    S_mom = S[mom_pars,mom_pars]
    diag_sigma = Diagonal(diag(S_mom))
    S_mom[mom_ind,:] .= 0.0
    S_mom[:,mom_ind] .= 0.0
    S_mom[mom_ind,mom_ind] = diag_sigma[mom_ind,mom_ind]
    W2 = inv(S_mom)
    W[mom_pars,mom_pars] = W2[:,:]


    println(S[mom_pars,mom_pars])
    println(W2)
    println(W[mom_pars,mom_pars])

    ## Estimate
    flag = ""
    p_stg2 = similar(p0)
    obj_2 = 0.0

    # Start at p_stg1, should add another starting point for robustness.
    p0[σ_ind]=rand(length(σ_ind)).*0.1 .- .05
    p_stg2, obj_2, flag = two_stage_est(m_GMM,p_stg1,W)
    # p_stg2 = copy(p_stg1)
    # obj_2 = copy(obj_1)
    println("Save Second Stage Result")
    file = "$filename-$rundate-stg2.jld2"
    # @save file p_stg2 obj_2 spec_Dict
    @load file p_stg2 obj_2 spec_Dict

    println("#### Calculate Standard Errors and Save Results ####")
    # AsVar, stdErr,t_stat, stars = GMM_var(m_GMM,p_stg2)
    AsVar, stdErr,t_stat, stars = res_process_ll(m_GMM,p_stg2)

    out1 = DataFrame(labels=param_labels,pars=p_stg2,se=stdErr,ts=t_stat,sig=stars)
    file1 = "$filename-$rundate-test.csv"
    CSV.write(file1,out1)

    out2 = DataFrame(delta=m_GMM.deltas,prods=m_GMM.prods)
    file2 = "$filename-$rundate-deltas-test.csv"
    CSV.write(file2,out2)

    return nothing
end


function estimate_demand(filename,rundate,
                    haltonDim,
                    spec_demoRaw,
                    spec_prodchars,
                    spec_prodchars_σ,
                    spec_fixedEffects)
    # Load the Data
    println("Loading Data...")
    codeDir = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Code/Demand_Estimation"
    include("$codeDir/load.jl")


    ### Run Specification 1 ####
    println("#### Run Specification ####")
    spec1 = run_specification_penalizedlikelihood(filename,rundate,
                        df,df_mkt,df_risk,df_transfer,
                        haltonDim = halton_draws,
                        spec_demoRaw=spec_demoRaw,
                        spec_prodchars=spec_prodchars,
                        spec_prodchars_σ=spec_prodchars_σ,
                        spec_fixedEffects=spec_fixedEffects)
        return nothing
end



function run_specification_penalizedlikelihood(filename::String,
                            rundate,
                            df::DataFrame,
                            df_mkt::DataFrame,
                            df_risk::DataFrame,
                            df_transfer::DataFrame;
                            haltonDim = 1,
                            spec_prodchars=[:Price,:MedDeduct,:High],
                            spec_prodchars_σ=Vector{Symbol}(undef,0),
                            spec_demoRaw=[:Age,:Family,:LowIncome],
                            spec_fixedEffects=Vector{Symbol}(undef,0),
                            nested = false)

    spec_Dict = Dict("prodchars" => spec_prodchars,
    "prodchars_σ"=> spec_prodchars_σ,
    "demoRaw"=>spec_demoRaw,
    "fixedEffects"=>spec_fixedEffects,
    "nested"=>nested,
    "haltonDim"=>haltonDim)

    cd("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/")

    println("Build LL Model - Fixed Effects Starting Point")
    c_ll = ChoiceData(df,df_mkt,df_risk,df_transfer;
        demoRaw=spec_demoRaw,
        prodchars=spec_prodchars,
        prodchars_σ=Vector{Symbol}(undef,0),
        fixedEffects=spec_fixedEffects)

    m_ll = InsuranceLogit(c_ll,1,nested=nested)

    ## Initialize Starting Parameters
    γstart = rand(m_ll.parLength[:γ])/10 .-.05
    β0start = rand(m_ll.parLength[:β])/10 .-.05
    βstart = rand(m_ll.parLength[:γ])/10 .- .05
    σstart = rand(m_ll.parLength[:σ])/10 .- .05
    FEstart = rand(m_ll.parLength[:FE])/100 .-.005

    fe_length = m_ll.parLength[:FE]
    p0 = vcat(γstart,β0start,βstart,σstart,FEstart)
    println("#### Estimate LL Starting Point ####")

    ## Estimate
    # W = zeros(length(m_ll.data.rMoments),length(m_ll.data.rMoments))
    # p_ll, fval = newton_raphson_ll(m_ll,p0,W)


    println("Save LL Result")
    file = "$filename-$rundate-ll.jld2"
    # @save file p_ll spec_Dict
    @load file p_ll spec_Dict

    ## Build GMM Model
    println("Build Model")
    c_data = ChoiceData(df,df_mkt,df_risk,df_transfer;
        demoRaw=spec_demoRaw,
        prodchars=spec_prodchars,
        prodchars_σ=spec_prodchars_σ,
        fixedEffects=spec_fixedEffects)

    param_labels = vcat(String.(spec_demoRaw),String.(spec_prodchars),"Price:" .* String.(spec_demoRaw),"Variance:".*String.(spec_prodchars_σ),c_data.feNames)

    m_ll = InsuranceLogit(c_data,haltonDim,nested=nested)

    # Initialize Starting Parameters
    ind1 = 1:(m_ll.parLength[:γ]*2+m_ll.parLength[:β])
    ind2 = (1 + maximum(ind1) + m_ll.parLength[:σ]):m_ll.parLength[:All]
    σ_ind = (1 + maximum(ind1)):(minimum(ind2)-1)


    p0 = rand(m_ll.parLength[:All]) .- 0.5
    p0[σ_ind]=rand(length(σ_ind)).*0.1 .- .05
    p0[(length(p0)-fe_length):length(p0)] = p_ll[(length(p_ll)-fe_length):length(p_ll)]
    println("Starting vector: $p0")

    println("#### Estimate First Stage ####")
    W = -Matrix(1.0I,length(m_ll.data.rMoments),length(m_ll.data.rMoments))
    W[1,1] = -5.0
    W[2,2] = -5.0
    W[3,3] = -5.0
    W[4,4] = -5.0
    W[5,5] = -5.0

    p_init, obj_init = gradient_ascent_ll(m_ll,p0,W,max_itr=50)
    p_stg1, obj_1 = newton_raphson_ll(m_ll,p_init,W)

    println("Save First Stage Result")
    file = "$filename-$rundate-stg1.jld2"
    @save file p_stg1 obj_1 spec_Dict

    # println("Load First Stage Result")
    # file = "$filename-$rundate-stg1.jld2"
    # @load file p_stg1 obj_1 spec_Dict


    println("#### Estimate GMM Second Stage ####")
    V = risk_moment_bootstrap(m_ll,p_stg1)
    W = -Matrix{Float64}(Diagonal(1 ./diag(V))./1000)
    println(diag(W))
    # W = - inv(V)

    ## Estimate
    p0 = rand(m_ll.parLength[:All]) .- 0.5
    p0[σ_ind]=rand(length(σ_ind)).*0.1 .- .05
    p_init, obj_init = gradient_ascent_ll(m_ll,p0,W,max_itr=50)
    p_stg2, obj_2 = newton_raphson_ll(m_ll,p_init,W)

    println("Save Second Stage Result")
    file = "$filename-$rundate-stg2.jld2"
    @save file p_stg2 obj_2 spec_Dict
    # @load file p_stg2 obj_2 spec_Dict

    println("#### Calculate Standard Errors and Save Results ####")
    # AsVar, stdErr,t_stat, stars = GMM_var(m_GMM,p_stg2)
    # AsVar, stdErr,t_stat, stars = res_process_ll(m_GMM,p_stg2)

    out1 = DataFrame(labels=param_labels,pars=p_stg2)#,se=stdErr,ts=t_stat,sig=stars)
    file1 = "$filename-$rundate.csv"
    CSV.write(file1,out1)

    return nothing
end
