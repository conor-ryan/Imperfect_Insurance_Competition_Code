function run_specification(df::DataFrame,
                            df_mkt::DataFrame,
                            df_risk::DataFrame;
                            haltonDim = 1,
                            spec_prodchars=[:Price,:MedDeduct,:High],
                            spec_prodchars_0=[:PriceDiff],
                            spec_demoRaw=[:Age,:Family,:LowIncome],
                            spec_fixedEffects=[],
                            nested = false)

    ## Build Model
    c_data = ChoiceData(df,df_mkt,df_risk;
        demoRaw=spec_demoRaw,
        prodchars=spec_prodchars,
        prodchars_0=spec_prodchars_0,
        fixedEffects=spec_fixedEffects)

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

    println("Begin Estimation")

    ## Estimate
    p_est, fval = newton_raphson_ll(m,p0)
    return p_est, m, fval
end




function res_process(model::InsuranceLogit,p_est::Vector{Float64})
    ## Create Param Dictionary

    paramFinal = parDict(model,p_est)

    AsVar = calc_Avar(model,paramFinal)
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

    return AsVar, stdErr, t_stat, stars
end


function run_specification_GMM(filename::String,
                            rundate::Date,
                            df::DataFrame,
                            df_mkt::DataFrame,
                            df_risk::DataFrame;
                            haltonDim = 1,
                            spec_prodchars=[:Price,:MedDeduct,:High],
                            spec_prodchars_0=Vector{Symbol}(undef,0),
                            spec_demoRaw=[:Age,:Family,:LowIncome],
                            spec_fixedEffects=Vector{Symbol}(undef,0),
                            nested = false)

    cd("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/")
    ## Build Log_Likehood Model
    println("Build LL Model")
    c_ll = ChoiceData(df,df_mkt,df_risk;
        demoRaw=spec_demoRaw,
        prodchars=spec_prodchars,
        prodchars_0=Vector{Symbol}(undef,0),
        fixedEffects=spec_fixedEffects)

    m_ll = InsuranceLogit(c_ll,1,nested=nested)

    ## Initialize Starting Parameters
    γstart = rand(m_ll.parLength[:γ])/10 .-.05
    β0start = rand(m_ll.parLength[:β])/10 .-.05
    βstart = rand(m_ll.parLength[:γ])/10 .- .05
    σstart = rand(m_ll.parLength[:σ])/10 .- .05
    FEstart = rand(m_ll.parLength[:FE])/100 .-.005

    p0 = vcat(γstart,β0start,βstart,σstart,FEstart)
    println("#### Estimate LL Starting Point ####")

    ## Estimate
    p_ll, fval = newton_raphson_ll(m_ll,p0)

    println("Save LL Result")
    file = "$filename-$rundate-ll.jld2"
    @save file p_ll


    ## Build GMM Model
    println("Build GMM Model")
    c_data = ChoiceData(df,df_mkt,df_risk;
        demoRaw=spec_demoRaw,
        prodchars=spec_prodchars,
        prodchars_0=spec_prodchars_0,
        fixedEffects=spec_fixedEffects)

    m_GMM = InsuranceLogit(c_data,haltonDim,nested=nested)

    ## Initialize Starting Parameters
    ind1 = 1:(m_GMM.parLength[:γ]*2+m_GMM.parLength[:β])
    ind2 = (1 + maximum(ind1) + m_GMM.parLength[:σ]):m_GMM.parLength[:All]
    σ_ind = (1 + maximum(ind1)):(minimum(ind2))

    p0 = zeros(m_GMM.parLength[:All])
    p0[ind1] = p_ll[ind1]
    p0[ind2] = p_ll[ind2.-m_GMM.parLength[:σ]]
    println("#### Estimate GMM First Stage ####")

    # file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/checkin_265.jld2"
    # @load file p_vec
    # p0 = copy(p_vec)

    W = Matrix(1.0I,length(p0)+length(m_GMM.data.tMoments),length(p0)+length(m_GMM.data.tMoments))
    ## Estimate
    # p_stg1, obj_1 = estimate_GMM(m_GMM,p0,W)
    # p_stg1, obj_1 = newton_raphson_GMM(m_GMM,p0,W,grad_tol = 1e-8,strict=true,checkin=true)
    p_stg1, obj_1 = two_stage_est(m_GMM,p0,W)

    println("Save First Stage Result")
    file = "$filename-$rundate-stg1.jld2"
    @save file p_stg1, obj_1

    println("#### Estimate GMM Second Stage ####")
    S = calc_mom_Avar(m_GMM,p_stg1)
    W2 = inv(S[σ_ind,σ_ind])
    W[σ_ind,σ_ind] = W2
    ## Estimate
    p_stg2, obj_2 = two_stage_est(m_GMM,p0,W)

    println("Save Second Stage Result")
    file = "$filename-$rundate-stg2.jld2"
    @save file p_stg2, obj_2

    println("#### Calculate Standard Errors and Save Results ####")
    AsVar, stdErr,t_stat, stars = res_process(m_GMM,p_stg2)

    out1 = DataFrame(pars=p_stg2,se=stdErr,ts=t_stat,sig=stars)
    file1 = "$filename-$rundate.csv"
    CSV.write(file1,out1)

    out2 = DataFrame(delta=m_GMM.deltas,prods=m_GMM.prods)
    file2 = "$filename-$rundate-deltas.csv"
    CSV.write(file2,out2)

    return nothing
end
