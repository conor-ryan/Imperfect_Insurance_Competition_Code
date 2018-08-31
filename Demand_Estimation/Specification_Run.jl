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
    γstart = rand(m.parLength[:γ])/10 -.05
    β0start = rand(m.parLength[:β])/10-.05
    βstart = rand(m.parLength[:γ])/10 - .05
    σstart = rand(m.parLength[:σ])/10 - .05
    FEstart = rand(m.parLength[:FE])/100-.005

    #p0 = vcat(γ0start,γstart,β0start,βstart,σstart,FEstart)
    p0 = vcat(γstart,β0start,βstart,σstart,FEstart)
    p0 = zeros(length(p0))

    println("Begin Estimation")

    ## Estimate
    p_est, fval = newton_raphson(m,p0)
    return p_est, m, fval
    
    # flag, fval, p_est = estimate!(m, p0)
    # return p_est , m, (flag,fval)

end




function res_process(model::InsuranceLogit,p_est::Vector{Float64})
    ## Create Param Dictionary

    paramFinal = parDict(model,p_est)

    AsVar = calc_Avar(model,paramFinal)
    stdErr = sqrt.(diag(AsVar))
    t_stat = p_est./stdErr

    stars = Vector{String}(length(t_stat))
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
