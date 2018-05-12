function run_specification(df::DataFrame,
                            df_mkt::DataFrame;
                            haltonDim = 1,
                            spec_prodchars=[:Price,:MedDeduct,:High],
                            spec_prodchars_0=[:PriceDiff],
                            spec_demoRaw=[:Age,:Family,:LowIncome],
                            spec_fixedEffects=[])

    ## Build Model
    c_data = ChoiceData(df,df_mkt;
        demoRaw=spec_demoRaw,
        prodchars=spec_prodchars,
        prodchars_0=spec_prodchars_0,
        fixedEffects=spec_fixedEffects)

    m = InsuranceLogit(c_data,haltonDim)

    println("Data Loaded")

    ## Initialize Starting Parameters
    γ0start = rand(1)-.5
    γstart = rand(m.parLength[:γ])-.5
    β0start = rand(m.parLength[:β]) - .5
    βstart = rand(m.parLength[:γ])/10 - .05
    σstart = rand(m.parLength[:σ])/10 - .05
    FEstart = rand(m.parLength[:FE])/10-.05

    p0 = vcat(γ0start,γstart,β0start,βstart,σstart,FEstart)

    println("Begin Estimation")

    ## Estimate
    est_res = estimate!(m, p0)

    return est_res

end
