function process_demand(rundate,spec,home_directory)
    #Load Data
    println("Loading Data...")
    include("EQ_load.jl")

    # df[:High_small] = df[:HighRisk].*df[:Small]

    mark_the_output_date = Dates.today()
    println("Running spec $rundate on $mark_the_output_date")

    file = "$home_directory/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg2_$spec-$rundate.jld2"
    @load file p_stg2 p_dem_est cost_spec spec_Dict
    mc_est = copy(p_stg2)
    #### Load Estimation Results ####


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


    #### Willingness to Pay, Elasticities, Costs ####
    Smat_r,Smat_nr = individual_share_matrix(model,par_dem)

    prem_base = df[!,:premBase].*12 .*df[!,:ageRate]./1000
    wtp_nr, wtp_r,elas_nr,elas_r,elas0_nr,elas0_r,elas0_nr_long,elas0_r_long = individual_wtp(model,par_dem,Smat_r,Smat_nr,prem_base)

    c_nr = par_cost.C_nonrisk
    c_r = individual_cost_matrix(model,par_cost)

    #### Weighting Matrices ####
    P = maximum(Int.(keys(chdf._personDict)))
    N = size(model.draws,1)
    wgts = weight(chdf)
    any_r = anyHCC(chdf)

    wgts_pp = zeros(P)
    any_r_pp = zeros(P)
    for i in chdf._personIDs
        idxitr = chdf._personDict[i]
        wgts_pp[Int(i)] = wgts[idxitr[1]]
        any_r_pp[Int(i)] = any_r[idxitr[1]]
    end

    wgts_pp_nr = wgts_pp[:].*(1 .- any_r_pp)
    wgts_pp_r = zeros(P,N)
    for i in 1:P, j in 1:N
        wgts_pp_r[i,j] = wgts_pp[i]*(any_r_pp[i])/N
    end
    wgts_pp_r_vec = wgts_pp_r[:]
    all_wgts_pp = vcat(wgts_pp_r_vec,wgts_pp_nr)

    wgts_nr = wgts[:].*(1 .- any_r).*Smat_nr
    wgts_r = zeros(length(wgts),N)
    for i in 1:length(wgts), j in 1:N
        wgts_r[i,j] = wgts[i]*(any_r[i])/N*Smat_r[i,j]
    end
    wgts_r_vec = wgts_r[:]
    all_wgts = vcat(wgts_r_vec,wgts_nr)


    #### Distribution of Willingness to Pay ####
    wtp_r_vec = wtp_r[:]

    all_wtp = vcat(wtp_r_vec,wtp_nr)
    all_wtp = all_wtp./12 .*1000 .*(0.1) # $ per month for 10% increase

    wtp_sort_index = sortperm(all_wtp)

    for pctile in [0.1,0.5,0.9,0.99]
        ind = find_pctile_index(all_wgts_pp,wtp_sort_index,pctile)
        wtp_val = all_wtp[wtp_sort_index[ind]]
        println("The $pctile of WTP is $wtp_val")
    end

    mean_wtp = sum(all_wtp.*all_wgts_pp)/sum(all_wgts_pp)
    println("The mean of WTP is $mean_wtp")

    #### Distribution of Extensive Elasticity ####
    elas0_r_vec = elas0_r[:]

    all_elas0 = vcat(elas0_r_vec,elas0_nr)
    all_elas0 = all_elas0.*12 ./1000 .*10 # $10 per month

    elas0_sort_index = sortperm(all_elas0)

    for pctile in [0.1,0.5,0.9,0.99]
        ind = find_pctile_index(all_wgts_pp,elas0_sort_index,pctile)
        elas0_val = all_elas0[elas0_sort_index[ind]]
        println("The $pctile of elas0 is $elas0_val")
    end

    mean_elas0 = sum(all_elas0.*all_wgts_pp)/sum(all_wgts_pp)
    println("The mean of elas0 is $mean_elas0")

    elas0_r_long_vec = elas0_r_long[:]

    all_elas0_long = vcat(elas0_r_long_vec,elas0_nr_long)
    all_elas0_long = all_elas0_long.*12 ./1000 .*10 # $10 per month

    elas0_long_sort_index = sortperm(all_elas0_long)

    #### Distribution of Own-price Semi Elasticity ####
    elas_r_vec = elas_r[:]

    all_elas = vcat(elas_r_vec,elas_nr)
    # all_elas = all_elas.*12 ./1000 .*10 # $10 per month

    elas_sort_index = sortperm(all_elas)

    for pctile in [0.1,0.5,0.9,0.99]
        ind = find_pctile_index(all_wgts,elas_sort_index,pctile)
        elas_val = all_elas[elas_sort_index[ind]]
        println("The $pctile of elas is $elas_val")
    end

    mean_elas = sum(all_elas.*all_wgts)/sum(all_wgts)
    println("The mean of elas is $mean_elas")


    #### Distribution of Costs ####
    c_r_vec = c_r[:]

    all_c = vcat(c_r_vec,c_nr)

    c_sort_index = sortperm(all_c)

    for pctile in [0.1,0.5,0.9,0.99]
        ind = find_pctile_index(all_wgts,c_sort_index,pctile)
        c_val = all_c[c_sort_index[ind]]
        println("The $pctile of costs is $c_val")
    end

    mean_c = sum(all_c.*all_wgts)/sum(all_wgts)
    println("The mean of costs is $mean_c")

    #### Relationship between Cost and Elasticity ####
    bottom_10th_elas = find_pctile_index(all_wgts,elas_sort_index,0.25)
    median_elas = find_pctile_index(all_wgts,elas_sort_index,0.50)
    top_10th_elas = find_pctile_index(all_wgts,elas_sort_index,0.75)
    bottom_10th = elas_sort_index[1:bottom_10th_elas]
    top_10th = elas_sort_index[top_10th_elas:length(elas_sort_index)]

    mean_c = sum(all_c[bottom_10th].*all_wgts[bottom_10th])/sum(all_wgts[bottom_10th])
    mean_elas = sum(all_elas[bottom_10th].*all_wgts[bottom_10th])/sum(all_wgts[bottom_10th])
    println("The mean of costs in the bottom 25th percentile of elasticities is $mean_c, mean elasticity is $mean_elas")
    mean_c = sum(all_c[top_10th].*all_wgts[top_10th])/sum(all_wgts[top_10th])
    mean_elas = sum(all_elas[top_10th].*all_wgts[top_10th])/sum(all_wgts[top_10th])
    println("The mean of costs in the top 75th percentile of elasticities is $mean_c, mean elasticity is $mean_elas")

    bottom_10th_elas = find_pctile_index(all_wgts,elas0_long_sort_index,0.1)
    median_elas = find_pctile_index(all_wgts,elas0_long_sort_index,0.50)
    top_10th_elas = find_pctile_index(all_wgts,elas0_long_sort_index,0.9)
    bottom_10th = elas0_long_sort_index[1:bottom_10th_elas]
    top_10th = elas0_long_sort_index[top_10th_elas:length(elas0_long_sort_index)]

    mean_c = sum(all_c[bottom_10th].*all_wgts[bottom_10th])/sum(all_wgts[bottom_10th])
    mean_elas = sum(all_elas0_long[bottom_10th].*all_wgts[bottom_10th])/sum(all_wgts[bottom_10th])
    println("The mean of costs in the bottom 25th percentile of insurance semi-elasticities is $mean_c, mean semi-elasticity is $mean_elas")
    mean_c = sum(all_c[top_10th].*all_wgts[top_10th])/sum(all_wgts[top_10th])
    mean_elas = sum(all_elas0_long[top_10th].*all_wgts[top_10th])/sum(all_wgts[top_10th])
    println("The mean of costs in the top 75th percentile of insurance semi-elasticities is $mean_c, mean semi-elasticity is $mean_elas")


    bottom_10th_cost = find_pctile_index(all_wgts,c_sort_index,0.10)
    top_10th_cost = find_pctile_index(all_wgts,c_sort_index,0.90)
    bottom_10th = c_sort_index[1:bottom_10th_cost]
    top_10th = c_sort_index[top_10th_cost:length(c_sort_index)]

    mean_c = sum(all_c[bottom_10th].*all_wgts[bottom_10th])/sum(all_wgts[bottom_10th])
    mean_elas = sum(all_elas[bottom_10th].*all_wgts[bottom_10th])/sum(all_wgts[bottom_10th])
    println("The mean of elasticities in the bottom 10th percentile of costs is $mean_elas, mean cost is $mean_c")
    mean_c = sum(all_c[top_10th].*all_wgts[top_10th])/sum(all_wgts[top_10th])
    mean_elas = sum(all_elas[top_10th].*all_wgts[top_10th])/sum(all_wgts[top_10th])
    println("The mean of elasticities in the top 10th percentile of costs is $mean_elas, mean cost is $mean_c")

    bottom_10th_cost = find_pctile_index(all_wgts,c_sort_index,0.25)
    top_10th_cost = find_pctile_index(all_wgts,c_sort_index,0.75)
    bottom_10th = c_sort_index[1:bottom_10th_cost]
    top_10th = c_sort_index[top_10th_cost:length(c_sort_index)]

    mean_c = sum(all_c[bottom_10th].*all_wgts[bottom_10th])/sum(all_wgts[bottom_10th])
    mean_elas = sum(all_elas[bottom_10th].*all_wgts[bottom_10th])/sum(all_wgts[bottom_10th])
    println("The mean of elasticities in the bottom 25th percentile of costs is $mean_elas, mean cost is $mean_c")
    mean_c = sum(all_c[top_10th].*all_wgts[top_10th])/sum(all_wgts[top_10th])
    mean_elas = sum(all_elas[top_10th].*all_wgts[top_10th])/sum(all_wgts[top_10th])
    println("The mean of elasticities in the top 25th percentile of costs is $mean_elas, mean cost is $mean_c")

    return nothing
end

function find_pctile_index(wgts::Vector{Float64},sort_index::Vector{Int64},pctile::Float64)
    ptile_index = 0
    total_pop = sum(wgts)
    running_pop = 0.0
    pctile_target = 0.0
    while pctile_target<pctile
        ptile_index+=1
        running_pop+=wgts[sort_index[ptile_index]]
        pctile_target = running_pop/total_pop
    end
    println("Got percentile $pctile_target at $ptile_index, w/ pop $running_pop")
    return ptile_index
end


function individual_wtp(d::InsuranceLogit,p::parDict{T},Smat_r::Matrix{Float64},Smat_nr::Vector{Float64},prem_base::Vector{Float64}) where T
    # Calculate μ_ij, which depends only on parameters
    P = maximum(Int.(keys(d.data._personDict)))
    wtp_nr = zeros(P)
    wtp_r = zeros(P,size(d.draws,1))
    elas_nr = Vector{Float64}(undef,length(Smat_nr))
    elas_r = Matrix{Float64}(undef,size(Smat_r,1),size(Smat_r,2))
    elas0_nr = zeros(P)
    elas0_r = zeros(P,size(d.draws,1))
    elas0_nr_long=  Vector{Float64}(undef,length(Smat_nr))
    elas0_r_long = Matrix{Float64}(undef,size(Smat_r,1),size(Smat_r,2))

    for app in eachperson(d.data)
        wtp_value!(wtp_nr,wtp_r,elas_nr,elas_r,elas0_nr,elas0_r,elas0_nr_long,elas0_r_long,app,p,Smat_r,Smat_nr,prem_base)
    end
    return wtp_nr,wtp_r,elas_nr,elas_r,elas0_nr,elas0_r,elas0_nr_long,elas0_r_long
end

function wtp_value!(wtp_nr::Vector{Float64},wtp_r::Matrix{Float64},
                    elas_nr::Vector{Float64},elas_r::Matrix{Float64},
                    elas0_nr::Vector{Float64},elas0_r::Matrix{Float64},
                    elas0_nr_long::Vector{Float64},elas0_r_long::Matrix{Float64},
                    app::ChoiceData,p::parDict{T},
                    Smat_r::Matrix{Float64},Smat_nr::Vector{Float64},prem_base::Vector{Float64}) where T
    γ_0 = p.γ_0
    γ = p.γ
    β_0= p.β_0
    β = p.β
    fe = p.FE
    randIndex = app._randCoeffs

    ind = person(app)[1]
    r_ind = Int(rIndS(app)[1])
    idxitr = app._personDict[ind]
    Z = demoRaw(app)[:,1]
    # price = prodchars(app)[1,:]
    price = prem_base[idxitr]
    β_z = β*Z
    β_i= calc_indCoeffs(p,r_ind)

    α = (β_0+β_z)[1]
    β_AV_nr = (β_0 + β_z)[2]
    β_AV_r = β_i[1,:]


    ## Willingness to Pay
    ind_index = Int(ind)
    wtp_nr[ind_index]  = -β_AV_nr/α
    wtp_r[ind_index,:] = -(β_AV_nr .+ β_AV_r)/α

    ## Own-price Semi-Elasticity
    elas_nr[idxitr] = α.*(1 .- Smat_nr[idxitr]).*price
    elas_r[idxitr,:] = α.*(1 .- Smat_r[idxitr,:]).*price

    ## Extensive Margin Semi-Elasticity
    elas0_nr[ind_index] = α*(sum(Smat_nr[idxitr]))
    elas0_r[ind_index,:] = α*(sum(Smat_r[idxitr,:],dims=1))

    ## Extensive Margin Semi-Elasticity - Long (Product)
    elas0_nr_long[idxitr].= α*(sum(Smat_nr[idxitr]))
    for k in idxitr
        elas0_r_long[k,:]= α*(sum(Smat_r[idxitr,:],dims=1))
    end
    return Nothing
end


function calc_smat(μ_ij::Vector{T},δ::Vector{T}) where T
    (K) = length(μ_ij)
    util = Vector{T}(undef,K)
    s_hat = Vector{T}(undef,K)

    expsum = 1.0
    #r_score = r[n,:]
    for i in 1:K
        a = μ_ij[i]*δ[i]
        util[i] = a
        expsum += a
    end
    for i in 1:K
        s = util[i]/expsum
        s_hat[i] = s
    end
    return s_hat
end

function calc_smat(μ_ij::Matrix{T},δ::Vector{T}) where T
    (N,K) = size(μ_ij)
    util = Matrix{T}(undef,K,N)
    s_hat = Matrix{T}(undef,K,N)
    for n in 1:N
        expsum = 1.0
        for i in 1:K
            a = μ_ij[n,i]*δ[i]
            util[i,n] = a
            expsum += a
        end
        for i in 1:K
            s_hat[i,n] = util[i,n]/expsum
        end
    end
    return s_hat
end

function individual_share_matrix(d::InsuranceLogit,p::parDict{T}) where T
    # Store Parameters
    δ_long = p.δ
    μ_ij_large = p.μ_ij
    μ_ij_nr_large = p.μ_ij_nonRisk
    risk_long = rInd(d.data)
    ageHCC_long = ageHCC(d.data)
    any_long = anyHCC(d.data)
    S_r = similar(p.μ_ij')
    S_nr = similar(p.μ_ij_nonRisk)
    for i in d.data._personIDs
        idxitr = d.data._personDict[i]
        anyR = any_long[idxitr][1]
        δ = δ_long[idxitr]
        u = μ_ij_large[:,idxitr]
        u_nr= μ_ij_nr_large[idxitr]
        smat_r = calc_smat(u,δ)
        smat_nr = calc_smat(u_nr,δ)

        S_r[idxitr,:]  = smat_r
        S_nr[idxitr]   = smat_nr[:]
    end
    return S_r,S_nr
end


function individual_cost_matrix(d::InsuranceLogit,p::parMC{T}) where T
    # Store Parameters
    cost_nonRisk = p.C_nonrisk
    C_r = similar(p.pars.μ_ij')
    risk_long = rIndS(d.data)
    for idxitr in values(d.data._personDict)
        r_ind = Int.(risk_long[idxitr])
        @inbounds r_cost = p.risks[:,r_ind]
        c_nr = cost_nonRisk[idxitr]
        cost_mat = calc_cmat(r_cost,c_nr)
        C_r[idxitr,:] = cost_mat
    end
    return C_r
end


function calc_cmat(r::Matrix{T},c::Vector{T}) where T
    (N,K) = size(r)
    c_hat_capped = Matrix{T}(undef,K,N)
    for n in 1:N
        for i in 1:K
            @inbounds @fastmath cost = r[n,i]*c[i]
            cost_capped = capped_cost(cost)
            @inbounds @fastmath c_hat_capped[i,n] = cost_capped
        end
    end
    return c_hat_capped
end
