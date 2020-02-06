function calc_consumer(d::InsuranceLogit,firm::firmData)
    # Store Parameters
    p_dem = firm.par_dem
    p_cost = firm.par_cost
    # δ_long = (firm.δ_nonprice).*(firm.δ_price)
    μ_ij_large = p_dem.μ_ij
    μnr_ij_large = p_dem.μ_ij_nonRisk

    any_long = anyHCC(d.data)
    demData = demoRaw(d.data)
    mandate_long = firm[:Mandate]

    N = size(d.draws,1)
    pers = sort(d.data._personIDs)
    prod_ind = firm.prods

    CW_long = Vector{Float64}(undef,length(pers))
    CW_r_long = Vector{Float64}(undef,length(pers))
    CW_nr_long = Vector{Float64}(undef,length(pers))



    for (i,p) in enumerate(pers)
        idxitr = d.data._personDict[p]
        # δ = δ_long[idxitr]
        man = mandate_long[idxitr][1]/1000
        @inbounds u = μ_ij_large[:,idxitr]
        @inbounds u_nr = μnr_ij_large[idxitr]

        @inbounds Z = demData[:,idxitr[1]]
        α = (p_dem.β_0 + p_dem.β*Z)[1]
        α_wf = (12/1000)*α
        tax = exp(α*man)

        #Adjust mandate as a tax rather than a subsidy
        δ_np = firm.δ_nonprice[idxitr]
        δ_p = firm.δ_price[idxitr].*tax
        δ = δ_np.*δ_p

        CW_r = -mean(log.(tax .+ u*δ))/α_wf
        CW_nr = -mean(log.(tax .+ sum(u_nr.*δ)))/α_wf

        CW_r_long[i] = CW_r
        CW_nr_long[i] = CW_nr

        @inbounds any_r = any_long[idxitr[1]]
        CW = (any_r.*CW_r + (1-any_r).*CW_nr)
        CW_long[i] = CW
    end
    return CW_long, CW_r_long, CW_nr_long, pers
end

function calc_cw_mkt(d::InsuranceLogit,firm::firmData,mkt::Int)
    # Store Parameters
    p_dem = firm.par_dem
    p_cost = firm.par_cost
    # δ_long = (firm.δ_nonprice).*(firm.δ_price)
    μ_ij_large = p_dem.μ_ij
    μnr_ij_large = p_dem.μ_ij_nonRisk

    any_long = anyHCC(d.data)
    demData = demoRaw(d.data)
    mandate_long = firm[:Mandate]


    N = size(d.draws,1)
    pers = d.data._personIDs
    prod_ind = firm.prods

    # CW_long = Vector{Float64}(undef,length(pers))
    CW_total = 0.0
    pop_total = 0.0

    for (i,p) in enumerate(firm._perMktDict[mkt])
        idxitr = d.data._personDict[p]
        wgt = weight(d.data)[idxitr[1]]
        man = mandate_long[idxitr][1]/1000
        # δ = δ_long[idxitr]
        @inbounds u = μ_ij_large[:,idxitr]
        @inbounds u_nr = μnr_ij_large[idxitr]

        @inbounds Z = demData[:,idxitr[1]]
        α = (p_dem.β_0 + p_dem.β*Z)[1]
        α_wf = (12/1000)*α
        tax = exp(α*man)

        #Adjust mandate as a tax rather than a subsidy
        δ_np = firm.δ_nonprice[idxitr]
        δ_p = firm.δ_price[idxitr].*tax
        δ = δ_np.*δ_p


        CW_r = -mean(log.(tax .+ u*δ))/α_wf
        CW_nr = -mean(log.(tax .+ sum(u_nr.*δ)))/α_wf


        @inbounds any_r = any_long[idxitr[1]]
        CW = (any_r.*CW_r + (1-any_r).*CW_nr)
        # CW_long[i] = CW
        CW_total += CW*wgt
        pop_total += wgt
    end
    CW_mean = CW_total/pop_total
    return CW_mean
end

function consumer_welfare_bymkt(d::InsuranceLogit,firm::firmData,type::String)
    markets = sort(Int.(keys(firm.mkt_index)))
    cw_mkt = Vector{Float64}(undef,length(markets))
    for mkt in markets
        cw = calc_cw_mkt(d,firm,mkt)
        cw_mkt[mkt] = cw
        # println("CW in Market $mkt: $(cw)")
    end
    file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/consumerWelfare_bymkt_$type-$spec-$rundate.csv"
    output =  DataFrame(markets=markets,CW=cw_mkt)
    CSV.write(file,output)

    return cw_mkt
end

function consumer_welfare(d::InsuranceLogit,firm::firmData,type::String)
    CW_long, CW_r_long, CW_nr_long, people = calc_consumer(d,firm)

    file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/consumerWelfare_$type-$spec-$rundate.csv"
    output =  DataFrame(pers = people,
                        CW = CW_long,
                        CW_risk = CW_r_long,
                        CW_nonrisk = CW_nr_long)
    CSV.write(file,output)
end

function total_welfare_bymkt(d::InsuranceLogit,firm::firmData,type::String;update_voucher=update_voucher)
    markets = sort(Int.(keys(firm.mkt_index)))
    cw_mkt = Vector{Float64}(undef,length(markets))
    gov_mkt = Vector{Float64}(undef,length(markets))
    for mkt in markets
        cw = calc_cw_mkt(d,firm,mkt)
        cw_mkt[mkt] = cw
        # println("CW in Market $mkt: $(cw)")
    end

    prof_mkt = market_profits(d,firm)
    spend_mkt, pop_mkt, ins_mkt = calc_gov_spending(d,firm,update_voucher=update_voucher)
    prof_mkt = prof_mkt./pop_mkt
    spend_mkt = -spend_mkt./pop_mkt
    trans_mkt = market_transfers(d,firm)

    file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/totalWelfare_bymkt_$type-$spec-$rundate.csv"
    output =  DataFrame(markets=markets,CW=cw_mkt,Profit=prof_mkt,Spending=spend_mkt,RA_transfers=trans_mkt,
                        Population=pop_mkt,Insured=ins_mkt)
    CSV.write(file,output)

    return cw_mkt
end

function calc_gov_spending(d::InsuranceLogit,firm::firmData;update_voucher=false)
    if update_voucher
        subsidy_long = firm.subsidy_ij
    else
        subsidy_long = firm.subsidy_ij_voucher
    end

    wgts_long = weight(d.data)[:]
    Mems_long   = firm[:MEMBERS]
    Mandate_long = firm[:Mandate]

    markets = sort(Int.(keys(firm.mkt_index)))
    market_subsidy = zeros(length(markets))
    market_population = zeros(length(markets))
    insured_population = zeros(length(markets))
    for mkt in markets
        for p in firm._perMktDict[mkt]
            idxitr = d.data._personDict[p]
            s_pred = firm.s_pred[idxitr]
            subsidy = subsidy_long[idxitr]
            Mems = Mems_long[idxitr]
            wgt = wgts_long[idxitr]
            mandate = Mandate_long[idxitr]

            for k in 1:length(idxitr)
                market_subsidy[mkt] += wgt[k]*s_pred[k]*(subsidy[k]/Mems[k] + mandate[1]/12)
                insured_population[mkt] += wgt[k]*s_pred[k]
            end
            market_subsidy[mkt] += -wgt[1]*(mandate[1]/12)
            market_population[mkt]+=wgt[1]
        end
    end

    return market_subsidy, market_population, insured_population
end
