function calc_consumer(d::InsuranceLogit,firm::firmData)
    # Store Parameters
    p_dem = firm.par_dem
    p_cost = firm.par_cost
    δ_long = (firm.δ_nonprice).*(firm.δ_price)
    μ_ij_large = p_dem.μ_ij
    μnr_ij_large = p_dem.μ_ij_nonRisk

    any_long = anyHCC(d.data)


    N = size(d.draws,1)
    pers = d.data._personIDs
    prod_ind = firm.prods

    CW_long = Vector{Float64}(undef,length(pers))
    CW_r_long = Vector{Float64}(undef,length(pers))
    CW_nr_long = Vector{Float64}(undef,length(pers))

    for (i,p) in enumerate(pers)
        idxitr = d.data._personDict[p]

        δ = δ_long[idxitr]
        @inbounds u = μ_ij_large[:,idxitr]
        @inbounds u_nr = μnr_ij_large[idxitr]

        CW_r = mean(log.(1 .+ u*δ))
        CW_nr = mean(log.(1 .+ sum(u_nr.*δ)))

        CW_r_long[i] = CW_r
        CW_nr_long[i] = CW_nr

        @inbounds any_r = any_long[idxitr[1]]
        CW = (any_r.*CW_r + (1-any_r).*CW_nr)
        CW_long[i] = CW
    end
    return CW_long, CW_r_long, CW_nr_long, pers
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
