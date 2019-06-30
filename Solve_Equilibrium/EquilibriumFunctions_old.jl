
function evaluate_model!(e::EqData;init=false,foc_check=false)
    ## Unpack Prices from Product sorted Data
    unpack_P!(e)
    ## Adjust Prices to Premium Paid
    premPaid!(e,foc_check=foc_check)
    # Sort prices by person
    e.price_ij = e.price_ij[e.data._prod_2_per_map]

    # Calculate Market Shares
    calcShares!(e)

    # Resort Market Shares by Product
    e.price_ij = e.price_ij[e.data._per_2_prod_map]
    e.s_pred = e.s_pred_byperson[e.data._per_2_prod_map]
    e.s_ins = e.s_ins_byperson[e.data._per_2_prod_map]

    ## Calculate Product Averages
    calcProd_Avg!(e)

    ## Set Cost
    #e.Cost_j[:] = e.Cost_base_j.*exp.(e[:AGE].*e.cost_pars[:Age_j] + e[:WTP].*e.cost_pars[:WTP_j])

    ## Set Fixed RA Parameters
    if init
        calc_RA_fix!(e)
    end

    return nothing
end


function unpack_P!(e::EqData)
    p_print = e.premBase_j
    n = 1
    for j in e.prods
        idx_j = e.data._productDict[j]
        for idx in idx_j
            e.price_ij[idx] = e.premBase_j[n]
        end
        n+=1
    end
    return nothing
end

function calcBenchmark(e::EqData)
    mkts = keys(e.mkt_index)
    benchmarkPrem = Vector{Float64}(undef,length(mkts))
    for m in mkts
        prems = e.premBase_j[e.mkt_index[m]][e.silver_index[m]]
        hix_cnts = e.hix_cnt[e.mkt_index[m]][e.silver_index[m]]
        ind = sortperm(prems)
        if (hix_cnts[ind][1]>1) | (length(prems)==1)
            bench_index = 1
        else
            bench_index = 2
        end
        benchmarkPrem[m] = prems[ind][bench_index]
    end
    benchLong = benchmarkPrem[e.mkt_index_long]
    return benchLong
end

function origBenchmark(e::EqData)
    mkts = keys(e.mkt_index)
    benchmarkPrem = Vector{Float64}(undef,length(mkts))
    for m in mkts
        benchmarkPrem[m] = unique(e.bench_base[e.mkt_index[m]])[1]
    end
    benchLong = benchmarkPrem[e.mkt_index_long]
    return benchLong
end

function calcSubsidy!(e::EqData;foc_check=false)
    if foc_check
        benchmarks = origBenchmark(e)
    else
        benchmarks = calcBenchmark(e)
    end
    ageRate = e.data[:ageRate]
    incCont = e.data[:IncomeCont]
    N = length(e.subsidy_ij)
    for n in 1:N
        e.subsidy_ij[n] = max(benchmarks[n]*ageRate[n]-incCont[n]*1000/12,0)
    end
    return nothing
end


function premPaid!(e::EqData;foc_check=false)
    Mandate = e.data[:Mandate]
    # subsidy = e.data[:subsidy]
    calcSubsidy!(e,foc_check=foc_check)
    subsidy = e.subsidy_ij

    ageRate = e.data[:ageRate]
    catas   = e.data[:Catastrophic]
    Mems   = e.data[:MEMBERS]
    N = length(e.price_ij)

    for n in 1:N
        price = max(e.price_ij[n]*ageRate[n]-subsidy[n]*(1-catas[n]),0)
        price = ((price*12/Mems[n]) - Mandate[n])/1000
        e.price_ij[n] = price
    end
    return nothing
end

function calcShares!(e::EqData)
    vidx = e.data.index
    alpha = e.data.data_byperson[:,vidx[:alpha]]
    other_util = e.data.data_byperson[:,vidx[:non_price_util]]
    price = e.price_ij
    people = e.data._personIDs

    for i in people
        idxitr = e.data._personDict[i]
        exp_sum = 0.0
        # exp_sum = [1.0]
        util = Vector{Float64}(undef,length(idxitr))
        for (n,k) in enumerate(idxitr)
            util[n] = other_util[k]*exp(alpha[k]*1000/12*price[k])
            exp_sum+=util[n]
        end
        for (n,k) in enumerate(idxitr)
            e.s_pred_byperson[k] = util[n]/(1+exp_sum)
            e.s_ins_byperson[k] = exp_sum/(1+exp_sum)
        end
    end
    return nothing
end

function calcProd_Avg!(e::EqData)
    ### Create Product Avg Data
    for (var,l) in e.p_index
        if var in [:S_j,:lives]
            continue
        end
        x_avg = avg_by_prod(e.data[var],e)
        e.p_avg[:,l] = x_avg
    end
    ### Add Marketshares
    #l = length(keys(e.p_index))+1
    l = e.p_index[:S_j]
    S_j = sum_by_prod(e.s_pred,e,e.data[:mkt_density])
    e.p_avg[:,l] = S_j


    ### Add Lives
    l = e.p_index[:lives]
    lives = sum_by_prod(e.s_pred,e,e.data[:PERWT])
    e.p_avg[:,l] = lives
    return nothing
end

function calc_RA_fix!(e::EqData)
    Catas_j = e[:Catastrophic]
    lives = e[:lives].*(1 .- Catas_j)
    A_Gamma_j = e[:A_Gamma_j]
    R_Gamma_j = e[:R_Gamma_j]
    A_j = e[:ageRate_avg]
    S_j = e[:S_j]

    st_lives = sum(lives)
    S_0 = similar(lives)
    S_m = similar(lives)
    for (m,prods) in e.mkt_index
        S_0_temp = 0.0
        mkt_lives = 0.0
        for j in prods
            S_0_temp+=S_j[j]*(1-Catas_j[j])
            mkt_lives+=lives[j]
        end
        S_0[prods] .=  S_0_temp
        S_m[prods] .= mkt_lives/st_lives
    end

    share_tilde = zeros(length(S_j))
    for i in 1:length(share_tilde)
        share_tilde[i] = S_m[i]*S_j[i]/S_0[i]*e.RA_share*(1-Catas_j[i])
    end

    ## ST_R and ST_A
    e.ST_R_fix = sum(share_tilde.*R_Gamma_j) + (1-e.RA_share)*e.Other_R
    e.ST_A_fix = sum(share_tilde.*A_Gamma_j)/sum(share_tilde)

    e.avgPrem_fix = sum(A_j.*e.premBase_j.*share_tilde)/sum(share_tilde)
end


function calc_deriv!(deriv::Vector{Float64},deriv_bp::Vector{Float64},prod::Int64,
                        crossprod_idx::SubArray{Int64,1,S,T,R},
                        alpha::Vector{Float64},ageRate::Vector{Float64},
                        e::EqData) where {S,T,R}
    idx_prod = e.data._prod_2_per_map

    shares = e.s_pred_byperson

    N  = length(shares)
    for n in 1:N
        @inbounds k = crossprod_idx[n]
        if k>0
            @inbounds s_prod = shares[k]
        else
            s_prod = 0.0
        end
        if n==k
            @fastmath @inbounds deriv[idx_prod[n]] = alpha[n]*ageRate[n]*shares[n]*(1-shares[n])
#            @fastmath @inbounds deriv_bp[n] = alpha[n]*ageRate[n]*shares[n]*(1-shares[n])
        else
            @fastmath @inbounds deriv[idx_prod[n]] = -alpha[n]*ageRate[n]*shares[n]*s_prod
#            @fastmath @inbounds deriv_bp[n] = -alpha[n]*ageRate[n]*shares[n]*s_prod
        end
    end
    return deriv
end

function calc_ins_deriv!(deriv::Vector{Float64},
                        crossprod_idx::SubArray{Int64,1,S,T,R},
                        alpha::Vector{Float64},ageRate::Vector{Float64},
                        e::EqData) where {S,T,R}
    idx_prod = e.data._prod_2_per_map

    shares = e.s_pred_byperson
    ins_shares = e.s_ins_byperson

    N  = length(shares)
    for n in 1:N
        @inbounds k = crossprod_idx[n]
        if k>0
            @inbounds s_prod = shares[k]
        else
            s_prod = 0.0
        end
        @fastmath @inbounds deriv[idx_prod[n]] = alpha[n]*ageRate[n]*s_prod*(1-ins_shares[n])
    end
    return deriv
end



function sum_by_prod(x::Vector{Float64},e::EqData,weights::Vector{Float64})
    out = zeros(length(e.prods))
    for (n,j) in enumerate(e.prods)
        idx_j = e.data._productDict[j]
        @fastmath @inbounds @simd for q in idx_j
            out[n]+= x[q]*weights[q]
        end
    end
    return out
end

function sum_by_person(x::Vector{Float64},e::EqData)
    out = zeros(length(e.data._personIDs))
    for (n,i) in enumerate(e.data._personIDs)
        idx_i = e.data._personDict[i]
        for q in idx_i
            out[n]+= x[q]
        end
    end
    return out
end
function gsum_by_person(deriv_all_long::Vector{Float64},x::Vector{Float64},e::EqData)
    cross_idx = e.data._per_2_prod_map
    for (n,i) in enumerate(e.data._personIDs)
        @inbounds idx_i = e.data._personDict[i]
#        idx_ji = cross_idx[idx_i]
        sumvar = 0.0
        for q in idx_i
            sumvar+= x[q]
        end
        deriv_all_long[idx_i] = sumvar
    end
    return deriv_all_long
end


function sum_by_prod(x::Vector{Float64},y::Vector{Float64},
    e::EqData,weights::Vector{Float64})
    out = zeros(length(e.prods))
    for (n,j) in enumerate(e.prods)
        idx_j = e.data._productDict[j]
        tot = 0.0
        @fastmath @inbounds @simd for q in idx_j
            tot+= x[q]*y[q]*weights[q]
        end
        out[n] = tot
    end
    return out
end


function avg_by_prod(x::Vector{Float64},e::EqData)
    out = zeros(length(e.prods))
    weights = e.data[:mkt_density]
    s = e.s_pred
    for (n,j) in enumerate(e.prods)
        idx_j = e.data._productDict[j]
        tot_share = 0.0
        for q in idx_j
            out[n]+= x[q]*weights[q]*s[q]
            tot_share+= weights[q]*s[q]
        end
        out[n] = out[n]/tot_share
    end
    return out
end



function chg_in_avg(deriv::Vector{Float64},v::Symbol,x_long::Vector{Float64},
                    S::Vector{Float64},dsdp::Vector{Float64},e::EqData,
                    weights::Vector{Float64})
    L = length(deriv)
    J = length(S)
    dx_avg_dp = Vector{Float64}(undef,J)

    x_avg = e[v]
    #x_long = e.data[v]
    dxdp = sum_by_prod(deriv,x_long,e,weights)

    for j in 1:J
        dx_avg_dp[j] = (dxdp[j] - dsdp[j]*x_avg[j])/S[j]
    end

    return dx_avg_dp
end


function sum_over_all(x::Vector{Float64},y::Vector{Float64})
    len = length(x)
    sum = 0.0
    @fastmath @inbounds @simd for i in 1:len
        sum+=x[i]*y[i]
    end
    return sum
end

function sum_over_all(x::Vector{Float64},y::Vector{Float64},w::Vector{Float64})
    len = length(x)
    sum = 0.0
    @fastmath @inbounds @simd for i in 1:len
        sum+=x[i]*y[i]*w[i]
    end
    return sum
end




function eval_FOC(e::EqData)
    Catas_j = e[:Catastrophic]
    lives = e[:lives].*(1 .-Catas_j)
    # A_Gamma_j = e[:A_Gamma_j]
    # R_Gamma_j = e[:R_Gamma_j]
    A_j = e[:ageRate_avg]
    S_j = e[:S_j]

    J = length(S_j)

    ## Draw Long Variables
    Cost_long = e.data[:C]
    weights = e.data[:mkt_density]
    full_weights = e.data[:PERWT]
    ageRate_long_byproduct = e.data[:ageRate_avg]
    weights_byperson = weights[e.data._prod_2_per_map]

    vidx = e.data.index
    alpha_long = e.data.data_byperson[:,vidx[:alpha]]
    alpha_long_byprod = e.data.data[:,vidx[:alpha]]
    ageRate_long =  e.data.data_byperson[:,vidx[:ageRate_avg]]

    ### Average Cost for Comparison ###
    st_lives = sum(lives)
    S_0 = similar(lives)
    S_m = similar(lives)
    for (m,prods) in e.mkt_index
        S_0_temp = 0.0
        mkt_lives = 0.0
        for j in prods
            S_0_temp+=S_j[j]*(1-Catas_j[j])
            mkt_lives+=lives[j]
        end
        S_0[prods] .=  S_0_temp
        S_m[prods] .= mkt_lives/st_lives
    end
    counts = ones(length(weights))
    mkt_size = sum_by_prod(full_weights,e,counts)./sum_by_prod(weights,e,counts)
    total_ins = sum(mkt_size.*S_j)

    share_tilde = zeros(length(S_j))
    for i in 1:length(share_tilde)
        share_tilde[i] = S_m[i]*S_j[i]/S_0[i]*e.RA_share*(1-Catas_j[i])
    end

    share_tilde_f = e.ownMat*share_tilde



    #### Market Pooled Cost ####
    All_Lives = sum_over_all(e.s_pred,full_weights)
    all_age_avg = sum_over_all(ageRate_long,full_weights)/sum(full_weights)

    PC = sum(e[:C].*share_tilde)/sum(share_tilde)

    ### Product - Level "Pooled Cost" ###
    ## Currently only calculated using the most local market, not the state.
    PC_j = sum_by_prod(e.s_ins,Cost_long,e,full_weights)
    lives_firm = sum_by_prod(e.s_ins,e,full_weights)
    PC_j = (PC_j./lives_firm)
    #
    # PC_j = PC_j./all_age_avg
    # PC = PC/all_age_avg
    PC_adj = PC/(sum(share_tilde.*PC_j)/sum(share_tilde))

    #
    # # PC = sum(S_m.*S_j.*e[:C])/sum(S_m.*S_j)
    # Ec = (sum(Cost_nonAV_long.*full_weights)/sum(full_weights))/all_age_avg
    # Ec = e.C_AV_j.*Ec
    # τ = PC - Ec
    τ = 0.0


    #### Derivatives By Product ####
    dsdp = Matrix{Float64}(undef,J,J)
    dsdp_rev = Matrix{Float64}(undef,J,J)
    dsdp_ins = Matrix{Float64}(undef,J,J)

    dCost = Matrix{Float64}(undef,J,J)
    dAllCost = Matrix{Float64}(undef,J,J)
    dsdp_rev_dτ = Matrix{Float64}(undef,J,J)


    (N,M) = size(e.data.data)
    deriv_long = Vector{Float64}(undef,N)
    deriv_long_byperson = Vector{Float64}(undef,N)
    deriv_ins_long = Vector{Float64}(undef,N)



    dsdp_0 = Vector{Float64}(undef,J)
    dsdp_rev_0 = Vector{Float64}(undef,J)

    for (n,j) in enumerate(e.prods)
        #println(j)
        m_idx = e.mkt_index[e.mkt_map[n]]
        @views cross_idx = e.data._crossprod_Dict[:,n]
        cross_idx_byprod = cross_idx[e.data._per_2_prod_map]
        #deriv_long = calc_deriv!(deriv_long,j,e)
        calc_deriv!(deriv_long,deriv_long_byperson,j,cross_idx,
                                    alpha_long,ageRate_long,e)
        calc_ins_deriv!(deriv_ins_long,cross_idx,
                                    alpha_long,ageRate_long,e)

        dsdp[:,n] = mkt_size.*sum_by_prod(deriv_long,e,weights)
        dsdp_rev[:,n] = mkt_size.*sum_by_prod(deriv_long,ageRate_long_byproduct,e,weights)

        dsdp_0[n] = sum(dsdp[:,n])
        dsdp_rev_0[n] = sum(dsdp_rev[:,n])


        dCost[:,n] = mkt_size.*sum_by_prod(deriv_long,Cost_long,e,weights)
        dAllCost[:,n] =  mkt_size.*sum_by_prod(deriv_ins_long,Cost_long,e,weights)

    end



    # P_Std = Vector{Float64}(undef,J)
    # P_RA= Vector{Float64}(undef,J)
    mkup = Vector{Float64}(undef,J)
    mc_Std = Vector{Float64}(undef,J)
    mc_RA = Vector{Float64}(undef,J)
    for (m,m_idx) in e.mkt_index
        L_m = S_m[m_idx][1]
        # P = e.premBase_j[m_idx]

        S = mkt_size[m_idx].*S_j[m_idx]
        # S = S_j[m_idx]
        # S_tilde = share_tilde[m_idx]
        # Sf = share_tilde_f[m_idx]
        A = A_j[m_idx]
        m_PC = PC_j[m_idx]
        # m_Ec = Ec[m_idx]

        m_dsdp = dsdp[m_idx,m_idx].*e.ownMat[m_idx,m_idx]
        m_dsdp_rev = dsdp_rev[m_idx,m_idx].*e.ownMat[m_idx,m_idx]
        # all_dsdp_rev = dsdp_rev[m_idx,m_idx]

        m_dSd0 = dsdp_0[m_idx]
        m_margMat = (S*m_dSd0').*e.ownMat[m_idx,m_idx]

        m_dCost = dCost[m_idx,m_idx].*e.ownMat[m_idx,m_idx]
        all_dCost = dAllCost[m_idx,m_idx].*e.ownMat[m_idx,m_idx]

        # P_Std[m_idx] = inv(m_dsdp_rev)*(-S.*A + sum(m_dCost,dims=1)')
        #
        # P_RA[m_idx] = inv(m_dsdp_rev)*(-S.*A + PC_adj*(m_dsdp*m_PC - (m_margMat*m_PC./sum(S)) + all_dCost'*S./sum(S)))

        mkup[m_idx]   = inv(m_dsdp_rev)*(-S.*A)
        mc_Std[m_idx] = inv(m_dsdp_rev)*(sum(m_dCost,dims=1)')
        mc_RA[m_idx]  = inv(m_dsdp_rev)*(PC_adj*(m_dsdp*m_PC - (m_margMat*m_PC./total_ins) + all_dCost'*S./total_ins))
        # mc_RA[m_idx]  = inv(m_dsdp_rev)*(PC_adj*(m_dsdp*m_PC))

    end
    P_Std = mkup + mc_Std
    P_RA = mkup + mc_RA
    T_j = e[:C] - PC_adj.*PC_j

    ### Catastrophic Plans Aren't Adjusted ###
    P_RA[Catas_j.==1] = P_Std[Catas_j.==1]

    return P_Std, P_RA, mkup, mc_Std, mc_RA, T_j
end
