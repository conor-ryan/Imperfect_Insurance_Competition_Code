
function evaluate_model!(e::EqData;init=false)
    ## Unpack Prices from Product sorted Data
    unpack_P!(e)
    ## Adjust Prices to Premium Paid
    premPaid!(e)
    # Sort prices by person
    e.price_ij = e.price_ij[e.data._prod_2_per_map]

    # Calculate Market Shares
    calcShares!(e)

    # Resort Market Shares by Product
    e.price_ij = e.price_ij[e.data._per_2_prod_map]
    e.s_pred = e.s_pred_byperson[e.data._per_2_prod_map]

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
        prems = sort(e.premBase_j[e.silver_index[m]])
        bench_index = min(2,length(prems))
        benchmarkPrem[m] = prems[bench_index]
    end
    benchLong = benchmarkPrem[e.mkt_index_long]
    return benchLong
end

function calcSubsidy!(e::EqData)
    benchmarks = calcBenchmark(e)
    ageRate = e.data[:ageRate]
    incCont = e.data[:IncomeCont]
    N = length(e.subsidy_ij)
    for n in 1:N
        e.subsidy_ij[n] = max(benchmarks[n]*ageRate[n]-incCont[n],0)
    end
    return nothing
end


function premPaid!(e::EqData)
    Mandate = e.data[:Mandate]
    # subsidy = e.data[:subsidy]
    calcSubsidy!(e)
    subsidy = e.subsidy_ij

    ageRate = e.data[:ageRate]
    catas   = e.data[:Catastrophic]
    Mems   = e.data[:MEMBERS]
    N = length(e.price_ij)

    for n in 1:N
        price = max(e.price_ij[n]*ageRate[n]-subsidy[n]*(1-catas[n]),0)
        price = ((price/Mems[n]) - Mandate[n]/12)
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
        exp_sum = 1.0
        util = Vector{Float64}(undef,length(idxitr))
        for (n,k) in enumerate(idxitr)
            util[n] = other_util[k]*exp(alpha[k]*price[k])
            exp_sum+=util[n]
        end
        for (n,k) in enumerate(idxitr)
            e.s_pred_byperson[k] = util[n]/exp_sum
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
#
# function calc_deriv!(deriv::Vector{Float64},prod::Int64,e::EqData)
#     vidx = e.data.index
#     #idx_prod = e.data._per_2_prod_map
#     idx_prod = e.data._prod_2_per_map
#
#     alpha = e.data.data_byperson[:,vidx[:alpha]]
#     ageRate =  e.data.data_byperson[:,vidx[:ageRate_avg]]
#     shares = e.s_pred_byperson
#
#     for i in e.data._personIDs
#         idxitr = e.data._personDict[i]
#         k = get(e.data._person_prod_Dict[i],prod,-1)
#         if k>0
#             s_prod = shares[idxitr[k]]
#         else
#             s_prod = 0.0
#         end
#         for (n,j) in enumerate(idxitr)
#             #println(j)
#             #println(idx_prod[j])
#             if n==k
#                 deriv[idx_prod[j]] = alpha[j]*ageRate[j]*shares[j]*(1-shares[j])
#             else
#                 deriv[idx_prod[j]] = -alpha[j]*ageRate[j]*shares[j]*s_prod
#             end
#         end
#     end
#     return deriv
# end

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
    # A_Gamma_long = e.data[:A_Gamma_j]
    # R_Gamma_long = e.data[:R_Gamma_j]
    #Age_long = e.data[:AGE]
    #WTP_long = e.data[:WTP]
    Cost_long = e.data[:C]
    Cost_nonAV_long = e.data[:C_nonAV]
    weights = e.data[:mkt_density]
    full_weights = e.data[:PERWT]
    ageRate_long_byproduct = e.data[:ageRate_avg]
    weights_byperson = weights[e.data._prod_2_per_map]

    vidx = e.data.index
    alpha_long = e.data.data_byperson[:,vidx[:alpha]]
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

    share_tilde = zeros(length(S_j))
    for i in 1:length(share_tilde)
        share_tilde[i] = S_m[i]*S_j[i]/S_0[i]*e.RA_share*(1-Catas_j[i])
    end

    share_tilde_f = e.ownMat*share_tilde


    # ## ST_R and ST_A
    # ST_R = sum(share_tilde.*R_Gamma_j) + (1-e.RA_share)*e.Other_R
    # ST_A = sum(share_tilde.*A_Gamma_j)/sum(share_tilde)
    #
    # R_norm_j = R_Gamma_j./ST_R
    # A_norm_j = A_Gamma_j./ST_A
    #
    # T_norm_j = R_norm_j - A_norm_j
    # avgPrem = sum(A_j.*e.premBase_j.*share_tilde)/sum(share_tilde)
    # avgPrem_base = sum(e.premBase_j.*share_tilde)/sum(share_tilde)
    #
    # T_j = T_norm_j.*avgPrem.*(1-Catas_j)
    #
    # T_j_fix = (R_Gamma_j./e.ST_R_fix -
    #                 A_Gamma_j./e.ST_A_fix).*e.avgPrem_fix.*(1-Catas_j)

    #### Market Pooled Cost ####
    All_Lives = sum_over_all(e.s_pred,full_weights)
    all_age_avg = sum_over_all(ageRate_long,full_weights)/sum(full_weights)

    PC = sum_over_all(e.s_pred,full_weights,Cost_nonAV_long)/All_Lives
    PC = PC/all_age_avg
    PC = e.C_AV_j.*PC
    # PC = sum(S_m.*S_j.*e[:C])/sum(S_m.*S_j)
    Ec = (sum(Cost_nonAV_long.*full_weights)/sum(full_weights))/all_age_avg
    Ec = e.C_AV_j.*Ec
    τ = PC - Ec



    #### Derivatives By Product ####
    dsdp = Matrix{Float64}(undef,J,J)
    dsdp_rev = Matrix{Float64}(undef,J,J)

    dCost = Matrix{Float64}(undef,J,J)
    dAllCost = Matrix{Float64}(undef,J,J)
    dsdp_rev_dτ = Matrix{Float64}(undef,J,J)

    # dTdp = Matrix{Float64}(J,J)
    # dTdp_avgPrice = Matrix{Float64}(J,J)
    # dTdp_norm = Matrix{Float64}(J,J)
    # davgCost = Matrix{Float64}(J,J)
    # dPool_dA = Matrix{Float64}(J,J)
    # dPool_dNorm = Matrix{Float64}(J,J)
    # dTdp_fix = Matrix{Float64}(J,J)

    (N,M) = size(e.data.data)
    deriv_long = Vector{Float64}(undef,N)
    deriv_long_byperson = Vector{Float64}(undef,N)
    # deriv_all_long = Vector{Float64}(N)

    # margCost = Vector{Float64}(J)
    # markup = Vector{Float64}(J)
    # margCost_est = Vector{Float64}(J)
    # avgCost_est = Vector{Float64}(J)
    # pooledCost = Vector{Float64}(J)

    dsdp_0 = Vector{Float64}(undef,J)
    dsdp_rev_0 = Vector{Float64}(undef,J)

    for (n,j) in enumerate(e.prods)
        #println(j)
        m_idx = e.mkt_index[e.mkt_map[n]]
        @views cross_idx = e.data._crossprod_Dict[:,n]
        #deriv_long = calc_deriv!(deriv_long,j,e)
        calc_deriv!(deriv_long,deriv_long_byperson,j,cross_idx,
                                    alpha_long,ageRate_long,e)
        dsdp[:,n] = sum_by_prod(deriv_long,e,weights)
        dsdp_rev[:,n] = sum_by_prod(deriv_long,ageRate_long_byproduct,e,weights)

        dsdp_0[n] = sum(dsdp[:,n])
        dsdp_rev_0[n] = sum(dsdp_rev[:,n])

        #dAge_j = chg_in_avg(deriv_long,:AGE,Age_long,S_j,dsdp[:,n],e,weights)
        #dWTP_j = chg_in_avg(deriv_long,:WTP,WTP_long,S_j,dsdp[:,n],e,weights)

        #dCost[:,n] = (dAge_j.*e.cost_pars[:Age_j] + dWTP_j.*e.cost_pars[:WTP_j]).*e.Cost_j
        # dR_Gamma_j = chg_in_avg(deriv_long,:R_Gamma_j,R_Gamma_long,
        #                 S_j,dsdp[:,n],e,weights)
        #
        # dA_Gamma_j = chg_in_avg(deriv_long,:A_Gamma_j,A_Gamma_long,
        #                 S_j,dsdp[:,n],e,weights)

        dCost[:,n] = sum_by_prod(deriv_long,Cost_long,e,weights)
        dAllCost[:,n] = (dCost[:,n]./e.C_AV_j).*e.C_AV_j[n]

        # deriv_long_byperson = deriv_long[e.data._prod_2_per_map]

        ## Need to correct for better pooled cost
        # gsum_by_person(deriv_all_long,deriv_long_byperson,e)
        # deriv_all_long = deriv_all_long[e.data._per_2_prod_map]
        # dMCost[:,n] = sum_by_prod(deriv_all_long,Cost_long,e,weights)


        ## Unique Costs per Person
        # Costs_thisProd = zeros(length(Cost_long))
        # Costs_thisProd[e.data._productDict[j]] = Cost_long[e.data._productDict[j]]
        # Cost_long_byperson = Costs_thisProd[e.data._prod_2_per_map]
        # cost_byperson = sum_by_person(Cost_long_byperson,e,weights_byperson)
        #

        # PC = sum_over_all(e.s_pred,full_weights,Cost_nonAV_long)/All_Lives
        dτ = sum_over_all(deriv_long,Cost_nonAV_long,full_weights)/All_Lives .-
                        sum_over_all(deriv_long,full_weights)/All_Lives*PC
        dτ = e.C_AV_j[n].*dτ./all_age_avg
        #
        dsdp_rev_dτ[:,n] = dsdp_rev[:,n].*(1 .+dτ)

        # ######### IGNORE ACTUAL POLICY ##########
        # ds_0_dp = -sum(dsdp[:,n].*(1-Catas_j))
        # # Check ds_M_dp for catastrophic plans
        # ds_M_dp = ds_0_dp*S_m[n].*S_m
        # ds_M_dp_mkt = -ds_0_dp*S_m[n]*(1-S_m[n])
        # S_0_mkt = S_0[n]
        # S_m_mkt = S_m[n]
        #
        # ds_tilde_dp = ds_M_dp.*S_j./S_0.*(1-Catas_j)
        #
        # if Catas_j[n]==0
        #     ds_tilde_dp[m_idx] = (ds_M_dp_mkt.*S_j[m_idx]./S_0_mkt +
        #                     dsdp[m_idx,n].*S_m_mkt/S_0_mkt +
        #                     S_m_mkt.*ds_0_dp.*S_j[m_idx]./(S_0_mkt)^2).*(1-Catas_j[m_idx])
        # else
        #     ds_tilde_dp[m_idx] = ds_M_dp_mkt.*S_j[m_idx]./S_0[m_idx].*(1-Catas_j[m_idx])
        # end
        #
        # dR_norm = dR_Gamma_j/ST_R -
        #         R_norm_j.*sum(ds_tilde_dp.*R_Gamma_j + share_tilde.*dR_Gamma_j)/ST_R
        #
        # dA_norm = dA_Gamma_j/ST_A -
        #         A_norm_j.*sum(ds_tilde_dp.*A_Gamma_j + share_tilde.*dA_Gamma_j)/ST_A
        #
        # dTnorm_dp = (dR_norm - dA_norm).*(1-Catas_j)
        #
        # # Check the dAge_j term in this. Should it be average?
        # dP_s = sum(ds_tilde_dp.*A_j.*e.premBase_j + share_tilde.*dsdp_rev[:,n].*e.premBase_j)
        # dP_s+= share_tilde[n]*A_j[n]
        #
        # dTdp[:,n] = (dTnorm_dp.*avgPrem + T_norm_j*dP_s).*(1-Catas_j)
        # dTdp_avgPrice[:,n] = (T_norm_j*dP_s).*(1-Catas_j)
        # dTdp_norm[:,n] = (dTnorm_dp.*avgPrem).*(1-Catas_j)
        # dTdp_fix[:,n] = (dR_Gamma_j./e.ST_R_fix -
        #                     dA_Gamma_j./e.ST_A_fix).*e.avgPrem_fix.*(1-Catas_j)
        #
        #
        # davgCost[:,n] = ((dR_Gamma_j./ST_R).*avgPrem).*(1-Catas_j)
        # dPool_dA[:,n] = ((dA_Gamma_j./ST_A).*avgPrem).*(1-Catas_j)
        # dPool_dNorm[:,n] = (A_norm_j.*sum(ds_tilde_dp.*A_Gamma_j + share_tilde.*dA_Gamma_j)/ST_A -
        #             R_norm_j.*sum(ds_tilde_dp.*R_Gamma_j + share_tilde.*dR_Gamma_j)/ST_R).*avgPrem.*(1-Catas_j)
        #
        # avgCost_est[n] = R_norm_j[n]*avgPrem.*(1-Catas_j[n])
        # pooledCost[n] = A_norm_j[n]*avgPrem.*(1-Catas_j[n])
        ##################################################

    end


    ## Output Needed dsdp, dsdp_rev, S_j, A_j
    ## Mkt_lives, T_j, dTdp, dTdp_fix,
    ## dAge, dWTP

    ## Calculate First Order Conditions
    # foc_err = Vector{Float64}(J)
    # P_new = Vector{Float64}(J)
    # P_test = Vector{Float64}(J)
    # foc_Std = Vector{Float64}(J)
    # foc_RA = Vector{Float64}(J)
    # foc_RA_fix = Vector{Float64}(J)
    # foc_merge = Vector{Float64}(J)
    #
    # RA_term1 = Vector{Float64}(J)
    # RA_term2 = Vector{Float64}(J)
    # RA_term3 = Vector{Float64}(J)
    # RA_term4 = Vector{Float64}(J)
    # RA_term5 = Vector{Float64}(J)

    P_Std = Vector{Float64}(undef,J)
    P_RA= Vector{Float64}(undef,J)
    P_RAτ= Vector{Float64}(undef,J)

    for (m,m_idx) in e.mkt_index
        L_m = S_m[m_idx][1]
        # P = e.premBase_j[m_idx]

        S = S_j[m_idx]
        Sf = share_tilde_f[m_idx]
        A = A_j[m_idx]
        m_PC = PC[m_idx]
        m_Ec = Ec[m_idx]

        #### IGNORE ACTUAL POLICY
        # T = T_j[m_idx]
        # T_fix = T_j_fix[m_idx]
        #C = e.Cost_j[m_idx]

        m_dsdp = dsdp[m_idx,m_idx].*e.ownMat[m_idx,m_idx]
        m_dsdp_rev = dsdp_rev[m_idx,m_idx].*e.ownMat[m_idx,m_idx]
        all_dsdp_rev = dsdp_rev[m_idx,m_idx]
        m_dsdp_rev_dτ = dsdp_rev_dτ[m_idx,m_idx].*e.ownMat[m_idx,m_idx]
        # m_dTdp = dTdp[:,m_idx].*e.ownMat[:,m_idx]
        # m_dTdp_Price = dTdp_avgPrice[:,m_idx].*e.ownMat[:,m_idx]
        # m_dTdp_Norm  = dTdp_norm[:,m_idx].*e.ownMat[:,m_idx]

        # m_dAC = davgCost[m_idx,m_idx].*e.ownMat[m_idx,m_idx]
        # m_dP_A = dPool_dA[m_idx,m_idx].*e.ownMat[m_idx,m_idx]
        # m_dP_Norm = dPool_dNorm[:,m_idx].*e.ownMat[:,m_idx]

        # m_dTdp_fix = dTdp_fix[:,m_idx].*e.ownMat[:,m_idx]
        m_dCost = dCost[m_idx,m_idx].*e.ownMat[m_idx,m_idx]
        #all_dCost = dMCost[m_idx,m_idx].*e.ownMat[m_idx,m_idx]
        all_dCost = dAllCost[m_idx,m_idx]
        #all_dCost = dCost[m_idx,m_idx]
        #m_dCost = sum(m_dCost,1)'
        #
        # m_dCost_mgd = dCost[m_idx,m_idx].*e.ownMat_merge[m_idx,m_idx]
        # m_dCost_mgd = sum(m_dCost_mgd,1)'
        #
        # m_dsdp_mgd = dsdp[m_idx,m_idx].*e.ownMat_merge[m_idx,m_idx]
        # m_dTdp_mgd = dTdp[:,m_idx].*e.ownMat_merge[:,m_idx]

        #rev = inv(m_dsdp_rev)*(m_dsdp_rev*P + S.*A)
        # markup = inv(m_dsdp_rev)*S.*A
        # tran = inv(L_m*m_dsdp_rev)*(L_m*m_dsdp*T + m_dTdp'*(S_m.*S_j))
        # tran_fix = inv(L_m*m_dsdp_rev)*(L_m*m_dsdp*T_fix + m_dTdp_fix'*(S_m.*S_j))
        # cost = inv(m_dsdp_rev)*(m_dsdp*C + m_dCost*S)

        # The cost matrix should maybe be transposed...
        #foc_Std[m_idx] = L_m*( S.*A - (m_dsdp*C + m_dCost'*S) )
        # foc_Std[m_idx] = L_m*( S.*A - m_dCost)
        # foc_RA[m_idx] = L_m*m_dsdp*T + m_dTdp'*(S_m.*S_j)
        # foc_RA_fix[m_idx] = L_m*m_dsdp*T_fix + m_dTdp_fix'*(S_m.*S_j)
        # foc_merge[m_idx] =  L_m*( S.*A - m_dCost_mgd) + L_m*m_dsdp_mgd*T + m_dTdp_mgd'*(S_m.*S_j)

        P_Std[m_idx] = inv(m_dsdp_rev)*(-S.*A + sum(m_dCost,dims=1)')

        P_RA[m_idx] = inv(m_dsdp_rev)*(-S.*A + m_dsdp_rev*m_PC - all_dsdp_rev*m_PC.*Sf + sum(all_dCost,dims=1)'.*Sf)

        P_RAτ[m_idx] = inv(m_dsdp_rev_dτ)*(-S.*A) .+ m_Ec

        # P_new[m_idx] = -inv(L_m*m_dsdp_rev)*(L_m*( S.*A - (m_dsdp*C + m_dCost*S) ) +
        #                         L_m*m_dsdp*T + m_dTdp'*(S_m.*S_j) )
        # foc_err[m_idx] = (rev+tran-cost)./100

        # P_new[m_idx] = -inv(L_m*m_dsdp_rev)*(L_m*( S.*A - (m_dsdp*C + m_dCost*S) ) )
        # foc_err[m_idx] = (rev-cost)./100

        # markup[m_idx] = -inv(m_dsdp_rev)*(S.*A)
        # margCost[m_idx] = inv(m_dsdp_rev)*(m_dCost)
        # RA_term1[m_idx] = -inv(m_dsdp_rev)*(m_dsdp*T)
        # RA_term2[m_idx] = -inv(m_dsdp_rev)*(m_dAC'*S)
        # RA_term3[m_idx] =  inv(m_dsdp_rev)*(m_dP_A'*S)
        # RA_term4[m_idx] = -inv(L_m*m_dsdp_rev)*(m_dP_Norm'*(S_m.*S_j))
        # RA_term5[m_idx] = -inv(L_m*m_dsdp_rev)*(m_dTdp_Price'*(S_m.*S_j))

        #margCost_est[m_idx] = inv(m_dsdp_rev)*(m_dAC'*S) + avgCost_est[m_idx]
    end

    # P_foc = -inv(S_m.*dsdp_rev.*e.ownMat)*(foc_Std + foc_RA)
    # P_test = markup + margCost + RA_term1 + RA_term2 + RA_term3 + RA_term4 + RA_term5
    # ## Decomposition
    # # Estimated Marginal Cost - Pooled Cost + Pooled Cost
    # PC = (avgPrem/ST_A)./A_Gamma_j
    # margCost_est = -(RA_term1 + RA_term2 + RA_term3) + PC
    # margCost_est_2 = -(RA_term1 + RA_term2) + avgPrem.*A_Gamma_j/ST_A
    #
    # Pooled_Cost = inv(dsdp_rev.*e.ownMat)*(dsdp_rev_0.*PC).*(A_Gamma_j/ST_A)
    # share_weights = e.ownMat*(S_j.*S_m)/sum(S_j.*S_m)
    # PC_wtd = PC.*share_weights
    #
    # mkt_MargCost = (RA_term4 + PC_wtd)./share_weights
    #
    # efficiency = margCost - margCost_est
    #
    # P_decomp = markup +
    #             efficiency + PC +
    #             share_weights.*mkt_MargCost +
    #             - share_weights.*PC +
    #             RA_term5
    #
    # P_marg = markup + efficiency + mkt_MargCost + RA_term5
    # P_pool = markup + efficiency + PC + RA_term5

    # Average Error, for consistency across product numebers
    #return foc_Std, foc_RA, foc_RA_fix, foc_merge, S_m, dsdp_rev
    return P_Std, P_RA, P_RAτ, τ
end
