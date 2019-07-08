
function compute_nonprice!(d::InsuranceLogit,firm::firmData)
    # Calculate μ_ij, which depends only on parameters
    for app in eachperson(d.data)
        nonprice_value!(app,firm)
    end
    return Nothing
end

function nonprice_value!(app::ChoiceData,firm::firmData)
    p = firm.par_dem
    γ_0 = p.γ_0
    γ = p.γ
    β_0= p.β_0
    β = p.β
    fe = p.FE
    randIndex = app._randCoeffs

    ind = person(app)[1]
    r_ind = Int(rIndS(app)[1])
    idxitr = app._personDict[ind]
    X = permutedims(prodchars(app),(2,1))
    Z = demoRaw(app)[:,1]
    #F = fixedEffects(app)
    F = fixedEffects(app,idxitr)

    β_z = β*Z
    demos = γ_0 + dot(γ,Z)
    #### No Price! ###
    X[:,1].=0.0
    chars_0 = X*(β_0+β_z)

    # FE is a row Vector
    # if T== Float64
    controls = zeros(size(F,2))
    for k in 1:length(controls)
        for j in app._rel_fe_Dict[ind]
            controls[k]+= fe[j]*F[j,k]
        end
    end
    # else
    #     controls = fe*F
    # end

    K = length(idxitr)

    for k = 1:K
        @fastmath d = exp(chars_0[k] + demos + controls[k])
        firm.δ_nonprice[idxitr[k]] = d
    end

    return Nothing
end



function compute_price!(d::InsuranceLogit,firm::firmData)
    # Calculate μ_ij, which depends only on parameters
    for app in eachperson(d.data)
        price_value!(app,firm)
    end
    return Nothing
end

function price_value!(app::ChoiceData,firm::firmData)
    p = firm.par_dem
    γ_0 = p.γ_0
    γ = p.γ
    β_0= p.β_0
    β = p.β

    ind = person(app)[1]
    idxitr = app._personDict[ind]
    Z = demoRaw(app)[:,1]
    X = permutedims(prodchars(app),(2,1))

    β_z = β*Z

    #### No Price! ###
    p_ij = firm.P_ij[idxitr]
    chars_0 = p_ij.*((β_0+β_z)[1])


    K = length(idxitr)

    for k = 1:K
        @fastmath d = exp(chars_0[k])
        firm.δ_price[idxitr[k]] = d
    end

    return Nothing
end



function individual_update(d::InsuranceLogit,firm::firmData)
    # Store Parameters
    p_dem = firm.par_dem
    p_cost = firm.par_cost
    δ_long = (firm.δ_nonprice).*(firm.δ_price)
    μ_ij_large = p_dem.μ_ij
    μnr_ij_large = p_dem.μ_ij_nonRisk
    risk_long = rIndS(d.data)
    cost_nonRisk = p_cost.C_nonrisk
    any_long = anyHCC(d.data)

    for idxitr in values(d.data._personDict)
        δ = δ_long[idxitr]
        @inbounds u = μ_ij_large[:,idxitr]
        @inbounds u_nr = μnr_ij_large[idxitr]
        r_ind = Int.(risk_long[idxitr])
        @inbounds r_cost = p_cost.risks[:,r_ind]
        @inbounds r_scores = d.draws[:,r_ind]
        @inbounds any_r = any_long[idxitr[1]]
        c_nr = cost_nonRisk[idxitr]
        # s_r,c,c_HCC,dc,dc_HCC,d2c,d2c_HCC = calc_cost(u,δ,r_cost,r_scores,c_nr,anyHCC_scores)
        s_r,c_r = calc_cost(u,δ,r_cost,r_scores,c_nr)
        s_nr = calc_shares(u_nr,δ)

        s_hat = (any_r.*s_r + (1-any_r).*s_nr)
        firm.c_pred[idxitr] = (any_r.*s_r.*c_r + (1-any_r).*s_nr.*c_nr)./s_hat
        firm.s_pred[idxitr] = s_hat
    end
    return Nothing
end


function calc_cost(μ_ij::Array{Float64},δ::Vector{Float64},r::Matrix{T},r_sc::Matrix{Float64},c::Vector{T};returnMat::Bool=false) where T
    (N,K) = size(μ_ij)
    util = Matrix{Float64}(undef,K,N)
    s_hat = Matrix{Float64}(undef,K,N)
    s_ins = Vector{Float64}(undef,N)
    c_hat = Matrix{T}(undef,K,N)
    c_hat_pool = Matrix{T}(undef,K,N)
    if returnMat
        c_mat = Matrix{T}(undef,K,N)
    end

    for n in 1:N
        expsum = 1.0
        #r_score = r[n,:]
        for i in 1:K
            @inbounds @fastmath a = μ_ij[n,i]*δ[i]
            @inbounds util[i,n] = a
            expsum += a
        end
        si = (expsum-1)/expsum
        s_ins[n] = si
        for i in 1:K
            @inbounds @fastmath s = util[i,n]/expsum
            @inbounds s_hat[i,n] = s

            @inbounds @fastmath cost = (r[n,i]*c[i])
            @inbounds c_hat[i,n] = s*cost
            @inbounds c_hat_pool[i,n] = si*cost
            if returnMat
                c_mat[i,n] = cost
            end
        end
    end

    s_sum  = sum(s_hat,dims=2)
    c_mean = sum(c_hat,dims=2)./s_sum
    c_mean_pool = sum(c_hat_pool,dims=2)./sum(s_ins)


    s_mean = s_sum./N

    if returnMat
        return s_mean,c_mean,c_mean_pool, s_hat,c_mat
    else
        return s_mean, c_mean, c_mean_pool
    end
end

function calc_der!(dsdp::Matrix{Float64},dcdp::Matrix{Float64},dcdp_pl::Matrix{Float64},
    s_hat::Matrix{Float64},c_hat::Matrix{Float64},
    der_ind::Int64,aα::Vector{Float64})
    (K,N) = size(s_hat)

    s_ins = sum(s_hat,dims=1)

    alpha = aα[der_ind]
    for n in 1:N
        s_der = s_hat[der_ind,n]
        si = s_ins[n]
        dsi = alpha*s_der*(1-si)
        for i in 1:K
            if i==der_ind
                @inbounds @fastmath ds = alpha*s_der*(1-s_der)
            else
                @inbounds @fastmath ds = -alpha*s_der*s_hat[i,n]
            end
            dsdp[i,n] = ds
            dcdp[i,n] = ds*c_hat[i,n]
            dcdp_pl[i,n] = dsi*c_hat[i,n]
        end
    end

    ds_mean = mean(dsdp,dims=2)
    dc_mean = mean(dcdp,dims=2)
    dc_mean_pool = mean(dcdp_pl,dims=2)
    return ds_mean, dc_mean, dc_mean_pool
end


function calc_der!(dsdp::Matrix{Float64},dcdp::Matrix{Float64},dcdp_pl::Matrix{Float64},
    s_hat::Matrix{Float64},c_hat::Matrix{Float64},
    der_ind::Int64,aα::Float64)
    (K,N) = size(s_hat)

    s_ins = sum(s_hat,dims=1)

    alpha = aα
    for n in 1:N
        s_der = s_hat[der_ind,n]
        si = s_ins[n]
        dsi = alpha*s_der*(1-si)
        for i in 1:K
            if i==der_ind
                @inbounds @fastmath ds = alpha*s_der*(1-s_der)
            else
                @inbounds @fastmath ds = -alpha*s_der*s_hat[i,n]
            end
            dsdp[i,n] = ds
            dcdp[i,n] = ds*c_hat[i,n]
            dcdp_pl[i,n] = dsi*c_hat[i,n]
        end
    end

    ds_mean = mean(dsdp,dims=2)
    dc_mean = mean(dcdp,dims=2)
    dc_mean_pool = mean(dcdp_pl,dims=2)
    return ds_mean, dc_mean, dc_mean_pool
end

function calc_der(s_hat::Vector{Float64},c_hat::Vector{Float64},
    der_ind::Int64,aα::Vector{Float64})
    K = length(s_hat)

    dsdp = Vector{Float64}(undef,K)
    dcdp = Vector{Float64}(undef,K)
    dcdp_pl = Vector{Float64}(undef,K)

    s_der = s_hat[der_ind]
    si = sum(s_hat)
    dsi = aα[der_ind]*s_der*(1-si)
    for i in 1:K
        if i==der_ind
            @inbounds @fastmath ds = aα[der_ind]*s_der*(1-s_der)
        else
            @inbounds @fastmath ds = -aα[der_ind]*s_der*s_hat[i]
        end
        dsdp[i] = ds
        dcdp[i] = ds*c_hat[i]
        dcdp_pl[i] = dsi*c_hat[i]
    end

    return dsdp, dcdp, dcdp_pl
end

function calc_der(s_hat::Vector{Float64},c_hat::Vector{Float64},
    der_ind::Int64,aα::Float64)
    K = length(s_hat)

    dsdp = Vector{Float64}(undef,K)
    dcdp = Vector{Float64}(undef,K)
    dcdp_pl = Vector{Float64}(undef,K)

    s_der = s_hat[der_ind]
    si = sum(s_hat)
    dsi = aα*s_der*(1-si)
    for i in 1:K
        if i==der_ind
            @inbounds @fastmath ds = aα*s_der*(1-s_der)
        else
            @inbounds @fastmath ds = -aα*s_der*s_hat[i]
        end
        dsdp[i] = ds
        dcdp[i] = ds*c_hat[i]
        dcdp_pl[i] = dsi*c_hat[i]
    end

    return dsdp, dcdp, dcdp_pl
end


function update_derivatives(d::InsuranceLogit,firm::firmData,ST::String;foc_check=false)
    # Store Parameters
    p_dem = firm.par_dem
    p_cost = firm.par_cost
    δ_long = (firm.δ_nonprice).*(firm.δ_price)
    μ_ij_large = p_dem.μ_ij
    μnr_ij_large = p_dem.μ_ij_nonRisk
    risk_long = rIndS(d.data)
    cost_nonRisk = p_cost.C_nonrisk
    any_long = anyHCC(d.data)
    wgt_long = weight(d.data)
    prod_long = Int.(product(d.data))
    demData = demoRaw(d.data)
    age_long = firm[:ageRate]
    mem_long = firm[:MEMBERS]

    N = size(d.draws,1)
    if ST=="All"
        pers = d.data._personIDs
        prod_ind = firm.prods
    else
        pers = firm._perSTDict[ST]
        prod_ind = firm._prodSTDict[ST]
    end
# for idxitr in values(d.data._personDict)
    for p in pers
        idxitr = d.data._personDict[p]
        δ = δ_long[idxitr]
        @inbounds u = μ_ij_large[:,idxitr]
        @inbounds u_nr = μnr_ij_large[idxitr]
        r_ind = Int.(risk_long[idxitr])
        @inbounds r_cost = p_cost.risks[:,r_ind]
        @inbounds r_scores = d.draws[:,r_ind]
        @inbounds any_r = any_long[idxitr[1]]
        wgt = wgt_long[idxitr]
        c_nr = cost_nonRisk[idxitr]

        s_r,c_r,c_r_pl,s_mat,c_mat = calc_cost(u,δ,r_cost,r_scores,c_nr,returnMat=true)
        s_nr = calc_shares(u_nr,δ)
        s_r_ins = sum(s_r)
        s_nr_ins = sum(s_nr)

        s_hat = (any_r.*s_r + (1-any_r).*s_nr)
        c_hat = (any_r.*s_r.*c_r + (1-any_r).*s_nr.*c_nr)./s_hat

        s_ins_hat = (any_r.*s_r_ins + (1-any_r).*s_nr_ins)
        c_pool = (any_r.*s_r_ins.*c_r_pl + (1-any_r).*s_nr_ins.*c_nr)./s_ins_hat

        firm.c_pred[idxitr] = c_hat[:]
        firm.c_pool[idxitr] = c_pool[:]
        firm.s_pred[idxitr] = s_hat[:]


        @inbounds Z = demData[:,idxitr[1]]
        @inbounds a = age_long[idxitr[1]]
        @inbounds m = mem_long[idxitr[1]]
        aα = ((12/1000)*(a/m)*(p_dem.β_0 + p_dem.β*Z)[1])#.*(1 .-Ze_prem)
        if foc_check
            Ze_prem = firm.zero_ij[idxitr]
            aα = aα.*(1 .- Ze_prem)
        end

        prod_ids = firm.stdMap[prod_long[idxitr]]
        rev = firm.Rev_ij[idxitr]

        K = length(idxitr)
        dsdp_mat = Matrix{Float64}(undef,K,N)
        dcdp_mat = Matrix{Float64}(undef,K,N)
        dcdp_pl_mat = Matrix{Float64}(undef,K,N)

        for k in 1:length(prod_ids)
            j = prod_ids[k]

            dsdp_r, dcdp_r, dcdp_r_pl = calc_der!(dsdp_mat,dcdp_mat,dcdp_pl_mat,s_mat,c_mat,k,aα)
            dsdp_nr, dcdp_nr, dcdp_nr_pl = calc_der(s_nr,c_nr,k,aα)

            @fastmath dsdp =  (any_r.*dsdp_r + (1-any_r).*dsdp_nr)
            @fastmath dcdp =  (any_r.*dcdp_r + (1-any_r).*dcdp_nr)


            dsdp_pl = sum(dsdp)
            @fastmath dcdp_pl =  (any_r.*dcdp_r_pl + (1-any_r).*dcdp_nr_pl)#./s_ins_hat - (dsdp_pl./s_ins_hat).*c_pool
            # @fastmath dcdp_pl = dcdp_pl.*s_hat + c_pool.*dsdp

            firm.C_j[j] += wgt[k]*s_hat[k]*c_hat[k]
            firm.PC_j[j] += wgt[k]*s_ins_hat*c_pool[k]
            firm.SA_j[j] += wgt[k]*s_hat[k]*(a/m)
            firm.S_j[j] += wgt[k]*s_hat[k]
            firm.Mkt_j[j] += wgt[k]*s_ins_hat

            for l in 1:length(prod_ids)
                @inbounds @fastmath firm.dSdp_j[j,prod_ids[l]]+= wgt[l]*dsdp[l]
                @inbounds @fastmath firm.dSAdp_j[j,prod_ids[l]]+= wgt[l]*dsdp[l]*(a/m)
                @inbounds @fastmath firm.dRdp_j[j,prod_ids[l]]+= wgt[l]*(dsdp[l]*rev[l])
                @inbounds @fastmath firm.dCdp_j[j,prod_ids[l]]+= wgt[l]*dcdp[l]
                @inbounds @fastmath firm.dCdp_pl_j[j,prod_ids[l]]+= wgt[l]*dcdp_pl[l]
                @inbounds @fastmath firm.dMdp_j[j,prod_ids[l]]+= wgt[l]*dsdp_pl
                if l==k
                    firm.dRdp_j[j,prod_ids[l]]+= wgt[l]*(s_hat[l]*a/m)
                end
            end
        end
    end

    non_catas = firm.prods[.!(inlist(firm.prods,firm.catas_prods))]


    firm.PC_j[prod_ind] = firm.PC_j[prod_ind]./firm.Mkt_j[prod_ind]#.*firm.S_j

    firm.Mkt_j = firm.poolMat*firm.S_j
    TotalCosts = firm.C_j[:]
    TotalCosts[firm.catas_prods].=0.0
    PooledCosts = (firm.PC_j[:].*firm.S_j[:])
    PooledCosts[firm.catas_prods].=0.0

    TotalCosts = firm.poolMat*TotalCosts
    PooledCosts = firm.poolMat*PooledCosts
    firm.Adj_j = zeros(length(firm.PC_j))
    firm.Adj_j[non_catas] = (TotalCosts./PooledCosts)[non_catas]



    for j in prod_ind, k in prod_ind
        firm.dCdp_pl_j[j,k] = (firm.dCdp_pl_j[j,k]/firm.Mkt_j[k]*firm.S_j[k] +
                                        firm.dSdp_j[j,k]*firm.PC_j[k] -
                                       (firm.dMdp_j[j,k]/firm.Mkt_j[k])*firm.PC_j[k]*firm.S_j[k])
    end
    # firm.PC_j[firm.prods] = firm.PC_j[firm.prods].*Adj_j[firm.prods]
    # dAdj_dp = firm.poolMat
    dC_pool = copy(firm.dCdp_j)
    dC_pool[:,firm.catas_prods].=0.0

    dPC_pool = copy(firm.dCdp_pl_j)
    dPC_pool[:,firm.catas_prods].=0.0

    dAdj_dp = zeros(length(firm.PC_j))
    dAdj_dp[prod_ind] = (sum(dC_pool,dims=2)./PooledCosts - (sum(dPC_pool,dims=2)./PooledCosts).*firm.Adj_j)[prod_ind]
    dAdj_dp = dAdj_dp.*firm.poolMat

    for j in prod_ind, k in prod_ind
        firm.dCdp_pl_j[j,k] = firm.Adj_j[k]*firm.dCdp_pl_j[j,k] + dAdj_dp[j,k]*firm.PC_j[k]*firm.S_j[k]
    end

    firm.dCdp_pl_j[:,firm.catas_prods] = firm.dCdp_j[:,firm.catas_prods]
    firm.PC_j[firm.catas_prods] = firm.C_j[firm.catas_prods]./firm.S_j[firm.catas_prods]
    firm.C_j = firm.C_j./firm.S_j

    return nothing
end
