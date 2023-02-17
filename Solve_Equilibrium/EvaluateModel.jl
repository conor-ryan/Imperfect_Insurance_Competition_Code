#### OVERWRITE DEMAND FUNCTION FOR SILVER HHC R ESTIMATES
function individual_shares(d::InsuranceLogit,p::parDict{T}) where T
    # Store Parameters
    δ_long = p.δ
    μ_ij_large = p.μ_ij
    μ_ij_nr_large = p.μ_ij_nonRisk
    risk_long = rIndS(d.data) ## Change Relative to Demand Function
    ageHCC_long = ageHCC(d.data)
    any_long = anyHCC(d.data)
    for i in d.data._personIDs
        idxitr = d.data._personDict[i]
        anyR = any_long[idxitr][1]
        δ = δ_long[idxitr]
        u = μ_ij_large[:,idxitr]
        u_nr= μ_ij_nr_large[idxitr]
        r_ind = Int.(risk_long[idxitr])
        r_scores = d.draws[:,r_ind]
        r_age_scores = ageHCC_long[idxitr]
        s,r = calc_shares(u,δ,r_scores,r_age_scores)
        s_nr = calc_shares(u_nr,δ)
        s_mean = anyR .* s + (1-anyR) .* s_nr
        p.s_hat[idxitr] = s_mean
        p.r_hat[idxitr] =( (anyR.*s.*r) + ((1-anyR).*s_nr.*r_age_scores) )./s_mean
    end

    #### Print Insurance Rate ###
    # ins_rate = sum(p.s_hat.*weight(d.data)[:])/sum(weight(d.data).*choice(d.data))
    # println("Aggregate Insurance Rate: $ins_rate")

    return Nothing
end

#### OVERWRITE CALC SHARES TO INSERT MINIMUM SHARE
function calc_shares(μ_ij::Matrix{T},δ::Vector{T},r::Matrix{Float64},r_age::Vector{Float64}) where T
    (N,K) = size(μ_ij)
    util = Matrix{T}(undef,K,N)
    s_hat = Matrix{T}(undef,K,N)
    r_hat = Matrix{T}(undef,K,N)

    for n in 1:N
        expsum = 1.0
        #r_score = r[n,:]
        for i in 1:K
            a = μ_ij[n,i]*δ[i]
            util[i,n] = a
            expsum += a
        end
        for i in 1:K
            s = util[i,n]/expsum
            s_hat[i,n] = max(s,1e-9)
            r_hat[i,n] = s*(r[n,i] + r_age[i])
        end
    end
    s_mean = mean(s_hat,dims=2)
    r_mean = sum(r_hat,dims=2)./sum(s_hat,dims=2)
    return s_mean, r_mean
end

function calc_shares(μ_ij::Vector{T},δ::Vector{T}) where T
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
        s_hat[i] = max(s,1e-9)
    end
    return s_hat
end

function calc_shares(μ_ij::Matrix{T},δ::Vector{T}) where T
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
            s_hat[i,n] = max(util[i,n]/expsum,1e-9)
        end
    end
    s_mean = mean(s_hat,dims=2)
    return s_mean
end

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
        c_nr = capped_cost(c_nr)
        # s_r,c,c_HCC,dc,dc_HCC,d2c,d2c_HCC = update_cost(u,δ,r_cost,r_scores,c_nr,anyHCC_scores)
        s_r,c_r = update_cost(u,δ,r_cost,r_scores,c_nr)
        s_nr = calc_shares(u_nr,δ)

        s_hat = (any_r.*s_r + (1-any_r).*s_nr)
        firm.c_pred[idxitr] = (any_r.*s_r.*c_r + (1-any_r).*s_nr.*c_nr)./s_hat
        firm.s_pred[idxitr] = s_hat
    end
    return Nothing
end


function update_cost(μ_ij::Array{Float64},δ::Vector{Float64},r::Matrix{T},c::Vector{T},catas::Vector{Int64};returnMat::Bool=false) where T
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
        si = (expsum-1-sum(util[catas,n]))/expsum
        s_ins[n] = si
        for i in 1:K
            @inbounds @fastmath s = util[i,n]/expsum
            @inbounds s_hat[i,n] = max(s,1e-9)

            @inbounds @fastmath cost = (r[n,i]*c[i])
            cost = capped_cost(cost)
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
    der_ind::Int64,aα::Vector{Float64},catas::Vector{Int64})
    (K,N) = size(s_hat)

    s_ins = sum(s_hat,dims=1) - sum(s_hat[catas,:],dims=1)
    catas_prod = der_ind in catas

    alpha = aα[der_ind]
    for n in 1:N
        s_der = s_hat[der_ind,n]
        si = s_ins[n]
        if catas_prod
            dsi = -alpha*s_der*si
        else
            dsi = alpha*s_der*(1-si)
        end
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
    der_ind::Int64,aα::Float64,catas::Vector{Int64})
    (K,N) = size(s_hat)

    s_ins = sum(s_hat,dims=1) - sum(s_hat[catas,:],dims=1)
    catas_prod = der_ind in catas

    alpha = aα
    for n in 1:N
        s_der = s_hat[der_ind,n]
        si = s_ins[n]
        if catas_prod
            dsi = -alpha*s_der*si
        else
            dsi = alpha*s_der*(1-si)
        end
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
    der_ind::Int64,aα::Vector{Float64},catas::Vector{Int64})
    K = length(s_hat)

    dsdp = Vector{Float64}(undef,K)
    dcdp = Vector{Float64}(undef,K)
    dcdp_pl = Vector{Float64}(undef,K)

    s_der = s_hat[der_ind]
    si = sum(s_hat) - sum(s_hat[catas])
    dsi = aα[der_ind]*s_der*(1-si)
    catas_prod = der_ind in catas
    if catas_prod
        dsi = -aα[der_ind]*s_der*si
    else
        dsi = aα[der_ind]*s_der*(1-si)
    end
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
    der_ind::Int64,aα::Float64,catas::Vector{Int64})
    K = length(s_hat)

    dsdp = Vector{Float64}(undef,K)
    dcdp = Vector{Float64}(undef,K)
    dcdp_pl = Vector{Float64}(undef,K)

    s_der = s_hat[der_ind]
    si = sum(s_hat) - sum(s_hat[catas])
    catas_prod = der_ind in catas
    if catas_prod
        dsi = -aα*s_der*si
    else
        dsi = aα*s_der*(1-si)
    end

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
    if ST=="All"
        pers = Int.(d.data._personIDs)
        prod_ind = firm.prods
    else
        pers = firm._perSTDict[ST]
        prod_ind = firm._prodSTDict[ST]
    end
    update_derivatives(d,firm,pers,prod_ind,foc_check=foc_check)
    return nothing
end
function update_derivatives(d::InsuranceLogit,firm::firmData,mkt::Int;foc_check=false)
    pers = firm._perMktDict[mkt]
    prod_ind = firm.mkt_index[mkt]
    update_derivatives(d,firm,pers,prod_ind,foc_check=foc_check)
    return nothing
end

function update_derivatives(d::InsuranceLogit,firm::firmData,
    pers::Vector{Int},prod_ind::Vector{Int};foc_check=false)
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
    sub_long = firm.subsidy_ij
    mandate_long = firm[:Mandate]

    N = size(d.draws,1)
# for idxitr in values(d.data._personDict)
    for p in pers
        idxitr = d.data._personDict[p]
        # prod_ids = firm.stdMap[prod_long[idxitr]]
        prod_ids =prod_long[idxitr]
        catas = findall(inlist(prod_ids,firm.catas_prods))
        subs = sub_long[idxitr]
        mand = mandate_long[idxitr]
        subs_pos = Float64.(any(subs.>0))

        δ = δ_long[idxitr]
        @inbounds u = μ_ij_large[:,idxitr]
        @inbounds u_nr = μnr_ij_large[idxitr]
        r_ind = Int.(risk_long[idxitr])
        @inbounds r_cost = p_cost.risks[:,r_ind]
        @inbounds any_r = any_long[idxitr[1]]
        wgt = wgt_long[idxitr]
        c_nr = cost_nonRisk[idxitr]
        c_nr = capped_cost(c_nr)
        Ze_prem = firm.zero_ij[idxitr]

        s_r,c_r,c_r_pl,s_mat,c_mat = update_cost(u,δ,r_cost,c_nr,catas,returnMat=true)
        s_nr = calc_shares(u_nr,δ)
        s_r_ins = sum(s_r) - sum(s_r[catas])
        s_nr_ins = sum(s_nr) - sum(s_nr[catas])

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
        # aα = ((12/1000)*(a)*(p_dem.β_0 + p_dem.β*Z)[1])#.*(1 .-Ze_prem)
        if foc_check
            aα = aα.*(1 .- Ze_prem)
        end


        rev = firm.Rev_ij[idxitr]

        K = length(idxitr)
        dsdp_mat = Matrix{Float64}(undef,K,N)
        dcdp_mat = Matrix{Float64}(undef,K,N)
        dcdp_pl_mat = Matrix{Float64}(undef,K,N)

        for k in 1:length(prod_ids)
            j = prod_ids[k]

            dsdp_r, dcdp_r, dcdp_r_pl = calc_der!(dsdp_mat,dcdp_mat,dcdp_pl_mat,s_mat,c_mat,k,aα,catas)
            dsdp_nr, dcdp_nr, dcdp_nr_pl = calc_der(s_nr,c_nr,k,aα,catas)

            @fastmath dsdp =  (any_r.*dsdp_r + (1-any_r).*dsdp_nr)
            @fastmath dcdp =  (any_r.*dcdp_r + (1-any_r).*dcdp_nr)


            dsdp_pl = sum(dsdp) - sum(dsdp[catas])
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
                @inbounds @fastmath firm.dMAdp_j[j,prod_ids[l]]+= wgt[l]*dsdp_pl*(a/m)*(1-subs_pos)
                @inbounds @fastmath firm.dSubdp_j[j,prod_ids[l]]+= wgt[l]*dsdp[l]*(subs[l]/m + mand[l]/12)
                if l==k
                    firm.dRdp_j[j,prod_ids[l]]+= wgt[l]*(s_hat[l]*a/m)
                end
            end
        end
    end


    #Complete Computation of Pooled Cost Avg and Derivatives
    firm.PC_j[prod_ind] = firm.PC_j[prod_ind]./firm.Mkt_j[prod_ind]#.*firm.S_j

    for j in prod_ind, k in prod_ind
        firm.dCdp_pl_j[j,k] = (firm.dCdp_pl_j[j,k]/firm.Mkt_j[k]*firm.S_j[k] +
                                        firm.dSdp_j[j,k]*firm.PC_j[k] -
                                       (firm.dMdp_j[j,k]/firm.Mkt_j[k])*firm.PC_j[k]*firm.S_j[k])
    end

    # Catastrophic plans don't have a pooled cost
    firm.dCdp_pl_j[:,firm.catas_prods] = firm.dCdp_j[:,firm.catas_prods]
    firm.PC_j[firm.catas_prods] = firm.C_j[firm.catas_prods]./firm.S_j[firm.catas_prods]



    # Total Pooled Avg Cost per Market
    TotalCost = firm.C_j[:]
    TotalCost[firm.catas_prods].=0.0

    #Total Pooled Lives
    TotalLives = firm.S_j[:]
    TotalLives[firm.catas_prods].=0.0
    poolMat = firm.poolMat[prod_ind,prod_ind]

    TotalAvgCost = zeros(length(firm.PC_j))
    TotalLives[prod_ind] = (poolMat*TotalLives[prod_ind])
    TotalAvgCost[prod_ind] =  (poolMat*TotalCost[prod_ind])./TotalLives[prod_ind]

    #Derivatives of Pooled Lives and Cost
    dLives = copy(firm.dSdp_j)
    dLives[:,firm.catas_prods].=0.0
    dLives = sum(dLives,dims=2)

    dAvgCost = zeros(length(firm.PC_j))
    dC_pool = copy(firm.dCdp_j)
    dC_pool[:,firm.catas_prods].=0.0
    dAvgCost[prod_ind] = (sum(dC_pool,dims=2)[prod_ind])./TotalLives[prod_ind] + TotalAvgCost[prod_ind].*(dLives[prod_ind]./TotalLives[prod_ind]) 

    #Create Transfer Index
    Transfer = zeros(length(firm.PC_j))
    Transfer[prod_ind] = (firm.PC_j[prod_ind].*firm.S_j[prod_ind]./firm.C_j[prod_ind])#.*TotalAvgCost[firm.prods]

    #Derivative of Transfer Index
    dTransfer = zeros(length(firm.PC_j),length(firm.PC_j))
    for j in prod_ind, k in prod_ind
        dTransfer[j,k] = (firm.dCdp_pl_j[j,k]./firm.C_j[k] - (firm.dCdp_j[j,k]/firm.C_j[k])*Transfer[k])
    end
    Transfer[firm.catas_prods].=0.0

    # Difference Out Mean Index (Budget Neutrality)
    Γ = zeros(length(firm.PC_j))
    Γ[prod_ind] = poolMat*(Transfer[prod_ind].*firm.S_j[prod_ind])./TotalLives[prod_ind]
    Γ[firm.catas_prods].=0.0

    dΓ = zeros(length(firm.PC_j))
    for j in prod_ind, k in prod_ind
        dΓ[k] += (dTransfer[j,k]*firm.S_j[k] + Transfer[k]*firm.dSdp_j[j,k])/TotalLives[k]
    end
    dΓ[prod_ind] = dΓ[prod_ind] + (dLives[prod_ind]./TotalLives[prod_ind]).*Γ[prod_ind]


    #Compute Total Risk Adjustment Transfer and Derivative 
    TotTransfer = zeros(length(firm.PC_j))
    TotTransfer[prod_ind] = (Γ[prod_ind] - Transfer[prod_ind]).*TotalAvgCost[prod_ind]

    dTotTransfer = zeros(length(firm.PC_j),length(firm.PC_j))
    for j in prod_ind, k in prod_ind
        dTransfer[j,k] = (dΓ[j] - dTransfer[j,k])*TotalAvgCost[k] + (Γ[k] - Transfer[k])*dAvgCost[j]
    end


    #Compute Actual Marginal Costs with Transfers and Adjustment 
    println(length(firm.ω_j))
    for j in prod_ind, k in prod_ind
        firm.dCdp_j[j,k] = firm.dCdp_j[j,k] + firm.dSdp_j[j,k]*firm.ω_j[k]
        firm.dCdp_pl_j[j,k] = firm.dCdp_j[j,k] - firm.dSdp_j[j,k]*TotTransfer[k] - firm.S_j[k]*dTotTransfer[j,k]
    end

    #Average Real and Risk Adjusted Costs
    firm.C_j = firm.C_j./firm.S_j .+ firm.ω_j
    firm.PC_j = firm.C_j .- TotTransfer

    return nothing
end


function update_shares(d::InsuranceLogit,firm::firmData,mkt::Int;foc_check=false)
    pers = firm._perMktDict[mkt]
    prod_ind = firm.mkt_index[mkt]
    update_shares(d,firm,pers,prod_ind,foc_check=foc_check)
    return nothing
end


function update_shares(d::InsuranceLogit,firm::firmData,
    pers::Vector{Int},prod_ind::Vector{Int};foc_check=false)
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
# for idxitr in values(d.data._personDict)
    for p in pers
        idxitr = d.data._personDict[p]
        # prod_ids = firm.stdMap[prod_long[idxitr]]
        prod_ids =prod_long[idxitr]
        catas = findall(inlist(prod_ids,firm.catas_prods))


        δ = δ_long[idxitr]
        @inbounds u = μ_ij_large[:,idxitr]
        @inbounds u_nr = μnr_ij_large[idxitr]
        r_ind = Int.(risk_long[idxitr])
        @inbounds r_cost = p_cost.risks[:,r_ind]
        @inbounds any_r = any_long[idxitr[1]]
        wgt = wgt_long[idxitr]
        c_nr = cost_nonRisk[idxitr]
        c_nr = capped_cost(c_nr)
        Ze_prem = firm.zero_ij[idxitr]

        s_r,c_r,c_r_pl,s_mat,c_mat = update_cost(u,δ,r_cost,c_nr,catas,returnMat=true)
        s_nr = calc_shares(u_nr,δ)
        s_r_ins = sum(s_r) - sum(s_r[catas])
        s_nr_ins = sum(s_nr) - sum(s_nr[catas])

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
        # aα = ((12/1000)*(a)*(p_dem.β_0 + p_dem.β*Z)[1])#.*(1 .-Ze_prem)
        if foc_check
            aα = aα.*(1 .- Ze_prem)
        end


        rev = firm.Rev_ij[idxitr]

        for k in 1:length(prod_ids)
            j = prod_ids[k]

            firm.C_j[j] += wgt[k]*s_hat[k]*c_hat[k]
            firm.PC_j[j] += wgt[k]*s_ins_hat*c_pool[k]
            firm.SA_j[j] += wgt[k]*s_hat[k]*(a/m)
            firm.S_j[j] += wgt[k]*s_hat[k]
            firm.Mkt_j[j] += wgt[k]*s_ins_hat
        end
    end


    #Complete Computation of Pooled Cost Avg and Derivatives
    firm.PC_j[prod_ind] = firm.PC_j[prod_ind]./firm.Mkt_j[prod_ind]#.*firm.S_j

    # Catastrophic plans don't have a pooled cost
    firm.PC_j[firm.catas_prods] = firm.C_j[firm.catas_prods]./firm.S_j[firm.catas_prods]

    # Total Pooled Avg Cost per Market
    TotalCost = firm.C_j[:]
    TotalCost[firm.catas_prods].=0.0

    #Total Pooled Lives
    TotalLives = firm.S_j[:]
    TotalLives[firm.catas_prods].=0.0
    poolMat = firm.poolMat[prod_ind,prod_ind]

    TotalAvgCost = zeros(length(firm.PC_j))
    TotalLives[prod_ind] = (poolMat*TotalLives[prod_ind])
    TotalAvgCost[prod_ind] =  (poolMat*TotalCost[prod_ind])./TotalLives[prod_ind]

    #Create Transfer Index
    Transfer = zeros(length(firm.PC_j))
    Transfer[prod_ind] = (firm.PC_j[prod_ind].*firm.S_j[prod_ind]./firm.C_j[prod_ind])
    Transfer[firm.catas_prods].=0.0

    # Difference Out Mean Index (Budget Neutrality)
    Γ = zeros(length(firm.PC_j))
    Γ[prod_ind] = poolMat*(Transfer[prod_ind].*firm.S_j[prod_ind])./TotalLives[prod_ind]
    Γ[firm.catas_prods].=0.0

    #Compute Total Risk Adjustment Transfer and Derivative 
    TotTransfer = zeros(length(firm.PC_j))
    TotTransfer[prod_ind] = (Γ[prod_ind] - Transfer[prod_ind]).*TotalAvgCost[prod_ind]

    #Average Real and Risk Adjusted Costs
    firm.C_j = firm.C_j./firm.S_j + firm.ω_j
    firm.PC_j = firm.C_j .- TotTransfer

    return nothing
end


function evaluate_model!(m::InsuranceLogit,f::firmData,ST::String;
    foc_check=false,voucher=false,update_voucher=true,no_policy=false)
    #Clear Derivative Values
    f.dSdp_j[:].=0.0
    f.dSAdp_j[:].=0.0
    f.dMdp_j[:].=0.0
    f.dMAdp_j[:].=0.0
    f.dRdp_j[:].=0.0
    f.dCdp_j[:].=0.0
    f.dCdp_pl_j[:].=0.0
    f.dSubdp_j[:].=0.0
    f.PC_j[:].=0.0
    f.S_j[:].=0.0
    f.C_j[:].=0.0
    f.SA_j[:].=0.0
    f.Mkt_j[:].=0.0
    if !voucher
        update_voucher = true
    end

    if foc_check==false
        premPaid!(f,update_voucher=update_voucher,no_policy=no_policy)
    else
        f.Rev_ij = f[:Rev_foc]
        f.P_ij   = price(m.data)
        f.zero_ij = Float64.((f.P_ij .+ f[:Mandate]/1000 .- 1e-6).<0.0)
    end

    compute_price!(m,f)
    update_derivatives(m,f,ST,foc_check=foc_check)
    return nothing
end

function evaluate_model!(m::InsuranceLogit,f::firmData,mkt::Int;
                        foc_check=false,voucher=false,update_voucher=true,no_policy=false,deriv=true)
    #Clear Derivative Values
    f.dSdp_j[:].=0.0
    f.dSAdp_j[:].=0.0
    f.dMdp_j[:].=0.0
    f.dMAdp_j[:].=0.0
    f.dRdp_j[:].=0.0
    f.dCdp_j[:].=0.0
    f.dCdp_pl_j[:].=0.0
    f.dSubdp_j[:].=0.0
    f.PC_j[:].=0.0
    f.S_j[:].=0.0
    f.C_j[:].=0.0
    f.SA_j[:].=0.0
    f.Mkt_j[:].=0.0
    if !voucher
        update_voucher = true
    end

    if foc_check==false
        premPaid!(f,update_voucher=update_voucher,no_policy=no_policy)
    else
        f.Rev_ij = f[:Rev_foc]
        f.P_ij   = price(m.data)
        f.zero_ij = Float64.((f.P_ij .+ f[:Mandate]/1000 .- 1e-6).<0.0)
    end

    compute_price!(m,f)
    if deriv
        update_derivatives(m,f,mkt,foc_check=foc_check)
    else
        update_shares(m,f,mkt,foc_check=foc_check)
    end
    return nothing
end
