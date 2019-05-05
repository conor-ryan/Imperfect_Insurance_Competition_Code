mutable struct parMC{T}
    ## Decision Parameters
    pars::parDict{Float64}
    ## MC Parameters
    β_MC::Vector{T}

    ## Risk Cost
    risks::Matrix{T}

    ## Cost without risks
    C_nonrisk::Vector{T}
    ## Total Average Cost, with risks
    C::Vector{T}
    ## HCC Average Cost
    C_HCC::Vector{T}
    ## Cost Derivative w.r.t. the risk parameter
    dCdr::Vector{T}
    dCdr_HCC::Vector{T}
    d2Cdr::Vector{T}
    d2Cdr_HCC::Vector{T}

    ## Choices conditional on 0 HCC
    s_hat_nonrisk::Vector{T}
    ## Choices conditional on >0 HCC
    s_hat_risk::Vector{T}

    ## Moment Derivatives
    # grad::Matrix{T}
    # hess::Array{T,3}
end

struct MC_Data
    # Matrix of the data (transposed, pre-sorted)
    data::Matrix{Float64}

    # Matrix of Fixed Effects
    fixedEffects::Matrix{Float64}

    # Vector of HCC Prevalence
    anyHCC::Vector{Float64}

    # Parameter Indices
    _baseIndex::Vector{Int}
    _riskIndex::Int
    _feIndex::Vector{Int}

    # Length Counters
    mom_length::Int
    par_length::Int

    #State-Level Sampling Dictionary
    _stDict::Dict{Int,Array{Int,1}}

    # Moment Values
    _firmMomentDict::Dict{Int,Array{Int,1}}
    _firmMomentBit::Dict{Int,BitArray{1}}
    _firmMomentProdDict::Dict{Int,Array{Int,1}}
    firmMoments::Vector{Float64}

    _metalMomentDict::Dict{Int,Array{Int,1}}
    _metalMomentBit::Dict{Int,BitArray{1}}
    _metalMomentProdDict::Dict{Int,Array{Int,1}}
    metalMoments::Vector{Float64}

    _ageMomentDict::Dict{Int,Array{Int,1}}
    _ageMomentBit::Dict{Int,BitArray{1}}
    ageMoments::Vector{Float64}

    riskMoment::Float64
end

function MC_Data(data_choice::DataFrame,
                mom_firm::DataFrame,
                mom_metal::DataFrame,
                mom_age::DataFrame,
                mom_risk::DataFrame;
        baseSpec=[:Age,:AV],
        fixedEffects=Vector{Symbol}(undef,0),
        constMoments = true)
    # Get the size of the data
    n, k = size(data_choice)

    # Convert everything to an array once for performance
    data = convert(Matrix{Float64},data_choice[baseSpec])

    println("Create Fixed Effects")
    bigFirm = false
    F, feNames = build_FE(data_choice,fixedEffects,bigFirm = bigFirm)
    F = permutedims(F,(2,1))

    index = Dict{Symbol, Int}()
    for l=1:size(data,2)
        k+=1
        index[baseSpec[l]] = k
    end

    data = permutedims(data,(2,1))

    anyHCC = data_choice[:Any_HCC]

    ## Parameter Specification
    _baseIndex = 1:length(baseSpec)
    _riskIndex = maximum(_baseIndex) + 1
    _feIndex = maximum(_riskIndex) .+ (1:size(F,1))

    println("Check Collinearity")
    all_data = vcat(data[_baseIndex,:],F)
    X = all_data*all_data'
    ev = sort(eigvals(X))
    smallest_ev = ev[1]
    println("Smallest Data Eigenvalue: $smallest_ev")

    ### State-Level Sampling Dictionary
    println("Sampling Dictionary")
    # st_vec = data_choice[:ST]
    states = sort(unique(data_choice[:ST]))
    st_vec = Vector{Int}(undef,length(data_choice[:ST]))
    for (k,s) in enumerate(states)
        st_vec[data_choice[:ST].==s] .= k
    end

    _stDict_temp = build_ProdDict(st_vec)
    _stDict = Dict{Int,Array{Int,1}}()
    person_vec = data_choice[:Person]
    for (st,ind) in _stDict_temp
        _stDict[st] = unique(person_vec[ind])
    end



    ### Moment Dictionaries
    println("Construct Moments")
    all_idx = 1:n

    _firmMomentDict = Dict{Int,Array{Int64,1}}()
    _firmMomentBit = Dict{Int,BitArray{1}}()
    _firmMomentProdDict = Dict{Int,Array{Int64,1}}()
    moments = sort(unique(mom_firm[:M_num]))
    firmMoments = Vector{Float64}(undef,length(moments))
    if constMoments
        for m in moments
            m_df_index = findall(mom_firm[:M_num].==m)
            m_index = mom_firm[:index][m_df_index]
            _firmMomentDict[m] = m_index
            _firmMomentBit[m] = inlist(all_idx,m_index)
            _firmMomentProdDict[m] = sort(unique(mom_firm[:Product][m_df_index]))
            firmMoments[m] = mom_firm[:logAvgCost][m_df_index][1]
        end
    end

    _metalMomentDict = Dict{Int,Array{Int64,1}}()
    _metalMomentBit = Dict{Int,BitArray{1}}()
    _metalMomentProdDict = Dict{Int,Array{Int64,1}}()
    moments = sort(unique(mom_metal[:M_num]))
    metalMoments = Vector{Float64}(undef,length(moments))
    if constMoments
        for m in moments
            m_df_index = findall(mom_metal[:M_num].==m)
            m_index = mom_metal[:index][m_df_index]
            _metalMomentDict[m] = m_index
            _metalMomentBit[m] = inlist(all_idx,m_index)
            _metalMomentProdDict[m] = sort(unique(mom_metal[:Product][m_df_index]))
            metalMoments[m] = mom_metal[:costIndex][m_df_index][1]
        end
    end

    _ageMomentDict = Dict{Int,Array{Int64,1}}()
    _ageMomentBit = Dict{Int,BitArray{1}}()
    moments = sort(unique(mom_age[:M_num]))
    ageMoments = Vector{Float64}(undef,length(moments))
    if constMoments
        for m in moments
            m_index = mom_age[:index][findall(mom_age[:M_num].==m)]
            _ageMomentDict[m] = m_index
            _ageMomentBit[m] = inlist(all_idx,m_index)
            ageMoments[m] = mom_age[:costIndex][findall(mom_age[:M_num].==m)][1]
        end
    end

    riskMoment = mom_risk[:costIndex][2]


    ### Lengths
    mom_length = length(firmMoments)  + (length(metalMoments)-1) + (length(ageMoments)-1) + length(riskMoment)
    par_length = length(_baseIndex) + length(_riskIndex) + length(_feIndex)



    return MC_Data(data,F,anyHCC,_baseIndex,_riskIndex,_feIndex,
                    mom_length,par_length,_stDict,
                    _firmMomentDict,_firmMomentBit,_firmMomentProdDict,firmMoments,
                    _metalMomentDict,_metalMomentBit,_metalMomentProdDict,metalMoments,
                    _ageMomentDict,_ageMomentBit,ageMoments,riskMoment)
end



function parMC(p_MC::Vector{T},par_est::parDict{Float64},d::InsuranceLogit,c::MC_Data) where T

    # Non Risk Costs
    basepars = p_MC[c._baseIndex]' * c.data
    fepars = p_MC[c._feIndex]' * c.fixedEffects
    C_nonrisk = basepars + fepars
    C_nonrisk = exp.(C_nonrisk[:])

    # Risk Costs
    risks = exp.(d.draws.*p_MC[c._riskIndex])

    ## Total Average Cost, with risks
    C = Vector{T}(undef,length(C_nonrisk))
    ## HCC Average Cost
    C_HCC = Vector{T}(undef,length(C_nonrisk))

    ## Cost Derivatives
    dC = Vector{T}(undef,length(C_nonrisk))
    dC_HCC = Vector{T}(undef,length(C_nonrisk))
    d2C = Vector{T}(undef,length(C_nonrisk))
    d2C_HCC = Vector{T}(undef,length(C_nonrisk))

    ## Total Average Cost, with risks
    s_nr = Vector{T}(undef,length(C_nonrisk))
    ## HCC Average Cost
    s_r  = Vector{T}(undef,length(C_nonrisk))

    ### Moment Characteristics
    # grad = Matrix{T}(undef,c.mom_length+2,c.par_length)
    # hess = Array{T,3}(undef,c.mom_length+2,c.par_length,c.par_length)

    return parMC(par_est,p_MC,risks,C_nonrisk,C,C_HCC,dC,dC_HCC,d2C,d2C_HCC,s_nr,s_r)
end


function individual_costs(d::InsuranceLogit,p::parMC{T}) where T
    # Store Parameters
    δ_long = p.pars.δ
    μ_ij_large = p.pars.μ_ij
    risk_long = rInd(d.data)
    cost_nonRisk = p.C_nonrisk
    anyHCC_ind = Float64.(d.draws.>0)
    for idxitr in values(d.data._personDict)
        δ = δ_long[idxitr]
        @inbounds u = μ_ij_large[:,idxitr]
        r_ind = Int.(risk_long[idxitr])
        @inbounds r_cost = p.risks[:,r_ind]
        @inbounds r_scores = d.draws[:,r_ind]
        @inbounds anyHCC_scores = anyHCC_ind[:,r_ind]
        c_nr = cost_nonRisk[idxitr]
        s,s_r,s_nr,c,c_HCC,dc,dc_HCC,d2c,d2c_HCC = calc_cost(u,δ,r_cost,r_scores,c_nr,anyHCC_scores)
        p.pars.s_hat[idxitr] = s
        p.C[idxitr] = c
        p.C_HCC[idxitr] = c_HCC
        p.dCdr[idxitr] = dc
        p.dCdr_HCC[idxitr] = dc_HCC
        p.d2Cdr[idxitr] = d2c
        p.d2Cdr_HCC[idxitr] = d2c_HCC
        p.s_hat_nonrisk[idxitr] = s_nr
        p.s_hat_risk[idxitr] = s_r
    end
    return Nothing
end


function calc_cost(μ_ij::Array{Float64},δ::Vector{Float64},r::Matrix{T},r_sc::Matrix{Float64},c::Vector{T},anyHCC::Matrix{Float64}) where T
    (N,K) = size(μ_ij)
    util = Matrix{Float64}(undef,K,N)
    s_hat = Matrix{Float64}(undef,K,N)
    s_hat_risk = Matrix{Float64}(undef,K,N)
    c_hat = Matrix{T}(undef,K,N)
    c_hat_risk = Matrix{T}(undef,K,N)
    dc_hat = Matrix{T}(undef,K,N)
    dc_hat_risk = Matrix{T}(undef,K,N)
    d2c_hat = Matrix{T}(undef,K,N)
    d2c_hat_risk = Matrix{T}(undef,K,N)

    for n in 1:N
        expsum = 1.0
        #r_score = r[n,:]
        for i in 1:K
            @inbounds @fastmath a = μ_ij[n,i]*δ[i]
            @inbounds util[i,n] = a
            expsum += a
        end
        for i in 1:K
            @inbounds @fastmath s = util[i,n]/expsum
            @inbounds s_hat[i,n] = s
            @inbounds @fastmath s_hat_risk[i,n] = s*(anyHCC[n,i])
            @inbounds @fastmath cost = s*(r[n,i]*c[i])
            @inbounds @fastmath dcost = r_sc[n,i]*cost
            @inbounds @fastmath d2cost = r_sc[n,i]^2*cost

            @inbounds c_hat[i,n] = cost
            @inbounds @fastmath c_hat_risk[i,n] = cost*(anyHCC[n,i])

            @inbounds dc_hat[i,n] = dcost
            @inbounds @fastmath dc_hat_risk[i,n] = dcost*(anyHCC[n,i])

            @inbounds d2c_hat[i,n] = d2cost
            @inbounds @fastmath d2c_hat_risk[i,n] = d2cost*(anyHCC[n,i])
        end
    end
    N_r = sum(anyHCC,dims=1)[1]
    # s_mean = mean(s_hat,dims=2)
    s_sum  = sum(s_hat,dims=2)
    s_risk_sum  = sum(s_hat_risk,dims=2)
    c_mean = sum(c_hat,dims=2)./s_sum
    c_mean_risk = sum(c_hat_risk,dims=2)./s_risk_sum

    dc_mean = sum(dc_hat,dims=2)./s_sum
    dc_mean_risk = sum(dc_hat_risk,dims=2)./s_risk_sum

    d2c_mean = sum(d2c_hat,dims=2)./s_sum
    d2c_mean_risk = sum(d2c_hat_risk,dims=2)./s_risk_sum

    s_mean = s_sum./N
    s_mean_risk = s_risk_sum./N_r
    s_mean_nonrisk = (s_sum - s_risk_sum)./(N-N_r)

    return s_mean, s_mean_risk, s_mean_nonrisk, c_mean, c_mean_risk, dc_mean, dc_mean_risk, d2c_mean, d2c_mean_risk
end



function costMoments(c::MC_Data,d::InsuranceLogit,p::parMC{T}) where T
    s_hat = p.pars.s_hat
    s_hat_nonrisk = p.s_hat_nonrisk
    s_hat_risk = p.s_hat_risk
    wgts = weight(d.data)[:]
    actuarial_values = c.data[2,:]

    wgts_share = wgts.*s_hat
    any_share = wgts.*s_hat_risk.*c.anyHCC
    none_share = wgts.*s_hat_nonrisk.*(1 .- c.anyHCC)

    c_hat = p.C
    c_hat_nonHCC = p.C_nonrisk
    c_hat_HCC = p.C_HCC

    c_hat_total = c_hat./actuarial_values
    c_hat_nonHCC_total = p.C_nonrisk./actuarial_values
    c_hat_HCC_total = p.C_HCC./actuarial_values

    fmom = Vector{T}(undef,length(c.firmMoments))
    mmom = Vector{T}(undef,length(c.metalMoments)-1)
    amom = Vector{T}(undef,length(c.ageMoments)-1)

    ## Firm Moments
    for (m,m_idx) in c._firmMomentDict
        c_avg = sliceMean_wgt(c_hat,wgts_share,m_idx)
        fmom[m] = log(c_avg) - c.firmMoments[m]
        # t = sum(test[m_idx])
        # if t>0.0
        #     println(m)
        #     println(t)
        # end
        # pmom[m] = (c_avg - exp(c.avgMoments[m]))/100
        # pmom[m] = c_avg
    end

    ## Metal Moments
    refval = sliceMean_wgt(c_hat,wgts_share,c._metalMomentDict[1])
    for (m,m_idx) in c._metalMomentDict
        if m==1
            continue
        else
            c_avg = sliceMean_wgt(c_hat,wgts_share,m_idx)
            mmom[m-1] = c_avg/refval[1] - c.metalMoments[m]
        end
    end

    ## Age Moments
    refval = sliceMean_wgt(c_hat_total,wgts_share,c._ageMomentDict[1])
    for (m,m_idx) in c._ageMomentDict
        if m==1
            continue
        else
            # avg_act = sum(wgts_share[m_idx].*actuarial_values[m_idx])/sum(wgts_share[m_idx])
            # println(m)
            # println(avg_act)
            c_avg = sliceMean_wgt(c_hat_total,wgts_share,m_idx)
            #println("$m: $c_avg")
            amom[m-1] = c_avg/refval[1] - c.ageMoments[m]
        end
    end
    all_idx = Int.(1:length(s_hat))
    HCC_avg = sliceMean_wgt(c_hat_HCC_total,any_share,all_idx)
    non_avg = sliceMean_wgt(c_hat_nonHCC_total,none_share,all_idx)
    rmom = HCC_avg/non_avg - c.riskMoment

    return vcat(fmom,mmom,amom,rmom)
    # return vcat(pmom,amom,rmom)
end


function costMoments(c::MC_Data,d::InsuranceLogit,p::Array{T},p_est::parDict{Float64}) where T
    par = parMC(p,p_est,d,c) # Fix p0
    individual_costs(d,par)
    mom = costMoments(c,d,par)
    return mom
end




# function costMoments_test(c::MC_Data,d::InsuranceLogit,p::parMC{T}) where T
#     s_hat = p.pars.s_hat
#     s_hat_nonrisk = p.s_hat_nonrisk
#     s_hat_risk = p.s_hat_risk
#     wgts = weight(d.data)[:]
#     actuarial_values = c.data[2,:]
#
#     # wgts_share = wgts.*s_hat
#     any_share = wgts.*s_hat_risk.*c.anyHCC
#     none_share = wgts.*s_hat_nonrisk.*(1 .- c.anyHCC)
#
#     # c_hat = p.C
#     c_hat_nonHCC = p.C_nonrisk
#     c_hat_HCC = p.C_HCC
#
#     # c_hat_total = c_hat./actuarial_values
#     c_hat_nonHCC_total = p.C_nonrisk./actuarial_values
#     c_hat_HCC_total = p.C_HCC./actuarial_values
#     #
#     # pmom = Vector{T}(undef,length(c.avgMoments))
#     # amom = Vector{T}(undef,length(c.ageMoments)-1)
#
#     ## Product and Firm Moments
#     # m = 1
#     # m_idx = c._avgMomentDict[m]
#     # c_avg = sliceMean_wgt(c_hat,wgts_share,m_idx)
#     # pmom = log(c_avg) - c.avgMoments[m]
#
#     all_idx = Int.(1:length(s_hat))
#     HCC_avg = sliceMean_wgt(c_hat_HCC_total,any_share,all_idx)
#     non_avg = sliceMean_wgt(c_hat_nonHCC_total,none_share,all_idx)
#     rmom = HCC_avg/non_avg - c.riskMoment
#
#     return rmom
# end
#
#
# function costMoments_test(c::MC_Data,d::InsuranceLogit,p::Array{T},p_est::parDict{Float64}) where T
#     par = parMC(p,p_est,d,c) # Fix p0
#     individual_costs(d,par)
#     mom = costMoments_test(c,d,par)
#     return mom
# end
