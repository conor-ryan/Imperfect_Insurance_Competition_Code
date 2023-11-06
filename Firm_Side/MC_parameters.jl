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
    C_cap::Vector{T}
    C_pool::Vector{T}
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
    ## Total Insured Probability
    s_hat_ins::Vector{T}

    ## Moment Derivatives
    # grad::Matrix{T}
    # hess::Array{T,3}
end

mutable struct MC_Data
    # Matrix of the data (transposed, pre-sorted)
    data::Matrix{Float64}

    # Matrix of Fixed Effects
    fixedEffects::Matrix{Float64}

    # Matrix of Metal Effects
    metalEffects::Matrix{Float64}

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

    _metalMomentDict::Dict{Int,Dict{Int,Array{Int64,1}}}
    _metalMomentBit::Dict{Int,BitArray{1}}
    _metalMomentProdDict::Dict{Int,Dict{Int,Array{Int64,1}}}
    metalMoments::Vector{Float64}

    _ageMomentDict::Dict{Int,Array{Int,1}}
    _ageMomentBit::Dict{Int,BitArray{1}}
    ageMoments::Vector{Float64}

    _agenoMomentDict::Dict{Int,Array{Int,1}}
    _agenoMomentBit::Dict{Int,BitArray{1}}
    agenoMoments::Vector{Float64}

    riskMoment::Float64

    _raMomentDict::Dict{Int,Array{Int,1}}
    _raMomentBit::Dict{Int,BitArray{1}}
    raMoments::Vector{Float64}

    ## Parameter Hold
    fPars::Vector{Float64}
end

function MC_Data(data_choice::DataFrame,
                mom_firm::DataFrame,
                mom_metal::DataFrame,
                mom_age::DataFrame,
                mom_ageno::DataFrame,
                mom_risk::DataFrame,
                mom_ra::DataFrame;
        baseSpec=[:Age,:AV],
        fixedEffects=Vector{Symbol}(undef,0),
        constMoments = true)
    # Get the size of the data
    n, k = size(data_choice)

    # Convert everything to an array once for performance
    data = Array(data_choice[!,baseSpec])

    # Convert everything to an array once for performance
    metal_levels = Array(data_choice[!,[:Catas,:Bronze,:Silver,:Gold,:Platinum]])

    println("Create Fixed Effects")
    bigFirm = false
    F, feNames = build_FE(data_choice,fixedEffects)
    F = permutedims(F,(2,1))

    index = Dict{Symbol, Int}()
    for l=1:size(data,2)
        k+=1
        index[baseSpec[l]] = k
    end

    data = permutedims(data,(2,1))

    anyHCC = data_choice[!,:Any_HCC]

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
    states = sort(unique(data_choice[!,:ST]))
    st_vec = Vector{Int}(undef,length(data_choice[!,:ST]))
    for (k,s) in enumerate(states)
        st_vec[data_choice[!,:ST].==s] .= k
    end

    _stDict_temp = build_ProdDict(st_vec)
    _stDict = Dict{Int,Array{Int,1}}()
    person_vec = data_choice[!,:Person]
    for (st,ind) in _stDict_temp
        _stDict[st] = unique(person_vec[ind])
    end



    ### Moment Dictionaries
    println("Construct Moments")
    all_idx = 1:n

    _firmMomentDict = Dict{Int,Array{Int64,1}}()
    _firmMomentBit = Dict{Int,BitArray{1}}()
    _firmMomentProdDict = Dict{Int,Array{Int64,1}}()
    moments = sort(unique(mom_firm[!,:M_num]))
    firmMoments = Vector{Float64}(undef,length(moments))
    if constMoments
        for m in moments
            m_df_index = findall(mom_firm[!,:M_num].==m)
            m_index = mom_firm[!,:index][m_df_index]
            _firmMomentDict[m] = m_index
            _firmMomentBit[m] = inlist(all_idx,m_index)
            _firmMomentProdDict[m] = sort(unique(mom_firm[!,:Product_std][m_df_index]))
            firmMoments[m] = mom_firm[!,:logAvgCost][m_df_index][1]
        end
    end

    _metalMomentDict = Dict{Int,Dict{Int,Array{Int64,1}}}()
    _metalMomentBit = Dict{Int,BitArray{1}}()
    _metalMomentProdDict = Dict{Int,Dict{Int,Array{Int64,1}}}()
    moments = sort(unique(mom_metal[!,:M_num]))
    firms = sort(unique(mom_metal[!,:F_M_num]))

    metalMoments = Vector{Float64}(undef,length(moments))
    if constMoments
        for f in firms
            _subDict = Dict{Int,Array{Int64,1}}()
            _subProdDict = Dict{Int,Array{Int64,1}}()
            for m in moments
                m_f_df_index = findall( (mom_metal[!,:M_num].==m) .& (mom_metal[!,:F_M_num].==f) )
                m_df_index = findall( (mom_metal[!,:M_num].==m) )
                m_index = mom_metal[!,:index][m_f_df_index]
                _subDict[m] = m_index
                _metalMomentBit[m] = inlist(all_idx,m_index)
                _subProdDict[m] = sort(unique(mom_metal[!,:Product_std][m_f_df_index]))
                metalMoments[m] = mom_metal[!,:costIndex][m_df_index][1]
            end
            _metalMomentDict[f] = _subDict
            _metalMomentProdDict[f] = _subProdDict
        end
    end

    _ageMomentDict = Dict{Int,Array{Int64,1}}()
    _ageMomentBit = Dict{Int,BitArray{1}}()
    moments = sort(unique(mom_age[!,:M_num]))
    ageMoments = Vector{Float64}(undef,length(moments))
    if constMoments
        for m in moments
            m_index = mom_age[!,:index][findall(mom_age[!,:M_num].==m)]
            _ageMomentDict[m] = m_index
            _ageMomentBit[m] = inlist(all_idx,m_index)
            ageMoments[m] = mom_age[!,:costIndex][findall(mom_age[!,:M_num].==m)][1]
        end
    end

    _agenoMomentDict = Dict{Int,Array{Int64,1}}()
    _agenoMomentBit = Dict{Int,BitArray{1}}()
    moments = sort(unique(mom_ageno[!,:M_num]))
    agenoMoments = Vector{Float64}(undef,length(moments))
    if constMoments
        for m in moments
            m_index = mom_ageno[!,:index][findall(mom_ageno[!,:M_num].==m)]
            _agenoMomentDict[m] = m_index
            _agenoMomentBit[m] = inlist(all_idx,m_index)
            agenoMoments[m] = mom_ageno[!,:costIndex][findall(mom_ageno[!,:M_num].==m)][1]
        end
    end

    riskMoment = mom_risk[!,:costIndex][2]


    _raMomentDict = Dict{Int,Array{Int64,1}}()
    _raMomentBit = Dict{Int,BitArray{1}}()
    moments = sort(unique(mom_ra[!,:M_num]))
    raMoments = Vector{Float64}(undef,length(moments))
    if constMoments
        for m in moments
            m_index = sort(unique(mom_ra[!,:Product_std][findall(mom_ra[!,:M_num].==m)]))
            _raMomentDict[m] = m_index
            # _agenoMomentBit[m] = inlist(all_idx,m_index)
            raMoments[m] = mom_ra[!,:avgTransfer][findall(mom_ra[!,:M_num].==m)][1]
        end
    end


    ### Lengths
    # mom_length = length(firmMoments)  + (length(metalMoments)-1) + (length(ageMoments)-1) + length(riskMoment) + length(raMoments)
    mom_length = length(firmMoments)  + (length(metalMoments)-1) + (length(ageMoments)-1) + length(riskMoment)
    par_length = length(_baseIndex) + length(_riskIndex) + length(_feIndex)

    fePars = zeros(length(firmMoments))



    return MC_Data(data,F,metal_levels,anyHCC,_baseIndex,_riskIndex,_feIndex,
                    mom_length,par_length,_stDict,
                    _firmMomentDict,_firmMomentBit,_firmMomentProdDict,firmMoments,
                    _metalMomentDict,_metalMomentBit,_metalMomentProdDict,metalMoments,
                    _ageMomentDict,_ageMomentBit,ageMoments,
                    _agenoMomentDict,_agenoMomentBit,agenoMoments,riskMoment,
                    _raMomentDict,_raMomentBit,raMoments,fePars)
end



function parMC(p_MC::Vector{T},par_est::parDict{Float64},d::InsuranceLogit,c::MC_Data) where T

    # Non Risk Costs
    basepars = p_MC[c._baseIndex]' * c.data
    fepars = p_MC[c._feIndex]' * c.fixedEffects
    metalpars = [0.57,0.60,0.70,0.80,0.90]' * c.metalEffects
    C_nonrisk = basepars + fepars
    C_nonrisk = exp.(C_nonrisk[:]).*metalpars

    # Risk Costs
    risks = exp.(d.draws.*p_MC[c._riskIndex])

    ## Total Average Cost, with risks
    C = Vector{T}(undef,length(C_nonrisk))
    C_cap = Vector{T}(undef,length(C_nonrisk))
    C_pool = Vector{T}(undef,length(C_nonrisk))
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
    ## Insured Probability
    s_ins  = Vector{T}(undef,length(C_nonrisk))

    ### Moment Characteristics
    # grad = Matrix{T}(undef,c.mom_length+2,c.par_length)
    # hess = Array{T,3}(undef,c.mom_length+2,c.par_length,c.par_length)

    return parMC(par_est,p_MC,risks,C_nonrisk,C,C_cap,C_pool,C_HCC,dC,dC_HCC,d2C,d2C_HCC,s_nr,s_r,s_ins)
end


function individual_costs(d::InsuranceLogit,p::parMC{T}) where T
    # Store Parameters
    δ_long = p.pars.δ
    μ_ij_large = p.pars.μ_ij
    μnr_ij_large = p.pars.μ_ij_nonRisk
    risk_long = rIndS(d.data)
    cost_nonRisk = p.C_nonrisk
    any_long = anyHCC(d.data)
    for idxitr in values(d.data._personDict)
        δ = δ_long[idxitr]
        @inbounds u = μ_ij_large[:,idxitr]
        @inbounds u_nr = μnr_ij_large[idxitr]
        r_ind = Int.(risk_long[idxitr])
        @inbounds r_cost = p.risks[:,r_ind]
        @inbounds r_scores = d.draws[:,r_ind]
        @inbounds any_r = any_long[idxitr[1]]
        c_nr = cost_nonRisk[idxitr]
        c_nr_cap = capped_cost.(c_nr)
        # s_r,c,c_HCC,dc,dc_HCC,d2c,d2c_HCC = calc_cost(u,δ,r_cost,r_scores,c_nr,anyHCC_scores)
        s_r,c_r,c_r_cap,c_r_pl = calc_cost(u,δ,r_cost,r_scores,c_nr)
        s_nr = calc_shares(u_nr,δ)
        # p.pars.s_hat[idxitr] = s
        p.C[idxitr] = (any_r.*s_r.*c_r + (1-any_r).*s_nr.*c_nr)./(any_r.*s_r + (1-any_r).*s_nr)
        p.C_cap[idxitr] = (any_r.*s_r.*c_r_cap + (1-any_r).*s_nr.*c_nr_cap)./(any_r.*s_r + (1-any_r).*s_nr)
        p.C_HCC[idxitr] = c_r

        s_r_ins = sum(s_r)
        s_nr_ins = sum(s_nr)
        s_ins_hat = (any_r.*s_r_ins + (1-any_r).*s_nr_ins)
        p.C_pool[idxitr] = (any_r.*s_r_ins.*c_r_pl + (1-any_r).*s_nr_ins.*c_nr)./s_ins_hat
        p.s_hat_ins[idxitr].= s_ins_hat

        # p.dCdr[idxitr] = dc
        # p.dCdr_HCC[idxitr] = dc_HCC
        # p.d2Cdr[idxitr] = d2c
        # p.d2Cdr_HCC[idxitr] = d2c_HCC
        p.s_hat_nonrisk[idxitr] = s_nr
        p.s_hat_risk[idxitr] = s_r
    end
    # Zero shares need zero costs
    p.C[p.pars.s_hat.==0.0].=0.0
    p.C_cap[p.pars.s_hat.==0.0].=0.0
    return Nothing
end


function calc_cost(μ_ij::Array{Float64},δ::Vector{Float64},r::Matrix{T},r_sc::Matrix{Float64},c::Vector{T}) where T
    (N,K) = size(μ_ij)
    util = Matrix{Float64}(undef,K,N)
    s_hat = Matrix{Float64}(undef,K,N)
    s_ins = Vector{Float64}(undef,N)
    # s_hat_risk = Matrix{Float64}(undef,K,N)
    c_hat = Matrix{T}(undef,K,N)
    c_hat_capped = Matrix{T}(undef,K,N)
    c_hat_pool = Matrix{T}(undef,K,N)
    # c_hat_risk = Matrix{T}(undef,K,N)
    # dc_hat = Matrix{T}(undef,K,N)
    # dc_hat_risk = Matrix{T}(undef,K,N)
    # d2c_hat = Matrix{T}(undef,K,N)
    # d2c_hat_risk = Matrix{T}(undef,K,N)

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

            @inbounds @fastmath cost = r[n,i]*c[i]
            cost_capped = capped_cost(cost)

            @inbounds @fastmath c_hat[i,n] = s*cost
            @inbounds @fastmath c_hat_capped[i,n] = s*cost_capped
            @inbounds c_hat_pool[i,n] = si*cost_capped

            # @inbounds @fastmath dcost = r_sc[n,i]*cost
            # @inbounds @fastmath d2cost = r_sc[n,i]^2*cost


            # @inbounds @fastmath c_hat_risk[i,n] = cost*(anyHCC[n,i])

            # @inbounds dc_hat[i,n] = dcost
            # @inbounds @fastmath dc_hat_risk[i,n] = dcost*(anyHCC[n,i])

            # @inbounds d2c_hat[i,n] = d2cost
            # @inbounds @fastmath d2c_hat_risk[i,n] = d2cost*(anyHCC[n,i])
        end
    end

    # s_mean = mean(s_hat,dims=2)
    s_sum  = sum(s_hat,dims=2)
    # s_risk_sum  = sum(s_hat_risk,dims=2)
    c_mean = sum(c_hat,dims=2)./s_sum
    c_mean_capped = sum(c_hat_capped,dims=2)./s_sum
    c_mean_pool = sum(c_hat_pool,dims=2)./sum(s_ins)
    # c_mean_risk = sum(c_hat_risk,dims=2)./s_risk_sum

    # dc_mean = sum(dc_hat,dims=2)./s_sum
    # dc_mean_risk = sum(dc_hat_risk,dims=2)./s_risk_sum

    # d2c_mean = sum(d2c_hat,dims=2)./s_sum
    # d2c_mean_risk = sum(d2c_hat_risk,dims=2)./s_risk_sum

    s_mean = s_sum./N
    # s_mean_risk = s_risk_sum./N_r

    ### Zero share gets zero cost
    c_mean[s_sum.==0.0] .= 0.0
    c_mean_capped[s_sum.==0.0] .= 0.0

    return s_mean, c_mean, c_mean_capped, c_mean_pool #, c_mean_risk, dc_mean, dc_mean_risk, d2c_mean, d2c_mean_risk
end

function capped_cost(c::T;thresh::Float64=3750.0,cap::Float64=20833.33,rate::Float64=0.55) where T
    c_base = min(c,thresh)
    c_rein = min(max(c-thresh,0),cap-thresh)
    c_exc  = max(c-cap,0)
    c = c_base + rate*c_rein + c_exc
    return c
end

function capped_cost(c::Vector{T};thresh::Float64=3750.0,cap::Float64=20833.33,rate::Float64=0.55) where T
    c_cap = similar(c)
    for i in 1:length(c)
        c_cap[i] = capped_cost(c[i],thresh=thresh,cap=cap,rate=rate)
    end
    return c_cap
end

function capped_cost(c::Matrix{T};thresh::Float64=3750.0,cap::Float64=20833.33,rate::Float64=0.55) where T
    c_cap = similar(c)
    (J,K) = size(c)
    for j in 1:J, k in 1:K
        c_cap[j,k] = capped_cost(c[j,k],thresh=thresh,cap=cap,rate=rate)
    end
    return c_cap
end




function costMoments(c::MC_Data,d::InsuranceLogit,p::parMC{T};print_moments::Bool=false) where T
    s_hat = p.pars.s_hat
    s_hat_nonrisk = p.s_hat_nonrisk
    s_hat_risk = p.s_hat_risk
    wgts = weight(d.data)[:]
    actuarial_values = c.data[2,:]

    wgts_share = wgts.*s_hat
    any_share = wgts.*s_hat_risk.*c.anyHCC
    none_share = wgts.*s_hat_nonrisk.*(1 .- c.anyHCC)

    c_hat = p.C
    c_hat_cap = p.C_cap
    c_hat_nonHCC = p.C_nonrisk
    c_hat_HCC = p.C_HCC


    fmom = Vector{T}(undef,length(c.firmMoments))
    mmom = Vector{T}(undef,length(c.metalMoments)-1)
    amom = Vector{T}(undef,length(c.ageMoments)-1)
    nmom = Vector{T}(undef,length(c.agenoMoments)-1)

    ## Firm Moments
    for (m,m_idx) in c._firmMomentDict
        c_avg = sliceMean_wgt(c_hat_cap,wgts_share,m_idx)
        fmom[m] = log(c_avg) #- c.firmMoments[m]
        # t = sum(test[m_idx])
        # if t>0.0
        #     println(m)
        #     println(t)
        # end
        # pmom[m] = (c_avg - exp(c.avgMoments[m]))/100
        # pmom[m] = c_avg
    end

    ## Metal Moments

    f_num = maximum(keys(c._metalMomentDict))
    m_num = maximum(keys(c._metalMomentDict[1]))
    m_mom_mat = Matrix{T}(undef,m_num-1,f_num)
    m_mom_mat[:].=0.0
    lives_f = zeros(f_num)
    for (f,sub_dict) in c._metalMomentDict
        refval = sliceMean_wgt(c_hat_cap,wgts_share,sub_dict[1])

        for (m,m_idx) in sub_dict
            lives_f[f]+=sum(wgts_share[m_idx])
            if m==1
                continue
            else
                c_avg = sliceMean_wgt(c_hat_cap,wgts_share,m_idx)
                # println("Firm: $f, Metal: $m, Cost: $c_avg")
                # mmom[m-1] = c_avg/refval[1] #- c.metalMoments[m]
                m_mom_mat[m-1,f] = c_avg/refval[1]
            end
        end
    end

    m_lives = Vector{T}(undef,m_num-1)
    mmom[:].=0.0
    m_lives[:].=0.0
    for m in 2:m_num
        for f in 1:f_num
            if !isnan(m_mom_mat[m-1,f]) & (m_mom_mat[m-1,f]!=0)
                mmom[m-1]+=m_mom_mat[m-1,f]*lives_f[f]
                m_lives[m-1]+= lives_f[f]
            end
        end
    end
    mmom = mmom./m_lives

    ## Age Moments
    refval = sliceMean_wgt(c_hat,wgts_share,c._ageMomentDict[1])
    for (m,m_idx) in c._ageMomentDict
        if m==1
            continue
        else
            # avg_act = sum(wgts_share[m_idx].*actuarial_values[m_idx])/sum(wgts_share[m_idx])
            # println(m)
            # println(avg_act)
            c_avg = sliceMean_wgt(c_hat,wgts_share,m_idx)
            #println("$m: $c_avg")
            amom[m-1] = c_avg/refval[1] #- c.ageMoments[m]
        end
    end

    ## Age without HCC Moments
    refval = sliceMean_wgt(c_hat_nonHCC,none_share,c._agenoMomentDict[1])
    for (m,m_idx) in c._agenoMomentDict
        if m==1
            continue
        else
            c_avg = sliceMean_wgt(c_hat_nonHCC,none_share,m_idx)
            # println("$m: $c_avg")
            nmom[m-1] = c_avg/refval[1] #- c.agenoMoments[m]
        end
    end


    ## Risk Moments
    all_idx = Int.(1:length(s_hat))
    HCC_avg = sliceMean_wgt(c_hat_HCC,any_share,all_idx)
    non_avg = sliceMean_wgt(c_hat_nonHCC,none_share,all_idx)
    rmom = HCC_avg/non_avg #- c.riskMoment

    ## Transfer Moment

    tmom = transferMoment(c,d,p)

    est_moments = vcat(fmom,mmom,nmom,rmom)
    targ_moments = vcat(c.firmMoments,c.metalMoments[2:length(c.metalMoments)],
                    c.agenoMoments[2:length(c.agenoMoments)],c.riskMoment)
    if print_moments
        return est_moments, targ_moments
    else
        return est_moments .- targ_moments
    end
    # est_moments = vcat(fmom,mmom,nmom,rmom,tmom)
    # targ_moments = vcat(c.firmMoments,c.metalMoments[2:length(c.metalMoments)],
    #                 c.agenoMoments[2:length(c.agenoMoments)],c.riskMoment,c.raMoments)
    # return est_moments .- targ_moments
    # return est_moments,targ_moments
end


function costMoments(c::MC_Data,d::InsuranceLogit,p::Array{T},p_est::parDict{Float64};print_moments::Bool=false) where T
    par = parMC(p,p_est,d,c) # Fix p0
    individual_costs(d,par)
    mom = costMoments(c,d,par,print_moments=print_moments)
    return mom
end

function transferMoment(c::MC_Data,d::InsuranceLogit,p::parMC{T}) where T
    wgts = weight(d.data)[:]
    wgts_ins = p.s_hat_ins.*wgts
    wgts_share = p.pars.s_hat.*wgts

    avgTransfer = Vector{T}(undef,length(keys(c._raMomentDict)))
    for (m,m_idx) in c._raMomentDict
        T_total = 0
        S_total = 0
        for j in m_idx
            idx = d.data._productDict[j]
            PC = sliceMean_wgt(p.C_pool,wgts_ins,idx)
            AC = sliceMean_wgt(p.C_cap,wgts_share,idx)
            S_j = sliceSum_wgt(p.pars.s_hat,wgts,idx)
            T_total+= S_j*(PC-AC)
            S_total+= S_j
        end
        avgTransfer[m] = (T_total/S_total)/10
    end

    return avgTransfer
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
#     # c_hat = c_hat./actuarial_values
#     c_hat_nonHCC = p.C_nonrisk./actuarial_values
#     c_hat_HCC = p.C_HCC./actuarial_values
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
#     HCC_avg = sliceMean_wgt(c_hat_HCC,any_share,all_idx)
#     non_avg = sliceMean_wgt(c_hat_nonHCC,none_share,all_idx)
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
