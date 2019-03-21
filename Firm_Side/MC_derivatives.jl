function costMoments!(hess::Array{T,3},grad::Matrix{T},c::MC_Data,d::InsuranceLogit,p::parMC{T}) where T
    s_hat = p.pars.s_hat
    s_hat_nonrisk = p.s_hat_nonrisk
    s_hat_risk = p.s_hat_risk
    wgts = weight(d.data)[:]
    actuarial_values = c.data[2,:]

    wgts_share = wgts.*s_hat
    any_share = wgts.*s_hat_risk.*c.anyHCC
    none_share = wgts.*s_hat_nonrisk.*(1 .- c.anyHCC)

    c_hat = p.C
    c_hat_total = c_hat./actuarial_values
    c_hat_nonHCC_total = p.C_nonrisk./actuarial_values
    c_hat_HCC_total = p.C_HCC./actuarial_values

    M1 = length(c.avgMoments)

    ### Compute Averages
    pval= Vector{T}(undef,length(c.avgMoments))
    aval = Vector{T}(undef,length(c.ageMoments))

    d_pmom = Matrix{T}(undef,c.par_length,length(c.avgMoments))
    d_amom = Matrix{T}(undef,c.par_length,length(c.ageMoments))
    d_HCC = Vector{T}(undef,c.par_length)
    d_nonHCC = Vector{T}(undef,c.par_length)

    pmom= Vector{T}(undef,length(c.avgMoments))
    amom = Vector{T}(undef,length(c.ageMoments)-1)

    ## Product and Firm Moments
    for (m,m_idx) in c._avgMomentDict
        c_avg = sliceMean_wgt(c_hat,wgts_share,m_idx)
        pval[m] = c_avg
        pmom[m] = log(c_avg) - c.avgMoments[m]
    end

    ## Age Moments
    refval = sliceMean_wgt(c_hat_total,wgts_share,c._ageMomentDict[1])
    for (m,m_idx) in c._ageMomentDict
        c_avg = sliceMean_wgt(c_hat_total,wgts_share,m_idx)
        aval[m] = c_avg
        if m>1
            amom[m-1] = c_avg/refval[1] - c.ageMoments[m]
        end
    end

    all_idx = Int.(1:length(s_hat))
    HCC_avg = sliceMean_wgt(c_hat_HCC_total,any_share,all_idx)
    non_avg = sliceMean_wgt(c_hat_nonHCC_total,none_share,all_idx)
    rmom = HCC_avg/non_avg - c.riskMoment
    moments = vcat(pmom,amom,rmom)

    # dc_vec = Vector{Vector{Float64}}(undef,c.par_length)
    # dc_nonHCC_vec = Vector{Vector{Float64}}(undef,c.par_length)
    # dc_HCC_vec = Vector{Vector{Float64}}(undef,c.par_length)

    for k in vcat(c._baseIndex,c._feIndex,c._riskIndex)
        # println("k: $k")
        dkc_hat, dkc_hat_nonHCC, dkc_hat_HCC = grad_par_return(k,c,p)

        dkc_hat_total = dkc_hat./actuarial_values
        dkc_hat_nonHCC_total = dkc_hat_nonHCC./actuarial_values
        dkc_hat_HCC_total = dkc_hat_HCC./actuarial_values

        # dc_vec[k] = dkc_hat[:]
        # dc_nonHCC_vec[k] = dkc_hat_nonHCC[:]
        # dc_HCC_vec[k] = dkc_hat_HCC[:]

        ## Product and Firm Moments
        for (m,m_idx) in c._avgMomentDict
            c_avg = pval[m]
            dkc_avg = sliceMean_wgt(dkc_hat,wgts_share,m_idx)
            d_pmom[k,m] = dkc_avg
            grad[k,m] = dkc_avg/c_avg
        end


        #Age Moments
        refval = aval[1]
        dkrefval = sliceMean_wgt(dkc_hat_total,wgts_share,c._ageMomentDict[1])
        for (m,m_idx) in c._ageMomentDict
            if m==1
                dkc_avg = sliceMean_wgt(dkc_hat_total,wgts_share,m_idx)
                d_amom[k,m] = dkc_avg
            else
                c_avg = aval[m]
                dkc_avg = sliceMean_wgt(dkc_hat_total,wgts_share,m_idx)
                d_amom[k,m] = dkc_avg
                grad[k,M1 + m-1] = dkc_avg/refval - dkrefval[1]*c_avg/(refval)^2
            end
        end

        #Risk Moments
        dkHCC_avg = sliceMean_wgt(dkc_hat_HCC_total,any_share,all_idx)
        dknon_avg = sliceMean_wgt(dkc_hat_nonHCC_total,none_share,all_idx)
        grad[k,c.mom_length] = dkHCC_avg/non_avg - dknon_avg*HCC_avg/(non_avg)^2
        d_HCC[k] = dkHCC_avg
        d_nonHCC[k] = dknon_avg

        if k in c._riskIndex
            l_index = 1:c.par_length
        else
            l_index = vcat(c._baseIndex,c._feIndex)
            l_index = l_index[l_index.<=k]
        end
        for l in l_index
            # println("l: $l")
            # dlc_hat = dc_vec[l]
            # dlc_hat_nonHCC = dc_nonHCC_vec[l]
            # dlc_hat_HCC = dc_HCC_vec[l]
            #
            # dlc_hat_total = dlc_hat./actuarial_values
            # dlc_hat_nonHCC_total = dlc_hat_nonHCC./actuarial_values
            # dlc_hat_HCC_total = dlc_hat_HCC./actuarial_values

            d2c_hat, d2c_hat_nonHCC, d2c_hat_HCC = hess_par_return(k,l,c,p,dkc_hat, dkc_hat_nonHCC, dkc_hat_HCC)

            d2c_hat_total = d2c_hat./actuarial_values
            d2c_hat_nonHCC_total = d2c_hat_nonHCC./actuarial_values
            d2c_hat_HCC_total = d2c_hat_HCC./actuarial_values

            ## Product and Firm Moments
            for (m,m_idx) in c._avgMomentDict
                c_avg = pval[m]
                dkc_avg = d_pmom[k,m]
                dlc_avg = d_pmom[l,m]
                # dlc_avg = sliceMean_wgt(dlc_hat,wgts_share,m_idx)
                d2c_avg = sliceMean_wgt(d2c_hat,wgts_share,m_idx)
                hess[k,l,m] = d2c_avg/c_avg - dlc_avg*dkc_avg/(c_avg^2)
                hess[l,k,m] = d2c_avg/c_avg - dlc_avg*dkc_avg/(c_avg^2)
            end

            ## Age Moments
            # dlrefval = sliceMean_wgt(dlc_hat_total,wgts_share,c._ageMomentDict[1])
            dlrefval = d_amom[l,1]
            d2refval = sliceMean_wgt(d2c_hat_total,wgts_share,c._ageMomentDict[1])
            for (m,m_idx) in c._ageMomentDict
                if m==1
                    continue
                else
                    c_avg = aval[m]
                    dkc_avg = d_amom[k,m]
                    dlc_avg = d_amom[l,m]
                    # dlc_avg = sliceMean_wgt(dlc_hat_total,wgts_share,m_idx)
                    d2c_avg = sliceMean_wgt(d2c_hat_total,wgts_share,m_idx)
                    hess[k,l,M1 + m-1] = d2c_avg/refval - dlrefval[1]*dkc_avg/(refval)^2 - d2refval[1]*c_avg/(refval)^2 - dkrefval[1]*dlc_avg/(refval)^2 + 2*dkrefval[1]*dlrefval[1]*c_avg/(refval)^3
                    hess[l,k,M1 + m-1] = d2c_avg/refval - dlrefval[1]*dkc_avg/(refval)^2 - d2refval[1]*c_avg/(refval)^2 - dkrefval[1]*dlc_avg/(refval)^2 + 2*dkrefval[1]*dlrefval[1]*c_avg/(refval)^3
                end
            end

            # dlHCC_avg = sliceMean_wgt(dlc_hat_HCC_total,any_share,all_idx)
            # dlnon_avg = sliceMean_wgt(dlc_hat_nonHCC_total,none_share,all_idx)
            dlHCC_avg = d_HCC[l]
            dlnon_avg = d_nonHCC[l]
            d2HCC_avg = sliceMean_wgt(d2c_hat_HCC_total,any_share,all_idx)
            d2non_avg = sliceMean_wgt(d2c_hat_nonHCC_total,none_share,all_idx)
            hess[k,l,c.mom_length] = d2HCC_avg/non_avg - dlnon_avg*dkHCC_avg/non_avg^2 - dknon_avg*dlHCC_avg/non_avg^2 - d2non_avg*HCC_avg/non_avg^2 + 2*dknon_avg*dlnon_avg*HCC_avg/non_avg^3
            hess[l,k,c.mom_length] = d2HCC_avg/non_avg - dlnon_avg*dkHCC_avg/non_avg^2 - dknon_avg*dlHCC_avg/non_avg^2 - d2non_avg*HCC_avg/non_avg^2 + 2*dknon_avg*dlnon_avg*HCC_avg/non_avg^3
        end
    end

    return moments
end

function costMoments!(grad::Matrix{T},c::MC_Data,d::InsuranceLogit,p::parMC{T}) where T
    s_hat = p.pars.s_hat
    s_hat_nonrisk = p.s_hat_nonrisk
    s_hat_risk = p.s_hat_risk
    wgts = weight(d.data)[:]
    actuarial_values = c.data[2,:]

    wgts_share = wgts.*s_hat
    any_share = wgts.*s_hat_risk.*c.anyHCC
    none_share = wgts.*s_hat_nonrisk.*(1 .- c.anyHCC)

    c_hat = p.C
    c_hat_total = c_hat./actuarial_values
    c_hat_nonHCC_total = p.C_nonrisk./actuarial_values
    c_hat_HCC_total = p.C_HCC./actuarial_values

    M1 = length(c.avgMoments)

    ### Compute Averages
    pval= Vector{T}(undef,length(c.avgMoments))
    aval = Vector{T}(undef,length(c.ageMoments))

    pmom= Vector{T}(undef,length(c.avgMoments))
    amom = Vector{T}(undef,length(c.ageMoments)-1)

    ## Product and Firm Moments
    for (m,m_idx) in c._avgMomentDict
        c_avg = sliceMean_wgt(c_hat,wgts_share,m_idx)
        pval[m] = c_avg
        pmom[m] = log(c_avg) - c.avgMoments[m]
    end

    ## Age Moments
    refval = sliceMean_wgt(c_hat_total,wgts_share,c._ageMomentDict[1])
    for (m,m_idx) in c._ageMomentDict
        c_avg = sliceMean_wgt(c_hat_total,wgts_share,m_idx)
        aval[m] = c_avg
        if m>1
            amom[m-1] = c_avg/refval[1] - c.ageMoments[m]
        end
    end

    all_idx = Int.(1:length(s_hat))
    HCC_avg = sliceMean_wgt(c_hat_HCC_total,any_share,all_idx)
    non_avg = sliceMean_wgt(c_hat_nonHCC_total,none_share,all_idx)
    rmom = HCC_avg/non_avg - c.riskMoment
    moments = vcat(pmom,amom,rmom)

    # dc_vec = Vector{Vector{Float64}}(undef,c.par_length)
    # dc_nonHCC_vec = Vector{Vector{Float64}}(undef,c.par_length)
    # dc_HCC_vec = Vector{Vector{Float64}}(undef,c.par_length)

    for k in vcat(c._baseIndex,c._feIndex,c._riskIndex)
        # println("k: $k")
        dkc_hat, dkc_hat_nonHCC, dkc_hat_HCC = grad_par_return(k,c,p)

        dkc_hat_total = dkc_hat./actuarial_values
        dkc_hat_nonHCC_total = dkc_hat_nonHCC./actuarial_values
        dkc_hat_HCC_total = dkc_hat_HCC./actuarial_values

        # dc_vec[k] = dkc_hat[:]
        # dc_nonHCC_vec[k] = dkc_hat_nonHCC[:]
        # dc_HCC_vec[k] = dkc_hat_HCC[:]

        ## Product and Firm Moments
        for (m,m_idx) in c._avgMomentDict
            c_avg = pval[m]
            dkc_avg = sliceMean_wgt(dkc_hat,wgts_share,m_idx)
            grad[k,m] = dkc_avg/c_avg
        end


        #Age Moments
        refval = aval[1]
        dkrefval = sliceMean_wgt(dkc_hat_total,wgts_share,c._ageMomentDict[1])
        for (m,m_idx) in c._ageMomentDict
            if m==1
                continue
            else
                c_avg = aval[m]
                dkc_avg = sliceMean_wgt(dkc_hat_total,wgts_share,m_idx)
                grad[k,M1 + m-1] = dkc_avg/refval - dkrefval[1]*c_avg/(refval)^2
            end
        end

        #Risk Moments
        dkHCC_avg = sliceMean_wgt(dkc_hat_HCC_total,any_share,all_idx)
        dknon_avg = sliceMean_wgt(dkc_hat_nonHCC_total,none_share,all_idx)
        grad[k,c.mom_length] = dkHCC_avg/non_avg - dknon_avg*HCC_avg/(non_avg)^2
    end

    return moments
end


function grad_par_return(k::Int,c::MC_Data,p::parMC{T}) where T
    if k in c._baseIndex
        x_k = c.data[k,:]

        dc = p.C.*x_k
        dc_nonHCC = p.C_nonrisk.*x_k
        dc_HCC = p.C_HCC.*x_k
    elseif k in c._riskIndex
        dc = p.dCdr
        dc_nonHCC = zeros(length(p.C))
        dc_HCC = p.dCdr_HCC
    else
        ind = k+1-minimum(c._feIndex)
        x_k = c.fixedEffects[ind,:]

        dc = p.C.*x_k
        dc_nonHCC = p.C_nonrisk.*x_k
        dc_HCC = p.C_HCC.*x_k
    end
    return dc, dc_nonHCC,dc_HCC
end

function hess_par_return(k::Int,l::Int64,c::MC_Data,p::parMC{T},dkc_hat::Vector{T}, dkc_hat_nonHCC::Vector{T}, dkc_hat_HCC::Vector{T}) where T
    if k in c._riskIndex
        if l in c._riskIndex
            d2c = p.d2Cdr
            d2c_nonHCC = zeros(length(p.C))
            d2c_HCC = p.d2Cdr_HCC
        else
            if l in c._baseIndex
                x_l = c.data[l,:]
            else
                ind = l+1-minimum(c._feIndex)
                x_l = c.fixedEffects[ind,:]
            end
            d2c = p.dCdr.*x_l
            d2c_nonHCC = zeros(length(p.C))
            d2c_HCC = p.dCdr_HCC.*x_l
        end
    else
        if l in c._baseIndex
            x_l = c.data[l,:]
        else
            ind = l+1-minimum(c._feIndex)
            x_l = c.fixedEffects[ind,:]
        end
        d2c = dkc_hat.*x_l
        d2c_nonHCC = dkc_hat_nonHCC.*x_l
        d2c_HCC = dkc_hat_HCC.*x_l
    end
    return d2c, d2c_nonHCC,d2c_HCC
end
