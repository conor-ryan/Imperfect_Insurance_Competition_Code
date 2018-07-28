@views function sliceSum_wgt{T}(x::Vector{T},w::Vector{Float64},idx::Array{Int64,1})
    wgts = w[idx]
    return sum(x[idx].*wgts)
end

function calc_risk_moments{T}(d::InsuranceLogit,p::parDict{T})
    wgts = weight(d.data)[1,:]
    wgts_share = wgts.*p.s_hat
    num_prods = length(d.prods)
    s_hat_j = Vector{Float64}(num_prods)
    r_hat_j = Vector{Float64}(num_prods)
    a_hat_j = Vector{Float64}(num_prods)
    ageRate_long = ageRate(d.data)[1,:]
    for j in d.prods
        j_index_all = d.data._productDict[j]
        s_hat_j[j]= sliceSum_wgt(p.s_hat,wgts,j_index_all)/d.lives[j]*d.data.st_share[j]
        r_hat_j[j] = sliceMean_wgt(p.r_hat,wgts_share,j_index_all)*m.Γ_j[j]
        a_hat_j[j] = sliceMean_wgt(ageRate_long,wgts_share,j_index_all)*m.AV_j[j]*m.Γ_j[j]
    end

    t_norm_j = zeros(num_prods)
    for (s,idx_prod) in d.data._stDict
        ST_R = sliceMean_wgt(r_hat_j,s_hat_j,idx_prod)
        ST_A = sliceMean_wgt(a_hat_j,s_hat_j,idx_prod)

        for j in idx_prod
            t_norm_j[j] = r_hat_j[j]/ST_R - a_hat_j[j]/ST_A
        end
    end

    mom_value = Vector{Float64}(length(d.data.tMoments))

    for (m,idx_mom) in d.data._tMomentDict
        t_est = sliceMean_wgt(t_norm_j,s_hat_j,idx_mom)
        mom_value[m] = d.data.tMoments[m] - t_est
    end

    return mom_value
end
