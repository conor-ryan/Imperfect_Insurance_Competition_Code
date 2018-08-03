@views function sliceSum_wgt{T,S}(x::Vector{T},w::Vector{S},idx::Array{Int64,1})
    tot = 0.0
    @inbounds @fastmath @simd for q in idx
        tot+= x[q]*w[q]
    end
    return tot
end
@views function sliceSum_wgt{S}(x::SubArray,w::Vector{S},idx::Array{Int64,1})
    tot = 0.0
    @inbounds @fastmath @simd for q in idx
        tot+= x[q]*w[q]
    end
    return tot
end

@views function sliceSum_wgt{T,S}(k::Int64,x::Matrix{T},w::Vector{S},idx::Array{Int64,1})
    tot = 0.0
    @inbounds @fastmath @simd for q in idx
        tot+= x[k,q]*w[q]
    end
    return tot
end

@views function sliceSum_wgt{T}(x::Vector{T},y::Vector{Float64},w::Vector{T},
                                    idx::Array{Int64,1})
    tot = 0.0
    @inbounds @fastmath @simd for q in idx
        tot+= x[q]*y[q]*w[q]
    end
    return tot
end

@views function sliceSum_wgt{T}(x::Vector{T},y::SubArray,w::Vector{T},
                                    idx::Array{Int64,1})
    tot = 0.0
    @inbounds @fastmath @simd for q in idx
        tot+= x[q]*y[q]*w[q]
    end
    return tot
end

@views function sliceSum_wgt{T}(k::Int64,x::Vector{T},y::Matrix{Float64},w::Vector{T},
                                    idx::Array{Int64,1})
    tot = 0.0
    @inbounds @fastmath @simd for q in idx
        tot+= x[q]*y[k,q]*w[q]
    end
    return tot
end

function calc_risk_moments!{T}(grad::Matrix{Float64},d::InsuranceLogit,p::parDict{T})
    wgts = weight(d.data)[1,:]
    wgts_share = wgts.*p.s_hat
    num_prods = length(d.prods)
    S_unwt = Vector{Float64}(num_prods)
    s_hat_j = Vector{Float64}(num_prods)
    r_hat_j = Vector{Float64}(num_prods)
    a_hat_j = Vector{Float64}(num_prods)

    Q = d.parLength[:All]
    dRdθ = Matrix{Float64}(Q,num_prods)
    dAdθ = Matrix{Float64}(Q,num_prods)
    dTdθ = zeros(Q,num_prods)
    ageRate_long = ageRate(d.data)[1,:]

    dSdθ_j = Matrix{Float64}(Q,num_prods)


    prodAvgs!(S_unwt,s_hat_j,r_hat_j,a_hat_j,ageRate_long,wgts,wgts_share,d,p)
    # for j in d.prods
    #     ## Fix state shares...
    #     j_index_all = d.data._productDict[j]
    #     S_unwt[j] = sliceSum_wgt(p.s_hat,wgts,j_index_all)
    #     @inbounds @fastmath s_hat_j[j]= (S_unwt[j]/d.lives[j])*d.data.st_share[j]
    #     r_hat_j[j] = sliceMean_wgt(p.r_hat,wgts_share,j_index_all)*d.Γ_j[j]
    #     #r_hat_j[j] = sliceSum_wgt(p.r_hat,wgts_share,j_index_all)*d.Γ_j[j] #/S_unwt[j]
    #     a_hat_j[j] = sliceMean_wgt(ageRate_long,wgts_share,j_index_all)*d.AV_j[j]*d.Γ_j[j]
    # end

    prodDerivatives!(dAdθ,dRdθ,dSdθ_j,S_unwt,r_hat_j,a_hat_j,ageRate_long,wgts,d,p)
    # for q in 1:Q
    #     dSdθ_long = p.dSdθ[q,:]
    #     dRdθ_long = p.dRdθ[q,:]
    #
    #     for j in d.prods
    #         j_index_all = d.data._productDict[j]
    #         dS_j = sliceSum_wgt(dSdθ_long,wgts,j_index_all)
    #         dR_1 = sliceSum_wgt(p.r_hat,dSdθ_long,wgts,j_index_all)*d.Γ_j[j]
    #         dR_2 = sliceSum_wgt(p.s_hat,dRdθ_long,wgts,j_index_all)*d.Γ_j[j]
    #         dA = sliceSum_wgt(ageRate_long,dSdθ_long,wgts,j_index_all)*d.Γ_j[j]*d.AV_j[j]
    #
    #         @inbounds @fastmath dAdθ[q,j] = dA/S_unwt[j] - (dS_j/S_unwt[j])*a_hat_j[j]
    #         @inbounds @fastmath dRdθ[q,j] = (dR_1+dR_2)/S_unwt[j] - (dS_j/S_unwt[j])*r_hat_j[j]
    #         @inbounds @fastmath dSdθ_j[q,j] = (dS_j/d.lives[j])*d.data.st_share[j]
    #     end
    # end

    # out=0.0


    t_norm_j = calc_Transfers!(dTdθ,s_hat_j,r_hat_j,a_hat_j,
                            dSdθ_j,dRdθ,dAdθ,d)


    mom_value = Vector{Float64}(length(d.data.tMoments))

    for (m,idx_mom) in d.data._tMomentDict
        t_est = sliceMean_wgt(t_norm_j,s_hat_j,idx_mom)
        #mom_value[m] = d.data.tMoments[m] - t_est
        mom_value[m] = t_est
    end

    for q in 1:Q
        for (m,idx_mom) in d.data._tMomentDict
            t1 = 0.0
            s_mom = sum(s_hat_j[idx_mom])
            dS_mom = sum(dSdθ_j[q,idx_mom])
            @inbounds @fastmath @simd for j in idx_mom
                t1+= dSdθ_j[q,j]*t_norm_j[j] + s_hat_j[j]*dTdθ[q,j]
            end
            grad[q,m] = t1/s_mom - (dS_mom/s_mom)*mom_value[m]
        end
    end

    return mom_value .- d.data.tMoments
end

function prodAvgs!{T}(S_unwt::Vector{Float64},
                    s_hat_j::Vector{Float64},
                    r_hat_j::Vector{Float64},
                    a_hat_j::Vector{Float64},
                    ageRate_long::Vector{Float64},
                    wgts::Vector{Float64},
                    wgts_share::Vector{Float64},
                    d::InsuranceLogit,p::parDict{T})
    for j in d.prods
        j_index_all = d.data._productDict[j]
        S_unwt[j] = sliceSum_wgt(p.s_hat,wgts,j_index_all)
        @inbounds @fastmath s_hat_j[j]= (S_unwt[j]/d.lives[j])*d.data.st_share[j]
        r_hat_j[j] = sliceMean_wgt(p.r_hat,wgts_share,j_index_all)*d.Γ_j[j]
        a_hat_j[j] = sliceMean_wgt(ageRate_long,wgts_share,j_index_all)*d.AV_j[j]*d.Γ_j[j]
    end
    return Void
end


function  prodDerivatives!{T}(dAdθ::Matrix{Float64},
                            dRdθ::Matrix{Float64},
                            dSdθ_j::Matrix{Float64},
                            S_unwt::Vector{Float64},
                            r_hat_j::Vector{Float64},
                            a_hat_j::Vector{Float64},
                            ageRate_long::Vector{Float64},
                            wgts::Vector{Float64},
                            d::InsuranceLogit,p::parDict{T})
    Q = d.parLength[:All]
    for q in 1:Q
        @views dSdθ_long = p.dSdθ[q,:]
        @views dRdθ_long = p.dRdθ[q,:]

        for j in d.prods
            j_index_all = d.data._productDict[j]
            # dS_j = sliceSum_wgt(q,p.dSdθ,wgts,j_index_all)
            # dR_1 = sliceSum_wgt(q,p.r_hat,p.dSdθ,wgts,j_index_all)*d.Γ_j[j]
            # dR_2 = sliceSum_wgt(q,p.s_hat,p.dRdθ,wgts,j_index_all)*d.Γ_j[j]
            # dA = sliceSum_wgt(q,ageRate_long,p.dSdθ,wgts,j_index_all)*d.Γ_j[j]*d.AV_j[j]

            dS_j = sliceSum_wgt(dSdθ_long,wgts,j_index_all)
            dR_1 = sliceSum_wgt(p.r_hat,dSdθ_long,wgts,j_index_all)*d.Γ_j[j]
            dR_2 = sliceSum_wgt(p.s_hat,dRdθ_long,wgts,j_index_all)*d.Γ_j[j]
            dA = sliceSum_wgt(ageRate_long,dSdθ_long,wgts,j_index_all)*d.Γ_j[j]*d.AV_j[j]

            @inbounds @fastmath dAdθ[q,j] = dA/S_unwt[j] - (dS_j/S_unwt[j])*a_hat_j[j]
            @inbounds @fastmath dRdθ[q,j] = (dR_1+dR_2)/S_unwt[j] - (dS_j/S_unwt[j])*r_hat_j[j]
            @inbounds @fastmath dSdθ_j[q,j] = (dS_j/d.lives[j])*d.data.st_share[j]
        end
    end

    return Void
end


function calc_risk_moments{T}(d::InsuranceLogit,p::parDict{T})
    wgts = weight(d.data)[1,:]
    wgts_share = wgts.*p.s_hat
    num_prods = length(d.prods)
    S_unwt = Vector{T}(num_prods)
    s_hat_j = Vector{T}(num_prods)
    r_hat_j = Vector{T}(num_prods)
    a_hat_j = Vector{T}(num_prods)

    ageRate_long = ageRate(d.data)[1,:]

    #prodAvgs!(S_unwt,r_hat_j,a_hat_j,ageRate_long,wgts,wgts_share,d,p)
    for j in d.prods
        j_index_all = d.data._productDict[j]
        S_unwt[j] = sliceSum_wgt(p.s_hat,wgts,j_index_all)
        @inbounds @fastmath s_hat_j[j]= (S_unwt[j]/d.lives[j])*d.data.st_share[j]
        r_hat_j[j] = sliceMean_wgt(p.r_hat,wgts_share,j_index_all)*d.Γ_j[j]
        a_hat_j[j] = sliceMean_wgt(ageRate_long,wgts_share,j_index_all)*d.AV_j[j]*d.Γ_j[j]
    end

    # out=0.0
    t_norm_j = Vector{T}(num_prods)
    t_norm_j[:] = 0.0
    for (s,idx_prod) in d.data._stDict
        ST_R = sliceMean_wgt(r_hat_j,s_hat_j,idx_prod)
        ST_A = sliceMean_wgt(a_hat_j,s_hat_j,idx_prod)
        # if s==10
        #     out = ST_R
        # end


        for j in idx_prod
            t_norm_j[j] = r_hat_j[j]/ST_R - a_hat_j[j]/ST_A
        end
    end


    mom_value = Vector{T}(length(d.data.tMoments))

    for (m,idx_mom) in d.data._tMomentDict
        t_est = sliceMean_wgt(t_norm_j,s_hat_j,idx_mom)
        #mom_value[m] = d.data.tMoments[m] - t_est
        mom_value[m] = t_est
    end

    return mom_value .- d.data.tMoments
end


function calc_risk_moments{T}(d::InsuranceLogit,p::Array{T})
    params = parDict(d,p)
    individual_values!(d,params)
    individual_shares(d,params)
    res = calc_risk_moments(d,params)
    return res
end

function calc_Transfers!(dTdθ::Matrix{Float64},
                        s_hat_j::Vector{Float64},
                        r_hat_j::Vector{Float64},
                        a_hat_j::Vector{Float64},
                        dSdθ_j::Matrix{Float64},
                        dRdθ::Matrix{Float64},
                        dAdθ::Matrix{Float64},
                        d::InsuranceLogit)
    Q = d.parLength[:All]
    num_prods = length(s_hat_j)
    t_norm_j = zeros(num_prods)
    for (s,idx_prod) in d.data._stDict
        ST_R = sliceMean_wgt(r_hat_j,s_hat_j,idx_prod)
        ST_A = sliceMean_wgt(a_hat_j,s_hat_j,idx_prod)


        for j in idx_prod
            t_norm_j[j] = r_hat_j[j]/ST_R - a_hat_j[j]/ST_A
        end


        for q in 1:Q
            dST_R = 0.0
            dST_A = 0.0
            for j in idx_prod
                @inbounds @fastmath dST_R+= dSdθ_j[q,j]*r_hat_j[j] + s_hat_j[j]*dRdθ[q,j]
                @inbounds @fastmath dST_A+= dSdθ_j[q,j]*a_hat_j[j] + s_hat_j[j]*dAdθ[q,j]
            end
            dST_R = dST_R/sum(s_hat_j[idx_prod]) - ST_R*sum(dSdθ_j[q,idx_prod])/sum(s_hat_j[idx_prod])
            dST_A = dST_A/sum(s_hat_j[idx_prod]) - ST_A*sum(dSdθ_j[q,idx_prod])/sum(s_hat_j[idx_prod])

            for j in idx_prod
                @inbounds @fastmath dR = dRdθ[q,j]/ST_R - r_hat_j[j]*dST_R/(ST_R^2)
                @inbounds @fastmath dA = dAdθ[q,j]/ST_A - a_hat_j[j]*dST_A/(ST_A^2)
                @inbounds @fastmath dTdθ[q,j] = dR - dA
            end
        end
    end
    return t_norm_j
end
