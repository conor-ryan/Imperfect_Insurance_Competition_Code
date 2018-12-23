@views function sliceSum_wgt(x::Vector{T},w::Vector{S},idx::Array{Int64,1}) where {T,S}
    tot = 0.0
    @inbounds @fastmath @simd for q in idx
        tot+= x[q]*w[q]
    end
    return tot
end
@views function sliceSum_wgt(x::SubArray,w::Vector{S},idx::Array{Int64,1}) where S
    tot = 0.0
    @inbounds @fastmath @simd for q in idx
        tot+= x[q]*w[q]
    end
    return tot
end

@views function sliceSum_wgt(k::Int64,x::Matrix{T},w::Vector{S},idx::Array{Int64,1}) where {T,S}
    tot = 0.0
    @inbounds @fastmath @simd for q in idx
        tot+= x[k,q]*w[q]
    end
    return tot
end

@views function sliceSum_wgt(x::Vector{T},y::Vector{Float64},w::Vector{T},
                                    idx::Array{Int64,1}) where T
    tot = 0.0
    @inbounds @fastmath @simd for q in idx
        tot+= x[q]*y[q]*w[q]
    end
    return tot
end

@views function sliceSum_wgt(x::Vector{T},y::SubArray,w::Vector{T},
                                    idx::Array{Int64,1}) where T
    tot = 0.0
    @inbounds @fastmath @simd for q in idx
        tot+= x[q]*y[q]*w[q]
    end
    return tot
end

@views function sliceSum_wgt(k::Int64,x::Vector{T},y::Matrix{Float64},w::Vector{T},
                                    idx::Array{Int64,1}) where T
    tot = 0.0
    @inbounds @fastmath @simd for q in idx
        tot+= x[q]*y[k,q]*w[q]
    end
    return tot
end

function calc_risk_moments!(hess::Array{Float64,3},grad::Matrix{Float64},d::InsuranceLogit,p::parDict{T}) where T
    wgts = weight(d.data)[1,:]
    wgts_share = wgts.*p.s_hat
    num_prods = length(d.prods)
    s_hat_j = Vector{Float64}(undef,num_prods)
    r_hat_j = Vector{Float64}(undef,num_prods)

    # Q = d.parLength[:All]

    dSdθ_j = p.dSdθ_j
    d2Sdθ_j = p.d2Sdθ_j

    prodAvgs!(s_hat_j,r_hat_j,wgts,wgts_share,d,p)

    mom_value = Vector{Float64}(undef,length(d.data.tMoments))

    calc_Mom!(mom_value,s_hat_j,r_hat_j,d,p)

    calc_Mom_Der!(grad,hess,dSdθ_j,d2Sdθ_j,mom_value,s_hat_j,r_hat_j,d,p)


    mom_disp = mom_value[1:6]
    println("Risk moments are $mom_disp")

    return mom_value .- d.data.tMoments
end


function calc_risk_moments!(grad::Matrix{Float64},d::InsuranceLogit,p::parDict{T}) where T
    wgts = weight(d.data)[1,:]
    wgts_share = wgts.*p.s_hat
    num_prods = length(d.prods)
    s_hat_j = Vector{Float64}(undef,num_prods)
    r_hat_j = Vector{Float64}(undef,num_prods)

    # Q = d.parLength[:All]

    dSdθ_j = p.dSdθ_j

    prodAvgs!(s_hat_j,r_hat_j,wgts,wgts_share,d,p)

    mom_value = Vector{Float64}(undef,length(d.data.tMoments))

    calc_Mom!(mom_value,s_hat_j,r_hat_j,d,p)

    calc_Mom_Der!(grad,dSdθ_j,mom_value,s_hat_j,r_hat_j,d,p)


    mom_disp = mom_value[1:6]
    println("Risk moments are $mom_disp")

    return mom_value .- d.data.tMoments
end

function calc_Mom!(mom_value::Vector{Float64},
                    s_hat_j::Vector{Float64},
                    r_hat_j::Vector{Float64},
                    d::InsuranceLogit,p::parDict{T}) where T
    for (m,idx_mom) in d.data._tMomentDict
        mom_value[m]  = sliceMean_wgt(r_hat_j,s_hat_j,idx_mom)
    end
end

function calc_Mom_Der!(grad::Matrix{Float64},
                    hess::Array{Float64,3},
                    dSdθ_j::Matrix{Float64},
                    d2Sdθ_j::Array{Float64,3},
                    mom_value::Vector{Float64},
                    s_hat_j::Vector{Float64},
                    r_hat_j::Vector{Float64},
                    d::InsuranceLogit,p::parDict{T}) where T
    Q = d.parLength[:All]
    for (m,idx_mom) in d.data._tMomentDict
        for q in 1:Q
            t1 = 0.0
            s_mom = sum(s_hat_j[idx_mom])
            dS_q_mom = sum(dSdθ_j[q,idx_mom])
            @inbounds @fastmath @simd for j in idx_mom
                t1+= p.dRdθ_j[q,j]
            end
            grad[q,m] = t1/s_mom - (dS_q_mom)/s_mom*mom_value[m]
            for l in 1:q
                dS_l_mom = sum(dSdθ_j[l,idx_mom])
                d2S_mom = sum(d2Sdθ_j[q,l,idx_mom])
                t2 = 0.0
                @inbounds @fastmath @simd for j in idx_mom
                    t2+= p.d2Rdθ_j[q,l,j]
                end
                h_term = t2/s_mom - (dS_q_mom/s_mom)*grad[l,m] - (dS_l_mom/s_mom)*grad[q,m] - d2S_mom/s_mom*mom_value[m]
                hess[q,l,m] = h_term
                hess[l,q,m] = h_term
            end
        end
    end
end

function calc_Mom_Der!(grad::Matrix{Float64},
                    dSdθ_j::Matrix{Float64},
                    mom_value::Vector{Float64},
                    s_hat_j::Vector{Float64},
                    r_hat_j::Vector{Float64},
                    d::InsuranceLogit,p::parDict{T}) where T
    Q = d.parLength[:All]
    for (m,idx_mom) in d.data._tMomentDict
        for q in 1:Q
            t1 = 0.0
            s_mom = sum(s_hat_j[idx_mom])
            dS_q_mom = sum(dSdθ_j[q,idx_mom])
            @inbounds @fastmath @simd for j in idx_mom
                t1+= p.dRdθ_j[q,j]
            end
            grad[q,m] = t1/s_mom - (dS_q_mom)/s_mom*mom_value[m]
        end
    end
end



function prodAvgs!(s_hat_j::Vector{Float64},
                    r_hat_j::Vector{Float64},
                    wgts::Vector{Float64},
                    wgts_share::Vector{Float64},
                    d::InsuranceLogit,p::parDict{T}) where T
    for j in d.prods
        j_index_all = d.data._productDict[j]
        s_hat_j[j] = sliceSum_wgt(p.s_hat,wgts,j_index_all)
        r_hat_j[j] = sliceMean_wgt(p.r_hat,wgts_share,j_index_all)#*d.Γ_j[j]
    end
    return nothing
end


function  prodDerivatives!(#dAdθ::Matrix{Float64},
                            dRdθ::Matrix{Float64},
                            dSdθ_j::Matrix{Float64},
                            d2Rdθ::Array{Float64,3},
                            d2Sdθ_j::Array{Float64,3},
                            S_unwt::Vector{Float64},
                            r_hat_j::Vector{Float64},
                            # a_hat_j::Vector{Float64},
                            # ageRate_long::Vector{Float64},
                            wgts::Vector{Float64},
                            d::InsuranceLogit,p::parDict{T}) where T
    Q = d.parLength[:All]

    t_q_list = Vector{Float64}(undef,Q)
    for j in d.prods
        Γ = d.Γ_j[j]
        S_j = S_unwt[j]
        R_j = r_hat_j[j]
        for q in 1:Q
            dS_q = dSdθ_j[q,j]
            dR_q = p.dRdθ_j[q,j]
            @inbounds @fastmath dRdθ[q,j] = dR_q*Γ/S_j - (dS_q/S_j)*R_j
            for m in 1:q
                dS_m = dSdθ_j[m,j]
                dR_m = p.dRdθ_j[m,j]
                @inbounds @fastmath d2Rdθ[q,m,j] = Γ*(p.d2Rdθ_j[q,m,j]/S_j - dR_q*(dS_m/S_j^2) - dR_m*(dS_q/S_j^2)) + 2*dS_m*dS_q*R_j/(S_j^2) - d2Sdθ_j[q,m,j]*R_j/S_j
            end
        end
    end

    return nothing
end

function  prodDer!(#dAdθ::Matrix{Float64},
                            dRdθ::Matrix{Float64},
                            dSdθ_j::Matrix{Float64},
                            S_unwt::Vector{Float64},
                            r_hat_j::Vector{Float64},
                            # a_hat_j::Vector{Float64},
                            # ageRate_long::Vector{Float64},
                            wgts::Vector{Float64},
                            d::InsuranceLogit,p::parDict{T}) where T
    Q = d.parLength[:All]

    t_q_list = Vector{Float64}(undef,Q)

    for q in 1:Q
        for j in d.prods
            # Γ = d.Γ_j[j]
            S_j = S_unwt[j]
            R_j = r_hat_j[j]
            dS_q = dSdθ_j[q,j]
            dR_q = p.dRdθ_j[q,j]
            @inbounds @fastmath dRdθ[q,j] = dR_q/S_j - (dS_q/S_j)*R_j
        end
    end

    return nothing
end

function  prodDer!(d2Rdθ::Array{Float64,3},
                        d2Sdθ_j::Array{Float64,3},
                        S_unwt::Vector{Float64},
                        r_hat_j::Vector{Float64},
                        # a_hat_j::Vector{Float64},
                        # ageRate_long::Vector{Float64},
                        wgts::Vector{Float64},
                        d::InsuranceLogit,p::parDict{T}) where T
    Q = d.parLength[:All]

    t_q_list = Vector{Float64}(undef,Q)

    for j in d.prods
        # Γ = d.Γ_j[j]
        S_j = S_unwt[j]
        R_j = r_hat_j[j]
        for q in 1:Q
            dS_q = p.dSdθ_j[q,j]
            dR_q = p.dRdθ_j[q,j]
            if (abs(dS_q)<1e-14)
                continue
            end
            for m in 1:q
                dS_m = p.dSdθ_j[m,j]
                dR_m = p.dRdθ_j[m,j]
                if (abs(dS_m)<1e-14)
                    continue
                end
                @inbounds @fastmath d2Rdθ[q,m,j] = (p.d2Rdθ_j[q,m,j]/S_j - dR_q*(dS_m/S_j^2) - dR_m*(dS_q/S_j^2)) + 2*dS_m*dS_q*R_j/(S_j^2) - d2Sdθ_j[q,m,j]*R_j/S_j
            end
        end
    end

    return nothing
end


function calc_risk_moments(d::InsuranceLogit,p::parDict{T}) where T
    wgts = weight(d.data)[1,:]
    wgts_share = wgts.*p.s_hat
    num_prods = length(d.prods)
    S_unwt = Vector{T}(undef,num_prods)
    s_hat_j = Vector{T}(undef,num_prods)
    r_hat_j = Vector{T}(undef,num_prods)
    r_hat_unwt_j = Vector{T}(undef,num_prods)
    a_hat_j = Vector{T}(undef,num_prods)

    ageRate_long = ageRate(d.data)[1,:]

    #prodAvgs!(S_unwt,r_hat_j,a_hat_j,ageRate_long,wgts,wgts_share,d,p)
    for j in d.prods
        j_index_all = d.data._productDict[j]
        S_unwt[j] = sliceSum_wgt(p.s_hat,wgts,j_index_all)
        #@inbounds @fastmath s_hat_j[j]= (S_unwt[j]/d.lives[j])*d.data.st_share[j]
        @inbounds s_hat_j[j]= S_unwt[j]
        r_hat_unwt_j[j] = sliceMean_wgt(p.r_hat,wgts_share,j_index_all)
        r_hat_j[j] = sliceMean_wgt(p.r_hat,wgts_share,j_index_all)*d.Γ_j[j]
        a_hat_j[j] = sliceMean_wgt(ageRate_long,wgts_share,j_index_all)*d.AV_j[j]*d.Γ_j[j]
    end
    # out=0.0
    t_norm_j = Vector{T}(undef,num_prods)
    t_norm_j[:] .= 0.0
    for (s,idx_prod) in d.data._stDict
        ST_R = sliceMean_wgt(r_hat_j,s_hat_j,idx_prod)
        ST_A = sliceMean_wgt(a_hat_j,s_hat_j,idx_prod)
        for j in idx_prod
            t_norm_j[j] = r_hat_j[j]/ST_R - a_hat_j[j]/ST_A
        end
    end


    mom_value = Vector{T}(undef,length(d.data.tMoments))

    for (m,idx_mom) in d.data._tMomentDict
        if true
            t_est = sliceMean_wgt(r_hat_unwt_j,s_hat_j,idx_mom)
            #mom_value[m] = d.data.tMoments[m] - t_est
            mom_value[m] = t_est
        else
            t_est = sliceMean_wgt(t_norm_j,s_hat_j,idx_mom)
            #mom_value[m] = d.data.tMoments[m] - t_est
            mom_value[m] = t_est
        end

    end

    return mom_value .- d.data.tMoments
end


function calc_risk_moments(d::InsuranceLogit,p::Array{T}) where T
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


function risk_moments_Avar(d::InsuranceLogit,p::parDict{T}) where T
    wgts = weight(d.data)[1,:]
    wgts_share = wgts.*p.s_hat
    num_prods = length(d.prods)
    S_unwt = Vector{T}(undef,num_prods)
    s_hat_2_j = zeros(num_prods)
    r_hat_2_j = zeros(num_prods)
    a_hat_2_j = zeros(num_prods)

    s_hat_obs_j = Vector{T}(undef,num_prods)
    r_hat_obs_j = Vector{T}(undef,num_prods)
    a_hat_obs_j = Vector{T}(undef,num_prods)

    T_grad  = zeros(num_prods,num_prods*3)

    mom_grad = zeros(length(d.data.tMoments),num_prods*3)

    ageRate_long = ageRate(d.data)[1,:]

    productIDs = Vector{Int64}(undef,length(wgts))
    for j in d.prods
        idx_prod = d.data._productDict[j]
        productIDs[idx_prod] .= j
    end

    #prodAvgs!(S_unwt,r_hat_j,a_hat_j,ageRate_long,wgts,wgts_share,d,p)
    ids = d.data._personIDs
    Pop = 0.0
    for i in ids
        idxitr = d.data._personDict[i]
        per_prods = productIDs[idxitr]
        for (k,j) in zip(idxitr,per_prods)
            s_hat_obs_j[j] = p.s_hat[k]
            r_hat_obs_j[j] = p.r_hat[k]*p.s_hat[k]*d.Γ_j[j]
            a_hat_obs_j[j] = ageRate_long[k]*p.s_hat[k]*d.AV_j[j]*d.Γ_j[j]

            s_hat_2_j[j]+= s_hat_obs_j[j]*wgts[k]
            r_hat_2_j[j]+= r_hat_obs_j[j]*wgts[k]
            a_hat_2_j[j]+= a_hat_obs_j[j]*wgts[k]
        end
        Pop+=wgts[idxitr[1]]
    end

    for j in d.prods
        s_hat_2_j[j] = s_hat_2_j[j]/Pop
        r_hat_2_j[j] = r_hat_2_j[j]/Pop
        a_hat_2_j[j] = a_hat_2_j[j]/Pop
    end

    # out=0.0
    t_norm_j = Vector{T}(undef,num_prods)
    t_norm_j[:] .= 0.0
    for (s,idx_prod) in d.data._stDict
        ST_R = sum(r_hat_2_j[idx_prod])/sum(s_hat_2_j[idx_prod])
        ST_A = sum(a_hat_2_j[idx_prod])/sum(s_hat_2_j[idx_prod])
        for j in idx_prod
            t_norm_j[j] = (r_hat_2_j[j]/s_hat_2_j[j])/ST_R - (a_hat_2_j[j]/s_hat_2_j[j])/ST_A

            t1 = r_hat_2_j[j]/sum(r_hat_2_j[idx_prod]) - a_hat_2_j[j]/sum(a_hat_2_j[idx_prod])
            t2 = sum(s_hat_2_j[idx_prod])/s_hat_2_j[j]
            for k in idx_prod
                if k==j
                    #S_hat_j Gradient
                    T_grad[j,j] = t1*(1/s_hat_2_j[j] - sum(s_hat_2_j[idx_prod])/(s_hat_2_j[j]^2))
                    #A_hat_j Gradient
                    T_grad[j,num_prods + j] = -t2*(1/sum(a_hat_2_j[idx_prod]) - a_hat_2_j[j]/(sum(a_hat_2_j[idx_prod])^2))
                    #R_hat_j Gradient
                    T_grad[j,num_prods*2 + j] = t2*(1/sum(r_hat_2_j[idx_prod]) - r_hat_2_j[j]/(sum(r_hat_2_j[idx_prod])^2))
                else
                    #S_hat_j Gradient
                    T_grad[j,k] = t1*(1/s_hat_2_j[j])
                    #A_hat_j Gradient
                    T_grad[j,num_prods + k] = t2*(a_hat_2_j[j]/(sum(a_hat_2_j[idx_prod])^2))
                    #R_hat_j Gradient
                    T_grad[j,num_prods*2 + k] = -t2*(r_hat_2_j[j]/(sum(r_hat_2_j[idx_prod])^2))
                end
            end
        end

    end

    mom_value = Vector{T}(undef,length(d.data.tMoments))

    R_unwt = similar(r_hat_2_j)
    J = length(r_hat_2_j)
    for j in 1:J
        R_unwt[j] = r_hat_2_j[j]/d.Γ_j[j]
    end

    for (m,idx_mom) in d.data._tMomentDict
        if true
            t_est = sum(R_unwt[idx_mom])/sum(s_hat_2_j[idx_mom])
            s_sum = sum(s_hat_2_j[idx_mom])
            for k in idx_mom
                #S_hat_j Gradient
                mom_grad[m,k] = (r_hat_2_j[k]/d.Γ_j[k])/s_sum - t_est/s_sum
                #A_hat_j Gradient
                mom_grad[m,(num_prods + k)] = 0.0
                #R_hat_j Gradient
                mom_grad[m,(num_prods*2 + k)] = (s_hat_2_j[k]/d.Γ_j[k])/s_sum
            end
        else
            t_est = sliceMean_wgt(t_norm_j,s_hat_2_j,idx_mom)
            #mom_value[m] = d.data.tMoments[m] - t_est
            mom_value[m] = t_est
            for k in d.prods
                #S_hat_j Gradient
                mom_grad[m,k] = sliceMean_wgt(T_grad[:,k],s_hat_2_j,idx_mom)
                #A_hat_j Gradient
                mom_grad[m,(num_prods + k)] = sliceMean_wgt(T_grad[:,(num_prods+k)],s_hat_2_j,idx_mom)
                #R_hat_j Gradient
                mom_grad[m,(num_prods*2 + k)] = sliceMean_wgt(T_grad[:,(num_prods*2+k)],s_hat_2_j,idx_mom)
            end
            for k in idx_mom
                #S_hat_j Gradient Addition
                mom_grad[m,k]+= (t_norm_j[k]-mom_value[m])/sum(s_hat_2_j[idx_mom])
            end
        end
    end

    return mom_grad
end


function test_Avar(d::InsuranceLogit,vec::Vector{T}) where T
    num_prods = length(d.prods)
    s_hat_2_j = vec[1:num_prods]
    a_hat_2_j = vec[(num_prods+1):(num_prods*2)]
    r_hat_2_j = vec[(num_prods*2+1):(num_prods*3)]


    # out=0.0
    t_norm_j = Vector{T}(undef,num_prods)
    t_norm_j[:] = 0.0
    for (s,idx_prod) in d.data._stDict
        ST_R = sum(r_hat_2_j[idx_prod])/sum(s_hat_2_j[idx_prod])
        ST_A = sum(a_hat_2_j[idx_prod])/sum(s_hat_2_j[idx_prod])
        for j in idx_prod
            t_norm_j[j] = (r_hat_2_j[j]/s_hat_2_j[j])/ST_R - (a_hat_2_j[j]/s_hat_2_j[j])/ST_A
        end
    end

    mom_value = Vector{T}(undef,length(d.data.tMoments))

    for (m,idx_mom) in d.data._tMomentDict
        t_est = sliceMean_wgt(t_norm_j,s_hat_2_j,idx_mom)
        #mom_value[m] = d.data.tMoments[m] - t_est
        mom_value[m] = t_est
    end

    return mean(mom_value)
end


# Calculate Standard Errors
# Hiyashi, p. 491
function calc_gmm_Avar(d::InsuranceLogit,p0::Vector{Float64})
    p = parDict(d,p0)
    # Calculate μ_ij, which depends only on parameters
    individual_values!(d,p)
    individual_shares(d,p)

    Pop =sum(weight(d.data).*choice(d.data))
    ageRate_long = ageRate(d.data)[1,:]
    wgts = weight(d.data)

    ### Unique Product IDs
    (N,M) = size(d.data.data)
    productIDs = Vector{Int64}(undef,M)
    for j in d.prods
        idx_prod = d.data._productDict[j]
        productIDs[idx_prod] .= j
    end
    num_prods = length(d.prods)

    mom_length = num_prods*3 + d.parLength[:All]
    Σ = zeros(mom_length,mom_length)

    risk_moments = Vector{Float64}(undef,num_prods*3)
    g_n = Vector{Float64}(undef,mom_length)
    grad_obs = Vector{Float64}(undef,d.parLength[:All])

    for app in eachperson(d.data)
        grad_obs[:] .= 0.0
        ll_obs,pars_relevant = ll_obs_gradient!(grad_obs,app,d,p)
        # ll_obs,grad_obs = ll_obs_gradient(app,d,p)

        idx_prod = risk_obs_moments!(risk_moments,productIDs,app,d,p)

        idx_nonEmpty = vcat(idx_prod,num_prods .+idx_prod,num_prods*2 .+idx_prod,(num_prods*3):mom_length)

        g_n[1:length(risk_moments)] = risk_moments[:]
        g_n[(length(risk_moments)+1):mom_length] = grad_obs[:]

        add_Σ(Σ,g_n,idx_nonEmpty)
    end

    Σ = Σ./(Pop)

    (N,M) = size(Σ)
    aVar = zeros(d.parLength[:All] + length(d.data.tMoments),M)
    (Q,R) = size(aVar)
    aVar[1:length(d.data.tMoments),(1:num_prods*3)] = risk_moments_Avar(d,p)
    aVar[(length(d.data.tMoments)+1):Q,(num_prods*3 + 1):R] = Matrix{Float64}(I,d.parLength[:All],d.parLength[:All])

    S_est = aVar*Σ*aVar'

    return S_est
end


function risk_obs_moments!(mom_obs::Vector{Float64},productIDs::Vector{Int64},
                    app::ChoiceData,d::InsuranceLogit,p::parDict{T}) where T
    mom_obs[:] .= 0.0
    wgts = weight(app)[1,:]
    ind = person(app)[1]
    num_prods = length(d.prods)


    ages = ageRate(app)[1,:]

    idxitr = app._personDict[ind]
    ind_itr = 1:length(idxitr)
    per_prods = productIDs[idxitr]

    for (i,k,j) in zip(ind_itr,idxitr,per_prods)
        #S_hat
        mom_obs[j] = p.s_hat[k]*wgts[i]
        #A_hat
        mom_obs[num_prods + j] = ages[i]*p.s_hat[k]*d.AV_j[j]*d.Γ_j[j]*wgts[i]
        #R_hat
        mom_obs[num_prods*2 + j] = p.r_hat[k]*p.s_hat[k]*d.Γ_j[j]*wgts[i]
    end

    return per_prods
end

function add_Σ(Σ::Matrix{Float64},g_n::Vector{Float64},idx::Vector{Int64})
    for i in idx, j in idx
        @fastmath @inbounds Σ[i,j]+=g_n[i]*g_n[j]
    end
    return nothing
end
