function unPackChars(app::ChoiceData,d::InsuranceLogit)
    ind = person(app)[1]
    r_ind = Int(rIndS(app)[1])
    r_ind_metal = Int.(rInd(app))[:]
    S_ij = transpose(choice(app))
    wgt = transpose(weight(app))
    urate = transpose(unins(app))
    risk_age = ageHCC(app)[:]
    idxitr = d.data._personDict[ind]

    X_t = prodchars(app)
    X_0_t = prodchars0(app)
    Z = demoRaw(app)[:,1]
    #F_t = fixedEffects(app)
    F_t = fixedEffects(app,idxitr)

    return ind, r_ind, r_ind_metal, S_ij, wgt, urate, idxitr, X_t, X_0_t, Z, F_t,risk_age
end

function unPackParChars{T}(p::parDict{T},idxitr::UnitRange{Int})
    μ_ij = p.μ_ij[:,idxitr]
    δ    = p.δ[idxitr]

    #Get Market Shares
    s_hat = p.s_hat[idxitr]

    return μ_ij,δ,s_hat
end

function sumShares!(s_hat::Vector{Float64},ind::Float64)
    # Fix possible computational error
    for k in eachindex(s_hat)
        if abs(s_hat[k])<=1e-300
            s_hat[k]=1e-15
            #println("Hit Share Constraint for person $ind, product $k")
        end
    end
    s_insured = sum(s_hat)
    if s_insured>=(1-1e-300)
        s_insured= 1 - 1e-15
        #println("Hit insured constraint for person $ind")
    end

    return s_insured
end

function relPar(app::ChoiceData,d::InsuranceLogit,F_t::SubArray,ind::Float64)
    Q = d.parLength[:All]
    Q_0 = Q - size(F_t,1)
    ## Relevant Parameters for this observation
    pars_relevant = vcat(1:Q_0,Q_0+app._rel_fe_Dict[ind])

    return pars_relevant
end

function preCalcμ(μ_ij::Matrix{Float64},δ::Vector{Float64})
    μ_ij_sums = 1.+μ_ij*δ
    # μ_ij_sums_sq = (μ_ij_sums).^2
    # μ_ij_sums_cu = (μ_ij_sums).^3

    return μ_ij_sums
end

function ll_Terms{N}(wgt::Array{Float64,N},S_ij::Array{Float64,N},urate::Array{Float64,N},
                    s_hat::Vector{Float64},s_insured::Float64)
    K = length(S_ij)
    ll_obs = 0.0
    gll_t1 = Vector{Float64}(K)
    gll_t2 = Vector{Float64}(K)
    gll_t3 = Vector{Float64}(K)
    gll_t4 = Vector{Float64}(K)
    for k in 1:K
        #Gradient Terms
        gll_t1[k] = wgt[k]*S_ij[k]*(1/s_hat[k])
        gll_t2[k] = wgt[k]*S_ij[k]*(1/s_hat[k]^2)
        gll_t3[k] = wgt[k]*S_ij[k]*urate[k]*(1/(s_insured) + 1/(1-s_insured))
        gll_t4[k] = wgt[k]*S_ij[k]*urate[k]*(1/(s_insured^2) - 1/((1-s_insured)^2))
        # Log Likelihood
        ll_obs+=wgt[k]*S_ij[k]*(log(s_hat[k]) -
                        urate[k]*(log(s_insured)-log(1-s_insured)))
    end

    return gll_t1, gll_t2, gll_t3, gll_t4, ll_obs
end

function ll_obs_hessian!{T}(hess::Matrix{Float64},grad::Vector{Float64},
                            app::ChoiceData,d::InsuranceLogit,p::parDict{T})

        ind, r_ind, r_ind_metal, S_ij, wgt, urate, idxitr, X_t, X_0_t, Z, F_t, r_age = unPackChars(app,d)

        draws = d.draws
        non_zero_draws = find(d.draws[:,r_ind].>0)
        zero_draws = find(d.draws[:,r_ind].==0)
        risk = draws[:,r_ind_metal]
        r_avg = p.r_hat[idxitr]

        # Get Utility and derivative of Utility
        μ_ij,δ,s_hat = unPackParChars(p,idxitr)

        s_insured = sumShares!(s_hat,ind)

        (N,K) = size(μ_ij)

        # Initialize Gradient
        #(Q,N,K) = size(dμ_ij)
        pars_relevant = relPar(app,d,F_t,ind)

        # Pre-Calculate Squares
        μ_ij_sums = preCalcμ(μ_ij,δ)

        # Pre-Calculate Log-Likelihood Terms for Gradient
        # Also Calculate Log-Likelihood itself
        gll_t1, gll_t2, gll_t3, gll_t4, ll_obs = ll_Terms(wgt,S_ij,urate,s_hat,s_insured)

        #hess = zeros(Q,Q)
        #hess[:] = 0.0
        #grad[:] = 0.0
        X_mat = Array{Float64}(N,K)
        Y_mat = Array{Array{Float64,2},1}(length(pars_relevant))
        #Y_mat = Array{Float64}(N,K)

        # Allocate Memory
        dS_xy = Vector{Float64}(K)
        dS_x = Vector{Float64}(K)
        dS_y = Vector{Float64}(K)

        s_n = Vector{Float64}(K)

        dR = Vector{Float64}(K)

        #γlen = 1 + d.parLength[:γ]
        γlen = d.parLength[:γ]
        β0len = γlen + d.parLength[:β0]
        βlen = β0len + d.parLength[:γ]
        σlen = βlen + d.parLength[:σ]
        FElen = σlen + d.parLength[:FE]


        for (q_i,q) in enumerate(pars_relevant)
            returnParameter!(q,X_mat,
                            Z,X_0_t,X_t,draws,F_t,r_ind,
                            γlen,β0len,βlen,σlen)
            @inbounds Y_mat[q_i] = X_mat[:,:]

            for (r_i,r) in enumerate(pars_relevant)
                if r>q
                    continue
                end
                Y = Y_mat[r_i]


                dS_xy_all, dS_x_all, dS_y_all = hess_calc!(dS_xy,dS_x,dS_y,s_n,
                            zero_draws,non_zero_draws,
                            dR,risk,r_age,
                            μ_ij,δ,X_mat,Y,
                            μ_ij_sums)

                hess_obs = combine_hess(N,gll_t1,gll_t2,gll_t3,gll_t4,
                            dS_xy,dS_x,dS_y,dS_xy_all,dS_x_all,dS_y_all)

                hess[q,r]+= hess_obs
                if (q!=r)
                    hess[r,q]+= hess_obs
                end
                if (q==r)
                    ## Calculate Gradient
                    grad[q]+= combine_grad(N,gll_t1,gll_t3,dS_x,dS_x_all)
                end
            end

            for (k,ij) in enumerate(idxitr)
                @inbounds @fastmath p.dSdθ[q,ij] = dS_x[k]/N
                @inbounds @fastmath p.dRdθ[q,ij] = dR[k]/(s_hat[k]*N) - (dS_x[k]/N)/s_hat[k] * r_avg[k]
            end
        end

    return ll_obs,pars_relevant
end

function combine_hess(N::Int64,
                    gll_t1::Vector{Float64},gll_t2::Vector{Float64},
                    gll_t3::Vector{Float64},gll_t4::Vector{Float64},
                    dS_xy::Vector{Float64},dS_x::Vector{Float64},
                    dS_y::Vector{Float64},dS_xy_all::Float64,
                    dS_x_all::Float64,dS_y_all::Float64)
    hess_obs = 0.0
    K = length(dS_xy)

    @inbounds @fastmath @simd for k in 1:K
        hess_obs += gll_t1[k]*(dS_xy[k]/N) - gll_t2[k]*(dS_x[k]/N)*(dS_y[k]/N) - (gll_t3[k]*(dS_xy_all/N) - gll_t4[k]*dS_x_all/N*dS_y_all/N)
    end
    return hess_obs
end


function combine_grad(N::Int64,
                    gll_t1::Vector{Float64},
                    gll_t3::Vector{Float64},
                    dS_x::Vector{Float64},
                    dS_x_all::Float64)
    grad_obs = 0.0
    K = length(dS_x)

    @inbounds @fastmath @simd for k in 1:K
        grad_obs += gll_t1[k]*dS_x[k] - gll_t3[k]*dS_x_all
    end
    return grad_obs/N
end

function calc_derSums!(n::Int64,s_n::Vector{Float64},
                    X_mat::Matrix{Float64},Y::Matrix{Float64},
                    μ_ij::Matrix{Float64},δ::Vector{Float64},
                    μ_ij_sums_n::Float64)

    dμ_ij_x_sums = 0.0
    dμ_ij_y_sums = 0.0
    dμ_ij_xy_sums = 0.0

    K = length(δ)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n,k]*δ[k]
        @inbounds @fastmath x = X_mat[n,k]
        @inbounds @fastmath y = Y[n,k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
        @inbounds @fastmath dμ_ij_x_sums+= u*x
        @inbounds @fastmath dμ_ij_y_sums+= u*y
        @inbounds @fastmath dμ_ij_xy_sums+= u*x*y
    end

    return dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_xy_sums
end


function calc_prodTerms!(n::Int64,dS_xy::Vector{Float64},
                        dS_x::Vector{Float64},dS_y::Vector{Float64},
                        dR::Vector{Float64},risk::Matrix{Float64},
                        risk_age::Vector{Float64},
                        X_mat::Matrix{Float64},Y::Matrix{Float64},
                        s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_xy::Float64)

    K = length(dS_xy)
    t_last = Γ_x*Γ_y -Γ_xy
    for k in 1:K
        @inbounds @fastmath tx = s_n[k]*(X_mat[n,k] - Γ_x)
        @inbounds @fastmath dS_xy[k]+= s_n[k]*((X_mat[n,k] - Γ_x)*(Y[n,k] - Γ_y)+t_last)
        @inbounds @fastmath dS_x[k]+= tx
        @inbounds @fastmath dS_y[k]+= s_n[k]*(Y[n,k] - Γ_y)
        @inbounds @fastmath dR[k]+= tx*(risk[n,k]+risk_age[k])
    end
end

function calc_prodTerms!(n::Int64,N::Int64,dS_xy::Vector{Float64},
                        dS_x::Vector{Float64},dS_y::Vector{Float64},
                        dR::Vector{Float64},risk::Matrix{Float64},
                        risk_age::Vector{Float64},
                        X_mat::Matrix{Float64},Y::Matrix{Float64},
                        s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_xy::Float64)

    K = length(dS_xy)
    t_last = Γ_x*Γ_y -Γ_xy
    for k in 1:K
        @inbounds @fastmath tx = s_n[k]*(X_mat[n,k] - Γ_x)
        @inbounds @fastmath dS_xy[k]+= N*s_n[k]*((X_mat[n,k] - Γ_x)*(Y[n,k] - Γ_y)+t_last)
        @inbounds @fastmath dS_x[k]+= N*tx
        @inbounds @fastmath dS_y[k]+= N*s_n[k]*(Y[n,k] - Γ_y)
        @inbounds @fastmath dR[k]+= N*tx*(risk[n,k]+risk_age[k])
    end
end

function hess_calc!(dS_xy::Vector{Float64},dS_x::Vector{Float64},
                    dS_y::Vector{Float64},s_n::Vector{Float64},
                    zero_draws::Vector{Int},non_zero_draws::Vector{Int},
                    dR::Vector{Float64},risk::Matrix{Float64},
                    risk_age::Vector{Float64},
                    μ_ij::Matrix{Float64},δ::Vector{Float64},
                    X_mat::Matrix{Float64},Y::Matrix{Float64},
                    μ_ij_sums::Vector{Float64})
    dS_xy[:] = 0.0
    dS_x[:] = 0.0
    dS_y[:] = 0.0
    dS_xy_all = 0.0
    dS_x_all = 0.0
    dS_y_all = 0.0
    dR[:] = 0.0

    (N,K) = size(μ_ij)

    ### 0 Draw Calculation ###
    n = zero_draws[1]
    Num = length(zero_draws)
    μ_ij_sums_n = μ_ij_sums[n]

    dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_xy_sums = calc_derSums!(n,s_n,X_mat,Y,μ_ij,δ,μ_ij_sums_n)

    @fastmath Γ_x = dμ_ij_x_sums/μ_ij_sums_n
    @fastmath Γ_y = dμ_ij_y_sums/μ_ij_sums_n
    @fastmath Γ_xy = dμ_ij_xy_sums/μ_ij_sums_n
    # s_n = s_n./μ_ij_sums_n

    calc_prodTerms!(n,Num,dS_xy,dS_x,dS_y,dR,risk,risk_age,
                X_mat,Y,s_n,Γ_x,Γ_y,Γ_xy)

    for n in non_zero_draws
        μ_ij_sums_n = μ_ij_sums[n]

        dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_xy_sums = calc_derSums!(n,s_n,X_mat,Y,μ_ij,δ,μ_ij_sums_n)

        @fastmath Γ_x = dμ_ij_x_sums/μ_ij_sums_n
        @fastmath Γ_y = dμ_ij_y_sums/μ_ij_sums_n
        @fastmath Γ_xy = dμ_ij_xy_sums/μ_ij_sums_n
        # s_n = μ_ij[n,:].*δ/μ_ij_sums_n

        calc_prodTerms!(n,dS_xy,dS_x,dS_y,dR,risk,risk_age,X_mat,Y,s_n,
                    Γ_x,Γ_y,Γ_xy)
    end
    for k in 1:K
        @inbounds @fastmath dS_xy_all+= dS_xy[k]
        @inbounds @fastmath dS_x_all+= dS_x[k]
        @inbounds @fastmath dS_y_all+= dS_y[k]
    end
    return dS_xy_all, dS_x_all, dS_y_all
end


function getIndex(A::Float64,n::Int64,k::Int64)
    return A
end

function getIndex(A::Array{Float64,1},n::Int64,k::Int64)
    return A[k]
end

function getIndex(A::Array{Float64,2},n::Int64,k::Int64)
    return A[n,k]
end

function returnParameter!(q::Int64,X_mat::Matrix{Float64},
                        Z::Vector{Float64},X_0_t::Matrix{Float64},
                        X_t::Matrix{Float64},draws::Matrix{Float64},
                        F_t::SubArray{Float64,2},r_ind::Int64,
                        γlen::Int64,β0len::Int64,βlen::Int64,σlen::Int64)
    (N,K) = size(X_mat)
    if q<0
        X_mat[:] = 1.0
    elseif q<=γlen
        X_mat[:] = Z[q]
    elseif q<=β0len
        for n in 1:N
            @inbounds X_mat[n,:] = X_0_t[q-γlen,:]
        end
    elseif q<=βlen
        # Characteristic Interactions
        for n in 1:N
            @inbounds X_mat[n,:] = X_0_t[1,:].*Z[q-β0len]
        end
    elseif q<=σlen
        #Quality Random Effect
        for n in 1:N,k in 1:K
            @inbounds X_mat[n,k] = draws[n,r_ind]*X_t[1+q-(βlen),k]
        end
    else
        #Fixed Effect
        for n in 1:N
            @inbounds X_mat[n,:] = F_t[q-σlen,:]
        end
    end
    return Void
end
