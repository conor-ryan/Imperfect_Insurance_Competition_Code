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
    μ_ij_sums_sq = (μ_ij_sums).^2
    μ_ij_sums_cu = (μ_ij_sums).^3

    return μ_ij_sums, μ_ij_sums_sq,μ_ij_sums_cu
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
        μ_ij_sums, μ_ij_sums_sq,μ_ij_sums_cu = preCalcμ(μ_ij,δ)

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
        dμ_ij_x = Vector{Float64}(K)
        dμ_ij_y = Vector{Float64}(K)
        dμ_ij_xy = Vector{Float64}(K)
        dS1 = Vector{Float64}(K)
        dS2a = Vector{Float64}(K)
        dS2b = Vector{Float64}(K)
        dS3 = [0.0]
        dS4a = [0.0]
        dS4b = [0.0]

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


                hess_calc!(dS1,dS2a,dS2b,dS3,dS4a,dS4b,
                            zero_draws,non_zero_draws,
                            dR,risk,r_age,
                            dμ_ij_x,dμ_ij_y,dμ_ij_xy,
                            μ_ij,δ,X_mat,Y,
                            μ_ij_sums,μ_ij_sums_sq,
                            μ_ij_sums_cu)

                hess_obs = combine_hess(N,gll_t1,gll_t2,gll_t3,gll_t4,
                            dS1,dS2a,dS2b,dS3,dS4a,dS4b)

                hess[q,r]+= hess_obs
                if (q!=r)
                    hess[r,q]+= hess_obs
                end
            end
            ## Calculate Gradient
            grad[q]+= combine_grad(N,gll_t1,gll_t3,dS2a,dS4a)
            for (k,ij) in enumerate(idxitr)
                @inbounds @fastmath p.dSdθ[q,ij] = dS2a[k]/N
                @inbounds @fastmath p.dRdθ[q,ij] = dR[k]/(s_hat[k]*N) - (dS2a[k]/N)/s_hat[k] * r_avg[k]
            end
        end

    return ll_obs,pars_relevant
end

function combine_hess(N::Int64,
                    gll_t1::Vector{Float64},gll_t2::Vector{Float64},
                    gll_t3::Vector{Float64},gll_t4::Vector{Float64},
                    dS1::Vector{Float64},dS2a::Vector{Float64},
                    dS2b::Vector{Float64},dS3::Vector{Float64},
                    dS4a::Vector{Float64},dS4b::Vector{Float64})
    hess_obs = 0.0
    K = length(dS1)

    @inbounds @fastmath @simd for k in 1:K
        hess_obs += gll_t1[k]*(dS1[k]/N) - gll_t2[k]*(dS2a[k]/N)*(dS2b[k]/N) - (gll_t3[k]*(dS3[1]/N) - gll_t4[k]*dS4a[1]/N*dS4b[1]/N)
    end
    return hess_obs
end


function combine_grad(N::Int64,
                    gll_t1::Vector{Float64},
                    gll_t3::Vector{Float64},
                    dS2a::Vector{Float64},
                    dS4a::Vector{Float64})
    grad_obs = 0.0
    K = length(dS2a)

    @inbounds @fastmath @simd for k in 1:K
        grad_obs += gll_t1[k]*dS2a[k] - gll_t3[k]*dS4a[1]
    end
    return grad_obs/N
end

function calc_derSums(n::Int64,dμ_ij_x::Vector{Float64},
                    dμ_ij_y::Vector{Float64},
                    dμ_ij_xy::Vector{Float64},
                    X_mat::Matrix{Float64},Y::Matrix{Float64},
                    μ_ij::Matrix{Float64},δ::Vector{Float64})

    dμ_ij_x_sums = 0.0
    dμ_ij_y_sums = 0.0
    dμ_ij_xy_sums = 0.0
    K = length(δ)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n,k]*δ[k]
        @inbounds @fastmath x = X_mat[n,k]
        @inbounds @fastmath y = Y[n,k]

        @inbounds @fastmath dμ_ij_x[k] = u*x
        @inbounds @fastmath dμ_ij_y[k] = u*y
        @inbounds @fastmath dμ_ij_xy[k] = u*x*y


        @inbounds @fastmath dμ_ij_x_sums+= dμ_ij_x[k]
        @inbounds @fastmath dμ_ij_y_sums+= dμ_ij_y[k]
        @inbounds @fastmath dμ_ij_xy_sums+= dμ_ij_xy[k]
    end

    return dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_xy_sums
end

function calc_totTerms!(dS3::Vector{Float64},
                        dS4a::Vector{Float64},dS4b::Vector{Float64},
                        dμ_ij_x_sums::Float64,
                        dμ_ij_y_sums::Float64,
                        dμ_ij_xy_sums::Float64,
                        μ_ij_sums_n::Float64,μ_ij_sums_sq_n::Float64,
                        μ_ij_sums_cu_n::Float64)

    @fastmath tx3 = (dμ_ij_x_sums/μ_ij_sums_sq_n)
    @fastmath ty3 = (dμ_ij_y_sums/μ_ij_sums_sq_n)

    @fastmath txy6 = dμ_ij_xy_sums/μ_ij_sums_n
    @fastmath txy7 = 2*dμ_ij_x_sums*dμ_ij_y_sums/(μ_ij_sums_cu_n)
    @fastmath txy8 = dμ_ij_xy_sums*(μ_ij_sums_n-1)/μ_ij_sums_sq_n

    @inbounds @fastmath dS3[1]+= txy6 - txy7 - txy8
    @inbounds @fastmath dS4a[1]+= tx3
    @inbounds @fastmath dS4b[1]+= ty3
    return tx3, ty3, txy7
end

function calc_totTerms!(Num::Int64,dS3::Vector{Float64},
                        dS4a::Vector{Float64},dS4b::Vector{Float64},
                        dμ_ij_x_sums::Float64,
                        dμ_ij_y_sums::Float64,
                        dμ_ij_xy_sums::Float64,
                        μ_ij_sums_n::Float64,μ_ij_sums_sq_n::Float64,
                        μ_ij_sums_cu_n::Float64)

    @fastmath tx3 = (dμ_ij_x_sums/μ_ij_sums_sq_n)
    @fastmath ty3 = (dμ_ij_y_sums/μ_ij_sums_sq_n)

    @fastmath txy6 = dμ_ij_xy_sums/μ_ij_sums_n
    @fastmath txy7 = 2*dμ_ij_x_sums*dμ_ij_y_sums/(μ_ij_sums_cu_n)
    @fastmath txy8 = dμ_ij_xy_sums*(μ_ij_sums_n-1)/μ_ij_sums_sq_n

    @inbounds @fastmath dS3[1]+= Num*(txy6 - txy7 - txy8)
    @inbounds @fastmath dS4a[1]+= Num*tx3
    @inbounds @fastmath dS4b[1]+= Num*ty3
    return tx3, ty3, txy7
end

function calc_prodTerms!(n::Int64,dS1::Vector{Float64},
                        dS2a::Vector{Float64},dS2b::Vector{Float64},
                        dR::Vector{Float64},risk::Matrix{Float64},
                        risk_age::Vector{Float64},
                        dμ_ij_x::Vector{Float64},dμ_ij_x_sums::Float64,
                        dμ_ij_y::Vector{Float64},dμ_ij_y_sums::Float64,
                        dμ_ij_xy::Vector{Float64},dμ_ij_xy_sums::Float64,
                        μ_ij::Matrix{Float64},δ::Vector{Float64},
                        μ_ij_sums_n::Float64,μ_ij_sums_sq_n::Float64,
                        μ_ij_sums_cu_n::Float64,
                        tx3::Float64,ty3::Float64,txy7::Float64)

    K = length(dS1)
    # #Try simd on three separate loops
    # @inbounds @fastmath @simd for k in 1:K
    #     dS1[k]+= dμ_ij_xy[k]/μ_ij_sums_n - dμ_ij_x[k]*ty3 - dμ_ij_y[k]*tx3 - dμ_ij_xy_sums*μ_ij[n,k]*δ[k]/μ_ij_sums_sq_n + txy7*μ_ij[n,k]*δ[k]
    #
    # end
    # @inbounds @fastmath @simd for k in 1:K
    #     dS2a[k]+= (dμ_ij_x[k]/μ_ij_sums_n-tx3*μ_ij[n,k]*δ[k])
    # end
    # @inbounds @fastmath @simd for k in 1:K
    #     dS2b[k]+= (dμ_ij_y[k]/μ_ij_sums_n-ty3*μ_ij[n,k]*δ[k])
    # end
    for k in 1:K
        # @inbounds @fastmath txy1 = dμ_ij_xy[k]/μ_ij_sums_n
        # @inbounds @fastmath txy2 = dμ_ij_x[k]*ty3
        # @inbounds @fastmath txy3 = dμ_ij_y[k]*tx3
        # @inbounds @fastmath txy4 = dμ_ij_xy_sums*μ_ij[n,k]*δ[k]/μ_ij_sums_sq_n
        # @inbounds @fastmath txy5 = txy7*μ_ij[n,k]*δ[k]

        @inbounds @fastmath tx = (dμ_ij_x[k]/μ_ij_sums_n-tx3*μ_ij[n,k]*δ[k])
        @inbounds @fastmath dS1[k]+= dμ_ij_xy[k]/μ_ij_sums_n - dμ_ij_x[k]*ty3 - dμ_ij_y[k]*tx3 - dμ_ij_xy_sums*μ_ij[n,k]*δ[k]/μ_ij_sums_sq_n + txy7*μ_ij[n,k]*δ[k]
        @inbounds @fastmath dS2a[k]+= tx
        @inbounds @fastmath dR[k]+= tx*(risk[n,k]+risk_age[k])
        @inbounds @fastmath dS2b[k]+= (dμ_ij_y[k]/μ_ij_sums_n-ty3*μ_ij[n,k]*δ[k])
    end
end

function calc_prodTerms!(n::Int64,N::Int64,dS1::Vector{Float64},
                        dS2a::Vector{Float64},dS2b::Vector{Float64},
                        dR::Vector{Float64},risk::Matrix{Float64},
                        risk_age::Vector{Float64},
                        dμ_ij_x::Vector{Float64},dμ_ij_x_sums::Float64,
                        dμ_ij_y::Vector{Float64},dμ_ij_y_sums::Float64,
                        dμ_ij_xy::Vector{Float64},dμ_ij_xy_sums::Float64,
                        μ_ij::Matrix{Float64},δ::Vector{Float64},
                        μ_ij_sums_n::Float64,μ_ij_sums_sq_n::Float64,
                        μ_ij_sums_cu_n::Float64,
                        tx3::Float64,ty3::Float64,txy7::Float64)

    K = length(dS1)

    for k in 1:K
        @inbounds @fastmath tx = (dμ_ij_x[k]/μ_ij_sums_n-tx3*μ_ij[n,k]*δ[k])
        @inbounds @fastmath dS1[k]+= N*(dμ_ij_xy[k]/μ_ij_sums_n - dμ_ij_x[k]*ty3 - dμ_ij_y[k]*tx3 - dμ_ij_xy_sums*μ_ij[n,k]*δ[k]/μ_ij_sums_sq_n + txy7*μ_ij[n,k]*δ[k])
        @inbounds @fastmath dS2a[k]+= N*tx
        @inbounds @fastmath dR[k]+= N*tx*(risk[n,k]+risk_age[k])
        @inbounds @fastmath dS2b[k]+= N*(dμ_ij_y[k]/μ_ij_sums_n-ty3*μ_ij[n,k]*δ[k])
    end
end

function hess_calc!(dS1::Vector{Float64},dS2a::Vector{Float64},
                    dS2b::Vector{Float64},dS3::Vector{Float64},
                    dS4a::Vector{Float64},dS4b::Vector{Float64},
                    zero_draws::Vector{Int},non_zero_draws::Vector{Int},
                    dR::Vector{Float64},risk::Matrix{Float64},
                    risk_age::Vector{Float64},
                    dμ_ij_x::Vector{Float64},
                    dμ_ij_y::Vector{Float64},
                    dμ_ij_xy::Vector{Float64},
                    μ_ij::Matrix{Float64},δ::Vector{Float64},
                    X_mat::Matrix{Float64},Y::Matrix{Float64},
                    μ_ij_sums::Vector{Float64},μ_ij_sums_sq::Vector{Float64},
                    μ_ij_sums_cu::Vector{Float64})
    dS1[:] = 0.0
    dS2a[:] = 0.0
    dS2b[:] = 0.0
    dS3[:] = 0.0
    dS4a[:] = 0.0
    dS4b[:] = 0.0
    dR[:] = 0.0

    (N,K) = size(μ_ij)

    ### 0 Draw Calculation ###
    n = zero_draws[1]
    Num = length(zero_draws)
    μ_ij_sums_n = μ_ij_sums[n]
    μ_ij_sums_sq_n = μ_ij_sums_sq[n]
    μ_ij_sums_cu_n = μ_ij_sums_cu[n]

    dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_xy_sums = calc_derSums(n,dμ_ij_x,dμ_ij_y,dμ_ij_xy,
                        X_mat,Y,μ_ij,δ)

    tx3, ty3, txy7 = calc_totTerms!(Num,dS3,dS4a,dS4b,
                    dμ_ij_x_sums,dμ_ij_y_sums,dμ_ij_xy_sums,
                    μ_ij_sums_n,μ_ij_sums_sq_n,μ_ij_sums_cu_n)

    calc_prodTerms!(n,Num,dS1,dS2a,dS2b,dR,risk,risk_age,
                dμ_ij_x,dμ_ij_x_sums,
                dμ_ij_y,dμ_ij_y_sums,
                dμ_ij_xy,dμ_ij_xy_sums,
                μ_ij,δ,μ_ij_sums_n,
                μ_ij_sums_sq_n,μ_ij_sums_cu_n,
                tx3,ty3,txy7)

    for n in non_zero_draws
        μ_ij_sums_n = μ_ij_sums[n]
        μ_ij_sums_sq_n = μ_ij_sums_sq[n]
        μ_ij_sums_cu_n = μ_ij_sums_cu[n]

        dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_xy_sums = calc_derSums(n,dμ_ij_x,dμ_ij_y,dμ_ij_xy,
                            X_mat,Y,μ_ij,δ)

        tx3, ty3, txy7 = calc_totTerms!(dS3,dS4a,dS4b,
                        dμ_ij_x_sums,dμ_ij_y_sums,dμ_ij_xy_sums,
                        μ_ij_sums_n,μ_ij_sums_sq_n,μ_ij_sums_cu_n)

        calc_prodTerms!(n,dS1,dS2a,dS2b,dR,risk,risk_age,
                    dμ_ij_x,dμ_ij_x_sums,
                    dμ_ij_y,dμ_ij_y_sums,
                    dμ_ij_xy,dμ_ij_xy_sums,
                    μ_ij,δ,μ_ij_sums_n,
                    μ_ij_sums_sq_n,μ_ij_sums_cu_n,
                    tx3,ty3,txy7)
    end
    return Void
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
