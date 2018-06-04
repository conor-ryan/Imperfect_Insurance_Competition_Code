import Base.getindex, Base.setindex!, Base.show
using NLopt
using ForwardDiff


########
# Parameter Structure
#########


type parDict{T}
    # Parameters
    γ_0::T
    γ::Vector{T}
    β_0::Vector{T}
    β::Matrix{T}
    σ::Vector{T}
    FE::Matrix{T}
    #Random Coefficients stored (function of σ and draws)
    randCoeffs::Array{T,2}
    # δ values for (ij) pairs
    δ::Vector{T}
    # Non-delta utility for (ij) pairs and draws
    μ_ij::Matrix{T}
    # Shares for (ij) pairs
    s_hat::Vector{T}
end

function parDict{T}(m::InsuranceLogit,x::Array{T})
    # Parameter Lengths from model
    #γlen = 1 + m.parLength[:γ]
    γlen = m.parLength[:γ]
    β0len = γlen + m.parLength[:β0]
    βlen = β0len + m.parLength[:γ]
    σlen = βlen + (m.parLength[:σ])
    FElen = σlen + m.parLength[:FE]

    # Distribute Parameters
    # γ_0 = x[1]
    # γ = x[2:γlen]
    γ_0 = 0.0
    γ = x[1:γlen]
    β_0 = x[(γlen+1):β0len]
    β_vec = x[(β0len+1):βlen]
    σ = x[(βlen+1):σlen]
    FE_vec = x[(σlen+1):FElen]

    # Store FE as row Vector
    FE = Matrix{T}(1,length(FE_vec))
    FE[1,:] = FE_vec

    # Stack Beta into a matrix
    K = m.parLength[:β]
    N = m.parLength[:γ]
    β = Matrix{T}(K,N)

    ind = 0
    for i in 1:N, j in 1:K
        if j==1
            ind+=1
            β[j,i] = β_vec[ind]
        else
            β[j,i] = 0
        end
    end

    #Calculate Random Coefficients matrix
    (S,R) = size(m.draws)
    randCoeffs = Array{T,2}(S,m.parLength[:σ])
    calcRC!(randCoeffs,σ,m.draws)

    #Initialize (ij) pairs of deltas
    L, M = size(m.data.data)
    δ = Vector{T}(M)
    μ_ij = Matrix{T}(S,M)
    s_hat = Vector{T}(M)
    unpack_δ!(δ,m)

    return parDict{T}(γ_0,γ,β_0,β,σ,FE,randCoeffs,δ,μ_ij,s_hat)
end

function calcRC!{T,S}(randCoeffs::Array{S},σ::Array{T},draws::Array{Float64,2})
    (N,K) = size(randCoeffs)
    #randCoeffs[:,1] = draws[:,1].*σ[1]
    #Skip Price Coefficient
    for k in 1:K,n in 1:N
        randCoeffs[n,k] = draws[n,1]*σ[k]
    end
    return Void
end


###########
# Calculating Preferences
###########

function calc_indCoeffs{T}(p::parDict{T},β::Array{T,1},d::T)
    Q = length(β)
    (N,K) = size(p.randCoeffs)
    β_i = Array{T,2}(N,Q)
    γ_i = d + p.randCoeffs[:,1]
    β_i[:,1] = β[1]

    for k in 2:Q, n in 1:N
        β_i[n,k] = β[k] + p.randCoeffs[n,k-1]
    end

    β_i = permutedims(β_i,(2,1))
    return β_i, γ_i
end

function individual_values!{T}(d::InsuranceLogit,p::parDict{T})
    # Calculate μ_ij, which depends only on parameters
    for app in eachperson(d.data)
        util_value!(app,p)
    end
    return Void
end

function util_value!{T}(app::ChoiceData,p::parDict{T})
    γ_0 = p.γ_0
    γ = p.γ
    β_0= p.β_0
    β = p.β
    fe = p.FE

    ind = person(app)[1]
    idxitr = app._personDict[ind]
    X = permutedims(prodchars(app),(2,1))
    X_0 = permutedims(prodchars0(app),(2,1))
    Z = demoRaw(app)[:,1]
    #F = fixedEffects(app)
    F = fixedEffects(app,idxitr)

    β_z = β*Z
    demos = γ_0 + vecdot(γ,Z)
    β_i, γ_i = calc_indCoeffs(p,β_z,demos)

    chars = X*β_i
    chars_0 = X_0*β_0

    # FE is a row Vector
    if T== Float64
        controls = zeros(size(F,2))
        for k in 1:length(controls)
            for j in app._rel_fe_Dict[ind]
                controls[k]+= fe[j]*F[j,k]
            end
        end
    else
        controls = fe*F
    end

    (K,N) = size(chars)
    for k = 1:K,n = 1:N
        u = exp(chars[k,n] + chars_0[k] + controls[k] + γ_i)
        p.μ_ij[n,idxitr[k]] = u
    end

    return Void
end

function calc_shares{T}(μ_ij::Array{T},δ::Vector{T})
    (N,K) = size(μ_ij)
    util = Matrix{T}(K,N)
    s_hat = Matrix{T}(K,N)

    for n in 1:N
        expsum = 1.0
        for i in 1:K
            a = μ_ij[n,i]*δ[i]
            util[i,n] = a
            expsum += a
        end
        for i in 1:K
            s_hat[i,n] = util[i,n]/expsum
        end
    end
    return mean(s_hat,2)
end

function individual_shares{T}(d::InsuranceLogit,p::parDict{T})
    # Store Parameters
    δ_long = p.δ
    μ_ij_large = p.μ_ij
    for idxitr in values(d.data._personDict)
        δ = δ_long[idxitr]
        u = μ_ij_large[:,idxitr]
        s = calc_shares(u,δ)
        p.s_hat[idxitr] = s
    end
    return Void
end


function ll_obs_gradient{T}(app::ChoiceData,d::InsuranceLogit,p::parDict{T})
        ind = person(app)[1]
        S_ij = transpose(choice(app))
        wgt = transpose(weight(app))
        urate = transpose(unins(app))
        idxitr = d.data._personDict[ind]

        X_t = prodchars(app)
        X_0_t = prodchars0(app)
        Z = demoRaw(app)[:,1]
        #F_t = fixedEffects(app)
        F_t = fixedEffects(app,idxitr)
        draws = d.draws

        # Get Utility and derivative of Utility
        μ_ij = p.μ_ij[:,idxitr]
        δ    = p.δ[idxitr]

        #Get Market Shares
        s_hat = p.s_hat[idxitr]
        s_insured = sum(s_hat)
        # Fix possible computational error
        for k in eachindex(s_hat)
            if abs(s_hat[k])<=1e-300
                s_hat[k]=1e-15
                println("Hit Share Constraint for person $ind, product $k")
            end
        end
        s_insured = sum(s_hat)
        if s_insured>=(1-1e-300)
            s_insured= 1 - 1e-15
            println("Hit insured constraint for person $ind")
        end


        # Initialize Gradient
        #(Q,N,K) = size(dμ_ij)
        Q = d.parLength[:All]
        Q_0 = Q - size(F_t,1)
        (N,K) = size(μ_ij)
        grad_obs = zeros(Q)
        ll_obs = 0.0

        ## Relevant Parameters for this observation
        #pars_relevant_2 = vcat(1:Q_0,Q_0+find(maximum(F_t,2)))
        pars_relevant = vcat(1:Q_0,Q_0+app._rel_fe_Dict[ind])

        #γlen = 1 + d.parLength[:γ]
        γlen = d.parLength[:γ]
        β0len = γlen + d.parLength[:β0]
        βlen = β0len + d.parLength[:γ]
        σlen = βlen + d.parLength[:σ]
        FElen = σlen + d.parLength[:FE]


        # Pre-Calculate Squares
        μ_ij_sums = 1.+μ_ij*δ
        μ_ij_sums_sq = (μ_ij_sums).^2

        # Pre-Calculate Log-Likelihood Terms for Gradient
        # Also Calculate Log-Likelihood itself
        gll_t1 = Vector{Float64}(K)
        gll_t2 = Vector{Float64}(K)
        for k in 1:K
            #Gradient Terms
            gll_t1[k] = wgt[k]*S_ij[k]*(1/s_hat[k])
            gll_t2[k] = wgt[k]*S_ij[k]*urate[k]*(1/(s_insured) + 1/(1-s_insured))

            # Log Likelihood
            ll_obs+=wgt[k]*S_ij[k]*(log(s_hat[k]) -
                            urate[k]*(log(s_insured)-log(1-s_insured)))
        end


        for q in pars_relevant
            #if q==1
            if q<0
                #Constant
                grad_obs[q] += par_gradient(1.0,μ_ij,δ,
                                        μ_ij_sums,μ_ij_sums_sq,
                                        gll_t1,gll_t2)
            elseif q<=γlen
                # Base Demographics
                # grad_obs[q] += par_gradient(Z[q-1],μ_ij,δ,
                #                         μ_ij_sums,μ_ij_sums_sq,
                #                         gll_t1,gll_t2)
                grad_obs[q] += par_gradient(Z[q],μ_ij,δ,
                                        μ_ij_sums,μ_ij_sums_sq,
                                        gll_t1,gll_t2)
            elseif q<=β0len
                # Base Characteristics
                grad_obs[q] += par_gradient(X_0_t[q-γlen,:],μ_ij,δ,
                                        μ_ij_sums,μ_ij_sums_sq,
                                        gll_t1,gll_t2)
            elseif q<=βlen
                # Characteristic Interactions
                grad_obs[q] += par_gradient(X_t[1,:]*Z[q-β0len],
                                        μ_ij,δ,
                                        μ_ij_sums,μ_ij_sums_sq,
                                        gll_t1,gll_t2)
            elseif q<=(βlen+1)
                #Insurance Random Effect
                grad_obs[q] += par_gradient_σ(draws[:,1],
                                        μ_ij,δ,
                                        μ_ij_sums,μ_ij_sums_sq,
                                        gll_t1,gll_t2)
            elseif q<=σlen
                #Quality Random Effect
                grad_obs[q] += par_gradient_σ(draws[:,2],X_t[1+q-(βlen+1),:],
                                        μ_ij,δ,
                                        μ_ij_sums,μ_ij_sums_sq,
                                        gll_t1,gll_t2)
            else
                #Fixed Effect
                fe_vec = F_t[q-σlen,:]
                grad_obs[q] += par_gradient(fe_vec,
                                        μ_ij,δ,
                                        μ_ij_sums,μ_ij_sums_sq,
                                        gll_t1,gll_t2)
            end
        end

    return ll_obs, grad_obs
end


##### Gradient Mini Functions #####


function par_gradient{T}(x::Float64,
                            μ_ij::Array{T,2},δ::Vector{T},
                            μ_ij_sums::Vector{T},μ_ij_sums_sq::Vector{T},
                            gll_t1::Vector{Float64},gll_t2::Vector{Float64})
    grad_obs = 0.0
    (N,K) = size(μ_ij)
    dμ_ij = Vector{Float64}(K)
    @inbounds(
    for n in 1:N
        dμ_ij_sums = 0.0
        for k in 1:K
            @fastmath dμ_ij[k] = μ_ij[n,k]*δ[k]*x
            @fastmath dμ_ij_sums+= dμ_ij[k]
        end
        grad_obs+= par_gradient_inner_loop(dμ_ij,dμ_ij_sums,
                                n,μ_ij,δ,
                                μ_ij_sums,μ_ij_sums_sq,
                                gll_t1,gll_t2)
    end
    )
    return grad_obs/N
end

function par_gradient{T}(x::Vector{Float64},
                            μ_ij::Array{T,2},δ::Vector{T},
                            μ_ij_sums::Vector{T},μ_ij_sums_sq::Vector{T},
                            gll_t1::Vector{Float64},gll_t2::Vector{Float64})
    grad_obs = 0.0
    (N,K) = size(μ_ij)
    dμ_ij = Vector{Float64}(K)
    @inbounds(
    for n in 1:N
        dμ_ij_sums = 0.0
        for k in 1:K
            @fastmath dμ_ij[k] = μ_ij[n,k]*δ[k]*x[k]
            @fastmath dμ_ij_sums+= dμ_ij[k]
        end
        grad_obs+= par_gradient_inner_loop(dμ_ij,dμ_ij_sums,
                                n,μ_ij,δ,
                                μ_ij_sums,μ_ij_sums_sq,
                                gll_t1,gll_t2)
    end
    )
    return grad_obs/N
end

function par_gradient_σ{T}(x::Vector{Float64},
                            μ_ij::Array{T,2},δ::Vector{T},
                            μ_ij_sums::Vector{T},μ_ij_sums_sq::Vector{T},
                            gll_t1::Vector{Float64},gll_t2::Vector{Float64})
    grad_obs = 0.0
    (N,K) = size(μ_ij)
    dμ_ij = Vector{Float64}(K)
    @inbounds(
    for n in 1:N
        dμ_ij_sums = 0.0
        for k in 1:K
            dμ_ij[k] = μ_ij[n,k]*δ[k]*x[n]
            dμ_ij_sums+= dμ_ij[k]
        end
        grad_obs+= par_gradient_inner_loop(dμ_ij,dμ_ij_sums,
                                n,μ_ij,δ,
                                μ_ij_sums,μ_ij_sums_sq,
                                gll_t1,gll_t2)
    end
    )
    return grad_obs/N
end
function par_gradient_σ{T}(x::Vector{Float64},Y::Vector{Float64},
                            μ_ij::Array{T,2},δ::Vector{T},
                            μ_ij_sums::Vector{T},μ_ij_sums_sq::Vector{T},
                            gll_t1::Vector{Float64},gll_t2::Vector{Float64})
    grad_obs = 0.0
    (N,K) = size(μ_ij)
    dμ_ij = Vector{Float64}(K)
    @inbounds(
    for n in 1:N
        dμ_ij_sums = 0.0
        for k in 1:K
            dμ_ij[k] = μ_ij[n,k]*δ[k]*Y[k]*x[n]
            dμ_ij_sums+= dμ_ij[k]
        end
        grad_obs+= par_gradient_inner_loop(dμ_ij,dμ_ij_sums,
                                n,μ_ij,δ,
                                μ_ij_sums,μ_ij_sums_sq,
                                gll_t1,gll_t2)
    end
    )
    return grad_obs/N
end

function par_gradient_inner_loop{T}(dμ_ij::Vector{Float64},dμ_ij_sums::Float64,
                            n::Int64,
                            μ_ij::Array{T,2},δ::Vector{T},
                            μ_ij_sums::Vector{T},μ_ij_sums_sq::Vector{T},
                            gll_t1::Vector{Float64},gll_t2::Vector{Float64})

    N,K = size(μ_ij)
    grad_obs = 0.0
    #dμ_ij_sums = sum(dμ_ij)
    @fastmath t2_0 = dμ_ij_sums/μ_ij_sums_sq[n]
    t3 = (dμ_ij_sums/μ_ij_sums_sq[n])
    @inbounds(
    for k in 1:K
        @fastmath t1= dμ_ij[k]/μ_ij_sums[n]
        @fastmath t2= t2_0*μ_ij[n,k]*δ[k]
        #t3= dμ_ij_sums/μ_ij_sums_sq[n]

        @fastmath grad_obs += gll_t1[k]*(t1 - t2) - gll_t2[k]*t3
    end
    )
    # t1 = @. dμ_ij/μ_ij_sums[n]
    # t2 = @. t2_0*μ_ij[n,:]*δ
    #
    # @inbounds @fastmath @simd for k in 1:K
    #     grad_obs += gll_t1[k]*(t1[k] - t2[k]) - gll_t2[k]*t3
    # end
    return grad_obs
end
