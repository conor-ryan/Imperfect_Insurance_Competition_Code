import Base.getindex, Base.setindex!, Base.show
# using NLopt
using ForwardDiff


########
# Parameter Structure
#########


mutable struct parDict{T}
    # Parameters
    γ_0::T
    γ::Vector{T}
    β_0::Vector{T}
    β::Matrix{T}
    σ::Vector{T}
    FE::Matrix{T}
    #Random Coefficients stored (function of σ and draws)
    randCoeffs::Array{T,3}
    # δ values for (ij) pairs
    δ::Vector{T}
    # Non-delta utility for (ij) pairs and draws
    μ_ij::Matrix{T}
    # Shares for (ij) pairs
    s_hat::Vector{T}
    r_hat::Vector{T}
    # Share Parameter Derivatives for (ij) pairs x Parameters
    dSdθ::Matrix{T}
    dRdθ::Matrix{T}
    # Share Parameter Derivatives for Products x Parameters
    dSdθ_j::Matrix{T}
    dRdθ_j::Matrix{T}

    d2Sdθ_j::Array{T,3}
    d2Rdθ_j::Array{T,3}
end

function parDict(m::InsuranceLogit,x::Array{T}) where T
    # Parameter Lengths from model
    #γlen = 1 + m.parLength[:γ]
    γlen = m.parLength[:γ]
    β0len = γlen + m.parLength[:β]
    βlen = β0len + m.parLength[:γ]
    σlen = βlen  + m.parLength[:σ]
    FElen = σlen + m.parLength[:FE]

    # Distribute Parameters
    # γ_0 = x[1]
    # γ = x[2:γlen]
    γ_0 = 0.0
    γ = x[1:γlen]
    β_0 = x[(γlen+1):β0len]
    β_vec = x[(β0len+1):βlen]
    σ_vec = x[(βlen+1):σlen]
    FE_vec = x[(σlen+1):FElen]

    # Store FE as row Vector
    FE = Matrix{T}(undef,1,length(FE_vec))
    FE[1,:] = FE_vec

    # Fill in σ
    σ = Vector{T}(undef,m.parLength[:β])
    σ[:] .= 0.0
    σ[m.data._randCoeffs] = σ_vec

    # Stack Beta into a matrix
    K = m.parLength[:β]
    N = m.parLength[:γ]
    β = Matrix{T}(undef,K,N)


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
    randCoeffs = Array{T,3}(undef,S,m.parLength[:β],size(m.data.rMoments,1))
    calcRC!(randCoeffs,σ,m.draws)

    #Initialize (ij) pairs of deltas
    L, M = size(m.data.data)
    μ_ij = Matrix{T}(undef,S,M)
    s_hat = Vector{T}(undef,M)
    r_hat = Vector{T}(undef,M)

    # Deltas are turned off
    δ = Vector{T}(undef,M)
    unpack_δ!(δ,m)
    #δ = ones(M)

    Q = m.parLength[:All]
    dSdθ = Matrix{T}(undef,Q,M)
    dRdθ = Matrix{T}(undef,Q,M)
    J = length(m.prods)
    dSdθ_j = Matrix{T}(undef,Q,J)
    dRdθ_j = Matrix{T}(undef,Q,J)
    # d2Sdθ_j = Array{T,3}(undef,Q,Q,J)
    # d2Rdθ_j = Array{T,3}(undef,Q,Q,J)
    d2Sdθ_j = Array{T,3}(undef,1,1,J)
    d2Rdθ_j = Array{T,3}(undef,1,1,J)

    return parDict{T}(γ_0,γ,β_0,β,σ,FE,randCoeffs,δ,μ_ij,s_hat,r_hat,
                            dSdθ,dRdθ,dSdθ_j,dRdθ_j,d2Sdθ_j,d2Rdθ_j)
end

function calcRC!(randCoeffs::Array{S},σ::Array{T},draws::Array{Float64,2}) where {T,S}
    (N,K,R) = size(randCoeffs)
    #randCoeffs[:,1] = draws[:,1].*σ[1]
    #Skip Price Coefficient
    for k in 1:K,n in 1:N,r in 1:R
        randCoeffs[n,k,r] = draws[n,r]*σ[k]
    end
    return Nothing
end


###########
# Calculating Preferences
###########

# function calc_indCoeffs{T}(p::parDict{T},β::Array{T,1},d::T)
#     Q = length(β)
#     (N,K) = size(p.randCoeffs)
#     β_i = Array{T,2}(N,Q)
#     γ_i = d
#     β_i[:,1] = β[1]
#
#     for k in 2:Q, n in 1:N
#         β_i[n,k] = β[k] + p.randCoeffs[n,k-1]
#     end
#
#     β_i = permutedims(β_i,(2,1))
#     return β_i, γ_i
# end

function calc_indCoeffs(p::parDict{T},β::Array{T,1},d::T,r_ind::Int) where T
    Q = length(β)
    (N,K) = size(p.randCoeffs)
    β_i = Array{T,2}(undef,N,Q)
    γ_i = d
    for n in 1:N
        β_i[n,1] = β[1]
    end

    for k in 1:K, n in 1:N
        β_i[n,k] = β[k] + p.randCoeffs[n,k,r_ind]
    end

    β_i = permutedims(β_i,(2,1))
    return β_i, γ_i
end

function individual_values!(d::InsuranceLogit,p::parDict{T}) where T
    # Calculate μ_ij, which depends only on parameters
    for app in eachperson(d.data)
        util_value!(app,p)
    end
    return Nothing
end

function util_value!(app::ChoiceData,p::parDict{T}) where T
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
    β_i, γ_i = calc_indCoeffs(p,β_z,demos,r_ind)

    chars = X*β_i
    chars_0 = X*β_0

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
        @fastmath u = exp(chars[k,n] + chars_0[k] + controls[k] + γ_i)
        p.μ_ij[n,idxitr[k]] = u
    end

    return Nothing
end

function calc_shares(μ_ij::Array{T},δ::Vector{T},r::Matrix{Float64},r_age::Vector{Float64}) where T
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
            s_hat[i,n] = s
            r_hat[i,n] = s*(r[n,i] + r_age[i])
        end
    end
    s_mean = mean(s_hat,dims=2)
    r_mean = sum(r_hat,dims=2)./sum(s_hat,dims=2)
    return s_mean, r_mean
end

function calc_shares(μ_ij::Array{T},δ::Vector{T}) where T
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
            s_hat[i,n] = util[i,n]/expsum
        end
    end
    s_mean = mean(s_hat,dims=2)
    return s_mean
end

function individual_shares(d::InsuranceLogit,p::parDict{T}) where T
    # Store Parameters
    δ_long = p.δ
    μ_ij_large = p.μ_ij
    risk_long = rInd(d.data)
    ageHCC_long = ageHCC(d.data)
    for idxitr in values(d.data._personDict)
        δ = δ_long[idxitr]
        u = μ_ij_large[:,idxitr]
        r_ind = Int.(risk_long[idxitr])
        r_scores = d.draws[:,r_ind]
        r_age_scores = ageHCC_long[idxitr]
        s,r = calc_shares(u,δ,r_scores,r_age_scores)
        p.s_hat[idxitr] = s
        p.r_hat[idxitr] = r
    end
    return Nothing
end

function individual_shares_norisk(d::InsuranceLogit,p::parDict{T}) where T
    # Store Parameters
    δ_long = p.δ
    μ_ij_large = p.μ_ij
    for idxitr in values(d.data._personDict)
        δ = δ_long[idxitr]
        u = μ_ij_large[:,idxitr]
        s = calc_shares(u,δ)
        p.s_hat[idxitr] = s
    end
    return Nothing
end
