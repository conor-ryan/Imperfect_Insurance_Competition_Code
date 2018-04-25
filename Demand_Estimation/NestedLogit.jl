import Base.getindex, Base.setindex!, Base.show
using NLopt
using ForwardDiff


########
# Parameter Structure
#########


type parDict{T}
    # Parameters
    γ::Vector{T}
    β_0::Vector{T}
    β::Matrix{T}
    σ::T
    # δ values for (ij) pairs
    δ::Vector{T}
    # Non-delta utility for (ij) pairs and draws
    μ_ij::Vector{T}
    # Shares for (ij) pairs
    s_hat::Vector{T}
end

function parDict{T}(m::InsuranceLogit,x::Array{T})
    # Parameter Lengths from model
    αlen = 0
    γlen = αlen + m.parLength[:γ]
    β0len = γlen + m.parLength[:β]
    βlen = β0len + m.parLength[:β]*m.parLength[:γ]
    σlen = βlen +  1

    #Distribute Parameters
    #α = x[1:αlen]
    # γ = x[(αlen+1):γlen]
    # β_0 = x[(γlen+1):β0len]
    #β_vec = x[12:18]
    # σ = x[βlen+1:σlen]

    γ = x[1:3]
    β_0 = x[4:6]
    β_vec = x[7]
    σ = x[8]

    # Stack Beta into a matrix
    K = m.parLength[:β]
    N = m.parLength[:γ]
    β = Matrix{T}(K,N)
    ind = 0
    for i in 1:N, j in 1:K
        ind+=1
        #β[j,i] = β_vec[ind]
        β[j,i] = 0
        # if j==2 & i==1
        #     println(β)
        #     β[j,i] =  β_vec[1]
        # elseif j==3 & i==1
        #     println(β)
        #     β[j,i] =  β_vec[2]
        # else
        #     β[j,i] = 0
        # end
    end
    β[1,3] = β_vec[1]
    # β[2,1] = β_vec[1]
    # β[3,1] = β_vec[2]
    # println(γ)
    # println(β)
    # println(β_0)

    #Initialize (ij) pairs of deltas
    L, M = size(m.data.data)
    δ = Vector{T}(M)
    μ_ij = Vector{T}(M)
    s_hat = Vector{T}(M)
    unpack_δ!(δ,m)

    return parDict{T}(γ,β_0,β,σ,δ,μ_ij,s_hat)
end

###########
# Calculating Preferences
###########

function individual_values!{T}(d::InsuranceLogit,p::parDict{T})
    # Store Parameters
    # Calculate μ_ij, which depends only on parameters
    for app in eachperson(d.data)
        burn = util_value!(app,p,false)
    end
    return Void
end

function util_value!{T}(app::ChoiceData,p::parDict{T},ret=false)
    γ = p.γ
    β_0= p.β_0
    β = p.β
    σ = p.σ

    ind = person(app)[1]
    X = permutedims(prodchars(app),(2,1))
    X_0 = permutedims(prodchars0(app),(2,1))
    Z = demoRaw(app)[:,1]
    β_z = β*Z
    demos = vecdot(γ,Z)

    chars = X*β_z
    chars_0 = X_0*β_0

    K= length(chars)
    idxitr = app._personDict[ind]
    for k = 1:K
        u = exp((chars[k] + chars_0[k] + demos)/(1-σ))
        p.μ_ij[idxitr[k]] = u
    end
    if ret
        return p.μ_ij[idxitr]
    else
        return similar(chars)
    end
end


function calc_shares{T}(μ_ij::Vector{T},δ::Vector{T},σ::T)
    K = length(μ_ij)
    util = Vector{T}(K)
    s_hat = Vector{T}(K)
    #s_mean = Vector{T}(K)
    expsum = 0.0
    for i in 1:K
        a = μ_ij[i]*δ[i]
        util[i] = a
        expsum += a
    end
    for i in 1:K
        s_hat[i] = (util[i]*expsum^(-σ))/(1+expsum^(1-σ))
    end

    return s_hat
end

function individual_shares{T}(d::InsuranceLogit,p::parDict{T})
    # Store Parameters
    δ_long = p.δ
    σ = p.σ
    μ_ij_large = p.μ_ij
    for idxitr in values(d.data._personDict)
        δ = δ_long[idxitr]
        u = μ_ij_large[idxitr]
        s = calc_shares(u,δ,σ)
        p.s_hat[idxitr] = s
    end
    return Void
end
