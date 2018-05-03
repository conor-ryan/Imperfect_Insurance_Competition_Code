import Base.getindex, Base.setindex!, Base.show
using NLopt
using ForwardDiff


########
# Parameter Structure
#########


type parDict{T}
    # Parameters
    #α::Vector{T}
    γ::Vector{T}
    β_0::Vector{T}
    β::Matrix{T}
    σ::Vector{T}
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
    αlen = 0
    γlen = αlen + m.parLength[:γ]
    β0len = γlen + m.parLength[:β]
    βlen = β0len + m.parLength[:β]*m.parLength[:γ]
    σlen = βlen + (m.parLength[:σ] -1) # No σ value for price

    #Distribute Parameters
    #α = x[1:αlen]
    # γ = x[(αlen+1):γlen]
    # β_0 = x[(γlen+1):β0len]
    #β_vec = x[12:18]
    # σ = x[βlen+1:σlen]

    γ = x[1:4]
    β_0 = [x[5]]
    β_vec = x[6:8]
    #β_vec = x[7:8]
    #σ = x[6:8]
    σ = x[9:11]

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
    β[1,1] = β_vec[1]
    β[1,3] = β_vec[2]
    β[1,4] = β_vec[3]
    # β[2,1] = β_vec[1]
    # β[3,1] = β_vec[2]
    # println(γ)
    # println(β)
    # println(β_0)

    #Calculate Random Coefficients matrix
    #Includes RC on Price, set to 0 atm
    (R,S) = size(m.draws)
    randCoeffs = Array{T,2}(m.parLength[:σ],S)
    calcRC!(randCoeffs,σ,m.draws)

    #Initialize (ij) pairs of deltas
    L, M = size(m.data.data)
    δ = Vector{T}(M)
    μ_ij = Matrix{T}(S,M)
    s_hat = Vector{T}(M)
    unpack_δ!(δ,m)

    return parDict{T}(γ,β_0,β,σ,randCoeffs,δ,μ_ij,s_hat)
end

function calcRC!{T,S}(randCoeffs::Array{S},σ::Array{T},draws::Array{Float64,2})
    (K, N) = size(randCoeffs)
    randCoeffs[1,:] = draws[1,:].*σ[1]
    #randCoeffs[1,:] = 0
    randCoeffs[2,:] = 0
    for k in 3:K,n in 1:N
        randCoeffs[k,n] = draws[2,n]*σ[k-1]
        #randCoeffs[k,n] = 0
    end
    return Void
end


###########
# Calculating Preferences
###########

function calc_indCoeffs{T}(p::parDict{T},β::Array{T,1},d::T)
    Q = length(β)
    (K,N) = size(p.randCoeffs)
    β_i = Array{T,2}(Q,N)
    γ_i = Array{T,1}(N)
    for n in 1:N
        γ_i[n] = d + p.randCoeffs[1,n]
        for k in 1:Q
            β_i[k,n] = β[k] + p.randCoeffs[k+1,n]
        end
    end
    return β_i, γ_i
end

function individual_values!{T}(d::InsuranceLogit,p::parDict{T})
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

    ind = person(app)[1]
    X = permutedims(prodchars(app),(2,1))
    X_0 = permutedims(prodchars0(app),(2,1))
    Z = demoRaw(app)[:,1]
    β_z = β*Z
    demos = vecdot(γ,Z)
    β_i, γ_i = calc_indCoeffs(p,β_z,demos)

    chars = X*β_i
    chars_0 = X_0*β_0

    (K,N) = size(chars)
    idxitr = app._personDict[ind]
    for k = 1:K,n = 1:N
        #u = exp(chars[k,n] + α*price[k] + γ_i[n])
        u = exp(chars[k,n] + chars_0[k] + γ_i[n])
        p.μ_ij[n,idxitr[k]] = u
    end
    if ret
        return p.μ_ij[:,idxitr]
    else
        return similar(chars)
    end
end

# function grad_test(x)
#     pars = parDict(m,x)
#     return util_value!(app,pars,true)
# end
#


function util_gradient{T}(d::InsuranceLogit,app::ChoiceData,p::parDict{T})
    γ = p.γ
    β_0= p.β_0
    β = p.β
    p_num = length(p.γ) + length(p.β_0) + length(p.β) + length(p.σ)

    ind = person(app)[1]
    X = permutedims(prodchars(app),(2,1))
    X_0 = permutedims(prodchars0(app),(2,1))
    Z = demoRaw(app)[:,1]
    β_z = β*Z
    demos = vecdot(γ,Z)
    β_i, γ_i = calc_indCoeffs(p,β_z,demos)

    chars = X*β_i
    chars_0 = X_0*β_0

    # Create Gradient Matrix
    (K,N) = size(chars)
    grad = Array{T,3}(p_num,N,K)

    # γ_derivs = ones(K,length(γ))
    # β_0_derivs = X_0
    #char_derivs = Array{T,2}(K,p_num-length(p.σ))
    char_derivs = Array{T,2}(K,p_num)
    for k in 1:K
        char_derivs[k,1:3] = Z
    end
    char_derivs[:,4:7] = X_0
    char_derivs[:,8:11] = Z[1].*X
    char_derivs[:,12:15] = Z[2].*X
    char_derivs[:,16:19] = Z[3].*X
    #char_derivs = [transpose(repeat(Z,outer=(1,K))) X_0 Z[1].*X Z[2].*X Z[3].*X]

    # X_large = repeat(X[:,2:4],outer=(1,1,N))
    # nu_h_large = repeat(m.draws[2,:],inner=(1,length(p.β_0)-1,K))
    # rand_derivs = [transpose(m.draws[1,:]) transpose[m.draws[2,:]]]
    #
    for k = 1:K,n = 1:N
        #u = exp(chars[k,n] + α*price[k] + γ_i[n])
        u = exp(chars[k,n] + chars_0[k] + γ_i[n])
        for q in 1:p_num
            if q<=19
                grad[q,n,k] = char_derivs[k,q]*u
            elseif q==20
                grad[q,n,k] = d.draws[1,n]*u
            else
                grad[q,n,k] = X[k,q-20+1]*d.draws[2,n]*u
            end
        end
    end

    return grad
end


function calc_shares{T}(μ_ij::Array{T},δ::Vector{T})
    (N,K) = size(μ_ij)
    util = Matrix{T}(K,N)
    s_hat = Matrix{T}(K,N)
    #s_mean = Vector{T}(K)
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
    # for i in 1:K
    #     s_mean[i] = mean(s_hat[i,:])
    # end
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
