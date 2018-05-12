########
# Parameter Structure
#########


type parDict{T}
    # Parameters
    γ_0::T
    γ::Vector{T}
    β_0::Vector{T}
    β::Matrix{T}
    FE::Vector{T}
    # δ values for (ij) pairs
    δ::Vector{T}
    # Non-delta utility for (ij) pairs and draws
    μ_ij::Vector{T}
    # Shares for (ij) pairs
    s_hat::Vector{T}
end

function parDict{T}(m::InsuranceLogit,x::Array{T})
    # Parameter Lengths from model
    γlen = 1 + m.parLength[:γ]
    β0len = γlen + m.parLength[:β]
    βlen = β0len + m.parLength[:γ]
    FElen = βlen + m.parLength[:FE]

    #Distribute Parameters
    γ_0 = x[1]
    γ = x[2:γlen]
    β_0 = x[(γlen+1):β0len]
    β_vec = x[(β0len+1):βlen]
    FE = x[(βlen+1):FElen]

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

    #Initialize (ij) pairs of deltas
    L, M = size(m.data.data)
    δ = Vector{T}(M)
    μ_ij = Vector{T}(M)
    s_hat = Vector{T}(M)
    unpack_δ!(δ,m)

    return parDict{T}(γ_0,γ,β_0,β,FE,δ,μ_ij,s_hat)
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
    γ_0 = p.γ_0
    γ = p.γ
    β_0= p.β_0
    β = p.β
    fe = p.FE

    ind = person(app)[1]
    X = permutedims(prodchars(app),(2,1))
    Z = demoRaw(app)[:,1]
    F = permutedims(fixedEffects(app),(2,1))
    β_z = β*Z
    demos =γ_0 + vecdot(γ,Z)

    chars = X*β_z
    chars_0 = X*β_0

    controls = F*fe

    K= length(chars)
    idxitr = app._personDict[ind]
    for k = 1:K
        u = exp(chars[k] + chars_0[k] + controls[k] + demos)
        p.μ_ij[idxitr[k]] = u
    end
    if ret
        return p.μ_ij[idxitr]
    else
        return similar(chars)
    end
end


function calc_shares{T}(μ_ij::Vector{T},δ::Vector{T})
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
        s_hat[i] = (util[i])/(1+expsum)
    end

    return s_hat
end

function individual_shares{T}(d::InsuranceLogit,p::parDict{T})
    # Store Parameters
    δ_long = p.δ
    μ_ij_large = p.μ_ij
    for idxitr in values(d.data._personDict)
        δ = δ_long[idxitr]
        u = μ_ij_large[idxitr]
        s = calc_shares(u,δ)
        p.s_hat[idxitr] = s
    end
    return Void
end



function util_gradient{T}(d::InsuranceLogit,app::ChoiceData,p::parDict{T})
    γ_0 = p.γ_0
    γ = p.γ
    β_0= p.β_0
    β = p.β
    fe = p.FE

    γlen = 1 + d.parLength[:γ]
    β0len = γlen + d.parLength[:β]
    βlen = β0len + d.parLength[:γ]
    FElen = βlen + d.parLength[:FE]

    p_num = d.parLength[:All]

    ind = person(app)[1]
    X_t = prodchars(app)
    X = permutedims(X_t,(2,1))
    Z = demoRaw(app)[:,1]
    F_t = fixedEffects(app)
    F = permutedims(F_t,(2,1))

    β_z = β*Z
    demos =γ_0 + vecdot(γ,Z)

    chars = X*β_z
    chars_0 = X*β_0

    controls = F*fe

    # Create Gradient Matrix
    K = length(chars)
    grad = Array{T,2}(p_num,K)

    char_deriv = Matrix{T}(p_num,K)
    char_deriv[1,:] = ones(K)
    #char_deriv[2:γlen,:] = repeat(Z,outer=(1,K))
    char_deriv[(γlen+1):β0len,:] = X_t
    #char_deriv[(β0len+1):βlen,:] = permutedims(repeat(X[:,1],outer=(1,length(Z))),(2,1)).*repeat(Z,outer=(1,K))
    for k in 1:K, b in 1:length(Z)
        char_deriv[1+b,k] = Z[b]
        char_deriv[β0len + b,k] = X[k,1]*Z[b]
    end
    char_deriv[(βlen+1):FElen,:] = F_t


    for k = 1:K
        u = exp(chars[k] + chars_0[k] + controls[k] + demos)
        for q in 1:p_num
            grad[q,k] = char_deriv[q,k]*u
        end
    end

    return grad
end



# Calculate Log Likelihood
function ll_gradient!{T}(grad::Vector{Float64},d::InsuranceLogit,p::parDict{T})
    p_num = d.parLength[:All]
    Pop =sum(weight(d.data).*choice(d.data))
    #Pop =0.0
    # Initialize Gradient
    grad[:] = 0

    # Calculate μ_ij, which depends only on parameters
    for app in eachperson(d.data)
        μ_ij = util_value!(app,p,true)
        dμ_ij = util_gradient(d,app,p)
        ind = person(app)[1]
        S_ij = transpose(choice(app))
        wgt = transpose(weight(app))
        urate = transpose(unins(app))
        idxitr = d.data._personDict[ind]


        # dμ_ij_sums = sum(dμ_ij,(1,3))


        δ = p.δ[idxitr]
        s_hat = calc_shares(μ_ij,δ)
        s_insured = sum(s_hat)



        #s2 = fill(0.0,K)
        (Q,K) = size(dμ_ij)

        for k = 1:K
            dμ_ij[:,k] = dμ_ij[:,k].*δ[k]
        end

        μ_ij_sums = 1+vecdot(μ_ij,δ)
        μ_ij_sums_sq = (1+vecdot(μ_ij,δ))^2
        dμ_ij_sums = sum(dμ_ij,2)

        for k = 1:K,q in 1:Q

            t1 = dμ_ij[q,k]/μ_ij_sums
            t2 = dμ_ij_sums[q]*μ_ij[k]*δ[k]/μ_ij_sums_sq
            t3 = dμ_ij_sums[q]/μ_ij_sums_sq

            grad[q] += wgt[k]*S_ij[k]*( (1/s_hat[k])*(t1 - t2) -
                urate[k]*( t3 )*(1/(s_insured) + 1/(1-s_insured) ) )/Pop
        end
        #Pop+= sum(wgt.*S_ij)
    end
    #grad = grad./Pop
    return grad
    # return fval/Pop
end
