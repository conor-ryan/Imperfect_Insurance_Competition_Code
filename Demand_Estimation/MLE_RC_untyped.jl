import Base.getindex, Base.setindex!, Base.show
using NLopt
using ForwardDiff

abstract type LogitModel end

# Indexing just looks up parameters
getindex(m::LogitModel, key) = m.parameters[key]
getindex(m::LogitModel, keys::AbstractArray) = [m[key] for key in keys]

function setindex!(m::LogitModel, val, key)
    m.parameters[key] = val
    return m
end

type parDict{T}
    # Parameters
    α::Vector{T}
    γ::Vector{T}
    β::Matrix{T}
    σ::Vector{T}
    randCoeffs::Array{T,2}
    δ::Vector{T}
    #δ_dev::Vector{T}
end
# Indexing just looks up parameters
# getindex{T}(p::parDict{T}, key) = p.parameters[key]
# getindex{T}(p::parDict{T}, keys::AbstractArray) = [m[key] for key in keys]


type InsuranceLogit <: LogitModel
    parLength::Dict{Symbol, Int}
    #   Includes:
    #   γ   Vector governing demographic preferences for insurance
    #   β   Vector governing preferences over different risk characteristics
    #   ϕ   Vector governing risk sensativity, accounting for fixed effects
    #   α   Vector governing price sensitivity, accounting for fixed effects
    data::ChoiceData

    #Store Halton Draws
    draws
    # Matrix to store estimated values, μ_ij and predicted share
    # These variables are length of the entire data
    # All sorted by person, identical to data
    δ::Array{Float64,1}
    s_hat

    # Product Level Data
    # Separate vectors, all sorted by product
    prods
    shares
    deltas
end

function parDict{T}(m::InsuranceLogit,x::Array{T})
    αlen = 1
    γlen = αlen + m.parLength[:γ]
    βlen = γlen + m.parLength[:β]*m.parLength[:γ]
    σlen = βlen + m.parLength[:σ]

    α = x[1:αlen]
    γ = x[(αlen+1):γlen]
    β_vec = x[(γlen+1):βlen]
    σ = x[βlen+1:σlen]

    # Stack Beta into a matrix
    K = m.parLength[:β]
    N = m.parLength[:γ]
    β = Matrix{T}(K,N)
    ind = 0
    for i in 1:N, j in 1:K
        ind+=1
        β[j,i] = β_vec[ind]
    end

    (R,S) = size(m.draws)
    randCoeffs = Array{T,2}(m.parLength[:σ],S)

    L, M = size(m.data.data)
    δ = Vector{T}(M)

    return parDict{T}(α,γ,β,σ,randCoeffs,δ)
end

function InsuranceLogit(data::ChoiceData,haltonDim::Int)
    # Construct the model instance

    # Get Parameter Lengths
    γlen = size(demoRaw(data),1)
    βlen = size(prodchars(data),1)
    σlen = βlen+1

    parLength = Dict(:γ=>γlen,:β=>βlen,:σ=>σlen)
    #parameters = Dict{Symbol, Array{Any}}()

    # Initialize Halton Draws
    # These are the same across all individuals
    draws = permutedims(MVHaltonNormal(haltonDim,3),(2,1))

    # Initialize Empty value prediction objects
    n, k = size(c.data)
    δ = Vector{Float64}(k)
    s_hat = Vector{Real}(k)

    # Copy Firm Level Data for Changing in Estimation
    pmat = c.pdata
    pmat[:delta] = 0.0
    sort!(pmat)

    d = InsuranceLogit(parLength,data,
                        draws,
                        δ,s_hat,
                        pmat[:Product],pmat[:Share],pmat[:delta])
    return d
end

function calc_RC!{T}(d::InsuranceLogit,p::parDict{T})
    σ = p.σ
    (K, N) = size(p.randCoeffs)
    for k in 1:K,n in 1:N
        l = min(k,3)
        p.randCoeffs[k,n] = d.draws[l,n]*σ[k]
    end
    return Void
end

function calc_indCoeffs{T}(p::parDict{T},β::Array{T,1},γ)
    Q = length(β)
    (K,N) = size(p.randCoeffs)
    β_i = Array{T,2}(Q,N)
    γ_i = Array{T,1}(N)
    for n in 1:N
        γ_i[n] = γ + p.randCoeffs[1,n]
        for k in 1:Q
            β_i[k,n] = β[k] + p.randCoeffs[k+1,n]
        end
    end
    return β_i, γ_i
end

function individual_values!{T}(d::InsuranceLogit,p::parDict{T};init::Bool=false)
    # Store Parameters
    γ = p.γ
    β = p.β
    α = p.α[1]
    if init
        δ_long = d.δ
    else
        δ_long = p.δ
    end
    # Calculate μ_ij, which depends only on parameters
    for app in eachperson(d.data)
        ind = person(app)[1]
        X = permutedims(prodchars(app),(2,1))
        price = X[:,1]
        Z = demoRaw(app)[:,1]
        β_z = β*Z
        demos = vecdot(γ,Z)
        β_i, γ_i = calc_indCoeffs(p,β_z,demos)
        chars = X*β_i

        (K,N) = size(chars)
        μ_ij = chars
        for n = 1:N,k = 1:K
            μ_ij[k,n] += α*price[k] + γ_i[n]
        end
        idxitr = d.data._personDict[ind]
        δ = δ_long[idxitr]
        d.s_hat[idxitr] = individual_shares_RC(μ_ij,δ)
    end
    return Void
end



function individual_shares_RC{T}(μ_ij::Array{T},δ;inside::Bool=false)
    (K,N) = size(μ_ij)
    util = Matrix{T}(K,N)
    s_hat = Matrix{T}(K,N)
    s_mean = Vector{T}(K)
    out = 1.0
    if inside
        out = 0.0
    end
    for n in 1:N
        expsum = out
        for i in 1:K
            a = exp(μ_ij[i,n] + δ[i])
            util[i,n] = a
            expsum += a
        end
        for i in 1:K
            s_hat[i,n] = util[i,n]/expsum
        end
    end
    #s_mean = mean(s_hat,2)
    for i in 1:K
        s_mean[i] = mean(s_hat[i,:])
    end
    return s_mean
end

# Calculate Log Likelihood
function log_likelihood{T}(d::InsuranceLogit,p::parDict{T})
    ll = 0.0
    γ = p.γ
    β = p.β
    α = p.α[1]
    # Calculate μ_ij, which depends only on parameters
    for app in eachperson(d.data)
        ind = person(app)[1]
        X = permutedims(prodchars(app),(2,1))
        price = X[:,1]
        Z = demoRaw(app)[:,1]
        Y = transpose(choice(app))
        urate = transpose(unins(app))

        β_z = β*Z
        demos = vecdot(γ,Z)
        β_i, γ_i = calc_indCoeffs(p,β_z,demos)
        chars = X*β_i

        (K,N) = size(chars)
        μ_ij = similar(chars)
        for n = 1:N,k = 1:K
            μ_ij[k,n] = chars[k,n] + α*price[k] + γ_i[n]
        end

        idxitr = d.data._personDict[ind]
        δ = p.δ[idxitr]
        s_hat = individual_shares_RC(μ_ij,δ;inside=false)
        s_insured = sum(s_hat)

        for i in eachindex(idxitr)
            ll+=Y[i]*(log(s_hat[i])-urate[i]*log(s_insured))
        end
    end
    return ll
end


function unpack_δ!{T}(d::InsuranceLogit,p::parDict{T})
    for j in d.prods
        idx_j = d.data._productDict[j]
        for idx in idx_j
            p.δ[idx] = d.deltas[j]
        end
    end
    return Void
end

function reset_δ!(d::InsuranceLogit)
    J = length(d.deltas)
    deltas_new = Array{Float64}(J)
    for j in d.prods
        deltas_new[j] = ForwardDiff.value(d.deltas[j])
    end
    d.deltas = deltas_new

    for j in d.prods
        idx_j = d.data._productDict[j]
        for idx in idx_j
            d.δ[idx] = d.deltas[j]
        end
    end
    return Void
end


# Update δ
@views sliceMean{T}(x::Vector{T},idx::Array{Int64,1}) = mean(x[idx])
function δ_update!{T}(d,p::parDict{T})
    # Calculate overall marketshares and update δ_j
    eps = 0.0
    J = length(d.deltas)
    δ_new = Array{T,1}(J)
    for j in d.prods
        j_index_all = d.data._productDict[j]
        #s_hat_j= mean(d.s_hat[j_index_all])
        s_hat_j= sliceMean(d.s_hat,j_index_all)
        s_j = d.shares[j]
        chg = log(s_j) - log(s_hat_j)
        #d.deltas[j] += chg
        δ_new[j] = d.deltas[j] + chg
        # if abs(chg)>eps
        #     j_high = j
        # end
        eps = max(eps,abs(chg))
    end
    d.deltas = δ_new
    return eps
end

function contraction!{T}(d::InsuranceLogit,p::parDict{T})
    calc_RC!(d,p)
    reset_δ!(d)
    # Contraction...
    rnd = 0
    eps = 1
    tol = 1e-10
    while (eps>tol) & (rnd<1000)
        rnd+=1
        init = rnd==1
        #Unpack δ_j into estimator data
        individual_values!(d,p;init=init)
        eps = δ_update!(d,p)
        unpack_δ!(d,p)
        println("Contraction Error")
        println(eps)
    end
end

function contraction!{T}(d::InsuranceLogit,p_vec::Array{T,1})
    p = parDict(d,p_vec)
    calc_RC!(d,p)
    reset_δ!(d)
    # Contraction...
    rnd = 0
    eps = 1
    tol = 1e-10
    while (eps>tol) & (rnd<1000)
        rnd+=1
        #Unpack δ_j into estimator data
        individual_values!(d,p,rnd)
        eps = δ_update!(d,p)
        unpack_δ!(d,p)
        println("Contraction Error")
        println(eps)
    end
    println("Contraction Rounds")
    println(rnd)
end

# Overwrite the Standard MLE Maximum Likelihood Computation
function evaluate_iteration{T}(d::InsuranceLogit,p::parDict{T})
    contraction!(d,p)
    ll = log_likelihood(d,p)
    return ll
end

"""
```
log_likelihood!(model::BurdettMortensen, x::Vector)
```

Unpacks the parameters stored in ``x`` and updates the model to use
them.  The function then calculates and returns the full log_likelihood
of the updated model.
"""
function evaluate_iteration!{T}(d::InsuranceLogit, x::Array{T,1})
    # Create Parameter Types
    parameters = parDict(d,x)
    return evaluate_iteration(d,parameters)
end


"""
```
estimate(m::MaximumLikelihood, p0)
```

Computes the maximum likelihood estimate of the parameters of the model
by maximizing the log-likelihood function, using Nelder-Mead, starting
at initial value p0.

Modifies the model object inplace, and returns it.
"""
function estimate!(d::InsuranceLogit, p0)
    # Set up the optimization
    #opt = Opt(:LD_MMA, length(p0))
    opt = Opt(:LN_NELDERMEAD, length(p0))
    #opt = Opt(:LN_SBPLX, length(p0))
    xtol_rel!(opt, 1e-4)
    maxeval!(opt, 2000)
    #upper_bounds!(opt, ones(length(p0))/10)
    initial_step!(opt,5e-3)

    # Objective Function
    ll(x) = evaluate_iteration!(d, x)
    # δ_cont(x) = contraction!(d,x)
    count = 0
    function ll(x, grad)
        # Store Gradient
        # println("Step 1")
        # δ_cont(x)
        # println("Step 2")
        # ForwardDiff.gradient!(grad, ll, x)
        count +=1
        likelihood = ll(x)
        println("likelihood equals $likelihood at $x on iteration $count")
        return likelihood
    end

    # Set Objective
    max_objective!(opt, ll)

    # Run Optimization
    minf, minx, ret = optimize(opt, p0)
    println("got $minf at $minx after $count iterations (returned $ret)")
    # Set the parameters
    evaluate_iteration!(d, minx)

    # Return the object
    return ret
end
