import Base.getindex, Base.setindex!, Base.show, Base.mean
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

type InsuranceLogit <: LogitModel
    # Parameters
    parameters::Dict{Symbol, Any}
    parLength::Dict{Symbol, Int}
    #   Includes:
    #   γ   Vector governing demographic preferences for insurance
    #   β   Vector governing preferences over different risk characteristics
    #   ϕ   Vector governing risk sensativity, accounting for fixed effects
    #   α   Vector governing price sensitivity, accounting for fixed effects
    data::ChoiceData

    #Store Halton Draws
    draws::Array{Float64,2}
    randCoeffs::Array{Float64,2}

    # Matrix to store estimated values, μ_ij and predicted share
    # These variables are length of the entire data
    # All sorted by person, identical to data
    μ::Array{Float64,1}
    δ::Array{Float64,1}
    util::Array{Float64,1}
    s_hat::Array{Float64,1}

    # Product Level Data
    # Separate vectors, all sorted by product
    prods::Array{Int64,1}
    shares::Array{Float64,1}
    deltas::Array{Float64,1}
end


function InsuranceLogit(data::ChoiceData,haltonDim::Int)
    # Construct the model instance

    # Get Parameter Lengths
    γlen = size(demoRaw(data),1)
    βlen = size(prodchars(data),1)
    σlen = βlen

    parLength = Dict(:γ=>γlen,:β=>βlen,:σ=>σlen)
    parameters = Dict{Symbol, Array{Float64}}()

    # Initialize Halton Draws
    # These are the same across all individuals
    draws = permutedims(MVHaltonNormal(haltonDim,σlen),(2,1))
    randCoeffs = Array{Float64,2}(σlen,haltonDim)

    # Initialize Empty value prediction objects
    n, k = size(c.data)
    μ = Vector{Float64}(k)
    δ = Vector{Float64}(k)
    util = Vector{Float64}(k)
    s_hat = Vector{Float64}(k)

    # Copy Firm Level Data for Changing in Estimation
    pmat = c.pdata
    pmat[:delta] = 0.0
    sort!(pmat)

    d = InsuranceLogit(parameters,parLength,data,
                        draws,randCoeffs,
                        μ,δ,util,s_hat,
                        pmat[:Product],pmat[:Share],pmat[:delta])
    return d
end


pack(m::InsuranceLogit) = vcat(m[:γ],m[:β],m[:ϕ],m[:α])
function unpack!{T}(m::InsuranceLogit, x::Vector{T})
    γlen = m.parLength[:γ]
    βlen = γlen + m.parLength[:β]*m.parLength[:γ]
    σlen = βlen + m.parLength[:σ]

    m[:γ] = x[1:γlen]
    β_vec = x[γlen+1:βlen]
    m[:σ] = x[βlen+1:end]

    # Stack Beta into a matrix
    K = m.parLength[:β]
    N = m.parLength[:γ]
    β = Matrix{Float64}(K,N)
    ind = 0
    for i in 1:N, j in 1:K
        ind+=1
        β[j,i] = β_vec[ind]
    end
    m[:β] = β

    return Void
end

function calc_RC!(d::InsuranceLogit)
    σ = d[:σ]
    (K, N) = size(d.draws)
    for k in 1:K,n in 1:N
        d.randCoeffs[k,n] = d.draws[k,n]*σ[k]
    end
    return Void
end

function calc_β(d::InsuranceLogit,β::Array{Float64,1})
    (K,N) = size(d.randCoeffs)
    β_i = Matrix{Float64}(K,N)
    for k in 1:K,n in 1:N
        β_i[k,n] = β[k] + d.randCoeffs[k,n]
    end
    return β_i
end

function individual_values!(d::InsuranceLogit)
    # Store Parameters
    γ = d[:γ]
    β = d[:β]
    σ = d[:σ]

    # Calculate μ_ij, which depends only on parameters
    for app in eachperson(d.data)
        ind = person(app)[1]
        X = permutedims(prodchars(app),(2,1))
        Z = demoRaw(app)[:,1]

        β_z = β*Z
        β_i = calc_β(d,β_z)
        chars = X*β_i
        demos = vecdot(γ,Z)
        idxitr = d.data._personDict[ind]
        δ = d.δ[idxitr]
        d.s_hat[idxitr] = individual_shares_RC!(chars,demos,δ)
    end
    return Void
end

function individual_shares_RC!(chars,demos,δ)
    (K,N) = size(chars)
    μ = Matrix{Float64}(K,N)
    s_hat = Matrix{Float64}(K,N)
    for n in 1:N
        expsum = 0.0
        for i in 1:K
            a = exp(chars[i,n] + demos + δ[i])
            μ[i,n] = a
            expsum += a
        end
        for i in 1:K
            s_hat[i,n] = μ[i,n]/expsum
        end
    end
    s_mean = mean(s_hat,2)
    return s_mean
end


function unpack_δ!(d::InsuranceLogit)
    for j in d.prods
        idx_j = d.data._productDict[j]
        for idx in idx_j
            d.δ[idx] = d.deltas[j]
        end
    end
    return Void
end


# Update δ
@views sliceMean(x::Vector{Float64},idx::Array{Int64,1}) = mean(x[idx])
function δ_update!(d)
    # Calculate overall marketshares and update δ_j
    eps = 0.0
    for j in d.prods
        j_index_all = d.data._productDict[j]
        #s_hat_j= mean(d.s_hat[j_index_all])
        s_hat_j= sliceMean(d.s_hat,j_index_all)
        s_j = d.shares[j]
        chg = log(s_j) - log(s_hat_j)
        d.deltas[j] += chg
        # if abs(chg)>eps
        #     j_high = j
        # end
        eps = max(eps,abs(chg))
    end
    #println(eps)
    return eps
end

# Overwrite the Standard MLE Maximum Likelihood Computation
function evaluate_iteration(d::InsuranceLogit)
    calc_RC!(d)
    # Contraction...
    rnd = 0
    eps = 1
    while (eps>1e-6) & (rnd<1000)
        rnd+=1
        #Unpack δ_j into estimator data
        unpack_δ!(d)
        individual_values!(d)
        eps = δ_update!(d)
        println(eps)
    end
    println(rnd)
    ll=0
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
function evaluate_iteration!(d::InsuranceLogit, x)
    unpack!(d, x)

    return evaluate_iteration(d)
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
    opt = Opt(:LD_MMA, length(p0))
    #opt = Opt(:LN_NELDERMEAD, length(p0))
    xtol_rel!(opt, 1e-4)
    maxeval!(opt, 2000)

    # Objective Function
    ll(x) = evaluate_iteration!(d, x)
    count = 0
    function ll(x, grad)
    #     # Store Gradient
        ForwardDiff.gradient!(grad, ll, x)
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
    return d
end
