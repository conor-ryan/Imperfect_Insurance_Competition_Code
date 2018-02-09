include("Models.jl")
include("CarData.jl")
include("Halton.jl")
include("NonlinearGMM.jl")
include("LinearGMM.jl")

using StatsFuns

########################################################################
#################### Basic Model Constructors ##########################
########################################################################

type BLP <: Model
    parameters::Dict{Symbol, Array}
    data::ModelData
    ndraws::Int
    _draws
    _δ
    _linear_gmm
    BLP(p,d,n) = new(p,d,n)
end

function BLP(data::ModelData; ndraws=5000)

    parameters = Dict{Symbol, Array}()

    b = BLP(parameters, data, ndraws)

    # Initialize a guess for δ0
    guess_delta!(b)

    # Initial Guess for the Parameters
    k = nobservables(b)
    unpack!(b, vcat(zeros(k), ones(k)))

    # Precompute draws for the simulation
    draws!(b)

    return b
end

nobservables(m::BLP) = size(observables(m.data), 2)
ninstruments(m::BLP) = size(instruments(m.data), 2)
nparams(m::BLP) = 2*nobservables(m)
nmoments(m::BLP) = size(observables(m.data), 1)

markets(m::BLP) = markets(m.data)


# Lower and Upper Bounds on the variance parameters
upper(m::BLP) = Inf*ones(nparams(m)/2)
lower(m::BLP) = zeros(nparams(m)/2)

# Compute Residuals
res(m::BLP, θ) = compute_ξ!(m, θ)

# Quickly Construct Subsets of the Data
function subset(m::BLP, idx)
    submod = BLP(m.parameters, subset(m.data, idx), m.ndraws)
    submod._draws = m._draws
    if isdefined(m, :_δ)
        submod._δ = m._δ[idx]
    end
    return submod
end

function unpack(m::BLP, x)

    l = length(x)
    @assert(mod(l, 2) == 0, "Need an Even number of Parameters")
    k = Int(l/2)

    β = x[1:k]
    σ = x[k+1:end]

    return β, σ
end

"""
Given a vector of parameters, unpacks them into the model
"""
function unpack!(m::BLP, x)

    β, σ = unpack(m,x)

    m[:β] = β
    m[:σ] = σ
    return m
end

pack(m::BLP) = vcat(m[:β], m[:σ])

########################################################################
#################### Compute Shares ####################################
########################################################################

"""
```
draws!(model)
```
Precompute random normal draws for integral simulation using scrambled
Halton sequences
"""
function draws!(m::BLP)
    S = MVHalton(m.ndraws, nobservables(m))'
    m._draws = norminvcdf.(S)
    return m
end


"""
```
s = shares(model, δ)
```
Computes the predicted shares of the BLP model integrating out over the
random coefficients by simulation.

Note: This should only be applied to a submarket, since it assumes that
all the observations are drawn from a single market.
"""
function shares{T}(m::BLP, δ::Array{T})

    ns = m.ndraws

    X = observables(m.data)::Array{T}
    σ = Diagonal(m[:σ]::Vector{T})

    s = zeros(T, size(δ))

    # Simulate the shares
    for i=1:ns
        # Get the random draws for this population member
        v = m._draws[:,i]

        # Compute that individual's idiosyncratic preferences
        μ = X*σ*v

        # Combine that with the average utility for each product
        g = exp.(δ + μ)

        # Compute the logit shares and combine their contribution to the
        # integral
        s += g/(1+sum(g))
    end

    return s./ns
end

"""
```
s = shares(model, δ, μ)
```
Computes the predicted shares of the BLP model integrating out over the
random coefficients by simulation.

Note: This should only be applied to a submarket, since it assumes that
all the observations are drawn from a single market.
"""
function shares{R,T}(m::BLP, δ::Array{R}, μ::Array{T})
    g = exp.(δ .+ μ)
    s = g./(1 .+ sum(g, 1))
    return mean(s, 2)

end
########################################################################
#################### Iterating over Markets ############################
########################################################################

# Construct an iterator to loop over the markets
function eachmarket(m::BLP)
    marks = sort(unique(markets(m)))
    return MarketIteratorBLP(m, marks)
end

# Define an Iterator Type
type MarketIteratorBLP
    data
    markets
end

# Make it actually iterable
start(itr::MarketIteratorBLP) = 1
function next(itr::MarketIteratorBLP, state)
    # Get the current market
    market = itr.markets[state]

    # Find which indices to use
    data = itr.data
    idx = find( markets(data).== market)

    # Subset the data to just look at the current market
    submod = subset(data, idx)

    return (submod, idx), state + 1
end
done(itr::MarketIteratorBLP, state) = state > length(itr.markets)


########################################################################
#################### Compute δ with Contraction ########################
########################################################################

"""
Initializes with a guess for δ0:
    δ0 = log(S_jt) - log(S_0t)
"""
function guess_delta!(m::BLP)
    m._δ = log.(shares(m.data)) .- log.(outside(m.data))
end

function compute_delta!(m::BLP, δ; kwargs...)
    m._δ = δ
    # Compute Contraction
    return compute_delta!(m; kwargs...)
end

function compute_delta!(m::BLP; rel_tol=1e-4)

    for (market, idx) in eachmarket(m)
        # Use the most recent one
        δ = market._δ

        # Get the shares from the data
        s = shares(market.data)
        ls = log.(s)

        # Compute the idiosyncratic preferences
        σ = Diagonal(m[:σ])
        X = observables(market.data)
        V = market._draws
        μ = X*σ*V

        # Iterate on the contraction until convergence
        err = Inf
        while !(err < rel_tol)
            δ0= δ
            estimated = shares(market, δ, μ)
            # @show size(estimated)
            # @show size(δ)
            le = log.(estimated)
            δ = δ + ls - le
            err = norm(δ - δ0)/norm(δ0)
        end

        # Store the result
        m._δ[idx] = δ
    end

    # Return the converged δ
    return m._δ
end


########################################################################
#################### Estimate with Nonlinear GMM #######################
########################################################################

function estimate_linear!(m::BLP)

    # Compute the linear parameters with linear GMM
    X = observables(m.data)
    Z = instruments(m.data)
    δ = m._δ
    gmm = LinearGMM(δ, X, Z)
    β̂ = estimate!(gmm)
    m[:β] = β̂

    m._linear_gmm = gmm

    return β̂
end

function moments(m::BLP, θ)

    ξ = compute_ξ!(m, θ)

    # These errors are orthogonal to the instruments
    return Z'*ξ
end

function compute_ξ!(m::BLP, θ)
    println("Solving the model at θ=$(θ)")

    # Compute the mean valuations
    m[:σ] = θ

    δ = compute_delta!(m)

    # Estimate the linear parameters
    β̂ = estimate_linear!(m)

    # Compute the structural error
    ξ = δ - X*β̂
    return ξ
end

function estimate!(m::BLP, W, θ0)

    m[:σ] = θ0

    # Estimate the variance parameters using Nonlinear GMM
    gmm = NonlinearGMM(m)
    estimate!(gmm, W, m[:σ])

    return gmm
end

function estimate!(m::BLP, θ0)

    Z   = instruments(m.data)

    # First stage
    gmm = estimate!(m, inv(Z'Z), θ0)
    θ̂ = params(gmm)

    # This gives us a consistent estimate of Ŝ
    Ŝ = gmm.vvcov(gmm)

    # Re-estimate with Ŵ = inv(Ŝ)
    estimate!(gmm, inv(Ŝ), θ̂)

    return gmm

end


# Compute the standard errors
se(m::BLP) = Dict(:β=>se(m._linear_gmm), :σ=>se(m._nonlinear_gmm))
