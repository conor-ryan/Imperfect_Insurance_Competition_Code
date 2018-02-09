include("MLE.jl")

type MultinomialLogit <: MaximumLikelihood
    # Parameters
    parameters::Dict{Symbol, Any}
    #   Includes:
    #   β   Vector of parameters governing preferences over observables
    #   α   Scalar governing price sensitivity
    data::ModelData
end

function MultinomialLogit(data; kwargs...)

    # Construc the model instance using the parameters passed as keyword
    # arguments as well as the data
    args = Dict(kwargs)
    d = MultinomialLogit(args, data)

    return d
end

pack(m::MultinomialLogit) = vcat(m[:β], m[:α])
function unpack!{T}(m::MultinomialLogit, x::Vector{T})
    m[:β] = x[1:end-1]
    m[:α] = x[end]
    return Void
end

# Overwrite the Standard MLE Maximum Likelihood Computation
function log_likelihood(d::MultinomialLogit)

    # Store Parameters
    β = d[:β]
    α = d[:α]

    ll = 0.0
    for market in eachmarket(d.data)
        X = chars(market)
        p = price(market)
        δ = X*β + α*p

        # Compute the perdicted shares
        g = exp.(δ)
        s = g./(1 + sum(g))

        # Weight the predicted shares by the observed shares to recover
        # the log-likelihood contribution of this market
        sd = shares(market)
        ll += sum(sd.*log.(s))
        ll += (1-sum(sd))*log(1/(1+sum(g)))
    end
    return ll
end
