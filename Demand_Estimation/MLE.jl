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

type InsuranceLogit <: LogitModel
    # Parameters
    parameters::Dict{Symbol, Any}
    parLength::Dict{Symbol, Int}
    #   Includes:
    #   γ   Vector governing demographic preferences for insurance
    #   β   Vector governing preferences over different risk characteristics
    #   ϕ   Vector governing risk sensativity, accounting for fixed effects
    #   α   Vector governing price sensitivity, accounting for fixed effects
    data::ModelData
end


function InsuranceLogit(data::ChoiceData; kwargs...)
    # Construct the model instance using the parameters passed as keyword
    # arguments as well as the data
    args = Dict(kwargs)

    γlen = size(demoRaw(data),1)
    βlen = size(riskchars(data),1)
    ϕlen = size(demoFE(data),1) + 1
    αlen = size(demoFE(data),1) + 1

    parLength = Dict(:γ=>γlen,:β=>βlen,
                    :ϕ=>ϕlen,:α=>αlen)

    d = InsuranceLogit(args,parLength,data)
    return d
end

pack(m::InsuranceLogit) = vcat(m[:γ],m[:β],m[:ϕ],m[:α])
function unpack!{T}(m::InsuranceLogit, x::Vector{T})
    γlen = m.parLength[:γ]
    βlen = γlen + m.parLength[:β]
    ϕlen = βlen + m.parLength[:ϕ]

    m[:γ] = x[1:γlen]
    m[:β] = x[γlen+1:βlen]
    m[:ϕ] = x[βlen+1:ϕlen]
    m[:α] = x[ϕlen+1:end]

    return Void
end

# Calculate the contraction


# Overwrite the Standard MLE Maximum Likelihood Computation
function log_likelihood(d::InsuranceLogit)

    # Store Parameters
    γ = d[:γ]
    β = d[:β]
    ϕ = d[:ϕ]
    α = d[:α]

    # Store scalar and fixed effect parameters
    ϕ0 = ϕ[1]
    ϕg = ϕ[2:end]
    α0 = α[1]
    αg = α[2:end]

    ll = 0.0
    for app in eachperson(d.data)
        p = transpose(price(app))
        X = permutedims(riskchars(app),(2,1))
        Z = permutedims(demoRaw(app),(2,1))
        D = transpose(demoFE(app)[:,1])
        Y = choice(app)
        urate = unins(app)

        chars = X*β
        demos = Z*γ
        alpha = (α0 + D*αg)
        phi = (ϕ0 + D*ϕg)
        δ = Array{Real}(length(p),1)
        #δ = similar(p)
        idxitr = eachindex(p)
        for i in idxitr
            δ[i] = p[i]*alpha + chars[i]*phi + demos[i]
        end
        #δ = p.*(α0 + D*αg) + X*β.*(ϕ0 + D*ϕg) + Z*γ

        # Compute the perdicted shares
        g = similar(δ)
        for i in idxitr
            g[i] = exp(δ[i])
        end
        expsum = sum(g)
        logsumcond = log(expsum)
        logsumtot = log(1+expsum)
        #s = g./(1 + sum(g))
        s0 = 1/(1+expsum)

        # the log-likelihood contribution of this consumer
        # Check for uniqueness...?
        unins_rate = urate[1]
        value = 0.0
        for i in idxitr
            value = value + (1-unins_rate)*Y[i]*(δ[i] - logsumcond)
        end
        #value = Y*δ - logsum
        #ll += Y*log.(s)
        ll = ll + value
        ll = ll + (1-unins_rate)*logsumcond - logsumtot
    end
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
function log_likelihood!(d::InsuranceLogit, x)
    unpack!(d, x)

    return log_likelihood(d)
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
    ll(x) = log_likelihood!(d, x)
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
    log_likelihood!(d, minx)

    # Return the object
    return d
end
