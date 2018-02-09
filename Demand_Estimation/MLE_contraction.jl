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
    data::ChoiceData

    # Matrix to store estimated values, μ_ij and predicted share
    μ::Array{Float64,1}
    δ::Array{Float64,1}
    util::Array{Float64,1}
    s_hat::Array{Float64,1}

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

    n, k = size(c.data)
    μ = Vector{Float64}(k)
    δ = Vector{Float64}(k)
    util = Vector{Float64}(k)
    s_hat = Vector{Float64}(k)

    d = InsuranceLogit(args,parLength,data,μ,δ,util,s_hat)
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
# Calculate individual Values
function individual_values!(d::InsuranceLogit)
    # Store Parameters
    γ = d[:γ]
    β= d[:β]
    ϕ = d[:ϕ]
    α = d[:α]
    # Store scalar and fixed effect parameters
    ϕ0 = ϕ[1]
    ϕg = ϕ[2:end]
    α0 = α[1]
    αg = α[2:end]


    # Calculate μ_ij, which depends only on parameters
    for app in eachperson(d.data)
        ind = person(app)[1]
        p = transpose(price(app))
        X = permutedims(riskchars(app),(2,1))
        Z = permutedims(demoRaw(app),(2,1))
        D = transpose(demoFE(app)[:,1])

        # chars = similar(X[:,1])
        # demos = similar(chars)
        # A_mul_B!(chars,X,β)
        # A_mul_B!(demos,Z,γ)
        chars = X*β
        demos = Z*γ
        alpha = (α0 + D*αg)
        phi = (ϕ0 + D*ϕg)
        idxitr = app._personDict[ind]
        ind_i = 0
        for i in idxitr
            ind_i += 1
            d.μ[i] = p[ind_i]*alpha + chars[ind_i]*phi + demos[ind_i]
        end
    end
    return Void
end

# @views vslice(x::Matrix,i) = x[:,i]
# function individual_values2!(d::InsuranceLogit)
#     # Store Parameters
#     γ = d[:γ]
#     β = d[:β]
#     ϕ = d[:ϕ]
#     α = d[:α]
#     # Store scalar and fixed effect parameters
#     ϕ0 = ϕ[1]
#     ϕg = ϕ[2:end]
#     α0 = α[1]
#     αg = α[2:end]
#     modelData = d.data
#     dmat = modelData.data
#     (n,k) = size(dmat)
#     # Calculate μ_ij, which depends only on parameters
#     for i in 1:k
#         col = vslice(dmat,i)
#         p = col[modelData._price][1]
#         X = col[modelData._riskchars]
#         Z = col[modelData._demoRaw]
#         D = col[modelData._demoFE]
#
#         chars = vecdot(β,X)
#         demos = vecdot(γ,Z)
#         alpha = (α0 + vecdot(αg,D))
#         phi = (ϕ0 + vecdot(ϕg,D))
#
#         d.μ[i] = p*alpha + chars*phi + demos
#     end
#     return Void
# end


function unpack_δ!(d::InsuranceLogit)
    prods = d.data.pdata[:Product]
    for j in prods
        δ_j= d.data.pdata[:delta][j]
        idx_j = d.data._productDict[j]
        d.δ[idx_j] = δ_j
    end
    return Void
end

#Calculate Utility Values
function utility_val!(d::InsuranceLogit)
    μ_ij = d.μ
    δ_j = d.δ
    idxitr = eachindex(μ_ij)
    for i in idxitr
        d.util[i] = exp(μ_ij[i]+ δ_j[i])
    end
    # d.util = exp.(μ_ij + δ_j)
    return Void
end

# Calculate individual market shares
@views sliceSum(x::Vector{Float64},idx::UnitRange{Int64}) = sum(x[idx])
function individual_shares!(d::InsuranceLogit)
        # Calculate predicted market shares, which depends only on parameters
        perLookup::Dict{Real, UnitRange{Int}} = d.data._personDict
        for idx_i in values(perLookup)

            expsum = sliceSum(d.util,idx_i)
            for i in idx_i
                d.s_hat[i] = d.util[i]/(expsum)
            end
        end
        return Void
end

# Update δ
@views sliceMean(x::Vector{Float64},idx) = mean(x[idx])
function δ_update!(d)
    # Calculate overall marketshares and update δ_j
    eps = 0
    for j in d.data.pdata[:Product]
        j_index_all = d.data._productDict[j]
        #s_hat_j= mean(d.s_hat[j_index_all])
        s_hat_j= sliceMean(d.s_hat,j_index_all)
        s_j = d.data.pdata[j,2]
        chg = log(s_j) - log(s_hat_j)
        d.data.pdata[j,3] += chg
        # if abs(chg)>eps
        #     j_high = j
        # end
        eps = max(eps,abs(chg))
    end
    #println(eps)
    return eps
end

# Overwrite the Standard MLE Maximum Likelihood Computation
function log_likelihood(d::InsuranceLogit)

    individual_values!(d)
    # Contraction...
    rnd = 0
    eps = 1
    while (eps>1e-10) & (rnd<1000)
        rnd+=1
        #Unpack δ_j into estimator data
        unpack_δ!(d)
        utility_val!(d)
        # Calculate predicted market shares
        individual_shares!(d)

        eps = δ_update!(d)
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
