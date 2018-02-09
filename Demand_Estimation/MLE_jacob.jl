#=
Interface for Maximum Likelihood

Implements
    1)  Evaluation of the log_likelihood function
    2)  Computation of the maximum likelihood estimates
    3)  Bootstrapped Standard Errors

Assumes that user has already implemented:
    1) log_likelihood(d, row) to compute the log likelihood of a given
        row of data
    2) An `unpack!` function, which takes a vector of parameters `x` and
        stores it as the appropriate parameter values in the
        `MaximumLikelihood` instance
    3)  A `pack` function, which takes the parameter values stored in
        the `MaximumLikelihood` instance and returns them in stacked
        vector form
=#

using NLopt
using ForwardDiff

include("Models.jl")

abstract type MaximumLikelihood <: Model end

"""
```
log_likelihood(model)
```

Computes the likelihood function given the parameters in model through
repeated calls to log_likelihood(model, obs) where obs iterates over
each row in model.data. Sums and returns the total log_likelihood
contributions of the model's observations.
"""
function log_likelihood(d::MaximumLikelihood)

    # Sum up all the log likelihoods
    ll = 0.0
    for row in eachrow(d.data)
        ll += log_likelihood(d, row)
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
function log_likelihood!(d::MaximumLikelihood, x)
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
function estimate!(d::MaximumLikelihood, p0)

    # Set up the optimization
    opt = Opt(:LD_MMA, length(p0))
    xtol_rel!(opt, 1e-4)

    # Objective Function
    ll(x) = log_likelihood!(d, x)
    function ll(x, grad)

        # Store Gradient
        ForwardDiff.gradient!(grad, ll, x)
        return ll(x)
    end

    # Set Objective
    max_objective!(opt, ll)

    # Run Optimization
    minf, minx, ret = optimize(opt, p0)

    # Set the parameters
    log_likelihood!(d, minx)

    # Return the object
    return d
end



########################################################################
#################### Bootstrapping Standard Errors #####################
########################################################################

function resample(d::MaximumLikelihood)

    # Sample with replacement
    n = size(d.data,1)
    idx = rand(1:n, n)

    T   = typeof(d)
    data= d.data[idx,:]
    x   = pack(d)

    # Construct the new model on the resampled data
    newd = T(data)
    unpack!(newd, x)

    return newd
end

function bootstrap!(d::MaximumLikelihood, N)

    # Our parameter estimates as a vector
    x = pack(d)

    # Preallocate vector
    estimated = zeros(length(x), N)

    for i=1:N
        # Print the progress
        print("\rEstimating Bootstrap Sample $i/$N")

        # Construct a new random sample with the same number of
        # observations (sampling with replacement)
        new = resample(d)

        # Estimate the model on the new sample
        estimate!(new, x)

        # Store the results
        estimated[:,i] = pack(new)

    end

    print("\n")

    # Store the standard errors
    d.std_errors = unpack(d, std(estimated,2)[:])

    return d
end
