# using NLopt
using ForwardDiff

# Calculate Log Likelihood
function log_likelihood(d::InsuranceLogit,p::parDict{T};cont_flag=false) where T
    ll = 0.0
    Pop = 0.0
    #α = p.α[1]
    # Calculate μ_ij, which depends only on parameters
    if cont_flag
        contraction!(d,p)
    else
        individual_values!(d,p)
        individual_shares(d,p)
    end
    for app in eachperson(d.data)
    #app = next(eachperson(d.data),100)[1]
        ind = person(app)[1]
        S_ij = transpose(choice(app))
        wgt = transpose(weight(app))
        urate = transpose(unins(app))
        idxitr = d.data._personDict[ind]


        # Get Market Shares
        s_hat = p.s_hat[idxitr]
        # Fix possible computational error
        for k in eachindex(s_hat)
            if abs(s_hat[k])<=1e-300
                s_hat[k]=1e-15
                #println("Hit Share Constraint for person $ind, product $k")
            end
        end
        s_insured = sum(s_hat)
        if s_insured>=(1-1e-300)
            s_insured= 1 - 1e-15
            #println("Hit insured constraint for person $ind")
        end

        for i in eachindex(idxitr)
            ll+=wgt[i]*S_ij[i]*(log(s_hat[i]) -urate[i]*(log(s_insured)-log(1-s_insured)))
            #ll+=wgt[i]*S_ij[i]*(log(s_hat[i]))
            Pop+=wgt[i]*S_ij[i]
        end
        # if isnan(ll)
        #     println(ind)
        #     break
        # end
    end
    return ll/Pop
end

function log_likelihood!(grad::Vector{T},d::InsuranceLogit,p::parDict{T};cont_flag = false) where T
    Q = d.parLength[:All]
    N = size(d.draws,1)
    grad[:] .= 0.0
    ll = 0.0
    Pop =sum(weight(d.data).*choice(d.data))

    # Calculate μ_ij, which depends only on parameters
    if cont_flag
        contraction!(d,p)
    else
        individual_values!(d,p)
        individual_shares(d,p)
    end

    #shell_full = zeros(Q,N,38)
    for app in eachperson(d.data)
        K = length(person(app))
        # if K>k_max
        #     k_max = K
        # end
        #shell = shell_full[:,:,1:K]
        ll_obs,grad_obs = ll_obs_gradient(app,d,p)

        ll+=ll_obs
        for q in 1:Q
            grad[q]+=grad_obs[q]/Pop
        end
    end
    if isnan(ll)
        ll = -1e20
    end
    return ll/Pop
end




function log_likelihood(d::InsuranceLogit,p::Array{T};cont_flag=false) where T
    params = parDict(d,p)
    ll = log_likelihood(d,params,cont_flag = cont_flag)
    convert_δ!(d)
    return ll
end

function log_likelihood!(grad::Vector{Float64},d::InsuranceLogit,p::Array{T};cont_flag=false) where T
    params = parDict(d,p)
    ll = log_likelihood!(grad,d,params,cont_flag = cont_flag)
    convert_δ!(d)
    return ll
end


# Calculate Standard Errors
# Hiyashi, p. 491
function calc_Avar(d::InsuranceLogit,p::parDict{T}) where T
    # Calculate μ_ij, which depends only on parameters
    individual_values!(d,p)
    individual_shares(d,p)

    Σ = zeros(d.parLength[:All],d.parLength[:All])
    Pop =sum(weight(d.data).*choice(d.data))

    for app in eachperson(d.data)
        ll_obs,grad_obs = ll_obs_gradient(app,d,p)
        S_n = grad_obs*grad_obs'
        Σ+= S_n
    end

    Σ = Σ./Pop
    # This last line is correct
    AsVar = inv(Σ)./Pop
    return AsVar
end


function log_likelihood!(grad::Vector{Float64},
                            d::InsuranceLogit,p::parDict{T};cont_flag::Bool=false) where T
    Q = d.parLength[:All]
    N = size(d.draws,1)
    grad[:] .= 0.0
    ll = 0.0
    Pop =sum(weight(d.data).*choice(d.data))
    grad_obs = Vector{Float64}(undef,Q)

    #Reset Derivatives
    p.dSdθ_j[:] .= 0.0
    p.dRdθ_j[:] .= 0.0
    p.d2Sdθ_j[:] .= 0.0
    p.d2Rdθ_j[:] .= 0.0

    if cont_flag
        contraction!(d,p)
    else
        individual_values!(d,p)
        individual_shares(d,p)
    end

    #shell_full = zeros(Q,N,38)
    for app in eachperson(d.data)
        ll_obs,pars_relevant = ll_obs_gradient!(grad,app,d,p)
        ll+=ll_obs
    end
    if isnan(ll)
        ll = -1e20
    end
    for q in 1:Q
        grad[q]=grad[q]/Pop
    end

    return ll/Pop
end



function log_likelihood!(hess::Matrix{Float64},grad::Vector{Float64},
                            d::InsuranceLogit,p::parDict{T};cont_flag::Bool=false) where T
    Q = d.parLength[:All]
    N = size(d.draws,1)
    hess[:] .= 0.0
    grad[:] .= 0.0
    ll = 0.0
    Pop =sum(weight(d.data).*choice(d.data))
    hess_obs = Matrix{Float64}(undef,Q,Q)
    grad_obs = Vector{Float64}(undef,Q)

    #Reset Derivatives
    p.dSdθ_j[:] .= 0.0
    p.dRdθ_j[:] .= 0.0
    p.d2Sdθ_j[:] .= 0.0
    p.d2Rdθ_j[:] .= 0.0

    if cont_flag
        contraction!(d,p)
    else
        individual_values!(d,p)
        individual_shares(d,p)
    end

    #shell_full = zeros(Q,N,38)
    for app in eachperson(d.data)
        K = length(person(app))
        # if K>k_max
        #     k_max = K
        # end
        #shell = shell_full[:,:,1:K]
        ll_obs,pars_relevant = ll_obs_hessian!(hess,grad,app,d,p)

        ll+=ll_obs

        #add_obs_mat!(hess,grad,hess_obs,grad_obs,Pop,pars_relevant)

    end
    if isnan(ll)
        ll = -1e20
    end
    for q in 1:Q
        grad[q]=grad[q]/Pop
        for r in 1:Q
        hess[q,r]=hess[q,r]/Pop
        end
    end

    return ll/Pop
end

function log_likelihood!(hess::Matrix{Float64},grad::Vector{Float64},
                            d::InsuranceLogit,p::Array{T}) where T
    params = parDict(d,p)
    ll = log_likelihood!(hess,grad,d,params)
    convert_δ!(d)
    return ll
end



function log_likelihood!(thD::Array{Float64,3},
                            hess::Matrix{Float64},grad::Vector{Float64},
                            d::InsuranceLogit,p::parDict{T};cont_flag::Bool=false) where T
    Q = d.parLength[:All]
    N = size(d.draws,1)

    thD[:] .= 0.0
    hess[:] .= 0.0
    grad[:] .= 0.0
    ll = 0.0
    Pop =sum(weight(d.data).*choice(d.data))

    #Reset Derivatives
    p.dSdθ_j[:] .= 0.0
    p.dRdθ_j[:] .= 0.0
    p.d2Sdθ_j[:] .= 0.0
    p.d2Rdθ_j[:] .= 0.0

    # thD_obs = Array{Float64,3}(Q,Q,Q)
    # hess_obs = Matrix{Float64}(Q,Q)
    # grad_obs = Vector{Float64}(Q)

    if cont_flag
        contraction!(d,p)
    else
        individual_values!(d,p)
        individual_shares(d,p)
    end

    #shell_full = zeros(Q,N,38)
    for app in eachperson(d.data)
        K = length(person(app))
        # if K>k_max
        #     k_max = K
        # end
        #shell = shell_full[:,:,1:K]
        ll_obs,pars_relevant = ll_obs_hessian!(thD,hess,grad,app,d,p)

        ll+=ll_obs

        #add_obs_mat!(hess,grad,hess_obs,grad_obs,Pop,pars_relevant)

    end
    if isnan(ll)
        ll = -1e20
    end
    for q in 1:Q
        grad[q]=grad[q]/Pop
        for r in 1:Q
            hess[q,r]=hess[q,r]/Pop
            for t in 1:Q
                thD[q,r,t]=thD[q,r,t]/Pop
            end
        end
    end

    return ll/Pop
end

function log_likelihood!(thD::Array{Float64,3},
                            hess::Matrix{Float64},grad::Vector{Float64},
                            d::InsuranceLogit,p::Array{T}) where T
    params = parDict(d,p)
    ll = log_likelihood!(thD,hess,grad,d,params)
    convert_δ!(d)
    return ll
end
