using NLopt
using ForwardDiff

# Calculate Log Likelihood
function log_likelihood{T}(d::InsuranceLogit,p::parDict{T};cont_flag=false)
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

function log_likelihood!{T}(grad::Vector{T},d::InsuranceLogit,p::parDict{T};cont_flag = false)
    Q = d.parLength[:All]
    N = size(d.draws,1)
    grad[:] = 0.0
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




function log_likelihood{T}(d::InsuranceLogit,p::Array{T};cont_flag=false)
    params = parDict(d,p)
    ll = log_likelihood(d,params,cont_flag = cont_flag)
    convert_δ!(d)
    return ll
end

function log_likelihood!{T}(grad::Vector{Float64},d::InsuranceLogit,p::Array{T};cont_flag=false)
    params = parDict(d,p)
    ll = log_likelihood!(grad,d,params,cont_flag = cont_flag)
    convert_δ!(d)
    return ll
end


# Calculate Standard Errors
# Hiyashi, p. 491
function calc_Avar{T}(d::InsuranceLogit,p::parDict{T})
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
    # This last line is ??
    AsVar = inv(Σ)./Pop
    return AsVar
end




function log_likelihood!{T}(hess::Matrix{Float64},grad::Vector{Float64},
                            d::InsuranceLogit,p::parDict{T};cont_flag::Bool=false)
    Q = d.parLength[:All]
    N = size(d.draws,1)
    hess[:] = 0.0
    grad[:] = 0.0
    ll = 0.0
    Pop =sum(weight(d.data).*choice(d.data))
    hess_obs = Matrix{Float64}(Q,Q)
    grad_obs = Vector{Float64}(Q)

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

function add_obs_mat!(hess::Matrix{Float64},grad::Vector{Float64},
                        hess_obs::Matrix{Float64},grad_obs::Vector{Float64},
                        Pop::Float64,pars_relevant::Vector{Int})
    Q = length(grad)
    for q in pars_relevant
        grad[q]+=grad_obs[q]/Pop
        for r in pars_relevant
        hess[q,r]+=hess_obs[q,r]/Pop
        end
    end
    return Void
end

function test()
    for i in 1:1000, j in 1:1000
        x = 3.4 + 9.0
    end
end


function log_likelihood!{T}(hess::Matrix{Float64},grad::Vector{Float64},
                            d::InsuranceLogit,p::Array{T})
    params = parDict(d,p)
    ll = log_likelihood!(hess,grad,d,params)
    convert_δ!(d)
    return ll
end
