using NLopt
using ForwardDiff

# Calculate Log Likelihood
function log_likelihood{T}(d::InsuranceLogit,p::parDict{T})
    ll = 0.0
    Pop = 0.0
    #α = p.α[1]
    # Calculate μ_ij, which depends only on parameters
    individual_values!(d,p)
    individual_shares(d,p)
    for app in eachperson(d.data)
    #app = next(eachperson(d.data),100)[1]
        ind = person(app)[1]
        S_ij = transpose(choice(app))
        wgt = transpose(weight(app))
        urate = transpose(unins(app))
        idxitr = d.data._personDict[ind]


        δ = p.δ[idxitr]
        s_hat = p.s_hat[idxitr]
        s_insured = sum(s_hat)

        for i in eachindex(idxitr)
            ll+=wgt[i]*S_ij[i]*(log(s_hat[i]) -urate[i]*(log(s_insured)-log(1-s_insured)))
            #ll+=wgt[i]*S_ij[i]*(log(s_hat[i]))
            Pop+=wgt[i]*S_ij[i]
        end
    end
    return ll/Pop
end

function log_likelihood{T}(d::InsuranceLogit,p::Array{T})
    params = parDict(d,p)
    ll = log_likelihood(d,params)
    convert_δ!(d)
    return ll
end

# Calculate Log Likelihood
function ll_gradient{T}(d::InsuranceLogit,p::parDict{T})
    p_num = length(p.γ) + length(p.β_0) + length(p.β) + length(p.σ)
    ll = fill(0.0,(p_num))
    Pop = 0.0
    γ = p.γ
    β = p.β
    #α = p.α[1]
    # Calculate μ_ij, which depends only on parameters
    for app in eachperson(d.data)
        μ_ij = util_value!(app,p)
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
        (Q,N,K) = size(dμ_ij)

        for k = 1:K
            dμ_ij[:,:,k] = dμ_ij[:,:,k].*δ[k]
        end

        μ_ij_sums = 1.+μ_ij*δ
        μ_ij_sums_sq = (1.+μ_ij*δ).^2
        dμ_ij_sums = sum(dμ_ij,3)

        for k = 1:K,q in 1:Q
            # ll[q] += wgt[k]/N*S_ij[k]*( (1/s_hat[k])*(
            #             dμ_ij[q,n,k]/μ_ij_sums[n] -
            #             dμ_ij_sums[q,n,1]*μ_ij[n,k]*δ[k]/μ_ij_sums_sq[n] ) -
            #     urate[k]*( dμ_ij_sums[q,n,1]/μ_ij_sums_sq[n] )*(
            #                     1/(s_insured) + 1/(1-s_insured) ) )
            t1 = mean(dμ_ij[q,:,k]./μ_ij_sums)
            t2 = mean(dμ_ij_sums[q,:,1].*μ_ij[:,k].*δ[k]./μ_ij_sums_sq)
            t3 = mean(dμ_ij_sums[q,:,1]./μ_ij_sums_sq)
            ll[q] += wgt[k]*S_ij[k]*( (1/s_hat[k])*(t1 - t2) -
                urate[k]*( t3 )*(1/(s_insured) + 1/(1-s_insured) ) )
        end
        Pop+= sum(wgt.*S_ij)
    end
    return ll./Pop
    # return fval/Pop
end


function ll_gradient{T}(d::InsuranceLogit,p::Array{T})
    params = parDict(d,p)
    grad = ll_gradient(d,params)
    convert_δ!(d)
    return grad
end

function GMM_objective{T}(d::InsuranceLogit,p::Array{T})
    grad = ll_gradient(d,p)
    println("gradient equals $grad")
    obj = vecdot(grad,grad)
    return obj
end


function evaluate_iteration{T}(d::InsuranceLogit,p::parDict{T};update::Bool=true)
    contraction!(d,p,update=update)
    ll = log_likelihood(d,p)
    convert_δ!(d)
    return ll
end

function evaluate_iteration!{T}(d::InsuranceLogit, x::Array{T,1};update::Bool=true)
    # Create Parameter Types
    parameters = parDict(d,x)
    return evaluate_iteration(d,parameters,update=update)
end
