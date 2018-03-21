function predict(d::InsuranceLogit,p0::Array{Float64})
        parameters = parDict(d,p0)
        contraction!(d,p)
        return predict(d,parameters)
end

function predict{T}(d::InsuranceLogit,p::parDict{T})
        individual_values!(d,p)
        return firmshares(d,p)
end

function firmshares{T}(d::InsuranceLogit,p::parDict{T})
    # Calculate overall marketshares
    J = length(d.shares)
    shares_pred = Array{T,1}(J)
    wgts = transpose(weight(d.data))[:,1]
    for j in d.prods
        j_index_all = d.data._productDict[j]
        shares_pred[j] =  sliceMean_wgt(p.s_hat,wgts,j_index_all)
    end
    return shares_pred
end

function elasticities{T}(d::InsuranceLogit,p::parDict{T},j::Int64,k::Int64)
    γ = p.γ
    β = p.β
    δ_long = p.δ

    j_idx_all = d.data._productDict[j]
    j_people = d.data.person[j_idx_all]
    k_idx_all = d.data._productDict[k]
    k_people = d.data.person[k_idx_all]
    relevant_mkts = intersect(j_people,k_people)
    if length(relevant_mkts)==0
        return 0.0
    end

    # Calculate μ_ij, which depends only on parameters
    for app in eachperson(d.data)
        ind = person(app)[1]
        if in(ind,relevant_mkts)
            continue
        end
        X = permutedims(prodchars(app),(2,1))
        Z = demoRaw(app)[:,1]
        idxitr = d.data._personDict[ind]
        prods = app.product[idxitr]
        j_ind = findin(j,prods)
        k_ind = findin(k,prods)


        β_z = β*Z
        β_i = calc_β(p,β_z)
        alpha = β_i[1,:]
        p_k = X[k.==prods,1]
        chars = X*β_i

        demos = vecdot(γ,Z)

        δ = δ_long[idxitr]
        d.s_hat[idxitr] = individual_shares_RC(chars,demos,δ)
    end

end

function individual_elast{T}(chars::Matrix{T},demos,δ;inside=false)
    (K,N) = size(chars)
    μ = Matrix{T}(K,N)
    s_hat = Matrix{T}(K,N)
    s_mean = Vector{T}(K)
    if inside
        out = 0.0
    else
        out = 1.0
    end
    for n in 1:N
        expsum = out
        for i in 1:K
            a = exp(chars[i,n] + demos + δ[i])
            μ[i,n] = a
            expsum += a
        end
        for i in 1:K
            s_hat[i,n] = μ[i,n]/expsum
        end
    end
    #s_mean = mean(s_hat,2)
    for i in 1:K
        s_mean[i] = mean(s_hat[i,:])
    end
    return s_mean
end
