d = m
p = par0

individual_values!(d,p)
individual_shares(d,p)


function calc_shares{T}(μ_ij::Array{T},δ::Vector{T},r::Vector{Float64})
    (N,K) = size(μ_ij)
    util = Matrix{T}(K,N)
    s_hat = Matrix{T}(K,N)
    r_hat = Matrix{T}(K,N)

    for n in 1:N
        expsum = 1.0
        r_score = r[n]
        for i in 1:K
            a = μ_ij[n,i]*δ[i]
            util[i,n] = a
            expsum += a
        end
        for i in 1:K
            s = util[i,n]/expsum
            s_hat[i,n] = s
            r_hat[i,n] = s*r_score
        end
    end
    s_mean = mean(s_hat,2)
    r_mean = sum(r_hat,2)./sum(s_hat,2)
    return s_mean, r_mean
end

function individual_shares{T}(d::InsuranceLogit,p::parDict{T})
    # Store Parameters
    δ_long = p.δ
    μ_ij_large = p.μ_ij
    risk_long = d.data[:riskIndex]
    for idxitr in values(d.data._personDict)
        δ = δ_long[idxitr]
        u = μ_ij_large[:,idxitr]
        r_ind = Int(risk_long[idxitr][1])
        r_scores = d.draws[:,r_ind]
        s,r = calc_shares(u,δ,r_scores)
        p.s_hat[idxitr] = s
    end
    return Void
end

Profile.init(n=10^7,delay=.001)
Profile.clear()
Juno.@profile individual_shares(d,p)
Juno.profiletree()
Juno.profiler()
