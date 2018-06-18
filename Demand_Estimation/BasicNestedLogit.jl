########
# Parameter Structure
#########


type parDict{T}
    # Parameters
    γ_0::T
    γ::Vector{T}
    β_0::Vector{T}
    β::Matrix{T}
    σ::T
    FE::Matrix{T}
    # δ values for (ij) pairs
    δ::Vector{T}
    # Non-delta utility for (ij) pairs and draws
    μ_ij::Vector{T}
    # Shares for (ij) pairs
    s_hat::Vector{T}
end


function parDict{T}(m::InsuranceLogit,x::Array{T})
    # Parameter Lengths from model
    γlen = m.parLength[:γ]
    β0len = γlen + m.parLength[:β]
    βlen = β0len + m.parLength[:γ]
    σlen = βlen +  m.parLength[:σ]
    FElen = σlen + m.parLength[:FE]


    #Distribute Parameters
    γ_0 = 0.0
    γ = x[1:γlen]
    β_0 = x[(γlen+1):β0len]
    β_vec = x[(β0len+1):βlen]
    σ = x[σlen]
    fe_vec = x[(σlen+1):FElen]

    # Store FE as row Vector
    FE = Matrix{T}(1,length(fe_vec))
    FE[1,:] = fe_vec

    # Stack Beta into a matrix
    K = m.parLength[:β]
    N = m.parLength[:γ]
    β = Matrix{T}(K,N)
    ind = 0
    for i in 1:N, j in 1:K
        if j==1
            ind+=1
            β[j,i] = β_vec[ind]
        else
            β[j,i] = 0
        end
    end
    #Initialize (ij) pairs of deltas
    L, M = size(m.data.data)
    δ = Vector{T}(M)
    μ_ij = Vector{T}(M)
    s_hat = Vector{T}(M)
    unpack_δ!(δ,m)

    return parDict{T}(γ_0,γ,β_0,β,σ,FE,δ,μ_ij,s_hat)
end
###########
# Calculating Preferences
###########

function individual_values!{T}(d::InsuranceLogit,p::parDict{T})
    # Store Parameters
    # Calculate μ_ij, which depends only on parameters
    for app in eachperson(d.data)
        burn = util_value!(app,p)
    end
    return Void
end

function util_value!{T}(app::ChoiceData,p::parDict{T})
    γ_0 = p.γ_0
    γ = p.γ
    β_0= p.β_0
    β = p.β
    fe = p.FE
    σ = p.σ

    ind = person(app)[1]
    idxitr = app._personDict[ind]
    X = permutedims(prodchars(app),(2,1))
    Z = demoRaw(app)[:,1]
    F = fixedEffects(app,idxitr)

    β_z = β*Z
    demos = γ_0 + vecdot(γ,Z)

    chars = X*β_z
    chars_0 = X*β_0

    # FE is a row Vector
    if T== Float64
        controls = zeros(size(F,2))
        for k in 1:length(controls)
            for j in app._rel_fe_Dict[ind]
                controls[k]+= fe[j]*F[j,k]
            end
        end
    else
        controls = fe*F
    end

    K= length(chars)
    for k = 1:K
        @fastmath u = exp((chars[k] + chars_0[k] + controls[k] + demos)/(1-σ))
        p.μ_ij[idxitr[k]] = u
    end

end


function calc_shares{T}(μ_ij::Vector{T},δ::Vector{T},σ::T)
    K = length(μ_ij)
    util = Vector{T}(K)
    s_hat = Vector{T}(K)
    #s_mean = Vector{T}(K)
    expsum = 0.0
    for i in 1:K
        a = μ_ij[i]*δ[i]
        util[i] = a
        expsum += a
    end
    for i in 1:K
        s_hat[i] = (util[i]*expsum^(-σ))/(1+expsum^(1-σ))
    end

    return s_hat
end

function individual_shares{T}(d::InsuranceLogit,p::parDict{T})
    # Store Parameters
    δ_long = p.δ
    σ = p.σ
    μ_ij_large = p.μ_ij
    for idxitr in values(d.data._personDict)
        δ = δ_long[idxitr]
        u = μ_ij_large[idxitr]
        s = calc_shares(u,δ,σ)
        p.s_hat[idxitr] = s
    end
    return Void
end


function ll_obs_gradient{T}(app::ChoiceData,d::InsuranceLogit,p::parDict{T})
        ind = person(app)[1]
        S_ij = transpose(choice(app))
        wgt = transpose(weight(app))
        urate = transpose(unins(app))
        idxitr = d.data._personDict[ind]

        X_t = prodchars(app)
        Z = demoRaw(app)[:,1]
        F_t = fixedEffects(app,idxitr)

        # Get Utility and derivative of Utility
        μ_ij = p.μ_ij[idxitr]
        δ    = p.δ[idxitr]

        #Get Market Shares
        s_hat = p.s_hat[idxitr]
        s_insured = sum(s_hat)

        # Fix possible computational error
        for k in eachindex(s_hat)
            if abs(s_hat[k])<=1e-300
                s_hat[k]=1e-15
                println("Hit Share Constraint for person $ind, product $k")
            end
        end
        s_insured = sum(s_hat)
        if s_insured>=(1-1e-300)
            s_insured= 1 - 1e-15
            println("Hit insured constraint for person $ind")
        end

        # Initialize Gradient
        #(Q,N,K) = size(dμ_ij)
        Q = d.parLength[:All]
        Q_0 = Q - size(F_t,1)
        K = length(μ_ij)
        grad_obs = zeros(Q)
        ll_obs = 0.0

        ## Relevant Parameters for this observation
        pars_relevant = vcat(1:Q_0,Q_0+app._rel_fe_Dict[ind])

        γlen = d.parLength[:γ]
        β0len = γlen + d.parLength[:β]
        βlen = β0len + d.parLength[:γ]
        σlen = βlen + d.parLength[:σ]
        FElen = σlen + d.parLength[:FE]


        # Pre-Calculate Squares
        σ = p.σ
        μ_ij_sums = 1 + (vecdot(μ_ij,δ))^(1-σ)
        μ_ig_σ = vecdot(μ_ij,δ)^(-σ)
        μ_ij_sums_sq = (μ_ij_sums)^2
        μ_ij_logsum = log(vecdot(μ_ij,δ))

        # Pre-Calculate Log-Likelihood Terms for Gradient
        # Also Calculate Log-Likelihood itself
        gll_t1 = Vector{Float64}(K)
        gll_t2 = Vector{Float64}(K)
        for k in 1:K
            #Gradient Terms
            gll_t1[k] = wgt[k]*S_ij[k]*(1/s_hat[k])
            gll_t2[k] = wgt[k]*S_ij[k]*urate[k]*(1/(s_insured) + 1/(1-s_insured))

            # Log Likelihood
            ll_obs+=wgt[k]*S_ij[k]*(log(s_hat[k]) -
                            urate[k]*(log(s_insured)-log(1-s_insured)))
        end


        for q in pars_relevant
            if q<0
                #Remove Initial Constant
                grad_obs[q] += par_gradient(1.0,μ_ij,δ,
                                        μ_ig_σ,σ,
                                        μ_ij_sums,μ_ij_sums_sq,
                                        gll_t1,gll_t2)
            elseif q<=γlen
                # Base Demographics
                grad_obs[q] += par_gradient(Z[q],μ_ij,δ,
                                        μ_ig_σ,σ,
                                        μ_ij_sums,μ_ij_sums_sq,
                                        gll_t1,gll_t2)
            elseif q<=β0len
                # Base Characteristics
                grad_obs[q] += par_gradient(X_t[q-γlen,:],μ_ij,δ,
                                        μ_ig_σ,σ,
                                        μ_ij_sums,μ_ij_sums_sq,
                                        gll_t1,gll_t2)
            elseif q<=βlen
                # Characteristic Interactions
                grad_obs[q] += par_gradient(X_t[1,:]*Z[q-β0len],
                                        μ_ij,δ,
                                        μ_ig_σ,σ,
                                        μ_ij_sums,μ_ij_sums_sq,
                                        gll_t1,gll_t2)
            elseif q<=σlen
                #Nesting Parameter
                grad_obs[q] += par_gradient_σ(μ_ij_logsum,μ_ij,δ,
                                        μ_ig_σ,σ,
                                        μ_ij_sums,μ_ij_sums_sq,
                                        gll_t1,gll_t2)
            else
                #Fixed Effect
                fe_vec = F_t[q-σlen,:]
                grad_obs[q] += par_gradient(fe_vec,
                                        μ_ij,δ,
                                        μ_ig_σ,σ,
                                        μ_ij_sums,μ_ij_sums_sq,
                                        gll_t1,gll_t2)
            end
        end

    return ll_obs, grad_obs
end



##### Gradient Mini Functions #####
function par_gradient{T}(x::Float64,
                            μ_ij::Array{T,1},δ::Vector{T},
                            μ_ig_σ::T,σ::T,
                            μ_ij_sums::T,μ_ij_sums_sq::T,
                            gll_t1::Vector{Float64},gll_t2::Vector{Float64})

    K = length(μ_ij)
    dμ_ij = Vector{Float64}(K)
    dμ_ij_sums = 0.0
    for k in 1:K
        @fastmath dμ_ij[k] = μ_ij[k]*δ[k]*x/(1-σ)
        @fastmath dμ_ij_sums+= dμ_ij[k]
    end
    grad_obs = par_gradient_inner_loop(dμ_ij,dμ_ij_sums,
                            μ_ij,δ,
                            μ_ig_σ,σ,
                            μ_ij_sums,μ_ij_sums_sq,
                            gll_t1,gll_t2)
    return grad_obs
end

function par_gradient{T}(x::Vector{Float64},
                            μ_ij::Array{T,1},δ::Vector{T},
                            μ_ig_σ::T,σ::T,
                            μ_ij_sums::T,μ_ij_sums_sq::T,
                            gll_t1::Vector{Float64},gll_t2::Vector{Float64})
    grad_obs = 0.0
    K = length(μ_ij)
    dμ_ij = Vector{Float64}(K)
    dμ_ij_sums = 0.0
    for k in 1:K
        @fastmath dμ_ij[k] = μ_ij[k]*δ[k]*x[k]/(1-σ)
        @fastmath dμ_ij_sums+= dμ_ij[k]
    end
    grad_obs= par_gradient_inner_loop(dμ_ij,dμ_ij_sums,
                            μ_ij,δ,
                            μ_ig_σ,σ,
                            μ_ij_sums,μ_ij_sums_sq,
                            gll_t1,gll_t2)
    return grad_obs
end

function par_gradient_σ{T}( μ_ij_logsum::T,
                            μ_ij::Array{T,1},δ::Vector{T},
                            μ_ig_σ::T,σ::T,
                            μ_ij_sums::T,μ_ij_sums_sq::T,
                            gll_t1::Vector{Float64},gll_t2::Vector{Float64})
    grad_obs = 0.0
    K = length(μ_ij)
    dμ_ij = Vector{Float64}(K)
    dμ_ij_sums = 0.0
    for k in 1:K
        @fastmath dμ_ij[k] = μ_ij[k]*δ[k]*log(μ_ij[k])/(1-σ)
        @fastmath dμ_ij_sums+= dμ_ij[k]
    end
    grad_obs= par_gradient_inner_loop_σ(dμ_ij,dμ_ij_sums,μ_ij_logsum,
                            μ_ij,δ,
                            μ_ig_σ,σ,
                            μ_ij_sums,μ_ij_sums_sq,
                            gll_t1,gll_t2)
    return grad_obs
end




function par_gradient_inner_loop{T}(dμ_ij::Vector{Float64},dμ_ij_sums::Float64,
                            μ_ij::Array{T,1},δ::Vector{T},
                            μ_ig_σ::T,σ::T,
                            μ_ij_sums::T,μ_ij_sums_sq::T,
                            gll_t1::Vector{Float64},gll_t2::Vector{Float64})

    K = length(μ_ij)
    grad_obs = 0.0
    @fastmath t3 = (1-σ)*dμ_ij_sums*μ_ig_σ/μ_ij_sums_sq
    @inbounds(
    for k in 1:K
        @fastmath t1= dμ_ij[k]*μ_ig_σ/μ_ij_sums
        @fastmath t2a = σ*dμ_ij_sums*μ_ij[k]*δ[k]*(μ_ig_σ/vecdot(μ_ij,δ))/μ_ij_sums
        @fastmath t2b = (1-σ)*dμ_ij_sums*μ_ij[k]*δ[k]*μ_ig_σ^2/μ_ij_sums_sq

        @fastmath grad_obs += gll_t1[k]*(t1 - t2a - t2b) - gll_t2[k]*t3
    end
    )
    return grad_obs
end

function par_gradient_inner_loop_σ{T}(dμ_ij::Vector{Float64},dμ_ij_sums::Float64,
                            μ_ij_logsum::T,
                            μ_ij::Array{T,1},δ::Vector{T},
                            μ_ig_σ::T,σ::T,
                            μ_ij_sums::T,μ_ij_sums_sq::T,
                            gll_t1::Vector{Float64},gll_t2::Vector{Float64})

    K = length(μ_ij)
    grad_obs = 0.0
    @fastmath t3 = (1-σ)*dμ_ij_sums*μ_ig_σ/μ_ij_sums_sq
    @fastmath t3_σ = μ_ij_logsum*μ_ig_σ*vecdot(μ_ij,δ)/μ_ij_sums_sq
    @inbounds(
    for k in 1:K
        @fastmath t1= dμ_ij[k]*μ_ig_σ/μ_ij_sums
        @fastmath t1_σ = μ_ij_logsum*μ_ij[k]*δ[k]*μ_ig_σ/μ_ij_sums_sq
        @fastmath t2a = σ*dμ_ij_sums*μ_ij[k]*δ[k]*(μ_ig_σ/vecdot(μ_ij,δ))/μ_ij_sums
        @fastmath t2b = (1-σ)*dμ_ij_sums*μ_ij[k]*δ[k]*μ_ig_σ^2/μ_ij_sums_sq

        @fastmath grad_obs += gll_t1[k]*(t1 - t2a - t2b - t1_σ) - gll_t2[k]*(t3 - t3_σ)
    end
    )
    return grad_obs
end
