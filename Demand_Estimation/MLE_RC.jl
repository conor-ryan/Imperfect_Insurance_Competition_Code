import Base.getindex, Base.setindex!, Base.show
using NLopt
using ForwardDiff

abstract type LogitModel end

type InsuranceLogit <: LogitModel
    # Dictionary of Parameters and implied lengths
    parLength::Dict{Symbol, Int}
    # ChoiceData struct
    data::ChoiceData

    #Store Halton Draws
    draws::Array{Float64,2}


    # Product Level Data
    # Separate vectors, all sorted by product
    prods
    shares
    #Unique firm-level deltas
    deltas
end

type parDict{T}
    # Parameters
    #α::Vector{T}
    γ::Vector{T}
    β_0::Vector{T}
    β::Matrix{T}
    σ::Vector{T}
    #Random Coefficients stored (function of σ and draws)
    randCoeffs::Array{T,2}
    # δ values for (ij) pairs
    δ::Vector{T}
    # Non-delta utility for (ij) pairs and draws
    μ_ij::Matrix{T}
    # Shares for (ij) pairs
    s_hat::Vector{T}
end

function parDict{T}(m::InsuranceLogit,x::Array{T})
    # Parameter Lengths from model
    αlen = 0
    γlen = αlen + m.parLength[:γ]
    β0len = γlen + m.parLength[:β]
    βlen = β0len + m.parLength[:β]*m.parLength[:γ]
    σlen = βlen + (m.parLength[:σ] -1) # No σ value for price

    #Distribute Parameters
    #α = x[1:αlen]
    γ = x[(αlen+1):γlen]
    β_0 = x[(γlen+1):β0len]
    β_vec = x[(β0len+1):βlen]
    #σ = x[βlen+1:σlen]
    σ = [0]

    # Stack Beta into a matrix
    K = m.parLength[:β]
    N = m.parLength[:γ]
    β = Matrix{T}(K,N)
    ind = 0
    for i in 1:N, j in 1:K
        ind+=1
        β[j,i] = β_vec[ind]
    end

    #Calculate Random Coefficients matrix
    #Includes RC on Price, set to 0 atm
    (R,S) = size(m.draws)
    randCoeffs = Array{T,2}(m.parLength[:σ],S)
    calcRC!(randCoeffs,σ,m.draws)

    #Initialize (ij) pairs of deltas
    L, M = size(m.data.data)
    δ = Vector{T}(M)
    μ_ij = Matrix{T}(S,M)
    s_hat = Vector{T}(M)
    unpack_δ!(δ,m)

    return parDict{T}(γ,β_0,β,σ,randCoeffs,δ,μ_ij,s_hat)
end

function calcRC!{T,S}(randCoeffs::Array{S},σ::Array{T},draws::Array{Float64,2})
    (K, N) = size(randCoeffs)
    #randCoeffs[1,:] = draws[1,:].*σ[1]
    randCoeffs[1,:] = 0
    randCoeffs[2,:] = 0
    for k in 3:K,n in 1:N
        #randCoeffs[k,n] = draws[2,n]*σ[k-1]
        randCoeffs[k,n] = 0
    end
    return Void
end


function InsuranceLogit(data::ChoiceData,haltonDim::Int)
    # Construct the model instance

    # Get Parameter Lengths
    γlen = size(demoRaw(data),1)
    βlen = size(prodchars(data),1)
    σlen = βlen + 1

    parLength = Dict(:γ=>γlen,:β=>βlen,:σ=>σlen)

    # Initialize Halton Draws
    # These are the same across all individuals
    draws = permutedims(MVHaltonNormal(haltonDim,2),(2,1))

    # Initialize Empty value prediction objects
    n, k = size(c.data)
    # Copy Firm Level Data for Changing in Estimation
    pmat = c.pdata
    pmat[:delta] = 1.0
    sort!(pmat)

    d = InsuranceLogit(parLength,data,
                        draws,
                        pmat[:Product],pmat[:Share],pmat[:delta])
    return d
end



function calc_indCoeffs{T}(p::parDict{T},β::Array{T,1},d::T)
    Q = length(β)
    (K,N) = size(p.randCoeffs)
    β_i = Array{T,2}(Q,N)
    γ_i = Array{T,1}(N)
    for n in 1:N
        γ_i[n] = d + p.randCoeffs[1,n]
        for k in 1:Q
            β_i[k,n] = β[k] + p.randCoeffs[k+1,n]
        end
    end
    return β_i, γ_i
end

function individual_values!{T}(d::InsuranceLogit,p::parDict{T})
    # Store Parameters
    γ = p.γ
    β_0= p.β_0
    β = p.β
    δ_long = p.δ
    # Calculate μ_ij, which depends only on parameters
    for app in eachperson(d.data)
        # ind = person(app)[1]
        # X = permutedims(prodchars(app),(2,1))
        # X_0 = permutedims(prodchars0(app),(2,1))
        # Z = demoRaw(app)[:,1]
        # β_z = β*Z
        # demos = vecdot(γ,Z)
        # β_i, γ_i = calc_indCoeffs(p,β_z,demos)
        #
        # chars = X*β_i
        # chars_0 = X_0*β_0
        #
        # (K,N) = size(chars)
        # idxitr = d.data._personDict[ind]
        # for k = 1:K,n = 1:N
        #     #u = exp(chars[k,n] + α*price[k] + γ_i[n])
        #     u = exp(chars[k,n] + chars_0[k] + γ_i[n])
        #     p.μ_ij[n,idxitr[k]] = u
        # end

        burn = util_value!(app,p)
        # δ = δ_long[idxitr]
        # μ_ij = d.μ_ij[:,idxitr]
        # d.s_hat[idxitr] = individual_shares_RC(μ_ij,δ)
    end
    return Void
end

function util_value!{T}(app::ChoiceData,p::parDict{T})
    γ = p.γ
    β_0= p.β_0
    β = p.β

    ind = person(app)[1]
    X = permutedims(prodchars(app),(2,1))
    X_0 = permutedims(prodchars0(app),(2,1))
    Z = demoRaw(app)[:,1]
    β_z = β*Z
    demos = vecdot(γ,Z)
    β_i, γ_i = calc_indCoeffs(p,β_z,demos)

    chars = X*β_i
    chars_0 = X_0*β_0

    (K,N) = size(chars)
    idxitr = app._personDict[ind]
    for k = 1:K,n = 1:N
        #u = exp(chars[k,n] + α*price[k] + γ_i[n])
        u = exp(chars[k,n] + chars_0[k] + γ_i[n])
        p.μ_ij[n,idxitr[k]] = u
    end
    return p.μ_ij[:,idxitr]
end

# function grad_test(x)
#     pars = parDict(m,x)
#     return util_value!(app,pars,true)
# end
#


function util_gradient{T}(d::InsuranceLogit,app::ChoiceData,p::parDict{T})
    γ = p.γ
    β_0= p.β_0
    β = p.β
    p_num = length(p.γ) + length(p.β_0) + length(p.β) #+ length(p.σ)

    ind = person(app)[1]
    X = permutedims(prodchars(app),(2,1))
    X_0 = permutedims(prodchars0(app),(2,1))
    Z = demoRaw(app)[:,1]
    β_z = β*Z
    demos = vecdot(γ,Z)
    β_i, γ_i = calc_indCoeffs(p,β_z,demos)

    chars = X*β_i
    chars_0 = X_0*β_0

    # Create Gradient Matrix
    (K,N) = size(chars)
    grad = Array{T,3}(p_num,N,K)

    # γ_derivs = ones(K,length(γ))
    # β_0_derivs = X_0
    #char_derivs = Array{T,2}(K,p_num-length(p.σ))
    char_derivs = Array{T,2}(K,p_num)
    for k in 1:K
        char_derivs[k,1:3] = Z
    end
    char_derivs[:,4:7] = X_0
    char_derivs[:,8:11] = Z[1].*X
    char_derivs[:,12:15] = Z[2].*X
    char_derivs[:,16:19] = Z[3].*X
    #char_derivs = [transpose(repeat(Z,outer=(1,K))) X_0 Z[1].*X Z[2].*X Z[3].*X]

    # X_large = repeat(X[:,2:4],outer=(1,1,N))
    # nu_h_large = repeat(m.draws[2,:],inner=(1,length(p.β_0)-1,K))
    # rand_derivs = [transpose(m.draws[1,:]) transpose[m.draws[2,:]]]
    #
    for k = 1:K,n = 1:N
        #u = exp(chars[k,n] + α*price[k] + γ_i[n])
        u = exp(chars[k,n] + chars_0[k] + γ_i[n])
        for q in 1:p_num
            if q<=19
                grad[q,n,k] = char_derivs[k,q]*u
            elseif q==20
                grad[q,n,k] = d.draws[1,n]*u
            else
                grad[q,n,k] = X[k,q-20+1]*d.draws[2,n]*u
            end
        end
    end

    return grad
end

#
# function per_val_calc{T}(app::ChoiceData,p::parDict{T})
#     # Store Parameters
#     γ = p.γ
#     β = p.β
#     α = p.α[1]
#     δ_long = p.δ
#
#     #ind, X, price, Z, idxitr
#
#     # Caculate shares mean shares for individual i
#     ind = person(app)[1]
#     X = permutedims(prodchars(app),(2,1))
#     price = X[:,1]
#     Z = demoRaw(app)[:,1]
#     β_z = β*Z
#     demos = vecdot(γ,Z)
#     β_i, γ_i = calc_indCoeffs(p,β_z,demos)
#     chars = X*β_i
#
#     (K,N) = size(chars)
#     μ_ij = chars
#     for n = 1:N,k = 1:K
#         μ_ij[k,n] += α*price[k] + γ_i[n]
#     end
#
#     idxitr = app._personDict[ind]
#     δ = δ_long[idxitr]
#     s_hat = individual_shares_RC(μ_ij,δ)
#
#     return s_hat, ind
# end
#
# function per_val_calc(inputTuple::Tuple)
#     # Store Parameters
#     appData = inputTuple[1]
#     α       = inputTuple[2]
#     γ       = inputTuple[3]
#     β       = inputTuple[4]
#     δ       = inputTuple[5]
#     RCs     = inputTuple[6]
#
#     # Caculate shares mean shares for individual i
#     ind = appData[1,1]
#     X = permutedims(appData[2:5,:],(2,1))
#     price = X[:,1]
#     Z = appData[7:9,1]
#     β_z = β*Z
#     demos = vecdot(γ,Z)
#     β_i, γ_i = calc_indCoeffs(RCs,β_z,demos)
#     chars = X*β_i
#
#     (K,N) = size(chars)
#     μ_ij = chars
#     for n = 1:N,k = 1:K
#         μ_ij[k,n] += α*price[k] + γ_i[n]
#     end
#
#     s_hat = individual_shares_RC(μ_ij,δ)
#
#     return s_hat, ind
# end
#
# function per_val_calc{T}(idxitr::UnitRange,p::parDict{T},c::ChoiceData)
#     # Store Parameters
#     appData = c.data[:,idxitr]
#     α       = p.α[1]
#     γ       = p.γ
#     β       = p.β
#     δ       = p.δ[idxitr]
#     RCs     = p.randCoeffs
#
#     # Caculate shares mean shares for individual i
#     ind = appData[1,1]
#     X = permutedims(appData[2:5,:],(2,1))
#     price = X[:,1]
#     Z = appData[7:9,1]
#     β_z = β*Z
#     demos = vecdot(γ,Z)
#     β_i, γ_i = calc_indCoeffs(RCs,β_z,demos)
#     chars = X*β_i
#
#     (K,N) = size(chars)
#     μ_ij = chars
#     for n = 1:N,k = 1:K
#         μ_ij[k,n] += α*price[k] + γ_i[n]
#     end
#
#     s_hat = individual_shares_RC(μ_ij,δ)
#
#     return s_hat, idxitr
# end
#
# function parallel_values!{T}(d::InsuranceLogit,p::parDict{T})
#     #Collection of individual datasets
#     idList = Set(eachperson(d.data))
#     #Collection of Parameters
#     parList =[p]
#     while length(parList)<length(idList)
#         parList = vcat(parList,[p])
#     end
#     #Function for parallel caculations
#     res = pmap(per_val_calc,idList,parList)
#
#     #Combine Results
#     for (s,ind) in res
#             idxitr = d.data._personDict[ind]
#             d.s_hat[idxitr] = s
#     end
#     return Void
# end

function calc_shares{T}(μ_ij::Array{T},δ::Vector{T})
    (N,K) = size(μ_ij)
    util = Matrix{T}(K,N)
    s_hat = Matrix{T}(K,N)
    #s_mean = Vector{T}(K)
    for n in 1:N
        expsum = 1.0
        for i in 1:K
            a = μ_ij[n,i]*δ[i]
            util[i,n] = a
            expsum += a
        end
        for i in 1:K
            s_hat[i,n] = util[i,n]/expsum
        end
    end
    # for i in 1:K
    #     s_mean[i] = mean(s_hat[i,:])
    # end
    return mean(s_hat,2)
end

function individual_shares_RC{T}(d::InsuranceLogit,p::parDict{T})
    # Store Parameters
    δ_long = p.δ
    μ_ij_large = p.μ_ij
    for idxitr in values(d.data._personDict)
        δ = δ_long[idxitr]
        u = μ_ij_large[:,idxitr]
        s = calc_shares(u,δ)
        p.s_hat[idxitr] = s
    end
    return Void
end


# Calculate Log Likelihood
function log_likelihood{T}(d::InsuranceLogit,p::parDict{T})
    ll = 0.0
    Pop = 0.0
    γ = p.γ
    β = p.β
    #α = p.α[1]
    # Calculate μ_ij, which depends only on parameters
    for app in eachperson(d.data)
    #app = next(eachperson(d.data),100)[1]
        μ_ij = util_value!(app,p)
        ind = person(app)[1]
        S_ij = transpose(choice(app))
        wgt = transpose(weight(app))
        urate = transpose(unins(app))
        idxitr = d.data._personDict[ind]


        δ = p.δ[idxitr]
        s_hat = calc_shares(μ_ij,δ)
        s_insured = sum(s_hat)

        for i in eachindex(idxitr)
            ll+=wgt[i]*S_ij[i]*(log(s_hat[i]) -urate[i]*(log(s_insured)-log(1-s_insured)))
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
    p_num = length(p.γ) + length(p.β_0) + length(p.β) #+ length(p.σ)
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


function unpack_δ!{T}(δ::Vector{T},d::InsuranceLogit)
    for j in d.prods
        idx_j = d.data._productDict[j]
        for idx in idx_j
            δ[idx] = d.deltas[j]
        end
    end
    return Void
end

function convert_δ!(d::InsuranceLogit)
    J = length(d.deltas)
    deltas_new = Array{Float64}(J)
    for j in d.prods
        if isnan(d.deltas[j])
            deltas_new[j] = 0.0
        else
            deltas_new[j] = ForwardDiff.value(d.deltas[j])
        end
    end
    d.deltas = deltas_new
    return Void
end


# Update δ
@views sliceMean{T}(x::Vector{T},idx::Array{Int64,1}) = mean(x[idx])
@views function sliceMean_wgt{T}(x::Vector{T},w::Vector{Float64},idx::Array{Int64,1})
    wgts = w[idx]
    return sum(x[idx].*wgts)/sum(wgts)
end

function δ_update!{T}(d::InsuranceLogit,p::parDict{T};update::Bool=true)
    # Calculate overall marketshares and update δ_j
    eps = 0.0
    J = length(d.deltas)
    δ_new = Array{T,1}(J)
    rn = Array{T,1}(J)
    wgts = transpose(weight(d.data))[:,1]
    for j in d.prods
        j_index_all = d.data._productDict[j]
        #s_hat_j= mean(d.s_hat[j_index_all])
        s_hat_j= sliceMean_wgt(p.s_hat,wgts,j_index_all)
        s_j = d.shares[j]
        rn[j] = log(s_j)-log(s_hat_j)
        #d.deltas[j] += chg
        δ_new[j] = d.deltas[j]*(s_j/s_hat_j)

        #err = log(chg)
        eps = max(eps,abs(rn[j]))
    end
    if update
        d.deltas = δ_new
    end
    return eps,δ_new,rn
end

function contraction!{T}(d::InsuranceLogit,p::parDict{T};update::Bool=true)
    # Contraction...
    rnd = 0
    eps0 = 1
    tol = 5e-15
    individual_values!(d,p)
    while (eps0>tol) & (rnd<5000)
        rnd+=1
        #Step 1
        δ_init = d.deltas
        individual_shares_RC(d,p)
        eps0,δ_0,r0 = δ_update!(d,p)
        unpack_δ!(p.δ,d)


        #Step 2
        individual_shares_RC(d,p)
        eps1,δ_1,r1 = δ_update!(d,p)
        unpack_δ!(p.δ,d)

        #Update
        vn = r1 - r0
        #αn1 = vecdot(r0,vn)/vecdot(vn,vn)
        αn = vecdot(r0,r0)/vecdot(r0,vn)
        #αn3 = -vecdot(r0,r0)/vecdot(vn,vn)

        chg = - 2*αn.*r0 + αn^2.*vn
        δ_new = δ_init.*exp.(chg)
        #δ_new = δ_init - αn.*r0

        d.deltas = δ_new
        unpack_δ!(p.δ,d)
        if !update
            d.deltas = δ_init
            break
        end
        #eps = maximum(abs.(chg))
        # println("Contraction Error")
        #print("Intitial Error:  ")
        # println(eps0)
        # print("SquareM Error:  ")
        # println(eps)
    end
end


# function contraction!{T}(d::InsuranceLogit,p::parDict{T};update::Bool=true)
#     # Contraction...
#     rnd = 0
#     eps = 1
#     tol = 1e-10
#     individual_values!(d,p)
#     while (eps>tol) & (rnd<501)
#         rnd+=1
#         individual_shares_RC(d,p)
#         if rnd>1
#             update = true
#         end
#         if !update & rnd==1
#             eps,x,y = δ_update!(d,p,update=false)
#         else
#             eps,x,y = δ_update!(d,p)
#         end
#         unpack_δ!(p.δ,d)
#         # println("Contraction Error")
#         println(eps)
#     end
#     println(rnd)
# end

function contraction!{T}(d::InsuranceLogit,p_vec::Array{T,1};update::Bool=true)
    p = parDict(d,p_vec)
    return contraction!(d,p,update=update)
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


function estimate!(d::InsuranceLogit, p0)
    # Set up the optimization
    #opt = Opt(:LD_MMA, length(p0))
    #opt = Opt(:LN_NELDERMEAD, length(p0))
    #opt = Opt(:LD_TNEWTON_PRECOND_RESTART,length(p0))
    #opt = Opt(:LD_TNEWTON,length(p0))
    opt = Opt(:LN_SBPLX, length(p0))
    xtol_rel!(opt, 1e-6)
    xtol_abs!(opt, 1e-6)
    ftol_rel!(opt, 1e-8)
    #maxeval!(opt,2100)
    maxtime!(opt, 600000)
    #upper_bounds!(opt, ones(length(p0))/10)
    initial_step!(opt,1e-1)

    # Objective Function
    # ll(x) = evaluate_iteration!(d, x,update=false)
    # cfg = ForwardDiff.GradientConfig(ll, p0, ForwardDiff.Chunk{6}());
    ll(x) = log_likelihood(d,x)
    gmm(x) = GMM_objective(d,x)
    #δ_cont(x) = contraction!(d,x,update=false)
    δ_cont(x) = contraction!(d,x)
    count = 0
    function gmm(x, grad)
        count +=1
        println("Iteration $count at $x")
        #Store Gradient
        # println("Step 1")
        δ_cont(x)
        # println("Step 2")
        #ForwardDiff.gradient!(grad, gmm, x)
        # println("Gradient: $grad")
        obj = gmm(x)
        #obj = gmm(x)
        #likelihood = ll(x)
        println("Objective equals $obj on iteration $count")
        return -obj
    end

    # Set Objective
    max_objective!(opt, gmm)

    # Run Optimization
    minf, minx, ret = optimize(opt, p0)
    println("got $minf at $minx after $count iterations (returned $ret)")

    # Return the object
    return ret, minf, minx
end

function gradient_ascent(d,p0;max_step=1e-5,init_step=1e-9,max_itr=2000,grad_tol=1e2)
    ## Initialize Parameter Vector
    p_vec = p0
    # Step Size
    #max_step = 1e-7
    step = init_step
    # Likelihood Functions
    #ll(x) = log_likelihood(d,x)
    ll(x) = evaluate_iteration!(d,x,update=false)
    # Tracking Variables
    count = 0
    grad_size = 1e8
    tol = 1
    f_eval_old = 1.0
    # # Initialize δ
    param_dict = parDict(d,p_vec)
    contraction!(d,param_dict)
    cfg = ForwardDiff.GradientConfig(ll, p_vec, ForwardDiff.Chunk{6}());
    # Maximize by Gradient Ascent
    while (grad_size>grad_tol) & (count<max_itr)
        count+=1
        # Compute δ with Contraction
        println("Update δ")
        param_dict = parDict(d,p_vec)
        contraction!(d,param_dict)
        # Evaluate Likelihood
        f_eval = ll(p_vec)
        println("likelihood equals $f_eval on iteration $count")

        if ((count>1) & ((f_eval-f_eval_old)/f_eval_old > 0.02)) | isnan(f_eval)
            step = step_old/10
            p_vec = p_old + step.*grad_new
            println("Reset Parameters to $p_vecvec")
            step_old = copy(step)
            continue
        end

        # Compute Gradient, holding δ fixed
        grad_new = similar(p_vec)
        ForwardDiff.gradient!(grad_new, ll, p_vec,cfg)
        println("Gradient is $grad_new")

        #Save Iteration
        p_old = copy(p_vec)
        f_eval_old = copy(f_eval)
        step_old = copy(step)

        # Update Parameters
        p_vec += step.*grad_new
        println("Update Parameters to $p_vec")


        # New Step Size
        if count>1
            grad_diff = (grad_new-grad_old)
            step = abs(vecdot(step.*grad_new,grad_diff)/vecdot(grad_diff,grad_diff))
            println("New optimal step size: $step")
        end
        # Save Gradient
        grad_old = copy(grad_new)

        grad_size = sqrt(vecdot(grad_new,grad_new))
        println("Gradient Size: $grad_size")

        #Update step size
        step = min(step,max_step)
    end
    return p_vec
end


function newton_raphson(d,p0;max_step=1e-5,max_itr=2000)
    ## Initialize Parameter Vector
    p_vec = p0
    N = length(p0)
    # Step Size
    #max_step = 1e-7
    step = max_step
    # Likelihood Functions
    #ll(x) = log_likelihood(d,x)
    ll(x) = evaluate_iteration!(d,x,update=false)
    # Tracking Variables
    count = 0
    grad_size = 10000
    tol = 1
    f_eval_old = 1.0
    # # Initialize δ
    param_dict = parDict(d,p_vec)
    #cfg = ForwardDiff.GradientConfig(ll, p_vec, ForwardDiff.Chunk{4}())
    #contraction!(d,param_dict)
    # Maximize by Gradient Ascent
    while (grad_size>1000) & (count<max_itr)
        count+=1
        # Compute δ with Contraction
        println("Update δ")
        param_dict = parDict(d,p_vec)
        contraction!(d,param_dict)
        # Evaluate Likelihood
        f_eval = ll(p_vec)
        println("likelihood equals $f_eval on iteration $count")

        # if (count>1) & ((f_eval-f_eval_old)/f_eval_old > 0.02)
        #     println("Reset")
        #     step = step_old/10
        #     p_vec = p_old + step.*grad_new
        #     continue
        # end

        # Compute Gradient, holding δ fixed
        grad_new = similar(p_vec)
        ForwardDiff.gradient!(grad_new, ll, p_vec)
        println("Gradient is $grad_new")


        hess_new = Matrix{Float64}(N,N)
        ForwardDiff.hessian!(hess_new, ll, p_vec)
        println("Hessian is $hess_new")

        #Save Iteration
        p_old = copy(p_vec)
        f_eval_old = copy(f_eval)

        # Update Parameters
        p_vec += inv(hess_new)*grad_new
        p_vec += step.*grad_new
        println("Update Parameters to $p_vec")


        # New Step Size
        if count>1
            grad_diff = (grad_new-grad_old)
            step = abs(vecdot(step.*grad_new,grad_diff)/vecdot(grad_diff,grad_diff))
            println("New optimal step size: $step")
        end
        # Save Gradient
        grad_old = copy(grad_new)

        grad_size = sqrt(vecdot(grad_new,grad_new))
        println("Gradient Size: $grad_size")

        #Update step size
        step = min(step,max_step)
    end
    return p_vec
end
