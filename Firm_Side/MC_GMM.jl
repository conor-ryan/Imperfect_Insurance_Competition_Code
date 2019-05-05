using NLopt
using ForwardDiff

function GMM_objective(p::Vector{T},p_est::parDict{Float64},
                d::InsuranceLogit,c::MC_Data,W::Matrix{Float64};squared=false) where T
    par = parMC(p,p_est,d,c)
    individual_costs(d,par)
    moments = costMoments(c,d,par)
    # println(moments)
    if squared
        moments_sq = moments.^2
        obj = calc_GMM_Obj(moments_sq,W)
    else
        obj = calc_GMM_Obj(moments,W)
    end
    return obj
end

function GMM_test(x::Vector{T},p::Vector{Float64},p_est::parDict{Float64},
                d::InsuranceLogit,c::MC_Data,W::Matrix{Float64};squared=false) where T
    p_vec = Vector{T}(undef,length(p))
    p_vec[:] = p[:]
    p_vec[1:length(x)] = x[:]
    par = parMC(p_vec,p_est,d,c)
    individual_costs(d,par)
    moments = costMoments(c,d,par)
    if squared
        moments_sq = moments.^2
        obj = calc_GMM_Obj(moments_sq,W)
    else
        obj = calc_GMM_Obj(moments,W)
    end
    return obj
end

function GMM_objective!(obj_hess::Matrix{Float64},obj_grad::Vector{Float64},p::Array{T},p_est::parDict{Float64},d::InsuranceLogit,c::MC_Data,W::Matrix{Float64};squared=false) where T
    mom_grad = Matrix{Float64}(undef,c.par_length,c.mom_length)
    mom_hess = Array{Float64,3}(undef,c.par_length,c.par_length,c.mom_length)
    mom_grad_sq = Matrix{Float64}(undef,c.par_length,c.mom_length)
    mom_hess_sq = Array{Float64,3}(undef,c.par_length,c.par_length,c.mom_length)

    par = parMC(p,p_est,d,c)
    individual_costs(d,par)
    moments = costMoments!(mom_hess,mom_grad,c,d,par)

    if squared
        (K,M) = size(mom_grad)
        for k in 1:K
            mom_grad_sq[k,:] = 2 .*moments.*mom_grad[k,:]
            for l in 1:k
                hess_vec = 2 .*(mom_grad[k,:] .* mom_grad[l,:] .+ moments.*mom_hess[k,l,:] )
                mom_hess_sq[k,l,:] = hess_vec
                mom_hess_sq[l,k,:] = hess_vec
            end
        end
        moments_sq = moments.^2
        obj = calc_GMM_Obj(moments_sq,W)
        calc_GMM_Grad!(obj_grad,moments_sq,mom_grad_sq,W)
        calc_GMM_Hess!(obj_hess,moments_sq,mom_grad_sq,mom_hess_sq,W)
    else
        obj = calc_GMM_Obj(moments,W)
        calc_GMM_Grad!(obj_grad,moments,mom_grad,W)
        calc_GMM_Hess!(obj_hess,moments,mom_grad,mom_hess,W)
    end

    return obj
end

function GMM_objective!(obj_grad::Vector{Float64},p::Array{T},p_est::parDict{Float64},d::InsuranceLogit,c::MC_Data,W::Matrix{Float64};squared=false) where T
    mom_grad = Matrix{Float64}(undef,c.par_length,c.mom_length)
    mom_grad_sq = Matrix{Float64}(undef,c.par_length,c.mom_length)

    par = parMC(p,p_est,d,c)
    individual_costs(d,par)
    moments = costMoments!(mom_grad,c,d,par)

    if squared
        (K,M) = size(mom_grad)
        for k in 1:K
            mom_grad_sq[k,:] = 2 .*moments.*mom_grad[k,:]
        end
        moments_sq = moments.^2
        obj = calc_GMM_Obj(moments_sq,W)
        calc_GMM_Grad!(obj_grad,moments_sq,mom_grad_sq,W)
    else
        obj = calc_GMM_Obj(moments,W)
        calc_GMM_Grad!(obj_grad,moments,mom_grad,W)
    end

    return obj
end

function calc_GMM_Obj(moments::Vector{T},W::Matrix{Float64}) where T
    obj = 0.0
    for i in 1:length(moments), j in 1:length(moments)
        obj+= W[i,j]*moments[j]*moments[i]
        # obj+= W[i,j]*moments[j]^2*moments[i]^2
    end
    return obj
end

function calc_GMM_Grad!(obj_grad::Vector{Float64},
                    moments::Vector{Float64},
                    moments_grad::Matrix{Float64},
                    W::Matrix{Float64})
    Q = length(obj_grad)
    obj_grad[:] .= 0.0
    for k in 1:Q,i in 1:length(moments), j in 1:length(moments)
        obj_grad[k]+= W[i,j]*(moments[j]*moments_grad[k,i] + moments[i]*moments_grad[k,j])
        # obj_grad[k]+= W[i,j]*(moments[j]^2*2*moments[i]*moments_grad[k,i] + moments[i]^2*2*moments[j]*moments_grad[k,j])
    end
end

function calc_GMM_Hess_Large!(obj_hess::Matrix{Float64},
                    moments::Vector{Float64},
                    moments_grad::Matrix{Float64},
                    moments_hess::Array{Float64,3},
                    W::Matrix{Float64})
    hess_vec = W*moments
    grad_mat = moments_grad*W*transpose(moments_grad)
    obj_hess[:] .= 0.0
    Q,K = size(obj_hess)
    for k in 1:Q
        for l in 1:k
            for i in 1:length(moments)
                obj_hess[k,l]+= 2*(hess_vec[i]*moments_hess[k,l,i])
            end
            obj_hess[k,l]+= 2*grad_mat[k,l]
            if l<k
                obj_hess[l,k]=obj_hess[k,l]
            end
        end
    end
end
function calc_GMM_Hess!(obj_hess::Matrix{Float64},
                    moments::Vector{Float64},
                    moments_grad::Matrix{Float64},
                    moments_hess::Array{Float64,3},
                    W::Matrix{Float64})
    obj_hess[:] .= 0.0
    Q,K = size(obj_hess)
    for k in 1:Q
        for l in 1:k
            for i in 1:length(moments)
                @inbounds @fastmath @simd for j in 1:length(moments)
                    obj_hess[k,l]+= W[i,j]*(moments[i]*moments_hess[k,l,j] + moments[j]*moments_hess[k,l,i] + moments_grad[k,i]*moments_grad[l,j] + moments_grad[l,i]*moments_grad[k,j])
                end
            end
            if l<k
                obj_hess[l,k]=obj_hess[k,l]
            end
        end
    end
end
