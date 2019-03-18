using NLopt
using ForwardDiff

function GMM_objective(p::Vector{T},p_est::parDict{Float64},
                d::InsuranceLogit,c::MC_Data,W::Matrix{Float64}) where T
    par = parMC(p,p_est,d,c)
    individual_costs(d,par)
    moments = costMoments(c,d,par)
    obj = calc_GMM_Obj(moments,W)
    return obj
end

function GMM_objective!(obj_hess::Matrix{Float64},obj_grad::Vector{Float64},p::Array{T},d::InsuranceLogit,c::MC_Data,W::Matrix{Float64}) where T
    mom_grad = Matrix{Float64}(undef,c.par_length,c.mom_length)
    mom_hess = Array{Float64,3}(undef,c.par_length,c.par_length,c.mom_length)

    grad = Vector{Float64}(undef,length(p))
    hess = Matrix{Float64}(undef,length(p),length(p))

    par = parMC(p,p_est,d,c)
    individual_costs(d,par)
    costMoments!(mom_hess,mom_grad,c,d,par)

    obj = calc_GMM_Obj(moments,W)
    calc_GMM_Grad!(grad,moments,mom_grad,W)
    calc_GMM_Hess_Large!(hess,moments,mom_grad,mom_hess,W)

    return obj
end

function calc_GMM_Obj(moments::Vector{T},W::Matrix{Float64}) where T
    obj = 0.0
    for i in 1:length(moments), j in 1:length(moments)
        obj+= W[i,j]*moments[j]*moments[i]
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

function estimate_GMM(p0::Vector{Float64},p_est::parDict{Float64},
                d::InsuranceLogit,c::MC_Data,W::Matrix{Float64};method=:LD_MMA,bounded=false)
    # Set up the optimization
    # opt = Opt(:LD_MMA, length(p0))
    # opt = Opt(:LD_TNEWTON_PRECOND_RESTART, length(p0))
    opt = Opt(method, length(p0))

    #maxeval!(opt_stage1,20000)
    maxtime!(opt, 580000)
    ftol_rel!(opt,1e-8)

    lb = repeat([-50],inner=length(p0))
    # # lb[14] = 0.0
    ub = repeat([50],inner=length(p0))
    # # ub[14] = .99
    #
    if bounded
        lower_bounds!(opt, lb)
        upper_bounds!(opt, ub)
    end

    gmm(x) = GMM_objective(x,p_est,d,c,W)
    grad = Vector{Float64}(undef,length(p0))
    # println(d.draws[1:30,:])
    disp_length = min(20,length(p0))
    count = 0
    function gmm(x, grad)
        count +=1
        x_displ = x[1:disp_length]
        println("Iteration $count at $x_displ")
        obj = gmm(x)
        # ForwardDiff.gradient!(grad, gmm, x)
        # grad_size = sqrt(dot(grad,grad))
        # println("Gradient size equals $grad_size")

        println("Objective equals $obj on iteration $count")

        return obj
    end

    # Set Objective
    min_objective!(opt, gmm)


    minf, minx, ret= optimize(opt, p0)


    println("Got $minf at $minx after $count iterations (returned $ret)")

    # Return the object
    return ret, minf, minx
end
