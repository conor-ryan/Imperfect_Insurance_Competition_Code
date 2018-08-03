using NLopt
using ForwardDiff

function GMM_objective!{T}(obj_grad::Vector{Float64},d::InsuranceLogit,p0::Array{T})
    grad = Vector{Float64}(length(p0))
    hess = Matrix{Float64}(length(p0),length(p0))
    par0 = parDict(d,p0)
    ll = log_likelihood!(hess,grad,d,par0)

    mom_grad = Matrix{Float64}(length(p0),length(d.data.tMoments))
    mom = calc_risk_moments!(mom_grad,d,par0)

    moments = vcat(grad,mom)
    moments_grad = hcat(hess,mom_grad)

    W = eye(length(d.data.tMoments)+length(p0))

    obj = moments'*W*moments

    obj_grad[:] = moments_grad*W*moments + moments_grad*W'*moments
    return obj
end

function GMM_objective{T}(d::InsuranceLogit,p0::Array{T})
    par0 = parDict(d,p0)
    grad = Vector{T}(length(p0))
    ll = log_likelihood!(grad,d,par0)
    mom = calc_risk_moments(d,par0)

    moments = vcat(grad,mom)
    W = eye(length(d.data.tMoments)+length(p0))

    obj = moments'*W*moments
    return obj
end


function estimate_GMM!(d::InsuranceLogit, p0;method=:LN_NELDERMEAD)
    # Set up the optimization
    opt_stage1 = Opt(:LD_TNEWTON_PRECOND_RESTART, length(p0))
    #opt = Opt(:LD_MMA, length(p0))

    #opt = Opt(:LD_TNEWTON_PRECOND_RESTART,length(p0))
    #opt = Opt(:LD_TNEWTON,length(p0))
    #opt = Opt(:LN_SBPLX, length(p0))
    #opt = Opt(:LN_COBYLA, length(p0))

    #maxeval!(opt_stage1,20000)
    maxtime!(opt_stage1, 500000)
    ftol_rel!(opt_stage1,1e-8)

    lb = repeat([-Inf],inner=length(p0))
    # lb[14] = 0.0
    ub = repeat([Inf],inner=length(p0))
    # ub[14] = .99

    lower_bounds!(opt_stage1, lb)
    upper_bounds!(opt_stage1, ub)


    gmm(x) = GMM_objective(d,x)
    gmm_grad!(grad,x) = GMM_objective!(grad,d,x)
    # println(d.draws[1:30,:])
    disp_length = min(20,length(p0))
    count = 0
    function gmm(x, grad)
        count +=1
        x_displ = x[1:disp_length]
        if count % 50 ==0
            x_displ = round.(x,1)
            println(find(abs.(x).>10))
        end
        println("Iteration $count at $x_displ")
        #obj = ll(x)
        obj = gmm_grad!(grad,x)
        grad_size = sqrt(vecdot(grad,grad))
        println("Gradient size equals $grad_size")
        #ForwardDiff.gradient!(grad, gmm, x)

        println("Objective equals $obj on iteration $count")

        return obj
    end
    # Set Objective
    min_objective!(opt_stage1, gmm)

    # Run Optimization
    minf, minx, ret= optimize(opt_stage1, p0)
    println("In Stage 1, got $minf at $minx after $count iterations (returned $ret)")

    # Return the object
    return ret, minf, minx
end
