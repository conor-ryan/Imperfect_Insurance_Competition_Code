using NLopt
using ForwardDiff

function GMM_objective!{T}(obj_grad::Vector{Float64},d::InsuranceLogit,p0::Array{T},W::Matrix{Float64})
    grad = Vector{Float64}(length(p0))
    hess = Matrix{Float64}(length(p0),length(p0))
    par0 = parDict(d,p0)
    ll = log_likelihood!(hess,grad,d,par0)

    mom_grad = Matrix{Float64}(length(p0),length(d.data.tMoments))
    mom = calc_risk_moments!(mom_grad,d,par0)

    moments = vcat(mom,grad)
    moments_grad = hcat(mom_grad,hess)

    #W = eye(length(d.data.tMoments)+length(p0))

    # moments = grad
    # moments_grad = hess
    #
    # W = eye(length(p0))
    obj = 0.0
    for i in 1:length(moments), j in 1:length(moments)
        obj+= W[i,j]*moments[j]*moments[i]
    end

    obj_grad[:] = 0.0
    for k in 1:length(p0),i in 1:length(moments), j in 1:length(moments)
        obj_grad[k]+= W[i,j]*(moments[j]*moments_grad[k,i] + moments[i]*moments_grad[k,j])
    end


    # obj = moments'*W*moments
    #
    # obj_grad[:] = moments_grad*W*moments + moments_grad*W'*moments

    return obj
end

function GMM_objective{T}(d::InsuranceLogit,p0::Array{T})
    par0 = parDict(d,p0)
    grad = Vector{T}(length(p0))
    ll = log_likelihood!(grad,d,par0)
    mom = calc_risk_moments(d,par0)

    moments = vcat(grad,mom)
    W = eye(length(d.data.tMoments)+length(p0))

    # moments = grad
    # W = eye(length(p0))

    obj = moments'*W*moments
    return obj
end

function GMM_objective!{T}(obj_hess::Matrix{Float64},obj_grad::Vector{Float64},d::InsuranceLogit,p0::Array{T},W::Matrix{Float64})
    grad = Vector{Float64}(length(p0))
    hess = Matrix{Float64}(length(p0),length(p0))
    thD = Array{Float64,3}(length(p0),length(p0),length(p0))
    par0 = parDict(d,p0)
    ll = log_likelihood!(thD,hess,grad,d,par0)



    mom_grad = Matrix{Float64}(length(p0),length(d.data.tMoments))
    mom_hess = Array{Float64,3}(length(p0),length(p0),length(d.data.tMoments))
    mom = calc_risk_moments!(mom_hess,mom_grad,d,par0)

    moments = vcat(mom,grad)
    moments_grad = hcat(mom_grad,hess)
    moments_hess = cat(3,thD,mom_hess)

    obj = calc_GMM_Obj(moments,W)

    calc_GMM_Grad!(obj_grad,moments,moments_grad,W)
    calc_GMM_Hess!(obj_hess,moments,moments_grad,moments_hess,W)

    return obj
end

function calc_GMM_Obj(moments::Vector{Float64},W::Matrix{Float64})
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
    obj_grad[:] = 0.0
    for k in 1:Q,i in 1:length(moments), j in 1:length(moments)
        obj_grad[k]+= W[i,j]*(moments[j]*moments_grad[k,i] + moments[i]*moments_grad[k,j])
    end
end

function calc_GMM_Hess!(obj_hess::Matrix{Float64},
                    moments::Vector{Float64},
                    moments_grad::Matrix{Float64},
                    moments_hess::Array{Float64,3},
                    W::Matrix{Float64})
    obj_hess[:] = 0.0
    Q,K = size(obj_hess)
    for k in 1:Q
        for l in 1:k
            for i in 1:length(moments), j in 1:length(moments)
                @inbounds @fastmath obj_hess[k,l]+= W[i,j]*(moments[i]*moments_hess[k,l,j] + moments[j]*moments_hess[k,l,i] + moments_grad[k,i]*moments_grad[l,j] + moments_grad[l,i]*moments_grad[k,j])
            end
            if l<k
                obj_hess[l,k]=obj_hess[k,l]
            end
        end
    end
end

function GMM_objective{T}(d::InsuranceLogit,p0::Array{T},W::Matrix{Float64})
    par0 = parDict(d,p0)
    grad = Vector{T}(length(p0))
    ll = log_likelihood!(grad,d,par0)
    mom = calc_risk_moments(d,par0)

    moments = vcat(grad,mom)

    obj = moments'*W*moments
    return obj
end


function estimate_GMM!(d::InsuranceLogit, p0::Vector{Float64},W::Matrix{Float64};method=:LN_NELDERMEAD)
    # Set up the optimization
    #opt_stage1 = Opt(:LD_LBFGS, length(p0))
    opt_stage1 = Opt(:LD_TNEWTON_PRECOND_RESTART, length(p0))
    #opt = Opt(:LD_MMA, length(p0))

    #opt = Opt(:LD_TNEWTON_PRECOND_RESTART,length(p0))
    #opt = Opt(:LD_TNEWTON,length(p0))
    #opt = Opt(:LN_SBPLX, length(p0))
    #opt = Opt(:LN_COBYLA, length(p0))

    #maxeval!(opt_stage1,20000)
    maxtime!(opt_stage1, 580000)
    ftol_rel!(opt_stage1,1e-8)

    lb = repeat([-1000],inner=length(p0))
    # # lb[14] = 0.0
    ub = repeat([1000],inner=length(p0))
    # # ub[14] = .99
    #
    lower_bounds!(opt_stage1, lb)
    upper_bounds!(opt_stage1, ub)


    #gmm(x) = GMM_objective(d,x,W)
    gmm_grad!(grad,x) = GMM_objective!(grad,d,x,W)
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
    # maxeval!(opt_stage1,10)
    #
    # lb = repeat([-5],inner=length(p0))
    # # lb[14] = 0.0
    # ub = repeat([5],inner=length(p0))
    # # ub[14] = .99
    # lower_bounds!(opt_stage1, lb)
    # upper_bounds!(opt_stage1, ub)
    # minf, minx, ret= optimize(opt_stage1, p0)

    maxeval!(opt_stage1,25000)
    #lb = repeat([-Inf],inner=length(p0))
    # lb[14] = 0.0
    #ub = repeat([Inf],inner=length(p0))
    # ub[14] = .99
    lower_bounds!(opt_stage1, lb)
    upper_bounds!(opt_stage1, ub)
    minf, minx, ret= optimize(opt_stage1, p0)


    println("In Stage 1, got $minf at $minx after $count iterations (returned $ret)")

    # Return the object
    return ret, minf, minx
end



function newton_raphson(d::InsuranceLogit,p0::Vector{Float64},W::Matrix{Float64};f_tol=1e-2,step_tol=1e-8,max_itr=10)
    ## Initialize Parameter Vector
    p_vec = p0
    N = length(p0)

    count = 0
    fval = 10000
    f_eval_old = 1.0
    # # Initialize δ
    param_dict = parDict(d,p_vec)

    # Initialize Gradient
    grad_new = similar(p0)
    f_final_val = 0.0
    max_trial_cnt = 0
    # Maximize by Newtons Method
    while (fval>f_tol) & (count<max_itr) & (max_trial_cnt<20)
        count+=1

        # Compute Gradient, holding δ fixed
        fval = GMM_objective!(grad_new,d,p_vec,W)

        if (fval<1e-2) & (count>10)
            println("Got to Break Point...?")
            break
        end

        # ForwardDiff.gradient!(grad_new, ll, p_vec)
        # println("Gradient is $grad_new")
        #
        #
        # hess_new = Matrix{Float64}(N,N)
        # ForwardDiff.hessian!(hess_new, ll, p_vec)
        # println("Hessian is $hess_new")
        grad_size = sqrt(vecdot(grad_new,grad_new))

        step = -fval./grad_new

        p_test = p_vec .+ step
        f_test = GMM_objective(d,p_test,W)
        trial_cnt = 0
        while ((f_test>fval) | isnan(f_test)) & (trial_cnt<10)
            p_test_disp = p_test[1:20]
            println("Trial: Got $f_test at parameters $p_test_disp")
            println("Previous Iteration at $fval")
            step/= 10
            p_test = p_vec .+ step
            f_test = GMM_objective(d,p_test,W)
            trial_cnt+=1
            if (trial_cnt==10) & (fval>100)
                println("Algorithm Stalled: Random Step")
                max_trial_cnt+=1
                step = rand(length(step))/1000-.005
            elseif (trial_cnt==10) & (fval<=100)
                println("Algorithm Stalled: Random Step")
                max_trial_cnt+=1
                step = rand(length(step))/10000-.005
            end
        end
        p_vec+= step
        p_vec_disp = p_vec[1:20]
        f_final_val = f_test
        println("Update Parameters to $p_vec_disp")


        println("Gradient Size: $grad_size")
        println("Function Value is $f_test at iteration $count")
    end
    # if (grad_size>grad_tol)
    #     println("Estimate Instead")
    #     ret, f_final_val, p_vec = estimate!(d,p0)


    return p_vec,f_final_val
end
