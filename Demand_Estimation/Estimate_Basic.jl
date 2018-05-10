using NLopt
using ForwardDiff


function estimate!(d::InsuranceLogit, p0;method=:LN_NELDERMEAD)
    # Set up the optimization
    #opt = Opt(method, length(p0))
    opt = Opt(:LD_MMA, length(p0))

    #opt = Opt(:LD_TNEWTON_PRECOND_RESTART,length(p0))
    #opt = Opt(:LD_TNEWTON,length(p0))
    #opt = Opt(:LN_SBPLX, length(p0))
    #opt = Opt(:LN_COBYLA, length(p0))

    xtol_rel!(opt, 1e-6)
    xtol_abs!(opt, 1e-6)
    ftol_rel!(opt, 1e-10)
    #maxeval!(opt,2100)
    maxtime!(opt, 600000)
    #upper_bounds!(opt, ones(length(p0))/10)
    initial_step!(opt,1e-1)
    #stopval!(opt,.00040)
    # Objective Function
    # ll(x) = evaluate_iteration!(d, x,update=false)
    # cfg = ForwardDiff.GradientConfig(ll, p0, ForwardDiff.Chunk{6}());
    ll(x) = log_likelihood(d,x)
    ll_grad!(grad,x) = ll_gradient!(grad,d,x)
    #gmm(x) = GMM_objective(d,x)

    count = 0
    function ll(x, grad)
        count +=1

        x_displ = x[1:20]
        println("Iteration $count at $x_displ")

        obj = ll(x)
        ll_grad!(grad,x)

        grad_size = sqrt(vecdot(grad,grad))
        println("Gradient size equals $grad_size")
        #ForwardDiff.gradient!(grad, ll, x)
        #println("Gradient equals $grad")
        #likelihood = ll(x)
        println("Objective equals $obj on iteration $count")
        return obj
    end

    # Set Objective
    max_objective!(opt, ll)

    # Run Optimization
    minf, minx, ret = optimize(opt, p0)
    println("got $minf at $minx after $count iterations (returned $ret)")

    # Return the object
    return ret, minf, minx
end

function gradient_ascent(d,p0;max_step=1e-3,init_step=1e-9,max_itr=2000,grad_tol=1e2)
    ## Initialize Parameter Vector
    p_vec = p0
    # Step Size
    #max_step = 1e-7
    step = init_step
    # Likelihood Functions
    ll(x) = log_likelihood(d,x)
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
            println("Reset Parameters to $p_vec")
            step_old = copy(step)
            continue
        end

        # Compute Gradient, holding δ fixed
        grad_new = similar(p_vec)
        ForwardDiff.gradient!(grad_new, ll, p_vec)
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
