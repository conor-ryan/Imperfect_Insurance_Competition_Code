using NLopt
using ForwardDiff


function estimate!(d::InsuranceLogit, p0;method=:LD_TNEWTON_PRECOND_RESTART)
    # Set up the optimization
    opt_stage1 = Opt(:LD_TNEWTON_PRECOND_RESTART, length(p0))
    opt_stage2 = Opt(:LD_MMA,length(p0))
    #opt = Opt(:LD_MMA, length(p0))

    #opt = Opt(:LD_TNEWTON_PRECOND_RESTART,length(p0))
    #opt = Opt(:LD_TNEWTON,length(p0))
    #opt = Opt(:LN_SBPLX, length(p0))
    #opt = Opt(:LN_COBYLA, length(p0))

    #maxeval!(opt_stage1,500)
    ftol_rel!(opt_stage1,1e-8)

    # xtol_rel!(opt_stage2, 1e-6)
    # xtol_abs!(opt_stage2, 1e-6)
    # ftol_rel!(opt_stage2, 1e-10)
    maxtime!(opt_stage2, 500000)

    lb = repeat([-Inf],inner=length(p0))
    # lb[14] = 0.0
    ub = repeat([Inf],inner=length(p0))
    # ub[14] = .99

    lower_bounds!(opt_stage1, lb)
    upper_bounds!(opt_stage1, ub)
    # lower_bounds!(opt_stage2, lb)
    # upper_bounds!(opt_stage2, ub)


    # initial_step!(opt_stage2,1e-1)
    #stopval!(opt,.00040)
    # Objective Function
    # ll(x) = evaluate_iteration!(d, x,update=false)
    # cfg = ForwardDiff.GradientConfig(ll, p0, ForwardDiff.Chunk{6}());
    ll(x) = log_likelihood(d,x)
    ll_grad!(grad,x) = log_likelihood!(grad,d,x)
    #gmm(x) = GMM_objective(d,x)
    # println(d.draws[1:30,:])
    disp_length = min(20,length(p0))
    count = 0
    function ll(x, grad)
        count +=1
        x_displ = x[1:disp_length]
        if count % 50 ==0
            x_displ = round.(x,1)
            println(find(abs.(x).>10))
        end
        println("Iteration $count at $x_displ")
        #obj = ll(x)
        obj = ll_grad!(grad,x)

        grad_size = sqrt(vecdot(grad,grad))
        println("Gradient size equals $grad_size")
        if count % 50 ==0
            grad_displ = round.(grad,4)
            println(grad_displ)
            println(find(abs.(grad).>1))
        end
        # grad_mean = mean(grad)
        # grad_median = std(grad)
        # println("Gradient mean equals $grad_mean")
        # println("Gradient median equals $grad_median")
        # grad_displ = grad[1:20]
        # println("Graident is $grad_displ")
        #ForwardDiff.gradient!(grad, ll, x)
        #println("Gradient equals $grad")
        #likelihood = ll(x)
        println("Objective equals $obj on iteration $count")

        return obj
    end
    # Set Objective
    max_objective!(opt_stage1, ll)
    # max_objective!(opt_stage2, ll)

    # Run Optimization
    minf, minx, ret = optimize(opt_stage1, p0)
    println("In Stage 1, got $minf at $minx after $count iterations (returned $ret)")
    # count = 0
    # minf, minx, ret = optimize(opt_stage2, init_minx)
    # println("In Stage 1, got $minf at $minx after $count iterations (returned $ret)")

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


function newton_raphson(d,p0;grad_tol=1e-12,step_tol=1e-8,max_itr=2000)
    ## Initialize Parameter Vector
    p_vec = p0
    N = length(p0)

    count = 0
    grad_size = 10000
    f_eval_old = 1.0
    # # Initialize δ
    param_dict = parDict(d,p_vec)

    # Initialize Gradient
    grad_new = similar(p0)
    hess_new = Matrix{Float64}(length(p0),length(p0))
    f_final_val = 0.0
    max_trial_cnt = 0
    # Maximize by Newtons Method
    while (grad_size>grad_tol) & (count<max_itr) & max_trial_cnt<20
        count+=1

        # Compute Gradient, holding δ fixed

        fval = log_likelihood!(hess_new,grad_new,d,p_vec)

        # ForwardDiff.gradient!(grad_new, ll, p_vec)
        # println("Gradient is $grad_new")
        #
        #
        # hess_new = Matrix{Float64}(N,N)
        # ForwardDiff.hessian!(hess_new, ll, p_vec)
        # println("Hessian is $hess_new")

        step = - inv(hess_new)*grad_new

        p_test = p_vec .+ step
        f_test = log_likelihood(d,p_test)
        trial_cnt = 0
        while ((f_test<fval) | isnan(f_test)) & (trial_cnt<10)
            p_test_disp = p_test[1:20]
            println("Trial: Got $f_test at parameters $p_test_disp")
            step/= 10
            p_test = p_vec .+ step
            f_test = log_likelihood(d,p_test)
            trial_cnt+=1
            if trial_cnt==10
                println("Algorithm Stalled: Random Step")
                max_trial_cnt+=1
                step = rand(length(step))/100-.005
            end
        end
        p_vec+= step
        p_vec_disp = p_vec[1:20]
        f_final_val = f_test
        println("Update Parameters to $p_vec_disp")

        grad_size = sqrt(vecdot(grad_new,grad_new))
        println("Gradient Size: $grad_size")
        println("Function Value is $f_test at iteration $count")

        if (grad_size<1e-12) & (count>10)
            println("Got to Break Point...?")
            break
        end
    end
    # if (grad_size>grad_tol)
    #     println("Estimate Instead")
    #     ret, f_final_val, p_vec = estimate!(d,p0)


    return p_vec,f_final_val
end
