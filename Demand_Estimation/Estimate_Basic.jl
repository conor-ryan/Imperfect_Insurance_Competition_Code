# using NLopt
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

        grad_size = sqrt(dot(grad,grad))
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


function gradient_ascent_ll(d,p0;grad_tol=1e-8,f_tol=1e-8,x_tol=1e-8,max_itr=2000,strict=false)
    ## Initialize Parameter Vector
    p_vec = p0
    N = length(p0)

    cnt = 0
    grad_size = 10000
    f_eval_old = 1.0
    # # Initialize δ
    param_dict = parDict(d,p_vec,no2Der=true)

    # Initialize Gradient
    grad_new = similar(p0)
    hess_new = Matrix{Float64}(undef,length(p0),length(p0))
    f_final_val = 0.0
    max_trial_cnt = 0
    p_last = copy(p_vec)
    grad_last = copy(grad_new)
    disp_length = min(length(p0),20)
    f_min = -1e3
    p_min  = similar(p_vec)
    no_progress=0
    flag = "empty"
    if strict
        mistake_thresh = 1.00
    else
        mistake_thresh = 1.25
    end

    ## Tolerance Counts
    f_tol_cnt = 0
    x_tol_cnt = 0
    # Maximize by Newtons Method
    while (cnt<max_itr)
        cnt+=1
        trial_cnt=0


        # Compute Gradient, holding δ fixed
        fval = log_likelihood!(grad_new,d,p_vec)
        if (cnt==1) | (fval>f_min)
            if abs(fval-f_min)<f_tol
                f_tol_cnt += 1
            end
            if maximum(abs.(p_vec - p_min))<x_tol
                x_tol_cnt += 1
            end

            f_min = copy(fval)
            p_min[:] = p_vec[:]

            no_progress=0
        else
            no_progress+=1
        end


        grad_size = maximum(abs.(grad_new))
        if (grad_size<grad_tol) |(f_tol_cnt>1) | (x_tol_cnt>1)
            println("Got to Break Point")
            println(grad_size)
            println(f_tol_cnt)
            println(x_tol_cnt)
            flag = "converged"
            break
        end
        if strict==false
            if grad_size>.1
                mistake_thresh = 1.25
            else
                mistake_thresh = 1.05
            end
        end

        if cnt==1
            step = 1/grad_size
        else
            g = p_vec - p_last
            y = grad_new - grad_last
            step = abs.(dot(g,g)/dot(g,y))
        end

        if no_progress>10
            no_progress = 0
            println("Return: Limit on No Progress")
            p_vec = copy(p_min)
            fval = log_likelihood!(grad_new,d,p_vec)
            grad_size = maximum(abs.(grad_new))
            step = 1/grad_size
            mistake_thresh = 1.00
        end



        p_test = p_vec .+ step.*grad_new

        f_test = log_likelihood(d,p_test)
        println("Initial Step Size: $step")
        while ((f_test<fval*mistake_thresh) | isnan(f_test)) & (step>1e-15)
            p_test_disp = p_test[1:disp_length]
            if trial_cnt==0
                println("Trial: Got $f_test at parameters $p_test_disp")
                println("Previous Iteration at $fval")
                println("Reducing Step Size...")
            end
            step/= 20
            p_test = p_vec .+ step.*grad_new
            f_test = log_likelihood(d,p_test)
            trial_cnt+=1
        end

        p_last = copy(p_vec)
        p_vec = copy(p_test)
        grad_last = copy(grad_new)
        p_vec_disp = p_vec[1:20]
        f_final_val = fval
        println("Update Parameters to $p_vec_disp")


        println("Gradient Size: $grad_size")
        println("Step Size: $step")
        println("Function Value is $f_test at iteration $cnt")
        println("Steps since last improvement: $no_progress")
    end
    println("Lowest Function Value is $f_min at $p_min")
    return p_min,f_min
end


function newton_raphson_ll(d,p0;grad_tol=1e-8,f_tol=1e-8,x_tol=1e-8,
    max_itr=2000,strict=true,Hess_Skip_Steps=15)
    ## Initialize Parameter Vector
    p_vec = p0
    N = length(p0)

    cnt = 0
    grad_size = 10000
    f_eval_old = 1.0
    # # Initialize δ
    param_dict = parDict(d,p_vec,no2Der=true)

    # Initialize Gradient
    grad_new = similar(p0)
    hess_new = Matrix{Float64}(undef,length(p0),length(p0))
    Eye = Matrix{Float64}(1.0I,length(p0),length(p0))
    f_final_val = 0.0
    max_trial_cnt = 0
    NaN_steps = 0
    trial_end = 5
    hess_steps = 0
    p_last = copy(p_vec)
    grad_last = copy(grad_new)
    H_last = copy(hess_new)
    disp_length = min(length(p0),20)
    f_min = -1e3
    p_min  = similar(p_vec)
    no_progress=0
    flag = "empty"
    if strict
        mistake_thresh = 1.00
    else
        mistake_thresh = 1.25
    end

    ## Initialize Step
    step = 1
    real_hessian=0

    ## Tolerance Counts
    f_tol_cnt = 0
    x_tol_cnt = 0
    # Maximize by Newtons Method
    while (grad_size>grad_tol) & (cnt<max_itr) & (max_trial_cnt<20)
        cnt+=1
        trial_cnt=0

        # Compute Gradient, holding δ fixed
        if hess_steps==0
            println("Compute Hessian")
            fval = log_likelihood!(hess_new,grad_new,d,p_vec)
            H_k = inv(hess_new)
            real_hessian=1
        else
            println("BFGS Approximation")
            fval = log_likelihood!(grad_new,d,p_vec)
            Δxk = p_vec - p_last
            yk = grad_new - grad_last
            # Δhess =  (yk*yk')./(yk'*Δxk) - (hess_new*Δxk*(hess_new*Δxk)')./(Δxk'*hess_new*Δxk)
            # hess_new = hess_new + (yk*yk')./(yk'*Δxk) - (yk*yk')./(yk'*Δxk) - (hess_new*Δxk*(hess_new*Δxk)')./(Δxk'*hess_new*Δxk)
            H_k = (Eye - (Δxk*yk')./(yk'*Δxk) )*H_last*(Eye - (yk*Δxk')./(yk'*Δxk) ) + (Δxk*Δxk')./(yk'*Δxk)
            real_hessian=0
        end

        if (cnt==1) | (fval>f_min)
            if abs(fval-f_min)<f_tol
                f_tol_cnt += 1
            end
            if maximum(abs.(p_vec - p_min))<x_tol
                x_tol_cnt += 1
            end

            f_min = copy(fval)
            p_min[:] = p_vec[:]

            no_progress=0
        else
            no_progress+=1
        end


        grad_size = maximum(abs.(grad_new))
        if (grad_size<grad_tol) |(f_tol_cnt>1) | (x_tol_cnt>1)
            println("Got to Break Point...?")
            println(grad_size)
            println(f_tol_cnt)
            println(x_tol_cnt)
            flag = "converged"
            break
        end
        if strict==false
            if grad_size>.1
                mistake_thresh = 1.25
            else
                mistake_thresh = 1.05
            end
        end

        update = -H_k*grad_new
        if any(isnan.(update))
            println("Step contains NaN")
            NaN_steps+=1
            if real_hessian==0
                println("No Advancement")
                hess_steps = 0
                update = 0
                grad_size = 100
            else
                println("ALGORITHM FAILED! SINGULAR HESSIAN!")
                return p_min,f_min
            end
        else
            NaN_steps = 0
        end


        if no_progress>5
            no_progress = 0
            hess_steps = 0
            println("Return: Limit on No Progress")
            p_vec = copy(p_min)
            fval = log_likelihood!(grad_new,d,p_vec)
            grad_size = maximum(abs.(grad_new))
            step = 1/grad_size
            update = - step.*grad_new
            mistake_thresh = 1.00
        end

        step_size = maximum(abs.(update))
        if step_size>10
            update = update./step_size
            ind = findall(abs.(update).==1)
            val_disp = p_vec[ind]
            println("Max Parameter Adjustment: $ind, $val_disp")
            step_size = 1
        end


        p_test = p_vec .+ update
        f_test = log_likelihood(d,p_test)

        if hess_steps<Hess_Skip_Steps
            hess_steps+=1
        else
            hess_steps=0
        end

        trial_max = 0
        while ((f_test<fval*mistake_thresh) | isnan(f_test)) & (trial_max==0)
            if trial_cnt==0
                p_test_disp = p_test[1:20]
                println("Trial (Init): Got $f_test at parameters $p_test_disp")
                println("Previous Iteration at $fval")
            end
            if trial_cnt<=2
                update/= 20
            else
                update/= 200
            end
            step_size = maximum(abs.(update))
            if (step_size>x_tol)
                p_test = p_vec .+ update
                f_test = log_likelihood(d,p_test)
                p_test_disp = p_test[1:20]
                println("Trial (NR): Got $f_test at parameters $p_test_disp")
                println("Previous Iteration at $fval")
                trial_cnt+=1
            elseif real_hessian==1
                hess_steps = 0
                trial_max = 1
                println("RUN ROUND OF GRADIENT ASCENT")
                p_test, f_test = gradient_ascent_ll(d,p_vec,max_itr=5,strict=true)
            else
                println("No Advancement")
                hess_steps = 0
                p_test = copy(p_vec)
                break
            end
        end
        if NaN_steps>5
            println("Hessian might be singular")
            println("RUN ROUND OF GRADIENT ASCENT")
            p_test, f_test = gradient_ascent_ll(d,p_test,max_itr=20,strict=true)
        end

        p_last = copy(p_vec)
        p_vec = copy(p_test)
        grad_last = copy(grad_new)
        H_last = copy(H_k)
        p_vec_disp = p_vec[1:20]
        f_final_val = fval
        println("Update Parameters to $p_vec_disp")


        println("Gradient Size: $grad_size")
        println("Step Size: $step_size")
        println("Function Value is $f_test at iteration $cnt")
        println("Steps since last improvement: $no_progress")
    end
    println("Lowest Function Value is $f_min at $p_min")
    return p_min,f_min
end
