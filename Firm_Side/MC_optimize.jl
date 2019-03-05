


function estimate_GMM2(p0::Vector{Float64},p_est::parDict{Float64},
                d::InsuranceLogit,c::MC_Data,W::Matrix{Float64})
    # First run a gradient ascent method to get close to optimum
    println("Gradient Ascent Method")
    p_est, fval = gradient_ascent_BB(p0,p_est,d,c,W,max_itr=500)
    # Then run newtons method until better convergence
    println("Newtons Method")
    p_est, fval = newton_raphson_GMM(p0,p_est,d,c,W,grad_tol = 1e-8)

    return p_est,fval
end



function gradient_ascent_BB(p0::Vector{Float64},p_est::parDict{Float64},
                d::InsuranceLogit,c::MC_Data,W::Matrix{Float64};grad_tol=1e-8,f_tol=1e-8,x_tol=1e-8,max_itr=2000,strict=false)
    ## Initialize Parameter Vector
    p_vec = p0
    N = length(p0)

    cnt = 0
    grad_size = 10000
    f_eval_old = 1.0

    # Initialize Gradient
    grad_new = similar(p0)
    gmm(x) = GMM_objective(x,p_est,d,c,W)

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
        # fval = gmm(p_vec)
        ForwardDiff.gradient!(grad_new, gmm, p_vec)
        fval = GMM_objective(p_vec,p_est,d,c,W)
        if (cnt==1) | (fval<f_min)
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
            fval = GMM_objective(p_vec,p_est,d,c,W)
            ForwardDiff.gradient!(grad, gmm, p_vec)
            grad_size = maximum(abs.(grad_new))
            step = 1/grad_size
            mistake_thresh = 1.00
        end



        p_test = p_vec .- step.*grad_new

        f_test = GMM_objective(p_vec,p_est,d,c,W)
        println("Initial Step Size: $step")
        while ((f_test>fval*mistake_thresh) | isnan(f_test)) & (step>1e-15)
            p_test_disp = p_test[1:disp_length]
            if trial_cnt==0
                println("Trial: Got $f_test at parameters $p_test_disp")
                println("Previous Iteration at $fval")
                println("Reducing Step Size...")
            end
            step/= 20
            p_test = p_vec .- step.*grad_new
            f_test = GMM_objective(p_vec,p_est,d,c,W)
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


function newton_raphson_GMM(d,p0,W;grad_tol=1e-8,f_tol=1e-8,x_tol=1e-8,max_itr=2000,strict=true)
    ## Initialize Parameter Vector
    p_vec = p0
    N = length(p0)

    cnt = 0
    grad_size = 10000
    f_eval_old = 1.0

    # Initialize Gradient
    grad_new = similar(p0)
    hess_new = Matrix{Float64}(undef,length(p0),length(p0))
    f_final_val = 0.0
    max_trial_cnt = 0
    trial_end = 5
    p_last = copy(p_vec)
    grad_last = copy(grad_new)
    disp_length = min(length(p0),20)
    f_min = -1e3
    p_min  = similar(p_vec)
    no_progress=0
    flag = "empty"
    step = 1
    if strict
        mistake_thresh = 1.00
    else
        mistake_thresh = 1.25
    end

    ## Tolerance Counts
    f_tol_cnt = 0
    x_tol_cnt = 0
    # Maximize by Newtons Method
    while (grad_size>grad_tol) & (cnt<max_itr) & (max_trial_cnt<20)
        cnt+=1
        trial_cnt=0


        # Compute Gradient, holding δ fixed

        fval = GMM_objective!(hess_new,grad_new,p_vec,p_est,d,c,W)

        if (cnt==1) | (fval<f_min)
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

        update = -inv(hess_new)*grad_new
        if any(isnan.(update))
            update = -(1/grad_size).*grad_new
        end


        if no_progress>5
            no_progress = 0
            println("Return: Limit on No Progress")
            p_vec = copy(p_min)
            fval = GMM_objective!(grad_new,p_vec,p_est,d,c,W)
            grad_size = maximum(abs.(grad_new))
            step = 1/grad_size
            update = - step.*grad_new
            mistake_thresh = 1.00
        end


        p_test = p_vec .+ update

        f_test = GMM_objective(p_vec,p_est,d,c,W)

        while ((f_test>fval*mistake_thresh) | isnan(f_test)) & (trial_cnt<=trial_end)
            if trial_cnt==0
                p_test_disp = p_test[1:20]
                println("Trial (Init): Got $f_test at parameters $p_test_disp")
                println("Previous Iteration at $fval")
            end
            if trial_cnt<trial_end
                update/= 10
                p_test = p_vec .+ update
                f_test = GMM_objective(p_vec,p_est,d,c,W)
                p_test_disp = p_test[1:20]
                println("Trial (NR): Got $f_test at parameters $p_test_disp")
                println("Previous Iteration at $fval")
                trial_cnt+=1
            else
                println("RUN ROUND OF GRADIENT ASCENT")
                p_test, f_test = gradient_ascent_BB(d,p_vec,W,max_itr=5,strict=true)
                trial_cnt+=1
            end
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
