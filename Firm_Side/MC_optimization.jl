function gradient_ascent_BB(p0::Vector{Float64},p_est::parDict{Float64},
                d::InsuranceLogit,c::MC_Data,W::Matrix{Float64};grad_tol=1e-8,f_tol=1e-8,x_tol=1e-8,max_itr=2000,strict=false,squared=false)
    ## Initialize Parameter Vector
    p_vec = p0
    N = length(p0)

    cnt = 0
    grad_size = 10000
    f_eval_old = 1.0
    # # Initialize δ

    # Initialize Gradient
    grad = Vector{Float64}(undef,length(p0))
    f_final_val = 0.0
    max_trial_cnt = 0
    p_last = copy(p_vec)
    grad_last = copy(grad)
    disp_length = min(length(p0),20)
    f_min = -1e3
    p_min  = similar(p_vec)
    no_progress=0
    flag = "empty"
    if strict
        mistake_thresh = 1.000005
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

        fval = GMM_objective!(grad,p_vec,p_est,d,c,W;squared=squared)
        println("Function Value is $fval at iteration $cnt")

        if (cnt==1) | (fval<f_min)
            if abs((fval-f_min)/f_min)<f_tol
                f_tol_cnt += 1
            end
            if maximum(abs.((p_vec - p_min)./p_min))<x_tol
                x_tol_cnt += 1
            end

            f_min = copy(fval)
            p_min[:] = p_vec[:]

            no_progress=0
        else
            no_progress+=1
        end


        grad_size = maximum(abs.(grad))
        if (grad_size<grad_tol) |(f_tol_cnt>1) | (x_tol_cnt>1)
            println("Got to Break Point")
            println(grad_size)
            println(f_tol_cnt)
            println(x_tol_cnt)
            flag = "converged"
            break
        end
        if strict==false
            if cnt<20
                if grad_size>.1
                    mistake_thresh = 1.1
                else
                    mistake_thresh = 1.01
                end
            else
                if grad_size>.1
                    mistake_thresh = 1.005
                else
                    mistake_thresh = 1.0005
                end
            end
        end

        if cnt==1
            step = 1/grad_size
        else
            g = p_vec - p_last
            y = grad - grad_last
            step = abs.(dot(g,g)/dot(g,y))
        end

        if no_progress>3
            no_progress = 0
            println("Return: Limit on No Progress")
            println("Lowest Value Reached: $f_min")
            p_vec = copy(p_min)
            fval = GMM_objective!(grad,p_vec,p_est,d,c,W)
            println("Function Value is $fval at iteration $cnt")
            grad_size = maximum(abs.(grad))
            step = 1/grad_size
            mistake_thresh = 1.00
        end



        p_test = p_vec .- step.*grad

        f_test = GMM_objective(p_test,p_est,d,c,W;squared=squared)
        println("Initial Step Size: $step")
        while ((f_test>fval*mistake_thresh) | isnan(f_test)) & (step>1e-15)
            p_test_disp = p_test[1:disp_length]
            if trial_cnt==0
                println("Trial: Got $f_test at parameters $p_test_disp")
                println("Previous Iteration at $fval")
                println("Reducing Step Size...")
            end
            step/= 20
            p_test = p_vec .- step.*grad
            f_test = GMM_objective(p_test,p_est,d,c,W;squared=squared)
        end

        p_last = copy(p_vec)
        p_vec = copy(p_test)
        grad_last = copy(grad)
        p_vec_disp = round.(p_vec[1:10],digits=4)
        f_final_val = fval
        println("Update Parameters to $p_vec_disp")


        println("Gradient Size: $grad_size")
        println("Step Size: $step")
        println("Steps since last improvement: $no_progress")
    end
    println("Lowest Function Value is $f_min at $p_min")
    return p_min,f_min
end


function newton_raphson_GMM(p0::Vector{Float64},p_est::parDict{Float64},
                d::InsuranceLogit,c::MC_Data,W::Matrix{Float64};grad_tol=1e-8,f_tol=1e-8,x_tol=1e-8,max_itr=2000,strict=false,squared=false)
    ## Initialize Parameter Vector
    p_vec = p0
    N = length(p0)

    cnt = 0
    grad_size = 10000
    f_eval_old = 1.0
    # # Initialize δ

    # Initialize Gradient
    grad = Vector{Float64}(undef,length(p0))
    hess_new = Matrix{Float64}(undef,length(p0),length(p0))
    f_final_val = 0.0
    max_trial_cnt = 0
    p_last = copy(p_vec)
    grad_last = copy(grad)
    disp_length = min(length(p0),20)
    f_min = -1e3
    p_min  = similar(p_vec)
    no_progress=0
    flag = "empty"
    if strict
        mistake_thresh = 1.000005
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
        fval = GMM_objective!(hess_new,grad,p_vec,p_est,d,c,W;squared=squared)
        println("Function Value is $fval at iteration $cnt")

        eigs = eigvals(hess_new)
        elg = maximum(eigs)
        esm = minimum(eigs)
        println("Hessian Eigenvalues between $elg and $esm")

        if (cnt==1) | (fval<f_min)
            if abs((fval-f_min)/f_min)<f_tol
                f_tol_cnt += 1
            end
            if maximum(abs.((p_vec - p_min)./p_min))<x_tol
                x_tol_cnt += 1
            end

            f_min = copy(fval)
            p_min[:] = p_vec[:]

            no_progress=0
        else
            no_progress+=1
        end


        grad_size = maximum(abs.(grad))
        if (grad_size<grad_tol) |(f_tol_cnt>1) | (x_tol_cnt>1)
            println("Got to Break Point")
            println(grad_size)
            println(f_tol_cnt)
            println(x_tol_cnt)
            flag = "converged"
            break
        end
        if strict==false
            if cnt<20
                if grad_size>.1
                    mistake_thresh = 1.1
                else
                    mistake_thresh = 1.01
                end
            else
                if grad_size>.1
                    mistake_thresh = 1.005
                else
                    mistake_thresh = 1.0005
                end
            end
        else
            mistake_thresh = 1.000005
        end

        update = -inv(hess_new)*grad
        if any(isnan.(update))
            println("NaN in the Update")
            update = -(1/grad_size).*grad
        end

        if no_progress>3
            no_progress = 0
            println("Return: Limit on No Progress")
            println("Lowest Value Reached: $f_min")
            p_vec = copy(p_min)
            fval = GMM_objective!(grad,p_vec,p_est,d,c,W;squared=squared)
            println("Function Value is $fval at iteration $cnt")
            grad_size = maximum(abs.(grad))
            step = 1/grad_size
            update = - step.*grad
            mistake_thresh = 1.00
        end





        step_size = maximum(abs.(update))
        if step_size>4
            update = update./step_size
            ind = findall(abs.(update).==1)
            val_disp = p_vec[ind]
            println("Max Parameter Adjustment: $ind, $val_disp")
            step_size = 1
        end
        p_test = p_vec .+ update
        f_test = GMM_objective(p_test,p_est,d,c,W;squared=squared)

        trial_max = 0
        while ((f_test>fval*mistake_thresh) | isnan(f_test)) & (trial_max==0)
            if trial_cnt==0
                p_test_disp = p_test[1:20]
                println("Trial (Init): Got $f_test at parameters $p_test_disp")
                println("Previous Iteration at $fval")
            end
            if (step_size>x_tol) & (trial_cnt<3)
                if trial_cnt<=4
                    update/= 10
                else
                    update/= 200
                end
                step_size = maximum(abs.(update))
                p_test = p_vec .+ update
                f_test = GMM_objective(p_test,p_est,d,c,W;squared=squared)
                p_test_disp = p_test[1:10]
                println("Trial (NR): Got $f_test at parameters $p_test_disp")
                println("Previous Iteration at $fval")
                trial_cnt+=1
            else
                trial_max = 1
                println("RUN ROUND OF GRADIENT ASCENT")
                p_test, f_test = gradient_ascent_BB(p_vec,par_est,m,costdf,W,max_itr=10,strict=true,squared=squared)
            end
        end

        p_last = copy(p_vec)
        p_vec = copy(p_test)
        grad_last = copy(grad)
        p_vec_disp = round.(p_vec[1:10],digits=4)
        f_final_val = fval
        println("Update Parameters to $p_vec_disp")


        println("Gradient Size: $grad_size")
        println("Step Size: $step_size")
        # println("Function Value is $f_test at iteration $cnt")
        println("Steps since last improvement: $no_progress")
    end
    println("Lowest Function Value is $f_min at $p_min")
    return p_min,f_min
end

function estimate_GMM(p0::Vector{Float64},p_est::parDict{Float64},
                d::InsuranceLogit,c::MC_Data,W::Matrix{Float64};squared=false)
    # First run a gradient ascent method to get close to optimum
    println("Gradient Ascent Method")
    p1, fval = gradient_ascent_BB(p0,p_est,d,c,W,max_itr=30,squared=squared)
    # Then run newtons method until better convergence
    println("Newtons Method")
    p_est, fval = newton_raphson_GMM(p1,p_est,d,c,W,grad_tol = 1e-8,strict=true,squared=squared)

    return p_est,fval
end
