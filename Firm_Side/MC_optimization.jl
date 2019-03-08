
function gradient_ascent_BB(p0::Vector{Float64},p_est::parDict{Float64},
                d::InsuranceLogit,c::MC_Data,W::Matrix{Float64};grad_tol=1e-8,f_tol=1e-8,x_tol=1e-8,max_itr=2000,strict=false)
    ## Initialize Parameter Vector
    p_vec = p0
    N = length(p0)

    cnt = 0
    grad_size = 10000
    f_eval_old = 1.0
    # # Initialize δ

    # Initialize Gradient
    gmm(x) = GMM_objective(x,p_est,d,c,W)
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

        fval = gmm(p_vec)
        ForwardDiff.gradient!(grad, gmm, p_vec)
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
            fval = gmm(p_vec)
            ForwardDiff.gradient!(grad, gmm, p_vec)
            println("Function Value is $fval at iteration $cnt")
            grad_size = maximum(abs.(grad))
            step = 1/grad_size
            mistake_thresh = 1.00
        end



        p_test = p_vec .- step.*grad

        f_test = GMM_objective(p_test,p_est,d,c,W)
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
            f_test = GMM_objective(p_test,p_est,d,c,W)
        end

        p_last = copy(p_vec)
        p_vec = copy(p_test)
        grad_last = copy(grad)
        p_vec_disp = p_vec
        f_final_val = fval
        println("Update Parameters to $p_vec_disp")


        println("Gradient Size: $grad_size")
        println("Step Size: $step")
        println("Steps since last improvement: $no_progress")
    end
    println("Lowest Function Value is $f_min at $p_min")
    return p_min,f_min
end
