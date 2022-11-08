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


function gradient_ascent_ll(d,p0,W;grad_tol=1e-8,f_tol=1e-8,x_tol=1e-8,max_itr=2000,
                            strict=false,var_parameters_only=false)
    ## Initialize Parameter Vector
    p_vec = p0
    N = length(p0)
    ind1 = 1:(d.parLength[:γ]*2+d.parLength[:β])
    ind2 = (1 + maximum(ind1) + d.parLength[:σ]):d.parLength[:All]
    σ_ind = (1 + maximum(ind1)):(minimum(ind2)-1)
    if var_parameters_only
        search_ind = σ_ind
    else
        search_ind = 1:length(p_vec)
    end
    cnt = 0
    grad_size = 10000
    f_eval_old = 1.0
    # # Initialize δ
    param_dict = parDict(d,p_vec,no2Der=true)

    # Initialize Gradient
    # grad_new = SharedArray{Float64}(length(p0))
    grad_new = Vector{Float64}(undef,length(p0))
    # hess_new = Matrix{Float64}(undef,length(p0),length(p0))
    f_final_val = 0.0
    max_trial_cnt = 0
    p_last = copy(p_vec)
    grad_last = copy(grad_new)
    step_last = 1.0
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

    println("Starting Vector: $(p_vec[vcat([6,7],[13,14],[8,9,10,11,12])])")
    # Maximize by Newtons Method
    while (cnt<max_itr)
        cnt+=1
        trial_cnt=0


        # Compute Gradient, holding δ fixed
        fval = log_likelihood_penalty!(grad_new,d,p_vec,W)
        grad_new = grad_new[search_ind]



        grad_size = maximum(abs.(grad_new))
        if (grad_size<grad_tol) #|(f_tol_cnt>1) | (x_tol_cnt>1)
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
            g = p_vec[search_ind] - p_last[search_ind]
            y = grad_new - grad_last
            step = abs.(dot(g,g)/dot(g,y))
            if step==Inf
                step = 1/grad_size
            end
            step = min(step,step_last*100)
        end

        if no_progress>10
            no_progress = 0
            println("Return: Limit on No Progress")
            p_vec = copy(p_min)
            fval = log_likelihood_penalty!(grad_new,d,p_vec,W)
            grad_size = maximum(abs.(grad_new))
            step = 1/grad_size
            mistake_thresh = 1.00
        end


        update = zeros(length(p_vec))
        update[search_ind] = step.*grad_new
        p_test = p_vec .+ update

        f_test = log_likelihood_penalty(d,p_test,W)
        println("Initial Step Size: $step")
        while ((f_test<fval*mistake_thresh) | isnan(f_test)) & (step>1e-15)
            p_test_disp = p_test[vcat([6,7],[13,14],[8,9,10,11,12])]
            if trial_cnt==0
                println("Trial: Got $f_test at parameters $p_test_disp")
                println("Previous Iteration at $fval")
                println("Reducing Step Size...")
            end
            update/= 5
            p_test = p_vec .+ update
            f_test = log_likelihood_penalty(d,p_test,W)
            trial_cnt+=1
        end

        if (cnt==1) | (f_test>f_min)
            if abs(f_test-f_min)<f_tol
                f_tol_cnt += 1
            end
            if maximum(abs.(p_test - p_min))<x_tol
                x_tol_cnt += 1
            end
            f_min = copy(f_test)
            p_min[:] = p_test[:]

            no_progress=0
        else
            no_progress+=1
        end

        p_last = copy(p_vec)
        p_vec = copy(p_test)
        step_last = copy(step)
        grad_last = copy(grad_new)
        p_vec_disp = p_vec[vcat([6,7],[13,14],[8,9,10,11,12])]
        # p_vec_disp = p_vec[vcat([6,7],[13,14,15,16,17,18,19,20,21,22])]
        f_final_val = fval
        println("Update Parameters to $p_vec_disp")

        min_val = minimum(p_vec)
        min_index = findall(p_vec.==min_val)[1]
        max_val = maximum(p_vec)
        max_index = findall(p_vec.==max_val)[1]
        println("Max value at $max_index ($max_val)")
        println("Min value at $min_index ($min_val)")



        println("Gradient Size: $grad_size")
        println("Step Size: $step")
        println("Function Value is $f_test at iteration $cnt")
        println("Steps since last improvement: $no_progress")
    end
    println("Lowest Function Value is $f_min at $p_min")
    return p_min,f_min
end


function newton_raphson_ll(d,p0,W;grad_tol=1e-8,f_tol=1e-8,x_tol=1e-10,
    max_itr=2000,strict=true,Hess_Skip_Steps=25,var_parameters_only=false)
    ## Initialize Parameter Vector
    p_vec = p0
    N = length(p0)
    ind1 = 1:(d.parLength[:γ]*2+d.parLength[:β])
    ind2 = (1 + maximum(ind1) + d.parLength[:σ]):d.parLength[:All]
    σ_ind = (1 + maximum(ind1)):(minimum(ind2)-1)
    if var_parameters_only
        search_ind = σ_ind
    else
        search_ind = 1:length(p_vec)
    end
    cnt = 0
    grad_size = 10000
    f_eval_old = 1.0
    # # Initialize δ
    param_dict = parDict(d,p_vec,no2Der=true)

    # Initialize Gradient
    grad_new = similar(p0)
    hess_new = Matrix{Float64}(undef,length(p0),length(p0))
    # grad_new = SharedVector{Float64}(length(p0))
    # hess_new = SharedMatrix{Float64}(length(p0),length(p0))
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
        mistake_thresh = 1.05
    else
        mistake_thresh = 1.25
    end

    ## Initialize Step
    step = 1
    real_hessian=0

    ## Tolerance Counts
    f_tol_cnt = 0
    f_tol_flag= 0
    x_tol_cnt = 0
    # Maximize by Newtons Method
    while (grad_size>grad_tol) & (cnt<max_itr) & (max_trial_cnt<20)
        cnt+=1
        trial_cnt=0

        # Hessian Computation and Updating
        if hess_steps==0
            println("Compute Hessian")
            fval = log_likelihood_penalty!(hess_new,grad_new,d,p_vec,W)
            while any(isnan.(hess_new)) & (trial_cnt<15)
                println("NA values in Hessian, RUN: Gradient Ascent")
                p_vec, f_test = gradient_ascent_ll(d,p_vec,W,max_itr=20,strict=true)
                println("Recompute Hessian")
                fval = log_likelihood_penalty!(hess_new,grad_new,d,p_vec,W)
                trial_cnt+=1
            end
            trial_cnt = 0
            hess_new, check = enforceNegDef(hess_new)
            # if !check
            #     hess_steps = Hess_Skip_Steps-5
            # end
            hess_new = hess_new[search_ind,search_ind]
            grad_new = grad_new[search_ind]
            H_k = inv(hess_new)
            real_hessian=1
        else
            println("BFGS Approximation")
            fval = log_likelihood_penalty!(grad_new,d,p_vec,W)
            grad_new = grad_new[search_ind]
            Δxk = p_vec[search_ind] - p_last[search_ind]
            yk = grad_new - grad_last
            # Δhess =  (yk*yk')./(yk'*Δxk) - (hess_new*Δxk*(hess_new*Δxk)')./(Δxk'*hess_new*Δxk)
            # hess_new = hess_new + (yk*yk')./(yk'*Δxk) - (yk*yk')./(yk'*Δxk) - (hess_new*Δxk*(hess_new*Δxk)')./(Δxk'*hess_new*Δxk)
            H_k = (Eye - (Δxk*yk')./(yk'*Δxk) )*H_last*(Eye - (yk*Δxk')./(yk'*Δxk) ) + (Δxk*Δxk')./(yk'*Δxk)
            real_hessian=0
        end

        ## Function and Parameter Tolerance Test
        if (cnt==1) | (fval>f_min)
            if (abs(fval-f_min)<f_tol) & (f_tol_flag==1)
                f_tol_cnt += 1
            end
            if (maximum(abs.(p_vec - p_min))<x_tol) & (f_tol_flag==1)
                x_tol_cnt += 1
            end

            f_min = copy(fval)
            p_min[:] = p_vec[:]

            no_progress=0
        else
            no_progress+=1
        end
        println("Steps since last improvement: $no_progress")

        ## Convergence Criteria
        grad_size = sqrt(mean(grad_new.^2))
        if (grad_size<grad_tol) |(f_tol_cnt>1) | (x_tol_cnt>1)
            println("Got to Break Point...?")
            println(grad_size)
            println(f_tol_cnt)
            println(x_tol_cnt)
            flag = "converged"
            break
        end

        ## Allowance for wrong direction
        if strict==false
            if grad_size>.1
                mistake_thresh = 1.25
            else
                mistake_thresh = 1.05
            end
        end

        ## Compute Update, Avoid NaN values (if possible)
        update = zeros(length(p_vec))
        update[search_ind] = -H_k*grad_new
        if any(isnan.(update))
            println("Step contains NaN")
            println(p_vec)
            if any(isnan.(H_k))
                println("Hessian contains NaN")
            end
            if any(isnan.(grad_new))
                println("Gradient contains NaN")
            end
            NaN_steps+=1
            return p_min,f_min
            if NaN_steps<5
                println("Hessian might be singular")
                println("RUN ROUND OF GRADIENT ASCENT")
                p_test, f_test = gradient_ascent_ll(d,p_vec,W,max_itr=20,strict=true)
                hess_steps = -1
            else
                println("ALGORITHM FAILED! SINGULAR HESSIAN!")
                flag = "failed"
                return p_min,f_min
            end
        else
            NaN_steps = 0
        end

        ## Revert Estimation, single gradient ascent step from last progress point
        if no_progress>5
            no_progress = 0
            hess_steps = -1
            println("Return: Limit on No Progress")
            p_vec = copy(p_min)
            fval = log_likelihood_penalty!(grad_new,d,p_vec,W)
            grad_new = grad_new[search_ind]
            grad_size = maximum(abs.(grad_new))
            step = 1/grad_size
            update[search_ind] =  step.*grad_new
            mistake_thresh = 1.00
            # return p_min, fval
        end

        ## Cap Maximum Parameter Update Change
        step_size = maximum(update)
        if (step_size>10) #& (grad_size>0.1)
            update[search_ind] = (update[search_ind]./step_size).*10
            ind = findall(abs.(update).==10)
            val_disp = p_vec[ind]
            println("Max Parameter Adjustment: $ind, $val_disp")
            step_size = maximum(abs.(update))
        end

        ## Attempt to correct for runaway negative values
        if any(p_vec.< -100) & (grad_size>0.001)
            large_neg_index = findall((p_vec.<-100).&(grad_new.>0))
            if length(large_neg_index)>0
                update[large_neg_index] = abs.(p_vec[large_neg_index])/2
                println("Attempting Correction to Large Negative Values: $large_neg_index")
            end
        end

        ## Trial Update and Updated FUnction Value
        p_test = p_vec .+ update
        f_test = log_likelihood_penalty(d,p_test,W)

        ## Track steps that use approximated hessian
        if hess_steps<Hess_Skip_Steps
            hess_steps+=1
        else
            hess_steps=0
        end

        ## Check if algorithm can move forward with candidate parameter vector
        ## Search for better vector if not
        trial_max = 0
        close_grad_step = 0
        while ((f_test<fval*mistake_thresh) | isnan(f_test)) & (trial_max==0)
            if trial_cnt==0
                p_test_disp = p_test[vcat([6,7],[13,14],[8,9,10,11,12])]
                println("Trial (Init): Got $f_test at parameters $p_test_disp")
                println("Previous Iteration at $fval")
            end
            if trial_cnt<=2
                update/= 10
            else
                update/= 200
            end
            step_size = maximum(abs.(update))
            if ((real_hessian==0) & (step_size>1e-4)) | ((real_hessian==1) & (step_size>x_tol))
                p_test = p_vec .+ update
                f_test = log_likelihood_penalty(d,p_test,W)
                p_test_disp = p_test[vcat([6,7],[13,14],[8,9,10,11,12])]
                println("Trial (NR): Got $f_test at parameters $p_test_disp")
                println("Previous Iteration at $fval")
                trial_cnt+=1
                # hess_steps=0
            elseif (real_hessian==1) & (grad_size>(grad_tol*1e4))
                hess_steps = 0
                trial_max = 1
                println("RUN ROUND OF GRADIENT ASCENT")
                p_test, f_test = gradient_ascent_ll(d,p_vec,W,max_itr=10,strict=true)
            elseif (real_hessian==1) & (grad_size<=(grad_tol*1e4))
                hess_steps = 0
                trial_max = 1
                close_grad_step = 1
                println("RUN ROUND OF GRADIENT ASCENT")
                p_test, f_test = gradient_ascent_ll(d,p_vec,W,max_itr=5,strict=true)
            else
                println("No Advancement")
                hess_steps = 0
                p_test = copy(p_vec)
                break
            end
            if step_size<1e-8
                hess_steps=0
            end
        end

        if real_hessian==1 & (hess_steps>0 | close_grad_step==1)
            f_tol_flag = 1
        else
            f_tol_flag = 0
        end

        p_last = copy(p_vec)
        p_vec = copy(p_test)
        grad_last = copy(grad_new)
        H_last = copy(H_k)
        p_vec_disp = p_vec[vcat([6,7],[13,14],[8,9,10,11,12])]
        # p_vec_disp = p_vec[vcat([6,7],[13,14,15,16,17,18,19,20,21,22])]
        f_final_val = fval
        println("Update Parameters to $p_vec_disp")

        if (cnt%25==0)
            println("Full Parameter Vector: $p_vec")
        end

        min_val = minimum(p_vec)
        min_index = findall(p_vec.==min_val)[1]
        max_val = maximum(p_vec)
        max_index = findall(p_vec.==max_val)[1]
        println("Max value at $max_index ($max_val)")
        println("Min value at $min_index ($min_val)")


        println("Gradient Size: $grad_size")
        println("Step Size: $step_size")
        println("Function Value is $f_test at iteration $cnt")
    end
    println("Lowest Function Value is $f_min at $p_min")
    return p_min,f_min,flag
end
