function two_stage_est(d,p0,W;grad_tol=1e-8,f_tol=1e-8,x_tol=1e-8,
    max_itr=2000,strict=true,Hess_Skip_Steps=15,checkin=false)
    ## Initialize Parameter Vector

    N = length(p0)
    cnt = 0
    grad_size = 10000
    f_eval_old = 1.0

    parLen = d.parLength[:All] - d.parLength[:FE]
    par_ind = 1:parLen
    FE_ind = (parLen+1):d.parLength[:All]
    p_vec = copy(p0)
    update = zeros(N)
    # Initialize Gradient
    grad_new = similar(p0)
    hess_new = Matrix{Float64}(undef,N,N)
    Eye = Matrix{Float64}(1.0I,parLen,parLen)
    f_final_val = 0.0
    max_trial_cnt = 0
    NaN_steps = 0
    trial_end = 5
    hess_steps = 0
    p_last = copy(p_vec)
    grad_last = copy(grad_new)
    H_last = copy(hess_new)
    disp_length = min(parLen,20)
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
    ga_conv_cnt = 0
    ga_itr = 10
    ga_skip = 0
    ga_strict = true
    ga_cnt = 0

    ### Initialize Fixed Effects
    fval = log_likelihood!(hess_new,grad_new,d,p_vec)
    grad_size = maximum(abs.(grad_new))
    println(grad_size)

    par = parDict(d,p_vec)
    individual_values!(d,par)
    individual_shares(d,par)
    res = NR_fixedEffects(d,par,Hess_Skip_Steps=30,max_itr=5)
    p_vec[FE_ind] = par.FE[:]

    # Maximize by Newtons Method
    while (grad_size>grad_tol) & (cnt<max_itr) & (max_trial_cnt<20)
        cnt+=1
        trial_cnt=0

        # Compute Gradient, holding δ fixed
        if hess_steps==0
            println("Compute Hessian")
            fval = GMM_objective!(hess_new,grad_new,d,p_vec,W,feFlag=0)
            H_k = inv(hess_new[par_ind,par_ind])
            real_hessian=1
        else
            println("BFGS Approximation")
            fval = GMM_objective!(grad_new,d,p_vec,W,feFlag=0)
            Δxk = (p_vec - p_last)[par_ind]
            yk = (grad_new - grad_last)[par_ind]
            # Δhess =  (yk*yk')./(yk'*Δxk) - (hess_new*Δxk*(hess_new*Δxk)')./(Δxk'*hess_new*Δxk)
            # hess_new = hess_new + (yk*yk')./(yk'*Δxk) - (yk*yk')./(yk'*Δxk) - (hess_new*Δxk*(hess_new*Δxk)')./(Δxk'*hess_new*Δxk)
            H_k = (Eye - (Δxk*yk')./(yk'*Δxk) )*H_last*(Eye - (yk*Δxk')./(yk'*Δxk) ) + (Δxk*Δxk')./(yk'*Δxk)
            real_hessian=0
        end


        # Advance Hessian Approx Counter
        if hess_steps<Hess_Skip_Steps
            hess_steps+=1
        else
            hess_steps=0
        end

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
        println(maximum(findall(abs.(grad_new).>1e-10)))
        println(grad_new[par_ind])
        if (grad_size<grad_tol) |(f_tol_cnt>1) | (x_tol_cnt>1) | (ga_conv_cnt>2)
            println("Got to Break Point...?")
            println(grad_size)
            println(f_tol_cnt)
            println(x_tol_cnt)
            println(ga_conv_cnt)
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


        update[par_ind] = -H_k*grad_new[par_ind]
        if any(isnan.(update))
            println("Step contains NaN")
            #Check Hessian
            # eig = sort(abs.(eigvals(hess_new)))
            # sm_e = eig[1]
            # println("Smallest Eigenvalue: $sm_e ")
            NaN_steps +=1
            grad_size = sqrt(dot(grad_new,grad_new))
            update = -(1/grad_size).*grad_new
        else
            NaN_steps = 0
        end


        if no_progress>5
            no_progress = 0
            println("Return: Limit on No Progress")
            p_vec = copy(p_min)
            fval = GMM_objective!(grad_new,d,p_vec,W)
            grad_size = maximum(abs.(grad_new))
            step = 1/grad_size
            update = - step.*grad_new
            mistake_thresh = 1.00
        end

        step_size = maximum(abs.(update))
        if step_size>10
        update = (update./step_size).*10
        ind = findall(abs.(update).==10)
        val_disp = p_vec[ind]
            println("Max Parameter Adjustment: $ind, $val_disp")
        step_size = 10
        end

        p_test = p_vec .+ update

        f_test = GMM_objective(d,p_test,W,feFlag=0)

        trial_max = 0
        while ((f_test>fval*mistake_thresh) | isnan(f_test)) & (trial_max==0)
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
                f_test = GMM_objective(d,p_test,W,feFlag=0)
                p_test_disp = p_test[1:20]
                println("Trial (NR): Got $f_test at parameters $p_test_disp")
                println("Previous Iteration at $fval")
                trial_cnt+=1
            elseif real_hessian==1
                hess_steps = 0
                trial_max = 1
                println("RUN ROUND OF GRADIENT ASCENT")
                if ga_cnt<=2
                    p_test, f_test,ga_conv = gradient_ascent_BB(d,p_vec,W,max_itr=10,strict=true,Grad_Skip_Steps=2)
                    ga_cnt+=1
                else
                    p_test, f_test,ga_conv = gradient_ascent_BB(d,p_vec,W,max_itr=50,strict=false,Grad_Skip_Steps=10)
                end
                if ga_conv==1
                    ga_conv_cnt+=1
                end
            else
                hess_steps = 0
                println("No Update")
                p_test = copy(p_vec)
                break
            end
        end

        if step_size>x_tol
            ga_cnt = 0
        end

        if NaN_steps>5
            println("Hessian might be singular")
            println("RUN ROUND OF GRADIENT ASCENT")
            p_test, f_test = gradient_ascent_BB(d,p_test,W,max_itr=20,strict=true,Grad_Skip_Steps=2)
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
        println("Repeated GA Steps: $ga_cnt")

        # if checkin & (cnt%5==0)
        #     file = "checkin_$cnt.jld2"
        #     @save file grad_size p_vec f_test no_progress
        # end
    end
    println("Lowest Function Value is $f_min at $p_min")
    return p_min,f_min
end


function NR_fixedEffects(d,par;grad_tol=1e-8,f_tol=1e-8,x_tol=1e-8,
    max_itr=30,strict=true,Hess_Skip_Steps=5)

    # Initialize Gradient
    grad_new = Vector{Float64}(undef,d.parLength[:All])
    hess_new = Matrix{Float64}(undef,d.parLength[:All],d.parLength[:All])
    Eye = Matrix{Float64}(1.0I,length(p0),length(p0))

    FE_ind = (d.parLength[:All]-d.parLength[:FE]+1):d.parLength[:All]
    Eye = Matrix{Float64}(1.0I,d.parLength[:FE],d.parLength[:FE])
    f_final_val = 0.0
    max_trial_cnt = 0
    NaN_steps = 0
    trial_end = 5
    hess_steps = 0
    p_vec = par.FE[:]
    p_last = copy(p_vec)
    grad_last = copy(grad_new)
    H_last = copy(hess_new)
    disp_length = min(length(p0),20)
    f_min = -1e3
    p_min  = similar(par.FE[:])
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
    grad_size = 1

    ## Tolerance Counts
    cnt=0
    f_tol_cnt = 0
    x_tol_cnt = 0
    skip_x_tol=0


    # Maximize by Newtons Method
    while (grad_size>grad_tol) & (cnt<max_itr) & (max_trial_cnt<20)
        cnt+=1
        trial_cnt=0

        p_vec = par.FE[:]
        # Compute Gradient, holding δ fixed
        if hess_steps==0
            println("Compute Hessian")
            fval = log_likelihood!(hess_new,grad_new,d,par,feFlag=1)
            println(grad_new[1:20])
            H_k = inv(hess_new[FE_ind,FE_ind])
            real_hessian=1
        else
            println("BFGS Approximation")
            fval = log_likelihood!(grad_new,d,par,feFlag=1)
            Δxk = (p_vec - p_last)
            yk = (grad_new - grad_last)[FE_ind]
            # Δhess =  (yk*yk')./(yk'*Δxk) - (hess_new*Δxk*(hess_new*Δxk)')./(Δxk'*hess_new*Δxk)
            # hess_new = hess_new + (yk*yk')./(yk'*Δxk) - (yk*yk')./(yk'*Δxk) - (hess_new*Δxk*(hess_new*Δxk)')./(Δxk'*hess_new*Δxk)
            H_k = (Eye - (Δxk*yk')./(yk'*Δxk) )*H_last*(Eye - (yk*Δxk')./(yk'*Δxk) ) + (Δxk*Δxk')./(yk'*Δxk)
            real_hessian=0
        end

        if (cnt==1) | (fval>f_min)
            if abs(fval-f_min)<f_tol
                f_tol_cnt += 1
            end
            if (maximum(abs.(p_vec - p_min))<x_tol) & (skip_x_tol==0)
                x_tol_cnt += 1
            end

            f_min = copy(fval)
            p_min[:] = p_vec[:]

            no_progress=0
        else
            no_progress+=1
        end
        skip_x_tol = 0

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

        update = -H_k*grad_new[FE_ind]
        if any(isnan.(update))
            println("Step contains NaN")
            println("Algorithm Failed")
            return p_min,f_min

            # #Check Hessian
            # eig = sort(abs.(eigvals(hess_new)))
            # sm_e = eig[1]
            # println("Smallest Eigenvalue: $sm_e ")
            # NaN_steps +=1
            # grad_size = sqrt(dot(grad_new,grad_new))
            # update = -(1/grad_size).*grad_new
        else
            NaN_steps = 0
        end


        # if no_progress>5
        #     no_progress = 0
        #     println("Return: Limit on No Progress")
        #     p_vec = copy(p_min)
        #     fval = log_likelihood!(grad_new,d,p_vec)
        #     grad_size = maximum(abs.(grad_new))
        #     step = 1/grad_size
        #     update = - step.*grad_new
        #     mistake_thresh = 1.00
        # end

        step_size = maximum(abs.(update))
        if step_size>10
        update = update./step_size
        ind = findall(abs.(update).==1)
        val_disp = p_vec[ind]
            println("Max Parameter Adjustment: $ind, $val_disp")
        step_size = 1
        end


        p_test = p_vec .+ update
        par.FE[:] = p_test
        f_test = log_likelihood(d,par,feFlag=1)

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
                par.FE[:] = p_test
                f_test = log_likelihood(d,par,feFlag=1)
                p_test_disp = p_test[1:20]
                println("Trial (NR): Got $f_test at parameters $p_test_disp")
                println("Previous Iteration at $fval")
                trial_cnt+=1
            else
                println("No Advancement")
                p_test = copy(p_vec)
                hess_steps=0
                skip_x_tol = 1
                break
            end
        end
        # if NaN_steps>5
        #     println("Hessian might be singular")
        #     println("RUN ROUND OF GRADIENT ASCENT")
        #     p_test, f_test = gradient_ascent_ll(d,p_test,max_itr=20,strict=true)
        # end

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
