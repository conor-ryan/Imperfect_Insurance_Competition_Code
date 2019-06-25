function two_stage_est(d,p0,W;grad_tol=1e-8,f_tol=1e-8,x_tol=1e-8,
    max_itr=2000,strict=true,Hess_Skip_Steps=15,checkin=false)
    ## Initialize Parameter Vector

    N = length(p0)
    cnt = 0
    grad_size = 10000
    f_eval_old = 1.0

    # Par Indices
    Q = d.parLength[:All]
    Q_0 = Q - d.parLength[:FE]
    Q_no_σ = Q_0 - d.parLength[:σ]
    par_ind = (Q_no_σ+1):Q_0
    parLen = length(par_ind)
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
    f_min = 1e3
    p_min  = copy(p_vec)
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
    # fval = log_likelihood!(hess_new,grad_new,d,p_vec)
    # grad_size = maximum(abs.(grad_new))
    # println(grad_size)

    p_vec,fe_itrs = reOpt_FE(d,p_vec)

    # Maximize by Newtons Method
    while (grad_size>0) & (cnt<max_itr) & (max_trial_cnt<20)
        cnt+=1
        trial_cnt=0

        if (cnt%8==0) | (flag=="converged")
            p_vec,fe_itrs = reOpt_FE(d,p_vec)
            if (flag=="converged") & (fe_itrs<2)
                println("Converged in two stages!")
                break
            end
            println("Gradient Conditioning")
            p_vec, f_null,ga_null = ga_twostage(d,p_vec,W,par_ind,max_itr=10,strict=true,Grad_Skip_Steps=0)
            f_min = 1e3
            flag = "empty"
            f_tol_cnt = 0
            x_tol_cnt = 0
            ga_conv_cnt = 0
            ga_cnt = 0
            hess_steps=0
        end

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


        grad_size = maximum(abs.(grad_new[par_ind]))
        # println(maximum(findall(abs.(grad_new).>1e-10)))
        println(grad_new[par_ind])
        if (grad_size<grad_tol) |(f_tol_cnt>1) | (x_tol_cnt>1) | (ga_conv_cnt>2)
            println("Got to Break Point...?")
            println(grad_size)
            println(f_tol_cnt)
            println(x_tol_cnt)
            println(ga_conv_cnt)
            flag = "converged"
            continue
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
                    p_test, f_test,ga_conv = ga_twostage(d,p_vec,W,par_ind,max_itr=10,strict=true,Grad_Skip_Steps=2)
                    ga_cnt+=1
                else
                    p_test, f_test,ga_conv = ga_twostage(d,p_vec,W,par_ind,max_itr=50,strict=false,Grad_Skip_Steps=10)
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
            p_test, f_test = ga_twostage(d,p_test,W,par_ind,max_itr=20,strict=true,Grad_Skip_Steps=2)
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

function reOpt_FE(d::InsuranceLogit,p_vec::Vector{Float64})
    println("## RE-Optimize Fixed Effects ##")
    # par = parDict(d,p_vec)
    # individual_values!(d,par)
    # individual_shares(d,par)
    p_min,f_min,cnt = NR_fixedEffects(d,p_vec,Hess_Skip_Steps=30,max_itr=30)
    # p_vec[FE_ind] = par.FE[:]
    return p_min,cnt
end

function NR_fixedEffects(d,p0;grad_tol=1e-8,f_tol=1e-8,x_tol=1e-8,
    max_itr=30,strict=true,Hess_Skip_Steps=5)

    # Initialize Gradient
    grad_new = Vector{Float64}(undef,d.parLength[:All])
    hess_new = Matrix{Float64}(undef,d.parLength[:All],d.parLength[:All])
    Eye = Matrix{Float64}(1.0I,length(p0),length(p0))

    FE_ind = vcat(1:(d.parLength[:All]-d.parLength[:FE]-d.parLength[:σ]),(d.parLength[:All]-d.parLength[:FE]+1):d.parLength[:All])
    p_len = length(FE_ind)
    Eye = Matrix{Float64}(1.0I,p_len,p_len)
    f_final_val = 0.0
    max_trial_cnt = 0
    NaN_steps = 0
    trial_end = 5
    hess_steps = 0
    p_vec = copy(p0)
    p_last = copy(p_vec)
    grad_last = copy(grad_new)
    H_last = copy(hess_new)
    disp_length = min(length(p0),20)
    f_min = -1e3
    p_min  = copy(p0)
    no_progress=0
    flag = "empty"
    if strict
        mistake_thresh = 1.00
    else
        mistake_thresh = 1.25
    end

    ## Initialize Par Dict
    par = parDict(d,p_vec)
    individual_values!(d,par)
    individual_shares(d,par)

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

        # p_vec = par.FE[:]
        update_par(d,par,p_vec)
        # Compute Gradient, holding δ fixed
        if hess_steps==0
            println("Compute Hessian")
            fval = log_likelihood!(hess_new,grad_new,d,par,feFlag=1)
            H_k = inv(hess_new[FE_ind,FE_ind])
            real_hessian=1
        else
            println("BFGS Approximation")
            fval = log_likelihood!(grad_new,d,par,feFlag=1)
            Δxk = (p_vec - p_last)[FE_ind]
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
            return p_min,f_min, cnt

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


        if no_progress>5
            no_progress = 0
            println("Return: Limit on No Progress")
            update = -(1/grad_size).*grad_new[FE_ind]
        end

        step_size = maximum(abs.(update))
        if step_size>10
        update = update./step_size
        ind = findall(abs.(update).==1)
        val_disp = p_vec[ind]
            println("Max Parameter Adjustment: $ind, $val_disp")
        step_size = 1
        end

        p_test = copy(p_vec)
        p_test[FE_ind] = p_vec[FE_ind] .+ update
        update_par(d,par,p_test)
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
            if (step_size>x_tol) | (real_hessian==1 & trial_cnt<5)
                p_test = copy(p_vec)
                p_test[FE_ind] = p_vec[FE_ind] .+ update
                update_par(d,par,p_test)
                f_test = log_likelihood(d,par,feFlag=1)
                p_test_disp = p_test[1:20]
                println("Trial (NR): Got $f_test at parameters $p_test_disp")
                println("Previous Iteration at $fval")
                trial_cnt+=1
            else
                println("No Advancement")
                p_test = copy(p_vec)
                skip_x_tol = 1
                hess_steps=0
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
        p_vec_disp = p_vec[1:10]
        f_final_val = fval
        println("Update Parameters to $p_vec_disp")


        println("Gradient Size: $grad_size")
        println("Step Size: $step_size")
        println("Function Value is $f_test at iteration $cnt")
        println("Steps since last improvement: $no_progress")
    end
    # println("Lowest Function Value is $f_min at $p_min")
    return p_min,f_min, cnt
end



function ga_twostage(d,p0,W,par_ind::Union{Vector{Int64},UnitRange{Int64}};grad_tol=1e-8,f_tol=1e-8,x_tol=1e-8,
    max_itr=2000,strict=false,Grad_Skip_Steps=5)
    ## Initialize Parameter Vector
    p_vec = p0
    N = length(p0)

    cnt = 0
    grad_size = 10000
    # # Initialize δ
    param_dict = parDict(d,p_vec)

    # Initialize Gradient
    grad_new = similar(p0)
    hess_new = Matrix{Float64}(undef,length(p0),length(p0))
    f_final_val = 0.0
    max_trial_cnt = 0
    grad_steps = 0
    p_last = copy(p_vec)
    grad_last = copy(grad_new)
    disp_length = min(length(p0),20)
    f_min = 1e3
    fval = 1e3
    p_min  = copy(p_vec)
    no_progress=0
    flag = "empty"
    if strict
        mistake_thresh = 1.00
    else
        mistake_thresh = 1.25
    end

    ## Initialize Step
    step = 1
    real_gradient=0

    ## Tolerance Counts
    f_tol_cnt = 0
    x_tol_cnt = 0
    conv_flag = 0
    # Maximize by Newtons Method
    while (cnt<max_itr)
        cnt+=1
        trial_cnt=0


        # Compute Gradient, holding δ fixed
        if grad_steps==0
            println("Compute Gradient")
            fval = GMM_objective!(grad_new,d,p_vec,W,feFlag=0)
            real_gradient=1
        else
            # fval = GMM_objective(d,p_vec,W)
            real_gradient=0
        end

        grad_size = maximum(abs.(grad_new[par_ind]))
        if (grad_size<grad_tol) |(f_tol_cnt>1) | (x_tol_cnt>1)
            conv_flag = 1
            println("Got to Break Point")
            println(grad_size)
            println(f_tol_cnt)
            println(x_tol_cnt)
            println(conv_flag)
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
        elseif (real_gradient==1)
            g = (p_vec - p_last)[par_ind]
            y = (grad_new - grad_last)[par_ind]
            step = abs.(dot(g,g)/dot(g,y))
        end

        if no_progress>10
            no_progress = 0
            println("Return: Limit on No Progress")
            p_vec = copy(p_min)
            fval = GMM_objective!(grad_new,d,p_vec,W,feFlag=0)
            grad_size = maximum(abs.(grad_new))
            step = 1/grad_size
            mistake_thresh = 1.00
        end


        p_test = copy(p_vec)
        p_test[par_ind] = p_vec[par_ind] .- step.*grad_new[par_ind]

        f_test = GMM_objective(d,p_test,W,feFlag=0)

        if (f_test>fval) & (real_gradient==0)
            grad_steps=0
            continue
        elseif (grad_steps<Grad_Skip_Steps)
            grad_steps+=1
        else
            grad_steps=0
        end

        println("Initial Step Size: $step")
        while ((f_test>fval*mistake_thresh) | isnan(f_test)) & (step>1e-15)
            p_test_disp = p_test[1:disp_length]
            if trial_cnt==0
                println("Trial: Got $f_test at parameters $p_test_disp")
                println("Previous Iteration at $fval")
                println("Reducing Step Size...")
            end
            step/= 20
            p_test = copy(p_vec)
            p_test[par_ind] = p_vec[par_ind] .- step.*grad_new[par_ind]
            f_test = GMM_objective(d,p_test,W,feFlag=0)
            if (step<x_tol) & (real_gradient==0)
                println("Failed with Approximate Gradient")
                break
            end
        end

        ## Update Minimum Value
        if (cnt==1) | (f_test<f_min)
            if (abs(f_test-f_min)<f_tol) & (real_gradient==1)
                f_tol_cnt += 1
            end
            if (maximum(abs.(p_test - p_min))<x_tol) & (real_gradient==1)
                    x_tol_cnt += 1
            end
            f_min = copy(f_test)
            p_min[:] = p_test[:]
            no_progress=0
        else
            no_progress+=1
        end


        if real_gradient==1
            p_last = copy(p_vec)
            grad_last = copy(grad_new)
        end
        p_vec = copy(p_test)
        fval = copy(f_test)
        p_vec_disp = p_vec[par_ind]
        f_final_val = fval
        println("Update Parameters to $p_vec_disp")


        println("Gradient Size: $grad_size")
        println("Step Size: $step")
        println("Function Value is $f_test at iteration $cnt")
        println("Steps since last improvement: $no_progress")
    end
    # println("Lowest Function Value is $f_min at $p_min")
    return p_min,f_min,conv_flag
end
