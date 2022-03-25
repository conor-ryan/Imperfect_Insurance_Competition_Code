function fit_firm_moments(p0::Vector{Float64},p_est::parDict{Float64},
                d::InsuranceLogit,c::MC_Data;itrFirms=false)

    ## Initialize Parameter Vector
    N = length(p0)
    p_vec = zeros(c.par_length)
    p_vec[1:N] = p0[:]


    err = 1
    cnt = 0
    if itrFirms
        p_vec[(N+1):length(p_vec)] = c.fPars
        # println(p_vec[(N+1):(N+5)])
        while err>1e-10
            # println("Error is $err at $cnt")
            ΔFpar = firmParameters(c,d,p_vec,p_est)
            p_vec[(N+1):length(p_vec)] = p_vec[(N+1):length(p_vec)] + ΔFpar
            err = sum((ΔFpar).^2)
            cnt+=1
        end
        c.fPars[:] = p_vec[(N+1):length(p_vec)]
    else
        ΔFpar = firmParameters(c,d,p_vec,p_est)
        p_vec[(N+1):length(p_vec)] = p_vec[(N+1):length(p_vec)] + ΔFpar
    end

    println("Fit Firm Moments in $cnt iterations")
    println(ΔFpar)
    return p_vec
end


function firmParameters(c::MC_Data,d::InsuranceLogit,p::Array{T},p_est::parDict{Float64}) where T
    par = parMC(p,p_est,d,c) # Fix p0
    individual_costs(d,par)

    s_hat = par.pars.s_hat
    wgts = weight(d.data)[:]

    wgts_share = wgts.*s_hat
    c_hat = par.C_cap

    fpar = Vector{T}(undef,length(c.firmMoments))


    # c_avg = sliceMean_wgt(c_hat,wgts_share,m_idx)
    # δ = -(log(c_avg) - c.firmMoments[1])


    ## Firm Moments
    for (m,m_idx) in c._firmMomentDict
        c_avg = sliceMean_wgt(c_hat,wgts_share,m_idx)
        fpar[m] = -(log(c_avg) - c.firmMoments[m])
    end

    return fpar
end


function GMM_outer_loop(p::Vector{T},p_est::parDict{Float64},
                d::InsuranceLogit,c::MC_Data,W::Matrix{Float64};squared=false,itrFirms=false) where T
    p_full = fit_firm_moments(p,p_est,d,c,itrFirms=itrFirms)
    obj = GMM_objective(p_full,p_est,d,c,W,squared=squared)
    return obj
end


function estimate_NLOpt(p0::Vector{Float64},p_est::parDict{Float64},
                d::InsuranceLogit,c::MC_Data,W::Matrix{Float64};method=:LN_NELDERMEAD,bounded=false,squared=false,itrFirms=false,tol=1e-8,max_itr=2000)
    # Set up the optimization
    # opt = Opt(:LD_MMA, length(p0))
    # opt = Opt(:LD_TNEWTON_PRECOND_RESTART, length(p0))
    opt = Opt(method, length(p0))

    maxeval!(opt,max_itr)
    maxtime!(opt, 580000)
    ftol_rel!(opt,tol)
    xtol_rel!(opt,tol)

    lb = repeat([-50],inner=length(p0))
    # lb[2:3] .= 0.0
    ub = repeat([50],inner=length(p0))
    # # ub[14] = .99
    #
    if bounded
        lower_bounds!(opt, lb)
        upper_bounds!(opt, ub)
    end

    gmm(x) = GMM_outer_loop(x,p_est,d,c,W,squared=squared,itrFirms=itrFirms)
    grad = Vector{Float64}(undef,length(p0))
    # println(d.draws[1:30,:])
    disp_length = min(20,length(p0))
    count = 0
    function gmm(x, grad)
        count +=1
        x_displ = x[1:disp_length]
        println("Iteration $count at $x_displ")
        obj = gmm(x)
        # ForwardDiff.gradient!(grad, gmm, x)
        # grad_size = sqrt(dot(grad,grad))
        # println("Gradient size equals $grad_size")

        println("Objective equals $obj on iteration $count")

        return obj
    end

    # Set Objective
    min_objective!(opt, gmm)


    minf, minx, ret= optimize(opt, p0)


    println("Got $minf at $minx after $count iterations (returned $ret)")

    # Return the object
    return ret, minf, minx
end



function twopart_NR_GMM(p0::Vector{Float64},p_est::parDict{Float64},
                d::InsuranceLogit,c::MC_Data,W::Matrix{Float64};grad_tol=1e-8,f_tol=1e-8,x_tol=1e-8,max_itr=2000,strict=false,squared=false)

    ## Initialize Parameter Vector
    p_vec = p0
    N = length(p0)
    p_vec = Vector{Float64}(undef,c.par_length)
    p_vec[1:N] = p0[:]
    p_vec[(N+1):length(p_vec)] = rand(length(costdf._feIndex)).+2

    cnt = 0
    grad_size = 10000
    f_eval_old = 1.0
    # # Initialize δ

    # Initialize Gradient
    grad = Vector{Float64}(undef,length(p_vec))
    hess_new = Matrix{Float64}(undef,length(p_vec),length(p_vec))
    update = zeros(length(p_vec))
    f_final_val = 0.0
    max_trial_cnt = 0
    trial_max = 0
    p_last = copy(p_vec)
    grad_last = copy(grad)
    disp_length = min(length(p_vec),20)
    f_min = -1e3
    p_min  = similar(p_vec)
    no_progress=0
    no_progress_cnt = 0
    flag = "empty"
    if strict
        mistake_thresh = 1.000005
    else
        mistake_thresh = 1.25
    end
    ga_itr = 10

    ## Tolerance Counts
    f_tol_cnt = 0
    x_tol_cnt = 0
    # Maximize by Newtons Method
    while (cnt<max_itr)
        cnt+=1
        trial_cnt=0

        p_full = fit_firm_moments(p_vec[1:N],p_est,d,c)


        # Compute Gradient, holding δ fixed
        fval = GMM_objective!(hess_new,grad,p_full,p_est,d,c,W;squared=squared)
        println("Function Value is $fval at iteration $cnt")

        eigs = eigvals(hess_new)
        elg = maximum(eigs)
        esm = minimum(eigs)
        println("Hessian Eigenvalues between $elg and $esm")

        if (cnt==1) | (fval<f_min)
            if abs((fval-f_min)/f_min)<f_tol
                f_tol_cnt += 1
            end
            if maximum(abs.((p_full - p_min)./p_min))<x_tol
                x_tol_cnt += 1
            end
            println("Update Parameters, $trial_max")
            if trial_max==0
                no_progress_cnt=0
            end

            f_min = copy(fval)
            p_min[:] = p_full[:]

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
            mistake_thresh = 1.00
        end

        update = -inv(hess_new)*grad





        step_size = maximum(abs.(update))
        if step_size>10
            update = update./step_size
            ind = findall(abs.(update).==1)
            val_disp = p_vec[ind]
            println("Max Parameter Adjustment: $ind, $val_disp")
            step_size = 1
        end
        p_test = p_full .+ update


        p_last = copy(p_vec)
        p_vec = copy(p_test)
        grad_last = copy(grad)
        p_vec_disp = round.(p_vec[1:N],digits=4)
        f_final_val = fval
        println("Update Parameters to $p_vec_disp")


        println("Gradient Size: $grad_size")
        println("Step Size: $step_size")
        # println("Function Value is $f_test at iteration $cnt")
    end
    println("Lowest Function Value is $f_min at $p_min")
    return p_min,f_min
end


function twopart_GA_GMM(p0::Vector{Float64},p_est::parDict{Float64},
                d::InsuranceLogit,c::MC_Data,W::Matrix{Float64};grad_tol=1e-8,f_tol=1e-8,x_tol=1e-8,max_itr=2000,strict=false,squared=false  )

    ## Initialize Parameter Vector
    p_vec = p0
    N = length(p0)
    p_vec = Vector{Float64}(undef,c.par_length)
    p_vec[1:N] = p0[:]
    p_vec[(N+1):length(p_vec)] = rand(length(costdf._feIndex)).+2

    cnt = 0
    grad_size = 10000
    f_eval_old = 1.0
    # # Initialize δ

    # Initialize Gradient
    grad = Vector{Float64}(undef,length(p_vec))
    hess_new = Matrix{Float64}(undef,length(p_vec),length(p_vec))
    update = zeros(length(p_vec))
    f_final_val = 0.0
    max_trial_cnt = 0
    trial_max = 0
    p_last = copy(p_vec)
    grad_last = copy(grad)
    disp_length = min(length(p_vec),20)
    f_min = -1e3
    p_min  = similar(p_vec)
    no_progress=0
    no_progress_cnt = 0
    flag = "empty"
    if strict
        mistake_thresh = 1.000005
    else
        mistake_thresh = 1.25
    end
    ga_itr = 10

    ## Tolerance Counts
    f_tol_cnt = 0
    x_tol_cnt = 0
    # Maximize by Newtons Method
    while (cnt<max_itr)
        cnt+=1
        trial_cnt=0

        p_full = fit_firm_moments(p_vec[1:N],p_est,d,c)


        # Compute Gradient, holding δ fixed
        fval = GMM_objective!(grad,p_full,p_est,d,c,W;squared=squared)
        println("Function Value is $fval at iteration $cnt")

        grad_size = maximum(abs.(grad))
        if cnt==1
            step = 1/grad_size
        else
            g = p_full[1:N] - p_last[1:N]
            y = grad[1:N] - grad_last[1:N]
            step = abs.(dot(g,g)/dot(g,y))
        end


        if (cnt==1) | (fval<f_min)
            if abs((fval-f_min)/f_min)<f_tol
                f_tol_cnt += 1
            end
            if maximum(abs.((p_vec - p_min)./p_min))<x_tol
                x_tol_cnt += 1
            end
            println("Update Parameters, $trial_max")
            if trial_max==0
                no_progress_cnt=0
            end

            f_min = copy(fval)
            p_min[:] = p_full[:]

            no_progress=0
        else
            no_progress+=1
        end



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
            mistake_thresh = 1.00
        end


        p_test = p_full .- step.*grad

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
            p_test = p_full .- step.*grad
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
        # println("Function Value is $f_test at iteration $cnt")
    end
    println("Lowest Function Value is $f_min at $p_min")
    return p_min,f_min
end
