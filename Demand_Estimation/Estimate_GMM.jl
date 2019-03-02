# using NLopt
using ForwardDiff

function GMM_objective!(obj_grad::Vector{Float64},d::InsuranceLogit,p0::Array{T},W::Matrix{Float64}) where T
    grad = Vector{Float64}(undef,length(p0))
    hess = Matrix{Float64}(undef,length(p0),length(p0))
    par0 = parDict(d,p0)
    ll = log_likelihood!(hess,grad,d,par0)

    mom_grad = Matrix{Float64}(undef,length(p0),length(d.data.tMoments))
    mom = calc_risk_moments!(mom_grad,d,par0)

    moments = vcat(mom,grad)
    moments_grad = hcat(mom_grad,hess)

    #W = eye(length(d.data.tMoments)+length(p0))

    # moments = grad
    # moments_grad = hess
    #
    # W = eye(length(p0))
    # obj = 0.0
    # for i in 1:length(moments), j in 1:length(moments)
    #     obj+= W[i,j]*moments[j]*moments[i]
    # end
    #
    # obj_grad[:] .= 0.0
    # for k in 1:length(p0),i in 1:length(moments), j in 1:length(moments)
    #     obj_grad[k]+= W[i,j]*(moments[j]*moments_grad[k,i] + moments[i]*moments_grad[k,j])
    # end
    obj = calc_GMM_Obj(moments,W)

    calc_GMM_Grad!(obj_grad,moments,moments_grad,W)

    # obj = moments'*W*moments
    #
    # obj_grad[:] = moments_grad*W*moments + moments_grad*W'*moments

    return obj
end


function GMM_objective!(obj_hess::Matrix{Float64},obj_grad::Vector{Float64},d::InsuranceLogit,p0::Array{T},W::Matrix{Float64}) where T
    grad = Vector{Float64}(undef,length(p0))
    hess = Matrix{Float64}(undef,length(p0),length(p0))
    thD = Array{Float64,3}(undef,length(p0),length(p0),length(p0))
    par0 = parDict(d,p0)
    ll = log_likelihood!(thD,hess,grad,d,par0)


    mom_grad = Matrix{Float64}(undef,length(p0),length(d.data.tMoments))
    mom_hess = Array{Float64,3}(undef,length(p0),length(p0),length(d.data.tMoments))
    mom = calc_risk_moments!(mom_hess,mom_grad,d,par0)

    moments = vcat(mom,grad)
    moments_grad = hcat(mom_grad,hess)
    moments_hess = cat(mom_hess,thD,dims=3)
    # moments = grad
    # moments_grad = hess
    # moments_hess = thD

    obj = calc_GMM_Obj(moments,W)

    calc_GMM_Grad!(obj_grad,moments,moments_grad,W)
    calc_GMM_Hess_Large!(obj_hess,moments,moments_grad,moments_hess,W)

    return obj
end

function GMM_objective(d::InsuranceLogit,p0::Array{T},W::Matrix{Float64}) where T

    par0 = parDict(d,p0)
    grad = Vector{T}(undef,length(p0))
    ll = log_likelihood!(grad,d,par0)
    # individual_values!(d,par0)
    mom = calc_risk_moments(d,par0)

    moments = vcat(mom,grad)
    # moments = grad
    obj = calc_GMM_Obj(moments,W)
    return obj
end

function GMM_objective(d::InsuranceLogit,
                    p_small::Array{T,1},p0::Array{Float64},
                    W::Matrix{Float64}) where T
    L = length(p_small)
    p_vec = Vector{T}(undef,length(p0))
    # p_vec[(length(p0)-L+1):length(p0)] = p_small[:]
    # p_vec[1:(length(p0)-L)] = p0[1:(length(p0)-L)]
    p_vec[1:L] = p_small[:]
    p_vec[(L+1):length(p0)] = p0[(L+1):length(p0)]

    obj = GMM_objective(d,p_vec,W)
    return obj
end

function calc_GMM_Obj(moments::Vector{T},W::Matrix{Float64}) where T
    obj = 0.0
    for i in 1:length(moments), j in 1:length(moments)
        obj+= W[i,j]*moments[j]*moments[i]
    end
    return obj
end

function calc_GMM_Grad!(obj_grad::Vector{Float64},
                    moments::Vector{Float64},
                    moments_grad::Matrix{Float64},
                    W::Matrix{Float64})
    Q = length(obj_grad)
    obj_grad[:] .= 0.0
    for k in 1:Q,i in 1:length(moments), j in 1:length(moments)
        obj_grad[k]+= W[i,j]*(moments[j]*moments_grad[k,i] + moments[i]*moments_grad[k,j])
    end
end

function calc_GMM_Hess!(obj_hess::Matrix{Float64},
                    moments::Vector{Float64},
                    moments_grad::Matrix{Float64},
                    moments_hess::Array{Float64,3},
                    W::Matrix{Float64})
    obj_hess[:] .= 0.0
    Q,K = size(obj_hess)
    for k in 1:Q
        for l in 1:k
            for i in 1:length(moments)
                @inbounds @fastmath @simd for j in 1:length(moments)
                    obj_hess[k,l]+= W[i,j]*(moments[i]*moments_hess[k,l,j] + moments[j]*moments_hess[k,l,i] + moments_grad[k,i]*moments_grad[l,j] + moments_grad[l,i]*moments_grad[k,j])
                end
            end
            if l<k
                obj_hess[l,k]=obj_hess[k,l]
            end
        end
    end
end

function calc_GMM_Hess_Large!(obj_hess::Matrix{Float64},
                    moments::Vector{Float64},
                    moments_grad::Matrix{Float64},
                    moments_hess::Array{Float64,3},
                    W::Matrix{Float64})
    hess_vec = W*moments
    grad_mat = moments_grad*W*transpose(moments_grad)
    obj_hess[:] .= 0.0
    Q,K = size(obj_hess)
    for k in 1:Q
        for l in 1:k
            for i in 1:length(moments)
                obj_hess[k,l]+= 2*(hess_vec[i]*moments_hess[k,l,i])
            end
            obj_hess[k,l]+= 2*grad_mat[k,l]
            if l<k
                obj_hess[l,k]=obj_hess[k,l]
            end
        end
    end
end

# function estimate_GMM!(d::InsuranceLogit, p0::Vector{Float64},W::Matrix{Float64};method=:LN_NELDERMEAD)
#     # Set up the optimization
#     #opt_stage1 = Opt(:LD_LBFGS, length(p0))
#     opt_stage1 = Opt(:LD_TNEWTON_PRECOND_RESTART, length(p0))
#     #opt = Opt(:LD_MMA, length(p0))
#
#     #opt = Opt(:LD_TNEWTON_PRECOND_RESTART,length(p0))
#     #opt = Opt(:LD_TNEWTON,length(p0))
#     #opt = Opt(:LN_SBPLX, length(p0))
#     #opt = Opt(:LN_COBYLA, length(p0))
#
#     #maxeval!(opt_stage1,20000)
#     maxtime!(opt_stage1, 580000)
#     ftol_rel!(opt_stage1,1e-8)
#
#     lb = repeat([-1000],inner=length(p0))
#     # # lb[14] = 0.0
#     ub = repeat([1000],inner=length(p0))
#     # # ub[14] = .99
#     #
#     lower_bounds!(opt_stage1, lb)
#     upper_bounds!(opt_stage1, ub)
#
#
#     #gmm(x) = GMM_objective(d,x,W)
#     gmm_grad!(grad,x) = GMM_objective!(grad,d,x,W)
#     # println(d.draws[1:30,:])
#     disp_length = min(20,length(p0))
#     count = 0
#     function gmm(x, grad)
#         count +=1
#         x_displ = x[1:disp_length]
#         if count % 50 ==0
#             x_displ = round.(x,1)
#             println(find(abs.(x).>10))
#         end
#         println("Iteration $count at $x_displ")
#         #obj = ll(x)
#         obj = gmm_grad!(grad,x)
#         grad_size = sqrt(dot(grad,grad))
#         println("Gradient size equals $grad_size")
#         #ForwardDiff.gradient!(grad, gmm, x)
#
#         println("Objective equals $obj on iteration $count")
#
#         return obj
#     end
#     # Set Objective
#     min_objective!(opt_stage1, gmm)
#
#     # Run Optimization
#     # maxeval!(opt_stage1,10)
#     #
#     # lb = repeat([-5],inner=length(p0))
#     # # lb[14] = 0.0
#     # ub = repeat([5],inner=length(p0))
#     # # ub[14] = .99
#     # lower_bounds!(opt_stage1, lb)
#     # upper_bounds!(opt_stage1, ub)
#     # minf, minx, ret= optimize(opt_stage1, p0)
#
#     maxeval!(opt_stage1,25000)
#     #lb = repeat([-Inf],inner=length(p0))
#     # lb[14] = 0.0
#     #ub = repeat([Inf],inner=length(p0))
#     # ub[14] = .99
#     lower_bounds!(opt_stage1, lb)
#     upper_bounds!(opt_stage1, ub)
#     minf, minx, ret= optimize(opt_stage1, p0)
#
#
#     println("In Stage 1, got $minf at $minx after $count iterations (returned $ret)")
#
#     # Return the object
#     return ret, minf, minx
# end

function estimate_GMM(d::InsuranceLogit, p0::Vector{Float64},W::Matrix{Float64})
    # First run a gradient ascent method to get close to optimum
    println("Gradient Ascent Method")
    p_est, fval = gradient_ascent_BB(d,p0,W,max_itr=500)
    # Then run newtons method until better convergence
    println("Newtons Method")
    p_est, fval = newton_raphson_GMM(m,p_est,W,grad_tol = 1e-8)

    return p_est,fval
end



#
# function newton_raphson_GMM(d,p0,W;grad_tol=1e-8,x_tol=1e-8,f_tol=1e-8,max_itr=2000)
#     ## Initialize Parameter Vector
#     p_vec = p0
#     N = length(p0)
#
#
#     grad_size = 10000
#     f_eval_old = 1.0
#     # # Initialize δ
#     param_dict = parDict(d,p_vec)
#
#     # Initialize Gradient
#     grad_new = similar(p0)
#     hess_new = Matrix{Float64}(undef,length(p0),length(p0))
#
#     cnt = 0
#     ga_cnt = 0
#     stall_cnt = 0
#
#     # Save Minimizing Function Value and Parameter Vector
#     f_min=0.0
#     p_min = Vector{Float64}(undef,length(p0))
#     p_last = copy(p_vec)
#
#     f_tol_cnt = 0
#     x_tol_cnt = 0
#
#     # Maximize by Newtons Method
#     while (grad_size>grad_tol) & (cnt<max_itr)
#         cnt+=1
#         # Compute Gradient, holding δ fixed
#
#         fval = GMM_objective!(hess_new,grad_new,d,p_vec,W)
#
#         grad_size = sqrt(dot(grad_new,grad_new))
#         if (grad_size<1e-8) & (cnt>10)
#             println("Gradient Near Zero, Local Minimum Found")
#             println("Gradient Size: $grad_size")
#             println("Function Value is $f_min at iteration $p_min")
#             break
#         end
#
#         # if (stall_cnt>5)
#         #     println("No Better Point Found")
#         #     println("Nearby Gradient Size: $grad_size")
#         #     println("Function Value is $f_min at $p_min")
#         #     break
#         # end
#
#         step_size = 1/grad_size
#
#         # ForwardDiff.gradient!(grad_new, ll, p_vec)
#         # println("Gradient is $grad_new")
#         #
#         #
#         # hess_new = Matrix{Float64}(N,N)
#         # ForwardDiff.hessian!(hess_new, ll, p_vec)
#         # println("Hessian is $hess_new")
#
#         update = -inv(hess_new)*grad_new
#
#         if any(isnan.(update))
#             p_vec = p_last.*(1 .+ rand(length(update))/100 .-.005)
#             println("Algorithm Went to Undefined Area: Random Step")
#             grad_size = 1
#             continue
#         end
#         # evals = eigvals(hess_new)
#         # min_e = minimum(evals)
#         # max_e = maximum(evals)
#         # println("Eiganvalues range from $min_e to $max_e")
#
#         p_test = p_vec .+ update
#         f_test = GMM_objective(d,p_test,W)
#         trial_cnt = 0
#         trial_end = 4
#         while ((f_test>fval) | isnan(f_test)) & (trial_cnt<=trial_end)
#             if trial_cnt==0
#                 p_test_disp = p_test[1:20]
#                 println("Trial (Init): Got $f_test at parameters $p_test_disp")
#                 println("Previous Iteration at $fval")
#             end
#             if trial_cnt<trial_end
#                 update/= 10
#                 p_test = p_vec .+ update
#                 f_test = GMM_objective(d,p_test,W)
#                 p_test_disp = p_test[1:20]
#                 println("Trial (NR): Got $f_test at parameters $p_test_disp")
#                 println("Previous Iteration at $fval")
#                 trial_cnt+=1
#             else
#                 ga_cnt+=1
#                 println("RUN ROUND OF GRADIENT ASCENT")
#                 p_test, f_test = gradient_ascent_GMM(d,p_vec,W,max_itr=5,strict=true)
#                 trial_cnt+=1
#             end
#         end
#         ### Update Minimum Vector Value
#         if (f_test<f_min) | (cnt<3)
#             stall_cnt = 0
#             f_min = f_test
#             p_min[:] = p_test[:]
#         else
#             stall_cnt+=1
#             println("STALL COUNT: $stall_cnt")
#         end
#
#
#         if trial_cnt<=trial_end
#             ga_cnt = 0
#         elseif ga_cnt>2
#             ga_cnt = 0
#             println("Algorithm Stalled: Random Step")
#             update = rand(length(update))/10000 .-.0005
#             p_test = p_test .+update
#         end
#
#
#         # update = p_test - p_vec
#         p_last = copy(p_vec)
#         p_vec = copy(p_test)
#         p_vec_disp = p_vec[1:20]
#         println("Update Parameters to $p_vec_disp")
#
#
#         println("Gradient Size: $grad_size")
#         println("Function Value is $f_test at iteration $cnt")
#     end
#     # if (grad_size>grad_tol)
#     #     println("Estimate Instead")
#     #     ret, f_final_val, p_vec = estimate!(d,p0)
#
#
#     return p_min,f_min
# end

function gradient_ascent_GMM(d,p0,W;grad_tol=1e-8,step_tol=1e-8,max_itr=2000)
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
    hess_new = Matrix{Float64}(undef,length(p0),length(p0))
    f_final_val = 0.0
    max_trial_cnt = 0
    p_last = p_vec
    # Maximize by Newtons Method
    while (grad_size>grad_tol) & (count<max_itr) & (max_trial_cnt<20)
        count+=1


        # Compute Gradient, holding δ fixed

        fval = GMM_objective!(grad_new,d,p_vec,W)


        grad_size = sqrt(dot(grad_new,grad_new))
        if (grad_size<1e-8) & (count>10)
            println("Got to Break Point...?")
            println(grad_size)
            break
        end
        step = 1/grad_size
        # ForwardDiff.gradient!(grad_new, ll, p_vec)
        # println("Gradient is $grad_new")
        #
        #
        # hess_new = Matrix{Float64}(N,N)
        # ForwardDiff.hessian!(hess_new, ll, p_vec)
        # println("Hessian is $hess_new")



        p_test = p_vec .- step.*grad_new
        f_test = GMM_objective(d,p_test,W)
        trial_cnt = 0
        while ((f_test>fval) | isnan(f_test)) & (trial_cnt<10)
            if trial_cnt==0
                p_test_disp = p_test[1:20]
                println("Trial (Init): Got $f_test at parameters $p_test_disp")
                println("Previous Iteration at $fval")
                println("Reduce Step size....")
            end
            if trial_cnt<10
                step/= 100
                p_test_disp = p_test[1:20]
                p_test = p_vec .- step.*grad_new
                f_test = GMM_objective(d,p_test,W)
                # println("Trial (GA): Got $f_test at parameters $p_test_disp")
                # println("Previous Iteration at $fval")
                trial_cnt+=1
            elseif (trial_cnt==10) & (grad_size>1e-5)
                println("Algorithm Stalled: Random Step")
                max_trial_cnt+=1
                step = rand(length(step))/1000 .-.005
            elseif (trial_cnt==10) & (grad_size<=1e-5)
                println("Algorithm Stalled: Random Step")
                max_trial_cnt+=1
                step = rand(length(step))/10000 .-.005
            end
        end
        par_step = p_test - p_vec
        p_last = copy(p_vec)
        p_vec = copy(p_test)
        p_vec_disp = p_vec[1:20]
        f_final_val = f_test
        println("Update Parameters to $p_vec_disp")


        println("Gradient Size: $grad_size")
        println("Step Size: $step")
        println("Function Value is $f_test at iteration $count")
    end
    # if (grad_size>grad_tol)
    #     println("Estimate Instead")
    #     ret, f_final_val, p_vec = estimate!(d,p0)


    return p_vec,f_final_val
end




function gradient_ascent_BB(d,p0,W;grad_tol=1e-8,f_tol=1e-8,x_tol=1e-8,max_itr=2000,strict=false)
    ## Initialize Parameter Vector
    p_vec = p0
    N = length(p0)

    cnt = 0
    grad_size = 10000
    f_eval_old = 1.0
    # # Initialize δ
    param_dict = parDict(d,p_vec)

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

        fval = GMM_objective!(grad_new,d,p_vec,W)
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
            fval = GMM_objective!(grad_new,d,p_vec,W)
            grad_size = maximum(abs.(grad_new))
            step = 1/grad_size
            mistake_thresh = 1.00
        end



        p_test = p_vec .- step.*grad_new

        f_test = GMM_objective(d,p_test,W)
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
            f_test = GMM_objective(d,p_test,W)
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
    # # Initialize δ
    param_dict = parDict(d,p_vec)

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

        fval = GMM_objective!(hess_new,grad_new,d,p_vec,W)

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
            fval = GMM_objective!(grad_new,d,p_vec,W)
            grad_size = maximum(abs.(grad_new))
            step = 1/grad_size
            update = - step.*grad_new
            mistake_thresh = 1.00
        end


        p_test = p_vec .+ update

        f_test = GMM_objective(d,p_test,W)

        while ((f_test>fval*mistake_thresh) | isnan(f_test)) & (trial_cnt<=trial_end)
            if trial_cnt==0
                p_test_disp = p_test[1:20]
                println("Trial (Init): Got $f_test at parameters $p_test_disp")
                println("Previous Iteration at $fval")
            end
            if trial_cnt<trial_end
                update/= 10
                p_test = p_vec .+ update
                f_test = GMM_objective(d,p_test,W)
                p_test_disp = p_test[1:20]
                println("Trial (NR): Got $f_test at parameters $p_test_disp")
                println("Previous Iteration at $fval")
                trial_cnt+=1
            else
                println("RUN ROUND OF GRADIENT ASCENT")
                p_test, f_test = gradient_ascent_BB(d,p_vec,W,max_itr=10,strict=true)
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
