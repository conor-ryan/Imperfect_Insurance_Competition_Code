using NLopt
using ForwardDiff


function GMM_objective{T}(d::InsuranceLogit,p0::Array{T})
    grad = Vector{Float64}(length(p0))
    par0 = parDict(d,p0)
    ll = log_likelihood!(grad,d,par0)
    mom = calc_risk_moments(d,par0)
    obj = 0.0
    for n in eachindex(grad)
        obj+= grad[n]^2
    end
    for n in eachindex(mom)
        obj+=mom[n]^2
    end
    return obj
end


function estimate_GMM!(d::InsuranceLogit, p0;method=:LN_NELDERMEAD)
    # Set up the optimization
    opt_stage1 = Opt(method, length(p0))
    #opt = Opt(:LD_MMA, length(p0))

    #opt = Opt(:LD_TNEWTON_PRECOND_RESTART,length(p0))
    #opt = Opt(:LD_TNEWTON,length(p0))
    #opt = Opt(:LN_SBPLX, length(p0))
    #opt = Opt(:LN_COBYLA, length(p0))

    maxeval!(opt_stage1,20000)
    ftol_rel!(opt_stage1,1e-8)

    lb = repeat([-Inf],inner=length(p0))
    # lb[14] = 0.0
    ub = repeat([Inf],inner=length(p0))
    # ub[14] = .99

    lower_bounds!(opt_stage1, lb)
    upper_bounds!(opt_stage1, ub)


    gmm(x) = GMM_objective(d,x)
    # println(d.draws[1:30,:])
    disp_length = min(20,length(p0))
    count = 0
    function gmm(x, grad)
        count +=1
        x_displ = x[1:disp_length]
        if count % 50 ==0
            x_displ = round.(x,1)
            println(find(abs.(x).>10))
        end
        println("Iteration $count at $x_displ")
        #obj = ll(x)
        obj = gmm(x)
        #ForwardDiff.gradient!(grad, gmm, x)

        println("Objective equals $obj on iteration $count")

        return obj
    end
    # Set Objective
    min_objective!(opt_stage1, gmm)

    # Run Optimization
    minf, minx, ret= optimize(opt_stage1, p0)
    println("In Stage 1, got $init_minf at $init_minx after $count iterations (returned $init_ret)")

    # Return the object
    return ret, minf, minx
end
