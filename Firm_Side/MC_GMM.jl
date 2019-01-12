using NLopt
using ForwardDiff

function GMM_objective(p::Vector{T},p_est::parDict{Float64},
                d::InsuranceLogit,c::MC_Data) where T
    par = parMC(p,p_est,d,c)
    individual_costs(d,par)
    moments = costMoments(c,d,par)
    obj = sum(moments.^2)
    return obj
end


function estimate_GMM(p0::Vector{Float64},p_est::parDict{Float64},
                d::InsuranceLogit,c::MC_Data;bounded=false)
    # Set up the optimization
    opt = Opt(:LD_MMA, length(p0))
    # opt = Opt(:LD_TNEWTON_PRECOND_RESTART, length(p0))

    #maxeval!(opt_stage1,20000)
    maxtime!(opt, 580000)
    ftol_rel!(opt,1e-8)

    lb = repeat([-10],inner=length(p0))
    # # lb[14] = 0.0
    ub = repeat([10],inner=length(p0))
    # # ub[14] = .99
    #
    if bounded
        lower_bounds!(opt, lb)
        upper_bounds!(opt, ub)
    end

    gmm(x) = GMM_objective(x,p_est,d,c)
    grad = Vector{Float64}(undef,length(p0))
    # println(d.draws[1:30,:])
    disp_length = min(20,length(p0))
    count = 0
    function gmm(x, grad)
        count +=1
        x_displ = x[1:disp_length]
        println("Iteration $count at $x_displ")
        obj = gmm(x)
        ForwardDiff.gradient!(grad, gmm, x)
        grad_size = sqrt(dot(grad,grad))
        println("Gradient size equals $grad_size")

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
