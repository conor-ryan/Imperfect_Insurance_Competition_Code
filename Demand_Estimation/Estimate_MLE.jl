import Base.getindex, Base.setindex!, Base.show
using NLopt
using ForwardDiff


# Calculate Log Likelihood
function log_likelihood{T}(d::InsuranceLogit,p::parDict{T})
    ll = 0.0
    Pop = 0.0
    γ = p.γ
    β = p.β
    #α = p.α[1]
    # Calculate μ_ij, which depends only on parameters
    individual_values!(d,p)
    individual_shares(d,p)
    for app in eachperson(d.data)
    #app = next(eachperson(d.data),100)[1]
        ind = person(app)[1]
        S_ij = transpose(choice(app))
        wgt = transpose(weight(app))
        urate = transpose(unins(app))
        idxitr = d.data._personDict[ind]


        δ = p.δ[idxitr]
        s_hat = p.s_hat[idxitr]
        s_insured = sum(s_hat)

        for i in eachindex(idxitr)
            ll+=wgt[i]*S_ij[i]*(log(s_hat[i]) -urate[i]*(log(s_insured)-log(1-s_insured)))
            Pop+=wgt[i]*S_ij[i]
        end
    end
    return ll/Pop
end

function log_likelihood{T}(d::InsuranceLogit,p::Array{T})
    params = parDict(d,p)
    ll = log_likelihood(d,params)
    convert_δ!(d)
    return ll
end

# Calculate Log Likelihood
function ll_gradient{T}(d::InsuranceLogit,p::parDict{T})
    p_num = length(p.γ) + length(p.β_0) + length(p.β) + length(p.σ)
    ll = fill(0.0,(p_num))
    Pop = 0.0
    γ = p.γ
    β = p.β
    #α = p.α[1]
    # Calculate μ_ij, which depends only on parameters
    for app in eachperson(d.data)
        μ_ij = util_value!(app,p)
        dμ_ij = util_gradient(d,app,p)
        ind = person(app)[1]
        S_ij = transpose(choice(app))
        wgt = transpose(weight(app))
        urate = transpose(unins(app))
        idxitr = d.data._personDict[ind]


        # dμ_ij_sums = sum(dμ_ij,(1,3))


        δ = p.δ[idxitr]
        s_hat = calc_shares(μ_ij,δ)
        s_insured = sum(s_hat)
        #s2 = fill(0.0,K)
        (Q,N,K) = size(dμ_ij)

        for k = 1:K
            dμ_ij[:,:,k] = dμ_ij[:,:,k].*δ[k]
        end

        μ_ij_sums = 1.+μ_ij*δ
        μ_ij_sums_sq = (1.+μ_ij*δ).^2
        dμ_ij_sums = sum(dμ_ij,3)

        for k = 1:K,q in 1:Q
            # ll[q] += wgt[k]/N*S_ij[k]*( (1/s_hat[k])*(
            #             dμ_ij[q,n,k]/μ_ij_sums[n] -
            #             dμ_ij_sums[q,n,1]*μ_ij[n,k]*δ[k]/μ_ij_sums_sq[n] ) -
            #     urate[k]*( dμ_ij_sums[q,n,1]/μ_ij_sums_sq[n] )*(
            #                     1/(s_insured) + 1/(1-s_insured) ) )
            t1 = mean(dμ_ij[q,:,k]./μ_ij_sums)
            t2 = mean(dμ_ij_sums[q,:,1].*μ_ij[:,k].*δ[k]./μ_ij_sums_sq)
            t3 = mean(dμ_ij_sums[q,:,1]./μ_ij_sums_sq)
            ll[q] += wgt[k]*S_ij[k]*( (1/s_hat[k])*(t1 - t2) -
                urate[k]*( t3 )*(1/(s_insured) + 1/(1-s_insured) ) )
        end
        Pop+= sum(wgt.*S_ij)
    end
    return ll./Pop
    # return fval/Pop
end


function ll_gradient{T}(d::InsuranceLogit,p::Array{T})
    params = parDict(d,p)
    grad = ll_gradient(d,params)
    convert_δ!(d)
    return grad
end

function GMM_objective{T}(d::InsuranceLogit,p::Array{T})
    grad = ll_gradient(d,p)
    println("gradient equals $grad")
    obj = vecdot(grad,grad)
    return obj
end


function evaluate_iteration{T}(d::InsuranceLogit,p::parDict{T};update::Bool=true)
    contraction!(d,p,update=update)
    ll = log_likelihood(d,p)
    convert_δ!(d)
    return ll
end

function evaluate_iteration!{T}(d::InsuranceLogit, x::Array{T,1};update::Bool=true)
    # Create Parameter Types
    parameters = parDict(d,x)
    return evaluate_iteration(d,parameters,update=update)
end


function estimate!(d::InsuranceLogit, p0)
    # Set up the optimization
    #opt = Opt(:LD_MMA, length(p0))
    opt = Opt(:LN_NELDERMEAD, length(p0))
    #opt = Opt(:LD_TNEWTON_PRECOND_RESTART,length(p0))
    #opt = Opt(:LD_TNEWTON,length(p0))
    #opt = Opt(:LN_SBPLX, length(p0))
    #opt = Opt(:LN_COBYLA, length(p0))
    xtol_rel!(opt, 1e-6)
    xtol_abs!(opt, 1e-6)
    ftol_rel!(opt, 1e-10)
    #maxeval!(opt,2100)
    maxtime!(opt, 600000)
    #upper_bounds!(opt, ones(length(p0))/10)
    initial_step!(opt,1e-1)
    #stopval!(opt,.00040)
    # Objective Function
    # ll(x) = evaluate_iteration!(d, x,update=false)
    # cfg = ForwardDiff.GradientConfig(ll, p0, ForwardDiff.Chunk{6}());
    ll(x) = log_likelihood(d,x)
    gmm(x) = GMM_objective(d,x)
    #δ_cont(x) = contraction!(d,x,update=false)
    δ_cont(x) = contraction!(d,x)
    count = 0
    function ll(x, grad)
        count +=1
        println("Iteration $count at $x")
        #Store Gradient
        # println("Step 1")
        δ_cont(x)
        # println("Step 2")
        #ForwardDiff.gradient!(grad, gmm, x)
        # println("Gradient: $grad")
        #obj = gmm(x)
        obj = ll(x)
        #likelihood = ll(x)
        println("Objective equals $obj on iteration $count")
        return obj
    end

    # Set Objective
    max_objective!(opt, ll)

    # Run Optimization
    minf, minx, ret = optimize(opt, p0)
    println("got $minf at $minx after $count iterations (returned $ret)")

    # Return the object
    return ret, minf, minx
end

function gradient_ascent(d,p0;max_step=1e-5,init_step=1e-9,max_itr=2000,grad_tol=1e2)
    ## Initialize Parameter Vector
    p_vec = p0
    # Step Size
    #max_step = 1e-7
    step = init_step
    # Likelihood Functions
    #ll(x) = log_likelihood(d,x)
    ll(x) = evaluate_iteration!(d,x,update=false)
    # Tracking Variables
    count = 0
    grad_size = 1e8
    tol = 1
    f_eval_old = 1.0
    # # Initialize δ
    param_dict = parDict(d,p_vec)
    contraction!(d,param_dict)
    cfg = ForwardDiff.GradientConfig(ll, p_vec, ForwardDiff.Chunk{6}());
    # Maximize by Gradient Ascent
    while (grad_size>grad_tol) & (count<max_itr)
        count+=1
        # Compute δ with Contraction
        println("Update δ")
        param_dict = parDict(d,p_vec)
        contraction!(d,param_dict)
        # Evaluate Likelihood
        f_eval = ll(p_vec)
        println("likelihood equals $f_eval on iteration $count")

        if ((count>1) & ((f_eval-f_eval_old)/f_eval_old > 0.02)) | isnan(f_eval)
            step = step_old/10
            p_vec = p_old + step.*grad_new
            println("Reset Parameters to $p_vecvec")
            step_old = copy(step)
            continue
        end

        # Compute Gradient, holding δ fixed
        grad_new = similar(p_vec)
        ForwardDiff.gradient!(grad_new, ll, p_vec,cfg)
        println("Gradient is $grad_new")

        #Save Iteration
        p_old = copy(p_vec)
        f_eval_old = copy(f_eval)
        step_old = copy(step)

        # Update Parameters
        p_vec += step.*grad_new
        println("Update Parameters to $p_vec")


        # New Step Size
        if count>1
            grad_diff = (grad_new-grad_old)
            step = abs(vecdot(step.*grad_new,grad_diff)/vecdot(grad_diff,grad_diff))
            println("New optimal step size: $step")
        end
        # Save Gradient
        grad_old = copy(grad_new)

        grad_size = sqrt(vecdot(grad_new,grad_new))
        println("Gradient Size: $grad_size")

        #Update step size
        step = min(step,max_step)
    end
    return p_vec
end


function newton_raphson(d,p0;max_step=1e-5,max_itr=2000)
    ## Initialize Parameter Vector
    p_vec = p0
    N = length(p0)
    # Step Size
    #max_step = 1e-7
    step = max_step
    # Likelihood Functions
    #ll(x) = log_likelihood(d,x)
    ll(x) = evaluate_iteration!(d,x,update=false)
    # Tracking Variables
    count = 0
    grad_size = 10000
    tol = 1
    f_eval_old = 1.0
    # # Initialize δ
    param_dict = parDict(d,p_vec)
    #cfg = ForwardDiff.GradientConfig(ll, p_vec, ForwardDiff.Chunk{4}())
    #contraction!(d,param_dict)
    # Maximize by Gradient Ascent
    while (grad_size>1000) & (count<max_itr)
        count+=1
        # Compute δ with Contraction
        println("Update δ")
        param_dict = parDict(d,p_vec)
        contraction!(d,param_dict)
        # Evaluate Likelihood
        f_eval = ll(p_vec)
        println("likelihood equals $f_eval on iteration $count")

        # if (count>1) & ((f_eval-f_eval_old)/f_eval_old > 0.02)
        #     println("Reset")
        #     step = step_old/10
        #     p_vec = p_old + step.*grad_new
        #     continue
        # end

        # Compute Gradient, holding δ fixed
        grad_new = similar(p_vec)
        ForwardDiff.gradient!(grad_new, ll, p_vec)
        println("Gradient is $grad_new")


        hess_new = Matrix{Float64}(N,N)
        ForwardDiff.hessian!(hess_new, ll, p_vec)
        println("Hessian is $hess_new")

        #Save Iteration
        p_old = copy(p_vec)
        f_eval_old = copy(f_eval)

        # Update Parameters
        p_vec += inv(hess_new)*grad_new
        p_vec += step.*grad_new
        println("Update Parameters to $p_vec")


        # New Step Size
        if count>1
            grad_diff = (grad_new-grad_old)
            step = abs(vecdot(step.*grad_new,grad_diff)/vecdot(grad_diff,grad_diff))
            println("New optimal step size: $step")
        end
        # Save Gradient
        grad_old = copy(grad_new)

        grad_size = sqrt(vecdot(grad_new,grad_new))
        println("Gradient Size: $grad_size")

        #Update step size
        step = min(step,max_step)
    end
    return p_vec
end
