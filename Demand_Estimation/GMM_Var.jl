function GMM_var(d::InsuranceLogit,p_est::Vector{Float64})
    ## Moment Variance
    S =  calc_mom_Avar(d,p_est)

    ## Derivative of Moments wrt Parameters
    grad = Vector{Float64}(undef,length(p_est))
    hess = Matrix{Float64}(undef,length(p_est),length(p_est))
    par = parDict(d,p_est)
    ll = log_likelihood!(hess,grad,d,par)

    mom_grad = Matrix{Float64}(undef,length(p_est),length(d.data.tMoments))
    mom = calc_risk_moments!(mom_grad,d,par)

    G = hcat(mom_grad,hess)

    ## Calculate Variance
    println("Invert Matrices")
    Avar = inv(G*inv(S)*G')
    Pop =sum(weight(d.data).*choice(d.data))

    V = (1/sqrt(Pop)).*Avar

    ## Calculate Standard Error
    if any(diag(V.<0))
        println("Some negative variances")
        stdErr = sqrt.(abs.(diag(V)))
    else
        stdErr = sqrt.(diag(V))
    end
    t_stat = p_est./stdErr

    stars = Vector{String}(undef,length(t_stat))
    for i in 1:length(stars)
        if abs(t_stat[i])>2.326
            stars[i] = "***"
        elseif abs(t_stat[i])>1.654
            stars[i] = "**"
        elseif abs(t_stat[i])>1.282
            stars[i] = "*"
        else
            stars[i] = ""
        end
    end

    return V, stdErr, t_stat, stars
end
