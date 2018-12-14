function expected_outcomes!(hess::Matrix{Float64},grad::Vector{Float64},
                            d::InsuranceLogit,p::parDict{T};cont_flag::Bool=false) where T
    Q = d.parLength[:All]
    N = size(d.draws,1)

    hess[:] .= 0.0
    grad[:] .= 0.0
    ll= 0.0
    Pop =sum(weight(d.data).*choice(d.data))

    #Reset Derivatives
    p.dSdθ_j[:] .= 0.0
    p.dRdθ_j[:] .= 0.0
    p.d2Sdθ_j[:] .= 0.0
    p.d2Rdθ_j[:] .= 0.0

    # thD_obs = Array{Float64,3}(Q,Q,Q)
    # hess_obs = Matrix{Float64}(Q,Q)
    # grad_obs = Vector{Float64}(Q)

    if cont_flag
        contraction!(d,p)
    else
        individual_values!(d,p)
        individual_shares(d,p)
    end

    #shell_full = zeros(Q,N,38)
    for app in eachperson(d.data)
        K = length(person(app))
        # if K>k_max
        #     k_max = K
        # end
        #shell = shell_full[:,:,1:K]
        ll_obs,pars_relevant = ll_obs_hessian!(hess,grad,app,d,p)
        ll+=ll_obs
        #add_obs_mat!(hess,grad,hess_obs,grad_obs,Pop,pars_relevant)
    end

    moments = Vector{Float64}(undef,length(d.prods))

    calc_prod_moments!(moments,d,p)

    return moments
end

function expected_outcomes(d::InsuranceLogit,p::parDict{T};cont_flag::Bool=false) where T
    Q = d.parLength[:All]
    N = size(d.draws,1)

    Pop =sum(weight(d.data).*choice(d.data))

    #Reset Derivatives
    p.dSdθ_j[:] .= 0.0
    p.dRdθ_j[:] .= 0.0
    p.d2Sdθ_j[:] .= 0.0
    p.d2Rdθ_j[:] .= 0.0

    # thD_obs = Array{Float64,3}(Q,Q,Q)
    # hess_obs = Matrix{Float64}(Q,Q)
    # grad_obs = Vector{Float64}(Q)

    if cont_flag
        contraction!(d,p)
    else
        individual_values!(d,p)
        individual_shares(d,p)
    end


    moments = Vector{Float64}(undef,length(d.prods))

    calc_prod_moments!(moments,d,p)

    return moments
end


function calc_prod_moments!(moments::Vector{T},d::InsuranceLogit,p::parDict{T}) where T
    wgts = transpose(weight(d.data))[:,1]
    for j in d.prods
        j_index_all = d.data._productDict[j]
        s_hat_j= sliceMean_wgt(p.s_hat,wgts,j_index_all)
        s_j = d.shares[j]
        moments[j] = s_j - s_hat_j
    end
    return nothing
end

function GMM_objective_exp!(obj_hess::Matrix{Float64},obj_grad::Vector{Float64},d::InsuranceLogit,p0::Array{T},W::Matrix{Float64}) where T
    grad = Vector{Float64}(undef,length(p0))
    hess = Matrix{Float64}(undef,length(p0),length(p0))
    par0 = parDict(d,p0)
    share_moments = expected_outcomes!(hess,grad,d,par0)


    risk_grad = Matrix{Float64}(undef,length(p0),length(d.data.tMoments))
    risk_hess = Array{Float64,3}(undef,length(p0),length(p0),length(d.data.tMoments))
    risk_mom = calc_risk_moments!(risk_hess,risk_grad,d,par0)

    moments = vcat(risk_mom,share_moments)
    moments_grad = hcat(risk_grad,par0.dSdθ_j)
    moments_hess = cat(risk_hess,par0.d2Sdθ_j,dims=3)
    # moments = share_moments
    # moments_grad = par0.dSdθ_j
    # moments_hess = par0.d2Sdθ_j

    obj = calc_GMM_Obj(moments,W)

    calc_GMM_Grad!(obj_grad,moments,moments_grad,W)
    # calc_GMM_Hess!(obj_hess,moments,moments_grad,moments_hess,W)
    calc_GMM_Hess_Large!(obj_hess,moments,moments_grad,moments_hess,W)

    return obj
end


function GMM_objective_exp!(obj_grad::Vector{Float64},d::InsuranceLogit,p0::Array{T},W::Matrix{Float64}) where T
    grad = Vector{Float64}(undef,length(p0))
    par0 = parDict(d,p0)
    share_moments = expected_outcomes!(grad,d,par0)


    risk_grad = Matrix{Float64}(undef,length(p0),length(d.data.tMoments))
    risk_hess = Array{Float64,3}(undef,length(p0),length(p0),length(d.data.tMoments))
    risk_mom = calc_risk_moments!(risk_hess,risk_grad,d,par0)

    moments = vcat(risk_mom,share_moments)
    moments_grad = hcat(risk_grad,par0.dSdθ_j)
    # moments_hess = cat(risk_hess,par0.d2Sdθ_j,dims=3)
    # moments = share_moments
    # moments_grad = par0.dSdθ_j

    obj = calc_GMM_Obj(moments,W)

    calc_GMM_Grad!(obj_grad,moments,moments_grad,W)
    # calc_GMM_Hess!(obj_hess,moments,moments_grad,moments_hess,W)
    calc_GMM_Hess_Large!(obj_hess,moments,moments_grad,moments_hess,W)

    return obj
end



function GMM_objective_exp(d::InsuranceLogit,p0::Array{T},W::Matrix{Float64}) where T
    par0 = parDict(d,p0)
    share_moments = expected_outcomes(d,par0)
    risk_mom = calc_risk_moments(d,par0)

    moments = vcat(risk_mom,share_moments)

    obj = calc_GMM_Obj(moments,W)

    return obj
end
