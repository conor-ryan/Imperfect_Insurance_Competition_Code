function log_likelihood_penalty(d::InsuranceLogit,p::Array{T},W::Matrix{Float64}
                                    ;cont_flag=false) where T
    params = parDict(d,p,no2Der=true)
    ll = log_likelihood(d,params,cont_flag = cont_flag)

    mom = calc_risk_moments(d,p)

    obj = calc_GMM_Obj(mom,W)
    return ll+obj
end


function log_likelihood_penalty!(grad::Vector{Float64},d::InsuranceLogit,p::Array{T},W::Matrix{Float64};
                        cont_flag::Bool=false,
                        feFlag=-1) where T

    params = parDict(d,p,no2Der=true)

    #Reset Derivatives
    grad[:].=0.0
    params.dSdθ_j[:] .= 0.0
    params.dRdθ_j[:] .= 0.0
    params.d2Sdθ_j[:] .= 0.0
    params.d2Rdθ_j[:] .= 0.0


    ll = log_likelihood!(grad,d,params,cont_flag = cont_flag,feFlag=feFlag)

    mom_grad = Matrix{Float64}(undef,length(p),length(d.data.rMoments))
    mom = calc_risk_moments!(mom_grad,d,params,feFlag=feFlag)


    obj = calc_GMM_Obj(mom,W)
    calc_GMM_Grad!(grad,mom,mom_grad,W)

    return ll + obj
end

function log_likelihood_penalty_parallel!(grad::SharedArray{Float64,1},d::InsuranceLogit,p::Array{T},W::Matrix{Float64};
                        cont_flag::Bool=false,
                        feFlag=-1) where T

    params = parDict(d,p,no2Der=true)

    #Reset Derivatives
    grad[:].=0.0
    params.dSdθ_j[:] .= 0.0
    params.dRdθ_j[:] .= 0.0
    params.d2Sdθ_j[:] .= 0.0
    params.d2Rdθ_j[:] .= 0.0


    ll = log_likelihood_parallel!(grad,d,params,cont_flag = cont_flag,feFlag=feFlag)

    mom_grad = Matrix{Float64}(undef,length(p),length(d.data.rMoments))
    mom = calc_risk_moments!(mom_grad,d,params,feFlag=feFlag)


    obj = calc_GMM_Obj(mom,W)
    calc_GMM_Grad!(grad,mom,mom_grad,W)

    return ll + obj
end

function log_likelihood_penalty!(hess::Matrix{Float64},grad::Vector{Float64},
                            d::InsuranceLogit,p::Array{T},W::Matrix{Float64};feFlag=-1) where T


    params = parDict(d,p,no2Der=false)

    #Reset Derivatives
    hess[:].=0.0
    grad[:].=0.0
    params.dSdθ_j[:] .= 0.0
    params.dRdθ_j[:] .= 0.0
    params.d2Sdθ_j[:] .= 0.0
    params.d2Rdθ_j[:] .= 0.0

    ll = log_likelihood!(hess,grad,d,params,feFlag=feFlag)

    mom_grad = Matrix{Float64}(undef,length(p),length(d.data.rMoments))
    mom_hess = Array{Float64,3}(undef,length(p),length(p),length(d.data.rMoments))
    mom = calc_risk_moments!(mom_hess,mom_grad,d,params,feFlag=feFlag)

    # hess_test = Array{Float64,2}(undef,length(p),length(p))
    # hess_test[:].=0.0
    obj = calc_GMM_Obj(mom,W)

    calc_GMM_Grad!(grad,mom,mom_grad,W)
    calc_GMM_Hess!(hess,mom,mom_grad,mom_hess,W)
    # calc_GMM_Hess!(hess_test,mom,mom_grad,mom_hess,W)
    return ll + obj
end


function calc_penalty(d::InsuranceLogit,p0::Array{T},W::Matrix{Float64};feFlag::Int64=-1) where T

    par0 = parDict(d,p0,no2Der=true)
    # grad = Vector{T}(undef,length(p0))
    # ll = log_likelihood(d,par0,feFlag=feFlag)
    mom = calc_risk_moments(d,p0)
    # moments = grad
    obj = calc_GMM_Obj(mom,W)
    return obj
end
