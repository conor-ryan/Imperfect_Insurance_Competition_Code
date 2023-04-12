function unPackChars(app::ChoiceData,d::InsuranceLogit)
    ind = person(app)[1]
    r_ind = Int(rIndS(app)[1])
    r_ind_metal = Int.(rInd(app))[:]
    S_ij = transpose(choice(app))
    wgt = transpose(weight(app))
    urate = transpose(unins(app))
    risk_age = ageHCC(app)[:]
    idxitr = d.data._personDict[ind]

    X_t = prodchars(app)
    X_0_t = prodcharsσ(app)
    Z = demoRaw(app)[:,1]
    #F_t = fixedEffects(app)
    F_t = fixedEffects(app,idxitr)

    return ind, r_ind, r_ind_metal, S_ij, wgt, urate, idxitr, X_t, X_0_t, Z, F_t,risk_age
end

function unPackChars_ll(app::ChoiceData,d::InsuranceLogit)
    ind = person(app)[1]
    S_ij = transpose(choice(app))
    wgt = transpose(weight(app))
    urate = transpose(unins(app))
    idxitr = d.data._personDict[ind]

    return ind, S_ij, wgt, urate, idxitr
end

function unPackParChars(p::parDict{T},idxitr::UnitRange{Int}) where T
    μ_ij = p.μ_ij[:,idxitr]
    μ_ij_nr = Matrix{T}(undef,1,size(μ_ij,2))
    μ_ij_nr[1,:] = p.μ_ij_nonRisk[idxitr]
    δ    = p.δ[idxitr]

    #Get Market Shares
    s_hat = p.s_hat[idxitr]

    return μ_ij,μ_ij_nr,δ,s_hat
end

function sumShares!(s_hat::Vector{T},ind::Float64) where T
    # Fix possible computational error
    for k in eachindex(s_hat)
        if abs(s_hat[k])<=1e-300
            s_hat[k]=1e-15
            #println("Hit Share Constraint for person $ind, product $k")
        end
    end
    s_insured = sum(s_hat)
    if s_insured>=(1-1e-300)
        s_insured= 1 - 1e-15
        #println("Hit insured constraint for person $ind")
    end

    return s_insured
end

function relPar(app::ChoiceData,d::InsuranceLogit,F_t::SubArray,ind::Float64,feFlag::Int64)
    Q = d.parLength[:All]
    F_length =size(F_t,1)
    σ_length = d.parLength[:σ]
    Q_0 = Q - F_length
    Q_no_σ = Q_0 - d.parLength[:σ]
    ## Relevant Parameters for this observation
    if feFlag==0
        pars_relevant = (Q_no_σ+1):Q_0
        # pars_relevant = vcat((d.parLength[:γ] + 2):(d.parLength[:γ]+d.parLength[:β]),(Q_no_σ+1):Q_0)
    elseif feFlag==1
        # pars_relevant = vcat(Q_0 .+ app._rel_fe_Dict[ind])
        pars_relevant = vcat(1:Q_no_σ,Q_0 .+ app._rel_fe_Dict[ind])
    else
        pars_relevant = vcat(1:Q_no_σ,Q_no_σ.+app._rel_rc_Dict[ind],Q_0 .+ app._rel_fe_Dict[ind])
    end
    return pars_relevant
end

function preCalcμ(μ_ij::Matrix{T},δ::Vector{T}) where T
    μ_ij_sums = 1 .+μ_ij*δ
    # μ_ij_sums_sq = (μ_ij_sums).^2
    # μ_ij_sums_cu = (μ_ij_sums).^3

    return μ_ij_sums
end

function ll_Terms(wgt::Array{Float64,N},S_ij::Array{Float64,N},urate::Array{Float64,N},
                    s_hat::Vector{T},s_insured::T) where {N,T}
    K = length(S_ij)
    ll_obs = 0.0
    gll_t1 = Vector{T}(undef,K)
    gll_t2 = Vector{T}(undef,K)
    gll_t3 = Vector{T}(undef,K)
    gll_t4 = Vector{T}(undef,K)
    gll_t5 = Vector{T}(undef,K)
    gll_t6 = Vector{T}(undef,K)
    for k in 1:K
        #Gradient Terms
        gll_t1[k] = wgt[k]*S_ij[k]*(1/s_hat[k])
        gll_t2[k] = wgt[k]*S_ij[k]*(1/s_hat[k]^2)
        gll_t3[k] = wgt[k]*S_ij[k]*2*(1/s_hat[k]^3)
        gll_t4[k] = wgt[k]*(S_ij[k]*urate[k]*(1/(s_insured)) + (1/K)*urate[k]*(1/(1-s_insured)))
        gll_t5[k] = wgt[k]*(S_ij[k]*urate[k]*(1/(s_insured^2)) - (1/K)*urate[k]*(1/((1-s_insured)^2)))
        gll_t6[k] = wgt[k]*(S_ij[k]*urate[k]*2*(1/(s_insured^3)) + (1/K)*urate[k]*2*(1/((1-s_insured)^3)))
        # Log Likelihood
        ll_obs+=wgt[k]*(S_ij[k]*(log(s_hat[k]) -
                        urate[k]*log(s_insured)) + (1/K)*urate[k]*(log(1-s_insured)))
    end

    return gll_t1, gll_t2, gll_t3, gll_t4,gll_t5,gll_t6, ll_obs
end

function ll_calc(wgt::Array{Float64,N},S_ij::Array{Float64,N},urate::Array{Float64,N},
                    s_hat::Vector{T},s_insured::T) where {N,T}
    K = length(S_ij)
    ll_obs = 0.0
    for k in 1:K
        # Log Likelihood
        ll_obs+=wgt[k]*(S_ij[k]*(log(s_hat[k]) -
                        urate[k]*log(s_insured)) + (1/K)*urate[k]*(log(1-s_insured)))
    end

    return ll_obs
end


function ll_obs_hessian!(thD::Array{Float64,3},hess::Matrix{Float64},grad::Vector{Float64},
                            app::ChoiceData,d::InsuranceLogit,p::parDict{T};feFlag=-1) where T

        ind, r_ind, r_ind_metal, S_ij, wgt, urate, idxitr, X_t, X_0_t, Z, F_t, r_age = unPackChars(app,d)
        wgt = convert(Array{Float64,2},wgt)
        S_ij = convert(Array{Float64,2},S_ij)
        urate = convert(Array{Float64,2},urate)

        prodidx = Int.(product(app))

        draws = d.draws
        non_zero_draws = findall(d.draws[:,r_ind].>0)
        zero_draws = findall(d.draws[:,r_ind].==0)
        risk = draws[:,r_ind_metal]
        r_avg = p.r_hat[idxitr]
        anyR = anyHCC(app)[1]

        # Get Utility and derivative of Utility
        μ_ij,μ_ij_nr,δ,s_hat = unPackParChars(p,idxitr)

        s_insured = sumShares!(s_hat,ind)

        (N,K) = size(μ_ij)

        # Initialize Gradient
        #(Q,N,K) = size(dμ_ij)
        pars_relevant = relPar(app,d,F_t,ind,feFlag)

        # Pre-Calculate Squares
        μ_ij_sums = preCalcμ(μ_ij,δ)
        # μ_ij_nr_sum = 1 .+ sum(μ_ij_nr,dims=2)[:]
        μ_ij_nr_sum = 1 .+ μ_ij_nr*δ

        # Pre-Calculate Log-Likelihood Terms for Gradient
        # Also Calculate Log-Likelihood itself
        gll_t1, gll_t2, gll_t3, gll_t4,gll_t5,gll_t6, ll_obs = ll_Terms(wgt,S_ij,urate,s_hat,s_insured)

        #hess = zeros(Q,Q)
        #hess[:] = 0.0
        #grad[:] = 0.0
        X_mat = Array{Float64}(undef,N+1,K)
        Y_list = Array{Union{Float64, Array{Float64,1}, Array{Float64,2}},1}(undef,length(pars_relevant))
        #Y_mat = Array{Float64}(N,K)

        # Allocate Memory
        dS_xyz = Vector{Float64}(undef,K)
        dS_xy = Vector{Float64}(undef,K)
        # dS_yz = Vector{Float64}(K)
        # dS_xz = Vector{Float64}(K)

        dS_x = Vector{Float64}(undef,K)
        # dS_y = Vector{Float64}(K)
        # dS_z = Vector{Float64}(K)

        # dS_x_list = Matrix{Float64}(K,length(pars_relevant))
        dS_x_list = Vector{Vector{Float64}}(undef,length(pars_relevant))
        dS_x_all_list = Vector{Float64}(undef,length(pars_relevant))

        # dS_xy_list = Array{Array{Float64,1},2}(length(pars_relevant),length(pars_relevant))
        # dS_xy_list = Array{Float64,3}(K,length(pars_relevant),length(pars_relevant))
        dS_xy_list = Array{Vector{Float64},2}(undef,length(pars_relevant),length(pars_relevant))
        dS_xy_all_list = Matrix{Float64}(undef,length(pars_relevant),length(pars_relevant))

        s_n = Vector{Float64}(undef,K)

        dR_x = Vector{Float64}(undef,K)
        dR_x_list = Vector{Vector{Float64}}(undef,length(pars_relevant))
        dR_xy = Vector{Float64}(undef,K)

        #γlen = 1 + d.parLength[:γ]
        γlen = d.parLength[:γ]
        β0len = γlen + d.parLength[:β]
        βlen = β0len + d.parLength[:γ]
        σlen = βlen + d.parLength[:σ]
        FElen = σlen + d.parLength[:FE]


        for (q_i,q) in enumerate(pars_relevant)
            X = returnParameterX!(q,X_mat,
                            Z,X_0_t,X_t,draws,F_t,r_ind,
                            γlen,β0len,βlen,σlen)
            @inbounds Y_list[q_i] = copy(X)

            dS_x_all = grad_calc!(dS_x,s_n,anyR,
                        dR_x,risk,r_age,
                        μ_ij,μ_ij_nr,δ,X,
                        μ_ij_sums,μ_ij_nr_sum)

            # Save Answers
            dS_x_list[q_i] = dS_x[:]
            dS_x_all_list[q_i] = dS_x_all

            ## Calculate Gradient
            grad[q]+= combine_grad(N,gll_t1,gll_t4,dS_x,dS_x_all)

            # for (k,ij) in enumerate(idxitr)
            #     @inbounds @fastmath p.dSdθ[q,ij] = dS_x[k]/N
            #     @inbounds @fastmath p.dRdθ[q,ij] = dR_x[k]/(s_hat[k]*N) - (dS_x[k]/N)/s_hat[k] * r_avg[k]
            # end

            dR_x_list[q_i] = dR_x[:]
            for k in 1:K
                @inbounds @fastmath p.dSdθ_j[q,prodidx[k]]+= wgt[k]*dS_x[k]
                @inbounds @fastmath p.dRdθ_j[q,prodidx[k]]+= wgt[k]*(dR_x[k])
            end


            for (r_i,r) in enumerate(pars_relevant)
                if r>q
                    continue
                end
                Y_mat = Y_list[r_i]


                dS_y = dS_x_list[r_i]
                dR_y = dR_x_list[r_i]
                dS_y_all = dS_x_all_list[r_i]

                dS_xy_all = hess_calc!(dS_xy,s_n,anyR,
                            dR_xy,risk,r_age,
                            μ_ij,μ_ij_nr,δ,X,Y_mat,
                            μ_ij_sums,μ_ij_nr_sum)

                dS_xy_list[q_i,r_i] = dS_xy[:]
                dS_xy_all_list[q_i,r_i] = dS_xy_all

                hess_obs = combine_hess(N,gll_t1,gll_t2,gll_t4,gll_t5,
                            dS_xy,dS_x,dS_y,dS_xy_all,dS_x_all,dS_y_all)

                hess[q,r]+= hess_obs
                if (q!=r)
                    hess[r,q]+= hess_obs
                end

                for k in 1:K
                    @inbounds @fastmath p.d2Sdθ_j[q,r,prodidx[k]]+= wgt[k]*dS_xy[k]
                    @inbounds @fastmath p.d2Rdθ_j[q,r,prodidx[k]]+= wgt[k]*(dR_xy[k])
                end



                for (t_i,t) in enumerate(pars_relevant)
                    if t>r
                        continue
                    end
                    Z_mat = Y_list[t_i]

                    dS_z = dS_x_list[t_i]
                    dS_z_all = dS_x_all_list[t_i]

                    dS_xz = dS_xy_list[q_i,t_i]
                    dS_xz_all = dS_xy_all_list[q_i,t_i]

                    dS_yz = dS_xy_list[r_i,t_i]
                    dS_yz_all = dS_xy_all_list[r_i,t_i]


                    dS_xyz_all  = thD_calc!(dS_xyz,s_n,anyR,
                                μ_ij,μ_ij_nr,δ,X,Y_mat,Z_mat,
                                μ_ij_sums,μ_ij_nr_sum)

                    thD_obs = combine_thD(N,gll_t1,gll_t2,gll_t3,gll_t4,gll_t5,gll_t6,
                                dS_xyz,dS_xy,dS_xz,dS_yz,
                                dS_x,dS_y,dS_z,
                                dS_xyz_all,dS_xy_all,dS_xz_all,dS_yz_all,
                                dS_x_all,dS_y_all,dS_z_all)

                    thD[q,r,t]+= thD_obs
                    if (r!=t)
                        thD[q,t,r]+= thD_obs
                        thD[t,r,q]+= thD_obs
                        if (r!=q)
                            thD[r,t,q]+= thD_obs
                            thD[t,q,r]+= thD_obs
                            thD[r,q,t]+= thD_obs
                        end
                    elseif (r!=q)
                        thD[t,r,q]+= thD_obs
                        thD[r,q,t]+= thD_obs
                    end

                end

            end
        end

    return ll_obs,pars_relevant
end

function combine_thD(N::Int64,
                    gll_t1::Vector{T},gll_t2::Vector{T},
                    gll_t3::Vector{T},gll_t4::Vector{T},
                    gll_t5::Vector{T},gll_t6::Vector{T},
                    dS_xyz::Vector{T},
                    dS_xy::Vector{T},dS_xz::Vector{T},dS_yz::Vector{T},
                    dS_x::Vector{T},dS_y::Vector{T},dS_z::Vector{T},
                    dS_xyz_all::T,
                    dS_xy_all::T,dS_xz_all::T,dS_yz_all::T,
                    dS_x_all::T,dS_y_all::T,dS_z_all::T) where T
    thD_obs = 0.0
    K = length(dS_xyz)

    @inbounds @fastmath @simd for k in 1:K
        # thD_obs += gll_t1[k]*dS_xyz[k]/N -
        #                 gll_t2[k]*((dS_x[k]/N)*(dS_yz[k]/N) + (dS_y[k]/N)*(dS_xz[k]/N) + (dS_z[k]/N)*(dS_xy[k]/N)) +
        #                 gll_t3[k]*(dS_x[k]/N)*(dS_y[k]/N)*(dS_z[k]/N) -
        #                 gll_t4[k]*(dS_xyz_all/N) +
        #                 gll_t5[k]*((dS_x_all/N)*(dS_yz_all/N)+(dS_y_all/N)*(dS_xz_all/N)+(dS_z_all/N)*(dS_xy_all/N)) -
        #                 gll_t6[k]*(dS_x_all/N)*(dS_y_all/N)*(dS_z_all/N)
        thD_obs += gll_t1[k]*dS_xyz[k] -
                        gll_t2[k]*((dS_x[k])*(dS_yz[k]) + (dS_y[k])*(dS_xz[k]) + (dS_z[k])*(dS_xy[k])) +
                        gll_t3[k]*(dS_x[k])*(dS_y[k])*(dS_z[k]) -
                        gll_t4[k]*(dS_xyz_all) +
                        gll_t5[k]*((dS_x_all)*(dS_yz_all)+(dS_y_all)*(dS_xz_all)+(dS_z_all)*(dS_xy_all)) -
                        gll_t6[k]*(dS_x_all)*(dS_y_all)*(dS_z_all)
    end
    return thD_obs
end


function combine_hess(N::Int64,
                    gll_t1::Vector{T},gll_t2::Vector{T},
                    gll_t3::Vector{T},gll_t4::Vector{T},
                    dS_xy::Vector{T},dS_x::Vector{T},
                    dS_y::Vector{T},dS_xy_all::T,
                    dS_x_all::T,dS_y_all::T) where T
    hess_obs = 0.0
    K = length(dS_xy)

    @inbounds @fastmath @simd for k in 1:K
        # hess_obs += gll_t1[k]*(dS_xy[k]/N) - gll_t2[k]*(dS_x[k]/N)*(dS_y[k]/N) - (gll_t3[k]*(dS_xy_all/N) - gll_t4[k]*dS_x_all/N*dS_y_all/N)
        hess_obs += gll_t1[k]*(dS_xy[k]) - gll_t2[k]*(dS_x[k])*(dS_y[k]) - (gll_t3[k]*(dS_xy_all) - gll_t4[k]*dS_x_all*dS_y_all)
    end
    return hess_obs
end


function combine_grad(N::Int64,
                    gll_t1::Vector{T},
                    gll_t3::Vector{T},
                    dS_x::Vector{T},
                    dS_x_all::T) where T
    grad_obs = 0.0
    K = length(dS_x)

    @inbounds @fastmath @simd for k in 1:K
        grad_obs += gll_t1[k]*dS_x[k] - gll_t3[k]*dS_x_all
    end
    return grad_obs
end



function hess_calc!(dS_xy::Vector{Float64},s_n::Vector{T},
                    anyR::Float64,
                    dR::Vector{T},risk::Matrix{Float64},
                    risk_age::Vector{Float64},
                    μ_ij::Matrix{T},μ_ij_nr::Matrix{T},δ::Vector{T},
                    X_mat::S,Y::R,
                    μ_ij_sums::Vector{Float64},μ_ij_nr_sum::Vector{T}) where {S,R,T}
    dS_xy[:] .= 0.0
    dR[:] .= 0.0

    dS_xy_all = 0.0


    (N,K) = size(μ_ij)

    ### 0 Draw Calculation ###
    risk_nr = zeros(1,K)
    Γ_x, Γ_y, Γ_xy = calc_derSums_xy!(0,s_n,X_mat,Y,μ_ij_nr,δ,μ_ij_nr_sum[1])

    # @fastmath Γ_x = dμ_ij_x_sums/μ_ij_nr_sum[1]
    # @fastmath Γ_y = dμ_ij_y_sums/μ_ij_nr_sum[1]
    # @fastmath Γ_xy = dμ_ij_xy_sums/μ_ij_nr_sum[1]

    calc_prodTerms_xy!(0,1-anyR,dS_xy,dR,risk_nr,risk_age,
                X_mat,Y,s_n,Γ_x,Γ_y,Γ_xy)

    for n in 1:N
        μ_ij_sums_n = μ_ij_sums[n]

        Γ_x, Γ_y, Γ_xy = calc_derSums_xy!(n,s_n,X_mat,Y,μ_ij,δ,μ_ij_sums_n)

        # @fastmath Γ_x = dμ_ij_x_sums/μ_ij_sums_n
        # @fastmath Γ_y = dμ_ij_y_sums/μ_ij_sums_n
        # @fastmath Γ_xy = dμ_ij_xy_sums/μ_ij_sums_n
        # s_n = μ_ij[n,:].*δ/μ_ij_sums_n

        calc_prodTerms_xy!(n,anyR/N,dS_xy,dR,risk,risk_age,
                            X_mat,Y,s_n,
                            Γ_x,Γ_y,Γ_xy)
    end
    for k in 1:K
        @inbounds @fastmath dS_xy_all+= dS_xy[k]
    end
    return dS_xy_all
end


# function thD_calc!(dS_xyz::Vector{Float64},s_n::Vector{Float64},
#                     anyR::Float64,
#                     μ_ij::Matrix{T},μ_ij_nr::Matrix{T},δ::Vector{T},
#                     X_mat::S,Y::R,Z::Q,
#                     μ_ij_sums::Vector{Float64},μ_ij_nr_sum::Vector{T}) where {S,R,Q,T}
#     dS_xyz[:] .= 0.0
#
#     dS_xyz_all = 0.0
#
#
#     (N,K) = size(μ_ij)
#
#     ### 0 Draw Calculation ###
#     dμ_ij_x_sums,dμ_ij_y_sums,dμ_ij_z_sums,dμ_ij_xy_sums,dμ_ij_xz_sums,dμ_ij_yz_sums,dμ_ij_xyz_sums =
#         calc_derSums_xyz!(0,s_n,X_mat,Y,Z,μ_ij_nr,δ,μ_ij_nr_sum[1])
#
#     @fastmath Γ_x = dμ_ij_x_sums/μ_ij_nr_sum[1]
#     @fastmath Γ_y = dμ_ij_y_sums/μ_ij_nr_sum[1]
#     @fastmath Γ_z = dμ_ij_z_sums/μ_ij_nr_sum[1]
#     @fastmath Γ_xy = dμ_ij_xy_sums/μ_ij_nr_sum[1]
#     @fastmath Γ_xz = dμ_ij_xz_sums/μ_ij_nr_sum[1]
#     @fastmath Γ_yz = dμ_ij_yz_sums/μ_ij_nr_sum[1]
#     @fastmath Γ_xyz = dμ_ij_xyz_sums/μ_ij_nr_sum[1]
#     # s_n = s_n./μ_ij_sums_n
#
#     calc_prodTerms_xyz!(0,1-anyR,dS_xyz,
#                 X_mat,Y,Z,s_n,Γ_x,Γ_y,Γ_z,Γ_xy,Γ_xz,Γ_yz,Γ_xyz)
#
#     for n in 1:N
#         μ_ij_sums_n = μ_ij_sums[n]
#
#         dμ_ij_x_sums,dμ_ij_y_sums,dμ_ij_z_sums,dμ_ij_xy_sums,dμ_ij_xz_sums,dμ_ij_yz_sums,dμ_ij_xyz_sums =
#             calc_derSums_xyz!(n,s_n,X_mat,Y,Z,μ_ij,δ,μ_ij_sums_n)
#
#         @fastmath Γ_x = dμ_ij_x_sums/μ_ij_sums_n
#         @fastmath Γ_y = dμ_ij_y_sums/μ_ij_sums_n
#         @fastmath Γ_z = dμ_ij_z_sums/μ_ij_sums_n
#         @fastmath Γ_xy = dμ_ij_xy_sums/μ_ij_sums_n
#         @fastmath Γ_xz = dμ_ij_xz_sums/μ_ij_sums_n
#         @fastmath Γ_yz = dμ_ij_yz_sums/μ_ij_sums_n
#         @fastmath Γ_xyz = dμ_ij_xyz_sums/μ_ij_sums_n
#         # s_n = s_n./μ_ij_sums_n
#
#         calc_prodTerms_xyz!(n,anyR/N,dS_xyz,
#                     X_mat,Y,Z,s_n,Γ_x,Γ_y,Γ_z,Γ_xy,Γ_xz,Γ_yz,Γ_xyz)
#     end
#     for k in 1:K
#         @inbounds @fastmath dS_xyz_all+= dS_xyz[k]
#     end
#     return dS_xyz_all
# end


function grad_calc!(dS_x::Vector{T},
                    s_n::Vector{T},
                    anyR::Float64,
                    dR::Vector{T},risk::Matrix{Float64},
                    risk_age::Vector{Float64},
                    μ_ij::Matrix{T},μ_ij_nr::Matrix{T},δ::Vector{T},
                    X_mat::S,
                    μ_ij_sums::Vector{T},μ_ij_nr_sum::Vector{T}) where {S,T}

    dS_x[:] .= 0.0
    dS_x_all = 0.0
    dR[:] .= 0.0

    (N,K) = size(μ_ij)

    ### 0 Draw Calculation ###
    risk_nr = zeros(1,K)
    Γ_x = calc_derSums_x!(0,s_n,X_mat,μ_ij_nr,δ,μ_ij_nr_sum[1])

    # s_n = s_n./μ_ij_sums_n

    calc_prodTerms_x!(0,1-anyR,dS_x,dR,risk_nr,risk_age,
                X_mat,s_n,Γ_x)

    for n in 1:N
        μ_ij_sums_n = μ_ij_sums[n]

        Γ_x = calc_derSums_x!(n,s_n,X_mat,μ_ij,δ,μ_ij_sums_n)


        # s_n = s_n./μ_ij_sums_n

        calc_prodTerms_x!(n,anyR/N,dS_x,dR,risk,risk_age,
                    X_mat,s_n,Γ_x)
    end
    for k in 1:K
        @inbounds @fastmath dS_x_all+= dS_x[k]
    end
    return dS_x_all
end

function getIndex(A::Float64,n::Int64,k::Int64)
    return A
end

function getIndex(A::Array{Float64,1},n::Int64,k::Int64)
    return A[k]
end

function getIndex(A::Array{Float64,2},n::Int64,k::Int64)
    return A[n,k]
end

function returnParameter!(q::Int64,X_mat::Matrix{Float64},
                        Z::Vector{Float64},X_0_t::Matrix{Float64},
                        X_t::Matrix{Float64},draws::Matrix{Float64},
                        F_t::SubArray{Float64,2},r_ind::Int64,
                        γlen::Int64,β0len::Int64,βlen::Int64,σlen::Int64)
    (N,K) = size(X_mat)
    if q<0
        X_mat[:] .= 1.0
    elseif q<=γlen
        X_mat[:] .= Z[q]
    elseif q<=β0len
        for n in 1:N
            @inbounds X_mat[n,:] = X_t[q-γlen,:]
        end
    elseif q<=βlen
        # Characteristic Interactions
        for n in 1:N
            @inbounds X_mat[n,:] = X_t[1,:].*Z[q-β0len]
        end
    elseif q<=σlen
        #Quality Random Effect
        X_mat[1,:].=0.0
        for n in 2:N,k in 1:K
            @inbounds X_mat[n,k] = draws[n-1,r_ind]*X_0_t[q-(βlen),k]
        end
    else
        #Fixed Effect
        for n in 1:N
            @inbounds X_mat[n,:] = F_t[q-σlen,:]
        end
    end
    return Nothing
end

function returnParameterX!(q::Int64,X_mat::Matrix{Float64},
                        Z::Vector{Float64},X_0_t::Matrix{Float64},
                        X_t::Matrix{Float64},draws::Matrix{Float64},
                        F_t::SubArray{Float64,2},r_ind::Int64,
                        γlen::Int64,β0len::Int64,βlen::Int64,σlen::Int64)
    (N,K) = size(X_mat)
    if q<0
        X = 1.0
    elseif q<=γlen
        X = Z[q]
    elseif q<=β0len
        X = X_t[q-γlen,:]
    elseif q<=βlen
        # Characteristic Interactions
        X = X_t[1,:].*Z[q-β0len]
    elseif q<=σlen
        #Quality Random Effect
        X_mat[1,:].=0.0
        for n in 2:N,k in 1:K
            @inbounds X_mat[n,k] = draws[n-1,r_ind]*X_0_t[q-(βlen),k]
        end
        X = similar(X_mat)
        X[:,:] = X_mat[:,:]
    else
        #Fixed Effect
        X = F_t[q-σlen,:]
    end
    return X
end

function ll_obs_hessian!(hess::Union{Matrix{Float64},SharedMatrix{Float64}},
                        grad::Union{Vector{Float64},SharedVector{Float64}},
                            app::ChoiceData,d::InsuranceLogit,p::parDict{T};feFlag=-1) where T

        ind, r_ind, r_ind_metal, S_ij, wgt, urate, idxitr, X_t, X_0_t, Z, F_t, r_age = unPackChars(app,d)
        wgt = convert(Array{Float64,2},wgt)
        S_ij = convert(Array{Float64,2},S_ij)
        urate = convert(Array{Float64,2},urate)

        prodidx = Int.(product(app))

        draws = d.draws
        non_zero_draws = findall(d.draws[:,r_ind].>0)
        zero_draws = findall(d.draws[:,r_ind].==0)
        risk = draws[:,r_ind_metal]
        r_avg = p.r_hat[idxitr]
        anyR = anyHCC(app)[1]

        # Get Utility and derivative of Utility
        μ_ij,μ_ij_nr,δ,s_hat = unPackParChars(p,idxitr)

        s_insured = sumShares!(s_hat,ind)

        (N,K) = size(μ_ij)

        # Initialize Gradient
        #(Q,N,K) = size(dμ_ij)
        pars_relevant = relPar(app,d,F_t,ind,feFlag)

        # Pre-Calculate Squares
        μ_ij_sums = preCalcμ(μ_ij,δ)
        μ_ij_nr_sum = 1 .+ μ_ij_nr*δ

        # Pre-Calculate Log-Likelihood Terms for Gradient
        # Also Calculate Log-Likelihood itself
        gll_t1, gll_t2, gll_t3, gll_t4,gll_t5,gll_t6, ll_obs = ll_Terms(wgt,S_ij,urate,s_hat,s_insured)

        #hess = zeros(Q,Q)
        #hess[:] = 0.0
        #grad[:] = 0.0
        X_mat = Array{Float64}(undef,N+1,K)
        Y_list = Array{Union{Float64, Array{Float64,1}, Array{Float64,2}},1}(undef,length(pars_relevant))
        #Y_mat = Array{Float64}(N,K)

        # Allocate Memory
        dS_xyz = Vector{Float64}(undef,K)
        dS_xy = Vector{Float64}(undef,K)
        # dS_yz = Vector{Float64}(K)
        # dS_xz = Vector{Float64}(K)

        dS_x = Vector{Float64}(undef,K)
        # dS_y = Vector{Float64}(K)
        # dS_z = Vector{Float64}(K)

        # dS_x_list = Matrix{Float64}(K,length(pars_relevant))
        dS_x_list = Vector{Vector{Float64}}(undef,length(pars_relevant))
        dS_x_all_list = Vector{Float64}(undef,length(pars_relevant))

        # dS_xy_list = Array{Array{Float64,1},2}(length(pars_relevant),length(pars_relevant))
        # dS_xy_list = Array{Float64,3}(K,length(pars_relevant),length(pars_relevant))
        dS_xy_list = Array{Vector{Float64},2}(undef,length(pars_relevant),length(pars_relevant))
        dS_xy_all_list = Matrix{Float64}(undef,length(pars_relevant),length(pars_relevant))

        s_n = Vector{Float64}(undef,K)

        dR_x = Vector{Float64}(undef,K)
        dR_x_list = Vector{Vector{Float64}}(undef,length(pars_relevant))
        dR_xy = Vector{Float64}(undef,K)

        #γlen = 1 + d.parLength[:γ]
        γlen = d.parLength[:γ]
        β0len = γlen + d.parLength[:β]
        βlen = β0len + d.parLength[:γ]
        σlen = βlen + d.parLength[:σ]
        FElen = σlen + d.parLength[:FE]


        for (q_i,q) in enumerate(pars_relevant)
            X = returnParameterX!(q,X_mat,
                            Z,X_0_t,X_t,draws,F_t,r_ind,
                            γlen,β0len,βlen,σlen)
            @inbounds Y_list[q_i] = copy(X)

            dS_x_all = grad_calc!(dS_x,s_n,anyR,
                        dR_x,risk,r_age,
                        μ_ij,μ_ij_nr,δ,X,
                        μ_ij_sums,μ_ij_nr_sum)

            # Save Answers
            dS_x_list[q_i] = dS_x[:]
            dS_x_all_list[q_i] = dS_x_all

            ## Calculate Gradient
            grad[q]+= combine_grad(N,gll_t1,gll_t4,dS_x,dS_x_all)

            # for (k,ij) in enumerate(idxitr)
            #     @inbounds @fastmath p.dSdθ[q,ij] = dS_x[k]/N
            #     @inbounds @fastmath p.dRdθ[q,ij] = dR_x[k]/(s_hat[k]*N) - (dS_x[k]/N)/s_hat[k] * r_avg[k]
            # end

            dR_x_list[q_i] = dR_x[:]
            for k in 1:K
                @inbounds @fastmath p.dSdθ_j[q,prodidx[k]]+= wgt[k]*dS_x[k]
                @inbounds @fastmath p.dRdθ_j[q,prodidx[k]]+= wgt[k]*(dR_x[k])
            end


            for (r_i,r) in enumerate(pars_relevant)
                if r>q
                    continue
                end
                Y_mat = Y_list[r_i]


                dS_y = dS_x_list[r_i]
                dR_y = dR_x_list[r_i]
                dS_y_all = dS_x_all_list[r_i]

                dS_xy_all = hess_calc!(dS_xy,s_n,anyR,
                            dR_xy,risk,r_age,
                            μ_ij,μ_ij_nr,δ,X,Y_mat,
                            μ_ij_sums,μ_ij_nr_sum)

                dS_xy_list[q_i,r_i] = dS_xy[:]
                dS_xy_all_list[q_i,r_i] = dS_xy_all

                hess_obs = combine_hess(N,gll_t1,gll_t2,gll_t4,gll_t5,
                            dS_xy,dS_x,dS_y,dS_xy_all,dS_x_all,dS_y_all)

                hess[q,r]+= hess_obs
                if (q!=r)
                    hess[r,q]+= hess_obs
                end

                for k in 1:K
                    @inbounds @fastmath p.d2Sdθ_j[q,r,prodidx[k]]+= wgt[k]*dS_xy[k]
                    @inbounds @fastmath p.d2Rdθ_j[q,r,prodidx[k]]+= wgt[k]*(dR_xy[k])
                end
            end
        end

    return ll_obs,pars_relevant
end



function ll_obs_gradient!(grad::Union{Vector{S},SharedVector{S}},
                            app::ChoiceData,d::InsuranceLogit,p::parDict{T};feFlag=-1) where {S,T}

        ind, r_ind, r_ind_metal, S_ij, wgt, urate, idxitr, X_t, X_0_t, Z, F_t, r_age = unPackChars(app,d)
        wgt = convert(Array{Float64,2},wgt)
        S_ij = convert(Array{Float64,2},S_ij)
        urate = convert(Array{Float64,2},urate)

        prodidx = Int.(product(app))

        draws = d.draws
        non_zero_draws = findall(d.draws[:,r_ind].>0)
        zero_draws = findall(d.draws[:,r_ind].==0)
        risk = draws[:,r_ind_metal]
        anyR = anyHCC(app)[1]

        # Get Utility and derivative of Utility
        μ_ij,μ_ij_nr,δ,s_hat = unPackParChars(p,idxitr)

        s_insured = sumShares!(s_hat,ind)

        (N,K) = size(μ_ij)

        # Initialize Gradient
        #(Q,N,K) = size(dμ_ij)
        pars_relevant = relPar(app,d,F_t,ind,feFlag)

        # Pre-Calculate Squares
        μ_ij_sums = preCalcμ(μ_ij,δ)
        μ_ij_nr_sum = 1 .+ μ_ij_nr*δ
        # Pre-Calculate Log-Likelihood Terms for Gradient
        # Also Calculate Log-Likelihood itself
        gll_t1, gll_t2, gll_t3, gll_t4,gll_t5,gll_t6, ll_obs = ll_Terms(wgt,S_ij,urate,s_hat,s_insured)

        #hess = zeros(Q,Q)
        #hess[:] = 0.0
        #grad[:] = 0.0
        X_mat = Array{Float64}(undef,N+1,K)
        #Y_mat = Array{Float64}(N,K)

        # Allocate Memory

        dS_x = Vector{S}(undef,K)
        # dS_y = Vector{Float64}(K)
        # dS_z = Vector{Float64}(K)

        s_n = Vector{S}(undef,K)

        dR_x = Vector{S}(undef,K)

        #γlen = 1 + d.parLength[:γ]
        γlen = d.parLength[:γ]
        β0len = γlen + d.parLength[:β]
        βlen = β0len + d.parLength[:γ]
        σlen = βlen + d.parLength[:σ]
        FElen = σlen + d.parLength[:FE]


        for (q_i,q) in enumerate(pars_relevant)
            # if q<0
            #     X = 1.0
            # elseif q<=γlen
            #     X = Z[q]
            # elseif q<=β0len
            #     X = X_t[q-γlen,:]
            # elseif q<=βlen
            #     # Characteristic Interactions
            #     X = X_t[1,:].*Z[q-β0len]
            # elseif q<=σlen
            #     #Quality Random Effect
            #     X_mat[1,:].=0.0
            #     for n in 1:N,k in 1:K
            #         @inbounds X_mat[n+1,k] = draws[n,r_ind]*X_0_t[q-(βlen),k]
            #     end
            #     X = X_mat
            # else
            #     #Fixed Effect
            #     X = F_t[q-σlen,:]
            # end
            X = returnParameterX!(q,X_mat,
                            Z,X_0_t,X_t,draws,F_t,r_ind,
                            γlen,β0len,βlen,σlen)

            dS_x_all = grad_calc!(dS_x,s_n,anyR,
                        dR_x,risk,r_age,
                        μ_ij,μ_ij_nr,δ,X,
                        μ_ij_sums,μ_ij_nr_sum)

            ## Calculate Gradient
            grad[q]+= combine_grad(N,gll_t1,gll_t4,dS_x,dS_x_all)

            for k in 1:K
                @inbounds @fastmath p.dSdθ_j[q,prodidx[k]]+= wgt[k]*dS_x[k]
                @inbounds @fastmath p.dRdθ_j[q,prodidx[k]]+= wgt[k]*(dR_x[k])
            end

        end

    return ll_obs,pars_relevant
end
