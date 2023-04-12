using StatsBase
using FiniteDiff

function aVar(c::MC_Data,d::InsuranceLogit,p::Array{Float64,1},p_est::parDict{Float64})
    println("Check 1")
    par = parMC(p,p_est,d,c) # Fix p0
    individual_costs(d,par)

    Pop = [0.0]
    Pop =calc_pop(d.data)

    N = length(unique(person(d.data)))
    sqrt_n = sqrt(N)
    wgts = weight(d.data)

    ### Unique Product IDs
    (N,M) = size(d.data.data)
    productIDs = Vector{Int64}(undef,M)
    for j in d.prods
        idx_prod = d.data._productDict[j]
        productIDs[idx_prod] .= j
    end
    num_prods = maximum(d.prods)

    # Compile the dropped Silver indices
    missing_index = Vector{Int}(undef,0)
    for j in 1:num_prods
        if length(findall(d.prods.==j))==0
            missing_index = vcat(missing_index,[j])
        end
    end
    println("Check 2")
    #### Newey McFadden  1994
    # ## Derivative of Cost Moments wrt Demand Parameters
    # G_γ = stage1_gradient(p_dem_vec,p,d,c)
    # println("Computed")
    # ## Derivative of Demand Moments wrt Demand Parameters
    # grad = Vector{Float64}(undef,length(p_dem_vec))
    # hess = Matrix{Float64}(undef,length(p_dem_vec),length(p_dem_vec))
    # ll = log_likelihood!(hess,grad,d,p_est)
    #
    # mom_grad = Matrix{Float64}(undef,length(p_dem_vec),length(d.data.tMoments))
    # mom = calc_risk_moments!(mom_grad,d,par)

    # M = hcat(mom_grad,hess)

    #### Covariance of Cost and Demand moments ####
    grad_obs = Vector{Float64}(undef,d.parLength[:All])
    risk_moments = Vector{Float64}(undef,num_prods*2)

    mean_cost_moments = Vector{Float64}(undef,num_prods*4+length(c.ageMoments)*2+length(c.agenoMoments)*2+4)
    mean_cost_moments[:] .= 0.0
    cost_mom_length = length(mean_cost_moments)

    dem_mom_length = length(risk_moments) + d.parLength[:All]
    # dem_mom_length = 0
    mean_dem_moments = Vector{Float64}(undef,dem_mom_length)
    mean_dem_moments[:] .= 0.0

    m_n = Vector{Float64}(undef,cost_mom_length)
    g_n = Vector{Float64}(undef,dem_mom_length)

    ## Estimate of population mean...
    itr = 0
    for app in eachperson(d.data)
        if itr%50==0
            println(person(app)[1])
        end
        itr += 1
        grad_obs[:] .= 0.0
        m_n[:] .= 0.0
        risk_moments[:] .= 0.0

        #### Cost Moments
        idx_prod, wgt_obs = cost_obs_moments!(m_n,productIDs,app,d,c,par)
        mean_cost_moments[:] += m_n[:]


        # #### Demand Moments (Ignore until I get these right)
        # ll_obs,pars_relevant = ll_obs_gradient!(grad_obs,app,d,par.pars)
        idx_prod = risk_obs_moments!(risk_moments,productIDs,app,d,par.pars)
        
        mean_dem_moments[1:length(risk_moments)] += risk_moments[:]
        mean_dem_moments[(length(risk_moments)+1):dem_mom_length] += grad_obs[:]

    end
    mean_cost_moments = mean_cost_moments./Pop
    # mean_dem_moments = mean_dem_moments./Pop
    println("Check 3")
    ## Estimate of variance...
    # m_n = Vector{Float64}(undef,mom_length)
    w_cov_sumsq = [0.0]
    breakflag = false
    # idx_all = 1:length(mean_moments)

    sum_sq_wgts = calc_pop_sq(d.data)
    # Σ_hold = sum_sq_wgts.*mean_moments*mean_moments'
    # Σ = sum_sq_wgts.*mean_moments*mean_moments'
    # Σ_hold = (mean_moments*mean_moments')
    # Σ = (mean_moments*mean_moments')
    Σ = zeros(dem_mom_length+cost_mom_length,dem_mom_length+cost_mom_length)
    Σ_hold = zeros(dem_mom_length+cost_mom_length,dem_mom_length+cost_mom_length)

    for app in eachperson(d.data)
        m_n[:].= 0.0
        g_n[:].= 0.0
        grad_obs[:].= 0.0
        risk_moments[:].=0.0
        w_i = weight(app)[1]
        w_cov = w_i/Pop
        w_cov_sumsq[:] += [w_cov^2]

        ### Cost Moments
        idx_prod, wgt_obs = cost_obs_moments!(m_n,productIDs,app,d,c,par)
        m_n[:] = (m_n[:]./w_i - mean_cost_moments[:])

        idx_nonEmpty_cost = vcat(idx_prod,num_prods .+idx_prod,num_prods*2 .+idx_prod,num_prods*3 .+idx_prod,(num_prods*4+1):cost_mom_length)

        ### Demand Moments
        # ll_obs,pars_relevant = ll_obs_gradient!(grad_obs,app,d,par.pars)
        idx_prod = risk_obs_moments!(risk_moments,productIDs,app,d,par.pars)
        
        g_n[1:length(risk_moments)] = risk_moments[:]
        g_n[(length(risk_moments)+1):dem_mom_length] = grad_obs[:]
        
        g_n[:] = (g_n[:]./w_i - mean_dem_moments[:])
        
        # idx_nonEmpty_dem = vcat(idx_prod,num_prods .+idx_prod,(num_prods*2) .+ pars_relevant)
        g_tilde = vcat(g_n,m_n)
        
        # idx_nonEmpty = vcat(idx_nonEmpty_dem,length(g_n).+idx_nonEmpty_cost)

        # add_Σ(Σ,m_n,idx_nonEmpty,w_cov,Σ_hold)
        # add_Σ(Σ,g_tilde,idx_nonEmpty,w_cov,Σ_hold)
    end
    # Σ = Σ./8300
    println("Check 4")
    # nonzero_index = findall(sum(abs.(Σ),dims=2)[1:dem_mom_length].!=0.0)
    # nonzero_length = sum(mean_dem_moments.!=0.0)
    nonmissing_prods = length(d.prods)
    # Σ = Σ./(1-w_cov_sumsq[1])
    # Σ = Σ./N
    # Σ = Σ.*Pop
    # println(Pop)
    Γ_m = Δavar(c,d,mean_cost_moments)
    Γ_g = zeros(d.parLength[:All] + length(d.data.rMoments),nonmissing_prods*2 + d.parLength[:All])
    (Q,R) = size(Γ_g)
    (N,M) = size(Γ_m)
    Γ_g_11 = risk_Δavar(mean_dem_moments[1:(num_prods*2)],d)
    Γ_g[1:length(d.data.tMoments),1:(nonmissing_prods*2)] = Γ_g_11[:,vcat(d.prods,num_prods.+d.prods)]
    Γ_g[(length(d.data.tMoments)+1):Q,(nonmissing_prods*2 + 1):R] = Matrix{Float64}(I,d.parLength[:All],d.parLength[:All])
    println("Check 5")
    Γ = zeros(Q+N,M+R)
    Γ[1:Q,1:R] = Γ_g[:,:]
    Γ[(Q+1):(Q+N),(R+1):(R+M)] = Γ_m

    Σ_m = Σ[(dem_mom_length+1):(dem_mom_length+cost_mom_length),(dem_mom_length+1):(dem_mom_length+cost_mom_length)]
    S_m = Γ_m*Σ_m*Γ_m'
    # S_m = Γ_m*Σ*Γ_m'

    # Σ = Σ[vcat(nonzero_index,(dem_mom_length+1):(dem_mom_length+cost_mom_length)),vcat(nonzero_index,(dem_mom_length+1):(dem_mom_length+cost_mom_length))]
    # # Γ = Γ[:,zero_index]
    #
    S_est = (Γ*Σ*Γ')

    # S_est = S_est.*(sqrt_n^2)

    return S_est, Σ, Γ, S_m
end


function cost_obs_moments!(mom_obs::Vector{Float64},productIDs::Vector{Int64},
                    app::ChoiceData,d::InsuranceLogit,c::MC_Data,p::parMC{Float64}) where T
    mom_obs[:] .= 0.0
    wgts = weight(app)[1,:]
    ind = person(app)[1]
    num_prods = maximum(d.prods)


    idxitr = app._personDict[ind]
    ind_itr = 1:length(idxitr)
    per_prods = productIDs[idxitr]
    actuarial_values = c.data[2,idxitr]

    age = c.data[1,idxitr][1]
    age_ind = Int.(max(floor((age-2.0)/.5),0)) + 1

    costs = p.C[idxitr]
    costs_cap = p.C_cap[idxitr]
    costs_pool = p.C_pool[idxitr]
    costs_risk = p.C_HCC[idxitr]
    costs_nonrisk = p.C_nonrisk[idxitr]


    s_hat = p.pars.s_hat[idxitr]
    s_hat_risk = p.s_hat_risk[idxitr]
    s_hat_nonrisk = p.s_hat_nonrisk[idxitr]

    ins_share = sum(s_hat)
    ins_share_risk = sum(s_hat_risk)
    ins_share_nonrisk = sum(s_hat_nonrisk)

    ins_cost  = sum(s_hat.*costs)
    ins_cost_risk  = sum(s_hat_risk.*costs_risk)
    ins_cost_nonrisk  = sum(s_hat_nonrisk.*costs_nonrisk)

    anyHCC = c.anyHCC[idxitr]

    M1 = num_prods*4
    M2 = M1 + length(c.ageMoments)*2
    M3 = M2 + length(c.agenoMoments)*2

    for (i,k,j) in zip(ind_itr,idxitr,per_prods)
        #S_hat
        mom_obs[j] = s_hat[i]*wgts[i]
        #C_hat
        mom_obs[num_prods + j] = costs_cap[i]*s_hat[i]*wgts[i]
        #S_Ins
        mom_obs[num_prods*2 + j] = ins_share*wgts[i]
        #C_pool
        mom_obs[num_prods*3 + j] = costs_pool[i]*ins_share*wgts[i]

    end

    ## Age Moments
    #Insurance Rate
    mom_obs[M1 + age_ind] = ins_share*wgts[1]
    #Weighted Cose
    mom_obs[M1 + length(c.ageMoments) + age_ind] = ins_cost*wgts[1]

    ## Age without HCC Moments
    #Insurance Rate
    mom_obs[M2 + age_ind] = ins_share_nonrisk*wgts[1]*(1-anyHCC[1])
    #Weighted Cose
    mom_obs[M2 + length(c.agenoMoments) + age_ind] = ins_cost_nonrisk*wgts[1]*(1-anyHCC[1])

    ## Risk Moments
    #Insurance by Risk
    mom_obs[M3+1] = ins_share_nonrisk*wgts[1]*(1-anyHCC[1])
    mom_obs[M3+2] = ins_share_risk*wgts[1]*(anyHCC[1])
    #Cost by Risk
    mom_obs[M3+3] = ins_cost_nonrisk*wgts[1]*(1-anyHCC[1])
    mom_obs[M3+4] = ins_cost_risk*wgts[1]*(anyHCC[1])

    return per_prods, wgts[1]
end

function add_Σ(Σ::Matrix{Float64},g_n::Vector{Float64},idx::Vector{Int64},weight::Float64,Σ_hold::Matrix{Float64})
    for i in idx, j in idx
        @fastmath @inbounds Σ[i,j]+=weight*(g_n[i]*g_n[j] - Σ_hold[i,j])
    end
    return nothing
end

function add_Σ(Σ::Matrix{Float64},g_n::Vector{Float64},idx::Vector{Int64})
    for i in idx, j in idx
        @fastmath @inbounds Σ[i,j]+=g_n[i]*g_n[j]
    end
    return nothing
end


function moments_Avar(c::MC_Data,d::InsuranceLogit,cost_moments::Vector{T}) where T

    num_prods = maximum(d.prods)
    f_moments_p1= Vector{T}(undef,length(c.firmMoments))
    f_moments_p2= Vector{T}(undef,length(c.firmMoments))

    f_num = maximum(keys(c._metalMomentDict))
    m_moments_p1= Matrix{T}(undef,length(c.metalMoments),f_num)
    m_moments_p2= Matrix{T}(undef,length(c.metalMoments),f_num)
    m_moments_p3= Vector{T}(undef,length(c.metalMoments))
    m_moments_p1[:] .= 0.0
    m_moments_p2[:] .= 0.0
    m_moments_p3[:] .= 0.0

    a_moments_p1= Vector{T}(undef,length(c.ageMoments))
    a_moments_p2= Vector{T}(undef,length(c.ageMoments))

    n_moments_p1= Vector{T}(undef,length(c.agenoMoments))
    n_moments_p2= Vector{T}(undef,length(c.agenoMoments))

    r_moments_p1= Vector{T}(undef,2)
    r_moments_p2= Vector{T}(undef,2)

    f_moments_p1[:] .= 0.0
    f_moments_p2[:] .= 0.0
    m_moments_p1[:] .= 0.0
    m_moments_p2[:] .= 0.0
    a_moments_p1[:] .= 0.0
    a_moments_p2[:] .= 0.0
    n_moments_p1[:] .= 0.0
    n_moments_p2[:] .= 0.0
    r_moments_p1[:] .= 0.0
    r_moments_p2[:] .= 0.0


    fmom = Vector{T}(undef,length(c.firmMoments))
    mmom = Vector{T}(undef,length(c.metalMoments)-1)
    amom = Vector{T}(undef,length(c.ageMoments)-1)
    nmom = Vector{T}(undef,length(c.agenoMoments)-1)

    M1 = num_prods*4
    M2 = M1 + length(c.ageMoments)*2
    M3 = M2 +length(c.agenoMoments)*2

    ## Product and Firm Moments
    for (m,m_idx) in c._firmMomentProdDict
        f_moments_p1[m] = sum(cost_moments[m_idx])
        f_moments_p2[m] = sum(cost_moments[num_prods .+ m_idx])
    end
    for (f,sub_dict) in c._metalMomentProdDict
        for m in 1:length(c.metalMoments)
            m_idx = sub_dict[m]
            if (m==1)&(length(sub_dict[m])==0)
                break
            elseif length(sub_dict[m])>0
                m_moments_p1[m,f] = sum(cost_moments[m_idx])
                m_moments_p2[m,f] = sum(cost_moments[num_prods .+ m_idx])
            end
        end
    end

    # for (m,m_idx) in c._metalMomentProdDict
    #     m_moments_p1[m] = sum(cost_moments[m_idx])
    #     m_moments_p2[m] = sum(cost_moments[num_prods .+ m_idx])
    # end
    ## Age Moments
    a_moments_p1[:] = cost_moments[M1 .+ (1:length(c.ageMoments))]
    a_moments_p2[:] = cost_moments[(M1 + length(c.ageMoments)) .+ (1:length(c.ageMoments))]

    ## Age without Risk Moments
    n_moments_p1[:] = cost_moments[M2 .+ (1:length(c.agenoMoments))]
    n_moments_p2[:] = cost_moments[(M2 + length(c.agenoMoments)) .+ (1:length(c.agenoMoments))]

    ## Risk Moments
    r_moments_p1[:] = cost_moments[M3 .+ (1:2)]
    r_moments_p2[:] = cost_moments[M3 .+ (3:4)]

    ## Total Firm Moments
    for (m,m_idx) in c._firmMomentProdDict
        fmom[m] = log(f_moments_p2[m]/f_moments_p1[m]) - c.firmMoments[m]
        # pmom[m] = f_moments_p2[m]/f_moments_p1[m]
    end
    ## Total Metal Moments
    mmom[:] .= 0.0
    for m in 2:size(m_moments_p1,1)
        for f in 1:size(m_moments_p1,2)
            refval = m_moments_p2[1,f]/m_moments_p1[1,f]
            c_avg = m_moments_p2[m,f]/m_moments_p1[m,f]
            if (refval!=0) & (m_moments_p1[m,f]!=0)
                mmom[m-1] += (c_avg/refval)*sum(m_moments_p1[:,f]) #- c.metalMoments[m]
                m_moments_p3[m] += sum(m_moments_p1[:,f])
            end
        end
        mmom[m-1] = mmom[m-1]/m_moments_p3[m] - c.metalMoments[m]
    end

    ## Total Age Moments
    refval = a_moments_p2[1]/a_moments_p1[1]
    for m in 2:length(a_moments_p1)
        c_avg = a_moments_p2[m]/a_moments_p1[m]
        amom[m-1] = c_avg /refval - c.ageMoments[m]
    end
    ## Total Age without HCC Moments
    refval = n_moments_p2[1]/n_moments_p1[1]
    for m in 2:length(n_moments_p1)
        c_avg = n_moments_p2[m]/n_moments_p1[m]
        # println("$m: $c_avg")
        nmom[m-1] = c_avg/refval - c.agenoMoments[m]
    end
    ## Total Risk Moments
    non_avg = r_moments_p2[1]/r_moments_p1[1]
    HCC_avg = r_moments_p2[2]/r_moments_p1[2]
    rmom = HCC_avg/non_avg - c.riskMoment

    ## Total Risk Transfer Moments
    # T_total = 0
    # S_total = 0
    # for j in c._raMomentDict[1]
    #     PC = cost_moments[num_prods*3 + j]/cost_moments[num_prods*2 + j]
    #     AC = cost_moments[num_prods + j]/cost_moments[j]
    #     S_j = cost_moments[j]
    #     T_total+= S_j*(PC-AC)
    #     S_total+= S_j
    # end
    # avgTransfer = (T_total/S_total)/10
    # tmom = avgTransfer - c.raMoments[1]


    return vcat(fmom,mmom,nmom,rmom)
    # return vcat(fmom,mmom,nmom,rmom,tmom)
end

function Δavar(c::MC_Data,d::InsuranceLogit,cost_moments::Vector{Float64})
    f_obj(x) = moments_Avar(c,d,x)
    grad = Matrix{Float64}(undef,c.mom_length,length(cost_moments))
    ForwardDiff.jacobian!(grad, f_obj, cost_moments)

    return grad
end


#
# function Δavar(c::MC_Data,d::InsuranceLogit,cost_moments::Vector{Float64})
#     M_num = c.mom_length
#     num_prods = maximum(d.prods)
#     Δ = zeros(M_num,length(cost_moments))
#     M1 = length(c.firmMoments)
#     M2 = length(c.firmMoments) + length(c.metalMoments) - 1
#
#     ##  Firm Moments
#     for (m,m_idx) in c._firmMomentProdDict
#         for j in m_idx
#             Δ[m,j] =  -1/sum(cost_moments[m_idx])
#             Δ[m,num_prods+j] = 1/sum(cost_moments[num_prods .+ m_idx])
#         end
#     end
#
#     ## Metal Moments
#     m_moments_p1 = Vector{Float64}(undef,length(c.metalMoments))
#     m_moments_p2 = Vector{Float64}(undef,length(c.metalMoments))
#
#     for (m,m_idx) in c._metalMomentProdDict
#         m_moments_p2[m] = sum(cost_moments[num_prods .+ m_idx])
#         m_moments_p1[m] = sum(cost_moments[m_idx])
#     end
#
#     refval = m_moments_p2[1]/m_moments_p1[1]
#     ref_idx = c._metalMomentProdDict[1]
#
#     for (m,m_idx) in c._metalMomentProdDict
#         if m==1
#             continue
#         end
#         for j in ref_idx
#             # ref value
#             Δ[M1 + m-1,j] = (1/m_moments_p2[1])*(m_moments_p2[m]/m_moments_p1[m])
#             Δ[M1 + m-1,num_prods .+ j] = -(1/refval)*(1/m_moments_p2[1])*(m_moments_p2[m]/m_moments_p1[m])
#         end
#         for j in m_idx
#                 # age val
#                 Δ[M1 + m-1,j] = -(1/refval)*(m_moments_p2[m]/m_moments_p1[m])*(1/m_moments_p1[m])
#                 Δ[M1 + m-1,num_prods .+ j] = (1/refval)*(1/m_moments_p1[m])
#         end
#     end
#
#     ## Age Moments
#
#     a_moments_p1 = cost_moments[num_prods*2 .+ (1:length(c.ageMoments))]
#     a_moments_p2 = cost_moments[(num_prods*2 + length(c.ageMoments)) .+ (1:length(c.ageMoments))]
#
#
#     refval = a_moments_p2[1]/a_moments_p1[1]
#     for m in 2:(length(c.ageMoments))
#         # ref value
#         Δ[M2 + m-1,num_prods*2 + 1] = (1/a_moments_p2[1])*(a_moments_p2[m]/a_moments_p1[m])
#         Δ[M2 + m-1,num_prods*2 + length(c.ageMoments) + 1] = -(1/refval)*(1/a_moments_p2[1])*(a_moments_p2[m]/a_moments_p1[m])
#         # age val
#         Δ[M2 + m-1,num_prods*2 + m] = -(1/refval)*(a_moments_p2[m]/a_moments_p1[m])*(1/a_moments_p1[m])
#         Δ[M2 + m-1,num_prods*2 + length(c.ageMoments) + m] = (1/refval)*(1/a_moments_p1[m])
#     end
#
#
#     ## Risk Moments
#     r_moments_p1 = cost_moments[(num_prods*2 + 2*length(c.ageMoments)) .+ (1:2)]
#     r_moments_p2 = cost_moments[(num_prods*2 + 2*length(c.ageMoments)) .+ (3:4)]
#
#     ## Total Risk Moments
#     non_avg = r_moments_p2[1]/r_moments_p1[1]
#     HCC_avg = r_moments_p2[2]/r_moments_p1[2]
#     # rmom = HCC_avg/non_avg  - c.riskMoment
#     Δ[M_num,num_prods*2 + length(c.ageMoments)*2 + 1] = (HCC_avg)*(1/r_moments_p2[1])
#     Δ[M_num,num_prods*2 + length(c.ageMoments)*2 + 2] = -(HCC_avg/non_avg)*(1/r_moments_p1[2])
#     Δ[M_num,num_prods*2 + length(c.ageMoments)*2 + 3] = -(HCC_avg/non_avg)*(1/r_moments_p2[1])
#     Δ[M_num,num_prods*2 + length(c.ageMoments)*2 + 4] = (1/r_moments_p1[2])*(1/non_avg)
#     return Δ
# end



function bootstrap_sample(d::InsuranceLogit,c::MC_Data)
    (K,N) = size(d.data.data)
    draw_vec = Array{Int64}(undef,N*2)
    # draw_vec_map = Array{Int64}(undef,N*2)
    x0 = 1
    for (st,perIDs) in c._stDict
        # perIDs = Int.(unique(d.data.data[1,:]))
        Per_num = length(perIDs)
        for j in 1:Per_num
            i = Int.(floor(rand()*Per_num) + 1)
            id = perIDs[i]
            idx_itr = d.data._personDict[id]
            xend = x0 + length(idx_itr)-1
            draw_vec[x0:xend] = idx_itr
            x0 = xend + 1
        end
    end
    draw_vec = draw_vec[1:(x0-1)]
    sort!(draw_vec)

    return draw_vec
end


function costMoments_bootstrap(c::MC_Data,d::InsuranceLogit,p::parMC{T}) where T
    ## Bootstrap Random Draw
    draw = bootstrap_sample(d,c)

    s_hat = p.pars.s_hat
    s_hat_nonrisk = p.s_hat_nonrisk
    s_hat_risk = p.s_hat_risk
    wgts = weight(d.data)[:]

    wgts_share = wgts.*s_hat
    any_share = wgts.*s_hat_risk.*c.anyHCC
    none_share = wgts.*s_hat_nonrisk.*(1 .- c.anyHCC)

    c_hat = p.C
    c_hat_nonHCC = p.C_nonrisk
    c_hat_HCC = p.C_HCC

    fmom = Vector{T}(undef,length(c.firmMoments))
    mmom = Vector{T}(undef,length(c.metalMoments)-1)
    amom = Vector{T}(undef,length(c.ageMoments)-1)
    nmom = Vector{T}(undef,length(c.agenoMoments)-1)


    ## Firm Moments
    for (m,m_idx) in c._firmMomentBit
        m_sample = draw[m_idx[draw]]
        c_avg = sliceMean_wgt(c_hat,wgts_share,m_sample)
        fmom[m] = log(c_avg) - c.firmMoments[m]
    end

    ## Metal Moments
    refval = sliceMean_wgt(c_hat,wgts_share,c._metalMomentDict[1])
    for (m,m_idx) in c._metalMomentBit
        if m==1
            continue
        else
            m_sample = draw[m_idx[draw]]
            c_avg = sliceMean_wgt(c_hat,wgts_share,m_sample)
            mmom[m-1] = c_avg/refval[1] - c.metalMoments[m]
        end
    end

    ## Age Moments
    refval = sliceMean_wgt(c_hat,wgts_share,c._ageMomentDict[1])
    for (m,m_idx) in c._ageMomentBit
        if m==1
            continue
        else
            # m_sample = draw[inlist_sorted(draw,m_idx)]
            m_sample = draw[m_idx[draw]]
            c_avg = sliceMean_wgt(c_hat,wgts_share,m_sample)
            amom[m-1] = c_avg/refval[1] - c.ageMoments[m]
        end
    end

    ## Age without HCC Moments
    refval = sliceMean_wgt(c_hat_nonHCC,wgts_share,c._agenoMomentDict[1])
    for (m,m_idx) in c._agenoMomentBit
        if m==1
            continue
        else
            # m_sample = draw[inlist_sorted(draw,m_idx)]
            m_sample = draw[m_idx[draw]]
            c_avg = sliceMean_wgt(c_hat_nonHCC,wgts_share,m_sample)
            nmom[m-1] = c_avg/refval[1] - c.agenoMoments[m]
        end
    end

    # all_idx = Int.(1:length(s_hat))
    m_sample = draw


    HCC_avg = sliceMean_wgt(c_hat_HCC,any_share,m_sample)
    non_avg = sliceMean_wgt(c_hat_nonHCC,none_share,m_sample)
    rmom = HCC_avg/non_avg - c.riskMoment

    return vcat(fmom,mmom,amom,nmom,rmom)
end

function var_bootstrap(c::MC_Data,d::InsuranceLogit,p::Array{T},p_est::parDict{Float64};draw_num::Int=1000) where T
    par = parMC(p,p_est,d,c) # Fix p0
    individual_costs(d,par)
    Σ, Σ_unwt, mom = var_bootstrap(c,d,par,draw_num=draw_num)
    return Σ, Σ_unwt, mom
end

function var_bootstrap(c::MC_Data,d::InsuranceLogit,p::parMC{T};draw_num::Int=1000) where T
    M_num = c.mom_length
    moment_draws = Matrix{Float64}(undef,M_num,draw_num)
    moment_var = Matrix{Float64}(undef,M_num,draw_num)
    # sqrt_n = sqrt(calc_pop(m.data))
    N = length(unique(person(d.data)))
    sqrt_n = sqrt(N)

    #Sample By States


    for i in 1:draw_num
        # moment_draws[:,i] = sqrt_n.*costMoments_bootstrap(c,d,p)
        moment_draws[:,i] = costMoments_bootstrap(c,d,p)
        # moment_draws[:,i] = sqrt_n.*costMoments_bootstrap(c,d,p).^2
    end
    check = sum(moment_draws,dims=1)
    nan_ind = findall( ! (isnan.(check[:]) .| isinf.(check[:])))
    moment_draws = moment_draws[:,nan_ind]

    Σ_unwt = scattermat(moment_draws,dims=2)
    Σ_unwt = Σ_unwt./draw_num

    ## Assymptotic Variance
    mean_moments = mean(moment_draws,dims=2)
    for i in 1:draw_num
        moment_var[:,i] = (moment_draws[:,i]-mean_moments)
    end

    Σ = scattermat(moment_var,dims=2)
    Σ = N.*(Σ./draw_num)

    return Σ, Σ_unwt, mean_moments
end

function calc_pop(df::ChoiceData)
    Pop = 0.0
    wgts = weight(df)[:]
    for (i,idx_itr) in df._personDict
        p_obs = wgts[idx_itr[1]]
        Pop += p_obs
    end
    return Pop
end

function calc_pop_sq(df::ChoiceData)
    Pop = 0.0
    wgts = weight(df)[:]
    for (i,idx_itr) in df._personDict
        p_obs = wgts[idx_itr[1]]
        Pop += p_obs^2
    end
    return Pop
end

function GMM_var(c::MC_Data,d::InsuranceLogit,p::Array{Float64},par_est::parDict{Float64},p_dem_vec::Vector{Float64},
                    W::Matrix{Float64},G_θ::Matrix{Float64};draw_num=1000)
    ## Moment Variance
    println("Moment Variance")
    # S,mom_est = var_bootstrap(c,d,p,par_est,draw_num=draw_num)
    S,Σ,Δ,S_m = aVar(c,d,p,par_est)
    println(size(S))
    ## Derivative of Moments wrt Parameters
    println("Moment Gradient")
    M_γ = mom_gradient(p,par_est,d,c)
    (K,Q) = size(M_γ)

    #### Newey McFadden  1994
    println("Demand Gradients")
    ## Derivative of Cost Moments wrt Demand Parameters
    M_θ = stage1_gradient(p_dem_vec,p,d,c)
    (K2,R) = size(M_θ)

    (R2,J) = size(G_θ)
    G = zeros(K+J,R+Q)
    G[1:J,1:R] = G_θ'
    G[(J+1):(J+K),1:R] = M_θ[:,:]
    G[(J+1):(J+K),(R+1):(Q+R)] = M_γ[:,:]

    W_new = Matrix{Float64}(I,J+K,J+K)
    W_new[1:J,1:J] = inv(S[1:J,1:J])
    W_new[(J+1):(J+K),(J+1):(J+K)] = W[:,:]

    ## Calculate Variance
    term1 = G'*W_new*G
    # inv_term1 = inv(term1)
    Avar =  inv(G'*W_new*G)*(G'*W_new*S*W_new*G)*inv(G'*W_new*G)
    Avar = Avar[(R+1):(Q+R),(R+1):(Q+R)]

    # Avar_test = inv(M_γ'*W*M_γ)*(M_γ'*W*S_m*W*M_γ)*inv(M_γ'*W*M_γ)

    N = length(unique(person(d.data)))

    V = Avar./N
    ## Calculate Standard Error
    if any(diag(V.<0))
        println("Some negative variances")
        stdErr = sqrt.(abs.(diag(V)))
    else
        stdErr = sqrt.(diag(V))
    end
    t_stat = p./stdErr

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



function mom_gradient(p::Vector{T},p_est::parDict{Float64},
                d::InsuranceLogit,c::MC_Data) where T
    Mlen = c.mom_length
    mom_grad = Matrix{Float64}(undef,Mlen,length(p))

    f_obj(x) = costMoments(c,d,x,p_est)
    ForwardDiff.jacobian!(mom_grad,f_obj,p)

    return mom_grad
end

function stage1_gradient(p_dem::Vector{T},p_cost::Vector{Float64},
                d::InsuranceLogit,c::MC_Data) where T
    Mlen = c.mom_length
    # mom_grad = Matrix{Float64}(undef,Mlen,length(p_dem))

    f_obj(x) = stage1_der_function(x,p_cost,d,c)
    mom_grad = FiniteDiff.finite_difference_jacobian(f_obj,p_dem)

    return mom_grad
end

function stage1_der_function(p_dem::Vector{T},p_cost::Vector{Float64},
                d::InsuranceLogit,c::MC_Data) where T
    par_est = parDict(d,p_dem,no2Der=true)
    individual_values!(d,par_est)
    individual_shares(d,par_est)

    moments = costMoments(c,d,p_cost,par_est)
    return moments
end
