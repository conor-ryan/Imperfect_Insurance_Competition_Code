using StatsBase

function aVar(c::MC_Data,d::InsuranceLogit,p::Array{Float64,1},p_est::parDict{Float64})

    par = parMC(p,p_est,d,c) # Fix p0
    individual_costs(d,par)

    # Pop = [0.0]
    Pop = 0.0
    wgts = weight(d.data)

    ### Unique Product IDs
    (N,M) = size(d.data.data)
    productIDs = Vector{Int64}(undef,M)
    for j in d.prods
        idx_prod = d.data._productDict[j]
        productIDs[idx_prod] .= j
    end
    num_prods = length(d.prods)



    cost_moments = Vector{Float64}(undef,num_prods*2+length(c.ageMoments)*2+4)
    cost_moments[:] .= 0.0
    mom_length = length(cost_moments)
    g_n = Vector{Float64}(undef,mom_length)

    Σ = zeros(mom_length,mom_length)

    ## Estimate of population mean...
    for app in eachperson(d.data)

        idx_prod, wgt_obs = cost_obs_moments!(g_n,productIDs,app,d,c,par)

        idx_nonEmpty = vcat(idx_prod,num_prods .+idx_prod,(num_prods*2+1):mom_length)
        # Pop[:] = Pop[:] + [wgt_obs]
        Pop += wgt_obs
        # add_Σ(Σ,g_n,idx_nonEmpty)
        cost_moments[:] += g_n[:]
    end
    mean_moments = cost_moments./Pop

    ## Estimate of variance...
    # g_n = Vector{Float64}(undef,mom_length)
    for app in eachperson(d.data)

        idx_prod, wgt_obs = cost_obs_moments!(g_n,productIDs,app,d,c,par)
        g_n[:] = g_n[:] - mean_moments[:]
        idx_nonEmpty = vcat(idx_prod,num_prods .+idx_prod,(num_prods*2+1):mom_length)
        add_Σ(Σ,g_n,idx_nonEmpty)
    end

    Σ = Σ./(Pop)
    # println(Pop)
    Δ = Δavar(c,d,mean_moments)

    # (N,M) = size(Σ)
    # aVar = zeros(d.parLength[:All] + length(d.data.tMoments),M)
    # (Q,R) = size(aVar)
    # aVar[1:length(d.data.tMoments),(1:num_prods*2)] = risk_moments_Avar(d,p)
    # aVar[(length(d.data.tMoments)+1):Q,(num_prods*2 + 1):R] = Matrix{Float64}(I,d.parLength[:All],d.parLength[:All])
    #
    S_est = Δ*Σ*Δ'

    return S_est, Σ, Δ, mean_moments
end


function cost_obs_moments!(mom_obs::Vector{Float64},productIDs::Vector{Int64},
                    app::ChoiceData,d::InsuranceLogit,c::MC_Data,p::parMC{Float64}) where T
    mom_obs[:] .= 0.0
    wgts = weight(app)[1,:]
    ind = person(app)[1]
    num_prods = length(d.prods)


    idxitr = app._personDict[ind]
    ind_itr = 1:length(idxitr)
    per_prods = productIDs[idxitr]

    age = c.data[1,idxitr][1]
    age_ind = Int.(max(floor((age-20.0)/5),0)) + 1

    costs = p.C[idxitr]
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

    for (i,k,j) in zip(ind_itr,idxitr,per_prods)
        #S_hat
        mom_obs[j] = s_hat[i]*wgts[i]
        #C_hat
        mom_obs[num_prods + j] = costs[i]*s_hat[i]*wgts[i]
    end

    #Insurance
    mom_obs[num_prods*2 + age_ind] = ins_share*wgts[1]
    #Age Bin
    mom_obs[num_prods*2 + length(c.ageMoments) + age_ind] = ins_cost*wgts[1]
    #Insurance by Risk
    mom_obs[num_prods*2+length(c.ageMoments)*2+1] = ins_share_nonrisk*wgts[1]*(1-anyHCC[1])
    mom_obs[num_prods*2+length(c.ageMoments)*2+2] = ins_share_risk*wgts[1]*(anyHCC[1])
    #Cost by Risk
    mom_obs[num_prods*2+length(c.ageMoments)*2+3] = ins_cost_nonrisk*wgts[1]*(1-anyHCC[1])
    mom_obs[num_prods*2+length(c.ageMoments)*2+4] = ins_cost_risk*wgts[1]*(anyHCC[1])

    return per_prods, wgts[1]
end


function add_Σ(Σ::Matrix{Float64},g_n::Vector{Float64},idx::Vector{Int64})
    for i in idx, j in idx
        @fastmath @inbounds Σ[i,j]+=g_n[i]*g_n[j]
    end
    return nothing
end


function test_Avar(c::MC_Data,d::InsuranceLogit,p::Array{Float64,1},p_est::parDict{Float64})
    par = parMC(p,p_est,d,c) # Fix p0
    individual_costs(d,par)

    Pop = 0.0
    wgts = weight(d.data)

    ### Unique Product IDs
    (N,M) = size(d.data.data)
    productIDs = Vector{Int64}(undef,M)
    for j in d.prods
        idx_prod = d.data._productDict[j]
        productIDs[idx_prod] .= j
    end
    num_prods = length(d.prods)

    cost_moments = Vector{Float64}(undef,num_prods*2+2*length(c.ageMoments)+4)
    mom_length = length(cost_moments)
    g_n = Vector{Float64}(undef,mom_length)

    Σ = zeros(mom_length,mom_length)
    p_moments_p1= Vector{Float64}(undef,length(c.avgMoments))
    p_moments_p2= Vector{Float64}(undef,length(c.avgMoments))
    a_moments_p1= Vector{Float64}(undef,length(c.ageMoments))
    a_moments_p2= Vector{Float64}(undef,length(c.ageMoments))
    r_moments_p1= Vector{Float64}(undef,2)
    r_moments_p2= Vector{Float64}(undef,2)

    p_moments_p1[:] .= 0.0
    p_moments_p2[:] .= 0.0
    a_moments_p1[:] .= 0.0
    a_moments_p2[:] .= 0.0
    r_moments_p1[:] .= 0.0
    r_moments_p2[:] .= 0.0


    pmom = Vector{Float64}(undef,length(c.avgMoments))
    amom = Vector{Float64}(undef,length(c.ageMoments)-1)

    for app in eachperson(d.data)

        idx_prod, wgt_obs = cost_obs_moments!(cost_moments,productIDs,app,d,c,par)

        ## Product and Firm Moments
        for (m,m_idx) in c._avgMomentProdDict
            p_moments_p1[m] += sum(cost_moments[m_idx])
            p_moments_p2[m] += sum(cost_moments[num_prods .+ m_idx])
        end
        ## Age Moments
        a_moments_p1[:] += cost_moments[num_prods*2 .+ (1:length(c.ageMoments))]
        a_moments_p2[:] += cost_moments[(num_prods*2 + length(c.ageMoments)) .+ (1:length(c.ageMoments))]
        ## Risk Moments
        r_moments_p1[:] += cost_moments[(num_prods*2 + 2*length(c.ageMoments)) .+ (1:2)]
        r_moments_p2[:] += cost_moments[(num_prods*2 + 2*length(c.ageMoments)) .+ (3:4)]
    end

    ## Total Product Moments
    for (m,m_idx) in c._avgMomentProdDict
        pmom[m] = log(p_moments_p2[m]/p_moments_p1[m]) - c.avgMoments[m]
        # pmom[m] = p_moments_p2[m]/p_moments_p1[m]
    end
    ## Total Age Moments
    refval = a_moments_p2[1]/a_moments_p1[1]
    for m in 2:length(a_moments_p1)
        c_avg = a_moments_p2[m]/a_moments_p1[m]
        amom[m-1] = c_avg/refval - c.ageMoments[m]
    end
    ## Total Risk Moments
    non_avg = r_moments_p2[1]/r_moments_p1[1]
    HCC_avg = r_moments_p2[2]/r_moments_p1[2]
    rmom = HCC_avg/non_avg  - c.riskMoment
    return vcat(pmom,amom,rmom)
end


function test_Avar(c::MC_Data,d::InsuranceLogit,cost_moments::Vector{T}) where T

    num_prods = length(d.prods)
    p_moments_p1= Vector{T}(undef,length(c.avgMoments))
    p_moments_p2= Vector{T}(undef,length(c.avgMoments))
    a_moments_p1= Vector{T}(undef,length(c.ageMoments))
    a_moments_p2= Vector{T}(undef,length(c.ageMoments))
    r_moments_p1= Vector{T}(undef,2)
    r_moments_p2= Vector{T}(undef,2)

    p_moments_p1[:] .= 0.0
    p_moments_p2[:] .= 0.0
    a_moments_p1[:] .= 0.0
    a_moments_p2[:] .= 0.0
    r_moments_p1[:] .= 0.0
    r_moments_p2[:] .= 0.0


    pmom = Vector{T}(undef,length(c.avgMoments))
    amom = Vector{T}(undef,length(c.ageMoments)-1)

    ## Product and Firm Moments
    for (m,m_idx) in c._avgMomentProdDict
        p_moments_p1[m] = sum(cost_moments[m_idx])
        p_moments_p2[m] = sum(cost_moments[num_prods .+ m_idx])
    end
    ## Age Moments
    a_moments_p1[:] = cost_moments[num_prods*2 .+ (1:length(c.ageMoments))]
    a_moments_p2[:] = cost_moments[(num_prods*2 + length(c.ageMoments)) .+ (1:length(c.ageMoments))]
    ## Risk Moments
    r_moments_p1[:] = cost_moments[(num_prods*2 + 2*length(c.ageMoments)) .+ (1:2)]
    r_moments_p2[:] = cost_moments[(num_prods*2 + 2*length(c.ageMoments)) .+ (3:4)]

    ## Total Product Moments
    for (m,m_idx) in c._avgMomentProdDict
        pmom[m] = log(p_moments_p2[m]/p_moments_p1[m]) - c.avgMoments[m]
        # pmom[m] = p_moments_p2[m]/p_moments_p1[m]
    end
    ## Total Age Moments
    refval = a_moments_p2[1]/a_moments_p1[1]
    for m in 2:length(a_moments_p1)
        c_avg = a_moments_p2[m]/a_moments_p1[m]
        amom[m-1] = c_avg/refval - c.ageMoments[m]
    end
    ## Total Risk Moments
    non_avg = r_moments_p2[1]/r_moments_p1[1]
    HCC_avg = r_moments_p2[2]/r_moments_p1[2]
    rmom = HCC_avg/non_avg  - c.riskMoment
    return vcat(pmom,amom,rmom)[315]
end


function Δavar(c::MC_Data,d::InsuranceLogit,cost_moments::Vector{Float64})
    M_num = length(c.avgMoments) + (length(c.ageMoments)-1) + 1
    num_prods = length(d.prods)
    Δ = zeros(M_num,length(cost_moments))

    ## Product and Firm Moments
    for (m,m_idx) in c._avgMomentProdDict
        for j in m_idx
            Δ[m,j] =  -1/sum(cost_moments[m_idx])
            Δ[m,num_prods+j] = 1/sum(cost_moments[num_prods .+ m_idx])
        end
    end

    ## Age Moments
    a_moments_p1 = cost_moments[num_prods*2 .+ (1:length(c.ageMoments))]
    a_moments_p2 = cost_moments[(num_prods*2 + length(c.ageMoments)) .+ (1:length(c.ageMoments))]


    refval = a_moments_p2[1]/a_moments_p1[1]
    for m in 2:(length(c.ageMoments))
        # ref value
        Δ[length(c.avgMoments) + m-1,num_prods*2 + 1] = (1/a_moments_p2[1])*(a_moments_p2[m]/a_moments_p1[m])
        Δ[length(c.avgMoments) + m-1,num_prods*2 + length(c.ageMoments) + 1] = -(1/refval)*(1/a_moments_p2[1])*(a_moments_p2[m]/a_moments_p1[m])
        # age val
        Δ[length(c.avgMoments) + m-1,num_prods*2 + m] = -(1/refval)*(a_moments_p2[m]/a_moments_p1[m])*(1/a_moments_p1[m])
        Δ[length(c.avgMoments) + m-1,num_prods*2 + length(c.ageMoments) + m] = (1/refval)*(1/a_moments_p1[m])
    end

    ## Risk Moments
    r_moments_p1 = cost_moments[(num_prods*2 + 2*length(c.ageMoments)) .+ (1:2)]
    r_moments_p2 = cost_moments[(num_prods*2 + 2*length(c.ageMoments)) .+ (3:4)]

    ## Total Risk Moments
    non_avg = r_moments_p2[1]/r_moments_p1[1]
    HCC_avg = r_moments_p2[2]/r_moments_p1[2]
    # rmom = HCC_avg/non_avg  - c.riskMoment
    Δ[M_num,num_prods*2 + length(c.ageMoments)*2 + 1] = (HCC_avg)*(1/r_moments_p2[1])
    Δ[M_num,num_prods*2 + length(c.ageMoments)*2 + 2] = -(HCC_avg/non_avg)*(1/r_moments_p1[2])
    Δ[M_num,num_prods*2 + length(c.ageMoments)*2 + 3] = -(HCC_avg/non_avg)*(1/r_moments_p2[1])
    Δ[M_num,num_prods*2 + length(c.ageMoments)*2 + 4] = (1/r_moments_p1[2])*(1/non_avg)
    return Δ
end



function bootstrap_sample(d::InsuranceLogit)
    (K,N) = size(d.data.data)
    draw_vec = Array{Int64}(undef,N*2)
    draw_vec_map = Array{Int64}(undef,N*2)
    perIDs = Int.(unique(d.data.data[1,:]))
    Per_num = length(perIDs)
    x0 = 1
    for j in 1:Per_num
        i = Int.(floor(rand()*Per_num) + 1)
        id = perIDs[i]
        idx_itr = d.data._personDict[id]
        xend = x0 + length(idx_itr)-1
        draw_vec[x0:xend] = idx_itr
        x0 = xend + 1
    end
    draw_vec = draw_vec[1:(x0-1)]
    sort!(draw_vec)

    return draw_vec
end


function costMoments_bootstrap(c::MC_Data,d::InsuranceLogit,p::parMC{T}) where T
    ## Bootstrap Random Draw
    draw = bootstrap_sample(d)

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

    pmom = Vector{T}(undef,length(c.avgMoments))
    amom = Vector{T}(undef,length(c.ageMoments)-1)

    ## Product and Firm Moments
    for (m,m_idx) in c._avgMomentBit
        # m_sample = draw[inlist_sorted(draw,m_idx)]
        m_sample = draw[m_idx[draw]]

        c_avg = sliceMean_wgt(c_hat,wgts_share,m_sample)
        pmom[m] = log(c_avg) - c.avgMoments[m]
        # pmom[m] = (c_avg - exp(c.avgMoments[m]))/100
        # pmom[m] = c_avg
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
    # all_idx = Int.(1:length(s_hat))
    m_sample = draw


    HCC_avg = sliceMean_wgt(c_hat_HCC,any_share,m_sample)
    non_avg = sliceMean_wgt(c_hat_nonHCC,none_share,m_sample)
    rmom = HCC_avg/non_avg - c.riskMoment

    return vcat(pmom,amom,rmom)
end

function var_bootstrap(c::MC_Data,d::InsuranceLogit,p::Array{T},p_est::parDict{Float64};draw_num::Int=1000) where T
    par = parMC(p,p_est,d,c) # Fix p0
    individual_costs(d,par)
    mom = var_bootstrap(c,d,par,draw_num=draw_num)
    return mom
end

function var_bootstrap(c::MC_Data,d::InsuranceLogit,p::parMC{T};draw_num::Int=1000) where T
    M_num = length(c.avgMoments) + length(c.ageMoments) -1 + 1
    moment_draws = Matrix{Float64}(undef,M_num,draw_num)
    sqrt_n = sqrt(calc_pop(m.data))
    for i in 1:draw_num
        moment_draws[:,i] = sqrt_n.*costMoments_bootstrap(c,d,p)
    end

    Σ = scattermat(moment_draws,2)
    Σ = Σ./draw_num
    mean_moments = mean(moment_draws./sqrt_n,dims=2)
    return Σ, mean_moments
end

function inlist(x::Vector{T},y::Vector{T}) where T
    bits = BitArray{1}(undef,length(x))
    # min_y = minimum(y)
    for (n,i) in enumerate(x)
        bits[n] = i in y
    end
    return bits
end

function inlist(x::UnitRange{T},y::Vector{Union{Missing,T}}) where T
    bits = BitArray{1}(undef,length(x))
    # min_y = minimum(y)
    for (n,i) in enumerate(x)
        bits[n] = i in y
    end
    return bits
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
