# #### Profit Derivative Functions ####
# function evaluate_FOC_dprof(f::firmData,std_ind::Vector{Int64},merg::String="Base",voucher::Bool=false)
#     # P_std = zeros(length(f.P_j))
#     # P_RA = zeros(length(f.P_j))
#     # MC = zeros(length(f.P_j))
#     # Mkup = zeros(length(f.P_j))
#     # dSubs = zeros(length(f.P_j))

#     ownershipMatrix = f.ownMat
#     if merg=="Merger"
#         ownershipMatrix = f.ownMat_merge
#     elseif merg=="SP"
#         ownershipMatrix = ones(size(f.ownMat))
#     end

#     if voucher
#         dSdp = (f.dSAdp_j.*ownershipMatrix)[std_ind,std_ind]
#     else
#         dSdp = ((f.dSAdp_j - f.bench_prods.*f.dMAdp_j).*ownershipMatrix)[std_ind,std_ind]
#     end

#     cost_std = sum(transpose(f.dCdp_j[std_ind,std_ind]).*ownershipMatrix[std_ind,std_ind],dims=2)
#     cost_pl = sum(transpose(f.dCdp_pl_j[std_ind,std_ind]).*ownershipMatrix[std_ind,std_ind],dims=2)
#     SA = f.SA_j[std_ind]

#     # P_std[std_ind]= inv(dSdp)*(-SA + cost_std)
#     # P_RA[std_ind] = inv(dSdp)*(-SA + cost_pl)
#     #
#     # Mkup[std_ind] = inv(dSdp)*(-SA)
#     # MC[std_ind] = inv(dSdp)*(cost_std)
#     subs_term = sum(f.dSubdp_j[std_ind,std_ind].*ownershipMatrix[std_ind,std_ind],dims=2)
#     # dSubs[std_ind] = inv(dSdp)*sum(f.dSubdp_j[std_ind,std_ind].*ownershipMatrix[std_ind,std_ind],dims=2)

#     return cost_std, cost_pl, SA, subs_term, dSdp
# end


# function evaluate_FOC(f::firmData,std_ind::Vector{Int},merg::String="Base")
#     P_std, P_RA, MR, MC = evaluate_FOC(f,std_ind,merg)
#     return P_std, P_RA, MR, MC
# end

# function evaluate_FOC_dprof(f::firmData,merg::String="Base")
#     std_ind = f.prods
#     cost_std, cost_pl, SA, subs_term, dSdp = evaluate_FOC_dprof(f,std_ind,merg)
#     return cost_std, cost_pl, SA, subs_term, dSdp
# end

# function predict_dprof(f::firmData,std_ind::Vector{Int};sim="Base",merg::String="Base",λ::Float64=0.0,voucher::Bool=false)
#     dProf = zeros(length(f.P_j))
#     cost_std, cost_pl, SA, subs_term, dSdp = evaluate_FOC_dprof(f,std_ind,merg,voucher)
#     # println(P_std[f._prodSTDict[ST]])

#     if sim=="RA"
#         # P_new = copy(P_std)
#         dProf[std_ind] = SA  + dSdp*f.P_j[std_ind]  - cost_std
#     elseif sim=="Base"
#         # P_new = copy(P_RA)
#         dProf[std_ind] = dSdp*f.P_j[std_ind] +  (SA  - cost_pl)
#     elseif sim=="SP"
#         # P_new = copy(MC)
#         dProf[std_ind] = dSdp*f.P_j[std_ind]  - cost_std
#     elseif sim=="SP_gov"
#         # P_new = copy(MC) + copy(dSubs)
#         dProf[std_ind] = dSdp*f.P_j[std_ind]  - cost_std + subs_term
#     elseif sim=="SPλ"
#         # P_new = MC + λ.*Mkup
#         dProf[std_ind] = λ.*SA  + dSdp*f.P_j[std_ind]  - cost_std
#     elseif sim=="SPλ_gov"
#         # P_new = MC + λ.*Mkup + (1-λ).*dSubs
#         dProf[std_ind] = λ.*SA  + dSdp*f.P_j[std_ind]  - cost_std - (1-λ).*subs_term
#     end
#     return dProf
# end



function compute_profit(d::InsuranceLogit,f::firmData)
    Revenue = zeros(length(d.prods))
    Cost = zeros(length(d.prods))
    Share = zeros(length(d.prods))
    Pooled_Cost = zeros(length(d.prods))

    Market_Total = zeros(length(d.prods))
    # mandate_long = f[:Mandate]
    # Mems   = f[:MEMBERS]
    wgts_long = weight(d.data)[:]
    prod_long = Int.(product(d.data))
    age_long = f[:ageRate]
    mem_long = f[:MEMBERS]

    for idxitr in values(d.data._personDict)
        # prod_ids = f.stdMap[prod_long[idxitr]]
        prod_ids =prod_long[idxitr]
        catas = findall(inlist(prod_ids,f.catas_prods))

        s_pred = f.s_pred[idxitr]
        cost = f.c_pred[idxitr]
        cost_pl = f.c_pool[idxitr]
        rev = f.Rev_ij[idxitr]
        age = age_long[idxitr]
        mem = mem_long[idxitr]

        s_ins = sum(s_pred) - sum(s_pred[catas])
        # pr = (f.P_ij[idxitr]*1000 + mandate_long[idxitr])
        # pr = ((pr.*Mems[idxitr])/12 + f.subsidy_ij[idxitr])./Mems[idxitr]

        wgt = wgts_long[idxitr]


        for k in eachindex(prod_ids)
            j = prod_ids[k]
            Revenue[j] += wgt[k]*s_pred[k]*rev[k]
            Cost[j] += wgt[k]*s_pred[k]*cost[k]
            Pooled_Cost[j] += wgt[k]*s_ins*cost_pl[k]
            Market_Total[j] += wgt[k]*s_ins
            Share[j] += wgt[k]*s_pred[k]*age[k]/mem[k]
            # if isnan(Cost[j])
            #     println(idxitr)
            #     return
            # end
        end
    end

    Pooled_Cost[f.prods] = Share[f.prods].*Pooled_Cost[f.prods]./Market_Total[f.prods]


    PC_adj = copy(Pooled_Cost)
    PC_adj[f.catas_prods].=0.0

    C_adj = copy(Cost)
    C_adj[f.catas_prods].=0.0

    Adj = zeros(length(Cost))
    Adj[f.prods] = ((f.poolMat*C_adj)./(f.poolMat*PC_adj))[f.prods]
    Adj[f.catas_prods].=0.0
    Pooled_Cost = Pooled_Cost.*Adj
    Pooled_Cost[f.catas_prods] = Cost[f.catas_prods]

    Rev_Firm = f.ownMat*Revenue
    Cost_Firm = f.ownMat*Cost
    PC_Firm = f.ownMat*Pooled_Cost
    Profit = Rev_Firm - Cost_Firm
    # MLR = Cost_Firm./Rev_Firm
    return Revenue, Cost, Share, Pooled_Cost, Adj
end

function test_MR(m::InsuranceLogit,f::firmData,prod_int::Int,mkt::Int)
    ϵ = 1e-6
    println("First Evaluation")
    evaluate_model!(model,f,mkt,foc_check=false,voucher=true,deriv=false)
    Rev1, Cost1, Share1, PC1, Adj1 = compute_profit(m,f)
    all_profits = market_profits(m,f)
    prof1 = all_profits[mkt]
    f.P_j[prod_int]+=ϵ
    println("Deviation Evaluation")
    evaluate_model!(model,f,mkt,foc_check=false,voucher=true,deriv=false)
    Rev2, Cost2, Share2, PC2, Adj2 = compute_profit(m,f)
    all_profits = market_profits(m,f)
    prof2 = all_profits[mkt]

    dProf = (prof2-prof1)/ϵ
    dR = (Rev2 - Rev1)./ϵ
    dC = (Cost2 - Cost1)./ϵ
    dS = (Share2 - Share1)./ϵ
    dPC = (PC2 - PC1)./ϵ
    dAdj = (Adj2 - Adj1)./ϵ
    return dProf, dC, dS, dPC, dAdj
end

function prof_margin(f::firmData,std_ind::Union{Vector{Int64},Missing}=missing)
    if ismissing(std_ind)
        std_ind = f.prods
    end
    dSdp = (f.dSAdp_j.*f.ownMat)[std_ind,std_ind]

    MR = inv(dSdp)*sum(f.dRdp_j.*f.ownMat,dims=2)[std_ind]
    Mkup = -inv(dSdp)*f.SA_j[std_ind]
    # MR = -inv(dSdp)*f.SA_j[std_ind]

    cost_std = sum(transpose(f.dCdp_j[std_ind,std_ind]).*f.ownMat[std_ind,std_ind],dims=2)
    cost_pl = sum(transpose(f.dCdp_pl_j[std_ind,std_ind]).*f.ownMat[std_ind,std_ind],dims=2)

    MC_std = inv(dSdp)*cost_std
    MC_RA = inv(dSdp)*cost_pl
    # MR = -sum(f.dRdp_j.*f.ownMat,dims=2)[std_ind]
    # MC_std = -sum(f.dCdp_j.*f.ownMat,dims=2)[std_ind]
    # MC_RA = -sum(f.dCdp_pl_j.*f.ownMat,dims=2)[std_ind]
    return Mkup, MR,MC_std[:],MC_RA[:]
end

function prof_margin_raw(f::firmData,std_ind::Union{Vector{Int64},Missing}=missing)
    if ismissing(std_ind)
        std_ind = f.prods
    end
    dSdp = (f.dSAdp_j)[std_ind,std_ind]

    MR = f.SA_j[std_ind] + dSdp*f.P_j[std_ind]
    # MR = -inv(dSdp)*f.SA_j[std_ind]

    cost_std = sum(transpose(f.dCdp_j[std_ind,std_ind]),dims=2)
    cost_pl = sum(transpose(f.dCdp_pl_j[std_ind,std_ind]),dims=2)

    return MR,cost_std,cost_pl
end


function checkMargin(m::InsuranceLogit,f::firmData,file::String)
    evaluate_model!(m,f,"All",foc_check=true)
    Mkup,MR,MC_std,MC_RA = prof_margin(f)
    avgCost = f.C_j[f.prods]
    pooledCost = f.PC_j[f.prods]
    lives = f.S_j[f.prods]
    ageRate = f.SA_j[f.prods]./lives
    P_obs = f.P_j[f.prods]
    avgR = calc_avgR(m,f)

    output =  DataFrame(Product=f.prods,
                        P_obs = P_obs,
                        Mkup = Mkup,
                        MR = MR,
                        MC_std = MC_std,
                        MC_RA = MC_RA,
                        avgCost = avgCost,
                        pooledCost = pooledCost,
                        lives=lives,
                        avgR = avgR,
                        ageRate=ageRate)

    CSV.write(file,output)
    return nothing
end

function setMarginCostAdjust!(m::InsuranceLogit,f::firmData)
    evaluate_model!(m,f,"All",foc_check=true)
    Mkup,MR,MC_std,MC_RA = prof_margin(f)
    
    f.ω_j = MR - MC_RA
    return nothing
end


function calc_avgR(m::InsuranceLogit,f::firmData)
    R_j = zeros(maximum(m.prods))
    wgts = weight(m.data)[:]
    s_hat = f.par_dem.s_hat
    wgt_share = wgts.*s_hat
    r_hat = f.par_dem.r_hat
    for (j,idx_j) in f._productDict
        avgR = sliceMean_wgt(r_hat,wgt_share,idx_j)
        R_j[j] = avgR
    end
    return R_j[f.prods]
end


function evaluate_FOC(f::firmData,std_ind::Vector{Int64},merg::String="Base",voucher::Bool=false)
    P_std = zeros(length(f.P_j))
    P_RA = zeros(length(f.P_j))
    MC = zeros(length(f.P_j))
    Mkup = zeros(length(f.P_j))
    Mkup_CS = zeros(length(f.P_j))
    dSubs = zeros(length(f.P_j))

    ownershipMatrix = f.ownMat
    if merg=="Merger"
        ownershipMatrix = f.ownMat_merge
    elseif merg=="SP"
        ownershipMatrix = ones(size(f.ownMat))
    end

    if voucher
        dSdp = (f.dSAdp_j.*ownershipMatrix)[std_ind,std_ind]
    else
        dSdp = ((f.dSAdp_j - f.bench_prods.*f.dMAdp_j).*ownershipMatrix)[std_ind,std_ind]
    end

    cost_std = sum(transpose(f.dCdp_j[std_ind,std_ind]).*ownershipMatrix[std_ind,std_ind],dims=2)
    cost_pl = sum(transpose(f.dCdp_pl_j[std_ind,std_ind]).*ownershipMatrix[std_ind,std_ind],dims=2)
    SA = f.SA_j[std_ind]
    S = f.S_j[std_ind]

    P_std[std_ind]= inv(dSdp)*(-SA + cost_std)
    P_RA[std_ind] = inv(dSdp)*(-SA + cost_pl)

    Mkup[std_ind] = inv(dSdp)*(-SA)
    Mkup_CS[std_ind] = inv(dSdp)*(-S)
    MC[std_ind] = inv(dSdp)*(cost_std)
    dSubs[std_ind] = inv(dSdp)*sum(transpose(f.dSubdp_j[std_ind,std_ind]).*ownershipMatrix[std_ind,std_ind],dims=2)

    return P_std, P_RA, Mkup, Mkup_SP, MC, dSubs
end

function evaluate_GePP(f::firmData,std_ind::Vector{Int64},voucher::Bool=false)
    J = length(f.S_j)
    S_mat = Matrix{Float64}(undef,J,J)
    P_mat = Matrix{Float64}(undef,J,J)
    AC_mat = Matrix{Float64}(undef,J,J)
    dSAdp_Diag_mat = Matrix{Float64}(undef,J,J)
    S_mat[:,:].=f.S_j
    P_mat[:,:].=f.P_j
    AC_mat[:,:].=f.C_j
    dSAdp_Diag_mat[:,:].=diag(f.dSAdp_j)


    dAdp = (f.dCdp_j .- f.dSdp_j.*AC_mat)./S_mat
    dAdp = dAdp - Diagonal(diag(dAdp))

    Div_jk = -f.dSAdp_j./transpose(dSAdp_Diag_mat) + I
    age_margin_ratio = f.dSdp_j./f.dSAdp_j
    age_margin_ratio[isnan.(age_margin_ratio)].=0.0
    UPP = Div_jk.*(P_mat .- age_margin_ratio.*AC_mat)
    UPP[isnan.(UPP)].=0.0
    # UPP_rev = Div_jk.*P_mat

    UPP_cost = f.dSdp_j.*AC_mat./transpose(dSAdp_Diag_mat)
    UPP_cost = UPP_cost - Diagonal(diag(UPP_cost))
    UPP_cost[isnan.(UPP_cost)].=0.0
    # UPP_cost = Div_jk.*age_margin_ratio.*AC_mat

    UPP_sel = (S_mat.*dAdp)./transpose(dSAdp_Diag_mat)
    UPP_sel = UPP_sel - Diagonal(diag(UPP_sel))
    UPP_sel[isnan.(UPP_sel)].=0.0

    Mkup = -f.SA_j./diag(f.dSAdp_j)
    MC = diag(f.dCdp_j)./diag(f.dSAdp_j)

    tot_UPP = transpose(sum(UPP.*f.ownMat,dims=1))
    tot_UPP_sel = transpose(sum(UPP_sel.*f.ownMat,dims=1))

    MC_all = diag(f.dCdp_j)./diag(f.dSAdp_j) .+ transpose(sum((UPP_cost .+ UPP_sel).*f.ownMat,dims=1))


    println(f.P_j[prod_ind] - ( Mkup + MC +tot_UPP + tot_UPP_sel)[prod_ind])
    maximum(abs.((f.P_j[prod_ind] - ( Mkup + MC +tot_UPP + tot_UPP_sel)[prod_ind])))

    ######
    # prod_ind =findall(f.SA_j.!=0.0)
    SA = f.SA_j[prod_ind]
    cost = sum(transpose(f.dCdp_j[prod_ind,prod_ind]).*f.ownMat[prod_ind,prod_ind],dims=2)
    dSdp = (f.dSAdp_j[prod_ind,prod_ind].*f.ownMat[prod_ind,prod_ind])
    P_foc = inv(dSdp)*(-f.SA_j[prod_ind]+cost)
    println(f.P_j[prod_ind] - P_foc)

    ###### 
    dSdp*P_foc + SA - sum(transpose(f.dCdp_j[prod_ind,prod_ind]).*f.ownMat[prod_ind,prod_ind],dims=2)
    dSdp_cross = dSdp - Diagonal(diag(dSdp))
    cost_cross = f.dCdp_j[prod_ind,prod_ind].*f.ownMat[prod_ind,prod_ind]
    cost_cross = cost_cross - Diagonal(diag(cost_cross))

    P_foc.*diag(dSdp) + dSdp_cross*P_foc + SA - cost
    # P_foc + (dSdp_cross*P_foc)./diag(dSdp) + SA./diag(dSdp) - diag(f.dCdp_j[prod_ind,prod_ind])./diag(dSdp) - sum(cost_cross,dims=2)./diag(dSdp)
    P_foc - (-SA./diag(dSdp)+ diag(f.dCdp_j[prod_ind,prod_ind])./diag(dSdp) -(dSdp_cross*f.P_j[prod_ind])./diag(dSdp) + sum(transpose(cost_cross),dims=2)./diag(dSdp))

    mkup_test =-SA./diag(dSdp) 
    mc_test = diag(f.dCdp_j[prod_ind,prod_ind])./diag(dSdp)
    upp_test = -(dSdp_cross*P_foc)./diag(dSdp) + sum(transpose(cost_cross)./diag(dSdp),dims=2)
    mc_all_test =  diag(f.dCdp_j[prod_ind,prod_ind])./diag(dSdp) + sum(transpose(cost_cross)./diag(dSdp),dims=2)-(dSdp_cross*P_foc)./diag(dSdp)
    mkup_all_test = -SA./diag(dSdp) 
    return UPP_sel
end

# function evaluate_FOC(f::firmData,std_ind::Vector{Int},merg::String="Base")
#     P_std, P_RA, MR, MC = evaluate_FOC(f,std_ind,merg)
#     return P_std, P_RA, MR, MC
# end

function evaluate_FOC(f::firmData,merg::String="Base")
    std_ind = f.prods
    P_std, P_RA, Mkup, Mkup_CS, MC, dSubs = evaluate_FOC(f,std_ind,merg)
    return P_std, P_RA, Mkup, Mkup_CS, MC, dSubs
end

function predict_price(f::firmData,prod_ind::Vector{Int};sim="Base",merg::String="Base",λ::Float64=0.0,voucher::Bool=false)

    P_std, P_RA, Mkup, Mkup_CS, MC, dSubs = evaluate_FOC(f,prod_ind,merg,voucher)
    # println(P_std[f._prodSTDict[ST]])

    if sim=="RA"
        P_new = copy(P_std)
    elseif sim=="Base"
        P_new = copy(P_RA)
    elseif sim=="SP"
        P_new = copy(Mkup).-copy(Mkup_CS) .+ copy(MC)
    elseif sim=="SP_gov"
        P_new = copy(Mkup).-copy(Mkup_CS) .+ copy(MC) .+ copy(dSubs)
    elseif sim=="SPλ"
        P_new = copy(Mkup).- (1-λ).*copy(Mkup_CS) .+ copy(MC)
    elseif sim=="SPλ_gov"
        P_new = copy(Mkup).- (1-λ).*copy(Mkup_CS) .+ copy(MC) .+ copy(dSubs)
    end
    return P_new
end

function foc_error(f::firmData,ST::String;λ::Float64=0.0,sim="Base",merg::String="Base",voucher::Bool=false)
    prod_ind = f._prodSTDict[ST]
    return foc_error(f,prod_ind,λ=λ,sim=sim,merg=merg,voucher=voucher)
end

function foc_error(f::firmData,mkt::Int;λ::Float64=0.0,sim="Base",merg::String="Base",voucher::Bool=false)
    prod_ind = f.mkt_index[mkt]
    return foc_error(f,prod_ind,λ=λ,sim=sim,merg=merg,voucher=voucher)
end

function foc_error(f::firmData,prod_ind::Vector{Int};λ::Float64=0.0,sim="Base",merg::String="Base",voucher::Bool=false)

    P_new = predict_price(f,prod_ind,sim=sim,merg=merg,λ=λ,voucher=voucher)
    tot_err = (P_new[prod_ind] - f.P_j[prod_ind])./100
    # println(prod_ind)
    # println(tot_err)
    # println(P_new[prod_ind])

    # dprof = predict_dprof(f,prod_ind,sim=sim,merg=merg,λ=λ,voucher=voucher)
    # println("Profit Derivatives: $(dprof[prod_ind])")

    non_prods = .!(inlist(Int.(1:length(P_new)),prod_ind))

    ### 0 Market Share
    exit_thresh = 0.001
    ProdExit = f.S_j.< exit_thresh
    # choke_point = 0.0001
    # ChokePrice = f.S_j.< choke_point
    P_new[ProdExit] = min.(f.P_j[ProdExit],P_new[ProdExit])

    #Price Cap
    # P_new[P_new.>5e4] .= 5e4

    # if any(ProdExit[prod_ind])
    #     exited = length(findall(ProdExit[prod_ind]))
    #     choked = length(findall(ChokePrice[prod_ind]))
    #     all = length(prod_ind)
    #     # println("Product Exits for $exited out of $all products, $choked at choke price.")
    # end

    ### Negative Prices
    P_new[non_prods].=0.0
    P_new[P_new.<0] = 0.5.*f.P_j[P_new.<0]

    ## Error in Prices
    prod_ind_ne = prod_ind[f.S_j[prod_ind].>exit_thresh]
    foc_err = (P_new[prod_ind_ne] - f.P_j[prod_ind_ne])./100


    # ### MLR Constraint
    # MLR_const = e[:C]./0.7
    # constrained_bool = (P_new.>=MLR_const).& (e[:S_j].>=(1e-5))
    # if any( constrained_bool )
    #     constrained = findall( constrained_bool )
    #     println("Hit MLR Constraint at products $constrained")
    #     P_const = MLR_const[constrained]
    #     println("Constrained prices: $P_const")
    #     foc_err[constrained] .= 0.0
    # end
    #P_new = min.(P_new,MLR_const)


    err_new = sum(foc_err.^2)/length(prod_ind_ne)
    tot_err = sum(tot_err.^2)/length(prod_ind)

    ### New Prices

    P_new[non_prods] = f.P_j[non_prods]
    # P_update = f.P_j.*(1-stp) + stp.*P_new
    # P_update[non_prods] = f.P_j[non_prods]
    # P_update[P_new.>=MLR_const] = MLR_const[P_new.>MLR_const]
    # Contrain Prices at 0
    #P_update = max.(P_update,0)

    # f.P_j[:] = P_update[:]

    return foc_err, err_new, tot_err, P_new
end


function calc_risk_avg(d::InsuranceLogit,f::firmData)
    wgts = weight(d.data)[1,:]
    wgts_share = wgts.*f.s_pred
    num_prods = maximum(d.prods)
    r_hat_j = Vector{Float64}(undef,num_prods)
    r_hat_j[:].=0.0
    r_hat = f.r_pred

    for j in d.prods
        j_index_all = d.data._productDict[j]
        r_hat_j[j] = sliceMean_wgt(r_hat,wgts_share,j_index_all)
    end
    
    return r_hat_j
end