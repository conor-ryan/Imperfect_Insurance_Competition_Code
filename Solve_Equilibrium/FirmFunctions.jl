function compute_profit(d::InsuranceLogit,firm::firmData)
    Revenue = zeros(length(d.prods))
    Cost = zeros(length(d.prods))
    Share = zeros(length(d.prods))
    Pooled_Cost = zeros(length(d.prods))

    Market_Total = zeros(length(d.prods))
    # mandate_long = firm[:Mandate]
    # Mems   = firm[:MEMBERS]
    wgts_long = weight(d.data)[:]
    prod_long = Int.(product(d.data))
    age_long = firm[:ageRate]
    mem_long = firm[:MEMBERS]

    for idxitr in values(d.data._personDict)
        s_pred = firm.s_pred[idxitr]
        cost = firm.c_pred[idxitr]
        cost_pl = firm.c_pool[idxitr]
        rev = firm.Rev_ij[idxitr]
        age = age_long[idxitr]
        mem = mem_long[idxitr]

        s_ins = sum(s_pred)
        # pr = (firm.P_ij[idxitr]*1000 + mandate_long[idxitr])
        # pr = ((pr.*Mems[idxitr])/12 + firm.subsidy_ij[idxitr])./Mems[idxitr]

        wgt = wgts_long[idxitr]

        prod_ids = firm.stdMap[prod_long[idxitr]]
        for k in 1:length(prod_ids)
            j = prod_ids[k]
            Revenue[j] += wgt[k]*s_pred[k]*rev[k]
            Cost[j] += wgt[k]*s_pred[k]*cost[k]
            Pooled_Cost[j] += wgt[k]*s_ins*cost_pl[k]
            Market_Total[j] += wgt[k]*s_ins
            Share[j] += wgt[k]*s_pred[k]#*age[k]/mem[k]
        end
    end
    Pooled_Cost[firm.prods] = Share[firm.prods].*Pooled_Cost[firm.prods]./Market_Total[firm.prods]

    Adj = (firm.poolMat*Cost)./(firm.poolMat*Pooled_Cost)
    Pooled_Cost = Pooled_Cost.*Adj

    Rev_Firm = firm.ownMat*Revenue
    Cost_Firm = firm.ownMat*Cost
    PC_Firm = firm.ownMat*Pooled_Cost
    Profit = Rev_Firm - Cost_Firm
    # MLR = Cost_Firm./Rev_Firm
    return Revenue, Cost, Share, Pooled_Cost, Adj
end

function test_MR(m::InsuranceLogit,firm::firmData)
    ϵ = 1e-6
    println("First Evaluation")
    evaluate_model!(m,firm)
    Rev1, Cost1, Share1, PC1, Adj1 = compute_profit(m,firm)
    firm.P_j[57]+=ϵ
    println("Deviation Evaluation")
    evaluate_model!(m,firm)
    Rev2, Cost2, Share2, PC2, Adj2 = compute_profit(m,firm)

    dR = (Rev2 - Rev1)./ϵ
    dC = (Cost2 - Cost1)./ϵ
    dS = (Share2 - Share1)./ϵ
    dPC = (PC2 - PC1)./ϵ
    dAdj = (Adj2 - Adj1)./ϵ
    return dR, dC, dS, dPC, dAdj
end

function prof_margin(firm::firmData)
    std_ind = firm.prods
    dSdp = (firm.dSAdp_j.*firm.ownMat)[std_ind,std_ind]

    # MR = inv(dSdp)*sum(firm.dRdp_j.*firm.ownMat,dims=2)[std_ind]
    MR = -inv(dSdp)*firm.SA_j[std_ind]
    MC_std = inv(dSdp)*sum(firm.dCdp_j.*firm.ownMat,dims=2)[std_ind]
    MC_RA = inv(dSdp)*sum(firm.dCdp_pl_j.*firm.ownMat,dims=2)[std_ind]
    # MR = -sum(firm.dRdp_j.*firm.ownMat,dims=2)[std_ind]
    # MC_std = -sum(firm.dCdp_j.*firm.ownMat,dims=2)[std_ind]
    # MC_RA = -sum(firm.dCdp_pl_j.*firm.ownMat,dims=2)[std_ind]
    return MR,MC_std,MC_RA
end


function checkMargin(m::InsuranceLogit,f::firmData,file::String)
    evaluate_model!(m,f,"All",foc_check=true)
    Mkup,MC_std,MC_RA = prof_margin(firm)
    avgCost = f.C_j[f.prods]
    pooledCost = f.PC_j[f.prods]
    lives = f.S_j[f.prods]
    ageRate = f.SA_j[f.prods]./lives
    P_obs = f.P_j[f.prods]
    avgR = calc_avgR(m,f)

    output =  DataFrame(Product=f.prods,
                        P_obs = P_obs,
                        Mkup = Mkup,
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

function calc_avgR(m::InsuranceLogit,f::firmData)
    R_j = zeros(length(m.prods))
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

function evaluate_FOC(firm::firmData,ST::String)

    P_std = zeros(length(firm.P_j))
    P_RA = zeros(length(firm.P_j))

    std_ind = firm._prodSTDict[ST]


    dSdp = (firm.dSdp_j.*firm.ownMat)[std_ind,std_ind]
    cost_std = sum(firm.dCdp_j.*firm.ownMat,dims=2)[std_ind]
    cost_pl = sum(firm.dCdp_pl_j[std_ind,std_ind].*firm.ownMat[std_ind,std_ind],dims=2)
    SA = firm.S_j[std_ind]


    P_std[std_ind]= inv(dSdp)*(-SA + cost_std)
    P_RA[std_ind] = inv(dSdp)*(-SA + cost_pl)

    return P_std, P_RA
end

function evaluate_FOC(firm::firmData)

    P_std = zeros(length(firm.P_j))
    P_RA = zeros(length(firm.P_j))

    std_ind = firm.prods


    dSdp = (firm.dSdp_j.*firm.ownMat)[std_ind,std_ind]
    cost_std = sum(firm.dCdp_j.*firm.ownMat,dims=2)[std_ind]
    cost_pl = sum(firm.dCdp_pl_j[std_ind,std_ind].*firm.ownMat[std_ind,std_ind],dims=2)
    SA = firm.S_j[std_ind]


    P_std[std_ind]= inv(dSdp)*(-SA + cost_std)
    P_RA[std_ind] = inv(dSdp)*(-SA + cost_pl)

    return P_std, P_RA
end

function predict_price(firm::firmData,ST::String;sim="Base")

    P_std, P_RA = evaluate_FOC(firm,ST)
    # println(P_std[firm._prodSTDict[ST]])

    if sim=="Base"
        P_new = copy(P_std)
    elseif sim=="RA"
        P_new = copy(P_RA)
    end
    return P_new
end


function foc_error(firm::firmData,ST::String,stp::Float64;sim="Base")

    P_new = predict_price(firm,ST,sim=sim)
    prod_ind = firm._prodSTDict[ST]
    tot_err = (P_new[prod_ind] - firm.P_j[prod_ind])./100
    # println(prod_ind)
    # println(tot_err)
    # println(P_new[prod_ind])

    non_prods = .!(inlist(Int.(1:length(P_new)),prod_ind))

    ### 0 Market Share
    exit_thresh = 1.0
    ProdExit = firm.S_j.< exit_thresh
    choke_point = 1e-3
    ChokePrice = firm.S_j.< choke_point
    P_new[ChokePrice] = min.(firm.P_j[ChokePrice],P_new[ChokePrice])


    if any(ProdExit[prod_ind])
        exited = length(findall(ProdExit[prod_ind]))
        choked = length(findall(ChokePrice[prod_ind]))
        all = length(prod_ind)
        println("Product Exits for $exited out of $all products, $choked at choke price.")
    end

    ### Negative Prices
    P_new[non_prods].=0.0
    P_new[P_new.<0] = 0.5.*firm.P_j[P_new.<0]

    ## Error in Prices
    prod_ind = prod_ind[firm.S_j[prod_ind].>exit_thresh]
    foc_err = (P_new[prod_ind] - firm.P_j[prod_ind])./100


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


    err_new = sum(foc_err.^2)/length(prod_ind)
    tot_err = sum(tot_err.^2)/length(firm._prodSTDict[ST])

    ### New Prices

    P_new[non_prods] = firm.P_j[non_prods]
    P_update = firm.P_j.*(1-stp) + stp.*P_new
    P_update[non_prods] = firm.P_j[non_prods]
    # P_update[P_new.>=MLR_const] = MLR_const[P_new.>MLR_const]
    # Contrain Prices at 0
    #P_update = max.(P_update,0)

    # firm.P_j[:] = P_update[:]

    return foc_err, err_new, tot_err, P_new
end

function solve_model!(m::InsuranceLogit,f::firmData;sim="Base")
    P_res = zeros(length(f.P_j))
    states = sort(String.(keys(f._prodSTDict)))
    for s in states
        println("Solving for $s")
        solve_model_st!(m,f,s,sim=sim)
        P_res[f._prodSTDict[s]] = f.P_j[f._prodSTDict[s]]
    end
    f.P_j[:] = P_res[:]
    return nothing
end

function solve_model_st!(m::InsuranceLogit,f::firmData,ST::String;sim="Base")
    err_new = 1
    err_last = 1
    itr_cnt = 0
    stp = 0.05
    no_prog_cnt = 0
    no_prog = 0
    P_last = zeros(length(f.P_j[:]))
    P_new_last = zeros(length(f.P_j[:]))
    while err_new>1e-12
        itr_cnt+=1
        println("Evaluate Model")
        evaluate_model!(m,f,ST)
        println("Update Price")


        foc_err, err_new, tot_err,P_new = foc_error(f,ST,stp,sim=sim)

        # if (err_last<1e-4) & (err_new>(err_last*2)) & (no_prog==0) #& (no_better==0)
        #     println("Attempt to Find Better Step")
        #     sub_cnt = 0
        #     println("Sub Iteration Count: $sub_cnt, Current Error: $err_new, Step Size: $stp ")
        #     println(f.P_j[f._prodSTDict[ST]])
        #     println(P_new[f._prodSTDict[ST]])
        #     # return P_last, P_new_last
        #
        #     stp = min(stp,.001)
        #     while (err_new>err_last) & (stp>1e-8)
        #         f.P_j[:] = (1-stp).*P_last[:] + stp.*P_new_last[:]
        #         evaluate_model!(m,f,ST)
        #         foc_err, err_new, tot_err,P_new = foc_error(f,ST,stp,sim=sim)
        #         sub_cnt+=1
        #
        #         println("Sub Iteration Count: $sub_cnt, Current Error: $err_new, Step Size: $stp ")
        #
        #         println(f.P_j[f._prodSTDict[ST]])
        #         println(P_new[f._prodSTDict[ST]])
        #         stp/=10
        #     end
        #     if (stp<1e-10)
        #         stp = 1.0
        #     end
        # end

        P_last[:] = copy(f.P_j[:])
        P_new_last[:] = copy(P_new[:])
        f.P_j[:] = (1-stp).*f.P_j[:] + stp.*P_new[:]
        println("Iteration Count: $itr_cnt, Current Error: $err_new, Step Size: $stp, Prog: $no_prog ")
        # println(foc_err)
        # println(P_new[f._prodSTDict[ST]])
        # println(f.P_j[f._prodSTDict[ST]])

        if stp==1.0
            stp = .001
        end
        stp = max(stp,1e-6)
        if stp<.05
            if err_new>1e-3
                stp = stp*2
            else
                stp = stp*1.1
            end
        elseif stp<.25
            stp = stp*(1.1)
        elseif stp<.75
            stp=stp*(1.1)
        end

        if ((err_new>err_last) & (no_prog==0)) | ((err_new<err_last) & (no_prog==1))
            stp = .05
            # if (itr_cnt>100) & (rand()<.2)
            #     stp = 1.0
            # end
        end
        if err_new>err_last
            no_prog = 1
        else
            no_prog=0
        end

        err_last = copy(err_new)
        # println(P_last)
    end
    return nothing
end


function solveMain(m::InsuranceLogit,f::firmData,file::String)
    P_Obs = Vector{Float64}(undef,length(m.prods))
    P_Base = Vector{Float64}(undef,length(m.prods))
    P_RA = Vector{Float64}(undef,length(m.prods))
    P_man = Vector{Float64}(undef,length(m.prods))
    P_RAman = Vector{Float64}(undef,length(m.prods))
    P_Base_m = Vector{Float64}(undef,length(m.prods))
    P_RA_m = Vector{Float64}(undef,length(m.prods))
    P_man_m = Vector{Float64}(undef,length(m.prods))
    P_RAman_m = Vector{Float64}(undef,length(m.prods))

    S_Base = Vector{Float64}(undef,length(m.prods))
    S_RA = Vector{Float64}(undef,length(m.prods))
    S_man = Vector{Float64}(undef,length(m.prods))
    S_RAman = Vector{Float64}(undef,length(m.prods))
    S_Base_m = Vector{Float64}(undef,length(m.prods))
    S_RA_m = Vector{Float64}(undef,length(m.prods))
    S_man_m = Vector{Float64}(undef,length(m.prods))
    S_RAman_m = Vector{Float64}(undef,length(m.prods))

    P_Obs[:] = f.P_j[:]

    #### Solve Baseline - With Risk Adjustment and Mandate ####
    println("####################################")
    println("#### Solve Baseline - With Risk Adjustment and Mandate ####")
    println("####################################")
    solve_model!(m,f,sim="RA")
    P_Base[:] = f.P_j[:]
    evaluate_model!(m,f,"All")
    S_Base[:] = f.S_j[:]

    f.ownMat = f.ownMat_merge
    println("###### Solve Merger Scenario ######")
    solve_model!(m,f,sim="RA")
    P_Base_m[:] = f.P_j[:]
    evaluate_model!(m,f,"All")
    S_Base_m[:] = f.S_j[:]

    #### Solve without Risk Adjustment ####
    println("####################################")
    println("#### Solve without Risk Adjustment ####")
    println("####################################")
    f.P_j[:] = P_Obs[:]
    solve_model!(m,f,sim="Base")
    P_RA[:] = f.P_j[:]
    evaluate_model!(m,f,"All")
    S_RA[:] = f.S_j[:]

    f.ownMat = f.ownMat_merge
    println("###### Solve Merger Scenario ######")
    solve_model!(m,f,sim="Base")
    P_RA_m[:] = f.P_j[:]
    evaluate_model!(m,f,"All")
    S_RA_m[:] = f.S_j[:]

    #### Solve without mandate ####
    println("####################################")
    println("#### Solve without mandate ####")
    println("####################################")
    f.P_j[:] = P_Obs[:]
    f.data[:,f.index[:Mandate]].=0.0
    solve_model!(m,f,sim="RA")
    P_man[:] = f.P_j[:]
    evaluate_model!(m,f,"All")
    S_man[:] = f.S_j[:]

    f.ownMat = f.ownMat_merge
    println("###### Solve Merger Scenario ######")
    solve_model!(m,f,sim="RA")
    P_man_m[:] = f.P_j[:]
    evaluate_model!(m,f,"All")
    S_man_m[:] = f.S_j[:]

    #### Solve without mandate NOR risk adjustment  ####
    println("####################################")
    println("#### Solve without mandate NOR risk adjustment  ####")
    println("####################################")
    f.P_j[:] = P_Obs[:]
    f.data[:,f.index[:Mandate]].=0.0
    solve_model!(m,f,sim="Base")
    P_RAman[:] = f.P_j[:]
    evaluate_model!(m,f,"All")
    S_RAman[:] = f.S_j[:]

    f.ownMat = f.ownMat_merge
    println("###### Solve Merger Scenario ######")
    solve_model!(m,f,sim="Base")
    P_RAman_m[:] = f.P_j[:]
    evaluate_model!(m,f,"All")
    S_RAman_m[:] = f.S_j[:]




    output =  DataFrame(Product=sort(model.prods),
                        Price_data=P_Obs,
                        Price_base=P_Base,
                        Price_RA =P_RA,
                        Price_man=P_man,
                        Price_RAman=P_RAman,
                        Price_base_m=P_Base_m,
                        Price_RA_m =P_RA_m,
                        Price_man_m=P_man_m,
                        Price_RAman_m=P_RAman_m,
                        Lives_base=S_Base,
                        Lives_RA =S_RA,
                        Lives_man=S_man,
                        Lives_RAman=S_RAman,
                        Lives_base_m=S_Base_m,
                        Lives_RA_m =S_RA_m,
                        Lives_man_m=S_man_m,
                        Lives_RAman_m=S_RAman_m)

    CSV.write(file,output)

    return nothing
end
