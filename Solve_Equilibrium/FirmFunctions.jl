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


function evaluate_FOC(firm::firmData)

    P_std = zeros(length(firm.P_j))
    P_RA = zeros(length(firm.P_j))

    std_ind = firm.prods


    dSdp = (firm.dSdp_j.*firm.ownMat)[std_ind,std_ind]
    cost_std = sum(firm.dCdp_j.*firm.ownMat,dims=2)[std_ind]
    cost_pl = sum(firm.dCdp_pl_j.*firm.ownMat,dims=2)[std_ind]
    SA = firm.S_j[std_ind]


    P_std[firm.prods]= inv(dSdp)*(-SA + cost_std)
    P_RA[firm.prods] = inv(dSdp)*(-SA + cost_pl)

    return P_std, P_RA
end

function predict_price(firm::firmData;sim="Base")

    P_std, P_RA = evaluate_FOC(firm)

    if sim=="Base"
        P_new = copy(P_std)
    elseif sim=="RA"
        P_new = copy(P_RA)
    end
    return P_new
end


function update_Prices!(firm::firmData;sim="Base")

    P_new = predict_price(firm,sim=sim)
    tot_err = (P_new[firm.prods] - firm.P_j[firm.prods])./100

    non_prods = .!(inlist(Int.(1:length(P_new)),firm.prods))

    ### 0 Market Share
    ProdExit = firm.S_j.< 1.0
    P_new[ProdExit] = min.(firm.P_j[ProdExit],P_new[ProdExit])


    if any(ProdExit[firm.prods])
        exited = length(findall(ProdExit[firm.prods]))
        println("Product Exits for $exited products")
    end

    ### Negative Prices
    P_new[non_prods].=0.0
    P_new[P_new.<0] = 0.5.*firm.P_j[P_new.<0]

    ## Error in Prices
    prod_ind = firm.prods[firm.S_j[firm.prods].>1.0]
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
    tot_err = sum(tot_err.^2)/length(m.prods)

    ### New Prices
    stp = 0.025
    if err_new>5e-1
        stp = 0.025
    elseif err_new>1e-3
        stp = 0.05
    elseif err_new>1e-6
        stp = 0.1
    elseif err_new>1e-8
        stp = 0.2
    else
        stp = 0.5
    end


    P_update = firm.P_j.*(1-stp) + stp.*P_new
    P_update[non_prods].=0.0
    # P_update[P_new.>=MLR_const] = MLR_const[P_new.>MLR_const]
    # Contrain Prices at 0
    #P_update = max.(P_update,0)

    firm.P_j[:] = P_update[:]

    return foc_err, err_new, tot_err
end


function solve_model!(m::InsuranceLogit,f::firmData;sim="Base")
    err_new = 1
    itr_cnt = 0
    while err_new>1e-10
        itr_cnt+=1
        println("Evaluate Model")
        evaluate_model!(m,f)
        println("Update Price")
        foc_err, err_new, tot_err = update_Prices!(f,sim=sim)
        println("Iteration Count: $itr_cnt, Current Error: $err_new, Total Error: $tot_err ")
    end
    return nothing
end


function solveMain(m::InsuranceLogit,f::firmData)
    P_Obs = Vector{Float64}(undef,length(m.prods))
    P_Base = Vector{Float64}(undef,length(m.prods))
    P_RA = Vector{Float64}(undef,length(m.prods))
    P_man = Vector{Float64}(undef,length(m.prods))
    P_RAman = Vector{Float64}(undef,length(m.prods))
    P_Base_m = Vector{Float64}(undef,length(m.prods))
    P_RA_m = Vector{Float64}(undef,length(m.prods))
    P_man_m = Vector{Float64}(undef,length(m.prods))
    P_RAman_m = Vector{Float64}(undef,length(m.prods))

    P_Obs[:] = f.P_j[:]

    #### Solve Baseline - With Risk Adjustment and Mandate ####
    solve_model!(m,f,sim="RA")
    P_Base[:] = f.P_j[:]
    f.ownMat = f.ownMat_merge
    solve_model!(m,f,sim="RA")
    P_Base_m[:] = f.P_j[:]

    #### Solve without Risk Adjustment ####
    f.P_j[:] = P_Obs[:]
    solve_model!(m,f,sim="Base")
    P_RA[:] = f.P_j[:]
    f.ownMat = f.ownMat_merge
    solve_model!(m,f,sim="Base")
    P_RA_m[:] = f.P_j[:]

    #### Solve without mandate ####
    f.P_j[:] = P_Obs[:]
    f[:Mandate].=0.0
    solve_model!(m,f,sim="RA")
    P_man[:] = f.P_j[:]
    f.ownMat = f.ownMat_merge
    solve_model!(m,f,sim="RA")
    P_man_m[:] = f.P_j[:]

    #### Solve without mandate NOR risk adjustment  ####
    f.P_j[:] = P_Obs[:]
    f[:Mandate].=0.0
    solve_model!(m,f,sim="Base")
    P_man[:] = f.P_j[:]
    f.ownMat = f.ownMat_merge
    solve_model!(m,f,sim="Base")
    P_man_m[:] = f.P_j[:]




    output =  DataFrame(Products=model.prods,
                        Price_base=P_Base,
                        Price_RA =P_RA,
                        Price_man=P_man,
                        Price_RAman=P_RAman,
                        Price_base_m=P_Base_m,
                        Price_RA_m =P_RA_m,
                        Price_man_m=P_man_m,
                        Price_RAman_m=P_RAman_m)
    file = "Estimation_Output/solvedEquilibrium_$rundate.csv"
    CSV.write(file,output)

    return nothing
end
