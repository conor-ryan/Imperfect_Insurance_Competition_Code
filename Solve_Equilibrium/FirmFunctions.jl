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

    Adj = (firm.poolMat_merge*Cost)./(firm.poolMat_merge*Pooled_Cost)
    Pooled_Cost = Pooled_Cost.*Adj

    Rev_Firm = firm.ownMat*Revenue
    Cost_Firm = firm.ownMat*Cost
    PC_Firm = firm.ownMat*Pooled_Cost
    Profit = Rev_Firm - Cost_Firm
    # MLR = Cost_Firm./Rev_Firm
    return Profit, Cost, Share, Pooled_Cost
end

function test_MR(m::InsuranceLogit,firm::firmData)
    ϵ = 1e-6
    println("First Evaluation")
    evaluate_model!(m,firm)
    Rev1, Cost1, Share1, PC1 = compute_profit(m,firm)
    firm.P_j[57]+=ϵ
    println("Deviation Evaluation")
    evaluate_model!(m,firm)
    Rev2, Cost2, Share2, PC2 = compute_profit(m,firm)

    dR = (Rev2 - Rev1)./ϵ
    dC = (Cost2 - Cost1)./ϵ
    dS = (Share2 - Share1)./ϵ
    dPC = (PC2 - PC1)./ϵ
    return dR, dC, dS, dPC
end

function prof_margin(firm::firmData)
    MR = sum(firm.dRdp_j.*firm.ownMat,dims=2)
    MC_std = sum(firm.dCdp_j.*firm.ownMat,dims=2)
    MC_RA = sum(firm.dCdp_pl_j.*firm.ownMat,dims=2)
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
