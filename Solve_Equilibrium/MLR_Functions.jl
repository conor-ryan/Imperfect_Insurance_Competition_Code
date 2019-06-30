function MLR_Constraint_Run(P_new::Vector{Float64},
                            Markup::Vector{Float64},
                            margCost::Vector{Float64},
                            Transfer::Vector{Float64},
                            e::EqData)
    ind_const = MLR_check(P_new,Transfer,e)
    if length(ind_const)==0
        return P_new
    end
    ### Constrained Firms ###
    f_const = unique(e.firms[ind_const])
    println("Firms are MLR Constrained: $f_const")
    for f in f_const
        mlr = MLR_value(P_new,Transfer,f,e)
        P_const_f = MLR_Const_FOC(f,Markup,margCost,Transfer,e)
        adj = P_const_f[:] - P_new[e.firms.==f]
        println("Price Reduction to Meet Constraint: $adj")
        P_new[e.firms.==f] = P_const_f[:]
    end
    return P_new
end

function MLR_check(P_new::Vector{Float64},Transfer::Vector{Float64},e::EqData)
    Cost_j = e[:C] - Transfer
    A_j = e[:ageRate_avg]
    Rev_j = P_new.*A_j
    L_j = e[:lives]

    Cost_f = e.ownMat*(Cost_j.*L_j)
    Rev_f  = e.ownMat*(Rev_j.*L_j)
    MLR = Cost_f./Rev_f

    ind_constrained = findall(MLR.<0.5)
    return ind_constrained
end

function MLR_value(P::Vector{Float64},Transfer::Vector{Float64},f::Int64,e::EqData)
    Cost_j = e[:C] - Transfer
    A_j = e[:ageRate_avg]
    Rev_j = P.*A_j
    L_j = e[:lives]

    Cost_f = sum((Cost_j.*L_j)[e.firms.==f])
    Rev_f  = sum((Rev_j.*L_j)[e.firms.==f])
    mlr = Cost_f./Rev_f
    return mlr
end

function MLR_Const_FOC(f::Int64,Markup::Vector{Float64},margCost::Vector{Float64},Transfer::Vector{Float64},e::EqData)
    L_j = e[:lives]
    A_j = e[:ageRate_avg]
    Cost_j = e[:C] - Transfer

    LA_j = L_j.*A_j
    ind_firm = e.firms.==f

    C_f = sum( (Cost_j.*L_j)[ind_firm] )
    Mkup_f = sum( (Markup.*LA_j)[ind_firm] )
    MC_f = sum( (margCost.*LA_j)[ind_firm] )
    λ = ((1/0.5)*C_f - Mkup_f)/MC_f

    P = Markup[ind_firm] + margCost[ind_firm]*λ

    return P
end
