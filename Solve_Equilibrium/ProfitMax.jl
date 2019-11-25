# function prod_quants(d::InsuranceLogit,f::firmData,mkt::Int)
#     Revenue = zeros(length(d.prods))
#     Cost = zeros(length(d.prods))
#     Share = zeros(length(d.prods))
#     SA = zeros(length(d.prods))
#
#     Market_Total = zeros(length(d.prods))
#
#     wgts_long = weight(d.data)[:]
#     prod_long = Int.(product(d.data))
#     age_long = f[:ageRate]
#     mem_long = f[:MEMBERS]
#
#     for idxitr in values(d.data._personDict)
#         prod_ids = f.stdMap[prod_long[idxitr]]
#         catas = findall(inlist(prod_ids,f.catas_prods))
#
#         s_pred = f.s_pred[idxitr]
#         cost = f.c_pred[idxitr]
#         rev = f.Rev_ij[idxitr]
#         wgt = wgts_long[idxitr]
#         age = age_long[idxitr]
#         mem = mem_long[idxitr]
#
#
#         for k in 1:length(prod_ids)
#             j = prod_ids[k]
#             Share[j] += wgt[k]*s_pred[k]
#             SA[j] += wgt[k]*s_pred[k]#*age[k]/mem[k]
#             Revenue[j] += wgt[k]*s_pred[k]*rev[k]
#             Cost[j] += wgt[k]*s_pred[k]*cost[k]
#         end
#     end
#
#     Profit = Revenue - Cost
#     market_profits = sum(Profit[f.mkt_index[mkt]])
#
#     return market_profits, Profit[f.mkt_index[mkt]], SA[f.mkt_index[mkt]], Cost[f.mkt_index[mkt]]
# end
#
# function prod_prof(x::Vector{T},d::InsuranceLogit,f::firmData,mkt::Int) where T
#     f.P_j[f.mkt_index[mkt]] = x
#     evaluate_model!(m,f,mkt,voucher=true,deriv=false)
#     total_prof, prof, S, C = prod_quants(d,f,mkt)
#     return prof
# end
#
# function prod_share(x::Vector{T},d::InsuranceLogit,f::firmData,mkt::Int) where T
#     f.P_j[f.mkt_index[mkt]] = x
#     evaluate_model!(m,f,mkt,voucher=true,deriv=false)
#     total_prof, prof, S, C = prod_quants(d,f,mkt)
#     p2 = f.P_j[2]
#     s2 = s.S_j[2]
#     println("Variation ($p2,$s2)")
#     return S
# end
#
# function prod_cost(x::Vector{T},d::InsuranceLogit,f::firmData,mkt::Int) where T
#     f.P_j[f.mkt_index[mkt]] = x
#     evaluate_model!(m,f,mkt,voucher=true,deriv=false)
#     total_prof, prof, S, C = prod_quants(d,f,mkt)
#     return C
# end

function test_derivs(d::InsuranceLogit,f::firmData,mkt::Int)
    prod_index = f.mkt_index[mkt]
    dProf = Vector{Float64}(undef,length(prod_index))
    dShr = Matrix{Float64}(undef,length(prod_index),length(prod_index))
    dCost = Matrix{Float64}(undef,length(prod_index),length(prod_index))

    for (i,p) in enumerate(prod_index)
        println("Calc Derivative ($i,$p)")
        dPi, dC,dS,dPC,dAdj = test_MR(d,f,p,mkt)
        dProf[i] = dPi
        dShr[i,:] = dS[prod_index]
        dCost[i,:] = dC[prod_index]
    end
    evaluate_model!(d,f,mkt,voucher=true)
    return dProf, dShr, dCost
end

function eval_profit(P::Vector{Float64},m::InsuranceLogit,f::firmData,mkt::Int)
    f.P_j[f.mkt_index[mkt]] = P[:]
    evaluate_model!(m,f,mkt,voucher=true,deriv=false)
    all_profits = market_profits(m,f)
    prof = all_profits[mkt]
    return prof
end


function prof_max(x::Vector{Float64},m::InsuranceLogit,f::firmData,mkt::Int)
    # Set up the optimization
    opt = Opt(:LN_NELDERMEAD, length(x))
    ftol_rel!(opt,1e-8)
    xtol_rel!(opt,1e-8)
    maxtime!(opt, 500000)


    func(x) = eval_profit(x,m,f,mkt)
    disp_length = min(20,length(x))
    count = 0
    function func(x, grad)
        count +=1
        x_displ = x[1:disp_length]
        println("Iteration $count at $x_displ")
        obj = func(x)
        println("Objective equals $obj on iteration $count")

        return obj
    end

    # Set Objective
    max_objective!(opt, func)

    # Run Optimization
    minf, minx, ret = optimize(opt, x)
    println("Got $minf at $minx after $count iterations (returned $ret)")

    # Return the object
    return ret, minf, minx
end


# flag, prof, P_max = prof_max(f.P_j[f.mkt_index[5]],m,f,5)
# P_max = [195.368, 198.353, 439.458, 320.481, 1232.89, 382.198, 215.248, 206.039, 512.504, 387.248, 334.986, 166.323, 186.568, 239.776, 879.183, 223.494, 238.865, 233.276, 414.424, 462.758, 217.09, 357.553, 263.843, 226.534, 381.22, 777.601, 397.313]
# prof = 2.950476350671173e6


function eval_profit(P::Vector{Float64},m::InsuranceLogit,f::firmData,mkt::Int)
    f.P_j[f.mkt_index[mkt]] = P[:]
    evaluate_model!(m,f,mkt,voucher=true,deriv=false)
    all_profits = market_profits(m,f)
    prof = all_profits[mkt]
    return prof
end
