function market_profits(d::InsuranceLogit,f::firmData)
    J = maximum(d.prods)
    Revenue = zeros(J)
    Cost = zeros(J)
    Share = zeros(J)

    Market_Total = zeros(J)

    wgts_long = weight(d.data)[:]
    prod_long = Int.(product(d.data))

    for idxitr in values(d.data._personDict)
        # prod_ids = f.stdMap[prod_long[idxitr]]
        prod_ids =prod_long[idxitr]
        catas = findall(inlist(prod_ids,f.catas_prods))

        s_pred = f.s_pred[idxitr]
        cost = f.c_pred[idxitr]
        rev = f.Rev_ij[idxitr]
        wgt = wgts_long[idxitr]


        for k in 1:length(prod_ids)
            j = prod_ids[k]
            Revenue[j] += wgt[k]*s_pred[k]*rev[k]
            Cost[j] += wgt[k]*s_pred[k]*cost[k]
        end
    end

    Profit = Revenue - Cost

    market_profits = Vector{Float64}(undef,length(keys(f.mkt_index)))
    for (m,m_idx) in f.mkt_index
        market_profits[m] = sum(Profit[m_idx])
    end

    return market_profits
end

function market_transfers(d::InsuranceLogit,f::firmData)
    J = maximum(d.prods)
    Cost_pl = zeros(J)
    Cost = zeros(J)
    Share = zeros(J)

    Market_Total = zeros(J)

    wgts_long = weight(d.data)[:]
    prod_long = Int.(product(d.data))

    for idxitr in values(d.data._personDict)
        # prod_ids = f.stdMap[prod_long[idxitr]]
        prod_ids =prod_long[idxitr]
        catas = findall(inlist(prod_ids,f.catas_prods))

        s_pred = f.s_pred[idxitr]
        cost = f.c_pred[idxitr]
        cost_pl = f.c_pool[idxitr]
        wgt = wgts_long[idxitr]


        for k in 1:length(prod_ids)
            j = prod_ids[k]
            Cost_pl[j] += wgt[k]*s_pred[k]*cost_pl[k]
            Cost[j] += wgt[k]*s_pred[k]*cost[k]
        end
    end

    Profit =  Cost - Cost_pl

    market_profits = Vector{Float64}(undef,length(keys(f.mkt_index)))
    for (m,m_idx) in f.mkt_index
        market_profits[m] = sum(Profit[m_idx])
    end

    return market_profits
end

function solve_SP_λ!(m::InsuranceLogit,f::firmData,Π_target::Vector{Float64};
    sim="SPλ",merg::String="SP",CW_target::Vector{Float64}=Vector{Float64}(undef,0))
    P_res = zeros(length(f.P_j))
    markets = sort(Int.(keys(f.mkt_index)))
    λ_vec = zeros(length(markets))
    for mkt in markets
            println("Solving for $mkt")
            println("Profit Target: $(Π_target[mkt])")
            λ = find_λ(m,f,mkt,Π_target[mkt])
            println("Got λ = $λ for market $mkt")
            λ_vec[mkt] = λ
            # solve_model_mkt!(m,f,mkt,λ=λ,sim=sim,merg=merg)
            # P_res[f.mkt_index[mkt],i] = f.P_j[f.mkt_index[mkt]]
            # println(f.P_j[f.mkt_index[mkt]])
            # profits = market_profits(m,f)
            # mkt_prof = profits[mkt]
            # println("Profits are $mkt_prof")

            # Output Consumer Welfare
            if length(CW_target)>0
                cw = calc_cw_mkt(m,f,mkt)
                dcw = cw-CW_target[mkt]
                println("Mean Consumer Welfare: $cw")
                println("Improvement in Mean Consumer Welfare: $dcw")
            end
    end
    return markets, λ_vec
end

function find_λ(m::InsuranceLogit,f::firmData,mkt::Int,
    Π_target::Float64;λ_min = 0.0,λ_max = 1.0)
    err = 1e4
    cnt = 0
    λ_err = 1.0
    tol = 1e-3
    λ_err = 0.5
    Π_old = 0
    λ_old = 1.0
    intcpt = 1.0
    slope = 1.0
    λ_new = 0.0
    while (λ_err>1e-3) & (err>1)
        cnt+=1
        sec_step = (Π_target-intcpt)/slope
        if cnt==1
            λ_new = 1.0
        elseif (sec_step>λ_min) & (sec_step<λ_max)
            # println("Secant Step")
            λ_new = sec_step
        else
            # println("Bisection Step")
            λ_new = (λ_max-λ_min)/2 + λ_min
        end

        println("Trying λ: $λ_new, $λ_err")
        if λ_err >.1
            tol = 1e-12
        else
            tol = 1e-12
        end
        solve_model_mkt!(m,f,mkt,λ=λ_new,sim="SPλ",merg="SP",tol=tol,voucher=true,update_voucher=false)
        # println(f.P_j[f.mkt_index[mkt]])
        profits = market_profits(m,f)
        Π_new = profits[mkt]
        if Π_new>Π_target
            λ_max = copy(λ_new)
            Π_max = copy(Π_new)
        else
            λ_min = copy(λ_new)
        end
        ## Secant Step
        slope = (Π_new - Π_old)/(λ_new-λ_old)
        intcpt = Π_new - slope*λ_new
        λ_old = copy(λ_new)
        Π_old = copy(Π_new)
        err = abs(Π_new - Π_target)
        λ_err = λ_max - λ_min
        println("Got Profit $Π_new at iteration $cnt, error $err")

        # cw = calc_cw_mkt(m,f,1)
        # println(" Mean CW in Mkt 1: $cw")

    end

    println("Iteration $cnt, Π error: $err, λ error: $λ_err")
    return λ_new
end


function solve_model_mkt!(m::InsuranceLogit,f::firmData,mkt::Int;
        λ::Float64=0.0,sim="Base",merg::String="Base",tol::Float64=1e-12,voucher=true,update_voucher=false)
    err_new = 1
    err_last = 1
    itr_cnt = 0
    stp = 0.05
    no_prog_cnt = 0
    no_prog = 0
    P_last = zeros(length(f.P_j[:]))
    P_new_last = zeros(length(f.P_j[:]))
    while err_new>tol
        itr_cnt+=1
        # println("Evaluate Model")
        evaluate_model!(m,f,mkt,voucher=voucher,update_voucher=update_voucher)
        # println("Update Price")


        foc_err, err_new, tot_err,P_new = foc_error(f,mkt,stp,λ=λ,sim=sim,merg=merg,voucher=voucher)


        P_last[:] = copy(f.P_j[:])
        P_new_last[:] = copy(P_new[:])
        f.P_j[:] = (1-stp).*f.P_j[:] + stp.*P_new[:]
        # println("Iteration Count: $itr_cnt, Current Error: $err_new, Step Size: $stp, Prog: $no_prog ")
        # println(foc_err)
        # println(P_new[f.mkt_index[mkt]])
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
    # println("Solved at Iteration Count: $itr_cnt, Error: $err_new")
    return nothing
end
