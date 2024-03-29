function market_profits(d::InsuranceLogit,f::firmData)

    Profit = f.SA_j.*f.P_j .- f.C_j.*f.S_j

    market_profits = Vector{Float64}(undef,length(keys(f.mkt_index)))
    for (m,m_idx) in f.mkt_index
        market_profits[m] = sum(Profit[m_idx])
    end

    return market_profits
end

function state_profits(d::InsuranceLogit,f::firmData;sim="Base")
    prod_profits = product_profits(d,f,sim=sim)
    st_profits = Dict{String,Float64}()
    for (st,prod_ind) in f._prodSTDict
        st_profits[st] = sum(prod_profits[prod_ind])
    end
    return st_profits
end

function product_profits(d::InsuranceLogit,f::firmData;sim="Base")
    J = maximum(d.prods)
    # Revenue = zeros(J)

    # wgts_long = weight(d.data)[:]
    # prod_long = Int.(product(d.data))

    # for idxitr in values(d.data._personDict)
    #     prod_ids =prod_long[idxitr]

    #     s_pred = f.s_pred[idxitr]
    #     rev = f.Rev_ij[idxitr]
    #     wgt = wgts_long[idxitr]


    #     for k in 1:length(prod_ids)
    #         j = prod_ids[k]
    #         Revenue[j] += wgt[k]*s_pred[k]*rev[k]
    #     end
    # end

    if sim=="RA"
        Profit = f.SA_j.*f.P_j - f.C_j.*f.S_j
    elseif sim=="Base"
        Profit = f.SA_j.*f.P_j - f.PC_j.*f.S_j
    end
    Profit[isnan.(Profit)].=0.0
    return Profit
end

function market_transfers(d::InsuranceLogit,f::firmData)

    market_profits = Vector{Float64}(undef,length(keys(f.mkt_index)))
    for (m,m_idx) in f.mkt_index
        market_profits[m] = sum(f.S_j[m_idx].*(f.PC_j[m_idx].-f.C_j[m_idx]))
    end

    return market_profits
end

function solve_SP_λ!(m::InsuranceLogit,f::firmData,Π_target::Vector{Float64};
    sim="SPλ",merg::String="SP",CW_target::Vector{Float64}=Vector{Float64}(undef,0),
    markets=Vector{Int}(undef,0))
    P_res = zeros(length(f.P_j))
    if length(markets) == 0
        markets = sort(Int.(keys(f.mkt_index)))
    end
    λ_vec = zeros(length(Int.(keys(f.mkt_index))))
    for mkt in markets
            println("Solving for $mkt")
            println("Profit Target: $(Π_target[mkt])")
            λ = find_λ(m,f,mkt,Π_target[mkt],sim=sim)
            # println("Got λ = $λ for market $mkt")
            λ_vec[mkt] = λ
            # solve_model_mkt!(m,f,mkt,λ=λ,sim=sim,merg=merg)
            # P_res[f.mkt_index[mkt],i] = f.P_j[f.mkt_index[mkt]]
            # println(f.P_j[f.mkt_index[mkt]])
            # profits = market_profits(m,f)
            # mkt_prof = profits[mkt]
            # println("Profits are $mkt_prof")

            # Output Consumer Welfare
            # if length(CW_target)>0
            #     cw = calc_cw_mkt(m,f,mkt)
            #     dcw = cw-CW_target[mkt]
            #     println("Mean Consumer Welfare: $cw")
            #     println("Improvement in Mean Consumer Welfare: $dcw")
            # end
    end
    return markets, λ_vec
end

function solve_SP_λ_parallel!(m::InsuranceLogit,f::firmData,Π_target::Vector{Float64};
                sim="SPλ",merg::String="SP",
                CW_target::Vector{Float64}=Vector{Float64}(undef,0),
                markets=Vector{Int}(undef,0),
                tol::Float64=1e-12,voucher=false,update_voucher=true)

    println("Send Data to Workers")
    @eval @everywhere m=$m
    @eval @everywhere f=$f
    @eval @everywhere Π_target=$Π_target
    println("Data Distributed")

    # @everywhere println("Mean Vouchers: $(mean(f.subsidy_ij_voucher))")
    # @everywhere evaluate_model!(m,f,"All",voucher=true,update_voucher=false)
    if length(markets) == 0
        println("All Markets")
        markets = sort(Int.(keys(f.mkt_index)))
        @everywhere markets = sort(Int.(keys(f.mkt_index)))
    else
        @eval @everywhere markets = $markets
    end
    P_res = SharedArray{Float64}(length(f.P_j))
    λ_vec = SharedArray{Float64}(maximum(markets))
    @sync @distributed for mkt in markets
        println("Solving for $mkt")
        # println("Profit Target: $(Π_target[mkt])")
        # profits = market_profits(m,f)

        λ = find_λ(m,f,mkt,Π_target[mkt])
        # println("Got λ = $λ for market $mkt")
        λ_vec[mkt] = λ
        # solve_model_mkt!(m,f,mkt,λ=λ,sim=sim,merg=merg)
        P_res[f.mkt_index[mkt]] = f.P_j[f.mkt_index[mkt]]
        # println(f.P_j[f.mkt_index[mkt]])
        # profits = market_profits(m,f)
        # mkt_prof = profits[mkt]
        # println("Profits are $mkt_prof")

        # Output Consumer Welfare
        # if length(CW_target)>0
        #     cw = calc_cw_mkt(m,f,mkt)
        #     dcw = cw-CW_target[mkt]
        #     println("Mean Consumer Welfare: $cw")
        #     println("Improvement in Mean Consumer Welfare: $dcw")
        # end
    end
    f.P_j[:] = P_res[:]
    println("Remove Data from Workers")
    @eval @everywhere Π_target=nothing
    return markets, λ_vec
end



function solve_SP_λ_parallel_st!(m::InsuranceLogit, f::firmData, Π_target::Dict{String,Float64};
    sim="SPλ", merg::String="SP",
    CW_target::Vector{Float64}=Vector{Float64}(undef, 0),
    states=Vector{String}(undef, 0),
    tol::Float64=1e-12, voucher=false, update_voucher=true)

    println("Send Data to Workers")
    @eval @everywhere m = $m
    @eval @everywhere f = $f
    @eval @everywhere Π_target = $Π_target
    println("Data Distributed")

    if length(states) == 0
        println("All Markets")
        @everywhere states = sort(String.(keys(f._prodSTDict)))
    else
        @eval @everywhere states = $states
    end
    P_res = SharedArray{Float64}(length(f.P_j))
    λ_vec = SharedArray{Float64}(length(states))
    @sync @distributed for st in states
        println("Solving for $st")


        λ = find_λ_st(m, f, st, Π_target[st])

        P_res[f.mkt_index[mkt]] = f.P_j[f.mkt_index[mkt]]

    end
    f.P_j[:] = P_res[:]
    println("Remove Data from Workers")
    @eval @everywhere Π_target = nothing
    return states, λ_vec
end

function solve_SP_λ_st!(m::InsuranceLogit, f::firmData, Π_target::Dict{String,Float64};
    sim="SPλ", merg::String="SP",
    CW_target::Vector{Float64}=Vector{Float64}(undef, 0),
    states=Vector{String}(undef, 0),
    tol::Float64=1e-12, voucher=false, update_voucher=true)

    if length(states) == 0
        println("All Markets")
        states = sort(String.(keys(f._prodSTDict)))
    end
    P_res = Vector{Float64}(undef,length(f.P_j))
    λ_vec = Vector{Float64}(undef,length(states))
    for st in states
        println("Solving for $st")
        λ = find_λ_st(m, f, st, Π_target[st])
        P_res[f.mkt_index[mkt]] = f.P_j[f.mkt_index[mkt]]

    end
    f.P_j[:] = P_res[:]
    Π_target = nothing
    return states, λ_vec
end

function find_λ(m::InsuranceLogit,f::firmData,mkt::Int,
    Π_target::Float64;λ_min = 0.0,λ_max = 1.0,sim="SPλ")
    err = 1e4
    cnt = 0
    λ_err = 1.0
    tol = 1e-6
    λ_err = 0.5
    Π_old = 0
    λ_old = 1.0
    intcpt = 1.0
    slope = 1.0
    λ_new = 0.0
    Π_min = -1e8
    Π_max = 1e8
    bottom_half_flag=false
    p_init = copy(f.P_j[:])
    while (err>tol)
        cnt+=1
        sec_step = (Π_target-intcpt)/slope
        if cnt==1
            λ_new = 1.0
        elseif cnt==2
            λ_new = 0.5
        elseif (cnt==3) & (bottom_half_flag)
            λ_new = 0.0
        elseif (sec_step>λ_min) & (sec_step<λ_max)
            # println("Secant Step")
            λ_new = sec_step
        else
            # println("Bisection Step")
            λ_new = (λ_max-λ_min)/2 + λ_min
        end

        f.P_j[:] = p_init[:]
        solve_model_mkt!(m,f,mkt,λ=λ_new,sim=sim,merg="SP",tol=tol,voucher=true,update_voucher=false)
        # println("Price vector ($λ_new): $(f.P_j[f.mkt_index[mkt]])")
        profits = market_profits(m,f)
        Π_new = profits[mkt]
        if (cnt==2) & (Π_new>Π_target)
            bottom_half_flag=true
        end

        if (Π_new>Π_target) #| (cnt==2)
            λ_max = copy(λ_new)
            Π_max = copy(Π_new)
        elseif (Π_new<=Π_target) #| (cnt==1)
            λ_min = copy(λ_new)
            Π_min = copy(Π_new)
        end
        ## Secant Step
        if cnt>=2
            if (cnt%2 == 0)
                slope = (Π_new - Π_max)/(λ_new-λ_max)
            else
                slope = (Π_new - Π_min)/(λ_new - λ_min)
            end
            intcpt = Π_new - slope*λ_new
        end

        if cnt>2
            λ_err = λ_max - λ_min
        else
            λ_err = 1.0
        end
        λ_old = copy(λ_new)
        Π_old = copy(Π_new)
        Π_err = abs(Π_new - Π_target)
        err = Π_err/Π_target
        # println("Got Profit $Π_new at iteration $cnt with λ=$λ_new, target $Π_target, bounds = ($λ_max,$λ_min)")
        if (Π_new<Π_target) & (λ_new==1.0)
            break
        elseif (Π_new>Π_target) & (λ_new==0.0)
            break
        end

        if ((λ_max - λ_min)<1e-13)
            println("λ value converged")
            println("Got Profit $Π_new with λ=$λ_new, err $err, bounds = ($λ_max,$λ_min)")
            break
        end
        # cw = calc_cw_mkt(m,f,mkt)
        # println(" Mean CW in Mkt: $cw")

    end

    println("Got λ = $λ_new in Market $mkt on Iteration $cnt, Π error: $err")
    return λ_new
end

function find_λ_st(m::InsuranceLogit,f::firmData,ST::String,
    Π_target::Float64;λ_min = 0.0,λ_max = 1.0,sim="SPλ")
    err = 1e4
    cnt = 0
    λ_err = 1.0
    tol = 1e-6
    λ_err = 0.5
    Π_old = 0
    λ_old = 1.0
    intcpt = 1.0
    slope = 1.0
    λ_new = 0.0
    Π_min = -1e8
    Π_max = 1e8
    bottom_half_flag=false
    p_init = copy(f.P_j[:])
    while (err>tol)
        cnt+=1
        sec_step = (Π_target-intcpt)/slope
        if cnt==1
            λ_new = 1.0
        elseif cnt==2
            λ_new = 0.5
        elseif (cnt==3) & (bottom_half_flag)
            λ_new = 0.0
        elseif (sec_step>λ_min) & (sec_step<λ_max)
            # println("Secant Step")
            λ_new = sec_step
        else
            # println("Bisection Step")
            λ_new = (λ_max-λ_min)/2 + λ_min
        end

        f.P_j[:] = p_init[:]
        solve_model_st!(m,f,ST,λ_new,sim=sim,merg="SP",tol=tol,voucher=true,update_voucher=false)

        profits = state_profits(m,f)
        Π_new = profits[ST]
        if (cnt==2) & (Π_new>Π_target)
            bottom_half_flag=true
        end

        if (Π_new>Π_target) #| (cnt==2)
            λ_max = copy(λ_new)
            Π_max = copy(Π_new)
        elseif (Π_new<=Π_target) #| (cnt==1)
            λ_min = copy(λ_new)
            Π_min = copy(Π_new)
        end
        ## Secant Step
        if cnt>=2
            if (cnt%2 == 0)
                slope = (Π_new - Π_max)/(λ_new-λ_max)
            else
                slope = (Π_new - Π_min)/(λ_new - λ_min)
            end
            intcpt = Π_new - slope*λ_new
        end

        if cnt>2
            λ_err = λ_max - λ_min
        else
            λ_err = 1.0
        end
        λ_old = copy(λ_new)
        Π_old = copy(Π_new)
        Π_err = abs(Π_new - Π_target)
        err = Π_err/Π_target
        println("Got Profit $Π_new at iteration $cnt with λ=$λ_new, target $Π_target")


    end

    println("Got λ = $λ_new in State $ST on Iteration $cnt, Π error: $err")
    return λ_new
end

function solve_SP!(m::InsuranceLogit,f::firmData;
                sim="SP",merg::String="SP",tol::Float64=1e-12,voucher=false,update_voucher=true)
    # P_res = zeros(length(f.P_j))
    markets = sort(Int.(keys(f.mkt_index)))
    for mkt in markets
        # if mkt<49
        #     continue
        # end
        println("Solving for $mkt")
        solve_model_mkt!(m,f,mkt,sim=sim,merg=merg,tol=tol,voucher=voucher,update_voucher=update_voucher)
        # P_res[f._prodSTDict[s]] = f.P_j[f._prodSTDict[s]]
    end
    # f.P_j[:] = P_res[:]
    return nothing
end

function solve_SP!(m::InsuranceLogit,f::firmData,markets::Vector{Int};
    sim="SP",merg::String="SP",tol::Float64=1e-12,voucher=false,update_voucher=true)
# P_res = zeros(length(f.P_j))
# markets = sort(Int.(keys(f.mkt_index)))
for mkt in markets
# if mkt<49
#     continue
# end
println("Solving for $mkt")
solve_model_mkt!(m,f,mkt,sim=sim,merg=merg,tol=tol,voucher=voucher,update_voucher=update_voucher)
# P_res[f._prodSTDict[s]] = f.P_j[f._prodSTDict[s]]
end
# f.P_j[:] = P_res[:]
return nothing
end


function solve_SP_parallel!(m::InsuranceLogit,f::firmData;
                sim="SP",merg::String="SP",tol::Float64=1e-12,markets=Vector{Int}(undef,0),voucher=false,update_voucher=true)
    println("Send Data to Workers")
    @eval @everywhere m=$m
    @eval @everywhere f=$f
    @eval @everywhere sim_SP_run=$sim
    @eval @everywhere merg_SP_run=$merg
    @eval @everywhere tol_SP_run=$tol
    @eval @everywhere voucher_SP_run=$voucher
    @eval @everywhere update_voucher_SP_run=$update_voucher
    println("Data Distributed")

    if length(markets) == 0
        println("All Markets")
        markets = sort(Int.(keys(f.mkt_index)))
        @everywhere markets = sort(Int.(keys(f.mkt_index)))
    else
        @eval @everywhere markets = $markets
    end

    P_res = SharedArray{Float64}(length(f.P_j))
    println("Parameters: voucher: $voucher_SP_run, update_voucher: $update_voucher_SP_run,  sim: $sim_SP_run")
    @sync @distributed for mkt in markets
        # println("Solving for $mkt")
        println("Parameters: voucher: $voucher_SP_run, update_voucher: $update_voucher_SP_run,  sim: $sim_SP_run")
        solve_model_mkt!(m,f,mkt,sim=sim_SP_run,merg=merg_SP_run,tol=tol_SP_run,voucher=voucher_SP_run,update_voucher=update_voucher_SP_run)
        println("Solved $(mkt)!")
        P_res[f.mkt_index[mkt]] = f.P_j[f.mkt_index[mkt]]
    end
    println("Remove Data from Workers")
    @eval @everywhere sim_SP_run=nothing
    @eval @everywhere merg_SP_run=nothing
    @eval @everywhere tol_SP_run=nothing
    @eval @everywhere voucher_SP_run=nothing
    @eval @everywhere update_voucher_SP_run=nothing
    f.P_j[:] = P_res[:]
    return nothing
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
        foc_err, err_new, tot_err,P_new = foc_error(f,mkt,λ=λ,sim=sim,merg=merg,voucher=voucher)


        P_last[:] = copy(f.P_j[:])
        P_new_last[:] = copy(P_new[:])
        update = stp.*(P_new[:] .- f.P_j[:])
        # update[abs.(update).>50].=50 .*sign.(update[abs.(update).>50])
        f.P_j[:] = f.P_j[:] .+ update[:]
        # println("Iteration Count: $itr_cnt, Current Error: $err_new, Step Size: $stp, Prog: $no_prog ")
        # println(foc_err)
        # println(P_new[f.mkt_index[mkt]])
        # println(f.P_j[f.mkt_index[mkt]])

        if stp==1.0
            stp = .001
        end
        stp = max(stp,1e-6)
        if stp<.05
            if err_new>1e-3
                stp = stp*1.1
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


function solve_model_st!(m::InsuranceLogit, f::firmData, ST::String,λ::Float64;
    sim="Base", merg::String="Base", tol::Float64=1e-12, voucher=false, update_voucher=true, no_policy=false)
    err_new = 1
    err_last = 1
    itr_cnt = 0
    stp = 0.05
    no_prog_cnt = 0
    no_prog = 0
    P_last = zeros(length(f.P_j[:]))
    P_new_last = zeros(length(f.P_j[:]))
    P_orig = copy(f.P_j[:])


    while (err_new > tol) & (!isnan(err_new))
        itr_cnt += 1
        evaluate_model!(m, f, ST, voucher=voucher, update_voucher=update_voucher, no_policy=no_policy)
        foc_err, err_new, tot_err,P_new = foc_error(f,ST,λ=λ,sim=sim,merg=merg,voucher=voucher)


        P_last[:] = copy(f.P_j[:])
        P_new_last[:] = copy(P_new[:])
        f.P_j[:] = (1 - stp) .* f.P_j[:] + stp .* P_new[:]
 
        ### If really no one buys, then we can't invert the derivative matrix
        inversion_stopgap = 1e-6
        problem_product = f.S_j .< inversion_stopgap
        if any(problem_product[f._prodSTDict[ST]])
            f.P_j[problem_product] = min.(0.5 * P_last[problem_product], f.P_j[problem_product])
            println("POTENTIAL INVERSION ISSUE: $(findall(problem_product[f._prodSTDict[ST]]))")
        end

        if stp == 1.0
            stp = 0.001
        end
        stp = max(stp, 1e-6)
        if stp < 0.05
            if err_new > 1e-3
                stp = stp * 2
            else
                stp = stp * 1.1
            end
        elseif stp < 0.25
            stp = stp * (1.1)
        elseif stp < 0.75
            stp = stp * (1.1)
        end

        if ((err_new > err_last) & (no_prog == 0)) | ((err_new < err_last) & (no_prog == 1))
            stp = 0.05
        end
        if err_new > err_last
            no_prog = 1
        else
            no_prog = 0
        end

        err_last = copy(err_new)
    end
    return nothing
end