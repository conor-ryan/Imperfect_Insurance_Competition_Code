function predict_price(foc_Std::Vector{Float64},
                        foc_RA::Vector{Float64},
                        foc_RA_fix::Vector{Float64},
                        foc_merge::Vector{Float64},
                        S_m::Vector{Float64},
                        dsdp_rev::Matrix{Float64},
                        e::EqData;sim="Base")
    J = length(foc_Std)
    P_new = Vector{Float64}(J)

    if sim=="Base"
        foc = foc_Std + foc_RA

    elseif sim=="No Transfer"
        foc = foc_Std
    elseif sim=="Half Transfer"
        foc = foc_Std + 0.5.*foc_RA
    elseif sim=="Fixed Transfer"
        foc = foc_Std + foc_RA_fix
    elseif sim=="Merger"
        foc = foc_merge
    else
        error("Missspecified First Order Condition")
    end


    for (m,m_idx) in e.mkt_index
        L_m = S_m[m_idx][1]
        if sim=="Merger"
            m_dsdp_rev = dsdp_rev[m_idx,m_idx].*e.ownMat_merge[m_idx,m_idx]
        else
            m_dsdp_rev = dsdp_rev[m_idx,m_idx].*e.ownMat[m_idx,m_idx]
        end
        P_new[m_idx] = -inv(L_m*m_dsdp_rev)*(foc[m_idx])
    end
    return P_new
end



function update_Prices!(P_new::Vector{Float64},
                                e::EqData)

    ### 0 Market Share
    ProdExit = (e[:S_j].<(1e-5) )
    P_new[ProdExit] = min.(e.premBase_j[ProdExit],P_new[ProdExit])


    if any(ProdExit)
        exited = find(ProdExit)
        println("Product Exits for $exited")
    end

    ### Negative Prices
    P_new[P_new.<0] = 0.5.*e.premBase_j[P_new.<0]

    ## Error in Prices
    foc_err = (P_new - e.premBase_j)./100

    ### MLR Constraint
    MLR_const = e[:C]./0.7
    constrained_bool = (P_new.>=MLR_const).& (e[:S_j].>=(1e-5))
    if any( constrained_bool )
        constrained = find( constrained_bool )
        println("Hit MLR Constraint at products $constrained")
        P_const = MLR_const[constrained]
        println("Constrained prices: $P_const")
        foc_err[constrained] = 0.0
    end
    #P_new = min.(P_new,MLR_const)


    err_new = sum(foc_err.^2)/sum(foc_err.!=0.0)

    ### New Prices
    step = 0.05
    if err_new>1e-1
        step = 0.05
    elseif err_new>1e-3
        step = 0.25
    elseif err_new>1e-6
        step = 0.5
    end


    P_update = e.premBase_j.*(1-step) + step.*P_new
    P_update[P_new.>=MLR_const] = MLR_const[P_new.>MLR_const]
    # Contrain Prices at 0
    #P_update = max.(P_update,0)

    e.premBase_j = P_update

    return foc_err, err_new
end

function solve_model!(e::EqData,tol::Float64=.5;sim="Base")
    err = 10
    cnt = 0
    P_low = similar(e.premBase_j)
    # Initialize Model
    evaluate_model!(e,init=true)
    while (err>tol) & (cnt<1000)
        cnt+=1
        evaluate_model!(e)
        #foc_err, P_new = eval_FOC(e)
        #foc_err = update_Prices!(foc_err,P_new,e)
        # foc_Std, foc_RA, foc_RA_fix, foc_merge,S_m, dsdp_rev = eval_FOC(e)
        # P_new = predict_price(foc_Std,foc_RA,foc_RA_fix,foc_merge,S_m,dsdp_rev,
        #                         e,sim=sim)
        P_Std, P_RA, P_RAτ, τ = eval_FOC(e)
        if sim=="Base"
            P_new = P_Std
        elseif sim=="RA"
            P_new = P_RA
        elseif sim=="RAτ"
            P_new = P_RAτ .+ τ
        else
            error("Missspecified First Order Condition")
        end

        foc_err, err_new = update_Prices!(P_new,e)


        println("Error is $err at iteration $cnt")
        P = e.premBase_j
        println("Prices are $P")

        if err_new<err
            P_low[:] = P[:]
        end
        err = err_new
    end
    if cnt==1000
        e.premBase_j = P_low
    end

    println("Model solved with error $err after $cnt iterations")
    return Void
end


function run_st_equil(st::String)
    cd("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/")
    println("Read in Data for $st")
    file1 = "Intermediate_Output/Equilibrium_Data/estimated_Data_$st.csv"
    df = CSV.read(file1,types=Dict("AGE"=>Float64,"Mandate"=>Float64,"MEMBERS"=>Float64,"Gamma_j"=>Union{Missing,Float64}),null="NA")
    file2 = "Intermediate_Output/Equilibrium_Data/estimated_prodData_$st.csv"
    df_mkt = CSV.read(file2)#,null="NA")
    #cost_pars = CSV.read("Intermediate_Output/Equilibrium_Data/cost_pars.csv",null="NA")

    # Solve Model
    println("Build Model")
    c = ChoiceData(df,df_mkt)

    model = EqData(c,df_mkt)

    # Initialize Price Vectors
    P_base = Vector{Float64}(length(model.prods))
    P_RA = Vector{Float64}(length(model.prods))
    P_RAτ = Vector{Float64}(length(model.prods))
    P_mandate = Vector{Float64}(length(model.prods))


    println("Estimate Base Model")
    solve_model!(model,1e-8)
    P_base[:] = model.premBase_j[:]

    println("Risk Adjustment Model")
    solve_model!(model,1e-8,sim="RA")
    P_RA[:] = model.premBase_j[:]

    println("Risk Adjustment τ Model")
    solve_model!(model,1e-8,sim="RAτ")
    P_RAτ[:] = model.premBase_j[:]

    println("Estimate Increased Mandate")
    model.data[:Mandate] = model.data[:Mandate] + 1000
    solve_model!(model,1e-8)
    P_mandate[:] = model.premBase_j[:]
    #
    # println("Estimate Half Transfers")
    # solve_model!(model,1e-3,sim="Half Transfer")
    # P_half[:] = model.premBase_j[:]
    #

    println("Solved: $st")

    output =  DataFrame(Products=model.prods,
                        Price_base=P_base,
                        Price_RA =P_RA,
                        Price_RA_p = P_RAτ,
                        Price_man = P_mandate)
                        # ,
                        # Price_non =P_non,
                        # Price_half=P_half)


    file3 = "Estimation_Output/solvedEquilibrium_RA_$st.csv"
    CSV.write(file3,output)
    return Void
end



function Check_Margin(st::String)
    cd("$(homedir())/Documents/Research/Imperfect_Insurance_Competition/")
    println("Read in Data for $st")
    file1 = "Intermediate_Output/Equilibrium_Data/estimated_Data_$st.csv"
    df = CSV.read(file1,types=Dict("AGE"=>Float64,"Mandate"=>Float64,"MEMBERS"=>Float64,"Gamma_j"=>Union{Missing,Float64}),null="NA")
    file2 = "Intermediate_Output/Equilibrium_Data/estimated_prodData_$st.csv"
    df_mkt = CSV.read(file2,null="NA")
    # cost_pars = CSV.read("Intermediate_Output/Equilibrium_Data/cost_pars.csv",null="NA")

    # Solve Model
    println("Build Model")
    c = ChoiceData(df,df_mkt)

    model = EqData(c,df_mkt)

    evaluate_model!(model,init=true)
    # foc_Std, foc_RA, foc_RA_fix, S_m, dsdp_rev = eval_FOC(model)
    P_Std, P_RA, P_RAτ, τ = eval_FOC(model)
    # P_new = predict_price(foc_Std,foc_RA,foc_RA_fix,S_m,dsdp_rev,
    #                         model,sim="Base")






    println("Solved: $st")

    output =  DataFrame(Products=model.prods,
                        Price_orig=model.premBase_j,
                        Price_Std =P_Std,
                        Price_RA = P_RA)


    file3 = "Estimation_Output/focMargin_$st.csv"
    CSV.write(file3,output)
    return Void
end
