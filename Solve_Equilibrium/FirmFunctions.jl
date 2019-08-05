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
        prod_ids = f.stdMap[prod_long[idxitr]]
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


        for k in 1:length(prod_ids)
            j = prod_ids[k]
            Revenue[j] += wgt[k]*s_pred[k]*rev[k]
            Cost[j] += wgt[k]*s_pred[k]*cost[k]
            Pooled_Cost[j] += wgt[k]*s_ins*cost_pl[k]
            Market_Total[j] += wgt[k]*s_ins
            Share[j] += wgt[k]*s_pred[k]#*age[k]/mem[k]
            if isnan(Cost[j])
                println(idxitr)
                return
            end
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

function test_MR(m::InsuranceLogit,f::firmData)
    ϵ = 1e-6
    println("First Evaluation")
    evaluate_model!(model,f,"GA",foc_check=false)
    Rev1, Cost1, Share1, PC1, Adj1 = compute_profit(m,f)
    f.P_j[1]+=ϵ
    println("Deviation Evaluation")
    evaluate_model!(model,f,"GA",foc_check=false)
    Rev2, Cost2, Share2, PC2, Adj2 = compute_profit(m,f)

    dR = (Rev2 - Rev1)./ϵ
    dC = (Cost2 - Cost1)./ϵ
    dS = (Share2 - Share1)./ϵ
    dPC = (PC2 - PC1)./ϵ
    dAdj = (Adj2 - Adj1)./ϵ
    return dR, dC, dS, dPC, dAdj, (Cost1,PC1,Cost2,PC2)
end

function prof_margin(f::firmData,std_ind::Union{Vector{Int64},Missing}=missing)
    if ismissing(std_ind)
        std_ind = f.prods
    end
    dSdp = (f.dSAdp_j.*f.ownMat)[std_ind,std_ind]

    MR = inv(dSdp)*sum(f.dRdp_j.*f.ownMat,dims=2)[std_ind]
    Mkup = -inv(dSdp)*f.SA_j[std_ind]
    # MR = -inv(dSdp)*f.SA_j[std_ind]

    cost_std = sum(f.dCdp_j[std_ind,std_ind].*f.ownMat[std_ind,std_ind],dims=2)
    cost_pl = sum(f.dCdp_pl_j[std_ind,std_ind].*f.ownMat[std_ind,std_ind],dims=2)

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

    cost_std = sum(f.dCdp_j[std_ind,std_ind],dims=2)
    cost_pl = sum(f.dCdp_pl_j[std_ind,std_ind],dims=2)

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

function evaluate_FOC(f::firmData,ST::String)

    P_std = zeros(length(f.P_j))
    P_RA = zeros(length(f.P_j))

    std_ind = f._prodSTDict[ST]


    dSdp = (f.dSAdp_j.*f.ownMat)[std_ind,std_ind]
    cost_std = sum(f.dCdp_j[std_ind,std_ind].*f.ownMat[std_ind,std_ind],dims=2)
    cost_pl = sum(f.dCdp_pl_j[std_ind,std_ind].*f.ownMat[std_ind,std_ind],dims=2)
    SA = f.SA_j[std_ind]

    P_std[std_ind]= inv(dSdp)*(-SA + cost_std)
    P_RA[std_ind] = inv(dSdp)*(-SA + cost_pl)

    MR = inv(dSdp)*(-SA)
    MC = inv(dSdp)*(cost_pl)

    return P_std, P_RA, MR, MC
end

function evaluate_FOC(f::firmData)

    P_std = zeros(length(f.P_j))
    P_RA = zeros(length(f.P_j))

    std_ind = f.prods


    dSdp = (f.dSdp_j.*f.ownMat)[std_ind,std_ind]
    cost_std = sum(f.dCdp_j.*f.ownMat,dims=2)[std_ind]
    cost_pl = sum(f.dCdp_pl_j[std_ind,std_ind].*f.ownMat[std_ind,std_ind],dims=2)
    SA = f.S_j[std_ind]


    P_std[std_ind]= inv(dSdp)*(-SA + cost_std)
    P_RA[std_ind] = inv(dSdp)*(-SA + cost_pl)

    return P_std, P_RA
end

function predict_price(f::firmData,ST::String;sim="Base")

    P_std, P_RA = evaluate_FOC(f,ST)
    # println(P_std[f._prodSTDict[ST]])

    if sim=="Base"
        P_new = copy(P_std)
    elseif sim=="RA"
        P_new = copy(P_RA)
    end
    return P_new
end


function foc_error(f::firmData,ST::String,stp::Float64;sim="Base")

    P_new = predict_price(f,ST,sim=sim)
    prod_ind = f._prodSTDict[ST]
    tot_err = (P_new[prod_ind] - f.P_j[prod_ind])./100
    # println(prod_ind)
    # println(tot_err)
    # println(P_new[prod_ind])

    non_prods = .!(inlist(Int.(1:length(P_new)),prod_ind))

    ### 0 Market Share
    exit_thresh = 1.0
    ProdExit = f.S_j.< exit_thresh
    choke_point = 1e-3
    ChokePrice = f.S_j.< choke_point
    P_new[ChokePrice] = min.(f.P_j[ChokePrice],P_new[ChokePrice])


    if any(ProdExit[prod_ind])
        exited = length(findall(ProdExit[prod_ind]))
        choked = length(findall(ChokePrice[prod_ind]))
        all = length(prod_ind)
        # println("Product Exits for $exited out of $all products, $choked at choke price.")
    end

    ### Negative Prices
    P_new[non_prods].=0.0
    P_new[P_new.<0] = 0.5.*f.P_j[P_new.<0]

    ## Error in Prices
    prod_ind = prod_ind[f.S_j[prod_ind].>exit_thresh]
    foc_err = (P_new[prod_ind] - f.P_j[prod_ind])./100


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
    tot_err = sum(tot_err.^2)/length(f._prodSTDict[ST])

    ### New Prices

    P_new[non_prods] = f.P_j[non_prods]
    P_update = f.P_j.*(1-stp) + stp.*P_new
    P_update[non_prods] = f.P_j[non_prods]
    # P_update[P_new.>=MLR_const] = MLR_const[P_new.>MLR_const]
    # Contrain Prices at 0
    #P_update = max.(P_update,0)

    # f.P_j[:] = P_update[:]

    return foc_err, err_new, tot_err, P_new
end

function solve_model!(m::InsuranceLogit,f::firmData;sim="Base")
    P_res = zeros(length(f.P_j))
    states = sort(String.(keys(f._prodSTDict)))#[1:6]
    # states = ["AK","NE","IA"]
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
        # println("Evaluate Model")
        evaluate_model!(m,f,ST)
        # println("Update Price")


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
        # println("Iteration Count: $itr_cnt, Current Error: $err_new, Step Size: $stp, Prog: $no_prog ")
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

    println(median(f[:Mandate]))
    println(median(f.P_j[findall(f.P_j.>0)]))

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

    println(median(f[:Mandate]))
    println(median(f.P_j[findall(f.P_j.>0)]))

    ### Solve without mandate ####
    println("####################################")
    println("#### Solve without mandate ####")
    println("####################################")
    f.P_j[:] = P_Obs[:]
    f.data[:,f.index[:Mandate]].=0.0
    solve_model!(m,f,sim="RA")
    P_man[:] = f.P_j[:]
    evaluate_model!(m,f,"All")
    S_man[:] = f.S_j[:]

    println(median(f[:Mandate]))
    println(median(f.P_j[findall(f.P_j.>0)]))

    f.ownMat = f.ownMat_merge
    println("###### Solve Merger Scenario ######")
    solve_model!(m,f,sim="RA")
    P_man_m[:] = f.P_j[:]
    evaluate_model!(m,f,"All")
    S_man_m[:] = f.S_j[:]

    println(median(f[:Mandate]))
    println(median(f.P_j[findall(f.P_j.>0)]))

    ### Solve without mandate NOR risk adjustment  ####
    println("####################################")
    println("#### Solve without mandate NOR risk adjustment  ####")
    println("####################################")
    f.P_j[:] = P_Obs[:]
    f.data[:,f.index[:Mandate]].=0.0
    solve_model!(m,f,sim="Base")
    P_RAman[:] = f.P_j[:]
    evaluate_model!(m,f,"All")
    S_RAman[:] = f.S_j[:]

    println(median(f[:Mandate]))
    println(median(f.P_j[findall(f.P_j.>0)]))

    f.ownMat = f.ownMat_merge
    println("###### Solve Merger Scenario ######")
    solve_model!(m,f,sim="Base")
    P_RAman_m[:] = f.P_j[:]
    evaluate_model!(m,f,"All")
    S_RAman_m[:] = f.S_j[:]

    println(median(f[:Mandate]))
    println(median(f.P_j[findall(f.P_j.>0)]))




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

function solve_equilibrium(rundate,spec)

    println("Loading Code...")
    codeDir = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Code/"
    # Data Structure
    include("$codeDir/Demand_Estimation/InsChoiceData.jl")
    include("$codeDir/Demand_Estimation/Halton.jl")
    include("$codeDir/Demand_Estimation/RandomCoefficients.jl")
    include("$codeDir/Demand_Estimation/RiskMoments.jl")
    include("$codeDir/Demand_Estimation/utility.jl")
    include("$codeDir/Demand_Estimation/Contraction.jl")
    include("$codeDir/Firm_Side/MC_parameters.jl")
    include("$codeDir/Firm_Side/Firm_Inner_Loop.jl")


    #Equilibrium Functions
    include("EvaluateModel.jl")
    include("PriceUpdate.jl")
    #Load Data
    println("Loading Data...")
    include("EQ_load.jl")

    df[:High_small] = df[:HighRisk].*df[:Small]

    mark_the_output_date = Dates.today()
    println("Running spec $rundate on $mark_the_output_date")

    file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg2_$spec-$rundate.jld2"
    @load file p_stg2 p_dem_est cost_spec spec_Dict
    mc_est = copy(p_stg2)
    #### Load Estimation Results ####


    #### Build Model ####
    println("Rebuild Demand Model...")
    # Structre the data
    chdf = ChoiceData(df,df_mkt,df_risk;
        demoRaw=spec_Dict["demoRaw"],
        prodchars=spec_Dict["prodchars"],
        prodchars_0=spec_Dict["prodchars_0"],
        fixedEffects=spec_Dict["fixedEffects"],
        wgt=[:PERWT])

    # Fit into model
    model = InsuranceLogit(chdf,spec_Dict["haltonDim"])


    if length(p_dem_est)!=model.parLength[:All]
        println(length(p_dem_est))
        println(model.parLength[:All])
        error("Parameter Vector Not Quite Right")
    end

    println("Rebuild Cost Data...")

    costdf = MC_Data(df,mom_firm,mom_metal,mom_age,mom_age_no,mom_risk,mom_ra;
                    baseSpec=cost_spec,
                    fixedEffects=[:Firm_ST],
                    constMoments=true)


    #### Compute Parameter Objects ####
    println("Compute Parameters...")
    par_dem = parDict(model,p_dem_est,no2Der=true)
    individual_values!(model,par_dem)
    individual_shares(model,par_dem)

    par_cost = parMC(mc_est,par_dem,model,costdf)


    #### Solve Equilibrium ####
    firm = firmData(model,df,eq_mkt,par_dem,par_cost)
    println("Check Margins...")
    file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/checkMargins_$spec-$rundate.csv"
    checkMargin(model,firm,file)

    println("Solve Equilibrium...")
    file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/solvedEquilibrium_$spec-$rundate.csv"
    solveMain(model,firm,file)

    return nothing
end
