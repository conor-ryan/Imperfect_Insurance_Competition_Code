function solve_model!(m::InsuranceLogit,f::firmData;
                sim="Base",merg::String="Base",tol::Float64=1e-12,voucher=false)
    P_res = zeros(length(f.P_j))
    states = sort(String.(keys(f._prodSTDict)))#[1:6]
    # states = ["AK","NE","IA"]
    for s in states #[1:2]
        # if s!="AK"
        #     continue
        # end
        println("Solving for $s")
        solve_model_st!(m,f,s,sim=sim,merg=merg,tol=tol,voucher=voucher)
        P_res[f._prodSTDict[s]] = f.P_j[f._prodSTDict[s]]
    end
    f.P_j[:] = P_res[:]
    return nothing
end

function solve_model_st!(m::InsuranceLogit,f::firmData,ST::String;
                sim="Base",merg::String="Base",tol::Float64=1e-12,voucher=false)
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
        evaluate_model!(m,f,ST,voucher=voucher)
        # println("Update Price")



        foc_err, err_new, tot_err,P_new = foc_error(f,ST,stp,sim=sim,merg=merg,voucher=voucher)


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
    println("Solved at Iteration Count: $itr_cnt, Error: $err_new")
    benchmarks = f.bench_prods[f._prodSTDict[ST]]
    benchmarks = benchmarks[benchmarks.>0]
    println("Benchmark Products: $benchmarks")
    return nothing
end


function solveMain(m::InsuranceLogit,f::firmData,file::String)
    J = maximum(m.prods)
    P_Obs = Vector{Float64}(undef,J)

    P_SP = Vector{Float64}(undef,J)
    P_SP_zp = Vector{Float64}(undef,J)
    P_SP_cp = Vector{Float64}(undef,J)
    P_SP_cpm = Vector{Float64}(undef,J)
    P_Base = Vector{Float64}(undef,J)
    P_RA = Vector{Float64}(undef,J)
    P_man = Vector{Float64}(undef,J)
    P_RAman = Vector{Float64}(undef,J)
    P_Base_m = Vector{Float64}(undef,J)
    P_RA_m = Vector{Float64}(undef,J)
    P_man_m = Vector{Float64}(undef,J)
    P_RAman_m = Vector{Float64}(undef,J)

    S_SP = Vector{Float64}(undef,J)
    S_SP_zp = Vector{Float64}(undef,J)
    S_SP_cp = Vector{Float64}(undef,J)
    S_SP_cpm = Vector{Float64}(undef,J)
    S_Base = Vector{Float64}(undef,J)
    S_RA = Vector{Float64}(undef,J)
    S_man = Vector{Float64}(undef,J)
    S_RAman = Vector{Float64}(undef,J)
    S_Base_m = Vector{Float64}(undef,J)
    S_RA_m = Vector{Float64}(undef,J)
    S_man_m = Vector{Float64}(undef,J)
    S_RAman_m = Vector{Float64}(undef,J)

    P_Obs[:] = f.P_j[:]

    #### Solve Baseline - With Risk Adjustment and Mandate ####
    println("####################################")
    println("#### Solve Baseline - With Risk Adjustment and Mandate ####")
    println("####################################")
    f.P_j[:] = P_Obs[:]
    evaluate_model!(m,f,"All")
    set_voucher!(f)

    solve_model!(m,f,sim="RA",voucher=false)
    P_Base[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=false)
    S_Base[:] = f.S_j[:]
    # solve_model!(m,f,sim="RA",voucher=false)

    base_profits = market_profits(m,f)
    base_welfare = consumer_welfare_bymkt(m,f,"Base")

    consumer_welfare(m,f,"Base")

    println("###### Solve Merger Scenario ######")
    # f.P_j[:] = P_Obs[:]
    solve_model!(m,f,sim="RA",merg="Merger",voucher=true)
    P_Base_m[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true)
    S_Base_m[:] = f.S_j[:]

    merger_profits = market_profits(m,f)

    consumer_welfare(m,f,"Base_m")

    # ## Monopolist...
    # # f.P_j[:] = P_Obs[:]
    solve_model!(m,f,sim="RA",merg="SP",voucher=true)
    evaluate_model!(m,f,"All",voucher=true)
    monopoly_profits = market_profits(m,f)
    P_mon = Vector{Float64}(undef,J)
    P_mon = f.P_j[:]
    S_mon = Vector{Float64}(undef,J)
    S_mon = f.S_j[:]
    #
    # P_max = [195.368, 198.353, 439.458, 320.481, 1232.89, 382.198, 215.248, 206.039, 512.504, 387.248, 334.986, 166.323, 186.568, 239.776, 879.183, 223.494, 238.865, 233.276, 414.424, 462.758, 217.09, 357.553, 263.843, 226.534, 381.22, 777.601, 397.313]
    # flag, prof_ng, P_max = prof_max(P_max,m,f,5)
    #
    #
    # # f.P_j[:] = P_Obs[:]
    # solve_model!(m,f,sim="Base",merg="SP")
    # evaluate_model!(m,f,"All")
    # monopoly_profits2 = market_profits(m,f)
    #
    #
    # markets_cpm, λ_vec_cpm = solve_SP_λ!(m,f,monopoly_profits)

    #### Solve Planner Problem ####
    println("####################################")
    println("#### Solve Social Planner Problem ####")
    println("####################################")
    solve_model!(m,f,sim="SP",merg="SP",voucher=true)
    P_SP[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true)
    S_SP[:] = f.S_j[:]

    sp = consumer_welfare_bymkt(m,f,"SP")

    consumer_welfare(m,f,"SP")

    println("#### ZERO PROFIT PROBLEM ####")
    markets_zp, λ_vec_zp = solve_SP_λ!(m,f,zeros(length(base_profits)))
    P_SP_zp[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true)
    S_SP_zp[:] = f.S_j[:]
    sp = consumer_welfare_bymkt(m,f,"SP_zp")
    # output =  DataFrame(Markets=markets,
    #                     lambdas = λ_vec)
    #
    # CSV.write(file,output)
    consumer_welfare(m,f,"SP_zp")

    println("#### CURRENT PROFIT PROBLEM ####")
    markets_cp, λ_vec_cp = solve_SP_λ!(m,f,base_profits,CW_target=base_welfare)
    P_SP_cp[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true)
    S_SP_cp[:] = f.S_j[:]

    sp_cp_welfare = consumer_welfare_bymkt(m,f,"SP_cp")

    consumer_welfare(m,f,"SP_cp")

    println("#### MERGER PROFIT PROBLEM ####")
    markets_cpm, λ_vec_cpm = solve_SP_λ!(m,f,merger_profits)
    P_SP_cpm[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true)
    S_SP_cpm[:] = f.S_j[:]

    consumer_welfare(m,f,"SP_cpm")

    #### Solve without Risk Adjustment ####
    println("####################################")
    println("#### Solve without Risk Adjustment ####")
    println("####################################")
    f.P_j[:] = P_Obs[:]
    solve_model!(m,f,sim="Base",voucher=true)
    P_RA[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true)
    S_RA[:] = f.S_j[:]

    consumer_welfare(m,f,"RA")

    println("###### Solve Merger Scenario ######")
    solve_model!(m,f,sim="Base",merg="Merger",voucher=true)
    P_RA_m[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true)
    S_RA_m[:] = f.S_j[:]

    consumer_welfare(m,f,"RA_m")


    ### Solve without mandate ####
    println("####################################")
    println("#### Solve without mandate ####")
    println("####################################")
    f.P_j[:] = P_Obs[:]
    f.data[:,f.index[:Mandate]].=0.0
    solve_model!(m,f,sim="RA",voucher=true)
    P_man[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true)
    S_man[:] = f.S_j[:]

    consumer_welfare(m,f,"man")

    # println(median(f[:Mandate]))
    # println(median(f.P_j[findall(f.P_j.>0)]))

    println("###### Solve Merger Scenario ######")
    solve_model!(m,f,sim="RA",merg="Merger",voucher=true)
    P_man_m[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true)
    S_man_m[:] = f.S_j[:]

    consumer_welfare(m,f,"man_m")


    ### Solve without mandate NOR risk adjustment  ####
    println("####################################")
    println("#### Solve without mandate NOR risk adjustment  ####")
    println("####################################")
    f.P_j[:] = P_Obs[:]
    f.data[:,f.index[:Mandate]].=0.0
    solve_model!(m,f,sim="Base",voucher=true)
    P_RAman[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true)
    S_RAman[:] = f.S_j[:]

    consumer_welfare(m,f,"RAman")


    println("###### Solve Merger Scenario ######")
    solve_model!(m,f,sim="Base",merg="Merger",voucher=true)
    P_RAman_m[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true)
    S_RAman_m[:] = f.S_j[:]

    consumer_welfare(m,f,"RAman_m")



    output =  DataFrame(Product=sort(m.prods),
                        Price_data=P_Obs,
                        Price_sp=P_SP,
                        Price_sp_zp=P_SP_zp,
                        Price_sp_cp=P_SP_cp,
                        Price_base=P_Base,
                        Price_RA =P_RA,
                        Price_man=P_man,
                        Price_RAman=P_RAman,
                        Price_base_m=P_Base_m,
                        Price_RA_m =P_RA_m,
                        Price_man_m=P_man_m,
                        Price_RAman_m=P_RAman_m,
                        Lives_sp=S_SP,
                        Lives_sp_zp=S_SP_zp,
                        Lives_sp_cp=S_SP_cp,
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
    #Load Data
    println("Loading Data...")
    include("EQ_load.jl")

    # df[:High_small] = df[:HighRisk].*df[:Small]

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
        product =[:Product_std],
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

    #### Print Costs Moments
    println("Print Cost Moments...")
    moments,targets = costMoments(costdf,model,mc_est,par_dem,print_moments=true)
    file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/costMoments_$spec-$rundate.csv"
    output =  DataFrame(estimated_moments=moments,
                        targeted_moments = targets)
    CSV.write(file,output)

    #### Solve Equilibrium ####
    firm = firmData(model,df,eq_mkt,par_dem,par_cost)
    evaluate_model!(model,firm,"All",foc_check=true)
    println("Check Margins...")
    file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/checkMargins_$spec-$rundate.csv"
    checkMargin(model,firm,file)

    println("Solve Equilibrium...")
    file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/solvedEquilibrium_$spec-$rundate.csv"
    solveMain(model,firm,file)

    return nothing
end
