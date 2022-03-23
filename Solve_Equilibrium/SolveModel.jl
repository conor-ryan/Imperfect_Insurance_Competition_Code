function solve_model!(m::InsuranceLogit,f::firmData;
                sim="Base",merg::String="Base",tol::Float64=1e-8,voucher=false,update_voucher=true,no_policy=false)
    # P_res = zeros(length(f.P_j))
    states = sort(String.(keys(f._prodSTDict)))#[1:6]
    # states = ["GA"]
    for s in states
        if s!="MI"
            continue
        end
        println("Solving for $s")
        solve_model_st!(m,f,s,sim=sim,merg=merg,tol=tol,voucher=voucher,update_voucher=update_voucher,no_policy=no_policy)
        # P_res[f._prodSTDict[s]] = f.P_j[f._prodSTDict[s]]
    end
    # f.P_j[:] = P_res[:]
    return nothing
end

function solve_model_st!(m::InsuranceLogit,f::firmData,ST::String;
                sim="Base",merg::String="Base",tol::Float64=1e-8,voucher=false,update_voucher=true,no_policy=false)
    err_new = 1
    tot_err = 1
    itr_cnt = 0
    stp = 0.000001
    no_prog_cnt = 0
    no_prog = 0
    P_last = zeros(length(f.P_j[:]))
    P_new_last = zeros(length(f.P_j[:]))
    P_orig = zeros(length(f.P_j[:]))
    P_orig[:] = f.P_j[:]
    ## Initialize Step Vector
    prod_ind = f._prodSTDict[ST]
    stp_vec = zeros(length(f.P_j[:]))
    stp_vec[prod_ind].=0.01

    dProf_last = zeros(length(f.P_j[:]))

    # println(f.P_j[f._prodSTDict[ST]])
    while (tot_err>tol) & !(isnan(tot_err))
        itr_cnt+=1
        # println("Evaluate Model")
        evaluate_model!(m,f,ST,voucher=voucher,update_voucher=update_voucher,no_policy=no_policy)
        # println("Update Price")



         tot_err, dProf = foc_error(f,prod_ind,stp,sim=sim,merg=merg,voucher=voucher)

         price_update = stp_vec.*dProf[:]
         # Cap update at 50 dollars
         update_sign = price_update
         price_update[abs.(price_update).>50].= 50 .*(sign.(price_update[abs.(price_update).>50]))

        f.P_j[:] = f.P_j[:] .+ price_update
        println("Iteration Count: $itr_cnt, Current Error: $tot_err")
        println("Prices: $(round.(f.P_j[prod_ind]))")
        println("Step Vector: $(round.(stp_vec[prod_ind],digits=4))")
        println("Profit Derivatives: $(dProf[prod_ind])")

        # println(foc_err)
        # println(f.P_j[f._prodSTDict[ST]])
        # println(f.P_j[f._prodSTDict[ST]])

        high_prices = f.P_j[prod_ind].>1e3
        # println("High Prices")
        # println("High Prices at indices: $(findall(high_prices))")
        # println("Prices: $(f.P_j[prod_ind][high_prices])")
        # println("Price Update: $(price_update[prod_ind][high_prices])")
        # # println("Lives: $(f.S_j[prod_ind][high_prices])")
        # println("Profit Derivatives: $(dProf[prod_ind][high_prices])")

        large_change = abs.(price_update[prod_ind]).>=10
        # println("Big Update")
        # println("High Prices at indices: $(findall(large_change))")
        # println("Prices: $(f.P_j[prod_ind][large_change])")
        # println("Price Update: $(price_update[prod_ind][large_change])")
        # # println("Lives: $(f.S_j[prod_ind][high_prices])")
        # println("Profit Derivatives: $(dProf[prod_ind][large_change])")

        stp_vec = stp_vec.*1.2
        stp_vec[abs.(price_update).>40] = stp_vec[abs.(price_update).>40]./1.2
        flipped_sign = ((dProf.>0) .& (dProf_last.<0)) .| ((dProf.<0) .& (dProf_last.>0))
        println("Flipped products: $(findall(flipped_sign))")
        println("Flipped steps: $(stp_vec[flipped_sign])")
        println("Flipped dProf: $(round.(dProf[flipped_sign]))")

        stp_vec[flipped_sign].=stp
        stp_vec[prod_ind][(dProf_last[prod_ind].==0.0)].=stp

        if tot_err>100
            stp_vec[stp_vec.>.1].=.1
        end



        dProf_last = copy(dProf)
        #
        #
        # if stp==1.0
        #     stp = .001
        # end
        # stp = max(stp,1e-6)
        # if stp<.05
        #     if err_new>1e-3
        #         stp = stp*2
        #     else
        #         stp = stp*1.1
        #     end
        # elseif stp<.25
        #     stp = stp*(1.1)
        # elseif stp<.75
        #     stp=stp*(1.1)
        # end
        #
        # if ((err_new>err_last) & (no_prog==0)) | ((err_new<err_last) & (no_prog==1))
        #     stp = .05
        #     # if (itr_cnt>100) & (rand()<.2)
        #     #     stp = 1.0
        #     # end
        # end
        # if err_new>err_last
        #     no_prog = 1
        # else
        #     no_prog=0
        # end

        # err_last = copy(err_new)
        # println(P_last)
    end
    println("Solved at Iteration Count: $itr_cnt, Error: $tot_err")
    benchmarks = f.bench_prods[f._prodSTDict[ST]]
    benchmarks = benchmarks[benchmarks.>0]
    println("Benchmark Products: $benchmarks")
    benchmarks = f.bench_prods[f._prodSTDict[ST]]
    silver = f.P_j[f._prodSTDict[ST]]
    silver = silver[benchmarks.>0]
    println("Silver Premiums: $silver")

    obs_silver = P_orig[f._prodSTDict[ST]]
    obs_silver = obs_silver[benchmarks.>0]
    println("Observed(Previous) Silver Premiums: $obs_silver")
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
    chdf = ChoiceData(df,df_mkt,df_risk,df_transfer;
        product =[:Product_std],
        demoRaw=spec_Dict["demoRaw"],
        prodchars=spec_Dict["prodchars"],
        prodchars_σ=spec_Dict["prodchars_σ"],
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

    println("Consumer Welfare at Observed Prices")
    evaluate_model!(model,firm,"All",voucher=true)
    consumer_welfare(model,firm,"obs")
    trash = total_welfare_bymkt(model,firm,"obs",update_voucher=true)

    println("Solve Equilibrium...")
    file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/solvedEquilibrium_$spec-$rundate.csv"
    solveMain(model,firm,file)


    println("Solve Social Welfare Decomposition...")
    firm = firmData(model,df,eq_mkt,par_dem,par_cost)
    evaluate_model!(model,firm,"All",foc_check=true)
    file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/solvedEquilibrium_Welfare_$spec-$rundate.csv"
    solveMain_SP(model,firm,file)

    println("Solve Social Welfare Decomposition, Robustness Check...")
    firm = firmData(model,df,eq_mkt,par_dem,par_cost)
    evaluate_model!(model,firm,"All",foc_check=true)
    file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/solvedEquilibrium_WelfareGov_$spec-$rundate.csv"
    solveMain_SP_Gov(model,firm,file)

    println("Solve Price-Linked Equilibrium, Robustness...")
    firm = firmData(model,df,eq_mkt,par_dem,par_cost)
    evaluate_model!(model,firm,"All",foc_check=true)
    file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Estimation_Output/solvedEquilibrium_PL_$spec-$rundate.csv"
    solveMain_PL(model,firm,file)

    return nothing
end


function solveMain(m::InsuranceLogit,f::firmData,file::String)
    J = maximum(m.prods)
    P_Obs = zeros(J)
    prod_vec = zeros(J)
    prod_vec[sort(m.prods)] = sort(m.prods)
    println(length(prod_vec))
    println(length(prod_vec[prod_vec.>0]))
    println(maximum(prod_vec))

    P_Base = zeros(J)
    P_RA = zeros(J)
    P_man = zeros(J)
    P_RAman = zeros(J)
    P_Base_m = zeros(J)
    P_RA_m = zeros(J)
    P_man_m = zeros(J)
    P_RAman_m = zeros(J)

    S_Base = zeros(J)
    S_RA = zeros(J)
    S_man = zeros(J)
    S_RAman = zeros(J)
    S_Base_m = zeros(J)
    S_RA_m = zeros(J)
    S_man_m = zeros(J)
    S_RAman_m = zeros(J)

    P_Obs[:] = f.P_j[:]

    #### Solve Baseline - With Risk Adjustment and Mandate ####
    println("####################################")
    println("#### Solve Baseline - With Risk Adjustment and Mandate ####")
    println("####################################")
    f.P_j[:] = P_Obs[:]
    solve_model!(m,f,sim="Base",voucher=true)
    P_Base[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true)
    S_Base[:] = f.S_j[:]
    # set_voucher!(f,refund=false)

    base_profits = market_profits(m,f)
    base_welfare = total_welfare_bymkt(m,f,"Base",update_voucher=true)
    consumer_welfare(m,f,"Base")

    println("###### Solve Merger Scenario ######")
    # f.P_j[:] = P_Obs[:]
    solve_model!(m,f,sim="Base",merg="Merger",voucher=true,update_voucher=true)
    P_Base_m[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=true)
    S_Base_m[:] = f.S_j[:]

    merger_base_profits = market_profits(m,f)
    consumer_welfare(m,f,"Base_m")
    trash = total_welfare_bymkt(m,f,"Base_m",update_voucher=true)


    #### Solve without Risk Adjustment ####
    println("####################################")
    println("#### Solve without Risk Adjustment ####")
    println("####################################")
    f.P_j[:] = P_Obs[:]
    solve_model!(m,f,sim="RA",voucher=true,update_voucher=true)
    P_RA[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=true)
    S_RA[:] = f.S_j[:]
    # set_voucher!(f,refund=false)

    consumer_welfare(m,f,"RA")
    RA_welfare = total_welfare_bymkt(m,f,"RA",update_voucher=true)

    println("###### Solve Merger Scenario ######")
    solve_model!(m,f,sim="RA",merg="Merger",voucher=true,update_voucher=true)
    P_RA_m[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=true)
    S_RA_m[:] = f.S_j[:]

    consumer_welfare(m,f,"RA_m")
    trash = total_welfare_bymkt(m,f,"RA_m",update_voucher=true)


    ### Solve without mandate ####
    println("####################################")
    println("#### Solve without mandate ####")
    println("####################################")
    f.P_j[:] = P_Obs[:]
    f.data[:,f.index[:Mandate]].=0.0
    solve_model!(m,f,sim="Base",voucher=true,update_voucher=true)
    P_man[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=true)
    S_man[:] = f.S_j[:]

    consumer_welfare(m,f,"man")
    trash = total_welfare_bymkt(m,f,"man",update_voucher=true)

    # println(median(f[:Mandate]))
    # println(median(f.P_j[findall(f.P_j.>0)]))

    println("###### Solve Merger Scenario ######")
    solve_model!(m,f,sim="Base",merg="Merger",voucher=true,update_voucher=true)
    P_man_m[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=true)
    S_man_m[:] = f.S_j[:]

    consumer_welfare(m,f,"man_m")
    trash = total_welfare_bymkt(m,f,"man_m",update_voucher=true)

    ### Solve without mandate NOR risk adjustment  ####
    println("####################################")
    println("#### Solve without mandate NOR risk adjustment  ####")
    println("####################################")
    f.P_j[:] = P_Obs[:]
    f.data[:,f.index[:Mandate]].=0.0
    solve_model!(m,f,sim="RA",voucher=true,update_voucher=true)
    P_RAman[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=true)
    S_RAman[:] = f.S_j[:]

    consumer_welfare(m,f,"RAman")
    trash = total_welfare_bymkt(m,f,"RAman",update_voucher=true)


    println("###### Solve Merger Scenario ######")
    solve_model!(m,f,sim="RA",merg="Merger",voucher=true,update_voucher=true)
    P_RAman_m[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=true)
    S_RAman_m[:] = f.S_j[:]

    consumer_welfare(m,f,"RAman_m")
    trash = total_welfare_bymkt(m,f,"RAman_m",update_voucher=true)


    output =  DataFrame(Product=prod_vec,
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


function solveMain_SP(m::InsuranceLogit,f::firmData,file::String)
    J = maximum(m.prods)
    P_Obs = zeros(J)
    prod_vec = zeros(J)
    prod_vec[sort(m.prods)] = sort(m.prods)
    println(length(prod_vec))
    println(length(prod_vec[prod_vec.>0]))
    println(maximum(prod_vec))

    P_SP_cp_base = zeros(J)
    P_SP_cpm_base = zeros(J)
    P_SP = zeros(J)
    P_SP_zp = zeros(J)
    P_SP_cp = zeros(J)
    P_SP_cpm = zeros(J)
    P_Base = zeros(J)
    P_RA = zeros(J)
    P_Base_m = zeros(J)
    P_RA_m = zeros(J)

    S_SP_cp_base = zeros(J)
    S_SP_cpm_base = zeros(J)
    S_SP = zeros(J)
    S_SP_zp = zeros(J)
    S_SP_cp = zeros(J)
    S_SP_cpm = zeros(J)
    S_Base = zeros(J)
    S_RA = zeros(J)
    S_Base_m = zeros(J)
    S_RA_m = zeros(J)

    P_Obs[:] = f.P_j[:]

    #### Solve Baseline - With Risk Adjustment and Mandate ####
    println("####################################")
    println("#### Solve Baseline - With Risk Adjustment and Mandate ####")
    println("####################################")
    f.P_j[:] = P_Obs[:]
    solve_model!(m,f,sim="Base",voucher=true)
    evaluate_model!(m,f,"All",voucher=true)
    set_voucher!(f,refund=true)

    solve_model!(m,f,sim="Base",voucher=true,update_voucher=false)
    P_Base[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=false)
    S_Base[:] = f.S_j[:]

    base_profits = market_profits(m,f)
    base_welfare = total_welfare_bymkt(m,f,"Base_vch",update_voucher=false)
    consumer_welfare(m,f,"Base_vch")

    println("###### Solve Merger Scenario ######")
    # f.P_j[:] = P_Obs[:]
    solve_model!(m,f,sim="Base",merg="Merger",voucher=true,update_voucher=false)
    P_Base_m[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=false)
    S_Base_m[:] = f.S_j[:]

    merger_base_profits = market_profits(m,f)
    consumer_welfare(m,f,"Base_m_vch")
    trash = total_welfare_bymkt(m,f,"Base_m_vch",update_voucher=false)

    ## Solve Planner Problem ####
    println("####################################")
    println("#### Solve Social Planner Problem ####")
    println("####################################")
    f.P_j[:] .= P_Obs[:]
    solve_SP!(m,f,sim="SP",merg="SP",voucher=true,update_voucher=false)
    P_SP[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=false)
    S_SP[:] = f.S_j[:]

    consumer_welfare(m,f,"SP")
    trash = total_welfare_bymkt(m,f,"SP",update_voucher=false)

    println("#### ZERO PROFIT PROBLEM ####")
    f.P_j[:] = P_SP[:]
    markets_zp, λ_vec_zp = solve_SP_λ!(m,f,zeros(length(base_profits)),sim="SPλ")
    P_SP_zp[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=false)
    S_SP_zp[:] = f.S_j[:]

    consumer_welfare(m,f,"SP_zp")
    trash = total_welfare_bymkt(m,f,"SP_zp",update_voucher=false)

    println("#### CURRENT PROFIT PROBLEM ####")
    f.P_j[:] = P_SP[:]
    markets_cp, λ_vec_cp = solve_SP_λ!(m,f,base_profits)
    P_SP_cp_base[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=false)
    S_SP_cp_base[:] = f.S_j[:]


    consumer_welfare(m,f,"SP_cp_base")
    trash = total_welfare_bymkt(m,f,"SP_cp_base",update_voucher=false)

    println("#### MERGER PROFIT PROBLEM ####")
    f.P_j[:] = P_SP[:]
    markets_cpm, λ_vec_cpm = solve_SP_λ!(m,f,merger_base_profits,markets=[4;5;6;7;8;9;10;11;12;13;14])
    P_SP_cpm_base[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=false)
    S_SP_cpm_base[:] = f.S_j[:]

    consumer_welfare(m,f,"SP_cpm_base")
    trash = total_welfare_bymkt(m,f,"SP_cpm_base",update_voucher=false)

    #### Solve without Risk Adjustment ####
    println("####################################")
    println("#### Solve without Risk Adjustment ####")
    println("####################################")
    f.P_j[:] = P_Obs[:]
    solve_model!(m,f,sim="RA",voucher=true,update_voucher=false)
    P_RA[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=false)
    S_RA[:] = f.S_j[:]

    RA_profits = market_profits(m,f)

    consumer_welfare(m,f,"RA_vch")
    RA_welfare = total_welfare_bymkt(m,f,"RA_vch",update_voucher=false)

    println("###### Solve Merger Scenario ######")
    solve_model!(m,f,sim="RA",merg="Merger",voucher=true,update_voucher=false)
    P_RA_m[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=false)
    S_RA_m[:] = f.S_j[:]

    merger_RA_profits = market_profits(m,f)

    consumer_welfare(m,f,"RA_m_vch")
    trash = total_welfare_bymkt(m,f,"RA_m_vch",update_voucher=false)

    println("#### CURRENT PROFIT PROBLEM ####")
    f.P_j[:] = P_SP[:]
    markets_cp, λ_vec_cp = solve_SP_λ!(m,f,RA_profits,CW_target=RA_welfare)
    P_SP_cp[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=false)
    S_SP_cp[:] = f.S_j[:]


    consumer_welfare(m,f,"SP_cp")
    trash = total_welfare_bymkt(m,f,"SP_cp",update_voucher=false)

    println("#### MERGER PROFIT PROBLEM ####")
    f.P_j[:] = P_SP[:]
    markets_cpm, λ_vec_cpm = solve_SP_λ!(m,f,merger_RA_profits,markets=[4;5;6;7;8;9;10;11;12;13;14])
    P_SP_cpm[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=false)
    S_SP_cpm[:] = f.S_j[:]

    consumer_welfare(m,f,"SP_cpm")
    trash = total_welfare_bymkt(m,f,"SP_cpm",update_voucher=false)


    #### Solve without Risk Adjustment ####
    println("####################################")
    println("#### Solve without Individual Mandate ####")
    println("####################################")
    f.P_j[:] = P_Obs[:]
    f.data[:,f.index[:Mandate]].=0.0
    solve_model!(m,f,sim="Base",voucher=true,update_voucher=false)
    P_man[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=false)
    S_man[:] = f.S_j[:]

    man_profits = market_profits(m,f)

    consumer_welfare(m,f,"man_vch")
    man_welfare = total_welfare_bymkt(m,f,"man_vch",update_voucher=false)

    println("###### Solve Merger Scenario ######")
    solve_model!(m,f,sim="Base",merg="Merger",voucher=true,update_voucher=false)
    P_man_m[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=false)
    S_man_m[:] = f.S_j[:]

    merger_man_profits = market_profits(m,f)

    consumer_welfare(m,f,"man_m_vch")
    trash = total_welfare_bymkt(m,f,"man_m_vch",update_voucher=false)

    println("#### CURRENT PROFIT PROBLEM ####")
    f.P_j[:] = P_SP[:]
    markets_cp, λ_vec_cp = solve_SP_λ!(m,f,man_profits,CW_target=man_welfare)
    P_SP_cp_man[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=false)
    S_SP_cp_man[:] = f.S_j[:]


    consumer_welfare(m,f,"SP_cp_man")
    trash = total_welfare_bymkt(m,f,"SP_cp_man",update_voucher=false)

    println("#### MERGER PROFIT PROBLEM ####")
    f.P_j[:] = P_SP[:]
    markets_cpm, λ_vec_cpm = solve_SP_λ!(m,f,merger_man_profits,markets=[4;5;6;7;8;9;10;11;12;13;14])
    P_SP_cpm_man[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=false)
    S_SP_cpm_man[:] = f.S_j[:]

    consumer_welfare(m,f,"SP_cpm_man")
    trash = total_welfare_bymkt(m,f,"SP_cpm_man",update_voucher=false)


    #### Solve without Risk Adjustment ####
    println("####################################")
    println("#### Solve without Individual Mandate ####")
    println("####################################")
    f.P_j[:] = P_Obs[:]
    f.data[:,f.index[:Mandate]].=0.0
    solve_model!(m,f,sim="RA",voucher=true,update_voucher=false)
    P_RAman[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=false)
    S_RAman[:] = f.S_j[:]

    RAman_profits = market_profits(m,f)

    consumer_welfare(m,f,"RAman_vch")
    RAman_welfare = total_welfare_bymkt(m,f,"RAman_vch",update_voucher=false)

    println("###### Solve Merger Scenario ######")
    solve_model!(m,f,sim="RA",merg="Merger",voucher=true,update_voucher=false)
    P_RAman_m[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=false)
    S_RAman_m[:] = f.S_j[:]

    merger_RAman_profits = market_profits(m,f)

    consumer_welfare(m,f,"RAman_m_vch")
    trash = total_welfare_bymkt(m,f,"RAman_m_vch",update_voucher=false)

    println("#### CURRENT PROFIT PROBLEM ####")
    f.P_j[:] = P_SP[:]
    markets_cp, λ_vec_cp = solve_SP_λ!(m,f,RAman_profits,CW_target=RAman_welfare)
    P_SP_cp_RAman[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=false)
    S_SP_cp_RAman[:] = f.S_j[:]


    consumer_welfare(m,f,"SP_cp_RAman")
    trash = total_welfare_bymkt(m,f,"SP_cp_RAman",update_voucher=false)

    println("#### MERGER PROFIT PROBLEM ####")
    f.P_j[:] = P_SP[:]
    markets_cpm, λ_vec_cpm = solve_SP_λ!(m,f,merger_RAman_profits,markets=[4;5;6;7;8;9;10;11;12;13;14])
    P_SP_cpm_RAman[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=false)
    S_SP_cpm_RAman[:] = f.S_j[:]

    consumer_welfare(m,f,"SP_cpm_RAman")
    trash = total_welfare_bymkt(m,f,"SP_cpm_RAman",update_voucher=false)


    output =  DataFrame(Product=prod_vec,
                        Price_data=P_Obs,
                        Price_sp=P_SP,
                        Price_sp_zp=P_SP_zp,

                        Price_sp_cp=P_SP_cp,
                        Price_sp_cpm=P_SP_cpm,
                        Price_sp_cp_base=P_SP_cp_base,
                        Price_sp_cpm_base=P_SP_cpm_base,
                        Price_sp_cp_man=P_SP_cp_man,
                        Price_sp_cpm_man=P_SP_cpm_man,
                        Price_sp_cp_RAman=P_SP_cp_RAman,
                        Price_sp_cpm_RAman=P_SP_cpm_RAman,

                        Price_base=P_Base,
                        Price_RA =P_RA,
                        Price_man =P_man,
                        Price_RAman =P_RAman,
                        Price_base_m=P_Base_m,
                        Price_RA_m =P_RA_m,
                        Price_man_m =P_man_m,
                        Price_RAman_m =P_RAman_m,

                        Lives_sp=S_SP,
                        Lives_sp_zp=S_SP_zp,
                        Lives_sp_cp=S_SP_cp,
                        Lives_sp_cpm=S_SP_cpm,
                        Lives_sp_cp_base=S_SP_cp_base,
                        Lives_sp_cpm_base=S_SP_cpm_base,
                        Lives_sp_cp_man=S_SP_cp_man,
                        Lives_sp_cpm_man=S_SP_cpm_man,
                        Lives_sp_cp_RAman=S_SP_cp_RAman,
                        Lives_sp_cpm_RAman=S_SP_cpm_RAman,


                        Lives_base=S_Base,
                        Lives_RA =S_RA,
                        Lives_man =S_man,
                        Lives_RAman =S_RAman,
                        Lives_base_m=S_Base_m,
                        Lives_RA_m =S_RA_m,
                        Lives_man_m =S_man_m,
                        Lives_RAman_m =S_RAman_m)

    CSV.write(file,output)

    return nothing
end


function solveMain_SP_Gov(m::InsuranceLogit,f::firmData,file::String)
    J = maximum(m.prods)
    P_Obs = zeros(J)
    prod_vec = zeros(J)
    prod_vec[sort(m.prods)] = sort(m.prods)
    println(length(prod_vec))
    println(length(prod_vec[prod_vec.>0]))
    println(maximum(prod_vec))

    P_SP_cp_base = zeros(J)
    P_SP_cpm_base = zeros(J)
    P_SP = zeros(J)
    P_SP_zp = zeros(J)
    P_SP_cp = zeros(J)
    P_SP_cpm = zeros(J)
    P_Base = zeros(J)
    P_RA = zeros(J)
    P_Base_m = zeros(J)
    P_RA_m = zeros(J)

    S_SP_cp_base = zeros(J)
    S_SP_cpm_base = zeros(J)
    S_SP = zeros(J)
    S_SP_zp = zeros(J)
    S_SP_cp = zeros(J)
    S_SP_cpm = zeros(J)
    S_Base = zeros(J)
    S_RA = zeros(J)
    S_Base_m = zeros(J)
    S_RA_m = zeros(J)

    P_Obs[:] = f.P_j[:]

    #### Solve Baseline - With Risk Adjustment and Mandate ####
    println("####################################")
    println("#### Solve Baseline - With Risk Adjustment and Mandate ####")
    println("####################################")
    f.P_j[:] = P_Obs[:]
    solve_model!(m,f,sim="Base",voucher=true)
    evaluate_model!(m,f,"All",voucher=true)
    set_voucher!(f,refund=true)

    solve_model!(m,f,sim="Base",voucher=true,update_voucher=false)
    P_Base[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=false)
    S_Base[:] = f.S_j[:]

    base_profits = market_profits(m,f)
    base_welfare = total_welfare_bymkt(m,f,"Base_vch",update_voucher=false)
    consumer_welfare(m,f,"Base_vch")

    println("###### Solve Merger Scenario ######")
    # f.P_j[:] = P_Obs[:]
    solve_model!(m,f,sim="Base",merg="Merger",voucher=true,update_voucher=false)
    P_Base_m[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=false)
    S_Base_m[:] = f.S_j[:]

    merger_base_profits = market_profits(m,f)
    consumer_welfare(m,f,"Base_m_vch")
    trash = total_welfare_bymkt(m,f,"Base_m_vch",update_voucher=false)

    ## Solve Planner Problem ####
    println("####################################")
    println("#### Solve Social Planner Problem ####")
    println("####################################")
    f.P_j[:] .= P_Obs[:]
    solve_SP!(m,f,sim="SP_gov",merg="SP",voucher=true,update_voucher=false)
    P_SP[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=false)
    S_SP[:] = f.S_j[:]

    consumer_welfare(m,f,"SP_gov")
    trash = total_welfare_bymkt(m,f,"SP_gov",update_voucher=false)

    println("#### ZERO PROFIT PROBLEM ####")
    f.P_j[:] = P_SP[:]
    markets_zp, λ_vec_zp = solve_SP_λ!(m,f,zeros(length(base_profits)),sim="SPλ_gov")
    P_SP_zp[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=false)
    S_SP_zp[:] = f.S_j[:]

    consumer_welfare(m,f,"SP_zp_gov")
    trash = total_welfare_bymkt(m,f,"SP_zp_gov",update_voucher=false)

    println("#### CURRENT PROFIT PROBLEM ####")
    f.P_j[:] = P_SP[:]
    markets_cp, λ_vec_cp = solve_SP_λ!(m,f,base_profits,sim="SPλ_gov")
    P_SP_cp_base[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=false)
    S_SP_cp_base[:] = f.S_j[:]


    consumer_welfare(m,f,"SP_cp_base_gov")
    trash = total_welfare_bymkt(m,f,"SP_cp_base_gov",update_voucher=false)

    println("#### MERGER PROFIT PROBLEM ####")
    f.P_j[:] = P_SP[:]
    markets_cpm, λ_vec_cpm = solve_SP_λ!(m,f,merger_base_profits,markets=[4;5;6;7;8;9;10;11;12;13;14],sim="SPλ_gov")
    P_SP_cpm_base[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=false)
    S_SP_cpm_base[:] = f.S_j[:]

    consumer_welfare(m,f,"SP_cpm_base_gov")
    trash = total_welfare_bymkt(m,f,"SP_cpm_base_gov",update_voucher=false)

    #### Solve without Risk Adjustment ####
    println("####################################")
    println("#### Solve without Risk Adjustment ####")
    println("####################################")
    f.P_j[:] = P_Obs[:]
    solve_model!(m,f,sim="RA",voucher=true,update_voucher=false)
    P_RA[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=false)
    S_RA[:] = f.S_j[:]

    RA_profits = market_profits(m,f)

    consumer_welfare(m,f,"RA_vch")
    RA_welfare = total_welfare_bymkt(m,f,"RA_vch",update_voucher=false)

    println("###### Solve Merger Scenario ######")
    solve_model!(m,f,sim="RA",merg="Merger",voucher=true,update_voucher=false)
    P_RA_m[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=false)
    S_RA_m[:] = f.S_j[:]

    merger_RA_profits = market_profits(m,f)

    consumer_welfare(m,f,"RA_m_vch")
    trash = total_welfare_bymkt(m,f,"RA_m_vch",update_voucher=true)

    println("#### CURRENT PROFIT PROBLEM ####")
    f.P_j[:] = P_SP[:]
    markets_cp, λ_vec_cp = solve_SP_λ!(m,f,RA_profits,CW_target=RA_welfare,sim="SPλ_gov")
    P_SP_cp[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=false)
    S_SP_cp[:] = f.S_j[:]


    consumer_welfare(m,f,"SP_cp_gov")
    trash = total_welfare_bymkt(m,f,"SP_cp_gov",update_voucher=false)

    println("#### MERGER PROFIT PROBLEM ####")
    f.P_j[:] = P_SP[:]
    markets_cpm, λ_vec_cpm = solve_SP_λ!(m,f,merger_RA_profits,markets=[4;5;6;7;8;9;10;11;12;13;14],sim="SPλ_gov")
    P_SP_cpm[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=true,update_voucher=false)
    S_SP_cpm[:] = f.S_j[:]

    consumer_welfare(m,f,"SP_cpm_gov")
    trash = total_welfare_bymkt(m,f,"SP_cpm_gov",update_voucher=false)


    output =  DataFrame(Product=prod_vec,
                        Price_data=P_Obs,
                        Price_sp=P_SP,
                        Price_sp_zp=P_SP_zp,
                        Price_sp_cp=P_SP_cp,
                        Price_sp_cpm=P_SP_cpm,
                        Price_sp_cp_base=P_SP_cp_base,
                        Price_sp_cpm_base=P_SP_cpm_base,
                        Price_base=P_Base,
                        Price_RA =P_RA,
                        Price_base_m=P_Base_m,
                        Price_RA_m =P_RA_m,
                        Lives_sp=S_SP,
                        Lives_sp_zp=S_SP_zp,
                        Lives_sp_cp=S_SP_cp,
                        Lives_sp_cpm=S_SP_cpm,
                        Lives_sp_cp_base=S_SP_cp_base,
                        Lives_sp_cpm_base=S_SP_cpm_base,
                        Lives_base=S_Base,
                        Lives_RA =S_RA,
                        Lives_base_m=S_Base_m,
                        Lives_RA_m =S_RA_m)

    CSV.write(file,output)

    return nothing
end


function solveMain_PL(m::InsuranceLogit,f::firmData,file::String)
    J = maximum(m.prods)
    P_Obs = zeros(J)
    prod_vec = zeros(J)
    prod_vec[sort(m.prods)] = sort(m.prods)
    println(length(prod_vec))
    println(length(prod_vec[prod_vec.>0]))
    println(maximum(prod_vec))

    P_Base_pl = zeros(J)
    P_RA_pl = zeros(J)
    P_man_pl = zeros(J)
    P_RAman_pl = zeros(J)
    P_Base_m_pl = zeros(J)
    P_RA_m_pl = zeros(J)
    P_man_m_pl = zeros(J)
    P_RAman_m_pl = zeros(J)

    S_Base_pl = zeros(J)
    S_RA_pl = zeros(J)
    S_man_pl = zeros(J)
    S_RAman_pl = zeros(J)
    S_Base_m_pl = zeros(J)
    S_RA_m_pl = zeros(J)
    S_man_m_pl = zeros(J)
    S_RAman_m_pl = zeros(J)

    P_Obs[:] = f.P_j[:]

    println("####################################")
    println("#### Solve Baseline - With Risk Adjustment and Mandate - Price Linked ####")
    println("####################################")
    solve_model!(m,f,sim="Base",voucher=false)
    P_Base_pl[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=false)
    S_Base_pl[:] = f.S_j[:]


    consumer_welfare(m,f,"Base_pl")
    trash = total_welfare_bymkt(m,f,"Base_pl",update_voucher=true)

    println("###### Solve Merger Scenario ######")
    # f.P_j[:] = P_Obs[:]
    solve_model!(m,f,sim="Base",merg="Merger",voucher=false)
    P_Base_m_pl[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=false)
    S_Base_m_pl[:] = f.S_j[:]

    consumer_welfare(m,f,"Base_m_pl")
    trash = total_welfare_bymkt(m,f,"Base_m_pl",update_voucher=true)

    #### Solve without Risk Adjustment ####
    println("####################################")
    println("#### Solve without Risk Adjustment - Price Linked  ####")
    println("####################################")
    f.P_j[:] = P_Obs[:]
    solve_model!(m,f,sim="RA",voucher=false)
    P_RA_pl[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=false)
    S_RA_pl[:] = f.S_j[:]

    consumer_welfare(m,f,"RA_pl")
    trash = total_welfare_bymkt(m,f,"RA_pl",update_voucher=true)

    println("###### Solve Merger Scenario ######")
    solve_model!(m,f,sim="RA",merg="Merger",voucher=false)
    P_RA_m_pl[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=false)
    S_RA_m_pl[:] = f.S_j[:]

    consumer_welfare(m,f,"RA_m_pl")
    trash = total_welfare_bymkt(m,f,"RA_m_pl",update_voucher=true)

    ### Solve without mandate ####
    println("####################################")
    println("#### Solve without mandate - Price Linked  ####")
    println("####################################")
    f.P_j[:] = P_Obs[:]
    f.data[:,f.index[:Mandate]].=0.0
    solve_model!(m,f,sim="Base",voucher=false)
    P_man_pl[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=false)
    S_man_pl[:] = f.S_j[:]

    consumer_welfare(m,f,"man_pl")
    trash = total_welfare_bymkt(m,f,"man_pl",update_voucher=true)

    # println(median(f[:Mandate]))
    # println(median(f.P_j[findall(f.P_j.>0)]))

    println("###### Solve Merger Scenario - Price Linked  ######")
    solve_model!(m,f,sim="Base",merg="Merger",voucher=false)
    P_man_m_pl[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=false)
    S_man_m_pl[:] = f.S_j[:]

    consumer_welfare(m,f,"man_m_pl")
    trash = total_welfare_bymkt(m,f,"man_m_pl",update_voucher=true)


    ### Solve without mandate NOR risk adjustment  ####
    println("####################################")
    println("#### Solve without mandate NOR risk adjustment - Price Linked   ####")
    println("####################################")
    f.P_j[:] = P_Obs[:]
    f.data[:,f.index[:Mandate]].=0.0
    solve_model!(m,f,sim="RA",voucher=false)
    P_RAman_pl[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=false)
    S_RAman_pl[:] = f.S_j[:]

    consumer_welfare(m,f,"RAman_pl")
    trash = total_welfare_bymkt(m,f,"RAman_pl",update_voucher=true)


    println("###### Solve Merger Scenario ######")
    solve_model!(m,f,sim="RA",merg="Merger",voucher=false)
    P_RAman_m_pl[:] = f.P_j[:]
    evaluate_model!(m,f,"All",voucher=false)
    S_RAman_m_pl[:] = f.S_j[:]

    consumer_welfare(m,f,"RAman_m_pl")
    trash = total_welfare_bymkt(m,f,"RAman_m_pl",update_voucher=true)



    output =  DataFrame(Product=prod_vec,
                        Price_data=P_Obs,
                        Price_base=P_Base_pl,
                        Price_RA =P_RA_pl,
                        Price_man=P_man_pl,
                        Price_RAman=P_RAman_pl,
                        Price_base_m=P_Base_m_pl,
                        Price_RA_m =P_RA_m_pl,
                        Price_man_m=P_man_m_pl,
                        Price_RAman_m=P_RAman_m_pl,
                        Lives_base=S_Base_pl,
                        Lives_RA =S_RA_pl,
                        Lives_man=S_man_pl,
                        Lives_RAman=S_RAman_pl,
                        Lives_base_m=S_Base_m_pl,
                        Lives_RA_m =S_RA_m_pl,
                        Lives_man_m=S_man_m_pl,
                        Lives_RAman_m=S_RAman_m_pl)

    CSV.write(file,output)

    return nothing
end
