function MergersMain(rundate,spec,home_directory)

    #Load Data
    println("Loading Data...")
    include("$(home_directory)/Research/Imperfect_Insurance_Competition/Code/Solve_Equilibrium/EQ_load.jl")

    # df[:High_small] = df[:HighRisk].*df[:Small]

    mark_the_output_date = Dates.today()
    println("Running spec $rundate on $mark_the_output_date")

    file = "$(home_directory)/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg2_$spec-$rundate.jld2"
    @load file p_stg2 p_dem_est cost_spec spec_Dict
    mc_est = copy(p_stg2)
    #### Load Estimation Results ####
    # file = "$(home_directory)/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/PLL_Estimate_$spec-$rundate-stg1.jld2"
    # @load file p_stg1 spec_Dict

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


    filestub = "AllMergers_$spec-$(rundate)"
    println("####################################")
    println("#### Solve Policy Baseline  ####")
    println("####################################")
    simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
                            filestub,policy="Base")
    println("####################################")
    println("#### Solve Without Risk Adjustment ####")
    println("####################################")
    simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
                            filestub,policy="RA_repeal")
    println("####################################")
    println("#### Solve Without Individual Mandate ####")
    println("####################################")
    simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
                            filestub,policy="Man_repeal")
    println("####################################")
    println("#### Solve Without Risk Adjustment nor Individual Mandate ####")
    println("####################################")
    simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
                            filestub,policy="RAMan_repeal")
    # filestub = "AllMergers_PL_$spec-$(rundate)"
    # println("####################################")
    # println("#### Solve Policy Baseline - Price Linked ####")
    # println("####################################")
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="Base",voucher=false)
    # println("####################################")
    # println("#### Solve Without Risk Adjustment - Price Linked ####")
    # println("####################################")
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="RA_repeal",voucher=false)
    # println("####################################")
    # println("#### Solve Without Individual Mandate - Price Linked ####")
    # println("####################################")
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="Man_repeal",voucher=false)
    # println("####################################")
    # println("#### Solve Without Risk Adjustment nor Individual Mandate - Price Linked ####")
    # println("####################################")
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="RAMan_repeal",voucher=false)

    println("Function End")
    return nothing
end


function MergersMain0(rundate,spec,home_directory)

    #Load Data
    println("Loading Data...")
    include("EQ_load.jl")

    # df[:High_small] = df[:HighRisk].*df[:Small]

    mark_the_output_date = Dates.today()
    println("Running spec $rundate on $mark_the_output_date")

    file = "$(home_directory)/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg2_$spec-$rundate.jld2"
    @load file p_stg2 p_dem_est cost_spec spec_Dict
    mc_est = copy(p_stg2)
    #### Load Estimation Results ####
    # file = "$(home_directory)/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/PLL_Estimate_$spec-$rundate-stg1.jld2"
    # @load file p_stg1 spec_Dict

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

    filestub = "AllMergers_$spec-$(rundate)"
    println("####################################")
    println("#### Solve Policy Baseline  ####")
    println("####################################")
    simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
                            filestub,policy="Base")
    # println("####################################")
    # println("#### Solve Without Risk Adjustment ####")
    # println("####################################")
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="RA_repeal")
    println("####################################")
    println("#### Solve Without Individual Mandate ####")
    println("####################################")
    simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
                            filestub,policy="Man_repeal")
    println("####################################")
    println("#### Solve Without Risk Adjustment nor Individual Mandate ####")
    println("####################################")
    simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
                            filestub,policy="RAMan_repeal")
    # filestub = "AllMergers_PL_$spec-$(rundate)"
    # println("####################################")
    # println("#### Solve Policy Baseline - Price Linked ####")
    # println("####################################")
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="Base",voucher=false)
    # println("####################################")
    # println("#### Solve Without Risk Adjustment - Price Linked ####")
    # println("####################################")
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="RA_repeal",voucher=false)
    # println("####################################")
    # println("#### Solve Without Individual Mandate - Price Linked ####")
    # println("####################################")
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="Man_repeal",voucher=false)
    # println("####################################")
    # println("#### Solve Without Risk Adjustment nor Individual Mandate - Price Linked ####")
    # println("####################################")
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="RAMan_repeal",voucher=false)

    println("Function End")
    return nothing
end


function MergersMain1(rundate,spec,home_directory)

    #Load Data
    println("Loading Data...")
    include("EQ_load.jl")

    # df[:High_small] = df[:HighRisk].*df[:Small]

    mark_the_output_date = Dates.today()
    println("Running spec $rundate on $mark_the_output_date")

    file = "$(home_directory)/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg2_$spec-$rundate.jld2"
    @load file p_stg2 p_dem_est cost_spec spec_Dict
    mc_est = copy(p_stg2)
    #### Load Estimation Results ####
    # file = "$(home_directory)/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/PLL_Estimate_$spec-$rundate-stg1.jld2"
    # @load file p_stg1 spec_Dict

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


    filestub = "AllMergers_$spec-$(rundate)"
    # println("####################################")
    # println("#### Solve Policy Baseline  ####")
    # println("####################################")
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="Base")
    # println("####################################")
    # println("#### Solve Without Risk Adjustment ####")
    # println("####################################")
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="RA_repeal")
    println("####################################")
    println("#### Solve Without Individual Mandate ####")
    println("####################################")
    simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
                            filestub,policy="Man_repeal")
    # println("####################################")
    # println("#### Solve Without Risk Adjustment nor Individual Mandate ####")
    # println("####################################")
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="RAMan_repeal")
    # filestub = "AllMergers_PL_$spec-$(rundate)"
    # println("####################################")
    # println("#### Solve Policy Baseline - Price Linked ####")
    # println("####################################")
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="Base",voucher=false)
    # println("####################################")
    # println("#### Solve Without Risk Adjustment - Price Linked ####")
    # println("####################################")
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="RA_repeal",voucher=false)
    # println("####################################")
    # println("#### Solve Without Individual Mandate - Price Linked ####")
    # println("####################################")
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="Man_repeal",voucher=false)
    # println("####################################")
    # println("#### Solve Without Risk Adjustment nor Individual Mandate - Price Linked ####")
    # println("####################################")
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="RAMan_repeal",voucher=false)

    println("Function End")
    return nothing
end

function MergersMain2(rundate,spec,home_directory)

    #Load Data
    println("Loading Data...")
    include("EQ_load.jl")

    # df[:High_small] = df[:HighRisk].*df[:Small]

    mark_the_output_date = Dates.today()
    println("Running spec $rundate on $mark_the_output_date")

    file = "$(home_directory)/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg2_$spec-$rundate.jld2"
    @load file p_stg2 p_dem_est cost_spec spec_Dict
    mc_est = copy(p_stg2)
    #### Load Estimation Results ####
    # file = "$(home_directory)/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/PLL_Estimate_$spec-$rundate-stg1.jld2"
    # @load file p_stg1 spec_Dict

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


    filestub = "AllMergers_$spec-$(rundate)"
    # println("####################################")
    # println("#### Solve Policy Baseline  ####")
    # println("####################################")
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="Base")
    # println("####################################")
    # println("#### Solve Without Risk Adjustment ####")
    # println("####################################")
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="RA_repeal")
    # println("####################################")
    # println("#### Solve Without Individual Mandate ####")
    # println("####################################")
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="Man_repeal")
    println("####################################")
    println("#### Solve Without Risk Adjustment nor Individual Mandate ####")
    println("####################################")
    simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
                            filestub,policy="RAMan_repeal")
    filestub = "AllMergers_PL_$spec-$(rundate)"
    # println("####################################")
    # println("#### Solve Policy Baseline - Price Linked ####")
    # println("####################################")
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="Base",voucher=false)
    # println("####################################")
    # println("#### Solve Without Risk Adjustment - Price Linked ####")
    # println("####################################")
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="RA_repeal",voucher=false)
    # println("####################################")
    # println("#### Solve Without Individual Mandate - Price Linked ####")
    # println("####################################")
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="Man_repeal",voucher=false)
    # println("####################################")
    # println("#### Solve Without Risk Adjustment nor Individual Mandate - Price Linked ####")
    # println("####################################")
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="RAMan_repeal",voucher=false)

    println("Function End")
    return nothing
end

function MergersMain3(rundate,spec,home_directory)

    #Load Data
    println("Loading Data...")
    include("EQ_load.jl")

    # df[:High_small] = df[:HighRisk].*df[:Small]

    mark_the_output_date = Dates.today()
    println("Running spec $rundate on $mark_the_output_date")

    file = "$(home_directory)/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg2_$spec-$rundate.jld2"
    @load file p_stg2 p_dem_est cost_spec spec_Dict
    mc_est = copy(p_stg2)
    #### Load Estimation Results ####
    # file = "$(home_directory)/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/PLL_Estimate_$spec-$rundate-stg1.jld2"
    # @load file p_stg1 spec_Dict

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


    # println("####################################")
    # println("#### Solve Policy Baseline  ####")
    # println("####################################")
    # filestub = "AllMergers_$spec-$(rundate)"
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="Base")
    # println("####################################")
    # println("#### Solve Without Risk Adjustment ####")
    # println("####################################")
    # filestub = "AllMergers_$spec-$(rundate)"
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="RA_repeal")
    # println("####################################")
    # println("#### Solve Without Individual Mandate ####")
    # println("####################################")
    # filestub = "AllMergers_$spec-$(rundate)"
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="Man_repeal")
    # println("####################################")
    # println("#### Solve Without Risk Adjustment nor Individual Mandate ####")
    # println("####################################")
    # filestub = "AllMergers_$spec-$(rundate)"
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="RAMan_repeal")
    filestub = "AllMergers_PL_$spec-$(rundate)"
    println("####################################")
    println("#### Solve Policy Baseline - Price Linked ####")
    println("####################################")
    simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
                            filestub,policy="Base",voucher=false)
    # println("####################################")
    # println("#### Solve Without Risk Adjustment - Price Linked ####")
    # println("####################################")
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="RA_repeal",voucher=false)
    # println("####################################")
    # println("#### Solve Without Individual Mandate - Price Linked ####")
    # println("####################################")
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="Man_repeal",voucher=false)
    println("####################################")
    println("#### Solve Without Risk Adjustment nor Individual Mandate - Price Linked ####")
    println("####################################")
    simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
                            filestub,policy="RAMan_repeal",voucher=false)

    println("Function End")
    return nothing
end


function ownerMatrix!(fdata::firmData)
    fdata.ownMat[:].=0.0
    prod_std = fdata.prods
    firm_list = fdata.firm_vector
    for j in prod_std
        f = firm_list[j]
        for i in prod_std
            if (f=="") | (firm_list[i]=="")
                continue
            elseif (f == firm_list[i])
                fdata.ownMat[j,i]=1
            end
        end
    end
    return nothing
end




function ownerMatrix!(fdata::firmData,merging_firms)
    fdata.ownMat[:].=0.0
    prod_std = fdata.prods
    firm_list = fdata.firm_vector
    for j in prod_std
        f = firm_list[j]
        for i in prod_std
            if (f=="") | (firm_list[i]=="")
                continue
            elseif (f == firm_list[i]) | ((f in merging_firms) & (firm_list[i] in merging_firms))
                fdata.ownMat[j,i]=1
            end
        end
    end
    return nothing
end


function check_states_if_merger(f::firmData,merging_parties)
    shared_markets = 0
    states = sort(String.(keys(f._prodSTDict)))
    state_bool = BitVector(undef,length(states))
    for (i,st) in enumerate(states)
        prods = f._prodSTDict[st]
        market_firms = f.firm_vector[prods]
        state_bool[i] = (merging_parties[1] in market_firms) & (merging_parties[2] in market_firms)
    end
    return states[state_bool]
end

function check_markets_if_merger(f::firmData,merging_parties)
    shared_markets = 0
    markets = sort(Int.(keys(f.mkt_index)))
    market_bool = BitVector(undef,length(markets))
    for (i,mkt) in enumerate(markets)
        prods = f.mkt_index[mkt]
        market_firms = f.firm_vector[prods]
        market_bool[i] = (merging_parties[1] in market_firms) & (merging_parties[2] in market_firms)
    end
    return markets[market_bool]
end
function simulate_all_mergers(m::InsuranceLogit,
                            df::DataFrame,mkt::DataFrame,par_dem::parDict,par_cost::parMC,
                            file_stub_short;
                            policy="",voucher=true)

    # Initialize Firm Data
    f = firmData(m,df,mkt,par_dem,par_cost)




    # Policy Regime
    if policy=="RA_repeal"
        @everywhere sim="RA"
        policy = "RA"
    elseif policy=="Man_repeal"
        @everywhere sim = "Base"
        f.data[:,f.index[:Mandate]].=0.0
        policy = "Man"
    elseif policy=="RAMan_repeal"
        @everywhere sim = "RA"
        f.data[:,f.index[:Mandate]].=0.0
        policy = "RAMan"
    else
        @everywhere sim = "Base"
    end
    file_stub = "$(file_stub_short)_$policy"
    # Price link Regime
    update_voucher = !voucher

    # Initialize Vectors
    J = maximum(m.prods)
    prod_vec = zeros(J)
    prod_vec[sort(m.prods)] = sort(m.prods)
    P_Base = zeros(J)
    P_Base_SP_cp = zeros(J)

    # ## Solve Baseline Model
    println("Solve Baseline Model")
    if policy=="Base" # Solve baseline model with vouchers adjusting on prices
        solve_model_parallel!(m,f,sim=sim,voucher=voucher)
        P_Base[:] = f.P_j[:]
        evaluate_model!(m,f,"All",voucher=voucher)
        set_voucher!(f,refund=true)
    else
        # Set Vouchers from Baseline Model
        file = "$(home_directory)/Research/Imperfect_Insurance_Competition/Estimation_Output/$(file_stub_short)_Base_baseline.csv"
        output = CSV.read(file,DataFrame)
        f.P_j[:] = output[!,:Price]
        evaluate_model!(m,f,"All",voucher=voucher)
        set_voucher!(f,refund=true)

        solve_model_parallel!(m,f,sim=sim,voucher=voucher,update_voucher=update_voucher)
        P_Base[:] = f.P_j[:]
        evaluate_model!(m,f,"All",voucher=voucher,update_voucher=update_voucher)
    end
    prod_profits = product_profits(m,f,sim=sim)

    base_profits = market_profits(m,f)
    # consumer_welfare(m,f,"$(file_stub)_baseline",spec,rundate)
    trash = total_welfare_bymkt(m,f,"$(file_stub)_baseline",spec,rundate,update_voucher=update_voucher)

    # Output Baseline Model
    file = "$(home_directory)/Research/Imperfect_Insurance_Competition/Estimation_Output/$(file_stub)_baseline.csv"
    output =  DataFrame(Product=prod_vec,
                        Price=f.P_j,
                        Lives=f.S_j,
                        Profit = prod_profits)
    CSV.write(file,output)

    println("Total Voucher: $(sum(f.subsidy_ij))")
    println("Total Fixed Voucher: $(sum(f.subsidy_ij_voucher))")

    #
    # # Solve Baseline Social Planner Problem
    # println("Solve Baseline Planner Problem")
    # solve_SP_parallel!(m,f,voucher=voucher,update_voucher=update_voucher)
    # evaluate_model!(m,f,"All",voucher=voucher,update_voucher=update_voucher)
    #
    # # consumer_welfare(m,f,"$(file_stub)_SP_baseline",spec,rundate)
    # trash = total_welfare_bymkt(m,f,"$(file_stub)_SP_baseline",spec,rundate,update_voucher=update_voucher)
    #
    # # Output Baseline SP Model
    # file = "$(home_directory)/Research/Imperfect_Insurance_Competition/Estimation_Output/$(file_stub)_SP_baseline.csv"
    # output =  DataFrame(Product=prod_vec,
    #                     Price=f.P_j,
    #                     Lives=f.S_j)
    # CSV.write(file,output)
    #
    # println("Total Voucher: $(sum(f.subsidy_ij))")
    # println("Total Fixed Voucher: $(sum(f.subsidy_ij_voucher))")
    #
    #
    # ## Solve Baseline Constrained Planner Problem
    # println("Solve Baseline Current Profit Planner Problem")
    # # markets_cp, λ_vec_cp = solve_SP_λ!(m,f,base_profits,markets=[1])
    #
    # markets_cp, λ_vec_cp = solve_SP_λ_parallel!(m,f,base_profits)
    # evaluate_model!(m,f,"All",voucher=voucher,update_voucher=update_voucher)
    # P_Base_SP_cp[:] = f.P_j[:]
    #
    # # println("Model Price: $(f.P_j)")
    # # # P_Base_SP_cp = P_Base[:]
    # # #
    # # consumer_welfare(m,f,"$(file_stub)_SP_cp_baseline",spec,rundate)
    # trash = total_welfare_bymkt(m,f,"$(file_stub)_SP_cp_baseline",spec,rundate,update_voucher=update_voucher)
    #
    # # Output Baseline Model
    # file = "$(home_directory)/Research/Imperfect_Insurance_Competition/Estimation_Output/$(file_stub)_SP_cp_baseline.csv"
    # output =  DataFrame(Product=prod_vec,
    #                     Price=f.P_j,
    #                     Lives=f.S_j)
    # CSV.write(file,output)

    ### Reset to Baseline
    f.P_j[:] = P_Base[:]
    evaluate_model!(m,f,"All",voucher=voucher,update_voucher=update_voucher)

    println("Send Data to Workers")
    @eval @everywhere m=$m
    @eval @everywhere f=$f
    println("Data Distributed")
    # Initialize Vectors
    @everywhere J = maximum(m.prods)
    @everywhere prod_vec = zeros(J)
    @everywhere prod_vec[sort(m.prods)] = sort(m.prods)
    @eval @everywhere P_Base = $P_Base
    @eval @everywhere P_Base_SP_cp = $P_Base_SP_cp



    # Iterate through all potential mergers
    unique_firms = sort(unique(f.firm_vector[f.firm_vector.!=""]))
    merging_party_list = Vector{Vector{String}}(undef,0)
    shared_state_list = Vector{Vector{String}}(undef,0)
    shared_market_list = Vector{Vector{Int}}(undef,0)
    for (f_index,merge_party_2) in enumerate(unique_firms)
        for merge_party_1 in unique_firms[1:(f_index-1)]
            # Merging parties
            merging_parties = [merge_party_1,merge_party_2]

            # Skip if merging parties do not operate in same state
            shared_states = check_states_if_merger(f,merging_parties)
            shared_markets = check_markets_if_merger(f,merging_parties)
            if length(shared_markets)==0
                continue
            end
            push!(merging_party_list,merging_parties)
            push!(shared_state_list,shared_states)
            push!(shared_market_list,shared_markets)
        end
    end
    println("Send to Iteration Parameters to Workers")
    # sendto(workers(),merging_party_list,file_stub,update_voucher)
    @eval @everywhere merging_party_list=$merging_party_list
    @eval @everywhere shared_state_list=$shared_state_list
    @eval @everywhere shared_market_list=$shared_market_list
    @eval @everywhere file_stub=$file_stub
    @eval @everywhere update_voucher=$update_voucher
    @eval @everywhere voucher=$voucher
    @eval @everywhere home_directory=$home_directory
    @eval @everywhere spec=$spec
    @eval @everywhere rundate=$rundate
    @eval @everywhere sim=$sim
    println("Data Distributed")

    @sync @distributed for i in eachindex(merging_party_list)
    # @sync @distributed for i in 1:length(merging_party_list[1:12])
        shared_markets = shared_market_list[i]
        shared_states = shared_state_list[i]
        merging_parties = merging_party_list[i]

        ### Only GA MergersMain
        if !("GA" in shared_states)
            println("Non-GA Merger")
            continue
        else
            shared_states = ["GA"]
        end

 
        println(merging_parties)

        ## Set post-merger ownership matrix
        ownerMatrix!(f,merging_parties)

        ## Initialize save vectors
        P_m=  zeros(J)
        S_m =  zeros(J)

        ## Reset to pre-merger baseline
        f.P_j[:] = P_Base[:]
        evaluate_model!(m,f,"All",voucher=voucher,update_voucher=update_voucher)
        # Solve model in the affected states
        println("Begin Competitive Equilibrium Solution")
        solve_model!(m,f,shared_states,sim=sim,voucher=voucher,update_voucher=update_voucher)
        evaluate_model!(m,f,"All",voucher=voucher,update_voucher=update_voucher)
        prod_profits = product_profits(m,f,sim=sim)
        # merger_profits = market_profits(m,f)
        # println(merger_profits[shared_markets])
        P_m[:] = f.P_j[:]
        S_m[:] = f.S_j[:]

        # Output welfare
        ## ADD FIRM 1 FIRM 2 TAGS
        # println("Competitive Consumer Welfare...")
        # consumer_welfare(m,f,"$(file_stub)_$(merging_parties[1])_$(merging_parties[2])",spec,rundate)

        # println("Competitive Total Welfare...")
        # trash = total_welfare_bymkt(m,f,"$(file_stub)_$(merging_parties[1])_$(merging_parties[2])",spec,rundate,update_voucher=update_voucher)
        # println("Competitive Profits...")


        # Output equilibrium
        println("Output Competitive Results")
        file = "$(home_directory)/Research/Imperfect_Insurance_Competition/Estimation_Output/$(file_stub)_$(merging_parties[1])_$(merging_parties[2]).csv"
        output =  DataFrame(Product=prod_vec,
                            Price=P_m,
                            Lives=S_m,
                            Profit=prod_profits)
        CSV.write(file,output)

        println("Resolve Pre-merger Baseline")
        P_pre=  zeros(J)
        S_pre =  zeros(J)
        ownerMatrix!(f)
        solve_model!(m,f,shared_states,sim=sim,voucher=voucher,update_voucher=update_voucher)
        evaluate_model!(m,f,"All",voucher=voucher,update_voucher=update_voucher)
        prod_profits = product_profits(m,f,sim=sim)
        P_pre[:] = f.P_j[:]
        S_pre[:] = f.S_j[:]
        file = "$(home_directory)/Research/Imperfect_Insurance_Competition/Estimation_Output/$(file_stub)_PRE_$(merging_parties[1])_$(merging_parties[2]).csv"
        output =  DataFrame(Product=prod_vec,
                            Price=P_pre,
                            Lives=S_pre,
                            Profit=prod_profits)
        CSV.write(file,output)
        println("Saved Resolved File at $file")

        # ## Solve Profit-Constrained Social Planner Problem
        # println("Begin Profit Constrained Planner Solution")
        # f.P_j[:] = P_Base_SP_cp
        # markets_cp, λ_vec_cp = solve_SP_λ!(m,f,merger_profits,markets=shared_markets)
        # evaluate_model!(m,f,"All",voucher=true,update_voucher=false)
        #
        # # println("SP Consumer Welfare")
        # # consumer_welfare(m,f,"$(file_stub)_SP_cp_$(merging_parties[1])_$(merging_parties[2])",spec,rundate)
        # println("SP Total Welfare")
        # trash = total_welfare_bymkt(m,f,"$(file_stub)_SP_cp_$(merging_parties[1])_$(merging_parties[2])",spec,rundate,update_voucher=update_voucher)
        #
        # # Output Baseline Model
        # file = "$(home_directory)/Research/Imperfect_Insurance_Competition/Estimation_Output/$(file_stub)_SP_cp_$(merging_parties[1])_$(merging_parties[2]).csv"
        # output =  DataFrame(Product=prod_vec,
        #                     Price=f.P_j,
        #                     Lives=f.S_j)
        # CSV.write(file,output)
    end
    return nothing
end

function sendto(p::Int, args...)
    for (nm, val) in args
        @spawnat(p, eval(Main, Expr(:(=), nm, val)))
    end
end
function sendto(ps::Vector{Int}, args...)
    for p in ps
        println("Sending to worker $p...")
        sendto(p,args)
    end
end
