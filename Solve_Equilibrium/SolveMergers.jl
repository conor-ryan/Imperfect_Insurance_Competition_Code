function MergersMain(rundate,spec,home_directory)

    #Load Data
    println("Loading Data...")
    @everywhere include("EQ_load.jl")

    # df[:High_small] = df[:HighRisk].*df[:Small]

    mark_the_output_date = Dates.today()
    println("Running spec $rundate on $mark_the_output_date")

    @everywhere file = "$(home_directory)/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/MCestimation_stg2_$spec-$rundate.jld2"
    @everywhere @load file p_stg2 p_dem_est cost_spec spec_Dict
    @everywhere mc_est = copy(p_stg2)
    #### Load Estimation Results ####
    # file = "$(home_directory)/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/PLL_Estimate_$spec-$rundate-stg1.jld2"
    # @load file p_stg1 spec_Dict

    #### Build Model ####
    println("Rebuild Demand Model...")
    # Structre the data
    @everywhere     chdf = ChoiceData(df,df_mkt,df_risk,df_transfer;
        product =[:Product_std],
        demoRaw=spec_Dict["demoRaw"],
        prodchars=spec_Dict["prodchars"],
        prodchars_σ=spec_Dict["prodchars_σ"],
        fixedEffects=spec_Dict["fixedEffects"],
        wgt=[:PERWT])

    # Fit into model
    @everywhere model = InsuranceLogit(chdf,spec_Dict["haltonDim"])


    if length(p_dem_est)!=model.parLength[:All]
        println(length(p_dem_est))
        println(model.parLength[:All])
        error("Parameter Vector Not Quite Right")
    end

    println("Rebuild Cost Data...")

    @everywhere costdf = MC_Data(df,mom_firm,mom_metal,mom_age,mom_age_no,mom_risk,mom_ra;
                    baseSpec=cost_spec,
                    fixedEffects=[:Firm_ST],
                    constMoments=true)


    #### Compute Parameter Objects ####
    println("Compute Parameters...")
    @everywhere par_dem = parDict(model,p_dem_est,no2Der=true)
    @everywhere individual_values!(model,par_dem)
    @everywhere individual_shares(model,par_dem)

    @everywhere par_cost = parMC(mc_est,par_dem,model,costdf)


    println("####################################")
    println("#### Solve Policy Baseline  ####")
    println("####################################")
    filestub = "AllMergers_$spec-$(rundate)_Base"
    simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
                            filestub,policy="Base")
    # println("####################################")
    # println("#### Solve Without Risk Adjustment ####")
    # println("####################################")
    # filestub = "AllMergers_$spec-$(rundate)_RA"
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="RA_repeal")
    # println("####################################")
    # println("#### Solve Without Individual Mandate ####")
    # println("####################################")
    # filestub = "AllMergers_$spec-$(rundate)_Man"
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="Man_repeal")
    # println("####################################")
    # println("#### Solve Without Risk Adjustment nor Individual Mandate ####")
    # println("####################################")
    # filestub = "AllMergers_$spec-$(rundate)_RAMan"
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="RAMan_repeal")
    # println("####################################")
    # println("#### Solve Policy Baseline - Price Linked ####")
    # println("####################################")
    # filestub = "AllMergers_PL_$spec-$(rundate)_Base"
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="Base",voucher=false)
    # println("####################################")
    # println("#### Solve Without Risk Adjustment - Price Linked ####")
    # println("####################################")
    # filestub = "AllMergers_PL_$spec-$(rundate)_RA"
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="RA_repeal",voucher=false)
    # println("####################################")
    # println("#### Solve Without Individual Mandate - Price Linked ####")
    # println("####################################")
    # filestub = "AllMergers_PL_$spec-$(rundate)_Man"
    # simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
    #                         filestub,policy="Man_repeal",voucher=false)
    println("####################################")
    println("#### Solve Without Risk Adjustment nor Individual Mandate - Price Linked ####")
    println("####################################")
    filestub = "AllMergers_PL_$spec-$(rundate)_RAMan"
    simulate_all_mergers(model,df,eq_mkt,par_dem,par_cost,
                            filestub,policy="RAMan_repeal",voucher=false)

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


function check_if_merger(f::firmData,merging_parties)
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

function simulate_all_mergers(m::InsuranceLogit,
                            df::DataFrame,mkt::DataFrame,par_dem::parDict,par_cost::parMC,
                            file_stub;
                            policy="",voucher=true)

    # Initialize Firm Data
    f = firmData(m,df,mkt,par_dem,par_cost)

    # println("Send Data to Workers")
    # sendto(workers(),m,f)
    # println("Data Distributed")

    for w in workers()
        println("Sending data to $w...")
        tag = @spawnat w f;
        fetch(tag)
    end

    @everywhere test = f

    # Policy Regime
    if policy=="RA_repeal"
        @everywhere sim="RA"
    elseif policy=="Man_repeal"
        @everywhere sim = "Base"
        f.data[:,f.index[:Mandate]].=0.0
    elseif policy=="RAMan_repeal"
        @everywhere sim = "RA"
        f.data[:,f.index[:Mandate]].=0.0
    else
        @everywhere sim = "Base"
    end

    # Price link Regime
    update_voucher = !voucher

    # Initialize Vectors
    @everywhere J = maximum(m.prods)
    @everywhere prod_vec = zeros(J)
    @everywhere prod_vec[sort(m.prods)] = sort(m.prods)
    @everywhere P_Base = zeros(J)
    @everywhere S_Base = zeros(J)

    # # Solve Baseline Model
    # solve_model!(m,f,sim=sim,voucher=voucher)
    # evaluate_model!(m,f,"All",voucher=voucher)
    # set_voucher!(f,refund=true)
    # @everywhere P_Base[:] = f.P_j[:]
    # @everywhere S_Base[:] = f.S_j[:]
    #
    # consumer_welfare(m,f,"$(file_stub)_baseline")
    # trash = total_welfare_bymkt(m,f,"$(file_stub)_baseline",update_voucher=update_voucher)
    #
    # # Output Baseline Model
    # file = "$(home_directory)/Research/Imperfect_Insurance_Competition/Estimation_Output/$(file_stub)_baseline.csv"
    # output =  DataFrame(Product=prod_vec,
    #                     Price=P_Base,
    #                     Lives=S_Base)
    # CSV.write(file,output)


    # Iterate through all potential mergers
    unique_firms = sort(unique(f.firm_vector[f.firm_vector.!=""]))
    merging_party_list = Vector{Vector{String}}(undef,0)
    for (f_index,merge_party_2) in enumerate(unique_firms)
        for merge_party_1 in unique_firms[1:(f_index-1)]
            # Merging parties
            merging_parties = [merge_party_1,merge_party_2]

            # Skip if merging parties do not operate in same state
            shared_markets = check_if_merger(f,merging_parties)
            if length(shared_markets)==0
                continue
            end
            push!(merging_party_list,merging_parties)
        end
    end
    println("Send to Iteration Parameters to Workers")
    sendto(workers(),merging_party_list,file_stub,update_voucher)
    println("Data Distributed")
    @distributed for merging_parties in merging_party_list
        println(merging_parties)
        ## Set post-merger ownership matrix
        ownerMatrix!(f,merging_parties)

        ## Initialize save vectors
        P_m=  zeros(J)
        S_m =  zeros(J)

        ## Reset to pre-merger baseline
        f.P_j[:] = P_Base[:]
        # Solve model in the affected states
        solve_model!(m,f,shared_markets,sim=sim,voucher=voucher,update_voucher=update_voucher)
        evaluate_model!(m,f,"All",voucher=voucher,update_voucher=update_voucher)
        P_m[:] = f.P_j[:]
        S_m[:] = f.S_j[:]

        # Output welfare
        ## ADD FIRM 1 FIRM 2 TAGS
        consumer_welfare(m,f,"$(file_stub)_$(merging_parties[1])_$(merging_parties[2])")
        trash = total_welfare_bymkt(m,f,"$(file_stub)_$(merging_parties[1])_$(merging_parties[2])",update_voucher=update_voucher)

        # Output equilibrium
        file = "$(home_directory)/Research/Imperfect_Insurance_Competition/Estimation_Output/$(file_stub)_$(merging_parties[1])_$(merging_parties[2]).csv"
        output =  DataFrame(Product=prod_vec,
                            Price=P_m,
                            Lives=S_m)

        CSV.write(file,output)
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

#
# addprocs(3)
#
# @everywhere
#
# sendto
# Send an arbitrary number of variables to specified processes.
#
# New variables are created in the Main module on specified processes. The name will be the key of the keyword argument and the value will be the associated value.
#

# Examples
# # creates an integer x and Matrix y on processes 1 and 2
# sendto([1, 2], x=100, y=rand(2, 3))
#
# # create a variable here, then send it everywhere else
# z = randn(10, 10); sendto(workers(), z=z)
# getfrom
# Retrieve an object defined in an arbitrary module on an arbitrary process. Defaults to the Main module.
#
# The name of the object to be retrieved should be a symbol.
#
# getfrom(p::Int, nm::Symbol; mod=Main) = fetch(@spawnat(p, getfield(mod, nm)))
# Examples
# # get an object from named x from Main module on process 2. Name it x
# x = getfrom(2, :x)
# passobj
# Pass an arbitrary number of objects from one process to arbitrary processes. The variable must be defined in the from_mod module of the src process and will be copied under the same name to the to_mod module on each target process.
#
# function passobj(src::Int, target::Vector{Int}, nm::Symbol;
#                  from_mod=Main, to_mod=Main)
#     r = RemoteRef(src)
#     @spawnat(src, put!(r, getfield(from_mod, nm)))
#     for to in target
#         @spawnat(to, eval(to_mod, Expr(:(=), nm, fetch(r))))
#     end
#     nothing
# end
#
#
# function passobj(src::Int, target::Int, nm::Symbol; from_mod=Main, to_mod=Main)
#     passobj(src, [target], nm; from_mod=from_mod, to_mod=to_mod)
# end
#
#
# function passobj(src::Int, target, nms::Vector{Symbol};
#                  from_mod=Main, to_mod=Main)
#     for nm in nms
#         passobj(src, target, nm; from_mod=from_mod, to_mod=to_mod)
#     end
# end
# Examples
# # pass variable named x from process 2 to all other processes
# passobj(2, filter(x->x!=2, procs()), :x)
#
# # pass variables t, u, v from process 3 to process 1
# passobj(3, 1, [:t, :u, :v])
#
# # Pass a variable from the `Foo` module on process 1 to Main on workers
# passobj(1, workers(), [:foo]; from_mod=Foo)
