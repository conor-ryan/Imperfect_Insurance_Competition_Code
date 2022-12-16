function run_specification(df::DataFrame,
                            df_mkt::DataFrame,
                            df_risk::DataFrame;
                            haltonDim = 1,
                            spec_prodchars=[:Price,:MedDeduct,:High],
                            spec_prodchars_σ=[:PriceDiff],
                            spec_demoRaw=[:Age,:Family,:LowIncome],
                            spec_fixedEffects=[],
                            nested = false,
                            pre_calc=true)

    ## Build Model
    c_data = ChoiceData(df,df_mkt,df_risk;
        demoRaw=spec_demoRaw,
        prodchars=spec_prodchars,
        prodchars_σ=spec_prodchars_σ,
        fixedEffects=spec_fixedEffects)

    param_labels = vcat(String.(spec_demoRaw),String.(spec_prodchars),"Price:" .* String.(spec_demoRaw),"Variance:".*String.(spec_prodchars_σ),c_data.feNames)

    m = InsuranceLogit(c_data,haltonDim,nested=nested)

    println("Data Loaded")

    ## Initialize Starting Parameters
    #γ0start = rand(1)-.5
    γstart = rand(m.parLength[:γ])/10 .-.05
    β0start = rand(m.parLength[:β])/10 .-.05
    βstart = rand(m.parLength[:γ])/10 .- .05
    σstart = rand(m.parLength[:σ])/10 .- .05
    FEstart = rand(m.parLength[:FE])/100 .-.005

    #p0 = vcat(γ0start,γstart,β0start,βstart,σstart,FEstart)
    p0 = vcat(γstart,β0start,βstart,σstart,FEstart)
    p0 = zeros(length(p0))

    if pre_calc
        println("Compute No Heterogeneity Parameters")
        ## Compute Non_heterogeneity Starting Point
        ll_res = run_specification(df,df_mkt,df_risk,
                        haltonDim = 1,
                            spec_prodchars = spec_prodchars,
                            spec_prodchars_σ= Vector{Symbol}(undef,0),
                            spec_demoRaw=spec_demoRaw,
                            spec_fixedEffects=spec_fixedEffects,
                            nested = nested,
                            pre_calc=false)

        x_est = ll_res[1]
        ind1 = 1:(length(p0) - m.parLength[:FE] - m.parLength[:σ])
        ind2 = (length(p0) - m.parLength[:FE] + 1):m.parLength[:All]
        p0[ind1] = x_est[ind1]
        p0[ind2] = x_est[ind2 .- m.parLength[:σ]]
        # indσ = (maximum(ind1) + 1):(minimum(ind2) - 1)
        # p0[indσ].= rand(m.parLength[:σ])/10 .+ 1e-4
    end

    println("Begin Estimation")

    ## Estimate
    p_est, fval = newton_raphson_ll(m,p0)
    return p_est, m, fval,param_labels
end

function res_process_ll(model::InsuranceLogit,p_est::Vector{Float64})
    ## Create Param Dictionary

    paramFinal = parDict(model,p_est,no2Der=true)
    Pop =sum(weight(model.data).*choice(model.data))
    #### Likelihood Errors ####
    # AsVar_ll = calc_Avar(model,paramFinal)
    hess = Matrix{Float64}(undef,length(p_est),length(p_est))
    grad = Vector{Float64}(undef,length(p_est))
    res = log_likelihood!(hess,grad,model,p_est)
    AsVar_ll = -inv(hess)./Pop
    if any(diag(AsVar_ll.<0))
        println("Some negative variances")
        stdErr = sqrt.(abs.(diag(AsVar_ll)))
    else
        stdErr = sqrt.(diag(AsVar_ll))
    end


    t_stat = p_est./stdErr

    stars = Vector{String}(undef,length(t_stat))
    for i in 1:length(stars)
        if abs(t_stat[i])>2.326
            stars[i] = "***"
        elseif abs(t_stat[i])>1.654
            stars[i] = "**"
        elseif abs(t_stat[i])>1.282
            stars[i] = "*"
        else
            stars[i] = ""
        end
    end

    return AsVar_ll, stdErr, t_stat, stars
end



function res_process_GMM(model::InsuranceLogit,p_est::Vector{Float64})
    ## Create Param Dictionary

    paramFinal = parDict(model,p_est,no2Der=true)

    #### GMM Errors ####
    V1 = calc_mom_Avar(model,p_est)
    V2 = integration_var_bootstrap(model,p_est;n=500)

    ## Derivative of Moments wrt Parameters
    grad = Vector{Float64}(undef,length(p_est))
    hess = Matrix{Float64}(undef,length(p_est),length(p_est))
    ll = log_likelihood!(hess,grad,model,paramFinal)
    mom_grad = Matrix{Float64}(undef,length(p_est),length(model.data.tMoments))
    mom = calc_risk_moments!(mom_grad,model,paramFinal)
    G = hcat(mom_grad,hess)

    ## Calculate Variance
    AsVar = inv(G*inv(V1 + V2)*G')
    N = length(d.data._personIDs)
    V = Avar./N

    if any(diag(AsVar.<0))
        println("Some negative variances")
        stdErr = sqrt.(abs.(diag(AsVar)))
    else
        stdErr = sqrt.(diag(AsVar))
    end


    t_stat = p_est./stdErr

    stars = Vector{String}(undef,length(t_stat))
    for i in 1:length(stars)
        if abs(t_stat[i])>2.326
            stars[i] = "***"
        elseif abs(t_stat[i])>1.654
            stars[i] = "**"
        elseif abs(t_stat[i])>1.282
            stars[i] = "*"
        else
            stars[i] = ""
        end
    end

    return S,G,AsVar, stdErr, t_stat, stars
end


function run_specification_GMM(filename::String,
                            rundate,
                            df::DataFrame,
                            df_mkt::DataFrame,
                            df_risk::DataFrame;
                            haltonDim = 1,
                            spec_prodchars=[:Price,:MedDeduct,:High],
                            spec_prodchars_σ=Vector{Symbol}(undef,0),
                            spec_demoRaw=[:Age,:Family,:LowIncome],
                            spec_fixedEffects=Vector{Symbol}(undef,0),
                            nested = false)

    spec_Dict = Dict("prodchars" => spec_prodchars,
    "prodchars_σ"=> spec_prodchars_σ,
    "demoRaw"=>spec_demoRaw,
    "fixedEffects"=>spec_fixedEffects,
    "nested"=>nested,
    "haltonDim"=>haltonDim)

    cd("$(home_directory)Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/")
    ## Build Log_Likehood Model
    println("Build LL Model")
    # c_ll = ChoiceData(df,df_mkt,df_risk;
    #     demoRaw=spec_demoRaw,
    #     prodchars=spec_prodchars,
    #     prodchars_σ=Vector{Symbol}(undef,0),
    #     fixedEffects=spec_fixedEffects)
    #
    # m_ll = InsuranceLogit(c_ll,1,nested=nested)
    #
    # ## Initialize Starting Parameters
    # γstart = rand(m_ll.parLength[:γ])/10 .-.05
    # β0start = rand(m_ll.parLength[:β])/10 .-.05
    # βstart = rand(m_ll.parLength[:γ])/10 .- .05
    # σstart = rand(m_ll.parLength[:σ])/10 .- .05
    # FEstart = rand(m_ll.parLength[:FE])/100 .-.005
    #
    # p0 = vcat(γstart,β0start,βstart,σstart,FEstart)
    # println("#### Estimate LL Starting Point ####")
    #
    # ## Estimate
    # p_ll, fval = newton_raphson_ll(m_ll,p0)
    #
    # println("Save LL Result")
    # file = "$filename-$rundate-ll.jld2"
    # @save file p_ll spec_Dict

    # println("Load LL Result")
    # file = "GMM_Estimate_FMC-2019-08-11-ll.jld2"
    # @load file p_ll spec_Dict



    ## Build GMM Model
    println("Build GMM Model")
    c_data = ChoiceData(df,df_mkt,df_risk;
        demoRaw=spec_demoRaw,
        prodchars=spec_prodchars,
        prodchars_σ=spec_prodchars_σ,
        fixedEffects=spec_fixedEffects)

    param_labels = vcat(String.(spec_demoRaw),String.(spec_prodchars),"Price:" .* String.(spec_demoRaw),"Variance:".*String.(spec_prodchars_σ),c_data.feNames)

    m_GMM = InsuranceLogit(c_data,haltonDim,nested=nested)

    # Initialize Starting Parameters
    ind1 = 1:(m_GMM.parLength[:γ]*2+m_GMM.parLength[:β])
    ind2 = (1 + maximum(ind1) + m_GMM.parLength[:σ]):m_GMM.parLength[:All]
    σ_ind = (1 + maximum(ind1)):(minimum(ind2)-1)


    p0 = zeros(m_GMM.parLength[:All])
    println(length(p0))
    println(length(p_ll))
    println(m_GMM.parLength[:All])
    println(m_GMM.parLength[:σ])

    p0[ind1] = p_ll[ind1]
    p0[ind2] = p_ll[ind2.-m_GMM.parLength[:σ]]

    println("#### Estimate GMM First Stage ####")
    # if spec_fixedEffects== [:Firm_Market_Cat]
    #     p0[σ_ind] = [0.534812, 0.497257, 0.0252315, -0.0605508, 0.144096]
    # end
    # file = "$(homedir())/Documents/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/checkin_265.jld2"
    # @load file p_vec
    # p0 = copy(p_vec)
    W = Matrix(1.0I,m_GMM.parLength[:All]+length(m_GMM.data.tMoments),m_GMM.parLength[:All]+length(m_GMM.data.tMoments))
    flag = ""
    p_stg1 = similar(p0)
    obj_1 = 0.0
    while flag!="converged"
        p0[σ_ind]=rand(length(σ_ind)).*0.1 .- .05
        p_stg1, obj_1, flag = two_stage_est(m_GMM,p0,W)
    end

    println("Save First Stage Result")
    file = "$filename-$rundate-stg1.jld2"
    @save file p_stg1 obj_1 spec_Dict

    println("Load First Stage Result")
    file = "$filename-$rundate-stg1.jld2"
    @load file p_stg1 obj_1 spec_Dict


    println("#### Estimate GMM Second Stage ####")
    mom_pars = vcat(1:length(m_GMM.data.tMoments),(length(m_GMM.data.tMoments)).+σ_ind)
    mom_ind = 1:length(m_GMM.data.tMoments)
    S = calc_mom_Avar(m_GMM,p_stg1)
    S_mom = S[mom_pars,mom_pars]
    diag_sigma = Diagonal(diag(S_mom))
    S_mom[mom_ind,:] .= 0.0
    S_mom[:,mom_ind] .= 0.0
    S_mom[mom_ind,mom_ind] = diag_sigma[mom_ind,mom_ind]
    W2 = inv(S_mom)
    W[mom_pars,mom_pars] = W2[:,:]


    println(S[mom_pars,mom_pars])
    println(W2)
    println(W[mom_pars,mom_pars])

    ## Estimate
    flag = ""
    p_stg2 = similar(p0)
    obj_2 = 0.0

    # Start at p_stg1, should add another starting point for robustness.
    p0[σ_ind]=rand(length(σ_ind)).*0.1 .- .05
    p_stg2, obj_2, flag = two_stage_est(m_GMM,p_stg1,W)
    # p_stg2 = copy(p_stg1)
    # obj_2 = copy(obj_1)
    println("Save Second Stage Result")
    file = "$filename-$rundate-stg2.jld2"
    # @save file p_stg2 obj_2 spec_Dict
    @load file p_stg2 obj_2 spec_Dict

    println("#### Calculate Standard Errors and Save Results ####")
    # AsVar, stdErr,t_stat, stars = GMM_var(m_GMM,p_stg2)
    AsVar, stdErr,t_stat, stars = res_process_ll(m_GMM,p_stg2)

    out1 = DataFrame(labels=param_labels,pars=p_stg2,se=stdErr,ts=t_stat,sig=stars)
    file1 = "$filename-$rundate-test.csv"
    CSV.write(file1,out1)

    out2 = DataFrame(delta=m_GMM.deltas,prods=m_GMM.prods)
    file2 = "$filename-$rundate-deltas-test.csv"
    CSV.write(file2,out2)

    return nothing
end


function estimate_demand(filename,rundate,home_directory,
                    haltonDim,
                    spec_demoRaw,
                    spec_prodchars,
                    spec_prodchars_σ,
                    spec_fixedEffects)
    # Load the Data
    println("Loading Data...")
    codeDir = "$(home_directory)/Research/Imperfect_Insurance_Competition/Code/Demand_Estimation"
    include("$codeDir/load.jl")


    ### Run Specification 1 ####
    println("#### Run Specification ####")
    spec1 = run_specification_penalizedlikelihood(filename,rundate,
                        df,df_mkt,df_risk,df_transfer,
                        haltonDim = halton_draws,
                        spec_demoRaw=spec_demoRaw,
                        spec_prodchars=spec_prodchars,
                        spec_prodchars_σ=spec_prodchars_σ,
                        spec_fixedEffects=spec_fixedEffects)
        return nothing
end



function run_specification_penalizedlikelihood(filename::String,
                            rundate,
                            df::DataFrame,
                            df_mkt::DataFrame,
                            df_risk::DataFrame,
                            df_transfer::DataFrame;
                            haltonDim = 1,
                            spec_prodchars=[:Price,:MedDeduct,:High],
                            spec_prodchars_σ=Vector{Symbol}(undef,0),
                            spec_demoRaw=[:Age,:Family,:LowIncome],
                            spec_fixedEffects=Vector{Symbol}(undef,0),
                            nested = false)

    spec_Dict = Dict("prodchars" => spec_prodchars,
    "prodchars_σ"=> spec_prodchars_σ,
    "demoRaw"=>spec_demoRaw,
    "fixedEffects"=>spec_fixedEffects,
    "nested"=>nested,
    "haltonDim"=>haltonDim)

    cd("$(home_directory)/Research/Imperfect_Insurance_Competition/Intermediate_Output/Estimation_Parameters/")

    println("Build LL Model - Fixed Effects Starting Point")
    c_ll = ChoiceData(df,df_mkt,df_risk,df_transfer;
        demoRaw=spec_demoRaw,
        prodchars=spec_prodchars,
        prodchars_σ=Vector{Symbol}(undef,0),
        fixedEffects=spec_fixedEffects)

    m_ll = InsuranceLogit(c_ll,1,nested=nested)

    ## Initialize Starting Parameters
    γstart = rand(m_ll.parLength[:γ])/10 .-.05
    β0start = rand(m_ll.parLength[:β])/10 .-.05
    βstart = rand(m_ll.parLength[:γ])/10 .- .05
    σstart = rand(m_ll.parLength[:σ])/10 .- .05
    FEstart = rand(m_ll.parLength[:FE])/100 .-.005

    fe_length = m_ll.parLength[:FE]
    p0 = vcat(γstart,β0start,βstart,σstart,FEstart)
    println("#### Estimate LL Starting Point ####")

    ## Estimate
    W = zeros(length(m_ll.data.rMoments),length(m_ll.data.rMoments))
    p_ll, fval = newton_raphson_ll(m_ll,p0,W)


    println("Save LL Result")
    file = "$filename-$rundate-ll.jld2"
    @save file p_ll spec_Dict
    # @load file p_ll spec_Dict

    ## Build GMM Model
    println("Build Model")
    c_data = ChoiceData(df,df_mkt,df_risk,df_transfer;
        demoRaw=spec_demoRaw,
        prodchars=spec_prodchars,
        prodchars_σ=spec_prodchars_σ,
        fixedEffects=spec_fixedEffects)
    param_labels = vcat(String.(spec_demoRaw),String.(spec_prodchars),"Price:" .* String.(spec_demoRaw),"Variance:".*String.(spec_prodchars_σ),c_data.feNames)

    m_ll = InsuranceLogit(c_data,haltonDim,nested=nested)

    # Initialize Starting Parameters

    fe_length = m_ll.parLength[:FE]
    ind1 = 1:(length(spec_demoRaw)+length(spec_prodchars)+length(spec_demoRaw))
    σ_ind = (1 + maximum(ind1)):(maximum(ind1)+length(spec_prodchars_σ))
    ind2 = (1 + maximum(σ_ind)):( maximum(σ_ind)+length(c_data.feNames))


    p0 = rand(m_ll.parLength[:All]).*1.0 .- 0.5
    p0[ind1] = p_ll[ind1]
    p0[σ_ind]=zeros(length(σ_ind))#.*0.1 .- .05
    p0[ind2] = p_ll[(length(p_ll)-fe_length+1):length(p_ll)]
    println("Starting vector: $p0")

    #
    # println("Risk Parameter Experiment")
    # for risk_val in [-1,-0.5,-0.2,0.0,.1,.3,.5,.7,.9,1.5,3.0,6.0,9.0,12.0,15.0]
    #     println("Trying Risk Parameters: $risk_val")
    #     p0[[25,26]].=risk_val
    #     p = parDict(m_ll,p0,no2Der=false)
    #     individual_values!(m_ll,p)
    #     individual_shares(m_ll,p)
    #     moms = calc_risk_moments(m_ll,p)
    # end

    println("#### Estimate First Stage ####")
    W = -Matrix(1.0I,length(m_ll.data.rMoments),length(m_ll.data.rMoments))
    W[1,1] = -5.0
    W[2,2] = -5.0
    W[3,3] = -5.0
    W[4,4] = -5.0
    W[5,5] = -5.0
    W = W./10

    # p_init, obj_init = gradient_ascent_ll(m_ll,p0,W,max_itr=20)
    # p_stg1, obj_1 = newton_raphson_ll(m_ll,p_init,W,grad_tol=1e-9,f_tol=1e-10,x_tol=1e-10)

    println("Save First Stage Result")
    # file = "$filename-$rundate-stg1.jld2"
    # @save file p_stg1 obj_1 spec_Dict

    # println("Load First Stage Result")
    # file = "$filename-$rundate-stg1.jld2"
    # @load file p_stg1 obj_1 spec_Dict


    println("#### Estimate GMM Second Stage ####")

    mom_pars = vcat(1:length(m_ll.data.rMoments),(length(m_ll.data.rMoments)).+σ_ind)
    mom_ind = 1:length(m_ll.data.rMoments)
    S = calc_mom_Avar(m_ll,p_stg1)
    S_mom = S[mom_pars,mom_pars]
    diag_sigma = Diagonal(diag(S_mom))
    println(diag(diag_sigma))
    S_mom[mom_ind,:] .= 0.0
    S_mom[:,mom_ind] .= 0.0
    S_mom[mom_ind,mom_ind] = diag_sigma[mom_ind,mom_ind]
    S_diag = Matrix(Diagonal(diag(S[mom_ind,mom_ind])))
    # W2 = inv(S_mom)
    # W[mom_pars,mom_pars] = W2[:,:]
    println(size(diag_sigma))
    println(size(S_mom))
    println(size(S_diag))
    println(size(W))
    W = -inv(S_diag)
    println(size(W))
    W = W*10

    # V = risk_moment_bootstrap(m_ll,p_stg1)
    # pop =sum(weight(m_ll.data).*choice(m_ll.data))
    # J = length(m_ll.data.rMoments)
    # W = -Matrix{Float64}(Diagonal(1 ./diag(V))./(pop*J))
    # println(diag(W))
    # W = - inv(V)

    ## Estimate
    p0 = rand(m_ll.parLength[:All]) .- 0.5
    p0[σ_ind]=rand(length(σ_ind)).*0.1 .- .05




    # p_init, obj_init = gradient_ascent_ll(m_ll,p_stg1,W,max_itr=50)
    # p_stg2, obj_2 = newton_raphson_ll(m_ll,p_init,W,grad_tol=1e-9,f_tol=1e-10,x_tol=1e-10)
    p_stg2 = [-0.263512362990967, -0.4096320231973833, -0.2752366803351952, -0.8302764911836378, -3.4276685550998223, -1.3059671656272547, 11.3216028885282, 0.2725385109380722, 0.4644789374634316, 0.762099632049196, 0.0376656788074534, 0.31603802911627177, 0.5377876736467336, -0.3524613506141601, -0.3226051205333672, -0.4018276157261243, -0.4019978554904007, -0.3638236808901147, -0.36523107262508425, -0.34964735648998274, -0.3804014266375524, -0.35392755117820845, -0.3676310246029481,
    -0.3633987997484457, -0.345883859544397, -0.30435077339155236, -0.2858295630775923, -0.4219312811183318, -0.3832307022443132, -0.34062027744645385, -0.34028627979045933, -0.2501380831763344, -0.3756109659635782, -0.3328588420452299, -0.3935315363846405, -0.38883388450826234, -0.29527901443616583, -0.3677133167243446, -0.3218686185513261, -0.40174589646170533, -0.37005705811864803, -0.32951007764757473, -0.3755106629742923, -0.42694157654278486, -0.42430600513836336, -0.3993884313427046,
    -0.44525471186205007, -0.41009786669771564, -0.3915892094479768, -0.3576193863039122, -0.32017068220919076, -0.34265370072370527, -0.36271025083771585, -0.3729233592676128, -0.3834315997697332, -0.2777727335756565, -0.27869829693901166, -0.2406622510822558, -0.36003035403668077, -0.3781487577361623, -0.36464055614945784, -0.3876543427546743, -0.40644014013355995, -0.4315770737158029, -0.4512989475450376, -0.22795283913103742, -0.3503869979571075, -0.3082655921422579, -0.3781303050459966,
    -0.11904681008683565, 0.015868825053322166, -0.0037329516614773048, -0.06223110877753269, -0.04112803477039401, -0.0020960902863822258, -0.03432338885265029, -0.0020462961169812276, -0.35244465787818696, -0.4125186763914106, -0.2745010554461425, -0.34677528173063077, -0.35411914520907245, -0.3226485415132889, -0.34727480603717636, -0.3791213350969742, -0.3939850029371484, -0.38654911291239025, -0.36812458489591987, -0.26951553108173903, -0.2885597311036899, -0.26671259762954025,
    -0.3241766803131753, -0.2882652310735929, -0.23129565001262495, -0.24862937393398607, -0.3108947813048093, -4.652396734072806, -3.9303834393514574, -4.474419046827809, -3.7431426902145954, -4.365663674996583, -3.218440656211901, -6.711768123179936, -6.876027187696111, -5.110934071795587, -8.633992207128955, -6.422191514149399, -8.159849431004696, -7.4009967610249054, -7.2581868580645565, -6.138801104376588, -7.089394346919123, -4.673040347422053, -7.3487962179546, -6.241722028707812,
    -7.156210689511287, -5.1073371970628925, -5.604142177828377, -7.326295665627772, -5.5278098428372235, -5.060588555218318, -7.444109000700939, -5.794373949041569, -5.931380209283612, -6.985741192491205, -5.530900927986397, -8.303532633157277, -5.758068641474126, -6.838087319637551, -5.368616428618006, -7.304037924486477, -5.727280522134443, -7.71750970338799, -6.107817215391524, -5.270274225145333, -7.475806212582352, -6.358788863448637, -8.246068077084445, -7.645302601016119,
    -6.472825369825518, -7.419979091560009, -8.60400839226412, -5.946820352289676, -7.774530582515263, -5.586682599437571, -7.229068023302023, -8.48877800644369, -8.083846756601337, -6.879455143062517, -6.30422743166714, -4.463399067416567, -4.536153687740096, -7.024560728769443, -5.6027057377518075, -6.527493472383021, -4.991663106683474, -4.965629993629646, -6.762357441342537, -5.291714975914157, -7.790872771435962, -7.202111884282675, -4.290482192689122, -6.581383156036041,
    -6.784890190577824, -5.925125098712399, -4.751024034327561, -7.189368292164363, -5.748345007480914, -5.144193175420298, -5.6858479504673625, -7.143603892832769, -3.727818125104416, -4.003093914258874, -3.623477751362315, -4.0189661080041565, -3.2445292320303944, -4.146807091590703, -3.537974750871824, -4.363366144880858, -3.7021660291600735, -4.33883357822985, -3.2957249180323367, -4.304569369892302, -3.43439061528607, -3.742380229621365, -2.954548080954775, -4.176133420833396,
    -4.668885728717963, -5.149661663927515, -3.163775932418818, -6.386227355023284, -4.89116912392716, -8.117283066798926, -4.392805819232731, -4.582234838578898, -6.086579807359521, -7.124830255550603, -4.105530258618601, -5.423938173583842, -6.389110339886822, -4.746192964247746, -6.885718992961968, -9.166737980028355, -9.157429997497704, -4.505779191869025, -9.397115601390517, -8.030955332573498, -7.038626987751831, -9.0771911712918, -9.102781828590297, -3.6693941513381136,
    -4.006907385089879, -5.343328705057276, -9.399171293065956, -3.717201220748685, -7.69957276369195, -6.85766295262674, -8.094532103229925, -4.425165352054513, -6.561812704790002, -7.269710803757137, -4.702295491211145, -3.5720255678318504, -5.760046704426847, -6.638919757706103, -7.031967304011931, -5.380524502053676, -6.406319852675802, -3.7636249063958482, -6.2149964862423825, -6.33721970010426, -4.379677252565475, -7.276096236601326, -8.649433445560318, -3.5377784147219815,
    -6.255969773537286, -6.668182283983068, -6.197861692872687, -7.1483844023233205, -7.503311234973283, -3.8629631775148767, -4.8951768297140354, -1.8530896330502589, -5.823262701194108, -3.500757016463122, -5.313248824621975, -6.16601521237865, -7.8271147243682115, -4.415161055668236, -7.953624477586258, -4.916241076846813, -7.076216961060425, -4.471789850246998, -7.987667313979114, -5.488820257158001, -7.527982257939728, -4.686751091363522, -7.963688717083315, -6.804388889110676,
    -3.9230225910916743, -7.309333998913509, -5.443277002175724, -6.340694650069541, -7.697987317923039, -5.887979263719335, -3.633841946082925, -7.0205520525424205, -4.9231755325431905, -7.88618499452429, -3.844012927224562, -4.270272597300628, -7.96736333791673, -4.160488344623109, -7.549306772699526, -4.913425124730758, -7.851415837509516, -5.379739506602744, -3.1291600699637105, -6.6262927455611935, -9.398709623304015, -9.461988559759138, -4.373714380754128, -7.240204711838611,
    -7.211406192034125, -7.026750035311236, -7.9812205117838815, -7.819488880066977, -8.155823118442687, -9.03351692724531, -7.565804940959762, -3.699910298605943, -6.6530090343076225, -6.53587758143492, -5.918154405588465, -7.8416062359617555, -7.325205594760504, -6.903262822575746, -6.77862324464384, -5.263387412812204, -6.368789864463115, -4.401136544874852, -7.845536932099305, -5.457784670545329, -6.771322366462646, -5.5506039319671485, -8.283843757149263, -9.00463336153541,
    -3.8867711322146747, -8.103870110982077, -5.730845752486005, -8.46458093111181, -7.897652157170464, -4.207780376007109, -7.2584103854516835, -6.595213091090581, -8.122481646874068, -5.157726458376687, -8.161240211259148, -7.9417649949880325, -4.286765718996596, -6.487838599506457, -4.9137953859775925, -5.356430624486025, -5.397226033243599, -6.545301616195004, -7.741543821250737, -6.158784335186525, -4.455392085291084, -5.353076246026617, -6.661815548712362, -6.996207635407815,
    -4.486154416765332, -5.98112718086352, -7.201245695695926, -6.520792902620048, -4.638219260177795, -5.552019472808721, -6.499797376937167, -8.216331820555062, -5.37468270686798, -4.461704992898317, -5.743657057811823, -5.970478921738742, -4.730035773355919, -5.370182364449913, -5.282245394686835, -7.213814145587838, -5.215024950662044, -6.244508650282401, -5.858661803421929, -6.47763478449001, -5.009861708682521, -9.633693959124507, -7.23246601457411, -5.889232949283281, -5.188260120687595,
     -6.3258127285267935, -6.2607007139362985, -5.643917718154194, -4.928617670118209, -8.111166789861736, -7.12172973477447, -4.582867644232888, -5.812157733876047, -7.801188623236453, -6.036651385980831, -3.5558167875141544, -5.10690712077186, -4.134815355873819, -5.024625260378143, -3.101842267740719, -4.665110763845668, -3.482902289426368, -5.398904493960989, -3.233537313683869, -4.70387949376147, -3.91727766195888, -5.236635281826966, -3.9294860550634994, -3.8615376681992717,
     -5.42082176357988, -4.684652785400896, -6.99845804811196, -5.873218250361249, -6.3334456051351955, -7.30200940842422, -8.607394396236892, -7.689271845208502, -7.270299320929253, -4.787713225190252, -6.36633167483055, -5.535832477194899, -6.9881912428225, -4.2947806898241385, -6.400035987591171, -5.6848852333800135, -6.269801017464072, -5.234169179438596, -7.055886749525917, -5.585568113870034, -6.42539445639045, -5.2533602827986225, -6.981799073977348, -3.2599928463135894,
     -5.231922602638209, -5.093812974558006, -4.651760609747401, -8.068360509881863, -11.207759636095137, -5.031183527971698, -9.986782922436396, -8.063692958174656, -5.123416136081152, -9.340157452539826, -8.346448663298045, -8.259556181908271, -8.05964317268083, -4.860731144978367, -4.030365228602192, -4.210771765611813, -6.6639649557488365, -3.486784749868944, -5.071588490312729, -0.5016644939769691, -6.378543624982981, -3.5703370715746634, -4.290179803955005, -5.529254422272926,
     -5.000548987096891, -5.836788211758476, -5.0469275771391375, -3.4356175834787925, -3.982840054559979, -5.808230541134717, -4.643702947068142, -6.015758370553083, -7.848281380209043, -3.5791623481182597, -3.53766440314781, -5.616945068881917, -5.639902921211611, -5.7570464798297385, -5.919675389652301, -3.4807365865372164, -4.087371079718007, -5.479115994244907, -6.240967406600873, -0.40858880068025816, -3.197854118316773, -3.7535605765876086, -5.647242420888786, -4.201613802121147,
     -5.4899975210619685, -7.668480518376888, -7.139713765915638, -9.92979020333382, -4.9351306604104535, -8.845086805636456, -8.814922133257978, -11.037075131354362, -11.383549494571213, -8.764104357562191, -6.386766920643313, -4.818037344411495, -4.297768524834847, -6.619662770835643, -4.394516381027319, -7.4142146966382665, -8.601633230357645, -5.099158321587434, -9.198077708959678, -4.622917401314616, -6.920729276931488, -7.483388200895654, -7.377043734295306, -9.37408137603602,
     -6.0145959532346405, -9.158396047258787, -9.931987984463365, -8.555991770076195, -6.830654349070163, -5.554619084316821, -9.596146620595425, -8.147671573632241, -4.2687583488693, -5.701269785394842, -6.492618973122678, -6.931991486227572, -6.779455793635499, -8.84178617997528, -4.9743473709606985, -5.568499732356297, -7.472164194319656, -4.4129016308763775, -6.3071290323246485, -6.645491571860618, -5.78794485995327, -11.582837177957726, -4.356755132304743, -7.40362662687817,
     -6.523415089319099, -3.9945133802713335, -6.597967780411276, -7.76038693307263, -8.269950020664542, -5.760298565271879, -5.8272864545389504, -6.508986231605158, -6.54296072652179, -7.921305421117273, -4.164428692912766, -8.58882134159582, -10.009001027448791, -4.785457421512638, -8.546639681075037, -8.532057357377534, -7.298399793197361, -10.444699715216851, -8.448321487826034, -8.281651604876762, -4.300487744856811, -4.985813658190207, -6.678134521000059, -7.188871541599469,
     -6.87430673183626, -7.089025070289217, -8.798804992034755, -7.8767848602013375, -5.158717935198119, -7.849325524988768, -5.591000394186581, -6.438454584274274, -7.924746237162765, -7.964135388083209, -5.782115035015473, -9.062781738634342, -11.126119452979541, -8.121729094964733, -5.632856570829337, -9.505187619621616, -6.583387625616192, -8.056081040707507, -7.844698306325801, -5.372148681047383, -6.275141281365134, -7.8860485839541585, -7.930008901040126, -7.008258498878225,
     -10.513274788191296, -4.820304978431426, -7.639080769247948, -9.82825010319152, -7.915372345008673, -8.331744357644318, -8.625480223662079, -5.619246122867233, -7.749876718225635, -4.183307771401904, -8.731501807542218, -8.15858103366272, -5.439554360707454, -5.539399352845842, -5.357518771217419, -7.095497008821543, -4.37696064268787, -4.035088031240001, -6.707417027380674, -7.491577942423933, -7.005467433078669, -6.123419768749683, -8.14651215687013, -8.284706951336407,
     -6.136124548625034, -5.412897472054516, -8.602952675475395, -7.144900590985731, -6.596791283099654, -7.928478584103261, -6.500094266633931, -8.035325559306536, -6.206553004203922, -4.953069565644249, -8.273164834101301, -6.643851911251414, -5.96150193663356, -7.627833088166842, -8.069613994462275, -7.841687156049421, -5.5927405479192736, -4.711145609760017, -7.4403087864895605, -7.346167033203387, -6.423092447562797, -7.589601722315798, -9.304141244726027, -6.721933147709274,
     -6.549125303182821, -9.823485417776661, -5.487095824126405, -7.361476983100742, -6.123689916664295, -5.389468196044278, -6.2173535461757785, -1.9243775153549776, -2.0336625710588416, -2.2414959466316033, -2.3208513098434347, -2.5064618572607507, -1.6688359676657913, -1.984621423251419, -2.410425674717933, -2.238567281894505, -2.2910671372805202, -2.3874018547322686, -2.922321766864018, -1.407845222852044, -1.8406708898023185, -2.1355711383891514, -1.6638405642026075, -1.7007870862594365,
     -1.6319227057293666, -1.9059183771770176, -2.051095114004089, -1.9100239059298174, -1.729165439702645, -1.2368724084387888, -2.8459319939244234, -1.7975648195664504, -3.088852089144673, -1.5630501169802284, -3.135342830341015, -1.8944212928945205, -3.024346437005793, -1.7051238823728156, -1.6482088921591425, -2.270083363254046, -2.15588843799357, -2.329341196630965, -2.1515177793905034, -2.315975478764601, -2.3434348455582, -1.1251255608677373, -1.800068459227077, -2.1764375741193804,
     -2.17485521183221, -2.333661594097531, -1.9942581558698225, -1.5961644905157129, -2.31764539954739, -2.313725688750917, -2.13700425710832, -1.1341008935417194, -1.4962242190213408, -1.9063072983120113, -2.376365028390945, -2.1806122063971523, -1.8468064595239249, -2.4980410367265615, -2.095034311072904, -2.268162384417132, -1.2819335228323974, -1.980670336118389, -1.7363486698451558, -1.194353322216763, -2.524973785343485, -2.353832516245558, -1.871711870032024, -1.205660663207993,
     -2.277501920417656, -1.9414436426810062, -2.4635074072903747, -1.8422131927159777, -1.5456613146216658, -1.669541361978322, -1.6528737207686341, -1.7340681189368436, -2.0072968517755334, -2.916007341893423, -1.776297770078535, -1.5181723372933384, -1.554851733593304, -2.549276480992448, -2.157416476721383, -1.7842802531853206, -1.7384929522739005, -1.8805659418753995, -2.5482580082973554, -1.8368942625376894, -1.8125472980346808, -2.501152267549166, -1.5587336759224886,
     -1.9156343652396268, -2.227236059651376, -2.2794266830290786, -2.2953790061500636, -2.0856959750760433, -2.027327873190255, -2.1189831004983333, -2.544371817643287, -2.0939081866724383, -1.9677179229204864, -2.344283318815098, -2.1766669404969496, -1.6123573613670146, -1.3405592600163942, -0.9956559276455377, -1.5916617351997064, -1.4329791737242898, -0.7700232434163937, -1.5720302148694998]

     obj_2 = -2.7
    println("Save Second Stage Result")
    file = "$filename-$rundate-stg2.jld2"
    @save file p_stg2 obj_2 spec_Dict
    # @load file p_stg2 obj_2 spec_Dict


    println("#### Calculate Standard Errors and Save Results ####")
    AsVar, stdErr,t_stat, stars = GMM_var(m_ll,p_stg2)
    # AsVar, stdErr,t_stat, stars = res_process_ll(m_GMM,p_stg2)

    out1 = DataFrame(labels=param_labels,pars=p_stg2,se=stdErr,ts=t_stat,sig=stars)
    file1 = "$filename-$rundate.csv"
    CSV.write(file1,out1)

    return nothing
end
