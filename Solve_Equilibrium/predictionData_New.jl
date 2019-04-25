mutable struct EqData{T}
    # Choice Data
    data::InsuranceLogit
    #Demand Estimates
    dem_est::parDict{T}
    # Cost Estimates
    mc_est::parMC{T}

    # Product Data
    p_avg::Matrix{Float64}
    p_index::Dict{Symbol, Int}
    # Products
    prods::Vector{Int64}

    # Firm Prices
    premBase_j::Vector{Float64}
    # Firm Base Cost
    Cost_base_j::Vector{Float64}
    C_AV_j::Vector{Float64}

    #Ownership Matrix
    ownMat::Matrix{Float64}
    ownMat_merge::Matrix{Float64}

    #Market Index
    mkt_index::Dict{Real,Array{Int64,1}}
    mkt_map::Dict{Real,Int64}
    silver_index::Dict{Real,Array{Int64,1}}
    mkt_index_long::Array{Int64,1}

    # Non-delta utility for (ij) pairs and draws
    μ_ij_nonprice::Matrix{T}
    μ_ij::Matrix{T}

    # ij pairs of market shares
    s_pred::Vector{Float64}
    s_pred_byperson::Vector{Float64}
    # ij pairs of prices
    price_ij::Vector{Float64}
    subsidy_ij::Vector{Float64}


end

function EqData(cdata::ChoiceData,mkt::DataFrame)#,cpars::DataFrame)
    J = sum([mkt[:Firm].!="OTHER"][1])
    premBase = Vector{Float64}(undef,J)
    premBase[:] = mkt[:premBase][mkt[:Firm].!="OTHER"]


    costBase = Vector{Float64}(undef,J)
    #cost = Vector{Float64}(J)
    cost = mkt[:C_AV][mkt[:Firm].!="OTHER"]
    #costBase[:] = mkt[:Cost_prod][mkt[:Firm].!="OTHER"]

    prods = Vector{Int64}(undef,J)
    prods[:] = sort(mkt[:Product][mkt[:Firm].!="OTHER"])
    (N,L) = size(cdata.data)
    s_pred = Vector{Float64}(undef,N)
    s_pred_byperson = Vector{Float64}(undef,N)
    price_ij = Vector{Float64}(undef,N)
    subsidy = Vector{Float64}(undef,N)

    #R_bench = mkt[:R_bench][1]
    R_bench = 1.0
    RA_share = mkt[:RA_share][1]

    mkt_index = Dict{Real,Array{Int64,1}}()
    silv_index = Dict{Real,Array{Int64,1}}()
    markets = mkt[:Market][mkt[:Firm].!="OTHER"]
    metals = mkt[:Metal_std][mkt[:Firm].!="OTHER"]
    uniq_mkts = sort(unique(markets))
    for (n,m) in enumerate(uniq_mkts)
        mkt_index[n] = findall(markets.==m)
        silv_index[n] = findall(metals[mkt_index[n]].=="SILVER")
    end

    mkt_map = Dict{Real,Int64}()
    for (m,prod_num) in mkt_index
        for j in prod_num
            mkt_map[j] = m
        end
    end

    mkt_index_long = Vector{Int64}(undef,length(cdata[:Product]))
    for (n,prod) in enumerate(cdata[:Product])
        j = findall(prods.==prod)[1]
        mkt_index_long[n] = mkt_map[j]
    end

    ### Create Product Avg Data
    index = Dict{Symbol, Int}()
    varNames = [:C,:ageRate_avg,
                :R_Gamma_j,:A_Gamma_j,:Catastrophic,
                :S_j,:lives]
    for (l,var) in enumerate(varNames)
        index[var]=l
    end

    pmat = Matrix{Float64}(undef,length(prods),length(varNames))

    prod_data = 0
    Other_R = mkt[:R_f][mkt[:Firm].=="OTHER"][1]

    ## Build Ownership Matrix
    firms = mkt[:Firm][mkt[:Firm].!="OTHER"]
    ownMat = Matrix{Float64}(undef,J,J)
    for j in 1:J
        f = firms[j]
        for i in 1:J
            if firms[i]==f
                ownMat[j,i]=1
            else
                ownMat[j,i]=0
            end
        end
    end
    firms_merge_1 = ["AETNA","HUMANA"]
    firms_merge_2 = ["ANTHEM_BLUE_CROSS_AND_BLUE_SHIELD","BLUE_CROSS_BLUE_SHIELD_OF_GEORGIA","CIGNA_HEALTH_AND_LIFE_INSURANCE_COMPANY"]
    ownMat_merge = Matrix{Float64}(undef,J,J)
    for j in 1:J
        f = firms[j]
        for i in 1:J
            if (f in firms_merge_1) & (firms[i] in firms_merge_1)
                ownMat_merge[j,i]=1
            elseif (f in firms_merge_2) & (firms[i] in firms_merge_2)
                ownMat_merge[j,i]=1
            else
                ownMat_merge[j,i]=ownMat[j,i]
            end
        end
    end

    # Initialize Fixed RA Properties
    ST_R_fix  = 0.0
    ST_A_fix = 0.0
    avgPrem_fix = 0.0

    return EqData(cdata,pmat,index,prods, #cpars,
    premBase,costBase,cost,ownMat,ownMat_merge,
    mkt_index,mkt_map,silv_index,mkt_index_long,
    s_pred,s_pred_byperson,price_ij,subsidy,
    R_bench,RA_share,Other_R,
    ST_R_fix,ST_A_fix,avgPrem_fix)
end

getindex(e::EqData, idx::Symbol) = e.p_avg[:,e.p_index[idx]]
