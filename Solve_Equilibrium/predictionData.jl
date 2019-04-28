using DataFrames
import Base.getindex, Base.setindex!

abstract type
    EstData
end


# Make a type to interface with the data for discrete choice models
struct ChoiceData <: EstData
    # Matrix of the data (transposed, pre-sorted)
    data::Matrix{Float64}
    data_byperson::Matrix{Float64}
    # Index of the data column names
    index::Dict{Symbol,Int}

    # ID Lookup Mappings
    _personIDs::Array{Float64,1}
    _personDict::Dict{Int, UnitRange{Int}}
    _productDict::Dict{Int, Array{Int,1}}
    _person_prod_Dict::Dict{Int,Dict{Real,Int}}
    _prod_person_Dict::Dict{Int,Dict{Real,Array{Int}}}

    _prod_2_per_map::Vector{Int64}
    _per_2_prod_map::Vector{Int64}

    _crossprod_Dict::Array{Int,2}
end


function ChoiceData(data_est::DataFrame,df_mkt::DataFrame)

    # Get the size of the data
    n, k = size(data_est)

    index = Dict{Symbol, Int}()
    dmat = Matrix{Float64}(undef,n,0)

    ## Set Catastrophic Gamma_j Values to -100
    data_est[:Gamma_j][findall(ismissing.(data_est[:Gamma_j]))] .= -100
    ## Get Benchmark
    #R_bench = df_mkt[:R_bench][df_mkt[:Firm].=="OTHER"][1]

    data_est[:R_Gamma_j] = data_est[:Gamma_j].*data_est[:R]  #./R_bench
    data_est[:A_Gamma_j] = data_est[:Gamma_j].*data_est[:ageRate_avg].*data_est[:AV]

    ## Adjust Alpha for Monthly Prices
    data_est[:alpha] = data_est[:alpha].*(12/1000)

    # Create a data matrix, only including person id
    println("Construct Data")
    varNames = [:Person,:Product,:R,:alpha,:AGE,:mkt_density,:ageRate,:ageRate_avg,
                :Mandate,:subsidy,:IncomeCont,:MEMBERS,:non_price_util,
                :R_Gamma_j,:A_Gamma_j,:C,:C_nonAV,
                :Catastrophic,:AV,:Gamma_j,:PERWT]
    for (l,var) in enumerate(varNames)
        mat_temp = Array{Float64}(data_est[var])
        dmat = hcat(dmat,mat_temp)
        index[var]=l
    end

    println("Product ID Mapping")
    _productDict = Dict{Real, UnitRange{Int}}()
    _prod_person_Dict = Dict{Int,Dict{Real,Array{Int}}}()
    allids = data_est[:Product]
    prodids = Int.(sort(unique(allids)))

    for id in prodids
        idx1 = searchsortedfirst(allids,id)
        idxJ = searchsortedlast(allids,id)
        _productDict[id] = idx1:idxJ
        # _prod_person_Dict[id] = build_IdxDict(dmat[idx1:idxJ,index[:Person]])
    end

    println("Person Mapping")
    _prod_2_per_map = sortperm(dmat[:,index[:Person]])
    dmat_per = dmat[_prod_2_per_map,:]

    # Reverse Sort
    _per_2_prod_map = sortperm(_prod_2_per_map)

    _personDict = Dict{Real, UnitRange{Int}}()
    _person_prod_Dict = Dict{Int,Dict{Real,Int64}}()
    allids = dmat_per[:,index[:Person]]
    uniqids = sort(unique(allids))
    for id in uniqids
        idx1 = searchsortedfirst(allids,id)
        idxJ = searchsortedlast(allids,id)
        _personDict[id] = idx1:idxJ
        _person_prod_Dict[id] = build_IdxDict(dmat_per[idx1:idxJ,index[:Product]])
    end


    # #Create Product Dictionary
    # println("Product Dictionary")
    # _productDict = build_ProdDict(data_est[:Product])

    #Build Cross Product Dict
    _crossprod_Dict = Array{Int64,2}(undef,n,length(prodids))
    for (q,j) in enumerate(sort(prodids))
        idx = Vector{Int64}(undef,n)
        _crossprod_Dict[:,q] = crossprod_index!(idx,j,
                                uniqids,
                                _personDict,
                                _person_prod_Dict)
    end


    # Make the data object
    m = ChoiceData(dmat,dmat_per,index,
    uniqids,_personDict,_productDict,
    _person_prod_Dict,_prod_person_Dict,
    _prod_2_per_map,_per_2_prod_map,
    _crossprod_Dict)
    return m
end


function crossprod_index!(cross_idx::Vector{Int64},
                prod::Int64,
                _personIDs::Array{Float64,1},
                _personDict::Dict{Real, UnitRange{Int}},
                _person_prod_Dict::Dict{Int,Dict{Real,Int}})

    for i in _personIDs
        idxitr = _personDict[i]
        k = get(_person_prod_Dict[i],prod,-1)
        if k>0
            cross_idx[idxitr] .= idxitr[k]
        else
            cross_idx[idxitr] .= -1
        end
    end
    return cross_idx
end



function build_IdxDict(j::Array{T,N}) where {T,N}
    ids = unique(j)
    dict = build_IdxDict(j,ids)
    return dict
end

function build_IdxDict(j::Array{T,N},ids::Vector{T}) where {T,N}
    sort!(ids)
    _productDict = Dict{Real,Int64}()

    for id in ids
        _productDict[id] = findall(j.==id)[1]
    end
    return _productDict
end

getindex(m::ChoiceData, idx::Symbol) = m.data[:,m.index[idx]]

function setindex!(m::ChoiceData,x::T,idx::Symbol) where T
    m.data[:,m.index[idx]] .= x
end


mutable struct EqData
    # Choice Data
    data::ChoiceData
    # Product Data
    p_avg::Matrix{Float64}
    p_index::Dict{Symbol, Int}
    # Products
    prods::Vector{Int64}
    # Cost Parameters
    #cost_pars::DataFrame

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

    # ij pairs of market shares
    s_pred::Vector{Float64}
    s_pred_byperson::Vector{Float64}
    # ij pairs of prices
    price_ij::Vector{Float64}
    subsidy_ij::Vector{Float64}

    # Benchmark Risk Score, RA Share
    R_bench::Float64
    RA_share::Float64
    Other_R::Float64

    # Fixed RA Properties
    ST_R_fix::Float64
    ST_A_fix::Float64
    avgPrem_fix::Float64
end

function EqData(cdata::ChoiceData,mkt::DataFrame,ψ_AV::Float64)#,cpars::DataFrame)
    J = sum([mkt[:Firm].!="OTHER"][1])
    premBase = Vector{Float64}(undef,J)
    premBase[:] = mkt[:premBase][mkt[:Firm].!="OTHER"]


    costBase = Vector{Float64}(undef,J)
    #cost = Vector{Float64}(J)
    cost = exp.(mkt[:AV_std][mkt[:Firm].!="OTHER"].*ψ_AV)
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
    # Other_R = mkt[:R_f][mkt[:Firm].=="OTHER"][1]
    Other_R = 0.0

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
