mutable struct firmData
    ## Demand parameters
    par_dem::parDict{Float64}

    ## Cost parameters
    par_cost::parMC{Float64}

    ## Supplemental Data
    data:: Matrix{Float64}
    index::Dict{Symbol,Int}

    ## Individual Level Values
    P_ij::Vector{Float64} # Individual Level Product Prices, paid by enrollees
    Rev_ij::Vector{Float64} # Individual-Product level revenue - monthly premium to firms.
    subsidy_ij::Vector{Float64} # Individual Level Subsidies
    zero_ij::Vector{Float64} # Does this individual/product have 0 premium?

    δ_nonprice::Vector{Float64} # Fixed non-price product chars
    δ_price::Vector{Float64} # Updated Price Characteristics
    s_pred:: Vector{Float64} # Updated Person-Product Shares
    c_pred:: Vector{Float64} # Updated Person-Product Costs
    c_pool:: Vector{Float64} # Updated Person-Product Costs, after RA transfers


    ## Firm Level Values
    P_j::Vector{Float64} #Price
    SA_j::Vector{Float64} #Price
    S_j::Vector{Float64} #Shares
    Mkt_j::Vector{Float64} # Average Revenue
    C_j::Vector{Float64} # Average Cost
    PC_j::Vector{Float64} # Pooled Average Cost
    Adj_j::Vector{Float64} # Pooled Average Cost


    hix_cnt::Vector{Float64} # Firm Product Weight (Hix)
    bench_base::Vector{Float64} # Original Benchmark

    ## Values Used in Computing Equilibrium
    dSdp_j::Matrix{Float64} # Demand Derivative
    dSAdp_j::Matrix{Float64} # Demand Derivative
    dRdp_j::Matrix{Float64} # Demand Derivative
    dCdp_j::Matrix{Float64} # Cost Derivative
    dMdp_j::Matrix{Float64} # Market Lives Derivatives
    dCdp_pl_j::Matrix{Float64} # Pooled Cost Derivative

    ## Market Organization
    stdMap::Vector{Int64}
    prods::Vector{Int64}
    catas_prods::Vector{Int64}
    _productDict::Dict{Int, Array{Int,1}}

    mkt_index::Dict{Real,Array{Int64,1}}
    silver_index::Dict{Real,Array{Int64,1}}
    mkt_index_long::Array{Int64,1}

    ownMat::Matrix{Float64}
    ownMat_merge::Matrix{Float64}
    poolMat::Matrix{Float64}


    _perSTDict::Dict{String, Array{Int,1}}
    _prodSTDict::Dict{String, Array{Int,1}}
end

function firmData(m::InsuranceLogit,df::DataFrame,mkt::DataFrame,
                    par_dem::parDict,par_cost::parMC)

    println("Compute Demand and Cost")
    individual_values!(m,par_dem)
    individual_shares(m,par_dem)
    individual_costs(m,par_cost)

    J = length(m.prods)
    M = size(m.data.data,2)

    println("Product Map/Dict")
    ### Standard Products
    prodMap = unique(df[[:Product,:Product_std]])
    sort!(prodMap,:Product)
    prodMap = convert(Vector{Int64},prodMap[:Product_std])
    prod_std = unique(prodMap)

    catas_prods = Int.(mkt[:Product][mkt[:Metal_std].=="CATASTROPHIC"])

    prod_vec = convert(Vector{Float64},df[:,:Product_std])
    _productDict = build_ProdDict(prod_vec)

    println("Supplemental Data")
    df[:Catastrophic] = Float64.(df[:Metal_std].=="CATASTROPHIC")
    df[:Rev_foc] = df[:premBase].*df[:ageRate]./df[:MEMBERS]
    dataNames = [:ageRate,:ageRate_avg,:IncomeCont,:Mandate,:MEMBERS,:Catastrophic,:subsidy,:Rev_foc]
    data = convert(Matrix{Float64},df[dataNames])
    index = Dict{Symbol, Int}()
    for (l,var) in enumerate(dataNames)
        index[var] = l
    end


    println("Personal and Product Characteristics")
    #### Individual Values
    P_ij= price(m.data) # Individual Level Product Prices
    Rev_ij=Vector{Float64}(undef,M)
    subsidy_ij=Vector{Float64}(undef,M) # Updated Subsidies
    zero_ij=Vector{Float64}(undef,M)

    δ_nonprice=Vector{Float64}(undef,M) # Fixed non-price product chars
    δ_price=Vector{Float64}(undef,M) # Updated Price Characteristics
    s_pred= Vector{Float64}(undef,M) # Updated Person-Product Shares
    c_pred= Vector{Float64}(undef,M) # Update Person-Product Costs
    c_pool= Vector{Float64}(undef,M) # Update Person-Product Costs, after transfer

    #### Firm Values
    # P_j = mkt[:premBase][mkt[:Firm].!="OTHER"]
    P_j = Vector{Float64}(undef,J)
    P_j[prod_std] = mkt[:premBase]
    SA_j = Vector{Float64}(undef,J)
    S_j = Vector{Float64}(undef,J)
    Mkt_j = Vector{Float64}(undef,J)
    C_j = Vector{Float64}(undef,J)
    PC_j = Vector{Float64}(undef,J)
    Adj_j = Vector{Float64}(undef,J)

    hix_cnt = Vector{Float64}(undef,J)
    hix_cnt[prod_std] = Float64.(mkt[:count_hix_prod])
    bench_base = Vector{Float64}(undef,J)
    bench_base[prod_std] = Float64.(mkt[:benchBase])


    dSdp_j = Matrix{Float64}(undef,J,J)
    dSAdp_j = Matrix{Float64}(undef,J,J)
    dRdp_j = Matrix{Float64}(undef,J,J)
    dCdp_j = Matrix{Float64}(undef,J,J)
    dMdp_j = Matrix{Float64}(undef,J,J)
    dCdp_pl_j = Matrix{Float64}(undef,J,J)


    println("Compute Market Organzation Maps")
    #### Dictionary/Mappings
    mkt_index = Dict{Real,Array{Int64,1}}()
    silv_index = Dict{Real,Array{Int64,1}}()
    markets = mkt[:Market]
    metals = mkt[:Metal_std]
    uniq_mkts = sort(unique(markets))
    for (n,mt) in enumerate(uniq_mkts)
        m_ind = findall(markets.==mt)
        mkt_index[n] = mkt[:Product][m_ind]
        silv_index[n] = mkt[:Product][findall(metals[m_ind].=="SILVER")]
    end

    mkt_map = Dict{Real,Int64}()
    for (m,prod_num) in mkt_index
        for j in prod_num
            mkt_map[j] = m
        end
    end

    mkt_index_long = Vector{Int64}(undef,M)
    for (n,prod) in enumerate(df[:Product_std])
        mkt_index_long[n] = mkt_map[prod]
    end

    #### Ownership Matrix
    ## Build Ownership Matrix
    firms = Vector{Union{Missing,String}}(missing,J)
    firms[prod_std] = mkt[:Firm]
    # firms[prod_std] = string.(mkt[:Product])
    # firms[prod_std] = mkt[:Market]
    ownMat = zeros(J,J)
    for j in prod_std
        f = firms[j]
        for i in prod_std
            if firms[i]==f
                ownMat[j,i]=1
            end
        end
    end
    firms_merge_1 = ["AETNA","HUMANA"]
    firms_merge_2 = ["ANTHEM_BLUE_CROSS_AND_BLUE_SHIELD","BLUE_CROSS_BLUE_SHIELD_OF_GEORGIA","CIGNA_HEALTH_AND_LIFE_INSURANCE_COMPANY"]
    ownMat_merge = zeros(J,J)
    for j in prod_std
        f = firms[j]
        for i in prod_std
            if (f in firms_merge_1) & (firms[i] in firms_merge_1)
                ownMat_merge[j,i]=1
            elseif (f in firms_merge_2) & (firms[i] in firms_merge_2)
                ownMat_merge[j,i]=1
            else
                ownMat_merge[j,i]=ownMat[j,i]
            end
        end
    end

    ### Pooled Cost Areas
    states = Vector{Union{Missing,String}}(missing,J)
    states[prod_std] = mkt[:Market]
    poolMat = zeros(J,J)
    for j in prod_std
        st = states[j]
        for i in prod_std
            if states[i]==st
                poolMat[j,i]=1
            end
        end
    end

    #### State Person Index ####
    st_per = unique(df[[:Person,:ST]])
    _perSTDict = Dict{String,Array{Int64,1}}()
    states = unique(st_per[:ST])
    for s in states
        _perSTDict[s] = st_per[:Person][st_per[:ST].==s]
    end


    _prodSTDict = Dict{String,Array{Int64,1}}()
    for s in states
        _prodSTDict[s] = mkt[:Product][mkt[:ST].==s]
    end




    firm = firmData(par_dem,par_cost,
    data,index,
    P_ij,Rev_ij,subsidy_ij,zero_ij,
    δ_nonprice,δ_price,s_pred,c_pred,c_pool,
    P_j,SA_j,S_j,Mkt_j,C_j,PC_j,Adj_j,
    hix_cnt,bench_base,
    dSdp_j,dSAdp_j,dRdp_j,dCdp_j,dMdp_j,dCdp_pl_j,
    prodMap,prod_std,catas_prods,_productDict,
    mkt_index,silv_index,mkt_index_long,
    ownMat,ownMat_merge,poolMat,
    _perSTDict,_prodSTDict)

    println("Initialize Shares/Derivatives")

    compute_nonprice!(m,firm)

    # evaluate_model!(m,firm,foc_check=true)

    return firm
end

# function evaluate_model!(m::InsuranceLogit,f::firmData;foc_check=false)
#     #Clear Derivative Values
#     f.dSdp_j[:].=0.0
#     f.dSAdp_j[:].=0.0
#     f.dMdp_j[:].=0.0
#     f.dRdp_j[:].=0.0
#     f.dCdp_j[:].=0.0
#     f.dCdp_pl_j[:].=0.0
#     f.PC_j[:].=0.0
#     f.S_j[:].=0.0
#     f.C_j[:].=0.0
#     f.SA_j[:].=0.0
#     f.Adj_j[:].=0.0
#     f.Mkt_j[:].=0.0
#
#     if foc_check==false
#         premPaid!(firm)
#     else
#         f.Rev_ij = f[:Rev_foc]
#         f.P_ij   = price(m.data)
#         f.zero_ij = Float64.((f.P_ij .+ f[:Mandate]/1000 .- 1e-6).<0.0)
#     end
#
#     compute_price!(m,f)
#     update_derivatives(m,f)
#     return nothing
# end


function evaluate_model!(m::InsuranceLogit,f::firmData,ST::String;foc_check=false)
    #Clear Derivative Values
    f.dSdp_j[:].=0.0
    f.dSAdp_j[:].=0.0
    f.dMdp_j[:].=0.0
    f.dRdp_j[:].=0.0
    f.dCdp_j[:].=0.0
    f.dCdp_pl_j[:].=0.0
    f.PC_j[:].=0.0
    f.S_j[:].=0.0
    f.C_j[:].=0.0
    f.SA_j[:].=0.0
    f.Adj_j[:].=0.0
    f.Mkt_j[:].=0.0

    if foc_check==false
        premPaid!(firm)
    else
        f.Rev_ij = f[:Rev_foc]
        f.P_ij   = price(m.data)
        f.zero_ij = Float64.((f.P_ij .+ f[:Mandate]/1000 .- 1e-6).<0.0)
    end

    compute_price!(m,f)
    update_derivatives(m,f,ST,foc_check=foc_check)
    return nothing
end

getindex(f::firmData, idx::Symbol) = f.data[:,f.index[idx]]
