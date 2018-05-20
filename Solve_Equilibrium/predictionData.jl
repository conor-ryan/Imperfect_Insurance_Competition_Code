using DataFrames
import Base.start, Base.next, Base.done, Base.getindex, Base.setindex!

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
    dmat = Matrix{Float64}(n,0)

    ## Set Catastrophic Gamma_j Values to -100
    data_est[:Gamma_j][find(ismissing.(data_est[:Gamma_j]))] = -100
    ## Get Benchmark
    R_bench = df_mkt[:R_bench][df_mkt[:Firm].=="OTHER"][1]

    data_est[:R_Gamma_j] = data_est[:Gamma_j].*data_est[:R]./R_bench
    data_est[:A_Gamma_j] = data_est[:Gamma_j].*data_est[:ageRate_avg].*data_est[:AV]

    ## Adjust Alpha for Monthly Prices
    data_est[:alpha] = data_est[:alpha].*(12/1000)

    # Create a data matrix, only including person id
    println("Construct Data")
    varNames = [:Person,:Product,:R,:alpha,:WTP,:AGE,:mkt_density,:ageRate,:ageRate_avg,
                :Mandate,:subsidy,:MEMBERS,:non_price_util,
                :R_Gamma_j,:A_Gamma_j,
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
    prodids = sort(unique(allids))

    for id in prodids
        idx1 = searchsortedfirst(allids,id)
        idxJ = searchsortedlast(allids,id)
        _productDict[id] = idx1:idxJ
        #_prod_person_Dict[id] = build_IdxDict(dmat[idx1:idxJ,index[:Person]])
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
    _crossprod_Dict = Array{Int64,2}(n,length(prodids))
    for (q,j) in enumerate(sort(prodids))
        idx = Vector{Int64}(n)
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
            cross_idx[idxitr] = idxitr[k]
        else
            cross_idx[idxitr] = -1
        end
    end
    return cross_idx
end



function build_IdxDict{T,N}(j::Array{T,N})
    ids = unique(j)
    dict = build_IdxDict(j,ids)
    return dict
end

function build_IdxDict{T,N}(j::Array{T,N},ids::Vector{T})
    sort!(ids)
    _productDict = Dict{Real,Int64}()

    for id in ids
        _productDict[id] = find(j.==id)[1]
    end
    return _productDict
end

getindex(m::ChoiceData, idx::Symbol) = m.data[:,m.index[idx]]

type EqData
    # Choice Data
    data::ChoiceData
    # Product Data
    p_avg::Matrix{Float64}
    p_index::Dict{Symbol, Int}
    # Products
    prods::Vector{Int64}
    # Cost Parameters
    cost_pars::DataFrame

    # Firm Prices
    premBase_j::Vector{Float64}
    # Firm Base Cost
    Cost_base_j::Vector{Float64}
    Cost_j::Vector{Float64}

    #Ownership Matrix
    ownMat::Matrix{Float64}

    #Market Index
    mkt_index::Dict{Real,Array{Int64,1}}
    mkt_map::Dict{Real,Array{Int64,1}}

    # ij pairs of market shares
    s_pred::Vector{Float64}
    s_pred_byperson::Vector{Float64}
    # ij pairs of prices
    price_ij::Vector{Float64}

    # Benchmark Risk Score, RA Share
    R_bench::Float64
    RA_share::Float64
    Other_R::Float64
end

function EqData(cdata::ChoiceData,mkt::DataFrame,cpars::DataFrame)
    J = sum([mkt[:Firm].!="OTHER"][1])
    premBase = Vector{Float64}(J)
    premBase[:] = mkt[:premBase][mkt[:Firm].!="OTHER"]


    costBase = Vector{Float64}(J)
    cost = Vector{Float64}(J)
    costBase[:] = mkt[:Cost_prod][mkt[:Firm].!="OTHER"]

    prods = Vector{Int64}(J)
    prods[:] = sort(mkt[:Product][mkt[:Firm].!="OTHER"])
    (N,L) = size(cdata.data)
    s_pred = Vector{Float64}(N)
    s_pred_byperson = Vector{Float64}(N)
    price_ij = Vector{Float64}(N)

    R_bench = mkt[:R_bench][1]
    RA_share = mkt[:RA_share][1]

    mkt_index = Dict{Real,Array{Int64,1}}()
    markets = mkt[:Market][mkt[:Firm].!="OTHER"]
    uniq_mkts = sort(unique(markets))
    for (n,m) in enumerate(uniq_mkts)
        mkt_index[n] = find(markets.==m)
    end

    mkt_map = Dict{Real,Array{Int64,1}}()
    for (m,prod_num) in mkt_index
        for j in prod_num
            mkt_map[j] = prod_num
        end
    end

    ### Create Product Avg Data
    index = Dict{Symbol, Int}()
    varNames = [:WTP,:AGE,:ageRate_avg,
                :R_Gamma_j,:A_Gamma_j,:Catastrophic,
                :S_j,:lives]
    for (l,var) in enumerate(varNames)
        index[var]=l
    end

    pmat = Matrix{Float64}(length(prods),length(varNames))

    prod_data = 0
    Other_R = mkt[:R_f][mkt[:Firm].=="OTHER"][1]

    ## Build Ownership Matrix
    firms = mkt[:Firm][mkt[:Firm].!="OTHER"]
    ownMat = Matrix{Float64}(J,J)
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


    return EqData(cdata,pmat,index,prods,cpars,
    premBase,costBase,cost,ownMat,
    mkt_index,mkt_map,
    s_pred,s_pred_byperson,price_ij,R_bench,RA_share,Other_R)
end

getindex(e::EqData, idx::Symbol) = e.p_avg[:,e.p_index[idx]]

function evaluate_model!(e::EqData)
    ## Unpack Prices from Product sorted Data
    unpack_P!(e)
    ## Adjust Prices to Premium Paid
    premPaid!(e)
    # Sort prices by person
    e.price_ij = e.price_ij[e.data._prod_2_per_map]

    # Calculate Market Shares
    calcShares!(e)

    # Resort Market Shares by Product
    e.price_ij = e.price_ij[e.data._per_2_prod_map]
    e.s_pred = e.s_pred_byperson[e.data._per_2_prod_map]

    ## Calculate Product Averages
    calcProd_Avg!(e)

    ## Set Cost
    e.Cost_j[:] = e.Cost_base_j.*exp.(e[:AGE].*e.cost_pars[:Age_j] + e[:WTP].*e.cost_pars[:WTP_j])
    return Void
end


function unpack_P!(e::EqData)
    n = 1
    for j in e.prods
        idx_j = e.data._productDict[j]
        for idx in idx_j
            e.price_ij[idx] = e.premBase_j[n]
        end
        n+=1
    end
    return Void
end

function premPaid!(e::EqData)
    Mandate = e.data[:Mandate]
    subsidy = e.data[:subsidy]
    ageRate = e.data[:ageRate]
    catas   = e.data[:Catastrophic]
    Mems   = e.data[:MEMBERS]
    N = length(e.price_ij)

    for n in 1:N
        price = max(e.price_ij[n]*ageRate[n]-subsidy[n]*(1-catas[n]),0)
        price = ((price/Mems[n]) - Mandate[n]/12)
        e.price_ij[n] = price
    end
    return Void
end

function calcShares!(e::EqData)
    vidx = e.data.index
    alpha = e.data.data_byperson[:,vidx[:alpha]]
    other_util = e.data.data_byperson[:,vidx[:non_price_util]]
    price = e.price_ij
    people = e.data._personIDs

    for i in people
        idxitr = e.data._personDict[i]
        exp_sum = 1.0
        util = Vector{Float64}(length(idxitr))
        for (n,k) in enumerate(idxitr)
            util[n] = other_util[k]*exp(alpha[k]*price[k])
            exp_sum+=util[n]
        end
        for (n,k) in enumerate(idxitr)
            e.s_pred_byperson[k] = util[n]/exp_sum
        end
    end
    return Void
end

function calcProd_Avg!(e::EqData)
    ### Create Product Avg Data
    for (var,l) in e.p_index
        if var in [:S_j,:lives]
            continue
        end
        x_avg = avg_by_prod(e.data[var],e)
        e.p_avg[:,l] = x_avg
    end
    ### Add Marketshares
    #l = length(keys(e.p_index))+1
    l = e.p_index[:S_j]
    S_j = sum_by_prod(e.s_pred,e,e.data[:mkt_density])
    e.p_avg[:,l] = S_j


    ### Add Lives
    l = e.p_index[:lives]
    lives = sum_by_prod(e.s_pred,e,e.data[:PERWT])
    e.p_avg[:,l] = lives
    return Void
end

function calc_deriv!(deriv::Vector{Float64},prod::Int64,e::EqData)
    vidx = e.data.index
    #idx_prod = e.data._per_2_prod_map
    idx_prod = e.data._prod_2_per_map

    alpha = e.data.data_byperson[:,vidx[:alpha]]
    ageRate =  e.data.data_byperson[:,vidx[:ageRate_avg]]
    shares = e.s_pred_byperson

    for i in e.data._personIDs
        idxitr = e.data._personDict[i]
        k = get(e.data._person_prod_Dict[i],prod,-1)
        if k>0
            s_prod = shares[idxitr[k]]
        else
            s_prod = 0.0
        end
        for (n,j) in enumerate(idxitr)
            #println(j)
            #println(idx_prod[j])
            if n==k
                deriv[idx_prod[j]] = alpha[j]*ageRate[j]*shares[j]*(1-shares[j])
            else
                deriv[idx_prod[j]] = -alpha[j]*ageRate[j]*shares[j]*s_prod
            end
        end
    end
    return deriv
end

function calc_deriv!(deriv::Vector{Float64},prod::Int64,
                        crossprod_idx::Array{Int,1},
                        alpha::Vector{Float64},ageRate::Vector{Float64},
                        e::EqData)
    idx_prod = e.data._prod_2_per_map

    shares = e.s_pred_byperson

    N  = length(shares)
    for n in 1:N
        k = crossprod_idx[n]
        if k>0
            s_prod = shares[k]
        else
            s_prod = 0.0
        end
        if n==k
            deriv[idx_prod[n]] = alpha[n]*ageRate[n]*shares[n]*(1-shares[n])
        else
            deriv[idx_prod[n]] = -alpha[n]*ageRate[n]*shares[n]*s_prod
        end
    end
    return deriv
end



function sum_by_prod(x::Vector{Float64},e::EqData,weights::Vector{Float64})
    out = zeros(length(e.prods))
    for (n,j) in enumerate(e.prods)
        idx_j = e.data._productDict[j]
        for q in idx_j
            out[n]+= x[q]*weights[q]
        end
    end
    return out
end

function sum_by_prod(x::Vector{Float64},y::Vector{Float64},
    e::EqData,weights::Vector{Float64})
    out = zeros(length(e.prods))
    for (n,j) in enumerate(e.prods)
        idx_j = e.data._productDict[j]
        tot = 0.0
        for q in idx_j
            tot+= x[q]*y[q]*weights[q]
        end
        out[n] = tot
    end
    return out
end


function avg_by_prod(x::Vector{Float64},e::EqData)
    out = zeros(length(e.prods))
    weights = e.data[:mkt_density]
    s = e.s_pred
    for (n,j) in enumerate(e.prods)
        idx_j = e.data._productDict[j]
        tot_share = 0.0
        for q in idx_j
            out[n]+= x[q]*weights[q]*s[q]
            tot_share+= weights[q]*s[q]
        end
        out[n] = out[n]/tot_share
    end
    return out
end



function chg_in_avg(deriv::Vector{Float64},v::Symbol,x_long::Vector{Float64},
                    S::Vector{Float64},dsdp::Vector{Float64},e::EqData,
                    weights::Vector{Float64})
    L = length(deriv)
    J = length(S)
    dx_avg_dp = Vector{Float64}(J)

    x_avg = e[v]
    #x_long = e.data[v]
    dxdp = sum_by_prod(deriv,x_long,e,weights)

    for j in 1:J
        dx_avg_dp[j] = (dxdp[j] - dsdp[j]*x_avg[j])/S[j]
    end

    return dx_avg_dp
end



function eval_FOC(e::EqData)
    Catas_j = e[:Catastrophic]
    lives = e[:lives].*(1-Catas_j)
    A_Gamma_j = e[:A_Gamma_j]
    R_Gamma_j = e[:R_Gamma_j]
    A_j = e[:ageRate_avg]
    S_j = e[:S_j]
    J = length(S_j)

    ## Draw Long Variables
    A_Gamma_long = e.data[:A_Gamma_j]
    R_Gamma_long = e.data[:R_Gamma_j]
    Age_long = e.data[:AGE]
    WTP_long = e.data[:WTP]
    weights = e.data[:mkt_density]

    vidx = e.data.index
    alpha_long = e.data.data_byperson[:,vidx[:alpha]]
    ageRate_long =  e.data.data_byperson[:,vidx[:ageRate_avg]]


    st_lives = sum(lives)
    S_0 = similar(lives)
    S_m = similar(lives)
    for (m,prods) in e.mkt_index
        S_0_temp = 0.0
        mkt_lives = 0.0
        for j in prods
            S_0_temp+=S_j[j]*(1-Catas_j[j])
            mkt_lives+=lives[j]
        end
        S_0[prods] =  S_0_temp
        S_m[prods] = mkt_lives/st_lives
    end

    share_tilde = zeros(length(S_j))
    for i in 1:length(share_tilde)
        share_tilde[i] = S_m[i]*S_j[i]/S_0[i]*e.RA_share*(1-Catas_j[i])
    end

    ## ST_R and ST_A
    ST_R = sum(share_tilde.*R_Gamma_j) + (1-e.RA_share)*e.Other_R
    ST_A = sum(share_tilde.*A_Gamma_j)/sum(share_tilde)

    R_norm_j = R_Gamma_j./ST_R
    A_norm_j = A_Gamma_j./ST_A

    T_norm_j = R_norm_j - A_norm_j
    avgPrem = sum(A_j.*e.premBase_j.*share_tilde)/sum(share_tilde)

    T_j = T_norm_j.*avgPrem.*(1-Catas_j)

    #### Derivatives By Product ####
    dsdp = Matrix{Float64}(J,J)
    dsdp_rev = Matrix{Float64}(J,J)

    dCost = Matrix{Float64}(J,J)

    dTdp = Matrix{Float64}(J,J)
    dTdp_fix = Matrix{Float64}(J,J)

    (N,M) = size(e.data.data)
    deriv_long = Vector{Float64}(N)
    test = Vector{Float64}(N)

    for (n,j) in enumerate(e.prods)
        #println(j)
        m_idx = e.mkt_map[n]
        cross_idx = e.data._crossprod_Dict[:,n]
        #deriv_long = calc_deriv!(deriv_long,j,e)
        deriv_long  = calc_deriv!(deriv_long,j,cross_idx,
                                    alpha_long,ageRate_long,e)
        dsdp[:,n] = sum_by_prod(deriv_long,e,weights)
        dsdp_rev[:,n] = sum_by_prod(deriv_long,e.data[:ageRate_avg],e,weights)

        dAge_j = chg_in_avg(deriv_long,:AGE,Age_long,S_j,dsdp[:,n],e,weights)
        dWTP_j = chg_in_avg(deriv_long,:WTP,WTP_long,S_j,dsdp[:,n],e,weights)

        dCost[:,n] = (dAge_j.*e.cost_pars[:Age_j] + dWTP_j.*e.cost_pars[:WTP_j]).*e.Cost_j
        dR_Gamma_j = chg_in_avg(deriv_long,:R_Gamma_j,R_Gamma_long,
                        S_j,dsdp[:,n],e,weights)

        dA_Gamma_j = chg_in_avg(deriv_long,:A_Gamma_j,A_Gamma_long,
                        S_j,dsdp[:,n],e,weights)

        ds_0_dp = -sum(dsdp[:,n].*(1-Catas_j))
        # Check ds_M_dp for catastrophic plans
        ds_M_dp = ds_0_dp*S_m[n].*S_m
        ds_M_dp_mkt = -ds_0_dp*S_m[n]*(1-S_m[n])
        S_0_mkt = S_0[n]
        S_m_mkt = S_m[n]

        ds_tilde_dp = ds_M_dp.*S_j./S_0.*(1-Catas_j)

        if Catas_j[n]==0
            ds_tilde_dp[m_idx] = (ds_M_dp_mkt.*S_j[m_idx]./S_0_mkt +
                            dsdp[m_idx,n].*S_m_mkt/S_0_mkt +
                            S_m_mkt.*ds_0_dp.*S_j[m_idx]./(S_0_mkt)^2).*(1-Catas_j[m_idx])
        else
            ds_tilde_dp[m_idx] = ds_M_dp_mkt.*S_j[m_idx]./S_0[m_idx].*(1-Catas_j[m_idx])
        end

        dR_norm = dR_Gamma_j/ST_R -
                R_norm_j.*sum(ds_tilde_dp.*R_Gamma_j + share_tilde.*dR_Gamma_j)/ST_R

        dA_norm = dA_Gamma_j/ST_A -
                A_norm_j.*sum(ds_tilde_dp.*A_Gamma_j + share_tilde.*dA_Gamma_j)/ST_A

        dTnorm_dp = (dR_norm - dA_norm).*(1-Catas_j)

        # Check the dAge_j term in this. Should it be average?
        dP_s = sum(ds_tilde_dp.*A_j.*e.premBase_j + share_tilde.*dsdp_rev[:,n].*e.premBase_j)
        dP_s+= share_tilde[n]*A_j[n]

        dTdp[:,n] = (dTnorm_dp.*avgPrem + T_norm_j*dP_s).*(1-Catas_j)
        dTdp_fix[:,n] = (dR_Gamma_j./ST_R - dA_Gamma_j./ST_A).*avgPrem.*(1-Catas_j)
    end


    ## Output Needed dsdp, dsdp_rev, S_j, A_j
    ## Mkt_lives, T_j, dTdp, dTdp_fix,
    ## dAge, dWTP

    ## Calculate First Order Conditions
    foc_err = Vector{Float64}(J)
    P_new = Vector{Float64}(J)
    for (m,m_idx) in e.mkt_index
        L_m = S_m[m_idx][1]
        P = e.premBase_j[m_idx]
        S = S_j[m_idx]
        A = A_j[m_idx]
        T = T_j[m_idx]
        C = e.Cost_j[m_idx]
        m_dsdp = dsdp[m_idx,m_idx].*e.ownMat[m_idx,m_idx]
        m_dsdp_rev = dsdp_rev[m_idx,m_idx].*e.ownMat[m_idx,m_idx]
        m_dTdp = dTdp[:,m_idx].*e.ownMat[:,m_idx]
        m_dCost = dCost[m_idx,m_idx].*e.ownMat[m_idx,m_idx]
        rev = inv(m_dsdp)*(m_dsdp_rev*P + S.*A)
        tran = inv(L_m*m_dsdp)*(L_m*m_dsdp*T + m_dTdp'*(S_m.*S_j))
        cost = inv(m_dsdp)*(m_dsdp*C + m_dCost*S)
        # The cost matrix should maybe be transposed...
        P_new[m_idx] = -inv(L_m*m_dsdp_rev)*(L_m*( S.*A - (m_dsdp*C + m_dCost*S) ) +
                                L_m*m_dsdp*T + m_dTdp'*(S_m.*S_j) )
        foc_err[m_idx] = (rev+tran-cost)./100
    end

    # Average Error, for consistency across product numebers
    return foc_err,P_new
end

function update_Prices!(foc_err::Vector{Float64},P_new::Vector{Float64},
                                e::EqData)

    ### 0 Market Share
    ProdExit = (e[:S_j].<(1e-5) )
    P_new[ProdExit] = min.(e.premBase_j[ProdExit],P_new[ProdExit])
    foc_err[ProdExit] = 0.0

    if any(ProdExit)
        exited = find(ProdExit)
        println("Product Exits for $exited")
    end

    ### Negative Prices
    P_new[P_new.<0] = 0.9.*e.premBase_j[P_new.<0]

    ### MLR Constraint
    MLR_const = e.Cost_j./0.7
    constrained_bool = (P_new.>=MLR_const).& (e[:S_j].>=(1e-5))
    if any( constrained_bool )
        constrained = find( constrained_bool )
        println("Hit MLR Constraint at products $constrained")
        P_const = MLR_const[constrained]
        println("Constrained prices: $P_const")
        foc_err[constrained] = 0.0
    end
    #P_new = min.(P_new,MLR_const)


    ### New Prices
    step = 0.05
    P_update = e.premBase_j.*(1-step) + step.*P_new
    P_update[P_new.>=MLR_const] = MLR_const[P_new.>MLR_const]
    # Contrain Prices at 0
    #P_update = max.(P_update,0)

    e.premBase_j = P_update

    return foc_err
end

function solve_model!(e::EqData,tol::Float64=.5)
    err = 10
    cnt = 0
    P_low = similar(e.premBase_j)
    while (err>tol) & (cnt<1000)
        cnt+=1
        evaluate_model!(e)
        foc_err, P_new = eval_FOC(e)
        foc_err = update_Prices!(foc_err,P_new,e)
        err_new = sum(foc_err.^2)/sum(foc_err.!=0.0)
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
    df_mkt = CSV.read(file2,null="NA")
    cost_pars = CSV.read("Intermediate_Output/Equilibrium_Data/cost_pars.csv",null="NA")

    # Solve Model
    println("Build Model")
    c = ChoiceData(df,df_mkt)

    model = EqData(c,df_mkt,cost_pars)

    println("Estimate Model")
    solve_model!(model,0.02)
    println("Solved: $st")

    output =  DataFrame(Products=model.prods,Prices=model.premBase_j)
    file3 = "Estimation_Output/solvedEquilibrium_$st.csv"
    CSV.write(file3,output)
    return Void
end
