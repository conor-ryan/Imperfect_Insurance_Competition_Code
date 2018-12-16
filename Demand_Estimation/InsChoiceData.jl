using DataFrames
import Base.getindex, Base.setindex!

abstract type
    ModelData
end


# Make a type to interface with the data for discrete choice models
struct ChoiceData <: ModelData
    # Matrix of the data (transposed, pre-sorted)
    data::Matrix{Float64}
    # Matrix of the product level data (pre-sorted)
    pdata::DataFrame
    # Risk Score Moments
    rMoments::Matrix{Float64}
    tMoments::Vector{Float64}
    st_share::Vector{Float64}
    # Matrix of Fixed Effects
    fixedEffects::Matrix{Float64}
    # Index of the data column names
    index
    # Names of rows (columns of input data)
    prodchars   # Product Characteristics
    prodchars_0   # Product Characteristics
    choice      # Binary choice indicator
    demoRaw    # Household Demographics - raw
    wgt     # Number of People in each type
    unins     # Outside Option Share
    # Precomputed Indices
    _person::Array{Int,1}
    _product::Array{Int,1}
    _prodchars::Array{Int,1}
    _prodchars_0::Array{Int,1}
    _choice::Array{Int,1}
    _demoRaw::Array{Int,1}
    _wgt::Array{Int,1}
    _ageRate::Array{Int,1}
    _ageHCC::Array{Int,1}
    _unins::Array{Int,1}
    _rInd::Array{Int,1}
    _rIndS::Array{Int,1}

    # ID Lookup Mappings
    _personIDs::Array{Float64,1}
    _personDict::Dict{Int, UnitRange{Int}}
    _productDict::Dict{Int, Array{Int,1}}

    _rel_fe_Dict::Dict{Real,Array{Int64,1}}
    _tMomentDict::Dict{Int,Array{Int,1}}
    _stDict::Dict{Int,Array{Int,1}}
end

function ChoiceData(data_choice::DataFrame,
                    data_market::DataFrame,
                    data_risk::DataFrame;
        person=[:Person],
        product=[:Product],
        prodchars=[:Price,:MedDeduct,:High],
        prodchars_0=[:PriceDiff],
        choice=[:S_ij],
        demoRaw=[:Age,:Family,:LowIncome],
        # demoRaw=[:F0_Y0_LI1,
        #          :F0_Y1_LI0,:F0_Y1_LI1,
        #          :F1_Y0_LI0,:F1_Y0_LI1,
        #          :F1_Y1_LI0,:F1_Y1_LI1],
        fixedEffects=Vector{Symbol}(undef,0),
        wgt=[:N],
        unins=[:unins_rate],
        riskscores=true)

    # Get the size of the data
    n, k = size(data_choice)

    # Convert everything to an array once for performance
    i = convert(Array{Float64},data_choice[person])
    j = convert(Array{Float64},data_choice[product])
    X = convert(Array{Float64},data_choice[prodchars])
    X_0 = convert(Array{Float64},data_choice[prodchars_0])
    y = convert(Array{Float64},data_choice[choice])
    Z = convert(Array{Float64},data_choice[demoRaw])
    w = convert(Array{Float64},data_choice[wgt])
    s0= convert(Array{Float64},data_choice[unins])

    riskChars=[:ageRate_avg,:HCC_age]
    rm = convert(Array{Float64},data_choice[riskChars])

    println("Create Fixed Effects")
    bigFirm = :Big in prodchars_0
    F, feNames = build_FE(data_choice,fixedEffects,bigFirm = bigFirm)
    F = permutedims(F,(2,1))



    index = Dict{Symbol, Int}()
    dmat = Matrix{Float64}(undef,n,0)

    #### Risk Score Moments ####
    if riskscores
        r_df = unique(data_choice[[:Rtype,:Any_HCC,
                                :mean_HCC_Catastrophic,:var_HCC_Catastrophic,
                                :mean_HCC_Bronze,:var_HCC_Bronze,
                                :mean_HCC_Silver,:var_HCC_Silver,
                                :mean_HCC_Gold,:var_HCC_Gold,
                                :mean_HCC_Platinum,:var_HCC_Platinum]])
        r_types = sort(unique(data_choice[:Rtype]))
        rmat = Matrix{Float64}(undef,length(r_types)*5,4)
        for r in r_types
            row_ind = findall(r_df[:Rtype].==r)[1]
            r_temp = r_df[row_ind:row_ind,:]
            for m in 1:5
                rmat[(r-1)*5+m,1] = (r-1)*5+m
                rmat[(r-1)*5+m,2] = r_temp[1,2]
                rmat[(r-1)*5+m,3] = r_temp[1,1+m*2]
                rmat[(r-1)*5+m,4] = r_temp[1,2+m*2]
            end
        end

        R_index = Vector{Float64}(undef,n)
        R_metal_index = Vector{Float64}(undef,n)
        for ind in eachindex(R_index)
            r =  findall((in)(df[ind,:Rtype]),rmat[:,1])[1]
            R_index[ind] = (r-1)*5 + 3
            AV = df[ind,:AV]
            if AV==.57
                R_metal_index[ind] = (r-1)*5 + 1
            elseif AV==0.6
                R_metal_index[ind] = (r-1)*5 + 2
            elseif (AV==0.7) | (AV==0.73)
                R_metal_index[ind] = (r-1)*5 + 3
            elseif (AV>.75) & (AV!=.9)
                R_metal_index[ind] = (r-1)*5 + 4
            else
                R_metal_index[ind] = (r-1)*5 + 5
            end
        end

        r_silv_var = [:riskIndex_Silver]
        r_var = [:riskIndex]
        #df[r_var] = R_index
    else
        r_silv_var = [:riskIndex_Silver]
        r_var = [:riskIndex]
        R_index = Vector{Float64}(undef,n)
        R_metal_index = Vector{Float64}(undef,n)
        rmat =  Matrix{Float64}(undef,0,0)
    end

    # Create a data matrix, only including person id
    println("Put Together Data non FE data together")
    k = 0
    for (d, var) in zip([i,X,X_0, y, Z,w,rm, s0,R_metal_index,R_index,j], [person,prodchars,
        prodchars_0,choice, demoRaw,wgt,riskChars,unins,r_var,r_silv_var,product])
        for l=1:size(d,2)
            k+=1
            dmat = hcat(dmat, d[:,l])
            index[var[l]] = k
        end
    end


    #Transpose data to store as rows
    dmat = permutedims(dmat,(2,1))
    i = permutedims(i,(2,1))
    j = permutedims(j,(2,1))

    # Precompute the row indices
    _person = getDictArray(index,person)
    _product = getDictArray(index,product)
    _prodchars = getDictArray(index, prodchars)
    _prodchars_0 = getDictArray(index, prodchars_0)
    _choice = getDictArray(index, choice)
    _demoRaw = getDictArray(index, demoRaw)
    _wgt = getDictArray(index, wgt)
    _ageRate = getDictArray(index, [:ageRate_avg])
    _ageHCC = getDictArray(index, [:HCC_age])
    _unins = getDictArray(index, unins)
    _rInd = getDictArray(index, r_var)
    _rIndS = getDictArray(index, r_silv_var)

    # Get Person ID Dictionary Mapping for Easy Subsets
    println("Person ID Mapping")
    _personDict = Dict{Real, UnitRange{Int}}()
    allids = dmat[_person,:][1,:]
    uniqids = sort(unique(allids))

    for id in uniqids
        idx1 = searchsortedfirst(allids,id)
        idxJ = searchsortedlast(allids,id)
        _personDict[id] = idx1:idxJ
    end


    #Create Product Dictionary
    println("Product Dictionary")
    prod_vec = j[1,:]
    _productDict = build_ProdDict(prod_vec)
    # allprods = sort(unique(j))
    # _productDict = Dict{Real, Array{Int}}()
    # for id in allprods
    #     _productDict[id] = findin(j,id)
    # end

    # Relevant Parameters Per Person
    rel_fe_Dict = Dict{Real,Array{Int64,1}}()
    for (id,idxitr) in _personDict
        F_t = view(F,:,idxitr)
        any_positive = maximum(F_t,dims=2)[:,1]
        pars_relevant = findall(any_positive .>0)
        rel_fe_Dict[id] = pars_relevant
    end

    # Construct Risk Moments
    println("Construct Risk Moments")
    _tMomentDict = Dict{Int,Array{Int64,1}}()
    moments = sort(unique(data_risk[:momentID]))
    tMoments = Vector{Float64}(undef,length(moments))
    st_share = zeros(length(keys(_productDict)))
    for m in moments
        _tMomentDict[m] = data_risk[:Product][findall(data_risk[:momentID].==m)]
        tMoments[m] = data_risk[:T_moment][findall(data_risk[:momentID].==m)][1]
    end


    _stDict = Dict{Int,Array{Int64,1}}()
    states = unique(data_risk[:ST])
    for s in states
        _stDict[s] = unique(data_risk[:Product][findall(data_risk[:ST].==s)])
    end

    for prod in keys(_productDict)
        ind = Int(prod)
        idx = findall(data_risk[:Product].==prod)
        if length(idx)>0
            st_share[ind] = data_risk[:st_share][idx[1]]
        end
    end

    # Make the data object
    m = ChoiceData(dmat,data_market,rmat,tMoments,st_share,
            F, index, prodchars,prodchars_0,
            choice, demoRaw,wgt, unins, _person,_product, _prodchars,_prodchars_0,
            _choice, _demoRaw, _wgt,_ageRate,_ageHCC,
             _unins,_rInd,_rIndS,
             uniqids,_personDict,_productDict,
            rel_fe_Dict,_tMomentDict,_stDict)
    return m
end

function getDictArray(dict::Dict{T,S},labels::Vector{T}) where {T,S}
    output = Vector{S}(undef,length(labels))
    for (i,lab) in enumerate(labels)
        output[i] = getindex(dict,lab)
    end
    return output
end

function build_ProdDict(j::Array{T,N}) where {T,N}
    allprods = unique(j)
    sort!(allprods)
    _productDict = Dict{Real, Array{Int64,1}}()
    for id in allprods
        _productDict[id] = findall(j.==id)
    end
    return _productDict
end

function build_FE(data_choice::DataFrame,fe_list::Vector{T};bigFirm=false) where T
    # Create Fixed Effects
    n, k = size(data_choice)
    L = 0

    # No Fixed effects for empty lists
    if typeof(fe_list)!=Vector{Symbol}
        println("No Fixed Effects")
        F = Matrix{Float64}(undef,n,L)
        feNames = Vector{Symbol}(undef,0)
        return F,feNames
    end

    for fe in fe_list
        fac_variables = data_choice[fe]
        factor_list = sort(unique(fac_variables))
        if fe==:constant
            num_effects=1
        elseif (!(:constant in fe_list)) & (fe==fe_list[1])
            num_effects = length(factor_list)
            # if fe==:Market
            #     num_effects = length(factor_list) - 3
            # end
        else
            num_effects = length(factor_list)-1
            # if fe==:Market
            #     num_effects = length(factor_list) - 4
            # end
        end
        if bigFirm & (fe==:Firm)
            num_effects-= 1
        end
        L+=num_effects
    end

    F = zeros(n,L)
    feNames = Vector{Symbol}(undef,0)
    ind = 1
    for fe in fe_list
        if fe==:constant
            F[:,ind] = 1
            ind+=1
            continue
        end
        fac_variables = data_choice[fe]
        factor_list = sort(unique(fac_variables))
        if (!(:constant in fe_list)) & (fe==fe_list[1])
            st_ind = 1
        else
            st_ind = 2
        end

        for fac in factor_list[st_ind:length(factor_list)]
            # fac_data = zeros(n)
            # fac_data[fac_variables.==fac] = 1.0
            # if fac in ["ND_4","MD_4","IA_7"]
            #     continue
            # end

            if bigFirm & (fac=="PREMERA_BLUE_CROSS_BLUE_SHIELD_OF_ALASKA")
                continue
            end

            F[fac_variables.==fac,ind] .= 1
            ind+= 1

            feNames = vcat(feNames,Symbol(fac))
        end
    end
    return F, feNames
end


# Defining Indexing Methods on ChoiceData
Symbols = Union{Symbol, Vector{Symbol}}
getindex(m::ChoiceData, idx) = m.data[idx,:]
getindex(m::ChoiceData, idx, cols) = m.data[idx, cols]
getindex(m::ChoiceData, idx::Array{Int,1}) = m.data[idx,:]
getindex(m::ChoiceData, idx::Array{Int,1}, cols::Array{Int,1}) = m.data[idx, cols]
getindex(m::ChoiceData, idx::Symbols) = m.data[getindex.(m.index, idx),:]
getindex(m::ChoiceData, idx::Symbols, cols) = m.data[getindex.(m.index, idx),cols]

# Define other retrieval methods on ChoiceData
person(m::ChoiceData)      = m[m._person]
product(m::ChoiceData)      = m[m._product]
prodchars(m::ChoiceData)   = m[m._prodchars]
prodchars0(m::ChoiceData)   = m[m._prodchars_0]
choice(m::ChoiceData)      = m[m._choice]
demoRaw(m::ChoiceData)     = m[m._demoRaw]
weight(m::ChoiceData)      = m[m._wgt]
ageRate(m::ChoiceData)      = m[m._ageRate]
ageHCC(m::ChoiceData)      = m[m._ageHCC]
unins(m::ChoiceData)       = m[m._unins]
rInd(m::ChoiceData)       = m[m._rInd]
rIndS(m::ChoiceData)       = m[m._rIndS]


fixedEffects(m::ChoiceData)= m.fixedEffects
fixedEffects(m::ChoiceData,idx)= view(m.fixedEffects,:,idx)

########################################################################
#################### Iterating over People ############################
########################################################################

# Quickly Generate Subsets on People
function subset(d::T, idx) where T<:ModelData

    data = d.data[:,idx]
    fixedEf = d.fixedEffects
    #fixedEf = view(d.fixedEffects,:,idx)
#    people = data[d._person,:]

    # Don't subset any other fields for now...
    return T(data,d.pdata,d.rMoments,d.tMoments,d.st_share,
    fixedEf,
    # Index of the column names
    d.index,
    # Names of rows (columns of input data)
    d.prodchars,   # Product Characteristics
    d.prodchars_0,   # Product Characteristics
    d.choice,      # Binary choice indicator
    d.demoRaw,    # Household Demographics - raw
    d.wgt,     # Demographic Fixed Effects
    d.unins,    # Outside Option Share
    # Precomputed Indices
    d._person,
    d._product,
    d._prodchars,
    d._prodchars_0,
    d._choice,
    d._demoRaw,
    d._wgt,
    d._ageRate,
    d._ageHCC,
    d._unins,
    d._rInd,
    d._rIndS,
    d._personIDs,
    d._personDict,
    d._productDict,
    d._rel_fe_Dict,
    d._tMomentDict,
    d._stDict)
end

########## People Iterator ###############
# Define an Iterator Type
mutable struct PersonIterator
    data
    id
end

# Construct an iterator to loop over people
function eachperson(m::ChoiceData)
    #ids = sort(unique(person(m)))
    ids = m._personIDs
    return PersonIterator(m, ids)
end

# Iterate Code in 0.6
# start(itr::PersonIterator) = 1
# function next(itr::PersonIterator, state)
#     # Get the current market
#     id = itr.id[state]
#
#     # Find which indices to use
#     idx = itr.data._personDict[id]
#
#     # Subset the data to just look at the current market
#     submod = subset(itr.data, idx)
#
#     return submod, state + 1
# end
# done(itr::PersonIterator, state) = state > length(itr.id)

function Base.iterate(iter::PersonIterator, state=1)

    if state> length(iter.id)
        return nothing
    end

    # Get the current market
    id = iter.id[state]

    # Find which indices to use
    idx = iter.data._personDict[id]

    # Subset the data to just look at the current market
    submod = subset(iter.data, idx)

    return (submod, state + 1)
end


###########################################################
### Model Object ########


abstract type LogitModel end

mutable struct InsuranceLogit <: LogitModel
    # Dictionary of Parameters and implied lengths
    parLength::Dict{Symbol, Int64}
    # ChoiceData struct
    data::ChoiceData

    #Store Halton Draws
    draws::Array{Float64,2}


    # Product Level Data
    # Separate vectors, all sorted by product
    prods::Vector{Int64}
    shares::Vector{Float64}
    lives::Vector{Float64}
    Γ_j::Vector{Float64}
    AV_j::Vector{Float64}
    #Unique firm-level deltas
    deltas
end


function InsuranceLogit(c_data::ChoiceData,haltonDim::Int;
    nested=false,riskscores=true)
    # Construct the model instance

    # Get Parameter Lengths
    γlen = size(demoRaw(c_data),1)
    β0len = size(prodchars0(c_data),1)
    βlen = size(prodchars(c_data),1)
    flen = size(fixedEffects(c_data),1)

    if haltonDim==1 & !nested
        σlen = 0
    elseif (haltonDim>1) & (!nested)
        σlen = (size(prodchars(c_data),1)-1)
    elseif (haltonDim==1) & nested
        σlen =1
    else
        error("Nesting Parameter not right")
        return
    end

    #total = 1 + γlen + β0len + γlen + flen + σlen
    total = γlen + β0len + γlen + flen + σlen
    parLength = Dict(:γ=>γlen,:β0=>β0len,:β=>βlen,:FE=>flen,
    :σ => σlen, :All=>total)

    # Initialize Halton Draws
    # These are the same across all individuals
    #draws = permutedims(MVHaltonNormal(haltonDim,2),(2,1))
    #draws = MVHaltonNormal(haltonDim,4;scrambled=false)

    draws = MVHalton(haltonDim,1;scrambled=false)
    if riskscores
        risk_draws = Matrix{Float64}(undef,haltonDim,size(c_data.rMoments,1))
        for mom in 1:size(c_data.rMoments,1)
            any = 1 - c_data.rMoments[mom,2]
            μ_risk = c_data.rMoments[mom,3]
            std_risk = sqrt(c_data.rMoments[mom,4])
            for ind in 1:haltonDim
                if draws[ind]<any
                    risk_draws[ind,mom] = 0
                else
                    d = (draws[ind] - any)/(1-any)
                    log_r = norminvcdf(d)*std_risk + μ_risk
                    risk_draws[ind,mom] = exp(log_r)
                end
            end
        end
    else
        risk_draws = draws
    end


    # Initialize Empty value prediction objects
    n, k = size(c_data.data)
    # Copy Firm Level Data for Changing in Estimation
    pmat = c_data.pdata
    pmat[:delta] = 1.0
    sort!(pmat)

    d = InsuranceLogit(parLength,
                        c_data,
                        risk_draws,
                        pmat[:Product],pmat[:Share],pmat[:lives],
                        pmat[:Gamma_j],pmat[:AV],pmat[:delta])
    return d
end
