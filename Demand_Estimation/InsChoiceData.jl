using DataFrames
import Base.start, Base.next, Base.done, Base.getindex, Base.setindex!

abstract type
    ModelData
end


# Make a type to interface with the data for discrete choice models
struct ChoiceData <: ModelData
    # Matrix of the data (transposed, pre-sorted)
    data::Matrix{Float64}
    # Matrix of the product level data (pre-sorted)
    pdata::DataFrame
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
    _prodchars::Array{Int,1}
    _prodchars_0::Array{Int,1}
    _choice::Array{Int,1}
    _demoRaw::Array{Int,1}
    _wgt::Array{Int,1}
    _unins::Array{Int,1}

    # ID Lookup Mappings
    _personIDs::Array{Float64,1}
    _personDict::Dict{Int, UnitRange{Int}}
    _productDict::Dict{Int, Array{Int,1}}

    _rel_fe_Dict::Dict{Real,Array{Int64,1}}
end

function ChoiceData(data_choice::DataFrame,data_market::DataFrame;
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
        fixedEffects=Vector{Symbol}(0),
        wgt=[:N],
        unins=[:unins_rate])

    # Get the size of the data
    n, k = size(data_choice)

    # Convert everything to an array once for performance
    i = hcat(Array{Float64}(data_choice[person]))
    j = hcat(Array(data_choice[product]))
    X = hcat(Array{Float64}(data_choice[prodchars]))
    X_0 = hcat(Array{Float64}(data_choice[prodchars_0]))
    y = hcat(Array{Float64}(data_choice[choice]))
    Z = hcat(Array{Float64}(data_choice[demoRaw]))
    w = hcat(Array{Float64}(data_choice[wgt]))
    s0= hcat(Array{Float64}(data_choice[unins]))

    println("Create Fixed Effects")
    F, feNames = build_FE(data_choice,fixedEffects)
    F = permutedims(F,(2,1))

    index = Dict{Symbol, Int}()
    dmat = Matrix{Float64}(n,0)

    # Create a data matrix, only including person id
    println("Put Together Data non FE data together")
    k = 0
    for (d, var) in zip([i,X,X_0, y, Z,w, s0], [person,prodchars,
        prodchars_0,choice, demoRaw,wgt,unins])
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
    _person = getindex.(index,person)
    _prodchars = getindex.(index, prodchars)
    _prodchars_0 = getindex.(index, prodchars_0)
    _choice = getindex.(index, choice)
    _demoRaw = getindex.(index, demoRaw)
    _wgt = getindex.(index, wgt)
    _unins = getindex.(index, unins)

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
    _productDict = build_ProdDict(j)
    # allprods = sort(unique(j))
    # _productDict = Dict{Real, Array{Int}}()
    # for id in allprods
    #     _productDict[id] = findin(j,id)
    # end

    # Relevant Parameters Per Person
    rel_fe_Dict = Dict{Real,Array{Int64,1}}()
    for (id,idxitr) in _personDict
        F_t = view(F,:,idxitr)
        pars_relevant = find(maximum(F_t,2))
        rel_fe_Dict[id] = pars_relevant
    end


    # Make the data object
    m = ChoiceData(dmat,data_market,F, index, prodchars,prodchars_0,
            choice, demoRaw,wgt, unins, _person, _prodchars,_prodchars_0,
            _choice, _demoRaw, _wgt, _unins,uniqids,_personDict,_productDict,
            rel_fe_Dict)
    return m
end

function build_ProdDict{T,N}(j::Array{T,N})
    allprods = unique(j)
    sort!(allprods)
    _productDict = Dict{Real, Array{Int64,1}}()

    for id in allprods
        _productDict[id] = find(j.==id)
    end
    return _productDict
end

function build_FE{T}(data_choice::DataFrame,fe_list::Vector{T})
    # Create Fixed Effects
    n, k = size(data_choice)
    L = 0

    # No Fixed effects for empty lists
    if typeof(fe_list)!=Vector{Symbol}
        println("No Fixed Effects")
        F = Matrix{Float64}(n,L)
        feNames = Vector{Symbol}(0)
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
        L+=num_effects
    end

    F = zeros(n,L)
    feNames = Vector{Symbol}(0)
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

            F[fac_variables.==fac,ind] = 1
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
prodchars(m::ChoiceData)   = m[m._prodchars]
prodchars0(m::ChoiceData)   = m[m._prodchars_0]
choice(m::ChoiceData)      = m[m._choice]
demoRaw(m::ChoiceData)     = m[m._demoRaw]
weight(m::ChoiceData)      = m[m._wgt]
unins(m::ChoiceData)       = m[m._unins]


fixedEffects(m::ChoiceData)= m.fixedEffects
fixedEffects(m::ChoiceData,idx)= view(m.fixedEffects,:,idx)

########################################################################
#################### Iterating over People ############################
########################################################################

# Quickly Generate Subsets on People
function subset{T<:ModelData}(d::T, idx)

    data = d.data[:,idx]
    fixedEf = d.fixedEffects
    #fixedEf = view(d.fixedEffects,:,idx)
#    people = data[d._person,:]

    # Don't subset any other fields for now...
    return T(data,d.pdata,fixedEf,
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
    d._prodchars,
    d._prodchars_0,
    d._choice,
    d._demoRaw,
    d._wgt,
    d._unins,
    d._personIDs,
    d._personDict,
    d._productDict,
    d._rel_fe_Dict)
end

########## People Iterator ###############
# Define an Iterator Type
type PersonIterator
    data
    id
end

# Construct an iterator to loop over people
function eachperson(m::ChoiceData)
    #ids = sort(unique(person(m)))
    ids = m._personIDs
    return PersonIterator(m, ids)
end

# Make it actually iterable
start(itr::PersonIterator) = 1
function next(itr::PersonIterator, state)
    # Get the current market
    id = itr.id[state]

    # Find which indices to use
    idx = itr.data._personDict[id]

    # Subset the data to just look at the current market
    submod = subset(itr.data, idx)

    return submod, state + 1
end
done(itr::PersonIterator, state) = state > length(itr.id)


###########################################################
### Model Object ########


abstract type LogitModel end

type InsuranceLogit <: LogitModel
    # Dictionary of Parameters and implied lengths
    parLength::Dict{Symbol, Int}
    # ChoiceData struct
    data::ChoiceData

    #Store Halton Draws
    draws::Array{Float64,2}


    # Product Level Data
    # Separate vectors, all sorted by product
    prods
    shares
    #Unique firm-level deltas
    deltas
end


function InsuranceLogit(c_data::ChoiceData,haltonDim::Int;nested=false)
    # Construct the model instance

    # Get Parameter Lengths
    γlen = size(demoRaw(c_data),1)
    β0len = size(prodchars0(c_data),1)
    βlen = size(prodchars(c_data),1)
    flen = size(fixedEffects(c_data),1)

    if haltonDim==1 & !nested
        σlen = 0
    elseif haltonDim>1 & !nested
        σlen = (size(prodchars(c_data),1)-1)
    elseif haltonDim==1 & nested
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
    draws = MVHaltonNormal(haltonDim,1;scrambled=false)

    # Initialize Empty value prediction objects
    n, k = size(c_data.data)
    # Copy Firm Level Data for Changing in Estimation
    pmat = c_data.pdata
    pmat[:delta] = 1.0
    sort!(pmat)

    d = InsuranceLogit(parLength,
                        c_data,
                        draws,
                        pmat[:Product],pmat[:Share],pmat[:delta])
    return d
end
