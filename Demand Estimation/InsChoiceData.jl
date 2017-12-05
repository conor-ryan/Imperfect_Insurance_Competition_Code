using DataFrames
import Base.start, Base.next, Base.done, Base.getindex, Base.setindex!

abstract type
    ModelData
end


# Make a type to interface with the data for discrete choice models
type ChoiceData <: ModelData
    # Matrix of the data
    data
    # Identifiers
    person      # Application ID
    firm       # Firm ID
    market      # Rating Area - Metal Level
    product     # Firm offerings in each market
    # Index of the column names
    index
    # Names of columns
    riskchars   # Risk Characteristics
    price       # Insurance Premium
    choice      # Binary choice indicator
    demoRaw    # Household Demographics - raw
    demoFE     # Demographic Fixed Effects
    unins     # Outside Option Share
    # Precomputed Indices
    _person
    _riskchars
    _price
    _choice
    _demoRaw
    _demoFE
    _unins
end

function ChoiceData(data::DataFrame;
        person=[:Person],
        firm=[:Firm],
        market=[:Market],
        product=[:Product],
        riskchars=[:MedDeduct,:MedOOP],
        price=[:Price],
        choice=[:Y],
        demoRaw=[:constant,:Age,:Family,:LowIncome],
        demoFE=[:F0_Y0_LI0,:F0_Y0_LI1,
                    :F0_Y1_LI0,:F0_Y1_LI1,
                    :F1_Y0_LI0,:F1_Y0_LI1,
                    :F1_Y1_LI0,:F1_Y1_LI1,],
        unins=[:unins_rate])

    # Get the size of the data
    n, k = size(data)

    # Convert everything to an array once for performance
    i = hcat(Array{Float64}(data[person]))
    f = hcat(Array(data[firm]))
    m = hcat(Array(data[market]))
    j = hcat(Array(data[product]))
    X = hcat(Array{Float64}(data[riskchars]))
    p = hcat(Array{Float64}(data[price]))
    y = hcat(Array{Float64}(data[choice]))
    Z = hcat(Array{Float64}(data[demoRaw]))
    D = hcat(Array{Float64}(data[demoFE]))
    s0= hcat(Array{Float64}(data[unins]))

    index = Dict{Symbol, Int}()
    dmat = Matrix{Float64}(n,0)

    # Create a data matrix, only including person id
    # Maybe implement firm or product number ids
    k = 0
    for (d, var) in zip([i,X, p, y, Z, D, s0], [person,riskchars, price, choice,
        demoRaw,demoFE,unins])
        for l=1:size(d,2)
            k+=1
            dmat = hcat(dmat, d[:,l])
            index[var[l]] = k
        end
    end

    # Precompute the indices
    _person = getindex.(index,person)
    _riskchars = getindex.(index, riskchars)
    _price = getindex.(index, price)
    _choice = getindex.(index, choice)
    _demoRaw = getindex.(index, demoRaw)
    _demoFE = getindex.(index, demoFE)
    _unins = getindex.(index, unins)


    # Make the data object
    m = ChoiceData(dmat, i, f, m, j, index, riskchars, price, choice, demoRaw,
            demoFE, unins, _person,_riskchars, _price, _choice, _demoRaw,
            _demoFE, _unins)
    return m
end

# Defining Indexing Methods on ChoiceData
Symbols = Union{Symbol, Vector{Symbol}}
getindex(m::ChoiceData, idx) = m.data[:,idx]
getindex(m::ChoiceData, rows, idx) = m.data[rows, idx]
getindex(m::ChoiceData, idx::Symbols) = m.data[:, getindex.(m.index, idx)]
getindex(m::ChoiceData, rows, idx::Symbols) = m.data[rows, getindex.(m.index, idx)]

# Define other retrieval methods on ChoiceData
observables(m::ChoiceData) = m[vcat(m._riskchars, m._price,
                                        m._demoRaw,m._demoFE)]
person(m::ChoiceData)      = m[m._person]
riskchars(m::ChoiceData)   = m[m._riskchars]
price(m::ChoiceData)       = m[m._price]
choice(m::ChoiceData)      = m[m._choice]
demoRaw(m::ChoiceData)     = m[m._demoRaw]
demoFE(m::ChoiceData)      = m[m._demoFE]
unins(m::ChoiceData)       = m[m._unins]

########################################################################
#################### Iterating over People ############################
########################################################################

# Quickly Generate Subsets on People
function subset{T<:ModelData}(m::T, idx)
    data = m.data[idx, :]
    people = m.person[idx]

    args = []
    for field in fieldnames(m)
        if !(field in [:data, :person])
            push!(args, getfield(m, field))
        end
    end
    # ....?
    return T(data, names, args...)
end

# Define an Iterator Type
type PersonIterator
    data
    id
end

# Construct an iterator to loop over people
function eachperson(m::ChoiceData)
    ids = sort(unique(person(m)))
    return PersonIterator(m, ids)
end

# Make it actually iterable
start(itr::PersonIterator) = 1
function next(itr::PersonIterator, state)
    # Get the current market
    id = itr.id[state]

    # Find which indices to use
    data = itr.data
    idx = find(person(data).== id)

    # Subset the data to just look at the current market
    submod = subset(itr.data, idx)

    return submod, state + 1
end
done(itr::PersonIterator, state) = state > length(itr.markets)
