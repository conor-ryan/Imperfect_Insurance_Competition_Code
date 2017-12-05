using DataFrames
import Base.start, Base.next, Base.done, Base.getindex, Base.setindex!

abstract type
    ModelData
end

# Make a type to interface with the data for discrete choice models
type CarData <: ModelData
    # Matrix of the data
    data
    # Product Identifier
    names
    # Index of the column names
    index
    # Names of columns
    chars # Observable Characteristics
    price       # Prices
    shares      # Market Shares
    markets     # Market Identifier
    firm        # Firm Identifier
    outside     # Outside Option Share
    # Precomputed Indices
    _chars
    _price
    _shares
    _markets
    _firm
    _outside
    _instruments
end

function CarData(data::DataFrame;
        chars=[:constant, :horsepower_weight, :length_width,
                    :ac_standard, :miles_per_dollar],
        price=[:price],
        shares=[:market_share],
        markets=[:year],
        names=[:vehicle_name],
        firm=[:firmid],
        outside=[:s0])

    # Get the size of the data
    n, k = size(data)

    # Convert everything to an array once for performance
    X = hcat(Array{Float64}(data[chars]))
    p = hcat(Array{Float64}(data[price]))
    s = hcat(Array{Float64}(data[shares]))
    t = hcat(Array{Float64}(data[markets]))
    f = hcat(Array{Float64}(data[firm]))
    s0= hcat(Array{Float64}(data[outside]))
    name = Array(data[names])

    index = Dict{Symbol, Int}()
    dmat = Matrix{Float64}(n,0)

    j = 0
    for (d, names) in zip([X, p, s, t, f, s0], [chars, price, shares,
        markets, firm, outside])
        for i=1:size(d,2)
            j+=1
            dmat = hcat(dmat, d[:,i])
            index[names[i]] = j
        end
    end

    # Precompute the indices
    _chars = getindex.(index, chars)
    _price = getindex.(index, price)
    _shares = getindex.(index, shares)
    _markets = getindex.(index, markets)
    _outside = getindex.(index, outside)
    _firm = getindex.(index, firm)

    # Make the data object
    m = CarData(dmat, name, index, chars, price, shares, markets,
            firm, outside, _chars, _price, _shares, _markets,
            _firm, _outside, [])
    return m
end

# Indexing CarData
Symbols = Union{Symbol, Vector{Symbol}}
getindex(m::CarData, idx) = m.data[:,idx]
getindex(m::CarData, rows, idx) = m.data[rows, idx]
getindex(m::CarData, idx::Symbols) = m.data[:, getindex.(m.index, idx)]
getindex(m::CarData, rows, idx::Symbols) = m.data[rows, getindex.(m.index, idx)]

# Get the periods
observables(m::CarData) = m[vcat(m._chars, m._price)]
chars(m::CarData)       = m[m._chars]
price(m::CarData)       = m[m._price]
shares(m::CarData)      = m[m._shares]
markets(m::CarData)     = m[m._markets]
firm(m::CarData)        = m[m._firm]
outside(m::CarData)     = m[m._outside]
instruments(m::CarData) = m[m._instruments]
########################################################################
#################### Construct the Instruments #########################
########################################################################

function construct_instruments!(m::CarData)

    # Get the firm names
    f = firm(m)
    flist = unique(f)

    # Get the market location
    mark = markets(m)

    # Get the nonconstant observables
    obs = [val for val in m.chars if val != :constant]
    X   = m[obs]

    # Loop over the observations and make the instruments
    n, k = size(X)
    Z = zeros(n, 3k)
    for i=1:n
        # Get the firm and market
        fname   = f[i]
        t       = mark[i]

        # Find which observations are in the same market
        idx1 = mark.== t

        # Find which observations are in the same firm
        idx2 = f.==fname

        # The characteristics are endogenous
        Z1 = X[i,:]'

        # Add up all the characteristics from the other products the
        # firm produces
        idx3 = idx1 .& idx2
        idx3[i] = false
        Z2 = sum(X[find(idx3), :],1)

        # Add up all the characteristics from products that other firms
        # in the market produce
        idx4 = idx1 .& .!idx2
        Z3 = sum(X[find(idx4),:],1)

        # Store the characteristics
        Z[i,:] = hcat(Z1, Z2, Z3)

    end

    l = size(m.data, 2)
    m._instruments = vcat(collect(l+1:l+3k), m.index[:constant])
    m.data = hcat(m.data, Z)

    return m
end

########################################################################
#################### Iterating over Markets ############################
########################################################################

# Quickly Generate Subsets
function subset{T<:ModelData}(m::T, idx)
    data = m.data[idx, :]
    names = m.names[idx]

    args = []
    for field in fieldnames(m)
        if !(field in [:data, :names])
            push!(args, getfield(m, field))
        end
    end

    return T(data, names, args...)
end

# Construct an iterator to loop over the markets
function eachmarket(m::CarData)
    marks = sort(unique(markets(m)))
    return MarketIterator(m, marks)
end

# Define an Iterator Type
type MarketIterator
    data
    markets
end

# Make it actually iterable
start(itr::MarketIterator) = 1
function next(itr::MarketIterator, state)
    # Get the current market
    market = itr.markets[state]

    # Find which indices to use
    data = itr.data
    idx = find( markets(data).== market)

    # Subset the data to just look at the current market
    submod = subset(itr.data, idx)

    return submod, state + 1
end
done(itr::MarketIterator, state) = state > length(itr.markets)
