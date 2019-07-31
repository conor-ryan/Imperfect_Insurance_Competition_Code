# Update δ
@views sliceMean(x::Vector{T},idx::Array{Int64,1}) where T = mean(x[idx])
@views function sliceMean_wgt(x::Vector{T},w::Vector{S},idx::Array{Int64,1}) where {T,S}
    wgts = w[idx]
    return sum(x[idx].*wgts)/sum(wgts)
end

@views function sliceSum_wgt(x::Vector{T},w::Vector{S},idx::Array{Int64,1}) where {T,S}
    tot = 0.0
    @inbounds @fastmath @simd for q in idx
        tot+= x[q]*w[q]
    end
    return tot
end
@views function sliceSum_wgt(x::SubArray,w::Vector{S},idx::Array{Int64,1}) where S
    tot = 0.0
    @inbounds @fastmath @simd for q in idx
        tot+= x[q]*w[q]
    end
    return tot
end

@views function sliceSum_wgt(k::Int64,x::Matrix{T},w::Vector{S},idx::Array{Int64,1}) where {T,S}
    tot = 0.0
    @inbounds @fastmath @simd for q in idx
        tot+= x[k,q]*w[q]
    end
    return tot
end

@views function sliceSum_wgt(x::Vector{T},y::Vector{Float64},w::Vector{T},
                                    idx::Array{Int64,1}) where T
    tot = 0.0
    @inbounds @fastmath @simd for q in idx
        tot+= x[q]*y[q]*w[q]
    end
    return tot
end

@views function sliceSum_wgt(x::Vector{T},y::SubArray,w::Vector{T},
                                    idx::Array{Int64,1}) where T
    tot = 0.0
    @inbounds @fastmath @simd for q in idx
        tot+= x[q]*y[q]*w[q]
    end
    return tot
end

@views function sliceSum_wgt(k::Int64,x::Vector{T},y::Matrix{Float64},w::Vector{T},
                                    idx::Array{Int64,1}) where T
    tot = 0.0
    @inbounds @fastmath @simd for q in idx
        tot+= x[q]*y[k,q]*w[q]
    end
    return tot
end

function inlist(x::Vector{T},y::Vector{T}) where T
    bits = BitArray{1}(undef,length(x))
    # min_y = minimum(y)
    for (n,i) in enumerate(x)
        bits[n] = i in y
    end
    return bits
end

function inlist(x::UnitRange{T},y::Vector{T}) where T
    bits = BitArray{1}(undef,length(x))
    # min_y = minimum(y)
    for (n,i) in enumerate(x)
        bits[n] = i in y
    end
    return bits
end

function inlist(x::UnitRange{T},y::Vector{Union{Missing,T}}) where T
    bits = BitArray{1}(undef,length(x))
    # min_y = minimum(y)
    for (n,i) in enumerate(x)
        bits[n] = i in y
    end
    return bits
end

function enforcePosDef(H::Matrix{Float64})
    check = issuccess(cholesky(H,check=false))
    if !check
        println("Not Convex: Compute Direction of Sufficient Descent") #http://web.stanford.edu/class/cme304/docs/newton-type-methods.pdf
        E = eigen(H)
        max_eig_val = minimum(E.values)
        println("Min Eigenvalue is $max_eig_val")
        Λ = abs.(Diagonal(E.values))
        hess = E.vectors*Λ*E.vectors'
    end
    return H
end

function enforceNegDef(H::Matrix{Float64})
    check = issuccess(cholesky(-H,check=false))
    if !check
        println("Not Concave: Compute Direction of Sufficient Ascent") #http://web.stanford.edu/class/cme304/docs/newton-type-methods.pdf
        E = eigen(H)
        max_eig_val = maximum(E.values)
        println("Max Eigenvalue is $max_eig_val")
        Λ = -abs.(Diagonal(E.values))
        hess = E.vectors*Λ*E.vectors'
    end
    return H
end
