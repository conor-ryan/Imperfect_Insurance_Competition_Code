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

function boundedUpdate(p_ind::UnitRange{Int64},p::Vector{Float64},
    α::Float64,grad::Vector{Float64},bound::Float64)
    return boundedUpdate(Int.(p_ind),p,α,grad,bound)
end

function boundedUpdate(p_ind::Vector{Int64},p::Vector{Float64},
    α::Float64,grad::Vector{Float64},bound::Float64)

    update = α*grad
    if length(p_ind)==0
        return update
    end

    bound_ind = p_ind[findall((p[p_ind].>=bound) .& (update[p_ind].>0))]

    if length(bound_ind)==0
        return update
    end

    update = zeros(length(p))
    unbounded = .!(inlist(1:length(p),bound_ind))

    update[unbounded] = α*grad[unbounded]
    return update
end


function boundedUpdate(p_ind::UnitRange{Int64},p::Vector{Float64},hess::Matrix{Float64},grad::Vector{Float64},bound::Float64)
    return boundedUpdate(Int.(p_ind),p,hess,grad,bound)
end

function boundedUpdate(p_ind::Vector{Int64},p::Vector{Float64},hess::Matrix{Float64},grad::Vector{Float64},bound::Float64)
    H_k_f = inv(hess)
    update = -H_k_f*grad

    if length(p_ind)==0
        println("Standard Update 1")
        return update, H_k_f
    end

    bound_ind = p_ind[findall((p[p_ind].>=bound) .& (update[p_ind].>0))]

    if length(bound_ind)==0
        println("Standard Update 2")
        return update, H_k_f
    end

    update = zeros(length(p))
    unbounded = .!(inlist(1:length(p),bound_ind))

    H_k = inv(hess[unbounded,unbounded])
    update[unbounded] = -H_k*grad[unbounded]

    H_k_f[unbounded,unbounded] = H_k

    println("Bounded Update")
    return update, H_k_f
end

function boundedUpdate(p_ind::UnitRange{Int64},p::Vector{Float64},
    Eye::Matrix{Float64},H_last::Matrix{Float64},Δx::Vector{Float64},Δy::Vector{Float64},
    grad::Vector{Float64},bound::Float64)
    return boundedUpdate(Int.(p_ind),p,Eye,H_last,Δx,Δy,grad,bound)
end

function boundedUpdate(p_ind::Vector{Int64},p::Vector{Float64},
    Eye::Matrix{Float64},H_last::Matrix{Float64},Δx::Vector{Float64},Δy::Vector{Float64},
    grad::Vector{Float64},bound::Float64)
    H_k_f = (Eye - (Δx*Δy')./(Δy'*Δx) )*H_last*(Eye - (Δy*Δx')./(Δy'*Δx) ) + (Δx*Δx')./(Δy'*Δx)
    update = -H_k_f*grad

    if length(p_ind)==0
        println("Standard Update 1")
        return update, H_k_f
    end

    bound_ind = p_ind[findall((p[p_ind].>=bound) .& (update[p_ind].>0))]

    if length(bound_ind)==0
        println("Standard Update 2")
        return update, H_k_f
    end

    update = zeros(length(p))
    unbounded = .!(inlist(1:length(p),bound_ind))

    Δxk = Δx[unbounded]
    Δyk = Δy[unbounded]

    H_k = (Eye[unbounded,unbounded] - (Δxk*Δyk')./(Δyk'*Δxk) )*H_last[unbounded,unbounded]*(Eye[unbounded,unbounded] - (Δyk*Δxk')./(Δyk'*Δxk) ) + (Δxk*Δxk')./(Δyk'*Δxk)
    update[unbounded] = -H_k*grad[unbounded]

    H_k_f[unbounded,unbounded] = H_k

    println("Bounded Update")
    return update, H_k_f
end
