function calc_derSums_xyz!(n::Int64,s_n::Vector{T},
                    X_mat::Matrix{Float64},Y::Matrix{Float64},
                    Z::Matrix{Float64},
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    dμ_ij_x_sums = 0.0
    dμ_ij_y_sums = 0.0
    dμ_ij_z_sums = 0.0

    dμ_ij_xy_sums = 0.0
    dμ_ij_xz_sums = 0.0
    dμ_ij_yz_sums = 0.0

    dμ_ij_xyz_sums = 0.0

    K = length(δ)
    n1 = max(n,1)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath x = X_mat[n+1,k]
        @inbounds @fastmath y = Y[n+1,k]
        @inbounds @fastmath z = Z[n+1,k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
        @fastmath dμ_ij_x_sums+= u*x
        @fastmath dμ_ij_y_sums+= u*y
        @fastmath dμ_ij_z_sums+= u*z
         @fastmath dμ_ij_xy_sums+= u*x*y
         @fastmath dμ_ij_xz_sums+= u*x*z
         @fastmath dμ_ij_yz_sums+= u*y*z
         @fastmath dμ_ij_xyz_sums+= u*x*y*z
    end

    return dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_z_sums,dμ_ij_xy_sums,dμ_ij_xz_sums,dμ_ij_yz_sums,dμ_ij_xyz_sums
end

function calc_derSums_xyz!(n::Int64,s_n::Vector{T},
                    X_mat::Matrix{Float64},Y::Matrix{Float64},
                    Z::Vector{Float64},
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    dμ_ij_x_sums = 0.0
    dμ_ij_y_sums = 0.0
    dμ_ij_z_sums = 0.0

    dμ_ij_xy_sums = 0.0
    dμ_ij_xz_sums = 0.0
    dμ_ij_yz_sums = 0.0

    dμ_ij_xyz_sums = 0.0

    K = length(δ)
    n1 = max(n,1)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath x = X_mat[n+1,k]
        @inbounds @fastmath y = Y[n+1,k]
        @inbounds @fastmath z = Z[k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
         @fastmath dμ_ij_x_sums+= u*x
         @fastmath dμ_ij_y_sums+= u*y
         @fastmath dμ_ij_z_sums+= u*z
         @fastmath dμ_ij_xy_sums+= u*x*y
         @fastmath dμ_ij_xz_sums+= u*x*z
         @fastmath dμ_ij_yz_sums+= u*y*z
         @fastmath dμ_ij_xyz_sums+= u*x*y*z
    end

    return dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_z_sums,dμ_ij_xy_sums,dμ_ij_xz_sums,dμ_ij_yz_sums,dμ_ij_xyz_sums
end

function calc_derSums_xyz!(n::Int64,s_n::Vector{T},
                    X_mat::Matrix{Float64},Y::Matrix{Float64},
                    z::Float64,
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    dμ_ij_x_sums = 0.0
    dμ_ij_y_sums = 0.0
    dμ_ij_z_sums = 0.0

    dμ_ij_xy_sums = 0.0
    dμ_ij_xz_sums = 0.0
    dμ_ij_yz_sums = 0.0

    dμ_ij_xyz_sums = 0.0

    K = length(δ)
    n1 = max(n,1)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath x = X_mat[n+1,k]
        @inbounds @fastmath y = Y[n+1,k]

         @fastmath s_n[k] = u/μ_ij_sums_n
         @fastmath dμ_ij_x_sums+= u*x
         @fastmath dμ_ij_y_sums+= u*y
         @fastmath dμ_ij_z_sums+= u*z
         @fastmath dμ_ij_xy_sums+= u*x*y
         @fastmath dμ_ij_xz_sums+= u*x*z
         @fastmath dμ_ij_yz_sums+= u*y*z
         @fastmath dμ_ij_xyz_sums+= u*x*y*z
    end

    return dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_z_sums,dμ_ij_xy_sums,dμ_ij_xz_sums,dμ_ij_yz_sums,dμ_ij_xyz_sums
end

function calc_derSums_xyz!(n::Int64,s_n::Vector{T},
                    X_mat::Matrix{Float64},Y::Vector{Float64},
                    Z::Matrix{Float64},
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    dμ_ij_x_sums = 0.0
    dμ_ij_y_sums = 0.0
    dμ_ij_z_sums = 0.0

    dμ_ij_xy_sums = 0.0
    dμ_ij_xz_sums = 0.0
    dμ_ij_yz_sums = 0.0

    dμ_ij_xyz_sums = 0.0

    K = length(δ)
    n1 = max(n,1)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath x = X_mat[n+1,k]
        @inbounds @fastmath y = Y[k]
        @inbounds @fastmath z = Z[n+1,k]

         @fastmath s_n[k] = u/μ_ij_sums_n
         @fastmath dμ_ij_x_sums+= u*x
         @fastmath dμ_ij_y_sums+= u*y
         @fastmath dμ_ij_z_sums+= u*z
         @fastmath dμ_ij_xy_sums+= u*x*y
         @fastmath dμ_ij_xz_sums+= u*x*z
         @fastmath dμ_ij_yz_sums+= u*y*z
         @fastmath dμ_ij_xyz_sums+= u*x*y*z
    end

    return dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_z_sums,dμ_ij_xy_sums,dμ_ij_xz_sums,dμ_ij_yz_sums,dμ_ij_xyz_sums
end

function calc_derSums_xyz!(n::Int64,s_n::Vector{T},
                    X_mat::Matrix{Float64},Y::Vector{Float64},
                    Z::Vector{Float64},
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    dμ_ij_x_sums = 0.0
    dμ_ij_y_sums = 0.0
    dμ_ij_z_sums = 0.0

    dμ_ij_xy_sums = 0.0
    dμ_ij_xz_sums = 0.0
    dμ_ij_yz_sums = 0.0

    dμ_ij_xyz_sums = 0.0

    K = length(δ)
    n1 = max(n,1)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath x = X_mat[n+1,k]
        @inbounds @fastmath y = Y[k]
        @inbounds @fastmath z = Z[k]

         @fastmath s_n[k] = u/μ_ij_sums_n
         @fastmath dμ_ij_x_sums+= u*x
         @fastmath dμ_ij_y_sums+= u*y
         @fastmath dμ_ij_z_sums+= u*z
         @fastmath dμ_ij_xy_sums+= u*x*y
         @fastmath dμ_ij_xz_sums+= u*x*z
         @fastmath dμ_ij_yz_sums+= u*y*z
         @fastmath dμ_ij_xyz_sums+= u*x*y*z
    end

    return dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_z_sums,dμ_ij_xy_sums,dμ_ij_xz_sums,dμ_ij_yz_sums,dμ_ij_xyz_sums
end

function calc_derSums_xyz!(n::Int64,s_n::Vector{T},
                    X_mat::Matrix{Float64},Y::Vector{Float64},
                    z::Float64,
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    dμ_ij_x_sums = 0.0
    dμ_ij_y_sums = 0.0
    dμ_ij_z_sums = 0.0

    dμ_ij_xy_sums = 0.0
    dμ_ij_xz_sums = 0.0
    dμ_ij_yz_sums = 0.0

    dμ_ij_xyz_sums = 0.0

    K = length(δ)
    n1 = max(n,1)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath x = X_mat[n+1,k]
        @inbounds @fastmath y = Y[k]

         @fastmath s_n[k] = u/μ_ij_sums_n
         @fastmath dμ_ij_x_sums+= u*x
         @fastmath dμ_ij_y_sums+= u*y
         @fastmath dμ_ij_z_sums+= u*z
         @fastmath dμ_ij_xy_sums+= u*x*y
         @fastmath dμ_ij_xz_sums+= u*x*z
         @fastmath dμ_ij_yz_sums+= u*y*z
         @fastmath dμ_ij_xyz_sums+= u*x*y*z
    end

    return dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_z_sums,dμ_ij_xy_sums,dμ_ij_xz_sums,dμ_ij_yz_sums,dμ_ij_xyz_sums
end

function calc_derSums_xyz!(n::Int64,s_n::Vector{T},
                    X_mat::Matrix{Float64},y::Float64,
                    Z::Matrix{Float64},
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    dμ_ij_x_sums = 0.0
    dμ_ij_y_sums = 0.0
    dμ_ij_z_sums = 0.0

    dμ_ij_xy_sums = 0.0
    dμ_ij_xz_sums = 0.0
    dμ_ij_yz_sums = 0.0

    dμ_ij_xyz_sums = 0.0

    K = length(δ)
    n1 = max(n,1)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath x = X_mat[n+1,k]
        @inbounds @fastmath z = Z[n+1,k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
         @fastmath dμ_ij_x_sums+= u*x
         @fastmath dμ_ij_y_sums+= u*y
         @fastmath dμ_ij_z_sums+= u*z
         @fastmath dμ_ij_xy_sums+= u*x*y
         @fastmath dμ_ij_xz_sums+= u*x*z
         @fastmath dμ_ij_yz_sums+= u*y*z
         @fastmath dμ_ij_xyz_sums+= u*x*y*z
    end

    return dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_z_sums,dμ_ij_xy_sums,dμ_ij_xz_sums,dμ_ij_yz_sums,dμ_ij_xyz_sums
end

function calc_derSums_xyz!(n::Int64,s_n::Vector{T},
                    X_mat::Matrix{Float64},y::Float64,
                    Z::Vector{Float64},
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    dμ_ij_x_sums = 0.0
    dμ_ij_y_sums = 0.0
    dμ_ij_z_sums = 0.0

    dμ_ij_xy_sums = 0.0
    dμ_ij_xz_sums = 0.0
    dμ_ij_yz_sums = 0.0

    dμ_ij_xyz_sums = 0.0

    K = length(δ)
    n1 = max(n,1)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath x = X_mat[n+1,k]
        @inbounds @fastmath z = Z[k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
         @fastmath dμ_ij_x_sums+= u*x
         @fastmath dμ_ij_y_sums+= u*y
         @fastmath dμ_ij_z_sums+= u*z
         @fastmath dμ_ij_xy_sums+= u*x*y
         @fastmath dμ_ij_xz_sums+= u*x*z
         @fastmath dμ_ij_yz_sums+= u*y*z
         @fastmath dμ_ij_xyz_sums+= u*x*y*z
    end

    return dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_z_sums,dμ_ij_xy_sums,dμ_ij_xz_sums,dμ_ij_yz_sums,dμ_ij_xyz_sums
end

function calc_derSums_xyz!(n::Int64,s_n::Vector{T},
                    X_mat::Matrix{Float64},y::Float64,
                    z::Float64,
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    dμ_ij_x_sums = 0.0
    dμ_ij_y_sums = 0.0
    dμ_ij_z_sums = 0.0

    dμ_ij_xy_sums = 0.0
    dμ_ij_xz_sums = 0.0
    dμ_ij_yz_sums = 0.0

    dμ_ij_xyz_sums = 0.0

    K = length(δ)
    n1 = max(n,1)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath x = X_mat[n+1,k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
         @fastmath dμ_ij_x_sums+= u*x
         @fastmath dμ_ij_y_sums+= u*y
         @fastmath dμ_ij_z_sums+= u*z
         @fastmath dμ_ij_xy_sums+= u*x*y
         @fastmath dμ_ij_xz_sums+= u*x*z
         @fastmath dμ_ij_yz_sums+= u*y*z
         @fastmath dμ_ij_xyz_sums+= u*x*y*z
    end

    return dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_z_sums,dμ_ij_xy_sums,dμ_ij_xz_sums,dμ_ij_yz_sums,dμ_ij_xyz_sums
end

function calc_derSums_xyz!(n::Int64,s_n::Vector{T},
                    X_mat::Vector{Float64},Y::Matrix{Float64},
                    Z::Matrix{Float64},
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    dμ_ij_x_sums = 0.0
    dμ_ij_y_sums = 0.0
    dμ_ij_z_sums = 0.0

    dμ_ij_xy_sums = 0.0
    dμ_ij_xz_sums = 0.0
    dμ_ij_yz_sums = 0.0

    dμ_ij_xyz_sums = 0.0

    K = length(δ)
    n1 = max(n,1)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath x = X_mat[k]
        @inbounds @fastmath y = Y[n+1,k]
        @inbounds @fastmath z = Z[n+1,k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
         @fastmath dμ_ij_x_sums+= u*x
         @fastmath dμ_ij_y_sums+= u*y
         @fastmath dμ_ij_z_sums+= u*z
         @fastmath dμ_ij_xy_sums+= u*x*y
         @fastmath dμ_ij_xz_sums+= u*x*z
         @fastmath dμ_ij_yz_sums+= u*y*z
         @fastmath dμ_ij_xyz_sums+= u*x*y*z
    end

    return dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_z_sums,dμ_ij_xy_sums,dμ_ij_xz_sums,dμ_ij_yz_sums,dμ_ij_xyz_sums
end

function calc_derSums_xyz!(n::Int64,s_n::Vector{T},
                    X_mat::Vector{Float64},Y::Matrix{Float64},
                    Z::Vector{Float64},
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    dμ_ij_x_sums = 0.0
    dμ_ij_y_sums = 0.0
    dμ_ij_z_sums = 0.0

    dμ_ij_xy_sums = 0.0
    dμ_ij_xz_sums = 0.0
    dμ_ij_yz_sums = 0.0

    dμ_ij_xyz_sums = 0.0

    K = length(δ)
    n1 = max(n,1)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath x = X_mat[k]
        @inbounds @fastmath y = Y[n+1,k]
        @inbounds @fastmath z = Z[k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
         @fastmath dμ_ij_x_sums+= u*x
         @fastmath dμ_ij_y_sums+= u*y
         @fastmath dμ_ij_z_sums+= u*z
         @fastmath dμ_ij_xy_sums+= u*x*y
         @fastmath dμ_ij_xz_sums+= u*x*z
         @fastmath dμ_ij_yz_sums+= u*y*z
         @fastmath dμ_ij_xyz_sums+= u*x*y*z
    end

    return dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_z_sums,dμ_ij_xy_sums,dμ_ij_xz_sums,dμ_ij_yz_sums,dμ_ij_xyz_sums
end

function calc_derSums_xyz!(n::Int64,s_n::Vector{T},
                    X_mat::Vector{Float64},Y::Matrix{Float64},
                    z::Float64,
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    dμ_ij_x_sums = 0.0
    dμ_ij_y_sums = 0.0
    dμ_ij_z_sums = 0.0

    dμ_ij_xy_sums = 0.0
    dμ_ij_xz_sums = 0.0
    dμ_ij_yz_sums = 0.0

    dμ_ij_xyz_sums = 0.0

    K = length(δ)
    n1 = max(n,1)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath x = X_mat[k]
        @inbounds @fastmath y = Y[n+1,k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
         @fastmath dμ_ij_x_sums+= u*x
         @fastmath dμ_ij_y_sums+= u*y
         @fastmath dμ_ij_z_sums+= u*z
         @fastmath dμ_ij_xy_sums+= u*x*y
         @fastmath dμ_ij_xz_sums+= u*x*z
         @fastmath dμ_ij_yz_sums+= u*y*z
         @fastmath dμ_ij_xyz_sums+= u*x*y*z
    end

    return dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_z_sums,dμ_ij_xy_sums,dμ_ij_xz_sums,dμ_ij_yz_sums,dμ_ij_xyz_sums
end

function calc_derSums_xyz!(n::Int64,s_n::Vector{T},
                    X_mat::Vector{Float64},Y::Vector{Float64},
                    Z::Matrix{Float64},
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    dμ_ij_x_sums = 0.0
    dμ_ij_y_sums = 0.0
    dμ_ij_z_sums = 0.0

    dμ_ij_xy_sums = 0.0
    dμ_ij_xz_sums = 0.0
    dμ_ij_yz_sums = 0.0

    dμ_ij_xyz_sums = 0.0

    K = length(δ)
    n1 = max(n,1)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath x = X_mat[k]
        @inbounds @fastmath y = Y[k]
        @inbounds @fastmath z = Z[n+1,k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
         @fastmath dμ_ij_x_sums+= u*x
         @fastmath dμ_ij_y_sums+= u*y
         @fastmath dμ_ij_z_sums+= u*z
         @fastmath dμ_ij_xy_sums+= u*x*y
         @fastmath dμ_ij_xz_sums+= u*x*z
         @fastmath dμ_ij_yz_sums+= u*y*z
         @fastmath dμ_ij_xyz_sums+= u*x*y*z
    end

    return dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_z_sums,dμ_ij_xy_sums,dμ_ij_xz_sums,dμ_ij_yz_sums,dμ_ij_xyz_sums
end

function calc_derSums_xyz!(n::Int64,s_n::Vector{T},
                    X_mat::Vector{Float64},Y::Vector{Float64},
                    Z::Vector{Float64},
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    dμ_ij_x_sums = 0.0
    dμ_ij_y_sums = 0.0
    dμ_ij_z_sums = 0.0

    dμ_ij_xy_sums = 0.0
    dμ_ij_xz_sums = 0.0
    dμ_ij_yz_sums = 0.0

    dμ_ij_xyz_sums = 0.0

    K = length(δ)
    n1 = max(n,1)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath x = X_mat[k]
        @inbounds @fastmath y = Y[k]
        @inbounds @fastmath z = Z[k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
         @fastmath dμ_ij_x_sums+= u*x
         @fastmath dμ_ij_y_sums+= u*y
         @fastmath dμ_ij_z_sums+= u*z
         @fastmath dμ_ij_xy_sums+= u*x*y
         @fastmath dμ_ij_xz_sums+= u*x*z
         @fastmath dμ_ij_yz_sums+= u*y*z
         @fastmath dμ_ij_xyz_sums+= u*x*y*z
    end

    return dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_z_sums,dμ_ij_xy_sums,dμ_ij_xz_sums,dμ_ij_yz_sums,dμ_ij_xyz_sums
end

function calc_derSums_xyz!(n::Int64,s_n::Vector{T},
                    X_mat::Vector{Float64},Y::Vector{Float64},
                    z::Float64,
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    dμ_ij_x_sums = 0.0
    dμ_ij_y_sums = 0.0
    dμ_ij_z_sums = 0.0

    dμ_ij_xy_sums = 0.0
    dμ_ij_xz_sums = 0.0
    dμ_ij_yz_sums = 0.0

    dμ_ij_xyz_sums = 0.0

    K = length(δ)
    n1 = max(n,1)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath x = X_mat[k]
        @inbounds @fastmath y = Y[k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
         @fastmath dμ_ij_x_sums+= u*x
         @fastmath dμ_ij_y_sums+= u*y
         @fastmath dμ_ij_z_sums+= u*z
         @fastmath dμ_ij_xy_sums+= u*x*y
         @fastmath dμ_ij_xz_sums+= u*x*z
         @fastmath dμ_ij_yz_sums+= u*y*z
         @fastmath dμ_ij_xyz_sums+= u*x*y*z
    end

    return dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_z_sums,dμ_ij_xy_sums,dμ_ij_xz_sums,dμ_ij_yz_sums,dμ_ij_xyz_sums
end

function calc_derSums_xyz!(n::Int64,s_n::Vector{T},
                    X_mat::Vector{Float64},y::Float64,
                    Z::Matrix{Float64},
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    dμ_ij_x_sums = 0.0
    dμ_ij_y_sums = 0.0
    dμ_ij_z_sums = 0.0

    dμ_ij_xy_sums = 0.0
    dμ_ij_xz_sums = 0.0
    dμ_ij_yz_sums = 0.0

    dμ_ij_xyz_sums = 0.0

    K = length(δ)
    n1 = max(n,1)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath x = X_mat[k]
        @inbounds @fastmath z = Z[n+1,k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
         @fastmath dμ_ij_x_sums+= u*x
         @fastmath dμ_ij_y_sums+= u*y
         @fastmath dμ_ij_z_sums+= u*z
         @fastmath dμ_ij_xy_sums+= u*x*y
         @fastmath dμ_ij_xz_sums+= u*x*z
         @fastmath dμ_ij_yz_sums+= u*y*z
         @fastmath dμ_ij_xyz_sums+= u*x*y*z
    end

    return dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_z_sums,dμ_ij_xy_sums,dμ_ij_xz_sums,dμ_ij_yz_sums,dμ_ij_xyz_sums
end

function calc_derSums_xyz!(n::Int64,s_n::Vector{T},
                    X_mat::Vector{Float64},y::Float64,
                    Z::Vector{Float64},
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    dμ_ij_x_sums = 0.0
    dμ_ij_y_sums = 0.0
    dμ_ij_z_sums = 0.0

    dμ_ij_xy_sums = 0.0
    dμ_ij_xz_sums = 0.0
    dμ_ij_yz_sums = 0.0

    dμ_ij_xyz_sums = 0.0

    K = length(δ)
    n1 = max(n,1)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath x = X_mat[k]
        @inbounds @fastmath z = Z[k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
         @fastmath dμ_ij_x_sums+= u*x
         @fastmath dμ_ij_y_sums+= u*y
         @fastmath dμ_ij_z_sums+= u*z
         @fastmath dμ_ij_xy_sums+= u*x*y
         @fastmath dμ_ij_xz_sums+= u*x*z
         @fastmath dμ_ij_yz_sums+= u*y*z
         @fastmath dμ_ij_xyz_sums+= u*x*y*z
    end

    return dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_z_sums,dμ_ij_xy_sums,dμ_ij_xz_sums,dμ_ij_yz_sums,dμ_ij_xyz_sums
end

function calc_derSums_xyz!(n::Int64,s_n::Vector{T},
                    X_mat::Vector{Float64},y::Float64,
                    z::Float64,
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    dμ_ij_x_sums = 0.0
    dμ_ij_y_sums = 0.0
    dμ_ij_z_sums = 0.0

    dμ_ij_xy_sums = 0.0
    dμ_ij_xz_sums = 0.0
    dμ_ij_yz_sums = 0.0

    dμ_ij_xyz_sums = 0.0

    K = length(δ)
    n1 = max(n,1)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath x = X_mat[k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
         @fastmath dμ_ij_x_sums+= u*x
         @fastmath dμ_ij_y_sums+= u*y
         @fastmath dμ_ij_z_sums+= u*z
         @fastmath dμ_ij_xy_sums+= u*x*y
         @fastmath dμ_ij_xz_sums+= u*x*z
         @fastmath dμ_ij_yz_sums+= u*y*z
         @fastmath dμ_ij_xyz_sums+= u*x*y*z
    end

    return dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_z_sums,dμ_ij_xy_sums,dμ_ij_xz_sums,dμ_ij_yz_sums,dμ_ij_xyz_sums
end


function calc_derSums_xyz!(n::Int64,s_n::Vector{T},
                    x::Float64,Y::Matrix{Float64},
                    Z::Matrix{Float64},
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    dμ_ij_x_sums = 0.0
    dμ_ij_y_sums = 0.0
    dμ_ij_z_sums = 0.0

    dμ_ij_xy_sums = 0.0
    dμ_ij_xz_sums = 0.0
    dμ_ij_yz_sums = 0.0

    dμ_ij_xyz_sums = 0.0

    K = length(δ)
    n1 = max(n,1)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath x = X_mat[k]
        @inbounds @fastmath y = Y[n+1,k]
        @inbounds @fastmath z = Z[n+1,k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
         @fastmath dμ_ij_x_sums+= u*x
         @fastmath dμ_ij_y_sums+= u*y
         @fastmath dμ_ij_z_sums+= u*z
         @fastmath dμ_ij_xy_sums+= u*x*y
         @fastmath dμ_ij_xz_sums+= u*x*z
         @fastmath dμ_ij_yz_sums+= u*y*z
         @fastmath dμ_ij_xyz_sums+= u*x*y*z
    end

    return dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_z_sums,dμ_ij_xy_sums,dμ_ij_xz_sums,dμ_ij_yz_sums,dμ_ij_xyz_sums
end

function calc_derSums_xyz!(n::Int64,s_n::Vector{T},
                    x::Float64,Y::Matrix{Float64},
                    Z::Vector{Float64},
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    dμ_ij_x_sums = 0.0
    dμ_ij_y_sums = 0.0
    dμ_ij_z_sums = 0.0

    dμ_ij_xy_sums = 0.0
    dμ_ij_xz_sums = 0.0
    dμ_ij_yz_sums = 0.0

    dμ_ij_xyz_sums = 0.0

    K = length(δ)
    n1 = max(n,1)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath y = Y[n+1,k]
        @inbounds @fastmath z = Z[k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
         @fastmath dμ_ij_x_sums+= u*x
         @fastmath dμ_ij_y_sums+= u*y
         @fastmath dμ_ij_z_sums+= u*z
         @fastmath dμ_ij_xy_sums+= u*x*y
         @fastmath dμ_ij_xz_sums+= u*x*z
         @fastmath dμ_ij_yz_sums+= u*y*z
         @fastmath dμ_ij_xyz_sums+= u*x*y*z
    end

    return dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_z_sums,dμ_ij_xy_sums,dμ_ij_xz_sums,dμ_ij_yz_sums,dμ_ij_xyz_sums
end

function calc_derSums_xyz!(n::Int64,s_n::Vector{T},
                    x::Float64,Y::Matrix{Float64},
                    z::Float64,
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    dμ_ij_x_sums = 0.0
    dμ_ij_y_sums = 0.0
    dμ_ij_z_sums = 0.0

    dμ_ij_xy_sums = 0.0
    dμ_ij_xz_sums = 0.0
    dμ_ij_yz_sums = 0.0

    dμ_ij_xyz_sums = 0.0

    K = length(δ)
    n1 = max(n,1)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath y = Y[n+1,k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
         @fastmath dμ_ij_x_sums+= u*x
         @fastmath dμ_ij_y_sums+= u*y
         @fastmath dμ_ij_z_sums+= u*z
         @fastmath dμ_ij_xy_sums+= u*x*y
         @fastmath dμ_ij_xz_sums+= u*x*z
         @fastmath dμ_ij_yz_sums+= u*y*z
         @fastmath dμ_ij_xyz_sums+= u*x*y*z
    end

    return dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_z_sums,dμ_ij_xy_sums,dμ_ij_xz_sums,dμ_ij_yz_sums,dμ_ij_xyz_sums
end

function calc_derSums_xyz!(n::Int64,s_n::Vector{T},
                    x::Float64,Y::Vector{Float64},
                    Z::Matrix{Float64},
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    dμ_ij_x_sums = 0.0
    dμ_ij_y_sums = 0.0
    dμ_ij_z_sums = 0.0

    dμ_ij_xy_sums = 0.0
    dμ_ij_xz_sums = 0.0
    dμ_ij_yz_sums = 0.0

    dμ_ij_xyz_sums = 0.0

    K = length(δ)
    n1 = max(n,1)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath y = Y[k]
        @inbounds @fastmath z = Z[n+1,k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
         @fastmath dμ_ij_x_sums+= u*x
         @fastmath dμ_ij_y_sums+= u*y
         @fastmath dμ_ij_z_sums+= u*z
         @fastmath dμ_ij_xy_sums+= u*x*y
         @fastmath dμ_ij_xz_sums+= u*x*z
         @fastmath dμ_ij_yz_sums+= u*y*z
         @fastmath dμ_ij_xyz_sums+= u*x*y*z
    end

    return dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_z_sums,dμ_ij_xy_sums,dμ_ij_xz_sums,dμ_ij_yz_sums,dμ_ij_xyz_sums
end

function calc_derSums_xyz!(n::Int64,s_n::Vector{T},
                    x::Float64,Y::Vector{Float64},
                    Z::Vector{Float64},
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    dμ_ij_x_sums = 0.0
    dμ_ij_y_sums = 0.0
    dμ_ij_z_sums = 0.0

    dμ_ij_xy_sums = 0.0
    dμ_ij_xz_sums = 0.0
    dμ_ij_yz_sums = 0.0

    dμ_ij_xyz_sums = 0.0

    K = length(δ)
    n1 = max(n,1)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath y = Y[k]
        @inbounds @fastmath z = Z[k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
         @fastmath dμ_ij_x_sums+= u*x
         @fastmath dμ_ij_y_sums+= u*y
         @fastmath dμ_ij_z_sums+= u*z
         @fastmath dμ_ij_xy_sums+= u*x*y
         @fastmath dμ_ij_xz_sums+= u*x*z
         @fastmath dμ_ij_yz_sums+= u*y*z
         @fastmath dμ_ij_xyz_sums+= u*x*y*z
    end

    return dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_z_sums,dμ_ij_xy_sums,dμ_ij_xz_sums,dμ_ij_yz_sums,dμ_ij_xyz_sums
end

function calc_derSums_xyz!(n::Int64,s_n::Vector{T},
                    x::Float64,Y::Vector{Float64},
                    z::Float64,
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    dμ_ij_x_sums = 0.0
    dμ_ij_y_sums = 0.0
    dμ_ij_z_sums = 0.0

    dμ_ij_xy_sums = 0.0
    dμ_ij_xz_sums = 0.0
    dμ_ij_yz_sums = 0.0

    dμ_ij_xyz_sums = 0.0

    K = length(δ)
    n1 = max(n,1)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath y = Y[k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
         @fastmath dμ_ij_x_sums+= u*x
         @fastmath dμ_ij_y_sums+= u*y
         @fastmath dμ_ij_z_sums+= u*z
         @fastmath dμ_ij_xy_sums+= u*x*y
         @fastmath dμ_ij_xz_sums+= u*x*z
         @fastmath dμ_ij_yz_sums+= u*y*z
         @fastmath dμ_ij_xyz_sums+= u*x*y*z
    end

    return dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_z_sums,dμ_ij_xy_sums,dμ_ij_xz_sums,dμ_ij_yz_sums,dμ_ij_xyz_sums
end

function calc_derSums_xyz!(n::Int64,s_n::Vector{T},
                    x::Float64,y::Float64,
                    Z::Matrix{Float64},
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    dμ_ij_x_sums = 0.0
    dμ_ij_y_sums = 0.0
    dμ_ij_z_sums = 0.0

    dμ_ij_xy_sums = 0.0
    dμ_ij_xz_sums = 0.0
    dμ_ij_yz_sums = 0.0

    dμ_ij_xyz_sums = 0.0

    K = length(δ)
    n1 = max(n,1)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath z = Z[n+1,k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
         @fastmath dμ_ij_x_sums+= u*x
         @fastmath dμ_ij_y_sums+= u*y
         @fastmath dμ_ij_z_sums+= u*z
         @fastmath dμ_ij_xy_sums+= u*x*y
         @fastmath dμ_ij_xz_sums+= u*x*z
         @fastmath dμ_ij_yz_sums+= u*y*z
         @fastmath dμ_ij_xyz_sums+= u*x*y*z
    end

    return dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_z_sums,dμ_ij_xy_sums,dμ_ij_xz_sums,dμ_ij_yz_sums,dμ_ij_xyz_sums
end

function calc_derSums_xyz!(n::Int64,s_n::Vector{T},
                    x::Float64,y::Float64,
                    Z::Vector{Float64},
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    dμ_ij_x_sums = 0.0
    dμ_ij_y_sums = 0.0
    dμ_ij_z_sums = 0.0

    dμ_ij_xy_sums = 0.0
    dμ_ij_xz_sums = 0.0
    dμ_ij_yz_sums = 0.0

    dμ_ij_xyz_sums = 0.0

    K = length(δ)
    n1 = max(n,1)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath z = Z[k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
         @fastmath dμ_ij_x_sums+= u*x
         @fastmath dμ_ij_y_sums+= u*y
         @fastmath dμ_ij_z_sums+= u*z
         @fastmath dμ_ij_xy_sums+= u*x*y
         @fastmath dμ_ij_xz_sums+= u*x*z
         @fastmath dμ_ij_yz_sums+= u*y*z
         @fastmath dμ_ij_xyz_sums+= u*x*y*z
    end

    return dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_z_sums,dμ_ij_xy_sums,dμ_ij_xz_sums,dμ_ij_yz_sums,dμ_ij_xyz_sums
end

function calc_derSums_xyz!(n::Int64,s_n::Vector{T},
                    x::Float64,y::Float64,
                    z::Float64,
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    dμ_ij_x_sums = 0.0
    dμ_ij_y_sums = 0.0
    dμ_ij_z_sums = 0.0

    dμ_ij_xy_sums = 0.0
    dμ_ij_xz_sums = 0.0
    dμ_ij_yz_sums = 0.0

    dμ_ij_xyz_sums = 0.0

    K = length(δ)
    n1 = max(n,1)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
         @fastmath dμ_ij_x_sums+= u*x
         @fastmath dμ_ij_y_sums+= u*y
         @fastmath dμ_ij_z_sums+= u*z
         @fastmath dμ_ij_xy_sums+= u*x*y
         @fastmath dμ_ij_xz_sums+= u*x*z
         @fastmath dμ_ij_yz_sums+= u*y*z
         @fastmath dμ_ij_xyz_sums+= u*x*y*z
    end

    return dμ_ij_x_sums, dμ_ij_y_sums, dμ_ij_z_sums,dμ_ij_xy_sums,dμ_ij_xz_sums,dμ_ij_yz_sums,dμ_ij_xyz_sums
end


##### HESSIAN FUNCTIONS ####

function calc_derSums_xy!(n::Int64,s_n::Vector{T},
                    X_mat::Matrix{Float64},Y::Matrix{Float64},
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

        Γ_x = 0.0
        Γ_y = 0.0
        Γ_xy = 0.0
    n1 = max(n,1)

    K = length(δ)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath x = X_mat[n+1,k]
        @inbounds @fastmath y = Y[n+1,k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
         @fastmath Γ_x+= u*x/μ_ij_sums_n
         @fastmath Γ_y+= u*y/μ_ij_sums_n
         @fastmath Γ_xy+= u*x*y/μ_ij_sums_n
    end

    return Γ_x, Γ_y, Γ_xy
end

function calc_derSums_xy!(n::Int64,s_n::Vector{T},
                    X_mat::Matrix{Float64},Y::Vector{Float64},
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    Γ_x = 0.0
    Γ_y = 0.0
    Γ_xy = 0.0
    n1 = max(n,1)

    K = length(δ)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath x = X_mat[n+1,k]
        @inbounds @fastmath y = Y[k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
        @fastmath Γ_x+= u*x/μ_ij_sums_n
        @fastmath Γ_y+= u*y/μ_ij_sums_n
        @fastmath Γ_xy+= u*x*y/μ_ij_sums_n
    end

    return Γ_x, Γ_y, Γ_xy
end

function calc_derSums_xy!(n::Int64,s_n::Vector{T},
                    X_mat::Matrix{Float64},y::Float64,
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    Γ_x = 0.0
    Γ_y = 0.0
    Γ_xy = 0.0
    n1 = max(n,1)

    K = length(δ)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath x = X_mat[n+1,k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
        @fastmath Γ_x+= u*x/μ_ij_sums_n
        @fastmath Γ_y+= u*y/μ_ij_sums_n
        @fastmath Γ_xy+= u*x*y/μ_ij_sums_n
    end

    return Γ_x, Γ_y, Γ_xy
end

function calc_derSums_xy!(n::Int64,s_n::Vector{T},
                    X_mat::Vector{Float64},Y::Matrix{Float64},
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    Γ_x = 0.0
    Γ_y = 0.0
    Γ_xy = 0.0
    n1 = max(n,1)

    K = length(δ)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath x = X_mat[k]
        @inbounds @fastmath y = Y[n+1,k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
        @fastmath Γ_x+= u*x/μ_ij_sums_n
        @fastmath Γ_y+= u*y/μ_ij_sums_n
        @fastmath Γ_xy+= u*x*y/μ_ij_sums_n
    end

    return Γ_x, Γ_y, Γ_xy
end

function calc_derSums_xy!(n::Int64,s_n::Vector{T},
                    X_mat::Vector{Float64},Y::Vector{Float64},
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    Γ_x = 0.0
    Γ_y = 0.0
    Γ_xy = 0.0
    n1 = max(n,1)

    K = length(δ)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath x = X_mat[k]
        @inbounds @fastmath y = Y[k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
        @fastmath Γ_x+= u*x/μ_ij_sums_n
        @fastmath Γ_y+= u*y/μ_ij_sums_n
        @fastmath Γ_xy+= u*x*y/μ_ij_sums_n
    end

    return Γ_x, Γ_y, Γ_xy
end

function calc_derSums_xy!(n::Int64,s_n::Vector{T},
                    X_mat::Vector{Float64},y::Float64,
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    Γ_x = 0.0
    Γ_y = 0.0
    Γ_xy = 0.0
    n1 = max(n,1)

    K = length(δ)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath x = X_mat[k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
        @fastmath Γ_x+= u*x/μ_ij_sums_n
        @fastmath Γ_y+= u*y/μ_ij_sums_n
        @fastmath Γ_xy+= u*x*y/μ_ij_sums_n
    end

    return Γ_x, Γ_y, Γ_xy
end

function calc_derSums_xy!(n::Int64,s_n::Vector{T},
                    x::Float64,Y::Matrix{Float64},
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    Γ_x = 0.0
    Γ_y = 0.0
    Γ_xy = 0.0
    n1 = max(n,1)

    K = length(δ)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath y = Y[n+1,k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
        @fastmath Γ_x+= u*x/μ_ij_sums_n
        @fastmath Γ_y+= u*y/μ_ij_sums_n
        @fastmath Γ_xy+= u*x*y/μ_ij_sums_n
    end

    return Γ_x, Γ_y, Γ_xy
end

function calc_derSums_xy!(n::Int64,s_n::Vector{T},
                    x::Float64,Y::Vector{Float64},
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    Γ_x = 0.0
    Γ_y = 0.0
    Γ_xy = 0.0
    n1 = max(n,1)

    K = length(δ)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath y = Y[k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
        @fastmath Γ_x+= u*x/μ_ij_sums_n
        @fastmath Γ_y+= u*y/μ_ij_sums_n
        @fastmath Γ_xy+= u*x*y/μ_ij_sums_n
    end

    return Γ_x, Γ_y, Γ_xy
end

function calc_derSums_xy!(n::Int64,s_n::Vector{T},
                    x::Float64,y::Float64,
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    Γ_x = 0.0
    Γ_y = 0.0
    Γ_xy = 0.0
    n1 = max(n,1)

    K = length(δ)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]

        @inbounds @fastmath s_n[k] = u/μ_ij_sums_n
        @fastmath Γ_x+= u*x/μ_ij_sums_n
        @fastmath Γ_y+= u*y/μ_ij_sums_n
        @fastmath Γ_xy+= u*x*y/μ_ij_sums_n
    end

    return Γ_x, Γ_y, Γ_xy
end

function calc_derSums_x!(n::Int64,s_n::Vector{T},
                    X_mat::Matrix{Float64},
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    Γ_x = 0.0
    n1 = max(n,1)
    K = length(δ)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath x = X_mat[n+1,k]

        @fastmath s = u/μ_ij_sums_n
        @inbounds s_n[k] = s
        @fastmath Γ_x+= s*x
    end

    return Γ_x
end

function calc_derSums_x!(n::Int64,s_n::Vector{T},
                    X_mat::Vector{Float64},
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    Γ_x = 0.0
    n1 = max(n,1)
    K = length(δ)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath x = X_mat[k]

        @fastmath s = u/μ_ij_sums_n
        @inbounds s_n[k] = s
        @fastmath Γ_x+= s*x
    end

    return Γ_x
end

function calc_derSums_x!(n::Int64,s_n::Vector{T},
                    X_mat::Float64,
                    μ_ij::Matrix{T},δ::Vector{T},
                    μ_ij_sums_n::T) where T

    Γ_x = 0.0
    n1 = max(n,1)
    K = length(δ)
    for k in 1:K
        @inbounds @fastmath u = μ_ij[n1,k]*δ[k]
        @inbounds @fastmath x = X_mat
        @fastmath s = u/μ_ij_sums_n
        @inbounds s_n[k] = s
        @fastmath Γ_x+= s*x
    end

    return Γ_x
end

#### PRODUCT TERMS ####
function calc_prodTerms_xyz!(n::Int64,N::Float64,dS_xyz::Vector{Float64},
                        X_mat::Matrix{Float64},Y::Matrix{Float64},
                        Z::Matrix{Float64},s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_z::Float64,
                        Γ_xy::Float64,Γ_xz::Float64,Γ_yz::Float64,
                        Γ_xyz::Float64)

    K = length(dS_xyz)
    t_last = Γ_x*Γ_yz + Γ_y*Γ_xz + Γ_z*Γ_xy - Γ_xyz - 2*Γ_x*Γ_y*Γ_z
    t_1 = (Γ_x*Γ_y - Γ_xy)
    t_2 = (Γ_x*Γ_z - Γ_xz)
    t_3 = (Γ_y*Γ_z - Γ_yz)
    for k in 1:K
        @inbounds @fastmath dS_xyz[k]+= N*s_n[k]*(
        (X_mat[n+1,k] - Γ_x)*(Y[n+1,k] - Γ_y)*(Z[n+1,k] - Γ_z) +
        t_1*(Z[n+1,k] - Γ_z) +
        t_2*(Y[n+1,k] - Γ_y) +
        t_3*(X_mat[n+1,k] - Γ_x) + t_last )
    end
end

function calc_prodTerms_xyz!(n::Int64,N::Float64,dS_xyz::Vector{Float64},
                        X_mat::Matrix{Float64},Y::Matrix{Float64},
                        Z::Vector{Float64},s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_z::Float64,
                        Γ_xy::Float64,Γ_xz::Float64,Γ_yz::Float64,
                        Γ_xyz::Float64)

    K = length(dS_xyz)
    t_last = Γ_x*Γ_yz + Γ_y*Γ_xz + Γ_z*Γ_xy - Γ_xyz - 2*Γ_x*Γ_y*Γ_z
    t_1 = (Γ_x*Γ_y - Γ_xy)
    t_2 = (Γ_x*Γ_z - Γ_xz)
    t_3 = (Γ_y*Γ_z - Γ_yz)
    for k in 1:K
        @inbounds @fastmath dS_xyz[k]+= N*s_n[k]*(
        (X_mat[n+1,k] - Γ_x)*(Y[n+1,k] - Γ_y)*(Z[k] - Γ_z) +
        t_1*(Z[k] - Γ_z) +
        t_2*(Y[n+1,k] - Γ_y) +
        t_3*(X_mat[n+1,k] - Γ_x) + t_last )
    end
end

function calc_prodTerms_xyz!(n::Int64,N::Float64,dS_xyz::Vector{Float64},
                        X_mat::Matrix{Float64},Y::Matrix{Float64},
                        Z::Float64,s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_z::Float64,
                        Γ_xy::Float64,Γ_xz::Float64,Γ_yz::Float64,
                        Γ_xyz::Float64)

    K = length(dS_xyz)
    t_last = Γ_x*Γ_yz + Γ_y*Γ_xz + Γ_z*Γ_xy - Γ_xyz - 2*Γ_x*Γ_y*Γ_z
    t_1 = (Γ_x*Γ_y - Γ_xy)
    t_2 = (Γ_x*Γ_z - Γ_xz)
    t_3 = (Γ_y*Γ_z - Γ_yz)
    for k in 1:K
        @inbounds @fastmath dS_xyz[k]+= N*s_n[k]*(
        (X_mat[n+1,k] - Γ_x)*(Y[n+1,k] - Γ_y)*(Z - Γ_z) +
        t_1*(Z - Γ_z) +
        t_2*(Y[n+1,k] - Γ_y) +
        t_3*(X_mat[n+1,k] - Γ_x) + t_last )
    end
end

function calc_prodTerms_xyz!(n::Int64,N::Float64,dS_xyz::Vector{Float64},
                        X_mat::Matrix{Float64},Y::Vector{Float64},
                        Z::Matrix{Float64},s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_z::Float64,
                        Γ_xy::Float64,Γ_xz::Float64,Γ_yz::Float64,
                        Γ_xyz::Float64)

    K = length(dS_xyz)
    t_last = Γ_x*Γ_yz + Γ_y*Γ_xz + Γ_z*Γ_xy - Γ_xyz - 2*Γ_x*Γ_y*Γ_z
    t_1 = (Γ_x*Γ_y - Γ_xy)
    t_2 = (Γ_x*Γ_z - Γ_xz)
    t_3 = (Γ_y*Γ_z - Γ_yz)
    for k in 1:K
        @inbounds @fastmath dS_xyz[k]+= N*s_n[k]*(
        (X_mat[n+1,k] - Γ_x)*(Y[k] - Γ_y)*(Z[n+1,k] - Γ_z) +
        t_1*(Z[n+1,k] - Γ_z) +
        t_2*(Y[k] - Γ_y) +
        t_3*(X_mat[n+1,k] - Γ_x) + t_last )
    end
end

function calc_prodTerms_xyz!(n::Int64,N::Float64,dS_xyz::Vector{Float64},
                        X_mat::Matrix{Float64},Y::Vector{Float64},
                        Z::Vector{Float64},s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_z::Float64,
                        Γ_xy::Float64,Γ_xz::Float64,Γ_yz::Float64,
                        Γ_xyz::Float64)

    K = length(dS_xyz)
    t_last = Γ_x*Γ_yz + Γ_y*Γ_xz + Γ_z*Γ_xy - Γ_xyz - 2*Γ_x*Γ_y*Γ_z
    t_1 = (Γ_x*Γ_y - Γ_xy)
    t_2 = (Γ_x*Γ_z - Γ_xz)
    t_3 = (Γ_y*Γ_z - Γ_yz)
    for k in 1:K
        @inbounds @fastmath dS_xyz[k]+= N*s_n[k]*(
        (X_mat[n+1,k] - Γ_x)*(Y[k] - Γ_y)*(Z[k] - Γ_z) +
        t_1*(Z[k] - Γ_z) +
        t_2*(Y[k] - Γ_y) +
        t_3*(X_mat[n+1,k] - Γ_x) + t_last )
    end
end

function calc_prodTerms_xyz!(n::Int64,N::Float64,dS_xyz::Vector{Float64},
                        X_mat::Matrix{Float64},Y::Vector{Float64},
                        Z::Float64,s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_z::Float64,
                        Γ_xy::Float64,Γ_xz::Float64,Γ_yz::Float64,
                        Γ_xyz::Float64)

    K = length(dS_xyz)
    t_last = Γ_x*Γ_yz + Γ_y*Γ_xz + Γ_z*Γ_xy - Γ_xyz - 2*Γ_x*Γ_y*Γ_z
    t_1 = (Γ_x*Γ_y - Γ_xy)
    t_2 = (Γ_x*Γ_z - Γ_xz)
    t_3 = (Γ_y*Γ_z - Γ_yz)
    for k in 1:K
        @inbounds @fastmath dS_xyz[k]+= N*s_n[k]*(
        (X_mat[n+1,k] - Γ_x)*(Y[k] - Γ_y)*(Z - Γ_z) +
        t_1*(Z - Γ_z) +
        t_2*(Y[k] - Γ_y) +
        t_3*(X_mat[n+1,k] - Γ_x) + t_last )
    end
end

function calc_prodTerms_xyz!(n::Int64,N::Float64,dS_xyz::Vector{Float64},
                        X_mat::Matrix{Float64},Y::Float64,
                        Z::Matrix{Float64},s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_z::Float64,
                        Γ_xy::Float64,Γ_xz::Float64,Γ_yz::Float64,
                        Γ_xyz::Float64)

    K = length(dS_xyz)
    t_last = Γ_x*Γ_yz + Γ_y*Γ_xz + Γ_z*Γ_xy - Γ_xyz - 2*Γ_x*Γ_y*Γ_z
    t_1 = (Γ_x*Γ_y - Γ_xy)
    t_2 = (Γ_x*Γ_z - Γ_xz)
    t_3 = (Γ_y*Γ_z - Γ_yz)
    for k in 1:K
        @inbounds @fastmath dS_xyz[k]+= N*s_n[k]*(
        (X_mat[n+1,k] - Γ_x)*(Y - Γ_y)*(Z[n+1,k] - Γ_z) +
        t_1*(Z[n+1,k] - Γ_z) +
        t_2*(Y - Γ_y) +
        t_3*(X_mat[n+1,k] - Γ_x) + t_last )
    end
end

function calc_prodTerms_xyz!(n::Int64,N::Float64,dS_xyz::Vector{Float64},
                        X_mat::Matrix{Float64},Y::Float64,
                        Z::Vector{Float64},s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_z::Float64,
                        Γ_xy::Float64,Γ_xz::Float64,Γ_yz::Float64,
                        Γ_xyz::Float64)

    K = length(dS_xyz)
    t_last = Γ_x*Γ_yz + Γ_y*Γ_xz + Γ_z*Γ_xy - Γ_xyz - 2*Γ_x*Γ_y*Γ_z
    t_1 = (Γ_x*Γ_y - Γ_xy)
    t_2 = (Γ_x*Γ_z - Γ_xz)
    t_3 = (Γ_y*Γ_z - Γ_yz)
    for k in 1:K
        @inbounds @fastmath dS_xyz[k]+= N*s_n[k]*(
        (X_mat[n+1,k] - Γ_x)*(Y - Γ_y)*(Z[k] - Γ_z) +
        t_1*(Z[k] - Γ_z) +
        t_2*(Y - Γ_y) +
        t_3*(X_mat[n+1,k] - Γ_x) + t_last )
    end
end

function calc_prodTerms_xyz!(n::Int64,N::Float64,dS_xyz::Vector{Float64},
                        X_mat::Matrix{Float64},Y::Float64,
                        Z::Float64,s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_z::Float64,
                        Γ_xy::Float64,Γ_xz::Float64,Γ_yz::Float64,
                        Γ_xyz::Float64)

    K = length(dS_xyz)
    t_last = Γ_x*Γ_yz + Γ_y*Γ_xz + Γ_z*Γ_xy - Γ_xyz - 2*Γ_x*Γ_y*Γ_z
    t_1 = (Γ_x*Γ_y - Γ_xy)
    t_2 = (Γ_x*Γ_z - Γ_xz)
    t_3 = (Γ_y*Γ_z - Γ_yz)
    for k in 1:K
        @inbounds @fastmath dS_xyz[k]+= N*s_n[k]*(
        (X_mat[n+1,k] - Γ_x)*(Y - Γ_y)*(Z - Γ_z) +
        t_1*(Z - Γ_z) +
        t_2*(Y - Γ_y) +
        t_3*(X_mat[n+1,k] - Γ_x) + t_last )
    end
end



function calc_prodTerms_xyz!(n::Int64,N::Float64,dS_xyz::Vector{Float64},
                        X_mat::Vector{Float64},Y::Matrix{Float64},
                        Z::Matrix{Float64},s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_z::Float64,
                        Γ_xy::Float64,Γ_xz::Float64,Γ_yz::Float64,
                        Γ_xyz::Float64)

    K = length(dS_xyz)
    t_last = Γ_x*Γ_yz + Γ_y*Γ_xz + Γ_z*Γ_xy - Γ_xyz - 2*Γ_x*Γ_y*Γ_z
    t_1 = (Γ_x*Γ_y - Γ_xy)
    t_2 = (Γ_x*Γ_z - Γ_xz)
    t_3 = (Γ_y*Γ_z - Γ_yz)
    for k in 1:K
        @inbounds @fastmath dS_xyz[k]+= N*s_n[k]*(
        (X_mat[k] - Γ_x)*(Y[n+1,k] - Γ_y)*(Z[n+1,k] - Γ_z) +
        t_1*(Z[n+1,k] - Γ_z) +
        t_2*(Y[n+1,k] - Γ_y) +
        t_3*(X_mat[k] - Γ_x) + t_last )
    end
end

function calc_prodTerms_xyz!(n::Int64,N::Float64,dS_xyz::Vector{Float64},
                        X_mat::Vector{Float64},Y::Matrix{Float64},
                        Z::Vector{Float64},s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_z::Float64,
                        Γ_xy::Float64,Γ_xz::Float64,Γ_yz::Float64,
                        Γ_xyz::Float64)

    K = length(dS_xyz)
    t_last = Γ_x*Γ_yz + Γ_y*Γ_xz + Γ_z*Γ_xy - Γ_xyz - 2*Γ_x*Γ_y*Γ_z
    t_1 = (Γ_x*Γ_y - Γ_xy)
    t_2 = (Γ_x*Γ_z - Γ_xz)
    t_3 = (Γ_y*Γ_z - Γ_yz)
    for k in 1:K
        @inbounds @fastmath dS_xyz[k]+= N*s_n[k]*(
        (X_mat[k] - Γ_x)*(Y[n+1,k] - Γ_y)*(Z[k] - Γ_z) +
        t_1*(Z[k] - Γ_z) +
        t_2*(Y[n+1,k] - Γ_y) +
        t_3*(X_mat[k] - Γ_x) + t_last )
    end
end

function calc_prodTerms_xyz!(n::Int64,N::Float64,dS_xyz::Vector{Float64},
                        X_mat::Vector{Float64},Y::Matrix{Float64},
                        Z::Float64,s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_z::Float64,
                        Γ_xy::Float64,Γ_xz::Float64,Γ_yz::Float64,
                        Γ_xyz::Float64)

    K = length(dS_xyz)
    t_last = Γ_x*Γ_yz + Γ_y*Γ_xz + Γ_z*Γ_xy - Γ_xyz - 2*Γ_x*Γ_y*Γ_z
    t_1 = (Γ_x*Γ_y - Γ_xy)
    t_2 = (Γ_x*Γ_z - Γ_xz)
    t_3 = (Γ_y*Γ_z - Γ_yz)
    for k in 1:K
        @inbounds @fastmath dS_xyz[k]+= N*s_n[k]*(
        (X_mat[k] - Γ_x)*(Y[n+1,k] - Γ_y)*(Z - Γ_z) +
        t_1*(Z - Γ_z) +
        t_2*(Y[n+1,k] - Γ_y) +
        t_3*(X_mat[k] - Γ_x) + t_last )
    end
end

function calc_prodTerms_xyz!(n::Int64,N::Float64,dS_xyz::Vector{Float64},
                        X_mat::Vector{Float64},Y::Vector{Float64},
                        Z::Matrix{Float64},s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_z::Float64,
                        Γ_xy::Float64,Γ_xz::Float64,Γ_yz::Float64,
                        Γ_xyz::Float64)

    K = length(dS_xyz)
    t_last = Γ_x*Γ_yz + Γ_y*Γ_xz + Γ_z*Γ_xy - Γ_xyz - 2*Γ_x*Γ_y*Γ_z
    t_1 = (Γ_x*Γ_y - Γ_xy)
    t_2 = (Γ_x*Γ_z - Γ_xz)
    t_3 = (Γ_y*Γ_z - Γ_yz)
    for k in 1:K
        @inbounds @fastmath dS_xyz[k]+= N*s_n[k]*(
        (X_mat[k] - Γ_x)*(Y[k] - Γ_y)*(Z[n+1,k] - Γ_z) +
        t_1*(Z[n+1,k] - Γ_z) +
        t_2*(Y[k] - Γ_y) +
        t_3*(X_mat[k] - Γ_x) + t_last )
    end
end

function calc_prodTerms_xyz!(n::Int64,N::Float64,dS_xyz::Vector{Float64},
                        X_mat::Vector{Float64},Y::Vector{Float64},
                        Z::Vector{Float64},s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_z::Float64,
                        Γ_xy::Float64,Γ_xz::Float64,Γ_yz::Float64,
                        Γ_xyz::Float64)

    K = length(dS_xyz)
    t_last = Γ_x*Γ_yz + Γ_y*Γ_xz + Γ_z*Γ_xy - Γ_xyz - 2*Γ_x*Γ_y*Γ_z
    t_1 = (Γ_x*Γ_y - Γ_xy)
    t_2 = (Γ_x*Γ_z - Γ_xz)
    t_3 = (Γ_y*Γ_z - Γ_yz)
    for k in 1:K
        @inbounds @fastmath dS_xyz[k]+= N*s_n[k]*(
        (X_mat[k] - Γ_x)*(Y[k] - Γ_y)*(Z[k] - Γ_z) +
        t_1*(Z[k] - Γ_z) +
        t_2*(Y[k] - Γ_y) +
        t_3*(X_mat[k] - Γ_x) + t_last )
    end
end

function calc_prodTerms_xyz!(n::Int64,N::Float64,dS_xyz::Vector{Float64},
                        X_mat::Vector{Float64},Y::Vector{Float64},
                        Z::Float64,s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_z::Float64,
                        Γ_xy::Float64,Γ_xz::Float64,Γ_yz::Float64,
                        Γ_xyz::Float64)

    K = length(dS_xyz)
    t_last = Γ_x*Γ_yz + Γ_y*Γ_xz + Γ_z*Γ_xy - Γ_xyz - 2*Γ_x*Γ_y*Γ_z
    t_1 = (Γ_x*Γ_y - Γ_xy)
    t_2 = (Γ_x*Γ_z - Γ_xz)
    t_3 = (Γ_y*Γ_z - Γ_yz)
    for k in 1:K
        @inbounds @fastmath dS_xyz[k]+= N*s_n[k]*(
        (X_mat[k] - Γ_x)*(Y[k] - Γ_y)*(Z - Γ_z) +
        t_1*(Z - Γ_z) +
        t_2*(Y[k] - Γ_y) +
        t_3*(X_mat[k] - Γ_x) + t_last )
    end
end

function calc_prodTerms_xyz!(n::Int64,N::Float64,dS_xyz::Vector{Float64},
                        X_mat::Vector{Float64},Y::Float64,
                        Z::Matrix{Float64},s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_z::Float64,
                        Γ_xy::Float64,Γ_xz::Float64,Γ_yz::Float64,
                        Γ_xyz::Float64)

    K = length(dS_xyz)
    t_last = Γ_x*Γ_yz + Γ_y*Γ_xz + Γ_z*Γ_xy - Γ_xyz - 2*Γ_x*Γ_y*Γ_z
    t_1 = (Γ_x*Γ_y - Γ_xy)
    t_2 = (Γ_x*Γ_z - Γ_xz)
    t_3 = (Γ_y*Γ_z - Γ_yz)
    for k in 1:K
        @inbounds @fastmath dS_xyz[k]+= N*s_n[k]*(
        (X_mat[k] - Γ_x)*(Y - Γ_y)*(Z[n+1,k] - Γ_z) +
        t_1*(Z[n+1,k] - Γ_z) +
        t_2*(Y - Γ_y) +
        t_3*(X_mat[k] - Γ_x) + t_last )
    end
end

function calc_prodTerms_xyz!(n::Int64,N::Float64,dS_xyz::Vector{Float64},
                        X_mat::Vector{Float64},Y::Float64,
                        Z::Vector{Float64},s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_z::Float64,
                        Γ_xy::Float64,Γ_xz::Float64,Γ_yz::Float64,
                        Γ_xyz::Float64)

    K = length(dS_xyz)
    t_last = Γ_x*Γ_yz + Γ_y*Γ_xz + Γ_z*Γ_xy - Γ_xyz - 2*Γ_x*Γ_y*Γ_z
    t_1 = (Γ_x*Γ_y - Γ_xy)
    t_2 = (Γ_x*Γ_z - Γ_xz)
    t_3 = (Γ_y*Γ_z - Γ_yz)
    for k in 1:K
        @inbounds @fastmath dS_xyz[k]+= N*s_n[k]*(
        (X_mat[k] - Γ_x)*(Y - Γ_y)*(Z[k] - Γ_z) +
        t_1*(Z[k] - Γ_z) +
        t_2*(Y - Γ_y) +
        t_3*(X_mat[k] - Γ_x) + t_last )
    end
end

function calc_prodTerms_xyz!(n::Int64,N::Float64,dS_xyz::Vector{Float64},
                        X_mat::Vector{Float64},Y::Float64,
                        Z::Float64,s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_z::Float64,
                        Γ_xy::Float64,Γ_xz::Float64,Γ_yz::Float64,
                        Γ_xyz::Float64)

    K = length(dS_xyz)
    t_last = Γ_x*Γ_yz + Γ_y*Γ_xz + Γ_z*Γ_xy - Γ_xyz - 2*Γ_x*Γ_y*Γ_z
    t_1 = (Γ_x*Γ_y - Γ_xy)
    t_2 = (Γ_x*Γ_z - Γ_xz)
    t_3 = (Γ_y*Γ_z - Γ_yz)
    for k in 1:K
        @inbounds @fastmath dS_xyz[k]+= N*s_n[k]*(
        (X_mat[k] - Γ_x)*(Y - Γ_y)*(Z - Γ_z) +
        t_1*(Z - Γ_z) +
        t_2*(Y - Γ_y) +
        t_3*(X_mat[k] - Γ_x) + t_last )
    end
end


function calc_prodTerms_xyz!(n::Int64,N::Float64,dS_xyz::Vector{Float64},
                        X_mat::Float64,Y::Matrix{Float64},
                        Z::Matrix{Float64},s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_z::Float64,
                        Γ_xy::Float64,Γ_xz::Float64,Γ_yz::Float64,
                        Γ_xyz::Float64)

    K = length(dS_xyz)
    t_last = Γ_x*Γ_yz + Γ_y*Γ_xz + Γ_z*Γ_xy - Γ_xyz - 2*Γ_x*Γ_y*Γ_z
    t_1 = (Γ_x*Γ_y - Γ_xy)
    t_2 = (Γ_x*Γ_z - Γ_xz)
    t_3 = (Γ_y*Γ_z - Γ_yz)
    for k in 1:K
        @inbounds @fastmath dS_xyz[k]+= N*s_n[k]*(
        (X_mat - Γ_x)*(Y[n+1,k] - Γ_y)*(Z[n+1,k] - Γ_z) +
        t_1*(Z[n+1,k] - Γ_z) +
        t_2*(Y[n+1,k] - Γ_y) +
        t_3*(X_mat - Γ_x) + t_last )
    end
end

function calc_prodTerms_xyz!(n::Int64,N::Float64,dS_xyz::Vector{Float64},
                        X_mat::Float64,Y::Matrix{Float64},
                        Z::Vector{Float64},s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_z::Float64,
                        Γ_xy::Float64,Γ_xz::Float64,Γ_yz::Float64,
                        Γ_xyz::Float64)

    K = length(dS_xyz)
    t_last = Γ_x*Γ_yz + Γ_y*Γ_xz + Γ_z*Γ_xy - Γ_xyz - 2*Γ_x*Γ_y*Γ_z
    t_1 = (Γ_x*Γ_y - Γ_xy)
    t_2 = (Γ_x*Γ_z - Γ_xz)
    t_3 = (Γ_y*Γ_z - Γ_yz)
    for k in 1:K
        @inbounds @fastmath dS_xyz[k]+= N*s_n[k]*(
        (X_mat - Γ_x)*(Y[n+1,k] - Γ_y)*(Z[k] - Γ_z) +
        t_1*(Z[k] - Γ_z) +
        t_2*(Y[n+1,k] - Γ_y) +
        t_3*(X_mat - Γ_x) + t_last )
    end
end

function calc_prodTerms_xyz!(n::Int64,N::Float64,dS_xyz::Vector{Float64},
                        X_mat::Float64,Y::Matrix{Float64},
                        Z::Float64,s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_z::Float64,
                        Γ_xy::Float64,Γ_xz::Float64,Γ_yz::Float64,
                        Γ_xyz::Float64)

    K = length(dS_xyz)
    t_last = Γ_x*Γ_yz + Γ_y*Γ_xz + Γ_z*Γ_xy - Γ_xyz - 2*Γ_x*Γ_y*Γ_z
    t_1 = (Γ_x*Γ_y - Γ_xy)
    t_2 = (Γ_x*Γ_z - Γ_xz)
    t_3 = (Γ_y*Γ_z - Γ_yz)
    for k in 1:K
        @inbounds @fastmath dS_xyz[k]+= N*s_n[k]*(
        (X_mat - Γ_x)*(Y[n+1,k] - Γ_y)*(Z - Γ_z) +
        t_1*(Z - Γ_z) +
        t_2*(Y[n+1,k] - Γ_y) +
        t_3*(X_mat - Γ_x) + t_last )
    end
end

function calc_prodTerms_xyz!(n::Int64,N::Float64,dS_xyz::Vector{Float64},
                        X_mat::Float64,Y::Vector{Float64},
                        Z::Matrix{Float64},s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_z::Float64,
                        Γ_xy::Float64,Γ_xz::Float64,Γ_yz::Float64,
                        Γ_xyz::Float64)

    K = length(dS_xyz)
    t_last = Γ_x*Γ_yz + Γ_y*Γ_xz + Γ_z*Γ_xy - Γ_xyz - 2*Γ_x*Γ_y*Γ_z
    t_1 = (Γ_x*Γ_y - Γ_xy)
    t_2 = (Γ_x*Γ_z - Γ_xz)
    t_3 = (Γ_y*Γ_z - Γ_yz)
    for k in 1:K
        @inbounds @fastmath dS_xyz[k]+= N*s_n[k]*(
        (X_mat - Γ_x)*(Y[k] - Γ_y)*(Z[n+1,k] - Γ_z) +
        t_1*(Z[n+1,k] - Γ_z) +
        t_2*(Y[k] - Γ_y) +
        t_3*(X_mat - Γ_x) + t_last )
    end
end

function calc_prodTerms_xyz!(n::Int64,N::Float64,dS_xyz::Vector{Float64},
                        X_mat::Float64,Y::Vector{Float64},
                        Z::Vector{Float64},s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_z::Float64,
                        Γ_xy::Float64,Γ_xz::Float64,Γ_yz::Float64,
                        Γ_xyz::Float64)

    K = length(dS_xyz)
    t_last = Γ_x*Γ_yz + Γ_y*Γ_xz + Γ_z*Γ_xy - Γ_xyz - 2*Γ_x*Γ_y*Γ_z
    t_1 = (Γ_x*Γ_y - Γ_xy)
    t_2 = (Γ_x*Γ_z - Γ_xz)
    t_3 = (Γ_y*Γ_z - Γ_yz)
    for k in 1:K
        @inbounds @fastmath dS_xyz[k]+= N*s_n[k]*(
        (X_mat - Γ_x)*(Y[k] - Γ_y)*(Z[k] - Γ_z) +
        t_1*(Z[k] - Γ_z) +
        t_2*(Y[k] - Γ_y) +
        t_3*(X_mat - Γ_x) + t_last )
    end
end

function calc_prodTerms_xyz!(n::Int64,N::Float64,dS_xyz::Vector{Float64},
                        X_mat::Float64,Y::Vector{Float64},
                        Z::Float64,s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_z::Float64,
                        Γ_xy::Float64,Γ_xz::Float64,Γ_yz::Float64,
                        Γ_xyz::Float64)

    K = length(dS_xyz)
    t_last = Γ_x*Γ_yz + Γ_y*Γ_xz + Γ_z*Γ_xy - Γ_xyz - 2*Γ_x*Γ_y*Γ_z
    t_1 = (Γ_x*Γ_y - Γ_xy)
    t_2 = (Γ_x*Γ_z - Γ_xz)
    t_3 = (Γ_y*Γ_z - Γ_yz)
    for k in 1:K
        @inbounds @fastmath dS_xyz[k]+= N*s_n[k]*(
        (X_mat - Γ_x)*(Y[k] - Γ_y)*(Z - Γ_z) +
        t_1*(Z - Γ_z) +
        t_2*(Y[k] - Γ_y) +
        t_3*(X_mat - Γ_x) + t_last )
    end
end

function calc_prodTerms_xyz!(n::Int64,N::Float64,dS_xyz::Vector{Float64},
                        X_mat::Float64,Y::Float64,
                        Z::Matrix{Float64},s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_z::Float64,
                        Γ_xy::Float64,Γ_xz::Float64,Γ_yz::Float64,
                        Γ_xyz::Float64)

    K = length(dS_xyz)
    t_last = Γ_x*Γ_yz + Γ_y*Γ_xz + Γ_z*Γ_xy - Γ_xyz - 2*Γ_x*Γ_y*Γ_z
    t_1 = (Γ_x*Γ_y - Γ_xy)
    t_2 = (Γ_x*Γ_z - Γ_xz)
    t_3 = (Γ_y*Γ_z - Γ_yz)
    for k in 1:K
        @inbounds @fastmath dS_xyz[k]+= N*s_n[k]*(
        (X_mat - Γ_x)*(Y - Γ_y)*(Z[n+1,k] - Γ_z) +
        t_1*(Z[n+1,k] - Γ_z) +
        t_2*(Y - Γ_y) +
        t_3*(X_mat - Γ_x) + t_last )
    end
end

function calc_prodTerms_xyz!(n::Int64,N::Float64,dS_xyz::Vector{Float64},
                        X_mat::Float64,Y::Float64,
                        Z::Vector{Float64},s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_z::Float64,
                        Γ_xy::Float64,Γ_xz::Float64,Γ_yz::Float64,
                        Γ_xyz::Float64)

    K = length(dS_xyz)
    t_last = Γ_x*Γ_yz + Γ_y*Γ_xz + Γ_z*Γ_xy - Γ_xyz - 2*Γ_x*Γ_y*Γ_z
    t_1 = (Γ_x*Γ_y - Γ_xy)
    t_2 = (Γ_x*Γ_z - Γ_xz)
    t_3 = (Γ_y*Γ_z - Γ_yz)
    for k in 1:K
        @inbounds @fastmath dS_xyz[k]+= N*s_n[k]*(
        (X_mat - Γ_x)*(Y - Γ_y)*(Z[k] - Γ_z) +
        t_1*(Z[k] - Γ_z) +
        t_2*(Y - Γ_y) +
        t_3*(X_mat - Γ_x) + t_last )
    end
end

function calc_prodTerms_xyz!(n::Int64,N::Float64,dS_xyz::Vector{Float64},
                        X_mat::Float64,Y::Float64,
                        Z::Float64,s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_z::Float64,
                        Γ_xy::Float64,Γ_xz::Float64,Γ_yz::Float64,
                        Γ_xyz::Float64)

    K = length(dS_xyz)
    t_last = Γ_x*Γ_yz + Γ_y*Γ_xz + Γ_z*Γ_xy - Γ_xyz - 2*Γ_x*Γ_y*Γ_z
    t_1 = (Γ_x*Γ_y - Γ_xy)
    t_2 = (Γ_x*Γ_z - Γ_xz)
    t_3 = (Γ_y*Γ_z - Γ_yz)
    for k in 1:K
        @inbounds @fastmath dS_xyz[k]+= N*s_n[k]*(
        (X_mat - Γ_x)*(Y - Γ_y)*(Z - Γ_z) +
        t_1*(Z - Γ_z) +
        t_2*(Y - Γ_y) +
        t_3*(X_mat - Γ_x) + t_last )
    end
end

#### HESSIAN FUNCTIONS ####

function calc_prodTerms_xy!(n::Int64,N::Float64,dS_xy::Vector{Float64},
                        dR::Vector{Float64},risk::Matrix{Float64},
                        risk_age::Vector{Float64},
                        X_mat::Matrix{Float64},Y::Matrix{Float64},
                        s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_xy::Float64)

    K = length(dS_xy)
    n1 = max(n,1)
    t_last = Γ_x*Γ_y -Γ_xy
    for k in 1:K
        @inbounds @fastmath txy = s_n[k]*((X_mat[n+1,k] - Γ_x)*(Y[n+1,k] - Γ_y)+t_last)
        @inbounds @fastmath dS_xy[k]+= N*txy
        @inbounds @fastmath dR[k]+= N*txy*(risk[n1,k]+risk_age[k])
    end
end

function calc_prodTerms_xy!(n::Int64,N::Float64,dS_xy::Vector{Float64},
                        dR::Vector{Float64},risk::Matrix{Float64},
                        risk_age::Vector{Float64},
                        X_mat::Matrix{Float64},Y::Vector{Float64},
                        s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_xy::Float64)

    K = length(dS_xy)
    n1 = max(n,1)
    t_last = Γ_x*Γ_y -Γ_xy
    for k in 1:K
        @inbounds @fastmath txy = s_n[k]*((X_mat[n+1,k] - Γ_x)*(Y[k] - Γ_y)+t_last)
        @inbounds @fastmath dS_xy[k]+= N*txy
        @inbounds @fastmath dR[k]+= N*txy*(risk[n1,k]+risk_age[k])
    end
end

function calc_prodTerms_xy!(n::Int64,N::Float64,dS_xy::Vector{Float64},
                        dR::Vector{Float64},risk::Matrix{Float64},
                        risk_age::Vector{Float64},
                        X_mat::Matrix{Float64},Y::Float64,
                        s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_xy::Float64)

    K = length(dS_xy)
    n1 = max(n,1)
    t_last = Γ_x*Γ_y -Γ_xy
    for k in 1:K
        @inbounds @fastmath txy = s_n[k]*((X_mat[n+1,k] - Γ_x)*(Y - Γ_y)+t_last)
        @inbounds @fastmath dS_xy[k]+= N*txy
        @inbounds @fastmath dR[k]+= N*txy*(risk[n1,k]+risk_age[k])
    end
end


function calc_prodTerms_xy!(n::Int64,N::Float64,dS_xy::Vector{Float64},
                        dR::Vector{Float64},risk::Matrix{Float64},
                        risk_age::Vector{Float64},
                        X_mat::Vector{Float64},Y::Matrix{Float64},
                        s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_xy::Float64)

    K = length(dS_xy)
    n1 = max(n,1)
    t_last = Γ_x*Γ_y -Γ_xy
    for k in 1:K
        @inbounds @fastmath txy = s_n[k]*((X_mat[k] - Γ_x)*(Y[n+1,k] - Γ_y)+t_last)
        @inbounds @fastmath dS_xy[k]+= N*txy
        @inbounds @fastmath dR[k]+= N*txy*(risk[n1,k]+risk_age[k])
    end
end

function calc_prodTerms_xy!(n::Int64,N::Float64,dS_xy::Vector{Float64},
                        dR::Vector{Float64},risk::Matrix{Float64},
                        risk_age::Vector{Float64},
                        X_mat::Vector{Float64},Y::Vector{Float64},
                        s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_xy::Float64)

    K = length(dS_xy)
    n1 = max(n,1)
    t_last = Γ_x*Γ_y -Γ_xy
    for k in 1:K
        @inbounds @fastmath txy = s_n[k]*((X_mat[k] - Γ_x)*(Y[k] - Γ_y)+t_last)
        @inbounds @fastmath dS_xy[k]+= N*txy
        @inbounds @fastmath dR[k]+= N*txy*(risk[n1,k]+risk_age[k])
    end
end

function calc_prodTerms_xy!(n::Int64,N::Float64,dS_xy::Vector{Float64},
                        dR::Vector{Float64},risk::Matrix{Float64},
                        risk_age::Vector{Float64},
                        X_mat::Vector{Float64},Y::Float64,
                        s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_xy::Float64)

    K = length(dS_xy)
    n1 = max(n,1)
    t_last = Γ_x*Γ_y -Γ_xy
    for k in 1:K
        @inbounds @fastmath txy = s_n[k]*((X_mat[k] - Γ_x)*(Y - Γ_y)+t_last)
        @inbounds @fastmath dS_xy[k]+= N*txy
        @inbounds @fastmath dR[k]+= N*txy*(risk[n1,k]+risk_age[k])
    end
end


function calc_prodTerms_xy!(n::Int64,N::Float64,dS_xy::Vector{Float64},
                        dR::Vector{Float64},risk::Matrix{Float64},
                        risk_age::Vector{Float64},
                        X_mat::Float64,Y::Matrix{Float64},
                        s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_xy::Float64)

    K = length(dS_xy)
    n1 = max(n,1)
    t_last = Γ_x*Γ_y -Γ_xy
    for k in 1:K
        @inbounds @fastmath txy = s_n[k]*((X_mat - Γ_x)*(Y[n+1,k] - Γ_y)+t_last)
        @inbounds @fastmath dS_xy[k]+= N*txy
        @inbounds @fastmath dR[k]+= N*txy*(risk[n1,k]+risk_age[k])
    end
end

function calc_prodTerms_xy!(n::Int64,N::Float64,dS_xy::Vector{Float64},
                        dR::Vector{Float64},risk::Matrix{Float64},
                        risk_age::Vector{Float64},
                        X_mat::Float64,Y::Vector{Float64},
                        s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_xy::Float64)

    K = length(dS_xy)
    n1 = max(n,1)
    t_last = Γ_x*Γ_y -Γ_xy
    for k in 1:K
        @inbounds @fastmath txy = s_n[k]*((X_mat - Γ_x)*(Y[k] - Γ_y)+t_last)
        @inbounds @fastmath dS_xy[k]+= N*txy
        @inbounds @fastmath dR[k]+= N*txy*(risk[n1,k]+risk_age[k])
    end
end

function calc_prodTerms_xy!(n::Int64,N::Float64,dS_xy::Vector{Float64},
                        dR::Vector{Float64},risk::Matrix{Float64},
                        risk_age::Vector{Float64},
                        X_mat::Float64,Y::Float64,
                        s_n::Vector{Float64},
                        Γ_x::Float64,Γ_y::Float64,Γ_xy::Float64)

    K = length(dS_xy)
    n1 = max(n,1)
    t_last = Γ_x*Γ_y -Γ_xy
    for k in 1:K
        @inbounds @fastmath txy = s_n[k]*((X_mat - Γ_x)*(Y - Γ_y)+t_last)
        @inbounds @fastmath dS_xy[k]+= N*txy
        @inbounds @fastmath dR[k]+= N*txy*(risk[n1,k]+risk_age[k])
    end
end

function calc_prodTerms_x!(n::Int64,N::Float64,
                        dS_x::Vector{T},
                        dR::Vector{T},risk::Matrix{Float64},
                        risk_age::Vector{Float64},
                        X_mat::Matrix{Float64},
                        s_n::Vector{T},
                        Γ_x::T) where T

    n1 = max(n,1)
    K = length(dS_x)
    for k in 1:K
        @inbounds @fastmath tx = s_n[k]*(X_mat[n+1,k] - Γ_x)
        @inbounds @fastmath dS_x[k]+= N*tx
        @inbounds @fastmath dR[k]+= N*tx*(risk[n1,k]+risk_age[k])
    end
end


function calc_prodTerms_x!(n::Int64,N::Float64,
                        dS_x::Vector{T},
                        dR::Vector{T},risk::Matrix{Float64},
                        risk_age::Vector{Float64},
                        X_mat::Vector{Float64},
                        s_n::Vector{T},
                        Γ_x::T) where T

    K = length(dS_x)
    n1 = max(n,1)
    for k in 1:K
        @inbounds @fastmath tx = s_n[k]*(X_mat[k] - Γ_x)
        @inbounds @fastmath dS_x[k]+= N*tx
        @inbounds @fastmath dR[k]+= N*tx*(risk[n1,k]+risk_age[k])
    end
end

function calc_prodTerms_x!(n::Int64,N::Float64,
                        dS_x::Vector{T},
                        dR::Vector{T},risk::Matrix{Float64},
                        risk_age::Vector{Float64},
                        X_mat::Float64,
                        s_n::Vector{T},
                        Γ_x::T) where T

    K = length(dS_x)
    n1 = max(n,1)
    for k in 1:K
        @inbounds @fastmath tx = s_n[k]*(X_mat - Γ_x)
        @inbounds @fastmath dS_x[k]+= N*tx
        @inbounds @fastmath dR[k]+= N*tx*(risk[n1,k]+risk_age[k])
    end
end
