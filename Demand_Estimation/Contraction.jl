function contraction!(d::InsuranceLogit,p::parDict{T};update::Bool=true) where T
    # Contraction...
    rnd = 0
    eps0 = 1
    tol = 1e-10
    individual_values!(d,p)
    δ_orig = d.deltas
    while (eps0>tol) & (rnd<500)
        rnd+=1
        #Step 1
        δ_init = d.deltas
        individual_shares_norisk(d,p)
        eps0,δ_0,r0 = δ_update!(d,p)
        unpack_δ!(p.δ,d)
        #println(eps0)

        #Step 2
        individual_shares_norisk(d,p)
        eps1,δ_1,r1 = δ_update!(d,p)
        unpack_δ!(p.δ,d)

        #Update
        vn = r1 - r0
        #αn1 = vecdot(r0,vn)/vecdot(vn,vn)
        αn = vecdot(r0,r0)/vecdot(r0,vn)
        #αn3 = -vecdot(r0,r0)/vecdot(vn,vn)

        chg = - 2*αn .*r0 + αn^2 .*vn
        δ_new = δ_init .*exp.(chg)
        #δ_new = δ_init - αn.*r0
        if rnd % 2==0
            #println(eps0)
        end
        if (eps0>1) & (rnd>10)
            println("Break Contraction")
            d.deltas = δ_orig
            break
        end
        if (eps0>10) #| (p.σ>.85)
            #println("Normal")
            d.deltas = δ_1
        else
            #println("SquareM")
            d.deltas = δ_new
            #d.deltas = δ_1
        end

        unpack_δ!(p.δ,d)
        if !update
            d.deltas = δ_init
            break
        end
        #eps = maximum(abs.(chg))
        # println("Contraction Error")
        #print("Intitial Error:  ")
        # print("SquareM Error:  ")
        # println(eps)
    end
    individual_shares(d,p)
end

function contraction!(d::InsuranceLogit,p_vec::Array{T,1};update::Bool=true) where T
    p = parDict(d,p_vec)
    return contraction!(d,p,update=update)
end


# function evaluate_iteration{T}(d::InsuranceLogit,p::parDict{T};update::Bool=true)
#     contraction!(d,p,update=update)
#     ll = log_likelihood(d,p)
#     convert_δ!(d)
#     return ll
# end
#
# function evaluate_iteration!{T}(d::InsuranceLogit, x::Array{T,1};update::Bool=true)
#     # Create Parameter Types
#     parameters = parDict(d,x)
#     return evaluate_iteration(d,parameters,update=update)
# end

function convert_δ!(d::InsuranceLogit)
    J = length(d.deltas)
    deltas_new = Array{Float64}(undef,J)
    for j in d.prods
        if isnan(d.deltas[j])
            deltas_new[j] = 1.0
        else
            deltas_new[j] = ForwardDiff.value(d.deltas[j])
        end
    end
    d.deltas = deltas_new
    return Nothing
end



function δ_update!(d::InsuranceLogit,p::parDict{T};update::Bool=true) where T
    # Calculate overall marketshares and update δ_j
    eps = 0.0
    J = length(d.deltas)
    δ_new = Array{T,1}(J)
    rn = Array{T,1}(J)
    wgts = transpose(weight(d.data))[:,1]
    for j in d.prods
        j_index_all = d.data._productDict[j]
        #s_hat_j= mean(d.s_hat[j_index_all])
        s_hat_j= sliceMean_wgt(p.s_hat,wgts,j_index_all)
        s_j = d.shares[j]
        rn[j] = log(s_j)-log(s_hat_j)
        #d.deltas[j] += chg
        δ_new[j] = d.deltas[j]*(s_j/s_hat_j)

        #err = log(chg)
        eps = max(eps,abs(rn[j]))
    end
    if update
        d.deltas = δ_new
    end
    return eps,δ_new,rn
end


function unpack_δ!(δ::Vector{T},d::InsuranceLogit) where T
    for j in d.prods
        idx_j = d.data._productDict[j]
        for idx in idx_j
            if isnan(d.deltas[j])
                δ[idx] = 1.0
            else
                δ[idx] = d.deltas[j]
            end
        end
    end
    return Nothing
end
