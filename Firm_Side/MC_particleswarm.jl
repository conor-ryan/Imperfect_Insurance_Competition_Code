
function particle_swarm(p0::Vector{Float64},p_est::parDict{Float64},
                d::InsuranceLogit,c::MC_Data,W::Matrix{Float64};
                grad_tol=1e-8,f_tol=1e-8,x_tol=1e-8,max_itr=2000,p_num=10)


    particles = Matrix{Float64}(undef,length(p0),p_num)

end
