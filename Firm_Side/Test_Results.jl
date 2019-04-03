using PyPlot

function momFitPlot(c::MC_Data,d::InsuranceLogit,p::parMC{T}) where T
    s_hat = p.pars.s_hat
    s_hat_nonrisk = p.s_hat_nonrisk
    s_hat_risk = p.s_hat_risk
    wgts = weight(d.data)[:]
    actuarial_values = c.data[2,:]

    wgts_share = wgts.*s_hat
    any_share = wgts.*s_hat_risk.*c.anyHCC
    none_share = wgts.*s_hat_nonrisk.*(1 .- c.anyHCC)

    c_hat = p.C
    c_hat_nonHCC = p.C_nonrisk
    c_hat_HCC = p.C_HCC

    c_hat_total = c_hat./actuarial_values
    c_hat_nonHCC_total = p.C_nonrisk./actuarial_values
    c_hat_HCC_total = p.C_HCC./actuarial_values

    pmom = Vector{T}(undef,length(c.avgMoments))
    amom = Vector{T}(undef,length(c.ageMoments))

    ## Product and Firm Moments
    for (m,m_idx) in c._avgMomentDict
        c_avg = sliceMean_wgt(c_hat,wgts_share,m_idx)
        pmom[m] = c_avg
    end

    minval = minimum(exp.(c.avgMoments[1:85]))
    maxval = maximum(exp.(c.avgMoments[1:85]))

    figure()
    plot(pmom[1:85],exp.(c.avgMoments[1:85]),linestyle="",marker="o")
    plot([minval,maxval],[minval,maxval],linestyle="--")
    gcf()

    minval = minimum(exp.(c.avgMoments[86:length(pmom)]))
    maxval = maximum(exp.(c.avgMoments[86:length(pmom)]))

    figure()
    plot(pmom[86:length(pmom)],exp.(c.avgMoments[86:length(pmom)]),linestyle="",marker="o")
    plot([minval,maxval],[minval,maxval],linestyle="--")
    gcf()

    minval = minimum(exp.(c.avgMoments))
    maxval = maximum(exp.(c.avgMoments))

    figure()
    plot(pmom,exp.(c.avgMoments),linestyle="",marker="o")
    plot([minval,maxval],[minval,maxval],linestyle="--")
    gcf()


    ## Age Moments
    refval = sliceMean_wgt(c_hat_total,wgts_share,c._ageMomentDict[1])
    for (m,m_idx) in c._ageMomentDict
        c_avg = sliceMean_wgt(c_hat_total,wgts_share,m_idx)
        amom[m] = c_avg/refval
    end

    figure()
    plot(20:5:60,c.ageMoments,linestyle="",marker="o")
    plot(20:5:60,amom,linestyle="--",marker="d")
    gcf()

    all_idx = Int.(1:length(s_hat))
    HCC_avg = sliceMean_wgt(c_hat_HCC_total,any_share,all_idx)
    non_avg = sliceMean_wgt(c_hat_nonHCC_total,none_share,all_idx)
    rmom = HCC_avg/non_avg

    return vcat(pmom,amom,rmom)
    # return vcat(pmom,amom,rmom)
end
