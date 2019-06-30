function unpack_P!(firm::firmData)
    age = firm[:ageRate]
    Mems   = firm[:MEMBERS]
    for j in firm.prods
        idx_j = firm._productDict[j]
        for idx in idx_j
            firm.Rev_ij[idx] = firm.P_j[j]*age[idx]/Mems[idx]
        end
    end
    return nothing
end



function calcBenchmark(firm::firmData)
    mkts = keys(firm.mkt_index)
    benchmarkPrem = Vector{Float64}(undef,length(mkts))
    for m in mkts
        prems = firm.P_j[firm.mkt_index[m]][firm.silver_index[m]]
        hix_cnts = firm.hix_cnt[firm.mkt_index[m]][firm.silver_index[m]]
        ind = sortperm(prems)
        if (hix_cnts[ind][1]>1) | (length(prems)==1)
            bench_index = 1
        else
            bench_index = 2
        end
        benchmarkPrem[m] = prems[ind][bench_index]
    end
    benchLong = benchmarkPrem[firm.mkt_index_long]
    return benchLong
end

function origBenchmark(firm::firmData)
    mkts = keys(firm.mkt_index)
    benchmarkPrem = Vector{Float64}(undef,length(mkts))
    for m in mkts
        benchmarkPrem[m] = unique(firm.bench_base[firm.mkt_index[m]])[1]
    end
    benchLong = benchmarkPrem[firm.mkt_index_long]
    return benchLong
end

function calcSubsidy!(firm::firmData;foc_check=false)
    if foc_check
        benchmarks = origBenchmark(firm)
    else
        benchmarks = calcBenchmark(firm)
    end
    ageRate = firm[:ageRate]
    incCont = firm[:IncomeCont]
    catas   = firm[:Catastrophic]
    Mems   = firm[:MEMBERS]
    N = length(firm.subsidy_ij)
    firm.zero_ij[:].=0.0
    for n in 1:N
        subs = max(benchmarks[n]*ageRate[n]-incCont[n]*1000/12,0)*(1-catas[n])
        firm.subsidy_ij[n] = min(firm.Rev_ij[n]*Mems[n],subs)
        if (subs>(firm.Rev_ij[n]*Mems[n]))
            firm.zero_ij[n]=1.0
        end
    end
    return nothing
end

function premPaid!(firm::firmData;foc_check=false)
    unpack_P!(firm::firmData)
    calcSubsidy!(firm,foc_check=foc_check)
    subsidy = firm.subsidy_ij

    Mandate = firm[:Mandate]
    age = firm[:ageRate]
    Mems   = firm[:MEMBERS]
    N = length(firm.P_ij)

    for n in 1:N
        # price = max(firm.Rev_ij[n]*Mems[n]-subsidy[n],0)
        price = firm.Rev_ij[n]*Mems[n]-subsidy[n]
        price = ((price*12/Mems[n]) - Mandate[n])/1000
        firm.P_ij[n] = price
    end
    return nothing
end
