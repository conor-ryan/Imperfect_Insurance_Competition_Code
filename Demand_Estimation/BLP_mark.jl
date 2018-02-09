
###############################################################################
# Berry, Levinsohn, Pakes (1995) Replication
# Author: Mark Ponder
###############################################################################
using GLM
using DataFrames
using DataFramesMeta
using TensorOperations
using NLopt



###############################################################################
# Data Setup
###############################################################################
cars = readtable("C:/Users/Ponde001/Desktop/Economic Research/Academic/Study Replications/Berry, Levinsohn, Pakes - 1995/Automobile Data.csv", header = true, separator = ',');

cars[:ln_hpwt] = log(cars[:hpwt])
cars[:ln_space] = log(cars[:space])
cars[:ln_mpg] = log(cars[:mpg])
cars[:ln_mpd] = log(cars[:mpd])
cars[:ln_price] = log(cars[:price])
cars[:trend] = cars[:market] + 70
cars[:cons] = 1

regSet = @linq cars |>
@by(:model_year, s_0 = log(1 - sum(:share)))

regSet = join(cars, regSet, on = :model_year);

regSet = @linq regSet |>
    @transform(s_i = log(:share)) |>
    @transform(dif = :s_i - :s_0);

regSet[:dif_2] = log(regSet[:share]) - log(regSet[:share_out]) ;
regSet[:ln_price] = log(regSet[:price]);

regSet = sort(regSet, cols = [:market,  :firmid])

markets = convert(Matrix, regSet[:, [ :market] ]);
marks = unique(markets)
firms = convert(Matrix, regSet[:, [ :firmid] ]);
modlist = unique(regSet[:newmodv])
modLoc = regSet[:newmodv];

X = convert(Matrix, regSet[:, [ :cons, :hpwt, :air, :mpd, :space] ]) # Demand Variables
W = convert(Matrix, regSet[:, [ :cons, :ln_hpwt, :air, :ln_mpg, :ln_space, :trend] ]) # Supply Variables
p = convert(Matrix, regSet[:, [ :price] ]) # Price
delta_0 = convert(Array, round(regSet[:,:dif_2], 20));

################################################################################
# Model Data Type, not really necessary. needs updating
################################################################################
type ModelData
    price::Array{Float64}
    X::Array{Float64}
    instDemand::Array{Float64}
    instSupply::Array{Float64}
    y::Array{Float64}
    beta::Array{Float64}
    guess::Array{Float64}

    function ModelData( price = [], X = [], instDemand = [], instSupply = [], y = [],
                        delta = [], beta = [] )
        return new(price, X, instDemand, instSupply, y, delta, beta )
    end
end

m = ModelData()
m.price = p
m.X = X
m.guess = regSet[:dif_2];

################################################################################
# Naive Model of Demand
################################################################################
reg = lm(@formula(dif_2 ~ hpwt + air + mpd + space + price), regSet)

################################################################################
# Naive Model of Supply
################################################################################
reg = lm(@formula(ln_price ~ ln_hpwt + air + ln_mpg + ln_space + trend), regSet)

################################################################################
# Calculating Instruments
# BLP calculate instruments are wrong for their Table 3, column 2
# Replace (sub[:,i] .* sameFirm) with (sub[:,i] .* sameFirm)' on line 103 to
# recreate BLP instruments
################################################################################
function gen_inst( inX, normal = 1 )
    totMarket = similar(inX)
    totFirm = similar(inX)

    for m in marks
        sub = inX[find(markets .== m),:]
        firminfo = firms[find(markets .== m),:]
        #modelinfo = modLoc[find(markets .== m),:]
        sameFirm = firminfo .== firminfo'
        #sameFirm = sameFirm - diagm(diag(sameFirm))

        #sameProduct = ones(sameFirm) - diagm(ones(size(sub,1), 1)[:])
        z_1 = similar(sub)
        for i = 1:size(sub, 2)
            z_1[:,i] = sum((sub[:,i] .* sameFirm), 1)' # Prime on sameFirm is key
        end
        totFirm[find(markets .== m),:] = z_1

        # Within Market
        sub = inX[find(markets .== m),:]
        z_1 = similar(sub)
        sameFirm = firminfo .== firminfo'
        for i = 1:size(sub, 2)
            z_1[:,i] = sum((sub[:,i] .* (!sameFirm + sameFirm)),1)
        end
        totMarket[find(markets .== m),:] = z_1
    end

    return [totFirm, totMarket]
end

tmpDemand = gen_inst(X)
Z = hcat(X, tmpDemand[1], tmpDemand[2])
tmpSupply = gen_inst(W)
Z_s = hcat(W, tmpSupply[1], tmpSupply[2], regSet[:,:mpd]);

m.instDemand = copy(Z)
m.instSupply = copy(Z_s);

################################################################################
# Efficient GMM
################################################################################
# Stage 1, Identity weight matrix
delta_0 = deepcopy(m.guess)
Z = hcat(X, tmpDemand[1], tmpDemand[2])
m.instDemand = copy(Z)

baseData = [ m.price m.X]
zxw1 = m.instDemand'baseData

bx1 = inv(zxw1'*zxw1)*zxw1'*m.instDemand'delta_0

# Calculate optimal weight matrix
e = delta_0 - baseData * bx1
g_i = m.instDemand .* e
vg = g_i'g_i/size(g_i,1)
w1 = inv(cholfact(Hermitian(vg)))

# Stage 2 Estimates
bx2 = inv(zxw1'w1*zxw1)*zxw1'w1*m.instDemand'delta_0

# Standard Errors
e = delta_0 - baseData * bx2
g_i = m.instDemand .* e
vg = g_i'g_i/size(g_i,1)
w1 = inv(cholfact(Hermitian(vg)));

ivse = sqrt(diag(inv((zxw1'./size(g_i,1)')*w1*(zxw1./size(g_i,1)))))/sqrt(2217)

################################################################################
# Draws for Numeric Integration
# 1. pseudo-Monte Carlo
# 2. Gaussian Quadrature
# 3. Halton Draws
# Only run subset of code for the type of draws you want to take
################################################################################
# Estimated means and deviations for lognormal distribution
incomeMeans = [2.01156, 2.06526, 2.07843, 2.05775, 2.02915, 2.05346, 2.06745,
2.09805, 2.10404, 2.07208, 2.06019, 2.06561, 2.07672, 2.10437, 2.12608, 2.16426,
2.18071, 2.18856, 2.21250, 2.18377]

sigma_v = 1.72


################################################################################
# pseudo-Monte Carlo
################################################################################
srand(719345)
ns = 1000
v_ik = randn(6,  ns )'
m_t = repeat(incomeMeans, inner = [ns, 1])

#y_it = exp(m_t + sigma_v * repeat(v_ik[:,end], outer = [length(incomeMeans),1]));
y_it = exp(incomeMeans .+ sigma_v * v_ik[:,end]');
unobs_weight = ones(ns)'/ns

################################################################################
# Halton Draws
################################################################################
using StatsFuns
#Pkg.add("Primes")
using Primes

function Halton(i = 1, b = 3)
    f = 1 # value
    r = 0
    while i > 0
        f = f / b
        r = r + f * mod(i, b)
        i = floor(i/b)
    end
    return r
end

function HaltonDraws( k=6, n = 100, primeList = [3, 7, 13, 11, 2, 5])
    draws = zeros(n, k)
    myPrimes = primeList
    for j = 1:k
        draws[:,j] = [norminvcdf( Halton(i, myPrimes[j]) ) for i = 50:50+n - 1]';
    end
    println(myPrimes)
    return draws
end

ns = 10000
v_ik = HaltonDraws(6, ns)
unobs_weight = ones(1, size(v_ik, 1))/ns

# Calculate Sample Incomes
y_it = exp(incomeMeans .+ sigma_v * v_ik[:,end]');

################################################################################
# Quadrature
################################################################################
using SparseGrids

Dim = size(X,2) + 1
nodes, weigths = sparsegrid(Dim,6,kpn)
ns = length(weigths)
v_ik = nodes' * sqrt(2)
unobs_weight = weigths' / sqrt(pi)^Dim

# Calculate Sample Incomes
y_it = exp(incomeMeans .+ sigma_v * v_ik[:,end]');

################################################################################
# Data Preparation, breaking up dataset by market
# The matrices get too large with 1,000,000 draws to be held in memory at the
# same time
################################################################################
using Base.Cartesian
myData = [p convert(Array{Float64,2}, X)]
M = length(marks)
@nexprs 20 j -> (begin
    myData_j = myData[find(markets .== j), :]
end);

# Initial parameter guess for checking code
params0 = [43.501,  3.612, 4.628, 1.818, 1.050, 2.056];
################################################################################
# Calculate the random utility component for market m1 at a set of parameters
# for α and σ
################################################################################
function sim_mu_m(params, m1)
    # Initialize Variables to be used in the calculation
    params = abs(params)
    y = view(y_it, m1, :) # Get current income observations
    v = v_ik[ :, 1:end-1]
    v2 = hcat(-1./y, v) .* params'
    sub = Symbol("myData_$m1")
    @eval begin
        mu = exp($sub * $v2')
    end
    return mu
end
@time test = sim_mu_m( params0, 1);

################################################################################
# Calculate market shares
################################################################################
function calc_share_m(expdelta, expmu)
    # Calculate the Market Shares
    # Combine the average effect with the individual random effect
    u_ijt = expdelta .* expmu

    prob = u_ijt ./ (1+sum(u_ijt, 1))
end
@time test2 = calc_share_m( exp(delta_0[1:92]), test);

################################################################################
# Calculate marginal costs by market given individual choice probabilities
# (from calc_share_m) and an estimate of α
################################################################################
function calc_mc_m(p_ijt, params, m)
    alpha = params[1]
    s_jt = p_ijt * unobs_weight'
    firm_yr = firms[find(markets .== m),:]
    price = p[find(markets .== m)]
    income = view(y_it, m, :)'

    sameFirm = convert(Array{Float64, 2}, firm_yr .== firm_yr')

    tmp2 = p_ijt .* unobs_weight ./ income * alpha

    @tensor πdiff[d,c] := tmp2[d,b]*p_ijt[c,b] # This is just matrix multiplication, @tensor seemed to do it faster

    subMatrix = -(πdiff .- diagm([sum(tmp2,2)...])) .* sameFirm

    b = inv(subMatrix)* s_jt
    mc = price - b
    mc[mc.<0] = .001
    return mc
end
@time t4 = calc_mc_m(test2, params0[1], 1);

################################################################################
# Root finding methods
# 1. Contraction mapping
# 2. Newton's method
# 3. BB gradient-free spectral method
# Contraction mapping is slow, Newton's method is fast and stable for
# Halton and pseudo-Monte Carlo draws. BB is stable, faster than contraction,
# but slower than Newton's. I combine BB and Newton's for the quadrature method
# Quadrature methods use negative weights, which can result in negative shares
# This is an issue for the contraction mapping (which won't converge) and
# Newton's method (because you need the log transformation for it to be globally)
# stable.
################################################################################

################################################################################
# Contraction mapping, using Nevo's transformation
################################################################################
function contraction_m( delta, mu, act_share_m)

    delta0 = exp(delta);  eps = 1;
    count = 0; flag = 0

    while (count < 1000 && eps > 10e-9 && flag == 0)
        s_0 = calc_share_m(delta0, mu) * unobs_weight'

        delta1 = delta0 .* (act_share_m ./ s_0)
        eps = maximum(abs((s_0./act_share_m) - 1))
        delta0 = delta1
        count += 1
        flag = sum(delta0 .< 0) > 0
    end
    flag != 0 && throw(BoundsError())
    eps > 10e-9 && throw(DomainError())
    return log(delta0)

end
@time newDelta2 = contraction_m( delta_0[1:92], test,  act_share[find(markets .== 1),:] );

################################################################################
# Newton's Method
################################################################################
function newtonsMethod_m(delta, mu, act_share_m)
    delta_base = zeros(size(mu,1),1)

    eps = 1 ; count = 0; δ1 = copy(delta);

    while (count < 200)*(maximum(abs(eps)) > 1e-14)
        count += 1
        u_ij = mu.*exp(δ1)
        P_1 = u_ij ./(1+sum(u_ij, 1))
        m1 = P_1 * unobs_weight'
        tmp1 = P_1 .* unobs_weight

        @tensor Jf[a,c] := tmp1[a,b]*P_1[c,b]  # This is just matrix multiplication, @tensor seemed to do it faster

        f_δ = log(m1) - log(act_share_m)

        Jf = (eye(size(Jf)...) - Jf./m1)
        eps = - inv(Jf) * f_δ

        isnan(maximum(abs(eps))) && throw(DomainError("Diverged to Infinity: First Newton"))

        δ1_0 = δ1 + eps
        δ1 = δ1_0
    end
    maximum(abs(eps)) > 1e-9 && throw(DomainError())
    isnan(maximum(abs(eps)))  && throw(DomainError())
    return δ1
end
@time myDelta = newtonsMethod_m( delta_0[1:92], test, act_share[find(markets .== 1),:]);

################################################################################
# BB Spectral Method
################################################################################
function BB(delta, mu1, act_share_m)
    eps = 1; h = 0;  δ1 = delta*0.0;
    αh = 1
    while (h < 1000)*(maximum(abs(eps)) > 1e-14)
        h += 1
        if h == 1
            u_ij = mu1.*exp(δ1)
            P_1 = u_ij ./(1+sum(u_ij, 1))
            m1 = P_1 * unobs_weight'
            fδ1 = m1 - act_share_m
            δ2 = δ1 - αh .* fδ1
            δ0 = δ1
            δ1 = δ2
            fδ0 = fδ1
        else
            u_ij = mu1.*exp(δ1)
            P_1 = u_ij ./(1+sum(u_ij, 1))
            m1 = P_1 * unobs_weight'
            fδ1 = m1 - act_share_m
            # Step Direction
            sh = δ1 - δ0
            yh = fδ1 - fδ0
            αh = sh'yh/yh'yh

            δ2 = δ1 - αh .* fδ1
            eps = maximum(abs(δ2 - δ1))
            δ0 = δ1
            δ1 = δ2
            fδ0 = fδ1
        end
    end
    isnan(maximum(abs(eps)))  && throw(DomainError())
   # eps > 10e-9 && throw(DomainError())
    δ1
end
@time BB( convert(Array, delta_0)[1:92], test, act_share[find(markets .== 1),:] );


################################################################################
################################################################################
# Combine Root finding techniques
# Try Newton's method, if it fails switch to BB
################################################################################
################################################################################
function BLPRoot(delta0, mu, act_share_m)
    try
        δNew = newtonsMethod_m(delta0, mu, act_share_m)
    catch
        try
            δNew = newtonsMethod_m(delta0*0.0, mu, act_share_m)
        catch
            δNew = BB(delta0, mu, act_share_m)
        end
    end
end

################################################################################
################################################################################
# Generate Dependent Variables
# Given parameter α and σ, calculate the δ and log(mc) for each market
################################################################################
################################################################################
function genDep( params, m )
    mc = zeros(length(markets),1)
    for m1 in marks
        IndObs = find(markets .== m1)

        δ0 = m.guess[IndObs]
        mu = sim_mu_m( params, m1)

        δ1 = BLPRoot(δ0, mu, act_share[IndObs,:])

        p_ijt = calc_share_m(exp(δ1), mu)
        m.guess[IndObs] = δ1
        mc1 = calc_mc_m(p_ijt, params[1], m1)
        mc[IndObs] = mc1
    end
    y = vcat(m.guess, log(mc))
end
@time genDep( [39.5892,4.10949,0.0,0.357889,2.03407,3.30828], m );


################################################################################
# GMM function and weight matrix
################################################################################
z = vcat(hcat(m.instDemand, zeros(m.instSupply)) , hcat(zeros(m.instDemand), m.instSupply))
xw = vcat(hcat(X, zeros(W)) , hcat(zeros(X), W))
zxw = z'xw;

w1 = eye(size(z,2)) # You can change this to a different initial weighting matrix
preMult = inv(zxw'w1*zxw)*zxw'w1*z'
function gmm( y, preMult )
    bxw = preMult*y;
    return bxw
end

################################################################################
# Minimizing the GMM Objective Function
################################################################################
m.guess = deepcopy(delta_0);
param0 = [43.501,  3.612, 4.628, 1.818, 1.050, 2.056]
function GMM( param0, weight, m )
    preMult = inv(zxw'weight*zxw)*zxw'weight*z'

    function ObjFunc(theta_2::Vector, grad::Vector)
            y = genDep(theta_2, m)
            bxw = gmm( y, preMult )
            xi_w = y - xw*bxw
            g = z'xi_w/size(xi_w,1)
            quadForm = (g'weight*g*34)[1]
             println(theta_2," ", quadForm)
            return quadForm

    end

    opt = Opt(:LN_NELDERMEAD, 6)
    lower_bounds!(opt, [10.0, 0., 0., 0., 0., 0.])
    initial_step!(opt, [6.0, 1.0, 1.0, 1.0, 1.0, 1.0]/2)
    ftol_rel!(opt,1e-2)
    maxeval!(opt, 200)
    min_objective!(opt, ObjFunc)

    minf,minx,ret = NLopt.optimize(opt, param0 )
end

################################################################################
# Standard Errors, follows Petrin (2002)
################################################################################
function BLPse(param0, weight, m)
    theta_2 = param0
    y = genDep(theta_2, m)
    preMult = inv(zxw'weight*zxw)*zxw'weight*z'
    bxw = gmm( y, preMult )
    base_xi_w = y - xw*bxw

    de = zeros( size(xw, 1), length(theta_2) )
    ident = eye( length(theta_2) )

    for i=1:length(theta_2)
        theta_2 = deepcopy(param0)
        theta_2[i] += max(1e-4*minx[i],1e-6)
        y = genDep(theta_2, m)
        bxw = gmm( y, preMult )
        xi_w = y - xw*bxw
        de[:,i] = (xi_w - base_xi_w) / (max(1e-4*minx[i],1e-6))
    end

    de2 = hcat(de, -xw)
    g_ind = z.*base_xi_w
    Gamma = z'de2/(size(g_ind,1))
    GammaInv = inv(Gamma'Gamma)
    g = mean(g_ind,1)
    vg = g_ind'g_ind/(size(g_ind,1)) - g.*g'

    variance= GammaInv'*Gamma'*vg*Gamma*GammaInv / (size(g_ind,1))
    standardErrors = sqrt(diag(variance))
end

################################################################################
################################################################################
# Efficient GMM
################################################################################
################################################################################
@time minf,minx,ret = GMM(param0, w1, m)
y = genDep( minx, m)
preMult = inv(zxw'w1*zxw)*zxw'w1*z'
bxw = gmm( y, preMult )
xi_w = y - xw*bxw

g_i = z.*xi_w

vg = g_i'g_i/size(xi_w,1)

w2 = inv(vg)
@time minf,minx,ret = GMM(param0, w2, m)
myse = BLPse(minx, w2, m);
y = genDep( minx, m)
preMult = inv(zxw'w2*zxw)*zxw'w2*z'
bxw = gmm( y, preMult )
