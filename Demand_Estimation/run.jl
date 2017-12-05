# Tables
include("Tables.jl")

# Structure the data
include("CarData.jl")

# Maximum Likelihood
include("LogitMLE.jl")

# Linear GMM
include("LinearGMM.jl")

# BLP
include("BLP.jl")

########################################################################
#################### Load and Clean the Data ###########################
########################################################################
include("load.jl")

########################################################################
#################### Estimate Multinomial Logit on Shares ##############
########################################################################

# Load the Data into Something we can pass to various estimation
# routines
c = CarData(df)
construct_instruments!(c)

# Run MLE on the Multinomial Logit Model
d = MultinomialLogit(c)
estimate!(d, zeros(6))

# Estimate the model using GMM on the log-shares
s = log.(shares(c)) - log.(outside(c))
X = hcat(observables(c))
γ = inv(X'X)*X's
β = γ[1:end-1]
α = γ[end]

# Estimate the model using GMM on the log-shares, instrumenting for
# price
Z = instruments(c)
gmm = LinearGMM(s, X, Z)
estimate!(gmm, inv(Z'Z))

X̂ = Z*inv(Z'Z)*Z'X
γ̂ = inv(X̂'X̂)*X̂'*s

# Estimate the model using 2SLS on the log-shares, with a fixed effect
# for the car model
prods= unique(c.names)
nprod= length(prods)
n    = size(X,1)
dums = zeros(n, nprod-1)

for i=1:nprod-1
    dums[:,i] = c.names .== prods[i+1]
end

P(X) = X*inv(X'X)*X'
M(X) = begin
    n,k = size(X)
    speye(n,n) - P(X)
end

gmm2 = LinearGMM(M(dums)*s, M(dums)*X, Z)
estimate!(gmm2, inv(Z'Z))

fmt  = "{:.3f}"
col1 = TableCol("MLE Logit",
            vcat(c.chars, c.price), pack(d), fmt=fmt)
col2 = TableCol("OLS Logit",
            vcat(c.chars, c.price), γ, fmt=fmt)
col3 = TableCol("IV Logit",
            vcat(c.chars, c.price), params(gmm),fmt=fmt)
col4 = TableCol("IV Logit w/ FE",
            vcat(c.chars, c.price), params(gmm2),fmt=fmt)
t = Table(col1, col2, col3, col4)


########################################################################
#################### Estimate BLP ######################################
########################################################################

b = BLP(c)
