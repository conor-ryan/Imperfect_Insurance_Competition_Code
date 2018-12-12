using StatsFuns
using Primes

### Code up Halton Draws
# Code by Jacob Adenbaum
## r - prime divisor
## g - index in Halton Sequence

### Calculate the gth index of a halton sequnce
# using base r

function Halton(g, r)
    σ = collect(0:r-1)
    return Halton(g, r, σ)
end

function Halton(g, r, σ)
    b = digits(g, base=r)

    ϕ = 0.0
    for (l, d) in enumerate(b)
        ϕ += σ[d+1]*(1/r)^(l)
    end
    return ϕ
end

function ScrambledHalton(n, r)
    σ = vcat(0, randperm(r-1))
    return [Halton(i, r, σ) for i=1:n]
end

function MVHalton(n, dims; scrambled=true, burn=500)
    p = 2
    S = zeros(n, dims)
    for d = 1:dims
        p = nextprime(p+1)
        if !scrambled
            seq = Halton.(1:n+burn, p)
        else
            seq = ScrambledHalton(n+burn, p)
        end
        S[:, d] = seq[burn+1:end]
    end
    return S
end


function MVHaltonNormal(n, dims; scrambled=true, burn=500)
    U = MVHalton(n, dims; scrambled=scrambled, burn=burn)
    Z = norminvcdf.(U)
    return Z
end
