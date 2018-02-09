library(nleqslv)
#### Pooling Analysis ####
rm(list = ls())
share <- function(u,out,N){
  y = exp(u-out)/(1 + N*exp(u-out))
  return(y)
}

share_der <- function(u,out,N){
  y = share(u,out,N)
  y_prime = y*(1-y)
  return(y_prime)
}

shareDev <- function(u_dev,u,out,N){
  y = exp(u_dev-out)/(1 + exp(u_dev-out)+(N-1)*exp(u-out))
  return(y)
}

utility <- function(x){
  #y  = (4/3*x)^(3/4)
  #y  = sqrt(2*x)
  y = -exp(-x)
  return(y)
}

cost <- function(u){
  #y = u^(4/3)*3/4
  #y = u^2/2
  y = -log(-u)
  return(y)
}

cost_der <- function(u){
  #y_prime = u^(1/3)
  #y_prime = u
  y_prime = -1/(u)
  return(y_prime)
}

## Accident and No-accident Utility Values, given u_h and u_l
util_al <- function(u_h,u_l){
  y = ((1-theta_l)*u_h - (1-theta_h)*u_l)/(theta_h - theta_l)
  return(y)
}

util_nl <- function(u_h,u_l){
  y = (theta_h*u_l - theta_l*u_h)/( theta_h - theta_l )
  return(y)
}

## Profit Functions, given u_h and u_l
prof_h <- function(u_h){
  y = wealth - theta_h*damage - cost(u_h)
  return(y)
}

prof_l <- function(u_h,u_l){
  y = wealth - theta_l*damage - theta_l*cost(util_al(u_h,u_l)) - (1-theta_l)*(cost(util_nl(u_h,u_l)))
  return(y)
}


#### Parameters ####
N = 1000
theta_l = .6
theta_h = .9
wealth = 5
damage = 4
mu_h = 0.5
mu_l = 1-mu_h

# Perfect Competition
theta_bar = mu_h*theta_h + mu_l*theta_l
theta_bar = .3*theta_h + .7*theta_l

# No Insurance Utility
u_l_low = theta_l*utility(wealth-damage) + (1-theta_l)*utility(wealth)
u_h_low = theta_h*utility(wealth-damage) + (1-theta_h)*utility(wealth)

# Actuarially Fair Full Insurance
u_l_high = utility(theta_l*(wealth-damage) + (1-theta_l)*wealth)
u_h_high = utility(theta_h*(wealth-damage) + (1-theta_h)*wealth)


# Non RA Equilibrium 
normEquil <- function(u,N){
  u_h = u[1]
  u_l = u[2]
  u_al = util_al(u_h,u_l)
  u_nl = util_nl(u_h,u_l)  
  pi_h = prof_h(u_h)
  pi_l = prof_l(u_h,u_l)
  c_uh = cost_der(u_h)
  c_unl = cost_der(u_nl)
  c_ual = cost_der(u_al)
  S_ul = share(u_l,u_l_low,N)
  S_uh = share(u_h,u_h_low,N)
  Sder_ul = share_der(u_l,u_l_low,N)
  Sder_uh = share_der(u_h,u_h_low,N)
  
  y1 = (Sder_uh/S_uh)*pi_h - c_uh + (mu_l/mu_h) * (S_ul/S_uh) * (theta_l*(1-theta_l)/(theta_h-theta_l)) *(c_unl - c_ual)
  y2 = (Sder_ul/S_ul)*pi_l*(theta_h-theta_l) - (theta_h*(1-theta_l)*c_unl) + (theta_l*(1-theta_h)*c_ual)

  return(c(y1,y2))
}


prof_pool <- function(u){
  if(length(u)>1){
    u_a = u[1]
    u_n = u[2]
  }else{
    u_a = u
    u_n = u
  }
  u_h = theta_h*u_a + (1-theta_h)*u_n
  u_l = theta_l*u_a + (1-theta_l)*u_n
  s_h = share(u_h,u_h_low,N)
  s_l = share(u_l,u_l_low,N)
  c_a = cost(u_a)
  c_n = cost(u_n)
  
  pi = (mu_h*s_h+mu_l*s_l)*(wealth-theta_bar*damage - (theta_bar*c_a + (1-theta_bar)*c_n))  
  return(pi)
}

prof_sep <- function(u){
  u_h = u[1]
  u_a = u[2]
  u_n = u[3]
  u_l = theta_l*u_a + (1-theta_l)*u_n
  s_h = share(u_h,u_h_low,N)
  s_l = share(u_l,u_l_low,N)
  c_a = cost(u_a)
  c_n = cost(u_n)
  
  pi = mu_h*s_h*(wealth-theta_bar*damage - cost(u_h)) +
    mu_l*s_l*(wealth-theta_bar*damage - (theta_bar*c_a + (1-theta_bar)*c_n))
  return(pi)
}

noConstEq <- function(u,N){
  u_h = u[1]
  u_a = u[2]
  u_n = u[3]
  u_l = theta_l*u_a + (1-theta_l)*u_n
  mkup_h = share(u_h,u_h_low,N)/share_der(u_h,u_h_low,N)
  mkup_l = share(u_l,u_l_low,N)/share_der(u_l,u_l_low,N)
  
  eq1 = wealth - theta_bar*damage - cost(u_h) - mkup_h*cost_der(u_h)
  eq2 = wealth - theta_bar*damage - (theta_bar*cost(u_a) + (1-theta_bar)*cost(u_n)) - 
    mkup_l*cost_der(u_a)*theta_bar/theta_l
  eq3 = cost_der(u_a)*theta_bar/theta_l - cost_der(u_n)*(1-theta_bar)/(1-theta_l)
  return(c(eq1,eq2,eq3))
}

constEquil <- function(u,N){
  u_h = u[1]
  u_a = u[2]
  u_n = u[3]
  u_l = theta_l*u_a + (1-theta_l)*u_n
  mkup_h = share(u_h,u_h_low,N)/share_der(u_h,u_h_low,N)
  mkup_l = share(u_l,u_l_low,N)/share_der(u_l,u_l_low,N)
  mkup_mix = mu_l*share(u_l,u_l_low,N)/(mu_h*share_der(u_h,u_h_low,N))

  eq1 = wealth - theta_bar*damage - (theta_bar*cost(u_a) + (1-theta_bar)*cost(u_n)) -
    mkup_l*(theta_bar*cost_der(u_a) + (1-theta_bar)*cost_der(u_n)) -
    mkup_l*(1/(theta_h-theta_l))*(theta_l*(1-theta_bar)*cost_der(u_n) - 
                                     theta_bar*(1-theta_l)*cost_der(u_a))
  eq2 = wealth - theta_bar*damage - cost(u_h) - mkup_h*cost_der(u_h) +
    mkup_mix*(1/(theta_h-theta_l))*(theta_l*(1-theta_bar)*cost_der(u_n) - 
                                      theta_bar*(1-theta_l)*cost_der(u_a))
  eq3 = u_h - theta_h*u_a - (1-theta_h)*u_n
  return(c(eq1,eq2,eq3))
}

poolEquil <- function(u,N){
  u_h = u
  u_l = u
  s_h = share(u_h,u_h_low,N)
  s_l = share(u_l,u_l_low,N)
  sder_h = share_der(u_h,u_h_low,N)
  sder_l = share_der(u_l,u_l_low,N)
  
  eq1 = (mu_h*sder_h + mu_l*sder_l)*(wealth - theta_bar*damage-cost(u)) -
    (mu_h*s_h + mu_l*s_l)*(cost_der(u))
    
  return(eq1)
}

noConstDist <- function(u_n,u_l){
  eq3= cost_der((u_l - (1-theta_l)*u_n)/theta_l)*theta_bar/theta_l - cost_der(u_n)*(1-theta_bar)/(1-theta_l)
  return(eq3)
}

#### Equilibria ####
# Normal non-RA Equilibrium
res =nleqslv(c(u_h_low,u_l_low),normEquil,control = list(stepmax=1e-1,maxit=4000),N=N)
print(res$message)
print(res$x)

# Unconstrained RA Equilibrium
res =nleqslv(c(u_h_low,u_l_low,u_l_low),noConstEq,control = list(stepmax=1e-1,maxit=40000),N=N)
print(res$message)
print(res$x)
prof_sep(res$x)
#u_l
theta_l*res$x[2] + (1-theta_l)*res$x[3]

# Constrained RA Equilibrium
res =nleqslv(c(u_h_low,u_l_low,u_l_low),constEquil,control = list(stepmax=1e-1,maxit=40000),N=N)
print(res$message)
print(res$x)
prof_sep(res$x)
#u_l
theta_l*res$x[2] + (1-theta_l)*res$x[3]

#Constrained?
cost_der(res$x[3])*(1-theta_bar)/(1-theta_l)-cost_der(res$x[2])*theta_bar/theta_l

# Pooling Equilibrium
res =nleqslv(u_l_low,poolEquil,control = list(stepmax=1e-1,maxit=40000),N=N)
print(res$message)
print(res$x)


#### Menu Analysis ####
# Unconst Distortion
u_l_seq = seq(u_l_low,u_l_high,length.out = 20)
res =nleqslv(u_l_low,noConstDist,control = list(stepmax=1e-1,maxit=40000),u_l = u_l)
u_n = res$x
u_a = (u_l - u_n*(1-theta_l))/theta_l



#### Pooling Analysis ####
prof_dev <- function(d,u){
  s_h = share(u,u_h_low,N)
  s_l = share(u,u_l_low,N)
  c = cost(u)
  c_der = cost_der(u)
  
  r = (mu_h*s_h*(1-s_h)*(1-theta_h-d*theta_h) + mu_l*s_l*(1-s_l)*(1-theta_l-d*theta_l))*
    (wealth-theta_bar*damage) - 
    (mu_h*s_h*(1-s_h)*(1-theta_h-d*theta_h) + mu_l*s_l*(1-s_l)*(1-theta_l-d*theta_l))*c - 
    (mu_h*s_h + mu_l*s_l)*c_der*(1-theta_bar-d*theta_bar)
    
  return(r)
}

prof_dev2 <- function(d,u){
  s_h = share(u,u_h_low,N)
  s_l = share(u,u_l_low,N)
  ds_h = share_der(u,u_h_low,N)
  ds_l = share_der(u,u_l_low,N)
  c = cost(u)
  c_der = cost_der(u)
  
  r = mu_h*(ds_h*(1-theta_h-d*theta_h)*(wealth-theta_bar*damage-c) - 
              s_h*(-theta_bar*d*c_der + (1-theta_bar)*c_der)) + 
    mu_l*(ds_l*(1-theta_l-d*theta_l)*(wealth-theta_bar*damage-c) - 
            s_l*(-theta_bar*d*c_der + (1-theta_bar)*c_der)) 
  return(r)
}


res =nleqslv(u_l_low,poolEquil,control = list(stepmax=1e-1,maxit=4000),N=N)
u_opt = res$x

eps = .001
d_low = (1-theta_h)/theta_h
d_high = (1-theta_l)/theta_l

prof_dev(d_low,u_opt)
prof_dev2(d_low,u_opt)


dR1 <- function(d,u){
  u_h = theta_h*(u-eps*d) + (1-theta_h)*(u+eps)
  u_l = theta_l*(u-eps*d) + (1-theta_l)*(u+eps)
  s_h_sep = shareDev(u_h,u,u_h_low,N)
  s_l_sep = shareDev(u_l,u,u_l_low,N)
  s_h_pl = share(u,u_h_low,N)
  s_l_pl = share(u,u_l_low,N)
  
  change = wealth*(mu_h*(s_h_sep-s_h_pl)+mu_l*(s_l_sep-s_l_pl))
  return(change)
}

dR2 <- function(d,u){
  u_h = theta_h*(u-eps*d) + (1-theta_h)*(u+eps)
  u_l = theta_l*(u-eps*d) + (1-theta_l)*(u+eps)
  s_h_sep = shareDev(u_h,u,u_h_low,N)
  s_l_sep = shareDev(u_l,u,u_l_low,N)
  s_h_pl = share(u,u_h_low,N)
  s_l_pl = share(u,u_l_low,N)
  
  change = damage*theta_bar*(mu_h*(s_h_pl-s_h_sep)+mu_l*(s_l_pl-s_l_sep))
  return(change)
}

dR3 <- function(d,u){
  u_h = theta_h*(u-eps*d) + (1-theta_h)*(u+eps)
  u_l = theta_l*(u-eps*d) + (1-theta_l)*(u+eps)
  s_h_sep = shareDev(u_h,u,u_h_low,N)
  s_l_sep = shareDev(u_l,u,u_l_low,N)
  s_h_pl = share(u,u_h_low,N)
  s_l_pl = share(u,u_l_low,N)
  c_pl = cost(u)
  c_a = cost(u-eps*d)
  c_n = cost(u+eps)
  
  change = mu_l*(s_l_pl*c_pl - s_l_sep*(theta_bar*c_a+(1-theta_bar)*c_n)) +
    mu_h*( theta_bar*(s_h_pl*c_pl - s_h_sep*c_a) + (1-theta_bar)*(s_h_pl*c_pl - s_h_sep*c_n))
  return(change)
}

totalDev1 <-function(d,u){
  return(dR1(d,u)+dR2(d,u)+dR3(d,u))
}
totalDev2 <-function(d,u){
  u_a = u - eps*d
  u_n = u + eps
  return(prof_pool(c(u_a,u_n))-prof_pool(u))
}

u = -.5
d = .6
totalDev1(d,u)/eps
totalDev2(d,u)/eps
prof_dev(d,u)
prof_dev2(d,u)


d_seq = seq(d_low,d_high,.01)
plot(d_seq,dR1(d_seq,u_opt))
plot(d_seq,dR2(d_seq,u_opt))
plot(d_seq,dR3(d_seq,u_opt))


plot(d_seq,totalDev1(d_seq,u_opt)/eps)
plot(d_seq,prof_dev(d_seq,u_opt))


