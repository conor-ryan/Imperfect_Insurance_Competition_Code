
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

utility <- function(x){
  #y  = sqrt(2*x)
  y = -exp(-x)
  return(y)
}

cost <- function(u){
  #y = u^2/2
  y = -log(-u)
  return(y)
}

cost_der <- function(u){
  #y_prime = u
  y_prime = -1/(u)
  return(y_prime)
}


# h and l switch... 
N = 2
theta_l = .6
theta_h = .9
wealth = 5
damage = 4
mu_h = 0.7
mu_l = 1-mu_h

# No Insurance Utility
u_l_low = theta_l*utility(wealth-damage) + (1-theta_l)*utility(wealth)
u_h_low = theta_h*utility(wealth-damage) + (1-theta_h)*utility(wealth)

# Actuarially Fair Full Insurance
u_l_high = utility(theta_l*(wealth-damage) + (1-theta_l)*wealth)
u_h_high = utility(theta_h*(wealth-damage) + (1-theta_h)*wealth)



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
  
  pi = mu_h*s_h*(wealth-theta_h*damage - (theta_h*c_a + (1-theta_h)*c_n)) +
    mu_l*s_l*(wealth-theta_l*damage - (theta_l*c_a + (1-theta_l)*c_n))
  return(pi)
}

prof_dev <- function(d,u){
  s_h = share(u,u_h_low,N)
  s_l = share(u,u_l_low,N)
  c = cost(u)
  c_der = cost_der(u)
  
  r = mu_h*s_h*(1-theta_h-d*theta_h)*((1-s_h)*(wealth-theta_h*damage-c)-c_der) +
    mu_l*s_l*(1-theta_l-d*theta_l)*((1-s_l)*(wealth-theta_l*damage-c)-c_der)
  return(r)
}

prof_dev2 <- function(d,u){
  s_h = share(u,u_h_low,N)
  s_l = share(u,u_l_low,N)
  ds_h = share_der(u,u_h_low,N)
  ds_l = share_der(u,u_l_low,N)
  c = cost(u)
  c_der = cost_der(u)
  
  r = mu_h*(ds_h*(1-theta_h-d*theta_h)*(wealth-theta_h*damage-c) - 
              s_h*(-theta_h*d*c_der + (1-theta_h)*c_der)) + 
    mu_l*(ds_l*(1-theta_l-d*theta_l)*(wealth-theta_l*damage-c) - 
            s_l*(-theta_l*d*c_der + (1-theta_l)*c_der)) 
  return(r)
}


u_opt = optim(u_l_low,prof_pool,control = list(fnscale=-1,maxit=4000))$par


eps = .001
d_low = (1-theta_h)/theta_h
d_high = (1-theta_l)/theta_l


dR1 <- function(d,u){
  u_h = theta_h*(u-eps*d) + (1-theta_h)*(u+eps)
  u_l = theta_l*(u-eps*d) + (1-theta_l)*(u+eps)
  s_h_sep = share(u_h,u_h_low,N)
  s_l_sep = share(u_l,u_l_low,N)
  s_h_pl = share(u,u_h_low,N)
  s_l_pl = share(u,u_l_low,N)
  
  change = wealth*(mu_h*(s_h_sep-s_h_pl)+mu_l*(s_l_sep-s_l_pl))
  return(change)
}
dopt1 = nleqslv(d_low,dR1,u=u_opt,control=list(ftol=1e-12))$x

dR2 <- function(d,u){
  u_h = theta_h*(u-eps*d) + (1-theta_h)*(u+eps)
  u_l = theta_l*(u-eps*d) + (1-theta_l)*(u+eps)
  s_h_sep = share(u_h,u_h_low,N)
  s_l_sep = share(u_l,u_l_low,N)
  s_h_pl = share(u,u_h_low,N)
  s_l_pl = share(u,u_l_low,N)
  
  change = damage*(mu_h*theta_h*(s_h_pl-s_h_sep)+mu_l*theta_l*(s_l_pl-s_l_sep))
  return(change)
}


dR3 <- function(d,u){
  u_h = theta_h*(u-eps*d) + (1-theta_h)*(u+eps)
  u_l = theta_l*(u-eps*d) + (1-theta_l)*(u+eps)
  s_h_sep = share(u_h,u_h_low,N)
  s_l_sep = share(u_l,u_l_low,N)
  s_h_pl = share(u,u_h_low,N)
  s_l_pl = share(u,u_l_low,N)
  c_pl = cost(u)
  c_a = cost(u-eps*d)
  c_n = cost(u+eps)
  
  change = mu_l*(s_l_pl*c_pl - s_l_sep*(theta_l*c_a+(1-theta_l)*c_n)) +
    mu_h*( theta_h*(s_h_pl*c_pl - s_h_sep*c_a) + (1-theta_h)*(s_h_pl*c_pl - s_h_sep*c_n))
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
u = -.9
d = .2
totalDev1(d,u)/eps
totalDev2(d,u)/eps
prof_dev(d,u)
prof_dev2(d,u)


d_seq = seq(d_low,d_high,.01)
plot(d_seq,dR1(d_seq,u_opt))
plot(d_seq,dR2(d_seq,u_opt))
plot(d_seq,dR3(d_seq,u_opt))


plot(d_seq,totalDev1(d_seq,u_opt))
plot(d_seq,prof_dev(d_seq,u_opt))

testFun <-function(u,N){
  s_h = share(u,u_h_low,N)
  c = cost(u)
  c_der = cost_der(u)
  r = (1-s_h)*(wealth-damage*theta_h-c) - c_der
  return(r)
}

testFun2 <-function(u,N){
  s_l = share(u,u_l_low,N)
  c = cost(u)
  c_der = cost_der(u)
  r = (1-s_l)*(wealth-damage*theta_l-c) - c_der
  return(r)
}

testFun(u_opt,N)
testFun2(u_opt,N)
