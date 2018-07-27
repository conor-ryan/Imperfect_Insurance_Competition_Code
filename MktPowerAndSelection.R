##########################
#### Fixed Contracts ####
##########################
rm(list = ls())
library(nleqslv)

#### Functions ####
lambda = 1
share <- function(u_1,u_2,N){
  y = exp(u_1)/(1 + N*(exp(u_1)+exp(u_2)))
  return(y)
}

share_der_low <- function(s){
  y_prime = -s*(1-s)
  return(y_prime)
}

share_der_high <- function(s){
  y_prime = -alpha_h*s*(1-s)
  return(y_prime)
}

share_der_cross_low <- function(s_1,s_2){
  y_prime = s_1*s_2
  return(y_prime)
}

share_der_cross_high <- function(s_1,s_2){
  y_prime = alpha_h*s_1*s_2
  return(y_prime)
}

utility_low <- function(p,x){
  u = (1*x - 1*p)
  return(u)
}
utility_high <- function(p,x){
  u = (5*x - alpha_h*p)
  return(u)
}

profit <- function(p,N=1){
  p_h = p[1]
  p_l = p[2]
  
  u_hh = utility_high(p_h,x_h)
  u_hl = utility_high(p_l,x_l)
  u_lh = utility_low(p_h,x_h)
  u_ll = utility_low(p_l,x_l)
  
  S_hh =  share(u_hh,u_hl,N)
  S_hl =  share(u_hl,u_hh,N)
  
  S_lh =  share(u_lh,u_ll,N)
  S_ll =  share(u_ll,u_lh,N)
  
  Pi = (mu_l*S_ll)*(p_l - x_l*c_l) + 
    (mu_l*S_lh)*(p_h - x_h*c_l) + 
    (mu_h*S_hl)*(p_l - x_l*c_h) + 
    (mu_h*S_hh)*(p_h - x_h*c_h)
  return(Pi)
}

profit_max <- function(p,N=1){
  Pi = -profit(p,N)
  return(Pi)
}

foc <- function(p,N=1){
  p_h = p[1]
  p_l = p[2]
  
  u_hh = utility_high(p_h,x_h)
  u_hl = utility_high(p_l,x_l)
  u_lh = utility_low(p_h,x_h)
  u_ll = utility_low(p_l,x_l)
  
  S_hh =  share(u_hh,u_hl,N)
  S_hl =  share(u_hl,u_hh,N)
  
  S_lh =  share(u_lh,u_ll,N)
  S_ll =  share(u_ll,u_lh,N)
  
  dS_hh =  share_der_high(S_hh)
  dS_hl =  share_der_high(S_hl)
  
  dS_lh =  share_der_low(S_lh)
  dS_ll =  share_der_low(S_ll)
  
  dS_hx =  share_der_cross_high(S_hh,S_hl)
  dS_lx =  share_der_cross_low(S_lh,S_ll)
  
  S_l = mu_h*S_hl + mu_l*S_ll
  dS_l = mu_h*dS_hl + mu_l*dS_ll
  S_h = mu_h*S_hh + mu_l*S_lh
  dS_h = mu_h*dS_hh + mu_l*dS_lh
  
  # foc_l = -p_l -S_l/dS_l + x_l*(mu_h*dS_hl*c_h + mu_l*dS_ll*c_l)/dS_l
  # foc_h = -p_h -S_h/dS_h + x_h*(mu_h*dS_hh*c_h + mu_l*dS_lh*c_l)/dS_h
  
  foc_l = p_l*(mu_l*dS_ll + mu_h*dS_hl) - (mu_l*dS_ll*x_l*c_l + mu_h*dS_hl*x_l*c_h) + mu_h*S_hl + mu_l*S_ll + 
          p_h*(mu_l*dS_lx + mu_h*dS_hx) - (mu_l*dS_lx*x_h*c_l + mu_h*dS_hx*x_h*c_h)
  
  
  foc_h = p_h*(mu_l*dS_lh + mu_h*dS_hh) - (mu_l*dS_lh*x_h*c_l + mu_h*dS_hh*x_h*c_h) + mu_h*S_hh + mu_l*S_lh + 
          p_l*(mu_l*dS_lx + mu_h*dS_hx) - (mu_l*dS_lx*x_l*c_l + mu_h*dS_hx*x_l*c_h)
  
  return(c(foc_h,foc_l))
}

foc_separate <- function(p,N=1){
  p_h = p[1]
  p_l = p[2]
  
  u_hh = utility_high(p_h,x_h)
  u_hl = utility_high(p_l,x_l)
  u_lh = utility_low(p_h,x_h)
  u_ll = utility_low(p_l,x_l)
  
  S_hh =  share(u_hh,u_hl,N)
  S_hl =  share(u_hl,u_hh,N)
  
  S_lh =  share(u_lh,u_ll,N)
  S_ll =  share(u_ll,u_lh,N)
  
  dS_hh =  share_der_high(S_hh)
  dS_hl =  share_der_high(S_hl)
  
  dS_lh =  share_der_low(S_lh)
  dS_ll =  share_der_low(S_ll)
  
  dS_hx =  share_der_cross_high(S_hh,S_hl)
  dS_lx =  share_der_cross_low(S_lh,S_ll)
  
  S_l = mu_h*S_hl + mu_l*S_ll
  dS_l = mu_h*dS_hl + mu_l*dS_ll
  S_h = mu_h*S_hh + mu_l*S_lh
  dS_h = mu_h*dS_hh + mu_l*dS_lh
  
  # foc_l = -p_l -S_l/dS_l + x_l*(mu_h*dS_hl*c_h + mu_l*dS_ll*c_l)/dS_l
  # foc_h = -p_h -S_h/dS_h + x_h*(mu_h*dS_hh*c_h + mu_l*dS_lh*c_l)/dS_h
  
  foc_l = p_l*(mu_l*dS_ll + mu_h*dS_hl) - (mu_l*dS_ll*x_l*c_l + mu_h*dS_hl*x_l*c_h) + mu_h*S_hl + mu_l*S_ll 
  
  
  foc_h = p_h*(mu_l*dS_lh + mu_h*dS_hh) - (mu_l*dS_lh*x_h*c_l + mu_h*dS_hh*x_h*c_h) + mu_h*S_hh + mu_l*S_lh 
  
  return(c(foc_h,foc_l))
}

PC_profit <- function(p,N=1){
  p_h = p[1]
  p_l = p[2]
  
  u_hh = utility_high(p_h,x_h)
  u_hl = utility_high(p_l,x_l)
  u_lh = utility_low(p_h,x_h)
  u_ll = utility_low(p_l,x_l)
  
  S_hh =  share(u_hh,u_hl,N)
  S_hl =  share(u_hl,u_hh,N)
  
  S_lh =  share(u_lh,u_ll,N)
  S_ll =  share(u_ll,u_lh,N)
  
  
  # 
  # pi_l = (mu_l*S_ll)*(p_l - x_l*c_l) + 
  #   (mu_h*S_hl)*(p_l - x_l*c_h)
  pi_l = p_l - x_l*(mu_l*S_ll*c_l + mu_h*S_hl*c_h)/(mu_l*S_ll+mu_h*S_hl)
  
  pi_h =  p_h - x_h*(mu_l*S_lh*c_l + mu_h*S_hh*c_h)/(mu_l*S_lh+mu_h*S_hh)
  
  
  return(c(pi_h,pi_l))
}



#### Parameters ####
mu_l = .7
mu_h = 1-mu_l
x_h = .8
x_l = .5
c_h = 9
c_l = 4

p_l = 1
p_h = 1.5

N = 1

alpha_h = 1

#### Equilibrium ####
## NEEDS A FIX
# Equal results for Monopoly
res1 = nleqslv(c(p_h,p_l),fn=foc,N=5)
p1 = res1$x

res2 = optim(p1,fn=profit_max,N=N)
p2 = res2$par

res3 = nleqslv(c(p_h,p_l),fn=PC_profit,N=1)
p3 = res3$x

res4 = nleqslv(c(p_h,p_l),fn=foc_separate,N=100)
p4 = res4$x

p5 = c(1.0125,1)
# Properties of Equilibrium 
p_h = p1[1]
p_l = p1[2]

u_hh = utility_high(p_h,x_h)
u_hl = utility_high(p_l,x_l)
u_lh = utility_low(p_h,x_h)
u_ll = utility_low(p_l,x_l)

S_hh =  share(u_hh,u_hl,N)
S_hl =  share(u_hl,u_hh,N)

S_lh =  share(u_lh,u_ll,N)
S_ll =  share(u_ll,u_lh,N)

dS_hh =  share_der_high(S_hh)
dS_hl =  share_der_high(S_hl)

dS_lh =  share_der_low(S_lh)
dS_ll =  share_der_low(S_ll)

dS_hx =  share_der_cross_high(S_hh,S_hl)
dS_lx =  share_der_cross_low(S_lh,S_ll)

S_l = mu_h*S_hl + mu_l*S_ll
dS_l = mu_h*dS_hl + mu_l*dS_ll
S_h = mu_h*S_hh + mu_l*S_lh
dS_h = mu_h*dS_hh + mu_l*dS_lh

#Price Spread
p_h
p_l
p_h - p_l
p_h/x_h
p_l/x_l


# Sorting
dS_hh*dS_ll - dS_hl*dS_lh

#Average Cost of Marginal Consumer
# Low Type
(mu_h*dS_hl*c_h + mu_l*dS_ll*c_l)/dS_l
# High Type
(mu_h*dS_hh*c_h + mu_l*dS_lh*c_l)/dS_h

#Marginal Cost with recapture
# Low Type
(mu_l*dS_ll*c_l + mu_h*dS_hl*c_h)/dS_l + x_h/x_l*(mu_l*dS_lx*c_l + mu_h*dS_hx*c_h)/dS_l
# High Type
(mu_l*dS_lh*c_l + mu_h*dS_hh*c_h)/dS_h + x_l/x_h*(mu_l*dS_lx*c_l + mu_h*dS_hx*c_h)/dS_h


#Markups
-S_l/dS_l
-S_h/dS_h

# Preferences
u_hh 
u_hl 
u_lh 
u_ll 