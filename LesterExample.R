rm(list=ls())
# library(deSolve)
library(nleqslv)
library(ggplot2)
library(scales)
setwd("C:/Users/cxr5626/Dropbox/Research/Imperfect_Insurance_Competition/Writing/Images")
#### General Functions ####
## Utility and Cost (inverted utility) Functions

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

## Accident and No-accident Utility Values, given u_h and u_l
util_ah <- function(u_l,u_h){
  y = ((1-theta_h)*u_l - (1-theta_l)*u_h)/(theta_l - theta_h)
  return(y)
}

util_nh <- function(u_l,u_h){
  y = (theta_l*u_h - theta_h*u_l)/( theta_l - theta_h )
  return(y)
}

## Profit Functions, given u_h and u_l
prof_l <- function(u_l){
  y = wealth - theta_l*damage - cost(u_l)
  return(y)
}

prof_h <- function(u_l,u_h){
  y = wealth - theta_h*damage - theta_h*cost(util_ah(u_l,u_h)) - (1-theta_h)*(cost(util_nh(u_l,u_h)))
  return(y)
}

#### Linear Example ####
## Model Primatives
v_l = 1
c_l = 0.5

v_h = 2
c_h = 1.5

pi = .8

mu_l = .8
mu_h = 1-mu_l

## Adverse Selection Criteria
phi = 1 - (mu_h/mu_l)*((v_h-c_h)/(c_h-c_l))


## Equilibrium Solution for General Pi in (0,1)
# Cumulative Distribution over u_l
optPol_l = function(u){
  y = ((1-pi)/pi)*( (v_l-c_l)^phi * (v_l-u)^(-phi) - 1)
  return(y)
}
# Optimal u_h for any given u_l
optPol_h = function(u){
  y = (1/ (mu_h*(v_h-c_l)/(c_h-c_l)))*(mu_h*v_h + mu_l*v_l - mu_l*phi*u - mu_l*(v_l-c_l)^(1-phi)*(v_l-u)^phi)
  return(y)
}

optPol_l(c_l)
optPol_h(c_l)

# Perfect Competition Solution for U_l
(v_h*(c_h-c_l) + v_l*(v_h-c_h))/(v_h-c_l)

## Plot Results
utils = seq(c_l,v_l,len=100)

dist = pmin(optPol_l(utils),1)
U_h = optPol_h(utils)[dist<1]

plot(utils, dist,type="l")
plot(utils[dist<1], U_h,type="l")


#### Insurance Market ####
## Model Primitives
theta_h = .6
theta_l = .9
wealth = 10
damage = 9
mu_l = .7
mu_h = 1 - mu_l 


## Benchmark Utility Values
# No Insurance Utility
u_l_low = theta_l*utility(wealth-damage) + (1-theta_l)*utility(wealth)
u_h_low = theta_h*utility(wealth-damage) + (1-theta_h)*utility(wealth)

# Actuarially Fair Full Insurance
u_l_high = utility(theta_l*(wealth-damage) + (1-theta_l)*wealth)
u_h_high = utility(theta_h*(wealth-damage) + (1-theta_h)*wealth)


## Adverse Selection Criteria
phi <- function(u_l,u_h){
  u_ah = util_ah(u_l,u_h)
  u_nh = util_nh(u_l,u_h)
  c_unh = cost_der(u_nh)
  c_uah = cost_der(u_ah)
  
  y = 1 - (mu_h/mu_l) * (theta_h*(1-theta_h)/(theta_l - theta_h)) * (c_unh - c_uah)
  
  return(y)
}

## ODE to solve for the optimal high policy, given low policy
util_high_ode <- function(u_l,u_h,parms){
  u_ah = util_ah(u_l,u_h)
  u_nh = util_nh(u_l,u_h)
  pi_h = prof_h(u_l,u_h)
  pi_l = prof_l(u_l)
  c_ul = cost_der(u_l)
  c_unh = cost_der(u_nh)
  c_uah = cost_der(u_ah)
  
  y_prime = (pi_h/pi_l) * 
    ( c_ul*(theta_l-theta_h) - (mu_h/mu_l)*theta_h*(1-theta_h)*(c_unh - c_uah) ) / 
    ( theta_l*(1-theta_h)*c_unh - theta_h*(1-theta_l)*c_uah )
  
  return(list(y_prime))
}


y_ini = u_h_low
u_l_highbnd = u_l_high - 1e-3

UpperPolicy = ode(y = y_ini, func = util_high_ode,
             times = seq(u_l_low,u_l_highbnd,length=1000),
             parms=NA)



## Interpolate U_l function
u_l = UpperPolicy[,1]
u_h = UpperPolicy[,2]
der = util_high_ode(u_l,u_h)[[1]]

UpPol_Interp = splinefunH(x=u_l,y=u_h,m=der)

test = seq(u_l_low,u_l_high,length=400)

plot(test,UpPol_Interp(test),type="l")
plot(UpperPolicy[,1],UpperPolicy[,2],type="l")

# Perfect Competition Solution for U_h
c_l = u_l_low
c_h = u_h_low
v_l = wealth-theta_l*damage
v_h = wealth-theta_h*damage
(v_h*(c_h-c_l) + v_l*(v_h-c_h))/(v_h-c_l)

## Least Cost Separating Contract

LC_contract <- function(u_h,u_l){
  u_ah = util_ah(u_l,u_h)
  u_nh = util_nh(u_l,u_h)
  c_unh = cost_der(u_nh)
  c_uah = cost_der(u_ah)
  
  y = wealth - theta_h*damage - (theta_h*cost(u_ah) + (1-theta_h)*cost(u_nh))
  return(y)
}
# Perfect Competition U_H
nleqslv(u_h_low,LC_contract,u_l = u_l_high)$x

## ODE to solve for the optimal distribution function
F_low_ode <- function(u_l,Fdist,parms){
  pi = parms
  u_h = UpPol_Interp(u_l)
  u_ah = util_ah(u_l,u_h)
  u_nh = util_nh(u_l,u_h)
  pi_l = prof_l(u_l)
  c_ul = cost_der(u_l)
  c_unh = cost_der(u_nh)
  c_uah = cost_der(u_ah)
  
  y_prime = ( (1- pi + pi*Fdist)/(pi*pi_l) ) *
    ( c_ul - (mu_h/mu_l)*(theta_h*(1-theta_h)/(theta_l - theta_h))*(c_unh-c_uah) ) 
  
  return(list(y_prime))
}

## Find Market Power Related Strategy
pi_seq = seq(.01,.99,length=15)

y_ini = 0 

#for (pi in pi_seq){
pi = .5
OptimalDist = ode(y = y_ini, func = F_low_ode,
                  times = seq(u_l_low,u_l_highbnd,length=1000),
                  parms=pi)



plot(OptimalDist[,1],pmin(OptimalDist[,2],1),type="l")

## Test Optimal Menu Margin
u_l_seq = OptimalDist[,1]
u_h_seq = UpperPolicy[,2]
F_l_seq = OptimalDist[,2]
f_l_seq = F_low_ode(u_l_seq,F_l_seq,pi)[[1]]
U_prime_seq = util_high_ode(u_l_seq,u_h_seq,NA)[[1]]
f_h_seq = f_l_seq/U_prime_seq
  
marg = (pi*f_l_seq)/(1-pi+pi*F_l_seq)
print(quantile(marg,probs = c(.25,.5,.75)))

# Fdist_Interp = splinefunH(x=u_h_seq,y=F_l_seq,m=f_h_seq)
# fdist_Interp = splinefun(x=u_h_seq,y=f_l_seq)

Fdist_Interp = splinefunH(x=u_l_seq,y=F_l_seq,m=f_l_seq)
fdist_Interp = splinefun(x=u_l_seq,y=f_l_seq)

menuPol <- function(u_h,u_l){
  u_ah = util_ah(u_l,u_h)
  u_nh = util_nh(u_l,u_h)  
  pi_l = prof_l(u_l)
  pi_h = prof_h(u_l,u_h)
  c_ul = cost_der(u_l)
  c_unh = cost_der(u_nh)
  c_uah = cost_der(u_ah)
  
  #y1 = (1-P_uh)*pi_h*(theta_l-theta_h) - (theta_l*(1-theta_h)*c_unh) + (theta_h*(1-theta_l)*c_uah)
  y1 = pi_h*pi*fdist_Interp(u_h)/(1-pi+pi*Fdist_Interp(u_h))*(theta_l-theta_h) - (theta_l*(1-theta_h)*c_unh) + (theta_h*(1-theta_l)*c_uah)
  
  return(y1)
}
#nleqslv(u_h_high,menuPol,control = list(stepmax=1e-3,maxit=4000),u_l=u_l_low)$x


#plot(u_l_seq,marg)



#### Logit Example ####
## Model Primitives

share <- function(u,out,N){
  y = exp(u-out)/(1 + N*exp(u-out))
  return(y)
}

share_der <- function(u,out,N){
  y = share(u,out,N)
  y_prime = y*(1-y)
  return(y_prime)
}

## Adverse Selection Criteria
phi <- function(u_l,u_h){
  u_ah = util_ah(u_l,u_h)
  u_nh = util_nh(u_l,u_h)
  c_unh = cost_der(u_nh)
  c_uah = cost_der(u_ah)
  
  y = 1 - (mu_h/mu_l) * (theta_h*(1-theta_h)/(theta_l - theta_h)) * (c_unh - c_uah)
  
  return(y)
}

psi <- function(u_l,u_h,N){
  u_ah = util_ah(u_l,u_h)
  u_nh = util_nh(u_l,u_h)
  c_unh = cost_der(u_nh)
  c_uah = cost_der(u_ah)
  P_ul = share(u_l,u_l_low,N)
  P_uh = share(u_h,u_h_low,N)
  
  y = (mu_h*P_uh)/(mu_l*P_ul) * (theta_h*(1-theta_h)/(theta_l - theta_h)) * (c_unh - c_uah)
  
  return(y)
}

psi_2 <- function(u_l,u_h){
  u_ah = util_ah(u_l,u_h)
  u_nh = util_nh(u_l,u_h)
  c_unh = cost_der(u_nh)
  c_uah = cost_der(u_ah)
  
  y = (theta_h*(1-theta_h)/(theta_l - theta_h)) * (c_unh - c_uah)
  
  return(y)
}

loss <- function(u_l,u_h){
  u_ah = util_ah(u_l,u_h)
  u_nh = util_nh(u_l,u_h)
  
  y = -cost(u_h) + (theta_h*cost(u_ah) + (1-theta_h)*cost(u_nh))
  return(y)
}

findRoots <- function(u,N){
  u_l = u[1]
  u_h = u[2]
  u_ah = util_ah(u_l,u_h)
  u_nh = util_nh(u_l,u_h)  
  pi_l = prof_l(u_l)
  pi_h = prof_h(u_l,u_h)
  pi_h = prof_h(u_l,u_h)
  c_ul = cost_der(u_l)
  c_unh = cost_der(u_nh)
  c_uah = cost_der(u_ah)
  P_ul = share(u_l,u_l_low,N)
  P_uh = share(u_h,u_h_low,N)
  Pder_ul = share_der(u_l,u_l_low,N)
  Pder_uh = share_der(u_h,u_h_low,N)
  
  y1 = (Pder_ul/P_ul)*pi_l - c_ul + (mu_h/mu_l) * (P_uh/P_ul) * (theta_h*(1-theta_h)/(theta_l-theta_h)) *(c_unh - c_uah)
  y2 = (Pder_uh/P_uh)*pi_h*(theta_l-theta_h) - (theta_l*(1-theta_h)*c_unh) + (theta_h*(1-theta_l)*c_uah)
  
  return(c(y1,y2))
}

## Find Equilibria
N = 1
theta_h = .6
theta_l = .9
wealth = 5
damage = 4
mu_l = 0.5
mu_h = 1-mu_l

# No Insurance Utility
u_l_low = theta_l*utility(wealth-damage) + (1-theta_l)*utility(wealth)
u_h_low = theta_h*utility(wealth-damage) + (1-theta_h)*utility(wealth)

# Actuarially Fair Full Insurance
u_l_high = utility(theta_l*(wealth-damage) + (1-theta_l)*wealth)
u_h_high = utility(theta_h*(wealth-damage) + (1-theta_h)*wealth)


menuPol <- function(u_h,u_l,N){
  u_ah = util_ah(u_l,u_h)
  u_nh = util_nh(u_l,u_h)  
  pi_l = prof_l(u_l)
  pi_h = prof_h(u_l,u_h)
  c_ul = cost_der(u_l)
  c_unh = cost_der(u_nh)
  c_uah = cost_der(u_ah)
  P_ul = share(u_l,u_l_low,N)
  P_uh = share(u_h,u_h_low,N)
  Pder_ul = share_der(u_l,u_l_low,N)
  Pder_uh = share_der(u_h,u_h_low,N)
  
  y1 = (1-P_uh)*pi_h*(theta_l-theta_h) - (theta_l*(1-theta_h)*c_unh) + (theta_h*(1-theta_l)*c_uah)
  #y1 = 1*pi_h*(theta_l-theta_h) - (theta_l*(1-theta_h)*c_unh) + (theta_h*(1-theta_l)*c_uah)
  return(y1)
}

u_l_low
u_h_low
findRoots(c(u_l_low,u_h_low),N=N)

res =nleqslv(c(u_l_low,u_h_low),findRoots,control = list(stepmax=1e-1,maxit=4000),N=N)
print(res$message)
print(res$x)

nleqslv(u_l_low,menuPol,control = list(stepmax=1e-3,maxit=4000),u_l=u_l_low,N=N)$x


# Run through a vector of u_l
u_l_highbnd = u_l_high - 1e-3
u_l_seq = seq(-1.5,u_l_highbnd,length=100)
u_h_seq = NULL
mes = NULL
for (u_l in u_l_seq){
  res = nleqslv(u_l,menuPol,control = list(maxit=4000),u_l=u_l,N=N)
  u_h = max(res$x,u_l)
  u_h_seq = c(u_h_seq,u_h)
  mes = c(mes,res$message)
}

plot(u_l_seq,u_h_seq)


equilibria <- function(u_l,u_h,N){
  u_ah = util_ah(u_l,u_h)
  u_nh = util_nh(u_l,u_h)  
  pi_l = prof_l(u_l)
  pi_h = prof_h(u_l,u_h)
  pi_h = prof_h(u_l,u_h)
  c_ul = cost_der(u_l)
  c_unh = cost_der(u_nh)
  c_uah = cost_der(u_ah)
  P_ul = share(u_l,u_l_low,N)
  P_uh = share(u_h,u_h_low,N)
  Pder_ul = share_der(u_l,u_l_low,N)
  Pder_uh = share_der(u_h,u_h_low,N)
  
  y1 = (Pder_ul/P_ul)*pi_l
  y2 = c_ul - (mu_l/mu_h) * (P_ul/P_uh) * (theta_h*(1-theta_l)/(theta_l-theta_h)) *(c_unh - c_uah)
  return(list(y1,y2))
}

u_ah = util_ah(u_l_seq,u_h_seq)
u_nh = util_nh(u_l_seq,u_h_seq)

c_unh = cost_der(u_nh)
c_uah = cost_der(u_ah)

P_ul = share(u_l_seq,u_l_low,N=2)
P_uh = share(u_h_seq,u_h_low,N=2)
pi_l = prof_l(u_l_seq)

t1 = u_nh-u_ah
t2 = (c_unh-c_uah)*theta_h*(1-theta_h)/(theta_l-theta_h) * (mu_h*P_uh)/(mu_l*P_ul) + cost_der(u_l)
t3 = equilibria(u_l_seq,u_h_seq,2)[[1]]
t4 = equilibria(u_l_seq,u_h_seq,2)[[2]]
t5 = psi(u_l_seq,u_h_seq,2)
t6 = cost_der(u_l_seq)-t5

## Comparative Statics

N = 1
theta_h = .6
theta_l = .9
wealth = 5
damage = 4
mu_l = 0.7       

psi_list = NULL
u_l_list = NULL
u_h_list = NULL
u_diff_list = NULL
pi_l_list = NULL
pi_h_list = NULL
loss_list = NULL

N_vec = seq(1,25,1)
theta_h_vec = seq(.1,.6,.05)
theta_l_vec = seq(.65,.95,.05)
mu_l_vec = seq(.05,.95,.05)
d_vec = seq(.5,4.9,length=10)


for (N in N_vec){
  mu_h = 1 - mu_l
  res =nleqslv(c(u_l_low,u_h_low),findRoots,control = list(stepmax=1e-3,maxit=4000),N=N)
  print(res$message)
  u_l = res$x[1]
  u_h = res$x[2]
  u_nh = util_nh(u_l,u_h)
  u_ah = util_ah(u_l,u_h)
  s_l = share(u_l,u_l_low,N)
  s_h = share(u_h,u_h_low,N)
  
  sel = psi_2(u_l,u_h)
  Pi_l = prof_l(u_l)
  Pi_h = prof_h(u_l,u_h)
  u_diff = u_nh - u_ah
  
  psi_list = c(psi_list,sel)
  u_l_list = c(u_l_list,u_l)
  u_h_list = c(u_h_list,u_h)
  u_diff_list = c(u_diff_list,u_h-u_l)
  pi_l_list = c(pi_l_list,Pi_l)
  pi_h_list = c(pi_h_list,Pi_h)
  loss_list = c(loss_list,loss(u_l,u_h))
}
x_vec = N_vec
plot(x_vec,psi_list)
plot(x_vec,u_l_list)
plot(x_vec,u_h_list)
plot(x_vec,u_diff_list)
plot(x_vec,pi_l_list)
plot(x_vec,pi_h_list)
plot(x_vec,loss_list)





#### Presentation Plots ####

## Plot Optimal Menus
theta_h = .6
theta_l = .9
wealth = 5
damage = 4
mu_l = 0.3

menuPlot = NULL
u_l_seq = seq(-1.5,-.4,length=100)

for (u_l in u_l_seq){
  res = nleqslv(u_l,menuPol,control = list(maxit=4000),u_l=u_l,N=1)
  temp = data.frame(u_l=u_l,u_h=max(res$x,u_l),N=1)
  menuPlot = rbind(menuPlot,temp)
  res = nleqslv(u_l,menuPol,control = list(maxit=4000),u_l=u_l,N=2)
  temp = data.frame(u_l=u_l,u_h=max(res$x,u_l),N=2)
  menuPlot = rbind(menuPlot,temp)
  res = nleqslv(u_l,menuPol,control = list(maxit=4000),u_l=u_l,N=5)
  temp = data.frame(u_l=u_l,u_h=max(res$x,u_l),N=25)
  menuPlot = rbind(menuPlot,temp)
  res = nleqslv(u_l,menuPol,control = list(maxit=4000),u_l=u_l,N=100)
  temp = data.frame(u_l=u_l,u_h=max(res$x,u_l),N=100)
  menuPlot = rbind(menuPlot,temp)
}
menuPlot$N = as.factor(menuPlot$N)

#png("MenuPol.png",width=2000,height=1500,res=275)
ggplot(menuPlot) + aes(x=u_l,y=u_h,color=N) + geom_line(size=1.1) + 
  ylab(expression(u[h])) + 
  xlab(expression(u[l])) + 
  ggtitle("Optimal Contract Menus") +
  theme(panel.background = element_rect(color=grey(.2),fill="transparent"),
        strip.text = element_blank(),
        panel.grid.major = element_line(color=grey(.8)),
        legend.background = element_rect(color=grey(.5)),
        legend.title=element_blank(),
        legend.text = element_text(size=18),
        legend.key.width = unit(.075,units="npc"),
        legend.key = element_rect(color="transparent",fill="transparent"),
        #legend.position = "none",
        axis.title=element_text(size=14),
        axis.text = element_text(size=12))
#dev.off()


## Plot Competition Statics
theta_h = .6
theta_l = .9
wealth = 5
damage = 4
mu_l = 0.3       


N_vec = seq(1,25,1)
mu_l_vec = seq(.05,.95,.05)

compPlot = NULL


for (N in N_vec){
  mu_h = 1 - mu_l
  res =nleqslv(c(u_l_low,u_h_low),findRoots,control = list(stepmax=1e-3,maxit=4000),N=N)
  print(res$message)
  u_l = res$x[1]
  u_h = res$x[2]
  u_nh = util_nh(u_l,u_h)
  u_ah = util_ah(u_l,u_h)
  s_l = share(u_l,u_l_low,N)
  s_h = share(u_h,u_h_low,N)
  
  sel = psi_2(u_l,u_h)
  Pi_l = prof_l(u_l)
  Pi_h = prof_h(u_l,u_h)
  u_diff = u_nh - u_ah
  eff = loss(u_l,u_h)
  
  markup = (1/(1-s_l))*(cost_der(u_l) - (mu_h*s_h)/(mu_l*s_l)*sel)
  
  temp = data.frame(N=N,psi=sel,u4_l=u_l,u3_h=u_h,
                    u1_ah=u_ah,u2_nh=u_nh,
                    u_diff = u_h-u_l,
                    u_h_diff = u_nh-u_ah,
                    Pi_l=Pi_l,Pi_h=Pi_h,loss=eff,
                    markup = markup,s_l=s_l*N,s_h=s_h*N)
  compPlot = rbind(compPlot,temp)
  rm(temp)
}

compPlot = reshape(compPlot,varying=names(compPlot)[-1],
                   v.names = "value",timevar="stat",
                   times = names(compPlot)[-1],
                   idvar = "N",
                   direction="long")

#png("Utility.png",width=2000,height=1500,res=275)
ggplot(compPlot[compPlot$stat%in%c("u4_l","u3_h","u1_ah","u2_nh"),]) + 
  aes(x=N,y=value,color=stat) + geom_line(size=1.3)  + 
  ylab("") + 
  xlab("Number of Competitors") + 
  scale_color_discrete(labels=c(expression(u[h]^a),expression(u[h]^n),expression(u[h]),
                                expression(u[l]))) + 
  #ggtitle("Comparative Statics wrt Competitors") +
  theme(panel.background = element_rect(color=grey(.2),fill="transparent"),
        strip.text = element_blank(),
        panel.grid.major = element_line(color=grey(.8)),
        legend.background = element_rect(color=grey(.5)),
        legend.title=element_blank(),
        legend.text = element_text(size=18),
        legend.key.width = unit(.075,units="npc"),
        legend.key.height = unit(.12,units="npc"),
        legend.key = element_rect(color="transparent",fill="transparent"),
        #legend.position = "none",
        axis.title=element_text(size=14),
        axis.text = element_text(size=12))
#dev.off()

#png("Insured.png",width=2000,height=1500,res=275)
ggplot(compPlot[compPlot$stat%in%c("s_l","s_h"),]) + 
  aes(x=N,y=value,color=stat) + geom_line(size=1.1)  + 
  ylab("Insured Rate") + 
  xlab("Number of Competitors") + 
  scale_color_discrete(labels=c("Low Type","High Type")) + 
  ggtitle("Comparative Statics wrt Competitors") +
  theme(panel.background = element_rect(color=grey(.2),fill="transparent"),
        strip.text = element_blank(),
        panel.grid.major = element_line(color=grey(.8)),
        legend.background = element_rect(color=grey(.5)),
        legend.title=element_blank(),
        legend.text = element_text(size=18),
        legend.key.width = unit(.075,units="npc"),
        legend.key = element_rect(color="transparent",fill="transparent"),
        #legend.position = "none",
        axis.title=element_text(size=14),
        axis.text = element_text(size=12))
#dev.off()

#png("Markup.png",width=2000,height=1500,res=275)
ggplot(compPlot[compPlot$stat%in%c("psi","markup"),]) + 
  aes(x=N,y=value,color=stat) + geom_line(size=1.3) + 
  ylab("") + 
  xlab("Number of Competitors") + 
  #ggtitle("Comparative Statics wrt Competitors") + 
  scale_color_discrete(labels=c("Markup",expression(psi))) + 
  theme(panel.background = element_rect(color=grey(.2),fill="transparent"),
        strip.text = element_blank(),
        panel.grid.major = element_line(color=grey(.8)),
        legend.background = element_rect(color=grey(.5)),
        legend.title=element_blank(),
        legend.text = element_text(size=18),
        legend.key.width = unit(.075,units="npc"),
        legend.key = element_rect(color="transparent",fill="transparent"),
        #legend.position = "none",
        axis.title=element_text(size=14),
        axis.text = element_text(size=12))
#dev.off()

#png("Loss1.png",width=2000,height=1500,res=275)
ggplot(compPlot[compPlot$stat%in%c("loss"),]) + 
  aes(x=N,y=value,color=stat) + geom_line(size=1.3) + 
  ylab("Loss") + 
  xlab("Number of Competitors") + 
  ggtitle("Resource Loss from Adverse Selection") +
  
  theme(panel.background = element_rect(color=grey(.2),fill="transparent"),
        strip.text = element_blank(),
        panel.grid.major = element_line(color=grey(.8)),
        legend.background = element_rect(color=grey(.5)),
        legend.title=element_blank(),
        legend.text = element_text(size=18),
        legend.key.width = unit(.075,units="npc"),
        legend.key = element_rect(color="transparent",fill="transparent"),
        legend.position = "none",
        axis.title=element_text(size=14),
        axis.text = element_text(size=12))
#dev.off()

loss = compPlot$value[compPlot$stat%in%c("loss")]
u_ah = compPlot$value[compPlot$stat%in%c("u1_ah")&compPlot$N==2]
u_nh = compPlot$value[compPlot$stat%in%c("u2_nh")&compPlot$N==2]
u_l = compPlot$value[compPlot$stat%in%c("u4_l")&compPlot$N==2]
s_h = compPlot$value[compPlot$stat%in%c("s_h")&compPlot$N==2]
s_l = compPlot$value[compPlot$stat%in%c("s_l")&compPlot$N==2]
insured = (s_l+s_h)/2
totCost = (mu_l*cost(u_l) + mu_h*(cost(u_ah)*theta_h + cost(u_nh)*(1-theta_h)))*insured
conv = 1.3e12/totCost


ins_vec = (compPlot$value[compPlot$stat%in%c("s_h")] + 
  compPlot$value[compPlot$stat%in%c("s_l")])/2
u_h_vec = compPlot$value[compPlot$stat%in%c("u3_h")]
u_nh_vec = compPlot$value[compPlot$stat%in%c("u2_nh")]
u_l_vec = compPlot$value[compPlot$stat%in%c("u4_l")]
totCost = (mu_l*cost(u_l) + mu_h*cost(u_h))*insured

costDiff = mu_h*insured*(loss)*conv/1e9

#png("Loss2.png",width=2000,height=1500,res=275)
ggplot(compPlot[compPlot$stat%in%c("loss"),]) + 
  aes(x=N,y=costDiff,color=stat) + geom_line(size=1.3) + 
  ylab("Loss (Billions)") + 
  xlab("Number of Competitors") + 
  ggtitle("Resource Loss from Adverse Selection") +
  scale_y_continuous(labels=dollar) + 
  theme(panel.background = element_rect(color=grey(.2),fill="transparent"),
        strip.text = element_blank(),
        panel.grid.major = element_line(color=grey(.8)),
        legend.background = element_rect(color=grey(.5)),
        legend.title=element_blank(),
        legend.text = element_text(size=18),
        legend.key.width = unit(.075,units="npc"),
        legend.key = element_rect(color="transparent",fill="transparent"),
        legend.position = "none",
        axis.title=element_text(size=14),
        axis.text = element_text(size=12))
#dev.off()

p_l = wealth - cost(u_l_vec)

