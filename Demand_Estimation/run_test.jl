using BenchmarkTools
using JLD2
using CSV
using Random
using Dates
using LinearAlgebra
using Statistics


# Data Structure
include("InsChoiceData.jl")

#Halton Draws
include("Halton.jl")

# Random Coefficients MLE
include("RandomCoefficients.jl")
# include("RandomCoefficients_2der_nonzero.jl")
include("RandomCoefficients_der.jl")
include("Contraction.jl")
include("Log_Likehood.jl")
include("RiskMoments.jl")
include("Estimate_Basic.jl")
include("Estimate_GMM.jl")
include("Estimate_TwoStage.jl")
include("GMM_Var.jl")
include("utility.jl")
include("DerivFunctions.jl")
include("Log_Likehood_Penalty.jl")
println("Code Loaded")

# Load the Data
include("load.jl")
spec_demoRaw = [:AgeFE_31_39,
          :AgeFE_40_51,
          :AgeFE_52_64,
          :Family,
          :LowIncome]
          spec_prodchars=[:Price,:constant,:AV]
          spec_prodchars_σ=[:AV, :AETNA_GA,:AETNA_IL,:AETNA_MI,:AETNA_LIFE_INSURANCE_COMPANY_TX,
          :ALTIUS_HEALTH_PLANS_UT,:AMBETTER_FROM_PEACH_STATE_HEALTH_PLAN_GA,:AMBETTER_FROM_SUPERIOR_HEALTHPLAN_TX,
          :AMBETTER_INSURED_BY_CELTIC_IL                    ,
          :ANTHEM_BLUE_CROSS_AND_BLUE_SHIELD_MO             ,
          :ARCHES_HEALTH_PLAN_UT                         ,
          :ASSURANT_HEALTH_GA,
          :ASSURANT_HEALTH_IL,
          :ASSURANT_HEALTH_MI,
          :ASSURANT_HEALTH_NE,
          :ASSURANT_HEALTH_TX ,
          :ASSURANT_HEALTH_OK,
          :AVERA_HEALTH_PLANS_IA,
          :BLUE_CROSS_AND_BLUE_SHIELD_OF_ILLINOIS_IL,
          :BLUE_CROSS_AND_BLUE_SHIELD_OF_KANSAS_CITY_MO,
          :BLUE_CROSS_AND_BLUE_SHIELD_OF_NEW_MEXICO_NM,
          :BLUE_CROSS_AND_BLUE_SHIELD_OF_OKLAHOMA_OK,
          :BLUE_CROSS_BLUE_SHIELD_OF_GEORGIA_GA,
          :BLUE_CROSS_AND_BLUE_SHIELD_OF_TEXAS_TX         ,
          :BLUE_CROSS_BLUE_SHIELD_OF_MICHIGAN_MI,
          :BLUE_CROSS_BLUE_SHIELD_OF_NORTH_DAKOTA_ND,
          :BLUECROSS_BLUESHIELD_OF_NEBRASKA_NE,
          :BRIDGESPAN_OR,
          :BRIDGESPAN_UT ,
          :CAREFIRST_BLUECROSS_BLUESHIELD_MD,
          :CIGNA_HEALTH_AND_LIFE_INSURANCE_COMPANY_GA,
          :CIGNA_HEALTH_AND_LIFE_INSURANCE_COMPANY_MO ,
          :CIGNA_HEALTH_AND_LIFE_INSURANCE_COMPANY_TX,
          :COVENTRY_IL,
          :COVENTRY_MO ,
          :COVENTRY_HEALTH_CARE_OF_IOWA_INC_IA,
          :COVENTRY_HEALTH_CARE_OF_NEBRASKA_INC_NE          ,
          :FIRSTCARE_TX                                   ,
          :HEALTH_ALLIANCE_PLAN_MI,
          :HEALTH_NET_OF_OREGON_OR,
          :HEALTH_REPUBLIC_INSURANCE_OR                   ,
          :HEALTHPLUS_MI                                   ,
          :HUMANA_GA                                        ,
          :HUMANA_IL                                      ,
          :HUMANA_MI                                       ,
          :HUMANA_MO                                        ,
          :HUMANA_OK                                      ,
          :HUMANA_TX                                       ,
          :HUMANA_UT                                        ,
          :KAISER_FOUNDATION_HEALTH_PLAN_OF_THE_NW_OR     ,
          :KAISER_MIDATLANTIC_MD                           ,
          :KAISER_PERMANENTE_GA_GA                          ,
          :LAND_OF_LINCOLN_HEALTH_IL                      ,
          :LIFEWISE_HEALTH_PLAN_OF_OREGON_OR               ,
          :MEDICA_ND                                        ,
          :MODA_HEALTH_PLAN_INC_AK                        ,
          :MODA_HEALTH_PLAN_INC_OR                         ,
          :MOLINA_HEALTH_CARE_MI                            ,
          :MOLINA_HEALTH_CARE_NM                          ,
          :MOLINA_HEALTH_CARE_TX                           ,
          :MOLINA_HEALTH_CARE_UT                            ,
          :MY_HEALTH_ALLIANCE_IL                          ,
          :NEW_MEXICO_HEALTH_CONNECTIONS_NM                ,
          :PACIFICSOURCE_HEALTH_PLANS_OR                    ,
          :PREMERA_BLUE_CROSS_BLUE_SHIELD_OF_ALASKA_AK    ,
          :PRESBYTERIAN_NM                                 ,
          :PRIORITY_HEALTH_MI                               ,
          :PROVIDENCE_HEALTH_PLAN_OR                      ,
          :REGENCE_BLUECROSS_BLUESHIELD_OF_OREGON_OR       ,
          :REGENCE_BLUECROSS_BLUESHIELD_OF_UTAH_UT          ,
          :SCOTT__WHITE_HEALTH_PLAN_TX                    ,
          :SELECTHEALTH_UT                                 ,
          :UNITEDHEALTHCARE_COMMUNITY_PLAN_INC_MI           ,
          :UNITEDHEALTHCARE_LIFE_INS_CO_GA                ,
          :UNITEDHEALTHCARE_LIFE_INS_CO_IL                 ,
          :UNITEDHEALTHCARE_LIFE_INS_CO_MI                  ,
          :UNITEDHEALTHCARE_LIFE_INS_CO_MO                ,
          :UNITEDHEALTHCARE_LIFE_INS_CO_NE                 ,
          :UNITEDHEALTHCARE_LIFE_INS_CO_OK                  ,
          :UNITEDHEALTHCARE_LIFE_INS_CO_TX                ,
          :UNITEDHEALTHCARE_LIFE_INS_CO_UT                 ,
          :UNITEDHEALTHCARE_OF_GEORGIA_INC_GA               ,
          :UNITEDHEALTHCARE_OF_THE_MIDWEST_INC_IL         ,
          :WELLMARK_BLUE_CROSS_AND_BLUE_SHIELD_OF_IOWA_IA ]
#Structure the data
c = ChoiceData(df,df_mkt,df_risk,df_transfer;
    demoRaw=spec_demoRaw,
    prodchars=spec_prodchars,
    prodchars_σ=spec_prodchars_σ,
    fixedEffects=[:Firm_ST])


param_labels = vcat(String.(spec_demoRaw),String.(spec_prodchars),"Price:" .* String.(spec_demoRaw),"Variance:".*String.(spec_prodchars_σ),c.feNames)

# Fit into model
m = InsuranceLogit(c,5)
println("Data Loaded")


γstart = rand(m.parLength[:γ])/10 .-.05
β0start = rand(m.parLength[:β])/10 .-.05
βstart = rand(m.parLength[:γ])/10 .- .05
σstart = rand(m.parLength[:σ])/10 .- .05

FEstart = rand(m.parLength[:FE])/100 .-.005

p0 = vcat(γstart,β0start,βstart,σstart,FEstart)


W = -Matrix(1.0I,length(m.data.rMoments),length(m.data.rMoments))

# p_est, fval = newton_raphson_ll(m,p0,W)

p_est = [-0.23735856593897112, -0.26235294506000145, 0.27865608916875867, -0.7759892482046534, -3.021459328293324, -1.4953324906403487, -3.448547835257781, 4.490988057601426, 0.15199558051633047, 0.4245723682210213, 0.6482658781701501, 0.09225866828888217, 0.07747486553036813, 1.0082816268894217, 0.4842742745107047, -0.7660784949673882, -0.3869774704415844, 0.08696269645191236, -1.0064726944264686, 0.5145463716954038, -0.16378338546664642, -0.6873372149470359, -0.010339869697761666, -0.9878784977771901, 0.4354494074634831, -1.1046807992426348, -0.5532324035983309, -0.5712312716394482, 0.06744485299701407, -1.2138644514552648, -0.3687822738292614, -0.9123118102860184, -0.01985245848216566, -0.3224631358819784, -1.2579980199609921, 0.47790596573510263, 0.05026895094940654, -0.5416583359738361, -0.36468639591860436, -0.7212818519712122, 2.609983556973882, -1.0111001414733503, 0.3303965341150728, 0.4873637261447699, -0.021447282459717103, -0.001348387452114065, -0.7816284220609939, -0.00422912503810505, 0.10076610662265957, -0.7186875455877056, 0.018492303407886903, -0.605714769753493, 2.1219227705187897, 2.4878816344370773, -0.4581523800502693, 0.3992719413035935, -0.8995834097185681, -0.5580757315368531, -0.04272839718063327, -1.4339545321807332, 0.014825553033153242, -1.0943730479774627, 2.5084341003599437, 0.33617611977380696, 0.4965645374983579, -0.823248742260295, 2.51625809637857, -0.3513565419819971, -0.13556569002567187, 2.5057541459645924, -0.5765583520013201, -0.30766235631408034, 0.008335287863212169, -1.0640657958839543, -0.8491284560378185, -0.27390494088157186, 2.4482011098943057, -0.16935045130651177, -0.3227368521445366, -0.46910035969451735, 2.5056608125039, 2.535681305402646, -0.9930499059132026, 0.057994616866711396, -1.4407416369908772, -0.5570664507945945, 0.38960271252449585, -0.8521208430581914, -0.6208322053028519, -0.09657862884942266, -0.8361857623783878, -1.5450250561327346, -0.03156367193043494, -1.5144335942659604, 0.3419823631281345, -0.8409847946840872, 0.14580641707404585, 4.307970367248034, -0.10095487089426866, 0.30752551074546775, 1.5661567396114382, 0.2684701513780676, 0.986382999578873, -0.5751605907542536, -0.1662776148190872, 0.6040813606876311, 2.1679871501628543, 1.982636534077111, -1.0926760613086721, 2.8615535055080565, 0.0193499283498273, 3.5232596408060886, -0.834265493318413, -1.7451170582653206, -19.784103760549982, 3.804374944317248, 3.5510706029430716, 2.888224527139632, 1.8389402737718135, 3.3393946976810396, 2.601948663242917, 0.9858614495375285, 3.8433501564061645, 4.232415985620227, 0.2296314608759205, -1.0247779590348016, 1.6663224513884807, -0.6605501776290625, 1.1723567609721062, -0.419879010217036, 0.3503773106841584, 3.486021635831349, 1.66985262040625, 2.2333799455654413, -0.8362609600904108, 2.854253891028665, 1.2248440336723234, -16.31668053148698, -3.7646858458875077, 1.10826946678974, 3.1473057240771314, 0.691988599186, 0.6818122166551496, 1.056627199022281, 0.6074588007304461, 0.8346515389634446, 1.9141460539467285, -0.5006494335915083, -0.6796267140744681, 1.596168245214051, 0.12242514969243341, 2.9903470096421123, 3.0653396954258745, 0.9935832901953334, -0.4225679833716027, 1.8352640071031376, -3.284005142938159, -0.031807603752733254, 2.395306278773659, 0.3415373716117633, -1.3290874371346446, 4.4746193793059925, 1.4490141250999624, 2.3007452180657935, 0.9383081363003556, -0.22663412005214598, 1.201172080648758, -0.23759360124327675, 3.1194908187743646, 0.2812205280592133, -1.2829559945057971, 1.0605324669244551, 0.12324559790801087, 0.8318302252585251, 2.7110348493878105, 1.2337440447573218, -0.759946081918603, -0.05873261529698317, -0.47912629364002046, -1.2401569847033873, 1.103934448574077]
# out1 = DataFrame(labels=param_labels,pars=p_est)
# file1 = "test.csv"
# CSV.write(file1,out1)


p0 = copy(p_est)

grad = Vector{Float64}(undef,length(p0))
grad_test = Vector{Float64}(undef,length(p0))
hess = Matrix{Float64}(undef,length(p0),length(p0))

ll = log_likelihood_penalty(m,p0,W)
println(ll)


ll = log_likelihood_penalty!(hess,grad,m,p0,W)
println(ll)
ll = log_likelihood_penalty!(grad_test,m,p0,W)
println(ll)


f_obj(x) = log_likelihood_penalty(m,x,W)


grad_1 = Vector{Float64}(undef,length(p0))
hess_1 = Matrix{Float64}(undef,length(p0),length(p0))
fval = f_obj(p0)
println(fval-ll)

println("Grad")
ForwardDiff.gradient!(grad_1,f_obj, p0)#, cfg)
println(maximum(abs.(grad_1-grad)))
#
println("Hessian")
cfg = ForwardDiff.HessianConfig(f_obj, p0, ForwardDiff.Chunk{8}())
ForwardDiff.hessian!(hess_1,f_obj, p0)#,cfg)
println(maximum(abs.(hess_1-hess)))



d = m
p = parDict(d,p_est,no2Der=false)

grad = Vector{Float64}(undef,length(p0))
hess = Matrix{Float64}(undef,length(p0),length(p0))
ll = log_likelihood!(hess,grad,m,p)

mom_grad = Matrix{Float64}(undef,length(p0),length(d.data.rMoments))
mom_hess = Array{Float64,3}(undef,length(p0),length(p0),length(d.data.rMoments))

grad = Vector{Float64}(undef,length(p0))
hess = Matrix{Float64}(undef,length(p0),length(p0))

mom = calc_risk_moments!(mom_grad,d,p)
mom = calc_risk_moments!(mom_hess,mom_grad,d,p)

obj = calc_GMM_Obj(mom,W)

grad[:].=0.0
hess[:].=0.0
calc_GMM_Grad!(grad,mom,mom_grad,W)
calc_GMM_Hess!(hess,mom,mom_grad,mom_hess,W)

# app = iterate(eachperson(d.data),11)[1]
# grad[:].=0.0
# hess[:].=0.0
# ll, ind = ll_obs_hessian!(hess,grad,app,d,p)
#
#
#
# f_obj_ll(x) = log_likelihood(m,x)
# grad_ll = Vector{Float64}(undef,length(p0))
# hess_ll = Matrix{Float64}(undef,length(p0),length(p0))
# println(f_obj_ll(p0))
# println("Hessian")
# cfg = ForwardDiff.HessianConfig(f_obj_ll, p0, ForwardDiff.Chunk{8}())
# ForwardDiff.hessian!(hess_ll,f_obj_ll, p0)#,cfg)


f_obj_p(x) = calc_penalty(m,x,W)
grad_pen = Vector{Float64}(undef,length(p0))
hess_pen = Matrix{Float64}(undef,length(p0),length(p0))
println(f_obj_p(p0))
println("Grad")
ForwardDiff.gradient!(grad_pen,f_obj_p, p0)#, cfg)



println("Hessian")
cfg = ForwardDiff.HessianConfig(f_obj_p, p0, ForwardDiff.Chunk{8}())
ForwardDiff.hessian!(hess_pen,f_obj_p, p0)#,cfg)
