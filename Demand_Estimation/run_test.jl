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
c = ChoiceData(df,df_mkt,df_risk;
    demoRaw=spec_demoRaw,
    prodchars=spec_prodchars,
    prodchars_σ=spec_prodchars_σ,
    fixedEffects=[:Firm_ST])


param_labels = vcat(String.(spec_demoRaw),String.(spec_prodchars),"Price:" .* String.(spec_demoRaw),"Variance:".*String.(spec_prodchars_σ),c.feNames)

# Fit into model
m = InsuranceLogit(c,5)
println("Data Loaded")


γstart = rand(m.parLength[:γ])/10 .-.05
β0start = rand(m.parLength[:γ])/10 .-.05
βstart = rand(m.parLength[:γ])/10 .- .05
σstart = rand(m.parLength[:σ])/10 .- .05

FEstart = rand(m.parLength[:FE])/100 .-.005

# p0 = vcat(γstart,β0start,βstart,σstart,FEstart)

p0 = [-0.07968388441613143, -0.07922715333906746, 0.2709581913767964, -0.3051432430527276, -3.214264547397332, -1.4561566596675222, -2.961040834238792, 6.052050281380145, 0.07841795257732122, 0.18039958396153633, 0.45619017393715383, -0.011320934823024879, -0.07211987944631137, 1.2680396020640592, 8.98317680058124, 11.681508935251655, 4.181833467357386, 0.1232204288960171, 7.537658397068719, 9.017726093439121, -0.2223488580874797, 11.63109178375033, 0.01499443543441492, 7.526883544756579, 8.944897531229252, 11.538747126796968, 4.053564501275695, 0.8344962219518471, 0.11888965797281471, 0.26579918707284433, 10.151133573595375, 11.503742791034238, 0.026748606397254456, 4.861280577254011, 0.15412474528703854, 8.991450974022628, 0.10124515452410528, 4.048452309431477, 4.231085468203154, 0.6892408387886167, 11.451563676362927, 7.540351649129138, 13.734594185066252, 8.99770188566677, -0.0006292695080549606, 0.03875540641915943, 11.576992548677067, 0.00920529894429844, 38.34415235316831, 0.6612752847311028, 0.042254320139284395, 3.9534243523020938, 10.416918947293775, 11.327817419444848, 4.0525459551573135, 8.8813561632287, 11.575690557422494, 3.9647487404340938, -0.03727582694110743, 0.2004956161630465, 0.03422682688645974, 7.424020013753604, 11.32035146411921, 13.721032377900448, 9.004003394584396, 11.522649448952198, 11.370450325042858, 4.230875440176362, 8.847286604308664, 11.360523608781884, 3.9882623648528273, 4.86672237385003, 0.02852191887947276, 7.503629290440451, 11.54938512719601, 4.903691976246184, 11.296861067505311, 8.847660339860825, 4.864582589461908, 4.104728827560091, 11.351170631867758, 11.399221535877208, 7.53904481739924, 0.08038466808408981, 7.471906752113422, 3.9606706193106493, 8.865513028867019, 11.528690081304815, 3.9254694166302295, -0.10274755544369246, 0.5313896701891168, 0.05446036576999139, -0.015627086297263875, 7.409103370262027, 8.803998576297081, 11.528084827748588, 38.398658584339735, -2.3350355691897486, -0.6436808839911029, -1.43726331350988, 0.4383529071941702, -0.40236690773374795, -1.2142993798156465, -0.5023285484718262, -0.9419765627203017, -2.8790600982866756, 0.8286290736729797, -0.32639531284058965, -1.5295366029253052, 2.27968142436804, -0.6205817560868219, 0.16359065901338546, -1.2782688496983248, -1.8097825091906712, -1.3175126032840752, 2.370926566975309, 1.737377444646701, 1.932049320776296, 2.535257336021764, 1.8583967803994341, 2.272884353091742, 0.9390126780316808, 2.6728417092080923, 1.9864352528198872, -3.4728820211776736, -1.3319225390492306, 0.3483238779750128, -0.7974479244753795, -0.1572683958436869, -0.7978705509023797, 1.8548168304211667, 2.3825973612756415, 0.28859755880924903, 1.0392536878631977, -1.454914055540065, 0.18840318557646613, -0.008359026311144447, -1.4978222427214305, -2.7349266591896066, 0.608576668791257, -1.1675401864614345, -0.03241641885589922, -0.35694063639708723, -2.177788555162775, 0.11326767116482597, -0.6762346204791702, 0.0591222300653255, -2.0663455067657863, -0.6420720066616847, -0.2610821747610123, 0.7971819267695581, 0.9520671161210169, 2.23531825792418, 0.6219396663672788, -1.5549922673806165, 0.16547358625422187, -3.6429636317143204, -2.250344397089218, 1.1781146865198946, 1.3658735717542825, -0.8026836098938369, 3.3356264027683165, 1.4795251078263663, 1.0897026249953816, 0.9288246523806634, -0.34669577009381625, -0.2642932093863891, -1.2251924169792967, 0.9563391429009235, -0.649584701133676, -1.6711294652945134, -1.0282751596457802, -0.6157582508128676, -0.24572758562057176, 1.1234321851252245, -0.9708873870553038, -1.1770918372618226, -1.079173168533506, -1.2431820263803226, -2.1618732534152816, 1.202866106668412]


W = -Matrix(1.0I,length(m.data.tMoments),length(m.data.tMoments))

# p_est, fval = newton_raphson_ll(m,p0,W)
# out1 = DataFrame(labels=param_labels,pars=p_est)
# file1 = "test.csv"
# CSV.write(file1,out1)




grad = Vector{Float64}(undef,length(p0))
grad_test = Vector{Float64}(undef,length(p0))
hess = Matrix{Float64}(undef,length(p0),length(p0))

ll = log_likelihood(m,p0)
println(ll)


ll = log_likelihood_penalty!(hess,grad,m,p0,W)
# ll = log_likelihood_penalty!(grad_test,m,p0,W)
println(ll)
ll = log_likelihood_penalty!(grad_test,m,p0,W)
println(ll)


# d = m
# p = parDict(d,p0,no2Der=true)
# individual_values!(d,p)
# individual_shares(d,p)
# app = iterate(eachperson(d.data),3171)[1]
# grad[:].=0.0
# ll, ind = ll_obs_gradient!(grad,app,d,p)

f_obj(x) = log_likelihood_penalty(m,x,W)


grad_1 = Vector{Float64}(undef,length(p0))
hess_1 = Matrix{Float64}(undef,length(p0),length(p0))
fval = f_obj(p0)
println(fval-ll)

println("Grad")
ForwardDiff.gradient!(grad_1,f_obj, p0)#, cfg)
println(maximum(abs.(grad_1-grad)))

println("Hessian")
cfg = ForwardDiff.HessianConfig(f_obj, p0, ForwardDiff.Chunk{8}())
ForwardDiff.hessian!(hess_1,f_obj, p0)#,cfg)
println(maximum(abs.(hess_1-hess)))


f_obj_ll(x) = log_likelihood(m,x)
grad_ll = Vector{Float64}(undef,length(p0))
hess_ll = Matrix{Float64}(undef,length(p0),length(p0))
println(f_obj_ll(p0))
println("Hessian")
cfg = ForwardDiff.HessianConfig(f_obj_ll, p0, ForwardDiff.Chunk{8}())
ForwardDiff.hessian!(hess_ll,f_obj_ll, p0)#,cfg)


f_obj_p(x) = calc_penalty(m,x,W)
grad_pen = Vector{Float64}(undef,length(p0))
hess_pen = Matrix{Float64}(undef,length(p0),length(p0))
println(f_obj_p(p0))
println("Grad")
ForwardDiff.gradient!(grad_pen,f_obj_p, p0)#, cfg)
println("Hessian")
cfg = ForwardDiff.HessianConfig(f_obj_p, p0, ForwardDiff.Chunk{8}())
ForwardDiff.hessian!(hess_pen,f_obj_p, p0)#,cfg)
