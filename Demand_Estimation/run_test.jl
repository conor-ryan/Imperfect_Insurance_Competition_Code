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
β0start = rand(m.parLength[:β])/10 .-.05
βstart = rand(m.parLength[:γ])/10 .- .05
σstart = rand(m.parLength[:σ])/10 .- .05

FEstart = rand(m.parLength[:FE])/100 .-.005

p0 = vcat(γstart,β0start,βstart,σstart,FEstart)


p0 =[-0.0918332909479551, -0.08317906511721498, 0.25004232607970794, -0.2910448487514561, -3.211772291502451, -1.4559615907022367, -2.999007601137356, 6.093684889202869, 0.07760213750338138, 0.17790780387902916, 0.4585974119715529, -0.011996896328934558, -0.07975916782984774, 1.2783478949876477, 30.07239605249516, 10.658595726904498, 29.680913559724868, 0.1245689886650655, 14.013808177352212, 30.107069376873522, -0.22175297446366288, 10.610371648224762, 0.015314094520274773, 14.003177489583424, 30.034181954137487, 10.516168381569182, 29.553324051038366, -0.0017958456208069537, 0.12052782032923827, 9.908296732439798, -15.17331905107676, 10.479134619298597, 0.027165383261324765, 6.812536480871054, 9.79718741138596, 30.080972522645908, 0.10280277396630907, 29.549894048932995, 2.9309209029564474, -0.15192689595944214, 14.833123052928197, 14.016577517231052, 7.8311477914108725, 30.087248256525406, -0.0004759122412344867, 0.04020740552569497, 10.551885527684892, 0.009326575196471646, 25.845848061728155, -0.18054923742911191, 0.04335068274140019, 29.448912185761984, 13.796988039249966, 14.709235569578679, 29.340460519216823, 29.969813990860604, 10.552910588683503, 29.461424831553742, -0.03790054001368677, 9.840412699996007, 0.0348292560149244, 13.899249333101144, 14.701808290903703, 7.817100857222235, 30.09346877008109, 10.498420320623241, 14.751815090822879, 2.9305580581381236, 38.34570606936785, 14.7419818526111, 29.485888658627914, 6.81765315226312, 0.02959646731062384, 13.97965140606306, 10.52436895620849, 6.854742264654198, 14.678403071369432, 38.345815079684066, 6.815344264147771, 29.60628227022954, 14.732541587199725, 14.781015757238048, 14.015451430937162, 0.08138497254835018, 13.947981824987375, 29.45550635928582, 29.953785942356518, 10.50386917953513, 29.422901313691334, -0.10368056843501076, -0.3038389693312075, 9.691488128511114, -0.015437742909293792, 13.88446926508279, 29.89229885305454, 10.504576729344727, 25.900902222859244, -2.3125404561964737, -0.6265478318442494, -1.3234323826017815, 0.4542347413856722, -0.39120419492397257, -1.2113050269213312, -0.5015012299964179, -0.9320651095906471, -2.2236393143902236, 0.8463178558373636, -0.32314900516922423, -1.5227288352368846, 3.0077012897833417, -0.5901863511853578, 1.2752173603289005, -1.4032882112563125, -1.7875339532113346, -1.2224413257636755, 2.262048037730214, 1.7550232517018425, 1.9506627309839601, 2.539712284220117, 1.9022784666789274, 2.2907470122714786, 0.9402446893096554, 2.693221709040369, 2.012701852583178, -3.0519039114544153, -1.329056857643531, 0.38101651630136385, -0.7927884810837812, -0.1405072423290247, -0.7804909713326421, 1.8724000150336095, 2.2218316104878877, 0.296338339130262, 1.0548043492344406, -1.4414277862465479, -0.555922166360773, 0.00010640818604747937, -1.4723307142049815, -2.461113051239683, 0.6089009602231148, -1.1468424223867464, -0.006214638818330688, -0.34349098255288124, -2.1925663911753, 0.12773177712723277, -0.6768743327487606, 0.07897153403168863, -2.0381819277312108, -0.6381405263851015, -0.25471771410657945, 0.8102384101030374, 0.9749075788371102, 2.2357539655473393, 0.6374965536979907, -1.5445674743623998, 0.1703599304944477, -3.63452252868906, -2.2519290316569274, 1.1836667839729667, 1.3749596449147834, -0.7832631463006247, 3.3365870416563808, 1.4826199065231633, 1.1337640726556002, 0.9452827701752807, -0.32667190873488494, -0.2594326547055738, -1.2122151121258655, 0.9544428071759024, -0.6084968979585823, -1.6580974176947227, -1.01271133292556, -0.6023028417514865, -0.22540502345657676, 1.161024876828826, -0.9926580278624807, -1.1553106094715007, -1.0659732710952863, -1.2334480481929873, -2.1387396582882525, 1.2242228293909614]


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


# ll = log_likelihood_penalty!(hess,grad,m,p0,W)
# ll = log_likelihood_penalty!(grad_test,m,p0,W)
println(ll)
ll = log_likelihood_penalty!(grad,m,p0,W)
println(ll)


d = m
p = parDict(d,p0,no2Der=true)
individual_values!(d,p)
individual_shares(d,p)
app = iterate(eachperson(d.data),3171)[1]
grad[:].=0.0
ll, ind = ll_obs_gradient!(grad,app,d,p)

f_obj(x) = log_likelihood_penalty(m,x,W)
# f_obj(x) = ll_obs_test(app,m,x)

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
