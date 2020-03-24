rm(list = ls())
library(data.table)
library(nleqslv)
library(ggplot2)
library(scales)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

run = "2020-03-10"
spec = "FMC"

#### Read in Base Data ####
baseDataFull = read.csv("Intermediate_Output/Simulated_BaseData/simchoiceData_discrete.csv")
baseData = as.data.table(unique(baseDataFull[,c("Person","ST","Market","PERWT","Age","AGE","ageRate","MEMBERS","Family","LowIncome","Mandate")]))


#### Merge in Market Info ####
file = paste("Estimation_Output/MktHHI_",spec,"-",run,".rData",sep="")
load(file)

baseData = merge(baseData,Mkt,by="Market")
baseData[,Market_num:=as.numeric(as.factor(Market))]

#### Welfare By Market Cross-Section ####

mktWelfare = unique(baseData[,c("Market","ST","HHI","Market_num")])

## Merge in Welfare Info 
for (cw in c("Base_vch","RA_vch","SP_cp_base","SP_cp","SP_zp","SP")){
  file = paste("Estimation_Output/totalWelfare_bymkt_",cw,"-",spec,"-",run,".csv",sep="")
  data_cw = as.data.table(read.csv(file))
  data_cw[,totalWelfare:=CW+Profit]
  cw = gsub("_vch","",cw)
  if (cw=="Base"){
    data_cw[,RA_transfers:=RA_transfers/Population]
    data_cw = data_cw[,c("markets","totalWelfare","Spending","Profit","RA_transfers","Population")]
  }else{
    data_cw[,RA_transfers:=0]
    data_cw = data_cw[,c("markets","totalWelfare","Spending","Profit")]
  }

  names(data_cw) = paste(names(data_cw),cw,sep="_")
  mktWelfare = merge(mktWelfare,data_cw,by.x="Market_num",by.y=paste("markets",cw,sep="_"))
}

mktWelfare[,ExtSel:=totalWelfare_SP-totalWelfare_SP_zp]
mktWelfare[,Markup:=totalWelfare_SP_zp-totalWelfare_SP_cp_base]
mktWelfare[,IntSel:=totalWelfare_SP_cp_base-totalWelfare_Base]

mktWelfare[,Markup_noRA:=totalWelfare_SP_zp-totalWelfare_SP_cp]
mktWelfare[,IntSel_noRA:=totalWelfare_SP_cp-totalWelfare_RA]
# mktWelfare[,RiskAdj:=totalWelfare_RA-(totalWelfare_Base-RA_transfers_Base)]

# test_mean = mktWelfare[,list(totalWelfare_SP=sum(Population_Base*totalWelfare_SP)/sum(Population_Base),
#                              totalWelfare_SP_zp=sum(Population_Base*totalWelfare_SP_zp)/sum(Population_Base),
#                              totalWelfare_SP_cp_base=sum(Population_Base*totalWelfare_SP_cp_base)/sum(Population_Base),
#                              totalWelfare_Base=sum(Population_Base*totalWelfare_Base)/sum(Population_Base),
#                              totalWelfare_SP_cp=sum(Population_Base*totalWelfare_SP_cp)/sum(Population_Base),
#                              totalWelfare_RA=sum(Population_Base*totalWelfare_RA)/sum(Population_Base),
#                             pop = sum(Population_Base))]

mktWelfare[,HHI_cat:=ceiling(HHI/500)*500]
mktWelfare[HHI<2500,HHI_cat:=3000]
mktWelfare[HHI>4500,HHI_cat:=ceiling((HHI-500)/1000)*1000+500]
mktWelfare[HHI>7500,HHI_cat:=7500]
mktWelfare[HHI>=9000,HHI_cat:=10000]
mktWelfare[,count:=1]
cross_section = mktWelfare[,list(ExtSel=sum(Population_Base*ExtSel)/sum(Population_Base),
                                 Markup=sum(Population_Base*Markup)/sum(Population_Base),
                                 IntSel=sum(Population_Base*IntSel)/sum(Population_Base),
                                 Markup_noRA=sum(Population_Base*Markup_noRA)/sum(Population_Base),
                                 IntSel_noRA=sum(Population_Base*IntSel_noRA)/sum(Population_Base),
                                 # RiskAdj=sum(Population_Base*RiskAdj)/sum(Population_Base),
                                 count = sum(count),
                                 pop = sum(Population_Base)),
                           by=c("HHI_cat")]
setkey(cross_section,HHI_cat)

round(cross_section,1)

all_mean = mktWelfare[,list(ExtSel=sum(Population_Base*ExtSel)/sum(Population_Base),
                                 Markup=sum(Population_Base*Markup)/sum(Population_Base),
                                 IntSel=sum(Population_Base*IntSel)/sum(Population_Base),
                                 Markup_noRA=sum(Population_Base*Markup_noRA)/sum(Population_Base),
                                 IntSel_noRA=sum(Population_Base*IntSel_noRA)/sum(Population_Base),
                                 # RiskAdj=sum(Population_Base*RiskAdj)/sum(Population_Base),
                                 count = sum(count),
                                 pop = sum(Population_Base))]

round(all_mean,1)

#### Welfare By Market Merger #### 
mktWelfare = unique(baseData[,c("Market","ST","HHI","Market_num")])

## Merge in Welfare Info 
for (cw in c("SP_zp","SP_cp_base","Base_vch","SP_cpm_base","Base_m_vch","SP_cp","RA_vch","SP_cpm","RA_m_vch")){
  file = paste("Estimation_Output/totalWelfare_bymkt_",cw,"-",spec,"-",run,".csv",sep="")
  data_cw = as.data.table(read.csv(file))
  cw = gsub("_vch","",cw)
  if (cw%in%c("Base","Base_m")){
    # data_cw[,Profit:=Profit-RA_transfers/Population]
    data_cw[,totalWelfare:=CW+Profit]
    
    data_cw = data_cw[,c("markets","CW","Profit","totalWelfare","Population")]
  }else{
    data_cw[,totalWelfare:=CW+Profit]
    data_cw = data_cw[,c("markets","CW","Profit","totalWelfare")]
  }
  # data_cw = data_cw[,c("markets","Profit")]

  
  names(data_cw) = paste(names(data_cw),cw,sep="_")
  mktWelfare = merge(mktWelfare,data_cw,by.x="Market_num",by.y=paste("markets",cw,sep="_"))
}
mktWelfare = mktWelfare[ST=="GA"&Market!="GA_1",]


mktWelfare[,Markup_RA:=totalWelfare_SP_zp-totalWelfare_SP_cp]
mktWelfare[,IntSel_RA:=totalWelfare_SP_cp-totalWelfare_RA]

mktWelfare[,Markup_RA_m:=totalWelfare_SP_zp-totalWelfare_SP_cpm]
mktWelfare[,IntSel_RA_m:=totalWelfare_SP_cpm-totalWelfare_RA_m]

mktWelfare[,Markup_base:=totalWelfare_SP_zp-totalWelfare_SP_cp_base]
mktWelfare[,IntSel_base:=totalWelfare_SP_cp_base-totalWelfare_Base]

mktWelfare[,Markup_base_m:=totalWelfare_SP_zp-totalWelfare_SP_cpm_base]
mktWelfare[,IntSel_base_m:=totalWelfare_SP_cpm_base-totalWelfare_Base_m]


mktWelfare[,dCW_base:=CW_Base_m - CW_Base]
mktWelfare[,dPS_base:=Profit_Base_m - Profit_Base]

mktWelfare[,dMkup_base:= Markup_base_m - Markup_base]
mktWelfare[,dInt_base:= IntSel_base_m - IntSel_base]

mktWelfare[,dCW_RA:=CW_RA_m - CW_RA]
mktWelfare[,dPS_RA:=Profit_RA_m - Profit_RA]

mktWelfare[,dMkup_RA:= Markup_RA_m - Markup_RA]
mktWelfare[,dInt_RA:= IntSel_RA_m - IntSel_RA]

mktWelfare[,netchg_RA:=dCW_RA+dPS_RA]
mktWelfare[,netchg_base:=dCW_base+dPS_base]
setkey(mktWelfare,HHI)

mktWelfare[,HHI_cat:=as.numeric(HHI<3800)]
mktWelfare[,count:=1]

merger = mktWelfare[,list(dCW_base=sum(dCW_base*Population_Base)/sum(Population_Base),
                          dPS_base=sum(dPS_base*Population_Base)/sum(Population_Base),
                          dMkup_base=sum(dMkup_base*Population_Base)/sum(Population_Base),
                          dInt_base=sum(dInt_base*Population_Base)/sum(Population_Base),
                          dCW_RA=sum(dCW_RA*Population_Base)/sum(Population_Base),
                          dPS_RA=sum(dPS_RA*Population_Base)/sum(Population_Base),
                          dMkup_RA=sum(dMkup_RA*Population_Base)/sum(Population_Base),
                          dInt_RA=sum(dInt_RA*Population_Base)/sum(Population_Base),
                          count = sum(count),
                          pop= sum(Population_Base)),
                    by=c("HHI_cat")]
round(merger,1)


#### Welfare Policy Analysis - By Market ####
mktWelfare = unique(baseData[,c("Market","ST","HHI","Market_num","HHI_flag","dhhi_actual")])


## Merge in Welfare Info 
for (cw in c("Base","RA","man","RAman")){
  file = paste("Estimation_Output/totalWelfare_bymkt_",cw,"-",spec,"-",run,".csv",sep="")
  data_cw = as.data.table(read.csv(file))
  data_cw[,insRate:=Insured/Population]
  if (cw%in%c("Base","Base_m","man","man_m")){
    data_cw[,Profit:=Profit-RA_transfers/Population]
    # data_cw[,totalWelfare:=CW+Profit-RA_transfers/Population]
    
    data_cw = data_cw[,c("markets","CW","Profit","Spending","insRate","Population")]
  }else{
    # data_cw[,totalWelfare:=CW+Profit]
    data_cw = data_cw[,c("markets","CW","Profit","Spending","insRate")]
  }
  # data_cw = data_cw[,c("markets","Profit")]
  
  
  names(data_cw) = paste(names(data_cw),cw,sep="_")
  mktWelfare = merge(mktWelfare,data_cw,by.x="Market_num",by.y=paste("markets",cw,sep="_"))
}
mktWelfare[,count:=1]
policy = mktWelfare[,list(CW_base=sum(CW_Base*Population_Base)/sum(Population_Base),
                          CW_RA=sum(CW_RA*Population_Base)/sum(Population_Base),
                          CW_man=sum(CW_man*Population_Base)/sum(Population_Base),
                          CW_RAman=sum(CW_RAman*Population_Base)/sum(Population_Base),
                          PS_base=sum(Profit_Base*Population_Base)/sum(Population_Base),
                          PS_RA=sum(Profit_RA*Population_Base)/sum(Population_Base),
                          PS_man=sum(Profit_man*Population_Base)/sum(Population_Base),
                          PS_RAman=sum(Profit_RAman*Population_Base)/sum(Population_Base),
                          Gov_base=sum(Spending_Base*Population_Base)/sum(Population_Base),
                          Gov_RA=sum(Spending_RA*Population_Base)/sum(Population_Base),
                          Gov_man=sum(Spending_man*Population_Base)/sum(Population_Base),
                          Gov_RAman=sum(Spending_RAman*Population_Base)/sum(Population_Base),
                          insRate_base=sum(insRate_Base*Population_Base)/sum(Population_Base),
                          insRate_RA=sum(insRate_RA*Population_Base)/sum(Population_Base),
                          insRate_man=sum(insRate_man*Population_Base)/sum(Population_Base),
                          insRate_RAman=sum(insRate_RAman*Population_Base)/sum(Population_Base),
                          count = sum(count),
                          pop= sum(Population_Base)),
                    by=c("HHI_flag")]
setkey(policy,HHI_flag)
round(policy,1)
round(policy,3)

#### Data Insurance Rates ####
load("Intermediate_Output/Simulated_BaseData/acs_prepped.rData")
mktWelfare = unique(baseData[,c("Market","ST","HHI","Market_num","HHI_flag")])
mktIns = acs[,list(insuranceRate=sum(PERWT*insured)/sum(PERWT),
                   population = sum(PERWT)),by=c("ST","AREA")]
mktIns[,Market:=paste(ST,gsub("Rating Area ","",AREA),sep="_")]
mktIns = merge(mktIns,mktWelfare,by=c("Market","ST"))

mktIns = mktIns[,list(insuranceRate=sum(insuranceRate*population)/sum(population)),
                by="HHI_flag"]



#### Welfare By Market Merger - Policy #### 
mktWelfare = unique(baseData[,c("Market","ST","HHI","Market_num","HHI_flag")])

## Merge in Welfare Info 
for (cw in c("Base","RA","man","RAman","Base_m","RA_m","man_m","RAman_m")){
  file = paste("Estimation_Output/totalWelfare_bymkt_",cw,"-",spec,"-",run,".csv",sep="")
  data_cw = as.data.table(read.csv(file))
  data_cw[,insRate:=Insured/Population]
  
  if (cw%in%c("Base","Base_m")){
    data_cw[,Profit:=Profit-RA_transfers/Population]
    # data_cw[,totalWelfare:=CW+Profit]
    
    data_cw = data_cw[,c("markets","CW","Profit","Spending","Population","Insured")]
  }else{
    # data_cw[,totalWelfare:=CW+Profit]
    data_cw = data_cw[,c("markets","CW","Profit","Spending","Insured")]
  }
  # data_cw = data_cw[,c("markets","Profit")]
  
  
  names(data_cw) = paste(names(data_cw),cw,sep="_")
  mktWelfare = merge(mktWelfare,data_cw,by.x="Market_num",by.y=paste("markets",cw,sep="_"))
}
mktWelfare = mktWelfare[ST=="GA",]


mktWelfare[,dCW_base:=CW_Base_m - CW_Base]
mktWelfare[,dPS_base:=Profit_Base_m - Profit_Base]
mktWelfare[,dGov_base:=Spending_Base_m - Spending_Base]
mktWelfare[,dIns_base:=Insured_Base_m - Insured_Base]

mktWelfare[,dCW_RA:=CW_RA_m - CW_RA]
mktWelfare[,dPS_RA:=Profit_RA_m - Profit_RA]
mktWelfare[,dGov_RA:=Spending_RA_m - Spending_RA]
mktWelfare[,dIns_RA:=Insured_RA_m - Insured_RA]

mktWelfare[,dCW_man:=CW_man_m - CW_man]
mktWelfare[,dPS_man:=Profit_man_m - Profit_man]
mktWelfare[,dGov_man:=Spending_man_m - Spending_man]
mktWelfare[,dIns_man:=Insured_man_m - Insured_man]

mktWelfare[,dCW_RAman:=CW_RAman_m - CW_RAman]
mktWelfare[,dPS_RAman:=Profit_RAman_m - Profit_RAman]
mktWelfare[,dGov_RAman:=Spending_RAman_m - Spending_RAman]
mktWelfare[,dIns_RAman:=Insured_RAman_m - Insured_RAman]



merger = mktWelfare[Market!="GA_1",list(dCW_base=sum(dCW_base*Population_Base)/sum(CW_Base*Population_Base),
                                   dCW_RA=sum(dCW_RA*Population_Base)/sum(CW_RA*Population_Base),
                                   dCW_man=sum(dCW_man*Population_Base)/sum(CW_man*Population_Base),
                                   dCW_RAman=sum(dCW_RAman*Population_Base)/sum(CW_RAman*Population_Base),
                                   dPS_base=sum(dPS_base*Population_Base)/sum(Profit_Base*Population_Base),
                                   dPS_RA=sum(dPS_RA*Population_Base)/sum(Profit_RA*Population_Base),
                                   dPS_man=sum(dPS_man*Population_Base)/sum(Profit_man*Population_Base),
                                   dPS_RAman=sum(dPS_RAman*Population_Base)/sum(Profit_RAman*Population_Base),
                                   dGov_base=sum(dGov_base*Population_Base)/sum(Spending_Base*Population_Base),
                                   dGov_RA=sum(dGov_RA*Population_Base)/sum(Spending_RA*Population_Base),
                                   dGov_man=sum(dGov_man*Population_Base)/sum(Spending_man*Population_Base),
                                   dGov_RAman=sum(dGov_RAman*Population_Base)/sum(Spending_RAman*Population_Base),
                                   dIns_base=sum(dIns_base*Population_Base)/sum(Insured_Base*Population_Base),
                                   dIns_RA=sum(dIns_RA*Population_Base)/sum(Insured_RA*Population_Base),
                                   dIns_man=sum(dIns_man*Population_Base)/sum(Insured_man*Population_Base),
                                   dIns_RAman=sum(dIns_RAman*Population_Base)/sum(Insured_RAman*Population_Base))]


round(merger*100,1)











#### Welfare By Market Cross-Section  - Government Robustness ####

mktWelfare = unique(baseData[,c("Market","ST","HHI","Market_num")])

## Merge in Welfare Info 
for (cw in c("Base_vch","RA_vch","SP_cp_base_gov","SP_cp_gov","SP_zp_gov","SP_gov")){
  file = paste("Estimation_Output/totalWelfare_bymkt_",cw,"-",spec,"-",run,".csv",sep="")
  data_cw = as.data.table(read.csv(file))
  data_cw[,totalWelfare:=CW+Profit+Spending]
  cw = gsub("_vch","",cw)
  cw = gsub("_gov","",cw)
  if (cw=="Base"){
    data_cw[,RA_transfers:=RA_transfers/Population]
    data_cw = data_cw[,c("markets","totalWelfare","Spending","Profit","RA_transfers","Population")]
  }else{
    data_cw[,RA_transfers:=0]
    data_cw = data_cw[,c("markets","totalWelfare","Spending","Profit")]
  }
  
  names(data_cw) = paste(names(data_cw),cw,sep="_")
  mktWelfare = merge(mktWelfare,data_cw,by.x="Market_num",by.y=paste("markets",cw,sep="_"))
}

mktWelfare[,ExtSel:=totalWelfare_SP-totalWelfare_SP_zp]
mktWelfare[,Markup:=totalWelfare_SP_zp-totalWelfare_SP_cp_base]
mktWelfare[,IntSel:=totalWelfare_SP_cp_base-totalWelfare_Base]

mktWelfare[,Markup_noRA:=totalWelfare_SP_zp-totalWelfare_SP_cp]
mktWelfare[,IntSel_noRA:=totalWelfare_SP_cp-totalWelfare_RA]
# mktWelfare[,RiskAdj:=totalWelfare_RA-(totalWelfare_Base-RA_transfers_Base)]

# test_mean = mktWelfare[,list(totalWelfare_SP=sum(Population_Base*totalWelfare_SP)/sum(Population_Base),
#                              totalWelfare_SP_zp=sum(Population_Base*totalWelfare_SP_zp)/sum(Population_Base),
#                              totalWelfare_SP_cp_base=sum(Population_Base*totalWelfare_SP_cp_base)/sum(Population_Base),
#                              totalWelfare_Base=sum(Population_Base*totalWelfare_Base)/sum(Population_Base),
#                              totalWelfare_SP_cp=sum(Population_Base*totalWelfare_SP_cp)/sum(Population_Base),
#                              totalWelfare_RA=sum(Population_Base*totalWelfare_RA)/sum(Population_Base),
#                             pop = sum(Population_Base))]

mktWelfare[,HHI_cat:=ceiling(HHI/500)*500]
mktWelfare[HHI<2500,HHI_cat:=3000]
mktWelfare[HHI>4500,HHI_cat:=ceiling((HHI-500)/1000)*1000+500]
mktWelfare[HHI>7500,HHI_cat:=7500]
mktWelfare[HHI>=9000,HHI_cat:=10000]
mktWelfare[,count:=1]
cross_section = mktWelfare[,list(ExtSel=sum(Population_Base*ExtSel)/sum(Population_Base),
                                 Markup=sum(Population_Base*Markup)/sum(Population_Base),
                                 IntSel=sum(Population_Base*IntSel)/sum(Population_Base),
                                 Markup_noRA=sum(Population_Base*Markup_noRA)/sum(Population_Base),
                                 IntSel_noRA=sum(Population_Base*IntSel_noRA)/sum(Population_Base),
                                 # RiskAdj=sum(Population_Base*RiskAdj)/sum(Population_Base),
                                 count = sum(count),
                                 pop = sum(Population_Base)),
                           by=c("HHI_cat")]
setkey(cross_section,HHI_cat)

round(cross_section,2)

all_mean = mktWelfare[,list(ExtSel=sum(Population_Base*ExtSel)/sum(Population_Base),
                            Markup=sum(Population_Base*Markup)/sum(Population_Base),
                            IntSel=sum(Population_Base*IntSel)/sum(Population_Base),
                            Markup_noRA=sum(Population_Base*Markup_noRA)/sum(Population_Base),
                            IntSel_noRA=sum(Population_Base*IntSel_noRA)/sum(Population_Base),
                            # RiskAdj=sum(Population_Base*RiskAdj)/sum(Population_Base),
                            count = sum(count),
                            pop = sum(Population_Base))]

round(all_mean,1)








#### Welfare By Market Merger - Policy - Price Linked Robustness #### 
mktWelfare = unique(baseData[,c("Market","ST","HHI","Market_num","HHI_flag")])

## Merge in Welfare Info 
for (cw in c("Base_pl","RA_pl","man_pl","RAman_pl","Base_m_pl","RA_m_pl","man_m_pl","RAman_m_pl")){
  file = paste("Estimation_Output/totalWelfare_bymkt_",cw,"-",spec,"-",run,".csv",sep="")
  data_cw = as.data.table(read.csv(file))
  data_cw[,insRate:=Insured/Population]
  cw = gsub("_pl","",cw)
  if (cw%in%c("Base","Base_m")){
    data_cw[,Profit:=Profit-RA_transfers/Population]
    # data_cw[,totalWelfare:=CW+Profit]
    
    data_cw = data_cw[,c("markets","CW","Profit","Spending","Population","Insured")]
  }else{
    # data_cw[,totalWelfare:=CW+Profit]
    data_cw = data_cw[,c("markets","CW","Profit","Spending","Insured")]
  }
  # data_cw = data_cw[,c("markets","Profit")]
  
  
  names(data_cw) = paste(names(data_cw),cw,sep="_")
  mktWelfare = merge(mktWelfare,data_cw,by.x="Market_num",by.y=paste("markets",cw,sep="_"))
}
mktWelfare = mktWelfare[ST=="GA",]


mktWelfare[,dCW_base:=CW_Base_m - CW_Base]
mktWelfare[,dPS_base:=Profit_Base_m - Profit_Base]
mktWelfare[,dGov_base:=Spending_Base_m - Spending_Base]
mktWelfare[,dIns_base:=Insured_Base_m - Insured_Base]

mktWelfare[,dCW_RA:=CW_RA_m - CW_RA]
mktWelfare[,dPS_RA:=Profit_RA_m - Profit_RA]
mktWelfare[,dGov_RA:=Spending_RA_m - Spending_RA]
mktWelfare[,dIns_RA:=Insured_RA_m - Insured_RA]

mktWelfare[,dCW_man:=CW_man_m - CW_man]
mktWelfare[,dPS_man:=Profit_man_m - Profit_man]
mktWelfare[,dGov_man:=Spending_man_m - Spending_man]
mktWelfare[,dIns_man:=Insured_man_m - Insured_man]

mktWelfare[,dCW_RAman:=CW_RAman_m - CW_RAman]
mktWelfare[,dPS_RAman:=Profit_RAman_m - Profit_RAman]
mktWelfare[,dGov_RAman:=Spending_RAman_m - Spending_RAman]
mktWelfare[,dIns_RAman:=Insured_RAman_m - Insured_RAman]



merger = mktWelfare[Market!="GA_1",list(dCW_base=sum(dCW_base*Population_Base)/sum(CW_Base*Population_Base),
                                        dCW_RA=sum(dCW_RA*Population_Base)/sum(CW_RA*Population_Base),
                                        dCW_man=sum(dCW_man*Population_Base)/sum(CW_man*Population_Base),
                                        dCW_RAman=sum(dCW_RAman*Population_Base)/sum(CW_RAman*Population_Base),
                                        dPS_base=sum(dPS_base*Population_Base)/sum(Profit_Base*Population_Base),
                                        dPS_RA=sum(dPS_RA*Population_Base)/sum(Profit_RA*Population_Base),
                                        dPS_man=sum(dPS_man*Population_Base)/sum(Profit_man*Population_Base),
                                        dPS_RAman=sum(dPS_RAman*Population_Base)/sum(Profit_RAman*Population_Base),
                                        dGov_base=sum(dGov_base*Population_Base)/sum(Spending_Base*Population_Base),
                                        dGov_RA=sum(dGov_RA*Population_Base)/sum(Spending_RA*Population_Base),
                                        dGov_man=sum(dGov_man*Population_Base)/sum(Spending_man*Population_Base),
                                        dGov_RAman=sum(dGov_RAman*Population_Base)/sum(Spending_RAman*Population_Base),
                                        dIns_base=sum(dIns_base*Population_Base)/sum(Insured_Base*Population_Base),
                                        dIns_RA=sum(dIns_RA*Population_Base)/sum(Insured_RA*Population_Base),
                                        dIns_man=sum(dIns_man*Population_Base)/sum(Insured_man*Population_Base),
                                        dIns_RAman=sum(dIns_RAman*Population_Base)/sum(Insured_RAman*Population_Base))]


round(merger*100,1)







baseData[,dCW_RA:=(CW_RA_m-CW_RA)]
# CW_RA = CW_SP_cp + (CW_RA-CW_SP_cp)
baseData[,dSort_merger:=(CW_RA_m-CW_SP_cpm) - (CW_RA-CW_SP_cp)]
baseData[,dProf_merger:=(CW_SP_cpm-CW_SP_cp)]
baseData[,dCW_merger:=dSort_merger + dProf_merger]
# 
ggplot(baseData[mergerLabel=="Strong"]) + aes(y=dSort_merger,x=CW_RA) +
  geom_point() + geom_smooth(method="lm") + facet_grid(as.factor(HHI)~as.factor(dhhi_actual))

merger.plot = as.data.table(reshape(baseData[ST=="GA",c("Market","Person","PERWT","HHI","ageRate","MEMBERS","CW_RA","dProf_merger","dSort_merger","dCW_merger")],
                      varying = c("dProf_merger","dSort_merger","dCW_merger"),
                      v.names="Welfare",
                      timevar="Source",
                      times=c("dProf_merger","dSort_merger","dCW_merger"),
                      idvar=c("Market","Person","PERWT","HHI","ageRate","MEMBERS","CW_RA"),
                      direction="long"))

merger.plot[,CW_RA_2:=CW_RA^2]
merger.plot[,CW_RA_3:=CW_RA^3]

merger.plot[,HHI_flag:=0]

merger.plot[HHI>3900,HHI_flag:=1]

merger.plot[Source=="dSort_merger",Source:="Improved Sorting"]
merger.plot[Source=="dProf_merger",Source:="Higher Markups"]
merger.plot[Source=="dCW_merger",Source:="Net Welfare Effect"]

# merger.plot[HHI_flag==0&Source=="dCW_merger",Welf_pred:=predict(lm(Welfare~CW_RA + CW_RA_2 + CW_RA_3))]
# merger.plot[HHI_flag==1&Source=="dCW_merger",Welf_pred:=predict(lm(Welfare~CW_RA + CW_RA_2 + CW_RA_3))]

merger.plot[,HHI_factor:=factor(HHI_flag,levels=c(0,1),
                              labels=c("Pre-merger HHI Less than 3900",
                             "Pre-merger HHI Greater than 3900"))]

png("Writing/Images/mergerWelfare.png",width=2500,height=1500,res=275)
ggplot(merger.plot[Source%in%c("Higher Markups","Improved Sorting")]) + 
  aes(x=CW_RA,y=Welfare,color=Source,shape=Source) + #,linetype=Source,group=Person) + 
  geom_point(size=3,alpha=0.5) +
  geom_smooth(data=merger.plot[Source=="Net Welfare Effect"],method="lm",se=FALSE) + #+ geom_smooth(method="lm",se=FALSE)  + 
  facet_wrap(~HHI_factor) +
  ylab("Change in Consumer Welfare") + 
  xlab("Pre-merger Consumer Welfare ($000s per year per person)") + 
  # labs(color = "Source of Welfare Change",shape = "Source of Welfare Change",linetype = "Source of Welfare Change") +
  # guides(shape=guide_legend(override.aes=list(size=3)),linetype=guide_legend(override.aes=list(size=1))) + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_rect(color=grey(.5)),
    legend.title = element_blank(),
    strip.text = element_text(size=14),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "bottom",
    axis.title=element_text(size=14),
    axis.text = element_text(size=16))
dev.off()


Markets = baseData[,list(dCW_RA_Base = sum(dCW_RA_Base*PERWT)/sum(PERWT),
                         dCW_SPcp_Base = sum(dCW_SPcp_Base*PERWT)/sum(PERWT),
                         dCW_SP_SPcp = sum(dCW_SP_SPcp*PERWT)/sum(PERWT),
                         dCW_SP_SPzp = sum(dCW_SP_SPzp*PERWT)/sum(PERWT)#,
                         # dSort_merger = sum(dSort_merger*PERWT)/sum(PERWT),
                         # dProf_merger = sum(dProf_merger*PERWT)/sum(PERWT)
                         ), by=c("ST","Market","HHI","HHI_flag","Market_num")]

market.plot = reshape(Markets,varying = c("dCW_RA_Base","dCW_SPcp_Base","dCW_SP_SPcp","dCW_SP_SPzp"),
                      v.names="Welfare",
                      timevar="Source",
                      times=c("Risk Adjustment","Inefficient Sorting","Equilibrium Profits","Zero Profit"),
                      idvar=c("ST","Market","HHI","Market_num"),
                      direction="long")

png("Writing/Images/crossWelfare.png",width=2500,height=1500,res=275)
ggplot(market.plot[Source%in%c("Inefficient Sorting","Equilibrium Profits","Zero Profit")]) + 
  aes(x=HHI,y=Welfare,color=Source,shape=Source,linetype=Source,group=Source) + 
  geom_point(size=3,alpha=0.8) + geom_smooth(method="loess",se=FALSE)  + 
  ylab("Welfare ($000s per year per person)") + 
  labs(color = "Source of Welfare Loss",shape = "Source of Welfare Loss",linetype = "Source of Welfare Loss") +
  # guides(shape=guide_legend(override.aes=list(size=3)),linetype=guide_legend(override.aes=list(size=1))) + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_rect(color=grey(.5)),
    legend.title = element_text(size=14),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "bottom",
    axis.title=element_text(size=14),
    axis.text = element_text(size=16))
dev.off()
