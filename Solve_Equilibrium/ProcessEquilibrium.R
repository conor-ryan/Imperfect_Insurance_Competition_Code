rm(list = ls())
library(data.table)
library(nleqslv)
library(ggplot2)
library(scales)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

run = "2019-10-19"
spec = "FMC"

#### Read in Data ####
eqFile = paste("Estimation_Output/solvedEquilibrium_",spec,"-",run,".csv",sep="")
eqData = as.data.table(read.csv(eqFile))

prodData = as.data.table(read.csv("Intermediate_Output/Equilibrium_Data/estimated_prodData_full.csv"))
prodData = prodData[,c("ST","Firm","Product","Metal_std","Market")]

load("Intermediate_Output/Simulated_BaseData/simMarketSize.rData")
prodData = merge(prodData,marketSize,by="Market")

prod_pred = merge(prodData,eqData,by="Product")

margFile = paste("Estimation_Output/checkMargins_",spec,"-",run,".csv",sep="")
mData = as.data.table(read.csv(margFile))

prod_pred = merge(prod_pred,mData[,c("Product","P_obs","lives")],by="Product")

#### Compute Shares ####
for (v in c("base","RA","man","RAman")){
  for (m in c("","_m")){
    var_name = paste("Share_",v,m,sep="")
    lives_name = paste("Lives_",v,m,sep="")
    prod_pred[,c(var_name):=.SD/size,.SDcols=c(lives_name)]
  }
}

# # states = sort(unique(prod_pred$ST))[1:6]
# states = c("GA")
# vars = names(prod_pred)[grepl("(_base$|_RA$)",names(prod_pred))]

# prod_pred = prod_pred[ST%in%states,.SD,.SDcols=c("Product","Market","ST","Firm","Metal_std","size",vars)]
# prod_pred = prod_pred[ST%in%c("AK","NE","IA"),]


#### HHI Breakdown ####
Firms = prod_pred[,lapply(.SD,sum),
                  .SDcols=c("Lives_base"),
                  by=c("ST","Market","Firm","size")]
Firms[,insured:=sum(Lives_base),by="Market"]
Firms[,share:=Lives_base/insured]

Mkt = Firms[,list(HHI=sum((share*100)^2)),by="Market"]
Mkt[,HHI_flag:=0]
Mkt[HHI>3350,HHI_flag:=1]
Mkt[HHI>5200,HHI_flag:=2]
table(Mkt$HHI_flag)
file = paste("Estimation_Output/MktHHI_",spec,"-",run,".rData",sep="")
save(Mkt,file=file)

prod_pred = merge(prod_pred,Mkt,by="Market")


#### Insured Results ####
Insured = prod_pred[,lapply(.SD,sum),
                    .SDcols=c("Lives_base","Lives_RA","Lives_man","Lives_RAman","Lives_base_m","Lives_RA_m","Lives_man_m","Lives_RAman_m"),
                    by=c("ST","Market","size","HHI_flag")]
Insured = Insured[,lapply(.SD,sum),
                  .SDcols=c("size","Lives_base","Lives_RA","Lives_man","Lives_RAman","Lives_base_m","Lives_RA_m","Lives_man_m","Lives_RAman_m"),
                  by=c("HHI_flag")]

for (v in c("base","RA","man","RAman")){
  for (m in c("","_m")){
    var_name = paste("Ins_",v,m,sep="")
    lives_name = paste("Lives_",v,m,sep="")
    Insured[,c(var_name):=.SD/size,.SDcols=c(lives_name)]
  }
}

#### Premium Results ####

prem = prod_pred[,list(Price_obs = round(12/1000*sum(P_obs*lives)/sum(lives),2),
                       Price_base = round(12/1000*sum(Price_base*Lives_base)/sum(Lives_base),2),
                       Price_RA = round(12/1000*sum(Price_RA*Lives_base)/sum(Lives_base),2),
                       Price_man = round(12/1000*sum(Price_man*Lives_base)/sum(Lives_base),2),
                       Price_RAman = round(12/1000*sum(Price_RAman*Lives_base)/sum(Lives_base),2)
),
by=c("Metal_std","HHI_flag")]
setkey(prem,HHI_flag,Price_base)
print(prem[Metal_std%in%c("BRONZE","SILVER","GOLD")])

prem_Monopoly = prod_pred[HHI>=8000,list(Price_obs = round(12/1000*sum(P_obs*lives)/sum(lives),2),
                       Price_base = round(12/1000*sum(Price_base*Lives_base)/sum(Lives_base),2),
                       Price_RA = round(12/1000*sum(Price_RA*Lives_base)/sum(Lives_base),2),
                       Price_man = round(12/1000*sum(Price_man*Lives_base)/sum(Lives_base),2),
                       Price_RAman = round(12/1000*sum(Price_RAman*Lives_base)/sum(Lives_base),2)
),
by=c("Metal_std","HHI_flag")]
setkey(prem_Monopoly,HHI_flag,Price_base)
print(prem_Monopoly[Metal_std%in%c("BRONZE","SILVER","GOLD")])

prem_Competitive = prod_pred[HHI<=2800,list(Price_obs = round(12/1000*sum(P_obs*lives)/sum(lives),2),
                                         Price_base = round(12/1000*sum(Price_base*Lives_base)/sum(Lives_base),2),
                                         Price_RA = round(12/1000*sum(Price_RA*Lives_base)/sum(Lives_base),2),
                                         Price_man = round(12/1000*sum(Price_man*Lives_base)/sum(Lives_base),2),
                                         Price_RAman = round(12/1000*sum(Price_RAman*Lives_base)/sum(Lives_base),2)
),
by=c("Metal_std","HHI_flag")]
setkey(prem_Competitive,HHI_flag,Price_base)
print(prem_Competitive[Metal_std%in%c("BRONZE","SILVER","GOLD")])

#### Merger Results ####
firms = prod_pred[,list(s_base = sum(Lives_base),
                        s_noRA   = sum(Lives_RA),
                        s_noMan  = sum(Lives_man),
                        s_none   = sum(Lives_RAman)),by=c("Market","Firm")]

firms[,s_base:=s_base/sum(s_base),by=c("Market")]
firms[,s_noRA :=s_noRA/sum(s_noRA),by=c("Market")]
firms[,s_noMan:=s_noMan/sum(s_noMan),by=c("Market")]
firms[,s_none :=s_none/sum(s_none),by=c("Market")]

firms[,merger:="None"]
firms[Firm%in%c("AETNA","HUMANA"),merger:= "Aetna-Humana"]
firms[Firm%in%c("ANTHEM_BLUE_CROSS_AND_BLUE_SHIELD","BLUE_CROSS_BLUE_SHIELD_OF_GEORGIA","CIGNA_HEALTH_AND_LIFE_INSURANCE_COMPANY"),merger:= "Anthem-Cigna"]


firms[,mergeMarket:= 0]
firms[Firm%in%c("AETNA","HUMANA"),mergeMarket:= 1]
firms[,mergeMarket:=sum(mergeMarket),by="Market"]
firms[mergeMarket<2,mergeMarket:=0]
firms[mergeMarket==0&Firm%in%c("AETNA","HUMANA"),merger:="None"]

firms[,mergeMarket_2:= 0]
firms[Firm%in%c("ANTHEM_BLUE_CROSS_AND_BLUE_SHIELD","BLUE_CROSS_BLUE_SHIELD_OF_GEORGIA","CIGNA_HEALTH_AND_LIFE_INSURANCE_COMPANY"),mergeMarket_2:= 1]
firms[,mergeMarket_2:=sum(mergeMarket_2),by="Market"]
firms[mergeMarket_2<2,mergeMarket_2:=0]
firms[mergeMarket_2==0&Firm%in%c("ANTHEM_BLUE_CROSS_AND_BLUE_SHIELD","BLUE_CROSS_BLUE_SHIELD_OF_GEORGIA","CIGNA_HEALTH_AND_LIFE_INSURANCE_COMPANY"),merger:="None"]

firms[,mergeMarket:=pmax(mergeMarket,mergeMarket_2)]
firms[,mergeMarket_2:=NULL]

firms[,dHHI:=2*prod(s_base*100,na.rm=TRUE),by=c("Market","merger")]
firms[merger=="None"|mergeMarket==0,dHHI:=0]
# firms[dHHI==0,merger:="None"]




hhi = firms[,list(hhi_base = sum((s_base*100)^2),
                  hhi_noRA   = sum((s_noRA*100)^2),
                  hhi_noMan  = sum((s_noMan*100)^2),
                  hhi_none  = sum((s_none*100)^2),
                  dhhi_pred  = sum(dHHI)/2), by=c("Market")]

prod_pred[Firm%in%c("AETNA","HUMANA"),Firm:= "AETNA"]
prod_pred[Firm%in%c("ANTHEM_BLUE_CROSS_AND_BLUE_SHIELD","BLUE_CROSS_BLUE_SHIELD_OF_GEORGIA","CIGNA_HEALTH_AND_LIFE_INSURANCE_COMPANY"),Firm:="ANTHEM"]

firms_m = prod_pred[,list(s_base_m = sum(Lives_base_m),
                        s_noRA_m   = sum(Lives_RA_m),
                        s_noMan_m  = sum(Lives_man_m),
                        s_none_m   = sum(Lives_RAman_m)),by=c("Market","Firm")]

firms_m[,s_base_m :=s_base_m/sum(s_base_m),by=c("Market")]
firms_m[,s_noRA_m :=s_noRA_m/sum(s_noRA_m),by=c("Market")]
firms_m[,s_noMan_m:=s_noMan_m/sum(s_noMan_m),by=c("Market")]
firms_m[,s_none_m :=s_none_m/sum(s_none_m),by=c("Market")]

hhi_m = firms_m[,list(hhi_base_m = sum((s_base_m*100)^2),
                      hhi_noRA_m   = sum((s_noRA_m*100)^2),
                      hhi_noMan_m  = sum((s_noMan_m*100)^2),
                      hhi_none_m  = sum((s_none_m*100)^2)), by=c("Market")]

hhi = merge(hhi,hhi_m,by="Market")
hhi[,dhhi_actual:=hhi_base_m-hhi_base]

hhi[,mergerLabel:="No Merger"]
hhi[dhhi_pred>0,mergerLabel:="Weak"]
hhi[dhhi_pred>200,mergerLabel:="Strong"]

## Label Categories
prod_pred = merge(prod_pred,hhi[,c("Market","dhhi_pred","mergerLabel")],by="Market")
prod_pred = merge(prod_pred,firms[,c("Market","Firm","merger")],by=c("Market","Firm"))


table_prem = prod_pred[mergerLabel!="No Merger"&merger!="None",
                       list(prem_base = sum(Price_base*Lives_base)/sum(Lives_base),
                            prem_noRA = sum(Price_RA*Lives_RA)/sum(Lives_RA),
                            prem_noMan = sum(Price_man*Lives_man)/sum(Lives_man),
                            prem_none = sum(Price_RAman*Lives_RAman)/sum(Lives_RAman),
                            prem_base_m = sum(Price_base_m*Lives_base)/sum(Lives_base),
                            prem_noRA_m = sum(Price_RA_m*Lives_RA)/sum(Lives_RA),
                            prem_noMan_m = sum(Price_man_m*Lives_man)/sum(Lives_man),
                            prem_none_m = sum(Price_RAman_m*Lives_RAman)/sum(Lives_RAman)),by=c("Metal_std")]


table_prem[,Metal_std:=factor(Metal_std,levels=c("CATASTROPHIC","BRONZE","SILVER","GOLD","PLATINUM"))]
# table_prem = table_prem[!Metal_std%in%c("CATASTROPHIC","PLATINUM"),]
setkey(table_prem,Metal_std)


table_prem[,base_effect:=round(100*(prem_base_m-prem_base)/prem_base,1)]
table_prem[,noRA_effect:=  round(100*(prem_noRA_m-prem_noRA)/prem_noRA,1)]
table_prem[,noMan_effect:= round(100*(prem_noMan_m-prem_noMan)/prem_noMan,1)]
table_prem[,none_effect:= round(100*(prem_none_m-prem_none)/prem_none,1)]

table_prem[,Group:="Merging Parties"]

table_prem_all = prod_pred[mergerLabel!="No Merger"&merger=="None",
                           list(prem_base = sum(Price_base*Lives_base)/sum(Lives_base),
                                prem_noRA = sum(Price_RA*Lives_RA)/sum(Lives_RA),
                                prem_noMan = sum(Price_man*Lives_man)/sum(Lives_man),
                                prem_none = sum(Price_RAman*Lives_RAman)/sum(Lives_RAman),
                                prem_base_m = sum(Price_base_m*Lives_base)/sum(Lives_base),
                                prem_noRA_m = sum(Price_RA_m*Lives_RA)/sum(Lives_RA),
                                prem_noMan_m = sum(Price_man_m*Lives_man)/sum(Lives_man),
                                prem_none_m = sum(Price_RAman_m*Lives_RAman)/sum(Lives_RAman)),by=c("Metal_std")]


table_prem_all[,Metal_std:=factor(Metal_std,levels=c("CATASTROPHIC","BRONZE","SILVER","GOLD","PLATINUM"))]
# table_prem_all = table_prem_all[!Metal_std%in%c("CATASTROPHIC","PLATINUM"),]
setkey(table_prem_all,Metal_std)


table_prem_all[,base_effect:=round(100*(prem_base_m-prem_base)/prem_base,1)]
table_prem_all[,noRA_effect:=  round(100*(prem_noRA_m-prem_noRA)/prem_noRA,1)]
table_prem_all[,noMan_effect:= round(100*(prem_noMan_m-prem_noMan)/prem_noMan,1)]
table_prem_all[,none_effect:= round(100*(prem_none_m-prem_none)/prem_none,1)]


table_prem_all[,Group:="All Other Firms"]

table_prem = rbind(table_prem[,c("Metal_std","Group","base_effect","noRA_effect","noMan_effect","none_effect")],
                   table_prem_all[,c("Metal_std","Group","base_effect","noRA_effect","noMan_effect","none_effect")])

print(table_prem[Metal_std%in%c("BRONZE","SILVER","GOLD")])
hhi[dhhi_pred>0,summary(dhhi_actual)]
hhi[dhhi_pred>0,table(mergerLabel)]
