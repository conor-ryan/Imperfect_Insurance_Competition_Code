rm(list = ls())
library(data.table)
library(nleqslv)
library(ggplot2)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition/")

## Run
run = "2018-03-18"

#### Read in GCF ####
gcf = read.csv("Data/2015_MLR/2015_GCF.csv")
gcf$Market = with(gcf,paste(State,Rating.Area,sep="_"))
gcf=as.data.table(gcf)
setkey(gcf,Market) 

#### Load Simulation Data and Merge in GCF/AV data ####
simFile = paste("Simulation_Risk_Output/simData_",run,".rData",sep="")
firmRiskFile = paste("Simulation_Risk_Output/FirmRiskScores_",run,".rData",sep="")
load(simFile)
load(firmRiskFile)


# Merge in GCF
acs[,Metal:=gsub("([A-Z_]*)(CATASTROPHIC|BRONZE|SILVER|GOLD|PLATINUM)([A-Z_0-9]*)","\\2",acs$Product_Name,perl=TRUE)]
acs[,ST:=gsub("_.*","",Market)]
setkey(acs,Market)
acs = merge(acs,gcf[,c("Market","GCF")],by="Market")

# Set IDF Values
acs[Metal=="BRONZE",IDF:=1.0]
acs[Metal=="SILVER",IDF:=1.03]
acs[Metal=="GOLD",IDF:=1.08]
acs[Metal=="PLATINUM",IDF:=1.15]

# Set Regulatory Factor
acs[,Gamma_j:=IDF*GCF]

# Remove Catastrophic Plans for now. 
acs = acs[Metal!="CATASTROPHIC",]


setkey(acs,Product,Person)
setkey(predict_data,Product,Person)

#### Predict Plan Average Allowable Rating Factors ####
parFile = paste("Estimation_Output/estimationresults_",run,".csv",sep="")
pars = read.csv(parFile)


alpha = pars$pars[1]
gamma = pars$pars[2:4]
beta = matrix(pars$pars[5:16],nrow=4,ncol=3,byrow=FALSE)
sigma = pars$pars[17:21]

## Calculate alpha for each demographic ##
acs[,alpha:=alpha+beta[1,1]*Age+beta[1,2]*Family+beta[1,3]*LowIncome]


## Integrate Draws and Prediction Data
randCoeffs = as.data.table(randCoeffs)
n_draws = nrow(randCoeffs)
randCoeffs[,nu_h:=V5/abs(sigma[5])]
randCoeffs[,alpha_draw:=V2]
randCoeffs[,d_ind:=as.integer(1:n_draws)]
randCoeffs = randCoeffs[,c("d_ind","alpha_draw","nu_h")]
setkey(randCoeffs,d_ind)
setkey(predict_data,d_ind,Person)
alpha_large = randCoeffs[predict_data$d_ind,c("alpha_draw")]
nu_large = randCoeffs[predict_data$d_ind,c("nu_h")]

predict_data[,alpha_draw:=alpha_large]
predict_data[,nu_h:=nu_large]
rm(alpha_large,nu_large)

#### Full Dataset ####
setkey(acs,Product,Person)
setkey(predict_data,Product,Person,d_ind)


risk_predict = merge(acs,predict_data,by=c("Product","Person"))
risk_predict[,pref_h:=nu_h/(alpha+alpha_draw)]

prodData = acs[,list(enroll=sum(s_pred_mean*PERWT)),by=c("Product","Market","Firm","ST","Gamma_j","Metal")]
prodData[,enroll:=NULL]

#### Load Risk Function ####
riskFile = paste("Simulation_Risk_Output/riskParameters_",run,".rData",sep="")
load(riskFile)
f_list = list()
p_list = list()
for (i in 1:length(res_list)){
  res = res_list[[i]]
  f_list[[i]] = res$value
  p_list[[i]] = res$par
}

opt= which(unlist(f_list)==min(unlist(f_list)))
psi_final = res_list[[opt]]$par



risk_predict[,R:= 1+psi_final[1]*Age+psi_final[2]*Family+psi_final[3]*LowIncome+psi_final[4]*nu_h]
risk_predict[,R:=-R]
#risk_predict[,R:= -R]






#### Firm Summary Stats ####
risk_predict[,PERWT:=PERWT/nrow(randCoeffs)]
risk_predict[,enroll:=s_pred*PERWT]

R_temp = risk_predict[,list(R_j=sum(R*enroll)/sum(enroll),
                            enroll = sum(enroll),
                            enroll_Fam = sum(Family*enroll)/sum(enroll),
                            enroll_Age = sum(Age*enroll)/sum(enroll),
                            enroll_Inc = sum(LowIncome*enroll)/sum(enroll),
                            nu_h_avg = sum(nu_h*enroll)/sum(enroll),
                            pref_h_avg = sum(pref_h*enroll)/sum(enroll)),
                      by=c("Product")]




prodData = merge(prodData,R_temp,by="Product")






firmData = prodData[,list(R_f_pred=sum(enroll*R_j*Gamma_j)/sum(enroll),
                          enroll_Fam=sum(enroll_Fam*enroll)/sum(enroll),
                          enroll_Age=sum(enroll_Age*enroll)/sum(enroll),
                          enroll_Inc=sum(enroll_Inc*enroll)/sum(enroll),
                          nu_h_avg=sum(nu_h_avg*enroll)/sum(enroll)),by=c("Firm","ST")]

firmData = merge(firmData,firm_RA,by=c("Firm","ST"))
setkey(firmData,ST,Firm)

firmData[,R_bench:=NULL]
firmData[R_f==1,R_bench:=R_f_pred]
firmData[,R_bench:=max(R_bench,na.rm=TRUE),by="ST"]
firmData[,R_f_pred:=R_f_pred/R_bench]

firmData[!Firm%in%c("HEALTH_REPUBLIC_INSURANCE","ASSURANT_HEALTH"),sum((R_f_pred-R_f)^2)]


#### Monopolist Anayslis ####
prodData[,share:=enroll/sum(enroll),by="Market"]

prodData = merge(prodData,firmData[,c("Firm","ST","RA_share","R_f_pred","R_f")],by=c("Firm","ST"))

prodData[,count:=1]

firmSpreads = prodData[,list(Rmin=min(R_j),
                             R25 = quantile(R_j,probs=.25),
                             R50 = median(R_j),
                             R75 = quantile(R_j,probs=.75),
                             Rmax = max(R_j),
                             mkt_share = sum(share),
                             count = sum(count)),
                       by=c("Firm","Market","ST","RA_share","R_f_pred","R_f")]

firmSpreads[,spread1:=Rmax-Rmin]
firmSpreads[,spread2:=R75-R25]
setkey(firmSpreads,spread2)
# 
# ggplot(firmSpreads[R_f>0,]) + 
#   geom_point(aes(x=R_f,y=R_f_pred))


png("Writing/Images/EstRiskSpread.png",width=2000,height=1500,res=275)
ggplot(firmSpreads) + 
  geom_point(aes(x=mkt_share,y=spread1)) +  
  xlab("Market Share") + 
  ylab("Estimated Risk Spread Among Plans") +
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_rect(color=grey(.5)),
    legend.title=element_blank(),
    legend.text = element_text(size=18),
    legend.key.width = unit(.075,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "none",
    axis.title=element_text(size=12),
    axis.text = element_text(size=12))
dev.off()





