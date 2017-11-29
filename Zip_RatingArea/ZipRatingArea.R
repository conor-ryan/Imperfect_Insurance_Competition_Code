rm(list=ls())
library(ggplot2)
library(scales)
library(extrafont)
library(grid)
library(doBy)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition")

#Rating Area - Zip Code Match File
areas = read.csv("Data/Zip_RatingArea/Marketplace_premium_databook_edit2017.csv")

#### Output Counties ####
counties = unique(areas[,c("ST","FIPS","County","RatingArea")])
counties$COUNTYFIPS = counties$FIPS%%1000
counties$STATEFIP = floor(counties$FIPS/1000)

write.csv(counties,"Intermediate_Output/Zip_RatingArea/County_to_RatingArea.csv",row.names=FALSE)

#### Output PUMA ####
pumaMap = read.csv("Data/Zip_RatingArea/PUMA_to_County.csv")
puma = merge(counties,pumaMap,by.x="FIPS",by.y="county",all=TRUE)
puma = puma[!puma$ST%in%c("PR","VI"),]
puma = summaryBy(afact+pop10 ~ ST + STATEFIP + puma12 + RatingArea,data=puma,FUN=sum,keep.names=TRUE)
puma$maxPop = ave(puma$afact,puma$ST,puma$puma12,FUN=max)

puma = puma[puma$afact==puma$maxPop,]
puma = puma[,c("ST","STATEFIP","puma12","RatingArea")]
names(puma) = c("ST","STATEFIP","PUMA","RatingArea")

write.csv(puma,"Intermediate_Output/Zip_RatingArea/PUMA_to_RatingArea.csv",row.names=FALSE)

#### Output by Zipcode ####
#Match Population Numbers
pop = read.csv("Data/Zip_RatingArea/State_Zip.csv",skip=1)
names(pop) = c("State","Zip","ST","zipname","pop","alloc")
areas = merge(areas,pop[,c("ST","Zip","pop")],by=c("ST","Zip"),all.x=TRUE)

#Assume that zip codes missing from population file have 0 population
#Should include county matches here
areas$pop[is.na(areas$pop)]=0

#Match to 3-digit Zip
areas$Zip = as.character(areas$Zip)
areas$Zip[nchar(areas$Zip)==3] = paste("00",areas$Zip[nchar(areas$Zip)==3],sep="")
areas$Zip[nchar(areas$Zip)==4] = paste("0",areas$Zip[nchar(areas$Zip)==4],sep="")

areas$Zip3 = substr(as.character(areas$Zip),0,3)

#Zip3 to Rating Area Mapping
areas = summaryBy(pop~ST+Zip3+RatingArea,data=areas,FUN=sum,keep.names=TRUE)


# Ideally we would like to wieght the match by population. For simplicity, 
# we will initially assign rating area based on largest population.

#Also, this match should be done vis-a-vis county matching. Since zip codes
#can spread across counties and the rating area deisgnation is county specific
#I think this can be important for population weighting in some cases.
# areas$maxPop = ave(areas$pop,areas$Zip3,areas$ST,FUN=max)
# areas$alloc = with(areas,pop==maxPop)
# areas = areas[areas$alloc,c("ST","Zip3","RatingArea")]

# #Weighted mapping based on population
areas$totalPop = ave(areas$pop,areas$Zip3,FUN=sum)
areas$alloc = with(areas,pop/totalPop)
areas$alloc[areas$totalPop==0] = 1


# For now, an imperfect mapping.
write.csv(areas,"Intermediate_Output/Zip_RatingArea/Zip3_to_RatingArea.csv",row.names=FALSE)

