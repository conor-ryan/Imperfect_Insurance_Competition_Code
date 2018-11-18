rm(list=ls())
library(ggplot2)
library(scales)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition")


##### Basic Example ####
df = data.frame(x=seq(0,1,length.out=100),P=seq(8000,1000,length.out=100))
df$AC = 5000 - 1000*df$x
df$MC = 5000 - 2000*df$x
df$MR = df$P - 4000*df$x

PCloss = data.frame(x=c(.5,.5,.6),y=c(4000,4500,3800))

Mloss = data.frame(x=c(.33333,.33333,.6),y=c(4333.33,5666.667,3800))

png("Writing/Images/SelectionExampleBasic_1.png",width=2500,height=1500,res=275)
ggplot(df) +
  geom_line(aes(x=x,y=P,color="Demand Curve")) +
  geom_text(aes(x=.25,y=6500),label="D(P)",parse=TRUE,vjust=0,size=6) + 
  geom_line(aes(x=x,y=AC,color="Average Cost")) +
  geom_text(aes(x=.865,y=4250),label="AC",parse=TRUE,vjust=0,size=6) + 
  geom_line(aes(x=x,y=MC,color="Marginal Cost")) +
  geom_text(aes(x=.865,y=3400),label="MC",parse=TRUE,vjust=0,size=6) + 
  # geom_line(aes(x=x,y=MR,color="Marginal Revenue")) +
  # geom_text(aes(x=.13,y=6000),label="MR",parse=TRUE,vjust=0,size=6) + 
  # geom_point(aes(x=.6,y=3800),size=2) +
  # geom_text(aes(x=.6,y=3800),label="p^w",parse=TRUE,vjust=0,size=6) + 
  # geom_point(aes(x=.5,y=4500),size=2) + 
  # geom_text(aes(x=.5,y=4500),label="p^c",parse=TRUE,vjust=0,size=6) + 
  # geom_segment(x=.3333,xend=.333,y=4333.33,yend=5666.667,linetype=2) + 
  # geom_point(aes(x=.3333,y=5666.667),size=2) + 
  # geom_text(aes(x=.33333,y=5666.667),label="p^m",parse=TRUE,vjust=0,size=6) + 
  coord_cartesian(xlim=c(.1,.9),ylim=c(3000,8000)) +
  scale_y_continuous(label=dollar) + 
  xlab("Q(P)") + 
  ylab("") + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "none",
    axis.title=element_text(size=12),
    axis.text = element_text(size=12))
dev.off()

png("Writing/Images/SelectionExampleBasic_2.png",width=2500,height=1500,res=275)
ggplot(df) +
  geom_line(aes(x=x,y=P,color="Demand Curve")) +
  geom_text(aes(x=.25,y=6500),label="D(P)",parse=TRUE,vjust=0,size=6) + 
  geom_line(aes(x=x,y=AC,color="Average Cost")) +
  geom_text(aes(x=.865,y=4250),label="AC",parse=TRUE,vjust=0,size=6) + 
  geom_line(aes(x=x,y=MC,color="Marginal Cost")) +
  geom_text(aes(x=.865,y=3400),label="MC",parse=TRUE,vjust=0,size=6) + 
  # geom_line(aes(x=x,y=MR,color="Marginal Revenue")) +
  # geom_text(aes(x=.13,y=6000),label="MR",parse=TRUE,vjust=0,size=6) + 
  geom_point(aes(x=.6,y=3800),size=2) +
  geom_text(aes(x=.6,y=3800),label="p^w",parse=TRUE,vjust=0,size=6) + 
  geom_point(aes(x=.5,y=4500),size=2) + 
  geom_text(aes(x=.5,y=4500),label="p^c",parse=TRUE,vjust=0,size=6) + 
  # geom_segment(x=.3333,xend=.333,y=4333.33,yend=5666.667,linetype=2) + 
  # geom_point(aes(x=.3333,y=5666.667),size=2) + 
  # geom_text(aes(x=.33333,y=5666.667),label="p^m",parse=TRUE,vjust=0,size=6) + 
  coord_cartesian(xlim=c(.1,.9),ylim=c(3000,8000)) +
  scale_y_continuous(label=dollar) + 
  xlab("Q(P)") + 
  ylab("") + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "none",
    axis.title=element_text(size=12),
    axis.text = element_text(size=12))
dev.off()

png("Writing/Images/SelectionExampleBasic_3.png",width=2500,height=1500,res=275)
ggplot(df) +
  geom_line(aes(x=x,y=P,color="Demand Curve")) +
  geom_text(aes(x=.25,y=6500),label="D(P)",parse=TRUE,vjust=0,size=6) + 
  geom_line(aes(x=x,y=AC,color="Average Cost")) +
  geom_text(aes(x=.865,y=4250),label="AC",parse=TRUE,vjust=0,size=6) + 
  geom_line(aes(x=x,y=MC,color="Marginal Cost")) +
  geom_text(aes(x=.865,y=3400),label="MC",parse=TRUE,vjust=0,size=6) + 
  geom_polygon(data=PCloss,aes(x=x,y=y),fill=grey(.45)) + 
  # geom_line(aes(x=x,y=MR,color="Marginal Revenue")) +
  # geom_text(aes(x=.13,y=6000),label="MR",parse=TRUE,vjust=0,size=6) + 
  geom_point(aes(x=.6,y=3800),size=2) +
  geom_text(aes(x=.6,y=3800),label="p^w",parse=TRUE,vjust=0,size=6) + 
  geom_point(aes(x=.5,y=4500),size=2) + 
  geom_text(aes(x=.5,y=4500),label="p^c",parse=TRUE,vjust=0,size=6) + 
  # geom_segment(x=.3333,xend=.333,y=4333.33,yend=5666.667,linetype=2) + 
  # geom_point(aes(x=.3333,y=5666.667),size=2) + 
  # geom_text(aes(x=.33333,y=5666.667),label="p^m",parse=TRUE,vjust=0,size=6) + 
  coord_cartesian(xlim=c(.1,.9),ylim=c(3000,8000)) +
  scale_y_continuous(label=dollar) + 
  xlab("Q(P)") + 
  ylab("") + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "none",
    axis.title=element_text(size=12),
    axis.text = element_text(size=12))
dev.off()

png("Writing/Images/SelectionExampleBasic_4.png",width=2500,height=1500,res=275)
ggplot(df) +
  geom_line(aes(x=x,y=P,color="Demand Curve")) +
  geom_text(aes(x=.25,y=6500),label="D(P)",parse=TRUE,vjust=0,size=6) + 
  geom_line(aes(x=x,y=AC,color="Average Cost")) +
  geom_text(aes(x=.865,y=4250),label="AC",parse=TRUE,vjust=0,size=6) + 
  geom_line(aes(x=x,y=MC,color="Marginal Cost")) +
  geom_text(aes(x=.865,y=3400),label="MC",parse=TRUE,vjust=0,size=6) + 
  geom_line(aes(x=x,y=MR,color="Marginal Revenue")) +
  geom_text(aes(x=.13,y=6000),label="MR",parse=TRUE,vjust=0,size=6) +
  geom_point(aes(x=.6,y=3800),size=2) +
  geom_text(aes(x=.6,y=3800),label="p^w",parse=TRUE,vjust=0,size=6) + 
  geom_point(aes(x=.5,y=4500),size=2) + 
  geom_text(aes(x=.5,y=4500),label="p^c",parse=TRUE,vjust=0,size=6) + 
  geom_segment(x=.3333,xend=.333,y=4333.33,yend=5666.667,linetype=2) +
  geom_point(aes(x=.3333,y=5666.667),size=2) +
  geom_text(aes(x=.33333,y=5666.667),label="p^m",parse=TRUE,vjust=0,size=6) +
  coord_cartesian(xlim=c(.1,.9),ylim=c(3000,8000)) +
  scale_y_continuous(label=dollar) + 
  xlab("Q(P)") + 
  ylab("") + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "none",
    axis.title=element_text(size=12),
    axis.text = element_text(size=12))
dev.off()


png("Writing/Images/SelectionExampleBasic_5.png",width=2500,height=1500,res=275)
ggplot(df) +
  geom_line(aes(x=x,y=P,color="Demand Curve")) +
  geom_text(aes(x=.25,y=6500),label="D(P)",parse=TRUE,vjust=0,size=6) + 
  geom_line(aes(x=x,y=AC,color="Average Cost")) +
  geom_text(aes(x=.865,y=4250),label="AC",parse=TRUE,vjust=0,size=6) + 
  geom_line(aes(x=x,y=MC,color="Marginal Cost")) +
  geom_text(aes(x=.865,y=3400),label="MC",parse=TRUE,vjust=0,size=6) + 
  geom_line(aes(x=x,y=MR,color="Marginal Revenue")) +
  geom_text(aes(x=.13,y=6000),label="MR",parse=TRUE,vjust=0,size=6) +
  geom_polygon(data=Mloss,aes(x=x,y=y),fill=grey(.45)) + 
  geom_point(aes(x=.6,y=3800),size=2) +
  geom_text(aes(x=.6,y=3800),label="p^w",parse=TRUE,vjust=0,size=6) + 
  geom_point(aes(x=.5,y=4500),size=2) + 
  geom_text(aes(x=.5,y=4500),label="p^c",parse=TRUE,vjust=0,size=6) + 
  geom_segment(x=.3333,xend=.333,y=4333.33,yend=5666.667,linetype=2) +
  geom_point(aes(x=.3333,y=5666.667),size=2) +
  geom_text(aes(x=.33333,y=5666.667),label="p^m",parse=TRUE,vjust=0,size=6) +
  coord_cartesian(xlim=c(.1,.9),ylim=c(3000,8000)) +
  scale_y_continuous(label=dollar) + 
  xlab("Q(P)") + 
  ylab("") + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "none",
    axis.title=element_text(size=12),
    axis.text = element_text(size=12))
dev.off()

##### Sorting Example ####
df = data.frame(x=seq(0,.5,length.out=100),dP=seq(5000,0,length.out=100))
df$AC = 4000 - 4000*df$x
df$MC = 2500 - 4000*df$x
df$MSB = 5000 - 12500*df$x
df$MSB = 5000 - 12500*df$x
df$MR = 5000 - 16666.67*df$x

PCloss = data.frame(x=c(.5,.5,.6),y=c(4000,4500,3800))

Mloss = data.frame(x=c(.33333,.33333,.6),y=c(4333.33,5666.667,3800))

png("Writing/Images/SelectionExampleSorting_1.png",width=2500,height=1500,res=275)
ggplot(df) +
  geom_line(aes(x=x,y=dP,color="Demand Curve")) +
  geom_text(aes(x=.1,y=4300),label="D(Delta*p)",parse=TRUE,vjust=0,size=6) + 
  geom_line(aes(x=x,y=AC,color="Average Cost")) +
  geom_text(aes(x=.4,y=2550),label="Delta*AC",parse=TRUE,vjust=0,size=6) + 
  geom_line(aes(x=x,y=MC,color="Marginal Cost")) +
  geom_text(aes(x=.5,y=550),label="Delta*MC",parse=TRUE,vjust=0,size=6) + 
  # geom_line(aes(x=x,y=MSB,color="Marginal Revenue")) +
  # geom_text(aes(x=.35,y=500),label="Delta*MSB",parse=TRUE,vjust=0,size=6) + 
  # geom_line(aes(x=x,y=MR,color="Marginal Revenue")) +
  # geom_text(aes(x=.15,y=500),label="Delta*MSB",parse=TRUE,vjust=0,size=6) + 
  # geom_segment(x=.295,xend=.295,y=1318,yend=2050,linetype=2) +
  # geom_point(aes(x=.295,y=2050),size=2) +
  # geom_point(aes(x=.417,y=832),size=2) +
  # geom_text(aes(x=.417,y=832),label="Delta*p^w",parse=TRUE,vjust=0,size=6) +
  # geom_point(aes(x=.1666,y=3333.33),size=2) +
  # geom_text(aes(x=.1666,y=3333.33),label="Delta*p^c",parse=TRUE,vjust=0,size=6) +
  coord_cartesian(xlim=c(.05,.5),ylim=c(0,4500)) +
  scale_y_continuous(label=dollar) + 
  xlab("Enrollment in High Plan") + 
  ylab("") + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "none",
    axis.title=element_text(size=12),
    axis.text = element_text(size=12))
dev.off()

png("Writing/Images/SelectionExampleSorting_2.png",width=2500,height=1500,res=275)
ggplot(df) +
  geom_line(aes(x=x,y=dP,color="Demand Curve")) +
  geom_text(aes(x=.1,y=4300),label="D(Delta*p)",parse=TRUE,vjust=0,size=6) + 
  geom_line(aes(x=x,y=AC,color="Average Cost")) +
  geom_text(aes(x=.4,y=2550),label="Delta*AC",parse=TRUE,vjust=0,size=6) + 
  geom_line(aes(x=x,y=MC,color="Marginal Cost")) +
  geom_text(aes(x=.5,y=550),label="Delta*MC",parse=TRUE,vjust=0,size=6) + 
  geom_text(aes(x=.4,y=4000),label="Pi==Pi[min]",parse=TRUE,vjust=0,size=7) + 
  # geom_line(aes(x=x,y=MSB,color="Marginal Revenue")) +
  # geom_text(aes(x=.35,y=500),label="Delta*MSB",parse=TRUE,vjust=0,size=6) + 
  # geom_line(aes(x=x,y=MR,color="Marginal Revenue")) +
  # geom_text(aes(x=.15,y=500),label="Delta*MSB",parse=TRUE,vjust=0,size=6) + 
  # geom_segment(x=.295,xend=.295,y=1318,yend=2050,linetype=2) +
  # geom_point(aes(x=.295,y=2050),size=2) +
  geom_point(aes(x=.417,y=832),size=2) +
  geom_text(aes(x=.417,y=832),label="Delta*p^w",parse=TRUE,vjust=0,size=6) +
  # geom_point(aes(x=.1666,y=3333.33),size=2) +
  # geom_text(aes(x=.1666,y=3333.33),label="Delta*p^c",parse=TRUE,vjust=0,size=6) +
  coord_cartesian(xlim=c(.05,.5),ylim=c(0,4500)) +
  scale_y_continuous(label=dollar) + 
  xlab("Enrollment in High Plan") + 
  ylab("") + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "none",
    axis.title=element_text(size=12),
    axis.text = element_text(size=12))
dev.off()



png("Writing/Images/SelectionExampleSorting_3.png",width=2500,height=1500,res=275)
ggplot(df) +
  geom_line(aes(x=x,y=dP,color="Demand Curve")) +
  geom_text(aes(x=.1,y=4300),label="D(Delta*p)",parse=TRUE,vjust=0,size=6) + 
  geom_line(aes(x=x,y=AC,color="Average Cost")) +
  geom_text(aes(x=.4,y=2550),label="Delta*AC",parse=TRUE,vjust=0,size=6) + 
  geom_line(aes(x=x,y=MC,color="Marginal Cost")) +
  geom_text(aes(x=.5,y=550),label="Delta*MC",parse=TRUE,vjust=0,size=6) + 
  # geom_line(aes(x=x,y=MSB,color="Marginal Revenue")) +
  # geom_text(aes(x=.35,y=500),label="Delta*MSB",parse=TRUE,vjust=0,size=6) +
  geom_text(aes(x=.4,y=4000),label="Pi==0",parse=TRUE,vjust=0,size=7) + 
  # geom_line(aes(x=x,y=MR,color="Marginal Revenue")) +
  # geom_text(aes(x=.15,y=500),label="Delta*MSB",parse=TRUE,vjust=0,size=6) +
  # geom_text(aes(x=.295,y=2050),label="Delta*p^w",parse=TRUE,vjust=0,size=6) +
  # geom_segment(x=.295,xend=.295,y=1318,yend=2050,linetype=2) +
  # geom_point(aes(x=.295,y=2050),size=2) +
  # geom_point(aes(x=.417,y=832),size=2) +
  # geom_text(aes(x=.417,y=832),label="Delta*p^w",parse=TRUE,vjust=0,size=6) +
  geom_point(aes(x=.1666,y=3333.33),size=2) +
  geom_text(aes(x=.1666,y=3333.33),label="Delta*p^c",parse=TRUE,vjust=0,size=6) +
  coord_cartesian(xlim=c(.05,.5),ylim=c(0,4500)) +
  scale_y_continuous(label=dollar) + 
  xlab("Enrollment in High Plan") + 
  ylab("") + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "none",
    axis.title=element_text(size=12),
    axis.text = element_text(size=12))
dev.off()

png("Writing/Images/SelectionExampleSorting_4.png",width=2500,height=1500,res=275)
ggplot(df) +
  geom_line(aes(x=x,y=dP,color="Demand Curve")) +
  geom_text(aes(x=.1,y=4300),label="D(Delta*p)",parse=TRUE,vjust=0,size=6) + 
  geom_line(aes(x=x,y=AC,color="Average Cost")) +
  geom_text(aes(x=.4,y=2550),label="Delta*AC",parse=TRUE,vjust=0,size=6) + 
  geom_line(aes(x=x,y=MC,color="Marginal Cost")) +
  geom_text(aes(x=.5,y=550),label="Delta*MC",parse=TRUE,vjust=0,size=6) + 
  geom_line(aes(x=x,y=MSB,color="Marginal Revenue")) +
  geom_text(aes(x=.35,y=500),label="Delta*MSB",parse=TRUE,vjust=0,size=6) +
  geom_text(aes(x=.4,y=4000),label="Pi==0",parse=TRUE,vjust=0,size=7) + 
  # geom_line(aes(x=x,y=MR,color="Marginal Revenue")) +
  # geom_text(aes(x=.15,y=500),label="Delta*MSB",parse=TRUE,vjust=0,size=6) +
  geom_text(aes(x=.295,y=2050),label="Delta*p^w",parse=TRUE,vjust=0,size=6) +
  geom_segment(x=.295,xend=.295,y=1318,yend=2050,linetype=2) +
  geom_point(aes(x=.295,y=2050),size=2) +
  # geom_point(aes(x=.417,y=832),size=2) +
  # geom_text(aes(x=.417,y=832),label="Delta*p^w",parse=TRUE,vjust=0,size=6) +
  geom_point(aes(x=.1666,y=3333.33),size=2) +
  geom_text(aes(x=.1666,y=3333.33),label="Delta*p^c",parse=TRUE,vjust=0,size=6) +
  coord_cartesian(xlim=c(.05,.5),ylim=c(0,4500)) +
  scale_y_continuous(label=dollar) + 
  xlab("Enrollment in High Plan") + 
  ylab("") + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "none",
    axis.title=element_text(size=12),
    axis.text = element_text(size=12))
dev.off()


png("Writing/Images/SelectionExampleSorting_5.png",width=2500,height=1500,res=275)
ggplot(df) +
  geom_line(aes(x=x,y=dP,color="Demand Curve")) +
  geom_text(aes(x=.1,y=4300),label="D(Delta*p)",parse=TRUE,vjust=0,size=6) + 
  geom_line(aes(x=x,y=AC,color="Average Cost")) +
  geom_text(aes(x=.4,y=2550),label="Delta*AC",parse=TRUE,vjust=0,size=6) + 
  geom_line(aes(x=x,y=MC,color="Marginal Cost")) +
  geom_text(aes(x=.5,y=550),label="Delta*MC",parse=TRUE,vjust=0,size=6) + 
  # geom_line(aes(x=x,y=MSB,color="Marginal Revenue")) +
  # geom_text(aes(x=.35,y=500),label="Delta*MSB",parse=TRUE,vjust=0,size=6) +
  geom_text(aes(x=.4,y=4000),label="Pi==Pi[max]",parse=TRUE,vjust=0,size=7) + 
  geom_line(aes(x=x,y=MR,color="Marginal Revenue")) +
  geom_text(aes(x=.2,y=500),label="Delta*MSB==Delta*MR",parse=TRUE,vjust=0,size=6) +
  geom_text(aes(x=.197,y=3030),label="Delta*p^w",parse=TRUE,vjust=0,size=6) +
  geom_segment(x=.197,xend=.197,y=1715,yend=3030,linetype=2) +
  geom_point(aes(x=.197,y=3030),size=2) +
  # geom_point(aes(x=.417,y=832),size=2) +
  # geom_text(aes(x=.417,y=832),label="Delta*p^w",parse=TRUE,vjust=0,size=6) +
  geom_point(aes(x=.1666,y=3333.33),size=2) +
  geom_text(aes(x=.1666,y=3333.33),label="Delta*p^c",parse=TRUE,vjust=0,size=6) +
  coord_cartesian(xlim=c(.05,.5),ylim=c(0,4500)) +
  scale_y_continuous(label=dollar) + 
  xlab("Enrollment in High Plan") + 
  ylab("") + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "none",
    axis.title=element_text(size=12),
    axis.text = element_text(size=12))
dev.off()

#### Two Product Example ####
df = data.frame(x=seq(0,1,length.out=100),P_l=seq(8000,1000,length.out=100))
df$AC_l = 5000 - 1000*df$x
df$MC_l = 5000 - 2000*df$x
df$Mkup_l = 5500 - 800*df$x

df$P_h = 7500 + 800*df$x
df$MC_h = 7000 + 500*df$x

plot_basic = ggplot(df) +
  geom_line(aes(x=x,y=P_l,color="Demand Curve")) +
  geom_line(aes(x=x,y=MC_l,color="Marginal Cost")) +
  geom_line(aes(x=x,y=Mkup_l,color="MC + Markup")) +
  geom_line(aes(x=x,y=MC_h,color="Marginal Cost")) +
  geom_line(aes(x=x,y=P_h,color="MC + Markup")) +
  geom_point(aes(x=.405,y=5175),size=2) + 
  geom_text(aes(x=.405,y=5175),label="p[l]^{1}",parse=TRUE,vjust=1.2,size=6) + 
  geom_point(aes(x=.405,y=7825),size=2) + 
  geom_text(aes(x=.405,y=7825),label="p[h]^{1}",parse=TRUE,vjust=-.2,size=6) + 
  geom_segment(aes(x=.405,xend=.405,y=3000,yend=9000),linetype=3) + 
  scale_y_continuous(label=dollar) + 
  coord_cartesian(xlim=c(.1,.9),ylim=c(3000,9000)) +
  xlab(expression(Q(P[l],P[h](P[l])))) + 
  ylab("") + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "right",
    axis.title=element_text(size=12),
    axis.text = element_text(size=12))

png("Writing/Images/SelectionExample_MP1a.png",width=2500,height=1500,res=275)
print(plot_basic)
dev.off()

png("Writing/Images/SelectionExample_MP1b.png",width=2500,height=1500,res=275)
print(plot_basic + 
        geom_text(aes(x=.7,y=5000),label="MC[l](p[l],p[h](p[l])) + Mkup[l](p[l],p[h](p[l]))",parse=TRUE,size=6) + 
        geom_text(aes(x=.75,y=3300),label="MC[l](p[l],p[h](p[l]))",parse=TRUE,size=6) + 
        geom_text(aes(x=.7,y=8200),label="p[h](p[l])",parse=TRUE,size=6) + 
        geom_text(aes(x=.75,y=7200),label="MC[h](p[l],p[h](p[l]))",parse=TRUE,size=6)  )
dev.off()

png("Writing/Images/SelectionExample_MP1c.png",width=2500,height=1500,res=275)
print(plot_basic + 
        geom_text(aes(x=.7,y=5000),label="MC[l](p[l],p[h](p[l])) + Mkup[l](p[l],p[h](p[l]))",parse=TRUE,size=6) + 
        geom_text(aes(x=.75,y=3300),label="MC[l](p[l],p[h](p[l]))",parse=TRUE,size=6) + 
        geom_text(aes(x=.7,y=8200),label="p[h](p[l])",parse=TRUE,size=6) + 
        geom_text(aes(x=.75,y=7200),label="MC[h](p[l],p[h](p[l]))",parse=TRUE,size=6)  + 
        geom_text(aes(x=.405,y=4200),label="MC[l](p[l]^{1},p[h]^{1})",parse=TRUE,vjust=1.2,size=6) + 
        geom_point(aes(x=.405,y=7200),size=2) + 
        geom_text(aes(x=.405,y=7200),label="MC[h](p[l]^{1},p[h]^{1})",parse=TRUE,vjust=-.2,size=6) + 
  geom_point(aes(x=.405,y=4200),size=2))
dev.off()


png("Writing/Images/SelectionExample_MP2.png",width=2500,height=1500,res=275)
ggplot(df) +
  geom_line(aes(x=x,y=P_l,color="Demand Curve")) +
  geom_line(aes(x=x,y=MC_l,color="Marginal Cost")) +
  geom_line(aes(x=x,y=Mkup_l,color="MC + Markup")) +
  geom_line(aes(x=x,y=Mkup_l+500,color="MC + Markup"),linetype=2) +
  geom_line(aes(x=x,y=MC_h,color="Marginal Cost")) +
  geom_line(aes(x=x,y=P_h,color="MC + Markup")) +
  geom_line(aes(x=x,y=P_h+200,color="MC + Markup"),linetype=2) +
  geom_point(aes(x=.405,y=5175),size=2) + 
  geom_text(aes(x=.405,y=5175),label="p[l]",parse=TRUE,vjust=1.2,size=6) + 
  geom_point(aes(x=.405,y=7825),size=2) + 
  geom_text(aes(x=.405,y=7825),label="p[h]",parse=TRUE,vjust=-.2,size=6) + 
  geom_segment(aes(x=.25,xend=.25,y=5300,yend=5700,color="MC + Markup"),
               arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed")) + 
  geom_segment(aes(x=.25,xend=.25,y=7700,yend=7900,color="MC + Markup"),
               arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed")) + 
  scale_y_continuous(label=dollar) + 
  coord_cartesian(xlim=c(.1,.9),ylim=c(3000,9000)) +
  xlab(expression(Q(P[l],P[h](P[l])))) + 
  ylab("") + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "right",
    axis.title=element_text(size=12),
    axis.text = element_text(size=12))
dev.off()

png("Writing/Images/SelectionExample_MP3.png",width=2500,height=1500,res=275)
ggplot(df) +
  geom_line(aes(x=x,y=P_l,color="Demand Curve")) +
  geom_line(aes(x=x,y=MC_l,color="Marginal Cost")) +
  geom_line(aes(x=x,y=MC_l+200,color="Marginal Cost"),linetype=2) +
  geom_line(aes(x=x,y=Mkup_l,color="MC + Markup")) +
  geom_line(aes(x=x,y=Mkup_l+600,color="MC + Markup"),linetype=2) +
  geom_line(aes(x=x,y=MC_h,color="Marginal Cost")) +
  geom_line(aes(x=x,y=MC_h-600,color="Marginal Cost"),linetype=2) +
  geom_line(aes(x=x,y=P_h,color="MC + Markup")) +
  geom_line(aes(x=x,y=P_h-200,color="MC + Markup"),linetype=2) +
  geom_segment(aes(x=.25,xend=.25,y=5300,yend=5700,color="MC + Markup"),
               arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed")) + 
  geom_segment(aes(x=.25,xend=.25,y=7700,yend=7900,color="MC + Markup"),
               arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed")) + 
  geom_segment(aes(x=.28,xend=.28,y=5700,yend=5800,color="MC + Markup"),
               arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed")) + 
  geom_segment(aes(x=.28,xend=.28,y=7900,yend=7500,color="MC + Markup"),
               arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed")) + 
  geom_segment(aes(x=.28,xend=.28,y=4450,yend=4650,color="Marginal Cost"),
               arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed")) + 
  geom_segment(aes(x=.28,xend=.28,y=7130,yend=6530,color="Marginal Cost"),
               arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed")) + 
  geom_point(aes(x=.405,y=5175),size=2) + 
  geom_text(aes(x=.405,y=5175),label="p[l]",parse=TRUE,vjust=1.2,size=6) + 
  geom_point(aes(x=.405,y=7825),size=2) + 
  geom_text(aes(x=.405,y=7825),label="p[h]",parse=TRUE,vjust=-.2,size=6) + 
  #geom_segment(aes(x=.405,xend=.405,y=5175,yend=7825),linetype=2) + 
  scale_y_continuous(label=dollar) + 
  coord_cartesian(xlim=c(.1,.9),ylim=c(3000,9000)) +
  xlab(expression(Q(P[l],P[h](P[l])))) + 
  ylab("") + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "right",
    axis.title=element_text(size=12),
    axis.text = element_text(size=12))
dev.off()

png("Writing/Images/SelectionExample_MP4.png",width=2500,height=1500,res=275)
ggplot(df) +
  geom_line(aes(x=x,y=P_l,color="Demand Curve")) +
  geom_line(aes(x=x,y=P_l+600,color="Demand Curve"),linetype=2) +
  geom_line(aes(x=x,y=MC_l,color="Marginal Cost")) +
  geom_line(aes(x=x,y=MC_l+200,color="Marginal Cost"),linetype=2) +
  geom_line(aes(x=x,y=Mkup_l,color="MC + Markup")) +
  geom_line(aes(x=x,y=Mkup_l+600,color="MC + Markup"),linetype=2) +
  geom_line(aes(x=x,y=MC_h,color="Marginal Cost")) +
  geom_line(aes(x=x,y=MC_h-600,color="Marginal Cost"),linetype=2) +
  geom_line(aes(x=x,y=P_h,color="MC + Markup")) +
  geom_line(aes(x=x,y=P_h-200,color="MC + Markup"),linetype=2) +
  geom_point(aes(x=.405,y=5775),size=2) + 
  geom_text(aes(x=.405,y=5775),label="p[l]^{2}",parse=TRUE,vjust=-.2,hjust=-.2,size=6) + 
  geom_point(aes(x=.405,y=5175),size=2) + 
  geom_text(aes(x=.405,y=5175),label="p[l]^{1}",parse=TRUE,vjust=1.2,size=6) + 
  geom_point(aes(x=.405,y=7825),size=2) + 
  geom_text(aes(x=.405,y=7825),label="p[h]^{1}",parse=TRUE,vjust=-.2,size=6) + 
  geom_point(aes(x=.405,y=7625),size=2) + 
  geom_text(aes(x=.405,y=7625),label="p[h]^{2}",parse=TRUE,vjust=1,hjust=-.2,size=6) +
  geom_segment(aes(x=.25,xend=.25,y=5300,yend=5700,color="MC + Markup"),
               arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed")) + 
  geom_segment(aes(x=.25,xend=.25,y=7700,yend=7900,color="MC + Markup"),
               arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed")) + 
  geom_segment(aes(x=.28,xend=.28,y=5700,yend=5800,color="MC + Markup"),
               arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed")) + 
  geom_segment(aes(x=.28,xend=.28,y=7900,yend=7500,color="MC + Markup"),
               arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed")) + 
  geom_segment(aes(x=.28,xend=.28,y=4450,yend=4650,color="Marginal Cost"),
               arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed")) + 
  geom_segment(aes(x=.28,xend=.28,y=7130,yend=6530,color="Marginal Cost"),
               arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed")) + 
  geom_segment(aes(x=.2525,xend=.33,y=6232,yend=6232,color="Demand Curve"),
               arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed")) + 
  #geom_segment(aes(x=.405,xend=.405,y=5175,yend=7825),linetype=2) + 
  scale_y_continuous(label=dollar) + 
  coord_cartesian(xlim=c(.1,.9),ylim=c(3000,9000)) +
  xlab(expression(Q(P[l],P[h](P[l])))) + 
  xlab("Q(P)") + 
  ylab("") + 
  theme(#panel.background = element_rect(color=grey(.2),fill=grey(.9)),
    strip.background = element_blank(),
    #panel.grid.major = element_line(color=grey(.8)),
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.text = element_text(size=14),
    legend.key.width = unit(.05,units="npc"),
    legend.key = element_rect(color="transparent",fill="transparent"),
    legend.position = "right",
    axis.title=element_text(size=12),
    axis.text = element_text(size=12))
dev.off()







