rm(list=ls())
library(ggplot2)
library(scales)
setwd("C:/Users/Conor/Documents/Research/Imperfect_Insurance_Competition")


##### Basic Example ####
df = data.frame(x=seq(0,1,length.out=100),P=seq(8000,1000,length.out=100))
df$AC = 5000 - 1000*df$x
df$MC = 5000 - 2000*df$x
df$Mkup = df$MC + 500 + 2000*df$x^2

#png("Writing/Images/SelectionExampleBasic.png",width=2500,height=1500,res=275)
ggplot(df) +
  geom_line(aes(x=x,y=P,color="Demand Curve")) +
  geom_line(aes(x=x,y=AC,color="Average Cost")) +
  geom_line(aes(x=x,y=MC,color="Marginal Cost")) +
  geom_line(aes(x=x,y=Mkup,color="MC + Markup")) +
  geom_point(aes(x=.6,y=3800),size=2) +
  geom_text(aes(x=.6,y=3800),label="p^w",parse=TRUE,vjust=0,size=6) + 
  geom_point(aes(x=.5,y=4500),size=2) + 
  geom_text(aes(x=.5,y=4500),label="p^c",parse=TRUE,vjust=0,size=6) + 
  geom_point(aes(x=.43,y=5000),size=2) + 
  geom_text(aes(x=.43,y=5000),label="p^m",parse=TRUE,vjust=0,size=6) + 
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
    legend.position = "right",
    axis.title=element_text(size=12),
    axis.text = element_text(size=12))
#dev.off()

png("Writing/Images/SelectionExampleBasic_Labeled.png",width=2500,height=1500,res=275)
ggplot(df) +
  geom_line(aes(x=x,y=P,color="Demand Curve")) +
  geom_line(aes(x=x,y=AC,color="Average Cost")) +
  geom_line(aes(x=x,y=MC,color="Marginal Cost")) +
  geom_line(aes(x=x,y=Mkup,color="MC + Markup")) +
  geom_point(aes(x=.6,y=3800),size=2) +
  geom_text(aes(x=.6,y=3800),label="p^w",parse=TRUE,vjust=0,size=6) + 
  geom_point(aes(x=.5,y=4500),size=2) + 
  geom_text(aes(x=.5,y=4500),label="p^c",parse=TRUE,vjust=0,size=6) + 
  geom_point(aes(x=.405,y=5175),size=2) + 
  geom_text(aes(x=.4,y=5175),label="p^m",parse=TRUE,vjust=0,size=6) +
  geom_text(aes(x=.125,y=7000),label="P[l]",parse=TRUE,vjust=0,size=6) + 
  geom_text(aes(x=.78,y=4800),label="MC(P[l]) + Markup(P[l])",parse=TRUE,vjust=0,size=6) + 
  geom_text(aes(x=.8,y=4200),label="AC(P[l])",parse=TRUE,vjust=0,size=6) + 
  geom_text(aes(x=.8,y=3400),label="MC(P[l])",parse=TRUE,vjust=0,size=6) + 
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
    legend.position = "right",
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







