#--------------------------------------------------------------
# Topic: Plotting Iceland 1882, mortality crises and lifespan variation
# Author: Serena Vigezzi
# Date: 27/10/2020
#--------------------------------------------------------------

source("Path/Handling data, Iceland 1882 - F.R")
source("Path/Handling data, Iceland 1882 - M.R")

source("Path/Plotting functions.R")

# Log-mortality rates
#---------------------

library(ggplot2)
library(ggthemes)
library(ggpubr)
library(scales)
library(gtable)

cbbPalette <- c("#91E0E0", "#DC143C", "#000000", "#008080")

# Females
png("Path/Log(mx), Iceland 1882 - F.png",
    width=1000,height=700)

plot.mx1(data.prior=Isl82f5.80.mean.prior,
         data.crisis=Isl1882f5.80,data.after=Isl82f5.80.mean.after,
         period=c("1877-81", "1882", "1883-87"),
         title=bquote('Icelandic females 1882'),
         palette=c("#91E0E0", "#DC143C", "#000000", "#008080"),
         legend=c(0.3,0.75))

dev.off()

# Males
png("Path/Log(mx), Iceland 1882 - M.png",
    width=1000,height=700)

plot.mx1(data.prior=Isl82m5.80.mean.prior,
         data.crisis=Isl1882m5.80,data.after=Isl82m5.80.mean.after,
         period=c("1877-81", "1882", "1883-87"),
         title=bquote('Icelandic males 1882'),
         palette=c("#91E0E0", "#DC143C", "#000000", "#008080"),
         legend=c(0.3,0.75))

dev.off()


# Variability
#-------------

png("Path/Absolute, Iceland 1882.png",
    width=1000,height=700)

abs.plot(sdf.data=sd5f.80.years,aGf.data=aGini5f.80.years,edf.data=edag5f.80.years,
         sdm.data=sd5m.80.years,aGm.data=aGini5m.80.years,edm.data=edag5m.80.years,
         Years=1877:1887,palette=c("#022A2A", "#008080", "#91E0E0"),
         xminplot=1881.5,xmaxplot=1882.5,
         xmin1=1878,xmax1=1880,ymin1=0,ymax1=8,
         xmin2=1884,xmax2=1886,ymin2=5,ymax2=7,
         ylim=c(-5,35))

dev.off


# Relative

png("Path/Relative, Iceland 1882.png",
    width=1000,height=700)

rel.plot(cvf.data=cv5f.80.years,rGf.data=rGini5f.80.years,hf.data=entropy5f.80.years,
         cvm.data=cv5m.80.years,rGm.data=rGini5m.80.years,hm.data=entropy5m.80.years,
         Years=1877:1887,palette=c("#540700", "#E34234", "#FFC9C7"),
         xminplot=1881.5,xmaxplot=1882.5,
         xmin1=1878,xmax1=1880,ymin1=1.2,ymax1=1.4)

dev.off

# Decomposition
#---------------

library(DemoDecomp)
library(tidyverse)
library(grid)
library(ggplot2)
library(ggpubr)
library(patchwork)
library(ggthemes)
library(colorspace)

source("Path/Decomposition functions.R")

# Edagger
#---------

merged_e <- dec.crisis1(func=edag5.dec,
                        parsf.prior=Isl82f5.80.mean.prior$mx,
                        parsf.crisis=Isl1882f5.80$mx,
                        parsf.after=Isl82f5.80.mean.after$mx,
                        axf.prior=Isl82f5.80.mean.prior$ax,
                        axf.crisis=Isl1882f5.80$ax,
                        axf.after=Isl82f5.80.mean.after$ax,                        
                        parsm.prior=Isl82m5.80.mean.prior$mx,
                        parsm.crisis=Isl1882m5.80$mx,
                        parsm.after=Isl82m5.80.mean.after$mx,
                        axm.prior=Isl82m5.80.mean.prior$ax,
                        axm.crisis=Isl1882m5.80$ax,
                        axm.after=Isl82m5.80.mean.after$ax)

png("Path/Edag dec, Iceland 1882.png",
    width=1000,height=700)

dec.plot1(merged=merged_e,breaks=c(-.75,0,.75),ylim=c(-1.3,1.3),
          title="Change in lifespan disparity by 5-year age groups, Iceland 1882",
          fill="#008080")

dev.off()



# Entropy
#---------

merged_h <- dec.crisis1(func=h5.dec,
                        parsf.prior=Isl82f5.80.mean.prior$mx,
                        parsf.crisis=Isl1882f5.80$mx,
                        parsf.after=Isl82f5.80.mean.after$mx,
                        axf.prior=Isl82f5.80.mean.prior$ax,
                        axf.crisis=Isl1882f5.80$ax,
                        axf.after=Isl82f5.80.mean.after$ax,                        
                        parsm.prior=Isl82m5.80.mean.prior$mx,
                        parsm.crisis=Isl1882m5.80$mx,
                        parsm.after=Isl82m5.80.mean.after$mx,
                        axm.prior=Isl82m5.80.mean.prior$ax,
                        axm.crisis=Isl1882m5.80$ax,
                        axm.after=Isl82m5.80.mean.after$ax)

png("Path/Entropy dec, Iceland 1882.png",
    width=1000,height=700)

dec.plot1(merged=merged_h,breaks=c(-.2,0,.2),ylim=c(-.42,.42),
          title="Change in lifetable entropy by 5-year age groups, Iceland 1882",
          fill="#008080")

dev.off()



# Threshold ages
##################

# Females

Isl82f.80 <- rbind(Isl82f.80.mean.prior,Isl1882f.80,
                   Isl82f.80.mean.after, fill=T)

data <- Isl82f.80[ ,age.H(Age = Age, ax = ax, dx = dx, lx = lx, ex = ex), by = list(Year)]

Isl82f.80.threshold <- data[data$Year!=1882][ ,get.a(Age= Age, gx = gx, ex = ex,  e.dag.x = e.dag.x, 
                                                     a.dag =a.dag), by = list(Year)]

e0 <-  data$ex[data$Year==1882][1]
f1 <- approxfun(data$Age[data$Year==1882],
                data$gx[data$Year==1882],
                method = "linear",rule=2 )
a.H <- uniroot(function(x) f1(x),c(0,80))$root

crisis <- data.table(cbind(Year=1882,a.H=a.H,a.dagger=0))

af.B <- Isl82f.80.threshold$a.dagger[Isl82f.80.threshold$Year=="1877-1881"]
af.D <- crisis$a.dagger
af.A <- Isl82f.80.threshold$a.dagger[Isl82f.80.threshold$Year=="1884-1888"]

# Males

Isl82m.80 <- rbind(Isl82m.80.mean.prior,Isl1882m.80,
                   Isl82m.80.mean.after, fill=T)

data <- Isl82m.80[ ,age.H(Age = Age, ax = ax, dx = dx, lx = lx, ex = ex), by = list(Year)]

Isl82m.80.threshold <- data[data$Year!=1882][ ,get.a(Age= Age, gx = gx, ex = ex,  e.dag.x = e.dag.x, 
                                                     a.dag =a.dag), by = list(Year)]

e0 <-  data$ex[data$Year==1882][1]
f1 <- approxfun(data$Age[data$Year==1882],
                data$gx[data$Year==1882],
                method = "linear",rule=2 )
a.H <- uniroot(function(x) f1(x),c(0,80))$root

crisis <- data.table(cbind(Year=1882,a.H=a.H,a.dagger=0))

am.B <- Isl82m.80.threshold$a.dagger[Isl82m.80.threshold$Year=="1877-1881"]
am.D <- crisis$a.dagger
am.A <- Isl82m.80.threshold$a.dagger[Isl82m.80.threshold$Year=="1884-1888"]
