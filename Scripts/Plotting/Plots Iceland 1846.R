#--------------------------------------------------------------
# Topic: Plotting Iceland 1846, mortality crises and lifespan variation
# Author: Serena Vigezzi
# Date: 27/10/2020
#--------------------------------------------------------------

source("Path/Handling data, Iceland 1846 - F.R")
source("Path/Handling data, Iceland 1846 - M.R")

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
png("Path/Log(mx), Iceland 1846 - F.png",
    width=1000,height=700)

plot.mx1(data.prior=Isl46f5.80.mean.prior,
         data.crisis=Isl1846f5.80,data.after=Isl46f5.80.mean.after,
         period=c("1841-45", "1846", "1847-51"),
         title=bquote('Icelandic females 1846'),
         palette=c("#91E0E0", "#DC143C", "#000000", "#008080"))

dev.off()

# Males
png("Path/Log(mx), Iceland 1846 - M.png",
    width=1000,height=700)

plot.mx1(data.prior=Isl46m5.80.mean.prior,
         data.crisis=Isl1846m5.80,data.after=Isl46m5.80.mean.after,
         period=c("1841-45", "1846", "1847-51"),
         title=bquote('Icelandic males 1846'),
         palette=c("#91E0E0", "#DC143C", "#000000", "#008080"))

dev.off()


# Variability
#-------------

png("Path/Absolute, Iceland 1846.png",
    width=1000,height=700)

abs.plot(sdf.data=sd5f.80.years,aGf.data=aGini5f.80.years,edf.data=edag5f.80.years,
         sdm.data=sd5m.80.years,aGm.data=aGini5m.80.years,edm.data=edag5m.80.years,
         Years=1841:1851,palette=c("#022A2A", "#008080", "#91E0E0"),
         xminplot=1845.5,xmaxplot=1846.5,
         xmin1=1843,xmax1=1843,ymin1=5,ymax1=6,
         xmin2=1848,xmax2=1850,ymin2=5,ymax2=10,
         ylim=c(0,35))

dev.off


# Relative

png("Path/Relative, Iceland 1846.png",
    width=1000,height=700)

rel.plot(cvf.data=cv5f.80.years,rGf.data=rGini5f.80.years,hf.data=entropy5f.80.years,
         cvm.data=cv5m.80.years,rGm.data=rGini5m.80.years,hm.data=entropy5m.80.years,
         Years=1841:1851,palette=c("#540700", "#E34234", "#FFC9C7"),
         xminplot=1845.5,xmaxplot=1846.5,
         xmin1=1843,xmax1=1843,ymin1=1.2,ymax1=1.6)

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
                        parsf.prior=Isl46f5.80.mean.prior$mx,
                        parsf.crisis=Isl1846f5.80$mx,
                        parsf.after=Isl46f5.80.mean.after$mx,
                        axf.prior=Isl46f5.80.mean.prior$ax,
                        axf.crisis=Isl1846f5.80$ax,
                        axf.after=Isl46f5.80.mean.after$ax,                        
                        parsm.prior=Isl46m5.80.mean.prior$mx,
                        parsm.crisis=Isl1846m5.80$mx,
                        parsm.after=Isl46m5.80.mean.after$mx,
                        axm.prior=Isl46m5.80.mean.prior$ax,
                        axm.crisis=Isl1846m5.80$ax,
                        axm.after=Isl46m5.80.mean.after$ax)

png("Path/Edag dec, Iceland 1846.png",
    width=1000,height=700)

dec.plot1(merged=merged_e,breaks=c(-.75,0,.75),ylim=c(-1.3,1.3),
          title="Change in lifespan disparity by 5-year age groups, Iceland 1846",
          fill="#008080")

dev.off()



# Entropy
#---------

merged_h <- dec.crisis1(func=h5.dec,
                        parsf.prior=Isl46f5.80.mean.prior$mx,
                        parsf.crisis=Isl1846f5.80$mx,
                        parsf.after=Isl46f5.80.mean.after$mx,
                        axf.prior=Isl46f5.80.mean.prior$ax,
                        axf.crisis=Isl1846f5.80$ax,
                        axf.after=Isl46f5.80.mean.after$ax,                        
                        parsm.prior=Isl46m5.80.mean.prior$mx,
                        parsm.crisis=Isl1846m5.80$mx,
                        parsm.after=Isl46m5.80.mean.after$mx,
                        axm.prior=Isl46m5.80.mean.prior$ax,
                        axm.crisis=Isl1846m5.80$ax,
                        axm.after=Isl46m5.80.mean.after$ax)

png("Path/Entropy dec, Iceland 1846.png",
    width=1000,height=700)

dec.plot1(merged=merged_h,breaks=c(-.2,0,.2),ylim=c(-.42,.42),
          title="Change in lifetable entropy by 5-year age groups, Iceland 1846",
          fill="#008080")

dev.off()



# Threshold ages
##################

# Females

Isl46f.80 <- rbind(Isl46f.80.mean.prior,Isl1846f.80,
                   Isl46f.80.mean.after, fill=T)

data <- Isl46f.80[ ,age.H(Age = Age, ax = ax, dx = dx, lx = lx, ex = ex), by = list(Year)]

Isl46f.80.threshold <- data[ ,get.a(Age= Age, gx = gx, ex = ex,  e.dag.x = e.dag.x, 
                                    a.dag =a.dag), by = list(Year)]

af.B <- Isl46f.80.threshold$a.dagger[Isl46f.80.threshold$Year=="1841-1845"]
af.D <- 0
af.A <- Isl46f.80.threshold$a.dagger[Isl46f.80.threshold$Year=="1847-1851"]

# Males

Isl46m.80 <- rbind(Isl46m.80.mean.prior,Isl1846m.80,
                   Isl46m.80.mean.after, fill=T)

data <- Isl46m.80[ ,age.H(Age = Age, ax = ax, dx = dx, lx = lx, ex = ex), by = list(Year)]

Isl46m.80.threshold <- data[ ,get.a(Age= Age, gx = gx, ex = ex,  e.dag.x = e.dag.x, 
                                    a.dag =a.dag), by = list(Year)]

am.B <- Isl46m.80.threshold$a.dagger[Isl46m.80.threshold$Year=="1841-1845"]
am.D <- 0
am.A <- Isl46m.80.threshold$a.dagger[Isl46m.80.threshold$Year=="1847-1851"]
