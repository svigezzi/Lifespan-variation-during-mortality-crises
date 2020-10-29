#--------------------------------------------------------------
# Topic: Plotting Sweden 1809, mortality crises and lifespan variation
# Author: Serena Vigezzi
# Date: 26/10/2020
#--------------------------------------------------------------

source("Path/Handling data, Sweden 1809 - F.R")
source("Path/Handling data, Sweden 1809 - M.R")

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
png("Path/Log(mx), Sweden 1809 - F.png",
    width=1000,height=700)

plot.mx2(data.prior=Swe09f5.80.mean.prior,data.first=Swe0314f5.80[Swe0314f5.80$Year==1808],
         data.second=Swe1809f5.80,data.after=Swe09f5.80.mean.after,
         period=c("1803-07", "1808", "1809", "1810-14"),
         title=bquote('Swedish females 1808-1809'),
         palette=c("#91E0E0", "#DC143C", "#000000", "#008080"))

dev.off()

# Males
png("Path/Log(mx), Sweden 1809 - M.png",
    width=1000,height=700)

plot.mx2(data.prior=Swe09m5.80.mean.prior,data.first=Swe0314m5.80[Swe0314m5.80$Year==1808],
         data.second=Swe1809m5.80,data.after=Swe09m5.80.mean.after,
         period=c("1803-07", "1808", "1809", "1810-14"),
         title=bquote('Swedish males 1808-1809'),
         palette=c("#91E0E0", "#DC143C", "#000000", "#008080"))

dev.off()


# Variability
#-------------

png("Path/Absolute, Sweden 1809.png",
    width=1000,height=700)

abs.plot(sdf.data=sd5f.80.years,aGf.data=aGini5f.80.years,edf.data=edag5f.80.years,
         sdm.data=sd5m.80.years,aGm.data=aGini5m.80.years,edm.data=edag5m.80.years,
         Years=1803:1814,palette=c("#022A2A", "#008080", "#91E0E0"),
         xminplot=1807.5,xmaxplot=1809.5,
         xmin1=1803,xmax1=1807,ymin1=36,ymax1=39,
         xmin2=1811,xmax2=1813,ymin2=39,ymax2=40)

dev.off


# Relative

png("Path/Relative, Sweden 1809.png",
    width=1000,height=700)

rel.plot(cvf.data=cv5f.80.years,rGf.data=rGini5f.80.years,hf.data=entropy5f.80.years,
         cvm.data=cv5m.80.years,rGm.data=rGini5m.80.years,hm.data=entropy5m.80.years,
         Years=1803:1814,palette=c("#540700", "#E34234", "#FFC9C7"),
         xminplot=1807.5,xmaxplot=1809.5,
         xmin1=1803,xmax1=1807,ymin1=1.2,ymax1=1.5)

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

merged_e <- dec.crisis2(func=edag5.dec,
                        parsf.prior=Swe09f5.80.mean.prior$mx,
                        parsf.first=Swe0314f5.80$mx[Swe0314f5.80$Year==1808],
                        parsf.second=Swe1809f5.80$mx,
                        parsf.after=Swe09f5.80.mean.after$mx,
                        axf.prior=Swe09f5.80.mean.prior$ax,
                        axf.first=Swe0314f5.80$ax[Swe0314f5.80$Year==1808],
                        axf.second=Swe1809f5.80$ax,
                        axf.after=Swe09f5.80.mean.after$ax,                        
                        parsm.prior=Swe09m5.80.mean.prior$mx,
                        parsm.first=Swe0314m5.80$mx[Swe0314m5.80$Year==1808],
                        parsm.second=Swe1809m5.80$mx,
                        parsm.after=Swe09m5.80.mean.after$mx,
                        axm.prior=Swe09m5.80.mean.prior$ax,
                        axm.first=Swe0314m5.80$ax[Swe0314m5.80$Year==1808],
                        axm.second=Swe1809m5.80$ax,
                        axm.after=Swe09m5.80.mean.after$ax)

png("Path/Edag dec, Sweden 1809.png",
    width=1000,height=700)

dec.plot2(merged=merged_e,breaks=c(-.25,0,.25),ylim=c(-.55,.55),
          title="Change in lifespan disparity by 5-year age groups, Sweden 1809",
          fill="#008080")

dev.off()



# Entropy
#---------

merged_h <- dec.crisis2(func=h5.dec,ukr=F,
                        parsf.prior=Swe09f5.80.mean.prior$mx,
                        parsf.first=Swe0314f5.80$mx[Swe0314f5.80$Year==1808],
                        parsf.second=Swe1809f5.80$mx,
                        parsf.after=Swe09f5.80.mean.after$mx,
                        axf.prior=Swe09f5.80.mean.prior$ax,
                        axf.first=Swe0314f5.80$ax[Swe0314f5.80$Year==1808],
                        axf.second=Swe1809f5.80$ax,
                        axf.after=Swe09f5.80.mean.after$ax,                        
                        parsm.prior=Swe09m5.80.mean.prior$mx,
                        parsm.first=Swe0314m5.80$mx[Swe0314m5.80$Year==1808],
                        parsm.second=Swe1809m5.80$mx,
                        parsm.after=Swe09m5.80.mean.after$mx,
                        axm.prior=Swe09m5.80.mean.prior$ax,
                        axm.first=Swe0314m5.80$ax[Swe0314m5.80$Year==1808],
                        axm.second=Swe1809m5.80$ax,
                        axm.after=Swe09m5.80.mean.after$ax)

png("Path/Entropy dec, Sweden 1809.png",
    width=1000,height=700)

dec.plot2(merged=merged_h,breaks=c(-.05,0,.05),ylim=c(-.1,.1),
          title="Change in lifetable entropy by 5-year age groups, Sweden 1809",
          fill="#008080")

dev.off()



# Threshold ages
##################

# Females

Swe09f.80 <- rbind(Swe09f.80.mean.prior,Swe0314f.80[Swe0314f.80$Year==1808],
                   Swe1809f.80,Swe09f.80.mean.after, fill=T)

data <- Swe09f.80[ ,age.H(Age = Age, ax = ax, dx = dx, lx = lx, ex = ex), by = list(Year)]

Swe09f.80.threshold <- data[ ,get.a(Age= Age, gx = gx, ex = ex,  e.dag.x = e.dag.x, 
                                    a.dag =a.dag), by = list(Year)]


af.B <- Swe09f.80.threshold$a.dagger[Swe09f.80.threshold$Year=="1803-1807"]
af.D1 <- Swe09f.80.threshold$a.dagger[Swe09f.80.threshold$Year=="1808"]
af.D2 <- Swe09f.80.threshold$a.dagger[Swe09f.80.threshold$Year=="1809"]
af.A <- Swe09f.80.threshold$a.dagger[Swe09f.80.threshold$Year=="1810-1814"]

# Males

Swe09m.80 <- rbind(Swe09m.80.mean.prior,Swe0314m.80[Swe0314m.80$Year==1808],
                   Swe1809m.80,Swe09m.80.mean.after, fill=T)

data <- Swe09m.80[ ,age.H(Age = Age, ax = ax, dx = dx, lx = lx, ex = ex), by = list(Year)]

Swe09m.80.threshold <- data[ ,get.a(Age= Age, gx = gx, ex = ex,  e.dag.x = e.dag.x, 
                                    a.dag =a.dag), by = list(Year)]


am.B <- Swe09m.80.threshold$a.dagger[Swe09m.80.threshold$Year=="1803-1807"]
am.D1 <- Swe09m.80.threshold$a.dagger[Swe09m.80.threshold$Year=="1808"]
am.D2 <- Swe09m.80.threshold$a.dagger[Swe09m.80.threshold$Year=="1809"]
am.A <- Swe09m.80.threshold$a.dagger[Swe09m.80.threshold$Year=="1810-1814"]
