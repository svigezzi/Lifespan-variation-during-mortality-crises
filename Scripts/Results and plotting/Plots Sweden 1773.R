#--------------------------------------------------------------
# Topic: Plotting Sweden 1773, mortality crises and lifespan variation
# Author: Serena Vigezzi
# Date: 26/10/2020
#--------------------------------------------------------------

source("Path/Handling data, Sweden 1773 - F.R")
source("Path/Handling data, Sweden 1773 - M.R")

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
png("Path/Log(mx), Sweden 1773 - F.png",
    width=1000,height=700)

plot.mx2(data.prior=Swe73f5.80.mean.prior,data.first=Swe6778f5.80[Swe6778f5.80$Year==1772],
      data.second=Swe1773f5.80,data.after=Swe73f5.80.mean.after,
      period=c("1767-71", "1772", "1773", "1774-78"),
      title=bquote('Swedish females 1772-1773'),
      palette=c("#91E0E0", "#DC143C", "#000000", "#008080"))

dev.off()

# Males
png("Path/Log(mx), Sweden 1773 - M.png",
    width=1000,height=700)

plot.mx2(data.prior=Swe73m5.80.mean.prior,data.first=Swe6778m5.80[Swe6778m5.80$Year==1772],
      data.second=Swe1773m5.80,data.after=Swe73m5.80.mean.after,
      period=c("1767-71", "1772", "1773", "1774-78"),
      title=bquote('Swedish males 1772-1773'),
      palette=c("#91E0E0", "#DC143C", "#000000", "#008080"))

dev.off()


# Variability
#-------------

png("Path/Absolute, Sweden 1773.png",
    width=1000,height=700)

abs.plot(sdf.data=sd5f.80.years,aGf.data=aGini5f.80.years,edf.data=edag5f.80.years,
         sdm.data=sd5m.80.years,aGm.data=aGini5m.80.years,edm.data=edag5m.80.years,
         Years=1767:1778,palette=c("#022A2A", "#008080", "#91E0E0"),
         xminplot=1771.5,xmaxplot=1773.5,
         xmin1=1768,xmax1=1771,ymin1=10,ymax1=11,
         xmin2=1775,xmax2=1777,ymin2=10,ymax2=13)

dev.off


# Relative

png("Path/Relative, Sweden 1773.png",
    width=1000,height=700)

rel.plot(cvf.data=cv5f.80.years,rGf.data=rGini5f.80.years,hf.data=entropy5f.80.years,
         cvm.data=cv5m.80.years,rGm.data=rGini5m.80.years,hm.data=entropy5m.80.years,
         Years=1767:1778,palette=c("#540700", "#E34234", "#FFC9C7"),
         xminplot=1771.5,xmaxplot=1773.5,
         xmin1=1768,xmax1=1771,ymin1=1.2,ymax1=1.6)

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
                        parsf.prior=Swe73f5.80.mean.prior$mx,
                        parsf.first=Swe6778f5.80$mx[Swe6778f5.80$Year==1772],
                        parsf.second=Swe1773f5.80$mx,
                        parsf.after=Swe73f5.80.mean.after$mx,
                        axf.prior=Swe73f5.80.mean.prior$ax,
                        axf.first=Swe6778f5.80$ax[Swe6778f5.80$Year==1772],
                        axf.second=Swe1773f5.80$ax,
                        axf.after=Swe73f5.80.mean.after$ax,                        
                        parsm.prior=Swe73m5.80.mean.prior$mx,
                        parsm.first=Swe6778m5.80$mx[Swe6778m5.80$Year==1772],
                        parsm.second=Swe1773m5.80$mx,
                        parsm.after=Swe73m5.80.mean.after$mx,
                        axm.prior=Swe73m5.80.mean.prior$ax,
                        axm.first=Swe6778m5.80$ax[Swe6778m5.80$Year==1772],
                        axm.second=Swe1773m5.80$ax,
                        axm.after=Swe73m5.80.mean.after$ax)

png("Path/Edag dec, Sweden 1773.png",
    width=1000,height=700)

dec.plot2(merged=merged_e,breaks=c(-.3,0,.3),ylim=c(-.6,.6),
                     title="Change in lifespan disparity by 5-year age groups, Sweden 1773",
                     fill="#008080")

dev.off()


# Entropy
#---------
merged_h <- dec.crisis2(func=h5.dec,ukr=F,
                      parsf.prior=Swe73f5.80.mean.prior$mx,
                      parsf.first=Swe6778f5.80$mx[Swe6778f5.80$Year==1772],
                      parsf.second=Swe1773f5.80$mx,
                      parsf.after=Swe73f5.80.mean.after$mx,
                      axf.prior=Swe73f5.80.mean.prior$ax,
                      axf.first=Swe6778f5.80$ax[Swe6778f5.80$Year==1772],
                      axf.second=Swe1773f5.80$ax,
                      axf.after=Swe73f5.80.mean.after$ax,                        
                      parsm.prior=Swe73m5.80.mean.prior$mx,
                      parsm.first=Swe6778m5.80$mx[Swe6778m5.80$Year==1772],
                      parsm.second=Swe1773m5.80$mx,
                      parsm.after=Swe73m5.80.mean.after$mx,
                      axm.prior=Swe73m5.80.mean.prior$ax,
                      axm.first=Swe6778m5.80$ax[Swe6778m5.80$Year==1772],
                      axm.second=Swe1773m5.80$ax,
                      axm.after=Swe73m5.80.mean.after$ax)

png("Path/Entropy dec, Sweden 1773.png",
    width=1000,height=700)

dec.plot2(merged=merged_h,breaks=c(-.1,0,.1),ylim=c(-.25,.25),
         title="Change in lifetable entropy by 5-year age groups, Sweden 1773",
         fill="#008080")

dev.off()



# Threshold ages
#################

# Females

Swe73f.80 <- rbind(Swe73f.80.mean.prior,
                   Swe6778f.80[Swe6778f.80$Year==1772],
                   Swe1773f.80,
                   Swe73f.80.mean.after, fill=T)

data <- Swe73f.80[ ,age.H(Age = Age, ax = ax, dx = dx, lx = lx, ex = ex), by = list(Year)]

Swe73f.80.threshold <- data[data$Year!=1773][ ,get.a(Age= Age, gx = gx, ex = ex,  e.dag.x = e.dag.x, 
                                                     a.dag =a.dag), by = list(Year)]

e0 <-  data$ex[data$Year==1773][1]
f1 <- approxfun(data$Age[data$Year==1773],
                data$gx[data$Year==1773],
                method = "linear",rule=2 )
a.H <- uniroot(function(x) f1(x),c(0,80))$root

crisis <- data.table(cbind(Year=1773,a.H=a.H,a.dagger=0))

af.B <- Swe73f.80.threshold$a.dagger[Swe73f.80.threshold$Year=="1767-1771"]
af.D1 <- Swe73f.80.threshold$a.dagger[Swe73f.80.threshold$Year=="1772"]
af.D2 <- crisis$a.dagger
af.A <- Swe73f.80.threshold$a.dagger[Swe73f.80.threshold$Year=="1774-1778"]

# Males

Swe73m.80 <- rbind(Swe73m.80.mean.prior,
                   Swe6778m.80[Swe6778m.80$Year==1772],
                   Swe1773m.80,
                   Swe73m.80.mean.after, fill=T)

data <- Swe73m.80[ ,age.H(Age = Age, ax = ax, dx = dx, lx = lx, ex = ex), by = list(Year)]

Swe73m.80.threshold <- data[data$Year!=1773][ ,get.a(Age= Age, gx = gx, ex = ex,  e.dag.x = e.dag.x, 
                                                     a.dag =a.dag), by = list(Year)]

e0 <-  data$ex[data$Year==1773][1]
f1 <- approxfun(data$Age[data$Year==1773],
                data$gx[data$Year==1773],
                method = "linear",rule=2 )
a.H <- uniroot(function(x) f1(x),c(0,80))$root

crisis <- data.table(cbind(Year=1773,a.H=a.H,a.dagger=0))

am.B <- Swe73m.80.threshold$a.dagger[Swe73m.80.threshold$Year=="1767-1771"]
am.D1 <- Swe73m.80.threshold$a.dagger[Swe73m.80.threshold$Year=="1772"]
am.D <- crisis$a.dagger
am.A <- Swe73m.80.threshold$a.dagger[Swe73m.80.threshold$Year=="1774-1778"]


