#--------------------------------------------------------------
# Topic: Plotting Ukraine 1933, mortality crises and lifespan variation
# Author: Serena Vigezzi
# Date: 26/10/2020
#--------------------------------------------------------------

source("Path/Handling data, Ukraine 1933 - F.R")
source("Path/Handling data, Ukraine 1933 - M.R")

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
png("Path/Log(mx), Ukraine 1933 - F.png",
    width=1000,height=700)

plot.mx2(data.prior=Ukr33f5.80.mean.prior,data.first=UkrF5.80[UkrF5.80$Year==1932],
         data.second=Ukr1933f5.80,data.after=Ukr33f5.80.mean.after,
         period=c("1927-31", "1932", "1933", "1934-38"),
         title=bquote('Ukrainian females 1932-1933'),
         palette=c("#91E0E0", "#DC143C", "#000000", "#008080"))

dev.off()

# Males
png("Path/Log(mx), Ukraine 1933 - M.png",
    width=1000,height=700)

plot.mx2(data.prior=Ukr33m5.80.mean.prior,data.first=UkrM5.80[UkrM5.80$Year==1932],
         data.second=Ukr1933m5.80,data.after=Ukr33m5.80.mean.after,
         period=c("1927-31", "1932", "1933", "1934-38"),
         title=bquote('Ukrainian males 1932-1933'),
         palette=c("#91E0E0", "#DC143C", "#000000", "#008080"))

dev.off()


# Variability
#-------------

png("Path/Absolute, Ukraine 1933.png",
    width=1000,height=700)

abs.plot(sdf.data=sd5f.80.years,aGf.data=aGini5f.80.years,edf.data=edag5f.80.years,
         sdm.data=sd5m.80.years,aGm.data=aGini5m.80.years,edm.data=edag5m.80.years,
         Years=1927:1938,palette=c("#022A2A", "#008080", "#91E0E0"),
         xminplot=1931.5,xmaxplot=1933.5,
         xmin1=1927,xmax1=1931,ymin1=6,ymax1=9,
         xmin2=1935,xmax2=1937,ymin2=7,ymax2=10)

dev.off


# Relative

png("Path/Relative, Ukraine 1933.png",
    width=1000,height=700)

rel.plot(cvf.data=cv5f.80.years,rGf.data=rGini5f.80.years,hf.data=entropy5f.80.years,
         cvm.data=cv5m.80.years,rGm.data=rGini5m.80.years,hm.data=entropy5m.80.years,
         Years=1927:1938,palette=c("#540700", "#E34234", "#FFC9C7"),
         xminplot=1931.5,xmaxplot=1933.5,
         xmin1=1927,xmax1=1931,ymin1=1.2,ymax1=1.6)

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

merged_e <- dec.crisis2(func=edag5.dec,ukr=T,
                        parsf.prior=Ukr33f5.80.mean.prior$mx,
                        parsf.first=UkrF5.80$mx[UkrF5.80$Year==1932],
                        parsf.second=Ukr1933f5.80$mx,
                        parsf.after=Ukr33f5.80.mean.after$mx,
                        axf.prior=Ukr33f5.80.mean.prior$ax,
                        axf.first=UkrF5.80$ax[UkrF5.80$Year==1932],
                        axf.second=Ukr1933f5.80$ax,
                        axf.after=Ukr33f5.80.mean.after$ax,                        
                        parsm.prior=Ukr33m5.80.mean.prior$mx,
                        parsm.first=UkrM5.80$mx[UkrM5.80$Year==1932],
                        parsm.second=Ukr1933m5.80$mx,
                        parsm.after=Ukr33m5.80.mean.after$mx,
                        axm.prior=Ukr33m5.80.mean.prior$ax,
                        axm.first=UkrM5.80$ax[UkrM5.80$Year==1932],
                        axm.second=Ukr1933m5.80$ax,
                        axm.after=Ukr33m5.80.mean.after$ax)

png("Path/Edag dec, Ukraine 1933.png",
    width=1000,height=700)

dec.plot2(merged=merged_e,breaks=c(-.75,0,.75),ylim=c(-1.3,1.3),
          title="Change in lifespan disparity by 5-year age groups, Ukraine 1933",
          fill="#008080")

dev.off()



# Entropy
#---------

merged_h <- dec.crisis2(func=h5.dec,ukr=T,
                        parsf.prior=Ukr33f5.80.mean.prior$mx,
                        parsf.first=UkrF5.80$mx[UkrF5.80$Year==1932],
                        parsf.second=Ukr1933f5.80$mx,
                        parsf.after=Ukr33f5.80.mean.after$mx,
                        axf.prior=Ukr33f5.80.mean.prior$ax,
                        axf.first=UkrF5.80$ax[UkrF5.80$Year==1932],
                        axf.second=Ukr1933f5.80$ax,
                        axf.after=Ukr33f5.80.mean.after$ax,                        
                        parsm.prior=Ukr33m5.80.mean.prior$mx,
                        parsm.first=UkrM5.80$mx[UkrM5.80$Year==1932],
                        parsm.second=Ukr1933m5.80$mx,
                        parsm.after=Ukr33m5.80.mean.after$mx,
                        axm.prior=Ukr33m5.80.mean.prior$ax,
                        axm.first=UkrM5.80$ax[UkrM5.80$Year==1932],
                        axm.second=Ukr1933m5.80$ax,
                        axm.after=Ukr33m5.80.mean.after$ax)

png("Path/Entropy dec, Ukraine 1933.png",
    width=1000,height=700)

dec.plot2(merged=merged_h,breaks=c(-.2,0,.2),ylim=c(-.45,.45),
          title="Change in lifetable entropy by 5-year age groups, Ukraine 1933",
          fill="#008080")

dev.off()



# Threshold ages
#################

# Females

Ukr33f.80 <- rbind(Ukr33f.80.mean.prior,
                   UkrF.80[UkrF.80$Year==1932],
                   Ukr1933f.80,
                   Ukr33f.80.mean.after, fill=T)

data <- Ukr33f.80[ ,age.H(Age = Age, ax = ax, dx = dx, lx = lx, ex = ex), by = list(Year)]

Ukr33f.80.threshold <- data[data$Year!=1933][ ,get.a(Age= Age, gx = gx, ex = ex,  e.dag.x = e.dag.x, 
                                                     a.dag =a.dag), by = list(Year)]

e0 <-  data$ex[data$Year==1933][1]
f1 <- approxfun(data$Age[data$Year==1933],
                data$gx[data$Year==1933],
                method = "linear",rule=2 )
a.H <- uniroot(function(x) f1(x),c(0,80))$root

crisis <- data.table(cbind(Year=1933,a.H=a.H,a.dagger=0))

af.B <- Ukr33f.80.threshold$a.dagger[Ukr33f.80.threshold$Year=="1927-1931"]
af.D1 <- Ukr33f.80.threshold$a.dagger[Ukr33f.80.threshold$Year=="1932"]
af.D2 <- crisis$a.dagger
af.A <- Ukr33f.80.threshold$a.dagger[Ukr33f.80.threshold$Year=="1934-1938"]

# Males

Ukr33m.80 <- rbind(Ukr33m.80.mean.prior,
                   UkrM.80[UkrM.80$Year==1932],
                   Ukr1933m.80,
                   Ukr33m.80.mean.after, fill=T)

data <- Ukr33m.80[ ,age.H(Age = Age, ax = ax, dx = dx, lx = lx, ex = ex), by = list(Year)]

Ukr33m.80.threshold <- data[data$Year!=1933][ ,get.a(Age= Age, gx = gx, ex = ex,  e.dag.x = e.dag.x, 
                                                     a.dag =a.dag), by = list(Year)]

e0 <-  data$ex[data$Year==1933][1]
f1 <- approxfun(data$Age[data$Year==1933],
                data$gx[data$Year==1933],
                method = "linear",rule=2 )
a.H <- uniroot(function(x) f1(x),c(0,80))$root

crisis <- data.table(cbind(Year=1933,a.H=a.H,a.dagger=0))

am.B <- Ukr33m.80.threshold$a.dagger[Ukr33m.80.threshold$Year=="1927-1931"]
am.D1 <- Ukr33m.80.threshold$a.dagger[Ukr33m.80.threshold$Year=="1932"]
am.D <- crisis$a.dagger
am.A <- Ukr33m.80.threshold$a.dagger[Ukr33m.80.threshold$Year=="1934-1938"]
