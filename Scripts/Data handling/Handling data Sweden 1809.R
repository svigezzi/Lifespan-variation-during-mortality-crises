#--------------------------------------------------------------
# Topic: Handling data Sweden 1809, mortality crises and lifespan variation
# Author: Serena Vigezzi
# Date: 29/10/2020
#--------------------------------------------------------------

source("Path/Data functions.R")

load("Path/HMD_Data_5years.RData")

#---------------
# Selecting data
#----------------

# Defining the age intervals
n <- c(1,4,rep(5,20))

# Sweden
Swe5 <- subset(HMDL5, subset=PopName=="SWE")

#---------
# FEMALES
#---------

# 1803-1814
Swe0314f5 <- subset(Swe5,subset=Year<1815 & Year>1802 & Sex=="f")

Swe0314f5.80 <- lt80(Swe0314f5[Year==1803])

for(i in unique(Swe0314f5$Year)[-1]){
  
  Swe0314f5.1 <- lt80(Swe0314f5[Year==i])
  
  Swe0314f5.80 <- rbind(Swe0314f5.80,Swe0314f5.1)
  
}

# 1809
Swe1809f5 <- subset(Swe5,subset=Year==1809 & Sex=="f")
Swe1809f5.80 <- lt80(Swe1809f5)

# Average of 5 years prior
Swe0307f5 <- subset(Swe5,subset=Year<1808 & Year>1802 & Sex=="f")

mean.mx.prior <- Swe0307f5[,list(mean.mx = mean(mx), mean.ax = mean(ax)),  by = list(Age)]

Swe09f5.mean.prior <- data.table(mean.mx.prior[,lifetable(mx=mean.mx,Age=Swe1809f5$Age,ax=mean.ax)])
Swe09f5.80.mean.prior <- lt80(Swe09f5.mean.prior)

# Average of 5 years after
Swe1014f5 <- subset(Swe5,subset=Year<1815 & Year>1809 & Sex=="f")

mean.mx.after <- Swe1014f5[,list(mean.mx = mean(mx), mean.ax = mean(ax)),  by = list(Age)]

Swe09f5.mean.after <- data.table(mean.mx.after[,lifetable(mx=mean.mx,Age=Swe1809f5$Age,ax=mean.ax)])
Swe09f5.80.mean.after <- lt80(Swe09f5.mean.after)

# Calculating indicators
#------------------------

# 1809
e05f.80      <- le0(Swe1809f5.80)
sd5f.80      <- sd.fun(Swe1809f5.80)
aGini5f.80   <- e05f.80*rGini.fun(Swe1809f5.80)
edag5f.80    <- edag.fun(Swe1809f5.80,n=Swe1809f5.80$n)
cv5f.80      <- sd5f.80/e05f.80
rGini5f.80   <- rGini.fun(Swe1809f5.80)
entropy5f.80 <- edag5f.80$edag/e05f.80

# Prior years
e05f.80.mean.prior      <- le0(Swe09f5.80.mean.prior)
sd5f.80.mean.prior      <- sd.fun(Swe09f5.80.mean.prior)
aGini5f.80.mean.prior   <- e05f.80.mean.prior*rGini.fun(Swe09f5.80.mean.prior)
edag5f.80.mean.prior    <- edag.fun(Swe09f5.80.mean.prior,n=Swe1809f5.80$n)
cv5f.80.mean.prior      <- sd5f.80.mean.prior/e05f.80.mean.prior
rGini5f.80.mean.prior   <- rGini.fun(Swe09f5.80.mean.prior)
entropy5f.80.mean.prior <- edag5f.80.mean.prior$edag/e05f.80.mean.prior

# Next years
e05f.80.mean.after      <- le0(Swe09f5.80.mean.after)
sd5f.80.mean.after      <- sd.fun(Swe09f5.80.mean.after)
aGini5f.80.mean.after   <- e05f.80.mean.after*rGini.fun(Swe09f5.80.mean.after)
edag5f.80.mean.after    <- edag.fun(Swe09f5.80.mean.after,n=Swe1809f5.80$n)
cv5f.80.mean.after      <- sd5f.80.mean.after/e05f.80.mean.after
rGini5f.80.mean.after   <- rGini.fun(Swe09f5.80.mean.after)
entropy5f.80.mean.after <- edag5f.80.mean.after$edag/e05f.80.mean.after

# Year-to-year
e05f.80.years <- Swe0314f5.80$ex[Swe0314f5.80$Age==0]
sd5f.80.years <- NA
for (i in 1803:1814){
  sd5f.80.years[i - 1802] <- sd.fun(Swe0314f5.80[Year==i])
}
sd5f.80.years <- as.numeric(sd5f.80.years)

aGini5f.80.years <- NA
for (i in 1803:1814){
  aGini5f.80.years[i - 1802] <- e05f.80.years[i - 1802]*
    Gini.fun(x = Swe0314f5.80$Age[Swe0314f5.80$Year==i],
             nax = Swe0314f5.80$ax[Swe0314f5.80$Year==i],
             ndx = Swe0314f5.80$dx[Swe0314f5.80$Year==i]/100000,
             ex = Swe0314f5.80$ex[Swe0314f5.80$Year==i])
}

edag5f.80.years <- NA
for (i in 1803:1814){
  edag5f.80.years[i - 1802] <- edag.fun(Swe0314f5.80[Year==i],n=Swe1809f5.80$n)
}
edag5f.80.years <- as.numeric(edag5f.80.years)

cv5f.80.years <- sd5f.80.years/e05f.80.years
rGini5f.80.years <- NA
for (i in 1803:1814){
  rGini5f.80.years[i - 1802] <- Gini.fun(x = Swe0314f5.80$Age[Swe0314f5.80$Year==i],
                                         nax = Swe0314f5.80$ax[Swe0314f5.80$Year==i],
                                         ndx = Swe0314f5.80$dx[Swe0314f5.80$Year==i]/100000,
                                         ex = Swe0314f5.80$ex[Swe0314f5.80$Year==i])
}
entropy5f.80.years <- edag5f.80.years/e05f.80.years


#---------
# MALES
#---------

# 1803-1815
Swe0314m5 <- subset(Swe5,subset=Year<1815 & Year>1802 & Sex=="m")

Swe0314m5.80 <- lt80(Swe0314m5[Year==1803])

for(i in unique(Swe0314m5$Year)[-1]){
  
  Swe0314m5.1 <- lt80(Swe0314m5[Year==i])
  
  Swe0314m5.80 <- rbind(Swe0314m5.80,Swe0314m5.1)
  
}

# 1809
Swe1809m5 <- subset(Swe5,subset=Year==1809 & Sex=="m")
Swe1809m5.80 <- lt80(Swe1809m5)

# Average of 5 years prior
Swe0307m5 <- subset(Swe5,subset=Year<1808 & Year>1802 & Sex=="m")

mean.mx.prior <- Swe0307m5[,list(mean.mx = mean(mx), mean.ax = mean(ax)),  by = list(Age)]

Swe09m5.mean.prior <- data.table(mean.mx.prior[,lifetable(mx=mean.mx,Age=Swe1809m5$Age,ax=mean.ax)])
Swe09m5.80.mean.prior <- lt80(Swe09m5.mean.prior)

# Average of 5 years after
Swe1014m5 <- subset(Swe5,subset=Year<1815 & Year>1809 & Sex=="m")

mean.mx.after <- Swe1014m5[,list(mean.mx = mean(mx), mean.ax = mean(ax)),  by = list(Age)]

Swe09m5.mean.after <- data.table(mean.mx.after[,lifetable(mx=mean.mx,Age=Swe1809m5$Age,ax=mean.ax)])
Swe09m5.80.mean.after <- lt80(Swe09m5.mean.after)

# Calculating indicators
#------------------------

# 1809
e05m.80      <- le0(Swe1809m5.80)
sd5m.80      <- sd.fun(Swe1809m5.80)
aGini5m.80   <- e05m.80*rGini.fun(Swe1809m5.80)
edag5m.80    <- edag.fun(Swe1809m5.80,n=Swe1809m5.80$n)
cv5m.80      <- sd5m.80/e05m.80
rGini5m.80   <- rGini.fun(Swe1809m5.80)
entropy5m.80 <- edag5m.80$edag/e05m.80

# Prior years
e05m.80.mean.prior      <- le0(Swe09m5.80.mean.prior)
sd5m.80.mean.prior      <- sd.fun(Swe09m5.80.mean.prior)
aGini5m.80.mean.prior   <- e05m.80.mean.prior*rGini.fun(Swe09m5.80.mean.prior)
edag5m.80.mean.prior    <- edag.fun(Swe09m5.80.mean.prior,n=Swe1809m5.80$n)
cv5m.80.mean.prior      <- sd5m.80.mean.prior/e05m.80.mean.prior
rGini5m.80.mean.prior   <- rGini.fun(Swe09m5.80.mean.prior)
entropy5m.80.mean.prior <- edag5m.80.mean.prior$edag/e05m.80.mean.prior

# Next years
e05m.80.mean.after      <- le0(Swe09m5.80.mean.after)
sd5m.80.mean.after      <- sd.fun(Swe09m5.80.mean.after)
aGini5m.80.mean.after   <- e05m.80.mean.after*rGini.fun(Swe09m5.80.mean.after)
edag5m.80.mean.after    <- edag.fun(Swe09m5.80.mean.after,n=Swe1809m5.80$n)
cv5m.80.mean.after      <- sd5m.80.mean.after/e05m.80.mean.after
rGini5m.80.mean.after   <- rGini.fun(Swe09m5.80.mean.after)
entropy5m.80.mean.after <- edag5m.80.mean.after$edag/e05m.80.mean.after

# Year-to-year
e05m.80.years <- Swe0314m5.80$ex[Swe0314m5.80$Age==0]
sd5m.80.years <- NA
for (i in 1803:1814){
  sd5m.80.years[i - 1802] <- sd.fun(Swe0314m5.80[Year==i])
}
sd5m.80.years <- as.numeric(sd5m.80.years)

aGini5m.80.years <- NA
for (i in 1803:1814){
  aGini5m.80.years[i - 1802] <- e05m.80.years[i - 1802]*
    Gini.fun(x = Swe0314m5.80$Age[Swe0314m5.80$Year==i],
             nax = Swe0314m5.80$ax[Swe0314m5.80$Year==i],
             ndx = Swe0314m5.80$dx[Swe0314m5.80$Year==i]/100000,
             ex = Swe0314m5.80$ex[Swe0314m5.80$Year==i])
}

edag5m.80.years <- NA
for (i in 1803:1814){
  edag5m.80.years[i - 1802] <- edag.fun(Swe0314m5.80[Year==i],n=Swe1809m5.80$n)
}
edag5m.80.years <- as.numeric(edag5m.80.years)

cv5m.80.years <- sd5m.80.years/e05m.80.years
rGini5m.80.years <- NA
for (i in 1803:1814){
  rGini5m.80.years[i - 1802] <- Gini.fun(x = Swe0314m5.80$Age[Swe0314m5.80$Year==i],
                                         nax = Swe0314m5.80$ax[Swe0314m5.80$Year==i],
                                         ndx = Swe0314m5.80$dx[Swe0314m5.80$Year==i]/100000,
                                         ex = Swe0314m5.80$ex[Swe0314m5.80$Year==i])
}
entropy5m.80.years <- edag5m.80.years/e05m.80.years




