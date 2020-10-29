#--------------------------------------------------------------
# Topic: Handling data Sweden 1773, mortality crises and lifespan variation
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

# 1767-1778
Swe6778f5 <- subset(Swe5,subset=Year<1779 & Year>1766 & Sex=="f")

Swe6778f5.80 <- lt80(Swe6778f5[Year==1767])

for(i in unique(Swe6778f5$Year)[-1]){
  
  Swe6778f5.1 <- lt80(Swe6778f5[Year==i])
  
  Swe6778f5.80 <- rbind(Swe6778f5.80,Swe6778f5.1)
  
}

# 1773
Swe1773f5 <- subset(Swe5,subset=Year==1773 & Sex=="f")
Swe1773f5.80 <- lt80(Swe1773f5)

# Average of 5 years prior
Swe6771f5 <- subset(Swe5,subset=Year<1772 & Year>1766 & Sex=="f")

mean.mx.prior <- Swe6771f5[,list(mean.mx = mean(mx), mean.ax = mean(ax)),  by = list(Age)]

Swe73f5.mean.prior <- data.table(mean.mx.prior[,lifetable(mx=mean.mx,Age=Swe1773f5$Age,ax=mean.ax)])
Swe73f5.80.mean.prior <- lt80(Swe73f5.mean.prior)

# Average of 5 years after
Swe7478f5 <- subset(Swe5,subset=Year<1779 & Year>1773 & Sex=="f")

mean.mx.after <- Swe7478f5[,list(mean.mx = mean(mx), mean.ax = mean(ax)),  by = list(Age)]

Swe73f5.mean.after <- data.table(mean.mx.after[,lifetable(mx=mean.mx,Age=Swe1773f5$Age,ax=mean.ax)])
Swe73f5.80.mean.after <- lt80(Swe73f5.mean.after)

# Calculating indicators
#------------------------

# 1773
e05f.80      <- le0(Swe1773f5.80)
sd5f.80      <- sd.fun(Swe1773f5.80)
aGini5f.80   <- e05f.80*rGini.fun(Swe1773f5.80)
edag5f.80    <- edag.fun(Swe1773f5.80,n=Swe1773f5.80$n)
cv5f.80      <- sd5f.80/e05f.80
rGini5f.80   <- rGini.fun(Swe1773f5.80)
entropy5f.80 <- edag5f.80$edag/e05f.80

# Prior years
e05f.80.mean.prior      <- le0(Swe73f5.80.mean.prior)
sd5f.80.mean.prior      <- sd.fun(Swe73f5.80.mean.prior)
aGini5f.80.mean.prior   <- e05f.80.mean.prior*rGini.fun(Swe73f5.80.mean.prior)
edag5f.80.mean.prior    <- edag.fun(Swe73f5.80.mean.prior,n=Swe1773f5.80$n)
cv5f.80.mean.prior      <- sd5f.80.mean.prior/e05f.80.mean.prior
rGini5f.80.mean.prior   <- rGini.fun(Swe73f5.80.mean.prior)
entropy5f.80.mean.prior <- edag5f.80.mean.prior$edag/e05f.80.mean.prior

# Next years
e05f.80.mean.after      <- le0(Swe73f5.80.mean.after)
sd5f.80.mean.after      <- sd.fun(Swe73f5.80.mean.after)
aGini5f.80.mean.after   <- e05f.80.mean.after*rGini.fun(Swe73f5.80.mean.after)
edag5f.80.mean.after    <- edag.fun(Swe73f5.80.mean.after,n=Swe1773f5.80$n)
cv5f.80.mean.after      <- sd5f.80.mean.after/e05f.80.mean.after
rGini5f.80.mean.after   <- rGini.fun(Swe73f5.80.mean.after)
entropy5f.80.mean.after <- edag5f.80.mean.after$edag/e05f.80.mean.after

# Year-to-year
e05f.80.years <- Swe6778f5.80$ex[Swe6778f5.80$Age==0]
sd5f.80.years <- NA
for (i in 1767:1778){
  sd5f.80.years[i - 1766] <- sd.fun(Swe6778f5.80[Year==i])
}
sd5f.80.years <- as.numeric(sd5f.80.years)

aGini5f.80.years <- NA
for (i in 1767:1778){
  aGini5f.80.years[i - 1766] <- e05f.80.years[i - 1766]*
    Gini.fun(x = Swe6778f5.80$Age[Swe6778f5.80$Year==i],
             nax = Swe6778f5.80$ax[Swe6778f5.80$Year==i],
             ndx = Swe6778f5.80$dx[Swe6778f5.80$Year==i]/100000,
             ex = Swe6778f5.80$ex[Swe6778f5.80$Year==i])
}

edag5f.80.years <- NA
for (i in 1767:1778){
  edag5f.80.years[i - 1766] <- edag.fun(Swe6778f5.80[Year==i],n=Swe1773f5.80$n)
}
edag5f.80.years <- as.numeric(edag5f.80.years)

cv5f.80.years <- sd5f.80.years/e05f.80.years
rGini5f.80.years <- NA
for (i in 1767:1778){
  rGini5f.80.years[i - 1766] <- Gini.fun(x = Swe6778f5.80$Age[Swe6778f5.80$Year==i],
                                         nax = Swe6778f5.80$ax[Swe6778f5.80$Year==i],
                                         ndx = Swe6778f5.80$dx[Swe6778f5.80$Year==i]/100000,
                                         ex = Swe6778f5.80$ex[Swe6778f5.80$Year==i])
}
entropy5f.80.years <- edag5f.80.years/e05f.80.years



#-------
# MALES
#-------

# 1767-1779
Swe6778m5 <- subset(Swe5,subset=Year<1779 & Year>1766 & Sex=="m")

Swe6778m5.80 <- lt80(Swe6778m5[Year==1767])

for(i in unique(Swe6778m5$Year)[-1]){
  
  Swe6778m5.1 <- lt80(Swe6778m5[Year==i])
  
  Swe6778m5.80 <- rbind(Swe6778m5.80,Swe6778m5.1)
  
}

# 1773
Swe1773m5 <- subset(Swe5,subset=Year==1773 & Sex=="m")
Swe1773m5.80 <- lt80(Swe1773m5)

# Average of 5 years prior
Swe4145m5 <- subset(Swe5,subset=Year<1772 & Year>1766 & Sex=="m")

mean.mx.prior <- Swe4145m5[,list(mean.mx = mean(mx), mean.ax = mean(ax)),  by = list(Age)]

Swe73m5.mean.prior <- data.table(mean.mx.prior[,lifetable(mx=mean.mx,Age=Swe1773m5$Age,ax=mean.ax)])
Swe73m5.80.mean.prior <- lt80(Swe73m5.mean.prior)

# Average of 5 years after
Swe4788m5 <- subset(Swe5,subset=Year<1779 & Year>1773 & Sex=="m")

mean.mx.after <- Swe4788m5[,list(mean.mx = mean(mx), mean.ax = mean(ax)),  by = list(Age)]

Swe73m5.mean.after <- data.table(mean.mx.after[,lifetable(mx=mean.mx,Age=Swe1773m5$Age,ax=mean.ax)])
Swe73m5.80.mean.after <- lt80(Swe73m5.mean.after)

# Calculating indicators
#------------------------

# 1773
e05m.80      <- le0(Swe1773m5.80)
sd5m.80      <- sd.fun(Swe1773m5.80)
aGini5m.80   <- e05m.80*rGini.fun(Swe1773m5.80)
edag5m.80    <- edag.fun(Swe1773m5.80,n=Swe1773m5.80$n)
cv5m.80      <- sd5m.80/e05m.80
rGini5m.80   <- rGini.fun(Swe1773m5.80)
entropy5m.80 <- edag5m.80$edag/e05m.80

# Prior years
e05m.80.mean.prior      <- le0(Swe73m5.80.mean.prior)
sd5m.80.mean.prior      <- sd.fun(Swe73m5.80.mean.prior)
aGini5m.80.mean.prior   <- e05m.80.mean.prior*rGini.fun(Swe73m5.80.mean.prior)
edag5m.80.mean.prior    <- edag.fun(Swe73m5.80.mean.prior,n=Swe1773m5.80$n)
cv5m.80.mean.prior      <- sd5m.80.mean.prior/e05m.80.mean.prior
rGini5m.80.mean.prior   <- rGini.fun(Swe73m5.80.mean.prior)
entropy5m.80.mean.prior <- edag5m.80.mean.prior$edag/e05m.80.mean.prior

# Next years
e05m.80.mean.after      <- le0(Swe73m5.80.mean.after)
sd5m.80.mean.after      <- sd.fun(Swe73m5.80.mean.after)
aGini5m.80.mean.after   <- e05m.80.mean.after*rGini.fun(Swe73m5.80.mean.after)
edag5m.80.mean.after    <- edag.fun(Swe73m5.80.mean.after,n=Swe1773m5.80$n)
cv5m.80.mean.after      <- sd5m.80.mean.after/e05m.80.mean.after
rGini5m.80.mean.after   <- rGini.fun(Swe73m5.80.mean.after)
entropy5m.80.mean.after <- edag5m.80.mean.after$edag/e05m.80.mean.after

# Year-to-year
e05m.80.years <- Swe6778m5.80$ex[Swe6778m5.80$Age==0]
sd5m.80.years <- NA
for (i in 1767:1778){
  sd5m.80.years[i - 1766] <- sd.fun(Swe6778m5.80[Year==i])
}
sd5m.80.years <- as.numeric(sd5m.80.years)

aGini5m.80.years <- NA
for (i in 1767:1778){
  aGini5m.80.years[i - 1766] <- e05m.80.years[i - 1766]*
    Gini.fun(x = Swe6778m5.80$Age[Swe6778m5.80$Year==i],
             nax = Swe6778m5.80$ax[Swe6778m5.80$Year==i],
             ndx = Swe6778m5.80$dx[Swe6778m5.80$Year==i]/100000,
             ex = Swe6778m5.80$ex[Swe6778m5.80$Year==i])
}

edag5m.80.years <- NA
for (i in 1767:1778){
  edag5m.80.years[i - 1766] <- edag.fun(Swe6778m5.80[Year==i],n=Swe1773m5.80$n)
}
edag5m.80.years <- as.numeric(edag5m.80.years)

cv5m.80.years <- sd5m.80.years/e05m.80.years
rGini5m.80.years <- NA
for (i in 1767:1778){
  rGini5m.80.years[i - 1766] <- Gini.fun(x = Swe6778m5.80$Age[Swe6778m5.80$Year==i],
                                         nax = Swe6778m5.80$ax[Swe6778m5.80$Year==i],
                                         ndx = Swe6778m5.80$dx[Swe6778m5.80$Year==i]/100000,
                                         ex = Swe6778m5.80$ex[Swe6778m5.80$Year==i])
}
entropy5m.80.years <- edag5m.80.years/e05m.80.years













