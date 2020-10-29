#--------------------------------------------------------------
# Topic: Handling data Iceland 1846, mortality crises and lifespan variation
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
Swe5 <- subset(HMDL5, subset=PopName=="ISL")

#---------
# FEMALES
#---------

# 1841-1851
Isl4151f5 <- subset(Isl5,subset=Year<1852 & Year>1840 & Sex=="f")

Isl4151f5.80 <- lt80(Isl4151f5[Year==1841])

for(i in unique(Isl4151f5$Year)[-1]){
  
  Isl4151f5.1 <- lt80(Isl4151f5[Year==i])
  
  Isl4151f5.80 <- rbind(Isl4151f5.80,Isl4151f5.1)
  
}

# 1846
Isl1846f5 <- subset(Isl5,subset=Year==1846 & Sex=="f")
Isl1846f5.80 <- lt80(Isl1846f5)

# Average of 5 years prior
Isl4145f5 <- subset(Isl5,subset=Year<1846 & Year>1840 & Sex=="f")

mean.mx.prior <- Isl4145f5[,list(mean.mx = mean(mx), mean.ax = mean(ax)),  by = list(Age)]

Isl46f5.mean.prior <- data.table(mean.mx.prior[,lifetable(mx=mean.mx,Age=Isl1846f5$Age,ax=mean.ax)])
Isl46f5.80.mean.prior <- lt80(Isl46f5.mean.prior)

# Average of 5 years after
Isl4751f5 <- subset(Isl5,subset=Year<1852 & Year>1846 & Sex=="f")

mean.mx.after <- Isl4751f5[,list(mean.mx = mean(mx), mean.ax = mean(ax)),  by = list(Age)]

Isl46f5.mean.after <- data.table(mean.mx.after[,lifetable(mx=mean.mx,Age=Isl1846f5$Age,ax=mean.ax)])
Isl46f5.80.mean.after <- lt80(Isl46f5.mean.after)

# Calculating indicators
#------------------------

# 1846
e05f.80      <- le0(Isl1846f5.80)
sd5f.80      <- sd.fun(Isl1846f5.80)
aGini5f.80   <- e05f.80*rGini.fun(Isl1846f5.80)
edag5f.80    <- edag.fun(Isl1846f5.80,n=Isl1846f5.80$n)
cv5f.80      <- sd5f.80/e05f.80
rGini5f.80   <- rGini.fun(Isl1846f5.80)
entropy5f.80 <- edag5f.80$edag/e05f.80

# Prior years
e05f.80.mean.prior      <- le0(Isl46f5.80.mean.prior)
sd5f.80.mean.prior      <- sd.fun(Isl46f5.80.mean.prior)
aGini5f.80.mean.prior   <- e05f.80.mean.prior*rGini.fun(Isl46f5.80.mean.prior)
edag5f.80.mean.prior    <- edag.fun(Isl46f5.80.mean.prior,n=Isl1846f5.80$n)
cv5f.80.mean.prior      <- sd5f.80.mean.prior/e05f.80.mean.prior
rGini5f.80.mean.prior   <- rGini.fun(Isl46f5.80.mean.prior)
entropy5f.80.mean.prior <- edag5f.80.mean.prior$edag/e05f.80.mean.prior

# Next years
e05f.80.mean.after      <- le0(Isl46f5.80.mean.after)
sd5f.80.mean.after      <- sd.fun(Isl46f5.80.mean.after)
aGini5f.80.mean.after   <- e05f.80.mean.after*rGini.fun(Isl46f5.80.mean.after)
edag5f.80.mean.after    <- edag.fun(Isl46f5.80.mean.after,n=Isl1882f5.80$n)
cv5f.80.mean.after      <- sd5f.80.mean.after/e05f.80.mean.after
rGini5f.80.mean.after   <- rGini.fun(Isl46f5.80.mean.after)
entropy5f.80.mean.after <- edag5f.80.mean.after$edag/e05f.80.mean.after

# Year-to-year
e05f.80.years <- Isl4151f5.80$ex[Isl4151f5.80$Age==0]
sd5f.80.years <- NA
for (i in 1841:1851){
  sd5f.80.years[i - 1840] <- sd.fun(Isl4151f5.80[Year==i])
}
sd5f.80.years <- as.numeric(sd5f.80.years)

aGini5f.80.years <- NA
for (i in 1841:1851){
  aGini5f.80.years[i - 1840] <- e05f.80.years[i - 1840]*
    Gini.fun(x = Isl4151f5.80$Age[Isl4151f5.80$Year==i],
             nax = Isl4151f5.80$ax[Isl4151f5.80$Year==i],
             ndx = Isl4151f5.80$dx[Isl4151f5.80$Year==i]/100000,
             ex = Isl4151f5.80$ex[Isl4151f5.80$Year==i])
}

edag5f.80.years <- NA
for (i in 1841:1851){
  edag5f.80.years[i - 1840] <- edag.fun(Isl4151f5.80[Year==i],n=Isl1846f5.80$n)
}
edag5f.80.years <- as.numeric(edag5f.80.years)

cv5f.80.years <- sd5f.80.years/e05f.80.years
rGini5f.80.years <- NA
for (i in 1841:1851){
  rGini5f.80.years[i - 1840] <- Gini.fun(x = Isl4151f5.80$Age[Isl4151f5.80$Year==i],
                                         nax = Isl4151f5.80$ax[Isl4151f5.80$Year==i],
                                         ndx = Isl4151f5.80$dx[Isl4151f5.80$Year==i]/100000,
                                         ex = Isl4151f5.80$ex[Isl4151f5.80$Year==i])
}
entropy5f.80.years <- edag5f.80.years/e05f.80.years



#-------
# MALES
#-------

# 1841-1851
Isl4151m5 <- subset(Isl5,subset=Year<1852 & Year>1840 & Sex=="m")

Isl4151m5.80 <- lt80(Isl4151m5[Year==1841])

for(i in unique(Isl4151m5$Year)[-1]){
  
  Isl4151m5.1 <- lt80(Isl4151m5[Year==i])
  
  Isl4151m5.80 <- rbind(Isl4151m5.80,Isl4151m5.1)
  
}

# 1846
Isl1846m5 <- subset(Isl5,subset=Year==1846 & Sex=="m")
Isl1846m5.80 <- lt80(Isl1846m5)

# Average of 5 years prior
Isl4145m5 <- subset(Isl5,subset=Year<1846 & Year>1840 & Sex=="m")

mean.mx.prior <- Isl4145m5[,list(mean.mx = mean(mx), mean.ax = mean(ax)),  by = list(Age)]

Isl46m5.mean.prior <- data.table(mean.mx.prior[,lifetable(mx=mean.mx,Age=Isl1846m5$Age,ax=mean.ax)])
Isl46m5.80.mean.prior <- lt80(Isl46m5.mean.prior)

# Average of 5 years after
Isl4751m5 <- subset(Isl5,subset=Year<1852 & Year>1846 & Sex=="m")

mean.mx.after <- Isl4751m5[,list(mean.mx = mean(mx), mean.ax = mean(ax)),  by = list(Age)]

Isl46m5.mean.after <- data.table(mean.mx.after[,lifetable(mx=mean.mx,Age=Isl1846m5$Age,ax=mean.ax)])
Isl46m5.80.mean.after <- lt80(Isl46m5.mean.after)

# Calculating indicators
#------------------------

# 1846
e05m.80      <- le0(Isl1846m5.80)
sd5m.80      <- sd.fun(Isl1846m5.80)
aGini5m.80   <- e05m.80*rGini.fun(Isl1846m5.80)
edag5m.80    <- edag.fun(Isl1846m5.80,n=Isl1846m5.80$n)
cv5m.80      <- sd5m.80/e05m.80
rGini5m.80   <- rGini.fun(Isl1846m5.80)
entropy5m.80 <- edag5m.80$edag/e05m.80

# Prior years
e05m.80.mean.prior      <- le0(Isl46m5.80.mean.prior)
sd5m.80.mean.prior      <- sd.fun(Isl46m5.80.mean.prior)
aGini5m.80.mean.prior   <- e05m.80.mean.prior*rGini.fun(Isl46m5.80.mean.prior)
edag5m.80.mean.prior    <- edag.fun(Isl46m5.80.mean.prior,n=Isl1846m5.80$n)
cv5m.80.mean.prior      <- sd5m.80.mean.prior/e05m.80.mean.prior
rGini5m.80.mean.prior   <- rGini.fun(Isl46m5.80.mean.prior)
entropy5m.80.mean.prior <- edag5m.80.mean.prior$edag/e05m.80.mean.prior

# Next years
e05m.80.mean.after      <- le0(Isl46m5.80.mean.after)
sd5m.80.mean.after      <- sd.fun(Isl46m5.80.mean.after)
aGini5m.80.mean.after   <- e05m.80.mean.after*rGini.fun(Isl46m5.80.mean.after)
edag5m.80.mean.after    <- edag.fun(Isl46m5.80.mean.after,n=Isl1882m5.80$n)
cv5m.80.mean.after      <- sd5m.80.mean.after/e05m.80.mean.after
rGini5m.80.mean.after   <- rGini.fun(Isl46m5.80.mean.after)
entropy5m.80.mean.after <- edag5m.80.mean.after$edag/e05m.80.mean.after

# Year-to-year
e05m.80.years <- Isl4151m5.80$ex[Isl4151m5.80$Age==0]
sd5m.80.years <- NA
for (i in 1841:1851){
  sd5m.80.years[i - 1840] <- sd.fun(Isl4151m5.80[Year==i])
}
sd5m.80.years <- as.numeric(sd5m.80.years)

aGini5m.80.years <- NA
for (i in 1841:1851){
  aGini5m.80.years[i - 1840] <- e05m.80.years[i - 1840]*
    Gini.fun(x = Isl4151m5.80$Age[Isl4151m5.80$Year==i],
             nax = Isl4151m5.80$ax[Isl4151m5.80$Year==i],
             ndx = Isl4151m5.80$dx[Isl4151m5.80$Year==i]/100000,
             ex = Isl4151m5.80$ex[Isl4151m5.80$Year==i])
}

edag5m.80.years <- NA
for (i in 1841:1851){
  edag5m.80.years[i - 1840] <- edag.fun(Isl4151m5.80[Year==i],n=Isl1846m5.80$n)
}
edag5m.80.years <- as.numeric(edag5m.80.years)

cv5m.80.years <- sd5m.80.years/e05m.80.years
rGini5m.80.years <- NA
for (i in 1841:1851){
  rGini5m.80.years[i - 1840] <- Gini.fun(x = Isl4151m5.80$Age[Isl4151m5.80$Year==i],
                                         nax = Isl4151m5.80$ax[Isl4151m5.80$Year==i],
                                         ndx = Isl4151m5.80$dx[Isl4151m5.80$Year==i]/100000,
                                         ex = Isl4151m5.80$ex[Isl4151m5.80$Year==i])
}
entropy5m.80.years <- edag5m.80.years/e05m.80.years







