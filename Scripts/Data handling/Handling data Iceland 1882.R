#--------------------------------------------------------------
# Topic: Handling data Iceland 1882, mortality crises and lifespan variation
# Author: Serena Vigezzi
# Date: 29/10/2020
#--------------------------------------------------------------

source("Path/Data functions.R")

load("Path/HMD_Data_5years.RData")

#----------------
# Selecting data
#----------------

# Defining the interval
n <- c(1,4,rep(5,20))

# Iceland
Isl5 <- subset(HMDL5, subset=PopName=="ISL")

#---------
# FEMALES
#---------

# 1877-1887
Isl7787f5 <- subset(Isl5,subset=Year<1888 & Year>1876 & Sex=="f")

Isl7787f5.80 <- lt80(Isl7787f5[Year==1877])

for(i in unique(Isl7787f5$Year)[-1]){
  
  Isl7787f5.1 <- lt80(Isl7787f5[Year==i])
  
  Isl7787f5.80 <- rbind(Isl7787f5.80,Isl7787f5.1)
  
}

# 1882
Isl1882f5 <- subset(Isl5,subset=Year==1882 & Sex=="f")
Isl1882f5.80 <- lt80(Isl1882f5)

# Average of 5 years prior
Isl7781f5 <- subset(Isl5,subset=Year<1882 & Year>1876 & Sex=="f")

mean.mx.prior <- Isl7781f5[,list(mean.mx = mean(mx), mean.ax = mean(ax)),  by = list(Age)]

Isl82f5.mean.prior <- data.table(mean.mx.prior[,lifetable(mx=mean.mx,Age=Isl1882f5$Age,ax=mean.ax)])
Isl82f5.80.mean.prior <- lt80(Isl82f5.mean.prior)

# Average of 5 years after
Isl8488f5 <- subset(Isl5,subset=Year<1888 & Year>1883 & Sex=="f")

mean.mx.after <- Isl8488f5[,list(mean.mx = mean(mx), mean.ax = mean(ax)),  by = list(Age)]

Isl82f5.mean.after <- data.table(mean.mx.after[,lifetable(mx=mean.mx,Age=Isl1882f5$Age,ax=mean.ax)])
Isl82f5.80.mean.after <- lt80(Isl82f5.mean.after)

# Calculating indicators
#------------------------

# 1882
e05f.80      <- le0(Isl1882f5.80)
sd5f.80      <- sd.fun(Isl1882f5.80)
aGini5f.80   <- e05f.80*rGini.fun(Isl1882f5.80)
edag5f.80    <- edag.fun(Isl1882f5.80,n=Isl1882f5.80$n)
cv5f.80      <- sd5f.80/e05f.80
rGini5f.80   <- rGini.fun(Isl1882f5.80)
entropy5f.80 <- edag5f.80$edag/e05f.80

# Prior years
e05f.80.mean.prior      <- le0(Isl82f5.80.mean.prior)
sd5f.80.mean.prior      <- sd.fun(Isl82f5.80.mean.prior)
aGini5f.80.mean.prior   <- e05f.80.mean.prior*rGini.fun(Isl82f5.80.mean.prior)
edag5f.80.mean.prior    <- edag.fun(Isl82f5.80.mean.prior,n=Isl1882f5.80$n)
cv5f.80.mean.prior      <- sd5f.80.mean.prior/e05f.80.mean.prior
rGini5f.80.mean.prior   <- rGini.fun(Isl82f5.80.mean.prior)
entropy5f.80.mean.prior <- edag5f.80.mean.prior$edag/e05f.80.mean.prior

# Next years
e05f.80.mean.after      <- le0(Isl82f5.80.mean.after)
sd5f.80.mean.after      <- sd.fun(Isl82f5.80.mean.after)
aGini5f.80.mean.after   <- e05f.80.mean.after*rGini.fun(Isl82f5.80.mean.after)
edag5f.80.mean.after    <- edag.fun(Isl82f5.80.mean.after,n=Isl1882f5.80$n)
cv5f.80.mean.after      <- sd5f.80.mean.after/e05f.80.mean.after
rGini5f.80.mean.after   <- rGini.fun(Isl82f5.80.mean.after)
entropy5f.80.mean.after <- edag5f.80.mean.after$edag/e05f.80.mean.after

# Year-to-year
e05f.80.years <- Isl7787f5.80$ex[Isl7787f5.80$Age==0]
sd5f.80.years <- NA
for (i in 1877:1887){
  sd5f.80.years[i - 1876] <- sd.fun(Isl7787f5.80[Year==i])
}
sd5f.80.years <- as.numeric(sd5f.80.years)

aGini5f.80.years <- NA
for (i in 1877:1887){
  aGini5f.80.years[i - 1876] <- e05f.80.years[i - 1876]*
    Gini.fun(x = Isl7787f5.80$Age[Isl7787f5.80$Year==i],
             nax = Isl7787f5.80$ax[Isl7787f5.80$Year==i],
             ndx = Isl7787f5.80$dx[Isl7787f5.80$Year==i]/100000,
             ex = Isl7787f5.80$ex[Isl7787f5.80$Year==i])
}

edag5f.80.years <- NA
for (i in 1877:1887){
  edag5f.80.years[i - 1876] <- edag.fun(Isl7787f5.80[Year==i],n=Isl1882f5.80$n)
}
edag5f.80.years <- as.numeric(edag5f.80.years)

cv5f.80.years <- sd5f.80.years/e05f.80.years
rGini5f.80.years <- NA
for (i in 1877:1887){
  rGini5f.80.years[i - 1876] <- Gini.fun(x = Isl7787f5.80$Age[Isl7787f5.80$Year==i],
                                         nax = Isl7787f5.80$ax[Isl7787f5.80$Year==i],
                                         ndx = Isl7787f5.80$dx[Isl7787f5.80$Year==i]/100000,
                                         ex = Isl7787f5.80$ex[Isl7787f5.80$Year==i])
}
entropy5f.80.years <- edag5f.80.years/e05f.80.years


#---------
# MALES
#---------

# 1877-1888
Isl7787m5 <- subset(Isl5,subset=Year<1888 & Year>1876 & Sex=="m")

Isl7787m5.80 <- lt80(Isl7787m5[Year==1877])

for(i in unique(Isl7787m5$Year)[-1]){
  
  Isl7787m5.1 <- lt80(Isl7787m5[Year==i])
  
  Isl7787m5.80 <- rbind(Isl7787m5.80,Isl7787m5.1)
  
}

# 1882
Isl1882m5 <- subset(Isl5,subset=Year==1882 & Sex=="m")
Isl1882m5.80 <- lt80(Isl1882m5)

# Average of 5 years prior
Isl7781m5 <- subset(Isl5,subset=Year<1882 & Year>1876 & Sex=="m")

mean.mx.prior <- Isl7781m5[,list(mean.mx = mean(mx), mean.ax = mean(ax)),  by = list(Age)]

Isl82m5.mean.prior <- data.table(mean.mx.prior[,lifetable(mx=mean.mx,Age=Isl1882m5$Age,ax=mean.ax)])
Isl82m5.80.mean.prior <- lt80(Isl82m5.mean.prior)

# Average of 5 years after
Isl8388m5 <- subset(Isl5,subset=Year<1888 & Year>1882 & Sex=="m")

mean.mx.after <- Isl8388m5[,list(mean.mx = mean(mx), mean.ax = mean(ax)),  by = list(Age)]

Isl82m5.mean.after <- data.table(mean.mx.after[,lifetable(mx=mean.mx,Age=Isl1882m5$Age,ax=mean.ax)])
Isl82m5.80.mean.after <- lt80(Isl82m5.mean.after)

# Calculating indicators
#------------------------

# 1882
e05m.80      <- le0(Isl1882m5.80)
sd5m.80      <- sd.fun(Isl1882m5.80)
aGini5m.80   <- e05m.80*rGini.fun(Isl1882m5.80)
edag5m.80    <- edag.fun(Isl1882m5.80,n=Isl1882m5.80$n)
cv5m.80      <- sd5m.80/e05m.80
rGini5m.80   <- rGini.fun(Isl1882m5.80)
entropy5m.80 <- edag5m.80$edag/e05m.80

# Prior years
e05m.80.mean.prior      <- le0(Isl82m5.80.mean.prior)
sd5m.80.mean.prior      <- sd.fun(Isl82m5.80.mean.prior)
aGini5m.80.mean.prior   <- e05m.80.mean.prior*rGini.fun(Isl82m5.80.mean.prior)
edag5m.80.mean.prior    <- edag.fun(Isl82m5.80.mean.prior,n=Isl1882m5.80$n)
cv5m.80.mean.prior      <- sd5m.80.mean.prior/e05m.80.mean.prior
rGini5m.80.mean.prior   <- rGini.fun(Isl82m5.80.mean.prior)
entropy5m.80.mean.prior <- edag5m.80.mean.prior$edag/e05m.80.mean.prior

# Next years
e05m.80.mean.after      <- le0(Isl82m5.80.mean.after)
sd5m.80.mean.after      <- sd.fun(Isl82m5.80.mean.after)
aGini5m.80.mean.after   <- e05m.80.mean.after*rGini.fun(Isl82m5.80.mean.after)
edag5m.80.mean.after    <- edag.fun(Isl82m5.80.mean.after,n=Isl1882m5.80$n)
cv5m.80.mean.after      <- sd5m.80.mean.after/e05m.80.mean.after
rGini5m.80.mean.after   <- rGini.fun(Isl82m5.80.mean.after)
entropy5m.80.mean.after <- edag5m.80.mean.after$edag/e05m.80.mean.after

# Year-to-year
e05m.80.years <- Isl7787m5.80$ex[Isl7787m5.80$Age==0]
sd5m.80.years <- NA
for (i in 1877:1887){
  sd5m.80.years[i - 1876] <- sd.fun(Isl7787m5.80[Year==i])
}
sd5m.80.years <- as.numeric(sd5m.80.years)

aGini5m.80.years <- NA
for (i in 1877:1887){
  aGini5m.80.years[i - 1876] <- e05m.80.years[i - 1876]*
    Gini.fun(x = Isl7787m5.80$Age[Isl7787m5.80$Year==i],
             nax = Isl7787m5.80$ax[Isl7787m5.80$Year==i],
             ndx = Isl7787m5.80$dx[Isl7787m5.80$Year==i]/100000,
             ex = Isl7787m5.80$ex[Isl7787m5.80$Year==i])
}

edag5m.80.years <- NA
for (i in 1877:1887){
  edag5m.80.years[i - 1876] <- edag.fun(Isl7787m5.80[Year==i],n=Isl1882m5.80$n)
}
edag5m.80.years <- as.numeric(edag5m.80.years)

cv5m.80.years <- sd5m.80.years/e05m.80.years
rGini5m.80.years <- NA
for (i in 1877:1887){
  rGini5m.80.years[i - 1876] <- Gini.fun(x = Isl7787m5.80$Age[Isl7787m5.80$Year==i],
                                         nax = Isl7787m5.80$ax[Isl7787m5.80$Year==i],
                                         ndx = Isl7787m5.80$dx[Isl7787m5.80$Year==i]/100000,
                                         ex = Isl7787m5.80$ex[Isl7787m5.80$Year==i])
}
entropy5m.80.years <- edag5m.80.years/e05m.80.years


