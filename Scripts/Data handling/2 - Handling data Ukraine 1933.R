#--------------------------------------------------------------
# Topic: Handling data Ukraine 1933, mortality crises and lifespan variation
# Author: Serena Vigezzi
# Date: 29/10/2020
#--------------------------------------------------------------

source("Path/Data functions.R")


#-------------
# FEMALES
#-------------


load("Path/Data/UkrF5.RData")


# Selecting data
#---------------

# Defining the interval
n <- c(1,4,rep(5,22))

# 1927-1939

UkrF5.80 <- lt80(UkrF5[Year==1927])

for(i in unique(UkrF5$Year)[-1]){
  
  UkrF5.1 <- lt80(UkrF5[Year==i])
  
  UkrF5.80 <- rbind(UkrF5.80,UkrF5.1)
  
}

# 1933
Ukr1933f5 <- subset(UkrF5,subset=Year==1933)
Ukr1933f5.80 <- lt80(Ukr1933f5)

# Average of 5 years prior
Ukr2731f5 <- subset(UkrF5,subset=Year<1932 & Year>1926)

mean.mx.prior <- Ukr2731f5[,list(mean.mx = mean(mx), mean.ax = mean(ax)),  by = list(Age)]

Ukr33f5.mean.prior <- data.table(mean.mx.prior[,lifetable(mx=mean.mx,Age=UkrF5$Age[UkrF5$Year==1932],ax=mean.ax)])
Ukr33f5.80.mean.prior <- lt80(Ukr33f5.mean.prior)

# Average of 5 years after
Ukr3438f5 <- subset(UkrF5,subset=Year<1939 & Year>1933)

mean.mx.after <- Ukr3438f5[,list(mean.mx = mean(mx), mean.ax = mean(ax)),  by = list(Age)]

Ukr33f5.mean.after <- data.table(mean.mx.after[,lifetable(mx=mean.mx,Age=UkrF5$Age[UkrF5$Year==1932],ax=mean.ax)])
Ukr33f5.80.mean.after <- lt80(Ukr33f5.mean.after)

# Calculating indicators
#------------------------

# 1933
e05f.80      <- le0(Ukr1933f5.80)
sd5f.80      <- sd.fun(Ukr1933f5.80)
aGini5f.80   <- e05f.80*rGini.fun(Ukr1933f5.80)
edag5f.80    <- edag.fun(Ukr1933f5.80,n=Ukr1933f5.80$n)
cv5f.80      <- sd5f.80/e05f.80
rGini5f.80   <- rGini.fun(Ukr1933f5.80)
entropy5f.80 <- edag5f.80$edag/e05f.80

# Prior years
e05f.80.mean.prior      <- le0(Ukr33f5.80.mean.prior)
sd5f.80.mean.prior      <- sd.fun(Ukr33f5.80.mean.prior)
aGini5f.80.mean.prior   <- e05f.80.mean.prior*rGini.fun(Ukr33f5.80.mean.prior)
edag5f.80.mean.prior    <- edag.fun(Ukr33f5.80.mean.prior,n=Ukr1933f5.80.mean.prior$n)
cv5f.80.mean.prior      <- sd5f.80.mean.prior/e05f.80.mean.prior
rGini5f.80.mean.prior   <- rGini.fun(Ukr33f5.80.mean.prior)
entropy5f.80.mean.prior <- edag5f.80.mean.prior$edag/e05f.80.mean.prior

# Next years
e05f.80.mean.after      <- le0(Ukr33f5.80.mean.after)
sd5f.80.mean.after      <- sd.fun(Ukr33f5.80.mean.after)
aGini5f.80.mean.after   <- e05f.80.mean.after*rGini.fun(Ukr33f5.80.mean.after)
edag5f.80.mean.after    <- edag.fun(Ukr33f5.80.mean.after,n=Ukr1933f5.80.mean.after$n)
cv5f.80.mean.after      <- sd5f.80.mean.after/e05f.80.mean.after
rGini5f.80.mean.after   <- rGini.fun(Ukr33f5.80.mean.after)
entropy5f.80.mean.after <- edag5f.80.mean.after$edag/e05f.80.mean.after

# Year-to-year
e05f.80.years <- UkrF5.80$ex[UkrF5.80$Age==0]
sd5f.80.years <- NA
for (i in 1927:1939){
  sd5f.80.years[i - 1926] <- sd.fun(UkrF5.80[Year==i])
}
sd5f.80.years <- as.numeric(sd5f.80.years)

aGini5f.80.years <- NA
for (i in 1927:1939){
  aGini5f.80.years[i - 1926] <- e05f.80.years[i - 1926]*
    Gini.fun(x = UkrF5.80$Age[UkrF5.80$Year==i],
             nax = UkrF5.80$ax[UkrF5.80$Year==i],
             ndx = UkrF5.80$dx[UkrF5.80$Year==i]/100000,
             ex = UkrF5.80$ex[UkrF5.80$Year==i])
}

edag5f.80.years <- NA
for (i in 1927:1939){
  edag5f.80.years[i - 1926] <- edag.fun(UkrF5.80[Year==i],n=Ukr1933m5.80$n)
}
edag5f.80.years <- as.numeric(edag5f.80.years)

cv5f.80.years <- sd5f.80.years/e05f.80.years
rGini5f.80.years <- NA
for (i in 1927:1939){
  rGini5f.80.years[i - 1926] <- Gini.fun(x = UkrF5.80$Age[UkrF5.80$Year==i],
                                         nax = UkrF5.80$ax[UkrF5.80$Year==i],
                                         ndx = UkrF5.80$dx[UkrF5.80$Year==i]/100000,
                                         ex = UkrF5.80$ex[UkrF5.80$Year==i])
}
entropy5f.80.years <- edag5f.80.years/e05f.80.years



#------------
# MALES
#------------

# Selecting data
#---------------

# Defining the interval
n <- c(1,4,rep(5,22))

# 1927-1939

UkrM5.80 <- lt80(UkrM5[Year==1927])

for(i in unique(UkrM5$Year)[-1]){
  
  UkrM5.1 <- lt80(UkrM5[Year==i])
  
  UkrM5.80 <- rbind(UkrM5.80,UkrM5.1)
  
}


# 1933
Ukr1933m5 <- subset(UkrM5,subset=Year==1933)
Ukr1933m5.80 <- lt80(Ukr1933m5)

# Average of 5 years prior
Ukr2731m5 <- subset(UkrM5,subset=Year<1932 & Year>1926)

mean.mx.prior <- Ukr2731m5[,list(mean.mx = mean(mx), mean.ax = mean(ax)),  by = list(Age)]

Ukr33m5.mean.prior <- data.table(mean.mx.prior[,lifetable(mx=mean.mx,Age=UkrM5$Age[UkrM5$Year==1932],ax=mean.ax)])
Ukr33m5.80.mean.prior <- lt80(Ukr33m5.mean.prior)

# Average of 5 years after
Ukr3438m5 <- subset(UkrM5,subset=Year<1939 & Year>1933)

mean.mx.after <- Ukr3438m5[,list(mean.mx = mean(mx), mean.ax = mean(ax)),  by = list(Age)]

Ukr33m5.mean.after <- data.table(mean.mx.after[,lifetable(mx=mean.mx,Age=UkrM5$Age[UkrM5$Year==1932],ax=mean.ax)])
Ukr33m5.80.mean.after <- lt80(Ukr33m5.mean.after)

# Calculating indicators
#------------------------

# 1933
e05m.80      <- le0(Ukr1933m5.80)
sd5m.80      <- sd.fun(Ukr1933m5.80)
aGini5m.80   <- e05m.80*rGini.fun(Ukr1933m5.80)
edag5m.80    <- edag.fun(Ukr1933m5.80,n=Ukr1933m5.80$n)
cv5m.80      <- sd5m.80/e05m.80
rGini5m.80   <- rGini.fun(Ukr1933m5.80)
entropy5m.80 <- edag5m.80$edag/e05m.80

# Prior years
e05m.80.mean.prior      <- le0(Ukr33m5.80.mean.prior)
sd5m.80.mean.prior      <- sd.fun(Ukr33m5.80.mean.prior)
aGini5m.80.mean.prior   <- e05m.80.mean.prior*rGini.fun(Ukr33m5.80.mean.prior)
edag5m.80.mean.prior    <- edag.fun(Ukr33m5.80.mean.prior,n=Ukr1933m5.80.mean.prior$n)
cv5m.80.mean.prior      <- sd5m.80.mean.prior/e05m.80.mean.prior
rGini5m.80.mean.prior   <- rGini.fun(Ukr33m5.80.mean.prior)
entropy5m.80.mean.prior <- edag5m.80.mean.prior$edag/e05m.80.mean.prior

# Next years
e05m.80.mean.after      <- le0(Ukr33m5.80.mean.after)
sd5m.80.mean.after      <- sd.fun(Ukr33m5.80.mean.after)
aGini5m.80.mean.after   <- e05m.80.mean.after*rGini.fun(Ukr33m5.80.mean.after)
edag5m.80.mean.after    <- edag.fun(Ukr33m5.80.mean.after,n=Ukr1933m5.80.mean.after$n)
cv5m.80.mean.after      <- sd5m.80.mean.after/e05m.80.mean.after
rGini5m.80.mean.after   <- rGini.fun(Ukr33m5.80.mean.after)
entropy5m.80.mean.after <- edag5m.80.mean.after$edag/e05m.80.mean.after

# Year-to-year
e05m.80.years <- UkrM5.80$ex[UkrM5.80$Age==0]
sd5m.80.years <- NA
for (i in 1927:1939){
  sd5m.80.years[i - 1926] <- sd.fun(UkrM5.80[Year==i])
}
sd5m.80.years <- as.numeric(sd5m.80.years)

aGini5m.80.years <- NA
for (i in 1927:1939){
  aGini5m.80.years[i - 1926] <- e05m.80.years[i - 1926]*
    Gini.fun(x = UkrM5.80$Age[UkrM5.80$Year==i],
             nax = UkrM5.80$ax[UkrM5.80$Year==i],
             ndx = UkrM5.80$dx[UkrM5.80$Year==i]/100000,
             ex = UkrM5.80$ex[UkrM5.80$Year==i])
}

edag5m.80.years <- NA
for (i in 1927:1939){
  edag5m.80.years[i - 1926] <- edag.fun(UkrM5.80[Year==i],n=Ukr1933m5.80$n)
}
edag5m.80.years <- as.numeric(edag5m.80.years)

cv5m.80.years <- sd5m.80.years/e05m.80.years
rGini5m.80.years <- NA
for (i in 1927:1939){
  rGini5m.80.years[i - 1926] <- Gini.fun(x = UkrM5.80$Age[UkrM5.80$Year==i],
                                         nax = UkrM5.80$ax[UkrM5.80$Year==i],
                                         ndx = UkrM5.80$dx[UkrM5.80$Year==i]/100000,
                                         ex = UkrM5.80$ex[UkrM5.80$Year==i])
}
entropy5m.80.years <- edag5m.80.years/e05m.80.years
