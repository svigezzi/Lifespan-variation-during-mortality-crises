#-----------------------------------------------------
# Topic: Functions for decomposition, crises and lifespan variation
# Author: Serena Vigezzi (code from AvR, JMA and TR)
# Date: 7/04/2020
#-----------------------------------------------------

# Adaptation of Tim Riffe's horiuchi function
horiuchi5 <- function (func, pars1, pars2, N, ax1, ax2,age=c(0,1,seq(5,80,5)), na.rm=F) 
{
  y1 <- func(pars1,ax1,age)
  y2 <- func(pars2,ax2,age)
  d <- pars2[1:min(length(pars1),length(pars2))] - pars1[1:min(length(pars1),length(pars2))]
  n <- length(pars1)
  delta <- d/N
  x <- pars1 + d * matrix(rep(0.5:(N - 0.5)/N, n), byrow = TRUE, 
                          ncol = N)
  cc <- matrix(0, nrow = n, ncol = N)
  zeros <- rep(0, n)
  for (j in 1:N) {
    DD <- diag(delta/2)
    for (i in 1:n) {
      cc[i, j] <- func((x[, j] + DD[, i]),(ax1+ax2)/2,age) - 
        func((x[, j] - DD[, i]),(ax1+ax2)/2,age)
    }
  }
  return(rowSums(cc, na.rm=na.rm))
}


# Variation indices
#-------------------

edag5.dec <- function(nmx=mx,nax=ax,age=c(0,1,seq(5,80,5))){
  
  n   <- c(diff(age), nax[length(nax)])
  
  nqx          <- (n * nmx)/(1 + (n - nax) * nmx)
  nqx          <- c(nqx[-(length(nqx))], 1)
  nqx[nqx > 1] <- 1
  
  npx <- 1 - nqx
  lx <- cumprod(c(1, npx))
  ndx <- -diff(lx)
  lxpn <- lx[-1]
  nLxpn <- n * lxpn + ndx * nax
  nLx <- c(nLxpn[-length(nLxpn)], lxpn[length(lxpn)-1]/nmx[length(nmx)])
  Tx <- rev(cumsum(rev(nLx)))
  lx <- lx[1:length(age)]
  ex <- Tx/lx
  
  # 
  edag <- sum((((nax/n)*c(ex[-1L], 0) + (1-nax/n)*ex)*ndx/lx[1]))
  
  return(edag)
}

h5.dec <- function(nmx=mx,nax=ax,age=c(0,1,seq(5,80,5))){
  
  n   <- c(diff(age), 999)
  
  nqx          <- (n * nmx)/(1 + (n - nax) * nmx)
  nqx          <- c(nqx[-(length(nqx))], 1)
  nqx[nqx > 1] <- 1
  
  npx <- 1 - nqx
  lx <- cumprod(c(1, npx))
  ndx <- -diff(lx)
  lxpn <- lx[-1]
  nLxpn <- n * lxpn + ndx * nax
  nLx <- c(nLxpn[-length(nLxpn)], lxpn[length(lxpn)-1]/nmx[length(nmx)])
  Tx <- rev(cumsum(rev(nLx)))
  lx <- lx[1:length(age)]
  ex <- Tx/lx
  
  # 
  edag <- sum((((nax/n)*c(ex[-1L], 0) + (1-nax/n)*ex)*ndx/lx[1]))
  h <- edag/ex[1]
  
  return(h)
  
}

sd5.dec <- function(nmx=mx,nax=ax,age=c(0,1,seq(5,80,5))){
  
  n   <- c(diff(age), 999)
  
  nqx          <- (n * nmx)/(1 + (n - nax) * nmx)
  nqx          <- c(nqx[-(length(nqx))], 1)
  nqx[nqx > 1] <- 1
  
  npx <- 1 - nqx
  lx <- cumprod(c(1, npx))
  ndx <- -diff(lx)
  lxpn <- lx[-1]
  nLxpn <- n * lxpn + ndx * nax
  nLx <- c(nLxpn[-length(nLxpn)], lxpn[length(lxpn)-1]/nmx[length(nmx)])
  Tx <- rev(cumsum(rev(nLx)))
  lx <- lx[1:length(age)]
  ex <- Tx/lx
  
  # 
  sd <-  sqrt(sum(ndx/lx[1]*(age + nax - ex[1])^2))
  
  return(sd)
}

cv5.dec <- function(nmx=mx,nax=ax,age=c(0,1,seq(5,80,5))){
  
  n   <- c(diff(age), 999)
  
  nqx          <- (n * nmx)/(1 + (n - nax) * nmx)
  nqx          <- c(nqx[-(length(nqx))], 1)
  nqx[nqx > 1] <- 1
  
  npx <- 1 - nqx
  lx <- cumprod(c(1, npx))
  ndx <- -diff(lx)
  lxpn <- lx[-1]
  nLxpn <- n * lxpn + ndx * nax
  nLx <- c(nLxpn[-length(nLxpn)], lxpn[length(lxpn)-1]/nmx[length(nmx)])
  Tx <- rev(cumsum(rev(nLx)))
  lx <- lx[1:length(age)]
  ex <- Tx/lx
  
  # 
  sd <-  sqrt(sum(ndx/lx[1]*(age + nax - ex[1])^2))
  cv <- sd/ex[1]
  
  return(cv)
}

aG5.dec <- function(nmx=mx,nax=ax,age=c(0,1,seq(5,80,5))){
  
  n   <- c(diff(age), 999)
  
  nqx          <- (n * nmx)/(1 + (n - nax) * nmx)
  nqx          <- c(nqx[-(length(nqx))], 1)
  nqx[nqx > 1] <- 1
  
  npx <- 1 - nqx
  lx <- cumprod(c(1, npx))
  ndx <- -diff(lx)
  lxpn <- lx[-1]
  nLxpn <- n * lxpn + ndx * nax
  nLx <- c(nLxpn[-length(nLxpn)], lxpn[length(lxpn)-1]/nmx[length(nmx)])
  Tx <- rev(cumsum(rev(nLx)))
  lx <- lx[1:length(age)]
  ex <- Tx/lx
  
  # 
  aG <- ex[1]*Gini.fun(x = age,nax = nax,ndx = ndx,ex = ex)
  
  return(aG)
}

rG5.dec <- function(nmx=mx,nax=ax,age=c(0,1,seq(5,80,5))){
  
  n   <- c(diff(age), 999)
  
  nqx          <- (n * nmx)/(1 + (n - nax) * nmx)
  nqx          <- c(nqx[-(length(nqx))], 1)
  nqx[nqx > 1] <- 1
  
  npx <- 1 - nqx
  lx <- cumprod(c(1, npx))
  ndx <- -diff(lx)
  lxpn <- lx[-1]
  nLxpn <- n * lxpn + ndx * nax
  nLx <- c(nLxpn[-length(nLxpn)], lxpn[length(lxpn)-1]/nmx[length(nmx)])
  Tx <- rev(cumsum(rev(nLx)))
  lx <- lx[1:length(age)]
  ex <- Tx/lx
  
  # 
  rG <- Gini.fun(x = age,nax = nax,ndx = ndx,ex = ex)
  
  return(rG)
}


e05.dec <- function(nmx=mx,nax=ax,age=c(0,1,seq(5,80,5))){
  
  n   <- c(diff(age), 999)
  
  nqx          <- (n * nmx)/(1 + (n - nax) * nmx)
  nqx          <- c(nqx[-(length(nqx))], 1)
  nqx[nqx > 1] <- 1
  
  npx <- 1 - nqx
  lx <- cumprod(c(1, npx))
  ndx <- -diff(lx)
  lxpn <- lx[-1]
  nLxpn <- n * lxpn + ndx * nax
  nLx <- c(nLxpn[-length(nLxpn)], lxpn[length(lxpn)-1]/nmx[length(nmx)])
  Tx <- rev(cumsum(rev(nLx)))
  lx <- lx[1:length(age)]
  ex <- Tx/lx
  e0 <- ex[1]
  # 
  return(e0)
  
}



