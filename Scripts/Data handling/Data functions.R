#-----------------------------------------------------
# Topic: Functions mortality, crises and lifespan variation
# Author: Serena Vigezzi (code from AvR, GC and JMA)
# Date: 7/04/2020
#-----------------------------------------------------

# Lifetable function (from Giancarlo and Allison)
lifetable <- function(mx,Age,ax){
  n <- c(diff(Age),ax[length(ax)]) # this is a vector of the length of the age categories
  # we should first check that we don't have any NaN in the mx vector
  # These are typically caused by 0 person-years in the denominator
  mx_na <- which(is.na(mx==T))
  # if so, we can close the lifetable at the last age before the NaN
  last <- mx_na[1]-1
  if(is.na(last)==F){
    mx <- mx[1:last]
    n <- n[1:last]
    ax <- ax[1:last]
    n[last] <- ax[last] <- 1/mx[last]
    Age <- Age[1:last]
  }
  # now running through all of the lifetable functions
  # probability of dying and surviving
  qx <- n*mx / (1+(n-ax)*mx)
  px <- 1 - qx
  # number of survivors using a radix of 100 000
  lx <- rep(NA,length(qx))
  lx[1] <- 100000
  for(i in 1:length(n)){
    lx[i+1] <- lx[i]*px[i]
  }
  lx <- lx[1:length(n)]
  # number of deaths (difference between the number of survivors at each age)
  # for open-aged interval it is the number of survivors at the open aged interval
  dx <- c(abs(diff(lx)),lx[length(n)])
  # person-years lived (contribution of the living plus the contribution of the dead)
  Lx <- rep(NA,length(n))
  for(i in 1:(length(n)-1)){
    Lx[i] <- lx[i+1]*n[i] + dx[i]*ax[i]
  }
  Lx[length(n)] <- lx[length(n)] * ax[length(n)]
  
  1
  
  # person-years lived above age x
  Tx = rev(cumsum(rev(Lx)))
  # remaining life expectancy
  ex <- Tx/lx
  # lifetable
  lifetable <- data.table(Age=Age,n=n,mx=round(mx,5),qx=round(qx,5),
                          px=round(px,5),ax=round(ax,2),
                          dx=round(dx,0),lx=round(lx,0),Lx=round(Lx,0),
                          Tx=round(Tx,0),ex=round(ex,2))
  
  return(lifetable)
}

# Lifetable function reversed (starting from lx and dx)

lifetable.rv <- function(lx,dx,ax,Age){
  n <- c(diff(Age),ax[length(ax)]) # this is a vector of the length of the age categories
  
  # we should first check that we don't have any NaN in the mx vector
  # These are typically caused by 0 person-years in the denominator
  
  lx_0 <- which(lx==0)
  # if so, we can close the lifetable at the last age before the NaN
  last <- lx_0[1]-1
  if(is.na(last)==F){
    n <- n[1:last]
    ax <- ax[1:last]
    Age <- Age[1:last]
    lx <- lx[1:last]
    dx <- dx[1:last]
    dx[last] <- lx[last]
  }
  
  # now running through all of the lifetable functions
  # probability of dying and surviving
  qx <- dx/lx
  qx <- c(qx[-(length(qx))], 1)
  qx[qx > 1] <- 1
  
  px <- 1 - qx
  
  # mortality rates
  mx <- qx/(n - qx*(n - ax))
  
  # person-years lived (contribution of the living plus the contribution of the dead)
  Lx <- rep(NA,length(n))
  for(i in 1:(length(n)-1)){
    Lx[i] <- lx[i+1]*n[i] + dx[i]*ax[i]
  }
  Lx[length(n)] <- lx[length(n)] * ax[length(n)]
  
  # person-years lived above age x
  Tx = rev(cumsum(rev(Lx)))
  # remaining life expectancy
  ex <- Tx/lx
  # lifetable
  lifetable <- data.frame(Age=Age,n=n,mx=round(mx,5),qx=round(qx,5),
                          px=round(px,5),ax=round(ax,2),
                          dx=round(dx,0),lx=round(lx,0),Lx=round(Lx,0),
                          Tx=round(Tx,0),ex=round(ex,2))
  
  return(lifetable)
}


# Life expectancy
#-----------------

le0 <- function(data){
  data$ex[data$Age==0]
}

# Variability indices
#----------------------

# Standard deviation

sd.fun <- function(data,n=1){
  data[,list(sd = sqrt(sum(dx/lx[1]*(Age + ax - ex[1])^2)))]
}

# Lifespan disparity

edag.fun <- function(data,n=1,na.rm=F){
  data[,list(edag = sum(((ax/n)*c(ex[-1L], 0) + (1-ax/n)*ex)*dx/lx[1],na.rm=na.rm))]
}

# Relative Gini

# Gini function from PASH
Gini.fun <- function (x, nax, ndx, ex) {
  e = rep(1, length(x))
  D = outer(ndx, ndx)
  x_ = x+nax
  X_ = abs(e%*%t(x_) - x_%*%t(e))
  G = sum(D*X_)/(2*ex[1L])
  return(g=G)
}

rGini.fun <- function(data){
  data[,list(Gini = Gini.fun(x = Age,nax = ax,ndx = dx/100000,ex = ex))]
}

#######################################
# Creating grouped ages lifetables    #
#######################################

# Using lx and sum of dx
group1to5rv <- function(lx,dx,ax,Age=c(0,1,seq(5,89,5))){
  n <- c(diff(Age),1)
  
  lx5 <- rep(NA,length(n))
  for(i in 1:length(n)){
    lx5[i] <- lx[sum(n[1:i-1])+1]
  }
  
  dx5 <- rep(NA,length(n))
  for(i in 1:length(n)){
    dx5[i] <- sum(dx[(sum(n[1:i-1])+1):sum(n[1:i])])
  }
  
  ax5 <- rep(NA,length(n))
  for(i in 1:length(n)){
    ax5[i] <- sum(ax[(sum(n[1:i-1])+1):sum(n[1:i])])
  }
  
  lifetable <- data.table(lifetable.rv(lx5,dx5,ax5,Age))
  return(lifetable)
}

######################################
# Threshold age (Jose Manuel Aburto) #
######################################

age.H<- function(Age, ax, dx, lx, ex){ 
  
  ax<- ax
  dx<- dx /100000
  lx<- lx /100000
  ex<- ex
  Age <-  Age
  
  #Age[Age == "110+"] <-110
  Age <- as.integer(as.character(Age))
  #e-dagger at age x
  ex.bar    <- ex +ax*(ex - c(ex[-1], ex[length(ex)]))
  ex.bar[length(ex.bar)] <- ex
  ex.bar.dx <- ex.bar * dx
  e.dag.x   <-  rev(cumsum(rev(ex.bar.dx)))/ lx
  
  #Keyfitz's entropy at age x
  Hx <- e.dag.x / ex
  
  #Cumulative hazard
  cumhaz <- -log(lx)
  
  #g(x)
  gx <- cumhaz + Hx - 1- Hx[1]
  
  #wx --> whole weights (W(x)w(x))
  
  wx <- (dx / Hx) * gx
  
  #a.dagger
  a.dag <- ex * (1- cumhaz)
  
  #get threshold age of H
  
  return(data.frame(Age,ex, lx, e.dag.x, Hx, cumhaz, gx, wx, a.dag))
}

get.a <- function(Age,gx,ex, e.dag.x, a.dag, fage=80){
  # fage == final age (open-ended ge category)
  e0 <-  ex[1]
  f1 <- approxfun(Age,gx,method = "linear",rule=2 )
  a.H <- uniroot(function(x) f1(x),c(0,fage))$root
  
  
  f2 <- approxfun(Age,e.dag.x,method = "linear",rule=2 )
  f3 <- approxfun(Age, a.dag ,method = "linear",rule=2 )
  
  a.dagger <- uniroot(function(x) f2(x) - f3(x),c(0,fage))$root
  
  result <-  data.frame(e0,a.H, a.dagger)
  return(result)
}

#########################
# Close lifetable at 80 #
#########################

lt80 <- function(data, age=80){
  
  if(data$Age[length(data$Age)]>=age){
    Agef <- ifelse(data$lx[data$Age==age]>0, 
                   age, 
                   data$Age[which(data$lx>0)][length(which(data$lx>0))])
  } else{
    Agef <- data$Age[which(data$lx>0)][length(which(data$lx>0))]
  }
  
  lx.new <- sum(data$lx[data$Age==Agef])
  
  dx.new <- sum(data$dx[data$Age>=Agef])
  
  ax.new <- ifelse(lx.new==data$lx[data$Age==Agef],
                   data$ax[data$Age==Agef],
                   sum(data$ax[data$Age>=Agef & data$lx!=0]))
  
  qx.new <- 1
  
  px.new <- 0
  
  Lx.new <- sum(data$Lx[data$Age>=Agef])
  
  Tx.new <- sum(data$Tx[data$Age>=Agef])
  
  ex.new <- Tx.new/lx.new
  
  mx.new <- ifelse(Lx.new==0,
                   0,
                   dx.new/Lx.new)
  
  n <- c(diff(data$Age[data$Age<=Agef]),ax.new)
  mx <- c(data$mx[data$Age<Agef], mx.new)
  qx <- c(data$qx[data$Age<Agef], qx.new)
  px <- c(data$px[data$Age<Agef], px.new)
  ax <- c(data$ax[data$Age<Agef], ax.new)
  dx <- c(data$dx[data$Age<Agef], dx.new)
  lx <- c(data$lx[data$Age<Agef], lx.new)
  Lx <- c(data$Lx[data$Age<Agef], Lx.new)
  Tx <- c(data$Tx[data$Age<Agef], Tx.new)
  ex <- c(data$ex[data$Age<Agef], ex.new)
  
  Age <- c(data$Age[data$Age<Agef],Agef)
  Year <- data$Year[data$Age<=Agef]
  
  lifetable <- data.table(Year=Year,Age=Age,
                          n=n,mx=round(mx,5),qx=round(qx,5),
                          px=round(px,5),ax=round(ax,2),
                          dx=round(dx,0),lx=round(lx,0),Lx=round(Lx,0),
                          Tx=round(Tx,0),ex=round(ex,2))
  
  return(lifetable)
}




# Sensitivity tests
######################

# Increase/decrease mx of all ages in same proportion

plusminus.mx <- function(olddata, value){
  
  newvalue <- value/100
  
  data <- olddata[,list(mx = mx + (mx*(newvalue))), by = list(Year, Age)]
  
  newdata <- lifetable(mx = data$mx[data$Year == min(data$Year)],
                       ax = olddata$ax[data$Year == min(data$Year)],
                       Age = olddata$Age[data$Year == min(data$Year)])
  
  for(i in unique(data$Year)[-1]){
    
    newdata.1 <- lifetable(mx = data$mx[data$Year==i],
                           ax = olddata$ax[data$Year == i],
                           Age = olddata$Age[data$Year == i])
    
    newdata <- rbind(newdata,newdata.1)
  }
  
  newdata <- cbind(Year = olddata$Year, newdata)
  
  return(newdata)
}

# Increase/decrease mx of infants

plusminus.inf.mx <- function(olddata, value){
  
  newvalue <- value/100
  
  data <- olddata[,list(mx = mx), by = list(Year, Age)]
  
  data$mx[data$Age==0] <- data$mx[data$Age==0] + data$mx[data$Age==0]*(newvalue)
  
  newdata <- lifetable(mx = data$mx[data$Year == min(data$Year)],
                       ax = olddata$ax[data$Year == min(data$Year)],
                       Age = olddata$Age[data$Year == min(data$Year)])
  
  for(i in unique(data$Year)[-1]){
    
    newdata.1 <- lifetable(mx = data$mx[data$Year==i],
                           ax = olddata$ax[data$Year == i],
                           Age = olddata$Age[data$Year == i])
    
    newdata <- rbind(newdata,newdata.1)
  }
  
  newdata <- cbind(Year = olddata$Year, newdata)
  
  return(newdata)
}




