#--------------------------------------------------------------
# Topic: Handling data Ukraine 1933, mortality crises and lifespan variation
# Author: Serena Vigezzi
# Date: 29/10/2020
#--------------------------------------------------------------

setwd("Path/Data")

#------------
# FEMALES
#------------

#-------------------
# Reshaping the data
#--------------------

# dx
ukrFdx <- read.table("UkrFdx.txt", header=T)
names(ukrFdx) <- c("Age","1927","1928","1929","1930","1931","1932","1933","1934","1935","1936","1937","1938","1939")
ukrFdx[nrow(ukrFdx)+1,] <- c(90,rep(NA,13))

ukrFdx1 <- reshape(ukrFdx, idvar="Age",
                   varying=c("1927","1928","1929","1930","1931","1932","1933","1934","1935","1936","1937","1938","1939"),
                   v.names=c("Fdx"),
                   times=c("1927","1928","1929","1930","1931","1932","1933","1934","1935","1936","1937","1938","1939"),
                   new.row.names=1:1183,             
                   direction="long")

# ex
ukrFex <- read.table("UkrFex.txt", header=T)
names(ukrFex) <- c("Age","1927","1928","1929","1930","1931","1932","1933","1934","1935","1936","1937","1938","1939")

ukrFex1 <- reshape(ukrFex, idvar="Age",
                   varying=c("1927","1928","1929","1930","1931","1932","1933","1934","1935","1936","1937","1938","1939"),
                   v.names=c("Fex"),
                   times=c("1927","1928","1929","1930","1931","1932","1933","1934","1935","1936","1937","1938","1939"),
                   new.row.names=1:1183,             
                   direction="long")

# lx
ukrFlx <- read.table("UkrFlx.txt", header=T)
names(ukrFlx) <- c("Age","1927","1928","1929","1930","1931","1932","1933","1934","1935","1936","1937","1938","1939")

ukrFlx1 <- reshape(ukrFlx, idvar="Age",
                   varying=c("1927","1928","1929","1930","1931","1932","1933","1934","1935","1936","1937","1938","1939"),
                   v.names=c("Flx"),
                   times=c("1927","1928","1929","1930","1931","1932","1933","1934","1935","1936","1937","1938","1939"),
                   new.row.names=1:1183,             
                   direction="long")

# qx
ukrFqx <- read.table("UkrFqx_1x1.txt", header=T)
names(ukrFqx) <- c("Age","1927","1928","1929","1930","1931","1932","1933","1934","1935","1936","1937","1938","1939")
ukrFqx[nrow(ukrFqx)+1,] <- c(90,rep(NA,13))


ukrFqx1 <- reshape(ukrFqx, idvar="Age",
                   varying=c("1927","1928","1929","1930","1931","1932","1933","1934","1935","1936","1937","1938","1939"),
                   v.names=c("Fqx"),
                   times=c("1927","1928","1929","1930","1931","1932","1933","1934","1935","1936","1937","1938","1939"),
                   new.row.names=1:1183,             
                   direction="long")

# Whole dataframe
#---------------------------

UkrF <- data.frame(Year=rep(1927:1939,rep(90,13)),
                   Age=rep(0:89,13),
                   qx=ukrFqx1$Fqx[ukrFqx1$Age!=90],
                   lx=ukrFlx1$Flx[ukrFqx1$Age!=90],
                   dx=ukrFdx1$Fdx[ukrFqx1$Age!=90],
                   ex=ukrFex1$Fex[ukrFqx1$Age!=90])
UkrF[,ncol(UkrF)+1] <- rep(c(0.3,rep(0.5,89)),13)
names(UkrF) <- c("Year","Age","qx","lx","dx","ex","ax")

# Lx
UkrF$Lx <- rep(NA,length(UkrF$Year))
for(k in unique((UkrF$Year))){
  for(i in 1:(length(UkrF$Age[UkrF$Year==k])-1)){
    UkrF$Lx[i+(k-1927)*90] <- UkrF$lx[(i+(k-1927)*90)+1] + UkrF$dx[i+(k-1927)*90]*UkrF$ax[i+(k-1927)*90]
    UkrF$Lx[90+(k-1927)*90] <- UkrF$lx[90+(k-1927)*90] * UkrF$ax[90+(k-1927)*90]
    
  }
}

# Tx
UkrF$Tx <- rep(NA,length(UkrF$Year))
for(k in unique((UkrF$Year))){
  UkrF$Tx[UkrF$Year==k] <- rev(cumsum(rev(UkrF$Lx[UkrF$Year==k])))
  
}
# mx
UkrF$mx <- rep(NA,length(UkrF$Year))
for(i in 1:length(UkrF$Year)){
  UkrF$mx[i] <- UkrF$qx[i]/(1-UkrF$qx[i]+UkrF$qx[i]*UkrF$ax[i])
}

UkrF <- as.data.table(UkrF)
n <- UkrF[,list(c(diff(Age),ax[length(ax)])),by=list(Year)]
UkrF$n <- n$V1

save(UkrF,file="Path/Data/UkrF.RData")


# Abrdiged lifetable
#--------------------

UkrF5 <- data.table(UkrF[,group1to5rv(Age=c(0,1,seq(5,89,5)),lx=lx,ax=ax,dx=dx),by=list(Year)])


save(UkrF5, file="Path/Data/UkrF5.RData")

#--------------
# MALES
#--------------

# Reshaping the data
#--------------------

# dx
ukrMdx <- read.table("UkrMdx.txt", header=T)
names(ukrMdx) <- c("Age","1927","1928","1929","1930","1931","1932","1933","1934","1935","1936","1937","1938","1939")
ukrMdx[nrow(ukrMdx)+1,] <- c(90,rep(NA,13))

ukrMdx1 <- reshape(ukrMdx, idvar="Age",
                   varying=c("1927","1928","1929","1930","1931","1932","1933","1934","1935","1936","1937","1938","1939"),
                   v.names=c("Mdx"),
                   times=c("1927","1928","1929","1930","1931","1932","1933","1934","1935","1936","1937","1938","1939"),
                   new.row.names=1:1183,             
                   direction="long")

# ex
ukrMex <- read.table("UkrMex.txt", header=T)

ukrMex$Age <- c(0:90)

names(ukrMex) <- c("1927","1928","1929","1930","1931","1932","1933","1934","1935","1936","1937","1938","1939","Age")


ukrMex1 <- reshape(ukrMex, idvar="Age",
                   varying=c("1927","1928","1929","1930","1931","1932","1933","1934","1935","1936","1937","1938","1939"),
                   v.names=c("Mex"),
                   times=c("1927","1928","1929","1930","1931","1932","1933","1934","1935","1936","1937","1938","1939"),
                   new.row.names=1:1183,             
                   direction="long")

# lx
ukrMlx <- read.table("UkrMlx.txt", header=T)
names(ukrMlx) <- c("Age","1927","1928","1929","1930","1931","1932","1933","1934","1935","1936","1937","1938","1939")

ukrMlx1 <- reshape(ukrMlx, idvar="Age",
                   varying=c("1927","1928","1929","1930","1931","1932","1933","1934","1935","1936","1937","1938","1939"),
                   v.names=c("Mlx"),
                   times=c("1927","1928","1929","1930","1931","1932","1933","1934","1935","1936","1937","1938","1939"),
                   new.row.names=1:1183,             
                   direction="long")

# qx
ukrMqx <- read.table("UkrMqx_1x1.txt", header=T)
names(ukrMqx) <- c("Age","1927","1928","1929","1930","1931","1932","1933","1934","1935","1936","1937","1938","1939")
ukrMqx[nrow(ukrMqx)+1,] <- c(90,rep(NA,13))

ukrMqx1 <- reshape(ukrMqx, idvar="Age",
                   varying=c("1927","1928","1929","1930","1931","1932","1933","1934","1935","1936","1937","1938","1939"),
                   v.names=c("Mqx"),
                   times=c("1927","1928","1929","1930","1931","1932","1933","1934","1935","1936","1937","1938","1939"),
                   new.row.names=1:1183,             
                   direction="long")

# Creating whole dataframe
#--------------------------

UkrM <- data.frame(Year=rep(1927:1939,rep(90,13)),
                   Age=rep(0:89,13),
                   qx=ukrMqx1$Mqx[ukrMqx1$Age!=90],
                   lx=ukrMlx1$Mlx[ukrMqx1$Age!=90],
                   dx=ukrMdx1$Mdx[ukrMqx1$Age!=90],
                   ex=ukrMex1$Mex[ukrMqx1$Age!=90])
UkrM[,ncol(UkrM)+1] <- rep(c(0.3,rep(0.5,89)),13)

names(UkrM) <- c("Year","Age","qx","lx","dx","ex","ax")

# Lx
UkrM$Lx <- rep(NA,length(UkrM$Year))
for(k in unique((UkrM$Year))){
  for(i in 1:(length(UkrM$Age[UkrM$Year==k])-1)){
    UkrM$Lx[i+(k-1927)*90] <- UkrM$lx[(i+(k-1927)*90)+1] + UkrM$dx[i+(k-1927)*90]*UkrM$ax[i+(k-1927)*90]
    UkrM$Lx[90+(k-1927)*90] <- UkrM$lx[90+(k-1927)*90] * UkrM$ax[90+(k-1927)*90]
    
  }
}

# Tx
UkrM$Tx <- rep(NA,length(UkrM$Year))
for(k in unique((UkrM$Year))){
  UkrM$Tx[UkrM$Year==k] <- rev(cumsum(rev(UkrM$Lx[UkrM$Year==k])))
  
}

# mx
UkrM$mx <- rep(NA,length(UkrM$Year))
for(i in 1:length(UkrM$Year)){
  UkrM$mx[i] <- UkrM$qx[i]/(1-UkrM$qx[i]+UkrM$qx[i]*UkrM$ax[i])
}

UkrM <- as.data.table(UkrM)
n <- UkrM[,list(c(diff(Age),ax[length(ax)])),by=list(Year)]
UkrM$n <- n$V1
save(UkrM,file="Path/Data/UkrM.RData")


# Abridged lifetable
#----------------------

UkrM5 <- data.table(UkrM[,group1to5rv(Age=c(0,1,seq(5,89,5)),lx=lx,ax=ax,dx=dx),by=list(Year)])
save(UkrM5, file="Path/Data/UkrM5.RData")
