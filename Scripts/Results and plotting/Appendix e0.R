#--------------------------------------------------------------
# Topic: Plotting appendix e0, mortality crises and lifespan variation
# Author: Serena Vigezzi
# Date: 26/10/2020
#--------------------------------------------------------------

# Relationship e0xlifespan variation
#------------------------------------

# Edagger
#-----------

edag.countries <- HMDL[,list(edag = sum(((ax)*c(ex[-1L], 0) + (1-ax)*ex)*dx/lx[1])),
                       by=list(Year,PopName,Sex)]

e0.countries <- HMDL[,list(e0 = ex[1]),
                     by=list(Year,PopName,Sex)]

countries.e <- cbind(edag.countries,e0.countries[,4])

png("Path/e0xedag.png",
    width=1000,height=700)

ggplot() +
  geom_point(countries.e,mapping=aes(x=e0,y=edag, col="non-crisis"), size=2) +
  geom_point(countries.e[countries.e$PopName=="SWE" & countries.e$Sex=="f" &
                           (countries.e$Year==1773 | countries.e$Year==1809)],
             mapping=aes(x=e0,y=edag,col="crisis"),size=4) +
  geom_point(countries.e[countries.e$PopName=="ISL" & countries.e$Sex=="f" &
                           (countries.e$Year==1846 | countries.e$Year==1882)],
             mapping=aes(x=e0,y=edag,col="crisis"),size=4)+
  scale_colour_manual("", 
                      breaks = c("crisis", "non-crisis"),
                      values = c("#C93312","grey60"),
                      labels = c("Case studies","Other country-years")) +
  scale_x_continuous(expression("e"["0"]),seq(20,90,10), minor_breaks=NULL) +
  scale_y_continuous(expression("e"^"\u2020")) +
  theme_tufte(ticks=F) +
  theme(text = element_text(size = 40),
        axis.text.x = element_text(angle = 45, hjust = 1, size=45),
        axis.text.y = element_text(size=45),
        axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), "mm"), size = 50),
        legend.position = c(0.3,0.2), 
        legend.text = element_text(size = 40),
        legend.title = element_text(size = 40)) +
  grids()
dev.off()


# Entropy
#---------

e0.countries <- HMDL[,list(e0 = ex[1]),
                     by=list(Year,PopName,Sex)]

h.countries <- edag.countries$edag/e0.countries$e0

countries.h <- cbind(e0.countries,h=h.countries)

png("C:/Users/sevi/Documents/Lifespan_variation/Mortality_crises/e0xh.png",
    width=1000,height=700)

ggplot() +
  geom_point(countries.h,mapping=aes(x=e0,y=h, col="non-crisis"), size=2) +
  geom_point(countries.h[countries.h$PopName=="SWE" & countries.h$Sex=="f" &
                           (countries.h$Year==1773 | countries.h$Year==1809)],
             mapping=aes(x=e0,y=h,col="crisis"),size=4) +
  geom_point(countries.h[countries.h$PopName=="ISL" & countries.h$Sex=="f" &
                           (countries.h$Year==1846 | countries.h$Year==1882)],
             mapping=aes(x=e0,y=h,col="crisis"),size=4)+
  scale_colour_manual("", 
                      breaks = c("crisis", "non-crisis"),
                      values = c("#C93312","grey60"),
                      labels = c("Case studies","Other country-years")) +
  scale_x_continuous(expression("e"["0"]),seq(20,90,10), minor_breaks=NULL) +
  scale_y_continuous(expression(bar(H)),seq(0.25,1.5,0.5)) +
  theme_tufte(ticks=F) +
  theme(text = element_text(size = 40),
        axis.text.x = element_text(angle = 45, hjust = 1, size=45),
        axis.text.y = element_text(size=45),
        axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), "mm"), size = 50),
        legend.position = "none", 
        legend.text = element_text(size = 40),
        legend.title = element_text(size = 40)) +
  grids()
dev.off()

# Life expectancy trends
#------------------------

source("Path/Handling data, Sweden 1773 - F.R")
source("Path/Handling data, Sweden 1773 - M.R")

png("Path/e0, Sweden 1773.png",
    width=1000,height=700)

e0.plot(e0f.data=e05f.80.years,
        e0m.data=e05m.80.years,
        Years=1767:1778,
        xmin=1771.5,xmax=1773.5)

dev.off()



source("Path/Handling data, Sweden 1809 - F.R")
source("Path/Handling data, Sweden 1809 - M.R")

png("Path/e0, Sweden 1809.png",
    width=1000,height=700)

e0.plot(e0f.data=e05f.80.years,
        e0m.data=e05m.80.years,
        Years=1803:1814,
        xmin=1807.5,xmax=1809.5)

dev.off()



source("Path/Handling data, Ukraine 1933 - F.R")
source("Path/Handling data, Ukraine 1933 - M.R")

png("Path/e0, Ukraine 1933.png",
    width=1000,height=700)

e0.plot(e0f.data=e05f.80.years[1:12],
        e0m.data=e05m.80.years[1:12],
        Years=1927:1938,
        xmin=1931.5,xmax=1933.5)

dev.off()



source("Path/Handling data, Iceland 1846 - F.R")
source("Path/Handling data, Iceland 1846 - M.R")

png("Path/e0, Iceland 1846.png",
    width=1000,height=700)

e0.plot(e0f.data=e05f.80.years,
        e0m.data=e05m.80.years,
        Years=1841:1851,
        xmin=1845.5,xmax=1846.5)

dev.off()




source("Path/Handling data, Iceland 1882 - F.R")
source("Path/Handling data, Iceland 1882 - M.R")

png("Path/e0, Iceland 1882.png",
    width=1000,height=700)

e0.plot(e0f.data=e05f.80.years,
        e0m.data=e05m.80.years,
        Years=1877:1887,
        xmin=1881.5,xmax=1882.5)

dev.off()

# Decomposition
#---------------

source("Path/Handling data, Sweden 1773 - F.R")
source("Path/Handling data, Sweden 1773 - M.R")

merged_e0 <- dec.crisis2(func=e05.dec,ukr=F,
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

dec.plot2(merged=merged_e0,breaks=c(-3,0,3),ylim=c(-6.5,6.5),
          title="Change in life expectancy by 5-year age groups, Sweden 1773",
          fill="#008080")

dev.off()




source("Path/Handling data, Sweden 1809 - F.R")
source("Path/Handling data, Sweden 1809 - M.R")

merged_e0 <- dec.crisis2(func=e05.dec,ukr=F,
                         parsf.prior=Swe09f5.80.mean.prior$mx,
                         parsf.first=Swe0314f5.80$mx[Swe0314f5.80$Year==1808],
                         parsf.second=Swe1809f5.80$mx,
                         parsf.after=Swe09f5.80.mean.after$mx,
                         axf.prior=Swe09f5.80.mean.prior$ax,
                         axf.first=Swe0314f5.80$ax[Swe0314f5.80$Year==1808],
                         axf.second=Swe1809f5.80$ax,
                         axf.after=Swe09f5.80.mean.after$ax,                        
                         parsm.prior=Swe09m5.80.mean.prior$mx,
                         parsm.first=Swe0314m5.80$mx[Swe0314m5.80$Year==1808],
                         parsm.second=Swe1809m5.80$mx,
                         parsm.after=Swe09m5.80.mean.after$mx,
                         axm.prior=Swe09m5.80.mean.prior$ax,
                         axm.first=Swe0314m5.80$ax[Swe0314m5.80$Year==1808],
                         axm.second=Swe1809m5.80$ax,
                         axm.after=Swe09m5.80.mean.after$ax)

png("Path/Edag dec, Sweden 1809.png",
    width=1000,height=700)

dec.plot2(merged=merged_e0,breaks=c(-2,0,2),ylim=c(-2.5,2.5),
          title="Change in life expectancy by 5-year age groups, Sweden 1809",
          fill="#008080")

dev.off()





source("Path/Handling data, Ukraine 1933 - F.R")
source("Path/Handling data, Ukraine 1933 - M.R")


merged_e0 <- dec.crisis2(func=e05.dec,ukr=T,
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

dec.plot2(merged=merged_e0,breaks=c(-5,0,5),ylim=c(-10.5,10.5),
          title="Change in life expectancy by 5-year age groups, Ukraine 1933",
          fill="#008080")

dev.off()





source("Path/Handling data, Iceland 1846 - F.R")
source("Path/Handling data, Iceland 1846 - M.R")

merged_e0 <- dec.crisis1(func=e05.dec,
                         parsf.prior=Isl46f5.80.mean.prior$mx,
                         parsf.crisis=Isl1846f5.80$mx,
                         parsf.after=Isl46f5.80.mean.after$mx,
                         axf.prior=Isl46f5.80.mean.prior$ax,
                         axf.crisis=Isl1846f5.80$ax,
                         axf.after=Isl46f5.80.mean.after$ax,                        
                         parsm.prior=Isl46m5.80.mean.prior$mx,
                         parsm.crisis=Isl1846m5.80$mx,
                         parsm.after=Isl46m5.80.mean.after$mx,
                         axm.prior=Isl46m5.80.mean.prior$ax,
                         axm.crisis=Isl1846m5.80$ax,
                         axm.after=Isl46m5.80.mean.after$ax)

png("Path/Edag dec, Iceland 1846.png",
    width=1000,height=700)

dec.plot1(merged=merged_e0,breaks=c(-6,0,6),ylim=c(-12.5,12.5),
          title="Change in life expectancy by 5-year age groups, Iceland 1846",
          fill="#008080")

dev.off()




source("Path/Handling data, Iceland 1882 - F.R")
source("Path/Handling data, Iceland 1882 - M.R")

merged_e0 <- dec.crisis1(func=e05.dec,
                         parsf.prior=Isl82f5.80.mean.prior$mx,
                         parsf.crisis=Isl1882f5.80$mx,
                         parsf.after=Isl82f5.80.mean.after$mx,
                         axf.prior=Isl82f5.80.mean.prior$ax,
                         axf.crisis=Isl1882f5.80$ax,
                         axf.after=Isl82f5.80.mean.after$ax,                        
                         parsm.prior=Isl82m5.80.mean.prior$mx,
                         parsm.crisis=Isl1882m5.80$mx,
                         parsm.after=Isl82m5.80.mean.after$mx,
                         axm.prior=Isl82m5.80.mean.prior$ax,
                         axm.crisis=Isl1882m5.80$ax,
                         axm.after=Isl82m5.80.mean.after$ax)

png("Path/Edag dec, Iceland 1882.png",
    width=1000,height=700)

dec.plot1(merged=merged_e0,breaks=c(-3,0,3),ylim=c(-6.5,6.5),
          title="Change in life expectancy by 5-year age groups, Iceland 1882",
          fill="#008080")

dev.off()
