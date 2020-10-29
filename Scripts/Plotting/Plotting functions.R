#--------------------------------------------------------------
# Topic: Plotting functions, mortality crises and lifespan variation
# Author: Serena Vigezzi
# Date: 26/10/2020
#--------------------------------------------------------------

##################
# Log-death rates
##################

# TWO-YEAR CRISES
##################

plot.mx2 <- function(data.prior,data.first,data.second,data.after,
                     period,title,palette, legend=c(0.3,0.7)){
  
  ggplot() +
    geom_line(data=data.prior, aes(x=Age,y=log(mx),col=period[1]),size=2) +
    geom_line(data=data.first, aes(x=Age,y=log(mx),col=period[2]),size=2) +
    geom_line(data=data.second, aes(x=Age,y=log(mx),col=period[3]),size=4) +
    geom_line(data=data.after, aes(x=Age,y=log(mx),col=period[4]),size=2) +
    ggtitle(title) +
    scale_colour_manual("Period", 
                        breaks = period,
                        values = palette) +  
    scale_x_continuous("",seq(0,110,10), minor_breaks=NULL) +
    scale_y_continuous("Log-mortality rate", lim=c(-6,0), breaks=seq(-6,0,1)) +
    theme_tufte(ticks=F) +
    theme(text = element_text(size = 40),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = legend, 
          legend.key.width = unit(3,"line"),
          legend.text = element_text(size = 45),
          legend.title = element_text(size = 45)) +
    coord_cartesian(xlim=c(0,82), ylim=c(-6,0)) +  
    grids(col="grey85")
  
}

# ONE-YEAR CRISES
##################

plot.mx1 <- function(data.prior,data.crisis,data.after,
                     period,title,palette, legend=c(0.3,0.7)){
  
  ggplot() +
    geom_line(data=data.prior, aes(x=Age,y=log(mx),col=period[1]),size=2) +
    geom_line(data=data.crisis, aes(x=Age,y=log(mx),col=period[2]),size=2) +
    geom_line(data=data.after, aes(x=Age,y=log(mx),col=period[3]),size=2) +
    ggtitle(title) +
    scale_colour_manual("Period", 
                        breaks = period,
                        values = palette) +  
    scale_x_continuous("",seq(0,110,10), minor_breaks=NULL) +
    scale_y_continuous("Log-mortality rate", lim=c(-6,0), breaks=seq(-6,0,1)) +
    theme_tufte(ticks=F) +
    theme(text = element_text(size = 40),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = legend, 
          legend.key.width = unit(3,"line"),
          legend.text = element_text(size = 45),
          legend.title = element_text(size = 45)) +
    coord_cartesian(xlim=c(0,82), ylim=c(-6,0)) +  
    grids(col="grey85")
  
}

#####################
# Absolute variation
#####################


abs.plot <- function(sdf.data,aGf.data,edf.data,
                     sdm.data,aGm.data,edm.data,
                     Years,palette,xminplot,xmaxplot,
                     xmin1,xmax1,ymin1,ymax1,
                     xmin2,xmax2,ymin2,ymax2,
                     ylim=c(5,35)){
  
  sdf <- data.table(Value=sdf.data, Years=Years)
  aGf <- data.table(Value=aGf.data, Years=Years)
  edf <- data.table(Value=edf.data, Years=Years)
  
  sdm <- data.table(Value=sdm.data, Years=Years)
  aGm <- data.table(Value=aGm.data, Years=Years)
  edm <- data.table(Value=edm.data, Years=Years)
  
  ## Step 1
  # Draw a plot with the colour legend
  p1 <- ggplot() +
    geom_line(data=sdf, aes(x=Years,y=Value,col="sd"),size=2) +
    geom_line(data=aGf, aes(x=Years,y=Value,col="aG"),size=2) +
    geom_line(data=edf, aes(x=Years,y=Value,col="ed"),size=2) +
    scale_colour_manual("Measure", 
                        breaks = c("sd", "ed", "aG"),
                        values = palette,
                        labels=c("SD", expression(e^"\u2020"), "Gini (A)")) +
    theme_tufte(ticks=F) +
    theme(legend.key.width = unit(3,"line"),
          legend.text = element_text(size = 40),
          legend.title = element_text(size = 40))
  
  # Extract the colour legend - leg1
  leg1 <- gtable_filter(ggplot_gtable(ggplot_build(p1)), "guide-box") 
  
  ## Step 2
  # Draw a plot with the linetype legend
  p2 <- ggplot() +
    geom_line(data=sdf, aes(x=Years,y=Value,linetype="Female"),size=2) +
    geom_line(data=sdm, aes(x=Years,y=Value,linetype="Male"),size=2) +
    labs(linetype="Sex") +
    theme_tufte(ticks=F) +
    theme(legend.key.width = unit(3,"line"),
          legend.text = element_text(size = 40),
          legend.title = element_text(size = 40))
  
  # Extract the size legend - leg2
  leg2 <- gtable_filter(ggplot_gtable(ggplot_build(p2)), "guide-box") 
  
  # Step 3
  # Draw a plot with no legends - plot
  plot <- ggplot() +
    geom_line(data=sdf, aes(x=Years,y=Value,col="sd", linetype="F"),size=2) +
    geom_line(data=aGf, aes(x=Years,y=Value,col="aG", linetype="F"),size=2) +
    geom_line(data=edf, aes(x=Years,y=Value,col="ed", linetype="F"),size=2) +
    geom_line(data=sdm, aes(x=Years,y=Value,col="sd", linetype="M"),size=2) +
    geom_line(data=aGm, aes(x=Years,y=Value,col="aG", linetype="M"),size=2) +
    geom_line(data=edm, aes(x=Years,y=Value,col="ed", linetype="M"),size=2) +
    annotate("rect", xmin = xminplot, 
             xmax = xmaxplot, 
             ymin = -Inf, ymax = Inf, 
             alpha = .3, fill="grey70") +
    scale_colour_manual("Measure", 
                        breaks = c("sd", "ed", "aG"),
                        values = palette,
                        labels=c("SD", expression(e^"\u2020"), "Gini (A)")) +
    scale_x_continuous("",Years, minor_breaks=NULL) +
    scale_y_continuous("Years") +
    theme_tufte(ticks=F) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size=45),
          axis.text.y = element_text(size=45),
          axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), "mm"), size = 50),
          legend.position = "none", 
          legend.key.width = unit(3,"line"),
          legend.text = element_text(size = 40),
          legend.title = element_text(size = 40)) +
    coord_cartesian(ylim=ylim) +  
    grids(col="grey85")
  
  
  ## Step 4
  # Arrange the three components (plot, leg1, leg2)
  
  return(plot +
           annotation_custom(grob = leg1, xmin = xmin1, xmax = xmax1, ymin = ymin1, ymax = ymax1) +
           annotation_custom(grob = leg2, xmin = xmin2, xmax = xmax2, ymin = ymin2, ymax = ymax2)
  )
}

#####################
# Relative variation
#####################


rel.plot <- function(cvf.data,rGf.data,hf.data,
                     cvm.data,rGm.data,hm.data,
                     Years,palette,xminplot,xmaxplot,
                     xmin1,xmax1,ymin1,ymax1,
                     xmin2,xmax2,ymin2,ymax2,
                     ylim=c(0.2,1.65)){
  
  cf <- data.table(Value=cvf.data, Years=Years)
  rGf <- data.table(Value=rGf.data, Years=Years)
  hf <- data.table(Value=hf.data, Years=Years)
  
  cm <- data.table(Value=cvm.data, Years=Years)
  rGm <- data.table(Value=rGm.data, Years=Years)
  hm <- data.table(Value=hm.data, Years=Years)
  
  ## Step 1
  # Draw a plot with the colour legend
  p1 <- ggplot() +
    geom_line(data=cf, aes(x=Years,y=Value,col="cv"),size=2) +
    geom_line(data=rGf, aes(x=Years,y=Value,col="rG"),size=2) +
    geom_line(data=hf, aes(x=Years,y=Value,col="h"),size=2) +
    scale_colour_manual("Measure", 
                        breaks = c("cv", "h", "rG"),
                        values = palette2,
                        labels=c("CV", expression(bar("H")), "Gini (R)")) +
    theme_tufte(ticks=F) +
    theme(legend.key.width = unit(3,"line"),
          legend.text = element_text(size = 40),
          legend.title = element_text(size = 40))
  
  # Extract the colour legend - leg1
  leg1 <- gtable_filter(ggplot_gtable(ggplot_build(p1)), "guide-box") 
  
  
  # Step 2
  # Draw a plot with no legends - plot
  (plot <- ggplot() +
      geom_line(data=cf, aes(x=Years,y=Value,col="cv", linetype="F"),size=2) +
      geom_line(data=rGf, aes(x=Years,y=Value,col="rG", linetype="F"),size=2) +
      geom_line(data=hf, aes(x=Years,y=Value,col="h", linetype="F"),size=2) +
      geom_line(data=cm, aes(x=Years,y=Value,col="cv", linetype="M"),size=2) +
      geom_line(data=rGm, aes(x=Years,y=Value,col="rG", linetype="M"),size=2) +
      geom_line(data=hm, aes(x=Years,y=Value,col="h", linetype="M"),size=2) +
      annotate("rect", xmin = xminplot, 
               xmax = xmaxplot, 
               ymin = -Inf, ymax = Inf, 
               alpha = .3, fill="grey70") +
      scale_colour_manual("Measure", 
                          breaks = c("cv", "h", "rG"),
                          values = palette2,
                          labels=c("CV", expression(bar("H")), "Gini (R)")) +
      scale_x_continuous("",Years, minor_breaks=NULL) +
      scale_y_continuous(bquote("Proportion of"~e[0]), seq(0.2,1.65,0.4)) +
      theme_tufte(ticks=F) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size=45),
            axis.text.y = element_text(size=45),
            axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), "mm"), size = 50),
            legend.position = "none", 
            legend.key.width = unit(3,"line"),
            legend.text = element_text(size = 40),
            legend.title = element_text(size = 40)) +
      coord_cartesian(ylim=ylim) +  
      grids(col="grey85")
  )
  
  ## Step 4
  # Arrange the three components (plot, leg1)
  
  return(plot + 
           annotation_custom(grob = leg1, xmin = xmin1, xmax = xmax1, ymin = ymin1, ymax = ymax1) 
  )
}

#####################
# Decomposition data
#####################

dec.hor <- function(func=func,
                    pars1=pars1,
                    pars2=pars2,
                    N=N,
                    ax1=ax1,
                    ax2=ax2,
                    age=age,
                    na.rm=F){
  
  dec <- horiuchi5(func=func,
                   pars1=pars1,
                   pars2=pars2,
                   N=N,
                   ax1=ax1,
                   ax2=ax2,
                   age,
                   na.rm=na.rm)
  
  data <- matrix(dec,nrow=length(dec),
                 ncol=1,byrow=F)
  data <- mutate(as.data.frame(data),Age=age)
  data <- reshape2::melt(data,id.vars="Age")
  return(data)
  
}

# TWO-YEAR CRISES
##################

dec.crisis2 <- function(func,ukr=F,
                        parsf.prior,parsf.first,parsf.second,parsf.after,
                        axf.prior,axf.first,axf.second,axf.after,
                        parsm.prior,parsm.first,parsm.second,parsm.after,
                        axm.prior,axm.first,axm.second,axm.after,
                        N=200,age=c(0,1,seq(5,80,5))){
  
  decf.prior <- dec.hor(func=func,
                        pars1=parsf.prior,
                        pars2=parsf.first,
                        N=N,
                        ax1=axf.prior,
                        ax2=axf.first,
                        age=age)
  
  decm.prior <- dec.hor(func=func,
                        pars1=parsm.prior,
                        pars2=parsm.first,
                        N=N,
                        ax1=axm.prior,
                        ax2=axm.first,
                        age=age)
  
  decf.crisis <- dec.hor(func=func,
                         pars1=parsf.first,
                         pars2=parsf.second,
                         N=N,
                         ax1=axf.first,
                         ax2=axf.second,
                         age=age)
  if(ukr==F){
    decm.crisis <- dec.hor(func=func,
                           pars1=parsm.first,
                           pars2=parsm.second,
                           N=N,
                           ax1=axm.first,
                           ax2=axm.second,
                           age=age)
  }else{
    decm.crisis <- dec.hor(func=func,
                           pars1=parsm.first[1:17],
                           pars2=parsm.second[1:17],
                           N=N,
                           ax1=axm.first[1:17],
                           ax2=axm.second[1:17],
                           age=c(0,1,seq(5,75,5)),
                           na.rm=T)
  }
  
  decf.after <- dec.hor(func=func,
                        pars1=parsf.second,
                        pars2=parsf.after,
                        N=N,
                        ax1=axf.second,
                        ax2=axf.after,
                        age=age)
  
  if(ukr==F){
    decm.after <- dec.hor(func=func,
                          pars1=parsm.second,
                          pars2=parsm.after,
                          N=N,
                          ax1=axm.second,
                          ax2=axm.after,
                          age=age)
  }else{
    decm.after <- dec.hor(func=func,
                          pars1=parsm.second[1:17],
                          pars2=parsm.after[1:17],
                          N=N,
                          ax1=axm.second[1:17],
                          ax2=axm.after[1:17],
                          age=c(0,1,seq(5,75,5)),
                          na.rm=T)
    
  }
  
  if(ukr==T){
    merged_e5 <- rbind.data.frame(decf.prior,decm.prior,
                                  decf.crisis,rbind.data.frame(decm.crisis,c(80,"V1",NaN)),
                                  decf.after,rbind.data.frame(decm.after,c(80,"V1",NaN)))
    
  }else{
    merged_e5 <- rbind(decf.prior,decm.prior,
                       decf.crisis,decm.crisis,
                       decf.after,decm.after)
  }
  
  merged_e5$variable <- c(rep(1,36),rep(2,36),rep(3,36))
  merged_e5$variable2 <- factor(merged_e5$variable,levels=c(1,2,3),
                                labels=c("Before","During","After"))
  merged_e5$Sex <- rep(c(rep("Females",18),rep("Males",18)),3)
  colnames(merged_e5) <- c("Age","Period","Contribution","Period2","Sex")
  merged_e5$Contribution <- as.numeric(merged_e5$Contribution)
  merged_e5$Age <- as.numeric(merged_e5$Age)
  
  return(merged_e5)
  
}

# ONE-YEAR CRISES
##################

dec.crisis1 <- function(func,
                        parsf.prior,parsf.crisis,parsf.after,
                        axf.prior,axf.crisis,axf.after,
                        parsm.prior,parsm.crisis,parsm.after,
                        axm.prior,axm.crisis,axm.after,
                        N=200,age=c(0,1,seq(5,80,5))){
  
  decf.prior <- dec.hor(func=func,
                        pars1=parsf.prior,
                        pars2=parsf.crisis,
                        N=N,
                        ax1=axf.prior,
                        ax2=axf.crisis,
                        age=age)
  
  decm.prior <- dec.hor(func=func,
                        pars1=parsm.prior,
                        pars2=parsm.crisis,
                        N=N,
                        ax1=axm.prior,
                        ax2=axm.crisis,
                        age=age)
  
  decf.after <- dec.hor(func=func,
                        pars1=parsf.crisis,
                        pars2=parsf.after,
                        N=N,
                        ax1=axf.crisis,
                        ax2=axf.after,
                        age=age)
  
  decm.after <- dec.hor(func=func,
                        pars1=parsm.crisis,
                        pars2=parsm.after,
                        N=N,
                        ax1=axm.crisis,
                        ax2=axm.after,
                        age=age)
  
    merged_e5 <- rbind(decf.prior,decm.prior,
                       decf.after,decm.after)
  
    merged_e5$variable <- c(rep(1,36),rep(2,36))
    merged_e5$variable2 <- factor(merged_e5$variable,levels=c(1,2),
                                  labels=c("Before","After"))
    merged_e5$Sex <- rep(c(rep("Females",18),rep("Males",18)),2)
    colnames(merged_e5) <- c("Age","Period","Contribution","Period2","Sex")
  
  return(merged_e5)
  
}

#########################
# Decomposition plotting
#########################

dec.plot.single <- function(data,fill,breaks,ylim,x.text=F){
  
  if(x.text==F){
    ggplot(data=data, 
           mapping=aes(x=as.factor(Age), y=Contribution, fill = fill)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_identity() +
      scale_x_discrete("Age", seq(0,80,20)) +
      scale_y_continuous("Contribution",breaks=breaks) +
      theme_tufte(ticks=F) + 
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
            axis.ticks.x = element_blank(), axis.title.y = element_blank(),
            text = element_text(size = 30), axis.text.y = element_text(size = 30)) +
      grids(col="grey85") +
      coord_cartesian(xlim=c(0,18),ylim=ylim)
    
  }else{
    ggplot(data=data, 
           mapping=aes(x=as.factor(Age), y=Contribution, fill = fill)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_identity() +
      scale_x_discrete("Age", seq(0,80,20)) +
      scale_y_continuous("Contribution",breaks=breaks) +
      theme_tufte(ticks=F) + 
      theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 30),
            axis.ticks.x = element_blank(), axis.title.y = element_blank(),
            text = element_text(size = 30), axis.text.y = element_text(size = 30)) +
      grids(col="grey85") +
      coord_cartesian(xlim=c(0,18),ylim=ylim)
  } 
  
}

# TWO-YEAR CRISES
##################

dec.plot2 <- function(merged,breaks,ylim,title,fill,x.text){
  
  # Before the crisis
  
  # Females
  
  Bf <- dec.plot.single(data=merged[1:18,],fill=fill,breaks=breaks,ylim=ylim)
  
  
  # Males
  Bm <- dec.plot.single(data=merged[19:36,],fill=fill,breaks=breaks,ylim=ylim)
  
  # During the crisis
  
  # Females
  Df <- dec.plot.single(data=merged[37:54,],fill=fill,breaks=breaks,ylim=ylim)
  
  # Males
  Dm <- dec.plot.single(data=merged[55:72,],fill=fill,breaks=breaks,ylim=ylim)
  
  # After the crisis
  
  # Females
  Af <- dec.plot.single(data=merged[73:90,],fill=fill,breaks=breaks,ylim=ylim,x.text=T)
  
  # Males
  Am <- dec.plot.single(data=merged[91:108,],fill=fill,breaks=breaks,ylim=ylim,x.text=T)
  
  
  # Annotations to the graph
  
  
  row1 <- ggplot() + annotate(geom = 'text', x=1, y=1, label="Before", 
                              angle = 270, size=10, family="serif") + theme_void()
  
  row2 <- ggplot() + annotate(geom = 'text', x=1, y=1, label="During", 
                              angle = 270, size=10, family="serif") + theme_void()
  
  row3 <- ggplot() + annotate(geom = 'text', x=1, y=1, label="After", 
                              angle = 270, size=10, family="serif") + theme_void()
  
  col1 <- ggplot() + annotate(geom = 'text', x=1, y=1, label="Females",
                              size = 10, family="serif") + theme_void()
  
  col2 <- ggplot() + annotate(geom = 'text', x=1, y=1, label="Males",
                              size = 10, family="serif") + theme_void()
  
  x <- ggplot() + annotate(geom = 'text', x=1, y=1, label="Age",
                           size = 10, family="serif") + theme_void()
  
  y <- ggplot() + annotate(geom = 'text', x=1, y=1, label="Contribution",
                           angle=90,size = 10, family="serif") + theme_void()
  
  title <- ggplot() + annotate(geom = 'text', x=1, y=1, family="serif",
                               label=title,
                               size=10) + theme_void()
  
  
  layoutplot <- "
tttttttttt
#ddd##eee#
yffffgggga
yffffgggga
yiiiillllb
yiiiillllb
ynnnnooooc
ynnnnooooc
###xxx####
"
  
  plotlist <- list(a = row1, b=row2, c=row3, 
                   d = col1, e = col2,
                   y=y, x=x, t=title,
                   f=Bf, g=Bm,
                   i=Df, l=Dm,
                   n=Af, o=Am)
  
  # Graph itself
  
  
  return(wrap_plots(plotlist, guides = 'collect', design = layoutplot))
  
}

# ONE-YEAR CRISES
##################

dec.plot1 <- function(merged,breaks,ylim,title,fill,x.text){
  
  # Before the crisis
  
  # Females
  
  Bf <- dec.plot.single(data=merged[1:18,],fill=fill,breaks=breaks,ylim=ylim)
  
  
  # Males
  Bm <- dec.plot.single(data=merged[19:36,],fill=fill,breaks=breaks,ylim=ylim)
  
  # After the crisis
  
  # Females
  Af <- dec.plot.single(data=merged[37:54,],fill=fill,breaks=breaks,ylim=ylim,x.text=T)
  
  # Males
  Am <- dec.plot.single(data=merged[55:72,],fill=fill,breaks=breaks,ylim=ylim,x.text=T)
  
  
  # Annotations to the graph
  
  
  row1 <- ggplot() + annotate(geom = 'text', x=1, y=1, label="Before", 
                              angle = 270, size=10, family="serif") + theme_void()

  row3 <- ggplot() + annotate(geom = 'text', x=1, y=1, label="After", 
                              angle = 270, size=10, family="serif") + theme_void()
  
  col1 <- ggplot() + annotate(geom = 'text', x=1, y=1, label="Females",
                              size = 10, family="serif") + theme_void()
  
  col2 <- ggplot() + annotate(geom = 'text', x=1, y=1, label="Males",
                              size = 10, family="serif") + theme_void()
  
  x <- ggplot() + annotate(geom = 'text', x=1, y=1, label="Age",
                           size = 10, family="serif") + theme_void()
  
  y <- ggplot() + annotate(geom = 'text', x=1, y=1, label="Contribution",
                           angle=90,size = 10, family="serif") + theme_void()
  
  title <- ggplot() + annotate(geom = 'text', x=1, y=1, family="serif",
                               label=title,
                               size=10) + theme_void()
  
  
  layoutplot <- "
tttttttttt
#ddd##eee#
yffffgggga
yffffgggga
ynnnnooooc
ynnnnooooc
###xxx####
"
  
  plotlist <- list(a = row1, c=row3, 
                   d = col1, e = col2,
                   y=y, x=x, t=title,
                   f=Bf, g=Bm,
                   n=Af, o=Am)
  
  # Graph itself
  
  
  return(wrap_plots(plotlist, guides = 'collect', design = layoutplot))
  
}


############
# e0 trends
############

e0.plot <- function(e0f.data,e0m.data,Years,
                    xmin,xmax){
  
  e0f <- data.table(Value=e0f.data, Years=Years)
  e0m <- data.table(Value=e0m.data, Years=Years)
  
  plot <-   ggplot() +
    geom_line(data=e0f, aes(x=Years,y=Value,linetype="Female"), size=2, col="#C93312") +
    geom_line(data=e0m, aes(x=Years,y=Value,linetype="Male"), size=2, col="#C93312") +
    annotate("rect", xmin = xmin, 
             xmax = xmax, 
             ymin = -Inf, ymax = Inf, 
             alpha = .3, fill="grey70") +
    scale_x_continuous("Year",Years, minor_breaks=NULL) +
    scale_y_continuous("Years") +
    labs(linetype="Sex") +
    theme_tufte(ticks=F) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size=45),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=45),
          axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), "mm"), size = 50),
          legend.position = c(.8,.3), 
          legend.key.width = unit(3,"line"),
          legend.text = element_text(size = 50),
          legend.title = element_text(size = 50)) +
    coord_cartesian(ylim=c(5,60)) +  
    grids(col="grey85")
  
  return(plot)
  
}

