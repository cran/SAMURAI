graphcontinuousforest <-
function(table, 
                                  alpha=0.05,
                                  event.desired=TRUE, 
                                  title=NA, 
                                  scale=1){
  ## graph individual effects and confidence intervals
  
  # for testing
  #   table=table4b
  #   title=NA
  #   event.desired=TRUE
  #   alpha=0.05
  
  # adjust font sizes 
  scale <- 0.75*scale
  scale2 <- scale * 1.2
  
  # count number of studies - needed to format forest plot 
  num.studies <- nrow(table)
  
  # set limits of plot
  ymin <- -5
  ymax <- num.studies + 3
  xmin <- -13
  xmax <- 6
  
  # get SMD and its variance from the table
  table$smd <- table$yi
  table$smd.v <- table$vi
  
  table$smd.round <- sprintf("%.3f", round(table$smd,3) )
  table$smd.v.round <- sprintf("%.3f", round(table$smd.v,3) )
  
  # make title
  main.default <- "Forest Plot"
  subtitle <- ""
  #   ifelse(event.desired==TRUE, 
  #          subtitle <- " : Event is GOOD", 
  #          subtitle <- " : Event is BAD")  
  if(is.na(title) == T){
    title <- paste(main.default, subtitle, sep="")
  }
  
  metafor::forest(x=table$smd, 
                  vi=table$smd.v, 
                  ylim = c(ymin,ymax),       # extra rows needed for labels
                  at = c(-1,-0.5, 0, 0.5,1),  # show axis ticks 
                  xlim = c(xmin, xmax),                    # horizontal dist relative to the vertical line at rr=1
                  slab = paste(table$study, table$year, table$outlook, sep = ", "),  # print author/year
                  ilab = cbind(table$expt.n, table$ctrl.n, table$smd.round, table$smd.v.round),  # print columns with count data
                  ilab.xpos = c(-7.5,-6,-4,-2.5),  # position columns with count data
                  cex = scale2,                        # enlarge/reduce font
                  main = title
  )
  # vertical abline at smd=0
  abline(h=0)  
  # add column labels
  text( c(-7.5,-6,-4,-2.5), y=num.studies+2, c("Expt", "Ctrl", "SMD", "Variance"), cex=scale )
  text( x=c(-6.75,-3.25), y=num.studies+3, labels=c("Sample size", "SMD"), cex=scale2 )
  text( x=xmin, y=num.studies+2, labels="Study", pos=4 , cex=scale2 )
  text( x=xmax, y=num.studies+2, labels="SMD [95% CI]", pos=2 , cex=scale2 )
  
  if(event.desired==TRUE){
    text( x=xmax, y=ymin, labels="(Event is GOOD)", pos=2, cex=scale )
    text( x=0, y=ymin, labels="Favors Control", pos=2, cex=scale )
    text( x=0, y=ymin, labels="Favors Intervention", pos=4, cex=scale )
  }
  if(event.desired==FALSE){
    text( x=xmax, y=ymin, labels="(Event is BAD)", pos=2, cex=scale )
    text( x=0, y=ymin, labels="Favors Intervention", pos=2, cex=scale )
    text( x=0, y=ymin, labels="Favors Control", pos=4, cex=scale )
  }
  text ( x=xmin, y=ymin, labels="All effects are estimated with random effects models", pos=4, cex=scale*0.8 )

  
  aggregates <- aggeffects.asis(table, confidencelevel=(1-alpha)*100)
  aggregates <- aggregates[1:3,]
  
  ## generate labels; include tau-squared
  agg.tau2 <- tau2(table)
  l.pub     <- paste("Published  ( tau^2 =",round(agg.tau2$pub,3),")")
  l.unpub   <- paste("Unpublished with specified outlooks ( tau^2 =",round(agg.tau2$unpub,3),")")
  l.all     <- paste("Published & Unpublished ( tau^2 =",round(agg.tau2$all,3),")")
  agglabels <- c(l.pub, l.unpub,l.all)
  
  addpoly(as.numeric(aggregates$m), sei=as.numeric(aggregates$m.se), mlab=agglabels, cex=scale2)
}
