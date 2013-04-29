graphbinaryforest <-
function(table, 
                              alpha=0.05,
                              rr.vpos, rr.pos, rr.neg, rr.vneg, rr.cur,
                              event.desired=FALSE, 
                              scale=1,
                              sigdigits=3,
                              title=NA,
                              ...){
  ## graph individual effects and confidence intervals
  
  testing=FALSE
  
  if(testing==TRUE){cat(7)}
  
  # for testing
  #   table=table5; rr.vpos=rr[1]; rr.pos=rr[2]; rr.neg=rr[3]; rr.vneg=rr[4]; rr.cur=rr[5]; event.desired=FALSE; title=NA
  
  # adjust font sizes 
  scale <- 0.75*scale
  scale2 <- scale * 1.2
  
  # count number of studies - needed to format forest plot 
  num.studies <- nrow(table)
  
  # set limits of plot
  ymin <- -5
  ymax <- num.studies + 3
  xmin <- -16
  xmax <- 8
  
  if(testing==TRUE){cat(8)}
  
  # get logrr and var(logrr) from table
  table$logrr <- table$yi
  table$logrr.var <- table$vi
  
  # make title
#   main.default <- "Forest Plot"
#   subtitle <- ""
#   #   ifelse(event.desired==TRUE, 
#   #          subtitle <- " : Event is GOOD", 
#   #          subtitle <- " : Event is BAD")  
#   if(is.na(title) == T){
#     title <- paste(main.default, subtitle, sep="")
#   }
#   
  if(testing==TRUE){cat(9)}
  
  metafor::forest(table$logrr, table$logrr.var, 
                  atransf = exp,                      # to go from logrr to rr
                  ylim = c(ymin,ymax),       # extra rows needed for labels
                  at = log(c(0.05, 0.25, 1, 4, 20)),  # show axis for RR (log scale)
                  xlim = c(xmin, xmax),                   # horizontal dist relative to the vertical line at rr=1
                  slab = paste(table$study, table$year, table$outlook, sep = ", "),  # print author/year
                  ilab = cbind(table$expt.events, table$expt.n, table$ctrl.events, table$ctrl.n),  # print columns with count data
                  ilab.xpos = c(-9.5, -8, -6, -4.5),  # position columns with count data
                  cex = scale,                        # enlarge/reduce font
                  main = title
  )
  # vertical abline at rr=1
  abline(h=0)  
  # add column labels
  text( c(-9.5,-8,-6,-4.5), y=num.studies+2, rep(c("Event", "Total"),2), cex=scale2 )
  text( x=c(-8.75,-5.25), y=num.studies+3, labels=c("Intervention", "Control"), cex=scale2 )
  text( x=xmin, y=num.studies+2, labels="Study", pos=4 , cex=scale2 )
  text( x=xmax, y=num.studies+2, labels="Relative Risk [95% CI]", pos=2 , cex=scale2 )
  
  if(testing==TRUE){cat(10)}
  
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
  
  #  round off rr for display
  rr.vpos <- round(rr.vpos,sigdigits)
  rr.pos <- round(rr.pos,sigdigits)
  rr.cur <- round(rr.cur,sigdigits)
  rr.neg <- round(rr.neg,sigdigits)
  rr.vneg <- round(rr.vneg,sigdigits)
  
  if(testing==TRUE){cat(11)}  
  
  aggregates <- aggeffects.asis(table, confidencelevel=(1-alpha)*100)
  aggregates <- aggregates[1:3,]
  
  ## generate labels; include tau-squared
  agg.tau2 <- tau2(table)
  l.pub     <- paste("Published  ( tau^2 =",round(agg.tau2$pub,sigdigits),")")
  l.unpub   <- paste("Unpublished with specified outlooks ( tau^2 =",round(agg.tau2$unpub,sigdigits),")")
  l.all     <- paste("Published & Unpublished ( tau^2 =",round(agg.tau2$all,sigdigits),")")
  agglabels <- c(l.pub, l.unpub, l.all)
  
  addpoly(as.numeric(aggregates$m), sei=as.numeric(aggregates$m.se), atransf=exp, mlab=agglabels, cex=scale)
}
