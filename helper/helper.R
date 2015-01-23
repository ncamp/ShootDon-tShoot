## for bootstrapping 95% confidence intervals
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  mean(x,na.rm=na.rm) - quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) - mean(x,na.rm=na.rm)}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



to.data.frame <- function(x) {
  do.call(data.frame, as.list(x))
}



#D prime and bias functions from Pellier (2002)

#dprime: separation between the signal and the noise distribution, compared
#against the sd of the noise distribution. Higher dprime means that the signal
#can more readily be distinguished from the noise.

dprime <- function(hit,fa) {
  qnorm(hit) - qnorm(fa)
}

#beta: bias towards responding that the target is present, independent of
#whether the target actually is present. Higher bias means greater tendency
#towards responding 'present'.

beta <- function(hit,fa) {
  zhr <- qnorm(hit)
  zfar <- qnorm(fa)
  exp(-zhr*zhr/2+zfar*zfar/2)
  
}

#c is defined as the distance between the criterion and the neutral point, where
#the noise and signal distributions cross over. Negative values of c signify a
#bias toward responding yes, whereas positive values signify a bias toward the
#no response (thecriterion lies to the right of the neutral point).

criterion<-function(hit,fa) {
  zhr <- qnorm(hit)
  zfar <- qnorm(fa)
  -(zfar+zhr)/2
}


reread <- function(worker) {
  trials.list.of.vectors <- fromJSON(worker$Answer.data)
  trials.list.of.demographic <- fromJSON(worker$Answer.demographicsData)
  
  ## convert every element in trials.list.of.vectors from a vector to a data frame
  ## (do this by "Map"ping the to.data.frame function over trials.list.of.vectors
  trials.list.of.dfs <- Map(to.data.frame, trials.list.of.vectors)
  trials.list.of.demographics <- Map(to.data.frame, trials.list.of.demographic)
  
  ## mush all the of the individual data frames into a single data frame
  trials.df <- do.call(rbind, trials.list.of.dfs) 
  trials.demographics <- do.call(rbind, trials.list.of.demographics) 
  data.frame(
    "workerid" = worker$workerid,
    trials.df,
    trials.demographics
  )
}