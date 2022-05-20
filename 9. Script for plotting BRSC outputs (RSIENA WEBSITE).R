# Plots for sienaBayesFit objects.
# In these functions, x should be of class sienaBayesFit.
# Programmed by Tom Snijders, some corrections by Christian Steglich.
# Version December 23, 2021
# Note that density plots start processing the sampled posteriors
# from x$nwarm+1 onward, or from nfirst, if given.

# The advice is to source this file:
# source("BayesPlots.r")
# Then, if the numbers of groups and effects are not too large,
# use the functions, if the object would be called x:
# NonRateTracePlots(x)
# RateTracePlots(x)
# GlobalRateParameterPlots(x)
# GlobalNonRateParameterPlots(x)
#    (gives trace plots of the global fixed parameters,
#     and of means and between-group standard deviations
#     of the global varying parameters)
# AllDensityPlots(x)
# If this produces plots that are too crowded,
# you can make variations of these functions
# (see the function headings below for how to use them)
# such as
# NonRateTracePlots(x, groups=3:5)
# RateTracePlots(x, groups=3:5)
# GlobalRateParameterPlots(x)
# GlobalNonRateParameterPlots(x, setOfEffects = 2:4, title="x_ef234")
# (where you may of course change "x_ef234" to something
# more directly comprehensible)
# AllDensityPlots(x, groups=3:5)

# The default value for title, the filename of the plot, is
# the name of the sienaBayes fit object to which identifying information
# is appended: NRTP for non-rate trace plot,
# RTP for rate trace plot, density,
# Mu and SD for trace plots of the global parameters mu (population mean)
# and the between-group standard deviations,
# Eta for the global fixed parameters,
# av and sd for the trace plots of the mean and standard deviation
# across the groups.

# For models with many parameters,
# the trace plotting functions also can be adapted to give, e.g.,
# separate plots for separate dependent variables,
# using x$dependentVariable;
# or for structural effects / covariate effects.
# If setOfEffects=NULL then plots will be given for all effects;
# else setOfEffects must be a vector of integers
# indicating the numbers of the effects in the model.
# These are numbered like in the effects object and the
# print of the sienaBayes result, in such a way
# that the rate parameters are not duplicated for the several groups.
# You can get the listing of the parameter numbering for object x
# by requesting getNames(x).

# First two utility functions
stretch2 <- function(x){
  x[2] <- x[2] + 0.3*(x[2]-x[1])
  x
}

getNames <- function(x){
  # effect names without duplicated rate parameters
  # and with "unspecified interaction" replaced by
  # information about the effects in question
  allnames <- x$requestedEffects$effectName
  alltypes <- x$requestedEffects$type
  alltypes[alltypes %in% c("rate", "eval")] <- ""
  allnames <- paste(allnames, alltypes)
  allperiods <- unlist(lapply(strsplit(allnames," \\(period "),
                              function(name){
                                as.numeric(sub("\\)","",c(name,'0')[2]))
                              }
  )) # period 0 now indicates non-rate parameter
  length.firstgroup <- min(unique(allperiods)[!((unique(allperiods)+1) %in% unique(allperiods))])
  # Now construct tp = True Parameters, i.e., all except rate parameters for groups 2 and up.
  # 1. For repeated multi-period data, rate parameters may differ over periods.
  # 2. For data with multiple dependent variables, each variable has different rates.
  tpar <- (allperiods<=length.firstgroup)
  return(allnames[tpar])
}

TracePlot <- function(x, i, indices, ff=0.4,
                      title=paste(deparse(substitute(x)),"_TP",sep="")){
  # For the sienaBayesFit object x,
  # for group i and parameters in vector indices,
  # traceplots are constructed in png format.
  # With positive ff, a lowess approximation is added to each trace plot.
  post <- x$ThinParameters
  EffName <- getNames(x)
  indices <- sort(indices)
  # Choose png format; if you prefer different format, this can be changed.
  png(filename=paste(title, "_",i, ".png", sep=""), width=960,height=720)
  par(oma=c(0,1,0,0), mar=c(5.1,5.1,4.1,2.1)) # to accommodate larger font for ylab
  # Create an empty plot big enough to fit all data points
  xlim <- c(0,dim(post)[1])
  ylim <- stretch2(range(post[,,indices], na.rm=TRUE))
  plot(NA, xlim=xlim, ylim=ylim, xlab="index", ylab="theta", cex.lab=2,
       cex.main=2, main=paste("Group", i))
  # Draw the plots
  for (k in indices)
  {
    colk <- (which(indices==k))
    points(1:dim(post)[1], post[,i,k], type="p", col=colk)
    if ((ff > 0)&&(!any(is.na(post[,i,k]))))
    {
      lines(lowess(post[,i,k], f=ff), col=colk)
    }
  }
  legend("topleft",legend=EffName[indices], pch=21,
         cex=2, col=rank(indices), bty="n")
  graphics.off()
}


NonRateTracePlots <- function(x, groups=1:x$nGroup, setOfEffects=NULL,
                              title=NULL, ff=0.4){
  indices <- which(x$generalParametersInGroup)
  if (!is.null(setOfEffects))
  {
    indices <- intersect(setOfEffects, indices)
  }
  if (is.null(title))
  {
    title <- paste(deparse(substitute(x)),"_NRTP",sep="")
  }
  EffName <- getNames(x)
  for (i in groups){
    TracePlot(x, i, indices, ff=ff, title)
  }
  meanpost <- apply(x$ThinParameters, c(1,3), mean)
  fulltitle <- paste(title, "av.png", sep="")
  ThePlot(fulltitle, meanpost, , ylab="average theta", EffName, indices, ff=ff)
  sdpost <- apply(x$ThinParameters, c(1,3), sd)
  fulltitle <- paste(title, "sd.png", sep="")
  ThePlot(fulltitle, sdpost, , ylab="s.d. theta", EffName, indices, ff=ff)
}

RateTracePlots <- function(x, groups=1:x$nGroup,
                           title=paste(deparse(substitute(x)),"_RTP",sep=""), ff=0.4){
  EffName <- x$effectName
  indices <- which(!x$generalParametersInGroup)
  for (i in groups){
    TracePlot(x, i, indices,
              ff=-1, title)
  }
  meanpost <- apply(x$ThinParameters, c(1,3), mean)
  fulltitle <- paste(title, "av.png", sep="")
  ThePlot(fulltitle, meanpost, , ylab="average theta", EffName, indices, ff=ff)
  sdpost <- apply(x$ThinParameters, c(1,3), sd)
  fulltitle <- paste(title, "sd.png", sep="")
  ThePlot(fulltitle, sdpost, , ylab="s.d. theta", EffName, indices, ff=ff)
}

AllTracePlots <- function(x, groups=1:x$nGroup,
                          title=paste(deparse(substitute(x)),"_TP",sep=""), ff=0.4){
  for (i in groups){
    TracePlot(x, i, 1:x$TruNumPars,
              ff=-1, title)
  }
  meanpost <- apply(x$ThinParameters, c(1,3), mean)
  EffName <- getNames(x)
  fulltitle <- paste(title, "av.png", sep="")
  ThePlot(fulltitle, meanpost, , ylab="average theta", EffName, 1:x$TruNumPars, ff=ff)
  sdpost <- apply(x$ThinParameters, c(1,3), sd)
  fulltitle <- paste(title, "sd.png", sep="")
  ThePlot(fulltitle, sdpost, , ylab="s.d. theta", EffName, 1:x$TruNumPars, ff=ff)
}

AggregateTracePlots <- function(x, groups=1:x$nGroup,
                                title=paste(deparse(substitute(x)),"_TP",sep=""), ff=0.4){
  meanpost <- apply(x$ThinParameters, c(1,3), mean)
  EffName <- getNames(x)
  fulltitle <- paste(title, "av.png", sep="")
  ThePlot(fulltitle, meanpost, , ylab="average theta", EffName, 1:x$TruNumPars, ff=ff)
  sdpost <- apply(x$ThinParameters, c(1,3), sd)
  fulltitle <- paste(title, "sd.png", sep="")
  ThePlot(fulltitle, sdpost, , ylab="s.d. theta", EffName, 1:x$TruNumPars, ff=ff)
}

# Now a basic plotting routine for trace plots.
ThePlot <- function(title, data, xlab="index", ylab="theta",
                    efnames, indices, ff=0.4){
  if (any(indices))
  {
    png(filename=title, width=960,height=720)
    par(oma=c(0,1,0,0), mar=c(5.1,5.1,4.1,2.1)) # to accommodate larger font for ylab
    #       xlim <- c(0,dim(data)[1])
    xlim <- c(0,sum(!is.na(data[,1])))
    ylim <- stretch2(range(data[,indices], na.rm=TRUE))
    plot(NA, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, cex.lab=2)
    for (k in indices)
    {
      colk <- (which(indices==k))
      points(1:dim(data)[1], data[,k], type="p", col = colk)
      if ((ff > 0)&&(!any(is.na(data[,k]))))
      {
        lines(lowess(data[,k], f=ff), col=colk)
      }
    }
    legend("topleft",legend=efnames[indices], pch=21,
           cex=2, col=rank(indices), bty="n")
    graphics.off()
  }
}


GlobalRateParameterPlots <- function(x, title=NULL, ff=0.4){
  if (is.null(title))
  {
    title <- deparse(substitute(x))
  }
  EffName <- getNames(x)[x$varyingParametersInGroup]
  postmu <- x$ThinPosteriorMu
  fulltitle <- paste(title, "_postMuRate.png", sep="")
  indices <- which(!x$objectiveInVarying)
  ThePlot(fulltitle, postmu, , ylab="posterior mu", EffName, indices, ff)
  postsig <- x$ThinPosteriorSigma
  postsigg <- matrix(NA,dim(postsig)[1],dim(postsig)[2])
  for (k in 1:dim(postsig)[2]){postsigg[,k] <- sqrt(postsig[,k,k])}
  fulltitle <- paste(title, "_postSDRate.png", sep="")
  indices <- which(!x$objectiveInVarying)
  ThePlot(fulltitle, postsigg, , ylab="posterior sigma", EffName, indices, ff)
}

GlobalNonRateParameterPlots <- function(x, setOfEffects=NULL,
                                        title=NULL, ff=0.4){
  # First the varying non-rate parameters
  if (is.null(setOfEffects))
  {
    efSet <- which(x$varyingGeneralParametersInGroup) # all random effects
  }
  else
  {
    efSet <- intersect(setOfEffects, which(x$varyingGeneralParametersInGroup)) # selected random effects
  }
  # EffName must be the columnnames for ThinPosteriorMu
  if (any(x$basicRate & x$set1))
  {
    EffName <- getNames(x)[x$varyingParametersInGroup] # all names of random effects
  }
  else # this means all rate parameters are fixed, not estimated
  {
    EffName <- getNames(x)[x$varyingGeneralParametersInGroup]
  }
  indices <- match(efSet, which(x$varyingParametersInGroup))
  indices <- indices[!is.na(indices)]
  if (is.null(title))
  {
    title <- deparse(substitute(x))
  }
  postmu <- x$ThinPosteriorMu
  fulltitle <- paste(title, "_postMu.png", sep="")
  
  if (length(indices) <= 0)
  {
    cat("Note: no varying non-rate parameters")
    if(!is.null(setOfEffects)) {cat(" in {", setOfEffects, "}")}
    cat(".\n")
    cat("Further note: there are", length(x$objectiveInVarying),
        " varying parameters.\n")
    if (length(x$objectiveInVarying) > 0)
    {
      cat("The varying non-rate parameters are ")
      cat(which(x$varyingParametersInGroup)[x$objectiveInVarying], sep=", ")
      cat(".\n", sep="")
    }
  }
  else
  {
    ThePlot(fulltitle, postmu, , ylab="posterior mu", EffName, indices, ff)
    postsig <- x$ThinPosteriorSigma
    postsigg <- matrix(NA,dim(postsig)[1],dim(postsig)[2])
    for (k in 1:dim(postsig)[2]){postsigg[,k] <- sqrt(postsig[,k,k])}
    fulltitle <- paste(title, "_postSD.png", sep="")
    ThePlot(fulltitle, postsigg, , ylab="posterior sigma", EffName, indices, ff)
  }
  # Now the fixed parameters
  # The following definition of estimatedNonvaryingParametersInGroup
  # is something that should have been done in sienaBayes.
  # It now is done in sienaBayes since the version of 23/12/21.
  # Here for backward compatibility.
  vec1 <- 4 - x$requestedEffects$randomEffects[x$requestedEffects$include]
  vec1[x$requestedEffects$fix[x$requestedEffects$include]] <- 5
  vec1[x$basicRate] <- 2
  vec1[x$ratePositions[[1]]] <- 1
  x$estimatedNonvaryingParametersInGroup <- vec1[vec1!= 2] == 4
  if (is.null(setOfEffects))
  {
    efSet <- which(x$estimatedNonvaryingParametersInGroup) # all fixed effects
  }
  else
  {
    efSet <- intersect(setOfEffects, which(x$estimatedNonvaryingParametersInGroup)) # selected fixed effects
  }
  EffName <- getNames(x)[x$estimatedNonvaryingParametersInGroup]
  posteta <- x$ThinPosteriorEta
  fulltitle <- paste(title, "_postEta.png", sep="")
  indices <- match(efSet, which(x$estimatedNonvaryingParametersInGroup))
  indices <- indices[!is.na(indices)]
  if (length(indices) <= 0)
  {
    cat("Note: no non-varying parameters")
    if(!is.null(setOfEffects))
    {
      cat(" in {", setOfEffects, "}.\n")
      cat("The non-varying parameters are ")
      cat(which(!x$varyingParametersInGroup), sep=", ")
      cat(".\n", sep="")
    }
  }
  else
  {
    ThePlot(fulltitle, posteta, , ylab="posterior eta", EffName, indices, ff)
  }
}


DensityPlots <- function(x, index, title=paste(deparse(substitute(x)),"_",index,sep=""),
                         groups=(1:x$nGroup), legendpos="topleft", nfirst=(x$nwarm+1)){
  # Makes a density plot of the sienaBayesFit object x,
  # parameter number index, for the desired groups.
  if (index > length(x$varyingParametersInGroup))
  {
    stop("index should be a number between 1 and ",
         length(x$varyingParametersInGroup), ".\n")
  }
  if (!x$varyingParametersInGroup[index])
  {
    stop("This is not a varying parameter.")
  }
  if (any(x$basicRate & x$set1))
  {
    indexInVarying <- match(index,which(x$varyingParametersInGroup))
  }
  else # this means all rate parameters are fixed, not estimated
  {
    indexInVarying <- match(index,which(x$varyingGeneralParametersInGroup))
  }
  if (is.na(indexInVarying))
  {
    stop("This is a fixed rate parameter, not varying.")
  }
  nlast <- dim(x$ThinParameters)[1]
  post <- x$ThinParameters[nfirst:nlast,,]
  meanpost <- apply(post, c(1,3), mean)
  postmu <- x$ThinPosteriorMu[nfirst:nlast,]
  EffName <- getNames(x)
  fulltitle <- paste(title, "_dens.png", sep="")
  png(filename=fulltitle, width=960,height=720)
  par(oma=c(0,1,0,0), mar=c(5.1,5.1,4.1,2.1)) # to accommodate larger font for ylab
  # First the densities by group
  t1 <- matrix(NA,dim(post)[1],length(groups))
  d1 <- list()
  xlim <- stretch2(range(post[,groups,index], na.rm=TRUE))
  ymini <- 1000
  ymaxi <- -1000
  for (k in groups) {
    kk <- which(groups==k)
    t1[,kk] <- post[,k,index]
    d1[[kk]] <- density(t1[,kk], na.rm=TRUE)
    ymini <- min(ymini,d1[[kk]]$y, na.rm=TRUE)
    ymaxi <- max(ymaxi,d1[[kk]]$y, na.rm=TRUE)
  }
  # Next the density of the global parameter = postmu
  # and of the mean parameter over all groups = postmean.
  t2 <- meanpost[,index]
  d2 <- density(t2, na.rm=TRUE)
  ylim <- stretch2(range(d2$y))
  ymini <- min(ylim[1],ymini, na.rm=TRUE)
  ymaxi <- max(ylim[2],ymaxi, na.rm=TRUE)
  t3 <- postmu[,indexInVarying]
  # Depending on the prior the postmu density may be
  # more spread out than the individual densities.
  # We wish to represent at least the 0.01 to 0.99 quantiles
  # of the postmu density.
  q <- quantile(postmu[,indexInVarying],probs=c(0.01,0.99), na.rm=TRUE)
  xlim[1] <- min(xlim[1], q[1])
  xlim[2] <- max(xlim[2], q[2])
  d3 <- density(t3, na.rm=TRUE)
  ylim <- stretch2(range(d3$y))
  ylim[1] <- min(ylim[1],ymini)
  ylim[2] <- max(ylim[2],ymaxi)
  plot(NA, xlim=xlim, ylim=ylim, xlab='theta', ylab="density",
       cex=4, cex.lab=2, cex.main=2, main=EffName[index])
  # legend
  leg <- NULL
  for (k in groups) {
    kk <- which(groups==k)
    lines(d1[[kk]], col=kk, lwd=2)
    leg <- c(leg, paste('Group',k))
  }
  
  lines(d2, col=length(groups)+1,lwd=2)
  lines(d3, col=length(groups)+2, lwd=2)
  leg <- c(leg, 'Average theta', 'Mu')
  legend(legendpos, legend=leg, cex=2, col=1:(length(groups)+2), lty=1, lwd=2)
  graphics.off()
}


DensityPlot <- function(x,index,
                        title=paste(deparse(substitute(x)),"_", index, sep=""),
                        group=1, legendpos="topleft", nfirst=(x$nwarm+1)){
  # Makes a density plot of the sienaBayesFit object x, one group.
  # This is meaningful for group 1 for non-varying parameters.
  nlast <- dim(x$ThinParameters)[1]
  post <- x$ThinParameters[nfirst:nlast,,]
  meanpost <- apply(post, c(1,3), mean)
  EffName <- getNames(x)
  fulltitle <- paste(title, "_dens.png", sep="")
  png(filename=fulltitle, width=960,height=720)
  par(oma=c(0,1,0,0), mar=c(5.1,5.1,4.1,2.1)) # to accommodate larger font for ylab
  # density by group
  t1 <- rep(NA,dim(post)[1])
  xlim <- stretch2(range(post[,group,index]))
  t1 <- post[,group,index]
  if (var(t1) < 1e-10) # meant as a check for variance == 0
  {
    d1 <- data.frame(t1)
    xlim <- round(c(xlim[1]-1, xlim[2]+1))
    ylim <- c(0.1,1.5)
    spw <- function(a, ...){suppressWarnings(plot(a, type="h", ...))}
  }
  else
  {
    d1 <- density(t1, na.rm=TRUE)
    ylim <- stretch2(range(d1$y))
    spw <- function(a, ...){plot(a, ...)}
  }
  if (x$varyingParametersInGroup[index])
  {
    xxlab <- 'theta'
  }
  else
  {
    xxlab <- 'eta'
  }
  spw(d1, xlim=xlim, ylim=ylim, xlab=xxlab, ylab="density", col=4,
      cex=4, cex.lab=2, cex.main=2, main=EffName[index], lwd=2)
  # legend
  leg <- xxlab
  legend(legendpos,legend=leg, cex=2, col=1, lty=1, lwd=2)
  graphics.off()
}

AllDensityPlots <- function(x, groups=(1:x$nGroup),
                            basetitle=paste(deparse(substitute(x))), legendpos="topleft",
                            nfirst=(x$nwarm+1)){
  #   basetitle <- paste(deparse(substitute(x)))
  for (i in 1:x$TruNumPars)
  {
    if (x$varyingParametersInGroup[i])
    {
      DensityPlots(x,i,paste(basetitle,"_",i,sep=""),groups, legendpos=legendpos, nfirst=nfirst)
    }
    else
    {
      DensityPlot(x,i,paste(basetitle,"_",i,sep=""), legendpos=legendpos, nfirst=nfirst)
    }
  }
}
