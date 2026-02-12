

#' Interaction Plot
#'
#' Create an interaction plot with error bars.
#'
#' This function generates an interaction plot showing the relationship between
#' two factors with optional error bars (standard error or standard deviation).
#' It can accommodate grouping variables and allows customization of colors and
#' labels.
#'
#' @export
#' @param y A numeric vector of response values.
#' @param x A factor or grouping variable for the x-axis.
#' @param lines A factor indicating different lines to be plotted.
#' @param group An optional grouping variable for additional stratification.
#' @param ylab Label for the y-axis.
#' @param xlabels Labels for the x-axis positions.
#' @param bars Type of error bars: 'se' for standard error or 'sd' for
#' standard deviation.
#' @param add If TRUE, lines are added to an existing plot. If FALSE, a new
#' plot is created.
#' @param cols A vector of colors for the plotted lines.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#'
iplot = function (y, x, lines, group = NULL, ylab ='', xlabels = c('',''), bars = 'se', add = FALSE, cols = c(1,1)){

  if (is.null(group)){
    mu = aggregate (y ~ x + lines, FUN = mean)
    stdv = aggregate (y ~ x + lines, FUN = sd)
    if (bars=='se'){
      n = aggregate (y>-2 ~ x + lines, FUN = sum)
      stdv[,3] = stdv[,3]/sqrt(n[,3])
    }
  }

  if (!is.null(group)){
    mu = aggregate (y ~ x + lines + group, FUN = mean)
    stdv = aggregate (y ~ x + lines, data = mu, FUN = sd)
    if (bars=='se'){
      n = aggregate (mu[,3]>(min(y)-2) ~ mu[,1] + mu[,2], FUN = sum)
      stdv[,3] = stdv[,3]/sqrt(n[,3])
    }
    mu = aggregate (y ~ x + lines, data = mu, FUN = mean)
  }

  mse = max(stdv[,3])
  print (stdv)
  ylim = range(mu[,3])+(c(-mse,mse)*1.1)

  if (!add) plot (1:2,mu[2:1,3], ylim = ylim, type = 'b', xaxt = 'n',ylab=ylab,
      xlim = c(0.75,2.25), lty = 'dashed', xlab='', lwd=2,cex.axis=1.2,cex.lab=1.2, col=cols[1])
  if (add) lines (1:2,mu[2:1,3], type = 'b', lty = 'dashed', lwd=2, col=cols[1])

  axis (side=1, at = 1:2, labels = xlabels,cex.axis=1.2)
  errorbars (1:2,mu[2:1,3], stdv[1:2,3], lwd=2, col=cols[1])
  lines (c(.95,1.95),mu[4:3,3], type = 'b', lwd=2, col=cols[2])
  errorbars (c(.95,1.95),mu[4:3,3], stdv[4:3,3], lwd=2, col=cols[2])
}

