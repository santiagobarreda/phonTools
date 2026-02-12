
#' Outlier Plot
#' 
#' Identify outliers in a two-dimensional vowel space using Mahalanobis distance.
#' 
#' This function visualizes vowels in a two-dimensional space and highlights
#' potential outliers based on their Mahalanobis distance from the center of 
#' each vowel category. Points are colored by their distance from the category center.
#' 
#' @export
#' @param x Numeric vector of x-axis values (typically F1 or dimension 1).
#' @param y Numeric vector of y-axis values (typically F2 or dimension 2).
#' @param category Factor or character vector indicating the category/vowel label for each point.
#' @param xsampa If TRUE, x-axis labels use X-SAMPA notation.
#' @param logaxes If TRUE, both axes are log-scaled.
#' @param ellipsesd Number of standard deviations to use for plotting ellipses around each category.
#' @param borders A vector of two numeric values defining Mahalanobis distance boundaries for 
#' coloring: distances < borders[1] are green, between borders are gold, > borders[2] are red.
#' @param select Number of points to select interactively (0 for none).
#' @param nearest If select > 0, number of nearest neighbors to return for each selected point.
#' @return If select = 0, returns NULL invisibly. If select > 0, returns a dataframe with
#' columns indicating the index of each selected point, which selection it was, and its rank
#' among nearest neighbors.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @examples
#' \dontrun{
#' # Create sample vowel data
#' f1 = c(300, 350, 200, 250, 500, 550, 400, 450)
#' f2 = c(2500, 2300, 2800, 2600, 1000, 950, 1500, 1400)
#' category = c("i", "i", "u", "u", "a", "a", "e", "e")
#' 
#' # Plot outliers
#' outlier.plot(f1, f2, category, logaxes = TRUE)
#' }
#' 

outlier.plot = function (x, y, category, xsampa = TRUE, logaxes = TRUE, ellipsesd = 2, borders = c(2,3), select = 0, nearest = 1){

  ffs = as.matrix(cbind(x,y))
  if (logaxes) ffs = log(ffs)
  
  cs = as.factor(category)
  lcs = levels (cs)
  temp = list(
    means = t(sapply(lcs, function(l) colMeans(ffs[cs == l, , drop = FALSE])))
  )
  ncs = length (lcs)
  type = as.numeric(cs)
  
  tmp.env = environment()
  
  dist = rep(0, length(x))
  for (i in 1:nrow(temp$means)){ 
    use = (category == lcs[i])
    dist[use] = mahalanobis (ffs[use,], temp$means[i,], cov (ffs[use,]))
  }
  dist = sqrt(dist)
  
  cols = rep('', length(dist))
  cols[dist<borders[1]] = 'forestgreen';
  cols[dist>borders[1] & dist < borders[2]] = 'gold3';
  cols[dist>borders[2]] = 'firebrick';
  
  sizes = rep(0, length(dist))
  sizes[dist<borders[1]] = .5;sizes[dist>borders[1] & dist < borders[2]] = 1.2;sizes[dist>borders[2]] = 1.7;

  #vns = c('x','y','category', 'xsampa', 'logaxes', 'sizes')
  #vs = list(x,y,category, xsampa, logaxes, sizes)
  #for (i in 1:6) assign (vns[i],vs[[i]],envir=.GlobalEnv)
  
  oldpar = par(no.readonly = TRUE)
  par (mar = c(4.1,4.1,1,1))
  vplot (x,y,category, xsampa = xsampa, logaxes = logaxes, cex = sizes, colors = cols,
  xlab='Dimension 1',ylab='Dimension 2')
  vplot (x,y,category, xsampa = xsampa, logaxes = logaxes, colors = 1, 
         add = TRUE, meansonly = TRUE, cex = 2.5)
  
  for (i in 1:nrow(temp$means)){  
      if (!logaxes) sdellipse (cbind (x[cs==lcs[i]],y[cs==lcs[i]]), 
                              stdev = ellipsesd, col = 1,lwd=2,lty='dotted') 
      
      if (logaxes){ tmp = sdellipse (log(cbind (x[cs==lcs[i]],y[cs==lcs[i]])), 
                          stdev = ellipsesd, show = FALSE); lines (exp(tmp), col = 1,
						  lwd=2,lty='dotted')}
  }  
  suppressWarnings (par (oldpar))
  if (select > 0){
    coords = locator(select) 
    coords = log(as.matrix(cbind(coords$x,coords$y)))
    siginv = solve (cov (ffs))  
  
    index = NULL
    selection = NULL
    closest = NULL
  
    for (i in 1:select){
      dists = mahalanobis (ffs, coords[i,], cov (ffs))
      index = c(index, order(dists)[1:nearest])
      selection = c(selection, rep(i, nearest))
      closest = c(closest, 1:nearest)
    }  
    return (data.frame (index, selection, closest))  
  }
}

