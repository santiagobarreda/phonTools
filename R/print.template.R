
#' @export

print.template = function (x, ...){
  cat ('\nTemplate with the following category-means:','\n\n')
  print (x$means)  
}
