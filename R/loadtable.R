
#' Load Table
#' 
#' Load text table data quickly.
#' 
#' This function is a wrapper for read.table() for those times when you just
#' don't feel like typing a filename. The function opens up a file selection
#' dialog box allowing the user to select the file containing the data.
#' 
#' @export
#' @param ... Arguments are passed to the internal call of read.table().
#' @return Returns the output of read.table().
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @examples
#' 
#' ## uncomment and run
#' # data = loadtable ()
#' # head (data)
#' 
loadtable = function (...){
  filename = file.choose()
  read.table (filename, ...)
}


