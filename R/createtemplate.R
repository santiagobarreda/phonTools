# Copyright (c) 2015 Santiago Barreda
# All rights reserved.





#' Create an LDA Template
#' 
#' Create a linear discriminant analysis template.
#' 
#' This function finds the location of the mean of each class in an
#' n-dimensional space, where each dimension corresponds to one of n columns in
#' 'features'. In addition, the pooled, within-category covariance matrix is
#' found.
#' 
#' The name for each vowel category is stored as the rownames of the 'means'
#' element.
#' 
#' The function plot() is defined for template objects and allows the user to
#' view the location of and expected variation around the different vowel
#' categories in the formant space.
#' 
#' This information may be used in conjunction with the PSTM() function,
#' included in this package. The mean and covariance matrices provided by this
#' function may also be useful for the ldclassify() function provided in this
#' package.
#' 
#' @aliases createtemplate print.template plot.template territorialmap
#' @param features A matrix of features in which each row represents a single
#' token and each column represents a different 'feature' used to classify the
#' token. For vowel sounds, each column should represent different single
#' formant frequency.
#' @param classes A vector indicating the category of each token described in
#' each row of 'features'. The length of this vector must equal the number of
#' rows in 'features'.
#' @return A 'template' object, a list containing the elements:
#' \item{classes}{The category labels.} \item{means}{A matrix containing the
#' mean location for each vowel category within the formant-space. Each row
#' represents a different category while each column represents a different
#' formant.} \item{covariance}{The pooled, within-category covariance matrix
#' for the formant frequencies provided.} \item{ranges}{A matrix of dimension
#' ranges, one row for each dimension.}
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @references Nearey, T. M. & P. F. Assmann. (2007). Pobabilistic 'sliding
#' template' models for indirect vowel normalization. in Experimental
#' Approaches to Phonology, edited by M.J. Sole, P. S., Beddor, and M. Ohala
#' (Oxford University Press, Oxford), pp. 246-269.
#' @examples
#' 
#' data (pb52)            ## load the Peterson and Barney vowels.
#' ## normalize them.
#' normdvowels = normalize (pb52[,7:9], pb52$speaker, pb52$vowel)  
#' formants = normdvowels[,1:3]
#' vowels = pb52$vowel
#' 
#' ## create a vowel template with the normalized formant frequencies
#' ## and information about the vowel category.
#' template = createtemplate (formants, vowels)
#' 
#' ## and inspect with plot()
#' plot (template, xsampa = TRUE)
#' 
createtemplate = function (features, classes){
    if (nrow(features) != length(classes)) 
        stop("Formant and vowel dimensions do not match.")
    vs = levels(as.factor(classes))
    nvs = length(vs)
    means = matrix(0, nvs, ncol(features))
    for (i in 1:ncol(features)) means[, i] = tapply(features[, 
        i], classes, mean)
    rownames(means) = vs
    colnames(means) = paste("f", 1:ncol(features), sep = "")
    tmp = features
    for (i in 1:nvs) tmp[classes == vs[i], ] = features[classes == 
        vs[i], ] - matrix(means[i, ], nrow(tmp[classes == vs[i], 
        ]), ncol(means), byrow = TRUE)
    covariance = var(tmp)
    ranges = matrix (0,ncol(features),2)
    for (i in 1:ncol(features)) ranges[i,] = range (features[,i])
    output = list(classes = vs, means = means, covariance = covariance, ranges = ranges, territory = NULL)
    class(output) = "template"
    return(output)
}

