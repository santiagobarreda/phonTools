#' Compare Normalization Methods
#' 
#' Compare the effectiveness of different normalization methods.
#' 
#' This function provides a relatively straightforward way to compare the
#' effectiveness of different normalization methods based on the assumption
#' that a good normalization method maximizes the separation between different
#' vowel categories and minimizes the variation within a single vowel category.
#' Minimizing the variation within a single vowel category means that the vowel
#' spaces of different speakers are maximally similar.
#' 
#' This function provides two measures to compare the performance of
#' normalization methods:
#' 
#' 1) The square root of the average Mahalanobis distance between vowel
#' categories is found for all pairs of vowel categories. This value indicates
#' the average separation of vowel categories with respect to within-category
#' error and the covariance patterns of the formant frequencies. A higher value
#' indicates a better performing normalization algorithm.
#' 
#' 2) The percentage of correct classifications using a linear discriminant
#' model trained on the normalized formant-frequencies using the given
#' category-labels.
#' 
#' @export
#' @param normd A list of dataframes containing the different formant data to
#' be compared. Each dataframe must contain columns called "f1", "f2",
#' "speaker", and "vowel", in any order.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @examples
#' 
#' ## load the Peterson and Barney vowels
#' data (pb52)
#' 
#' ## normalize using several different methods
#' neareyE = normalize (pb52[,7:9], pb52$speaker, pb52$vowel, 
#' method = 'neareyE')
#' neareyI = normalize (pb52[,7:9], pb52$speaker, pb52$vowel, 
#' method = 'neareyI')
#' lobanov = normalize (pb52[,7:9], pb52$speaker, pb52$vowel, 
#' method = 'lobanov')
#' wandf = normalize (pb52[,7:9], pb52$speaker, pb52$vowel, 
#' method = 'wandf', corners =  c('i','A'))
#' normd = list (pb52, neareyE, neareyI, lobanov, wandf)
#' 
#' ## compare outcome of methods (and unnormalized vowels)
#' ## uncomment to run
#' #normalize.compare (normd)
#' 
normalize.compare = function (normd){
  for (j in 1:length(normd)) {
    if (length(normd) > 1) 
      tmp = normd[[j]]
    else tmp = normd
    between = 0
    within = 0
    withintot = 0
    total = 0
    formants = as.matrix(cbind(tmp$f1, tmp$f2))
    vowels = tmp$vowel
    vowelsf = levels(vowels)
    n = length(vowelsf)
    d = 0
    for (i in 1:(n - 1)) for (k in (i + 1):n) d = d + hotelling.test(formants[vowels == 
                                                                                vowelsf[i], ], formants[vowels == vowelsf[k], ])$f.value
    d = (d/(gamma(n + 1)/(gamma(n - 1) * 2)))^0.5
    template = createtemplate(formants, tmp$vowel)
    winners = ldclassify(formants, template$means, template$covariance)
    correct = mean(winners == tmp$vowel)
    cat("\n   Method ", j, "\n\n")
    cat("   Average between-category distance: ", d, 
        "\n\n")
    cat("   Correct Classification: ", correct * 100, 
        "%\n\n")
  }
  cat("\n\n")
}

