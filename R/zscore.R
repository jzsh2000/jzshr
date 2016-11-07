#' Z-score normalization
#'
#' A wrapper for the function \code{\link{scale}}
#'
#' @param x a numeric matrix
#' @param byrow normalize by each row of matrix?
#'
#' @examples
#' x <- matrix(1:16, 4, 4)
#' zscore(x)
#' zscore(x, byrow=FALSE)
#'
#' @export

zscore <-
function(x, byrow = TRUE) {
    if(byrow) t(scale(t(x)))
    else scale(x)
}
