#' Check if a package is already installed
#'
#' Read package list and check if a package is installed or not.
#' @param x a character vector, each value represents a package name.
#'
#' @return a logical vector. TRUE means the package is installed.
#'
#' @examples
#' has_package('stringr')
#'
#' @export

has_package <-
function(x)
{
    is.element(x, (.packages(all=TRUE)))
}
