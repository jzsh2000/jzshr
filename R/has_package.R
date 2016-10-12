#' Check if a package is already installed
#'
#' Read package list and check if a package is installed or not.
#' @param pkg a character vector, each value represents a package name.
#' @param lib.loc a character vector of directory names of R libraries, or
#'                 'NULL'.  The default value of 'NULL' corresponds to all
#'                 libraries currently known.'
#'
#' @return a logical vector. TRUE means the package is installed.
#'
#' @examples
#' has_package('stringr')
#'
#' @export

has_package <-
function(pkg, lib.loc = NULL)
{
    is.element(pkg, (.packages(all.available = TRUE, lib.loc = lib.loc)))
}
