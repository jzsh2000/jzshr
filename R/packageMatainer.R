#' Package Maintainer Information
#'
#' Parses and returns maintainer-associated information of a package.
#' @param pkg a character string with the package name.
#' @param lib.loc a character vector of directory names of R libraries, or
#'                 'NULL'.  The default value of 'NULL' corresponds to all
#'                 libraries currently known.'
#'
#' @return
#' a character vector, with each element represents an maintainer name. When the
#' package isn't available, an empty vector would be returned.
#'
#' @examples
#' packageMaintainer('base')
#'
#' @export

packageMaintainer <-
function(pkg, lib.loc = NULL)
{
    if(!has_package(pkg, lib.loc))
    {
	return(character(0))
    }

    maintainer.str = utils::packageDescription(pkg = pkg, lib.loc = lib.loc, fields = "Maintainer")
    maintainer.str = gsub('\\s*\\n\\s*', ' ', maintainer.str)
    maintainer.vec = stringr::str_extract_all(maintainer.str, '[^,<\\[(]+(\\s*(<[^>]+>)?(\\[[^\\]]+\\])?(\\([^)]+\\))?)*', simplify = TRUE)
    stringr::str_trim(stringr::str_extract(maintainer.vec, '^[^<\\[(]+'))
}

