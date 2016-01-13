#' Package Author Information
#'
#' Parses and returns author-associated information of a package.
#' @param pkg a character string with the package name.
#' @param lib.loc a character vector of directory names of R libraries, or
#'                 'NULL'.  The default value of 'NULL' corresponds to all
#'                 libraries currently known.'
#'
#' @return
#' a character vector, with each element represents an author name. When the
#' package isn't available, an empty vector would be returned.
#'
#' @examples
#' packageAuthor('base')
#'
#' @export

packageAuthor <-
function(pkg, lib.loc = NULL)
{
    if(!has_package(pkg, lib.loc))
    {
	return(character(0))
    }

    author.str = utils::packageDescription(pkg = pkg, lib.loc = lib.loc, fields = "Author")
    author.str = gsub('\\s*\\n\\s*', ' ', author.str)
    author.vec = stringr::str_extract_all(author.str, '[^,<\\[(]+(\\s*(<[^>]+>)?(\\[[^\\]]+\\])?(\\([^)]+\\))?)*', simplify = TRUE)
    stringr::str_trim(stringr::str_extract(author.vec, '^[^<\\[(]+'))
}
