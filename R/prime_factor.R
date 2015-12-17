#' Do prime factorization on integers
#'
#' Invoke 'factor' command (part of GNU coreutils)
#' @param x an numerical vector.
#' @param uniq logical; if TRUE, duplicated prime factors would be removed.
#'
#' @return a list, with length equal to the input vector.
#'
#' @examples
#' prime_factor(1001:1010)
#' prime_factor(65536, uniq=TRUE)
#'
#' @export

prime_factor <-
function(x, uniq=FALSE)
{
    command = ifelse(Sys.which("gfactor")!="", "gfactor", "factor")
    result.vec <- system2(command, paste(x, collapse = " "), stdout = TRUE)

    lapply(result.vec,
	   function(result){
	       result <- sub("^[0-9]*: *", "", result)
	       result <- strsplit(result, " ", fixed = TRUE)
	       result <- unlist(result)
	       if(uniq) {
		   result <- unique(result)
	       }
	       return(result)
	   })
}
