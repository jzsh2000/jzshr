#' Form Contingency Table
#'
#' Construct contingency table from two sets. This step can be used to prepare
#' for fisher exact test.
#'
#' @param set1 the 1st set
#' @param set2 the 2nd set
#' @param total the full set
#'
#' @examples
#' form_table(letters[1:10], letters[5:15], letters)
#'
#' @export

form_table <-
function(set1, set2, total=NULL){
    if(is.null(total)) total <- union(set1, set2)
    matrix(c(length(intersect(set1, set2)),
             length(setdiff  (set1, set2)),
             length(intersect(setdiff(total, set1), set2)),
             length(setdiff  (setdiff(total, set1), set2))),
           2,2)
}
