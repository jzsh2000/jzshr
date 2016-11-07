#' Get matched index
#'
#' Map elements from one set to another, and return the index
#'
#' @importFrom stats na.omit
#'
#' @param query,subject each of them is a vector
#' @param na.rm remove NA from result?
#' @param sort sort result?
#' @param uniq remove duplicated value?
#'
#' @return a integer vector
#'
#' @examples
#' mapidx(c('h','e','l','l','o'), c('w','o','r','l','d'))
#'
#' @export

mapidx <-
function(query, subject, na.rm=TRUE, sort=TRUE, uniq=TRUE) {
    res = match(query, subject)
    if(na.rm) res = na.omit(res)
    if(sort)  res = sort(res)
    if(uniq)  res = unique(res)
    res
}
