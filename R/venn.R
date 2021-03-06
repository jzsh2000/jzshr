#' Generate a plot to illustrate the relationship between two sets
#'
#' This function does the following things: 1) get input from two vectors; 2)
#' calculate the intersect between them; 3) show the relative ratio between the
#' intersect size and the original sets. To some extent it has the same
#' functionality as the venn plot.
#'
#' @importFrom graphics plot rect text
#'
#' @param set1 1st vector of values
#' @param set2 2nd vector of values
#' @param uniq should duplicated values be removed?
#' @param xlab default title for the x axis
#' @param ylab default title for the y axis
#' @param ... other parameters passed to `plot` function
#'
#' @examples
#' venn(sample(letters, 20), sample(letters, 25))
#' venn(sample(letters, 20, TRUE), sample(letters, 25, TRUE), FALSE)
#'
#' @export

venn <-
function(set1, set2, uniq=TRUE, xlab="set2", ylab="set1", ...) {
    color.1 = "#FF6347"
    color.2 = "#47E3FF"
    if(uniq) {
        set.1i = set.2i = length(intersect(set1, set2))
        set.1 = length(unique(set1))
        set.2 = length(unique(set2))
    } else {
        set.1i = sum(set1 %in% intersect(set1, set2))
        set.2i = sum(set2 %in% intersect(set1, set2))
        set.1 = length(set1)
        set.2 = length(set2)
    }
    plot(c(0,set.2), c(0,set.1), type="n", xlab=xlab, ylab=ylab, ...)
    # color: tomato
    rect(0,0,set.2, set.1i, col=color.1, border="transparent")
    # color generated by colortools::opposite("tomato")
    rect(0,0,set.2i, set.1, col=color.2, border="transparent")
    rect(0,0,set.2i, set.1i, col="grey", border="transparent")
    # text(set.2, set.1, as.character(set.i), adj=c(1,1))
    text((set.2+set.2i)/2, set.1i/2, as.character(set.2-set.2i),
         col=ifelse(set.1i==0, color.1, color.2), cex=2)
    text(set.2i/2, (set.1+set.1i)/2, as.character(set.1-set.1i),
         col=ifelse(set.2i==0, color.2, color.1), cex=2)
    if(uniq || set.2i == set.1i) {
        text(set.2i/2, set.1i/2, as.character(set.2i),
             col=ifelse(set.2i==0, "grey", "white"), cex=2)
    } else {
        text(set.2i/2, set.1i/2, paste(set.1i, '|', set.2i),
             col=ifelse(set.2i==0, "grey", "white"), cex=2)
    }
}
