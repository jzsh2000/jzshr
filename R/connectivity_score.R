#' Broad CMAP gene set enrichment metrics
#'
#' A method for computing Broad CMAP connectivity scores. This method is a kind
#' of gene set enrichment analysis, it's initially designed for microarray
#' samples, but could also applied to RNA-seq data. To use this method, we need
#' a expression matrix and two corresponding gene sets.
#'
#' @param data.matrix the gene expression matrix
#' @param id.up first gene set, usually the up-regulated genes in a cell line
#' @param id.down second gene set, usually the down-regulated genes in a cell
#'                line
#'
#' @seealso \code{\link[gCMAP]{connectivity_score}}
#' @references
#' \url{http://portals.broadinstitute.org/cmap/help_topics_linkified.jsp#connectivity score}
#'
#' @export

connectivity_score <- function(data.matrix, id.up, id.down)
{
    normal <- function (scores)
    {
        p <- max(scores)
        q <- min(scores)
        ifelse(scores == 0, 0, ifelse(scores > 0, scores/p, -scores/q))
    }

    ks <- function (V, n)
    {
        t <- length(V)
        if (t == 0) {
            return(0)
        }
        else {
            if (is.unsorted(V))
                V <- sort(V)
            d <- (1:t)/t - V/n
            a <- max(d)
            b <- -min(d) + 1/t
            ifelse(a > b, a, -b)
        }
    }

    id.up   = intersect(id.up,   rownames(data.matrix))
    id.down = intersect(id.down, rownames(data.matrix))

    raw.score <- apply(data.matrix, 2, function(x) {
        r = rank(-x)
        ks_up   = ks(r[id.up],   length(r))
        ks_down = ks(r[id.down], length(r))
        ifelse(sign(ks_up) == sign(ks_down), 0, ks_up - ks_down)
    })

    score <- normal(raw.score)
    return(score)
}
