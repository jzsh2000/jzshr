#' Generate a PCA plot from a gene expression matrix
#'
#' This function is able to log transform (use rlog function in DESeq2 package)
#' count data originated from a RNA-seq experiment, and draw a PCA plot to
#' reflect sample relationship. That's a good way to check batch effect from
#' gene expression matrix.
#'
#' @importFrom DESeq2 DESeqDataSetFromMatrix rlog plotPCA
#' @import ggplot2
#'
#' @param count the gene expression count matrix
#' @param color a character vector whose length is same as samples, different
#'              value corresponds to different point color in the plot
#' @param shape a character vector whose length is same as samples, different
#'              value corresponds to different point shape in the plot
#' @param color.legend the legend text corresponds to point color
#' @param shape.legend the legend text corresponds to point shape
#'
#' @examples
#' count <- read.delim("CD5-DC/rawcount/hg19/raw-count.txt", row.names = 1)
#' # head(count)
#' plot_pca(count, rep(c("CD5p", "CD5n"), each=3),
#'     rep(c("AN239","AN342","AN403"),2))
#'
#' @export

plot_pca <-
function(count, color, shape, color.legend = "cell", shape.legend = "donor")
{
    dds <- DESeqDataSetFromMatrix(count,
              colData=data.frame(id=colnames(count), color=color, shape=shape),
              design=~color)
    rld <- rlog(dds)
    data <- plotPCA(rld, intgroup=c("color", "shape"), returnData=TRUE)
    percentVar <- round(100 * attr(data, "percentVar"))
    ggplot(data, aes(PC1, PC2, color=color, shape=shape)) +
        geom_point(size=3) +
        xlab(paste0("PC1: ",percentVar[1],"% variance")) +
        ylab(paste0("PC2: ",percentVar[2],"% variance")) +
        labs(color=color.legend, shape=shape.legend) +
        coord_fixed()
}
