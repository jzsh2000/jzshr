#' Read multiple HTSeq-count data into a matrix
#'
#' HTSeq-count output a tab-delimed count file, while the first column records
#' gene_id, and the second column records raw read count. \code{read_htseq}
#' merges multiple HTSeq-count output into one matrix. The count files could be
#' selected using \code{\link{Sys.glob}}.
#'
#' @importFrom utils read.delim
#'
#' @param files HTSeq-count output files
#' @param labels sample labels used as colnames in count matrix
#' @param clean clean meta-line (start with `__`) or not
#'
#' @return a count matrix, with rows as genes and columns as samples
#' @aliases read.htseq
#' @export

read_htseq <-
function(files, labels=NULL, clean=TRUE) {
    count.matrix = sapply(files,
        function(file){
            read.delim(file, header = FALSE, stringsAsFactors = FALSE)[,2]
        })

    rownames(count.matrix)=read.delim(
        files[1], header = FALSE, stringsAsFactors = FALSE)[,1]

    if(is.null(labels)) {
        colnames(count.matrix) = sub("\\.[^.]*$", "", basename(files))
    } else {
        colnames(count.matrix) = labels
    }

    if(clean) {
        row.valid = !grepl("^__", rownames(count.matrix))
        count.matrix = count.matrix[row.valid,]
    }

    return(count.matrix)
}
