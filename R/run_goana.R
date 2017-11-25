#' Run gene ontology enrichment analysis
#'
#' Find enriched GO terms from a list of gene entrez id or gene symbol.
#'
#' @importFrom limma goana alias2Symbol
#' @import dplyr
#' @importFrom purrr map_chr
#' @importFrom AnnotationDbi toTable mapIds
#'
#' @param de a character vector of Entrez Gene IDs, or a list of such vectors, or an `MArrayLM` fit object.
#' @param species character string specifying the species. Possible values include "Hs" (human), "Mm" (mouse), "Rn" (rat), "Dm" (fly) or "Pt" (chimpanzee), but other values are possible if the corresponding organism package is available.
#' @param FDR false discovery rate cutoff for differentially expressed genes. Numeric value between 0 and 1.
#' @param N.min minimal number of genes in a GO term
#'
#' @return a data_frame
#'
#' @examples
#' gene_list = c('SIGLEC1', 'ADAM33', 'PPP1R14A', 'FFAR1', 'CD22', 'DNASE2',
#'               'TNFSF13', 'CCND3', 'VASH1', 'RNF141', 'MED12L', 'SIGLEC6',
#'               'GPR160', 'POU3F1', 'AXL', 'ING1', 'EPHB4', 'SLC1A4', 'B3GNT7',
#'               'ATP1A2', 'SPOCK2', 'TGFBR1', 'LRRC8A', 'LTA4H', 'VASN', 'LTK',
#'               'CDH1')
#' run_goana(gene_list, FDR = 0.05)
#'
#' @seealso \code{\link[limma]{goana}}
#' @export

run_goana <- function(de, species = "Hs", FDR = 0.01, N.min = 10) {

    orgPkg <- paste0("org.", species, ".eg.db")
    suppressPackageStartupMessages(OK <- requireNamespace(orgPkg, quietly = TRUE))
    if (!OK)
        stop(orgPkg, " package required but not not installed (or can't be loaded)")
    ORG_DB <- tryCatch(
        getFromNamespace(paste0("org.", species, ".eg.db"), orgPkg),
        error = function(e)
            FALSE
    )

    de = AnnotationDbi::mapIds(ORG_DB,
                               keys = de,
                               keytype = 'SYMBOL',
                               column = 'ENTREZID')
    de = unname(na.omit(de))

    obj <- paste0("org.", species, ".egGO2ALLEGS")
    egGO2ALLEGS <- tryCatch(getFromNamespace(obj, orgPkg), error = function(e) FALSE)
    if (is.logical(egGO2ALLEGS))
        stop("Can't find gene ontology mappings in package ",
             orgPkg)
    EG.GO <- toTable(egGO2ALLEGS)
    d <- duplicated(EG.GO[, c("gene_id", "go_id", "Ontology")])
    EG.GO <- EG.GO[!d, ]

    goana_res = goana(de, species = species)
    goana_res = as_data_frame(goana_res) %>%
        mutate(go_id = rownames(goana_res)) %>%
        mutate(padj = p.adjust(P.DE, method = 'fdr')) %>%
        filter(Ont == 'BP', N >= N.min, padj < FDR) %>%
        arrange_('P.DE') %>%
        mutate(gene_list = map_chr(.$go_id, function(id) {
            full_table = subset(EG.GO, go_id == id)
            gene_name_list = mapIds(ORG_DB,
                                    keys = intersect(full_table$gene_id,
                                                     de),
                                    keytype = 'ENTREZID',
                                    column = 'SYMBOL')
            paste(unique(gene_name_list), collapse = ', ')
        })) %>%
        select_('go_id', 'Term', 'N', 'DE', 'gene_list', 'P.DE', 'padj')
}
