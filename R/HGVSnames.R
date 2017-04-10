#' @title Generate genomic variant data to HGVS nomenclature
#' @description This function follows the official reference HGVS nomenclature.
#' At this moment, it supports only 'substitution' and 'indel' for DNA sequences.
#' @param start genomic location of start
#' @param ref reference sequence
#' @param alt alternate sequence
#' @param type Sequence type to be used as prefix. Allowed options are:
#' \itemize{
#'   \item \code{g} genomic (default);
#'   \item \code{m} mitochondrial;
#'   \item \code{c} coding DNA;
#'   \item \code{n} non-coding DNA.
#' }
#' @param seqnames name of sequence (e.g. chr1, 1). It is optional.
#' @return Genomic coordinates of variants formatted as HGVS nomenclature.
#' @references \href{http://varnomen.hgvs.org/}{Sequence Variant Nomenclature}.
#' @examples
#' start <- c(45576, "88+1", 6775, 6775, 145, 9002, 4, 12345611, 58347698)
#' ref <- c("A", "G", "T", "TCA", "CGA", "AAAAAAAA", "GC", "G", "A")
#' alt <- c("C", "T", "GA", "C", "TGG", "TTT", "TG", "A", "*")
#' type <- c("g", "c", "g", "g", "c", "g", "g", "g", "g")
#' seqnames <- c("", "", NA, NA, NA, NA, NA, "chr11", NA)
#' HGVSnames(start, ref, alt, type, seqnames)
#' @export HGVSnames
HGVSnames <- function(start, ref, alt, type = "g", seqnames=NA_character_)
{
    if (!any(type %in% c("g", "m", "c", "n")))
        stop("Invalid type.")
    is.del <- alt == "*"
    is.sub <- nchar(ref) - nchar(alt) == 0 & nchar(ref) == 1 & !is.del
    has.seqnames <- !(is.na(seqnames) | seqnames == "")

    seqname <- ifelse(has.seqnames, paste0(seqnames, ":"), "")
    end <- as.integer(sub("\\D.+$", "", start)) + nchar(ref) - 1
    mode <- ifelse(is.del, "del", ifelse(is.sub, ">", "delins"))

    paste0(seqname, type, ".", start,
        ifelse(is.sub | nchar(ref) == 1, "", paste0("_", end)),
        ifelse(is.sub, ref, ""),
        mode,
        ifelse(is.del, "", alt))
}
