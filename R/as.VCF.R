#' @export as.VCF
as.VCF <- function(variants)
{
    rowRanges <- makeGRangesFromDataFrame(variants,
        seqnames.field = "referenceName")
    colData <- DataFrame(row.names = variants$calls[[1]]$callSetName)
    fixed <- DataFrame(REF = DNAStringSet(variants$referenceBases),
        ALT = DNAStringSetList(variants$alternateBases))
    info.idx <- startsWith(names(variants), "info.")
    info <- DataFrame(lapply(variants[, info.idx], SimpleList))
    names(info) <- sub("^info\\.", "", names(info))
    VCF(rowRanges = rowRanges, colData = colData, fixed = fixed, info = info)
}
