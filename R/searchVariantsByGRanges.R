#' @title searchVariantsByGranges function
#' @description Search for variants by genomic ranges (lines of VCF files)
#' @details This function maps to the body of \code{POST host/variants/search}.
#' @param host URL of GA4GH API data server.
#' @param variantSetId The variant set to search.
#' @param granges A GRanges object containing one or more genomic ranges.
#' @param callSetIds Only return variant calls which belong to callsets with
#' these IDs. If unspecified, return all variants and no variant call objects.
#' @param nrows Number of rows of the data frame returned by this function.
#' If not defined, the function will return all entries. If the number of
#' available entries is less than the value of this this parameter, the function
#' will silently return only the available entries.
#' @param responseSize Specifies the number of entries to be returned by the
#' server until reach the number of rows defined in \code{nrows} parameter or
#' until get all available entries. If not defined, the server will return the
#' allowed maximum reponse size. Increasing this the value of this parameter will
#' reduce the number of requests and reducing the time required. The will not
#' respect this parameter if the value if larger than its maximum response size.
#' @param asVCF If \code{TRUE} the function will return a list of
#' \code{\link{VCF}} object with headers (default), otherwise it will return a
#' list of \code{DataFrame} objects.
#' @return List of \code{\link{VCF}} objects (when \code{asVCF = TRUE}) or
#' a list of \code{\link{DataFrame}} objects (otherwise).
#' Each row in GRanges object will be a element of the list.
#' @references \href{https://ga4gh-schemas.readthedocs.io/en/latest/schemas/variant_service.proto.html#SearchVariants}{Official documentation}.
#' @examples
#' library(GenomicRanges)
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' datasetId <- searchDatasets(host, nrows = 1)$id
#' variantSetId <- searchVariantSets(host, datasetId, nrows = 1)$id[1]
#' granges <- GRanges(seqnames = "1", IRanges(start = 15000, end = 16000))
#' searchVariantsByGRanges(host, variantSetId, granges)
#' }
#' @seealso \code{\link{DataFrame}}, \code{\link{searchVariants}}
#' \code{\link{getVariant}}, \code{\link{VCF}}
#' @export searchVariantsByGRanges
searchVariantsByGRanges <- function(host, variantSetId, granges,
    callSetIds = character(), nrows = Inf, responseSize = NA_integer_,
    asVCF = FALSE)
{
    granges.list <- as(granges, "GRangesList")
    lapply(granges.list, function(grange) {
        referenceName <- as.character(seqnames(grange))
        start <- start(grange)
        end <- end(grange)
        searchVariants(host, variantSetId, referenceName, start, end,
            callSetIds, nrows, responseSize, asVCF)
    })
}
