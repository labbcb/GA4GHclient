#' @title searchVariants function
#' @description Search for variants by genomic ranges (lines of VCF files).
#' @details This function maps to \code{POST host/variants/search}.
#' @param host URL of GA4GH API data server.
#' @param variantSetId The variant set to search.
#' @param referenceName Required. Only return variants on this reference.
#' @param start Required. The beginning of the window (1-based, inclusive) for
#' which overlapping variants should be returned. Genomic positions are
#' non-negative integers less than reference length. Requests spanning the join
#' of circular genomes are represented as two requests one on each side of the
#' join (position 1).
#' @param end Required. The end of the window (1-based, inclusive) for which
#' overlapping variants should be returned.
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
#' @param asVCF If \code{TRUE} the function will return an \code{\link{VCF}}
#' with header (default), otherwise it will return an
#' \code{DataFrame}.
#' @return \code{\link{VCF}} object (when \code{asVCF = TRUE}) or
#' \code{\link{DataFrame}} object (otherwise).
#' @references \href{https://ga4gh-schemas.readthedocs.io/en/latest/schemas/variant_service.proto.html#SearchVariants}{Official documentation}.
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' datasetId <- searchDatasets(host, nrows = 1)$id
#' variantSetId <- searchVariantSets(host, datasetId, nrows = 1)$id
#' searchVariants(host, variantSetId, referenceName = "1",
#'     start = 15000, end = 16000)
#'
#' searchVariants(host, variantSetId, referenceName = "1",
#'     start = 15000, end = 16000, asVCF = FALSE)
#' }
#' @seealso \code{\link{DataFrame}}, \code{\link{getVariant}},
#' \code{\link{searchVariantsByGRanges}}, \code{\link{VCF}},
#' \code{\link{makeVCFFromGA4GHResponse}}
#' @export searchVariants
searchVariants <- function(host, variantSetId, referenceName, start, end,
    callSetIds = character(), nrows = Inf, responseSize = NA_integer_,
    asVCF = TRUE)
{
    request <- unbox(data.frame(variantSetId, referenceName, start = start - 1,
        end = end, pageSize = responseSize, stringsAsFactors = FALSE))
    if (length(callSetIds) != 0)
        request$callSetIds <- list(callSetIds)
    response <- request.post(host, "variants/search", request)
    while (!is.null(response$nextPageToken) && nrow(response$variants) < nrows) {
        request$pageToken <- response$nextPageToken
        tmp <- request.post(host, "variants/search", request)
        response$variants <- bind_rows(response$variants, tmp$variants)
        response$nextPageToken <- tmp$nextPageToken
    }
    if (length(response$variants) == 0)
        return(DataFrame())

    if (nrow(response$variants) > nrows)
        response$variants <- response$variants[seq(1, nrows), ]

    is.na(response$variants) <- response$variants == "NULL"

    variants <- select(response$variants, -starts_with("attributes")) %>%
        mutate(
            start = as.numeric(start) + 1,
            end = as.numeric(end),
            names = lapply(names, function(x) ifelse(length(x) == 0, NA, x))
        )

    info <- select(response$variants, starts_with("attributes"))
    names(info) <- sub("^attributes\\.attr\\.(.+)\\.values", "info.\\1", names(info))
    info <- data.frame(do.call(cbind, lapply(info, function(x) lapply(x, unlist, use.names = FALSE))))

    if (asVCF) {
        vcf <- makeVCFFromGA4GHResponse(cbind(variants, info))
        header(vcf) <- getVariantSet(host, variantSetId)
        vcf
    } else {
        DataFrame(cbind(variants, info))
    }
}
