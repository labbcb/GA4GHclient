#' @title getVariant function
#' @description Get a variant by its ID with all call sets for this variant.
#' @details This function requests \code{GET host/variants/variantId}.
#' @param host URL of GA4GH API data server.
#' @param variantId  The ID of the Variant to be retrieved.
#' @param asVCF If \code{TRUE} the function will return an \code{\link{VCF}}
#' with header (default), otherwise it will return an
#' \code{DataFrame}.
#' @return \code{\link{VCF}} object (when \code{asVCF = TRUE}) or
#' \code{\link{DataFrame}} object (otherwise).
#' @references \href{https://ga4gh-schemas.readthedocs.io/en/latest/schemas/variant_service.proto.html#GetVariant}{Official documentation}.
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' datasetId <- searchDatasets(host, nrows = 1)$id
#' variantSetId <- searchVariantSets(host, datasetId, nrows = 1)$id
#' variantId <- searchVariants(host, variantSetId, "1", 15031, 15031)$id
#' getVariant(host, variantId)
#'
#' getVariant(host, variantId, asVCF = FALSE)
#' }
#' @seealso \code{\link{DataFrame}}, \code{\link{searchVariants}},
#' \code{\link{searchVariantsByGRanges}}, \code{\link{VCF}},
#' \code{\link{makeVCFFromGA4GHResponse}}
#' @export getVariant
getVariant <- function(host, variantId, asVCF = TRUE)
{
    variant <- request.get(host, "variants", variantId)
    variant$start <- as.numeric(variant$start) + 1
    variant$end <- as.numeric(variant$end)
    info <- data.frame(lapply(variant$attributes$attr, function(x) {
        r <- x$values
        names(r) <- NULL
        r
    }))
    variant$attributes <- NULL
    calls <- variant$calls
    variant$calls <- NULL
    variant <- DataFrame(variant, info = info)

    if (length(calls) != 0)
        variant$calls <- list(calls)

    if (asVCF) {
        vcf <- makeVCFFromGA4GHResponse(variant)
        header(vcf) <- getVariantSet(host, variant$variantSetId[1])
        vcf
    } else {
        variant
    }
}
