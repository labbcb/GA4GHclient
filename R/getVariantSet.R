#' @title getVariantSet function.
#' @description Get a variant set by its ID.
#' @details This function requests \code{GET host/variantsets/variantSetId}.
#' @param host URL of GA4GH API data server.
#' @param variantSetId The ID of the VariantSet to be retrieved.
#' @param asVCFHeader If \code{TRUE} the function will return an
#' \code{\link{VCFHeader}} object (default), otherwise it will return an
#' \code{DataFrame}.
#' @return \code{\link{DataFrame}} object. It can be converted
#' into \code{\link{VCFHeader}} object.
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' datasetId <- searchDatasets(host, nrows = 1)$id
#' variantSetId <- searchVariantSets(host, datasetId, nrows = 1)$id
#' getVariantSet(host, variantSetId)
#'
#' getVariantSet(host, variantSetId, asVCF = FALSE)
#' }
#' @seealso \code{\link{DataFrame}}, \code{\link{searchVariantSets}},
#' \code{\link{VCFHeader}}, \code{\link{makeVCFHeaderFromGA4GHResponse}}
#' @export getVariantSet
getVariantSet <- function(host, variantSetId, asVCFHeader = TRUE)
{
    response <- request.get(host, "variantsets", variantSetId)
    metadata <- response$metadata
    response$metadata <- NULL
    response <- DataFrame(response)
    response$metadata <- list(DataFrame(metadata))

    if (asVCFHeader)
        makeVCFHeaderFromGA4GHResponse(response)
    else
        response
}
