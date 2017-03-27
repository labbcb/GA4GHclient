#' @title Gets getVariantAnnotationSet function
#' @description Get a variant annotation set by its ID.
#' @details This function requests
#' \code{GET host/variantannotationsets/variantAnnotationSetId}.
#' @param host URL of GA4GH API data server.
#' @param variantAnnotationSetId ID of variant annotation set.
#' @return \code{\link{DataFrame}} object.
#' @references \href{https://ga4gh-schemas.readthedocs.io/en/latest/schemas/allele_annotation_service.proto.html#GetVariantAnnotationSet}{Official documentation}.
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' datasetId <- searchDatasets(host, nrows = 1)$id
#' variantSetId <- searchVariantSets(host, datasetId, nrows = 2)$id[2]
#' id <- searchVariantAnnotationSets(host, variantSetId, nrows = 1)$id
#' getVariantAnnotationSet(host, variantAnnotationSetId = id)
#' }
#' @seealso \code{\link{DataFrame}},
#' \code{\link{searchVariantAnnotationSets}}
#' @export getVariantAnnotationSet
getVariantAnnotationSet <- function(host, variantAnnotationSetId)
{
    response <- request.get(host,"variantannotationsets",variantAnnotationSetId)
    DataFrame(as.list(unlist(response)))
}
