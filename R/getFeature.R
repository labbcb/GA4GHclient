#' @title getFeature function
#' @description Get a feature set by its ID (a line of genomic feature file).
#' @details This function requests \code{GET host/features/featureId}.
#' @param host URL of GA4GH API data server.
#' @param featureId The ID of the feature to be retrieved.
#' @return \code{\link{DataFrame}} object.
#' @references \href{https://ga4gh-schemas.readthedocs.io/en/latest/schemas/sequence_annotation_service.proto.html#GetFeature}{Official documentation}.
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' datasetId <- searchDatasets(host, nrows = 1)$id
#' featureSetId <- searchFeatureSets(host, datasetId, nrows = 1)$id
#' featureId <- searchFeatures(host, featureSetId, nrows = 1)$id
#' getFeature(host, featureId)
#' }
#' @seealso \code{\link{DataFrame}}, \code{\link{searchFeatures}}
#' @export getFeature
getFeature <- function(host, featureId)
{
    response <- request.get(host, "features", featureId)
    DataFrame(as.list(unlist(response)))
}
