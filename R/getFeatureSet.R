#' @title getFeatureSet function
#' @description Get a feature set by its ID.
#' @details This function requests \code{GET host/featuresets/featureSetId}.
#' @param host URL of GA4GH API data server.
#' @param featureSetId The ID of the FeatureSet to be retrieved.
#' @return \code{\link{DataFrame}} object.
#' @references \href{https://ga4gh-schemas.readthedocs.io/en/latest/schemas/sequence_annotation_service.proto.html#GetFeatureSet}{Official documentation}.
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' datasetId <- searchDatasets(host, nrows = 1)$id
#' featureSetId <- searchFeatureSets(host, datasetId, nrows = 1)$id
#' getFeatureSet(host, featureSetId)
#' }
#' @seealso \code{\link{DataFrame}}, \code{\link{searchFeatureSets}}
#' @export getFeatureSet
getFeatureSet <- function(host, featureSetId)
{
    response <- request.get(host, "featuresets", featureSetId)
    DataFrame(as.list(unlist(response)))
}
