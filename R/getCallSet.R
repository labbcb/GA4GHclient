#' @title getCallSet function
#' @description Get a call set by its ID.
#' @details This request maps to \code{GET host/callsets/callSetId}.
#' @param host URL of GA4GH API data server.
#' @param callSetId The ID of the CallSet to be retrieved.
#' @return \code{\link{DataFrame}} object.
#' @references \href{https://ga4gh-schemas.readthedocs.io/en/latest/schemas/variant_service.proto.html#GetCallSet}{Official documentation}.
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' datasetId <- searchDatasets(host, nrows = 1)$id
#' variantSetId <- searchVariantSets(host, datasetId, nrows = 1)$id
#' callSetId <- searchCallSets(host, variantSetId, nrows = 1)$id
#' getCallSet(host, callSetId)
#' }
#' @seealso \code{\link{DataFrame}}, \code{\link{searchCallSets}}
#' @export getCallSet
getCallSet <- function(host, callSetId)
{
    response <- request.get(host, "callsets", callSetId)
    DataFrame(as.list(unlist(response)))
}
