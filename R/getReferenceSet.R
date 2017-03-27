#' @title getReferenceSet function
#' @description Get a reference set by its ID.
#' @details This function requests \code{GET host/referencesets/referenceSetId}.
#' @param host URL of GA4GH API data server.
#' @param referenceSetId The ID of the ReferenceSet to be retrieved.
#' @return \code{\link{DataFrame}} object.
#' @references \href{https://ga4gh-schemas.readthedocs.io/en/latest/schemas/reference_service.proto.html#GetReferenceSet}{Official documentation}.
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' referenceSetId <- searchReferenceSets(host, nrows = 1)$id
#' getReferenceSet(host, referenceSetId)
#' }
#' @seealso \code{\link{DataFrame}}, \code{\link{searchReferenceSets}}
#' @export getReferenceSet
getReferenceSet <- function(host, referenceSetId)
{
    response <- request.get(host, "referencesets", referenceSetId)
    DataFrame(as.list(unlist(response)))
}
