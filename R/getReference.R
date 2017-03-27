#' @title getReference function
#' @description Get a reference by its ID.
#' @details This function requests \code{GET host/references/referenceId}.
#' @param host URL of GA4GH API data server.
#' @param referenceId The ID of the Reference to be retrieved.
#' @return \code{\link{DataFrame}} object.
#' @references \href{https://ga4gh-schemas.readthedocs.io/en/latest/schemas/reference_service.proto.html#GetReference}{Official documentation}.
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' referenceSetId <- searchReferenceSets(host, nrows = 1)$id
#' referenceId <- searchReferences(host, referenceSetId, nrows = 1)$id
#' getReference(host, referenceId)
#' }
#' @seealso \code{\link{DataFrame}}, \code{\link{searchReferences}}
#' @export getReference
getReference <- function(host, referenceId)
{
    response <- request.get(host, "references", referenceId)
    DataFrame(as.list(unlist(response)))
}
