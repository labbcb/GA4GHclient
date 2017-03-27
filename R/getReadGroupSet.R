#' @title getReadGroupSet function
#' @description Get a read group set by its ID.
#' @details This function requests \code{GET host/readgroupsets/readGroupSetId}.
#' @param host URL of GA4GH API data server.
#' @param readGroupSetId The ID of the ReadGroupSet to be retrieved.
#' @return \code{\link{DataFrame}} object.
#' @references \href{https://ga4gh-schemas.readthedocs.io/en/latest/schemas/read_service.proto.html#GetReadGroupSet}{Official documentation}.
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' datasetId <- searchDatasets(host, nrows = 1)$id
#' readGroupSetId <- searchReadGroupSets(host, datasetId, nrows = 1)$id
#' getReadGroupSet(host, readGroupSetId)
#' }
#' @seealso \code{\link{DataFrame}}, \code{\link{searchReadGroupSets}}
#' @export getReadGroupSet
getReadGroupSet <- function(host, readGroupSetId)
{
    response <- request.get(host, "readgroupsets", readGroupSetId)
    DataFrame(response)
}
