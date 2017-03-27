#' @title getIndividual function
#' @description Get an individual by its ID.
#' @details This function requests \code{GET host/individuals/individualId}.
#' @param host URL of GA4GH API data server.
#' @param individualId ID of the individual requested.
#' @return \code{\link{DataFrame}} object.
#' @references \href{http://ga4gh-schemas.readthedocs.io/en/latest/schemas/bio_metadata_service.proto.html#GetIndividual}{Official documentation}.
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' datasetId <- searchDatasets(host, nrows = 1)$id
#' individualId <- searchIndividuals(host, datasetId, nrows = 1)$id
#' getIndividual(host, individualId)
#' }
#' @seealso \code{\link{DataFrame}}, \code{\link{searchIndividuals}}
#' @export getIndividual
getIndividual <- function(host, individualId)
{
    response <- request.get(host, "individuals", individualId)
    DataFrame(as.list(unlist(response)))
}
