#' @title getRnaQuantificationSet function
#' @description Get an RNA quantification set by its ID.
#' @details This function requests \code{GET host/rnaquantificationsets/rnaQuantificationSetId}.
#' @param host URL of GA4GH API data server.
#' @param rnaQuantificationSetId ID of the RNA quantification set requested.
#' @return \code{\link{DataFrame}} object.
#' @references \href{http://ga4gh-schemas.readthedocs.io/en/latest/schemas/rna_quantification_service.proto.html#GetRnaQuantificationSet}{Official documentation}.
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' datasetId <- searchDatasets(host, nrows = 1)$id
#' rnaQuantificationSetId <- searchRnaQuantificationSets(host, datasetId, nrows = 1)$id
#' getRnaQuantificationSet(host, rnaQuantificationSetId)
#' }
#' @seealso \code{\link{DataFrame}}, \code{\link{searchRnaQuantificationSets}}
#' @export getRnaQuantificationSet
getRnaQuantificationSet <- function(host, rnaQuantificationSetId)
{
    response <- request.get(host, "rnaquantificationsets", rnaQuantificationSetId)
    DataFrame(as.list(unlist(response)))
}
