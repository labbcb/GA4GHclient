#' @title getRnaQuantification function
#' @description Get an RNA quantification by its ID.
#' @details This function requests \code{GET host/rnaquantifications/rnaQuantificationId}.
#' @param host URL of GA4GH API data server.
#' @param rnaQuantificationId ID of the RNA quantification requested.
#' @return \code{\link{DataFrame}} object.
#' @references \href{http://ga4gh-schemas.readthedocs.io/en/latest/schemas/rna_quantification_service.proto.html#GetRnaQuantification}{Official documentation}.
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' datasetId <- searchDatasets(host, nrows = 1)$id
#' rnaQuantificationSetId <- searchRnaQuantificationSets(host, datasetId, nrows = 1)$id
#' rnaQuantificationId <- searchRnaQuantifications(host, rnaQuantificationSetId, nrows = 1)$id
#' getRnaQuantification(host, rnaQuantificationId)
#' }
#' @seealso \code{\link{DataFrame}}, \code{\link{searchRnaQuantifications}}
#' @export getRnaQuantification
getRnaQuantification <- function(host, rnaQuantificationId)
{
    response <- request.get(host, "rnaquantifications", rnaQuantificationId)
    DataFrame(as.list(unlist(response)))
}
