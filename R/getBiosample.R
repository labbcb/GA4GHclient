#' @title getBiosample function
#' @description Get a biosample by its ID.
#' @details This function requests \code{GET host/datasets/biosampleId}.
#' @param host URL of GA4GH API data server.
#' @param biosampleId ID of the biosample requested.
#' @return \code{\link{DataFrame}} object.
#' @references \href{http://ga4gh-schemas.readthedocs.io/en/latest/schemas/bio_metadata_service.proto.html#GetBiosample}{Official documentation}.
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' datasetId <- searchDatasets(host, nrows = 1)$id
#' biosampleId <- searchBiosamples(host, datasetId, nrows = 1)$id
#' getBiosample(host, biosampleId)
#' }
#' @seealso \code{\link{DataFrame}}, \code{\link{searchBiosamples}}
#' @export getBiosample
getBiosample <- function(host, biosampleId)
{
    response <- request.get(host, "biosamples", biosampleId)
    DataFrame(as.list(unlist(response)))
}
