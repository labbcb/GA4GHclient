#' @title getDataset function
#' @description Get a dataset by its ID.
#' @details This function requests \code{GET host/datasets/datasetId}.
#' @param host URL of GA4GH API data server.
#' @param datasetId The ID of the dataset to be retrieved.
#' @return \code{\link{DataFrame}} object.
#' @references \href{https://ga4gh-schemas.readthedocs.io/en/latest/schemas/metadata_service.proto.html#GetDataset}{Official documentation}.
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' datasetId <- searchDatasets(host, nrows = 1)$id
#' getDataset(host, datasetId)
#' }
#' @seealso \code{\link{DataFrame}}, \code{\link{searchDatasets}}
#' @export getDataset
getDataset <- function(host, datasetId)
{
    response <- request.get(host, "datasets", datasetId)
    DataFrame(as.list(unlist(response)))
}
