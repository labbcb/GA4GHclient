#' @title searchBiosamples function
#' @description This function gets Biosamples matching the search criteria.
#' @details This function requests to \code{/biosamples/search}.
#' @param host URL of GA4GH API data server.
#' @param datasetId Id of the dataset to search.
#' @param name Returns Biosamples with the given name found by case-sensitive
#' string matching.
#' @param individualId Returns Biosamples for the provided individual ID.
#' @param nrows Number of rows of the data frame returned by this function.
#' If not defined, the function will return all entries. If the number of
#' available entries is less than the value of this this parameter, the function
#' will silently return only the available entries.
#' @param responseSize Specifies the number of entries to be returned by the
#' server until reach the number of rows defined in \code{nrows} parameter or
#' until get all available entries. If not defined, the server will return the
#' allowed maximum reponse size. Increasing this the value of this parameter will
#' reduce the number of requests and reducing the time required. The will not
#' respect this parameter if the value if larger than its maximum response size.
#' @return \code{\link{DataFrame}} object. \code{NULL} means no registry found.
#' @references \href{http://ga4gh-schemas.readthedocs.io/en/latest/schemas/bio_metadata_service.proto.html#SearchBiosamples}{Official documentation}.
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' datasetId <- searchDatasets(host, nrows = 1)$id
#' searchBiosamples(host, datasetId, nrows = 10)
#' }
#' @seealso \code{\link{DataFrame}}, \code{\link{getBiosample}}
#' @export searchBiosamples
searchBiosamples <- function(host, datasetId, name = NA_character_,
    individualId = NA_character_, nrows = Inf, responseSize = NA_integer_)
{
    request <- unbox(data.frame(datasetId, name, individualId,
        pageSize = responseSize))
    response <- request.post(host, "biosamples/search", request)
    while (response$nextPageToken != "" && nrow(response$biosamples) < nrows) {
        request$pageToken <- response$nextPageToken
        tmp <- request.post(host, "biosamples/search", request)
        response$biosamples <- bind_rows(response$biosamples, tmp$biosamples)
        response$nextPageToken <- tmp$nextPageToken
    }
    if (length(response$biosamples) == 0)
        return(NULL)

    if (nrow(response$biosamples) > nrows)
        response$biosamples <- response$biosamples[seq(1, nrows), ]

    is.na(response$biosamples) <- response$biosamples == "NULL"
    DataFrame(response$biosamples)
}
