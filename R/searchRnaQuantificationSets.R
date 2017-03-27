#' @title searchRnaQuantificationSets function
#' @description This function gets a list of RNA quantification sets matching
#' the search criteria.
#' @details This function requests to \code{/rnaquantificationsets/search}.
#' @param host URL of GA4GH API data server.
#' @param datasetId Id of the dataset to search.
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
#' @references \href{http://ga4gh-schemas.readthedocs.io/en/latest/schemas/rna_quantification_service.proto.html#SearchRnaQuantificationSets}{Official documentation}.
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' datasetId <- searchDatasets(host, nrows = 1)$id
#' searchRnaQuantificationSets(host, datasetId, nrows = 1)
#' }
#' @seealso \code{\link{DataFrame}}, \code{\link{getRnaQuantificationSet}}
#' @export searchRnaQuantificationSets
searchRnaQuantificationSets <- function(host, datasetId, nrows = Inf,
    responseSize = NA_integer_)
{
    request <- unbox(data.frame(datasetId, pageSize = responseSize))
    response <- request.post(host, "rnaquantificationsets/search", request)
    while (response$nextPageToken != "" && nrow(response$rnaQuantificationSets) < nrows) {
        request$pageToken <- response$nextPageToken
        tmp <- request.post(host, "rnaquantificationsets/search", request)
        response$rnaQuantificationSets <- bind_rows(response$rnaQuantificationSets, tmp$rnaQuantificationSets)
        response$nextPageToken <- tmp$nextPageToken
    }
    if (length(response$rnaQuantificationSets) == 0)
        return(NULL)

    if (nrow(response$rnaQuantificationSets) > nrows)
        response$rnaQuantificationSets <- response$rnaQuantificationSets[seq(1, nrows), ]

    is.na(response$rnaQuantificationSets) <- response$rnaQuantificationSets == "NULL"
    DataFrame(response$rnaQuantificationSets)
}
