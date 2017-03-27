#' @title searchDatasets function
#' @description Search for datasets.
#' @details This function requests to \code{POST /datasets/search}.
#' @param host URL of GA4GH API data server.
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
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' searchDatasets(host)
#' }
#' @seealso \code{\link{DataFrame}}, \code{\link{getDataset}}
#' @export searchDatasets
searchDatasets <- function(host, nrows = Inf, responseSize = NA_integer_)
{
    request <- unbox(data.frame(pageSize = responseSize))
    response <- request.post(host, "datasets/search", request)
    while (response$nextPageToken != "" && nrow(response$datasets) < nrows) {
        request$pageToken <- response$nextPageToken
        tmp <- request.post(host, "datasets/search", request)
        response$datasets <- bind_rows(response$datasets, tmp$datasets)
        response$nextPageToken <- tmp$nextPageToken
    }
    if (length(response$datasets) == 0)
        return(NULL)

    if (nrow(response$datasets) > nrows)
        response$datasets <- response$datasets[seq(1, nrows), ]

    is.na(response$datasets) <- response$datasets == "NULL"
    DataFrame(response$datasets)
}
