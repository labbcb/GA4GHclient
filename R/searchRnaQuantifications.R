#' @title searchRnaQuantifications function
#' @description This function gets a list of RnaQuantifications matching the
#' search criteria.
#' @details This function requests to \code{/rnaquantifications/search}.
#' @param host URL of GA4GH API data server.
#' @param rnaQuantificationSetId IReturn only Rna Quantifications which belong
#' to this set.
#' @param biosampleId Return only RNA quantifications regarding the specified
#' biosample.
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
#' @references \href{http://ga4gh-schemas.readthedocs.io/en/latest/schemas/rna_quantification_service.proto.html#SearchRnaQuantifications}{Official documentation}.
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' datasetId <- searchDatasets(host, nrows = 1)$id
#' id <- searchRnaQuantificationSets(host, datasetId, nrows = 1)$id
#' searchRnaQuantifications(host, rnaQuantificationSetId = id)
#' }
#' @seealso \code{\link{DataFrame}}
#' @export searchRnaQuantifications
searchRnaQuantifications <- function(host, rnaQuantificationSetId,
    biosampleId = NA_character_, nrows = Inf, responseSize = NA_integer_)
{
    request <- unbox(data.frame(rnaQuantificationSetId, biosampleId,
        pageSize = responseSize))
    response <- request.post(host, "rnaquantifications/search", request)
    while (response$nextPageToken != "" && nrow(response$rnaQuantifications) < nrows) {
        request$pageToken <- response$nextPageToken
        tmp <- request.post(host, "rnaquantifications/search", request)
        response$rnaQuantifications <- bind_rows(response$rnaQuantifications, tmp$rnaQuantifications)
        response$nextPageToken <- tmp$nextPageToken
    }
    if (length(response$rnaQuantifications) == 0)
        return(NULL)

    if (nrow(response$rnaQuantifications) > nrows)
        response$rnaQuantifications <- response$rnaQuantifications[seq(1, nrows), ]

    is.na(response$rnaQuantifications) <- response$rnaQuantifications == "NULL"
    DataFrame(response$rnaQuantifications)
}
