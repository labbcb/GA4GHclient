#' @title searchExpressionLevels function
#' @description This function gets expression levels matching the search
#' criteria.
#' @details This function requests to \code{/expressionlevels/search}.
#' @param host URL of GA4GH API data server.
#' @param rnaQuantificationId Id of the rnaQuantification to restrict search to.
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
#' @references \href{http://ga4gh-schemas.readthedocs.io/en/latest/schemas/rna_quantification_service.proto.html#SearchExpressionLevels}{Official documentation}.
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' datasetId <- searchDatasets(host, nrows = 1)$id
#' rnaQuantificationSetId <- searchRnaQuantificationSets(host, datasetId, nrow = 1)$id
#' rnaQuantificationId <- searchRnaQuantifications(host, rnaQuantificationSetId, nrows = 1)$id
#' searchExpressionLevels(host, rnaQuantificationId, nrows = 10)
#' }
#' @seealso \code{\link{DataFrame}}, \code{\link{getExpressionLevel}},
#' \code{\link{searchRnaQuantificationSets}}
#' @export searchExpressionLevels
searchExpressionLevels <- function(host, rnaQuantificationId, nrows = Inf,
    responseSize = NA_integer_)
{
    request <- unbox(data.frame(rnaQuantificationId, pageSize = responseSize))
    response <- request.post(host, "expressionlevels/search", request)
    while (!is.null(response$nextPageToken) && nrow(response$expressionLevels) < nrows) {
        request$pageToken <- response$nextPageToken
        tmp <- request.post(host, "expressionlevels/search", request)
        response$expressionLevels <- bind_rows(response$expressionLevels, tmp$expressionLevels)
        response$nextPageToken <- tmp$nextPageToken
    }
    if (length(response$expressionLevels) == 0)
        return(NULL)

    if (nrow(response$expressionLevels) > nrows)
        response$expressionLevels <- response$expressionLevels[seq(1, nrows), ]

    is.na(response$expressionLevels) <- response$expressionLevels == "NULL"
    DataFrame(response$expressionLevels)
}
