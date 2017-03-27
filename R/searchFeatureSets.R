#' @title searchFeatureSets function
#' @description Search for feature sets (genomic features, e.g. GFF files).
#' @details This function requests \code{POST host/featuresets/search}.
#' @param host URL of GA4GH API data server.
#' @param datasetId The Dataset to search.
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
#' @return \code{\link{DataFrame}} object.
#' @references \href{https://ga4gh-schemas.readthedocs.io/en/latest/schemas/sequence_annotation_service.proto.html#SearchFeatureSets}{Official documentation}.
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' datasetId <- searchDatasets(host, nrows = 1)$id
#' searchFeatureSets(host, datasetId)
#' }
#' @seealso \code{\link{DataFrame}}, \code{\link{getFeatureSet}}
#' @export searchFeatureSets
searchFeatureSets <- function(host, datasetId, nrows = Inf,
    responseSize = NA_integer_)
{
    request <- unbox(data.frame(datasetId, pageSize = responseSize))
    response <- request.post(host, "featuresets/search", request)
    while (response$nextPageToken != "" && nrow(response$featureSets) < nrows) {
        request$pageToken <- response$nextPageToken
        tmp <- request.post(host, "featuresets/search", request)
        response$featureSets <- bind_rows(response$featureSets, tmp$featureSets)
        response$nextPageToken <- tmp$nextPageToken
    }
    if (length(response$featureSets) == 0)
        return(NULL)

    if (length(response$featureSets) >  nrows)
        response$featureSets <- response$featureSets[seq(1, nrows), ]

    is.na(response$featureSets) <- response$featureSets == "NULL"
    DataFrame(response$featureSets)
}
