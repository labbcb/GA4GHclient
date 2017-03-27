#' @title searchVariantSets function
#' @description Search for for variant sets (VCF files).
#' @details This request maps to the body of \code{POST host/variantsets/search}.
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
#' @return \code{\link{DataFrame}} object.
#' @references \href{https://ga4gh-schemas.readthedocs.io/en/latest/schemas/variant_service.proto.html#SearchVariantSets}{Official documentation}.
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' datasetId <- searchDatasets(host, nrows = 1)$id
#' searchVariantSets(host, datasetId)
#' }
#' @seealso \code{\link{DataFrame}}, \code{\link{getVariantSet}}
#' @export searchVariantSets
searchVariantSets <- function(host, datasetId, nrows = Inf,
    responseSize = NA_integer_)
{
    request <- unbox(data.frame(datasetId, pageSize = responseSize))
    response <- request.post(host, "variantsets/search", request)
    while (response$nextPageToken != "" && nrow(response$variantSets) < nrows) {
        request$pageToken <- response$nextPageToken
        tmp <- request.post(host, "variantsets/search", request)
        response$variantSets <- bind_rows(response$variantSets, tmp$variantSets)
        response$nextPageToken <- tmp$nextPageToken
    }
    if (length(response$variantSets) == 0)
        return(NULL)

    if (nrow(response$variantSets) > nrows)
        response$variantSets <- response$variantSets[seq(1, nrows), ]

    is.na(response$variantSets) <- response$variantSets == "NULL"
    metadata <- response$variantSets$metadata
    response$variantSets$metadata <- NULL
    response <- DataFrame(response$variantSets)
    response$metadata <- lapply(metadata, DataFrame)
    response
}
