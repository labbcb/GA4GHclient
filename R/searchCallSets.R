#' @title searchCallSets function
#' @description Search for call sets (sample columns of VCF files).
#' @details This function requests \code{POST host/callsets/search}.
#' @param host URL of GA4GH API data server.
#' @param variantSetId The VariantSet to search.
#' @param name Only return call sets with this name (case-sensitive, exact match).
#' @param biosampleId Return only call sets generated from the provided BioSample ID.
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
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' datasetId <- searchDatasets(host, nrows = 1)$id
#' variantSetId <- searchVariantSets(host, datasetId, nrows = 1)$id
#' searchCallSets(host, variantSetId)
#' }
#' @seealso \code{\link{DataFrame}}, \code{\link{getCallSet}}
#' @export searchCallSets
searchCallSets <- function(host, variantSetId, name = NA_character_,
    biosampleId = NA_character_, nrows = Inf, responseSize = NA_integer_)
{
    request <- unbox(data.frame(variantSetId, name, biosampleId,
        pageSize = responseSize))
    response <- request.post(host, "callsets/search", request)
    while (response$nextPageToken != "" && nrow(response$callSets) < nrows) {
        request$pageToken <- response$nextPageToken
        tmp <- request.post(host, "callsets/search", request)
        response$callSets <- bind_rows(response$callSets, tmp$callSets)
        response$nextPageToken <- tmp$nextPageToken
    }
    if (length(response$callSets) == 0)
        return(NULL)

    if (nrow(response$callSets) > nrows)
        response$callSets <- response$callSets[seq(1, nrows), ]

    is.na(response$callSets) <- response$callSets == "NULL"
    DataFrame(response$callSets)
}
