#' @title searchReadGroupSets function
#' @description Search for read group sets (sequence alignement, e.g BAM files).
#' @details This function requests \code{POST host/readgroupsets/search}
#' @param host URL of GA4GH API data server.
#' @param datasetId The dataset to search.
#' @param name Only return read group sets with this name
#' (case-sensitive, exact match).
#' @param biosampleId Specifying the id of a BioSample record will return only
#' readgroups with the given biosampleId.
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
#' searchReadGroupSets(host, datasetId, nrows = 1)
#' }
#' @seealso \code{\link{DataFrame}}, \code{\link{getReadGroupSet}}
#' @export searchReadGroupSets
searchReadGroupSets <- function(host, datasetId, name = NA_character_,
    biosampleId = NA_character_, nrows = Inf, responseSize = NA_integer_)
{
    request <- unbox(data.frame(datasetId, name, biosampleId,
        pageSize = responseSize))
    response <- request.post(host, "readgroupsets/search", request)
    while (!is.null(response$nextPageToken) && nrow(response$readGroupSets) < nrows) {
        request$pageToken <- response$nextPageToken
        tmp <- request.post(host, "readgroupsets/search", request)
        response$readGroupSets <- bind_rows(response$readGroupSets, tmp$readGroupSets)
        response$nextPageToken <- tmp$nextPageToken
    }
    if (length(response$readGroupSets) == 0)
        return(NULL)

    if (nrow(response$readGroupSets) > nrows)
        response$readGroupSets <- response$readGroupSets[seq(1, nrows), ]

    is.na(response$readGroupSets) <- response$readGroupSets == "NULL"
    DataFrame(response$readGroupSets)
}
