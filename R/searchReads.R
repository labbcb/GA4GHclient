#' @title searchReads function
#' @description Search for reads by genomic range (bases of aligned sequences)
#' @details This function requests \code{POST host/reads/search}.
#' @param host URL of GA4GH API data server.
#' @param readGroupIds The ReadGroups to search. At least one id must be
#' specified.
#' @param referenceId The reference to query. Leaving blank returns results from
#' all references, including unmapped reads - this could be very large.
#' @param start The start position (1-based) of this query. If a reference is
#' specified, this defaults to 0. Genomic positions are non-negative integers
#' less than reference length. Requests spanning the join of circular genomes
#' are represented as two requests one on each side of the join (position 1).
#' @param end The end position (1-based, exclusive) of this query. If a
#' reference is specified, this defaults to the reference's length.
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
#' @references \href{https://ga4gh-schemas.readthedocs.io/en/latest/schemas/read_service.proto.html#SearchReads}{Official documentation}.
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' readGroupIds <- "WyIxa2dlbm9tZXMiLCJyZ3MiLCJIRzAzMjcwIiwiRVJSMTgxMzI5Il0"
#' referenceSetId <- searchReferenceSets(host, nrows = 1)$id
#' referenceId <- searchReferences(host, referenceSetId, nrows = 1)$id
#' searchReads(host, readGroupIds, referenceId, start = 15000, end = 16000)
#' }
#' @seealso \code{\link{DataFrame}}
#' @export searchReads
searchReads <- function(host, readGroupIds, referenceId = NA_character_,
    start = NA_integer_, end = NA_integer_, nrows = Inf,
    responseSize = NA_integer_)
{
    request <- unbox(data.frame(referenceId, start = start - 1, end = end - 1,
        pageSize = responseSize))
    request$readGroupIds <- list(readGroupIds)
    response <- request.post(host, "reads/search", request)
    while (response$nextPageToken != "" && nrow(response$alignments) < nrows) {
        request$pageToken <- response$nextPageToken
        tmp <- request.post(host, "reads/search", request)
        response$alignments <- bind_rows(response$alignments, tmp$alignments)
        response$nextPageToken <- tmp$nextPageToken
    }
    if (length(response$alignments) == 0)
        return(NULL)

    if (nrow(response$alignments) >  nrows)
        response$alignments <- response$alignments[seq(1, nrows), ]

    is.na(response$alignments) <- response$alignments == "NULL"
    DataFrame(response$alignments)
}
