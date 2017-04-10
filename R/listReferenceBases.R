#' @title listReferenceBases function
#' @description Get the sequence bases of a reference genome by genomic range.
#' @details This function requests \code{POST host/listreferencebases}.
#' @param host URL of GA4GH API data server.
#' @param referenceId The ID of the Reference to be retrieved.
#' @param start The start position (1-based) of this query. Defaults to 0.
#' Genomic positions are non-negative integers less than reference length.
#' Requests spanning the join of circular genomes are represented as two
#' requests one on each side of the join (position 1).
#' @param end The end position (1-based, inclusive) of this query.
#' Defaults to the length of this Reference.
#' @return \code{\link{BString}} object.
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' referenceSetId <- searchReferenceSets(host, nrows = 1)$id
#' referenceId <- searchReferences(host, referenceSetId, nrows = 1)$id
#' listReferenceBases(host, referenceId, start = 1, end = 100)
#' }
#' @seealso \code{\link{searchReferenceSets}}, \code{\link{searchReferences}}
#' @export listReferenceBases
listReferenceBases <- function(host, referenceId, start = 1, end = NA_integer_)
{
    request <- unbox(data.frame(referenceId, start = start - 1, end = end))
    response <- request.post(host, "listreferencebases", request)
    sequence <- BString(response$sequence)
    while (!is.null(response$nextPageToken)) {
        request$pageToken <- response$nextPageToken
        tmp <- request.post(host, "listreferencebases", request)
        sequence <- c(sequence, BString(tmp$sequence))
        response$nextPageToken <- tmp$nextPageToken
    }
    sequence
}
