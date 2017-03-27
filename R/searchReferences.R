#' @title searchReferences function
#' @description Search for references (genome sequences, e.g. chromosomes).
#' @details This function requests \code{POST host/references/search}.
#' @param host URL of GA4GH API data server.
#' @param referenceSetId The ReferenceSet to search.
#' @param md5checksum If specified, return the references for which the
#' md5checksum matches this string (case-sensitive, exact match).
#' See ReferenceSet::md5checksum for details.
#' @param accession If specified, return the references for which the accession
#' matches this string (case-sensitive, exact match).
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
#' @references \href{https://ga4gh-schemas.readthedocs.io/en/latest/schemas/reference_service.proto.html#SearchReferences}{Official documentation}.
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' referenceSetId <- searchReferenceSets(host, nrows = 1)$id
#' searchReferences(host, referenceSetId)
#' }
#' @seealso \code{\link{DataFrame}}, \code{\link{getReference}}
#' @export searchReferences
searchReferences <- function(host, referenceSetId, md5checksum = NA_character_,
    accession = NA_character_, nrows = Inf, responseSize = NA_integer_)
{
    request <- unbox(data.frame(referenceSetId, md5checksum, accession,
        pageSize = responseSize))
    response <- request.post(host, "references/search", request)
    while (response$nextPageToken != "" && nrow(response$references) < nrows) {
        request$pageToken <- response$nextPageToken
        tmp <- request.post(host, "references/search", request)
        response$references <- bind_rows(response$references, tmp$references)
        response$nextPageToken <- tmp$nextPageToken
    }
    if (length(response$references) == 0)
        return(NULL)

    if (nrow(response$references) > nrows)
        response$references <- response$references[seq(1, nrows), ]

    is.na(response$references) <- response$references == "NULL"
    DataFrame(response$references)
}
