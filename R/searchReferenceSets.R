#' @title searchReferenceSets function
#' @description Search for reference sets (reference genomes).
#' @details This function requests \code{POST host/references/search}.
#' @param host URL of GA4GH API data server.
#' @param md5checksum If unset, return the reference sets for which the
#' md5checksum matches this string (case-sensitive, exact match).
#' See ReferenceSet::md5checksum for details.
#' @param accession If unset, return the reference sets for which the accession
#' matches this string (case-sensitive, exact match).
#' @param assemblyId If unset, return the reference sets for which the
#' assemblyId matches this string (case-sensitive, exact match).
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
#' @references \href{https://ga4gh-schemas.readthedocs.io/en/latest/schemas/reference_service.proto.html#SearchReferenceSets}{Official documentation}.
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' searchReferenceSets(host)
#' }
#' @seealso \code{\link{DataFrame}}, \code{\link{getReferenceSet}}
#' @export searchReferenceSets
searchReferenceSets <- function(host, md5checksum = NA_character_,
    accession = NA_character_, assemblyId = NA_character_, nrows = Inf,
    responseSize = NA_integer_)
{
    request <- unbox(data.frame(md5checksum, accession, assemblyId,
        pageSize = responseSize))
    response <- request.post(host, "referencesets/search", request)
    while (!is.null(response$nextPageToken) && nrow(response$referenceSets) < nrows) {
        request$pageToken <- response$nextPageToken
        tmp <- request.post(host, "referencesets/search", request)
        response$referenceSets <- bind_rows(response$referenceSets,
            tmp$referenceSets)
        response$nextPageToken <- tmp$nextPageToken
    }
    if (length(response$referenceSets) == 0)
        return(NULL)

    if (nrow(response$referenceSets) > nrows)
        response$referenceSets <- response$referenceSets[seq(1, nrows), ]

    is.na(response$referenceSets) <- response$referenceSets == "NULL"
    DataFrame(response$referenceSets)
}
