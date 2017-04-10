#' @title searchVariantAnnotationSets function
#' @description Search for variant annotation sets (annotated VCF files).
#' @details This function maps to \code{POST host/variantannotationsets/search}.
#' @param host URL of GA4GH API data server.
#' @param variantSetId Required. The VariantSet to search.
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
#' @references \href{https://ga4gh-schemas.readthedocs.io/en/latest/schemas/allele_annotation_service.proto.html#SearchVariantAnnotationSets}{Official documentation}.
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' datasetId <- searchDatasets(host, nrows = 1)$id
#' variantSetId <- searchVariantSets(host, datasetId, nrows = 2)$id[2]
#' searchVariantAnnotationSets(host, variantSetId)
#' }
#' @seealso \code{\link{DataFrame}}
#' @export searchVariantAnnotationSets
searchVariantAnnotationSets <- function(host, variantSetId, nrows = Inf,
    responseSize = NA_integer_)
{
    request <- unbox(data.frame(variantSetId, pageSize = responseSize))
    response <- request.post(host, "variantannotationsets/search", request)
    while (!is.null(response$nextPageToken) && nrow(response$variantAnnotationSets) < nrows) {
        request$pageToken <- response$nextPageToken
        tmp <- request.post(host, "variantannotationsets/search", request)
        response$variantAnnotationSets <-
            bind_rows(response$variantAnnotationSets, tmp$variantAnnotationSets)
        response$nextPageToken <- tmp$nextPageToken
    }
    if (length(response$variantAnnotationSets) == 0)
        return(NULL)

    if (nrow(response$variantAnnotationSets) > nrows)
        response$variantAnnotationSets <- response$variantAnnotationSets[seq(1, nrows), ]

    is.na(response$variantAnnotationSets) <- response$variantAnnotationSets == "NULL"
    DataFrame(response$variantAnnotationSets)
}
