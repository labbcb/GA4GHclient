#' @title searchVariantAnnotations function
#' @description Search for annotated variants by genomic range.
#' @details This function requests \code{POST host/variantannotations/search}.
#' @param host URL of GA4GH API data server.
#' @param variantAnnotationSetId Required. The ID of the variant annotation set
#' to search over.
#' @param referenceName Only return variants with reference alleles on the
#' reference with this name. One of this field or reference_id is required.
#' @param referenceId Only return variants with reference alleles on the
#' reference with this ID. One of this field or reference_name is required.
#' @param start Required if reference_name or reference_id supplied. The
#' beginning of the window (1-based, inclusive) for which variants with
#' overlapping reference alleles should be returned. Genomic positions are
#' non-negative integers less than reference length. Requests spanning the
#' join of circular genomes are represented as two requests one on each side
#' of the join (position 1).
#' @param end Required if reference_name or reference_id supplied. The end of
#' the window (1-based, exclusive) for which variants with overlapping reference
#' alleles should be returned.
#' @param effects This filter allows variant, transcript combinations to be
#' extracted by effect type(s). Only return variant annotations including any
#' of these effects and only return transcript effects including any of these
#' effects. Exact matching across all fields of the Sequence Ontology
#' OntologyTerm is required. (A transcript effect may have multiple SO effects
#' which will all be reported.) If empty, return all variant annotations.
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
#' @references \href{https://ga4gh-schemas.readthedocs.io/en/latest/schemas/allele_annotation_service.proto.html#SearchVariantAnnotations}{Official documentation}.
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' datasetId <- searchDatasets(host, nrows = 1)$id
#' variantSetId <- searchVariantSets(host, datasetId, nrows = 2)$id[2]
#' id <- searchVariantAnnotationSets(host, variantSetId, nrows = 1)$id
#' searchVariantAnnotations(host, variantAnnotationSetId = id,
#'     referenceName = "1", start = 15000, end = 16000)
#' }
#' @seealso \code{\link{DataFrame}}
#' @export searchVariantAnnotations
searchVariantAnnotations <- function(host, variantAnnotationSetId,
    referenceName = NA_character_, referenceId = NA_character_,
    start = NA_integer_, end = NA_integer_, effects = list(),
    nrows = Inf, responseSize = NA_integer_)
{
    request <- unbox(data.frame(variantAnnotationSetId, referenceName,
        referenceId, start = start - 1, end = end - 1, pageSize = responseSize))
    if (length(effects) != 0)
        request$effects <- effects
    response <- request.post(host, "variantannotations/search", request)
    while (!is.null(response$nextPageToken) && nrow(response$variantAnnotations) < nrows) {
        request$pageToken <- response$nextPageToken
        tmp <- request.post(host, "variantannotations/search", request)
        response$variantAnnotations <- bind_rows(response$variantAnnotations,
            tmp$variantAnnotations)
        response$nextPageToken <- tmp$nextPageToken
    }
    if (length(response$variantAnnotations) == 0)
        return(NULL)

    if (nrow(response$variantAnnotations) > nrows)
        response$variantAnnotations <- response$variantAnnotations[seq(1, nrows), ]

    is.na(response$variantAnnotations) <- response$variantAnnotations == "NULL"
    DataFrame(response$variantAnnotations)
}
