#' @title searchFeatures function
#' @description Search for features (lines of genomic feature files).
#' @details This function requests \code{POST host/features/search}.
#' @param host URL of GA4GH API data server.
#' @param featureSetId The annotation set to search within. Either featureSetId
#' or parentId must be non-empty.
#' @param name Only returns features with this name
#' (case-sensitive, exact match).
#' @param geneSymbol Only return features with matching the provided gene symbol
#' (case-sensitive, exact match). This field may be replaced with a more generic
#' representation in a future version.
#' @param parentId Restricts the search to direct children of the given parent
#' feature ID. Either feature_set_id or parent_id must be non-empty.
#' @param referenceName Only return features on the reference with this name
#' (matched to literal reference name as imported from the GFF3).
#' @param start Required, if name or symbol not provided. The beginning of the
#' window (0-based, inclusive) for which overlapping features should be
#' returned. Genomic positions are non-negative integers less than reference
#' length. Requests spanning the join of circular genomes are represented as two
#' requests one on each side of the join (position 0).
#' @param end Required, if name or symbol not provided. The end of the window
#' (0-based, exclusive) for which overlapping features should be returned.
#' @param featureTypes TODO: To be replaced with a fully featured ontology
#' search once the Metadata definitions are rounded out. If specified, this
#' query matches only annotations whose feature_type matches one of the provided
#' ontology terms.
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
#' @references \href{https://ga4gh-schemas.readthedocs.io/en/latest/schemas/sequence_annotation_service.proto.html#SearchFeatures}{Official documentation}.
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' datasetId <- searchDatasets(host, nrows = 1)$id
#' featureSetId <- searchFeatureSets(host, datasetId, nrows = 1)$id
#' searchFeatures(host, featureSetId, nrows = 10)
#' }
#' @seealso \code{\link{DataFrame}}, \code{\link{getFeature}}
#' @export searchFeatures
searchFeatures <- function(host, featureSetId, name = NA_character_,
    geneSymbol = NA_character_, parentId = NA_character_,
    referenceName = NA_character_, start = NA_integer_, end = NA_integer_,
    featureTypes = character(), nrows = Inf, responseSize = NA_integer_)
{
    request <- unbox(data.frame(featureSetId, name, geneSymbol, parentId,
        referenceName, start, end, pageSize = responseSize))
    if (length(featureTypes) != 0)
        request$featureTypes <- list(featureTypes)
    response <- request.post(host, "features/search", request)
    while (response$nextPageToken != "" && nrow(response$features) < nrows) {
        request$pageToken <- response$nextPageToken
        tmp <- request.post(host, "features/search", request)
        response$features <- bind_rows(response$features, tmp$features)
        response$nextPageToken <- tmp$nextPageToken
    }
    if (length(response$features) == 0)
        return(NULL)

    if (nrow(response$feature) > nrows)
        response$features <- response$features[seq(1, nrows), ]

    is.na(response$features) <- response$features == "NULL"
    DataFrame(response$feature)
}
