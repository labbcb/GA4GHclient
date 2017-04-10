#' @title searchPhenotypeAssociations function
#' @description This function gets a list of phenotype associations matching the
#' search criteria.
#' @details This function requests to \code{/featurephenotypeassociations/search}.
#' @param host URL of GA4GH API data server.
#' @param phenotypeAssociationSetId  Id of the PhenotypeAssociationSet to search.
#' @param featureIds Ids of the features. At least one \code{featureId} or
#' \code{phenotypeId} must be provided.
#' @param phenotypeIds Ids of the phenotypes. At least one \code{featureId} or
#' \code{phenotypeId} must be provided.
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
#' @return \code{\link{DataFrame}} object. \code{NULL} means no registry found.
#' @references \href{https://ga4gh-schemas.readthedocs.io/en/latest/schemas/genotype_phenotype_service.proto.html#SearchPhenotypeAssociations}{Official documentation}.
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' datasetId <- searchDatasets(host, nrows = 1)$id
#' id <- searchPhenotypeAssociationSets(host, datasetId, nrows = 1)$id
#' searchPhenotypeAssociations(host, id, nrows = 10)
#' }
#' @seealso \code{\link{DataFrame}}, \code{\link{searchPhenotypeAssociationSets}}
#' @export searchPhenotypeAssociations
searchPhenotypeAssociations <- function(host, phenotypeAssociationSetId,
    featureIds = character(), phenotypeIds = character(),
    nrows = Inf, responseSize = NA_integer_)
{
    request <- unbox(data.frame(phenotypeAssociationSetId,
        pageSize = responseSize))
    if (length(featureIds) > 0)
        request$featureIds <- list(featureIds)
    if (length(phenotypeIds) > 0)
        request$phenotypeIds <- list(phenotypeIds)
    response <- request.post(host, "featurephenotypeassociations/search", request)
    while (!is.null(response$nextPageToken) && nrow(response$associations) < nrows) {
        request$pageToken <- response$nextPageToken
        tmp <- request.post(host, "featurephenotypeassociations/search", request)
        response$associations <- bind_rows(response$associations, tmp$associations)
        response$nextPageToken <- tmp$nextPageToken
    }
    if (length(response$associations) == 0)
        return(NULL)

    if (nrow(response$associations) > nrows)
        response$associations <- response$associations[seq(1, nrows), ]

    is.na(response$associations) <- response$associations == "NULL"
    DataFrame(response$associations)
}
