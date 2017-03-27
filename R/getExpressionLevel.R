#' @title getExpressionLevel function
#' @description Get an expression level by its ID.
#' @details This function requests \code{GET host/expressionlevels/expressionLevelId}.
#' @param host URL of GA4GH API data server.
#' @param expressionLevelId ID of the expression level.
#' @return \code{\link{DataFrame}} object.
#' @references \href{https://ga4gh-schemas.readthedocs.io/en/latest/schemas/rna_quantification_service.proto.html#GetExpressionLevel}{Official documentation}.
#' @examples
#' host <- "http://1kgenomes.ga4gh.org/"
#' \dontrun{
#' datasetId <- searchDatasets(host, nrows = 1)$id
#' rnaQuantificationSetId <- searchRnaQuantificationSets(host, datasetId, nrow = 1)$id
#' rnaQuantificationId <- searchRnaQuantifications(host, rnaQuantificationSetId, nrows = 1)$id
#' expressionLevelId <- searchExpressionLevels(host, rnaQuantificationId, nrows = 1)$id
#' getExpressionLevel(host, expressionLevelId)
#' }
#' @seealso \code{\link{DataFrame}}, \code{\link{searchExpressionLevels}}
#' @export getExpressionLevel
getExpressionLevel <- function(host, expressionLevelId)
{
    response <- request.get(host, "expressionlevels", expressionLevelId)
    DataFrame(as.list(unlist(response)))
}
