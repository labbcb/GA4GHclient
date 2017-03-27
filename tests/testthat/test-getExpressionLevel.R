context("getExpressionLevel")

test_that("getExpressionLevel works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    id <- searchRnaQuantificationSets(host, datasetId, nrows = 1)$id
    rnaQuantificationId <- searchRnaQuantifications(host, id, nrows = 1)$id
    expressionLevelId <- searchExpressionLevels(host, rnaQuantificationId, nrows = 1)$id
    response <- getExpressionLevel(host, expressionLevelId)
    expect_s4_class(response, "DataFrame")
    expect_equal(dim(response), c(1, 11))
})
