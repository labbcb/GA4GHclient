context("searchExpressionLevels")

test_that("searchExpressionLevels works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    id <- searchRnaQuantificationSets(host, datasetId, nrows = 1)$id
    rnaQuantificationId <- searchRnaQuantifications(host, id, nrows = 1)$id
    response <- searchExpressionLevels(host, rnaQuantificationId, nrows = 100)
    expect_s4_class(response, "DataFrame")
    expect_equal(dim(response), c(100, 11))
})

test_that("searchExpressionLevels nrows parameter works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    id <- searchRnaQuantificationSets(host, datasetId, nrows = 1)$id
    rnaQuantificationId <- searchRnaQuantifications(host, id, nrows = 1)$id
    response <- searchExpressionLevels(host, rnaQuantificationId, nrows = 1)
    expect_s4_class(response, "DataFrame")
    expect_equal(dim(response), c(1, 11))
})

test_that("searchExpressionLevels reponseSize parameter works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    id <- searchRnaQuantificationSets(host, datasetId, nrows = 1)$id
    rnaQuantificationId <- searchRnaQuantifications(host, id, nrows = 1)$id
    response <- searchExpressionLevels(host, rnaQuantificationId, nrows = 100,
        responseSize = 10)
    expect_s4_class(response, "DataFrame")
    expect_equal(dim(response), c(100, 11))
})
