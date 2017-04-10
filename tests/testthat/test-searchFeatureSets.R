context("searchFeatureSets")

host <- "http://1kgenomes.ga4gh.org/"

test_that("searchFeatureSets works", {
    skip_on_bioc()
    datasetId <- searchDatasets(host, nrows = 1)$id
    response <- searchFeatureSets(host, datasetId)
    expect_s4_class(response, "DataFrame")
    expect_equal(dim(response), c(1, 4))
})

test_that("searchFeatureSets full parameter works", {
    skip_on_bioc()
    datasetId <- searchDatasets(host, nrows = 1)$id
    response <- searchFeatureSets(host, datasetId, responseSize = 1)
    expect_equal(dim(response), c(1, 4))
})
