context("searchFeatures")

host <- "http://1kgenomes.ga4gh.org/"

test_that("searchFeatures works", {
    skip_on_bioc()
    datasetId <- searchDatasets(host, nrows = 1)$id
    featureSetId <- searchFeatureSets(host, datasetId)$id
    response <- searchFeatures(host, featureSetId, referenceName = "chr1",
        start = 15000, end = 16000)
    expect_is(response, "DataFrame")
    expect_equal(nrow(response), 4)
})

test_that("searchFeatures nrows parameter works", {
    skip_on_bioc()
    datasetId <- searchDatasets(host, nrows = 1)$id
    featureSetId <- searchFeatureSets(host, datasetId)$id
    response <- searchFeatures(host, featureSetId, referenceName = "chr1",
        start = 15000, end = 16000, nrows = 1)
    expect_equal(dim(response), c(1, 34))
})

test_that("searchFeatures responseSize parameter works", {
    skip_on_bioc()
    datasetId <- searchDatasets(host, nrows = 1)$id
    featureSetId <- searchFeatureSets(host, datasetId)$id
    response <- searchFeatures(host, featureSetId, referenceName = "chr1",
        start = 15000, end = 16000, responseSize = 1)
    expect_equal(dim(response), c(4, 34))
})
