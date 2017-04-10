context("getFeature")

test_that("getFeature works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    featureSetId <- searchFeatureSets(host, datasetId, nrows = 1)$id
    featureId <- searchFeatures(host, featureSetId, nrows = 1)$id
    feature <- getFeature(host, featureId)
    expect_s4_class(feature, "DataFrame")
    expect_equal(dim(feature), c(1, 30))
})

