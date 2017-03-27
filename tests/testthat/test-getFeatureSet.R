context("getFeatureSet")

test_that("getFeatureSet works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    featureSetId <- searchFeatureSets(host, datasetId, nrows = 1)$id
    featureSet <- getFeatureSet(host, featureSetId)
    expect_s4_class(featureSet, "DataFrame")
    expect_named(featureSet, c("name", "sourceUri", "referenceSetId", "id",
        "datasetId"))
})
