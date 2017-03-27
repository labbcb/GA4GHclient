context("getCallSet")

test_that("getCallSet works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    variantSetId <- searchVariantSets(host, datasetId, nrows = 1)$id
    callSetId <- searchCallSets(host, variantSetId, nrows = 1)$id
    callSet <- getCallSet(host, callSetId)
    expect_s4_class(callSet, "DataFrame")
    expect_named(callSet, c("updated", "name", "created",
        "variantSetIds", "biosampleId", "id"))
})
