context("getVariantSet")

test_that("getVariantSet works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    variantSetId <- searchVariantSets(host, datasetId, nrows = 1)$id
    variantSet <- getVariantSet(host, variantSetId)
    expect_s4_class(variantSet, "VCFHeader")
})
