context("getVariantAnnotationSet")

test_that("getVariantAnnotationSet works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    variantSetId <- searchVariantSets(host, datasetId, nrows = 2)$id[2]
    variantAnnotationSetId <- searchVariantAnnotationSets(host, variantSetId,
        nrows = 1)$id
    variantAnnotationSet <- getVariantAnnotationSet(host,variantAnnotationSetId)
    expect_s4_class(variantAnnotationSet, "DataFrame")
})
