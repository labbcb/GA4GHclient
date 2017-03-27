context("searchVariantAnnotations")

test_that("searchVariantAnnotations works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    variantSetId <- searchVariantSets(host, datasetId, nrows = 2)$id[2]
    id <- searchVariantAnnotationSets(host, variantSetId, nrows = 1)$id
    response <- searchVariantAnnotations(host, variantAnnotationSetId = id,
        referenceName = "1", start = 15000, end = 16000)
    expect_s4_class(response, "DataFrame")
})
