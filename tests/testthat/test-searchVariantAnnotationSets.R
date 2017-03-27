context("searchVariantAnnotationSets")

host <- "http://1kgenomes.ga4gh.org/"

test_that("searchVariantAnnotationSets works", {
    skip_on_bioc()
    datasetId <- searchDatasets(host, nrows = 1)$id
    variantSetId <- searchVariantSets(host, datasetId, nrows = 2)$id[2]
    response <- searchVariantAnnotationSets(host, variantSetId)
    expect_s4_class(response, "DataFrame")
    expect_equal(nrow(response), 1)
})

test_that("searchVariantAnnotationSets responseSize parameter works", {
    skip_on_bioc()
    datasetId <- searchDatasets(host, nrows = 1)$id
    variantSetId <- searchVariantSets(host, datasetId, nrows = 2)$id[2]
    response <- searchVariantAnnotationSets(host, variantSetId, responseSize =1)
    expect_equal(dim(response), c(1, 47))
})

test_that("searchVariants with no result should return NULL", {
    skip_on_bioc()
    datasetId <- searchDatasets(host, nrows = 1)$id
    variantSetId <- searchVariantSets(host, datasetId, nrows = 2)$id[2]
    variantSetId <- searchVariantSets(host, datasetId, nrows = 1)$id
    response <- searchVariantAnnotationSets(host, variantSetId)
    expect_null(response)
})
