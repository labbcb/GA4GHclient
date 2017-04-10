context("searchVariants")

test_that("searchVariants works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    variantSetId <- searchVariantSets(host, datasetId, nrows = 1)$id
    response <- searchVariants(host, variantSetId, referenceName = "1",
        start = 15000, end = 16000)
    expect_s4_class(response, "VCF")
    expect_equal(length(response), 26)
})

test_that("searchVariants nrows parameter works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    variantSetId <- searchVariantSets(host, datasetId, nrows = 1)$id
    response <- searchVariants(host, variantSetId, referenceName = "1",
        start = 15000, end = 16000, nrows = 10)
    expect_equal(length(response), 10)
})

test_that("searchVariants responseSize parameter works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    variantSetId <- searchVariantSets(host, datasetId, nrows = 1)$id
    response <- searchVariants(host, variantSetId, referenceName = "1",
        start = 15000, end = 16000, responseSize = 10)
    expect_equal(length(response), 26)
})

test_that("searchVariants with one callset works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    variantSetId <- searchVariantSets(host, datasetId, nrows = 1)$id
    callSetId <- searchCallSets(host, variantSetId, nrows = 1)$id
    response <- searchVariants(host, variantSetId, referenceName = "1",
        start = 15000, end = 16000, callSetIds = callSetId)
    expect_equal(nrow(response), 26)
    expect_length(geno(response)$GT, 26)
})

test_that("searchVariants with many callsets works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    variantSetId <- searchVariantSets(host, datasetId, nrows = 1)$id
    callSetIds <- searchCallSets(host, variantSetId, nrows = 5)$id
    response <- searchVariants(host, variantSetId, referenceName = "1",
        start = 15000, end = 16000, callSetIds = callSetIds)
    expect_equal(nrow(response), 26)
    expect_equal(dim(geno(response)$GT), c(26, 5))
})

test_that("searchVariants asVCF=FALSE returns DataFrame", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    variantSetId <- searchVariantSets(host, datasetId, nrows = 1)$id
    response <- searchVariants(host, variantSetId, referenceName = "1",
        start = 15000, end = 16000, asVCF = FALSE)
    expect_s4_class(response, "DataFrame")
    expect_equal(dim(response), c(26, 24))
})

test_that("searchVariants with no result should return empty data frame", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    variantSetId <- searchVariantSets(host, datasetId, nrows = 1)$id
    response <- searchVariants(host, variantSetId, referenceName = "invalid",
        start = 15000, end = 16000)
    expect_equal(dim(response), c(0,0))
})
