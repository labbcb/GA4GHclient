context("searchCallSets")

test_that("searchCallSets works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    variantSetId <- searchVariantSets(host, datasetId, nrows = 1)$id
    response <- searchCallSets(host, variantSetId)
    expect_s4_class(response, "DataFrame")
    expect_equal(dim(response), c(2504, 4))
})

test_that("searchCallSets nrows parameter works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    variantSetId <- searchVariantSets(host, datasetId, nrows = 1)$id
    response <- searchCallSets(host, variantSetId, nrows = 1)
    expect_equal(dim(response), c(1, 4))
})

test_that("searchCallSets responseSize parameter works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    variantSetId <- searchVariantSets(host, datasetId, nrows = 1)$id
    response <- searchCallSets(host, variantSetId, responseSize = 1000)
    expect_equal(dim(response), c(2504, 4))
})

test_that("searchCallSets name parameter works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    variantSetId <- searchVariantSets(host, datasetId, nrows = 1)$id
    name <- searchCallSets(host, variantSetId, nrows = 1)$name
    response <- searchCallSets(host, variantSetId, name = name)
    expect_equal(response$name, name)
})

test_that("searchCallSets biosampleId parameter works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    variantSetId <- searchVariantSets(host, datasetId, nrows = 1)$id
    biosampleId <- searchCallSets(host, variantSetId, nrows = 1)$biosampleId
    response <- searchCallSets(host, variantSetId, biosampleId = biosampleId)
    expect_equal(response$biosampleId, biosampleId)
})
