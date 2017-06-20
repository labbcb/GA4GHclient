context("searchVariantSets")

test_that("searchVariantSets works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    response <- searchVariantSets(host, datasetId)
    expect_s4_class(response, "DataFrame")
    expect_equal(dim(response), c(2, 5))
    expect_s4_class(response$metadata[[1]], "DataFrame")
    expect_equal(dim(response$metadata[[1]]), c(29, 6))
})

test_that("searchVariantSets nrows parameter works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    response <- searchVariantSets(host, datasetId, nrows = 1)
    expect_equal(dim(response), c(1, 5))
})

test_that("searchVariantSets responseSize parameter works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    response <- searchVariantSets(host, datasetId, responseSize = 1)
    expect_equal(dim(response), c(2, 5))
})
