context("searchVariantSets")

host <- "http://1kgenomes.ga4gh.org/"

test_that("searchVariantSets works", {
    skip_on_bioc()
    datasetId <- searchDatasets(host, nrows = 1)$id
    response <- searchVariantSets(host, datasetId)
    expect_s4_class(response, "DataFrame")
    expect_named(response, c("referenceSetId", "id", "datasetId",
        "name", "metadata"))
    expect_s4_class(response$metadata[[1]], "DataFrame")
    expect_equal(names(response$metadata[[1]]), c("description",
        "number", "value", "key", "type", "id"))
    expect_equal(nrow(response), 2)
})

test_that("searchVariantSets nrows parameter works", {
    skip_on_bioc()
    datasetId <- searchDatasets(host, nrows = 1)$id
    response <- searchVariantSets(host, datasetId, nrows = 1)
    expect_equal(dim(response), c(1, 5))
})

test_that("searchVariantSets responseSize parameter works", {
    skip_on_bioc()
    datasetId <- searchDatasets(host, nrows = 1)$id
    response <- searchVariantSets(host, datasetId, responseSize = 1)
    expect_equal(dim(response), c(2, 5))
})
