context("searchRnaQuantificationSets")

test_that("searchRnaQuantificationSets works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    response <- searchRnaQuantificationSets(host, datasetId)
    expect_s4_class(response, "DataFrame")
    expect_equal(dim(response), c(1, 3))
})

test_that("searchRnaQuantificationSets nrows parameter works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    response <- searchRnaQuantificationSets(host, datasetId, nrows = 1)
    expect_s4_class(response, "DataFrame")
    expect_equal(dim(response), c(1, 3))
})

test_that("searchRnaQuantificationSets reponseSize parameter works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    response <- searchRnaQuantificationSets(host, datasetId, responseSize = 1)
    expect_s4_class(response, "DataFrame")
    expect_equal(dim(response), c(1, 3))
})
