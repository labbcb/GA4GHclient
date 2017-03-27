context("searchRnaQuantifications")

test_that("searchRnaQuantifications works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    id <- searchRnaQuantificationSets(host, datasetId, nrows = 1)$id
    response <- searchRnaQuantifications(host, id)
    expect_s4_class(response, "DataFrame")
    expect_equal(dim(response), c(7, 8))
})

test_that("searchRnaQuantifications nrows parameter works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    id <- searchRnaQuantificationSets(host, datasetId, nrows = 1)$id
    response <- searchRnaQuantifications(host, id, nrows = 1)
    expect_s4_class(response, "DataFrame")
    expect_equal(dim(response), c(1, 8))
})

test_that("searchRnaQuantifications reponseSize parameter works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    id <- searchRnaQuantificationSets(host, datasetId, nrows = 1)$id
    response <- searchRnaQuantifications(host, id, responseSize = 1)
    expect_s4_class(response, "DataFrame")
    expect_equal(dim(response), c(7, 8))
})

test_that("searchRnaQuantifications biosampleId parameter works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    id <- searchRnaQuantificationSets(host, datasetId, nrows = 1)$id
    response <- searchRnaQuantifications(host, id,
        biosampleId = "WyIxa2dlbm9tZXMiLCJiIiwiSEcwMDEwNCJd")
    expect_s4_class(response, "DataFrame")
    expect_equal(dim(response), c(1, 8))
})
