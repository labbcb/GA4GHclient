context("searchBiosamples")

test_that("searchBiosamples works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    response <- searchBiosamples(host, datasetId)
    expect_s4_class(response, "DataFrame")
    expect_equal(dim(response), c(3500, 26))
})

test_that("searchBiosamples nrows parameter works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    response <- searchBiosamples(host, datasetId, nrows = 10)
    expect_s4_class(response, "DataFrame")
    expect_equal(dim(response), c(10, 26))
})

test_that("searchBiosamples reponseSize parameter works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    response <- searchBiosamples(host, datasetId, responseSize = 1000)
    expect_s4_class(response, "DataFrame")
    expect_equal(dim(response), c(3500, 26))
})

test_that("searchBiosamples individualId parameter works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    response <- searchBiosamples(host, datasetId,
        individualId = "WyIxa2dlbm9tZXMiLCJpIiwiSEcwMDA5NiJd")
    expect_s4_class(response, "DataFrame")
    expect_equal(dim(response), c(1, 26))
})

test_that("searchBiosamples name parameter works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    response <- searchBiosamples(host, datasetId, name = "HG00096")
    expect_s4_class(response, "DataFrame")
    expect_equal(dim(response), c(1, 26))
})
