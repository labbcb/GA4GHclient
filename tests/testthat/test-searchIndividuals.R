context("searchIndividuals")

test_that("searchIndividuals works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    response <- searchIndividuals(host, datasetId)
    expect_s4_class(response, "DataFrame")
    expect_equal(dim(response), c(3500, 8))
})

test_that("searchIndividuals nrows parameter works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    response <- searchIndividuals(host, datasetId, nrows = 10)
    expect_s4_class(response, "DataFrame")
    expect_equal(dim(response), c(10, 8))
})

test_that("searchIndividuals reponseSize parameter works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    response <- searchIndividuals(host, datasetId, responseSize = 1000)
    expect_s4_class(response, "DataFrame")
    expect_equal(dim(response), c(3500, 8))
})

test_that("searchIndividuals name parameter works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    response <- searchIndividuals(host, datasetId, name = "HG00096")
    expect_s4_class(response, "DataFrame")
    expect_equal(dim(response), c(1, 8))
})
