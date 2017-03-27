context("getIndividual")

test_that("getIndividual works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    individualId <- searchIndividuals(host, datasetId, nrows = 1)$id
    response <- getIndividual(host, individualId)
    expect_s4_class(response, "DataFrame")
    expect_equal(ncol(response), 29)
})
