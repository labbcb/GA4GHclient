context("getBiosample")

test_that("getBiosample works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    biosampleId <- searchBiosamples(host, datasetId, nrows = 1)$id
    response <- getBiosample(host, biosampleId)
    expect_s4_class(response, "DataFrame")
    expect_equal(dim(response), c(1, 5))
})
