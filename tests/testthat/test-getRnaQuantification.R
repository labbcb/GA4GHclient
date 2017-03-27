context("getRnaQuantification")

test_that("getRnaQuantification works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    rnaQuantificationSetId <- searchRnaQuantificationSets(host, datasetId, nrows = 1)$id
    rnaQuantificationId <- searchRnaQuantifications(host, rnaQuantificationSetId, nrows = 1)$id
    response <- getRnaQuantification(host, rnaQuantificationId)
    expect_s4_class(response, "DataFrame")
    expect_equal(dim(response), c(1, 6))
})
