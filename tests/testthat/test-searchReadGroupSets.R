context("searchReadGroupSets")

test_that("searchReadGroupSets works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    response <- searchReadGroupSets(host, datasetId, nrows = 1)
    expect_s4_class(response, "DataFrame")
})
