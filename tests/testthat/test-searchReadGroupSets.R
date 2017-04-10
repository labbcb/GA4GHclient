context("searchReadGroupSets")

test_that("searchReadGroupSets works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    response <- searchReadGroupSets(host, datasetId, nrows = 1)
    skip_if_not(!is.null(response), "No read group set found.")
    expect_s4_class(response, "DataFrame")
})
