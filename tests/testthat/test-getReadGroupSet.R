context("getReadGroupSet")

test_that("getReadGroupSet works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    readGroupSetId <- searchReadGroupSets(host, datasetId, nrows = 1)$id
    skip_if_not(!is.null(readGroupSetId), "No read group set found.")
    response <- getReadGroupSet(host, readGroupSetId)
    expect_s4_class(response, "DataFrame")
})
