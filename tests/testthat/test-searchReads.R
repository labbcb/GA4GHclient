context("searchReads")

test_that("searchReads works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    readGroupSetId <- searchReadGroupSets(host, datasetId)
    skip_if_not(!is.null(readGroupSetId), "No read group set found.")
    referenceSetId <- searchReferenceSets(host, nrows = 1)$id
    referenceId <- searchReferences(host, referenceSetId, nrows = 1)$id
    response <- searchReads(host, readGroupIds, referenceId, start = 15000,
        end = 16000)
    expect_s4_class(response, "DataFrame")
})
