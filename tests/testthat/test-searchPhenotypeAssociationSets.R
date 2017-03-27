context("searchPhenotypeAssociationSets")

test_that("searchPhenotypeAssociationSets works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    response <- searchPhenotypeAssociationSets(host, datasetId)
    expect_s4_class(response, "DataFrame")
    expect_equal(dim(response), c(1, 3))
})
