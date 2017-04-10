context("getDataset")

test_that("getDataset works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    dataset <- getDataset(host, datasetId)
    expect_s4_class(dataset, "DataFrame")
    expect_equal(dim(dataset), c(1, 3))
})
