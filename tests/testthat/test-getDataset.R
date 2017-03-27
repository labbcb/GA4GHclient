context("getDataset")

host <- "http://1kgenomes.ga4gh.org/"

test_that("getDataset works", {
    skip_on_bioc()
    datasetId <- searchDatasets(host, nrows = 1)$id
    dataset <- getDataset(host, datasetId)
    expect_s4_class(dataset, "DataFrame")
    expect_named(dataset, c("description", "id", "name"))
})
