context("searchDatasets")

host <- "http://1kgenomes.ga4gh.org/"

test_that("searchDatasets works", {
    skip_on_bioc()
    response <- searchDatasets(host)
    expect_is(response, "DataFrame")
    expect_named(response, c("description", "id", "name"))
    expect_equal(nrow(response), 1)
})

test_that("searchDatasets nrows parameter works", {
    skip_on_bioc()
    response <- searchDatasets(host, nrows = 1)
    expect_equal(dim(response), c(1, 3))
})

test_that("searchDatasets responseSize parameter works", {
    skip_on_bioc()
    response <- searchDatasets(host, responseSize = 1)
    expect_equal(dim(response), c(1, 3))
})
