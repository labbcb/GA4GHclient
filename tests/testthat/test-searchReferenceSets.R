context("searchReferenceSets")

host <- "http://1kgenomes.ga4gh.org/"

test_that("searchReferenceSets works", {
    skip_on_bioc()
    response <- searchReferenceSets(host)
    expect_s4_class(response, "DataFrame")
    expect_equal(dim(response), c(1, 6))
})

test_that("searchReferenceSets nrows parameter works", {
    skip_on_bioc()
    response <- searchReferenceSets(host, nrows = 1)
    expect_equal(dim(response), c(1, 6))
})

test_that("searchReferenceSets responseSize parameter works", {
    skip_on_bioc()
    response <- searchReferenceSets(host, responseSize = 1)
    expect_equal(dim(response), c(1, 6))
})
