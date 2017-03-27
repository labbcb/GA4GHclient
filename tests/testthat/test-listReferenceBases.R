context("listReferenceBases")

test_that("listReferenceBases works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    referenceSetId <- searchReferenceSets(host, nrows = 1)$id
    referenceId <- searchReferences(host, referenceSetId, nrows = 1)$id

    response <- listReferenceBases(host, referenceId, start = 1, end = 3000000)
    expect_s4_class(response, "BString")
    expect_equal(length(response), 3000000)
})
