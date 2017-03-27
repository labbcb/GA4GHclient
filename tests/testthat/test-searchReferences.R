context("searchReferences")

host <- "http://1kgenomes.ga4gh.org/"

test_that("searchReferences works", {
    skip_on_bioc()
    referenceSetId <- searchReferenceSets(host, nrows = 1)$id
    response <- searchReferences(host, referenceSetId)
    expect_s4_class(response, "DataFrame")
    expect_named(response, c("name", "sourceUri", "sourceAccessions",
        "sourceDivergence", "length", "md5checksum", "isDerived", "id",
        "ncbiTaxonId"))
    expect_equal(nrow(response), 86)
})

test_that("searchReferences nrows parameter works", {
    skip_on_bioc()
    referenceSetId <- searchReferenceSets(host, nrows = 1)$id
    response <- searchReferences(host, referenceSetId, nrows = 2)
    expect_equal(dim(response), c(2, 9))
})

test_that("searchReferences responseSize parameter works", {
    skip_on_bioc()
    referenceSetId <- searchReferenceSets(host, nrows = 1)$id
    response <- searchReferences(host, referenceSetId, responseSize = 43)
    expect_equal(dim(response), c(86, 9))
})

test_that("searchReferences md5checksum parameter works", {
    skip_on_bioc()
    referenceSetId <- searchReferenceSets(host, nrows = 1)$id
    md5checksum <- searchReferences(host, referenceSetId,
        nrows = 1)$md5checksum
    response <- searchReferences(host, referenceSetId,
        md5checksum = md5checksum)
    expect_equal(dim(response), c(1, 9))
})
