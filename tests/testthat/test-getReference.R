context("getReference")

test_that("getReference works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    referenceSetId <- searchReferenceSets(host, nrows = 1)$id
    referenceId <- searchReferences(host, referenceSetId, nrows = 1)$id
    reference <- getReference(host, referenceId)
    expect_s4_class(reference, "DataFrame")
    expect_named(reference, c("name", "sourceUri", "sourceDivergence", "length",
        "md5checksum", "isDerived", "id", "ncbiTaxonId"))
})
