context("getReferenceSet")

test_that("getReferenceSet works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    referenceSetId <- searchReferenceSets(host, nrows = 1)$id
    referenceSet <- getReferenceSet(host, referenceSetId)
    expect_s4_class(referenceSet, "DataFrame")
    expect_named(referenceSet, c("description", "sourceUri",
        "assemblyId", "md5checksum", "isDerived",
        "id", "ncbiTaxonId", "name"))
})
