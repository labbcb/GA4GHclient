context("as.VCFHeader")

test_that("as.VCFHeader works", {
    skip_on_bioc()
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host)$datasets$id
    variantSetId <- searchVariantSets(host, datasetId, pageSize = 1)$variantSets$id
    variantSet <- getVariantSet(host, variantSetId)
    output <- as.VCFHeader(variantSet)
    expect_is(output, "VCFHeader")
    cols <- c("number", "type", "description")
    expect_equal(as.data.frame(info(output)), variantSet$metadat[, cols],
        check.attributes = FALSE)
})
