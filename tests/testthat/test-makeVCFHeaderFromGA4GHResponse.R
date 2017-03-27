context("makeVCFHeaderFromGA4GHResponse")

test_that("makeVCFHeaderFromGA4GHResponse works", {
    skip_on_bioc()
    library(VariantAnnotation)
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    variantSetId <- searchVariantSets(host, datasetId, nrows = 1)$id
    variantSet <- getVariantSet(host, variantSetId, asVCFHeader = FALSE)

    output <- makeVCFHeaderFromGA4GHResponse(variantSet)
    expect_s4_class(output, "VCFHeader")
    cols <- c("number", "type", "description")
    expect_equal(as.data.frame(info(output)),
        as.data.frame(variantSet$metadata[[1]][-1, cols]),
        check.name = FALSE, check.attributes = FALSE)
})
