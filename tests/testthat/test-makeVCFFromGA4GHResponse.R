context("makeVCFFromGA4GHResponse")

test_that("makeVCFFromGA4GHResponse works", {
    skip_on_bioc()
    library(VariantAnnotation)
    host <- "http://1kgenomes.ga4gh.org/"
    datasetId <- searchDatasets(host, nrows = 1)$id
    variantSetId <- searchVariantSets(host, datasetId, nrows = 1)$id
    callSetIds <- searchCallSets(host, variantSetId, nrows = 2)$id
    response <- searchVariants(host, variantSetId, referenceName = "1",
        start = 15000, end = 16000, callSetIds = callSetIds, asVCF = FALSE)

    output <- makeVCFFromGA4GHResponse(response)
    info.idx <- startsWith(names(response), "info.")
    expect_equal(unlist(as.data.frame(info(output))[, -15]),
        unlist(as.data.frame(response[, info.idx])),
        check.attributes = FALSE, check.names = FALSE)
})
